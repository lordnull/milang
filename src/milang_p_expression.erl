-module(milang_p_expression).

-export([parse/1]).
-export(
	[ expression/0
	, primary/0
	, sub_expression/0
	, call/0
	, match/0
	, argument/0
	, literal/0
	, record_field_key/0
	]).
-export(
	[ literal_map/0
	, map_key_access/0
	, map_update/0
	, map_entry_update/0
	]).
-export(
	[ literal_list/0
	]).

parse(String) ->
	parse:it(String, expression()).

-spec expression() -> parse:parser(term(), milang_ast:expression()).
expression() ->
	Series = parse:repeat_when(opt_space(primary()), opt_space(milang_p_atomic:infix())),
	Tagged = parse:tag(undefined, Series),
	Mapper = fun
		({_, _, [Only]}) ->
			Only;
		({_, L, [Head | Tail]}) ->
			InfixOps = pair_infix_ops(Tail),
			Data = #{ head => Head, infix_ops => InfixOps},
			milang_ast:ast_node(L, <<>>, expression, Data)
	end,
	parse:set_tag(expression, parse:map(Tagged, Mapper)).

pair_infix_ops(List) ->
	pair_infix_ops(List, []).

pair_infix_ops([], Acc) ->
	lists:reverse(Acc);
pair_infix_ops([Op, Expr | Tail], Acc) ->
	pair_infix_ops(Tail, [{Op, Expr} | Acc]).

%	TailElement = parse:series([opt_space(milang_p_atomic:infix()), opt_space(primary())]),
%	TailParser = parse:repeat_until_error(TailElement),
%	Series = parse:series([primary(), TailParser]),
%	Tagged = parse:tag(expression, Series),
%	Mapper = fun
%		({_, _, [Head, []]}) ->
%			Head;
%		({T, L, [Head, DirtyTail]}) ->
%			Tail = [{milang_ast:set_doc(OpDoc, Op), milang_ast:set_doc(EDoc, E)} || [OpDoc, Op, EDoc, E] <- DirtyTail],
%			milang_ast:ast_node(L, <<>>, T, #{ head => Head, infix_ops => Tail})
%	end,
%	parse:map(Tagged, Mapper).

-spec primary() -> parse:parser(term(), milang_ast:expression_primary()).
primary() ->
	parse:lazy(fun() ->
		parse:first_of(
			[ call()
			, literal()
			, match()
			, sub_expression()
			])
	end).

-spec sub_expression() -> parse:parser(term(), milang_ast:expression_parens()).
sub_expression() ->
	Series = parse:series([parse:character($(), opt_space(expression()), opt_space(parse:character($)))]),
	Mapper = fun([_OpenParen, E, _CloseParen]) ->
		E
	end,
	parse:set_tag(sub_expression, parse:map(Series, Mapper)).

-spec match() -> parse:parser(term(), milang_ast:expression_match()).
match() ->
	Series = parse:series([parse:string(<<"match">>), space(primary()), space(parse:string(<<"when">>)), opt_space(parse:character(${)), match_cases()]),
	Tagged = parse:tag(expression_match, Series),
	Mapper = fun({T, L, [_, Expression, _, _, Tests]}) ->
		milang_ast:ast_node(L, <<>>, T, #{ expression => Expression, tests => Tests})
	end,
	Mapped = parse:map(Tagged, Mapper),
	parse:set_tag(expression_match, Mapped).

match_cases() ->
	Series = parse:series([opt_space(parse:character($,)), opt_space(match_against()), opt_space(parse:string(<<"->">>)), opt_space(expression())]),
	TaggedSeries = parse:tag(match_test, Series),
	Mapper = fun({T, L, [_, Test, _, Expression]}) ->
		milang_ast:ast_node(L, <<>>, T, #{ test => Test, expression => Expression })
	end,
	Mapped = parse:map(TaggedSeries, Mapper),
	RepeatUntil = parse:repeat_until(Mapped, opt_space(parse:character($}))),
	RepeatMapper = fun({E, _}) ->
		E
	end,
	Final = parse:map(RepeatUntil, RepeatMapper),
	parse:set_tag(match_test, Final).

match_against() ->
	parse:first_of([ literal(), call(milang_p_atomic:type_name()), milang_p_atomic:variable()]).


-spec call() -> parse:parser(term(), milang_ast:expression_call()).
call() ->
	call(parse:first_of([ milang_p_atomic:function_name(), milang_p_atomic:type_name()])).

call(Call) ->
	ArgsElement = space(argument()),
	Args = parse:repeat_when(parse:success(ok), ArgsElement),
	Series = parse:series([Call, Args]),
	Tagged = parse:tag(expression_call, Series),
	Mapper = fun({T, L, [F, ArgsWithOks]}) ->
		CleanedArgs = [A || A <- ArgsWithOks, A =/= ok ],
		milang_ast:ast_node(L, <<>>, T, #{ name => F, args => CleanedArgs})
	end,
	parse:set_tag(call, parse:map(Tagged, Mapper)).

argument() ->
	parse:first_of([literal(), milang_p_atomic:function_name(), milang_p_atomic:type_name(), sub_expression()]).

-spec literal() -> parse:parser(term(), milang_ast:expression_literal()).
literal() ->
	parse:first_of(
		[ literal_list()
		, literal_map()
		, literal_record()
		, literal_string()
		, literal_float()
		, literal_integer()
		]).

literal_list() ->
	Untagged = milang_p_atomic:list(expression()),
	Tagged = parse:tag(literal_list, Untagged),
	Mapper = fun({T, L, Elements}) ->
		milang_ast:ast_node(L, <<>>, T, Elements)
	end,
	parse:set_tag(literal_list, parse:map(Tagged, Mapper)).

literal_map() ->
	parse:set_tag(literal_map, parse:first_of(
		[ map_key_access()
		, map_update()
		])).

map_key_access() ->
	Series = parse:series(
		[ parse:string(<<"{=">>)
		, opt_space(expression())
		, opt_space(parse:string(<<"=}">>))
		, opt_space(expression())
		]),
	Tagged = parse:tag(map_key_access, Series),
	Mapper = fun({T, L, [_, KeyExpr, _, MapExpre]}) ->
		milang_ast:ast_node(L, <<>>, T, #{ key => KeyExpr, map => MapExpre })
	end,
	parse:map(Tagged, Mapper).

map_update() ->
	ExistingMap = parse:series(
		[ opt_space(expression())
		, opt_space(parse:character($:))
		]),
	Series = parse:series(
		[ parse:string(<<"{=">>)
		, parse:optional(ExistingMap)
		, parse:repeat_until(opt_space(map_entry_update()), opt_space(parse:string(<<"=}">>)))
		]),
	Tagged = parse:tag(undefined, Series),
	Mapper = fun({undefined, L, ParsedEntries}) ->
		case ParsedEntries of
			[_, [], {Entries, _}] ->
				milang_ast:ast_node(L, <<>>, literal_map, Entries);
			[_, [[Map, _]], {Entries, _}] ->
				milang_ast:ast_node(L, <<>>, map_update, #{ map => Map, entries => Entries})
		end
	end,
	parse:map(Tagged, Mapper).

map_entry_update() ->
	SetEntry = parse:tag(set, parse:series(
		[ opt_space(expression())
		, opt_space(parse:character($=))
		, opt_space(expression())
		])),
	DelEntry = parse:tag(del, opt_space(expression())),
	Series = parse:series(
		[ parse:character($,)
		, parse:first_of([SetEntry, DelEntry])
		]),
	Tagged = parse:tag(undefined, Series),
	Mapper = fun({undefined, L, [_, ParsedEntry]}) ->
		case ParsedEntry of
			{set, _, [KeyExpr, _, ValueExpr]} ->
				milang_ast:ast_node(L, <<>>, map_set, #{ key => KeyExpr, value => ValueExpr});
			{del, _, Expr} ->
				milang_ast:ast_node(L, <<>>, map_del, Expr)
		end
	end,
	parse:map(Tagged, Mapper).

literal_record() ->
	Entry = opt_space(record_field()),
	Entries = parse:repeat_until(Entry, opt_space(parse:character($}))),
	Series = parse:series([parse:character(${), Entries]),
	Tagged = parse:tag(literal_record, Series),
	Mapper = fun({T, L, [_, {ParsedFields, _}]}) ->
		milang_ast:ast_node(L, <<>>, T, ParsedFields)
	end,
	parse:set_tag(literal_record, parse:map(Tagged, Mapper)).

record_field() ->
	Field = parse:series([parse:character($,), opt_space(record_field_key()), opt_space(parse:character($=)), opt_space(expression())]),
	Tagged = parse:tag(literal_record_field, Field),
	Mapper = fun({T, L, [_, Key, _Equals, Value]}) ->
		Data = { Key, Value},
		milang_ast:ast_node(L, <<>>, T, Data)
	end,
	parse:map(Tagged, Mapper).

record_field_key() ->
	NumberRegex = parse:regex("[1-9][0-9]*"),
	Mapper = fun(Binary) ->
		binary_to_integer(Binary)
	end,
	NumberIdx = parse:map(NumberRegex, Mapper),
	NameIdx = parse:map(milang_p_atomic:downcase_name(), fun(N) -> binary_to_atom(N, utf8) end),
	parse:first_of([ NameIdx, NumberIdx ]).

literal_string() ->
	% I apologize for all the slashes.
	% The original does not need to escape the double quotes. Nor did it need to
	% escape the backslashes from another parser before it could get to the
	% regex parser.
	% from https://stackoverflow.com/questions/249791/regex-for-quoted-string-with-escaping-quotes:
	%     /"(?:[^"\\]|\\.)*"
	% I've also extended it so it captures the inner string.
	Regex = <<"\"((?:[^\"\\\\]|\\\\.)*)\"">>,
	ParsedRegex = parse:regex(Regex),
	Tagged = parse:tag(literal_string, ParsedRegex),
	Mapper = fun({T, L, [_, V]}) ->
		milang_ast:ast_node(L, <<>>, T, V)
	end,
	parse:map(Tagged, Mapper).

literal_float() ->
	Sign = parse:optional(parse:first_of([parse:character($-), parse:character($+)])),
	NumberRegex = parse:regex("[0-9]+"),
	Series = parse:series([Sign, NumberRegex, parse:character($.), NumberRegex]),
	Tagged = parse:tag(literal_float, Series),
	Mapper = fun({T, L, Result}) ->
		Combined = unicode:characters_to_binary(Result),
		Value = binary_to_float(Combined),
		milang_ast:ast_node(L, <<>>, T, Value)
	end,
	parse:map(Tagged, Mapper).

literal_integer() ->
	Sign = parse:optional(parse:first_of([parse:character($-), parse:character($+)])),
	NumberRegex = parse:regex("[0-9]+"),
	Series = parse:series([Sign, NumberRegex]),
	Tagged = parse:tag(literal_integer, Series),
	Mapper = fun({T, L, Result}) ->
		Combined = unicode:characters_to_binary(Result),
		Value = binary_to_integer(Combined),
		milang_ast:ast_node(L, <<>>, T, Value)
	end,
	parse:map(Tagged, Mapper).

opt_space(AfterSpace) ->
	milang_p_atomic:space_opt_then(AfterSpace).

space(AfterSpace) ->
	milang_p_atomic:space_then(AfterSpace).
