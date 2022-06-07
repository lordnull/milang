-module(milang_p_expression).

-export([parse/1]).
-export(
	[ expression/0
	, primary/0
	, sub_expression/0
	, call/0
	, argument/0
	, literal/0
	, record_field_key/0
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
	parse:map(Tagged, Mapper).

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
			, sub_expression()
			])
	end).

-spec sub_expression() -> parse:parser(term(), milang_ast:expression_parens()).
sub_expression() ->
	Series = parse:series([parse:character($(), opt_space(expression()), opt_space(parse:character($)))]),
	Mapper = fun([_OpenParen, E, _CloseParen]) ->
		E
	end,
	parse:map(Series, Mapper).

-spec call() -> parse:parser(term(), milang_ast:expression_call()).
call() ->
	Call = parse:first_of([ milang_p_atomic:function_name(), milang_p_atomic:type_name()]),
	ArgsElement = space(argument()),
	Args = parse:repeat_when(parse:success(ok), ArgsElement),
	Series = parse:series([Call, Args]),
	Tagged = parse:tag(expression_call, Series),
	Mapper = fun({T, L, [F, ArgsWithOks]}) ->
		CleanedArgs = [A || A <- ArgsWithOks, A =/= ok ],
		milang_ast:ast_node(L, <<>>, T, #{ name => F, args => CleanedArgs})
	end,
	parse:map(Tagged, Mapper).

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
	Element = opt_space(list_element()),
	Elements = parse:repeat_until(Element, opt_space(parse:character($]))),
	Series = parse:series([parse:character($[), Elements]),
	Tagged = parse:tag(literal_list, Series),
	Mapper = fun({T, L, [_, {ParsedElements, _}]}) ->
		milang_ast:ast_node(L, <<>>, T, ParsedElements)
	end,
	parse:map(Tagged, Mapper).

list_element() ->
	Series = parse:series([parse:character($,), opt_space(expression())]),
	Mapper = fun([_, E]) ->
		E
	end,
	parse:map(Series, Mapper).

literal_map() ->
	Entry = opt_space(map_entry()),
	Entries = parse:repeat_until(Entry, opt_space(parse:string(<<"}#">>))),
	Series = parse:series([parse:string(<<"#{">>), Entries]),
	Tagged = parse:tag(literal_map, Series),
	Mapper = fun({T, L, [_, {ParsedEntries, _}]}) ->
		milang_ast:ast_node(L, <<>>, T, ParsedEntries)
	end,
	parse:map(Tagged, Mapper).

map_entry() ->
	Series = parse:series([parse:character($,), opt_space(expression()), opt_space(parse:character($=)), opt_space(expression())]),
	Tagged = parse:tag(literal_map_entry, Series),
	Mapper = fun({T, L, [_, Key, _Equals, Value]}) ->
		Data = #{ key => Key, value => Value },
		milang_ast:ast_node(L, <<>>, T, Data)
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
	parse:map(Tagged, Mapper).

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
