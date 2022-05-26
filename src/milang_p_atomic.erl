-module(milang_p_atomic).

-export(
	[ parse/1
	]).
-export(
	[ downcase_name/0
	, upcase_name/0
	, space/0
	, whitespace/0
	, comment_line/0
	, type_name/0
	, type_name_local/0
	, type_name_remote/0
	, module_name_prefix/0
	, variable/0
	, infix/0
	, infix_symbol/0
	, function_name/0
	, function_name_symbol/0
	, function_name_local/0
	, list/1
	, record/2
	, parens/1
	]).

parse(Subject) ->
	Parser = parse:first_of(
		[ space()
		, function_name()
		, type_name()
		, infix()
		, variable()
		, module_name_prefix()
		]),
	parse:it(Subject, Parser).

parens(Parser) ->
	parse:lazy(fun() ->
		SeriesP = parse:series([parse:character($(), space_opt(), Parser, space_opt(), parse:character($))]),
		parse:map(SeriesP, fun([_, _, Parsed, _, _]) ->
			Parsed
		end)
	end).

list(ElementParser) ->
	ElementP = parse:lazy(fun() ->
		parse:series([space_opt(), parse:character($,), space_opt(), ElementParser])
	end),
	ListP = parse:repeat_until_error(ElementP),
	SeriesP = parse:series([parse:character($[), ListP, space_opt(), parse:character($])]),
	Mapper = fun([_, DirtyElements, _, _]) ->
		[ E || [_, _, _, E] <- DirtyElements]
	end,
	parse:map(SeriesP, Mapper).

record(KeyParser, ValueParser) ->
	ElementP = parse:lazy(fun() ->
		ESeriesP = parse:series([space_opt(), parse:character($,), space_opt(), KeyParser, space_opt(), parse:character($=), space_opt(), ValueParser]),
		ETagged = parse:tag(record_field, ESeriesP),
		EMapper = fun({T, L, Node}) ->
			[_, _Comma, _, Key, _, _Equals, _, Value] = Node,
			milang_ast:ast_node(L, <<>>, T, #{ key => Key, value => Value })
		end,
		parse:map(ETagged, EMapper)
	end),
	ElementsP = parse:repeat_until_error(ElementP),
	SeriesP = parse:series([parse:character(${), ElementsP, space_opt(), parse:character($})]),
	Mapper = fun([_, Elements, _, _]) ->
		Elements
	end,
	parse:map(SeriesP, Mapper).

downcase_name() ->
	RegEx = parse:regex("\\p{Ll}\\w*", first),
	parse:map(RegEx, fun([V]) -> V end).

upcase_name() ->
	RegEx = parse:regex("\\p{Lu}\\w*", first),
	parse:map(RegEx, fun([V]) -> V end).

space() ->
	SeriesP = parse:repeat(1, infinity, parse:first_of([whitespace(), comment_line()])),
	Mapper = fun(Series) ->
		Joined = lists:join($\n, Series),
		AsBinary = unicode:characters_to_binary(Joined),
		string:trim(AsBinary)
	end,
	parse:map(SeriesP, Mapper).

space_opt() ->
	parse:optional(space()).

whitespace() ->
	parse:regex("\\s+").

comment_line() ->
	Series = parse:series([parse:string(<<"-doc">>), parse:regex("[ \\t]+"), parse:regex("[^\\n]*", first)]),
	Mapper = fun([_, _, [String]]) ->
		String
	end,
	parse:map(Series, Mapper).

type_name() ->
	parse:first_of([ type_name_remote(), type_name_local() ]).

type_name_remote() ->
	Series = parse:series([module_name_prefix(), upcase_name()]),
	Tagged = parse:tag(type_name_remote, Series),
	Mapper = fun({T, L, [M, N]}) ->
		Data = #{ name => binary_to_atom(N, utf8), module => binary_to_atom(M, utf8)},
		milang_ast:ast_node(L, <<>>, T, Data)
	end,
	parse:map(Tagged, Mapper).

type_name_local() ->
	Tagged = parse:tag(type_name_local, upcase_name()),
	parse:map(Tagged, fun({T, L, N}) ->
		milang_ast:ast_node(L, <<>>, T, binary_to_atom(N, utf8))
	end).

module_name_prefix() ->
	Element = parse:series([upcase_name(), parse:character($.), parse:peek(".")]),
	Repeat = parse:repeat(1, infinity, Element),
	Mapper = fun(ElementsWithDots) ->
		Elements = [E || [E, _Dot, _] <- ElementsWithDots],
		Joined = lists:join($., Elements),
		unicode:characters_to_binary(Joined)
	end,
	parse:map(Repeat, Mapper).

variable() ->
	RegEx = parse:regex("[\\p{Ll}_]\\w*", first),
	Tagged = parse:tag(variable, RegEx),
	Mapper = fun({T, L, [V]}) ->
		milang_ast:ast_node(L, <<>>, T, binary_to_atom(V))
	end,
	parse:map(Tagged, Mapper).

infix() ->
	parse:first_of([ infix_notation(), infix_symbol() ]).

infix_notation() ->
	WeightAndAssoc = parse:regex(<<"«+|»+"/utf8>>, first),
	Function = parse:first_of([function_name(), infix_symbol()]),
	Sequence = parse:series([WeightAndAssoc, Function]),
	Tagged = parse:tag(infix_notation, Sequence),
	Mapper = fun({T, L, [[WeightAndAssocChars], FunctionAst]}) ->
		<<ArrowChar/utf8, _/binary>> = WeightAndAssocChars,
		Weight = round(size(WeightAndAssocChars) / 2), % the « and » are each 2 bytes.
		Assoc = case ArrowChar of
			$« ->
				right;
			$» ->
				left
		end,
		Data = #{ assoc => Assoc, weight => Weight, function => FunctionAst},
		milang_ast:ast_node(L, <<>>, T, Data)
	end,
	parse:map(Tagged, Mapper).

infix_symbol() ->
	DisallowedSet = ordsets:from_list("'\"[]{}(),"),
	AllowedRegexTest = "[\\pS\\pP]",
	{ok, Compiled} = re:compile(AllowedRegexTest, [unicode, ucp]),
	ChompTest = fun(C) ->
		case ordsets:is_element(C, DisallowedSet) of
			true ->
				false;
			false ->
				case re:run([C], Compiled) of
					{match, _} ->
						true;
					nomatch ->
						false
				end
		end
	end,
	ChompP = parse:chomp_while(ChompTest),
	ValueCheckP = parse:unless(ChompP, fun
		(<<"=">>) ->
			{error, solo_equals_not_allowed};
		(<<".">>) ->
			{error, solo_dot_not_allowed};
		(_Passing) ->
			ok
	end),
	Tagged = parse:tag(infix_symbol, ValueCheckP),
	Mapped = fun({T, L, B}) ->
		milang_ast:ast_node(L, <<>>, T, binary_to_atom(B, utf8))
	end,
	parse:map(Tagged, Mapped).

function_name() ->
	parse:first_of([ function_name_local(), function_name_remote(), function_name_symbol()]).

function_name_local() ->
	Tagged = parse:tag(function_name_local, downcase_name()),
	Mapper = fun({T, L, N}) ->
		milang_ast:ast_node(L, <<>>, T, binary_to_atom(N, utf8))
	end,
	parse:map(Tagged, Mapper).

function_name_remote() ->
	LocalNameRaw = parse:first_of([downcase_name(), function_name_symbol()]),
	LocalNamePart = parse:map(LocalNameRaw, fun(Node) ->
		if
			is_binary(Node) ->
				binary_to_atom(Node, utf8);
			is_tuple(Node) ->
				milang_ast:data(Node)
		end
	end),
	Series = parse:series([module_name_prefix(), LocalNamePart]),
	Tag = parse:tag(function_name_remote, Series),
	Mapper = fun({T, L, [M, F]}) ->
		Data = #{ name => F, module => binary_to_atom(M) },
		milang_ast:ast_node(L, <<>>, T, Data)
	end,
	parse:map(Tag, Mapper).

function_name_symbol() ->
	Series = parse:series([parse:character($'), infix_symbol(), parse:character($')]),
	Tagged = parse:tag(function_name_symbol, Series),
	Mapper = fun({T, L, [_, SymbolAST, _]}) ->
		#{ data := Symbol } = milang_ast:to_map(SymbolAST),
		milang_ast:ast_node(L, <<>>, T, Symbol)
	end,
	parse:map(Tagged, Mapper).
