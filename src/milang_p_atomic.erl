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
	, module_name_prefix/0
	, variable/0
	, infix/0
	, infix_symbol/0
	, function_name/0
	, function_name_symbol/0
	, function_name_local/0
	, list/1
	, record/2
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
			{T, L, Key, Value}
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
	parse:repeat(1, infinity, parse:first_of([whitespace(), comment_line()])).

space_opt() ->
	parse:optional(space()).

whitespace() ->
	parse:regex("\\s+").

comment_line() ->
	Series = parse:series([parse:string(<<"-doc ">>), parse:regex("[^\\n]*", first)]),
	Mapper = fun([_, [String]]) ->
		String
	end,
	Mapped = parse:map(Series, Mapper),
	parse:tag(docstring, Mapped).

type_name() ->
	parse:first_of([ type_name_remote(), type_name_local() ]).

type_name_remote() ->
	Series = parse:series([module_name_prefix(), upcase_name()]),
	Tagged = parse:tag(type_name_remote, Series),
	Mapper = fun({T, L, [M, N]}) ->
		{T, L, M, N}
	end,
	parse:map(Tagged, Mapper).

type_name_local() ->
	parse:tag(type_name_local, upcase_name()).

module_name_prefix() ->
	Element = parse:series([upcase_name(), parse:character($.), parse:peek(".")]),
	Repeat = parse:repeat(1, infinity, Element),
	Mapper = fun(ElementsWithDots) ->
		Elements = [E || [E, _Dot, _] <- ElementsWithDots],
		Joined = lists:join($., Elements),
		unicode:characters_to_binary(Joined)
	end,
	Mapped = parse:map(Repeat, Mapper),
	parse:tag(module_name, Mapped).

variable() ->
	parse:tag(variable, downcase_name()).

infix() ->
	parse:first_of([ infix_notation(), infix_symbol() ]).

infix_notation() ->
	parse:first_of([infix_left_assoc(), infix_right_assoc()]).

infix_left_assoc() ->
	Weight = parse:repeat(1, infinity, parse:character($|)),
	Arrow = parse:character($>),
	Function = parse:first_of([function_name(), infix_symbol()]),
	Sequence = parse:series([Weight, Arrow, Function]),
	Tagged = parse:tag(left_assoc, Sequence),
	Mapper = fun({T, L, [WeightChars, _, FunctionAst]}) ->
		{T, L, length(WeightChars), FunctionAst}
	end,
	parse:map(Tagged, Mapper).

infix_right_assoc() ->
	Arrow = parse:character($<),
	Weight = parse:repeat(1, infinity, parse:character($|)),
	Function = parse:first_of([function_name(), infix_symbol()]),
	Sequence = parse:series([Arrow, Weight, Function]),
	Tagged = parse:tag(right_assoc, Sequence),
	Mapper = fun({T, L, [_Arrow, WeightChars, FunctionAst]}) ->
		{T, L, length(WeightChars), FunctionAst}
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
	parse:tag(infix_symbol, ValueCheckP).

function_name() ->
	parse:first_of([ function_name_local(), function_name_remote(), function_name_symbol()]).

function_name_local() ->
	parse:tag(function_name_local, downcase_name()).

function_name_remote() ->
	Series = parse:series([module_name_prefix(), downcase_name()]),
	Tag = parse:tag(function_name_remote, Series),
	Mapper = fun({T, L, [M, F]}) ->
		{T, L, M, F}
	end,
	parse:map(Tag, Mapper).

function_name_symbol() ->
	Series = parse:series([parse:character($'), infix_symbol(), parse:character($')]),
	Mapper = fun([_, SymbolAST, _]) ->
		{infix_symbol, _, Symbol} = SymbolAST,
		Symbol
	end,
	Map = parse:map(Series, Mapper),
	parse:tag(function_name_symbol, Map).
