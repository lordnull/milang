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

expression() ->
	TailElement = parse:series([opt_space(), milang_p_atomic:infix(), opt_space(), primary()]),
	TailParser = parse:repeat_until_error(TailElement),
	Series = parse:series([primary(), TailParser]),
	Tagged = parse:tag(expression, Series),
	Mapper = fun
		({_, _, [Head, []]}) ->
			Head;
		({T, L, [Head, DirtyTail]}) ->
			Tail = [{Op, E} || [_, Op, _, E] <- DirtyTail],
			{T, L, Head, Tail}
	end,
	parse:map(Tagged, Mapper).

primary() ->
	parse:lazy(fun() ->
		parse:first_of(
			[ call()
			, literal()
			, sub_expression()
			])
	end).

sub_expression() ->
	Series = parse:series([parse:character($(), opt_space(), expression(), opt_space(), parse:character($))]),
	Mapper = fun([_, _, E, _, _]) ->
		E
	end,
	parse:map(Series, Mapper).

call() ->
	Call = parse:first_of([ milang_p_atomic:function_name(), milang_p_atomic:type_name()]),
	ArgsElement = parse:series([space(), argument()]),
	Args = parse:repeat_until_error(ArgsElement),
	Series = parse:series([Call, Args]),
	Tagged = parse:tag(call, Series),
	Mapper = fun({T, L, [F, ArgsWithSpaces]}) ->
		CleanedArgs = [A || [_, A] <- ArgsWithSpaces],
		{T, L, F, CleanedArgs}
	end,
	parse:map(Tagged, Mapper).

argument() ->
	parse:first_of([literal(), milang_p_atomic:function_name(), milang_p_atomic:type_name(), sub_expression()]).

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
	Element = parse:series([opt_space(), list_element()]),
	Elements = parse:repeat_until_error(Element),
	Series = parse:series([parse:character($[), Elements, opt_space(), parse:character($])]),
	Mapper = fun([_, ElementsWithSpaces, _, _]) ->
		[ E || [_, E] <- ElementsWithSpaces]
	end,
	Mapped = parse:map(Series, Mapper),
	parse:tag(literal_list, Mapped).

list_element() ->
	Series = parse:series([parse:character($,), opt_space(), expression()]),
	Mapper = fun([_, _, E]) ->
		E
	end,
	parse:map(Series, Mapper).

literal_map() ->
	Entry = parse:series([opt_space(), map_entry()]),
	Entries = parse:repeat_until_error(Entry),
	Series = parse:series([parse:string(<<"#{">>), opt_space(), Entries, opt_space(), parse:string(<<"}#">>)]),
	Mapper = fun([_, _, DirtyEntries, _, _]) ->
		[ E || [_, E] <- DirtyEntries]
	end,
	Mapped = parse:map(Series, Mapper),
	parse:tag(literal_map, Mapped).

map_entry() ->
	Series = parse:series([parse:character($,), opt_space(), expression(), opt_space(), parse:character($=), opt_space(), expression()]),
	Tagged = parse:tag(map_entry, Series),
	Mapper = fun({T, L, [_, _, Key, _, _, _, Value]}) ->
		{T, L, Key, Value}
	end,
	parse:map(Tagged, Mapper).

literal_record() ->
	Entry = parse:series([opt_space(), record_field()]),
	Entries = parse:repeat_until_error(Entry),
	Series = parse:series([parse:character(${), opt_space(), Entries, opt_space(), parse:character($})]),
	Mapper = fun([_, _, DirtyFields, _, _]) ->
		[ E || [_, E] <- DirtyFields]
	end,
	Mapped = parse:map(Series, Mapper),
	parse:tag(literal_record, Mapped).

record_field() ->
	Field = parse:series([parse:character($,), opt_space(), record_field_key(), opt_space(), parse:character($=), opt_space(), expression()]),
	Tagged = parse:tag(record_field, Field),
	Mapper = fun({T, L, [_, _, Key, _, _, _, Value]}) ->
		{T, L, Key, Value}
	end,
	parse:map(Tagged, Mapper).

record_field_key() ->
	NumberRegex = parse:regex("[1-9][0-9]*"),
	Mapper = fun(Binary) ->
		{number, binary_to_integer(Binary)}
	end,
	NumberIdx = parse:map(NumberRegex, Mapper),
	NameIdx = parse:map(milang_p_atomic:downcase_name(), fun(N) -> {string, N} end),
	parse:first_of([ NameIdx, NumberIdx ]).

literal_string() ->
	% I appologize for all the slashes.
	% The original does not need to escape the double quotes. Nor did it need to
	% escape the backslashes from another parser before it could get to the
	% regex parser.
	% from https://stackoverflow.com/questions/249791/regex-for-quoted-string-with-escaping-quotes:
	%     /"(?:[^"\\]|\\.)*"
	% I've also extended it so it captures the inner string.
	Regex = <<"\"((?:[^\"\\\\]|\\\\.)*)\"">>,
	ParsedRegex = parse:regex(Regex),
	Mapper = fun([_, V]) ->
		io:format("the regex result? ~p~n", [V]),
		V
	end,
	Mapped = parse:map(ParsedRegex, Mapper),
	parse:tag(literal_string, Mapped).
	%parse:tag(literal_string, ParsedRegex).

literal_float() ->
	Sign = parse:optional(parse:first_of([parse:character($-), parse:character($+)])),
	NumberRegex = parse:regex("[0-9]+"),
	Series = parse:series([Sign, NumberRegex, parse:character($.), NumberRegex]),
	Mapper = fun(Result) ->
		Combined = unicode:characters_to_binary(Result),
		binary_to_float(Combined)
	end,
	Mapped = parse:map(Series, Mapper),
	parse:tag(literal_float, Mapped).

literal_integer() ->
	Sign = parse:optional(parse:first_of([parse:character($-), parse:character($+)])),
	NumberRegex = parse:regex("[0-9]+"),
	Series = parse:series([Sign, NumberRegex]),
	Mapper = fun(Result) ->
		Combined = unicode:characters_to_binary(Result),
		binary_to_integer(Combined)
	end,
	Mapped = parse:map(Series, Mapper),
	parse:tag(literal_integer, Mapped).

opt_space() ->
	parse:optional(milang_p_atomic:space()).

space() ->
	milang_p_atomic:space().
