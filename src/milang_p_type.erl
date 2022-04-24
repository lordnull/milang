-module(milang_p_type).

-export([parse/1]).
-export(
	[ data/0
	, function/0
	, record/0
	, concrete/0
	]).

parse(Subject) ->
	TopP = parse:first_of([concrete(), function(), record(), data()]),
	Recusive = milang_p_atomic:parens(parse:lazy(fun() -> TopP end)),
	Parse = parse:first_of([TopP, Recusive]),
	parse:it(Subject, Parse).

space() ->
	milang_p_atomic:space().

space_opt() ->
	parse:optional(space()).

data() ->
	TailElementP = parse:map(parse:series([space(), data_arg()]), fun([_, A]) ->
		A
	end),
	TailP = parse:repeat_until_error(TailElementP),
	HeadP = milang_p_atomic:type_name(),
	SeriesP = parse:series([HeadP, TailP]),
	Tagged = parse:tag(type_data, SeriesP),
	parse:map(Tagged, fun({T, L, [H, Args]}) ->
		{T, L, H, Args}
	end).

data_arg() ->
	parse:first_of(
		[ milang_p_atomic:type_name()
		, milang_p_atomic:variable()
		, record()
		, milang_p_atomic:parens(concrete())
		]).

function() ->
	DirtyTailElementP = parse:series([space_opt(), parse:string(<<"->">>), space_opt(), function_arg()]),
	TailElementP = parse:map(DirtyTailElementP, fun([_, _Arrow, _, Arg]) ->
		Arg
	end),
	TailP = parse:repeat_until_error(TailElementP),
	SeriesP = parse:series([function_arg(), TailP]),
	Tagged = parse:tag(type_function, SeriesP),
	Merged = parse:map(Tagged, fun({T, L, [Head, Tail]}) ->
		{T, L, [Head | Tail]}
	end),
	parse:map(Merged, fun
		({_T, _L, [OnlyArg]}) -> OnlyArg;
		(E) -> E
	end).

function_arg() ->
	parse:first_of(
		[ data()
		, record()
		, milang_p_atomic:parens(concrete())
		, milang_p_atomic:variable()
		]).

record() ->
	RecordP = milang_p_atomic:record(parse:first_of([milang_p_atomic:downcase_name(), parse:regex("[1-9][0-9]*")]), concrete()),
	parse:tag(type_record, RecordP).

concrete() ->
	parse:first_of(
		[ parse:lazy(fun function/0)
		, parse:lazy(fun record/0)
		, parse:lazy(fun data/0)
		]).
