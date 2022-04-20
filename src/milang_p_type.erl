-module(milang_p_type).

-export([parse/1]).
-export(
	[ top/0
	, primary/0
	, concrete/0
	]).

parse(Subject) ->
	parse:it(Subject, top()).

space() ->
	milang_p_atomic:space().

space_opt() ->
	parse:optional(space()).

downcase_name() ->
	milang_p_atomic:downcase_name().

top() ->
	HeadP = primary(),
	TailElementP = parse:lazy(fun() ->
		parse:series([space_opt(), parse:string(<<"->">>), space_opt(), primary()])
	end),
	TailP = parse:repeat_until_error(TailElementP),
	SeriesP = parse:series([HeadP, TailP]),
	Tagged = parse:tag(type_function, SeriesP),
	Mapper = fun({_T, _L, [Head, []]}) ->
		Head;
		({T, L, [FirstArg, DirtyArgTail]}) ->
			CleanTail = [A || [_, _, _, A] <- DirtyArgTail],
			ArgsWithReturn = [FirstArg | CleanTail],
			[Return | ReversedArgs] = lists:reverse(ArgsWithReturn),
			Args = lists:reverse(ReversedArgs),
			{T, L, Args, Return }
	end,
	parse:map(Tagged, Mapper).

primary() ->
	parse:first_of(
		[parse:lazy(fun concrete/0)
		,parse:lazy(fun variable/0)
		,parse:lazy(fun sub/0)
		]).

concrete() ->
	ArgElementP = parse:series([space(), type_arg()]),
	ArgsP = parse:repeat_until_error(ArgElementP),
	SeriesP = parse:series([milang_p_atomic:type_name(), ArgsP]),
	Tagged = parse:tag(type_concrete, SeriesP),
	Mapper = fun({T, L, Series}) ->
		[BaseName, ArgsWithSpaces] = Series,
		Args = [Arg || [_, Arg] <- ArgsWithSpaces],
		{T, L, BaseName, Args}
	end,
	parse:map(Tagged, Mapper).

variable() ->
	parse:tag(type_variable, downcase_name()).

sub() ->
	SeriesP = parse:series([parse:character($(), space_opt(), top(), space_opt(), parse:character($))]),
	Mapper = fun([_, _, T, _, _]) ->
		T
	end,
	parse:map(SeriesP, Mapper).

type_arg() ->
	FirstOf = parse:first_of(
		[ milang_p_atomic:type_name()
		, variable()
		, sub()
		]),
	Mapper = fun(Node) ->
		case Node of
			{type_name_local, L, _} ->
				{type_concrete, L, Node, []};
			{type_name_remote, L, _, _} ->
				{type_concrete, L, Node, []};
			_ ->
				Node
		end
	end,
	parse:map(FirstOf, Mapper).
