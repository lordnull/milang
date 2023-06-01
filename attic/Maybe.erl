-module('Maybe').

-export(
	[ 'Nothing'/0
	, 'Some'/0
	, 'Some'/1
	, map/0
	, and_then/0
	, with_default/2
	, and_then_n/2
	, map_n/2
	]).

-type maybe(A) :: undefined | {some, A}.
-export_type([maybe/1]).

'Nothing'() ->
	undefined.

'Some'() ->
	milang_curry:stack(fun 'Some'/1).

'Some'(E) ->
	{some, E}.

map() ->
	milang_curry:stack(fun map/2).

map(_Mapper, undefined) ->
	undefined;
map(Mapper, {some, E}) ->
	'Some'(Mapper(E)).

and_then() ->
	milang_curry:stack(fun and_then/2).

and_then(_AndThen, undefined) ->
	undefined;
and_then(AndThen, {some, E}) ->
	AndThen(E).

with_default(WhenNothing, undefined) ->
	WhenNothing;
with_default(_WhenNothing, {ok, V}) ->
	V.

map_list(Mapper, Maybes) ->
	Acc = lists:foldl(fun
		(undefined, _Acc) ->
			undefined;
		({ok, V}, Acc) ->
			map(fun(InAcc) -> [Mapper(V) | InAcc] end, Acc)
	end, 'Some'([]), Maybes),
	map(fun lists:reverse/1, Acc).

map_n(Maybes, Mapper) ->
	MaybesLength = length(Maybes),
	case erlang:fun_info(Mapper, arity) of
		{arity, MaybesLength} ->
			Flattened = map_list(fun(E) -> E end, Maybes),
			map(fun(Args) -> erlang:apply(Mapper, Args) end, Flattened);
		_ ->
			error(badarg)
	end.

and_then_n(Maybes, Next) ->
	MaybesLength = length(Maybes),
	case erlang:fun_info(Next, arity) of
		{arity, MaybesLength} ->
			Flattened = map_list(fun(E) -> E end, Maybes),
			and_then(fun(Args) -> erlang:apply(Next, Args) end, Flattened);
		_ ->
			error(badarg)
	end.
