%%% @doc Like the maybe module, this takes a common erlang pattern and
%%% formalizes it.
%%% @end
-module(result).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type result(Err, Ok) :: {error, Err} | {ok, Ok}.

-export_type(
	[ result/2
	]).

-export(
	[ err/1
	, ok/1
	, map/2
	, map_ok/2
	, map_err/2
	, and_then/2
	, try_recover/2
	, with_default/2
	, to_maybe/1
	, from_maybe/2
	, map_n/2
	, and_then_n/2
	, and_then_all/2
	]).

%% @doc A pessimistic function the never succeedes.
%% @end
-spec err(X) -> result(X, none()).
err(Err) -> {error, Err}.

%% @doc An optimistic function that never fails.
-spec ok(A) -> result(none(), A).
ok(Ok) -> {ok, Ok}.

%% @doc An alias for map_ok/1.
%% @end.
%% @end
-spec map(fun((A) -> B), result(Err, A)) -> result(Err, B).
map(Mapper, Result) ->
	map_ok(Mapper, Result).

%% @doc Take a result, and if happy, make it a different kind of happy.
%% @end
-spec map_ok(fun((A) -> B), result(Err, A)) -> result(Err, B).
map_ok(_Mapper, {error, _} = Result) ->
	Result;
map_ok(Mapper, {ok, Ok}) ->
	{ok, Mapper(Ok)}.


%% @doc Take a result and if bad, make it a different kind of bad.
%% @end
-spec map_err(fun((ErrA) -> ErrB), result(ErrA, T)) -> result(ErrB, T).
map_err(_Mapper, {ok, _} = Result) ->
	Result;
map_err(Mapper, {error, Err}) ->
	{error, Mapper(Err)}.

-ifdef(TEST).

map_test_() ->
	[ fun() ->
		{ok, 1} = map_ok(fun(N) -> N + 1 end, ok(0))
	end
	, fun() ->
		{error, 1} = map_ok(fun(_) -> 5 end, err(1))
	end
	, fun() ->
		{error, 2} = map_err(fun(N) -> N + 1 end, err(1))
	end
	, fun() ->
		{ok, 1} = map_err(fun(N) -> N + 1 end, ok(1))
	end
	].

-endif.

%% @doc Take result and if good, create a whole new result.
%% @end
-spec and_then(fun((A) -> result(Err, B)), result(Err, A)) -> result(Err, B).
and_then(DoNext, {ok, Ok}) ->
	DoNext(Ok);
and_then(_DoNext, {error, _} = Result) ->
	Result.

%% @doc Take a result and if bad, create a whole new result.
%% @end
-spec try_recover(fun((ErrA) -> result(ErrB, T)), result(ErrA, T)) -> result(ErrB, T).
try_recover(Recovery, {error, Error}) ->
	Recovery(Error);
try_recover(_Recovery, {ok, _} = Result) ->
	Result.

%% @doc Take a result, and if bad, use the lazy to create a new value, otherwise
%% used extract the value from the good result.
-spec with_default(lazy:lazy(T), result(term(), T)) -> T.
with_default(Lazy, {error, _}) ->
	Lazy();
with_default(_Lazy, {ok, Result}) ->
	Result.

-ifdef(TEST).

continue_test_() ->
	[ fun() ->
		{ok, 2} = and_then(fun(N) -> ok(N + 1) end, ok(1))
	end
	, fun() ->
		{error, 1} = and_then(fun(N) -> ok(N + 1) end, err(1))
	end
	, fun() ->
		{error, 2} = and_then(fun(N) -> err(N + 1) end, ok(1))
	end
	, fun() ->
		{ok, 2} = try_recover(fun(N) -> ok(N + 1) end, err(1))
	end
	, fun() ->
		{error, 2} = try_recover(fun(N) -> err(N + 1) end, err(1))
	end
	, fun() ->
		{ok, 1} = try_recover(fun(N) -> ok(N + 1) end, ok(1))
	end
	, fun() ->
		1 = with_default(fun() -> 2 end, ok(1))
	end
	, fun() ->
		2 = with_default(fun() -> 2 end, err(1))
	end
	].

-endif.

%% @doc Convert a result to a maybe where a bad result is nothing, but a good
%% result is something.
%% @end
-spec to_maybe(result(term(), T)) -> maybe:maybe(T).
to_maybe({error, _}) ->
	'undefined';
to_maybe({ok, V}) ->
	maybe:some(V).

%% @doc Convert a maybe to a result, using the given error if it was nothing for
%% a bad result.
%% @end
-spec from_maybe(Err, maybe:maybe(T)) -> result(Err, T).
from_maybe(IfNothing, undefined) ->
	{error, IfNothing};
from_maybe(_IfNothing, {some, V}) ->
	{ok, V}.

%% @doc Given a function and list of lazys that produce results, if all the
%% lazy's resolve to a good result, apply those good results to the
%% function.
%% @end
-spec map_n(fun((...) -> T), [ lazy:lazy(result(Err, term())) ]) -> result(Err, T).
map_n(Mapper, Lazys) ->
	Partial = partial:func(Mapper),
	map_n_loop(Partial, Lazys).

map_n_loop(Partial, []) ->
	{ok, lazy:resolve(Partial)};
map_n_loop(Partial, [ Lazy | LazyTail]) ->
	case Lazy() of
		{ok, V} ->
			map_n_loop(Partial(V), LazyTail);
		{error, _} = Error ->
			Error
	end.

-ifdef(TEST).

map_n_test_() ->
	[ fun() ->
		{ok, 1} = map_n(fun() -> 1 end, [])
	end
	, fun() ->
		{ok, 2} = map_n(fun(A, B) -> A + B end, [fun() -> ok(1) end, fun() -> ok(1) end])
	end
	, fun() ->
		{error, 1} = map_n(fun(_, _) -> never end, [fun() -> err(1) end, fun() -> ok(1) end])
	end
	, fun() ->
		{error, 2} = map_n(fun(_, _) -> never end, [fun() -> ok(1) end, fun() -> err(2) end])
	end
	].

-endif.

and_then_n(AndThenner, Lazys) ->
	Partial = partial:func(AndThenner),
	and_then_n_loop(Partial, Lazys).

and_then_n_loop(Partial, []) ->
	lazy:resolve(Partial);
and_then_n_loop(Partial, [Lazy | LazyTail]) ->
	case Lazy() of
		{ok, V} ->
			and_then_n_loop(Partial(V), LazyTail);
		{error, _} = Error ->
			Error
	end.

and_then_all(AndThen, Elements) ->
	Reversed = lists:foldl(fun(Element, Acc) ->
		result:map_n(fun(GoodAcc, GoodElement) ->
			[ GoodElement | GoodAcc ]
		end, [ fun() -> Acc end, lazy:func(AndThen, [Element]) ])
	end, {ok, []}, Elements),
	result:map(fun lists:reverse/1, Reversed).
