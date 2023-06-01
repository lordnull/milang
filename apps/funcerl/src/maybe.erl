%%% @doc While erlang has a convention for blank and non-blank values, it can
%%% be helpful to have code that can unify the representation. By doing so,
%%% we can then use some helpers to make code less verbose.
-module(maybe).

-type maybe(T) :: 'undefined' | {some, T}.

-export_type(
	[ maybe/1
	]).

-export(
	[ some/1
	, nothing/0
	, map/2
	, and_then/2
	, with_default/2
	, map_n/2
	]).

%% @doc A function that simply returns the 'this exists' version of a maybe/1.
%% Primaryily useful to wrap values for use with map_n/2.
%% @end
-spec some(T) -> maybe(T).
some(V) -> {some, V}.

%% @doc A function that simply returns the 'nothing' version of a maybe/1.
%% Useful primarily to avoid hard-coded 'undefined'.
%% @end.
-spec nothing() -> maybe(none()).
nothing() -> undefined.

%% @doc Given a function and a maybe, if the maybe is a 'some', apply the
%% function to it and return a new maybe. Otherwise, nothing.
%% @end
-spec map(fun((A) -> B), maybe(A)) -> maybe(B).
map(Mapper, {some, V}) ->
	{some, Mapper(V)};
map(_Mapper, undefined) ->
	undefined.

%% @doc If the given maybe is something, get a completely new maybe (something
%% or nothing) using the given function and the maybe's value. Otherwise,
%% nothing.
%% @end
-spec and_then(fun((A) -> maybe(B)), maybe(A)) -> maybe(B).
and_then(DoNext, {some, V}) ->
	DoNext(V);
and_then(_DoNext, undefined) ->
	undefined.

%% @doc Given a maybe and a lazy, if the maybe is nothing, resolve the lazy.
%% This can be useful to avoid a 2nd dip in a database to grab a default value,
%% for example.
%% @end
-spec with_default(lazy:lazy(T), maybe(T)) -> T.
with_default(Lazy, 'undefined') ->
	Lazy();
with_default(_Lazy, {ok, V}) ->
	V.

%% @doc Given a function and list of lazy's that produce maybe's, if all the
%% maybe's are something, call the given function with those somethings as the
%% args.
%%
%% This is essentially a short-hand for creating a partial for the function
%% and iterating over the lazy's and applying them.
%% @end.
-spec map_n(fun((...) -> T), [ lazy:lazy(term()) ]) -> maybe(T).
map_n(Mapper, Lazys) ->
	Partial = partial:func(Mapper),
	lists:foldl(fun(Lazy, PartialAcc) ->
		map(PartialAcc, lazy:resolve(Lazy))
	end, {some, Partial}, Lazys).
