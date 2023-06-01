%%% @doc Wraps other functions in a 0-arity function. Use this when the cost
%%% of an operation is higher than the cost(s) of the function creation and
%%% using erlang:apply/2.
%%% @end
-module(lazy).

-type lazy_fun(T) :: fun(() -> T).
-type lazy(T) :: lazy_fun(T) | T.

-export_type(
	[ lazy_fun/1
	, lazy/1
	]).

-export(
	[ func/2
	, resolve/1
	, recursive_resolve/1
	]).

%% @doc Wrap the function and arguments passed to the function in a 0 arity
%% fun. This is useful when we may not know if we need the result of the
%% function, but calling the function in an eager manner is more expensive than
%% temporarily storing the arguments and function itself and using erlang:apply/2
%% later.
%%
%% The args given are assumed to be lazy as well, so you can build up a tree call
%% of sorts.
%% @end
-spec func(fun((...) -> T), [ term() ]) -> fun(() -> T).
func(Fun, Args) ->
	fun() ->
		erlang:apply(Fun, resolve(Args))
	end.

%% @doc Given an erlang term, walk the data structure tree and call any
%% 0-arity functions encountered. This will _not_ resolve an results from the
%% 0-arity function call.
%% @end.
-spec resolve(term()) -> term().
resolve(Term) when is_function(Term, 0) ->
	Term();
resolve(Term) when is_list(Term) ->
	lists:map(fun resolve/1, Term);
resolve(Term) when is_tuple(Term) ->
	AsList = tuple_to_list(Term),
	ResolvedList = resolve(AsList),
	list_to_tuple(ResolvedList);
resolve(Term) when is_map(Term) ->
	maps:fold(fun(Key, Val, Acc) ->
		Acc#{ resolve(Key) => resolve(Val) }
	end,#{},Term);
resolve(Term) ->
	Term.

%% @doc Given an erlang term, walk the data structure tree and call any
%% 0-arity functions encountered. This _will_ resolve any results from the
%% 0-arity function call.
%% @end.
-spec recursive_resolve(term()) -> term().
recursive_resolve(Term) when is_function(Term, 0) ->
	recursive_resolve(Term());
recursive_resolve(Term) when is_list(Term) ->
	lists:map(fun recursive_resolve/1, Term);
recursive_resolve(Term) when is_tuple(Term) ->
	AsList = tuple_to_list(Term),
	ResolvedList = recursive_resolve(AsList),
	list_to_tuple(ResolvedList);
recursive_resolve(Term) when is_map(Term) ->
	maps:fold(fun(Key, Val, Acc) ->
		Acc#{ recursive_resolve(Key) => recursive_resolve(Val) }
	end,#{},Term);
recursive_resolve(Term) ->
	Term.


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

using_lazy(A, B, C) ->
	{A, B, C}.

use_lazy_test_() ->
	[ ?_assertEqual({1, 2, 3}, lazy:resolve(lazy:func(fun using_lazy/3, [1, 2, 3])))
	, ?_assertEqual(5, lazy:resolve(5))
	, ?_assertEqual([{1,2,3}], lazy:resolve([ lazy:func(fun using_lazy/3, [1,2,3])]))
	, ?_assertEqual(5, lazy:recursive_resolve(fun() -> fun() -> fun() -> 5 end end end))
	].

-endif.