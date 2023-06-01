%%% @doc Erlang doesn't have a way to conviently do partial application of
%%% a function. This can may using monads difficult or verbose. This wraps
%%% functions in a way to do that. This _will_ make calling them slower, however
%%% if you're using erlang, that's not likely your first concern. In short,
%%% don't use them if you are cpu or memory constrained.
%%% @end
-module(partial).

-record(partial,
	{ function
	, args_left
	, args_gathered_reversed
	}).

-type partial(A, B) :: fun((A) -> B).
-type partial_step(A, B, C) :: fun((A) -> partial(B, C)).

-export_type(
	[ partial/2
	, partial_step/3
	]).

-export(
	[ func/1
	, func/2
	, apply/2
	, call/1
	, call/2
	, sequence/2
	]).

%% @doc Take a function and some arguments and create a partial. If you supplied
%% a 0 or 1 arity function, you get the same thing back. Otherwise, you get a
%% function that takes 1 argument and either completes the function call, or
%% returns yet another partial for the next argument needed.
%%
%% The reason we don't partial 1 arity functions (aside from the obvious
%% silliness) is there is the possibility it's already been partial'ed. So if
%% we did partial the partial, you would need to give the same argument twice
%% to get the expected result.
%% @end
-spec func(fun((...) -> A)) -> partial(term(), A).
func(Fun) ->
	case erlang:fun_info(Fun, arity) of
		{arity, 0} -> Fun;
		{arity, 1} -> Fun;
		{arity, Arity} ->
			Partial = #partial
				{ function = Fun
				, args_left = Arity
				, args_gathered_reversed = []
				},
			build_next_step(Partial)
	end.

%% @doc Take a function and some arguments and create a partial. This is the
%% same as calling `apply(Args, func(Fun))'.
%%
%% If you supply a complete list of arguments, it's the same as calling the
%% function itself (just slower). Otherwise, you get back a 1-arity function
%% that takes the next argument. If you supplied the last argument to the
%% function, you get the function fully applied.
%% @end
-spec func(fun((...) -> A), [ term() ]) -> partial(term(), A).
func(Fun, Args) ->
	Partial = func(Fun),
	?MODULE:apply(Args, Partial).

%% @doc Apply several arguments at once to the given partial function. Useful
%% as erlang doesn't have a syntax for calling chain returned fun's.
%% @end
-spec apply([term()], partial(term(), A)) -> partial(term(), A) | A.
apply(Args, Fun) ->
	lists:foldl(fun(Arg, Acc) ->
		Acc(Arg)
	end, Fun, Args).

%% @doc Same as `call([], Fun).'.
%% @end.
-spec call(fun((...) -> A)) -> A | partial(term(), term()).
call(Fun) ->
	call([], Fun).

%% @doc Given a function of arity N and a list of terms, create apartial for the
%% given function (if needed), and supply the terms given one by one as long as
%% a 1 arity function is returned. This will explode if there are terms left and
%% the return value _is not_ a 1 arity function. If there are no terms left in
%% the argument list, what ever was last returned is the output.
%% @end
-spec call([ term() ], fun((...) -> A)) -> A | partial(term(), term()).
call([], Fun) ->
	Fun;
call([Arg | Tail], Fun) when is_function(Fun, 1) ->
	call(Tail, Fun(Arg));
call(Args, Fun) ->
	Partial = func(Fun),
	call(Args, Partial).

%% @doc Given an initial value and list of partials, apply the partials 1 by 1,
%% and feed the result to the next partial in the list.
%% @end
-spec sequence(A, [ partial(A, B) ]) -> B.
sequence(Initial, Partials) ->
	lists:foldl(fun(Partial, Intermediate) ->
		Partial(Intermediate)
	end, Initial, Partials).

next_step(Arg, Partial) ->
	#partial{ args_left = ArgsLeft, args_gathered_reversed = ArgsListRev } = Partial,
	NewArgsLeft = ArgsLeft - 1,
	NewArgsGathered = [Arg | ArgsListRev],
	NewPartial = Partial#partial{
		args_left = NewArgsLeft,
		args_gathered_reversed = NewArgsGathered
	},
	resolve_or_next(NewPartial).

resolve_or_next(#partial{args_left = 0} = Partial) ->
	#partial{ function = Fun, args_gathered_reversed = ArgsRev } = Partial,
	erlang:apply(Fun, lists:reverse(ArgsRev));
resolve_or_next(Partial) ->
	build_next_step(Partial).

build_next_step(Partial) ->
	fun(NextArg) ->
		next_step(NextArg, Partial)
	end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

add(A, B) -> A + B.

add(A) -> partial:func(fun add/2, [A]).

add() -> partial:func(fun add/2, []).

use_partial_test_() ->
	[ ?_assertEqual(5, ((partial:func(fun add/2))(2))(3))
	, ?_assertEqual(5, partial:sequence(0,
		[ add(1)
		, add(1)
		, add(1)
		, add(2)
		]))
	, ?_assertEqual(5, ((add())(2))(3))
	, ?_assertEqual(5, partial:apply([2,3], add()))
	, ?_assertEqual(5, partial:apply([2], add(3)))
	, ?_assertEqual(5, partial:func(add(2), [3]))
	].

-endif.