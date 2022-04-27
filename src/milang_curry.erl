%%% @doc Kind of a run-time module for milang. Milang is curry by default. This
%%% allows me to wrap erlang functions up so they can simulate curry.
-module(milang_curry).

-export(
	[ stack/1
	, stack_and_call/2
	, call/2
	]).

%% @doc Given a fun (even `fun mod:func/N`), create a fun of exactly 1 arg that
%% returns either a fun that takes 1 arg (the next in line), or a fun of 0 args,
%% indicating we're done.
stack(FinalFunction) ->
	{arity, Arity} = erlang:fun_info(FinalFunction, arity),
	stack(FinalFunction, Arity, []).

stack(FinalFunction, 0, ArgsGathered) ->
	fun() ->
		erlang:apply(FinalFunction, lists:reverse(ArgsGathered))
	end;
stack(FinalFunction, ArityLeft, ArgsGathered) ->
	fun(Arg) ->
		stack(FinalFunction, ArityLeft - 1, [ Arg | ArgsGathered ])
	end.

%% @doc Create a curry stack, and then call it with the given args.
stack_and_call(FinalFunction, Args) ->
	CurryStack = stack(FinalFunction),
	call(CurryStack, Args).

%% @doc Using either a the return from curry_stack, or a return from the curry
%% stack return, keep on calling up the stack.
call(CurryStack, Args) ->
	MaybeStack = lists:foldl(fun(A, Curry) ->
		Curry(A)
	end, CurryStack, Args),
	case MaybeStack of
		_ when is_function(MaybeStack, 0) ->
			MaybeStack();
		_ ->
			MaybeStack
	end.
