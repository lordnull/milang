-module('Result').

-type result(Err, Ok) :: {error, Err} | {ok, Ok}.

-export_type([result/2]).

-export(
	[ 'Ok'/1
	, 'Ok'/0
	, 'Err'/1
	, 'Err'/0
	, ok/1
	, err/1
	, and_then/2
	, recover/2
	, map/2
	, map_error/2
	, with_default/2
	, foldl/3
	, map_list/2
	, map_n/2
	, and_then_n/2
	, to_maybe/1
	, from_maybe/2
	]).

-spec ok(Ok) -> result(none(), Ok).
ok(Ok) ->
	{ok, Ok}.

'Ok'() ->
	milang_curry:stack(fun 'Ok'/1).

'Ok'(V) ->
	ok(V).

'Err'() ->
	milang_curry:stack(fun 'Err'/1).

'Err'(X) ->
	err(X).

-spec err(Err) -> result(Err, none()).
err(Err) ->
	{error, Err}.

-spec map(fun((A) -> B), result(Err, A)) -> result(Err, B).
map(_Mapper, {error, _} = Error) ->
	Error;
map(Mapper, {ok, V}) ->
	{ok, Mapper(V)}.

-spec map_error(fun((ErrA) -> ErrB), result(ErrA, A)) -> result(ErrB, A).
map_error(Mapper, {error, Err}) ->
	{error, Mapper(Err)};
map_error(_, {ok, _} = Ok) ->
	Ok.

-spec and_then(fun((A) -> result(NewErr, B)), result(OldErr, A)) -> result(OldErr, A) | result(NewErr, B).
and_then(_AndThen, {error, _} = Error) ->
	Error;
and_then(AndThen, {ok, V}) ->
	AndThen(V).

-spec recover(fun((Err) -> result(NewErr, A)), result(Err, A)) -> result(NewErr, A).
recover(_Recovery, {ok, _} = Ok) ->
	Ok;
recover(Recover, {error, Err}) ->
	Recover(Err).

-spec with_default(result(_Err, A), A) -> A.
with_default({error, _}, Default) ->
	Default;
with_default({ok, V}, _Default) ->
	V.

to_maybe({ok, V}) ->
	'Maybe':'Some'(V);
to_maybe(_) ->
	'Maybe':'Nothing'().

from_maybe(_ErrValue, {ok, _} = Ok) ->
	Ok;
from_maybe(ErrorValue, undefined) ->
	{error, ErrorValue}.

-spec foldl(fun((Input, Acc) -> result(Err, Acc)), Acc, [ Input ]) -> result(Err, Acc).
foldl(FoldFun, Init, Inputs) ->
	InitResult = ok(Init),
	do_foldl(Inputs, FoldFun, InitResult).

do_foldl(_, _, {error, _} = Error) ->
	Error;
do_foldl([], _, Final) ->
	Final;
do_foldl([Input | Tail], FoldFun, {ok, Acc}) ->
	NewAcc = FoldFun(Input, Acc),
	do_foldl(Tail, FoldFun, NewAcc).

-spec map_list(fun((Input) -> result(Err, Output)), [ Input ]) -> result(Err, [ Output ]).
map_list(Mapper, Elements) ->
	FoldFun = fun(E, Acc) ->
		case Mapper(E) of
			{ok, V} ->
				{ok, [V | Acc]};
			Error ->
				Error
		end
	end,
	Folded = foldl(FoldFun, [], Elements),
	map(fun lists:reverse/1, Folded).

map_n(Results, Mapper) ->
	ResultsLength = length(Results),
	case erlang:fun_info(Mapper, arity) of
		{arity, ResultsLength} ->
			Unwrapped = map_list(fun(E) -> E end, Results),
			map(fun(Args) -> erlang:apply(Mapper, Args) end, Unwrapped);
		_ ->
			error(badarg)
	end.

and_then_n(Results, Next) ->
	ResultsLength = length(Results),
	case erlang:fun_info(Next, arity) of
		{arity, ResultsLength} ->
			Unwrapped = map_list(fun(E) -> E end, Results),
			and_then(fun(Args) -> erlang:apply(Next, Args) end, Unwrapped);
		_ ->
			error(badarg)
	end.
