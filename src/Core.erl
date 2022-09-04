-module('Core').

-export(
	[ '|'/2
	, '|'/0
	, always/1
	, always/0
	, identity/0
	, identity/1
	]).

identity(E) -> E.

identity() ->
	milang_curry:stack(fun identity/1).

always(A) ->
	fun(_) -> A end.

always() ->
	milang_curry:stack(fun always/1).

'|'(Input, Func) ->
	Func(Input).

'|'() ->
	milang_curry:stack(fun '|'/2).
