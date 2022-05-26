-module('Core').

-export(
	[ '|'/2
	, always/1
	, identity/0
	]).

identity() ->
	fun(E) -> E end.

always(A) ->
	fun(_) -> A end.

'|'(Input, Func) ->
	Func(Input).
