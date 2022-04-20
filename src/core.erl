-module(core).

-export(
	[ identity/0
	, always/1
	]).

identity() ->
	fun(E) -> E end.

always(A) ->
	fun(_) -> A end.
