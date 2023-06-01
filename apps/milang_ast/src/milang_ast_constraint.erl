-module(milang_ast_constraint).

-record(constraint,
	{ var
	, class
	}).
-type constraint() :: #constraint{}.

-export_type([constraint/0]).

-export(
	[ new/2
	, var/1, var/2
	, class/1, class/2
	]).

new(Var, Class) ->
	#constraint{ var = Var, class = Class}.

var(R) -> R#constraint.var.
var(V, R) -> R#constraint{ var = V }.

class(R) -> R#constraint.class.
class(C, R) -> R#constraint{ class = C }.
