-module(milang_ast_constraints).

-record(constraints,
	{ constraints = []
	}).

-export(
	[ new/1
	, constraints/1, constraints/2
	]).

new(Constraints) ->
	#constraints{ constraints = Constraints }.

constraints(Rec) ->
	Rec#constraints.constraints.

constraints(C, Rec) ->
	Rec#constraints{ constraints = C }.
