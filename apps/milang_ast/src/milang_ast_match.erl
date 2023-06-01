-module(milang_ast_match).

-record(match,
	{ expression
	, clauses = []
	}).
-type match() :: #match{}.
-export_type([match/0]).

-export(
	[ new/2
	, 'expression'/1, 'expression'/2
	, 'clauses'/1, 'clauses'/2
	]).

new(V0,V1) -> #'match'{'expression'=V0,'clauses'=V1}.

'expression'(R) -> R#'match'.'expression'.
'expression'(V,R) -> R#'match'{ 'expression' = V }.

'clauses'(R) -> R#'match'.'clauses'.
'clauses'(V,R) -> R#'match'{ 'clauses' = V }.

