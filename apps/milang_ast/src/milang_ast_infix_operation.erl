-module(milang_ast_infix_operation).
-record(infix_operation,
	{ notation
	, expression
	}).
-type infix_operation() :: #infix_operation{}.
-export_type([infix_operation/0]).
-export(
	[ new/2
	, 'notation'/1, 'notation'/2
	, 'expression'/1, 'expression'/2
	]).

new(V0,V1) -> #'infix_operation'{'notation'=V0,'expression'=V1}.

'notation'(R) -> R#'infix_operation'.'notation'.
'notation'(V,R) -> R#'infix_operation'{ 'notation' = V }.

'expression'(R) -> R#'infix_operation'.'expression'.
'expression'(V,R) -> R#'infix_operation'{ 'expression' = V }.

