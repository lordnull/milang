-module(milang_ast_infix_operation).
-record(?MODULE,
	{ notation :: milang_ast_infix_notation:ast_node()
	, expression :: milang_ast_expression:ast_node()
	}).
-type data() :: #?MODULE{}.
-type ast_node() :: milang:ast_node(data()).
-export_type([data/0, ast_node/0]).
-export(
	[ new/2
	, 'notation'/1, 'notation'/2
	, 'expression'/1, 'expression'/2
	]).

new(V0,V1) -> #?MODULE{'notation'=V0,'expression'=V1}.

'notation'(R) -> R#?MODULE.'notation'.
'notation'(V,R) -> R#?MODULE{ 'notation' = V }.

'expression'(R) -> R#?MODULE.'expression'.
'expression'(V,R) -> R#?MODULE{ 'expression' = V }.

