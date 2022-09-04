-module(milang_ast_function).

-record(function,
	{ args
	, binds
	, expression
	}).

-type function_expression() :: #function{}.
-export_type([function_expression/0]).

-export(
	[ new/3
	, 'args'/1, 'args'/2
	, 'binds'/1, 'binds'/2
	, 'expression'/1, 'expression'/2
	]).

new(V0,V1,V2) -> #'function'{'args'=V0,'binds'=V1,'expression'=V2}.

'args'(R) -> R#'function'.'args'.
'args'(V,R) -> R#'function'{ 'args' = V }.

'binds'(R) -> R#'function'.'binds'.
'binds'(V,R) -> R#'function'{ 'binds' = V }.

'expression'(R) -> R#'function'.'expression'.
'expression'(V,R) -> R#'function'{ 'expression' = V }.

