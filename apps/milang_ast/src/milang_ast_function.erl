-module(milang_ast_function).

-record(?MODULE,
	{ args :: [ milang_ast:ast_node(milang_ast_indentifier:bindable()) ]
	, binds :: [ milang_ast_binding:ast_node() ]
	, expression :: milang_ast_expression:ast_node()
	}).

-type data() :: #?MODULE{}.
-type ast_node() :: mialng_ast:ast_node(data()).
-export_type([data/0, ast_node/0]).

-export(
	[ new/3
	, 'args'/1, 'args'/2
	, 'binds'/1, 'binds'/2
	, 'expression'/1, 'expression'/2
	]).

new(V0,V1,V2) -> #?MODULE{'args'=V0,'binds'=V1,'expression'=V2}.

'args'(R) -> R#?MODULE.'args'.
'args'(V,R) -> R#?MODULE{ 'args' = V }.

'binds'(R) -> R#?MODULE.'binds'.
'binds'(V,R) -> R#?MODULE{ 'binds' = V }.

'expression'(R) -> R#?MODULE.'expression'.
'expression'(V,R) -> R#?MODULE{ 'expression' = V }.

