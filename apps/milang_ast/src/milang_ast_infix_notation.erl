-module(milang_ast_infix_notation).

-record(?MODULE,
	{ function :: milang_ast_expression:ast_node()
	, assoc :: left | right
	, weight :: pos_integer()
	}).
-type data() :: #?MODULE{}.
-type ast_node() :: milang_ast:ast_node(data()).
-export_type([data/0, ast_node/0]).
-export(
	[ new/3
	, 'function'/1, 'function'/2
	, 'assoc'/1, 'assoc'/2
	, 'weight'/1, 'weight'/2
	]).

new(V0,V1,V2) -> #?MODULE{'function'=V0,'assoc'=V1,'weight'=V2}.

'function'(R) -> R#?MODULE.'function'.
'function'(V,R) -> R#?MODULE{ 'function' = V }.

'assoc'(R) -> R#?MODULE.'assoc'.
'assoc'(V,R) -> R#?MODULE{ 'assoc' = V }.

'weight'(R) -> R#?MODULE.'weight'.
'weight'(V,R) -> R#?MODULE{ 'weight' = V }.

