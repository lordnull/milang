-module(milang_ast_infix_tree).

-record(?MODULE,
	{ notation
	, left
	, right
	}).
-type data() :: #?MODULE{}.
-type ast_node() :: milang_ast:ast_node(data()).
-export_type([data/0, ast_node/0]).

-export(
	[ new/3
	, 'notation'/1, 'notation'/2
	, 'left'/1, 'left'/2
	, 'right'/1, 'right'/2
	]).

new(V0,V1,V2) -> #?MODULE{'notation'=V0,'left'=V1,'right'=V2}.

'notation'(R) -> R#?MODULE.'notation'.
'notation'(V,R) -> R#?MODULE{ 'notation' = V }.

'left'(R) -> R#?MODULE.'left'.
'left'(V,R) -> R#?MODULE{ 'left' = V }.

'right'(R) -> R#?MODULE.'right'.
'right'(V,R) -> R#?MODULE{ 'right' = V }.
