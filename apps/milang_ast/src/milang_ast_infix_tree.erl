-module(milang_ast_infix_tree).

-record(infix_tree,
	{ notation
	, left
	, right
	}).
-type infix_tree() :: #infix_tree{}.
-export_type([infix_tree/0]).

-export(
	[ new/3
	, 'notation'/1, 'notation'/2
	, 'left'/1, 'left'/2
	, 'right'/1, 'right'/2
	]).

new(V0,V1,V2) -> #'infix_tree'{'notation'=V0,'left'=V1,'right'=V2}.

'notation'(R) -> R#'infix_tree'.'notation'.
'notation'(V,R) -> R#'infix_tree'{ 'notation' = V }.

'left'(R) -> R#'infix_tree'.'left'.
'left'(V,R) -> R#'infix_tree'{ 'left' = V }.

'right'(R) -> R#'infix_tree'.'right'.
'right'(V,R) -> R#'infix_tree'{ 'right' = V }.
