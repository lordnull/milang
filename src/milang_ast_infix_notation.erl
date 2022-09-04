-module(milang_ast_infix_notation).

-record(infix_notation,
	{ function
	, assoc
	, weight
	}).
-type infix_notation() :: #infix_notation{}.
-export_type([infix_notation/0]).
-export(
	[ new/3
	, 'function'/1, 'function'/2
	, 'assoc'/1, 'assoc'/2
	, 'weight'/1, 'weight'/2
	]).

new(V0,V1,V2) -> #'infix_notation'{'function'=V0,'assoc'=V1,'weight'=V2}.

'function'(R) -> R#'infix_notation'.'function'.
'function'(V,R) -> R#'infix_notation'{ 'function' = V }.

'assoc'(R) -> R#'infix_notation'.'assoc'.
'assoc'(V,R) -> R#'infix_notation'{ 'assoc' = V }.

'weight'(R) -> R#'infix_notation'.'weight'.
'weight'(V,R) -> R#'infix_notation'{ 'weight' = V }.

