-module(milang_ast_infix_series).

-record(infix_series,
	{ head
	, ops
	}).
-type infix_series() :: #infix_series{}.
-export_type([infix_series/0]).

-export(
	[ new/2
	, 'head'/1, 'head'/2
	, 'ops'/1, 'ops'/2
	]).

new(V0,V1) -> #'infix_series'{'head'=V0,'ops'=V1}.

'head'(R) -> R#'infix_series'.'head'.
'head'(V,R) -> R#'infix_series'{ 'head' = V }.

'ops'(R) -> R#'infix_series'.'ops'.
'ops'(V,R) -> R#'infix_series'{ 'ops' = V }.

