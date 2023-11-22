-module(milang_ast_infix_series).

-record(?MODULE,
	{ head
	, ops
	}).
-type data() :: #?MODULE{}.
-type ast_node() :: milang_ast:ast_node(data()).
-export_type([data/0, ast_node/0]).

-export(
	[ new/2
	, 'head'/1, 'head'/2
	, 'ops'/1, 'ops'/2
	]).

new(V0,V1) -> #?MODULE{'head'=V0,'ops'=V1}.

'head'(R) -> R#?MODULE.'head'.
'head'(V,R) -> R#?MODULE{ 'head' = V }.

'ops'(R) -> R#?MODULE.'ops'.
'ops'(V,R) -> R#?MODULE{ 'ops' = V }.

