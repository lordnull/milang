-module(milang_ast_concrete).
-record(concrete,
	{ constructor
	, args
	}).
-type concrete() :: #concrete{}.
-export_type([concrete/0]).
-export(
	[ new/2
	, 'constructor'/1, 'constructor'/2
	, 'args'/1, 'args'/2
	]).

new(V0,V1) -> #'concrete'{'constructor'=V0,'args'=V1}.

'constructor'(R) -> R#'concrete'.'constructor'.
'constructor'(V,R) -> R#'concrete'{ 'constructor' = V }.

'args'(R) -> R#'concrete'.'args'.
'args'(V,R) -> R#'concrete'{ 'args' = V }.

