-module(milang_ast_concrete).
-record(concrete,
	{ name
	, args
	}).
-type concrete() :: #concrete{}.
-export_type([concrete/0]).
-export(
	[ new/2
	, 'name'/1, 'name'/2
	, 'args'/1, 'args'/2
	]).

new(V0,V1) -> #'concrete'{'name'=V0,'args'=V1}.

'name'(R) -> R#'concrete'.'name'.
'name'(V,R) -> R#'concrete'{ 'name'= V }.

'args'(R) -> R#'concrete'.'args'.
'args'(V,R) -> R#'concrete'{ 'args' = V }.

