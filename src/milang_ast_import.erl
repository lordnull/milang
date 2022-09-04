-module(milang_ast_import).

-record(import,
	{ name
	}).

-type import() :: #import{}.
-export_type([import/0]).

-export(
	[ new/1
	, 'name'/1, 'name'/2
	]).

new(V0) -> #'import'{'name'=V0}.

'name'(R) -> R#'import'.'name'.
'name'(V,R) -> R#'import'{ 'name' = V }.
