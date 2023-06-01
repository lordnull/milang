-module(milang_ast_import).

-record(import,
	{ name
	, imported_ast
	}).

-type import() :: #import{}.
-export_type([import/0]).

-export(
	[ new/1, new/2
	, 'name'/1, 'name'/2
	, imported_ast/1, imported_ast/2
	]).

new(V0) -> #'import'{'name'=V0}.
new(Name, AST) -> #'import'{'name'=Name, imported_ast = AST }.


'name'(R) -> R#'import'.'name'.
'name'(V,R) -> R#'import'{ 'name' = V }.

imported_ast(R) -> R#import.imported_ast.
imported_ast(V, R) -> R#import{ imported_ast = V }.
