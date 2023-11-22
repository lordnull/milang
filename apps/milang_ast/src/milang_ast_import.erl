-module(milang_ast_import).

-record(?MODULE,
	{ name :: milang_ast_identifier:bound()
	, imported_ast :: maybe:maybe([ milang_ast:ast_node() ])
	}).

-type data() :: #?MODULE{}.
-type ast_node() :: mialng_ast:ast_node(data()).
-export_type([data/0, ast_node/0]).

-export(
	[ new/1, new/2
	, 'name'/1, 'name'/2
	, imported_ast/1, imported_ast/2
	, to_string/2
	]).

new(Name) ->
	new(Name, undefined).

new(Name, AST) -> #?MODULE{'name'=Name, imported_ast = AST }.


'name'(R) -> R#?MODULE.'name'.
'name'(V,R) -> R#?MODULE{ 'name' = V }.

imported_ast(R) -> R#?MODULE.imported_ast.
imported_ast(V, R) -> R#?MODULE{ imported_ast = V }.

to_string(Data, _Depth) ->
	["import ", milang_ast_identifier:to_string(name(Data)), ".\n"].