-module(milang_ast_alias).

-record(alias,
	{ name
	, args
	, constraints
	, original
	}).

-type alias() :: #alias{
	name :: milang_ast:ast_node(milang_ast_identifier:bound() | milang_ast_identifier:type()),
	args :: [ milang_ast:ast_node(milang_ast_identifier:bound())],
	constraints :: milang_ast:ast_node(milang_ast_constraints:constraints()),
	original :: milang_ast:ast_node(milang_ast_concrete:concrete())
}.

-export_type([alias/0]).

-export(
	[ new/4
	, name/1, name/2
	, args/1, args/2
	, constraints/1, constraints/2
	, original/1, original/2
	]).

new(V0,V1,V2,V3) -> #'alias'{'name'=V0,'args'=V1,'constraints'=V2,'original'=V3}.

'name'(R) -> R#'alias'.'name'.
'name'(V,R) -> R#'alias'{ 'name' = V }.

'args'(R) -> R#'alias'.'args'.
'args'(V,R) -> R#'alias'{ 'args' = V }.

'constraints'(R) -> R#'alias'.'constraints'.
'constraints'(V,R) -> R#'alias'{ 'constraints' = V }.

'original'(R) -> R#'alias'.'original'.
'original'(V,R) -> R#'alias'{ 'original' = V }.
