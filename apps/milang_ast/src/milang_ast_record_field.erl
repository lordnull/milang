-module(milang_ast_record_field).

-record(?MODULE,
	{ name :: milang_ast_identifier:bound()
	, expression :: milang_ast_expression:ast_node()
	}).

-type data() :: #?MODULE{}.
-type ast_node() :: milang_ast:ast_node(data()).

-export_type([data/0, ast_node/0]).

-export(
	[ new/2
	, name/1, name/2
	, expression/1, expression/2
	, to_string/2
	]).

new(Name, Expression) ->
	#?MODULE{ name = Name, expression = Expression }.

name(R) -> R#?MODULE.name.
name(V,R) -> R#?MODULE{ name = V}.

expression(R) -> R#?MODULE.expression.
expression(V,R) -> R#?MODULE{ expression = V }.

to_string(Data, Depth) ->
	[", ", milang_ast_identifier:to_string(name(Data)), " = ", milang_ast:to_string(expression(Data), Depth + 1, fun milang_ast_expression:to_string/2)].
