-module(milang_ast_concrete_record_field).

-record(?MODULE,
	{ name :: milang_ast_identifier:bound()
	, type :: milang_ast_concrete:ast_node()
	}).

-type data() :: #?MODULE{}.
-type ast_node() :: milang_ast:ast_node(data()).

-export_type([data/0, ast_node/0]).

-export(
	[ new/2
	, name/1, name/2
	, type/1, type/2
	, to_string/2
	]).

new(Name, Expression) ->
	#?MODULE{ name = Name, type = Expression }.

name(R) -> R#?MODULE.name.
name(V,R) -> R#?MODULE{ name = V}.

type(R) -> R#?MODULE.type.
type(V,R) -> R#?MODULE{ type = V }.

to_string(Data, Depth) ->
	[", ", milang_ast_identifier:to_string(name(Data)), " = ", milang_ast:to_string(type(Data), Depth + 1, fun milang_ast_type:to_string/2)].
