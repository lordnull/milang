-module(milang_ast_record_access).

-record(?MODULE,
	{ field_name:: milang_ast_identifier:bound()
	}).

-type data() :: #?MODULE{}.
-type ast_node() :: milang_ast:ast_node(data()).

-export_type([data/0, ast_node/0]).

-export(
	[ new/1
	, field_name/1, field_name/2
	, to_string/2
	]).

new(FieldName) ->
	#?MODULE{ field_name = FieldName }.

field_name(R) -> R#?MODULE.field_name.
field_name(V,R) -> R#?MODULE{ field_name = V}.

to_string(Data, _Depth) ->
	[$., milang_ast_identifier:to_string(field_name(Data)) ].
