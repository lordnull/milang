-module(milang_ast_concrete).
-record(?MODULE,
	{ type :: 'data' | 'function' | 'record' | 'any'
	, arg :: milang_ast_concrete_data:ast_node() | milang_ast_concrete_function:ast_node() | milang_ast_identifier:ignored() | milang_ast_concrete_record:ast_node()
	}).
-type data() :: #?MODULE{}.
-type ast_node() :: milang_ast:ast_node(data()).
-export_type([data/0,ast_node/0]).
-export(
	[ new/2
	, 'type'/1, 'type'/2
	, 'arg'/1, 'arg'/2
	, to_string/2
	]).

new(V0,V1) -> #?MODULE{'type'=V0,'arg'=V1}.

'type'(R) -> R#?MODULE.'type'.
'type'(V,R) -> R#?MODULE{ 'type'= V }.

'arg'(R) -> R#?MODULE.'arg'.
'arg'(V,R) -> R#?MODULE{ 'arg' = V }.

to_string(Data, Depth) ->
	ToStringFunc = case type(Data) of
		function ->
			fun milang_ast_concrete_function:to_string/2;
		data ->
			fun milang_ast_concrete_data:to_string/2;
		record ->
			fun milang_ast_concrete_record:to_string/2;
		any ->
			fun millang_ast_identifier:to_string/2
	end,
	milang_ast:to_string(arg(Data), Depth, ToStringFunc).

