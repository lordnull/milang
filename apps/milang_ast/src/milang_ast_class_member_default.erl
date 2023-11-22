-module(milang_ast_class_member_default).

-record(?MODULE,
	{ name :: milang_ast_identifier:bound()
	, function :: milang_ast_function:ast_node()
	}).

-type data() :: #?MODULE{}.
-type ast_node() :: milang_ast:ast_node(data()).

-export_type([data/0, ast_node/0]).

-export(
	[ new/2
	, name/1, name/2
	, function/1, function/2
	, to_string/2
	]).

new(Name, Func) ->
	#?MODULE{ name = Name, function= Func}.

name(R) ->
	R#?MODULE.name.

name(N, R) ->
	R#?MODULE{ name = N}.

function(R) ->
	R#?MODULE.function.

function(F, R) ->
	R#?MODULE{ function = F }.

to_string(Data, Depth) ->
	AsBinding = milang_ast_binding:new(name(Data), function(Data)),
	milang_ast_binding:to_string(AsBinding, Depth).
