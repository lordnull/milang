-module(milang_ast_constraint).

-record(?MODULE,
	{ var :: milang_ast_identifier:bound()
	, class :: milang_ast_identifier:bound()
	}).
-type data() :: #?MODULE{}.
-type ast_node() :: mialng_ast:ast_node(data()).

-export_type([ast_node/0, data/0]).

-export(
	[ new/2
	, var/1, var/2
	, class/1, class/2
	, to_string/2
	]).

new(Var, Class) ->
	#?MODULE{ var = Var, class = Class}.

var(R) -> R#?MODULE.var.
var(V, R) -> R#?MODULE{ var = V }.

class(R) -> R#?MODULE.class.
class(C, R) -> R#?MODULE{ class = C }.

to_string(Data, Depth) ->
	[ lists_more:repeat(Depth, "\t")
	, ", "
	, milang_ast_identifier:to_string(var(Data))
	, " "
	, milang_ast_identifier:to_string(class(Data))
	].