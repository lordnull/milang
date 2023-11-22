-module(milang_ast_constructors).

-record(?MODULE,
	{ constructors = [ milang_ast_concrete:ast_node() ]
	}).

-type data() :: #?MODULE{}.
-type ast_node() :: milang_ast:ast_node(data).
-export_type([data/0, ast_node/0]).

-export(
	[ new/1
	, constructors/1, constructors/2
	, to_string/2
	]).

new(A) -> #?MODULE{ constructors = A }.

constructors(R) -> R#?MODULE.constructors.
constructors(A, R) -> R#?MODULE{ constructors= A }.

to_string(Data, Depth) ->
	UnJoined = lists:map(fun(C) ->
		milang_ast:to_string(C, Depth + 1, fun milang_ast_concrete:to_string/2)
	end, Data),
	Joined = lists:join(", ", UnJoined),
	["[\n", Joined, lists_more:repeat(Depth, "\t"), "]"].
