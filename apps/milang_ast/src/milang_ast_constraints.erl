-module(milang_ast_constraints).

-record(?MODULE,
	{ constraints = [] :: [ milang_ast_constraint:ast_node() ]
	}).
-type data() :: #?MODULE{}.
-type ast_node() :: milang_ast:ast_node(data()).
-export_type([data/0, ast_node/0]).

-export(
	[ new/1
	, constraints/1, constraints/2
	, to_string/2
	]).

new(Constraints) ->
	#?MODULE{ constraints = Constraints }.

constraints(Rec) ->
	Rec#?MODULE.constraints.

constraints(C, Rec) ->
	Rec#?MODULE{ constraints = C }.

to_string(Data, Depth) ->
	case constraints(Data) of
		[] ->
			"";
		_ ->
			UnJoined = lists:map(fun(C) ->
				milang_ast:to_string(C, Depth, fun milang_ast_constraint:to_string/2)
			end, constraints(Data)),
			Joined = lists:join("\n", UnJoined),
			[ lists_more:repeat(Depth, "\t")
			, " when [ "
			, Joined
			, "]"
			]
	end.