-module(milang_ast_match).

-record(?MODULE,
	{ expression :: milang_ast_expression:ast_node()
	, clauses = [ milang_ast_match_clause:ast_node() ]
	}).
-type data() :: #?MODULE{}.
-type ast_node() :: milang_ast:ast_node(data()).
-export_type([data/0, ast_node/0]).

-export(
	[ new/2
	, 'expression'/1, 'expression'/2
	, 'clauses'/1, 'clauses'/2
	, to_string/2
	]).

new(V0,V1) -> #?MODULE{'expression'=V0,'clauses'=V1}.

'expression'(R) -> R#?MODULE.'expression'.
'expression'(V,R) -> R#?MODULE{ 'expression' = V }.

'clauses'(R) -> R#?MODULE.'clauses'.
'clauses'(V,R) -> R#?MODULE{ 'clauses' = V }.

to_string(Data, Depth) ->
	ExpressionString = milang_ast:to_string(expression(Data), Depth, fun milang_ast_expression:to_string/2),
	ClausesString = lists:map(fun(Clause) ->
		milang_ast:to_string(Clause, Depth + 1, fun milang_ast_match_clause:to_string/2)
	end, clauses(Data)),
	[ lists_more:repeat(Depth, "\t")
	, "match "
	, ExpressionString
	, " with"
	, ClausesString
	, "\n"
	, lists_more:repeat(Depth, "\t")
	, "."
	].
