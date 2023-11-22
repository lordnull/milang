-module(milang_ast_match_clause).

-record(?MODULE,
	{ match :: milang_ast_match_bind:ast_node()
	, expression :: milang_ast_expression:ast_node()
	}).
-type data() :: #?MODULE{}.
-type ast_node() :: milang_ast:ast_node(data()).
-export_type([data/0, ast_node/0]).

-export(
	[ new/2
	, 'match'/1, 'match'/2
	, 'expression'/1, 'expression'/2
	, to_string/2
	]).

new(V0,V2) -> #?MODULE{'match'=V0,'expression'=V2}.

'match'(R) -> R#?MODULE.'match'.
'match'(V,R) -> R#?MODULE{ 'match' = V }.

'expression'(R) -> R#?MODULE.'expression'.
'expression'(V,R) -> R#?MODULE{ 'expression' = V }.

to_string(Data, Depth) ->
	ExpressionString = milang_ast:ast_node(expression(Data), Depth + 1, fun milang_ast_expression:to_string/2),
	MatchString = match_to_string(match(Data), Depth + 2),
	[ "\n"
	, lists_more:repeat(Depth, "\t")
	, MatchString
	, " -> \n"
	, lists_more:repeat(Depth + 1, "\t")
	, ExpressionString
	, "."
	].

match_to_string(Match, Depth) ->
	milang_ast:to_string(Match, Depth, fun milang_ast_match_bind:to_string/2).
