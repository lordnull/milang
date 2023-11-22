-module(milang_ast_binding).

-record(?MODULE,
	{ match :: milang_ast_match_clause:ast_node()
	, expression :: milang_ast_expression:ast_node()
	}).
-type data() :: #?MODULE{}.
-type ast_node() :: mialng_ast:ast_node(data()).

-export_type([ data/0, ast_node/0]).

-export(
	[ new/2
	, match/2, match/1
	, expression/1, expression/2
	, to_string/2
	]).


new(Match, Expression) ->
	#?MODULE{ match = Match, expression = Expression }.

match(Rec) ->
	Rec#?MODULE.match.

match(Match, Rec) ->
	Rec#?MODULE{ match = Match }.

expression(Rec) ->
	Rec#?MODULE.expression.

expression(Expr, Rec) ->
	Rec#?MODULE{ expression = Expr}.

to_string(Data, Depth) ->
	MatchStr = milang_ast:to_string(match(Data), Depth, fun milang_ast_match_clause:to_string/2),
	Expression = milang_ast:to_string(expression(Data), Depth, fun milang_ast_expression:to_string/2),
	Indents = [ "\t" || _ <- lists:seq(1, Depth) ],
	[ Indents, "let ", MatchStr, " = ", Expression, "." ].
