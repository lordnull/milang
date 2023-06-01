-module(milang_ast_binding).

-export(
	[ new/2
	, match/2, match/1
	, expression/1, expression/2
	]).

-record(binding,
	{ match
	, expression
	}).

new(Match, Expression) ->
	#binding{ match = Match, expression = Expression }.

match(Rec) ->
	Rec#binding.match.

match(Match, Rec) ->
	Rec#binding{ match = Match }.

expression(Rec) ->
	Rec#binding.expression.

expression(Expr, Rec) ->
	Rec#binding{ expression = Expr}.
