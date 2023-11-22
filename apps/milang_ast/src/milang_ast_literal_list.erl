-module(milang_ast_literal_list).

-record(?MODULE,
	{ cons :: maybe:maybe(milang_ast_expression:ast_node())
	, elements = [] :: [milang_ast_expression:ast_node()]
	}).
-type data() :: #?MODULE{}.
-type ast_node() :: milang_ast:ast_node(data()).

-export_type([ data/0, ast_node/0]).
-export(
	[ new/1, new/2
	, cons/1, cons/2
	, elements/1, elements/2
	, to_string/2
	]).

new(Elements) ->
	new(Elements, undefined).

new(Elements, Cons) ->
	#?MODULE{ elements = Elements, cons = Cons}.

cons(Rec) ->
	Rec#?MODULE.cons.

cons(Ref, Rec) ->
	Rec#?MODULE{ cons = Ref}.

elements(Rec) ->
	Rec#?MODULE.elements.

elements(Elements, Rec) ->
	Rec#?MODULE{ elements = Elements}.

to_string(Data, Depth) ->
	ElementsUnjoined = lists:map(fun(Element) ->
		["\n", lists_more:repeat(Depth, "\t"), milang_ast:to_string(Element, Depth + 1, fun milang_ast_expression:to_string/2)]
	end, elements(Data)),
	ElementsString = lists:join("\n", ElementsUnjoined),
	ConsString = case cons(Data) of
		undefined ->
			[];
		Cons ->
			["\n", lists_more:repeat(Depth, "\t"), ",, ", milang_ast:to_string(Cons, Depth + 1, fun milang_ast_expression:to_string/2)]
	end,
	["[\n", ElementsString, ConsString, "\n", lists_more:repeat(Depth, "\t"), "]"].
