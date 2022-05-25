%%% @doc The code for managing infix ast and organizing it can be a bit verbose,
%%% so in a new module it goes.
%%%
%%% A 'weight' is a priority indicator. Higher weights get done sooner.
%%%
%%% All operations having the same weight _must_ have the same assciativity.
%%% The default weight is 1, the default association is left, with 2 exceptions.
%%%   * `|>` and `>>` both have a default wieght of 0.
%%%   * `<|` and `<<` both have default weight of 0, and is right associative.
%%%   * infix symbol '' (empty string) is the pipe operator. A syntactic quick
%%%     I've yet to iron out, if I even want to.
%%%   * parens beat all, a default weight of +infinity essentially.
%%%
%%% Things to remember: associativity only really matters when you have more
%%% than 1 infix (even of the same operation).
%%%
%%% So:
%%%   a op b op c == a |>op b |>op c
%%%   a |> b |> c == b a |> c == c (b a)
%%%   f a |> g b |> h c == g b (f a) |> h c == h c (g b (f a))
%%%   f a |>op g b |>op h c == op (g b) (f a) |> op h c == op (h c) (op g b (f a))
%%%
%%% and conversely:
%%%   a <| b <| c == a <| b c == a (b c)
%%%   f a <| g b <| h c == f a <| g b (h c) == f a (g b (h c))
%%%   a <|op b <|op c == a <|op (op b c) == op (op b c) a
%%%   f a <|op g b <| op h c == f a <|op (op (g b) (h c) ) == op (f a) (op (g b) (h c))
%%%
%%% Okay, so the infix ops can be thought of a tree of sorts.
%%% let's start with standard order of operations: pemdas.
%%% We're ignoring parens for this.
%%% exponent, mutl/div, add/sub.
%%% so e has weight of 1, mult has weight of 2, add has weight of 3.
%%% 2 ^ 3 + 4 * 7 == (2 ^ 3) + (4 * 7)
%%% as tree, the lowest weight ends up on top:
%%% (+) +-> (^) +-> 2
%%%     |       +-> 3
%%%     +-> (*) +-> 4
%%%             +-> 7
%%% we can then walk the tree, resolving (parenthsizing) expressios as we go
%%% up.
%%%
%%% A more complex experiment
%%% 1 * 2 ^ 3 + 4 * 5 + 6
%%% (+) +-> 6
%%%     +-> (*) +-> 4
%%%             +-> 5
%%%     +-> (*) +-> 1
%%%             +-> (^) +-> 3
%%%                     +-> 2
%%%
%%% Mising operators of the same weight. Now, instead of the branch nodes simply
%%% having the operator, the branch nodes are a level and the 'head' of the
%%% operation. The head for left associative is all the way to the left. Thus,
%%% the head for right associative is all the way to the right.
%%% 1 + 2 - 3 - 4 * 5 * 6 ^ 7 ^ 8 / 9 / 10 - 11
%%% (1) 1 +-> (+) 2
%%%       +-> (-) 3
%%%       +-> (-) (2) 4 +-> (*) 5
%%%                     +-> (*) (3) 6 +-> (^) 7
%%%                                   +-> (^) 8
%%%                     +-> (/) 9
%%%                     +-> (/) 10
%%%       +-> (-) 11
%%%
-module(milang_infix_tree).

-include("milang_ast.hrl").

-export([from_ast/1]).

from_ast(#milang_ast{ type = expression } = Node) ->
	Data = Node#milang_ast.data,
	#{ infix_ops := InfixOps } = Data,
	Head = maps:get(head, Data),
	MaybePriorToAssoc = weight_to_assoc(InfixOps),
	MaybeTree = maybe_to_tree({Head, InfixOps}, MaybePriorToAssoc),
	maybe_tree_to_ast(MaybeTree);
from_ast(Node) ->
	{ok, Node}.

weight_to_assoc(Ops) ->
	weight_to_assoc(Ops, #{}).

weight_to_assoc([], Map) ->
	{ok, Map};
weight_to_assoc([ {Op, _} | Tail], Map) ->
	{Weight, Assoc} = weight_and_assoc(Op),
	case maps:find(Weight, Map) of
		error ->
			NewMap = Map#{ Weight => Assoc },
			weight_to_assoc(Tail, NewMap);
		{ok, Assoc} ->
			weight_to_assoc(Tail, Map);
		{ok, _DifferAssoc} ->
			{error, {operation_reversed_associativity, Op}}
	end.

-record(op_rec, {
	assoc = left :: 'left' | 'right',
	left :: #milang_ast{} | #op_rec{},
	right :: #milang_ast{} | #op_rec{},
	op = '' :: atom()
}).

maybe_tree_to_ast({error, _} = Error) ->
	Error;
maybe_tree_to_ast({ok, Tree}) ->
	{ok, tree_to_ast(Tree)}.

tree_to_ast(Op) ->
	NewLeft = case Op#op_rec.left of
		#op_rec{} = Left ->
			tree_to_ast(Left);
		_ ->
			Op#op_rec.left
	end,
	NewRight = case Op#op_rec.right of
		#op_rec{} = Right ->
			tree_to_ast(Right);
		_ ->
			Op#op_rec.right
	end,
	FunctionName = case Op#op_rec.op of
		A when is_atom(A) ->
			milang_ast:ast_node({0,0}, <<>>, function_name_local, A);
		NotAtom ->
			NotAtom
	end,
	CallData = #{ name => FunctionName, args => [NewLeft, NewRight] },
	milang_ast:ast_node({0,0}, <<>>, expression_call, CallData).

maybe_to_tree(_OpList, {error, _} = Error) ->
	Error;
maybe_to_tree(OpList, {ok, Map}) ->
	Weights = lists:sort(maps:to_list(Map)),
	{ok, to_tree(OpList, Weights)}.

to_tree({Head, []}, _WeightNoMatter) ->
	Head;
to_tree({Head, Ops}, [{Weight, left} | WeightTail] = AllWeight) ->
	SplitFun = split_fun(Weight),
	case lists:splitwith(SplitFun, Ops) of
		{Ops, []} ->
			to_tree({Head, Ops}, WeightTail);
		{LeftOps, [{Op, NextHead} | OpTail]} ->
			RawLeft = {Head, LeftOps},
			Left = to_tree(RawLeft, WeightTail),
			Right = to_tree({NextHead, OpTail}, AllWeight),
			op_rec(Left, Op, Right)
	end;
to_tree(Ops, [{Weight, right} | WeightTail] = AllWeight) ->
	{Head, ReversedOps} = reverse_oplist(Ops),
	SplitFun = split_fun(Weight),
	case lists:splitwith(SplitFun, ReversedOps) of
		{ReversedOps, []} ->
			to_tree(Ops, WeightTail);
		{RightOps, [{Op, NextHead} | OpTail]} ->
			RawRightReversed = {Head, RightOps},
			RawRight = reverse_oplist(RawRightReversed),
			Right = to_tree(RawRight, WeightTail),
			RawLeft = reverse_oplist({NextHead, OpTail}),
			Left = to_tree(RawLeft, AllWeight),
			op_rec(Left, Op, Right)
	end;
to_tree(Node, []) ->
	Node.



reverse_oplist({Head, Oplist}) ->
	reverse_oplist(Head, Oplist, []).

reverse_oplist(Left, [{Op, Right}], Acc) ->
	{Right, [{Op, Left} | Acc]};
reverse_oplist(Left, [{Op, Right} | Tail], Acc) ->
	reverse_oplist(Right, Tail, [{Op, Left} | Acc]).

split_fun(Weight) ->
	fun({E, _}) ->
		io:format("ye old e: ~p~n", [E]),
		Data = E#milang_ast.data,
		case Data of
			#{weight := N} ->
				N =/= Weight;
			_ when is_atom(Data) ->
				Weight == 1
		end

	end.




op_rec(Left, Op, Right) ->
	{_Weight, Assoc} = weight_and_assoc(Op),
	Data = Op#milang_ast.data,
	RawFunction = maps:get(function, Data),
	FunctionName = case RawFunction of
		#milang_ast{ type = infix_symbol, data = '' } when Assoc =:= left ->
			'|>';
		#milang_ast{ type = infix_symbol, data = '' } when Assoc =:= right ->
			'<|';
		_ ->
			RawFunction
	end,
	#op_rec{
		assoc = Assoc,
		left = Left,
		right = Right,
		op = FunctionName
	}.


weight_and_assoc(Op) ->
	case Op#milang_ast.data of
		'>>' -> {0, left};
		'<<' -> {0, right};
		#{ function := '', assoc := Assoc} ->
			{0, Assoc};
		#{ weight := Weight, assoc := Assoc } ->
			{Weight, Assoc}
	end.



