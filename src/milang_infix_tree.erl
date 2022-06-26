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
-include("milang_log.hrl").

-export([from_ast/1]).

-spec from_ast(milang_ast:ast_node(A)) -> {ok, milang_ast:ast_node(A)} | {error, term()}.
from_ast(#milang_ast{ data = #expression_infix{ infix_ops = [] } = Data}) ->
	milang_log:it(debug, ?log_info, "An expression infix with no ops is just the head"),
	Node = Data#expression_infix.head,
	{ok, Node};
from_ast(#milang_ast{ data = #expression_infix{} } = Node) ->
	milang_log:it(debug, ?log_info, "Start of expression handling: ~p", [Node]),
	Data = Node#milang_ast.data,
	#expression_infix{ head = Head, infix_ops = InfixOps } = Data,
	milang_log:it(debug, ?log_info, "Infix ops: ~p", [InfixOps]),
	milang_log:it(debug, ?log_info, "The head: ~p", [Head]),
	MaybePriorToAssoc = weight_to_assoc(InfixOps),
	milang_log:it(debug, ?log_info, "wieghts and assoc: ~p", [MaybePriorToAssoc]),
	MaybeTree = 'Result':map(fun(WeightToAssocMap) ->
		WeightToAssoc = lists:sort(maps:to_list(WeightToAssocMap)),
		to_tree(Node, WeightToAssoc)
	end, MaybePriorToAssoc),
	milang_log:it(debug, ?log_info, "Dah tree: ~p", [MaybeTree]),
	'Result':map(fun tree_to_ast/1, MaybeTree);
from_ast(Node) ->
	{ok, Node}.

weight_to_assoc(Ops) ->
	weight_to_assoc(Ops, #{}).

weight_to_assoc([], Map) ->
	{ok, Map};
weight_to_assoc([ Op | Tail], Map) ->
	{Weight, Assoc} = weight_and_assoc(Op),
	case maps:find(Weight, Map) of
		error ->
			NewMap = Map#{ Weight => Assoc },
			weight_to_assoc(Tail, NewMap);
		{ok, Assoc} ->
			weight_to_assoc(Tail, Map);
		{ok, either} ->
			NewMap = Map#{ Weight => Assoc },
			weight_to_assoc(Tail, NewMap);
		{ok, _ExistingAssoc} when Assoc =:= either ->
			weight_to_assoc(Tail, Map);
		{ok, _DifferAssoc} ->
			{error, {operation_reversed_associativity, Op}}
	end.

-record(op_rec, {
	assoc = left :: 'left' | 'right',
	left :: #milang_ast{} | #op_rec{},
	right :: #milang_ast{} | #op_rec{},
	op = '' :: milang_ast:name()
}).

tree_to_ast(Op) ->
	milang_log:it(debug, ?log_info, "op tree to ast: ~p", [Op]),
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
	FunctionName = Op#op_rec.op,
	CallData = #expression_call{ function = FunctionName, args = [NewLeft, NewRight] },
	milang_ast:ast_node({1,1}, <<>>, CallData).

to_tree(#milang_ast{ data = #expression_infix{ infix_ops = [] } } = Node, _WeightNoMatter) ->
	milang_log:it(debug, ?log_info, "Dah head!", []),
	Node#milang_ast.data#expression_infix.head;
to_tree(Node, [{Weight, right} | WeightTail] = AllWeight) ->
	ReversedNode = reverse_oplist(Node),
	ReversedOps = ReversedNode#milang_ast.data#expression_infix.infix_ops,
	SplitFun = split_fun(Weight),
	case lists:splitwith(SplitFun, ReversedOps) of
		{ReversedOps, []} ->
			to_tree(Node, WeightTail);
		{RightOps, [OpWithNextHead | OpTail]} ->
			OriginalHead = Node#milang_ast.data#expression_infix.head,
			RawRightReversedData = #expression_infix{ head = OriginalHead, infix_ops = RightOps },
			RawRight = reverse_oplist(Node#milang_ast{ data = RawRightReversedData}),
			Right = to_tree(RawRight, WeightTail),
			NextHead = OpWithNextHead#infix_operation.expression,
			Op = OpWithNextHead#infix_operation.operator,
			RawLeftReversedData = #expression_infix{ head = NextHead, infix_ops = OpTail },
			RawLeftReversed = Node#milang_ast{ data = RawLeftReversedData },
			RawLeft = reverse_oplist(RawLeftReversed),
			Left = to_tree(RawLeft, AllWeight),
			op_rec(Left, Op, Right)
	end;
to_tree(Node, [{Weight, _LeftOrEither} | WeightTail] = AllWeight) ->
	SplitFun = split_fun(Weight),
	Ops = Node#milang_ast.data#expression_infix.infix_ops,
	case lists:splitwith(SplitFun, Ops) of
		{Ops, []} ->
			to_tree(Node, WeightTail);
		{LeftOps, [OpNodeWithNextHead | OpTail]} ->
			Op = OpNodeWithNextHead#milang_ast.data#infix_operation.operator,
			NextHead = OpNodeWithNextHead#milang_ast.data#infix_operation.expression,
			OriginalHead = Node#milang_ast.data#expression_infix.head,
			OriginalData = Node#milang_ast.data,
			RawLeft = OriginalData#expression_infix{ head = OriginalHead, infix_ops = LeftOps},
			Left = to_tree(Node#milang_ast{ data = RawLeft }, WeightTail),
			RawRight = OriginalData#expression_infix{ head = NextHead, infix_ops = OpTail },
			Right = to_tree( Node#milang_ast{ data = RawRight }, AllWeight ),
			op_rec(Left, Op, Right)
	end;
to_tree(Node, []) ->
	milang_log:it(debug, ?log_info, "dah node: ~p", [Node]),
	Node.

reverse_oplist(#milang_ast{data = #expression_infix{} = Expression} = Node) ->
	#expression_infix{ head = Head, infix_ops = Oplist} = Expression,
	{NewHead, NewList} = reverse_oplist(Head, Oplist),
	NewData = Expression#expression_infix{ head = NewHead, infix_ops = NewList },
	Node#milang_ast{ data = NewData }.

reverse_oplist(Head, Oplist) ->
	reverse_oplist(Head, Oplist, []).

reverse_oplist(Left, [OpNode], Acc) ->
	#milang_ast{ data = OpData } = OpNode#milang_ast.data,
	#infix_operation{ expression = Right } = OpData,
	ReversedOp = OpData#infix_operation{ expression = Left },
	{Right, [ReversedOp | Acc]};
reverse_oplist(Left, [OpNode | Tail], Acc) ->
	#milang_ast{ data = OpData } = OpNode#milang_ast.data,
	#infix_operation{ expression = Right } = OpData,
	NewOpData = OpData#infix_operation{ expression = Left },
	NewOp = OpNode#milang_ast{ data = NewOpData },
	reverse_oplist(Right, Tail, [ NewOp | Acc]).

split_fun(Weight) ->
	fun(E) ->
		milang_log:it(debug, ?log_info, "ye old e: ~p", [E]),
		N = E#milang_ast.data#infix_operation.operator#milang_ast.data#infix_notation.weight,
		N =/= Weight
	end.


-spec op_rec(milang_ast:expression(), milang_ast:infix_notation(), milang_ast:expression()) -> #op_rec{}.
op_rec(Left, Op, Right) ->
	{_Weight, Assoc} = weight_and_assoc(Op),
	FunctionName = Op#milang_ast.data#infix_notation.name,
	#op_rec{
		assoc = Assoc,
		left = Left,
		right = Right,
		op = FunctionName
	}.


weight_and_assoc(#milang_ast{ data = Data}) ->
	weight_and_assoc(Data);
weight_and_assoc(#infix_operation{ operator = Op}) ->
	weight_and_assoc(Op);
weight_and_assoc(#infix_notation{ assoc = Assoc, weight = Weight }) ->
	{Weight, Assoc};
weight_and_assoc(Op) when is_atom(Op) ->
	{1, either}.

