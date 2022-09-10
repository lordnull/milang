%%% @doc Convert between 2 representations of infix operations: tree and series.
%%% Series is useful for parsing and writing since it requires basically no
%%% backtracking or redoing. Tree is useful for expression resolution and compile
%%% because it better represents what the actual resolution is.
%%%
%%% A 'weight' is a priority indicator like a stone is a depth helper. The larger
%%% the weight, the lower on the list of operations it will be done. Given a
%%% weight of 1 vs a weight of 10, 1 is resolved first, 10 after.
%%%
%%% Operations also have associativity; or rather, which side gets resolved
%%% first. Left associative gets the left side resolved first, with right the
%%% opposite. The naming is irksome because the intuitive symbols "»" for left
%%% and "«" for right point away from the expression to be resolved first.
%%%
%%% The defaults for all operators is a weight of 1 and left associativity. This
%%% means PEDMAS is not observed, and all operators must be written in a way that
%%% their associativity can be swapped. Parenthesis still work as intended though,
%%% as that creates it's own sub-expression.
%%%
%%% Associativity only matters if you have more than one infix operator, even if
%%% it's the same operator. All operators of the same weight _must_ have the same
%%% associativity.
%%%
%%% Let's start with standard order of operations: PEMDAS. We're ignoring parens
%%% for this. Exponent (^), Multiply (*), divide (/), add (+) and sub (-).
%%% (^) has weight 1, (*) and (/) have weight 2, and (+) and (-) have weight 3.
%%%
%%% So given the expression `2 '^ 3 '+ 4 '* 7', we can make the grouping explicite
%%% with parenthesis to get `(2 '^ 3) '+ (4 '* 7)'. With milang's default behavior,
%%% this is actually `( ( (2 '^ 3) '+ 4 ) '* 7 )'.
%%%
%%% We can represent this as a tree. The lowest weights end up near the leaves,
%%% higher weights near the root.
%%%
%%%    L3: 2     3     4     7
%%%        |     |     |     |
%%%    L2: +-(^)-+     +-(*)-+
%%%           |           |
%%%    L1:    +----(+)----+
%%%                 |
%%%
%%% We can then Climb the tree, going to the left (as the operators are all left
%%% associative), until we get to a leaf. We resolve that, then climb up the
%%% other side of the branch as deep as we can go. Rinse and repeat until you're
%%% back at the root.
%%%
%%% * L1 (+) : go left.
%%% * L2 (^) : go left.
%%% * L3 (2) : resolve, backup
%%% * L2 (^) : go right.
%%% * L3 (3) : resolve, backup
%%% * L2 (^) : resolve (2 ^ 3), backup
%%% * L1 (+) : go right.
%%% * L2 (*) : go left.
%%% * L3 (4) : resolve, backup
%%% * L2 (*) : go right
%%% * L3 (7) : resolve, backup
%%% * L2 (*) : resolve (4 * 7), backup
%%% * L1 (+) : resolve ((2 ^ 3) + (4 * 7))
%%%
%%% A more complex Example using the same weights from above:
%%%
%%% 1 * 2 ^ 3 + 4 * 5 + 6
%%%
%%%    L5:    2     3  4     5
%%%           |     |  |     |
%%%    L4: 1  +-(^)-+  +-(*)-+
%%%        |     |        |
%%%    L3: +-(*)-+        |
%%%           |           |
%%%    L2:    +----(+)----+     6
%%%                 |           |
%%%    L1:          +----(+)----+
%%%                       |
%%%
%%% Same algo as before.
%%%
%%% * L1 (+) : go left.
%%% * L2 (+) : go left.
%%% * L3 (*) : go left.
%%% * L4 (1) : resolve, backup.
%%% * L3 (1 *) : go right.
%%% * L4 (^) : go left.
%%% * L5 (2) : resolve, backup.
%%% * L4 (2 ^) : go right.
%%% * L5 (3) : resolve, backup.
%%% * L4 (2 ^ 3) : resolve, backup.
%%% * L3 (1 * 8) : resolve, backup.
%%% * L2 (8 +) : go right.
%%% * L4 (*) : go left.
%%% * L5 (4) : resolve, backup.
%%% * L4 (4 *) : go right.
%%% * L5 (5) : resolve, backup.
%%% * L4 (4 * 5) : resolve, backup.
%%% * L2 (8 + 20) : resolve, backup.
%%% * L1 (28 +) : go right.
%%% * L2 (6) : resolve, backup
%%% * L1 (28 + 6) : resolve
%%%
%%% For any right associative operators, go right first instead.

-module(milang_ast_infix).

-include_lib("kernel/include/logger.hrl").

-export([as_series/1, as_tree/1]).

as_series(Node) ->
	case milang_ast:type(Node) of
		{ok, infix_series} ->
			{ok, Node};
		{ok, inifix_tree} ->
			{NewHead, Ops} = as_series(Node, new_branch, []),
			Data = milang_ast_infix_series:new(NewHead, Ops),
			milang_ast:transform_data(fun(_) -> Data end, Node)
	end.

as_tree(Node) ->
	?LOG_DEBUG("series we're converting: ~p", [Node]),
	case milang_ast:type(Node) of
		{ok, infix_tree} ->
			{ok, Node};
		{ok, infix_series} ->
			Data = milang_ast:data(Node),
			Head = milang_ast_infix_series:head(Data),
			Ops = milang_ast_infix_series:ops(Data),
			ResWeightToAssoc = weight_to_assoc(Ops),
			'Result':map(fun(WeightToAssocMap) ->
				WeightToAssoc = lists:sort(maps:to_list(WeightToAssocMap)),
				as_tree(Head, Ops, WeightToAssoc)
			end, ResWeightToAssoc)
	end.

weight_to_assoc(Ops) ->
	weight_to_assoc(Ops, #{}).

weight_to_assoc([], Map) ->
	{ok, Map};
weight_to_assoc([ Op | Tail], Map) ->
	OpData = milang_ast:data(Op),
	Notation = milang_ast_infix_operation:notation(OpData),
	Weight = milang_ast_infix_notation:weight(Notation),
	Assoc = milang_ast_infix_notation:assoc(Notation),
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

as_tree(Head, [], _DoesNotMatter) ->
	% we have reached the end of leftward branches, and thus, have hit the head.
	% the head is almost certainly not an infix expression.
	Head;
as_tree(Head, Ops, [{Weight, right} | WeightTail] = AllWeight) ->
	% we need to be looking for rightward branches. However, the head and ops we
	% have are optimized for leftward looking. So while much of this is the same
	% as the next clause for leftward branches, we need to do some uno reverse
	% card magic.
	{NewHead, ReversedOps} = reverse_oplist(Head, Ops),
	SplitFun = split_fun(Weight),
	case lists:splitwith(SplitFun, ReversedOps) of
		{ReversedOps, []} ->
			as_tree(Head, Ops, WeightTail);
		{RightOps, [OpWithNextHead | OpTail]} ->
			Right = as_tree(NewHead, RightOps, WeightTail),
			OpWithNextHeadData = milang_ast:data(OpWithNextHead),
			NextHead = milang_ast_infix_operation:expression(OpWithNextHeadData),
			Op = milang_ast_infix_operation:notation(OpWithNextHeadData),
			{LeftHead, LeftOps} = reverse_oplist(NextHead, OpTail),
			Left = as_tree(LeftHead, LeftOps, AllWeight),
			Data = milang_ast_infix_tree:new(Op, Left, Right),
			milang_ast:transform_data(fun(_) -> Data end, Head)
	end;

as_tree(Head, Ops, [{Weight, _LeftOrEither} | WeightTail] = AllWeight) ->
	SplitFun = split_fun(Weight),
	case lists:splitwith(SplitFun, Ops) of
		{Ops, []} ->
			as_tree(Head, Ops, WeightTail);
		{LeftOps, [ OpNodeWithNextHead | OpTail]} ->
			OpNodeWithNextHeadData = milang_ast:data(OpNodeWithNextHead),
			Op = milang_ast_infix_operation:notation(OpNodeWithNextHeadData),
			NextHead = milang_ast_infix_operation:expression(OpNodeWithNextHeadData),
			Left = as_tree(Head, LeftOps, WeightTail),
			Right = as_tree(NextHead, OpTail, AllWeight),
			Data = milang_ast_infix_tree:new(Op, Left, Right),
			milang_ast:transform_data(fun(_) -> Data end, Head)
	end;

as_tree(Head, Ops, []) ->
	?LOG_DEBUG("Not sure how I got here. Head: ~p; Ops: ~p", [Head, Ops]),
	error(mass_confusion).

reverse_oplist(Head, Ops) ->
	reverse_oplist(Head, Ops, []).

reverse_oplist(Left, [OpNode], Acc) ->
	OpData = milang_ast:data(OpNode),
	Notation = milang_ast_infix_operation:notation(OpData),
	Right = milang_ast_infix_operation:expression(OpData),
	ReveresedOpData = milang_ast_infix_operation:new(Notation, Left),
	ReversedOp = milang_ast:transform_data(fun(_) -> ReveresedOpData end, OpNode),
	{Right, [ ReversedOp | Acc]};

reverse_oplist(Left, [OpNode | Tail], Acc) ->
	OpData = milang_ast:data(OpNode),
	Notation = milang_ast_infix_operation:notation(OpData),
	Right = milang_ast_infix_operation:expression(OpData),
	ReveresedOpData = milang_ast_infix_operation:new(Notation, Left),
	ReversedOp = milang_ast:transform_data(fun(_) -> ReveresedOpData end, OpNode),
	reverse_oplist(Right, Tail, [ ReversedOp | Acc]).

split_fun(Weight) ->
	fun(Node) ->
		?LOG_DEBUG("ye old e: ~p", [Node]),
		Data = milang_ast:data(Node),
		Notation = milang_ast_infix_operation:notation(Data),
		N = milang_ast_infix_notation:weight(Notation),
		N =/= Weight
	end.

as_series(Node, new_branch, Acc) ->
	Data = milang_ast:data(Node),
	Left = milang_ast_infix_tree:left(Data),
	Right = milang_ast_infix_tree:right(Data),
	Notation = milang_ast_infix_tree:notation(Data),
	case milang_ast:type(Right) of
		{ok, infix_tree} ->
			{NewHead, NewAcc} = as_series(Right, new_branch, Acc),
			NewOpData = milang_ast_infix_operation:new(Notation, NewHead),
			NewOp = milang_ast:transform_data(fun(_) -> NewOpData end, Node),
			NewAcc = [NewOp | Acc],
			as_series(Left, the_left_side, NewAcc);
		_ ->
			NewOpData = milang_ast_infix_operation:new(Notation, Right),
			NewOp = milang_ast:transform_data(fun(_) -> NewOpData end, Node),
			NewAcc = [NewOp | Acc],
			as_series(Left, the_left_side, NewAcc)
	end;

as_series(Node, the_left_side, Acc) ->
	case milang_ast:type(Node) of
		{ok, infix_tree} ->
			as_series(Node, new_branch, Acc);
		_ ->
			{Node, Acc}
	end.
