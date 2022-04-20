%% Helpers
`
simple_idx({{line, Line}, {column, Col}}) -> {Line, Col}.

log_rule(RuleName, Node, Message, Args) ->
	log_rule(RuleName, Node, Node, Message, Args).

log_rule(RuleName, Node, Node, Message, Args) ->
	ok = io:format("~s ~s: ~p~n", [RuleName, io_lib:format(Message, Args), Node]),
	Node;
log_rule(RuleName, InNode, OutNode, Message, Args) ->
	ok = io:format("~s ~s: ~p -> ~p~n", [RuleName, io_lib:format(Message, Args), InNode, OutNode]),
	OutNode.

log_rule(RuleName, Node) ->
	log_rule(RuleName, Node, Node, "", []).

`
