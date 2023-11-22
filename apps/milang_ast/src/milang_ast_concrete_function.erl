-module(milang_ast_concrete_function).
-record(?MODULE,
	{ head :: milang_ast_concrete:ast_node()
	, tail :: milang_ast_concrete:ast_node()
	}).
-type data() :: #?MODULE{}.
-type ast_node() :: milang_ast:ast_node(data()).
-export_type([data/0,ast_node/0]).
-export(
	[ new/2
	, 'head'/1, 'head'/2
	, tail/1, tail/2
	, to_string/2
	]).

new(V0,V1) -> #?MODULE{'head'=V0,tail=V1}.

'head'(R) -> R#?MODULE.'head'.
'head'(V,R) -> R#?MODULE{ 'head'= V }.

tail(R) -> R#?MODULE.tail.
tail(V,R) -> R#?MODULE{ tail = V }.

to_string(Data, Depth) ->
	Head = head(Data),
	Args = tail(Data),
	args_to_string([Head | Args], Depth).

args_to_string(Args, Depth) ->
	Unjoined = lists:map(fun(Arg) ->
		ArgString = milang_ast:to_string(Arg, Depth, fun milang_ast_concrete:to_string/2),
		maybe_paren_wrap(Arg, ArgString)
	end, Args),
	lists:join(" -> ", Unjoined).

maybe_paren_wrap(Arg, String) ->
	Data = milang_ast:data(Arg),
	Concrete = milang_ast_concrete:arg(Data),
	NoWrapNeeded = case milang_ast_concrete:type(Data) of
		function ->
			[] =:= milang_ast_concrete_function:args(Concrete);
		data ->
			true
	end,
	case NoWrapNeeded of
		true ->
			String;
		false ->
			[$(, String, $)]
	end.

