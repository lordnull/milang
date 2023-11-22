-module(milang_ast_call).

-record(?MODULE,
	{ function :: milang_ast_expression:ast_node()
	, args = [] :: [ milang_ast_expression:ast_node() ]
	}).
-type data() :: #?MODULE{}.
-type ast_node() :: mialng_ast:ast_node(data()).

-export_type([ data/0, ast_node/0]).

-export(
	[ new/2
	, function/1, function/2
	, args/1, args/2
	, to_string/2
	]).

new(Function, Args) ->
	#?MODULE{ function = Function , args = Args }.

function(Rec) ->
	Rec#?MODULE.function.

function(Func, Rec) ->
	Rec#?MODULE{ function = Func }.

args(Rec) ->
	Rec#?MODULE.args.

args(Args, Rec) ->
	Rec#?MODULE{ args = Args }.

to_string(Rec, Depth) ->
	FunctionStr = milang_ast:to_string(function(Rec), Depth, fun milang_ast_expression:to_string/2),
	ArgsStr = case args(Rec) of
		[] ->
			[];
		Args ->
			Unjoined = lists:map(fun(A) ->
				milang_ast:to_string(A, Depth, fun milang_ast_expression:to_string/2)
			end, Args),
			Joined = lists:join(" ", Unjoined),
			[" " | Joined]
	end,
	[FunctionStr | ArgsStr].
