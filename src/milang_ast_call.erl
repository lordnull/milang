-module(milang_ast_call).

-record(call,
	{ function
	, args = []
	}).

-export(
	[ new/2
	, function/1, function/2
	, args/1, args/2
	]).

new(Function, Args) ->
	#call{ function = Function , args = Args }.

function(Rec) ->
	Rec#call.function.

function(Func, Rec) ->
	Rec#call{ function = Func }.

args(Rec) ->
	Rec#call.args.

args(Args, Rec) ->
	Rec#call{ args = Args }.
