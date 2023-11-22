-module(milang_ast_concrete_data).
-record(?MODULE,
	{ name :: milang_ast_identifier:bound()
	, args :: [ milang_ast_concrete:ast_node() ]
	}).
-type data() :: #?MODULE{}.
-type ast_node() :: milang_ast:ast_node(data()).
-export_type([data/0,ast_node/0]).
-export(
	[ new/2
	, 'name'/1, 'name'/2
	, 'args'/1, 'args'/2
	, to_string/2
	]).

new(V0,V1) -> #?MODULE{'name'=V0,'args'=V1}.

'name'(R) -> R#?MODULE.'name'.
'name'(V,R) -> R#?MODULE{ 'name'= V }.

'args'(R) -> R#?MODULE.'args'.
'args'(V,R) -> R#?MODULE{ 'args' = V }.

to_string(Data, Depth) ->
	ArgsString = args_to_string(args(Data), Depth),
	NameString = milang_ast_identifier:to_string(name(Data)),
	case ArgsString of
		[] ->
			NameString;
		_ ->
			[NameString, " ", ArgsString]
	end.

args_to_string(Args, Depth) ->
	UnjoinedArgs = lists:map(fun(Arg) ->
		ArgData = milang_ast:data(Arg),
		case milang_ast_concrete:type(ArgData) of
			function ->
				function_arg_to_string(ArgData, Depth);
			data ->
				data_arg_to_string(ArgData, Depth)
		end
	end, Args),
	lists:join(" ", UnjoinedArgs).

function_arg_to_string(Arg, Depth) ->
	AsString = milang_ast_concrete:to_string(Arg, Depth),
	case milang_ast_concrete_function:steps(Arg) of
		[] ->
			AsString;
		_ ->
			[$(, AsString, $)]
	end.

data_arg_to_string(Arg, Depth) ->
	AsString = milang_ast_concrete:to_string(Arg, Depth),
	case milang_ast_concrete_data:args(Arg) of
		[] ->
			AsString;
		_ ->
			[$(, AsString, $)]
	end.

