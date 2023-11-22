-module(milang_ast_alias).

-record(?MODULE,
	{ name :: milang_ast_identifier:data()
	, args :: [ milang_ast_identifier:ast_node() ]
	, constraints :: milang_ast_constraints:ast_node()
	, original :: milang_ast_concrete:ast_node()
	}).

-type data() :: #?MODULE{ }.
-type ast_node() :: mialng_ast:ast_node(data()).

-export_type([data/0, ast_node/0]).

-export(
	[ new/4
	, name/1, name/2
	, args/1, args/2
	, constraints/1, constraints/2
	, original/1, original/2
	]).
-export([to_string/2]).

new(V0,V1,V2,V3) -> #?MODULE{'name'=V0,'args'=V1,'constraints'=V2,'original'=V3}.

'name'(R) -> R#?MODULE.'name'.
'name'(V,R) -> R#?MODULE{ 'name' = V }.

'args'(R) -> R#?MODULE.'args'.
'args'(V,R) -> R#?MODULE{ 'args' = V }.

'constraints'(R) -> R#?MODULE.'constraints'.
'constraints'(V,R) -> R#?MODULE{ 'constraints' = V }.

'original'(R) -> R#?MODULE.'original'.
'original'(V,R) -> R#?MODULE{ 'original' = V }.

to_string(Data, Depth) ->
	NameStr = milang_ast_identifier:to_string(name(Data)),
	ArgsStr = args_string(args(Data)),
	ConstraintsString = milang_ast:to_string(constraints(Data), Depth, fun milang_ast_constraints:to_string/2),
	OriginalStr = milang_ast:to_string(original(Data), Depth, fun milang_ast_concrete:to_string/2),
	["alias ", NameStr, ArgsStr, " ", ConstraintsString, "= ", OriginalStr, ".\n"].

args_string(Args) ->
	Unjoined = lists:map(fun(Arg) ->
		milang_ast:to_string(Arg, 0, fun milang_ast_identifier:to_string/2)
	end, Args),
	Joined = lists:join(" ", Unjoined),
	[" " | Joined].
