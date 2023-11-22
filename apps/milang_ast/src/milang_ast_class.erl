-module(milang_ast_class).

-record(?MODULE,
	{ name :: milang_ast_identifier:bound()
	, constraints :: milang_ast_constraints:ast_node()
	, args :: [ milang_ast:ast_node(milang_ast_identifier:bound()) ]
	, members :: [ milang_ast_class_member:ast_node() ]
	, exposed :: boolean()
	}).
-type data() :: #?MODULE{}.
-type ast_node() :: mialng_ast:ast_node(data()).

-export_type([ data/0, ast_node/0]).

-export(
	[ new/5
	, 'name'/1, 'name'/2
	, 'constraints'/1, 'constraints'/2
	, 'args'/1, 'args'/2
	, 'members'/1, 'members'/2
	, exposed/1, exposed/2
	, to_string/2
	]).

new(V0,V1,V2,V3,V4) -> #?MODULE{'name'=V0,'constraints'=V1,'args'=V2,'members'=V3,exposed=V4}.

'name'(R) -> R#?MODULE.'name'.
'name'(V,R) -> R#?MODULE{ 'name' = V }.

'constraints'(R) -> R#?MODULE.'constraints'.
'constraints'(V,R) -> R#?MODULE{ 'constraints' = V }.

'args'(R) -> R#?MODULE.'args'.
'args'(V,R) -> R#?MODULE{ 'args' = V }.

'members'(R) -> R#?MODULE.'members'.
'members'(V,R) -> R#?MODULE{ 'members' = V }.

exposed(R) -> R#?MODULE.exposed.
exposed(V,R) -> R#?MODULE{ exposed = V }.

to_string(Data, Depth) ->
	NameStr = milang_ast_identifier:to_string(name(Data), Depth),
	ArgsString = args_string(args(Data), Depth),
	ConstraintsString = milang_ast:to_string(constraints(Data), Depth, fun milang_ast_constraints:to_string/2),
	MembersString = members_string(members(Data), Depth),
	Indent = lists:map(compose:always(" "), lists:seq(1,Depth)),
	[Indent, "class ", NameStr, " ", ArgsString, ConstraintsString, MembersString, "." ].

args_string(Args, Depth) ->
	Unjoined = lists:map(fun(Arg) ->
		milang_ast:to_string(Arg, Depth, fun milang_ast_identifier:to_string/2)
	end, Args),
	lists:join(" ", Unjoined).

members_string(Members, Depth) ->
	Unjoined = lists:map(fun(Member) ->
		milang_ast:to_string(Member, Depth + 1, fun milang_ast_class_member:to_string/2)
	end, Members),
	Indent = lists:map(compose:always(" "), lists:seq(1,Depth)),
	Joined = lists:join([",", Indent], Unjoined),
	["[\n", Joined, Indent, "]"].

