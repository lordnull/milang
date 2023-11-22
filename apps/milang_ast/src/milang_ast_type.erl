-module(milang_ast_type).

-record(?MODULE,
	{ name :: milang_ast_identifier:bound()
	, args :: [ milang_ast_identifier:bound() ]
	, constraints :: milang_ast_constraints:ast_node()
	, constructors :: milang_ast_constructors:ast_node()
	, exposed :: none | name_only | all
	}).
-type data() :: #?MODULE{}.
-type ast_node() :: milang_ast:ast_node(data()).
-export_type([ data/0, ast_node/0 ]).

-export(
	[ new/5
	, 'name'/1, 'name'/2
	, 'args'/1, 'args'/2
	, 'constraints'/1, 'constraints'/2
	, 'constructors'/1, 'constructors'/2
	, exposed/1, exposed/2
	, to_string/2
	]).

new(V0,V1,V2,V3,V4) -> #?MODULE{'name'=V0,'args'=V1,'constraints'=V2,'constructors'=V3,exposed=V4}.

'name'(R) -> R#?MODULE.'name'.
'name'(V,R) -> R#?MODULE{ 'name' = V }.

'args'(R) -> R#?MODULE.'args'.
'args'(V,R) -> R#?MODULE{ 'args' = V }.

'constraints'(R) -> R#?MODULE.'constraints'.
'constraints'(V,R) -> R#?MODULE{ 'constraints' = V }.

'constructors'(R) -> R#?MODULE.'constructors'.
'constructors'(V,R) -> R#?MODULE{ 'constructors' = V }.

exposed(R) -> R#?MODULE.exposed.
exposed(V,R) -> R#?MODULE{ exposed = V }.

to_string(Data, Depth) ->
	ExposeString = case exposed(Data) of
		none -> "";
		name_only -> "expose ";
		all -> "expose all "
	end,
	NameString = milang_ast_identifier:to_string(name(Data)),
	ArgsString = lists:map(fun(Arg) ->
		[" ", milang_ast_identifier:to_string(Arg)]
	end, args(Data)),
	ConstraintsString = milang_ast:to_string(constraints(Data), Depth + 1, fun milang_ast_constraints:to_string/2),
	ConstructorString = case constructors(Data) of
		[] -> "";
		Constructors ->
			UnwrappedConstructors = lists:map(fun(Constructor) ->
				[ "\n"
				, lists_more:repeat(Depth + 1, "\t")
				, ", "
				, milang_ast:to_string(Constructor, Depth + 1, fun milang_ast_constructor:to_string/2)
				]
			end, Constructors),
			[" = [", UnwrappedConstructors, "\n", lists_more:repeat(Depth + 1, "\t"), "]"]
	end,
	[ lists_more:repeat(Depth, "\t")
	, ExposeString
	, NameString
	, ArgsString
	, ConstraintsString
	, ConstructorString
	, "."
	].


