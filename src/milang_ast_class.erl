-module(milang_ast_class).

-record(class,
	{ name
	, constraints
	, args
	, members
	}).

-export(
	[ new/4
	, 'name'/1, 'name'/2
	, 'constraints'/1, 'constraints'/2
	, 'args'/1, 'args'/2
	, 'members'/1, 'members'/2
	]).

new(V0,V1,V2,V3) -> #'class'{'name'=V0,'constraints'=V1,'args'=V2,'members'=V3}.

'name'(R) -> R#'class'.'name'.
'name'(V,R) -> R#'class'{ 'name' = V }.

'constraints'(R) -> R#'class'.'constraints'.
'constraints'(V,R) -> R#'class'{ 'constraints' = V }.

'args'(R) -> R#'class'.'args'.
'args'(V,R) -> R#'class'{ 'args' = V }.

'members'(R) -> R#'class'.'members'.
'members'(V,R) -> R#'class'{ 'members' = V }.
