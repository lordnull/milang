-module(milang_ast_type).

-record(type,
	{ name
	, args
	, constraints
	, constructors
	}).
-type type() :: #type{}.
-export_type([ type/0 ]).

-export(
	[ new/4
	, 'name'/1, 'name'/2
	, 'args'/1, 'args'/2
	, 'constraints'/1, 'constraints'/2
	, 'constructors'/1, 'constructors'/2
	]).

new(V0,V1,V2,V3) -> #'type'{'name'=V0,'args'=V1,'constraints'=V2,'constructors'=V3}.

'name'(R) -> R#'type'.'name'.
'name'(V,R) -> R#'type'{ 'name' = V }.

'args'(R) -> R#'type'.'args'.
'args'(V,R) -> R#'type'{ 'args' = V }.

'constraints'(R) -> R#'type'.'constraints'.
'constraints'(V,R) -> R#'type'{ 'constraints' = V }.

'constructors'(R) -> R#'type'.'constructors'.
'constructors'(V,R) -> R#'type'{ 'constructors' = V }.

