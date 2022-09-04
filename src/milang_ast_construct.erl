-module(milang_ast_construct).

-record(construct,
	{ name
	, args
	}).

-type construct() :: #construct{}.

-export_type([construct/0]).

-export(
	[ new/2
	, 'name'/1, 'name'/2
	, 'args'/1, 'args'/2
	]).

new(V0,V1) -> #'construct'{'name'=V0,'args'=V1}.

'name'(R) -> R#'construct'.'name'.
'name'(V,R) -> R#'construct'{ 'name' = V }.

'args'(R) -> R#'construct'.'args'.
'args'(V,R) -> R#'construct'{ 'args' = V }.
