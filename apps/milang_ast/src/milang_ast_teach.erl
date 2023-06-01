-module(milang_ast_teach).

-record(teach,
	{ student
	, class
	, bindings
	}).
-type teach() :: #teach{}.
-export_type([teach/0]).

-export(
	[ new/3
	, 'student'/1, 'student'/2
	, 'class'/1, 'class'/2
	, 'bindings'/1, 'bindings'/2
	]).

new(V0,V1,V2) -> #'teach'{'student'=V0,'class'=V1,'bindings'=V2}.

'student'(R) -> R#'teach'.'student'.
'student'(V,R) -> R#'teach'{ 'student' = V }.

'class'(R) -> R#'teach'.'class'.
'class'(V,R) -> R#'teach'{ 'class' = V }.

'bindings'(R) -> R#'teach'.'bindings'.
'bindings'(V,R) -> R#'teach'{ 'bindings' = V }.
