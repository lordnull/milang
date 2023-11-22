-module(milang_ast_teach).

-record(?MODULE,
	{ student :: milang_ast_identifier:bound()
	, class :: milang_ast_identifier:bound()
	, bindings :: [ milang_ast_binding:ast_node() ]
	}).
-type data() :: #?MODULE{}.
-type ast_node() :: milang_ast:ast_node(data()).
-export_type([data/0, ast_node/0]).

-export(
	[ new/3
	, 'student'/1, 'student'/2
	, 'class'/1, 'class'/2
	, 'bindings'/1, 'bindings'/2
	, to_string/2
	]).

new(V0,V1,V2) -> #?MODULE{'student'=V0,'class'=V1,'bindings'=V2}.

'student'(R) -> R#?MODULE.'student'.
'student'(V,R) -> R#?MODULE{ 'student' = V }.

'class'(R) -> R#?MODULE.'class'.
'class'(V,R) -> R#?MODULE{ 'class' = V }.

'bindings'(R) -> R#?MODULE.'bindings'.
'bindings'(V,R) -> R#?MODULE{ 'bindings' = V }.

to_string(Data, Depth) ->
	Student = milang_ast_identifier:to_string(student(Data)),
	Class = milang_ast_identifier:to_string(class(Data)),
	Bindings = lists:map(fun(Bind) ->
		["\n", milang_ast:to_string(Bind, Depth + 1, fun milang_ast_binding:to_string/2)]
	end, bindings(Data)),
	[ lists_more:repeat(Depth, "\t")
	, "teach "
	, Student
	, " class "
	, Class
	, " -> "
	, Bindings
	, "\n."
	].

