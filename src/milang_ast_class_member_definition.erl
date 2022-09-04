-module(milang_ast_class_member_definition).

-record(definition,
	{ name
	, spec
	}).

-type definition() :: #definition{}.

-export_type([ definition/0 ]).

-export(
	[ new/2
	, name/1, name/2
	, spec/1, spec/2
	]).

new(Name, Spec) -> #definition{ name = Name, spec = Spec }.

name(R) -> R#definition.name.
name(N, R) -> R#definition{ name = N}.

spec(R) -> R#definition.spec.
spec(S, R) -> R#definition{ spec = S}.
