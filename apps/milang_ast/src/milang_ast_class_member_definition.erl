-module(milang_ast_class_member_definition).

-record(?MODULE,
	{ name :: milang_ast_identifier:bound()
	, spec :: milang_ast_concrete:ast_node()
	}).

-type data() :: #?MODULE{}.
-type ast_node() :: milang_ast:ast_node(data()).

-export_type([ data/0, ast_node/0 ]).

-export(
	[ new/2
	, name/1, name/2
	, spec/1, spec/2
	, to_string/2
	]).

new(Name, Spec) -> #?MODULE{ name = Name, spec = Spec }.

name(R) -> R#?MODULE.name.
name(N, R) -> R#?MODULE{ name = N}.

spec(R) -> R#?MODULE.spec.
spec(S, R) -> R#?MODULE{ spec = S}.

to_string(Data, Depth) ->
	AsSpec = milang_ast_spec:new(name(Data), [], spec(Data)),
	milang_ast_spec:to_string(AsSpec, Depth).