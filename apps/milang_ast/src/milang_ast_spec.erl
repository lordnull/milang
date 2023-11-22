-module(milang_ast_spec).

-record(?MODULE,
	{ name :: milang_ast_identifier:bound()
	, constraints :: milang_ast_constraints:ast_node()
	, type :: milang_ast_concrete:ast_node()
	, exposed = false :: boolean()
	}).
-type data() :: #?MODULE{ }.
-type ast_node() :: mialng_ast:ast_node(data()).

-export_type([ data/0, ast_node/0 ]).
-export(
	[ new/3, new/4
	, name/1, name/2
	, constraints/1, constraints/2
	, type/1, type/2
	, exposed/1, exposed/2
	, to_string/2
	]).

-spec new(milang_ast_identifier:bound(), milang_ast_constraints:ast_node(), milang_ast_concrete:ast_node()) -> milang_ast_spec:data().
new(Name, Constraints, Type) ->
	new(Name, Constraints, Type, false).

-spec new(milang_p_token:identifier_bound(), milang_ast_constraints:ast_node(), milang_ast_concrete:ast_node(), boolean()) -> milang_ast_spec:data().
new(Name, Constraints, Type, Exposed) ->
	#?MODULE{ name = Name, constraints = Constraints, type = Type, exposed = Exposed}.

name(Rec) ->
	Rec#?MODULE.name.

name(Name, Rec) ->
	Rec#?MODULE{ name = Name }.

type(Rec) ->
	Rec#?MODULE.type.

type(Type, Rec) ->
	Rec#?MODULE{ type = Type}.

constraints(Rec) ->
	Rec#?MODULE.constraints.

constraints(Constraints, Rec) ->
	Rec#?MODULE{ constraints = Constraints }.

exposed(Rec) ->
	Rec#?MODULE.exposed.

exposed(Exposed, Rec) ->
	Rec#?MODULE{ exposed = Exposed }.

to_string(Data, Depth) ->
	ExposedStr = case exposed(Data) of
		true -> "expose ";
		false -> ""
	end,
	NameStr = milang_ast_identifier:to_string(name(Data)),
	TypeString = milang_ast:to_string(type(Data), Depth, fun milang_ast_concrete:to_string/2),
	ConstraintsString = milang_ast:to_string(constraints(Data), Depth + 1, fun milang_ast_constraints:to_string/2),
	[ ExposedStr
	, "spec "
	, NameStr
	, " = "
	, TypeString
	, ConstraintsString
	, "."
	].
