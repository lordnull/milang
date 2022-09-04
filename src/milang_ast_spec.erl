-module(milang_ast_spec).

-record(spec,
	{ name
	, constraints
	, type
	, exposed = false
	}).

-export(
	[ new/3, new/4
	, name/1, name/2
	, constraints/1, constraints/2
	, type/1, type/2
	, exposed/1, exposed/2
	]).

new(Name, Constraints, Type) ->
	new(Name, Constraints, Type, false).

new(Name, Constraints, Type, Exposed) ->
	#spec{ name = Name, constraints = Constraints, type = Type, exposed = Exposed}.

name(Rec) ->
	Rec#spec.name.

name(Name, Rec) ->
	Rec#spec{ name = Name }.

type(Rec) ->
	Rec#spec.type.

type(Type, Rec) ->
	Rec#spec{ type = Type}.

constraints(Rec) ->
	Rec#spec.constraints.

constraints(Constraints, Rec) ->
	Rec#spec{ constraints = Constraints }.

exposed(Rec) ->
	Rec#spec.exposed.

exposed(Exposed, Rec) ->
	Rec#spec{ exposed = Exposed }.
