-module(milang_declaration).

-record(alias,
	{ name :: unicode:chardata()
	, docs = [] :: unicode:chardata()
	, exposed = false :: boolean()
	, arg_names = [] :: milang_name:name()
	, arg_constraints = #{} :: milang_class:constraints()
	, alias_of :: milang_type:concrete()
	}).
-type alias() :: #alias{}.

-record(class,
	{ name :: unicode:chardata()
	, docs = [] :: unicode:chardata()
	, arg_names = [] :: milang_name:name()
	, arg_constraints = #{} :: milang_class:constraints()
	, definitions = [] :: [ milang_class:definition() ]
	}).
-type class() :: #class{}.

-record(spec,
	{ for :: milang_name:name() % the binding the spec is for
	, docs = [] :: unicode:chardata()
	, exposed = false :: boolean()
	, arg_constraints = #{} :: milang_class:constraints()
	, spec :: milang_type:concrete()
	}).
-type spec() :: #spec{}.

-record(binding,
	{ name :: unicode:chardata()
	, docs = [] :: unicode:chardata()
	, expression :: milang_expression:expression()
	}).
-type binding() :: #binding{}.

-type declaration()
	:: type()
	|  alias()
	|  class()
	|  spec()
	|  binding()
	.
