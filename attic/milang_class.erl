-module(milang_class).

-record(default
	{ name :: milang_name:name()
	, docs = [] :: unicode:chardata()
	, arg_names = [] :: [ milang_name:name() ]
	, bindings = [] :: [ {milang_name:name(), milang_expression:expression()} ]
	, expression :: milang_expression:expression()
	}).
-type default() :: #default{}.

-record(definition
	{ name :: milang_name:name()
	, docs = [] :: unicode:chardata()
	, type :: milang_type:concrete_function()
	}).
-type definition() :: #definition{}.

-type member() :: #default{} | #definition{}.

-type constraints() :: #{ milang_name:name() => milang_name:name() }.

-record(class
	{ name :: milang_name:name()
	, docs = [] :: unicode:chardata()
	, inherits = [] :: [ milang_name:name() ]
	, members = [] :: [ member() ]
	}).
-type class() :: #class{}.

-record(implement
	{ base_type :: milang_name:name()
	, docs = [] :: unicode:chardata()
	, class :: milang_name:name()
	, definitions = [] :: [ {milang_name:name(), milang_expression:function()}]
	}).
-type implement() :: #implement{}.

-export_type(
	[ default/0
	, definition/0
	, member/0
	, constraints/0
	, class/0
	, implement/0
	]).

-export(
	[ new_default/2
	, new_definition/2
	, new_class/1
	, new_implement/2
	, name/1, name/2
	, docs/1, docs/2
	, type/1, type/2
	, inherits/1, inherits/2
	, members/1, members/2
	, base_type/1, base_type/2
	, class/1, class/2
	, definitions/1, definitions/2
	]).

name(#default{ name = Name}) ->
	Name;
name(#definition{ name = Name}) ->
	Name;
name(#class{ name = Name}) ->
	Name.

name(Name, Rec) when is_record(Rec, default) ->
	Rec#default{ name = Name};
name(Name, Rec) when is_record(Rec, definition) ->
	Rec#definition{ name = Name };
name(Name, Rec) when is_record(Rec, class) ->
	Rec#class{ name = Name }.

docs(Rec) ->
	element(#class.docs, Rec).

docs(Docs, Rec) ->
	setelement(#class.docs, Rec, Docs).

type(Definition) ->
	Definition#definition.type.

type(T, Definition) ->
	Definition#definition{ type = T}.

inherits(Class) ->
	Class#class.inherits.

inherits(Ancesters, Class) ->
	Class#class{ inherits = Ancesters }.

members(Class) ->
	Class#class.members.

members(Members, Class) ->
	Class#class{ members = Members }.

base_type(Implement) ->
	Implement#implement.base_type.

base_type(Base, Implement) ->
	Implement#implement{ base_type = Base }.

class(Implement) ->
	Implement#implement.class.

class(ClassName, Implement) ->
	Implement#implement{ class = ClassName }.

definitions(Implement) ->
	Implement#implement.definitions.

definitions(Defs, Implement) ->
	Implement#implement{ definitions = Defs }.

