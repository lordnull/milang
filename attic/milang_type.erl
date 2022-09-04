-module(milang_type).

-record(constructor,
	{ name :: milang_name:name()
	, docs = [] :: unicode:chardata()
	, arg_types = [] :: milang_type:concrete()
	}).
-type constructor() :: #constructor{}.

-record(milang_type,
	{ name :: milang_name:name()
	, docs = [] :: unicode:chardata()
	, exposing = none :: none | name_only | all
	, arg_names = [] :: milang_name:name()
	, arg_constraints = #{} :: milang_class:constraints()
	, constructors = [] :: [ constructor() ]
	}).
-type type() :: #milang_type{}.

-record(concrete_type,
	{ concrete_of :: milang_name:name() % name of the type, never constructor.
	, docs = [] :: unicode:chardata()
	, arg_list = []
	}).

-record(concrete_function,
	% this has no name because function specs are declarations.
	{ arg_list = []
	, docs :: unicode:chardata()
	}).

-type concrete()
	:: #concrete_type{ arg_list :: [ milang_name:name() | #concrete_type{} | #concrete_function{} ] }
	|  #concrete_function{ arg_list :: [ milang_name:name() | #concrete_type{} | #concrete_function{} ] }
	.

-export_type(
	[ constructor/0
	, type/0
	, concrete_type/0
	]).

-export(
	[ new_type/1
	, new_constructor/2
	, new_concrete_type/2
	, new_concrete_function/1
	, token_type/1
	, name/1, name/2
	, docs/1, docs/2
	, arg_types/1, arg_types/2
	, exposing/1, exposing/2
	, arg_names/1, arg_names/2
	, arg_list/1, arg_list/2
	, concrete_of/1, concrete_of/2
	, arg_constraints/1, arg_constraints/2
	, constructors/1, constructors/2
	]).

new_type(Name) ->
	#milang_type{ name = Name }.

new_constructor(Name, Args) ->
	#constructor{ name = Name, arg_types = Args }.

new_concrete_type(Name, Args) ->
	#concrete_type{ concrete_of = Name, arg_list = Args }.

new_concrete_function(Args) ->
	#concrete_function{ arg_list = Args }.

token_type(R) ->
	element(1, R).

name(#constructor{ name = Name}) ->
	Name;
name(#milang_type{ name = Name}) ->
	Name.

name(NewName, Constructor) when is_record(Constructor, constructor) ->
	Constructor#constructor{ name = NewName};
name(NewName, Type) when is_record(Type, milang_type) ->
	Type#milang_type{ name = NewName }.

docs(R) ->
	element(#constructor.docs, R).

docs(NewDocs, R) ->
	setelement(#constructor.docs, R, NewDocs).

arg_types(Constructor) ->
	Constructor#constructor.arg_types.

arg_types(ArgTypes, Constructor) ->
	Constructor#constructor{ arg_types = ArgTypes }.

exposing(Type) ->
	Type#milang_type.exposing.

exposing(ExposingMode, Type) ->
	Type#milang_type{ exposing = ExposingMode }.

arg_names(Type) ->
	Type#milang_type.arg_names.

arg_names(Names, Type) ->
	Type#milang_type{ arg_names = Names }.

arg_list(Concrete) when is_record(Concrete, concrete_type) ->
	Concrete#concrete_type.arg_list;
arg_list(Concrete) when is_record(Concrete, concrete_function) ->
	Concrete#concrete_function.arg_list.

arg_list(List, Concrete) when is_record(Concrete, concrete_type) ->
	Concrete#concrete_type{ arg_list = List };
arg_list(List, Concrete) when is_record(Concrete, concrete_function) ->
	Concrete#concrete_function{ arg_list = List }.

concrete_of(Concrete) ->
	Concrete#concrete_type.concrete_of.

concrete_of(Of, Concrete) ->
	Concrete#concrete_type{ concrete_of = Of }.

arg_constraints(Type) ->
	Type#milang_type.arg_constraints.

arg_constraints(Constraints, Type) ->
	Type#milang_type{ arg_constraints = Constraints }.

constructors(Type) ->
	Type#milang_type.constructors.

constructors(Constructors, Type) ->
	Type#milang_type{ constructors = Constructors }.
