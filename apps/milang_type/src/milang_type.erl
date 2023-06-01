-module(milang_type).

-type type_name() :: [ unicode:chardata() ].
-type variable_name() :: unicode:chardata().

-record(variable_type,
	{ name :: variable_name()
	, ignored = false :: boolean()
	}).
-type variable_type() :: #variable_type{}.

%% `List a`.
%% `List Int`.
-record(closed_type,
	{ name :: type_name()
	, parameters :: [ any() ]
	}).
-type closed_type(ParamType) :: #closed_type{parameters :: [ ParamType ]}.

%% `Cons a (List a).`
%% `Nil.`
-record(constructor,
	{ parent_type :: type_name()
	, name :: type_name()
	, parameters = [] :: [any()]
	}).
-type constructor(ParamType) :: #constructor{parameters :: [ ParamType ]}.

%% `when [ a Comparable ]`
%% `when [ a SomeClass b]`
-record(constraint,
	{ name :: variable_name()
	, class :: type_name()
	, parameters :: [ any() ]
	}).
-type constraint(ParamType) :: #constraint{ parameters :: [ ParamType ]}.

%% `List a [
%%     , Cons a (List a)
%%     , Nil
%% ].`
%% `SortableList a when [ , a Sortable ] [
%%     , Cons a (SortableList a)
%%     , Nil
%% ].`
%% `WeirdCallable a [
%%     , Doable (() -> a)
%%     , Done
%% ].`
-record(data_type,
	{ name :: type_name()
	, constraints = [] :: [constraint(any())]
	, constructors = [] :: [constructor(any())]
	}).
-type data_type(ParamType) :: #data_type{ constraints :: [ constraint(ParamType)], constructors :: [ constructor(ParamType) ]}.

%% `a -> b.`
%% `SomeType -> a.`
%% `List a -> List b`.
%% `when [a Sortable] Map _any a -> SortedList a.`
-record(function_type,
	{ maybe_argument :: maybe:maybe(any())
	, constraints :: [ any() ]
	, return_type
	}).
-type function_type(ParamType) :: #function_type{ maybe_argument :: maybe:maybe(ParamType), return_type :: ParamType, constraints :: [ constraint(ParamType) ]}.

-type parameter()
	:: variable_type()
	|  closed_type(parameter())
	|  function_type(parameter())
	.

-type data_type() :: data_type(parameter()).

-export_type(
	[ parameter/0
	, data_type/0
	]).