%%% @doc Things which alter the "world". Naming modules, assigning
%%% varaibles, creating types. They don't return values and have
%%% little relavenc at run time, but do define the envirment and
%%% known quantities for compile time.
-module(milang_declaration).

-type type_name() :: [ unicode:chardata() ].
-type variable_name() :: unicode:chardata().

%% @doc module declaration line.
-record(module,
	{ name :: type_name()
	}).
-type module_declare() :: #module{}.

%% @doc An import, ie: what we're using
-record(import,
	{ name :: type_name()
	}).
-type import() :: #import{}.

%% @doc data type Declarations
%% ```milang
%% type List a = [
%%     , Cons a (List a)
%%     , Nil
%%     ].
%% ```
%%
%% ```milang
%% type Flag.
%% ```
%%
%% ```milang
%% type SummableList a when [ a Number ] = [
%%     , Cons a (SummableList a)
%%     , Nil
%%     .
%% ```
%%
%% ```milang
%% type ClickClack a = [
%%     , Click (a -> List Int )
%%     , Clack Boolean
%%     , Clank { clunk = a }
%%     ].
%% ```
-record(constraint,
	{ name :: variable_name()
	, class :: type_name()
	, parameters = [] :: [ any() ]
	}).
-type constraint(ParamType) :: #constraint{ parameters :: [ ParamType ]}.

-record(constructor,
	{ name :: type_name()
	, parameters = [] :: [any()]
	}).
-type constructor(ParamType) :: #constructor{parameters :: [ ParamType ]}.

-record(data_type,
	{ name :: type_name()
	, parameters = [] :: [ variable_name() ]
	, constraints = [] :: [ constraint(any()) ]
	, constructors = [] :: [ constructor(any()) ]
	}).
-type data_type(ParamType) :: #data_type{ constraints :: [ constraint(ParamType) ], constructors :: [ constructor(ParamType) ]}.

%% @doc Body for function types. Used in other type declarations, specs, and
%% aliases, thus doesn't have a name of it's own.
-record(function_type,
	{ maybe_argument :: maybe:maybe(any())
	, constraints = [] :: [ constraint(any()) ]
	, return_type
	}).
-type function_type(ParamType) :: #function_type{ maybe_argument :: maybe:maybe(ParamType), return_type :: ParamType, constraints :: [ constraint(ParamType) ]}.

%% @doc Body for record types. Used in other type declarations and aliases, thus
%% doesn't have a name of it's own.
-record(record_field,
	{ name :: variable_name()
	, type :: any()
	}).
-type record_field(ParamType) :: #record_field{ type :: ParamType }.

-record(record_type,
	{ record_fields = [] :: [ record_field(any())]
	}).
-type record_type(ParamType) :: #record_type{ record_fields :: [ record_field(ParamType) ]}.

-type type_declaraion()
	:: data_type(type_declaraion())
	|  function_type(type_declaraion())
	|  record_type(type_declaraion())
	.

%% @doc Set the expected type of a given function binding.
-record(spec_declaration,
	{ name :: variable_name()
	, type :: function_type(any())
	}).
-type spec_declaration(ParamType) :: #spec_declaration{ type :: function_type(ParamType)}.

%% @doc Let a type (be it data, function, or record) be refered to by a given
%% name in addition to any it already has.
-record(alias_declaration,
	{ name :: type_name()
	, type :: any()
	}).
-type alias_declaration(ParamType) :: #alias_declaration{ type :: ParamType }.

-type named_type()
	:: spec_declaration(type_declaraion())
	|  alias_declaration(type_declaraion())
	.

%% @doc And now the binding declarations. While a binding could return a value,
%% we're not going to allow that. In essence, a function cannot return a let.
-record(single_binding,
	{ bind_name :: milang_type:variable_name()
	}).

-type single_binding() :: #single_binding{}.

-record(list_binding,
	{ head_bindings :: [ any() ]
	, tail_binding :: maybe:maybe(any())
	, full_list_binding :: maybe:maybe(milang_type:variable_name())
	}).
-type list_binding(BindType) :: #list_binding{ head_bindings :: [ BindType ], tail_binding :: maybe:maybe(BindType)}.

-record(record_binding,
	{ element_bindings :: [ {milang_type:variable_name(), any()} ]
	, record_itself :: maybe:maybe(variable_name())
	}).
-type record_binding(BindType) :: #record_binding{ element_bindings :: [BindType]}.

-record(constructor_binding,
	{ constructor_name :: milang_type:type_name()
	, parameters = [] :: [ any() ]
	}).
-type constructor_binding(BindType) :: #constructor_binding{parameters :: [ BindType ]}.

-type binding()
	:: single_binding()
	|  list_binding(binding())
	|  record_binding(binding())
	|  constructor_binding(binding())
	.


%% @doc class support. Defining a class and teaching a class.
-record(class_spec,
	{ name :: milang_type:variable_name()
	, spec :: function_type(type_declaraion())
	}).
-type class_spec() :: #class_spec{}.

-record(class_default,
	{ binding :: single_binding()
	}).
-type class_default() :: #class_default{}.

-type class_member()
	:: class_spec()
	|  class_default()
	.

-record(class,
	{ name :: milang_type:type_name()
	, parameters :: [milang_type:variable_name()]
	, members :: [ class_member() ]
	}).
-type class() :: #class{}.

-record(teach,
	{ student_name :: milang_type:type_name()
	, class_name :: milang_type:type_name()
	, implementations :: [ single_binding() ]
	}).
-type teach() :: #teach{}.

-export_type(
	[ module_declare/0
	, import/0
	, named_type/0
	, binding/0
	, class/0
	, teach/0
	]).