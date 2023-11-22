-module(milang_type_validation).

-export(
	[ new/0
	, validate_list/2
	]).

-include_lib("kernel/include/logger.hrl").

% okay, so given the following:
% type Goober = [
%     , Pants
%     , Slacks Integer
%     ].
%
% spec inseam = Goober -> Integer.
% inseam goober = function
%     match goober when
%         Pants -> 1.
%         Slacks n -> n.
%     .
% .
%
% We need to:
%   Load up Goober as a type with no constraints and no args
%   Load up Pants as a type with no constraints, no args, and Goober as parent.
%   Load up Slacks as a type with no constraints, and 1 arg:
%       Integer type (base type? primitive? something like that.)
%   Load up inseam as a type with no constrats, function with 1 arg, and 1 return value
%       arg is a Goober
%       return is an Integer
%   Check the implemetation of inseam matches the spec, or do some inferencing.

-type local_name() :: binary().
-type remote_name() :: #{ local := local_name(), module := local_name() }.
-type name() :: local_name() | remote_name().
-type class_taught() :: {class_taught, StudentName :: name(), ClassName :: name(), SpecName :: name() }.
-type mi_identifier() :: name() | class_taught().
-type maybe_identifier() :: undefined | mi_identifier().

% What is used in specs and defining a type as an argument to other types.
% for example, given the declaration of:
% ```milang
% type Maybe a = [
%     , Just a
%     , Nothing
%     ].
% ```
% We can define:
% ```milang
% alias Greeting = Maybe String.
% ```
% Where the `Maybe String` is a concrete of the data type `Maybe`.
%
% The same is true of function specs.
% ```milang
% spec Maybe.map = ( a -> b ) -> Maybe a -> Maybe b.
% ```
%
% The Spec itself is not concrete, but the use of `Maybe` within it is. This is
% despite the use of variables. Essentially, during type inference and checking,
% the spec definition will have the variables replaced with actual types. Where
% the function the spec is defining is called from uses the concrete of the spec.
%
% So while a spec is not a concrete, a function call expression is the concerete
% version of a spec. It is the usage and context that determines if a concrete
% is solidifying tv_data or tv_function.
-record(tv_concrete, {
	concrete_of = undefined :: maybe_identifier(),
	args = [] :: [ #tv_concrete{} | mi_identifier() ]
}).

% The top level definition of data types. This is generated from type declarations,
% and only for the type that is being declared. Thus why the constructors that
% reference the type use concretes and variables.
-record(tv_data, {
	constraints = #{} :: #{ atom() => #tv_concrete{} },
	arg_names = [] :: [ atom() ],
	has_constructors = false :: boolean(),
	classes_learned = []
}).
-type data() :: #tv_data{}.

% what a class means.
-record(tv_class,
	{ prereq_classes = []
	, type_variable
	, member_specs = #{}
	, default_implementations = #{}
	}).
-type class() :: #tv_class{}.

% the individual constructors for a top type. Generally used in type
% declarataions, or to find the type of an expression.
% -type Maybe a | Just a | Nothing. :: #{
%     'Just' => #constructor{ constructor_of = 'Maybe', args = [ a ]},
%     'Nothing' => #constructor{ constructor_of = 'Maybe', args = []}
% }.
-record(tv_constructor, {
	constructor_of :: mi_identifier(),
	args = [] :: [ #tv_concrete{} | mi_identifier() ]
}).

% The top level definition of function types. This is generated from spec
% declarations, and only for the function that is being spec'ed. This is distinct
% from the tv_data because the args are not just a list of variable names, nor
% does it have a list of constructors. Instead, args is a list of variables or
% concretes.
-record(tv_function, {
	constraints = #{} :: #{ atom() => #tv_concrete{} },
	args = [] :: [ mi_identifier() | #tv_concrete{} ]
}).

-type concrete() :: #tv_concrete{ args :: [ mi_identifier() | #tv_concrete{} ]}.

-type constructor() :: #tv_constructor{ args :: [ mi_identifier() | #tv_concrete{} ]}.

% points to another name.
-record(tv_alias, {
	arg_names = [] :: [ mi_identifier() ],
	constraints = #{} :: #{ atom() => #tv_concrete{} },
	truename :: concrete()
}).
-type alias() :: #tv_alias{}.

-type placeholder() :: {placeholder, mi_identifier()}.

-type entry() :: concrete() | data() | constructor() | alias() | class() | placeholder().

-type lookup_table() :: nonempty_list( #{ mi_identifier() => entry()} ).

-export_type(
	[ concrete/0
	, data/0
	, constructor/0
	, alias/0
	, class/0
	, entry/0
	, lookup_table/0
	, mi_identifier/0
	, maybe_identifier/0]).

-spec new() -> lookup_table().
new() ->
	[#{}].

-spec validate_list([ milang_ast:ast_node() ], lookup_table()) -> {ok, lookup_table()} | {error, term()}.
validate_list(Nodes, Table) ->
	lists:foldl(fun(Node, Acc) ->
		result:and_then(partial:func(fun validate_node/2, [Node]), Acc)
	end, {ok, Table}, Nodes).

-spec validate_node(milang_ast:ast_node(), lookup_table()) -> {ok, lookup_table()} | {error, term()}.
validate_node(Node, Table) ->
	Type = milang_ast:type_simply(Node),
	?LOG_DEBUG("validate node of type ~p (full node: ~p)", [Type, Node]),
	validate_node(Type, Node, Table).

validate_node(milang_ast_module, _Node, Table) ->
	?LOG_DEBUG("Easy validating a module declaration.", []),
	{ok, Table};
validate_node(milang_ast_import, _Node, Table) ->
	% TODO currenly import does not even allow alias of the module, nor quick
	% expose of the functions. It's further up to the caller to load the table
	% with the header data found for the import. So for now, this is a quicky
	% pass.
	?LOG_DEBUG("Easy validating an import declaration.", []),
	{ok, Table};

validate_node(milang_ast_spec, Node, Table) ->
	Data = milang_ast:data(Node),
	validate_spec(Data, Table);

validate_node(milang_ast_type, Node, Table) ->
	Data = milang_ast:data(Node),
	NameComplex = milang_ast_type:name(Data),
	ConstraintsNode = milang_ast_type:constraints(Data),
	ConstructorsNode = milang_ast_type:constructors(Data),
	Args = milang_ast_type:args(Data),
	{_, Name} = NameComplex,
	Constraints = milang_ast_constraints:constraints(milang_ast:data(ConstraintsNode)),
	ConstraintMap = lists:foldl(fun(ConstraintNode, Acc) ->
		Constraint = milang_ast:data(ConstraintNode),
		Key = milang_ast_constraint:var(Constraint),
		Val = milang_ast_constraint:class(Constraint),
		case type_of_node(Val, Table) of
			{ok, T} ->
				Acc#{ Key => T };
			Wut ->
				error({invalid_constraint, Key, Wut})
		end
	end, #{}, Constraints),
	ArgNames = [ {placeholder, identifier(milang_ast:data(A))} || A <- Args],
	ConstructorsData = milang_ast:data(ConstructorsNode),
	Constructors = milang_ast_constructors:constructors(ConstructorsData),
	HasConstructors = case Constructors of
		[] -> false;
		_ -> true
	end,
	TopType = #tv_data{ arg_names = ArgNames, constraints = ConstraintMap, has_constructors = HasConstructors },
	MidTable = set_entry(Name, TopType, Table),
	ConstructorFoldFun = fun(ConstructorNode, TableAcc) ->
		ConstructorData = milang_ast:data(ConstructorNode),
		ConstructorRes = case milang_ast_concrete:type(ConstructorData) of
			function ->
				{error, {constructor_cannot_be_function, ConstructorData}};
			data ->
				{ok, milang_ast_concrete:arg(ConstructorData)}
		end,

		ConcreteToConstructor = fun(ConcreteNode) ->
			ConcreteData = milang_ast:data(ConcreteNode),
			ConstructorName = milang_ast_concrete_data:name(ConcreteData),
			ConstructorArgsNodes = milang_ast_concrete_data:args(ConcreteData),
			{ConstructorName, ConstructorArgsNodes}
		end,

		AsConstructorRes = result:map(ConcreteToConstructor, ConstructorRes),

		ValidateConstructorArgs = fun({_ConstructorName, ConstructorArgsNodes}) ->
			lists:foldl(fun(N, TypesThusFar) ->
				result:map_n(fun(NodeType, Types) ->
					[NodeType | Types]
				end, [ fun() -> type_of_node(N, TableAcc) end, fun() -> TypesThusFar end ])
			end, {ok, []}, ConstructorArgsNodes)
		end,

		result:map_n(fun({{identifier_bound, ConstructorName}, _}, TypedArgs) ->
			Entry = #tv_constructor{ constructor_of = Name, args = TypedArgs},
			set_entry(ConstructorName, Entry, TableAcc)
		end, [ fun() -> AsConstructorRes end, lazy:func(fun result:map/2, [ValidateConstructorArgs, AsConstructorRes]) ])
	end,
	?LOG_DEBUG("So the constructors I got: ~p", [Constructors]),
	Out = lists:foldl(fun(CNode, TableRes) ->
		result:and_then(fun(InTable) ->
			ConstructorFoldFun(CNode, InTable)
		end, TableRes)
	end, {ok, MidTable}, Constructors),
	?LOG_DEBUG("validated a declaration_type: ~p~n"
		"    Out: ~p"
		, [Node, case Out of {ok, _} -> ok; _ -> Out end]),
	Out;

validate_node(milang_ast_alias, Node, Table) ->
	Data = milang_ast:data(Node),
	NameNode = milang_ast_alias:name(Data),
	Constraints = milang_ast_alias:constraints(Data),
	ArgNodes = milang_ast_alias:args(Data),
	Args = [{placeholder, identifier(N)} || N <- ArgNodes],
	OriginalNode = milang_ast_alias:original(Data),
	TypeRes = type_of_node(OriginalNode, Table),
	TableWithAliasRes = result:and_then(fun(Original) ->
		?LOG_DEBUG("the original: ~p", [Original]),
		Alias = #tv_alias{ arg_names = Args, constraints = Constraints, truename = Original },
		add_entry(identifier(NameNode), Alias, Table)
	end, TypeRes),
	% if we're aliasing a data type (like Maybe a), it's useful / needed to
	% localize the constructors as well ( if they have been exposed ).
	ConstructorsRes = result:map_n(fun(OriginalType, TableWithAlias) ->
		case OriginalType of
			#tv_concrete{ concrete_of = DataTypeName } ->
				get_constructors_for(DataTypeName, TableWithAlias);
			_ ->
				[]
		end
	end, [fun() -> TypeRes end, fun() -> TableWithAliasRes end] ),
	result:and_then(fun(Constructors) ->
		lists:foldl(fun
			({#{local := LocalName}, Constructor}, {ok, T}) ->
				add_entry(LocalName, Constructor, T);
			(_, T) ->
				T
		end, TableWithAliasRes, Constructors)
	end, ConstructorsRes);

validate_node(milang_ast_binding, Node, Table) ->
	Data = milang_ast:data(Node),
	validate_binding(Data, Table);

validate_node(milang_ast_teach, Node, Table) ->
	Data = milang_ast:data(Node),
	validate_teach(Data, Table);

validate_node(milang_ast_class, Node, Table) ->
	Data = milang_ast:data(Node),
	validate_class(Data, Table);

validate_node(_, Node, Table) ->
	?LOG_ERROR("Cannot validate a non-top level node: ~p", [Node]),
	{error, {cannot_validate_non_declarations, Node, Table}}.

validate_binding(Binding, Table) ->
	MatchNode = milang_ast_binding:match(Binding),
	Expression = milang_ast_binding:expression(Binding),
	MaybeExpressionType = type_of_node(Expression, Table),
	MaybeExisting = lookup(identifier(MatchNode), Table),
	validate_binding(MatchNode, MaybeExpressionType, MaybeExisting, Table).

validate_binding(_, {error, _} = Error, _, _) ->
	Error;
validate_binding({identifier_bound, _Name}, {ok, Type}, {ok, ExistingType}, Table) ->
	case check_type_match(Type, ExistingType, Table) of
		{ok, NewTable} ->
			{ok, NewTable};
		Error ->
			Error
	end;
validate_binding({identifier_bound, Name}, {ok, Type}, _NotFound, Table) ->
	add_entry(identifier(Name), Type, Table);
validate_binding({identifier_ignored, _}, _AnyType, _NeverFound, Table) ->
	{ok, Table};
%validate_binding({class_taught, _StudentName, _ClassName, _Binding} = Id, {ok, Implementation}, {ok, Spec}, Table) ->
%	case check_type_match(Implementation, Spec, Table) of
%		{ok, NewTable} ->
%			add_entry(Id, Spec, NewTable);
%		Error ->
%			?LOG_ERROR("class_taught binding ~p failed to register due to ~p", [Id, Error]),
%			Error
%	end;
validate_binding(Match, {ok, Type}, _NoMatter, _Table) ->
	?LOG_DEBUG("binding failed to validate as we don't support the match ~p for type ~p", [Match, Type]),
	{error, {validate_binding, bad_match_node, Match, Type}}.

validate_spec(Data, Table) ->
	Name = milang_ast_spec:name(Data),
	Spec = case milang_ast_spec:type(Data) of
		{ok, T} -> T;
		U -> U
	end,
	?LOG_DEBUG("Spec: ~p", [Spec]),
	ResultSpecType = type_of_node(Spec, Table),
	?LOG_DEBUG("ResultSpecType: ~p", [ResultSpecType]),
	ResultAddEntry = result:and_then(fun(Type) ->
		add_entry(identifier(Name), Type, Table)
	end, ResultSpecType),
	Out = ResultAddEntry,
	?LOG_DEBUG("validating a declaration spec: ~p~n"
		"    Output: ~p"
		, [ Data, case Out of {ok, _} -> ok; _ -> Out end]),
	Out.

validate_teach(Data, Table) ->
	StudentNode = milang_ast_teach:student(Data),
	ClassNode = milang_ast_teach:class(Data),
	Bindings = milang_ast_teach:bindings(Data),
	StudentNameRes = case milang_ast:data(StudentNode) of
		{identifier_type, StudentName} ->
			{ok, StudentName};
		_ ->
			{error, {invalid_student_name, StudentNode}}
	end,
	StudentTypeRes = result:and_then(fun(Name) ->
		case lookup(Name, Table) of
			{ok, #tv_data{}} = Ok ->
				Ok;
			{ok, Wut} ->
				{error, {not_a_data, Name, Wut}};
			Else ->
				Else
		end
	end, StudentNameRes),
	ClassNameRes = case milang_ast:data(ClassNode) of
		{identifier_type, ClassName} ->
			{ok, ClassName};
		_ ->
			{error, {invalid_class_name, ClassNode}}
	end,
	ClassTypeRes = result:and_then(fun(Name) ->
		case lookup(Name, Table) of
			{ok, #tv_class{}} = Ok -> Ok;
			{ok, Wut} -> {error, {not_a_class, Name, Wut}};
			Else -> Else
		end
	end, ClassNameRes),
	AddClassToStudentTypeRes = result:and_then_n(fun(StudentName, StudentType, ClassName, ClassType) ->
		#tv_data{ classes_learned = Learned } = StudentType,
		#tv_class{ prereq_classes = Prereqs } = ClassType,
		case ordsets:is_subset(Prereqs, Learned) of
			true ->
				NewLearned = ordsets:add_element(ClassName, Learned),
				NewStudent = StudentType#tv_data{ classes_learned = NewLearned },
				NewTable = set_entry(StudentName, NewStudent, Table),
				{ok, NewTable};
			false ->
				?LOG_ERROR("~p has not learned enough to learn ~p. It has learned ~p", [StudentName, ClassName, Learned]),
				{error, {missing_prereqs, ordsets:subtract(Prereqs, Learned)}}
		end
	end, [fun() -> StudentNameRes end, fun() -> StudentTypeRes end, fun() -> ClassNameRes end, fun() -> ClassTypeRes end]),
	% we're only going to check for the class functions. Checking if there are
	% extras defined is a task for the linter, as is / will be ensuring we have
	% prerequeisits for defaults defined as well. TODO is that second part
	% appropriate for the linter? It might actually be done by the nature of the
	% type checker.
	BindingsAsMap = lists:foldl(fun(BindingNode, Acc) ->
		BindingData = milang_ast:data(BindingNode),
		MatchNode = milang_ast_binding:match(BindingData),
		{_, Match} = milang_ast:data(MatchNode),
		ExpressionNode = milang_ast_binding:expression(BindingData),
		%Expression = milang_ast:data(ExpressionNode),
		Acc#{ Match => ExpressionNode }
	end, #{}, Bindings),
	FinalRes = result:and_then_n(
		fun(StudentName, _StudentType, ClassName, ClassType, TableWithLearnedStudent) ->
			Defaults = ClassType#tv_class.default_implementations,
			Specs = ClassType#tv_class.member_specs,
			SpecList = maps:to_list(Specs),
			TypePlaceholder = ClassType#tv_class.type_variable,
			lists:foldl(fun({SpecName, Spec}, TableRes) ->
				HasDefaultRes = case maps:find(SpecName, Defaults) of
					error ->
						{error, no_default};
					{ok, _} = Ok1 ->
						Ok1
				end,
				HasBindingRes = case maps:find(SpecName, BindingsAsMap) of
					error ->
						{error, no_implementation};
					{ok, _} = Ok2 ->
						Ok2
				end,
				ImplementationRes = case {HasBindingRes, HasDefaultRes} of
					{{ok, _}, _} ->
						?LOG_DEBUG("Using the binding in teach: ~p", [HasBindingRes]),
						HasBindingRes;
					{_, {ok, _} } ->
						?LOG_DEBUG("Using the default in class: ~p", [HasDefaultRes]),
						HasDefaultRes;
					_ ->
						?LOG_ERROR("Likely our table or other lookups are screwed.~n"
							"    Defaults: ~p~n"
							"    BindingsAsMap: ~p~n"
							"    TableWithLearnedStudent: ~p"
							, [Defaults, BindingsAsMap, TableWithLearnedStudent]),
						{error, {no_implementation_nor_default, SpecName, ClassName, StudentName}}
				end,
				ImplementationTypeRes = result:and_then_n(fun(Implementation, InTable) ->
					type_of_node(Implementation, InTable)
				end, [fun() -> ImplementationRes end, fun() -> TableRes end]),
				TranslatedSpec = update_type_variable({placeholder, TypePlaceholder}, #tv_concrete{ concrete_of = StudentName }, Spec),
				?LOG_DEBUG("Spec translation done.~n    Original: ~p~n    New: ~p", [Spec, TranslatedSpec]),
				result:and_then_n(fun(Implementation, TableAcc) ->
					validate_binding({class_taught, StudentName, ClassName, SpecName}, {ok, Implementation}, {ok, TranslatedSpec}, TableAcc)
				end, [fun() -> ImplementationTypeRes end, fun() -> TableRes end])
			end, {ok, TableWithLearnedStudent}, SpecList)
		end, [fun() -> StudentNameRes end, fun() -> StudentTypeRes end, fun() -> ClassNameRes end, fun() -> ClassTypeRes end, fun() -> AddClassToStudentTypeRes end]),
	?LOG_DEBUG("Final teach result: ~p", [case FinalRes of {ok, _} -> ok; _ -> FinalRes end]),
	FinalRes.

validate_class(Data, Table) ->
	NameNode = milang_ast_class:name(Data),
	{_, Name} = milang_ast:data(NameNode),
	PreReqs = case milang_ast_class:constraints(Data) of
		undefined ->
			[];
		ConstraintsNode ->
			ConstraintsData = milang_ast:data(ConstraintsNode),
			Constraints = milang_ast_constraints:constraints(ConstraintsData),
			lists:map(fun(ConstraintNode) ->
				ConstraintData  = milang_ast:data(ConstraintNode),
				ConstraintClassNode = milang_ast_constraint:class(ConstraintData),
				{_, ConstraintName} = milang_ast:data(ConstraintClassNode),
				ConstraintName
			end, Constraints)
	end,
	[TypeVariableNode] = milang_ast_class:args(Data),
	{_, TypeVariable} = milang_ast:data(TypeVariableNode),
	MemberNodes = milang_ast_class:members(Data),
	{DefaultsAsList, SpecsAsList} = lists:foldl(fun(MemberNode, {DefaultAcc, SpecAcc}) ->
		MemberData = milang_ast:data(MemberNode),
		case milang_ast:type_simply(MemberNode) of
			spec ->
				SpecTypeNode = milang_ast_spec:type(MemberData),
				{ok, SpecType} = type_of_node(SpecTypeNode, Table),
				SpecName = milang_ast_spec:name(MemberData),
				{DefaultAcc, [{SpecName, SpecType} | SpecAcc]};
			binding ->
				BindingExpressionNode = milang_ast_binding:expression(MemberData),
				BindingNameNode = milang_ast_binding:match(MemberData),
				{_, BindingName} = milang_ast:data(BindingNameNode),
				% TODO ensure the binding / default matches the spec.
				%{ok, BindingExpressionType} = type_of_node(BindingExpressionNode, Table),
				{[{BindingName, BindingExpressionNode} | DefaultAcc], SpecAcc}
		end
	end, {[], []}, MemberNodes),
	TvClass = #tv_class{
		prereq_classes = ordsets:from_list(PreReqs),
		type_variable = TypeVariable,
		member_specs = maps:from_list(SpecsAsList),
		default_implementations = maps:from_list(DefaultsAsList)
	},

	result:and_then(fun(TableWithClass) ->
			lists:foldl(fun({SpecName, Spec}, SpecTableRes) ->
				result:and_then(fun(SpecTable) ->
					add_entry(SpecName, Spec#tv_function{ constraints = #{ TypeVariable => Name } }, SpecTable)
				end, SpecTableRes)
			end, {ok, TableWithClass}, SpecsAsList)
	end, add_entry(Name, TvClass, Table)).

-spec check_type_match(concrete(), concrete() | placeholder(), lookup_table()) -> {ok, lookup_table()} | {error, term()}.
check_type_match(A, B, C) ->
	?LOG_DEBUG("Checking type~n"
		"    Inferred: ~p~n"
		"    Known: ~p~n", [A, B]),
	check_type_match_(A, B, C).

-spec check_type_match_(concrete(), concrete() | placeholder(), lookup_table()) -> {ok, lookup_table()} | {error, term()}.
check_type_match_(_Known, {placeholder, _}, Table) ->
	{ok, Table};
check_type_match_({placeholder, _}, _Known, Table) ->
	{ok, Table};
check_type_match_(#tv_concrete{ concrete_of = OfA} = Known, #tv_concrete{ concrete_of = OfB} = Inferred, Table)
		when OfA =:= OfB; OfA =:= undefined; OfB =:= undefined ->
	KnownArgs = Known#tv_concrete.args,
	InferredArgs = Inferred#tv_concrete.args,
	check_type_list_match(KnownArgs, InferredArgs, Table);
check_type_match_(_Known, any, Table) ->
	{ok, Table};
check_type_match_(any, _Inferred, Table) ->
	{ok, Table};
check_type_match_(#tv_function{} = A, #tv_concrete{} = B, Table) ->
	KnownArgs = A#tv_function.args,
	InferredArgs = B#tv_concrete.args,
	check_type_list_match(KnownArgs, InferredArgs, Table);
check_type_match_(#tv_concrete{} = A, #tv_function{} = B, Table) ->
	KnownArgs = A#tv_concrete.args,
	InferredArgs = B#tv_function.args,
	check_type_list_match(KnownArgs, InferredArgs, Table);
check_type_match_(#tv_function{} = A, #tv_function{} = B, Table) ->
	% This is for when the type specifier is inferred, as opposed to the actual
	% type being inferred.
	KnownArgs = A#tv_function.args,
	InferredArgs = B#tv_function.args,
	check_type_list_match(KnownArgs, InferredArgs, Table);
check_type_match_(A, B, _Table) ->
	?LOG_ERROR("type mismatch marker.~n"
		"    A: ~p~n"
		"    B: ~p~n"
		"    Table: ~p"
		, [A, B, _Table]),
	{error, {type_mismatch, A, B}}.

check_type_list_match(_KnowListLengthGTE, [], Table) ->
	{ok, Table};
check_type_list_match([], _InferredListTooLong, Table) ->
	{ok, Table};
check_type_list_match([ any | KnownTail], [_Inferred | InferredTail], Table) ->
	check_type_list_match(KnownTail, InferredTail, Table);
check_type_list_match([ _Known | KnownTail], [ any | InferredTail], Table) ->
	check_type_list_match(KnownTail, InferredTail, Table);
check_type_list_match([{type_variable, _} = KnownVariableName | KnownTail], [Inferred | InferredTail], Table) ->
	NewKnown = update_type_variable(KnownVariableName, Inferred, KnownTail),
	check_type_list_match(NewKnown, InferredTail, Table);
check_type_list_match([Known | KnownTail], [{type_variable, _} = Variable | InferredTail], Table) ->
	NewInferred = update_type_variable(Variable, Known, InferredTail),
	check_type_list_match(KnownTail, NewInferred, Table);
check_type_list_match([Known | KnownTail], [Inferred | InferredTail], Table) ->
	case check_type_match(Known, Inferred, Table) of
		{ok, NewTable} ->
			check_type_list_match(KnownTail, InferredTail, NewTable);
		Error ->
			Error
	end.

-spec update_type_variable(mi_identifier() | placeholder(), concrete(), concrete() | [ concrete() ]) -> concrete() | [ concrete() ].
update_type_variable(TypeVariable, Replacement, List) when is_list(List) ->
	lists:map(fun(E) ->
		update_type_variable(TypeVariable, Replacement, E)
	end, List);
update_type_variable(TypeVariable, Replacement, TypeVariable) ->
	Replacement;
update_type_variable(TypeVariable, Replacement, Map) when is_map(Map) ->
	maps:map(fun(_K, E) ->
		update_type_variable(TypeVariable, Replacement, E)
	end, Map);
update_type_variable(_TypeVariable, _Replacement, {type_variable, _} = Keeper) ->
	Keeper;
update_type_variable(TypeVariable, Replacement, Tuple) when is_tuple(Tuple) ->
	AsList = tuple_to_list(Tuple),
	UpdatedList = update_type_variable(TypeVariable, Replacement, AsList),
	list_to_tuple(UpdatedList);
update_type_variable(_TypeVariable, _Replacement, Term) ->
	Term.

-spec type_of_node(milang_ast:ast_node(), lookup_table()) -> {ok, concrete() | mi_identifier()} | {error, term()}.
type_of_node(Node, Table) ->
	?LOG_DEBUG("type of node ~p", [Node]),
	Type = milang_ast:type_simply(Node),
	Out = type_of_node(Type, Node, Table),
	?LOG_DEBUG("And the type is ~p", [Out]),
	Out.

type_of_node(milang_ast_concrete, Node, Table) ->
	Data = milang_ast:data(Node),
	ArgNode = milang_ast_concrete:arg(Data),
	type_of_node(ArgNode, Table);

type_of_node(milang_ast_concrete_data, Node, Table) ->
	Data = milang_ast:data(Node),
	NameNode = milang_ast_concrete_data:name(Data),
	Name = identifier(NameNode),
	Args = milang_ast_concrete_data:args(Data),
	ResultArgTypesReversed = lists:foldl(fun(Arg, Acc) ->
		result:and_then(fun(GoodAcc) ->
			result:map(fun(GotType) ->
				[GotType | GoodAcc]
			end, type_of_node(Arg, Table))
		end, Acc)
	end, {ok, []}, Args),
	ResultArgTypes = result:map(fun lists:reverse/1, ResultArgTypesReversed),
	% a concrete can be for either an alias or a data (type). To make type checks
	% easier going forward, if we get an alias, we merge the concrete the alias
	% uses with the one we got, and check them.
	% a rough-cut of it. Its up to the validator or type check part to actually
	% figure out the type proper.
	ResultLookup = lookup(Name, Table),
	result:and_then_n(partial:func(fun(ArgTypes, Resolved) ->
		case Resolved of
			#tv_alias{} ->
				{ok, alias_as_concrete(Resolved, ArgTypes)};
			#tv_data{} ->
				{ok, #tv_concrete{ concrete_of = Name, args = ArgTypes}}
		end
	end), [fun() -> ResultArgTypes end, fun() -> ResultLookup end]);

type_of_node(milang_ast_concrete_function, Node, Table) ->
	Data = milang_ast:data(Node),
	HeadNode = milang_ast_concrete_function:head(Data),
	TailNodes = milang_ast_concrete_function:tail(Data),
	HeadResult = type_of_node(HeadNode, Table),
	TailResultsReversed = lists:foldl(fun(ArgNode, Acc) ->
		result:and_then(fun(GoodAcc) ->
			result:map(fun(GoodTail) ->
				[GoodTail | GoodAcc]
			end, type_of_node(ArgNode, Table))
		end, Acc)
	end, {ok, []}, TailNodes),
	TailResult = result:map(fun(L) ->
		lists:reverse(L)
	end, TailResultsReversed),
	result:map_n(partial:func(fun(Head, Tail) ->
		#tv_concrete{concrete_of = undefined, args = [ Head | Tail]}
	end), [fun() -> HeadResult end, fun() -> TailResult end]);

type_of_node(identifier_ignored, _, _Table) ->
	{ok, any};
type_of_node(identifier_type, Node, Table) ->
	{identifier_type, Name} = milang_ast:data(Node),
	case lookup(Name, Table) of
		{ok, #tv_alias{} = Alias} ->
			{ok, alias_as_concrete(Alias, [])};
		{ok, #tv_data{} = Data} ->
			% the most likely thing here is we're in a spec or type declaration,
			% and this is the first and only step in building up a concrete.
			Args = Data#tv_data.arg_names,
			Concrete = #tv_concrete{ concrete_of = Name, args = Args },
			{ok, Concrete};
		{ok, _} = Ok -> Ok;
		NotOk -> NotOk
	end;
	% TODO turns out not being able to know if an identifier is for something
	% that should exist (name for a type) vs something that will be filled in
	% later (type variable) can make type inference / lexing very very hard.
	% so the real TODO is: what do we determine the syntax / textual difference
	% between a type name vs a type variable?
	%
	% there are several possibilities here. An identifier can be anything, a
	% variable in a type to be used later, or the name of an existing type.
	% We'll just have to lookup and shrug if we don't find it.
	% in theory, the linter will find things like unused variable names and
	% shadowed / duplicate names.
type_of_node(identifier_bound, Node, Table) ->
	% this is a less formal identifier, likely an type variable. Still,
	% everything from above still applies.
	Id = identifier(milang_ast:data(Node)),
	case lookup(Id, Table) of
		{ok, _} = OK -> OK;
		_NotOk -> {ok, {placeholder, Id}}
	end;

type_of_node(milang_ast_call, Node, Table) ->
	% There are a few things going on here.
	% 1st: any function that can be replaced with the result of the function
	%      and pass type checking. In essence, the function `String -> Int`
	%      when applied to a string has the type Int.
	% 2nd: Functions can be paritailly applied, and when done so, the paramters
	%      left over form a new function type.
	% So we'll expand the 2nd here.
	% Given a function `f : a -> b -> c -> d`, the expression `f 1 2` has the
	% type `c -> d`. In other words, the partial application means we are left
	% with a shorted function. Thus, the 'ResultChoppedArgs' below.
	%
	% Now if we apply a final value to our f above, we are left with a type `d`.
	% so a fully applied f has a type d. This feeds into the 1st point.
	%
	% To make sure this works, the `type_of_node` _must_ do some type checking,
	% and fill in data if missing.
	Data = milang_ast:data(Node),
	Args = milang_ast_call:args(Data),
	% TODO not all function names follow the pattern of {name_type(), mi_identifier()}.
	% hopefully this will get fixed.
	FunctionExpression = milang_ast_call:function(Data),
	BaseTypeResult = case milang_ast:type_simply(FunctionExpression) of
		milang_ast_function ->
			type_of_node(FunctionExpression, Table);
		milang_ast_expression ->
			type_of_node(FunctionExpression, Table);
		{_NameType, CallName} ->
			lookup(CallName, Table);
		identifier_bound ->
			{identifier_bound, CallName} = milang_ast:data(FunctionExpression),
			lookup(CallName, Table);
		_SomethingElse ->
			{error, {not_a_function, FunctionExpression}}
	end,
	?LOG_DEBUG("Function expression: ~p; args: ~p", [FunctionExpression, Args]),
	ResultArgTypes = result:and_then_all(fun(ArgNode) ->
		type_of_node(ArgNode, Table)
	end, Args),
	ResultLong = result:map(fun(ArgTypes) ->
		#tv_function{ args = ArgTypes }
	end, ResultArgTypes),
	ResultExisting = result:and_then(fun
		(#tv_function{} = A) ->
			{ok, A};
		({placeholder, _A}) ->
			% TODO is this right?
			% the difference between a literal value and a function that takes
			% no args but returns a literal value is: nothing. There is no
			% difference.
			{ok, #tv_function{}};
		(#tv_constructor{ } = Constructor) ->
			DataName = Constructor#tv_constructor.constructor_of,
			ConstructorArgs = Constructor#tv_constructor.args,
			{ok, DataType} = lookup(DataName, Table),
			Concrete = #tv_concrete{ concrete_of = DataName, args = DataType#tv_data.arg_names },
			Function = #tv_function{ args = ConstructorArgs ++ [ Concrete ]},
			{ok, Function};
		(#tv_data{ has_constructors = false} = TvData) ->
			Concrete = #tv_concrete{ concrete_of = FunctionExpression, args = TvData#tv_data.arg_names },
			Function = #tv_function{ args = TvData#tv_data.arg_names ++ [ Concrete]},
			{ok, Function};
		(#tv_concrete{} = Concrete) when Args =:= [] ->
			% this is likely the result of a let binding resolving to a fully
			% called function. eg:
			%     let add = function a b -> a '+ b..
			%     let sum = add 5 3.
			% the type of 'sum' will be Integer. However, if we then use it
			% as follows:
			%     let f = function a b ->
			%         let sum = add 5 3.
			%         sum '+ 7..
			% The lexxer says "oh, sum must be a call". Well, it's not
			% technically wrong. However, the difference between a function with
			% no args and the value it generates is _none_, therefore, our type
			% validation should assumed the same.
			{ok, #tv_function{ args = [Concrete]}};
		(#tv_concrete{} = Concrete) ->
			% there are some valid lexical structures that will appears as
			% concretes to the lexer, but are also valid constructors. An Exmaple
			% is a simple 'Tuple' type defined as:
			% type Tuple a b.
			% The type has no constructors defined because it would have exactly
			% one: itself. So, we need to lookup the data type and see if this
			% concrete is actually a single constructor.
			DataName = Concrete#tv_concrete.concrete_of,
			{ok, DataType} = lookup(DataName, Table),
			case DataType#tv_data.has_constructors of
				false ->
					Function = #tv_function{ args = DataType#tv_data.arg_names ++ [ Concrete ]},
					{ok, Function};
				true ->
					{error, {not_a_function, FunctionExpression, Concrete}}
			end;
		(Actual) ->
			{error, {not_a_function, FunctionExpression, Actual}}
	end, BaseTypeResult),
	ResultTypeCheck = result:and_then_n(fun(Known, Inferred) ->
		check_type_match(Known, Inferred, Table)
	end, [fun() -> ResultExisting end, fun() -> ResultLong end]),
	ResultChoppedArgs = result:and_then_n(fun(TypeCheck, Known, Inferred) ->
		InferredArgs = Inferred#tv_function.args,
		KnownArgs = Known#tv_function.args,
		if
			length(InferredArgs) > length(KnownArgs) ->
				?LOG_ERROR("Inferred args ended up longer than the known args somehow.~n"
					"    TypeCheck: ~p~n"
					"    Known: ~p~n"
					"    Inferred: ~p"
					,[ TypeCheck, Known, Inferred]),
				{error, {too_many_args, Known, Inferred}};
			true ->
				{_, NewArgs} = lists:split(length(Inferred#tv_function.args), Known#tv_function.args),
				{ok, Inferred#tv_function{ args = NewArgs }}
		end
	end, [fun() -> ResultTypeCheck end, fun() -> ResultExisting end, fun() -> ResultLong end]),
	result:map(fun(MinimalFunction) ->
		case MinimalFunction#tv_function.args of
			[Singleton] ->
				Singleton;
			_ ->
				MinimalFunction
		end
	end, ResultChoppedArgs);
type_of_node(string, _Node, _Table) ->
	{ok, string_type()};
type_of_node(literal_float, _Node, _Table) ->
	{ok, float_type()};
type_of_node(literal_integer, _Node, _Table) ->
	{ok, integer_type()};

type_of_node(literal_list, Node, Table) ->
	{literal_list, ElementNodes} = milang_ast:data(Node),
	ElementTypeRes = lists:foldl(fun
		(_ElementNode, {error, _} = Acc) ->
			Acc;
		(ElementNode, Acc) ->
			ElementType = type_of_node(ElementNode, Table),
			case {ElementType, Acc} of
				{{ok, _}, placeholder} ->
					ElementType;
				{{ok, T1}, {ok, T2}} ->
					case check_type_match(T1, T2, Table) of
						{ok, _} ->
							Acc;
						Error ->
							Error
					end;
				{Error, _} ->
					Error
			end
	end, placeholder, ElementNodes),
	case ElementTypeRes of
		{ok, T} ->
			{ok, list_type(T)};
		placeholder ->
			% TODO perhaps this should have a name?
			{ok, any};
		_ ->
			ElementTypeRes
	end;


type_of_node(milang_ast_function, Node, BaseTable) ->
	Table = [#{} | BaseTable],
	Data = milang_ast:data(Node),

	% the args are serving 2 functions here.
	% each arg will (eventually) map to a type, so a simple arg is a name
	% placeholder. However, an arg could also be a pattern match of a record.
	% this means it also provides some type hints, and has _multiple_ name
	% placeholders. We haven't really touched on records yet, so I think I
	% can skip that for now.
	% TODO get arg -> type mapping, and then load that into the type table.
	ArgNodes = milang_ast_function:args(Data),
	ReversedArgsListAndTableRes = lists:foldl(fun(ArgNode, Acc) ->
		TypeRes = result:and_then(fun({_, TableAcc}) ->
			type_of_node(ArgNode, TableAcc)
		end, Acc),
		result:and_then_n(fun({ArgTypes, TableAcc}, T) ->
			{ok, {[{ArgNode, T} | ArgTypes], TableAcc}}
		end, [fun() -> Acc end, fun() -> TypeRes end])
	end, {ok, {[], Table}}, ArgNodes),
	{ReversedArgsListRes, TableRes} = case ReversedArgsListAndTableRes of
		{ok, {ArgsListReversed, AccTable}} ->
			{{ok, ArgsListReversed}, {ok, AccTable}};
		_ ->
			{ReversedArgsListAndTableRes, ReversedArgsListAndTableRes}
	end,
	MaybeArgsList = result:map(fun lists:reverse/1, ReversedArgsListRes),
	AddNode = fun(ArgNode, Type, OldTable) ->
		Id = identifier(ArgNode),
		add_entry(Id, Type, OldTable)
	end,
	AddNodes = fun(ArgList) ->
		lists:foldl(fun({ArgNode, Type}, TableResAcc) ->
			result:and_then(fun(OldTable) ->
				AddNode(ArgNode, Type, OldTable)
			end, TableResAcc)
		end, TableRes, ArgList)
	end,
	MaybeTableWithArgs = result:and_then(fun(ArgList) ->
		AddNodes(ArgList)
	end, MaybeArgsList),

	BindNodes = milang_ast_function:binds(Data),
	MaybeTableWithBinds = lists:foldl(fun(BindNode, Acc) ->
		BindData = milang_ast:data(BindNode),
		MatchNode = milang_ast_binding:match(BindData),
		ExprNode = milang_ast_binding:expression(BindData),
		TypeRes = result:and_then(fun(T) ->
			type_of_node(ExprNode, T)
		end, Acc),
		result:and_then_n(fun(Type, AccTable) ->
			Id = identifier(MatchNode),
			add_entry(Id, Type, AccTable)
		end, [fun() -> TypeRes end, fun() -> Acc end])
	end, MaybeTableWithArgs, BindNodes),

	ExprNode = milang_ast_function:expression(Data),
	ExprRes = result:and_then(fun(TableAcc) ->
		type_of_node(ExprNode, TableAcc)
	end, MaybeTableWithBinds),
	result:map_n(fun(Args, Res) ->
		ArgTypes = lists:map(fun({_FullArgNode, T}) ->
			T
		end, Args),
		FullType = [Res | ArgTypes],
		#tv_function{ args = lists:reverse(FullType)}
	end, [fun() -> ReversedArgsListRes end, fun() -> ExprRes end]);

type_of_node(milang_ast_infix_tree, Node, Table) ->
	Data = milang_ast:data(Node),
	NotationNode = milang_ast_infix_tree:notation(Data),
	LeftNode = milang_ast_infix_tree:left(Data),
	RightNode = milang_ast_infix_tree:right(Data),

	Function = milang_ast_infix_notation:function(milang_ast:data(NotationNode)),
	Args = [LeftNode, RightNode],
	CallNode = milang_ast:transform_data(fun(_) ->
		milang_ast_call:new(Function, Args)
	end, Node),
	type_of_node(CallNode, Table);

type_of_node(milang_ast_infix_series, Node, Table) ->
	case milang_ast_infix:as_tree(Node) of
		{ok, AsTree} ->
			type_of_node(AsTree, Table);
		TreeConverstionFailed ->
			{error, {infix_series_invalid, TreeConverstionFailed}}
	end;

type_of_node(milang_ast_match, Node, Table) ->
	Data = milang_ast:data(Node),
	Expression = milang_ast_match:expression(Data),
	ExpressionTypeRes = type_of_node(Expression, Table),
	Clauses = milang_ast_match:clauses(Data),
	ClauseTypes = lists:map(fun(Clause) ->
		type_of_node(Clause, Table)
	end, Clauses),
	ClauseHeads = lists:map(fun(Clause) ->
		ClauseData = milang_ast:data(Clause),
		Head = milang_ast_match_clause:match(ClauseData),
		type_of_node(Head, Table)
	end, Clauses),
	ClauseHeadsRes = lists:foldl(fun
		({ok, HeadType}, {ok, List}) ->
			{ok, List ++ [HeadType]};
		(_, {error, _} = Error) ->
			Error;
		({error, _} = Error, _) ->
			Error
	end, {ok, []}, ClauseHeads),
	ExprAndHeadMatchRes = result:and_then_n(fun(ExprType, HeadTypes) ->
		lists:foldl(fun(HeadType, AccTable) ->
			result:and_then(fun(_) ->
				check_type_match(ExprType, HeadType, Table)
			end, AccTable)
		end, {ok, Table}, HeadTypes)
	end, [fun() -> ExpressionTypeRes end, fun() -> ClauseHeadsRes end]),

	ClauseMatchRes = lists:foldl(fun
		({ok, ClauseType}, {ok, List}) ->
			{ok, List ++ [ClauseType]};
		(_, {error, _} = Error) ->
			Error;
		({error, _} = Error, _) ->
			Error
	end, {ok, []}, ClauseTypes),

	AllClauseMatchesRes = case ClauseMatchRes of
		{ok, []} ->
			{error, {match_neads_minimum_1_clause, Node}};
		{error, _} = Error ->
			Error;
		{ok, [ Clause1 | ClauseTail]} ->
			lists:foldl(fun(ClauseElement, TableAcc) ->
				result:and_then(fun(_) ->
					check_type_match(Clause1, ClauseElement, Table)
				end, TableAcc)
			end, {ok, Table}, ClauseTail)
	end,
	result:map_n(fun(_, _, [Clause1 | _]) ->
		Clause1
	end, [fun() -> AllClauseMatchesRes end, fun() -> ExprAndHeadMatchRes end, fun() -> ClauseMatchRes end]);

type_of_node(milang_ast_match_clause, Node, Table) ->
	Data = milang_ast:data(Node),
	Head = milang_ast_match_clause:match(Data),
	Binds = milang_ast_match_clause:binds(Data),
	Expression = milang_ast_match_clause:expression(Data),

	NewScopedTable = [#{} | Table],

	HeadBindsTableRes = match_binds(Head, NewScopedTable),

	WithBindsTableRes = result:and_then(fun(HeadTable) ->
		lists:foldl(fun(BindNode, AccTableRes) ->
			result:and_then(fun(AccTable) ->
				BindData = milang_ast:data(BindNode),
				MatchNode = milang_ast_binding:match(BindData),
				ExprNode = milang_ast_binding:expression(BindData),
				TypeRes = type_of_node(ExprNode, AccTable),
				result:and_then(fun(Type) ->
					Id = identifier(MatchNode),
					add_entry(Id, Type, AccTable)
				end, TypeRes)
			end, AccTableRes)
		end, {ok, HeadTable}, Binds)
	end, HeadBindsTableRes),
	result:and_then(fun(BindTable) ->
		type_of_node(Expression, BindTable)
	end, WithBindsTableRes);

type_of_node(match_type, Node, Table) ->
	{match_type, NameNode, Args} = milang_ast:data(Node),
	{_, Name} = milang_ast:data(NameNode),
	ArgTypesRes = lists:foldl(fun
		(_ArgNode, {error, _} = Acc) ->
			Acc;
		(ArgNode, {ok, TList}) ->
			case type_of_node(ArgNode, Table) of
				{ok, T} ->
					{ok, TList ++ [T]};
				Error ->
					Error
			end
	end, {ok, []}, Args),
	PrimaryTypeRes = lookup(Name, Table),
	MatchableTypeRes = result:and_then(fun(PrimaryType) ->
		case PrimaryType of
			#tv_data{ has_constructors = true } ->
				{error, match_only_constructors};
			#tv_data{} ->
				{ok, PrimaryType};
			#tv_constructor{} ->
				{ok, PrimaryType};
			_ ->
				{error, {match_only_constructors, PrimaryType}}
		end
	end, PrimaryTypeRes),
	result:and_then_n(fun(MatchableType, ArgTypes) ->
		case MatchableType of
			#tv_data{ arg_names = Names } ->
				if
					length(Names) == length(ArgTypes) ->
						{ok, #tv_concrete{ concrete_of = Name, args = ArgTypes }};
					length(Names) > length(ArgTypes) ->
						{error, {not_enough_args, Name, length(Names), ArgTypes}};
					true ->
						{error, {too_many_args, Name, length(Names), ArgTypes}}
				end;
			#tv_constructor{ args = ConstructorArgs, constructor_of = DataTypeName} ->
				{ok, DataType} = lookup(DataTypeName, Table),
				DataNames = DataType#tv_data.arg_names,
				if
					length(ArgTypes) == length(ConstructorArgs) ->
						Zipped = lists:zip(ConstructorArgs, ArgTypes),
						DataArgs = lists:map(fun(DataName) ->
							case proplists:get_value(DataName, Zipped) of
								#tv_concrete{} = Concrete ->
									Concrete;
								undefined ->
									any;
								NewArgName ->
									{placeholder, NewArgName}
							end
						end, DataNames),
						{ok, #tv_concrete{ concrete_of = DataTypeName, args = DataArgs}};
					length(ArgTypes) < length(ConstructorArgs) ->
						{error, {too_many_args, Name, length(ConstructorArgs), ArgTypes}};
					true ->
						{error, {not_enough_args, Name, length(ConstructorArgs), ArgTypes}}
				end
		end
	end, [fun() -> MatchableTypeRes end, fun() -> ArgTypesRes end]);

type_of_node(match_list, Node, Table) ->
	{match_list, MatchElements} = milang_ast:data(Node),
	case MatchElements of
		[] ->
			{ok, list_type(any)};
		_ ->
			ElementType = lists:foldl(fun
				(Element, undefined) ->
					type_of_node(Element, Table);
				(_Element, {error, _} = Error) ->
					Error;
				(Element, {ok, Type}) ->
					case type_of_node(Element, Table) of
						{ok, NewType} ->
							case check_type_match(Type, NewType, Table) of
								{ok, _} ->
									{ok, Type};
								Error ->
									Error
							end;
						Error ->
							Error
					end
			end, undefined, MatchElements),
			case ElementType of
				{ok, T} ->
					{ok, list_type(T)};
				Error ->
					Error
			end
	end;

type_of_node(match_list_head, Node, Table) ->
	{match_list_head, Heads, _Tail} = milang_ast:data(Node),
	FakeData = {match_list, Heads},
	FakeNode = milang_ast:transform_data(fun(_) -> FakeData end, Node),
	type_of_node(match_list, FakeNode, Table);

type_of_node(milang_ast, Node, Table)  ->
	Data = milang_ast:data(Node),
	Type = milang_ast:type_simply(Data),
	type_of_node(Type, Data, Table);

type_of_node(_, Node, Table) ->
	{error, {unable_to_determine_type_from_node, Node, Table}}.

string_type() ->
	#tv_concrete{ concrete_of = #{ module => <<"Core">>, local => <<"String">> } }.

integer_type() ->
	#tv_concrete{ concrete_of = #{ module => <<"Core">>, local => <<"Integer">> } }.

float_type() ->
	#tv_concrete{ concrete_of = #{ module => <<"Core">>, local => <<"Float">> } }.

list_type(Type) ->
	#tv_concrete{ args = [Type], concrete_of = #{ module => <<"List">>, local => <<"List">> } }.

alias_as_concrete(Alias, ArgTypes) ->
	?LOG_DEBUG("Converting alias ~p with types ~p", [Alias, ArgTypes]),
	ArgNames = Alias#tv_alias.arg_names,
	Original = Alias#tv_alias.truename,
	NameToType = maps:from_list(lists:zip(ArgNames, ArgTypes)),
	ConcreteArgs = Original#tv_concrete.args,
	FixedArgs = lists:map(fun
		({placeholder, _} = N) ->
			case maps:find(N, NameToType) of
				{ok, T} ->
					T;
				error ->
					N
			end;
		(E) ->
			E
	end, ConcreteArgs),
	Original#tv_concrete{ args = FixedArgs }.

match_binds(Node, Table) ->
	match_binds(milang_ast:type_simply(Node), milang_ast:data(Node), Table).

match_binds(milang_ast, Node, Table) ->
	Type = milang_ast:type_simply(Node),
	Data = milang_ast:data(Node),
	match_binds(Type, Data, Table);

match_binds(literal_integer, _, Table) ->
	{ok, Table};

match_binds(literal_float, _, Table) ->
	{ok, Table};

match_binds(literal_string, _, Table) ->
	{ok, Table};

match_binds(match_type, Data, Table) ->
	{match_type, _Name, Args} = Data,
	lists:foldl(fun(Arg, TableRes) ->
		result:and_then(fun(T) ->
			match_binds(Arg, T)
		end, TableRes)
	end, {ok, Table}, Args);

match_binds(match_list, {match_list, []}, Table) ->
	{ok, Table};

match_binds(match_list, {match_list, Matches}, Table) ->
	lists:foldl(fun(Match, TableRes) ->
		result:and_then(fun(AccTable) ->
			match_binds(Match, AccTable)
		end, TableRes)
	end, {ok, Table}, Matches);

match_binds(match_list_head, {match_list_head, Matches, TailMatch}, Table) ->
	HeadMatchRes = lists:foldl(fun(Match, TableRes) ->
		result:and_then(fun(TableAcc) ->
			match_binds(Match, TableAcc)
		end, TableRes)
	end, {ok, Table}, Matches),
	result:and_then(fun(TableAcc) ->
		match_binds(TailMatch, TableAcc)
	end, HeadMatchRes);

match_binds(identifier_ignored, _, Table) ->
	{ok, Table};

match_binds(identifier_bound, Data, Table) ->
	{identifier_bound, Name} = Data,
	add_entry(Name, {placeholder, Name}, Table).


identifier(Name) when is_binary(Name) ->
	Name;
identifier(Name) when is_map(Name) ->
	Name;
identifier({_, Name}) ->
	identifier(Name);
identifier(Name) when is_list(Name) ->
	case unicode:characters_to_binary(Name) of
		Encoded when is_binary(Encoded) ->
			Encoded;
		NotEncoded ->
			error({non_unicode_name, Name, NotEncoded})
	end;
identifier(Node) ->
	identifier(milang_ast:data(Node)).

get_constructors_for(Name, Table) ->
	get_constructors_for(Name, Table, []).

get_constructors_for(_Name, [], Acc) ->
	lists:reverse(Acc);
get_constructors_for(Name, [Table | Tail], Acc) ->
	NewAcc = maps:fold(fun(Key, Value, InAcc) ->
		case Value of
			#tv_constructor{ constructor_of = Name} ->
				[{Key, Value} | InAcc];
			_ ->
				InAcc
		end
	end, Acc, Table),
	get_constructors_for(Name, Tail, NewAcc).

-spec add_entry(mi_identifier(), entry(), lookup_table()) -> {ok, lookup_table()} | {error, {shadowing, entry()}}.
add_entry(Name, Entry, Table) ->
	case lookup(Name, Table) of
		{error, {notfound, Name}} ->
			NewTable = set_entry(Name, Entry, Table),
			{ok, NewTable};
		{ok, Entry} ->
			% meh, it's an exact match so we're all good.
			{ok, Table};
		{ok, ExistingEntry} ->
			{error, {shadowing, Name, ExistingEntry}}
	end.

-spec set_entry(mi_identifier(), entry(), lookup_table()) -> lookup_table().
set_entry(Name, Entry, Table) ->
	[OldHead | Tail] = Table,
	NewHead = maps:put(Name, Entry, OldHead),
	[NewHead | Tail].

-spec lookup(mi_identifier(), lookup_table()) -> {error, {notfound, mi_identifier()}} | {ok, entry()}.
lookup(Name, [Table | Tail]) ->
	case maps:find(Name, Table) of
		error when Tail =:= [] ->
			?LOG_DEBUG("Did not find ~p", [Name]),
			{error, {notfound, Name}};
		error ->
			lookup(Name, Tail);
		{ok, _} = Ok ->
			Ok
	end.

