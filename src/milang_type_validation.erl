-module(milang_type_validation).

-export(
	[ new/0
	, validate_list/2
	]).

-include("milang_ast.hrl").
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

-type mi_identifier() :: milang_ast_identifier:milang_identifier().
-type maybe_identifier() :: undefined | milang_ast_identifier:milang_identifier().

%-type type_any() :: 'any'.

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
	args = []
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

-record(tv_class_function,
	{ of_class
	}).
-type class_function() :: #tv_class_function{}.

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

-type entry() :: concrete() | data() | constructor() | alias() | placeholder().

-type lookup_table() :: nonempty_list( #{ mi_identifier() => entry()} ).

-export_type(
	[ concrete/0
	, data/0
	, constructor/0
	, alias/0
	, class/0
	, class_function/0
	, entry/0
	, lookup_table/0
	, mi_identifier/0
	, maybe_identifier/0]).

-spec new() -> lookup_table().
new() ->
	[#{}].

-spec validate_list([ milang_ast:ast_node() ], lookup_table()) -> {ok, lookup_table()} | {error, term()}.
validate_list(Nodes, Table) ->
	FoldFun = fun validate_node/2,
	'Result':foldl(FoldFun, Table, Nodes).

-spec validate_node(milang_ast:ast_node(), lookup_table()) -> {ok, lookup_table()} | {error, term()}.
validate_node(Node, Table) ->
	Type = milang_ast:type_simply(Node),
	?LOG_DEBUG("validate node of type ~p (full node: ~p)", [Type, Node]),
	validate_node(Type, Node, Table).

validate_node(module, _Node, Table) ->
	?LOG_DEBUG("Easy validating a module declaration.", []),
	{ok, Table};
validate_node(import, _Node, Table) ->
	% TODO currenly import does not even allow alias of the module, nor quick
	% expose of the functions. It's further up to the caller to load the table
	% with the header data found for the import. So for now, this is a quicky
	% pass.
	?LOG_DEBUG("Easy validating an import declaration.", []),
	{ok, Table};
%validate_node(import, Node, Table) ->
%	% it is up to the caller to have loaded the table with the header data via
%	% previous validate_list calls. This just sets the name of the imports to
%	% be aliased to the remote name.
%	#declaration_import{ name = NameActual, alias = MaybeAlias, exposing = DirectImports } = Node#milang_ast.data,
%	WithModuleAlias = add_module_alias(NameActual, MaybeAlias, Table),
%	WithDirectImports = add_direct_imports(NameActual, DirectImports, WithModuleAlias),
%	?LOG_INFO("validating an import.~n"
%		"    Node: ~p~n"
%		"    FinalTable: ~p"
%		, [Node, WithDirectImports]),
%	{ok, WithDirectImports};

validate_node(spec, Node, Table) ->
	Data = milang_ast:data(Node),
	validate_spec(Data, Table);

validate_node(type, Node, Table) ->
	Data = milang_ast:data(Node),
	NameNode = milang_ast_type:name(Data),
	Constraints = milang_ast_type:constraints(Data),
	Constructors = milang_ast_type:constructors(Data),
	Args = milang_ast_type:args(Data),
	NameComplex = milang_ast:data(NameNode),
	{_, Name} = NameComplex,
	ConstraintMap = lists:foldl(fun({KeyNode, ValNode}, Acc) ->
		Key = milang_ast:data(KeyNode),
		case type_of_node(ValNode, Table) of
			{ok, T} ->
				Acc#{ Key => T };
			Wut ->
				error({invalid_constraint, Key, Wut})
		end
	end, #{}, Constraints),
	ArgNames = [ {placeholder, identifier(milang_ast:data(A))} || A <- Args],
	HasConstructors = case Constructors of
		[] -> false;
		_ -> true
	end,
	TopType = #tv_data{ arg_names = ArgNames, constraints = ConstraintMap, has_constructors = HasConstructors },
	MidTable = set_entry(Name, TopType, Table),
	ConstructorFoldFun = fun(ConstructorNode, TableAcc) ->
		ConstructorNameNode = milang_ast_constructor:name(milang_ast:data(ConstructorNode)),
		ConstructorArgsNodes = milang_ast_constructor:args(milang_ast:data(ConstructorNode)),
		{identifier_type, ConstructorName} = milang_ast:data(ConstructorNameNode),
		ConstructorArgsResult = 'Result':map_list(fun(N) ->
			type_of_node(N, TableAcc)
		end, ConstructorArgsNodes),
		?LOG_DEBUG("Constructor arg result: ~p", [ConstructorArgsResult]),
		'Result':map(fun(TypedArgs) ->
			Entry = #tv_constructor{ constructor_of = Name, args = TypedArgs},
			set_entry(ConstructorName, Entry, TableAcc)
		end, ConstructorArgsResult)
	end,
	?LOG_DEBUG("So the constructors I got: ~p", [Constructors]),
	Out = 'Result':foldl(ConstructorFoldFun, MidTable, Constructors),
	?LOG_DEBUG("validated a declaration_type: ~p~n"
		"    Out: ~p"
		, [Node, case Out of {ok, _} -> ok; _ -> Out end]),
	Out;

%validate_node(#milang_ast{ data = #declaration_function{} } = Node, Table) ->
%	?LOG_DEBUG("Starting declaration_function type validation."),
%	Data = Node#milang_ast.data,
%	#declaration_function{ name = FunctionNameComplex, args = Args, bindings = Bindings, expression = Expression } = Data,
%	{_, FunctionName} = FunctionNameComplex,
%	NewScope = [#{} | Table],
%	ResultResolvedType = 'Result':map_error(fun(_) ->
%		{notfound, FunctionName}
%	end, resolve_type(FunctionName, NewScope)),
%	?LOG_DEBUG("ResultResolvedType: ~p", [ResultResolvedType]),
%	TypeResolved = 'Result':with_default(ResultResolvedType, placeholder),
%	ResultLoadedArgs = load_args(TypeResolved, Args, NewScope),
%	?LOG_DEBUG("ResultLoadedArgs: ~p", [ResultLoadedArgs]),
%	ResultArgTypes = 'Result':and_then(fun(LoadedArgs) ->
%		'Result':map_list(fun(Arg) ->
%			type_of_node(Arg, LoadedArgs)
%		end, Args)
%	end, ResultLoadedArgs),
%	?LOG_DEBUG("ResultArgTypes: ~p", [ResultArgTypes]),
%	ResultBindingsLoaded = 'Result':and_then(fun(LoadedArgs) ->
%		load_bindings(Bindings, LoadedArgs)
%	end, ResultLoadedArgs),
%	ResultExpressionType = 'Result':and_then(fun(BindingsLoaded) ->
%		type_of_node(Expression, BindingsLoaded)
%	end, ResultBindingsLoaded),
%	ResultFullType = 'Result':map_n([ResultArgTypes, ResultExpressionType],fun(ArgTypes, ExpressionType) ->
%		#tv_concrete{ concrete_of = FunctionName, args = ArgTypes ++ [ ExpressionType ]}
%	end),
%	ResultFinalCheck = 'Result':and_then_n([{ok, TypeResolved}, ResultFullType, ResultBindingsLoaded], fun check_type_match/3),
%	Out = 'Result':map(fun([_Pop | OutTable]) ->
%		OutTable
%	end, ResultFinalCheck),
%	?LOG_INFO("validated a declaration_function: ~p~n"
%		"    Out: ~p~n"
%		"    Inital Table: ~p"
%		, [Node, Out, Table]),
%	Out;

validate_node(alias, Node, Table) ->
	Data = milang_ast:data(Node),
	NameNode = milang_ast_alias:name(Data),
	Constraints = milang_ast_alias:constraints(Data),
	ArgNodes = milang_ast_alias:args(Data),
	Args = [{placeholder, identifier(N)} || N <- ArgNodes],
	OriginalNode = milang_ast_alias:original(Data),
	TypeRes = type_of_node(OriginalNode, Table),
	TableWithAliasRes = 'Result':and_then(fun(Original) ->
		?LOG_DEBUG("the original: ~p", [Original]),
		Alias = #tv_alias{ arg_names = Args, constraints = Constraints, truename = Original },
		add_entry(identifier(NameNode), Alias, Table)
	end, TypeRes),
	% if we're aliasing a data type (like Maybe a), it's useful / needed to
	% localize the constructors as well ( if they have been exposed ).
	ConstructorsRes = 'Result':map_n([TypeRes, TableWithAliasRes], fun(OriginalType, TableWithAlias) ->
		case OriginalType of
			#tv_concrete{ concrete_of = DataTypeName } ->
				get_constructors_for(DataTypeName, TableWithAlias);
			_ ->
				[]
		end
	end),
	'Result':and_then(fun(Constructors) ->
		lists:foldl(fun
			({#{local := LocalName}, Constructor}, {ok, T}) ->
				add_entry(LocalName, Constructor, T);
			(_, T) ->
				T
		end, TableWithAliasRes, Constructors)
	end, ConstructorsRes);

validate_node(binding, Node, Table) ->
	Data = milang_ast:data(Node),
	validate_binding(Data, Table);

validate_node(teach, Node, Table) ->
	Data = milang_ast:data(Node),
	validate_teach(Data, Table);

validate_node(class, Node, Table) ->
	Data = milang_ast:data(Node),
	validate_class(Data, Table);

validate_node(_, Node, Table) ->
	?LOG_ERROR("Cannot validate a non-top level node: ~p", [Node]),
	{error, {cannot_validate_non_declarations, Node, Table}}.

validate_binding(Binding, Table) ->
	MatchNode = milang_ast_binding:match(Binding),
	Expression = milang_ast_binding:expression(Binding),
	MaybeExpressionType = type_of_node(Expression, Table),
	MatchData = milang_ast:data(MatchNode),
	MaybeExisting = lookup(identifier(MatchData), Table),
	validate_binding(MatchData, MaybeExpressionType, MaybeExisting, Table).

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
validate_binding({class_taught, _StudentName, _ClassName, _Binding} = Id, {ok, Implementation}, {ok, Spec}, Table) ->
	case check_type_match(Implementation, Spec, Table) of
		{ok, NewTable} ->
			add_entry(Id, Spec, NewTable);
		Error ->
			?LOG_ERROR("class_taught binding ~p failed to register due to ~p", [Id, Error]),
			Error
	end;
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
	ResultAddEntry = 'Result':and_then(fun(Type) ->
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
	StudentTypeRes = 'Result':and_then(fun(Name) ->
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
	ClassTypeRes = 'Result':and_then(fun(Name) ->
		case lookup(Name, Table) of
			{ok, #tv_class{}} = Ok -> Ok;
			{ok, Wut} -> {error, {not_a_class, Name, Wut}};
			Else -> Else
		end
	end, ClassNameRes),
	AddClassToStudentTypeRes = 'Result':and_then_n([StudentNameRes, StudentTypeRes, ClassNameRes, ClassTypeRes], fun(StudentName, StudentType, ClassName, ClassType) ->
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
	end),
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
	FinalRes = 'Result':and_then_n([StudentNameRes, StudentTypeRes, ClassNameRes, ClassTypeRes, AddClassToStudentTypeRes],
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
				ImplementationTypeRes = 'Result':and_then_n([ImplementationRes, TableRes], fun(Implementation, InTable) ->
					type_of_node(Implementation, InTable)
				end),
				TranslatedSpec = update_type_variable({placeholder, TypePlaceholder}, #tv_concrete{ concrete_of = StudentName }, Spec),
				?LOG_DEBUG("Spec translation done.~n    Original: ~p~n    New: ~p", [Spec, TranslatedSpec]),
				'Result':and_then_n([ImplementationTypeRes, TableRes], fun(Implementation, TableAcc) ->
					validate_binding({class_taught, StudentName, ClassName, SpecName}, {ok, Implementation}, {ok, TranslatedSpec}, TableAcc)
				end)
			end, {ok, TableWithLearnedStudent}, SpecList)
		end),
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
				SpecNameNode = milang_ast_spec:name(MemberData),
				{_, SpecName} = milang_ast:data(SpecNameNode),
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
	case add_entry(Name, TvClass, Table) of
		{ok, TableWithClass} ->
			'Result':foldl(fun({SpecName, _}, SpecTable) ->
				add_entry(SpecName, #tv_class_function{ of_class = Name }, SpecTable)
			end, TableWithClass, SpecsAsList);
		Error ->
			Error
	end.



%-spec load_args([ concrete() ], [ milang_ast:ast_node()], lookup_table()) -> {ok, lookup_table()} | {error, term()}.
%load_args(FuncType, Nodes, Table) ->
%	do_load_args(FuncType, Nodes, {ok, Table}).
%
%do_load_args(_FuncType, [], {ok, _} = Ok) ->
%	Ok;
%do_load_args([Type | TypeTail], [ArgAST | ArgTail], {ok, Table}) ->
%	case ArgAST of
%		{name_underscore, _, _} ->
%			do_load_args(TypeTail, ArgTail, {ok, Table});
%		{_, _, Name} when is_atom(Name) ->
%			NewTable = add_entry(Name, Type, Table),
%			do_load_args(TypeTail, ArgTail, NewTable)
%	end;
%do_load_args([], _MoreArgs, {ok, _Table}) ->
%	{error, too_many_args};
%do_load_args(_, _, Error) ->
%	Error.

%-spec load_bindings([ milang_ast:ast_node() ], lookup_table()) -> {error, term()} | {ok, lookup_table()}.
%load_bindings(Bindings, Table) ->
%	FoldFun = fun do_load_binding/2,
%	'Result':foldl(FoldFun, Table, Bindings).
%
%do_load_binding(Binding, Table) ->
%	Name = milang_ast_binding:name(Binding),
%	ExpressionAST = milang_ast_binding:expression(Binding),
%	case type_of_node(ExpressionAST, Table) of
%		{ok, T} ->
%			add_entry(Name, T, Table);
%		Error ->
%			Error
%	end.

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

-spec update_type_variable(mi_identifier(), concrete(), concrete() | [ concrete() ]) -> concrete() | [ concrete() ].
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

%-spec add_module_alias(atom(), undefined | { ok, atom()}, lookup_table()) -> lookup_table().
%add_module_alias(_NameActual, undefined, Table) ->
%	Table;
%add_module_alias(NameActual, {ok, Alias}, Table) ->
%	AllKeys = maps:keys(Table),
%	KeysForModule = [ Key || #{ module := ModuleNameActual } = Key <- AllKeys , ModuleNameActual =:= NameActual ],
%	lists:foldl(fun(TrueName, Acc) ->
%		NewKey = TrueName#{ module => Alias },
%		Entry = #tv_alias{ truename = TrueName },
%		set_entry(NewKey, Entry, Acc)
%	end, Table, KeysForModule).

%-spec add_direct_imports(atom(), [milang_ast:name()], lookup_table()) -> lookup_table().
%add_direct_imports(NameActual, DirectImports, Table) ->
%	?LOG_DEBUG("Adding direct imports.~n"
%		"    NameActual: ~p~n"
%		"    DirectImports: ~p"
%		, [NameActual, DirectImports]),
%	JustNames = [ E || {_, E} <- DirectImports ],
%	lists:foldl(fun(LocalName, Acc) ->
%		Entry = #tv_alias{ truename = #{ module => NameActual, local => LocalName }},
%		set_entry(LocalName, Entry, Acc)
%	end, Table, JustNames).

-spec type_of_node(milang_ast:ast_node(), lookup_table()) -> {ok, concrete() | mi_identifier()} | {error, term()}.
type_of_node(Node, Table) ->
	?LOG_DEBUG("type of node ~p", [Node]),
	Type = milang_ast:type_simply(Node),
	Out = type_of_node(Type, Node, Table),
	?LOG_DEBUG("And the type is ~p", [Out]),
	Out.

type_of_node(signature, Node, Table) ->
	%?LOG_DEBUG("type of node:~n    Node: ~p", [Node]),
	ArgNodes = milang_ast_signature:args(milang_ast:data(Node)),
	ResultArgTypes = 'Result':map_list(fun(N) ->
		type_of_node(N, Table)
	end, ArgNodes),
	'Result':map(fun(ArgTypes) -> #tv_function{ args = ArgTypes } end, ResultArgTypes);

type_of_node(concrete, Node, Table) ->
	%?LOG_DEBUG("type of node:~n    Node: ~p~n", [Node]),
	Data = milang_ast:data(Node),
	NameNode = milang_ast_concrete:name(Data),
	Name = identifier(NameNode),
	Args = milang_ast_concrete:args(Data),
	ResultArgTypes = 'Result':map_list(fun(N) ->
		type_of_node(N, Table)
	end, Args),
	% a concrete can be for either an alias or a data (type). To make type checks
	% easier going forward, if we get an alias, we merge the concrete the alias
	% uses with the one we got, and check them.
	% a rough-cut of it. Its up to the validator or type check part to actually
	% figure out the type proper.
	ResultLookup = lookup(Name, Table),
	'Result':and_then_n([ResultArgTypes, ResultLookup], fun(ArgTypes, Resolved) ->
		case Resolved of
			#tv_alias{} ->
				{ok, alias_as_concrete(Resolved, ArgTypes)};
			#tv_data{} ->
				{ok, #tv_concrete{ concrete_of = Name, args = ArgTypes}}
		end
	end);

type_of_node(identifier_ignored, _, _Table) ->
	{ok, any};
%type_of_node(#milang_ast{ data = #type_variable{ name = {_, Name} }}, _Table) ->
%	% we don't know that this can be or should be, at least not yet.
%	?LOG_DEBUG("Attempting to figure out type of type variable ~p", [Name]),
%	{ok, {type_variable, Name}};
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
%type_of_node(identifier_bound, Node, Table) ->
%	{identifier_bound, Name} = milang_ast:data(Node),
%	case resolve_type(Name, Table) of
%		{ok, _} = Ok -> Ok;
%		_NotOk -> {ok, placeholder}
%	end;
%type_of_node(#milang_ast{ data = #function_variable{ name = {name_downcase, Name}}}, Table) ->
%	% at this point we should have loaded the table w/ the arg type if there is
%	% a spec. If there isn't we'll set a placeholder and try to figure it out
%	% later.
%	case resolve_type(Name, Table) of
%		{ok, _} = Ok -> Ok;
%		_NotOld -> {ok, placeholder}
%	end;
type_of_node(call, Node, Table) ->
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
	Name = case milang_ast_call:function(Data) of
		{_NameType, CallName} ->
			CallName;
		JustCallName ->
			JustCallName
	end,
	?LOG_DEBUG("Function name: ~p; args: ~p", [Name, Args]),
	ResultArgTypes = 'Result':map_list(fun(N) ->
		?LOG_DEBUG("expression call arg type: ~p", [N]),
		type_of_node(N, Table)
	end, Args),
	ResultLong = 'Result':map(fun(ArgTypes) ->
		#tv_function{ args = ArgTypes }
	end, ResultArgTypes),
	ResultExistingAtAll = lookup(Name, Table),
	ResultExisting = 'Result':and_then(fun
		(#tv_function{} = A) ->
			{ok, A};
		({placeholder, _A}) ->
			% TODO is this right?
			% the difference between a literal value and a function that takes
			% no args but returns a literal value is: nothing. There is no
			% difference.
			{ok, #tv_function{}};
		(#tv_class_function{of_class = ClassName}) ->
			ClassResult = lookup(ClassName, Table),
			ClassSpecsResult = 'Result':and_then(fun(MaybeTvClass) ->
				case MaybeTvClass of
					#tv_class{ member_specs = ClassSpecs } ->
						{ok, ClassSpecs};
					_NotAClass ->
						{error, {not_a_class, ClassName, MaybeTvClass}}
				end
			end, ClassResult),
			'Result':and_then(fun(Specs) ->
				case maps:find(Name, Specs) of
					error ->
						{error, {not_a_member_of_class, ClassName, Name}};
					{ok, #tv_function{}} = Ok ->
						Ok;
					{ok, NotFunction} ->
						{error, {not_a_function, Name, ClassName, NotFunction}}
				end
			end, ClassSpecsResult);
		(#tv_constructor{ } = Constructor) ->
			DataName = Constructor#tv_constructor.constructor_of,
			ConstructorArgs = Constructor#tv_constructor.args,
			{ok, DataType} = lookup(DataName, Table),
			Concrete = #tv_concrete{ concrete_of = DataName, args = DataType#tv_data.arg_names },
			Function = #tv_function{ args = ConstructorArgs ++ [ Concrete ]},
			{ok, Function};
		(#tv_data{ has_constructors = false} = TvData) ->
			Concrete = #tv_concrete{ concrete_of = Name , args = TvData#tv_data.arg_names },
			Function = #tv_function{ args = TvData#tv_data.arg_names ++ [ Concrete]},
			{ok, Function};
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
					{error, {not_a_function, Name, Concrete}}
			end;
		(Actual) ->
			{error, {not_a_function, Name, Actual}}
	end, ResultExistingAtAll),
	ResultTypeCheck = 'Result':and_then_n([ResultExisting, ResultLong], fun(Known, Inferred) ->
		check_type_match(Known, Inferred, Table)
	end),
	ResultChoppedArgs = 'Result':and_then_n([ResultTypeCheck, ResultExisting, ResultLong], fun(TypeCheck, Known, Inferred) ->
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
	end),
	'Result':map(fun(MinimalFunction) ->
		case MinimalFunction#tv_function.args of
			[Singleton] ->
				Singleton;
			_ ->
				MinimalFunction
		end
	end, ResultChoppedArgs);
type_of_node(literal_string, _Node, _Table) ->
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


type_of_node(function, Node, BaseTable) ->
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
		TypeRes = 'Result':and_then(fun({_, TableAcc}) ->
			type_of_node(ArgNode, TableAcc)
		end, Acc),
		'Result':and_then_n([Acc, TypeRes], fun({ArgTypes, TableAcc}, T) ->
			{ok, {[{ArgNode, T} | ArgTypes], TableAcc}}
		end)
	end, {ok, {[], Table}}, ArgNodes),
	{ReversedArgsListRes, TableRes} = case ReversedArgsListAndTableRes of
		{ok, {ArgsListReversed, AccTable}} ->
			{{ok, ArgsListReversed}, {ok, AccTable}};
		_ ->
			{ReversedArgsListAndTableRes, ReversedArgsListAndTableRes}
	end,
	MaybeArgsList = 'Result':map(fun lists:reverse/1, ReversedArgsListRes),
	AddNode = fun(ArgNode, Type, OldTable) ->
		Id = identifier(ArgNode),
		add_entry(Id, Type, OldTable)
	end,
	AddNodes = fun(ArgList) ->
		lists:foldl(fun({ArgNode, Type}, TableResAcc) ->
			'Result':and_then(fun(OldTable) ->
				AddNode(ArgNode, Type, OldTable)
			end, TableResAcc)
		end, TableRes, ArgList)
	end,
	MaybeTableWithArgs = 'Result':and_then(fun(ArgList) ->
		AddNodes(ArgList)
	end, MaybeArgsList),

	BindNodes = milang_ast_function:binds(Data),
	MaybeTableWithBinds = lists:foldl(fun(BindNode, Acc) ->
		BindData = milang_ast:data(BindNode),
		MatchNode = milang_ast_binding:match(BindData),
		ExprNode = milang_ast_binding:expression(BindData),
		TypeRes = 'Result':and_then(fun(T) ->
			type_of_node(ExprNode, T)
		end, Acc),
		'Result':and_then_n([TypeRes, Acc], fun(Type, AccTable) ->
			Id = identifier(MatchNode),
			add_entry(Id, Type, AccTable)
		end)
	end, MaybeTableWithArgs, BindNodes),

	ExprNode = milang_ast_function:expression(Data),
	ExprRes = 'Result':and_then(fun(TableAcc) ->
		type_of_node(ExprNode, TableAcc)
	end, MaybeTableWithBinds),
	'Result':map_n([ReversedArgsListRes, ExprRes], fun(Args, Res) ->
		ArgTypes = lists:map(fun({_FullArgNode, T}) ->
			T
		end, Args),
		FullType = [Res | ArgTypes],
		#tv_function{ args = lists:reverse(FullType)}
	end);

type_of_node(infix_tree, Node, Table) ->
	Data = milang_ast:data(Node),
	Notation = milang_ast_infix_tree:notation(Data),
	LeftNode = milang_ast_infix_tree:left(Data),
	RightNode = milang_ast_infix_tree:right(Data),

	Function = milang_ast_infix_notation:function(Notation),
	Args = [LeftNode, RightNode],
	CallNode = milang_ast:transform_data(fun(_) ->
		milang_ast_call:new({identifier_bound, Function}, Args)
	end, Node),
	type_of_node(call, CallNode, Table);

type_of_node(match, Node, Table) ->
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
	ExprAndHeadMatchRes = 'Result':and_then_n([ExpressionTypeRes, ClauseHeadsRes], fun(ExprType, HeadTypes) ->
		'Result':foldl(fun(HeadType, _AccTable) ->
			check_type_match(ExprType, HeadType, Table)
		end, Table, HeadTypes)
	end),

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
			'Result':foldl(fun(ClauseType, _AccTable) ->
				check_type_match(Clause1, ClauseType, Table)
			end, Table, ClauseTail)
	end,

	'Result':map_n([AllClauseMatchesRes, ExprAndHeadMatchRes, ClauseMatchRes], fun(_, _, [Clause1 | _]) ->
		Clause1
	end);

type_of_node(match_clause, Node, Table) ->
	Data = milang_ast:data(Node),
	Head = milang_ast_match_clause:match(Data),
	Binds = milang_ast_match_clause:binds(Data),
	Expression = milang_ast_match_clause:expression(Data),

	NewScopedTable = [#{} | Table],

	HeadBindsTableRes = match_binds(Head, NewScopedTable),

	WithBindsTableRes = 'Result':and_then(fun(HeadTable) ->
		'Result':foldl(fun(BindNode, AccTable) ->
			BindData = milang_ast:data(BindNode),
			MatchNode = milang_ast_binding:match(BindData),
			ExprNode = milang_ast_binding:expression(BindData),
			TypeRes = type_of_node(ExprNode, AccTable),
			'Result':and_then(fun(Type) ->
				Id = identifier(MatchNode),
				add_entry(Id, Type, AccTable)
			end, TypeRes)
		end, HeadTable, Binds)
	end, HeadBindsTableRes),

	'Result':and_then(fun(BindTable) ->
		type_of_node(Expression, BindTable)
	end, WithBindsTableRes);

type_of_node(match_type, Node, Table) ->
	{match_type, Name, Args} = milang_ast:data(Node),
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
	MatchableTypeRes = 'Result':and_then(fun(PrimaryType) ->
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
	'Result':and_then_n([MatchableTypeRes, ArgTypesRes], fun(MatchableType, ArgTypes) ->
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
	end);

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

%type_of_node(#milang_ast{ data = {name_upcase, Name}}, _Table) ->
%	% TODO double-check the below.
%	% if we're here, we're not just looking at some binding, we're trying
%	% to figure out types. Which means this name _is_ defining a type, and
%	% not just the name for a type.
%	% ie, this is the 'String's in "repeat : String -> Int -> String."
%	{ok, #tv_concrete{ concrete_of = Name, args = []}};
%% the clause below ended up getting subsumed by thhe clause above. They do
%% different things, though. So, which is right?
%%type_of_node(#milang_ast{ data = {name_upcase, LocalName}} = Node, Table) ->
%%	% TODO are we _sure_ about this?
%%	case resolve_type(LocalName, Table) of
%%		{error, notfound} ->
%%			{error, {type_not_found, LocalName}};
%%		Ok ->
%%			Ok
%%	end;
%type_of_node({identifier_type, Name}, Table) ->
%	case lookup(Name, Table) of
%		{error, notfound} ->
%			{error, {unable_to_determine_type_from_node, {identifier_type, Name, Table}}};
%		{ok, _} = Ok ->
%			Ok
%	end;
%type_of_node(#milang_ast{ data = {type_variable, Name}}, Table) ->
%	case lookup(Name, Table) of
%		{error,notfound} ->
%			{ok, {bound_variable, Name}};
%		Ok ->
%			Ok
%	end.
%type_of_node(#milang_ast{ data = unbound_variable }, _Table) ->
%	{ok, unbound_variable};
%type_of_node({name_underscore, _}, _Table) ->
%	{ok, unbound_variable};
%type_of_node(#milang_ast{ data = {literal_string, _} }, _Table) ->
%	{ok, #tv_concrete{ concrete_of = #{ module => 'Core', local => 'String' }}};
%type_of_node(#milang_ast{ data = {literal_integer, _} }, _Table) ->
%	{ok, #tv_concrete{ concrete_of = #{ module => 'Core', local => 'Integer' }}};
%type_of_node(#milang_ast{ data = {literal_float, _} }, _Table) ->
%	{ok, #tv_concrete{ concrete_of = #{ module => 'Core', local => 'Float'}}};

% TODO implement literal lists and records.
%type_of_node(#milang_ast{ data = #literal_map{} } = Node, Table) ->
%	Data = Node#milang_ast.data,
%	InitKeyValueTypes = {ok, {placeholder, placeholder}},
%	FoldFun = fun
%		(_, {error, _} = Error) ->
%			Error;
%		(#milang_ast{ type = map_set } = KVNode, {ok, {InKeyType, InValueType}}) ->
%			#{ key := KeyNode, value := ValueNode} = KVNode#milang_ast.data,
%			ResultKeyType = type_of_node(KeyNode, Table),
%			ResultValueType = type_of_node(ValueNode, Table),
%			ResultKeyTypeCheck = 'Result':and_then(fun(KeyType) ->
%				check_type_match(InKeyType, KeyType, Table)
%			end, ResultKeyType),
%			ResultValueTypeCheck = 'Result':and_then(fun(ValueType) ->
%				check_type_match(InValueType, ValueType, Table)
%			end, ResultValueType),
%			'Result':map_n([ResultKeyTypeCheck, ResultValueTypeCheck], fun(KeyType, ValueType) ->
%				{KeyType, ValueType}
%			end)
%	end,
%	ResultFold = lists:foldl(FoldFun, InitKeyValueTypes, Data),
%	'Result':map(fun({KeyType, ValueType}) ->
%		#concrete{
%			concrete_of = #{ module => 'Core.Map', name => 'Map' },
%			args = [KeyType, ValueType]
%		}
%	end, ResultFold);

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
	'Result':foldl(fun(Arg, TableAcc) ->
		match_binds(Arg, TableAcc)
	end, Table, Args);

match_binds(match_list, {match_list, []}, Table) ->
	{ok, Table};

match_binds(match_list, {match_list, Matches}, Table) ->
	'Result':foldl(fun(Match, AccTable) ->
		match_binds(Match, AccTable)
	end, Table, Matches);

match_binds(match_list_head, {match_list_head, Matches, TailMatch}, Table) ->
	HeadMatchRes = 'Result':foldl(fun(Match, TableAcc) ->
		match_binds(Match, TableAcc)
	end, Table, Matches),
	'Result':and_then(fun(TableAcc) ->
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
	unicode:characters_to_binary(Name);
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

