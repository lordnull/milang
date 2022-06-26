-module(milang_type_validation).

-export(
	[ new/0
	, validate_list/2
	, resolve_name/2
	, resolve_type/2
	]).

-include("milang_ast.hrl").
-include("milang_log.hrl").

% okay, so given the following:
% -type Goober | Pants | Slacks Integer .
%
% -spec inseam : Goober -> Integer.
% inseam goober =
%   case goober of
%       Pants -> 1,
%       Slacks n -> n.
%
% We need to:
%   Load up Goober as a type with no constraints and no args
%   Load up Pants as a type with no constraints, no args, and Goober as parent.
%   Load up Slacks as a type with no constraints, and 1 arg:
%       Integer type (base type? primitive? something like that.)
%   Load up inseam as a type with noconstrats, function with 1 arg, and 1 return value
%       arg is a Goober
%       return is an Integer
%   Check the implemetation of inseam matches the spec, or do some inferencing.

-type name() :: atom() | #{ local := atom(), module := atom() }.

-type maybe_name() :: undefined | name().

-type type_variable() :: {type_variable, atom()}.

-type type_any() :: 'any'.

% What is used in specs and defining a type as an argument to other types.
% for example, given the declaration of:
% ```milang
% -type Maybe a [
%     , Just a
%     , Nothing
% ].
% ```
% We can define:
% ```milang
% -alias Greeting = Maybe String.
% ```
% Where the `Maybe String` is a concrete of the data type `Maybe`.
%
% The same is true of function specs.
% ```milang
% Maybe.map : ( a -> b ) -> Maybe a -> Maybe b.
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
	concrete_of = undefined :: maybe_name(),
	args = []
}).

% The top level definition of data types. This is generated from type declarations,
% and only for the type that is being declared. Thus why the constructors that
% reference the type use concretes and variables.
-record(tv_data, {
	constraints = #{} :: #{ atom() => #tv_concrete{} },
	arg_names = [] :: [ atom() ]
}).
-type data() :: #tv_data{}.

% the individual constructors for a top type. Generally used in -type
% declarataions, or to find the type of an expression.
% -type Maybe a | Just a | Nothing. :: #{
%     'Just' => #constructor{ constructor_of = 'Maybe', args = [ a ]},
%     'Nothing' => #constructor{ constructor_of = 'Maybe', args = []}
% }.
-record(tv_constructor, {
	constructor_of :: name(),
	args = [] :: [ #tv_concrete{} | type_variable() ]
}).

% The top level definition of function types. This is generated from spec
% declarations, and only for the function that is being spec'ed. This is distinct
% from the tv_data because the args are not just a list of variable names, nor
% does it have a list of constructors. Instead, args is a list of variables or
% concretes.
-record(tv_function, {
	constraints = #{} :: #{ atom() => #tv_concrete{} },
	args = [] :: [ type_variable() | #tv_concrete{} ]
}).

-type concrete() :: #tv_concrete{ args :: [ type_variable() | type_any() | #tv_concrete{} ]}.

-type constructor() :: #tv_constructor{ args :: [ type_variable() | #tv_concrete{} ]}.

% points to another name.
-record(tv_alias, {
	truename :: name()
}).
-type alias() :: #tv_alias{}.

-type entry() :: concrete() | data() | constructor() | alias() | placeholder.

-type lookup_table() :: nonempty_list( #{ name() => entry()} ).

-export_type([concrete/0, data/0, constructor/0, alias/0, entry/0, lookup_table/0, name/0, maybe_name/0, type_variable/0]).

-spec new() -> lookup_table().
new() ->
	[#{}].

-spec validate_list([ milang_ast:ast_node() ], lookup_table()) -> {ok, lookup_table()} | {error, term()}.
validate_list(Nodes, Table) ->
	FoldFun = fun validate_node/2,
	'Result':foldl(FoldFun, Table, Nodes).

-spec validate_node(milang_ast:ast_node(), lookup_table()) -> {ok, lookup_table()} | {error, term()}.
validate_node(#milang_ast{ data = #declaration_module{} }, Table) ->
	milang_log:it(info, ?log_info, "Easy validating a module declaration.", []),
	{ok, Table};
validate_node(#milang_ast{ data = #declaration_import{} } = Node, Table) ->
	% it is up to the caller to have loaded the table with the header data via
	% previous validate_list calls. This just sets the name of the imports to
	% be aliased to the remote name.
	#declaration_import{ name = NameActual, alias = MaybeAlias, exposing = DirectImports } = Node#milang_ast.data,
	WithModuleAlias = add_module_alias(NameActual, MaybeAlias, Table),
	WithDirectImports = add_direct_imports(NameActual, DirectImports, WithModuleAlias),
	milang_log:it(info, ?log_info, "validated an import.~n"
		"    Node: ~p~n"
		"    FinalTable: ~p"
		, [ Node, WithDirectImports]),
	{ok, WithDirectImports};

validate_node(#milang_ast{ data = #declaration_spec{}} = Node, Table) ->
	#declaration_spec{ name = NameComplex, type = Spec } = Node#milang_ast.data,
	{_, Name} = NameComplex,
	ResultSpecType = type_of_node(Spec, Table),
	ResultAddEntry = 'Result':and_then(fun(Type) ->
		add_entry(Name, Type, Table)
	end, ResultSpecType),
% the below is if we want to be able to declare a function before it's spec.
% Well, I don't want that to be valid, so it's either spec -> function or just
% function.
%	Out = 'Result':recover(fun
%		({shadowing, Entry}) ->
%			'Result':and_then(fun(SpecType) ->
%				check_type_match(SpecType, Entry, Table)
%			end, ResultSpecType);
%		(Error) ->
%			{error, Error}
%	end, ResultAddEntry),
	Out = ResultAddEntry,
	milang_log:it(info, ?log_info, "validating a declaraion spec: ~p~n"
		"    Output: ~p"
		, [ Node, Out]),
	Out;

validate_node(#milang_ast{ data = #declaration_type{}} = Node, Table) ->
	#declaration_type{ name = NameComplex, args = Args, constraints = Constraints, constructors = Constructors} = Node#milang_ast.data,
	{_, Name} = NameComplex,
	ConstraintMap = lists:foldl(fun({KeyNode, ValNode}, Acc) ->
		Key = KeyNode#milang_ast.data,
		case type_of_node(ValNode, Table) of
			{ok, T} ->
				Acc#{ Key => T };
			Wut ->
				error({invalid_constraint, Key, Wut})
		end
	end, #{}, Constraints),
	ArgNames = [ A || #milang_ast{ data = A} <- Args],
	TopType = #tv_data{ arg_names = ArgNames, constraints = ConstraintMap},
	MidTable = set_entry(Name, TopType, Table),
	ConstructorFoldFun = fun(ConstructorNode, TableAcc) ->
		#constructor{ name = {_, ConstructorName}, args = ConstructorArgsNodes} = ConstructorNode#milang_ast.data,
		ConstructorArgsResult = 'Result':map_list(fun(N) ->
			type_of_node(N, TableAcc)
		end, ConstructorArgsNodes),
		'Result':map(fun(TypedArgs) ->
			Entry = #tv_constructor{ constructor_of = Name, args = TypedArgs},
			set_entry(ConstructorName, Entry, TableAcc)
		end, ConstructorArgsResult)
	end,
	Out = 'Result':foldl(ConstructorFoldFun, MidTable, Constructors),
	milang_log:it(info, ?log_info, "validated a declaration_type: ~p~n"
		"    Out: ~p"
		, [Node, Out]),
	Out;

validate_node(#milang_ast{ data = #declaration_function{} } = Node, Table) ->
	milang_log:it(debug, ?log_info, "Starting declaration_function type validation."),
	Data = Node#milang_ast.data,
	#declaration_function{ name = FunctionNameComplex, args = Args, bindings = Bindings, expression = Expression } = Data,
	{_, FunctionName} = FunctionNameComplex,
	NewScope = [#{} | Table],
	ResultResolvedType = 'Result':map_error(fun(_) ->
		{notfound, FunctionName}
	end, resolve_type(FunctionName, NewScope)),
	milang_log:it(debug, ?log_info, "ResultResolvedType: ~p", [ResultResolvedType]),
	TypeResolved = 'Result':with_default(ResultResolvedType, placeholder),
	ResultLoadedArgs = load_args(TypeResolved, Args, NewScope),
	milang_log:it(debug, ?log_info, "ResultLoadedArgs: ~p", [ResultLoadedArgs]),
	ResultArgTypes = 'Result':and_then(fun(LoadedArgs) ->
		'Result':map_list(fun(Arg) ->
			type_of_node(Arg, LoadedArgs)
		end, Args)
	end, ResultLoadedArgs),
	milang_log:it(debug, ?log_info, "ResultArgTypes: ~p", [ResultArgTypes]),
	ResultBindingsLoaded = 'Result':and_then(fun(LoadedArgs) ->
		load_bindings(Bindings, LoadedArgs)
	end, ResultLoadedArgs),
	ResultExpressionType = 'Result':and_then(fun(BindingsLoaded) ->
		type_of_node(Expression, BindingsLoaded)
	end, ResultBindingsLoaded),
	ResultFullType = 'Result':map_n([ResultArgTypes, ResultExpressionType],fun(ArgTypes, ExpressionType) ->
		#tv_concrete{ concrete_of = FunctionName, args = ArgTypes ++ [ ExpressionType ]}
	end),
	ResultFinalCheck = 'Result':and_then_n([{ok, TypeResolved}, ResultFullType, ResultBindingsLoaded], fun check_type_match/3),
	Out = 'Result':map(fun([_Pop | OutTable]) ->
		OutTable
	end, ResultFinalCheck),
	milang_log:it(info, ?log_info, "validated a declaration_function: ~p~n"
		"    Out: ~p~n"
		"    Inital Table: ~p"
		, [Node, Out, Table]),
	Out;

validate_node(Node, Table) ->
	milang_log:it(error, ?log_info, "Cannot validate a non-top level node: ~p", [Node]),
	{error, {cannot_validate_non_declarations, Node, Table}}.

-spec load_args([ concrete() ], [ milang_ast:ast_node()], lookup_table()) -> {ok, lookup_table()} | {error, term()}.
load_args(FuncType, Nodes, Table) ->
	do_load_args(FuncType, Nodes, {ok, Table}).

do_load_args(_FuncType, [], {ok, _} = Ok) ->
	Ok;
do_load_args([Type | TypeTail], [ArgAST | ArgTail], {ok, Table}) ->
	case ArgAST of
		{name_underscore, _, _} ->
			do_load_args(TypeTail, ArgTail, {ok, Table});
		{_, _, Name} when is_atom(Name) ->
			NewTable = add_entry(Name, Type, Table),
			do_load_args(TypeTail, ArgTail, NewTable)
	end;
do_load_args([], _MoreArgs, {ok, _Table}) ->
	{error, too_many_args};
do_load_args(_, _, Error) ->
	Error.

-spec load_bindings([ milang_ast:ast_node() ], lookup_table()) -> {error, term()} | {ok, lookup_table()}.
load_bindings(Bindings, Table) ->
	FoldFun = fun do_load_binding/2,
	'Result':foldl(FoldFun, Table, Bindings).

do_load_binding(Binding, Table) ->
	#binding{ name = Name, expression = ExpressionAST } = Binding#milang_ast.data,
	case type_of_node(ExpressionAST, Table) of
		{ok, T} ->
			add_entry(Name, T, Table);
		Error ->
			Error
	end.

-spec check_type_match(concrete(), concrete() | placeholder, lookup_table()) -> {ok, lookup_table()} | {error, term()}.
check_type_match(_Known, placeholder, Table) ->
	{ok, Table};
check_type_match(placeholder, _Known, Table) ->
	{ok, Table};
check_type_match(#tv_concrete{ concrete_of = OfA} = Known, #tv_concrete{ concrete_of = OfB} = Inferred, Table)
		when OfA =:= OfB; OfA =:= undefined; OfB =:= undefined ->
	KnownArgs = Known#tv_concrete.args,
	InferredArgs = Inferred#tv_concrete.args,
	check_type_list_match(KnownArgs, InferredArgs, Table);
check_type_match(_Known, any, Table) ->
	{ok, Table};
check_type_match(any, _Inferred, Table) ->
	{ok, Table};
check_type_match(#tv_function{} = A, #tv_concrete{} = B, Table) ->
	KnownArgs = A#tv_function.args,
	InferredArgs = B#tv_concrete.args,
	check_type_list_match(KnownArgs, InferredArgs, Table);
check_type_match(#tv_function{} = A, #tv_function{} = B, Table) ->
	% This is for when the type specifier is inferred, as opposed to the actual
	% type being inferred.
	KnownArgs = A#tv_function.args,
	InferredArgs = B#tv_function.args,
	check_type_list_match(KnownArgs, InferredArgs, Table);
check_type_match(A, B, _Table) ->
	milang_log:it(error, ?log_info, "type mismatch marker.~n"
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

-spec update_type_variable(type_variable(), concrete(), concrete() | [ concrete() ]) -> concrete() | [ concrete() ].
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

-spec add_module_alias(atom(), undefined | { ok, atom()}, lookup_table()) -> lookup_table().
add_module_alias(_NameActual, undefined, Table) ->
	Table;
add_module_alias(NameActual, {ok, Alias}, Table) ->
	AllKeys = maps:keys(Table),
	KeysForModule = [ Key || #{ module := ModuleNameActual } = Key <- AllKeys , ModuleNameActual =:= NameActual ],
	lists:foldl(fun(TrueName, Acc) ->
		NewKey = TrueName#{ module => Alias },
		Entry = #tv_alias{ truename = TrueName },
		set_entry(NewKey, Entry, Acc)
	end, Table, KeysForModule).

-spec add_direct_imports(atom(), [milang_ast:name()], lookup_table()) -> lookup_table().
add_direct_imports(NameActual, DirectImports, Table) ->
	milang_log:it(debug, ?log_info, "Adding direct imports.~n"
		"    NameActual: ~p~n"
		"    DirectImports: ~p"
		, [NameActual, DirectImports]),
	JustNames = [ E || {_, E} <- DirectImports ],
	lists:foldl(fun(LocalName, Acc) ->
		Entry = #tv_alias{ truename = #{ module => NameActual, local => LocalName }},
		set_entry(LocalName, Entry, Acc)
	end, Table, JustNames).

-spec type_of_node(milang_ast:ast_node(), lookup_table()) -> {ok, concrete() | type_variable()} | {error, term()}.
type_of_node(#milang_ast{ data = #type_function{} } = Node, Table) ->
	milang_log:it(debug, ?log_info, "type of node:~n    Node: ~p", [Node]),
	#type_function{ args = ArgNodes} = Node#milang_ast.data,
	ResultArgTypes = 'Result':map_list(fun(N) ->
		type_of_node(N, Table)
	end, ArgNodes),
	'Result':map(fun(ArgTypes) -> #tv_function{ args = ArgTypes } end, ResultArgTypes);
type_of_node(#milang_ast{ data = #type_concrete{} } = Node, Table) ->
	milang_log:it(debug, ?log_info, "type of node:~n    Node: ~p~n", [Node]),
	#type_concrete{ name = {_, Name}, args = Args } = Node#milang_ast.data,
	ResultArgTypes = 'Result':map_list(fun(N) ->
		type_of_node(N, Table)
	end, Args),
	ResultLookupName = resolve_name(Name, Table),
	'Result':and_then_n([ResultArgTypes, ResultLookupName], fun(ArgTypes, ResolvedName) ->
		{ok, #tv_concrete{ concrete_of = ResolvedName, args = ArgTypes }}
	end);
type_of_node(#milang_ast{ data = #type_variable{ name = {name_underscore, _}}}, _Table) ->
	{ok, any};
type_of_node(#milang_ast{ data = #type_variable{ name = {_, Name} }}, _Table) ->
	% we don't know that this can be or should be, at least not yet.
	milang_log:it(debug, ?log_info, "Attempting to figure out type of type variable ~p", [Name]),
	{ok, {type_variable, Name}};
type_of_node(#milang_ast{ data = #function_variable{ name = {name_underscore, _}}}, _Table) ->
	{ok, any};
type_of_node(#milang_ast{ data = #function_variable{ name = {name_downcase, Name}}}, Table) ->
	% at this point we should have loaded the table w/ the arg type if there is
	% a spec. If there isn't we'll set a placeholder and try to figure it out
	% later.
	case resolve_type(Name, Table) of
		{ok, _} = Ok -> Ok;
		_NotOld -> {ok, placeholder}
	end;
type_of_node(#milang_ast{ data = #expression_call{} = Data}, Table) ->
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
	Args = Data#expression_call.args,
	{_NameType, Name} = Data#expression_call.function,
	ResultArgTypes = 'Result':map_list(fun(N) ->
		milang_log:it(debug, ?log_info, "expression call arg type: ~p", [N]),
		type_of_node(N, Table)
	end, Args),
	ResultLong = 'Result':map(fun(ArgTypes) ->
		#tv_concrete{ concrete_of = Name, args = ArgTypes }
	end, ResultArgTypes),
	ResultExistingAtAll = resolve_type(Name, Table),
	ResultExisting = 'Result':and_then(fun
		(#tv_function{} = A) ->
			{ok, A};
		(_) ->
			{error, not_a_function}
	end, ResultExistingAtAll),
	ResultTypeCheck = 'Result':and_then_n([ResultExisting, ResultLong], fun(Known, Inferred) ->
		check_type_match(Known, Inferred, Table)
	end),
	ResultChoppedArgs = 'Result':map_n([ResultTypeCheck, ResultExisting, ResultLong], fun(_, Known, Inferred) ->
		{_, NewArgs} = lists:split(length(Inferred#tv_concrete.args), Known#tv_function.args),
		Inferred#tv_concrete{ args = NewArgs }
	end),
	'Result':map(fun(Concrete) ->
		case Concrete#tv_concrete.args of
			[Singleton] ->
				Singleton;
			_ ->
				Concrete
		end
	end, ResultChoppedArgs);
type_of_node(#milang_ast{ data = {literal_string, _}}, _Table) ->
	{ok, string_type()};
type_of_node(#milang_ast{ data = {literal_float, _}}, _Table) ->
	{ok, float_type()};
type_of_node(#milang_ast{ data = {literal_integer, _}}, _Table) ->
	{ok, integer_type()};
type_of_node({name_underscore, _, _}, _Table) ->
	{ok, 'any'}.
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
%type_of_node(#milang_ast{ data = #type_variable{}} = Node, Table) ->
%	Name = Node#milang_ast.data#type_variable.name,
%	case lookup(Name, Table) of
%		{error,notfound} ->
%			{ok, {bound_variable, Name}};
%		Ok ->
%			Ok
%	end;
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

% TODO implement literal maps, lists, and records.
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

%type_of_node(Node, Table) ->
%	{error, {unable_to_determine_type_from_node, Node, Table}}.

string_type() ->
	#tv_concrete{ concrete_of = #{ module => 'Core', local => 'String' } }.

integer_type() ->
	#tv_concrete{ concrete_of = #{ module => 'Core', local => 'Integer' } }.

float_type() ->
	#tv_concrete{ concrete_of = #{ module => 'Core', local => 'Float' } }.

-spec add_entry(name(), entry(), lookup_table()) -> {ok, lookup_table()} | {error, {shadowing, entry()}}.
add_entry(Name, Entry, Table) ->
	case lookup(Name, Table) of
		{error, notfound} ->
			NewTable = set_entry(Name, Entry, Table),
			{ok, NewTable};
		{ok, Entry} ->
			{error, {shadowing, Entry}}
	end.

-spec set_entry(name(), entry(), lookup_table()) -> lookup_table().
set_entry(Name, Entry, Table) ->
	[OldHead | Tail] = Table,
	NewHead = maps:put(Name, Entry, OldHead),
	[NewHead | Tail].

-spec lookup(name(), lookup_table()) -> {error, notfound} | {ok, entry()}.
lookup(Name, [Table | Tail]) ->
	case maps:find(Name, Table) of
		error when Tail =:= [] ->
			milang_log:it(debug, ?log_info, "Did not find ~p", [Name]),
			{error, notfound};
		error ->
			lookup(Name, Tail);
		{ok, _} = Ok ->
			Ok
	end.


-spec resolve_type(name(), lookup_table()) -> {ok, placeholder | concrete() | data() | constructor()} | {error, notfound}.
resolve_type(Name, Table) ->
	case lookup(Name, Table) of
		{ok, #tv_alias{} = A} ->
			resolve_type(A#tv_alias.truename, Table);
		ErrOrNotAlias ->
			ErrOrNotAlias
	end.

-spec resolve_name(name(), lookup_table()) -> {ok, name()} | {error, notfound}.
resolve_name(Name, Table) ->
	milang_log:it(debug, ?log_info, "looking for ~p, ", [Name]),
	case lookup(Name, Table) of
		{ok, #tv_alias{} = A} ->
			resolve_name(A#tv_alias.truename, Table);
		{ok, _NotAlias} ->
			milang_log:it(debug, ?log_info, "and found!~n", []),
			{ok, Name};
		Error ->
			milang_log:it(info, ?log_info, "Did not find ~p.", [Name]),
			Error
	end.
