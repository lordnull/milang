-module(milang_type_validation).

-export(
	[ new/0
	, validate_list/2
	, resolve_name/2
	, resolve_type/2
	]).

-include("milang_ast.hrl").

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

-type name() :: {local, atom()} | #{ name := atom(), module := atom() }.

-type maybe_name() :: undefined | name().

-type type_variable() :: {type_variable, atom()}.

% What is used in specs and defining other types.
% Maybe a :: #concrete{ concrete_of = 'Maybe', args = [{type_variable, 'a'}]}.
% Maybe ( List a ) :: #concrete{ concrete_of = 'Maybe', args = [
%     #concrete{ concrete_of = 'List', args = [{type_variable, a}]}
% ]}.
% map : (a -> Maybe b) -> Maybe a -> Maybe b ::
%     #concrete{ concrete_of = 'map', args = [
%         #concrete{ concrete_of = 'undefined', args = [
%             {type_variable, a},
%             #concrete{ concrete_of = 'Maybe', args = [ {type_variable, b}]}
%         ]},
%         #concrete{ concrete_of = 'Maybe', args = [
%             {type_variable, a}
%         ]},
%         #concrete{ concrete_of = 'Maybe', args = [
%             {type_variable, b}
%         ]}
% ]}
-record(concrete, {
	concrete_of = undefined :: maybe_name(),
	args = [] :: [ type_variable() | #concrete{} ]
}).
-type concrete() :: #concrete{}.

% The top level definition of data types. Generally only used in -type
% declarations, or to compare against ast nodes.
% -type Maybe a | Just a | Nothing. :: #data{ constraints = #{}, arg_names = [ a ]}.
-record(data, {
	constraints = #{} :: #{ atom() => #concrete{} },
	arg_names = [] :: [ atom() ]
}).
-type data() :: #data{}.

% the individual constructors for a top type. Generally used in -type
% declarataions, or to find the type of an expression.
% -type Maybe a | Just a | Nothing. :: #{
%     'Just' => #constructor{ constructor_of = 'Maybe', args = [ a ]},
%     'Nothing' => #constructor{ constructor_of = 'Maybe', args = []}
% }.
-record(constructor, {
	constructor_of :: name(),
	args = [] :: [ #concrete{} | type_variable() ]
}).
-type constructor() :: #constructor{}.

% points to another name.
-record(alias, {
	truename :: name()
}).
-type alias() :: #alias{}.

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
validate_node(#milang_ast{ type = declaration_module }, Table) ->
	io:format("Easy validating a module declarataion.~n"),
	{ok, Table};
validate_node(#milang_ast{ type = declaration_import} = Node, Table) ->
	% it is up to the caller to have loaded the table with the header data via
	% previous validate_list calls. This just sets the name of the imports to
	% be aliased to the remote name.
	#{ name := NameActual, alias := MaybeAlias, exposing := DirectImports } = Node#milang_ast.data,
	WithModuleAlias = add_module_alias(NameActual, MaybeAlias, Table),
	WithDirectImports = add_direct_imports(NameActual, DirectImports, WithModuleAlias),
	io:format("validated an import.~n"
		"    Node: ~p~n"
		"    FinalTable: ~p"
		, [ Node, WithDirectImports]),
	{ok, WithDirectImports};

validate_node(#milang_ast{ type = declaration_spec} = Node, Table) ->
	#{ name := NameAST, spec := Spec } = Node#milang_ast.data,
	NameRaw = NameAST#milang_ast.data,
	Name = if
		is_atom(NameRaw) -> {local, NameRaw};
		is_map(NameRaw) -> NameRaw
	end,
	ResultSpecType = type_of_node(Spec, Table),
	ResultAddEntry = 'Result':and_then(fun(SpecType) ->
		add_entry(Name, SpecType#concrete{ concrete_of = Name }, Table)
	end, ResultSpecType),
	Out = 'Result':recover(fun
		({shadowing, Entry}) ->
			'Result':and_then(fun(SpecType) ->
				check_type_match(SpecType, Entry, Table)
			end, ResultSpecType);
		(Error) ->
			{error, Error}
	end, ResultAddEntry),
	io:format("validating a declaraion spec: ~p~n"
		"    Output: ~p~n"
		, [ Node, Out]),
	Out;

validate_node(#milang_ast{ type = declaration_type} = Node, Table) ->
	#{ name := NameAST, args := Args, constraints := Constraints, constructors := Constructors} = Node#milang_ast.data,
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
	TopType = #data{ arg_names = ArgNames, constraints = ConstraintMap},
	Name = NameAST#milang_ast.data,
	MidTable = set_entry(Name, TopType, Table),
	ConstructorFoldFun = fun(ConstructorNode, TableAcc) ->
		#{ name := ConstructorNameAST, args := ConstructorArgsNodes} = ConstructorNode#milang_ast.data,
		ConstructorArgsResult = 'Result':map_list(fun(N) ->
			type_of_node(N, TableAcc)
		end, ConstructorArgsNodes),
		'Result':map(fun(TypedArgs) ->
			#milang_ast{ data = ConstructorName} = ConstructorNameAST,
			Entry = #constructor{ constructor_of = Name, args = TypedArgs},
			set_entry(ConstructorName, Entry, TableAcc)
		end, ConstructorArgsResult)
	end,
	Out = 'Result':foldl(ConstructorFoldFun, MidTable, Constructors),
	io:format("validated a declaration_type: ~p~n"
		"    Out: ~p~n"
		, [Node, Out]),
	Out;

validate_node(#milang_ast{ type = declaration_function } = Node, Table) ->
	Data = Node#milang_ast.data,
	#{ name := NameAST, args := Args, bindings := Bindings, expression := Expression } = Data,
	FunctionNameRaw = NameAST#milang_ast.data,
	FunctionName = if
		is_atom(FunctionNameRaw) -> {local, FunctionNameRaw};
		is_map(FunctionNameRaw) -> FunctionNameRaw
	end,
	NewScope = [#{} | Table],
	ResultResolvedType = 'Result':map_error(fun(_) ->
		{notfound, FunctionName}
	end, resolve_type(FunctionName, NewScope)),
	TypeResolved = 'Result':with_default(ResultResolvedType, placeholder),
	ResultLoadedArgs = load_args(TypeResolved, Args, NewScope),
	ResultArgTypes = 'Result':and_then(fun(LoadedArgs) ->
		'Result':map_list(fun(Arg) ->
			type_of_node(Arg, LoadedArgs)
		end, Args)
	end, ResultLoadedArgs),
	ResultBindingsLoaded = 'Result':and_then(fun(LoadedArgs) ->
		load_bindings(Bindings, LoadedArgs)
	end, ResultLoadedArgs),
	ResultExpressionType = 'Result':and_then(fun(BindingsLoaded) ->
		type_of_node(Expression, BindingsLoaded)
	end, ResultBindingsLoaded),
	ResultFullType = 'Result':map_n([ResultArgTypes, ResultExpressionType],fun(ArgTypes, ExpressionType) ->
		#concrete{ concrete_of = FunctionName, args = ArgTypes ++ [ ExpressionType ]}
	end),
	ResultFinalCheck = 'Result':and_then_n([{ok, TypeResolved}, ResultFullType, ResultBindingsLoaded], fun check_type_match/3),
	Out = 'Result':map(fun([_Pop | OutTable]) ->
		OutTable
	end, ResultFinalCheck),
	io:format("validated a declaration_function: ~p~n"
		"    Out: ~p~n"
		"    Inital Table: ~p~n"
		, [Node, Out, Table]),
	Out;

validate_node(Node, Table) ->
	io:format("Cannot validate a non-top level node: ~p~n", [Node]),
	{error, {cannot_validate_non_declarations, Node, Table}}.

-spec load_args([ concrete() ], [ milang_ast:ast_node()], lookup_table()) -> {ok, lookup_table()} | {error, term()}.
load_args(FuncType, Nodes, Table) ->
	do_load_args(FuncType, Nodes, {ok, Table}).

do_load_args(_FuncType, [], {ok, _} = Ok) ->
	Ok;
do_load_args([Type | TypeTail], [ArgAST | ArgTail], {ok, Table}) ->
	Name = ArgAST#milang_ast.data,
	case atom_to_binary(Name, utf8) of
		<<$_, _/binary>> ->
			do_load_args(TypeTail, ArgTail, {ok, Table});
		_ ->
			NewTable = add_entry({local, Name}, Type, Table),
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
	#{ variable := NameAST, expression := ExpressionAST } = Binding#milang_ast.data,
	Name = NameAST#milang_ast.data,
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
check_type_match(#concrete{ concrete_of = OfA} = Known, #concrete{ concrete_of = OfB} = Inferred, Table)
		when OfA =:= OfB; OfA =:= undefined; OfB =:= undefined ->
	KnownArgs = Known#concrete.args,
	InferredArgs = Inferred#concrete.args,
	check_type_list_match(KnownArgs, InferredArgs, Table);
check_type_match(_Known, {type_variable, '_'}, Table) ->
	{ok, Table};
check_type_match(A, B, _Table) ->
	io:format("type mismatch marker.~n"
		"    A: ~p~n"
		"    B: ~p~n"
		"    Table: ~p~n"
		, [A, B, _Table]),
	{error, {type_mismatch, A, B}}.

check_type_list_match(_KnowListLengthGTE, [], Table) ->
	{ok, Table};
check_type_list_match([], _InferredListTooLong, Table) ->
	{ok, Table};
check_type_list_match([{type_variable, '_'} | KnownTail], [_Inferred | InferredTail], Table) ->
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

-spec update_type_variable(type_variable(), concrete(), milang_ast:ast_node() | concrete()) -> milang_ast:ast_node() | concrete().
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

-spec add_module_alias(atom(), atom(), lookup_table()) -> lookup_table().
add_module_alias(_NameActual, undefined, Table) ->
	Table;
add_module_alias(NameActual, Alias, Table) ->
	AllKeys = maps:keys(Table),
	KeysForModule = [ Key || #{ module := ModuleNameActual } = Key <- AllKeys , ModuleNameActual =:= NameActual ],
	lists:foldl(fun(TrueName, Acc) ->
		NewKey = TrueName#{ module => Alias },
		Entry = #alias{ truename = TrueName },
		set_entry(NewKey, Entry, Acc)
	end, Table, KeysForModule).

-spec add_direct_imports(atom(), milang_ast:ast_node(), lookup_table()) -> lookup_table().
add_direct_imports(NameActual, DirectImports, Table) ->
	JustNames = [ E#milang_ast.data || E <- DirectImports ],
	lists:foldl(fun(LocalName, Acc) ->
		Entry = #alias{ truename = #{ module => NameActual, name => LocalName }},
		set_entry({local, LocalName}, Entry, Acc)
	end, Table, JustNames).

-spec type_of_node(milang_ast:ast_node(), lookup_table()) -> {ok, concrete()} | {error, term()}.
type_of_node(#milang_ast{ type = type_function } = Node, Table) ->
	ArgNodes = Node#milang_ast.data,
	ResultArgTypes = 'Result':map_list(fun(N) ->
		type_of_node(N, Table)
	end, ArgNodes),
	'Result':map(fun(ArgTypes) -> #concrete{ args = ArgTypes } end, ResultArgTypes);
type_of_node(#milang_ast{ type = type_data } = Node, Table) ->
	#{ name := NameAST, args := Args } = Node#milang_ast.data,
	ResultArgTypes = 'Result':map_list(fun(N) ->
		type_of_node(N, Table)
	end, Args),
	LookupName = case NameAST#milang_ast.data of
		A when is_atom(A) -> {local, A};
		D -> D
	end,
	ResultLookupName = resolve_name(LookupName, Table),
	'Result':and_then_n([ResultArgTypes, ResultLookupName], fun(ArgTypes, ResolvedName) ->
		{ok, #concrete{ concrete_of = ResolvedName, args = ArgTypes }}
	end);
type_of_node(#milang_ast{ type = type_name_remote} = Node, _Table) ->
	% if we're here, we're not just looking at some binding, we're trying
	% to figure out types. Which means this name _is_ defining a type, and
	% not just the name for a type.
	% ie, this is the 'String's in "repeat : String -> Int -> String."
	Name = Node#milang_ast.data,
	{ok, #concrete{ concrete_of = Name, args = []}};
type_of_node(#milang_ast{ type = variable} = Node, Table) ->
	Name = Node#milang_ast.data,
	case lookup({local, Name}, Table) of
		{error,notfound} ->
			{ok, {type_variable, Name}};
		Ok ->
			Ok
	end;
type_of_node(#milang_ast{ type = literal_string }, _Table) ->
	{ok, #concrete{ concrete_of = #{ module => 'Core', name => 'String' }}};
type_of_node(#milang_ast{ type = literal_integer }, _Table) ->
	{ok, #concrete{ concrete_of = #{ module => 'Core', name => 'Integer' }}};
type_of_node(#milang_ast{ type = type_name_local } = Node, Table) ->
	LocalName = Node#milang_ast.data,
	case resolve_type({local, LocalName}, Table) of
		{error, notfound} ->
			{error, {type_not_found, LocalName}};
		Ok ->
			Ok
	end;
type_of_node(#milang_ast{ type = expression_call } = Node, Table) ->
	#{ name := NameAST, args := ArgsASTs } = Node#milang_ast.data,
	NameRaw = NameAST#milang_ast.data,
	Name = if
		is_atom(NameRaw) -> 'Result':with_default(resolve_name({local, NameRaw}, Table), {local, NameRaw});
		is_map(NameRaw) -> NameRaw
	end,
	ResultArgTypes = 'Result':map_list(fun(ArgAst) ->
		type_of_node(ArgAst, Table)
	end, ArgsASTs),
	ResultLong = 'Result':map(fun(ArgTypes) ->
		#concrete{ concrete_of = Name, args = ArgTypes}
	end, ResultArgTypes),
	ResultExisting = resolve_type(Name, Table),
	ResultTypeCheck = 'Result':and_then_n([ResultExisting, ResultLong], fun(Known, Inferred) ->
		check_type_match(Known, Inferred, Table)
	end),
	ResultChoppedArgs = 'Result':map_n([ResultTypeCheck, ResultExisting, ResultLong], fun(_, Known, Inferred) ->
		{_, NewArgs} = lists:split(length(Inferred#concrete.args), Known#concrete.args),
		Known#concrete{ args = NewArgs }
	end),
	'Result':map(fun(Concrete) ->
		case Concrete#concrete.args of
			[Singleton] ->
				Singleton;
			_ ->
				Concrete
		end
	end, ResultChoppedArgs);

type_of_node(Node, Table) ->
	{error, {unable_to_determine_type_from_node, Node, Table}}.


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
			{error, notfound};
		error ->
			lookup(Name, Tail);
		{ok, _} = Ok ->
			Ok
	end.


-spec resolve_type(name(), lookup_table()) -> {ok, placeholder | concrete() | data() | constructor()} | {error, notfound}.
resolve_type(Name, Table) ->
	case lookup(Name, Table) of
		{ok, #alias{} = A} ->
			resolve_type(A#alias.truename, Table);
		ErrOrNotAlias ->
			ErrOrNotAlias
	end.

-spec resolve_name(name(), lookup_table()) -> {ok, name()} | {error, notfound}.
resolve_name(Name, Table) ->
	io:format("looking for ~p, ", [Name]),
	case lookup(Name, Table) of
		{ok, #alias{} = A} ->
			resolve_name(A#alias.truename, Table);
		{ok, _NotAlias} ->
			io:format("and found!~n"),
			{ok, Name};
		Error ->
			io:format("Did not find."),
			Error
	end.
