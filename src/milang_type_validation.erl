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

-type name() :: atom() | #{ name := atom(), module := atom() }.

% The Just a or Nothing part of Maybe a.
-record(constructor, {
	constructor_of :: name(),
	args = [] :: t_type()
}).

-type t_constructor() :: #constructor{}.

% The first argument you give to Maybe.map, ie (a -> Maybe b).
% also the spec for functions, ie Maybe.map : (a -> Maybe b) -> Maybe a -> Maybe b
-record(function, {
	types = [] :: [#function{}]
}).

-type t_function() :: #function{}.

% a defined type, Maybe Integer or Maybe (List a)
-record(concrete, {
	concrete_of :: name(),
	args = []
}).

-type t_concrete() :: #concrete{ args :: [ t_concrete() ]}.

% The Maybe a itself.
-record(type, {
	constraints = #{} :: #{ atom() => #concrete{} },
	arg_names = []
}).

-type t_type() :: #type{}.

% a name that just referes to one of the above.
-record(alias, {
	truename :: name()
	}).

-type t_alias() :: #alias{}.

-type type_entry() :: t_constructor() | t_type() | t_function() | t_concrete() | t_alias().

-type lookup_table() :: nonempty_list( #{ name() => type_entry()} ).

-export_type([type_entry/0, lookup_table/0, name/0]).

-spec new() -> lookup_table().
new() ->
	[#{}].

-spec validate_list([ milang_ast:ast_node() ], lookup_table()) -> {ok, lookup_table()} | {error, term()}.
validate_list(Nodes, Table) ->
	do_validate_list(Nodes, {ok, Table}).

do_validate_list([], Done) ->
	Done;
do_validate_list(_, {error, _} = Done) ->
	Done;
do_validate_list([ Node | Tail], {ok, Table}) ->
	Result = validate_node(Node, Table),
	io:format("Node validation."
		"	Node: ~p~n"
		"	Result: ~p~n"
		"	(I'm gonna regret this) Table: ~p~n"
		, [ Node, Result, Table]),
	do_validate_list(Tail, Result).

validate_node(#milang_ast{ type = declaration_module }, Table) ->
	{ok, Table};
validate_node(#milang_ast{ type = declaration_import} = Node, Table) ->
	% it is up to the caller to have loaded the table with the header data via
	% previous validate_list calls. This just sets the name of the imports to
	% be aliased to the remote name.
	#{ name := NameActual, alias := MaybeAlias, exposing := DirectImports } = Node#milang_ast.data,
	WithModuleAlias = add_module_alias(NameActual, MaybeAlias, Table),
	WithDirectImports = add_direct_imports(NameActual, DirectImports, WithModuleAlias),
	{ok, WithDirectImports};
validate_node(#milang_ast{ type = declaration_spec} = Node, Table) ->
	#{ name := NameAST, spec := Spec } = Node#milang_ast.data,
	Name = NameAST#milang_ast.data,
	case type_of_nodes(Spec, Table) of
		{ok, SpecType} ->
			refine_entry(Name, SpecType, Table);
		Error ->
			Error
	end;
validate_node(#milang_ast{ type = declaration_type} = Node, Table) ->
	#{ name := NameAST, args := Args, constraints := Constraints, constructors := Constructors} = Node#milang_ast.data,
	ConstraintMap = lists:foldl(fun({KeyNode, ValNode}, Acc) ->
		Key = KeyNode#milang_ast.data,
		case type_of_nodes(ValNode, Table) of
			{ok, T} ->
				Acc#{ Key => T };
			Wut ->
				error({invalid_constraint, Key, Wut})
		end
	end, #{}, Constraints),
	ArgNames = [ A || #milang_ast{ data = A} <- Args],
	TopType = #type{ arg_names = ArgNames, constraints = ConstraintMap},
	Name = NameAST#milang_ast.data,
	MidTable = set_entry(Name, TopType, Table),
	NewTable = lists:foldl(fun(Constructor, Acc) ->
		{ok, CType} = type_of_nodes(Constructor, Acc),
		#{ name := ConstructorNameAST} = Constructor#milang_ast.data,
		#milang_ast{ data = ConstructorName } = ConstructorNameAST,
		set_entry(ConstructorName, #constructor{ constructor_of = Name, args = CType }, Acc)
	end, MidTable, Constructors),
	{ok, NewTable};

validate_node(#milang_ast{ type = declaration_function } = Node, Table) ->
	Data = Node#milang_ast.data,
	#{ name := NameAST, args := Args, bindings := Bindings, expression := Expression } = Data,
	FunctionName = NameAST#milang_ast.data,
	NewScope = [#{} | Table],
	MaybeFunctionType = case resolve_type(FunctionName, NewScope) of
		{error, notfound} ->
			{error, {function_not_found, FunctionName}};
		TypeResolved ->
			TypeResolved
	end,
	MaybeWithArgs = maybe_load_args(MaybeFunctionType, Args, NewScope),
	MaybeArgTypes = maybe_type_of_nodes(Args, MaybeWithArgs),

	MaybeWithBindings = maybe_load_bindings(MaybeFunctionType, MaybeWithArgs, Bindings),
	%MaybeValidatedExpression = maybe_validate_node(Expression, MaybeWithBindings),
	MaybeReturnType = maybe_type_of_nodes(Expression, MaybeWithBindings),

	MaybeFullType = maybe_combine_type(MaybeArgTypes, MaybeReturnType),

	case maybe_check_type_match(MaybeFunctionType, MaybeFullType, MaybeWithBindings) of
		{ok, FinalTable} ->
			[ _Pop | OutTable] = FinalTable,
			{ok, OutTable};
		Error ->
			Error
	end;

validate_node(#milang_ast{ type = expression_call } = Node, Table) ->
	#{ name := NameAST, args := ArgsAST } = Node#milang_ast.data,
	FuncName = NameAST#milang_ast.data,
	MaybeFuncType = resolve_type(FuncName, Table),
	MaybeArgTypes = type_of_nodes(ArgsAST, Table),
	case MaybeFuncType of
		{error,notfound} ->
			{error, {function_not_found, FuncName}};
		{ok, FuncType} ->
			case MaybeArgTypes of
				{ok, ArgTypes} ->
					io:format("validating expression_call.~n"
						"	FuncName: ~p~n"
						"	FuncType: ~p~n"
						"	ArgTypes: ~p"
						,[FuncName, FuncType, ArgTypes]),
					check_type_match(FuncType, ArgTypes, Table);
				E ->
					E
			end
	end;

validate_node(#milang_ast{ type = literal_string } , Table) ->
	{ok, Table};

validate_node(Node, Table) ->
	{error, {nyi, Node, Table}}.

maybe_validate_node(Node, {ok, Table}) ->
	validate_node(Node, Table);
maybe_validate_node(_Node, Error) ->
	Error.

maybe_load_args({ok, FuncType}, Args, Table) ->
	load_args(FuncType#function.types, Args, {ok, Table});
maybe_load_args(Error, _, _) ->
	Error.

load_args(_FuncType, [], {ok, _} = Ok) ->
	Ok;
load_args([Type | TypeTail], [ArgAST | ArgTail], {ok, Table}) ->
	Name = ArgAST#milang_ast.data,
	case atom_to_binary(Name, utf8) of
		<<$_, _/binary>> ->
			load_args(TypeTail, ArgTail, {ok, Table});
		_ ->
			NewTable = add_entry(Name, Type, Table),
			load_args(TypeTail, ArgTail, NewTable)
	end;
load_args([], _MoreArgs, {ok, _Table}) ->
	{error, too_many_args};
load_args(_, _, Error) ->
	Error.

maybe_load_bindings({ok, _FuncType}, {ok, _} = OkTable, Bindings) ->
	load_bindings(Bindings, OkTable);
maybe_load_bindings({ok, _}, NotOkay, _) ->
	NotOkay;
maybe_load_bindings(NotOkay, _, _) ->
	NotOkay.

load_bindings([], {ok, _} = Ok) ->
	Ok;
load_bindings([Binding | BTail], {ok, Table}) ->
	#{ variable := NameAST, expression := ExpressionAST } = Binding#milang_ast.data,
	Name = NameAST#milang_ast.data,
	case type_of_nodes(ExpressionAST, Table) of
		{ok, T} ->
			NewTable = add_entry(Name, T, Table),
			load_bindings(BTail, NewTable);
		Error ->
			Error
	end.

maybe_combine_type({error, _} = Error, _) ->
	Error;
maybe_combine_type(_, {error, _} = Error) ->
	Error;
maybe_combine_type({ok, Head}, {ok, Tail}) ->
	io:format("Dash head: ~p~ndas tail: ~p~n", [Head, Tail]),
	Head ++ Tail.


maybe_check_type_match({error, _} = Error, _, _) ->
	Error;
maybe_check_type_match(_, {error, _} = Error, _) ->
	Error;
maybe_check_type_match(_, _, {error, _} = Error) ->
	Error;
maybe_check_type_match({ok, FunctionType}, Nodes, Table) ->
	maybe_check_type_match(FunctionType, Nodes, Table);
maybe_check_type_match(FunctionType, {ok, Nodes}, Table) ->
	maybe_check_type_match(FunctionType, Nodes, Table);
maybe_check_type_match(FunctionType, Nodes, {ok, Table}) ->
	maybe_check_type_match(FunctionType, Nodes, Table);
maybe_check_type_match(FunctionType, Nodes, Table) ->
	check_type_match(FunctionType, Nodes, Table).


check_type_match(_Type, undefined, Table) ->
	{ok, Table};
check_type_match(#function{} = Func, Args, Table) when is_list(Args) ->
	FuncTypes = Func#function.types,
	check_type_match(FuncTypes, Args, Table);
check_type_match(Known, Known, Table) ->
	{ok, Table};
check_type_match([ Type | KnownTail], [Type | UnknownTail], Table) ->
	check_type_match(KnownTail, UnknownTail, Table);
check_type_match([_ | KnownTail], [undefined | UnknownTail], Table) ->
	check_type_match(KnownTail, UnknownTail, Table);
check_type_match([{type_variable, '_'} | KnownTail], [_Ignored | UnknownTail], Table) ->
	check_type_match(KnownTail, UnknownTail, Table);
check_type_match([{type_variable, _} = TypeVariable | KnownTail], [ Type | UnknownTail], Table) ->
	FixedKnownTail = update_type_variable(TypeVariable, Type, KnownTail),
	check_type_match(FixedKnownTail, UnknownTail, Table);
check_type_match([Known | KnownTail], [{type_variable, _} = TypeVariable | UnknownTail], Table) ->
	FixedUnknownTail = update_type_variable(TypeVariable, Known, UnknownTail),
	check_type_match(KnownTail, FixedUnknownTail, Table);
check_type_match(_Known, [], Table) ->
	{ok, Table};
check_type_match([], [ _ | _], _Table) ->
	{error, too_many_types};
check_type_match([Known | KnownTail], [Unknown | UnknownTail], Table) ->
	case check_type_match(Known, Unknown, Table) of
		{ok, NewTable} ->
			check_type_match(KnownTail, UnknownTail, NewTable);
		Else ->
			Else
	end;
check_type_match(#function{ types = Known }, #function{ types = UnKnown }, Table) ->
	check_type_match(Known, UnKnown, Table);
check_type_match(_, {type_variable, '_'}, Table) ->
	{ok, Table};
check_type_match([Known], UnKnown, Table) ->
	check_type_match(Known, UnKnown, Table);
check_type_match(#concrete{} = Known, #concrete{} = UnKnown, Table) when Known =/= UnKnown ->
	io:format("reasonably certain we have a type mismatch:~nKnown: ~p~nUnknown: ~p~n", [Known, UnKnown]),
	{error, {type_mismatch, Known, UnKnown}};
check_type_match(Known, UnKnown, _Table) ->
	io:format("marker for check_type_match failure finding"),
	{error, {nyi, check_type_match, Known, UnKnown}}.

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

add_direct_imports(NameActual, DirectImports, Table) ->
	JustNames = [ E#milang_ast.data || E <- DirectImports ],
	lists:foldl(fun(LocalName, Acc) ->
		Entry = #alias{ truename = #{ module => NameActual, name => LocalName }},
		set_entry(LocalName, Entry, Acc)
	end, Table, JustNames).

refine_entry(Name, Refinement, Table) ->
	case add_entry(Name, Refinement, Table) of
		{ok, _} = Ok ->
			Ok;
		{error, {shadowing, OldEntry}} ->
			case refine(Refinement, OldEntry) of
				{ok, Refined} ->
					{ok, set_entry(Name, Refined, Table)};
				Error ->
					Error
			end
	end.

refine(undefined, Old) ->
	{ok, Old};
refine(New, undefined) ->
	{ok, New};
refine(New, Old) ->
	{error, {nyi, refine, New, Old}}.

maybe_type_of_nodes(_, {error, _} = Error) ->
	Error;
maybe_type_of_nodes(Nodes, {ok, Table}) ->
	type_of_nodes(Nodes, Table).

type_of_nodes(Nodes, Table) when is_list(Nodes) ->
	types_of_nodes(Nodes, Table);
type_of_nodes(#milang_ast{ type = type_function } = Node, Table) ->
	ArgNodes = Node#milang_ast.data,
	case type_of_nodes(ArgNodes, Table) of
		{ok, ArgTypes} ->
			{ok, #function{ types = ArgTypes}};
		Error ->
			Error
	end;
type_of_nodes(#milang_ast{ type = type_data } = Node, Table) ->
	#{ name := NameAST, args := Args } = Node#milang_ast.data,
	case type_of_nodes(Args, Table) of
		{ok, ArgTypes} ->
			Name = NameAST#milang_ast.data,
			{ok, #concrete{ concrete_of = Name, args = ArgTypes }};
		Error ->
			Error
	end;
type_of_nodes(#milang_ast{ type = type_name_remote} = Node, _Table) ->
	% if we're here, we're not just looking at some binding, we're trying
	% to figure out types. Which means this name _is_ defining a type, and
	% not just the name for a type.
	% ie, this is the 'String's in "repeat : String -> Int -> String."
	Name = Node#milang_ast.data,
	{ok, #concrete{ concrete_of = Name, args = []}};
type_of_nodes(#milang_ast{ type = variable} = Node, Table) ->
	Name = Node#milang_ast.data,
	case lookup(Name, Table) of
		{error,notfound} ->
			{ok, {type_variable, Name}};
		Ok ->
			Ok
	end;
type_of_nodes(#milang_ast{ type = literal_string }, _Table) ->
	{ok, #concrete{ concrete_of = #{ module => 'Core', name => 'String' }}};
type_of_nodes(#milang_ast{ type = literal_integer }, _Table) ->
	{ok, #concrete{ concrete_of = #{ module => 'Core', name => 'Integer' }}};
type_of_nodes(#milang_ast{ type = type_name_local } = Node, Table) ->
	LocalName = Node#milang_ast.data,
	case resolve_type(LocalName, Table) of
		{error, notfound} ->
			{error, {type_not_found, LocalName}};
		Ok ->
			Ok
	end;
type_of_nodes(#milang_ast{ type = expression_call } = Node, Table) ->
	#{ name := NameAST, args := ArgsASTs } = Node#milang_ast.data,
	Name = NameAST#milang_ast.data,
	case resolve_type(Name, Table) of
		{error, notfound} ->
			{error, {function_not_found, Name}};
		{ok, FuncType} ->
			case types_of_nodes(ArgsASTs, Table) of
				{ok, ArgTypes} ->
					FuncArgs = FuncType#function.types,
					merge_types(FuncArgs, ArgTypes);
				E ->
					E
			end
	end;
type_of_nodes(Nodes, Table) ->
	{error, {nyi, type_of_nodes, Nodes, Table}}.

types_of_nodes(Nodes, Table) ->
	types_of_nodes(Nodes, Table, []).

types_of_nodes([], _Table, Acc) ->
	{ok, lists:reverse(Acc)};
types_of_nodes([ Head | Tail], Table, Acc) ->
	case type_of_nodes(Head, Table) of
		{ok, Type} ->
			types_of_nodes(Tail, Table, [Type | Acc]);
		Error ->
			Error
	end.

merge_types(A, A) ->
	{ok, A};
merge_types(A, undefined) ->
	{ok, A};
merge_types([Single], []) ->
	{ok, Single};
merge_types(Func, []) when is_list(Func) ->
	{ok, #function{ types = Func }};
merge_types(T, []) ->
	{ok, T};
merge_types([A | ATail], [A | BTail]) ->
	merge_types(ATail, BTail);
merge_types([{type_variable, '_'} | ATail], [_ | BTail]) ->
	merge_types(ATail, BTail);
merge_types([_ | ATail], [{type_variable, '_'} | BTail]) ->
	merge_types(ATail, BTail);
merge_types([{type_variable, _} = AVar | _], [{type_variable, _} = BVar | _]) ->
	{error, {type_mismatch_differ_varnames, AVar, BVar}};
merge_types([{type_variable, _} = AVar | ATail], [ B | BTail]) ->
	FixedA = update_type_variable(AVar, B, ATail),
	FixedB = update_type_variable(AVar, B, BTail),
	merge_types(FixedA, FixedB);
merge_types([A | ATail], [B | BTail]) ->
	case merge_types(ATail, BTail) of
		{ok, NewTail} ->
			case merge_types(A, B) of
				{ok, NewHead} when is_list(NewHead), is_list(NewTail) ->
					{ok, NewHead ++ NewTail};
				{ok, NewHead} when is_list(NewHead) ->
					{ok, NewHead ++ [NewTail]};
				{ok, NewHead} when is_list(NewTail) ->
					{ok, [NewHead | NewTail]};
				{ok, NewHead} ->
					{ok, [NewHead, NewTail]};
				HeadError ->
					HeadError
			end;
		TailError ->
			TailError
	end;
merge_types(#function{ types = A}, #function{ types = B }) ->
	merge_types(A, B);
merge_types(A,B) ->
	{error,{type_mismatch, A, B}}.

-spec add_entry(name(), type_entry(), lookup_table()) -> {ok, lookup_table()} | {error, {shadowing, type_entry()}}.
add_entry(Name, Entry, Table) ->
	case lookup(Name, Table) of
		{error, notfound} ->
			NewTable = set_entry(Name, Entry, Table),
			{ok, NewTable};
		{ok, Entry} ->
			{error, {shadowing, Entry}}
	end.

-spec set_entry(name(), type_entry(), lookup_table()) -> lookup_table().
set_entry(Name, Entry, Table) ->
	[OldHead | Tail] = Table,
	NewHead = maps:put(Name, Entry, OldHead),
	[NewHead | Tail].

-spec lookup(name(), lookup_table()) -> {error, notfound} | {ok, type_entry()}.
lookup(Name, [Table | Tail]) ->
	case maps:find(Name, Table) of
		error when Tail =:= [] ->
			{error, notfound};
		error ->
			lookup(Name, Tail);
		{ok, _} = Ok ->
			Ok
	end.


-spec resolve_type(name(), lookup_table()) -> {ok, type_entry()} | {error, notfound}.
resolve_type(Name, Table) ->
	case lookup(Name, Table) of
		{ok, #alias{} = A} ->
			resolve_type(A#alias.truename, Table);
		{ok, _NotAlias} = Ok ->
			Ok;
		Error ->
			Error
	end.

-spec resolve_name(name(), lookup_table()) -> {ok, name()} | {error, notfound}.
resolve_name(Name, Table) ->
	case lookup(Name, Table) of
		{ok, #alias{} = A} ->
			resolve_name(A#alias.truename, Table);
		{ok, _NotAlias} ->
			{ok, Name};
		Error ->
			Error
	end.
