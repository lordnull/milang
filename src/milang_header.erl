%% @doc This is to generate 'header' files based on partial milang source code.
%%
%% Problem: importing modules without needing to constantly re-compile them, or
%% in the case of system modules, where compiling is basically impossible.
%%
%% Solution: header files. A header file is basically meant to be a file that
%% can be slurpped into a milang lookup table used by milang_compile (implemented
%% in milang_type_validation). The first attempt was just doing an elrang config
%% formatted file and reading that directly into the lookup table. This worked
%% until the lookup table format changed.
%%
%% I could have written a way to read erlang with annotations for the system
%% files, but I already have a milang parser. If I run it through something
%% other than a compiler, say something that interacts with the type validation
%% module, I can write my "headers" in milang, as well as generate the headers
%% for milang.
-module(milang_header).

%-include("milang_ast.hrl").
-include_lib("kernel/include/logger.hrl").

-export([create_header/2]).

%% @doc The ast must already have passed type checking and linter, otherwise
%% this will likely make a bad header. Also we make no check that the module name
%% in the ast will match whatever we're writing to (file or otherwise).
%%
%% A header file is "valid" milang syntax. Valid in that it's got type and
%% function names where we expect them, but any aliases or local names become
%% fully qualified. This allows other systems to simply read the header and
%% to load into the type table without translations.
-spec create_header([ milang_ast:ast_node()], io:device()) -> 'ok'.
create_header(AST, IoDev) ->
	ModuleName = extract_module_name(AST),
	Translations = #{},
	_ = lists:foldl(fun(Node, TranslationsAcc) ->
		maybe_write_node(Node, ModuleName, IoDev, TranslationsAcc)
	end, Translations, AST),
	ok.

maybe_write_node(Node, Module, Io, Translations) ->
	Type = milang_ast:type_simply(Node),
	maybe_write_node(Type, Node, Module, Io, Translations).

maybe_write_node(expose, Node, Module, Io, Translations) ->
	Data = milang_ast:data(Node),
	DeclarationNode = milang_ast_expose:declaration(Data),
	DeclarationType = milang_ast:type_simply(DeclarationNode),
	write_node(DeclarationType, DeclarationNode, Module, Io, Translations);
maybe_write_node('expose all', Node, Module, Io, Translations) ->
	Data = milang_ast:data(Node),
	DeclarationNode = milang_ast_expose:declaration(Data),
	DeclarationType = milang_ast:type_simply(DeclarationNode),
	write_node(DeclarationType, DeclarationNode, Module, Io, Translations);
maybe_write_node(alias, Node, Module, _Io, Translations) ->
	?LOG_DEBUG("Determine if an alias is simple or not: ~p", [Node]),
	case is_simple_alias(milang_ast:data(Node)) of
		true ->
			add_alias_translation(Node, Translations);
		false ->
			Data = milang_ast:data(Node),
			NameNode = milang_ast_alias:name(Data),
			maybe_add_translation(NameNode, Module, Translations)
	end;
maybe_write_node(_, Node, _Module, _Io, Translations) ->
	?LOG_DEBUG("No need to write a node that is not exposed: ~p", [Node]),
	Translations.

add_alias_translation(Node, Translations) ->
	Data = milang_ast:data(Node),
	NameNode = milang_ast_alias:name(Data),
	case milang_ast:data(NameNode) of
		{_, Map} when is_map(Map) ->
			Translations;
		{_, Name} ->
			OriginalNode = milang_ast_alias:original(Data),
			Original = milang_ast:data(OriginalNode),
			OriginalNameNode = milang_ast_concrete:name(Original),
			{_, OriginalName} = milang_ast:data(OriginalNameNode),
			?LOG_DEBUG("Adding translation of alias ~p to ~p", [Name, OriginalName]),
			Translations#{ Name => OriginalName}
	end.

maybe_add_translation(Name, _Module, Translations) when is_map(Name) ->
	Translations;
maybe_add_translation(Name, Module, Translations) when is_binary(Name) ->
	?LOG_DEBUG("Making ~p an remote for module ~p", [Name, Module]),
	Translations#{ Name => #{ local => Name, module => Module }};
maybe_add_translation({_, Name}, Module, Translations) ->
	maybe_add_translation(Name, Module, Translations);
maybe_add_translation(Node, Module, Translations) ->
	maybe_add_translation(milang_ast:data(Node), Module, Translations).

lookup_translation(Key, Translations) ->
	maps:get(Key, Translations, Key).

write_node(Type, Node, Module, Io, Translations) ->
	{Converted, NewTranslations} = convert_node(Type, Node, Module, Translations),
	String = to_string(Converted),
	ok = io:put_chars(Io, String),
	NewTranslations.

convert_node(type, Node, Module, Translations) ->
	Type = milang_ast:data(Node),
	NameNode = milang_ast_type:name(Type),
	NameType = milang_ast:type_simply(NameNode),
	TranslationsWithName = maybe_add_translation(NameNode, Module, Translations),
	{NewName, TranslationsWithName2} = convert_node(NameType, NameNode, Module, TranslationsWithName),

	ConstraintNodes = milang_ast_type:constraints(Type),
	{NewConstraints, TranslationsWithConstraints} = lists:mapfoldl(fun(N, TAcc) ->
		NType = milang_ast:type_simply(N),
		convert_node(NType, N, Module, TAcc)
	end, TranslationsWithName2, ConstraintNodes),

	ConstructorNodes = milang_ast_type:constructors(Type),
	{NewConstructors, TranslationsWithConstructors} = lists:mapfoldl(fun(N, TAcc) ->
		NType = milang_ast:type_simply(N),
		convert_node(NType, N, Module, TAcc)
	end, TranslationsWithConstraints, ConstructorNodes),

	NewNode = milang_ast:transform_data(fun(_) ->
		milang_ast_type:name(NewName, milang_ast_type:constructors(NewConstructors, milang_ast_type:constraints(NewConstraints, Type)))
	end, Node),

	{NewNode, TranslationsWithConstructors};

convert_node(constructor, Node, Module, Translations) ->
	Constructor = milang_ast:data(Node),

	NameNode = milang_ast_constructor:name(Constructor),
	NameType = milang_ast:type_simply(NameNode),
	TranslationsWithName = maybe_add_translation(NameNode, Module, Translations),
	{NewName, TranslationsWithName2} = convert_node(NameType, NameNode, Module, TranslationsWithName),

	ArgNodes = milang_ast_constructor:args(Constructor),
	{NewArgs, TranslationsWithArgs} = lists:mapfoldl(fun(ArgNode, TAcc) ->
		case milang_ast:type_simply(ArgNode) of
			type_concrete ->
				convert_node(type_concrete, ArgNode, Module, TAcc);
			identifier_type ->
				convert_node(identifier_type, ArgNode, Module, TAcc);
			_ ->
				ArgNode
		end
	end, TranslationsWithName2, ArgNodes),

	NewNode = milang_ast:transform_data(fun(_) ->
		milang_ast_constructor:name(NewName, milang_ast_constructor:args(NewArgs, Constructor))
	end, Node),
	{NewNode, TranslationsWithArgs};

convert_node(alias, Node, Module, Translations) ->
	Alias = milang_ast:data(Node),

	NameNode = milang_ast_alias:name(Alias),
	NameType = milang_ast:type_simply(NameNode),
	TranslationsWithName = maybe_add_translation(NameNode, Module, Translations),
	{NewName, TranslationsWithName2} = convert_node(NameType, NameNode, Module, TranslationsWithName),

	ConstraintNodes = milang_ast_alias:constraints(Alias),
	{NewConstraints, TranslationsWithConstraints} = lists:mapfoldl(fun(ConstraintNode, TAcc) ->
		ConstraintType = milang_ast:type_simply(ConstraintNode),
		convert_node(ConstraintType, ConstraintNode, Module, TAcc)
	end, TranslationsWithName2, ConstraintNodes),

	OriginalNode = milang_ast_alias:original(Alias),
	OriginalType = milang_ast:type_simply(OriginalNode),
	{NewOriginal, FinalTranslations} = convert_node(OriginalType, OriginalNode, Module, TranslationsWithConstraints),

	NewNode = milang_ast:transform_data(fun(_) ->
		milang_ast_alias:name(NewName, milang_ast_alias:constraints(NewConstraints, milang_ast_alias:original(NewOriginal, Alias)))
	end, Node),

	{NewNode, FinalTranslations};

convert_node(concrete, Node, Module, Translations) ->
	Concrete = milang_ast:data(Node),

	NameNode = milang_ast_concrete:name(Concrete),
	%NewName = lookup_translation(Name, Translations),
	{NewName, TransaltionsWithConcrete} = convert_node(milang_ast:type_simply(NameNode), NameNode, Module, Translations),

	ArgNodes = milang_ast_concrete:args(Concrete),
	{NewArgs, NewTranslations} = lists:mapfoldl(fun(ArgNode, TAcc) ->
		T = milang_ast:type_simply(ArgNode),
		convert_node(T, ArgNode, Module, TAcc)
	end, TransaltionsWithConcrete, ArgNodes),

	NewNode = milang_ast:transform_data(fun(_) ->
		milang_ast_concrete:name(NewName, milang_ast_concrete:args(NewArgs, Concrete))
	end, Node),
	{NewNode, NewTranslations};

convert_node(spec, Node, Module, Translations) ->
	Spec = milang_ast:data(Node),

	NameNode = milang_ast_spec:name(Spec),
	NameType = milang_ast:type_simply(NameNode),
	TranslationsWithName = maybe_add_translation(NameNode, Module, Translations),
	{NewName, TranslationsWithName2} = convert_node(NameType, NameNode, Module, TranslationsWithName),

	TypeNode = milang_ast_spec:type(Spec),
	TypeType = milang_ast:type_simply(TypeNode),
	{NewType, TranslationsWithType} = convert_node(TypeType, TypeNode, Module, TranslationsWithName2),

	NewNode = milang_ast:transform_data(fun(_) ->
		milang_ast_spec:name(NewName, milang_ast_spec:type(NewType, Spec))
	end, Node),

	{NewNode, TranslationsWithType};

convert_node(signature, Node, Module, Translations) ->
	Sig = milang_ast:data(Node),

	ArgNodes = milang_ast_signature:args(Sig),
	{NewArgs, FinalTranslations} = lists:mapfoldl(fun(ArgNode, TAcc) ->
		ArgType = milang_ast:type_simply(ArgNode),
		convert_node(ArgType, ArgNode, Module, TAcc)
	end, Translations, ArgNodes),
	NewNode = milang_ast:transform_data(fun(_) ->
		milang_ast_signature:args(NewArgs, Sig)
	end, Node),
	{NewNode, FinalTranslations};

convert_node(identifier_type, Node, _Module, Translations) ->
	NewNode = milang_ast:transform_data(fun({T, Name}) ->
		NewName = lookup_translation(Name, Translations),
		{T, NewName}
	end, Node),
	{NewNode, Translations};

convert_node(identifier_bound, Node, _Module, Translations) ->
	NewNode = milang_ast:transform_data(fun({T, Name}) ->
		NewName = lookup_translation(Name, Translations),
		{T, NewName}
	end, Node),
	{NewNode, Translations};

convert_node(_Type, Node, _Module, Translations) ->
	?LOG_DEBUG("oh got, what? ~p", [Node]),
	{Node, Translations}.

to_string(Node) ->
	milang_delex:string(Node).

is_simple_alias(Data) ->
	ArgNodes = milang_ast_alias:args(Data),
	OriginalNode = milang_ast_alias:original(Data),
	case milang_ast:type_simply(OriginalNode) of
		concrete ->
			OriginalData = milang_ast:data(OriginalNode),
			OriginalArgs = milang_ast_concrete:args(OriginalData),
			length(ArgNodes) == length(OriginalArgs);
		_ ->
			false
	end.

%% @doc The ast must already have passed type checking and linter, otherwise
%% this will likely make a bad header. Also we make no check that the module name
%% in the ast will match whatever we're writing to (file or otherwise).
%%
%% A header file is "valid" milang syntax. Valid in that it's got type and
%% function names where we expect them, but any aliases or local names become
%% fully qualified. This allows other systems to simply read the header and
%% to load into the type table without translations.
%-spec create_header([ milang_ast:ast_node()], io:device()) -> 'ok'.
%create_header(AST, IoDev) ->
%	TranslationTable = build_translation_table(AST),
%	?LOG_DEBUG("Built that translation table: ~p", [TranslationTable]),
%	OutAST = create_header_ast(AST, TranslationTable),
%	lists:foreach(fun(HeaderWorthy) ->
%		ok = io:put_chars(IoDev, milang_ast:to_string(HeaderWorthy))
%	end, OutAST).

extract_module_name([]) ->
	error(no_module_name);
extract_module_name([Node | Tail]) ->
	case milang_ast:type_simply(Node) of
		module ->
			Data = milang_ast:data(Node),
			milang_ast_module:name_as_string(Data);
		_ ->
			extract_module_name(Tail)
	end.
%
%build_translation_table(AST) ->
%	ModuleName = extract_module_name(AST),
%	FoldFun = fun(Node, Table) ->
%		Type = milang_ast:type_simply(Node),
%		build_translation_table(Type, Node, Module, Table)
%	end,
%	lists:foldl(FoldFun, #{}, AST).
%
%build_translation_table('expose all', Node, Module, Table) ->
%	ExposeData = milang_ast:data(Node),
%	TypeDeclarationNode = milang_ast_expose:declaration(ExposeData),
%	add_remote_translations(Module, TypeDeclarationNode, Table);
%build_translation_table(expose, Node, Module, Table) ->
%	ExposeData = milang_ast:data(Node),
%	DeclarationNode = milang_ast_expose:declaration(ExposeData),
%	add_remote_translations(Module, DeclarationNode, Table);
%build_translation_table(_NotExpose, _Node, _Module, Table) ->
%	Table.



%build_translation_table(module, Node, Table) ->
%	Data = milang_ast:data(Node),
%
%	#declaration_module{ name = Name, exposing = Exposing } = Node#milang_ast.data,
%	lists:foldl(fun(ExposingNode, Acc) ->
%		add_remote_translation(Name, ExposingNode, Acc)
%	end, Table, Exposing);
%build_translation_table(#milang_ast{ data = #declaration_import{}} = Node, Table) ->
%	#declaration_import{ name = ModuleName, alias = Alias, exposing = Exposing} = Node#milang_ast.data,
%	TableWithAlias = case Alias of
%		undefined ->
%			Table;
%		_ ->
%			Table#{ {module, Alias} => ModuleName }
%	end,
%	lists:foldl(fun(ExposingNode, Acc) ->
%		add_remote_translation(ModuleName, ExposingNode, Acc)
%	end, TableWithAlias, Exposing);
%build_translation_table(#milang_ast{ data = #declaration_type{}} = Node, Table) ->
%	#declaration_type{ name = LocalName} = Node#milang_ast.data,
%	case maps:find(LocalName, Table) of
%		error ->
%			% it's an internal type (not exposed), so we can ignore it.
%			Table;
%		{ok, RemoteName} ->
%			{_, #{ module := Module}} = RemoteName,
%			#declaration_type{ args = Args, constraints = Constraints, constructors = Constructors } = Node#milang_ast.data,
%			lists:foldl(fun(TypeNode, Acc) ->
%				add_remote_translation(Module, TypeNode, Acc)
%			end, Table, Args ++ Constraints ++ Constructors)
%	end;
%build_translation_table(_, Table) ->
%	Table.
%
%add_remote_translations(Module, Nodes, Table) when is_list(Nodes) ->
%	lists:foldl(fun(E, Acc) ->
%		add_remote_translations(Module, E, Acc)
%	end, Table, Nodes);
%add_remote_translations(Module, Node, Table) ->
%	Type = milang_ast:type_simply(Node),
%	add_remote_translations(Type, Module, Node, Table).
%
%add_remote_translations(type, Module, Node, Table) ->
%	Data = milang_ast:data(Node),
%	NameNode = milang_ast_type:name(Data),
%	Constructors = milang_ast_type:constructors(Data),
%	WithName = add_remote_translations(Module, NameNode, Table),
%	add_remote_translations(Module, Constructors, WithName);
%add_remote_translations(constructor, Module, Node, Table) ->
%	Data = milang_ast:data(Node),
%	NameNode = milang_ast_type:name(Data),
%	add_remote_translations(Module, NameNode, Table);
%add_remote_translations(spec, Module, Node, Table) ->
%	Data = milang_ast:data(Node),
%	NameNode = milang_ast_type:name(Data),
%	add_remote_translations(Module, NameNode, Table);
%add_remote_translations(alias, Module, Node, Table) ->
%	Data = milang_ast:data(Node),
%	NameNode = milang_ast_type:name(Data),
%	add_remote_translations(Module, NameNode, Table);
%add_remote_translations(_type, _Module, Node, Table) ->
%	?LOG_DEBUG("No translation to register for node ~p", [Node]),
%	Table.


%add_remote_translation(ModuleName, {Nametype, LocalName} = OriginalName, Table) when is_atom(LocalName) ->
%	NewName = {Nametype, #{ local => LocalName, module => ModuleName }},
%	Table#{ OriginalName => NewName };
%add_remote_translation(ModuleName, #milang_ast{ data = #declaration_type{} } = Node, Table) ->
%	#declaration_type{ name = NameAST, args = Args } = Node#milang_ast.data,
%	MidTable = add_remote_translation(ModuleName, NameAST, Table),
%	lists:foldl(fun(ArgNode, Acc) ->
%		add_remote_translation(ModuleName, ArgNode, Acc)
%	end, MidTable, Args);
%add_remote_translation(ModuleName, #milang_ast{ data = #constructor{} } = Node, Table) ->
%	#constructor{ name = Name } = Node#milang_ast.data,
%	% The args use exising types, and thus get converted after we've built the
%	% translation table fully.
%	add_remote_translation(ModuleName, Name, Table);
%add_remote_translation(_Module, _Node, Table) ->
%	milang_log:it(debug, ?log_info, "Skipping tranlation for ~p.", [_Node]),
%	Table.
%
%create_header_ast(AST, TranslationTable) ->
%	lists:filtermap(fun(Node) ->
%		maybe_convert_node(Node, TranslationTable)
%	end, AST).
%
%maybe_convert_node(Node, Table) ->
%	Type = milang_ast:type_simply(Node),
%	maybe_convert_node(Type, Node, Table).
%
%maybe_convert_node(spec, Node, Table) ->
%	Data = milang_ast:data(Node),
%	NameNode = milang_ast_spec:name(Data),
%	case has_translation(NameNode, Table) of
%		{ok, TrueName} ->
%			{true, convert_node(spec, Node, Table)};
%		error ->
%			false
%	end;
%
%maybe_convert_node(#milang_ast{ data = #declaration_function{}}, _Translations) ->
%	milang_log:it(debug, ?log_info, "ignoring declaration_function"),
%	false;
%maybe_convert_node(#milang_ast{ data = #declaration_spec{}} = Node, Translations) ->
%	#declaration_spec{ name = SpecName } = Node#milang_ast.data,
%	case maps:find(SpecName, Translations) of
%		error ->
%			milang_log:it(debug, ?log_info, "Ignoring declaration_spec for ~p", [SpecName]),
%			false;
%		{ok, _TrueName} ->
%			{true, convert_node(Node, Translations)}
%	end;
%maybe_convert_node(#milang_ast{ data = #declaration_module{}}, _Translations) ->
%	milang_log:it(debug, ?log_info, "Ignoring declaration_module"),
%	false;
%maybe_convert_node(#milang_ast{ data = #declaration_import{}}, _Translations) ->
%	milang_log:it(debug, ?log_info, "Ignoring declaration_import"),
%	false;
%maybe_convert_node(#milang_ast{ data = #declaration_type{}} = Node, Translations) ->
%	#declaration_type{ name = TypeName } = Node#milang_ast.data,
%	case maps:find(TypeName, Translations) of
%		error ->
%			milang_log:it(debug, ?log_info, "Ignoring declaration_type for ~p", [TypeName]),
%			false;
%		{ok, _} ->
%			{true, convert_node(Node, Translations)}
%	end;
%maybe_convert_node(#milang_ast{ data = #declaration_alias{} } = Node, Translations) ->
%	#declaration_alias{ name = TypeName } = Node#milang_ast.data,
%	case maps:find(TypeName, Translations) of
%		error ->
%			milang_log:it(debug, ?log_info, "Ignoring declaration_alias for ~p", [TypeName]),
%			false;
%		{ok, _} ->
%			{true, convert_node(Node, Translations)}
%	end.
%
%convert_node(Node, Translations) ->
%	milang_log:it(debug, ?log_info, "Converting node ~p", [Node]),
%	milang_ast:transform_data(fun(D) ->
%		convert_node_data(D, Translations)
%	end, Node).
%
%convert_node_data(#declaration_module{exposing = Exposing} = Data, Translations) ->
%	NewExposing = lists:map(fun(N) -> convert_node(N, Translations) end, Exposing),
%	Data#declaration_module{ exposing = NewExposing };
%convert_node_data(#declaration_spec{ name = SpecName, type = Type} = Data, Translations) ->
%	NewName = maps:get(SpecName, Translations, SpecName),
%	NewType = convert_node(Type, Translations),
%	Data#declaration_spec{ name = NewName, type = NewType };
%convert_node_data(#declaration_alias{ name = Name, alias_of = Original, constraints = Constraints} = Data, Translations) ->
%	NewName = maps:get(Name, Translations, Name),
%	NewOriginal = convert_node(Original, Translations),
%	NewConstraints = lists:map(fun(N) -> convert_node(N, Translations) end, Constraints),
%	Data#declaration_alias{ name = NewName, alias_of = NewOriginal, constraints = NewConstraints };
%convert_node_data(#declaration_type{ name = Name, constraints = Constraints, constructors = Constructors} = Data, Translations) ->
%	NewName = maps:get(Name, Translations, Name),
%	NewConstraints = lists:map(fun(N) -> convert_node(N, Translations) end, Constraints),
%	NewConstructors = lists:map(fun(N) -> convert_node(N, Translations) end, Constructors),
%	Data#declaration_type{ name = NewName, constraints = NewConstraints, constructors = NewConstructors };
%convert_node_data(#constructor{ name = Name, args = Args } = Data, Translations) ->
%	NewName = maps:get(Name, Translations, Name),
%	NewArgs = lists:map(fun(N) -> convert_node(N, Translations) end, Args),
%	Data#constructor{ name = NewName, args = NewArgs};
%convert_node_data(#type_concrete{ name = Name, args = Args } = Data, Translations) ->
%	NewName = maps:get(Name, Translations, Name),
%	NewArgs = [ convert_node(N, Translations) || N <- Args ],
%	Data#type_concrete{ name = NewName, args = NewArgs};
%convert_node_data(#type_record{ fields = Fields } = Data, Translations) ->
%	NewFields = [convert_node(F, Translations) || F <- Fields],
%	Data#type_record{ fields = NewFields };
%convert_node_data(#type_record_field{ type = Type } = Data, Translations) ->
%	NewType = convert_node(Type, Translations),
%	Data#type_record_field{ type = NewType };
%convert_node_data(#type_function{ args = Args} = Data, Translations) ->
%	NewArgs = [ convert_node(A, Translations) || A <- Args ],
%	Data#type_function{ args = NewArgs }.
