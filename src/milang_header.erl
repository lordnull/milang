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

-include("milang_ast.hrl").
-include("milang_log.hrl").

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
	TranslationTable = build_translation_table(AST),
	milang_log:it(debug, ?log_info, "Built that translation table: ~p", [TranslationTable]),
	OutAST = create_header_ast(AST, TranslationTable),
	lists:foreach(fun(HeaderWorthy) ->
		ok = io:put_chars(IoDev, milang_ast:to_string(HeaderWorthy))
	end, OutAST).

build_translation_table(AST) ->
	lists:foldl(fun build_translation_table/2, #{}, AST).

build_translation_table(#milang_ast{ data = #declaration_module{}} = Node, Table) ->
	#declaration_module{ name = Name, exposing = Exposing } = Node#milang_ast.data,
	lists:foldl(fun(ExposingNode, Acc) ->
		add_remote_translation(Name, ExposingNode, Acc)
	end, Table, Exposing);
build_translation_table(#milang_ast{ data = #declaration_import{}} = Node, Table) ->
	#declaration_import{ name = ModuleName, alias = Alias, exposing = Exposing} = Node#milang_ast.data,
	TableWithAlias = case Alias of
		undefined ->
			Table;
		_ ->
			Table#{ {module, Alias} => ModuleName }
	end,
	lists:foldl(fun(ExposingNode, Acc) ->
		add_remote_translation(ModuleName, ExposingNode, Acc)
	end, TableWithAlias, Exposing);
build_translation_table(#milang_ast{ data = #declaration_type{}} = Node, Table) ->
	#declaration_type{ name = LocalName} = Node#milang_ast.data,
	case maps:find(LocalName, Table) of
		error ->
			% it's an internal type (not exposed), so we can ignore it.
			Table;
		{ok, RemoteName} ->
			{_, #{ module := Module}} = RemoteName,
			#declaration_type{ args = Args, constraints = Constraints, constructors = Constructors } = Node#milang_ast.data,
			lists:foldl(fun(TypeNode, Acc) ->
				add_remote_translation(Module, TypeNode, Acc)
			end, Table, Args ++ Constraints ++ Constructors)
	end;
build_translation_table(_, Table) ->
	Table.

add_remote_translation(ModuleName, {Nametype, LocalName} = OriginalName, Table) when is_atom(LocalName) ->
	NewName = {Nametype, #{ local => LocalName, module => ModuleName }},
	Table#{ OriginalName => NewName };
add_remote_translation(ModuleName, #milang_ast{ data = #declaration_type{} } = Node, Table) ->
	#declaration_type{ name = NameAST, args = Args } = Node#milang_ast.data,
	MidTable = add_remote_translation(ModuleName, NameAST, Table),
	lists:foldl(fun(ArgNode, Acc) ->
		add_remote_translation(ModuleName, ArgNode, Acc)
	end, MidTable, Args);
add_remote_translation(ModuleName, #milang_ast{ data = #constructor{} } = Node, Table) ->
	#constructor{ name = Name } = Node#milang_ast.data,
	% The args use exising types, and thus get converted after we've built the
	% translation table fully.
	add_remote_translation(ModuleName, Name, Table);
add_remote_translation(_Module, _Node, Table) ->
	milang_log:it(debug, ?log_info, "Skipping tranlation for ~p.", [_Node]),
	Table.

create_header_ast(AST, TranslationTable) ->
	lists:filtermap(fun(Node) ->
		maybe_convert_node(Node, TranslationTable)
	end, AST).

maybe_convert_node(#milang_ast{ data = #declaration_function{}}, _Translations) ->
	milang_log:it(debug, ?log_info, "ignoring declaration_function"),
	false;
maybe_convert_node(#milang_ast{ data = #declaration_spec{}} = Node, Translations) ->
	#declaration_spec{ name = SpecName } = Node#milang_ast.data,
	case maps:find(SpecName, Translations) of
		error ->
			milang_log:it(debug, ?log_info, "Ignoring declaration_spec for ~p", [SpecName]),
			false;
		{ok, _TrueName} ->
			{true, convert_node(Node, Translations)}
	end;
maybe_convert_node(#milang_ast{ data = #declaration_module{}}, _Translations) ->
	milang_log:it(debug, ?log_info, "Ignoring declaration_module"),
	false;
maybe_convert_node(#milang_ast{ data = #declaration_import{}}, _Translations) ->
	milang_log:it(debug, ?log_info, "Ignoring declaration_import"),
	false;
maybe_convert_node(#milang_ast{ data = #declaration_type{}} = Node, Translations) ->
	#declaration_type{ name = TypeName } = Node#milang_ast.data,
	case maps:find(TypeName, Translations) of
		error ->
			milang_log:it(debug, ?log_info, "Ignoring declaration_type for ~p", [TypeName]),
			false;
		{ok, _} ->
			{true, convert_node(Node, Translations)}
	end;
maybe_convert_node(#milang_ast{ data = #declaration_alias{} } = Node, Translations) ->
	#declaration_alias{ name = TypeName } = Node#milang_ast.data,
	case maps:find(TypeName, Translations) of
		error ->
			milang_log:it(debug, ?log_info, "Ignoring declaration_alias for ~p", [TypeName]),
			false;
		{ok, _} ->
			{true, convert_node(Node, Translations)}
	end.

convert_node(Node, Translations) ->
	milang_log:it(debug, ?log_info, "Converting node ~p", [Node]),
	milang_ast:transform_data(fun(D) ->
		convert_node_data(D, Translations)
	end, Node).

convert_node_data(#declaration_module{exposing = Exposing} = Data, Translations) ->
	NewExposing = lists:map(fun(N) -> convert_node(N, Translations) end, Exposing),
	Data#declaration_module{ exposing = NewExposing };
convert_node_data(#declaration_spec{ name = SpecName, type = Type} = Data, Translations) ->
	NewName = maps:get(SpecName, Translations, SpecName),
	NewType = convert_node(Type, Translations),
	Data#declaration_spec{ name = NewName, type = NewType };
convert_node_data(#declaration_alias{ name = Name, alias_of = Original, constraints = Constraints} = Data, Translations) ->
	NewName = maps:get(Name, Translations, Name),
	NewOriginal = convert_node(Original, Translations),
	NewConstraints = lists:map(fun(N) -> convert_node(N, Translations) end, Constraints),
	Data#declaration_alias{ name = NewName, alias_of = NewOriginal, constraints = NewConstraints };
convert_node_data(#declaration_type{ name = Name, constraints = Constraints, constructors = Constructors} = Data, Translations) ->
	NewName = maps:get(Name, Translations, Name),
	NewConstraints = lists:map(fun(N) -> convert_node(N, Translations) end, Constraints),
	NewConstructors = lists:map(fun(N) -> convert_node(N, Translations) end, Constructors),
	Data#declaration_type{ name = NewName, constraints = NewConstraints, constructors = NewConstructors };
convert_node_data(#constructor{ name = Name, args = Args } = Data, Translations) ->
	NewName = maps:get(Name, Translations, Name),
	NewArgs = lists:map(fun(N) -> convert_node(N, Translations) end, Args),
	Data#constructor{ name = NewName, args = NewArgs};
convert_node_data(#type_concrete{ name = Name, args = Args } = Data, Translations) ->
	NewName = maps:get(Name, Translations, Name),
	NewArgs = [ convert_node(N, Translations) || N <- Args ],
	Data#type_concrete{ name = NewName, args = NewArgs};
convert_node_data(#type_record{ fields = Fields } = Data, Translations) ->
	NewFields = [convert_node(F, Translations) || F <- Fields],
	Data#type_record{ fields = NewFields };
convert_node_data(#type_record_field{ type = Type } = Data, Translations) ->
	NewType = convert_node(Type, Translations),
	Data#type_record_field{ type = NewType };
convert_node_data(#type_function{ args = Args} = Data, Translations) ->
	NewArgs = [ convert_node(A, Translations) || A <- Args ],
	Data#type_function{ args = NewArgs }.
