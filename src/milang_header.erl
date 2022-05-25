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

-export([create_header/2]).


%% @doc The ast must already have passed type checking and linter, otherwise
%% this will likley make a bad header. Also we make no check that the module name
%% in the as will match whatever we're writing to (file or otherwise).
%%
%% A header file is "valid" milang syntax. Valid in that it's got type and
%% function names where we expect them, but any aliases or local names become
%% fully qualified. This allows other systems to simply read the header and
%% to load into the type table without translations.
create_header(AST, IoDev) ->
	TranslationTable = build_translation_table(AST),
	OutAST = create_header_ast(AST, TranslationTable),
	lists:foreach(fun(HeaderWorthy) ->
		ok = io:put_chars(IoDev, milang_ast:to_string(HeaderWorthy))
	end, OutAST).

build_translation_table(AST) ->
	lists:foldl(fun build_translation_table/2, #{}, AST).

build_translation_table(#milang_ast{ type = declaration_module} = Node, Table) ->
	#{ name := Name, exposing := Exposing } = Node#milang_ast.data,
	lists:foldl(fun(ExposingNode, Acc) ->
		add_remote_translation(Name, ExposingNode, Acc)
	end, Table, Exposing);
build_translation_table(#milang_ast{ type = declaration_import} = Node, Table) ->
	#{ name := ModuleName, alias := Alias, exposing := Exposing} = Node#milang_ast.data,
	TableWithAlias = case Alias of
		undefined ->
			Table;
		_ ->
			Table#{ {module, Alias} => ModuleName }
	end,
	lists:foldl(fun(ExposingNode, Acc) ->
		add_remote_translation(ModuleName, ExposingNode, Acc)
	end, TableWithAlias, Exposing);
build_translation_table(#milang_ast{ type = declaration_type} = Node, Table) ->
	#{ name := NameAST} = Node#milang_ast.data,
	LocalName = NameAST#milang_ast.data,
	case maps:find(LocalName, Table) of
		error ->
			% it's an internal type (not exposed), so we can ignore it.
			Table;
		{ok, RemoteName} ->
			#{ module := Module} = RemoteName,
			#{ args := Args, constraints := Constraints, constructors := Constructors } = Node#milang_ast.data,
			lists:foldl(fun(TypeNode, Acc) ->
				add_remote_translation(Module, TypeNode, Acc)
			end, Table, Args ++ Constraints ++ Constructors)
	end;
build_translation_table(_, Table) ->
	Table.

add_remote_translation(ModuleName, #milang_ast{ type = type_name_local } = Node, Table) ->
	LocalName = Node#milang_ast.data,
	NewName = #{ name => LocalName, module => ModuleName },
	Table#{ LocalName => NewName };
add_remote_translation(ModuleName, #milang_ast{ type = function_name_local } = Node, Table) ->
	LocalName = Node#milang_ast.data,
	NewName = #{ name => LocalName, module => ModuleName },
	Table#{ LocalName => NewName };
add_remote_translation(ModuleName, #milang_ast{ type = function_name_symbol} = Node, Table) ->
	LocalName = Node#milang_ast.data,
	NewName = #{ name => LocalName, module => ModuleName },
	Table#{ LocalName => NewName };
add_remote_translation(ModuleName, #milang_ast{ type = type_data } = Node, Table) ->
	#{ name := NameAST, args := Args } = Node#milang_ast.data,
	MidTable = add_remote_translation(ModuleName, NameAST, Table),
	lists:foldl(fun(ArgNode, Acc) ->
		add_remote_translation(ModuleName, ArgNode, Acc)
	end, MidTable, Args);
add_remote_translation(_Module, _Node, Table) ->
	Table.
	%% TODO add remote translations for type constructors.

create_header_ast(AST, TranslationTable) ->
	lists:filtermap(fun(Node) ->
		maybe_convert_node(Node, TranslationTable)
	end, AST).

maybe_convert_node(#milang_ast{ type = declaration_function}, _Translations) ->
	false;
maybe_convert_node(#milang_ast{ type = declaration_spec} = Node, Translations) ->
	#{ name := SpecName } = Node#milang_ast.data,
	case maps:find(SpecName#milang_ast.data, Translations) of
		error ->
			false;
		{ok, _TrueName} ->
			{true, convert_node(Node, Translations)}
	end;
maybe_convert_node(#milang_ast{ type = declaration_module}, _Translations) ->
	false;
maybe_convert_node(#milang_ast{ type = declaration_import}, _Translations) ->
	false;
maybe_convert_node(#milang_ast{ type = declaration_type} = Node, Translations) ->
	#{ name := TypeName } = Node#milang_ast.data,
	case maps:find(TypeName#milang_ast.data, Translations) of
		error ->
			false;
		{ok, _} ->
			{true, convert_node(Node, Translations)}
	end;
maybe_convert_node(#milang_ast{ type = declaration_alias } = Node, Translations) ->
	#{ name := TypeName } = Node#milang_ast.data,
	case maps:find(TypeName#milang_ast.data, Translations) of
		error ->
			false;
		{ok, _} ->
			{true, convert_node(Node, Translations)}
	end.

convert_node(#milang_ast{ type = function_name_local} = Node, Translations) ->
	case maps:find(Node#milang_ast.data, Translations) of
		error ->
			Node;
		{ok, RemoteName} ->
			Node#milang_ast{ type = function_name_remote, data = RemoteName }
	end;
convert_node(#milang_ast{ type = function_name_symbol } = Node, Translations) ->
	case maps:find(Node#milang_ast.data, Translations) of
		error ->
			Node;
		{ok, RemoteName} ->
			Node#milang_ast{ type = function_name_remote, data = RemoteName }
	end;
convert_node(#milang_ast{ type = type_name_local } = Node, Translations) ->
	case maps:find(Node#milang_ast.data, Translations) of
		error ->
			Node;
		{ok, RemoteName} ->
			Node#milang_ast{ type = type_name_remote, data = RemoteName}
	end;
convert_node(#milang_ast{ type = function_name_remote} = Node, Translations) ->
	#{ module := Mod} = Data = Node#milang_ast.data,
	case maps:find({module, Mod}, Translations) of
		error ->
			Node;
		{ok, TrueModule} ->
			NewData = Data#{ module => TrueModule },
			Node#milang_ast{ data = NewData}
	end;
convert_node(#milang_ast{ type = type_name_remote} = Node, Translations) ->
	#{ module := Mod} = Data = Node#milang_ast.data,
	case maps:find({module, Mod}, Translations) of
		error ->
			Node;
		{ok, TrueModule} ->
			NewData = Data#{module => TrueModule},
			Node#milang_ast{ data = NewData }
	end;
convert_node(#milang_ast{} = Node, Translations) ->
	io:format("Node conversion naively: ~p~n", [Node]),
	NewData = convert_node_data(Node#milang_ast.data, Translations),
	Node#milang_ast{ data = NewData};
convert_node(Wut, _Translations) ->
	io:format("Not an ast node, likely something from a data that got through: ~p~n", [Wut]),
	Wut.

convert_node_data(Map, Translations) when is_map(Map) ->
	Mapper = fun
		(_K, Values) when is_list(Values) ->
			[convert_node(V, Translations) || V <- Values];
		(_K, Value) ->
			convert_node(Value, Translations)
	end,
	maps:map(Mapper, Map);
convert_node_data(List, Translations) when is_list(List) ->
	[ convert_node(E, Translations) || E <- List ];
convert_node_data(Data, _Translations) ->
	Data.
