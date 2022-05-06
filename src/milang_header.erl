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

-export([read_header_file/1, read_header_string/1]).
-export([create_header/2]).

read_header_file(Filename) ->
	case file:read_file(Filename) of
		{ok, FileContents} ->
			read_header_string(FileContents);
		Error ->
			Error
	end.

read_header_string(Src) ->
	case parse:it(Src, milang_p:module()) of
		{ok, Parsed, <<>>} ->
			{ok, build_table(Parsed, undefined, _LocalizedEntries = #{})};
		{ok, _Parsed, Trailing} ->
			{error, {trailing_characters, Trailing}};
		Error ->
			Error
	end.

build_table([], ModuleName, Entries) ->
	finalize_table(ModuleName, Entries);
build_table([#{ type := declaration_module} = Node | Tail], _, Entries) ->
	#{ name := ModuleName, exposing := Exposing } = maps:get(data, Node),
	NewEntries = lists:foldl(fun(E, Acc) ->
		EntryName = build_name(ModuleName, milang_ast:data(E)),
		maps:put(EntryName, undefined, Acc)
	end, Entries, Exposing),
	build_table(Tail, ModuleName, NewEntries);
build_table([#{type := declaration_type} = Node | Tail], ModuleName, Entries) ->
	#{ name := Name, constraints := Constraints, args := Args, constructors := Constructors} = maps:get(data, Node),
	TopName = build_name(ModuleName, Name),
	TypeEntry = milang_type_validation:type(TopName, Constraints, Args),
	WithTopType = maps:put(TopName, TypeEntry, Entries),
	NewEntries = lists:foldl(fun(E, Acc) ->
		add_constructors(TopName, ModuleName, E, Acc)
	end, WithTopType, Constructors),
	build_table(Tail, ModuleName, NewEntries);
build_table([#{ type := declaration_spec } = Node | Tail], ModuleName, Entries) ->
	#{ name := Name, spec := Spec } = maps:get(data, Node),
	TopName = build_name(ModuleName, Name),
	TypeEntry = milang_type_validation:function(TopName, Spec),
	NewEntries = maps:put(TopName, TypeEntry, Entries),
	build_table(Tail, ModuleName, NewEntries);
build_table([ Node | Tail], ModuleName, Entries) when is_tuple(Node) ->
	AsMap = milang_ast:to_map(Node),
	build_table([AsMap | Tail], ModuleName, Entries);
build_table([ Node | _Tail], _ModuleName, _Entries) ->
	error({nyi, Node}).

add_constructors(ParentType, ModuleName, ConstrutorAST, Entries) ->
	Data = milang_ast:data(ConstrutorAST),
	#{ name := NameAST, args := Args} = Data,
	Name = milang_ast:data(NameAST),
	FullName = build_name(ModuleName, Name),
	% TODO convert the args to actually type checkable things.
	Entry = milang_type_validation:constructor(FullName, ParentType, Args),
	maps:put(FullName, Entry, Entries).

build_name(ModuleName, AST) when is_tuple(AST) ->
	build_name(ModuleName, milang_ast:to_string(AST));
build_name(ModuleName, LocalName) ->
	binary_to_atom(unicode:characters_to_binary(io_lib:format("~s.~s", [ModuleName, LocalName]))).

finalize_table(_ModuleName, Entries) ->
	Entries.

create_header(AST, IoDev) ->
	OnlyHeaderWorthy = lists:filter(fun header_worthy/1, AST),
	lists:foreach(fun(HeaderWorthy) ->
		ok = io:put_chars(IoDev, milang_ast:to_string(HeaderWorthy))
	end, OnlyHeaderWorthy).

header_worthy(Node) ->
	case milang_ast:type(Node) of
		declaration_module ->
			true;
		declaration_type ->
			true;
		declaration_alias ->
			true;
		declaration_spec ->
			true;
		declaration_class ->
			true;
		_ ->
			false
	end.
