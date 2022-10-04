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
