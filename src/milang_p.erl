-module(milang_p).

-export(
	[ module/0
	, declaration/0
	, declaration_module/0
	, declaration_import/0
	, declaration_alias/0
	, declaration_type/0
	, declaration_class/0
	, declaration_spec/0
	, declaration_function/0
	]).

-spec space_opt() -> parse:parser(none(), unicode:chardata()).
space_opt() ->
	parse:optional(milang_p_atomic:space()).

space() ->
	milang_p_atomic:space().

-spec dot() -> parse:parser({expected, $.}, $.).
dot() ->
	parse:character($.).

upcase_name() ->
	milang_p_atomic:upcase_name().

-spec module() -> parse:parser(term(), [ milang_ast:ast_node() ]).
module() ->
	DeclarationRepeat = parse:repeat_until_error(parse:series([declaration(), space_opt()])),
	Series = parse:series([ space_opt(), DeclarationRepeat]),
	Mapper = fun([_, DecsWithSpaces]) ->
		[ D || [D, _] <- DecsWithSpaces]
	end,
	parse:map(Series, Mapper).

declaration() ->
	PeekRegex = "(-module|-import|-alias|-type|-class)",
	PeekP = parse:peek(PeekRegex),

	AndThenner = fun
		([<<"-module">> | _]) ->
			declaration_module();
		([<<"-import">> | _]) ->
			declaration_import();
		([<<"-alias">> | _]) ->
			declaration_alias();
		([<<"-type">> | _]) ->
			declaration_type();
		([<<"-class">> | _]) ->
			declaration_class();
		([_, _, <<":">> | _]) ->
			declaration_spec();
		(_) ->
			declaration_function()
	end,
	NonFunctionDecs = parse:andThen(PeekP, AndThenner),

	FunctionSpecTest = parse:test(parse:series([milang_p_atomic:function_name(), space_opt(), parse:character($:)])),
	FunctionDefTest = parse:test(milang_p_atomic:function_name()),

	FunctionSpec = parse:andThen(FunctionSpecTest, fun(_) -> declaration_spec() end),
	FunctionDef = parse:andThen(FunctionDefTest, fun(_) -> declaration_function() end),

	parse:first_of([NonFunctionDecs, FunctionSpec, FunctionDef]).

declaration_module() ->
	SeriesP = parse:series(
		[ parse:string(<<"-module">>)
		, space()
		, module_name()
		, space()
		, parse:string(<<"exposing">>)
		, space_opt()
		, exposing_list()
		, space_opt()
		, dot()
		]),
	Tagged = parse:tag(declaration_module, SeriesP),
	Mapper = fun({T, L, Nodes}) ->
		[_, Doc, Name, _, _ExposingKeyword, _, Exposing, _, _] = Nodes,
		Data = #{ name => binary_to_atom(Name, utf8), exposing => Exposing},
		milang_ast:ast_node(L, Doc, T, Data)
	end,
	parse:map(Tagged, Mapper).

module_name() ->
	TailPartP = parse:series([ dot(), milang_p_atomic:upcase_name()]),
	TailP = parse:repeat_until_error(TailPartP),
	Series = parse:series([milang_p_atomic:upcase_name(), TailP]),
	Mapper = fun(P) ->
		unicode:characters_to_binary(P)
	end,
	parse:map(Series, Mapper).

exposing_list() ->
	milang_p_atomic:list(parse:first_of([milang_p_atomic:type_name(), milang_p_atomic:function_name()])).

declaration_import() ->
	SeriesP = parse:series(
		[ parse:string(<<"-import">>)
		, space()
		, module_name()
		, module_alias_section()
		, parse:optional(exposing_section())
		, space_opt()
		, dot()
		]),
	Tagged = parse:tag(declaration_import, SeriesP),
	Mapper = fun({T, L, [_, Doc, Module, Alias, MaybeExposing, _, _]}) ->
		Exposing = case MaybeExposing of
			[] -> [];
			[E] -> E
		end,
		Data = #{ name => binary_to_atom(Module, utf8), alias => Alias, exposing => Exposing},
		milang_ast:ast_node(L, Doc, T, Data)
	end,
	parse:map(Tagged, Mapper).

module_alias_section() ->
	SeriesP = parse:series([space(), parse:string(<<"as">>), space(), upcase_name()]),
	OptionalP = parse:optional(SeriesP),
	Mapper = fun([[_, _, _, Name]]) ->
		binary_to_atom(Name, utf8);
		([]) ->
			undefined
	end,
	parse:map(OptionalP, Mapper).

exposing_section() ->
	SeriesP = parse:series([space(), parse:string(<<"exposing">>), space_opt(), exposing_list()]),
	Mapper = fun([_, _, _, E]) ->
		E
	end,
	parse:map(SeriesP, Mapper).

declaration_alias() ->
	SeriesP = parse:series([parse:string(<<"-alias">>), constraints_section(), space(), milang_p_atomic:type_name(), args_list(), space_opt(), parse:character($=), space_opt(), milang_p_type:concrete(), space_opt(), dot()]),
	Tagged = parse:tag(declaration_alias, SeriesP),
	Mapped = fun({T, L, Series}) ->
		[Doc1, Constraints, Doc2, Name, Args, _, _Equals, _, Original, _, _Dot] = Series,
		Docs = unicode:characters_to_binary([Doc1, "\n", Doc2]),
		Data = #{ name => Name, constraints => Constraints, args => Args, original => Original },
		milang_ast:ast_node(L, Docs, T, Data)
	end,
	parse:map(Tagged, Mapped).

declaration_type() ->
	SeriesP = parse:series([parse:string(<<"-type">>), constraints_section(), space(), milang_p_atomic:type_name(), args_list(), constructors(), space_opt(), dot()]),
	Tagged = parse:tag(declaration_type, SeriesP),
	Mapper = fun({T, L, Series}) ->
		[_TypeTag, Constraints, Doc1, Name, Args, Constructors, _, _Dot] = Series,
		Docs = unicode:characters_to_binary(Doc1),
		Data = #{ name => Name, args => Args, constraints => Constraints, constructors => Constructors },
		milang_ast:ast_node(L, Docs, T, Data)
	end,
	parse:map(Tagged, Mapper).

constructors() ->
	parse:repeat(1, infinity, constructor_element()).

constructor_element() ->
	SeriesP = parse:series([space_opt(), parse:character($|), space_opt(), milang_p_type:data()]),
	Mapper = fun(Series) ->
		[Doc1, _Pipe, Doc2, Data] = Series,
		Docs = unicode:characters_to_binary([Doc1, "\n", Doc2]),
		milang_ast:set_doc(Docs, Data)
	end,
	parse:map(SeriesP, Mapper).

declaration_class() ->
	ClassMembersP = class_members(),
	SeriesP = parse:series([parse:string(<<"-class">>), constraints_section(), space(), milang_p_atomic:type_name(), args_list(), ClassMembersP, space_opt(), dot()]),
	Tagged = parse:tag(declaration_class, SeriesP),
	Mapper = fun({T, L, [_DeclareType, Constraints, _, Name, Args, Members, _, _Dot]}) ->
		Data = #{ name => Name, args => Args, constraints => Constraints, members => Members },
		milang_ast:ast_node(L, <<>>, T, Data)
	end,
	parse:map(Tagged, Mapper).

class_members() ->
	ElementsP = parse:lazy(fun() ->
		SeriesP = parse:series([space_opt(), parse:character($|), space_opt(), class_member()]),
		Mapper = fun([Doc1, _, Doc2, Member]) ->
			milang_ast:set_doc([Doc1, "\n", Doc2], Member)
		end,
		parse:map(SeriesP, Mapper)
	end),
	parse:repeat_until_error(ElementsP).

class_member() ->
	parse:first_of([ class_member_definition(), class_member_default()]).

class_member_definition() ->
	NameP = milang_p_atomic:function_name(),
	SeriesP = parse:series([NameP, space_opt(), parse:character($:), space_opt(), milang_p_type:function()]),
	Tagged = parse:tag(class_member_definition, SeriesP),
	Mapper = fun({T, L, [Name, _, _Colon, _, Def]}) ->
		Data = #{ name => Name, definition => Def},
		milang_ast:ast_node(L, <<>>, T, Data)
	end,
	parse:map(Tagged, Mapper).

class_member_default() ->
	NameP = milang_p_atomic:function_name(),
	ArgsP = args_list(),
	ExpressionP = parse:lazy(fun milang_p_expression:expression/0),
	SeriesP = parse:series([NameP, ArgsP, space_opt(), parse:character($=), space_opt(), ExpressionP, space_opt(), dot()]),
	Tagged = parse:tag(class_member_default, SeriesP),
	Mapper = fun({T, L, [Name, Args, _, _Equals, _, Expression, _, _Dot]}) ->
		Data = #{ name => Name, args => Args, expression => Expression },
		milang_ast:ast_node(L, <<>>, T, Data)
	end,
	parse:map(Tagged, Mapper).


declaration_function() ->
	NameP = milang_p_atomic:function_name_local(),
	SeriesP = parse:series([NameP, args_list(), space_opt(), parse:string(<<"->">>), space_opt(), function_bindings(), space_opt(), milang_p_expression:expression(), space_opt(), dot()]),
	Tagged = parse:tag(declaration_function, SeriesP),
	Mapper = fun({T, L, [Name, Args, _, _Equals, _, Bindings, _, Expression, _, _Dot]}) ->
		Data = #{ name => Name, args => Args, bindings => Bindings, expression => Expression },
		milang_ast:ast_node(L, <<>>, T, Data)
	end,
	parse:map(Tagged, Mapper).

function_bindings() ->
	SeriesP = parse:series([space_opt(), milang_p_atomic:variable(), space_opt(), parse:character($=), space_opt(), parse:lazy(fun milang_p_expression:expression/0), space_opt(), parse:character($,)]),
	BindingTagged = parse:tag(binding, SeriesP),
	BindingMapper = fun({T, L, [_, Variable, _, _Equals, _, Expression, _, _Comma]}) ->
		milang_ast:ast_node(L, <<>>, T, #{ name => Variable, expression => Expression})
	end,
	BindingP = parse:map(BindingTagged, BindingMapper),
	parse:repeat_until_error(BindingP).


declaration_spec() ->
	NameP = milang_p_atomic:function_name(),
	SeriesP = parse:series([NameP, space_opt(), parse:character($:), space_opt(), milang_p_type:function(), space_opt(), dot()]),
	Tagged = parse:tag(declaration_spec, SeriesP),
	Mapper = fun({T, L, [Name, _, _Colon, _, Def, _, _Dot]}) ->
		Data = #{ name => Name, spec => Def },
		milang_ast:ast_node(L, <<>>, T, Data)
	end,
	parse:map(Tagged, Mapper).

constraints_section() ->
	KeyP = milang_p_atomic:variable(),
	ValueP = milang_p_type:concrete(),
	ItemsP = milang_p_atomic:record(KeyP, ValueP),
	SeriesP = parse:series([space(), parse:string(<<"when">>), space_opt(), ItemsP]),
	WhenExistsMapper = fun([_, _, _, Items]) ->
		[{K, V} || {record_field, _, K, V} <- Items]
	end,
	WhenExistsMapped = parse:map(SeriesP, WhenExistsMapper),
	parse:optional(WhenExistsMapped).

args_list() ->
	ItemP = parse:series([space(), milang_p_atomic:variable()]),
	Repeat = parse:repeat_until_error(ItemP),
	Mapper = fun(DirtyItems) ->
		[I || [_, I] <- DirtyItems]
	end,
	parse:map(Repeat, Mapper).

