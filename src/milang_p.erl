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
-export(
	[ args_list/0
	, space_opt/1
	, space/1
	, function_bindings/0
	]).

-spec space_opt(parser:parser(Err, Ok)) -> parse:parser(Err, Ok).
space_opt(P) ->
	milang_p_atomic:space_opt_then(P).

space(P) ->
	milang_p_atomic:space_then(P).

-spec dot() -> parse:parser({expected, $.}, $.).
dot() ->
	parse:character($.).

upcase_name() ->
	milang_p_atomic:upcase_name().

-spec module() -> parse:parser(term(), [ milang_ast:ast_node() ]).
module() ->
	WithTrailingSpace = parse:repeat_until(space_opt(declaration()), space_opt(parse:end_of_input())),
	parse:map(WithTrailingSpace, fun({Declarations, ok}) ->
		Declarations
	end).

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

	FunctionSpecTest = parse:test(parse:series([milang_p_atomic:function_name(), space_opt(parse:character($:))])),
	FunctionDefTest = parse:test(milang_p_atomic:function_name()),

	FunctionSpec = parse:andThen(FunctionSpecTest, fun(_) -> declaration_spec() end),
	FunctionDef = parse:andThen(FunctionDefTest, fun(_) -> declaration_function() end),

	parse:first_of([NonFunctionDecs, FunctionSpec, FunctionDef]).

declaration_module() ->
	SeriesP = parse:series(
		[ parse:string(<<"-module">>)
		, space(module_name())
		, space(parse:string(<<"exposing">>))
		, space_opt(exposing_list())
		, space_opt(dot())
		]),
	Tagged = parse:tag(declaration_module, SeriesP),
	Mapper = fun({T, L, Nodes}) ->
		[_, Name, _ExposingKeyword, Exposing, _] = Nodes,
		Data = #{ name => binary_to_atom(Name, utf8), exposing => Exposing},
		milang_ast:ast_node(L, <<>>, T, Data)
	end,
	parse:map(Tagged, Mapper).

module_name() ->
	Split = parse:repeat_when(milang_p_atomic:upcase_name(), parse:character($.)),
	Mapper = fun(P) ->
		unicode:characters_to_binary(P)
	end,
	parse:map(Split, Mapper).

exposing_list() ->
	milang_p_atomic:list(parse:first_of([milang_p_atomic:type_name(), milang_p_atomic:function_name()])).

declaration_import() ->
	ModuleNamePart = milang_p_atomic:upcase_name(),
	ModuleNameRepeatWhen = parse:map(parse:series([parse:character($.), parse:test(ModuleNamePart)]), fun(_) ->
		ok
	end),
	ModuleNameParts = parse:repeat_when(ModuleNamePart, ModuleNameRepeatWhen),
	SeriesP = parse:series(
		[ parse:string(<<"-import">>)
		, space(ModuleNameParts)
		, module_alias_section()
		, parse:optional(exposing_section())
		, space_opt(dot())
		]),
	Tagged = parse:tag(declaration_import, SeriesP),
	Mapper = fun({T, L, [_, ModuleUnjoinedWithOks, Alias, MaybeExposing, _]}) ->
		Exposing = case MaybeExposing of
			[] -> [];
			[E] -> E
		end,
		ModuleUnjoined = [ M || M <- ModuleUnjoinedWithOks, M =/= ok],
		Modulejoined = lists:join($., ModuleUnjoined),
		Module = unicode:characters_to_binary(Modulejoined),
		Data = #{ name => binary_to_atom(Module, utf8), alias => Alias, exposing => Exposing},
		milang_ast:ast_node(L, <<>>, T, Data)
	end,
	parse:map(Tagged, Mapper).

module_alias_section() ->
	SeriesP = parse:series([space(parse:string(<<"as">>)), space(upcase_name())]),
	OptionalP = parse:optional(SeriesP),
	Mapper = fun([[_, Name]]) ->
		binary_to_atom(Name, utf8);
		([]) ->
			undefined
	end,
	parse:map(OptionalP, Mapper).

exposing_section() ->
	SeriesP = parse:series([space(parse:string(<<"exposing">>)), space_opt(exposing_list())]),
	Mapper = fun([_, E]) ->
		E
	end,
	parse:map(SeriesP, Mapper).

declaration_alias() ->
	SeriesP = parse:series([parse:string(<<"-alias">>), constraints_section(), space(milang_p_atomic:type_name()), args_list(), space_opt(parse:character($=)), space_opt(milang_p_type:concrete()), space_opt(dot())]),
	Tagged = parse:tag(declaration_alias, SeriesP),
	Mapped = fun({T, L, Series}) ->
		[_, Constraints, Name, Args, _Equals, Original, _Dot] = Series,
		Data = #{ name => Name, constraints => Constraints, args => Args, original => Original },
		milang_ast:ast_node(L, <<>>, T, Data)
	end,
	parse:map(Tagged, Mapped).

declaration_type() ->
	SeriesP = parse:series([parse:string(<<"-type">>), constraints_section(), space(milang_p_atomic:type_name()), args_list(), parse:optional(constructors()), space_opt(dot())]),
	Tagged = parse:tag(declaration_type, SeriesP),
	Mapper = fun({T, L, Series}) ->
		[_TypeTag, Constraints, Name, Args, MaybeConstructors, _Dot] = Series,
		Constructors = case MaybeConstructors of
			[] -> [];
			[C] -> C
		end,
		Data = #{ name => Name, args => Args, constraints => Constraints, constructors => Constructors },
		milang_ast:ast_node(L, <<>>, T, Data)
	end,
	parse:map(Tagged, Mapper).

constructors() ->
	ListP = milang_p_atomic:list(milang_p_type:data()),
	space_opt(ListP).

declaration_class() ->
	ClassMembersP = class_members(),
	SeriesP = parse:series([parse:string(<<"-class">>), constraints_section(), space(milang_p_atomic:type_name()), args_list(), space_opt(ClassMembersP), space_opt(dot())]),
	Tagged = parse:tag(declaration_class, SeriesP),
	Mapper = fun({T, L, [_DeclareType, Constraints, Name, Args, Members, _Dot]}) ->
		Data = #{ name => Name, args => Args, constraints => Constraints, members => Members },
		milang_ast:ast_node(L, <<>>, T, Data)
	end,
	parse:map(Tagged, Mapper).

class_members() ->
	ElementP = parse:lazy(fun() ->
		class_member()
	end),
	milang_p_atomic:list(ElementP).

class_member() ->
	parse:first_of([ class_member_definition(), class_member_default()]).

class_member_definition() ->
	NameP = milang_p_atomic:function_name(),
	SeriesP = parse:series([NameP, space_opt(parse:character($:)), space_opt(milang_p_type:function())]),
	Tagged = parse:tag(class_member_definition, SeriesP),
	Mapper = fun({T, L, [Name, _Colon, Def]}) ->
		Data = #{ name => Name, definition => Def},
		milang_ast:ast_node(L, <<>>, T, Data)
	end,
	parse:map(Tagged, Mapper).

class_member_default() ->
	NameP = milang_p_atomic:function_name(),
	ArgsP = args_list(),
	ExpressionP = parse:lazy(fun milang_p_expression:expression/0),
	SeriesP = parse:series([NameP, ArgsP, space_opt(parse:character($=)), space_opt(ExpressionP), space_opt(dot())]),
	Tagged = parse:tag(class_member_default, SeriesP),
	Mapper = fun({T, L, [Name, Args, _Equals, Expression, _Dot]}) ->
		Data = #{ name => Name, args => Args, expression => Expression },
		milang_ast:ast_node(L, <<>>, T, Data)
	end,
	parse:map(Tagged, Mapper).


declaration_function() ->
	NameP = milang_p_atomic:function_name_local(),
	SeriesP = parse:series([NameP, args_list(), space_opt(parse:string(<<"->">>)), space_opt(function_bindings()), space_opt(milang_p_expression:expression()), space_opt( dot())]),
	Tagged = parse:tag(declaration_function, SeriesP),
	Mapper = fun({T, L, [Name, MaybeArgs, _Equals, Bindings, Expression, _Dot]}) ->
		Args = case MaybeArgs of
			[] -> [];
			_ -> hd(MaybeArgs)
		end,
		Data = #{ name => Name, args => Args, bindings => Bindings, expression => Expression },
		milang_ast:ast_node(L, <<>>, T, Data)
	end,
	parse:set_tag(declaration_function, parse:map(Tagged, Mapper)).

function_bindings() ->
	SeriesP = parse:series([space_opt(milang_p_atomic:variable()), space_opt(parse:character($=)), space_opt(parse:lazy(fun milang_p_expression:expression/0)), space_opt(parse:character($,))]),
	BindingTagged = parse:tag(binding, SeriesP),
	BindingMapper = fun({T, L, [Variable, _Equals, Expression, _Comma]}) ->
		milang_ast:ast_node(L, <<>>, T, #{ name => Variable, expression => Expression})
	end,
	Binding = parse:map(BindingTagged, BindingMapper),
	WhenCheck = parse:test(parse:map(parse:series([space_opt(milang_p_atomic:variable()), space_opt(parse:character($=))]), fun(_) -> ok end)),
	RepeatedHasOks = parse:repeat_when(Binding, WhenCheck),
	Repeated = parse:map(RepeatedHasOks, fun(L) ->
		[ E || E <- L, L =/= ok]
	end),
	parse:set_tag(function_bindings, parse:optional(Repeated)).


declaration_spec() ->
	NameP = milang_p_atomic:function_name(),
	SeriesP = parse:series([NameP, space_opt(parse:character($:)), space_opt(milang_p_type:function()), space_opt(dot())]),
	Tagged = parse:tag(declaration_spec, SeriesP),
	Mapper = fun({T, L, [Name, _Colon, Def, _Dot]}) ->
		Data = #{ name => Name, spec => Def },
		milang_ast:ast_node(L, <<>>, T, Data)
	end,
	parse:map(Tagged, Mapper).

constraints_section() ->
	KeyP = milang_p_atomic:variable(),
	ValueP = milang_p_type:concrete(),
	ItemsP = milang_p_atomic:record(KeyP, ValueP),
	SeriesP = parse:series([space(parse:string(<<"when">>)), space_opt(ItemsP)]),
	WhenExistsMapper = fun([_, Items]) ->
		Items
	end,
	WhenExistsMapped = parse:map(SeriesP, WhenExistsMapper),
	parse:optional(WhenExistsMapped).

args_list() ->
	ItemP = space(milang_p_atomic:variable()),
	RepeatTest = parse:map(parse:test(ItemP), fun(_) -> ok end),
	Repeated = parse:repeat_when(ItemP, RepeatTest),
	Mapped = parse:map(Repeated, fun(L) ->
		[E || E <- L, E =/= ok]
	end),
	parse:set_tag(args_list, parse:optional(Mapped)).
