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

space_opt() ->
	parse:optional(milang_p_atomic:space()).

space() ->
	milang_p_atomic:space().

dot() ->
	parse:character($.).

upcase_name() ->
	milang_p_atomic:upcase_name().

%downcase_name() ->
%	milang_p_atomic:downcase_name().

module() ->
	DeclarationRepeat = parse:repeat_until_error(parse:series([declaration(), space_opt()])),
	Series = parse:series([ space_opt(), DeclarationRepeat]),
	Mapper = fun([_, DecsWithSpaces]) ->
		[ D || [D, _] <- DecsWithSpaces]
	end,
	parse:map(Series, Mapper).

declaration() ->
	PeekRegex = "(-module|-import|-alias|-type|-class|[a-z][\\w]*[\\s]*(:)|\'[\\pS\\pP]+\'[\s]*(:)|[\\w]+)",
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
	parse:andThen(PeekP, AndThenner).

declaration_module() ->
	SeriesP = parse:series(
		[ parse:string(<<"-module">>)
		, space()
		, module_name()
		, space()
		, parse:string(<<"exposing">>)
		, space_opt()
		, parse:optional(exposing_list())
		, space_opt()
		, dot()
		]),
	Tagged = parse:tag(declaration_module, SeriesP),
	Mapper = fun({T, L, Nodes}) ->
		[_, _, Name, _, _ExposingKeyword, _, Exposing, _, _] = Nodes,
		{T, L, Name, Exposing}
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
	Mapper = fun({T, L, [_, _, Module, Alias, Exposing, _, _]}) ->
		{T, L, Module, Alias, Exposing}
	end,
	parse:map(Tagged, Mapper).

module_alias_section() ->
	SeriesP = parse:series([space(), parse:string(<<"as">>), space(), upcase_name()]),
	OptionalP = parse:optional(SeriesP),
	Mapper = fun([[_, _, _, Name]]) ->
		Name;
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
	SeriesP = parse:series([parse:string(<<"-alias">>), constraints_section(), space(), milang_p_atomic:upcase_name(), args_list(), space_opt(), parse:character($=), space_opt(), literal_type(), space_opt(), dot()]),
	Tagged = parse:tag(declaration_alias, SeriesP),
	Mapped = fun({T, L, Series}) ->
		[_, Constraints, _, Name, Args, _, _Equals, _, Original, _, _Dot] = Series,
		{T, L, Name, Args, Constraints, Original}
	end,
	parse:map(Tagged, Mapped).

declaration_type() ->
	SeriesP = parse:series([parse:string(<<"-type">>), constraints_section(), space(), upcase_name(), args_list(), constructors(), space_opt(), dot()]),
	Tagged = parse:tag(declaration_type, SeriesP),
	Mapper = fun({T, L, Series}) ->
		[_, Constraints, _, Name, Args, Constructors, _, _Dot] = Series,
		{T, L, Name, Args, Constraints, Constructors}
	end,
	parse:map(Tagged, Mapper).

constructors() ->
	parse:repeat(1, infinity, constructor_element()).

constructor_element() ->
	SeriesP = parse:series([space_opt(), parse:character($|), space_opt(), upcase_name(), constructor_args()]),
	Tagged = parse:tag(type_constructor, SeriesP),
	Mapper = fun({T, L, Series}) ->
		[_, _, _, Name, Args] = Series,
		{T, L, Name, Args}
	end,
	parse:map(Tagged, Mapper).

constructor_args() ->
	SeriesP = parse:series([space(), constructor_arg()]),
	RepeatP = parse:repeat_until_error(SeriesP),
	Mapped = fun(Args) ->
		[A || [_, A] <- Args]
	end,
	parse:map(RepeatP, Mapped).

constructor_arg() ->
	parse:first_of([ milang_p_type:primary(), type_literal_record()]).

declaration_class() ->
	ClassMembersP = class_members(),
	SeriesP = parse:series([parse:string(<<"-class">>), constraints_section(), space(), upcase_name(), args_list(), ClassMembersP, space_opt(), dot()]),
	Tagged = parse:tag(declaration_class, SeriesP),
	Mapper = fun({T, L, [_DeclareType, Constraints, _, Name, Args, Members, _, _Dot]}) ->
		{T, L, Name, Constraints, Args, Members}
	end,
	parse:map(Tagged, Mapper).

class_members() ->
	ElementsP = parse:lazy(fun() ->
		SeriesP = parse:series([space_opt(), parse:character($|), space_opt(), class_member()]),
		Mapper = fun([_, _, _, Member]) ->
			Member
		end,
		parse:map(SeriesP, Mapper)
	end),
	parse:repeat_until_error(ElementsP).

class_member() ->
	parse:first_of([ class_member_definition(), class_member_default()]).

class_member_definition() ->
	NameP = parse:first_of([milang_p_atomic:function_name_local(), milang_p_atomic:function_name_symbol()]),
	SeriesP = parse:series([NameP, space_opt(), parse:character($:), space_opt(), milang_p_type:top()]),
	Tagged = parse:tag(class_member, SeriesP),
	Mapper = fun({T, L, [Name, _, _Colon, _, Def]}) ->
		{T, L, Name, Def}
	end,
	parse:map(Tagged, Mapper).

class_member_default() ->
	NameP = parse:first_of([milang_p_atomic:function_name_local(), milang_p_atomic:function_name_symbol()]),
	ArgsP = args_list(),
	ExpressionP = parse:lazy(fun milang_p_expression:expression/0),
	SeriesP = parse:series([NameP, ArgsP, space_opt(), parse:character($=), space_opt(), ExpressionP, space_opt(), dot()]),
	Tagged = parse:tag(class_default, SeriesP),
	Mapper = fun({T, L, [Name, Args, _, _Equals, _, Expression, _, _Dot]}) ->
		{T, L, Name, Args, Expression}
	end,
	parse:map(Tagged, Mapper).


declaration_function() ->
	NameP = parse:first_of([milang_p_atomic:function_name_local(), milang_p_atomic:function_name_symbol()]),
	SeriesP = parse:series([NameP, args_list(), space_opt(), parse:string(<<"->">>), space_opt(), function_bindings(), space_opt(), milang_p_expression:expression(), space_opt(), dot()]),
	Tagged = parse:tag(declaration_function, SeriesP),
	Mapper = fun({T, L, [Name, Args, _, _Equals, _, Bindings, _, Expression, _, _Dot]}) ->
		{T, L, Name, Args, Bindings, Expression}
	end,
	parse:map(Tagged, Mapper).

function_bindings() ->
	SeriesP = parse:series([space_opt(), milang_p_atomic:variable(), space_opt(), parse:character($=), space_opt(), parse:lazy(fun milang_p_expression:expression/0), space_opt(), parse:character($,)]),
	BindingTagged = parse:tag(bind, SeriesP),
	BindingMapper = fun({T, L, [_, Variable, _, _Equals, _, Expression, _, _Comma]}) ->
		{T, L, Variable, Expression}
	end,
	BindingP = parse:map(BindingTagged, BindingMapper),
	parse:repeat_until_error(BindingP).


declaration_spec() ->
	NameP = parse:first_of([milang_p_atomic:function_name_local(), milang_p_atomic:function_name_symbol()]),
	SeriesP = parse:series([NameP, space_opt(), parse:character($:), space_opt(), milang_p_type:top(), space_opt(), dot()]),
	Tagged = parse:tag(declaration_spec, SeriesP),
	Mapper = fun({T, L, [Name, _, _Colon, _, Def, _, _Dot]}) ->
		{T, L, Name, Def}
	end,
	parse:map(Tagged, Mapper).

constraints_section() ->
	KeyP = milang_p_atomic:variable(),
	ValueP = milang_p_type:concrete(),
	ItemsP = milang_p_atomic:record(KeyP, ValueP),
	SeriesP = parse:series([space(), parse:string(<<"when">>), space_opt(), ItemsP]),
	WhenExistsMapper = fun([_, _, _, Items]) ->
		Items
	end,
	WhenExistsMapped = parse:map(SeriesP, WhenExistsMapper),
	OptionalP = parse:optional(WhenExistsMapped),
	parse:tag(constraints, OptionalP).

args_list() ->
	ItemP = parse:series([space(), milang_p_atomic:variable()]),
	Repeat = parse:repeat_until_error(ItemP),
	Mapper = fun(DirtyItems) ->
		[I || [_, I] <- DirtyItems]
	end,
	parse:map(Repeat, Mapper).

literal_type() ->
	parse:first_of([milang_p_type:top(), type_literal_record()]).

type_literal_record() ->
	KeyParser = milang_p_expression:record_field_key(),
	ValueParser = parse:lazy(fun literal_type/0),
	RecordP = milang_p_atomic:record(KeyParser, ValueParser),
	parse:tag(type_literal_record, RecordP).


%%
%exposing_list <- '[' (space? exposing_item)* space? ']' `
%	[_, DirtyElements, _, _] = Node,
%	Elements = [E || [_, E] <- DirtyElements],
%	{exposing, simple_idx(Idx), Elements}
%`;
%
%exposing_item <- ',' space? (upcase_name / local_function_name) `
%	[_, _, Elem] = Node,
%	Elem
%`;
%
%declaration_seperator <- '.' ~;
%
%%list_seperator <- ',' ~;
%
%%%@import milang_expression.ppeg
%
%%%@import milang_atomic.ppeg
%
%%%@import milang_type.ppeg
%
%%%@append milang_helpers.hrl
%
