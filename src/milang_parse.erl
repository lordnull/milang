-module(milang_parse).

-include_lib("kernel/include/logger.hrl").

-export([file/1, string/1]).

-export([expression/1, expression_infix/1, expression_non_infix/1]).
-export(
	[ type_concrete/0
	, match_expression/0
	]).

-type milang_ast() :: [ milang_ast:ast_node() ].

-spec string(unicode:unicode_binary()) -> {ok, milang_ast()} | {error, term()}.
string(Binary) ->
	parse:string(Binary, parse_statem()).

-spec file(file:filename()) -> {ok, milang_ast()} | {error, term()}.
file(Filename) ->
	{ok, Data} = file:read_file(Filename),
	string(Data).

-define(chunk_size, 10000).

parse_statem() ->
	parse:loop({root, []}, fun parse_statem/1).


parse_statem({root, Ast}) ->
	Finalize = partial:func(fun
		(_Doc, eof) ->
			done;
		(Doc, Node) ->
			Out = milang_ast:pre_doc(Doc, Node),
			Out
	end, []),
	Pipeline = parse:pipeline(parse:success(Finalize),
		[ parse:keep(milang_p_token:any_space())
		, parse:keep(declaration_or_done())
		]),
	parse:map(fun
		(done) ->
			{done, lists:reverse(Ast)};
		(Finalized) ->
			{step, {root, [Finalized | Ast]}}
	end, Pipeline).

declaration_or_done() ->
	parse:first_of(
		[ parse:map(compose:always(eof), parse:end_of_input())
		, declaration()
		]).

declaration() ->
	parse:first_of(
		[ module()
		, import()
		, an_alias()
		, data()
		, spec()
		, class()
		, teach()
		, bind()
		, expose_all()
		, expose()
		]).

module() ->
	Finalize = fun(Doc1, Location, Doc2, ModuleName) ->
		milang_ast:ast_node(Location, [Doc1, Doc2], milang_ast_module:new(ModuleName))
	end,

	ModuleDeclaration = parse:pipeline(parse:success(partial:func(Finalize, [])),
		[ parse:keep(milang_p_token:any_space())
		, parse:keep(parse:location())
		, parse:discard(milang_p_token:keyword_module())
		, parse:keep(milang_p_token:documentation())
		, parse:keep(milang_p_token:identifier_bound())
		, parse:discard(milang_p_token:any_space())
		, parse:discard(milang_p_token:syntax_dot())
		]),
	parse:in_context(module_declaration, ModuleDeclaration).

import() ->
	Finalize = partial:func(fun(Location, MoarDoc, Imported) ->
		milang_ast:ast_node(Location, MoarDoc, milang_ast_import:new(Imported, undefined))
	end, []),
	parse:in_context(import, parse:pipeline(parse:success(Finalize),
		[ parse:keep(parse:location())
		, parse:discard(milang_p_token:keyword_import())
		, parse:keep(milang_p_token:documentation())
		, parse:keep(milang_p_token:identifier_bound())
		, parse:discard(milang_p_token:any_space())
		, parse:discard(milang_p_token:syntax_dot())
		])).

an_alias() ->
	Final = partial:func(fun(Location, Doc1, AliasName, Args, SpecDoc1, SpecDoc2, Spec) ->
		FullSpec = milang_ast:pre_doc([SpecDoc1, SpecDoc2], Spec),
		milang_ast:ast_node(Location, Doc1, milang_ast_alias:new(AliasName, Args, [], FullSpec))
	end),
	Pipeline = parse:pipeline(parse:success(Final),
		[ parse:keep(parse:location())
		, parse:discard(milang_p_token:keyword_alias())
		, parse:keep(milang_p_token:documentation())
		, parse:keep(milang_p_token:identifier_bound())
		, parse:keep(arg_list())
		, parse:keep(milang_p_token:any_space())
		, parse:discard(milang_p_token:syntax_bind())
		, parse:keep(milang_p_token:any_space())
		, parse:keep(type_concrete())
		, parse:discard(milang_p_token:any_space())
		, parse:discard(milang_p_token:syntax_dot())
		]),
	parse:in_context(alias, Pipeline).

data() ->
	Final = partial:func(fun(Location, MoarDoc, TypeName, Args, _Constraints, ConstructorNode) ->
		% TODO support constraints
		ConstraintsData = milang_ast_constraints:new([]),
		ConstraintsNode = milang_ast:ast_node(Location, [], ConstraintsData),
		milang_ast:ast_node(Location, MoarDoc, milang_ast_type:new(TypeName, Args, ConstraintsNode, ConstructorNode, none))
	end, []),
	parse:in_context(data, parse:pipeline(parse:success(Final),
		[ parse:keep(parse:location())
		, parse:discard(milang_p_token:keyword_data())
		, parse:keep(milang_p_token:documentation())
		, parse:keep(milang_p_token:identifier_bound())
		, parse:keep(arg_list()) % takes the space before the bind
		%, parse:keep(milang_p_token:documentation())
		% TODO support constraints
		%, parse:keep(constraints())
		, parse:keep(parse:success([]))
		, parse:keep(maybe_constructors())
		%, parse:discard(milang_p_token:syntax_bind())
		%, parse:keep(milang_p_token:documentation())
		%, parse:keep(parse:location())
		%, parse:keep(maybe_constructors())
		, parse:discard(milang_p_token:any_space())
		, parse:discard(milang_p_token:syntax_dot())
		])).

arg_list() ->
	parse:loop({doc_or_done, []}, fun arg_list/1).

arg_list({doc_or_done, ArgAcc}) ->
	DocOrDone = parse:first_of(
		[ milang_p_token:documentation()
		, parse:success(done)
		]),
	parse:map(fun
		(done) ->
			{done, lists:reverse(ArgAcc)};
		(Doc) ->
			{step, {arg_or_done, Doc, ArgAcc}}
	end, DocOrDone);

arg_list({arg_or_done, Doc, ArgAcc}) ->
	ArgOrDone = parse:first_of(
		[ parse:map_n(fun(L, T) ->
			{L, T}
		end, [parse:location(), parse:backtrackable(milang_p_token:identifier_bound())])
		, parse:success(done)
		]),
	parse:map(fun
		(done) ->
			{done, lists:reverse(ArgAcc)};
		({L, ArgName}) ->
			Node = milang_ast:ast_node(L, Doc, ArgName),
			{step, {doc_or_done, [Node | ArgAcc]}}
	end, ArgOrDone).

maybe_constructors() ->
	parse:loop(init, fun maybe_constructors/1).

maybe_constructors(init) ->
	BindFoundParse = parse:map(fun(_) ->
		{step, has_constructors}
	end, milang_p_token:syntax_bind()),
	NoConstructors = parse:map(fun(Location) ->
		Data = milang_ast_constructors:new([]),
		Node = milang_ast:ast_node(Location, [], Data),
		{done, Node}
	end, parse:location()),
	parse:first_of(
		[ BindFoundParse
		, NoConstructors
	]);

maybe_constructors(has_constructors) ->
	DocParse = milang_p_token:documentation(),
	ConstructorsParse = constructors(),
	parse:map_n(fun(Doc, Location, Constructors) ->
		Data = milang_ast_constructors:new(Constructors),
		{done, milang_ast:ast_node(Location, Doc, Data)}
	end, [ DocParse, parse:location(), ConstructorsParse]).

constructors() ->
	parse:pipeline(parse:success(compose:id()),
		[ parse:discard(milang_p_token:syntax_open_list())
		, parse:keep(constructor_list_elements())
		]).

constructor_list_elements() ->
	parse:loop({start_or_done, []}, fun constructor_list_elements/1).

constructor_list_elements({start_or_done, Acc}) ->
	StartOrDoneParser = parse:first_of(
		[ parse:map(fun(_) ->
			element_seperator
		end, milang_p_token:syntax_element_seperator())
		, parse:map(fun(_) ->
			list_closed
		end, milang_p_token:syntax_close_list())
		]),
	parse:map_n(fun
		(Doc, element_seperator) ->
			{step, {constructor, Doc, Acc}};
		(_Doc, list_closed) ->
			{done, lists:reverse(Acc)}
	end, [ milang_p_token:any_space(), StartOrDoneParser ]);

constructor_list_elements({constructor, PreDoc1, Acc}) ->
	parse:map_n(fun(PreDoc2, Concrete) ->
		Node = milang_ast:pre_doc([PreDoc1, PreDoc2], Concrete),
		{step, {start_or_done, [ Node | Acc]}}
	end, [milang_p_token:any_space(), type_concrete()]).

class() ->
	parse:loop(init, fun class/1).

class(init) ->
	class(preamble);

class(preamble) ->
	Finalize = partial:func(fun(Location, Doc, ClassName, KindDoc, KindLocation, KindName) ->
		KindNode = milang_ast:ast_node(KindLocation, KindDoc, KindName),
		ProtoData = milang_ast_class:new(ClassName, undefined, [KindNode], [], false),
		ProtoNode = milang_ast:ast_node(Location, Doc, ProtoData),
		{step, {next_class_part, ProtoNode}}
	end),
	parse:pipeline(parse:success(Finalize),
		[ parse:keep(parse:location())
		, parse:discard(milang_p_token:keyword_class())
		% documentation and class name
		, parse:keep(milang_p_token:documentation())
		, parse:keep(milang_p_token:identifier_bound())
		% documentation and primary kind
		, parse:keep(milang_p_token:documentation())
		, parse:keep(parse:location())
		, parse:keep(milang_p_token:identifier_bound())
		]);

class({next_class_part, ProtoNode}) ->
	ConstraintParse = parse:map(fun(_) ->
		constraints
	end, milang_p_token:keyword_when()),
	MembersParse = parse:map(fun(_) ->
		members
	end, milang_p_token:syntax_bind()),
	ArgParse = parse:map(fun(Arg) ->
		{arg, Arg}
	end, milang_p_token:identifier_bound()),
	WhichParser = parse:first_of(
		[ ConstraintParse
		, MembersParse
		, ArgParse
		]),
	parse:map_n(fun(Doc, Location, WhatToDo) ->
		case WhatToDo of
			constraints ->
				{step, {constraints, Doc, ProtoNode}};
			members ->
				{step, {members, Doc, ProtoNode}};
			{arg, Arg} ->
				ArgNode = milang_ast:ast_node(Location, Doc, Arg),
				NewProtoNode = milang_ast:transform_data(fun(Data) ->
					OldArgs = milang_ast_class:args(Data),
					NewArgs = [ ArgNode | OldArgs],
					milang_ast_class:args(NewArgs, Data)
				end, ProtoNode),
				{step, {next_class_part, NewProtoNode}}
		end
	end, [ milang_p_token:documentation(), parse:location(), WhichParser]);

class({constraints, Doc, ProtoNode}) ->
	parse:map(fun(ConstrainsNode) ->
		ConstraintsNode = milang_ast:pre_doc(Doc, ConstrainsNode),
		NewProtoNode = milang_ast:transform_data(fun(Data) ->
			milang_ast_class:constraints(ConstraintsNode, Data)
		end, ProtoNode),
		{step, {bind_then_members, NewProtoNode}}
	end, ProtoNode);

class({bind_then_members, ProtoNode}) ->
	parse:map_n(fun(_Doc, _Location, _) ->
		{step, {members, ProtoNode}}
	end, [ milang_p_token:documentation(), parse:location(), milang_p_token:syntax_bind()]);

class({members, _FirstMemberDoc, ProtoNode}) ->
	MembersParse = class_members(),
	NodeParse = parse:map(fun(Members) ->
		milang_ast:transform_data(fun(Data) ->
			milang_ast_class:members(Members, Data)
		end, ProtoNode)
	end, MembersParse),
	parse:map(fun(Node) ->
		{step, {finish, Node}}
	end, NodeParse);

class({finish, Node}) ->
	parse:map_n(fun(_, _) ->
		{done, Node}
	end, [ milang_p_token:any_space(), milang_p_token:syntax_dot()]).

class_members() ->
	parse:loop(init, fun class_members/1).

class_members(init) ->
	class_members({doc_or_done, []});

class_members({doc_or_done, Acc}) ->
	DocParse = parse:map(fun(Doc) ->
		{step, {member_or_done, Doc, Acc}}
	end, milang_p_token:documentation()),
	DoneParse = parse:success({step, {finish, Acc}}),
	parse:first_of(
		[ DocParse
		, DoneParse
		]);

class_members({member_or_done, Doc, Acc}) ->
	DefaultParse = parse:map(fun(Binding) ->
		BindData = milang_ast:data(Binding),
		NameNode = milang_ast_binding:match(BindData),
		ExpressionNode = milang_ast_binding:expression(BindData),
		MemberData = milang_ast_class_member_default:new(NameNode, ExpressionNode),
		Location = milang_ast:location(Binding),
		OldDoc = milang_ast:doc(Binding),
		Node = milang_ast:ast_node(Location, [Doc, OldDoc], MemberData),
		NewAcc = [ Node | Acc ],
		{step, {doc_or_done, NewAcc}}
	end, bind()),
	DefineParse = parse:map(fun(Spec) ->
		SpecData = milang_ast:data(Spec),
		NameNode = milang_ast_spec:name(SpecData),
		DefinitionNode = milang_ast_spec:type(SpecData),
		Location = milang_ast:location(Spec),
		OldDoc = milang_ast:doc(Spec),
		MemberData = milang_ast_class_member_definition:new(NameNode, DefinitionNode),
		Node = milang_ast:ast_node(Location, [Doc,OldDoc], MemberData),
		NewAcc = [ Node | Acc ],
		{step, {doc_or_done, NewAcc}}
	end, spec()),
	DoneParse = parse:success({step, {finish, Acc}}),
	parse:first_of(
		[ DefaultParse
		, DefineParse
		, DoneParse
		]);

class_members({finish, Acc}) ->
	parse:success({done, lists:reverse(Acc)}).

teach() ->
	Final = fun(Location, StudentName, ExtraClassDoc, ClassNameRaw, Members) ->
		ClassName = milang_ast:pre_doc(ExtraClassDoc, ClassNameRaw),
		Data = milang_ast_teach:new(StudentName, ClassName, Members),
		milang_ast:ast_node(Location, [], Data)
	end,
	parse:pipeline(parse:success(partial:func(Final)),
		[ parse:keep(parse:location())
		, parse:discard(milang_p_token:keyword_teach())
		, parse:keep(identifier_node())
		, parse:keep(milang_p_token:documentation())
		, parse:discard(milang_p_token:keyword_class())
		, parse:keep(identifier_node())
		, parse:discard(milang_p_token:documentation())
		, parse:discard(milang_p_token:syntax_implies())
		, parse:discard(milang_p_token:documentation())
		, parse:keep(teach_members())
		, parse:discard(milang_p_token:any_space())
		, parse:discard(milang_p_token:syntax_dot())
		]).

teach_members() ->
	parse:loop(init, fun teach_members/1).

teach_members(init) ->
	teach_members({bind_or_done, []});

teach_members({bind_or_done, Acc}) ->
	BindParse = parse:map(fun(B) ->
		{step, {bind_or_done, [B | Acc]}}
	end, bind()),
	DoneParse = parse:success({step, {finish, Acc}}),
	parse:first_of(
		[ BindParse
		, DoneParse
		]);

teach_members({finish, AccRev}) ->
	parse:success({done, lists:reverse(AccRev)}).


identifier_node() ->
	parse:map_n(fun(Doc, Location, Id) ->
		milang_ast:ast_node(Location, Doc, Id)
	end, [ milang_p_token:any_space(), parse:location(), milang_p_token:identifier()]).

constraints() ->
	parse:loop(init, fun constraints/1).

constraints(init) ->
	constraints('when');

constraints('when') ->
	ParseWhen = parse:map(fun(_) ->
		has_constraints
	end, milang_p_token:keyword_when()),
	ParseNone = parse:success(no_when),
	parse:map_n(fun
		(Location, has_constraints) ->
			{step, {open_list, Location}};
		(Location, no_when) ->
			{step, {finish, Location, [], []}}
	end, [ parse:location(), parse:first_of([ ParseWhen, ParseNone ])]);

constraints({open_list, Location}) ->
	parse:map_n(fun(MoarDoc, _) ->
		{step, {next_element_or_done, Location, MoarDoc, []}}
	end, [milang_p_token:documentation(), milang_p_token:syntax_open_list()]);

constraints({next_element_or_done, Location, CDoc, Acc}) ->
	ParseDone = parse:map(fun(_) ->
		done
	end, milang_p_token:syntax_close_list()),
	ParseElementStart = parse:map(fun(_) ->
		element
	end, milang_p_token:syntax_element_seperator()),
	parse:map_n(fun
		(_Doc, _Location, done) ->
			{step, {finish, Location, CDoc, Acc}};
		(Doc, ElementStartLoc, element) ->
			{step, {element, Location, CDoc, Acc, Doc, ElementStartLoc}}
	end, [ milang_p_token:any_space(), parse:location(), parse:first_of([ ParseDone, ParseElementStart])]);

constraints({element, Location, CDoc, Acc, Doc1, ElementLoc}) ->
	Final = fun(VarNode, ClassNode) ->
		ConstraintData = milang_ast_constraint:new(VarNode, ClassNode),
		ConstraintNode = milang_ast:ast_node(ElementLoc, Doc1, ConstraintData),
		NewAcc = [ ConstraintNode | Acc ],
		{step, {next_element_or_done, Location, CDoc, NewAcc}}
	end,
	parse:map_n(Final, [ identifier_node(), identifier_node() ]);

constraints({finish, Location, CDoc, Constraints}) ->
	Data = milang_ast_constraints:new(lists:reverse(Constraints)),
	Node = milang_ast:ast_node(Location, CDoc, Data),
	parse:success({done, Node}).

bind() ->
	Finalize = partial:func(fun(Location, MatchDoc, Match, ExpressionDoc1, ExpressionDoc2, ExpressionRes) ->
		#{ expression := Expression } = ExpressionRes,
		FullExpressionNode = milang_ast:pre_doc([ExpressionDoc1, ExpressionDoc2], Expression),
		milang_ast:ast_node(Location, MatchDoc, milang_ast_binding:new(Match, FullExpressionNode))
	end, []),
	parse:in_context(bind, parse:pipeline(parse:success(Finalize),
		[ parse:keep(parse:location())
		, parse:discard(milang_p_token:keyword_let())
		, parse:keep(milang_p_token:documentation())
		, parse:keep(milang_p_token:identifier_bound())
		, parse:keep(milang_p_token:documentation())
		, parse:discard(milang_p_token:syntax_bind())
		, parse:keep(milang_p_token:documentation())
		, parse:keep(parse:lazy(fun() ->
			expression(milang_p_token:syntax_dot())
		end))
		])).

expression_call({function_called, Ender}) ->
	EndFail = parse:and_then(fun(Ended) ->
		parse:fail({empty_expression, Ended})
	end, Ender),
	ParseFirst = parse:first_of(
		[ EndFail
		, record_syntax()
		, expression_match()
		, literal()
		, record_access()
		, identifier_node()
		, subexpression()
		]),
	parse:map_n(fun(Location, Head) ->
		{step, {doc_or_done, Ender, Location, Head, []}}
	end, [ parse:location(), ParseFirst ]);

expression_call({doc_or_done, Ender, Location, Head, Args}) ->
	ParseDoc = parse:map(fun(Doc) ->
		{step, {arg_or_done, Ender, Location, Head, Args, Doc}}
	end, milang_p_token:documentation()),
	ParseDone = parse:map(fun(Ended) ->
		{step, {finish, Location, Head, Args, Ended}}
	end, Ender),
	parse:first_of([ ParseDoc, ParseDone ]);

expression_call({arg_or_done, Ender, Location, Head, Args, Doc}) ->
	EndParse = parse:map(fun(Ended) ->
		{step, {finish, Location, Head, Args, Ended}}
	end, Ender),
	ArgParse = parse:first_of(
		[ expression_match()
		, literal()
		, identifier_node()
		, subexpression()
		]),
	StepParse = parse:map(fun(Node) ->
		ArgNode = milang_ast:pre_doc(Doc, Node),
		NewArgs = [ ArgNode | Args ],
		{step, {doc_or_done, Ender, Location, Head, NewArgs}}
	end, ArgParse),
	parse:first_of([ EndParse, StepParse]);

expression_call({finish, _Location, Head, [], Ended}) ->
	parse:success({done, #{ expression => Head, ended => Ended }});

expression_call({finish, Location, Head, Args, Ended}) ->
	Data = milang_ast_call:new(Head, lists:reverse(Args)),
	Node = milang_ast:ast_node(Location, [], Data),
	parse:success({done, #{ expression => Node, ended => Ended}});

expression_call(Ender) ->
	parse:loop({function_called, Ender}, fun expression_call/1).

expression(Ender) ->
	expression_infix(Ender).

expression_infix({head, Ender}) ->
	FullEnder = parse:first_of(
		[ parse:map(fun(N) -> {full_end, N} end, Ender)
		, parse:map(fun(N) -> {infix, N} end, parse:backtrackable(infix_notation()))
		]),
	parse:map(fun(ExpressionRes) ->
		case ExpressionRes of
			#{ expression := Expression, ended := {infix, InfixNotation}} ->
				{step, {tail, Ender, Expression, [], InfixNotation}};
			#{ expression := Expression, ended := {full_end, EndedProper}} ->
				{step, {finish, Expression, [], EndedProper}};
			Expression ->
				{step, {maybe_tail, Ender, Expression}}
		end
	end, expression_non_infix(FullEnder));

expression_infix({maybe_tail, Ender, Head}) ->
	FullEnder = parse:first_of(
		[ parse:map(fun(N) -> {full_end, N} end, Ender)
		, parse:map(fun(N) -> {infix, N} end, parse:backtrackable(infix_notation()))
		]),
	parse:map_n(fun
		(Doc, {infix, InfixNotation}) ->
			{step, {tail, Ender, Head, [], InfixNotation, Doc}};
		(_Doc, {full_end, N}) ->
			{step, {finish, Head, [], N}}
	end, [ milang_p_token:any_space(), FullEnder ]);

expression_infix({tail, Ender, Head, Tails, Notation}) ->
	parse:map(fun(Doc) ->
		{step, {tail, Ender, Head, Tails, Notation, Doc}}
	end, milang_p_token:documentation());

expression_infix({tail, Ender, Head, Tails, Notation, Doc}) ->
	FullEnder = parse:first_of(
		[ parse:map(fun(N) -> {infix, N} end, parse:backtrackable(infix_notation()))
		, parse:map(fun(N) -> {full_end, N} end, Ender)
		]),
	parse:map(fun(ExpressionRes) ->
		#{ expression := Expression, ended := Ended} = ExpressionRes,
		Data = milang_ast_infix_operation:new(Notation, milang_ast:pre_doc(Doc, Expression)),
		Node = milang_ast:ast_node(milang_ast:location(Notation), [], Data),
		NewTails = [ Node | Tails ],
		case Ended of
			{infix, InfixNotation} ->
				{step, {tail, Ender, Head, NewTails, InfixNotation}};
			{full_end, EndedProper} ->
				{step, {finish, Head, NewTails, EndedProper}}
		end
	end, expression_non_infix(FullEnder));

expression_infix({finish, Head, [], EndedProper}) ->
	parse:success({done, #{expression => Head, ended => EndedProper}});

expression_infix({finish, Head, TailsRev, EndedProper}) ->
	Data = milang_ast_infix_series:new(Head, lists:reverse(TailsRev)),
	NodeLocation = milang_ast:location(Head),
	Node = milang_ast:ast_node(NodeLocation, [], Data),
	parse:success({done, #{ expression => Node, ended => EndedProper }});

expression_infix(Ender) ->
	parse:loop({head, Ender}, fun expression_infix/1).

infix_notation() ->
	ParseIdentifer = parse:map_n(fun
		(_InfixLocation, 0, _IdentifierNode, 0) ->
			{error, no_infix};
		(InfixLocation, L, IdentifierNode, R) when L * R == 0 ->
			{Assoc, N} = if
				L > 0 ->
					{left, L};
				R > 0->
					{right, R}
			end,
			InfixNotation = milang_ast_infix_notation:new(IdentifierNode, Assoc, N),
			InfixNode = milang_ast:ast_node(InfixLocation, [], InfixNotation),
			{ok, InfixNode};
		(_InfixLocation, _L, _IdentifierNode, _R) ->
			{error, infix_both_sides}
	end, [ parse:location(), milang_p_token:any_syntax_infix_indicator(), parse:backtrackable(identifier_node()), milang_p_token:any_syntax_infix_indicator()]),
	parse:and_then(fun
		({error, Error}) ->
			parse:fail(Error);
		({ok, Node}) ->
			parse:success(Node)
	end, ParseIdentifer).

expression_non_infix(Ender) ->
	AndEnded = fun(Parser) ->
		parse:map_n(fun(Node, _Doc, Ended) ->
			#{expression => Node, ended => Ended}
		end, [Parser, milang_p_token:any_space(), Ender])
	end,
	parse:first_of(
		[ AndEnded(expression_match())
		, AndEnded(parse:lazy(fun subexpression/0))
		, parse:lazy(fun() ->
			expression_call(Ender)
		end)
		, AndEnded(literal())
		]).

record_access() ->
	parse:and_then(fun(Node) ->
		case milang_ast:type_simply(Node) of
			milang_ast_record_access ->
				parse:success(Node);
			Got ->
				parse:fail({expected_record_accessor, Got})
		end
	end, record_syntax()).

subexpression() ->
	Final = fun(Doc, ExpressionRes) ->
		#{ expression := Node, ended := subexpression } = ExpressionRes,
		milang_ast:pre_doc(Doc, Node)
	end,
	ParseExpression = parse:lazy(fun() ->
		expression(milang_p_token:syntax_close_subexpression())
	end),
	parse:pipeline(parse:success(partial:func(Final)),
		[ parse:discard(milang_p_token:syntax_open_subexpression())
		, parse:keep(milang_p_token:any_space())
		, parse:keep(ParseExpression)
		]).

literal() ->
	LiteralParse = parse:first_of(
		[ parse:map(fun(N) -> {literal_number, N} end, literal_number())
		, parse:map(fun(S) -> {literal_string, S} end, literal_string())
		, parse:lazy(fun literal_list/0)
		, parse:lazy(fun literal_record/0)
		, parse:lazy(fun function/0)
		]),
	parse:map_n(fun
		(Location, {literal_number, N}) when is_integer(N) ->
			Data = milang_ast_literal:new_integer(N),
			milang_ast:ast_node(Location, [], Data);
		(Location, {literal_number, N}) ->
			Data = milang_ast_literal:new_float(N),
			milang_ast:ast_node(Location, [], Data);
		(Location, {literal_string, S}) ->
			Data = milang_ast_literal:new_string(S),
			milang_ast:ast_node(Location, [], Data);
		(_, Node) ->
			Node
	end, [ parse:location(), LiteralParse ]).

as_node(Parser) ->
	parse:map_n(fun(Location, Data) ->
		milang_ast:ast_node(Location, [], Data)
	end, [ parse:location(), Parser]).

% becuase the syntax bind (->) starts with a '-' just like some numbers, we
% need to beable to handle those cases. The easiest way is to make the number
% here backtrackable.
literal_number() -> parse:in_context(literal_number, parse:backtrackable(milang_p_token:literal_number())).

literal_string() -> parse:in_context(literal_string, milang_p_token:literal_string()).

literal_list() ->
	Finalize = partial:func(fun(list, Location, #{ cons := Cons, elements := Elements}) ->
		milang_ast:ast_node(Location, [], milang_ast_literal_list:new(Elements, Cons))
	end, []),
	parse:in_context(literal_list, parse:pipeline(parse:success(Finalize),
		[ parse:keep(milang_p_token:syntax_open_list())
		, parse:keep(parse:location())
		, parse:keep(list_expression_elements())
		])).

list_expression_elements() ->
	parse:loop(init, fun list_expression_elements/1).

list_expression_elements(init) ->
	ParseDoc = milang_p_token:any_space(),
	ParseElementSeperator = parse:map(fun(_) -> element end, milang_p_token:syntax_element_seperator()),
	ParseClose = parse:map(fun(_) -> empty_list end, milang_p_token:syntax_close_list()),
	parse:map_n(fun
		(Doc, element) ->
			{step, {element, [], Doc}};
		(_, empty_list) ->
			{step, {finish, undefined, []}}
	end, [ ParseDoc, parse:first_of([ ParseElementSeperator, ParseClose])]);


list_expression_elements({element, Acc, Doc}) ->
	Ender = parse:first_of(
		[ parse:map(fun(_) ->
			next_element
		end, milang_p_token:syntax_element_seperator())
		, parse:map(fun(_) ->
			cons
		end, milang_p_token:syntax_cons())
		, parse:map(fun(_) ->
			done
		end, milang_p_token:syntax_close_list())
		]),
	Element = parse:lazy(fun() ->
		expression(Ender)
	end),
	parse:map_n(fun(Doc2, ExpressionRes) ->
		#{ expression := N, ended := Ended } = ExpressionRes,
		WithDoc = milang_ast:pre_doc([Doc, Doc2], N),
		NewAcc = [ WithDoc | Acc],
		case Ended of
			next_element ->
				{step, {element, NewAcc, []}};
			cons ->
				{step, {cons, NewAcc}};
			done ->
				{step, {finish, undefined, NewAcc}}
		end
	end, [milang_p_token:any_space(), Element]);

list_expression_elements({cons, Acc}) ->
	Ender = milang_p_token:syntax_close_list(),
	Element = parse:lazy(fun() ->
		expression(Ender)
	end),
	parse:map(fun(ExpressionRes) ->
		#{ expression := Node } = ExpressionRes,
		{step, {finsih, Node, Acc}}
	end, Element);

list_expression_elements({finish, Cons, Elements}) ->
	parse:success({done, #{ cons => Cons, elements => lists:reverse(Elements)}}).


literal_record() ->
	Finalize = partial:func(fun(record, Location, #{ cons := Cons, elements := Elements}) ->
		milang_ast:ast_node(Location, [], milang_ast_literal_record:new(Cons, Elements))
	end, []),
	parse:in_context(literal_record, parse:pipeline(parse:success(Finalize),
		[ parse:keep(milang_p_token:syntax_open_record())
		, parse:keep(parse:location())
		, parse:keep(parse:loop(init, fun literal_record_elements/1))
		])).

literal_record_elements(init) ->
	literal_record_elements({elements, []});

literal_record_elements({elements, Acc}) ->
	Done = parse:map(compose:always(done), milang_p_token:syntax_close_record()),
	Cons = parse:map(compose:always(cons), milang_p_token:syntax_cons()),
	Element = parse:map(compose:always(element), milang_p_token:syntax_element_seperator()),
	Stepper = parse:first_of(
		[ Done
		, Cons
		, Element
		]),
	parse:map_n(fun
		(_Doc, done) ->
			{step, {finish, undefined, Acc}};
		(Doc, cons) ->
			{step, {cons, Doc, Acc}};
		(Doc, element) ->
			{step, {element, Doc, Acc}}
	end, [ milang_p_token:any_space(), Stepper]);


literal_record_elements({element, Doc, Acc}) ->
	Ender = parse:first_of(
		[ parse:map(compose:always(cons), milang_p_token:syntax_cons())
		, parse:map(compose:always(element), milang_p_token:syntax_element_seperator())
		, parse:map(compose:always(done), milang_p_token:syntax_close_record())
		]),

	Finalize = fun(FieldDoc2, FieldName, ExpressionDoc1, ExpressionDoc2, Expression) ->
		#{ expression := ExpressionNode, ended := Ended} = Expression,
		ExpreNode = milang_ast:pre_doc([ExpressionDoc1, ExpressionDoc2], ExpressionNode),
		FieldData = milang_ast_record_field:new(FieldName, ExpreNode),
		FieldNodeSansDoc = milang_ast:transform_data(fun(_) ->
			FieldData
		end, FieldName),
		FieldNode = milang_ast:pre_doc([Doc, FieldDoc2], FieldNodeSansDoc),
		case Ended of
			cons ->
				{step, {cons, [], [ FieldNode | Acc]}};
			element ->
				{step, {element, [], [ FieldNode | Acc]}};
			done ->
				{step, {finish, undefined, [ FieldNode | Acc]}}
		end
	end,

	parse:pipeline(parse:success(partial:func(Finalize)),
		[ parse:keep(milang_p_token:any_space())
		, parse:keep(identifier_node())
		, parse:keep(milang_p_token:any_space())
		, parse:discard(milang_p_token:syntax_bind())
		, parse:keep(milang_p_token:any_space())
		, parse:keep(expression(Ender))
		]);

literal_record_elements({cons, Doc, Elements}) ->
	Ender = milang_p_token:syntax_close_record(),
	parse:map_n(fun(Doc2, Cons) ->
		#{ expression := Node } = Cons,
		WithDoc = milang_ast:pre_doc([Doc, Doc2], Node),
		{step, {finish, WithDoc, Elements}}
	end, [ milang_p_token:any_space(), parse:lazy(fun() ->
		expression(Ender)
	end)]);

literal_record_elements({finish, Cons, ElementsRev}) ->
	parse:success({done, #{ cons => Cons, elements => lists:reverse(ElementsRev)}}).

function() ->
	Finalize = fun(Location, Args, #{ binds := Binds, expression := Expression}) ->
		milang_ast:ast_node(Location, [], milang_ast_function:new(Args, Binds, Expression))
	end,
	Final = partial:func(Finalize, []),
	parse:in_context(literal_function, parse:pipeline(parse:success(Final),
		[ parse:keep(parse:location())
		, parse:discard(milang_p_token:keyword_function())
		, parse:keep(function_arg_loop())
		, parse:discard(milang_p_token:any_space())
		, parse:discard(milang_p_token:syntax_implies())
		, parse:keep(function_body_loop())
		])).

function_arg_loop() ->
	parse:in_context(function_args, parse:loop([], fun function_arg_loop/1)).

function_arg_loop(Acc) ->

	Arg = parse:and_then(fun(Doc) ->

		ArgWithLocation = parse:map_n(fun(Location, Arg) ->
			Node = milang_ast:ast_node(Location, Doc, Arg),
			{step, [Node | Acc]}
		end, [ parse:location(), parse:backtrackable(milang_p_token:identifier_bindable())]),

		ActuallyDone = parse:lazy(fun() ->
			parse:success({done, lists:reverse(Acc)})
		end),

		parse:first_of([ ArgWithLocation, ActuallyDone])

	end, milang_p_token:documentation()),

	NoDocNoArg = parse:lazy(fun() ->
		parse:success({done, lists:reverse(Acc)})
	end),

	parse:first_of([ Arg, NoDocNoArg ]).

function_body_loop() ->
	parse:in_context(function_body_loop, parse:loop({let_or_expression, []}, fun function_body_loop/1)).

function_body_loop({let_or_expression, Binds}) ->
	Doc = milang_p_token:any_space(),
	BindOrDone = parse:first_of(
		[ parse:in_context(function_bind, parse:map(fun(B) ->
			{bind, B}
		end, bind()))
		, parse:in_context(function_expression, parse:map(fun(E) ->
			{expression, E}
		end, parse:lazy(fun() ->
			expression(milang_p_token:syntax_dot())
		end)))
		]),
	parse:map_n(fun(PreDoc, {Type, Ast}) ->
		case Type of
			bind ->
				{step, {let_or_expression, [milang_ast:pre_doc(PreDoc, Ast) | Binds]}};
			expression ->
				#{expression := Expression} = Ast,
				ExpressionNode = milang_ast:pre_doc(PreDoc, Expression),
				{done, #{ binds => lists:reverse(Binds), expression => ExpressionNode}}
		end
	end, [Doc, BindOrDone]).

expression_match() ->
	match_expression().

match_expression() ->
	Final = fun(Location, ExprDoc, ExprRes, Clauses) ->
		#{ expression := Expr } = ExprRes,
		ExprNode = milang_ast:pre_doc(ExprDoc, Expr),
		Data = milang_ast_match:new(ExprNode, Clauses),
		milang_ast:ast_node(Location, [], Data)
	end,
	parse:in_context(expression_match, parse:pipeline(parse:success(partial:func(Final)),
		[ parse:keep(parse:location())
		, parse:discard(milang_p_token:keyword_match())
		, parse:keep(milang_p_token:documentation())
		, parse:keep(parse:in_context(match_the_expression, parse:lazy(fun() -> expression(milang_p_token:keyword_with())
		end)))
		%, parse:discard(milang_p_token:documentation())
		%, parse:discard(milang_p_token:keyword_with())
		, parse:keep(parse:in_context(match_clauses, match_expression_clauses()))
		, parse:discard(milang_p_token:any_space())
		, parse:discard(milang_p_token:syntax_dot())
		])).

match_expression_clauses() ->
	parse:loop(init, fun match_expression_clauses/1).

match_expression_clauses(init) ->
	match_expression_clauses({bind_or_done, []});

match_expression_clauses({bind_or_done, Acc}) ->
	ParseBindOrDone = parse:first_of(
		[ parse:in_context(match_clause_bind, match_clause_bind())
		, parse:success(done)
		]),
	parse:map_n(fun
		(_Doc, done) ->
			{step, {finish, Acc}};
		(Doc, {Bind, HangingDoc, _Ended}) ->
			Node = milang_ast:pre_doc(Doc, Bind),
			{step, {bind_expr, Acc, Node, HangingDoc}}
	end, [ milang_p_token:any_space(), ParseBindOrDone ]);

match_expression_clauses({bind_expr, Acc, Bind, Doc1}) ->
	Final = fun(ExprDoc2, ExpreReturn) ->
		#{ expression := ExprNode } = ExpreReturn,
		ExpressionNodeProper = milang_ast:pre_doc([Doc1, ExprDoc2], ExprNode),
		Data = milang_ast_match_clause:new(Bind, ExpressionNodeProper),
		Location = milang_ast:location(Bind),
		Node = milang_ast:ast_node(Location, [], Data),
		{step, {bind_or_done, [ Node | Acc]}}
	end,
	parse:pipeline(parse:success(partial:func(Final)),
		[ parse:keep(milang_p_token:any_space())
		, parse:keep(parse:in_context(match_clause_expression, parse:lazy(fun() ->
			 expression(milang_p_token:syntax_dot())
		end)))
		%, parse:discard(milang_p_token:any_space())
		%, parse:discard(milang_p_token:syntax_dot())
		]);

match_expression_clauses({finish, Acc}) ->
	parse:success({done, lists:reverse(Acc)}).

match_clause_bind() ->
	DefaultEnder = milang_p_token:syntax_implies(),
	match_clause_bind(DefaultEnder).

match_clause_bind(Ender) ->
	EndMapper = fun(Matched, HangingDoc, Ended) ->
		{Matched, HangingDoc, Ended}
	end,
	parse:first_of(
		[ parse:map_n(EndMapper, [match_literal(), milang_p_token:any_space(), Ender])
		% there is no 'match bind' because we cannot determine here if the
		% match is for a constructor w/ no args, or to bind to a name.
		% same goes for the args of a constructor match. Thus, any identifier
		% is considered a 'constructor' match until proven otherwise.
		, parse:map_n(EndMapper, [match_any(), milang_p_token:any_space(), Ender])
		, parse:map_n(EndMapper, [match_list(), milang_p_token:any_space(), Ender])
		, parse:map_n(EndMapper, [match_record(), milang_p_token:any_space(), Ender])
		, match_constructor(Ender)
		]).

match_literal() ->
	Literal = parse:first_of(
		[ as_node(literal_number())
		, as_node(literal_string())
		]),
	parse:map(fun(Node) ->
		Location = milang_ast:location(Node),
		Data = milang_ast_match_bind:new({literal, Node}),
		milang_ast:ast_node(Location, [], Data)
	end, Literal).

match_any() ->
	parse:map_n(fun(Location, IdToken) ->
		IdData = milang_ast_identifier:from_token(IdToken),
		IdNode = milang_ast:ast_node(Location, [], IdData),
		Data = milang_ast_match_bind:new({match_bind, IdNode}),
		milang_ast:ast_node(Location, [], Data)
	end, [parse:location(), milang_p_token:identifier_ignored()]).

match_list() ->
	parse:loop(init, fun match_list/1).

match_list(init) ->
	match_list(open_list);

match_list(open_list) ->
	parse:map_n(fun(Location, _) ->
		{step, {element_root, Location, []}}
	end, [ parse:location(), milang_p_token:syntax_open_list()]);

match_list({element_root, Location, Acc}) ->
	StartElementParse = parse:map(fun(_) ->
		element
	end, milang_p_token:syntax_element_seperator()),
	ConsParse = parse:map(fun(_) ->
		cons
	end, milang_p_token:syntax_cons()),
	CloseParse = parse:map(fun(_) ->
		done
	end, milang_p_token:syntax_close_list()),
	NextParse = parse:first_of(
		[ ConsParse
		, StartElementParse
		, CloseParse
		]),
	parse:map_n(fun
		(Doc, element) ->
			{step, {element, Location, Acc, Doc}};
		(Doc, cons) ->
			{step, {cons, Location, Acc, Doc}};
		(_Doc, done) ->
			{step, {finish, Location, Acc, undefined}}
	end, [ milang_p_token:any_space(),  NextParse]);

match_list({element, Location, Acc, Doc}) ->
	Ender = parse:first_of(
		[ parse:map(compose:always(cons), milang_p_token:syntax_cons())
		, parse:map(compose:always(element), milang_p_token:syntax_element_seperator())
		, parse:map(compose:always(done), milang_p_token:syntax_close_list())
		]),
	parse:map_n(fun(MoarDoc, {MatchNode, HangingDoc, Ended}) ->
		Node = milang_ast:pre_doc([Doc, MoarDoc], MatchNode),
		case Ended of
			cons ->
				{step, {cons, Location, [ Node | Acc], HangingDoc}};
			element ->
				{step, {element, Location, [ Node | Acc], HangingDoc}};
			done ->
				{step, {finish, Location, [ Node | Acc], undefined}}
		end
	end, [milang_p_token:any_space(), parse:lazy(fun() -> match_clause_bind(Ender) end)]);

match_list({cons, Location, Acc, Doc}) ->
	parse:map(fun(IdentifierNode) ->
		Node = milang_ast:pre_doc(Doc, IdentifierNode),
		{step, {close, Location, Acc, Node}}
	end, identifier_node());

match_list({close, Location, Heads, Tail}) ->
	parse:map_n(fun(_, _) ->
		{step, {finish, Location, Heads, Tail}}
	end, [ milang_p_token:any_space(), milang_p_token:syntax_close_list()]);

match_list({finish, Location, Heads, undefined}) ->
	Data = {match_list, lists:reverse(Heads)},
	Node = milang_ast:ast_node(Location, [], Data),
	parse:success({done, Node});

match_list({finish, Location, Heads, Tail}) ->
	Data = {match_list_head, lists:reverse(Heads), Tail},
	Node = milang_ast:ast_node(Location, [], Data),
	parse:success({done, Node}).

match_record() ->
	parse:loop(init, fun match_record/1).

match_record(init) ->
	match_record(open);

match_record(open) ->
	parse:map_n(fun(Location, _) ->
		{step, {element_root, Location, []}}
	end, [ parse:location(), milang_p_token:syntax_open_record() ]);

match_record({element_root, Location, Acc}) ->
	StartElementParse = parse:map(fun(_) ->
		element
	end, milang_p_token:syntax_element_seperator()),
	ConsParse = parse:map(fun(_) ->
		cons
	end, milang_p_token:syntax_cons()),
	CloseParse = parse:map(fun(_) ->
		done
	end, milang_p_token:syntax_close_record()),
	StepParse = parse:first_of(
		[ ConsParse
		, StartElementParse
		, CloseParse
		]),
	parse:map_n(fun
		(Doc, element) ->
			{step, {element, Location, Acc, Doc}};
		(Doc, cons) ->
			{step, {cons, Location, Acc, Doc}};
		(_Doc, done) ->
			{step, {finish, Location, Acc, undefined}}
	end, [ milang_p_token:any_space(), StepParse ]);

match_record({element, Location, Acc, Doc}) ->
	FieldNodeParse = parse:map_n(fun(FieldName) ->
		milang_ast:pre_doc(Doc, FieldName)
	end, [ identifier_node()]),
	NextStepParse = parse:first_of(
		[ parse:map(fun(_) -> submatch end, milang_p_token:syntax_bind())
		, parse:map(fun(_) -> element end, milang_p_token:syntax_element_seperator())
		, parse:map(fun(_) -> cons end, milang_p_token:syntax_cons())
		, parse:map(fun(_) -> done end, milang_p_token:syntax_close_record())
		]),
	parse:map_n(fun(NextStepLocation, FieldName, Doc2, NextStep) ->
		case NextStep of
			submatch ->
				{step, {submatch, Location, Acc, FieldName, Doc2}};
			element ->
				{step, {element, NextStepLocation, [FieldName | Acc], Doc2}};
			cons ->
				{step, {cons, Location, [ FieldName | Acc], Doc2}};
			done ->
				{step, {finish, Location, [ FieldName | Acc], undefined}}
		end
	end, [ parse:location(), FieldNodeParse, milang_p_token:any_space(), NextStepParse]);

match_record({submatch, Location, Acc, FieldName, SubMatchDoc}) ->
	Ender = parse:first_of(
		[ parse:map(compose:always(cons), milang_p_token:syntax_cons())
		, parse:map(compose:always(element), milang_p_token:syntax_element_seperator())
		, parse:map(compose:always(done), milang_p_token:syntax_close_record())
		]),
	parse:map(fun({SubMatch, Doc, StepHint}) ->
		Node = milang_ast:pre_doc(SubMatchDoc, SubMatch),
		FieldMatch = { FieldName, Node },
		NewAcc = [ FieldMatch | Acc ],
		case StepHint of
			cons ->
				{step, {cons, Location, NewAcc, Doc}};
			element ->
				{step, {element, Location, NewAcc, Doc}};
			done ->
				{step, {finish, Location, NewAcc, undefined}}
		end
	end, parse:lazy(fun() -> match_clause_bind(Ender) end));

match_record({cons, Location, Acc, ConsDoc}) ->
	parse:map_n(fun(TailName, _, _) ->
		Node = milang_ast:pre_doc(ConsDoc, TailName),
		{step, {finish, Location, Acc, Node}}
	end, [identifier_node(), milang_p_token:any_space(), milang_p_token:syntax_close_record()]);

match_record({finish, Location, Fields, Cons}) ->
	RawData = case Cons of
		undefined ->
			{match_record, lists:reverse(Fields)};
		_ ->
			{match_record_head, lists:reverse(Fields), Cons}
	end,
	Data = milang_ast_match_bind:new(RawData),
	Node = milang_ast:ast_node(Location, [], Data),
	parse:success({done, Node}).

match_constructor(Ender) ->
	parse:loop(init, partial:func(fun match_constructor/2, [Ender])).

match_constructor(Ender, init) ->
	match_constructor(Ender, constructor_name);

match_constructor(_Ender, constructor_name) ->
	parse:map_n(fun(Location, ConstructorName) ->
		{step, {arg_root, Location, ConstructorName, []}}
	end, [ parse:location(), identifier_node()]);

match_constructor(Ender, {arg_root, Location, ConstructorName, Acc}) ->
	Ended = parse:map(fun(V) ->
		{done, V}
	end, Ender),
	ParseIdentifierArg = parse:map(fun(Node) ->
		NodeProper = milang_ast:transform_data(fun(_Identifier) ->
			milang_ast_match_bind:new({match_type, Node, []})
		end, Node),
		{continue, NodeProper}
	end, identifier_node()),
	ParseLiteralArg = parse:map(fun(Node) ->
		NodeProper = milang_ast:transform_data(fun(_Literal) ->
			milang_ast_match_bind:new({literal, Node})
		end, Node),
		{continue, NodeProper}
	end, literal()),
	ParseSubMatch = parse:map(fun(_) ->
		submatch
	end, milang_p_token:syntax_open_subexpression()),
	ParseMatchList = parse:map(fun(Node) ->
		{continue, Node}
	end, match_list()),
	ParseMatchRecord = parse:map(fun(Node) ->
		{continue, Node}
	end, match_record()),
	ParseDone = parse:success(done),
	ParseStep = parse:first_of(
		[ Ended
		, ParseIdentifierArg
		, ParseLiteralArg
		, ParseSubMatch
		, ParseMatchList
		, ParseMatchRecord
		, ParseDone
		]),
	ParseDocifyStep = fun
		(Doc, {done, V}) ->
			{step, {finish, Location, ConstructorName, Acc, Doc, V}};
		(Doc, submatch) ->
			{step, {submatch, Location, ConstructorName, Acc, Doc}};
		(Doc, {continue, MatchNode}) ->
			Node = milang_ast:pre_doc(Doc, MatchNode),
			{step, {arg_root, Location, ConstructorName, [ Node | Acc ]}}
	end,
	ParseWithDoc = parse:map_n(ParseDocifyStep, [ milang_p_token:documentation(), ParseStep]),
	ParseNoDoc = parse:map(fun(V) ->
		{step, {finish, Location, ConstructorName, Acc, [], V}}
	end, Ender),
	parse:first_of(
		[ ParseWithDoc
		, ParseNoDoc
		]);

match_constructor(_GivenEnder, {submatch, Location, ConstructorName, Acc, Doc}) ->
	Ender = milang_p_token:syntax_close_subexpression(),
	parse:map_n(fun(Doc2, {Match, _, _}) ->
		Node = milang_ast:pre_doc([Doc, Doc2], Match),
		{step, {arg_root, Location, ConstructorName, [Node | Acc]}}
	end, [ milang_p_token:any_space(), parse:lazy(fun match_clause_bind/1, [Ender])]);

match_constructor(_Ender, {finish, Location, ConstructorName, Acc, HangingDoc, Ended}) ->
	Data = milang_ast_match_bind:new({match_type, ConstructorName, lists:reverse(Acc)}),
	Node = milang_ast:ast_node(Location, [], Data),
	parse:success({done, {Node, HangingDoc, Ended}}).

expose() ->
	Final = partial:func(fun(Location, MoarDoc, ThingExposed) ->
		DocedThing = milang_ast:pre_doc(MoarDoc, ThingExposed),
		milang_ast:ast_node(Location, [], milang_ast_expose:new(DocedThing))
	end, []),
	parse:in_context(expose, parse:pipeline(parse:success(Final),
		[ parse:keep(parse:location())
		, parse:discard(milang_p_token:keyword_expose())
		, parse:keep(milang_p_token:documentation())
		, parse:keep(parse:first_of(
			[ data()
			, spec()
			, an_alias()
			]))
		])).

expose_all() ->
	Final = partial:func(fun(Location, MoarDoc, ThingExposed) ->
		DocedThing = milang_ast:pre_doc(MoarDoc, ThingExposed),
		milang_ast:ast_node(Location, [], milang_ast_expose:new(DocedThing, expose_all))
	end, []),
	parse:in_context(expose_all, parse:pipeline(parse:success(Final),
		[ parse:keep(parse:location())
		, parse:discard(milang_p_token:keyword_expose_all())
		, parse:keep(milang_p_token:documentation())
		, parse:keep(data())
		])).

spec() ->
	Final = partial:func(fun(Location, SpecDoc1, SpecName, ConstraintsDoc, Constraints, OriginalDoc2, Original) ->
			DoccedConstraints = milang_ast:pre_doc(ConstraintsDoc, Constraints),
			DocedOriginal = milang_ast:pre_doc(OriginalDoc2, Original),
			milang_ast:ast_node(Location, SpecDoc1, milang_ast_spec:new(SpecName, DoccedConstraints, DocedOriginal))
	end, []),
	parse:in_context(spec, parse:pipeline(parse:success(Final),
		[ parse:keep(parse:location())
		, parse:discard(milang_p_token:keyword_spec())
		, parse:keep(milang_p_token:documentation())
		, parse:keep(parse:map(fun(N) ->
			io:format("~s: spec name: ~p~n", [?FUNCTION_NAME, N]),
			N
		end, milang_p_token:identifier_bound()))
		, parse:keep(milang_p_token:documentation())
		, parse:keep(constraints())
		, parse:discard(milang_p_token:any_space())
		, parse:discard(milang_p_token:syntax_bind())
		, parse:keep(milang_p_token:documentation())
		, parse:keep(type_concrete())
		, parse:discard(milang_p_token:any_space())
		, parse:discard(milang_p_token:syntax_dot())
		])).

type_concrete() ->
	parse:in_context(type_concrete, parse:loop(init, fun type_concrete/1)).

type_concrete(init) ->
	parse:success({step, {start_spec_arg, _Docs = [], _SpecAcc = []}});

type_concrete({start_spec_arg, Docs, SpecAcc}) ->
	io:format("~s: start_spec_arg~n", [?FUNCTION_NAME]),
	TypeParse = parse:map(fun(TypeName) ->
		{data_type, TypeName}
	end, milang_p_token:identifier_bound()),
	AnyTypeParse = parse:map(fun(AnyName) ->
		{ignored_type, AnyName}
	end, milang_p_token:identifier_ignored()),
	SubTypeParse = parse:map(fun(_) ->
		subtype
	end, milang_p_token:syntax_open_subexpression()),
	RecordTypeParse = parse:map(fun(R) ->
		{record, R}
	end, record_concrete()),
	StepIndicatorParse = parse:first_of(
		[ TypeParse
		, AnyTypeParse
		, SubTypeParse
		, RecordTypeParse
		]),
	parse:in_context(start_spec_arg, parse:map_n(fun(Location, StepIndicator) ->
		case StepIndicator of
			{data_type, TypeName} ->
				io:format("~s: next step is type_args~n", [?FUNCTION_NAME]),
				{step, {type_args, Location, Docs, TypeName, [], SpecAcc}};
			{ignored_type, AnyName} ->
				io:format("~s: next step docced_spec_seperator_or_done~n", [?FUNCTION_NAME]),
				Data = milang_ast_concrete:new(any, AnyName),
				Node = milang_ast:ast_node(Location, Docs, Data),
				{step, {docced_spec_seperator_or_done, [ Node | SpecAcc ]}};
			subtype ->
				NextStepper = fun(ConcreteNode) ->
					io:format("~s: next step is  docced_spec_seperator_or_done~n", [?FUNCTION_NAME]),
					DoccedNode = milang_ast:pre_doc(Docs, ConcreteNode),
					{step, {docced_spec_seperator_or_done, [ DoccedNode | SpecAcc]}}
				end,
				io:format("~s: next step subtype~n", [?FUNCTION_NAME]),
				{step, {subtype, Docs, NextStepper}};
			{record, R} ->
				NewNode = milang_ast:pre_doc(Docs, R),
				{step, {docced_spec_seperator_or_done, [ NewNode | SpecAcc]}}
		end
	end, [ parse:location(), StepIndicatorParse ]));

type_concrete({type_args, Location, Docs, TypeName, ArgsAcc, SpecAcc}) ->
	io:format("~s: type_args~n", [?FUNCTION_NAME]),
	DocOrDoneParse = parse:first_of(
		[ milang_p_token:documentation()
		, parse:success(done)
		]),
	ArgOrDoneParse = parse:and_then(fun
		(done) ->
			io:format("~s: done~n", [?FUNCTION_NAME]),
			parse:success({done, []});
		(Doc) ->
			io:format("~s: type_args got doc, determining next step~n", [?FUNCTION_NAME]),
			SubtypeParse = parse:map_n(fun(_, Doc2) ->
				NextStepper = fun(ConcreteNode) ->
					DoccedNode = milang_ast:pre_doc(Doc, ConcreteNode),
					NewArgsAcc = [ DoccedNode | ArgsAcc],
					io:format("~s: next step is type_args~n", [?FUNCTION_NAME]),
					{step, {type_args, Location, Docs, TypeName, NewArgsAcc, SpecAcc}}
				end,
				{subtype, Doc2, NextStepper}
			end, [milang_p_token:syntax_open_subexpression(), milang_p_token:any_space()]),
			%% need to catch the -> otherwise it matches as an identifier,
			%% but ends up failing since it would then match a reserved word.
			%% To void backtracking and re-parsting it, we check for it here.
			ImpliesParse = parse:map_n(fun(_, MoarDoc) ->
				{next_spec, [Doc, MoarDoc]}
			end, [milang_p_token:syntax_implies(), milang_p_token:any_space()]),
			IdentifierParse = parse:map_n(fun(IdLocation, I) ->
				Node = milang_ast:ast_node(IdLocation, Doc, I),
				NewArgsAcc = [ Node | ArgsAcc ],
				{next_arg, NewArgsAcc}
			end, [parse:location(), milang_p_token:identifier()]),
			StillDoneParse = parse:success({done, Doc}),
			parse:first_of(
				[ SubtypeParse
				, ImpliesParse
				, IdentifierParse
				, StillDoneParse
				])
	end, DocOrDoneParse),
	parse:in_context(type_args, parse:map(fun
		({done, Doc}) ->
			io:format("~s: next step spec_seperator_or_done~n", [?FUNCTION_NAME]),
			DataTypeData = milang_ast_concrete_data:new(TypeName, lists:reverse(ArgsAcc)),
			DataTypeNode = milang_ast:ast_node(Location, Docs, DataTypeData),
			Wrapping = milang_ast:ast_node(Location, [], milang_ast_concrete:new(data, DataTypeNode)),
			NewSpecAcc = [ Wrapping | SpecAcc],
			{step, {spec_seperator_or_done, Doc, NewSpecAcc}};
		({next_spec, Doc}) ->
			io:format("~s: next step start_spec_arg~n", [?FUNCTION_NAME]),
			DataTypeData = milang_ast_concrete_data:new(TypeName, lists:reverse(ArgsAcc)),
			DataTypeNode = milang_ast:ast_node(Location, Docs, DataTypeData),
			Wrapping = milang_ast:ast_node(Location, [], milang_ast_concrete:new(data, DataTypeNode)),
			NewSpecAcc = [ Wrapping | SpecAcc],
			{step, {start_spec_arg, Doc, NewSpecAcc}};
		({subtype, D, Nexter}) ->
			io:format("~s: next step is subtype~n", [?FUNCTION_NAME]),
			{step, {subtype, D, Nexter}};
		({next_arg, NewArgsAcc}) ->
			io:format("~s: next step is type_args~n", [?FUNCTION_NAME]),
			{step, {type_args, Location, Docs, TypeName, NewArgsAcc, SpecAcc}}
	end, ArgOrDoneParse));

type_concrete({docced_spec_seperator_or_done, SpecAcc}) ->
	io:format("~s: docced_spec_seperator_or_done~n", [?FUNCTION_NAME]),
	parse:map(fun(Doc) ->
		{step, {spec_seperator_or_done, Doc, SpecAcc}}
	end, milang_p_token:any_space());

type_concrete({spec_seperator_or_done, Doc, SpecAcc}) ->
	io:format("~s: spec_seperator_or_done~n", [?FUNCTION_NAME]),
	ImpliesParse = parse:map_n(fun(_, Doc2) ->
		io:format("~s: next step is start_spac_arg~n", [?FUNCTION_NAME]),
		{step, {start_spec_arg, [Doc, Doc2], SpecAcc}}
	end, [milang_p_token:syntax_implies(), milang_p_token:any_space()]),
	DoneParse = parse:success({step, {finish, SpecAcc}}),
	parse:in_context(spec_seperator_or_done, parse:first_of(
		[ ImpliesParse
		, DoneParse
		]));

type_concrete({subtype, Doc, Nexter}) ->
	io:format("~s: subtype~n", [?FUNCTION_NAME]),
	ParseSubtype = parse:in_context(subtype, parse:map_n(fun(MoarDoc, Concrete) ->
		milang_ast:pre_doc([Doc, MoarDoc], Concrete)
	end, [milang_p_token:any_space(), type_concrete()])),
	parse:map_n(fun(Type, _) ->
		Nexter(Type)
	end, [ ParseSubtype, milang_p_token:syntax_close_subexpression()]);

type_concrete({finish, []}) ->
	io:format("~s: finish (no data)~n", [?FUNCTION_NAME]),
	parse:fail(type_has_no_data);

type_concrete({finish, [One]}) ->
	io:format("~s: finish One~n", [?FUNCTION_NAME]),
	parse:success({done, One});

type_concrete({finish, Many}) ->
	io:format("~s: finish~n", [?FUNCTION_NAME]),
	[Head | Args] = lists:reverse(Many),
	Location = milang_ast:location(Head),
	WrappedData = milang_ast_concrete_function:new(Head, Args),
	WrappedNode = milang_ast:ast_node(Location, [], WrappedData),
	WrapperData = milang_ast_concrete:new(function, WrappedNode),
	WrapperNode = milang_ast:ast_node(Location, [], WrapperData),
	parse:success({done, WrapperNode}).

record_syntax() ->
	parse:loop(init, fun record_syntax/1).

record_syntax(init) ->
	parse:map_n(fun(Location, _) ->
		{step, {opened, Location}}
	end, [ parse:location(), milang_p_token:syntax_open_record()]);

record_syntax({opened, Location}) ->
	InstantClose = parse:map(fun(_) ->
		{step, {instant_closed, Location}}
	end, milang_p_token:syntax_close_record()),
	RecordLiteral = parse:success({step, {record_literal, Location}}),
	parse:first_of(
		[ InstantClose
		, RecordLiteral
		]);

record_syntax({instant_closed, Location}) ->
	Identifier = parse:map(fun(FieldName) ->
		Data = milang_ast_record_access:new(FieldName),
		Node = milang_ast:ast_node(Location, [], Data),
		{done, Node}
	end, milang_p_token:identifier_local()),
	AllDone = parse:lazy(fun() ->
		Data = milang_ast_literal_record:new(undefined, []),
		Node = milang_ast:ast_node(Location, [], Data),
		parse:success({done, Node})
	end),
	parse:first_of(
		[ Identifier
		, AllDone
		]);

record_syntax({record_literal, Location}) ->
	InitElements = {elements, []},
	Loop = parse:loop(InitElements, fun literal_record_elements/1),
	parse:map(fun(#{ cons := Cons, elements := Elements}) ->
		Data = milang_ast_literal_record:new(Cons, Elements),
		Node = milang_ast:ast_node(Location, [], Data),
		{done, Node}
	end, Loop).

record_concrete() ->
	parse:loop(init, fun record_concrete/1).

record_concrete(init) ->
	record_concrete(opener);

record_concrete(opener) ->
	parse:map_n(fun(Location, _) ->
		BaseNode = milang_ast:ast_node(Location, [], undefined),
		{step, {element_or_done, BaseNode, []}}
	end, [ parse:location(), milang_p_token:syntax_open_record()]);

record_concrete({element_or_done, BaseNode, Elements}) ->
	Element = parse:map(fun(_) ->
		element
	end, milang_p_token:syntax_element_seperator()),
	Done = parse:map(fun(_) ->
		done
	end, milang_p_token:syntax_close_record()),
	Step = parse:first_of(
		[ Element
		, Done
		]),
	parse:map_n(fun
		(Doc, ElementLoc, element) ->
			{step, {element, BaseNode, ElementLoc, Doc, Elements}};
		(_Doc, _EndLocation, done) ->
			{step, {finish, BaseNode, Elements}}
	end, [ milang_p_token:any_space(), parse:location(), Step]);

record_concrete({element, BaseNode, Location, Doc, Elements}) ->
	Final = partial:func(fun(Doc2, FieldName, Doc3, Doc4, FieldType) ->
		NameNode = milang_ast:pre_doc(Doc2, FieldName),
		TypeNode = milang_ast:pre_doc([Doc3, Doc4], FieldType),
		Data = milang_ast_concrete_record_field:new(NameNode, TypeNode),
		Node = milang_ast:ast_node(Location, Doc, Data),
		{step, {element_or_done, BaseNode, [ Node | Elements]}}
	end),
	parse:pipeline(parse:success(Final),
		[ parse:keep(milang_p_token:any_space())
		, parse:keep(identifier_node())
		, parse:keep(milang_p_token:any_space())
		, parse:discard(milang_p_token:syntax_bind())
		, parse:keep(milang_p_token:any_space())
		, parse:keep(parse:lazy(fun() -> type_concrete() end))
		]);

record_concrete({finish, BaseNode, ElementsRev}) ->
	Data = milang_ast_concrete_record:new(lists:reverse(ElementsRev)),
	Node = milang_ast:transform_data(fun(_) -> Data end, BaseNode),
	parse:success({done, Node}).


