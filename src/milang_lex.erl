-module(milang_lex).

-include_lib("kernel/include/logger.hrl").

-export([as_module/1, as_header/1, it/2]).

-spec as_module([ milang_p_token:token() ]) -> {ok, [ milang_ast:ast_node() ]} | {error, term()}.
as_module(Tokens) ->
	it(Tokens, []).

-spec as_header([ milang_p_token:token() ]) -> {ok, [ milang_ast:ast_node() ]} | {error, term()}.
as_header(Tokens) ->
	it(Tokens, [{mode, header}]).

-type options() :: #{ mode := header | module }.
-spec options(list()) -> options().
options(Options) ->
	Defaults = #{ mode => module },
	lists:foldl(fun option/2, Defaults, Options).

option({mode, Mode}, Map) ->
	Map#{ mode => Mode};
option(_, Acc) ->
	Acc.

-spec it([ milang_p_token:token() ], [{mode, header | module}]) -> {ok, [ milang_ast:ast_node() ]} | {error, term()}.
it(Tokens, Options) ->
	root(Tokens, options(Options)).

-spec root([ milang_p_token:token() ], options()) -> {error, term()} | {ok, [ milang_ast:ast_node()]}.
root(Tokens, Options) ->
	root(consume_whitespace, Tokens, Options, []).

root(_, [], _Options, Acc) ->
	{ok, lists:reverse(Acc)};
root(S, [{eof, _, _} | Tail], Options, Acc) ->
	root(S, Tail, Options, Acc);
root(consume_whitespace, Tokens, Options, Acc) ->
	{_, Comments, NewTokens} = space(Tokens),
	root({expect_declaration, Comments}, NewTokens, Options, Acc);
root({expect_declaration, Comments}, [{keyword, L, module} | Tokens], Options, Acc) ->
	case declaration_module(L, Comments, Options, Tokens) of
		{ok, Declaration, NewTokens} ->
			root(consume_whitespace, NewTokens, Options, [Declaration | Acc]);
		Error ->
			Error
	end;
root({expect_declaration, Comments}, [{keyword, L, import} | Tokens], Options, Acc) ->
	ProtoNode = milang_ast:ast_node(L, Comments, undefined),
	case declaration_import(ProtoNode, Tokens, Options) of
		{ok, Declaration, NewTokens} ->
			root(consume_whitespace, NewTokens, Options, [Declaration | Acc]);
		Error ->
			Error
	end;
root({expect_declaration, Comments}, [{keyword, L, alias} | Tokens], Options, Acc) ->
	ProtoNode = milang_ast:ast_node(L, Comments, undefined),
	case declaration_alias(ProtoNode, Tokens, Options) of
		{ok, Declaration, NewTokens} ->
			root(consume_whitespace, NewTokens, Options, [Declaration | Acc]);
		Error ->
			Error
	end;
root({expect_declaration, Comments}, [{keyword, _, type} = T | Tokens], Options, Acc) ->
	case declaration_type(Comments ++ [T] ++ Tokens, Options) of
		{ok, Declaration, NewTokens} ->
			root(consume_whitespace, NewTokens, Options, [Declaration | Acc]);
		Error ->
			Error
	end;
root({expect_declaration, Comments}, [{keyword, L, class} | Tokens], Options, Acc) ->
	ProtoNode = milang_ast:ast_node(L, Comments),
	case declaration_class(ProtoNode, Tokens, Options) of
		{ok, Declaration, NewTokens} ->
			root(consume_whitespace, NewTokens, Options, [Declaration | Acc]);
		Error ->
			Error
	end;
root({expect_declaration, Comments}, [{keyword, L, 'teach'} | Tokens], Options, Acc) ->
	ProtoNode = milang_ast:ast_node(L, Comments),
	case declaration_teach(ProtoNode, Tokens, Options) of
		{ok, Declaration, NewTokens} ->
			root(consume_whitespace, NewTokens, Options, [Declaration | Acc]);
		Error ->
			Error
	end;
root({expect_declaration, Comments}, [{keyword, L, 'let'} | Tokens], Options, Acc) ->
	ProtoNode = milang_ast:ast_node(L, Comments, undefined),
	case declaration_let(ProtoNode, Tokens, Options) of
		{ok, Declaration, NewTokens} ->
			root(consume_whitespace, NewTokens, Options, [Declaration | Acc]);
		Error ->
			Error
	end;
root({expect_declaration, Comments}, [{keyword, L, expose} | Tokens], Options, Acc) ->
	ProtoNode = milang_ast:ast_node(L, Comments, undefined),
	case exposed_declaration(ProtoNode, Tokens, Options) of
		{ok, Declaration, NewTokens} ->
			root(consume_whitespace, NewTokens, Options, [Declaration | Acc]);
		Error ->
			Error
	end;
root({expect_declaration, Comments}, [{keyword, L, 'expose all'} | Tokens], Options, Acc) ->
	ProtoNode = milang_ast:ast_node(L, Comments, milang_ast_type:new(undefined, [], [], [])),
	case expose_all_declaration(ProtoNode, Tokens, Options) of
		{ok, Declaration, NewTokens} ->
			root(consume_whitespace, NewTokens, Options, [Declaration | Acc]);
		Error ->
			Error
	end;
root({expect_declaration, Comments}, [{keyword, L, spec} | Tokens], Options, Acc) ->
	ProtoNode = milang_ast:ast_node(L, Comments),
	case declaration_spec(ProtoNode, Tokens, Options) of
		{ok, DeclarationSpec, NewTokens} ->
			root(consume_whitespace, NewTokens, Options, [DeclarationSpec | Acc]);
		Error ->
			Error
	end;
root(State, [Token | _], _Options, _Acc) ->
	lex_error(Token, [expose, 'expose_all', spec, 'let', whitespace, module, import], {root, State}).

exposed_declaration(ProtoNode, Tokens, Options) ->
	expose_declaration(consume_whitespace, ProtoNode, Tokens, Options).

expose_declaration(consume_whitespace, ProtoNode, Tokens, Options) ->
	{true, Comments, TokensSansSpace} = space(Tokens),
	[{_, L, _} | _] = TokensSansSpace,
	ExposedProtoNode = milang_ast:ast_node(L, Comments),
	expose_declaration({declaration, ExposedProtoNode}, ProtoNode, TokensSansSpace, Options);

expose_declaration({declaration, ExposedProtoNode}, ProtoNode, [{keyword, _, 'spec'} | Tokens], Options) ->
	case declaration_spec(ExposedProtoNode, Tokens, Options) of
		{ok, Declaration, NewTokens} ->
			FinalNode = milang_ast:transform_data(fun(_) -> milang_ast_expose:new(Declaration) end, ProtoNode),
			{ok, FinalNode, NewTokens};
		Error ->
			Error
	end;
expose_declaration({declaration, ExposedProtoNode}, ProtoNode, [{keyword, _, 'type'} | Tokens], Options) ->
	case declaration_type(ExposedProtoNode, Tokens, Options) of
		{ok, Declaration, NewTokens} ->
			FinalNode = milang_ast:transform_data(fun(_) -> milang_ast_expose:new(Declaration) end, ProtoNode),
			{ok, FinalNode, NewTokens};
		Error ->
			Error
	end;
expose_declaration({declaration, ExposedProtoNode}, ProtoNode, [{keyword, _, 'alias'} | Tokens], Options) ->
	case declaration_alias(ExposedProtoNode, Tokens, Options) of
		{ok, Declaration, NewTokens} ->
			FinalNode = milang_ast:transform_data(fun(_) -> milang_ast_expose:new(Declaration) end, ProtoNode),
			{ok, FinalNode, NewTokens};
		Error ->
			Error
	end;
expose_declaration({declaration, ExposedProtoNode}, ProtoNode, [{keyword, _, 'class'} | Tokens], Options) ->
	case declaration_class(ExposedProtoNode, Tokens, Options) of
		{ok, Declaration, NewTokens} ->
			FinalNode = milang_ast:transform_data(fun(_) -> milang_ast_expose:new(Declaration) end, ProtoNode),
			{ok, FinalNode, NewTokens};
		Error ->
			Error
	end;
expose_declaration({declaration, ExposedProtoNode}, _ProtoNode, [Token | _], _Options) ->
	lex_error(Token, ['class', 'type', 'alias', 'spec'], {simple_expose, {declaration, ExposedProtoNode}}).

expose_all_declaration(ProtoNode, Tokens, Options) ->
	expose_all_declaration(consume_whitespace, ProtoNode, Tokens, Options).

expose_all_declaration(consume_whitespace, ProtoNode, Tokens, Options) ->
	{true, Comments, TokensSansSpace} = space(Tokens),
	NewNode = milang_ast:add_doc(Comments, ProtoNode),
	expose_all_declaration(declaration, NewNode, TokensSansSpace, Options);

expose_all_declaration(declaration, ProtoNode, [{keyword, _, 'type'} | Tokens], Options) ->
	case declaration_type(ProtoNode, Tokens, Options) of
		{ok, Declaration, NewTokens} ->
			FinalNode = milang_ast:transform_data(fun(_) -> milang_ast_expose:new(Declaration, expose_all) end, ProtoNode),
			{ok, FinalNode, NewTokens};
		Error ->
			Error
	end;
expose_all_declaration(declaration, _ProtoNode, [Token | _], _Options) ->
	lex_error(Token, 'type', expose_all).

declaration_spec(ProtoNode, Tokens, Options) ->
	case space(Tokens) of
		{true, MoreComments, NewTokens} ->
			NewNode = milang_ast:add_doc(MoreComments, ProtoNode),
			declaration_spec(expect_name, NewNode, NewTokens, Options);
		_ ->
			lex_error(hd(Tokens), space, {declaration_spec, space_before_name})
	end.

declaration_spec(expect_name, ProtoNode, Tokens, Options) ->
	case identifier_bound(Tokens, Options) of
		{ok, Identifier, NewTokens} ->
			declaration_spec({space_after_name, Identifier}, ProtoNode, NewTokens, Options);
		Error ->
			Error
	end;

declaration_spec({space_after_name, Identifier}, ProtoNode, Tokens, Options) ->
	case space(Tokens) of
		{false, _, _} ->
			lex_error(hd(Tokens), space, {declaration_spec, {space_after_name, Identifier}});
		{true, Comments, TokensSansSpace} ->
			declaration_spec({when_or_bind, Identifier, Comments}, ProtoNode, TokensSansSpace, Options)
	end;

declaration_spec({when_or_bind, Identifier, Comments}, ProtoNode, Tokens, Options) ->
	case Tokens of
		[{keyword, L, 'when'} | Tail] ->
			ConstraintsNode = milang_ast:ast_node(L, Comments),
			declaration_spec({constraints, ConstraintsNode, Identifier}, ProtoNode, Tail, Options);
		[{syntax_bind, _, _} | Tail] ->
			declaration_spec({spec_proper, undefined, Identifier}, ProtoNode, Tail, Options);
		_ ->
			lex_error(hd(Tokens), ['when', syntax_bind], {declaration_spec, {Identifier, Comments}})
	end;

declaration_spec({constraints, Node, Identifier}, ProtoNode, Tokens, Options) ->
	case constraints(Node, Tokens, Options) of
		{ok, ConstraintsNode, NewTokens} ->
			declaration_spec({bind_proper, ConstraintsNode, Identifier}, ProtoNode, NewTokens, Options);
		Error ->
			Error
	end;

declaration_spec({bind_proper, ConstraintsNode, Identifier}, ProtoNode, Tokens, Options) ->
	{_, _, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_bind, _, _} | NewTokens] ->
			declaration_spec({spec_proper, ConstraintsNode, Identifier}, ProtoNode, NewTokens, Options);
		[H | _] ->
			lex_error(H, syntax_bind, {declaration_spec, {bind_proper, ConstraintsNode, Identifier, ProtoNode}})
	end;

declaration_spec({spec_proper, Constraints, Identifier}, ProtoNode, Tokens, Options) ->
	case type(Tokens, Options) of
		{ok, Node, NewTokens} ->
			declaration_spec({dot, Identifier, Constraints, Node}, ProtoNode, NewTokens, Options);
		Error ->
			Error
	end;

declaration_spec({dot, Identifier, Constraints, Node}, ProtoNode, Tokens, _Options) ->
	{_, _, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_dot, _, _} | NewTokens] ->
			NewNode = milang_ast:transform_data(fun(_) -> milang_ast_spec:new(Identifier, Constraints, Node) end, ProtoNode),
			{ok, NewNode, NewTokens};
		_ ->
			lex_error(hd(TokensSansSpace), dot, {declaration_spec, {Identifier, Node, ProtoNode}})
	end.

declaration_let(ProtoNode, Tokens, Options) ->
	case space(Tokens) of
		{true, MoreComments, NewTokens} ->
			NewNode = milang_ast:add_doc(MoreComments, ProtoNode),
			declaration_let(expect_name, NewNode, NewTokens, Options);
		_ ->
			lex_error(hd(Tokens), space, {declaration_let, {space_before_name, ProtoNode}})
	end.

declaration_let(expect_name, ProtoNode, Tokens, Options) ->
	case identifier_bound(Tokens, Options) of
		{ok, NameNode, NewTokens} ->
			declaration_let({syntax_bind, NameNode}, ProtoNode, NewTokens, Options);
		Error ->
			Error
	end;

declaration_let({syntax_bind, Name}, ProtoNode, Tokens, Options) ->
	case space(Tokens) of
		{true, ExtraComments, [{syntax_bind, _, _} | NewTokens]} ->
			declaration_let({expression, Name, ExtraComments}, ProtoNode, NewTokens, Options);
		{true, _, TokensSansSpace} ->
			lex_error(hd(TokensSansSpace), syntax_bind, {declaration_let, {Name, ProtoNode}});
		_ ->
			lex_error(hd(Tokens), space, {declaration_let, {Name, ProtoNode}})
	end;

declaration_let({expression, Name, MoreComments}, ProtoNode, Tokens, Options) ->
	case expression(Tokens, Options) of
		{ok, Node, NewTokens} ->
			ExprNode = milang_ast:pre_doc(MoreComments, Node),
			declaration_let({dot, Name, ExprNode}, ProtoNode, NewTokens, Options);
		Error ->
			Error
	end;

declaration_let({dot, Name, Expression}, ProtoNode, Tokens, _Options) ->
	{_, _, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_dot, _, _} | NewTokens] ->
			Node = milang_ast:transform_data(fun(_) -> milang_ast_binding:new(Name, Expression) end, ProtoNode),
			{ok, Node, NewTokens};
		_ ->
			lex_error(hd(TokensSansSpace), dot, {declaration_let, ProtoNode})
	end.

declaration_alias(ProtoNode, Tokens, Options) ->
	case space(Tokens) of
		{false, _, _} ->
			lex_error(hd(Tokens), space, {declaration_alias, {space_before_name, ProtoNode}});
		{true, Comments, TokensSansSpace} ->
			NewNode = milang_ast:add_doc(Comments, ProtoNode),
			declaration_alias(name, NewNode, TokensSansSpace, Options)
	end.

declaration_alias(name, ProtoNode, Tokens, Options) ->
	case identifier_type(Tokens, Options) of
		{ok, NameNode, NewTokens} ->
			declaration_alias({args_when_or_equal, NameNode, []}, ProtoNode, NewTokens, Options);
		Error ->
			Error
	end;

declaration_alias({args_when_or_equal, NameNode, ArgsAcc}, ProtoNode, Tokens, Options) ->
	{HasSpace, Comments, TokensSansSpace} = space(Tokens),
	case {HasSpace, TokensSansSpace} of
		{false, _} ->
			lex_error(hd(Tokens), space, {declaration_alias, {args_when_or_equal, NameNode, ArgsAcc, ProtoNode}});
		{_, [{syntax_keyword, L, 'when'} | Tail]} ->
			ConstraintNode = milang_ast:ast_node(L, Comments),
			declaration_alias({constraints, ConstraintNode, NameNode, ArgsAcc}, ProtoNode, Tail, Options);
		{_, [{identifier_bound, L, Data} | Tail]} ->
			ArgNode = milang_ast:ast_node(L, Comments, {identifier_bound, Data}),
			declaration_alias({args_when_or_equal, NameNode, [ ArgNode | ArgsAcc]}, ProtoNode, Tail, Options);
		{_, [{syntax_bind, _L, _} | Tail]} ->
			declaration_alias({space_then_original, Comments, [], NameNode, ArgsAcc}, ProtoNode, Tail, Options)
	end;

declaration_alias({constraints, ConstraintNode, NameNode, Args}, ProtoNode, Tokens, Options) ->
	case constraints(ConstraintNode, Tokens, Options) of
		{ok, FullConstraintNode, NewTokens} ->
			declaration_alias({space_then_equals, FullConstraintNode, NameNode, Args}, ProtoNode, NewTokens, Options);
		Error ->
			Error
	end;

declaration_alias({space_then_equals, Constraints, Name, Args}, ProtoNode, Tokens, Options) ->
	{HasSpace, Comments, TokensSansSpace} = space(Tokens),
	case {HasSpace, TokensSansSpace} of
		{false, _} ->
			lex_error(hd(Tokens), space, {declaration_alias, {space_then_equals, Constraints, Name, Args, ProtoNode}});
		{true, [{syntax_bind, _, _} | Tail]} ->
			declaration_alias({space_then_original, Comments, Constraints, Name, Args}, ProtoNode, Tail, Options)
	end;

declaration_alias({space_then_original, Comments, Constraints, Name, Args}, ProtoNode, Tokens, Options) ->
	case space(Tokens) of
		{true, MoreComments, [{_, L, _} | _] = TokensSansSpace} ->
			OriginalProtoNode = milang_ast:ast_node(L, [Comments, MoreComments]),
			declaration_alias({original, OriginalProtoNode, Constraints, Name, Args}, ProtoNode, TokensSansSpace, Options);
		{false, _, _} ->
			lex_error(hd(Tokens), space, {declaration_alias, {space_then_original, Comments, Constraints, Name, Args, ProtoNode}})
	end;

declaration_alias({original, OriginalProtoNode, Constraints, Name, Args}, ProtoNode, Tokens, Options) ->
	?LOG_DEBUG("And now the original proper.~n    OriginalProto: ~p~n    Name: ~p~n  NextToken: ~p", [OriginalProtoNode, Name, hd(Tokens)]),
	case type(OriginalProtoNode, Tokens, Options) of
		{ok, Node, NewTokens} ->
			AliasNode = milang_ast:transform_data(fun(_) ->
				milang_ast_alias:new(Name, Args, Constraints, Node)
			end, ProtoNode),
			declaration_alias(finish, AliasNode, NewTokens, Options);
		Error ->
			Error
	end;

declaration_alias(finish, Node, Tokens, _Options) ->
	{_, _, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_dot, _, _} | Tail] ->
			{ok, Node, Tail};
		_ ->
			lex_error(hd(TokensSansSpace), dot, {declaration, {finish, Node}})
	end.

declaration_class(ProtoNode, Tokens, Options) ->
	declaration_class(name, ProtoNode, Tokens, Options).

declaration_class(name, ProtoNode, Tokens, Options) ->
	case space(Tokens) of
		{false, _, _} ->
			lex_error(hd(Tokens), space, {declaration_class, {name, ProtoNode}});
		{true, Comments, TokensSansSpace} ->
			declaration_class({name, Comments}, ProtoNode, TokensSansSpace, Options)
	end;

declaration_class({name, Comments}, ProtoNode, Tokens, Options) ->
	case Tokens of
		[{identifier_type, L, Name} | NewTokens] ->
			NameNode = milang_ast:ast_node(L, Comments, {identifier_type, Name}),
			declaration_class({args, [], NameNode}, ProtoNode, NewTokens, Options);
		[H | _] ->
			lex_error(H, identifier_type, {declaration_class, {name, Comments, ProtoNode}})
	end;

declaration_class({args, ArgAcc, NameNode}, ProtoNode, Tokens, Options) ->
	case space(Tokens) of
		{false, _, _} ->
			lex_error(hd(Tokens), space, {declaration_class, {args, ArgAcc, NameNode, ProtoNode}});
		{true, Comments, [{identifier_bound, L, Name} | Tail]} ->
			ArgNode = milang_ast:ast_node(L, Comments, {identifier_bound, Name}),
			declaration_class({args, [ ArgNode | ArgAcc ], NameNode}, ProtoNode, Tail, Options);
		{true, Comments, [{keyword, L, 'when'} | Tail]} ->
			{_, MoreComments, CosntraintsStart} = space(Tail),
			ConstraintsNode = milang_ast:ast_node(L, [Comments, MoreComments]),
			declaration_class({constraints, ConstraintsNode, ArgAcc, NameNode}, ProtoNode, CosntraintsStart, Options);
		{true, Comments, [{syntax_bind, L, _} | Tail]} ->
			Constraints = milang_ast_constraints:new([]),
			ConstraintsNode = milang_ast:ast_node(L, [], Constraints),
			declaration_class({members, [], ConstraintsNode, ArgAcc, NameNode}, ProtoNode, Comments ++ Tail, Options)
	end;

declaration_class({constraints, ConstraintsProtoNode, ArgNodes, NameNode}, ProtoNode, Tokens, Options) ->
	case constraints(ConstraintsProtoNode, Tokens, Options) of
		{ok, Constraints, NewTokens} ->
			declaration_class({syntax_bind, Constraints, ArgNodes, NameNode}, ProtoNode, NewTokens, Options);
		Error ->
			Error
	end;

declaration_class({syntax_bind, Constraints, ArgNodes, NameNode}, ProtoNode, Tokens, Options) ->
	{_, _, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_bind, _, _} | Tail] ->
			declaration_class({members, [], Constraints, ArgNodes, NameNode}, ProtoNode, Tail, Options);
		[H | _] ->
			lex_error(H, syntax_bind, {declaration_class, {syntax_bind, Constraints, ArgNodes, NameNode, ProtoNode}})
	end;

declaration_class({members, MembersAcc, Constraints, Args, Name}, ProtoNode, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	[{_, L, _} = Head | TokenTail ] = TokensSansSpace,
	MemberProtoNode = milang_ast:ast_node(L, Comments),
	case Head of
		{keyword, _, 'let'} ->
			case declaration_let(MemberProtoNode, TokenTail, Options) of
				{ok, Node, NewTokens} ->
					declaration_class({members, [Node | MembersAcc], Constraints, Args, Name}, ProtoNode, NewTokens, Options);
				Error ->
					Error
			end;
		{keyword, _, spec} ->
			case declaration_spec(MemberProtoNode, TokenTail, Options) of
				{ok, Node, NewTokens} ->
					declaration_class({members, [Node | MembersAcc], Constraints, Args, Name}, ProtoNode, NewTokens, Options);
				Error ->
					Error
			end;
		{syntax_dot, _, _} ->
			declaration_class({finish, MembersAcc, Constraints, Args, Name}, ProtoNode, TokenTail, Options);
		_ ->
			lex_error(Head, [keyword_let, keyword_spec, syntax_dot], {declaration_class, {members, MembersAcc, Constraints, Args, Name, ProtoNode}})
	end;

declaration_class({finish, ReversedMembers, Constraints, ReversedArgs, Name}, ProtoNode, Tokens, _Options) ->
	Members = lists:reverse(ReversedMembers),
	Args = lists:reverse(ReversedArgs),
	Data = milang_ast_class:new(Name, Constraints, Args, Members),
	Node = milang_ast:transform_data(fun(_) -> Data end, ProtoNode),
	{ok, Node, Tokens}.

declaration_teach(ProtoNode, Tokens, Options) ->
	declaration_teach(name_of_student, ProtoNode, Tokens, Options).

declaration_teach(name_of_student, ProtoNode, Tokens, Options) ->
	case space(Tokens) of
		{false, _, _} ->
			lex_error(hd(Tokens), space, {declaration_teach, {name_of_student, ProtoNode}});
		{true, Comments, [{identifier_type, L, Name} | Tail]} ->
			StudentNode = milang_ast:ast_node(L, Comments, {identifier_type, Name}),
			declaration_teach({class_keyword, StudentNode}, ProtoNode, Tail, Options);
		{_, _, [H | _]} ->
			lex_error(H, identifier_type, {declaration_teach, {name_of_student, ProtoNode}})
	end;

declaration_teach({class_keyword, StudentNode}, ProtoNode, Tokens, Options) ->
	case space(Tokens) of
		{false, _, _} ->
			lex_error(hd(Tokens), space, {declaration_teach, {class_keyword, StudentNode, ProtoNode}});
		{true, Comments, [{keyword, _, class} | Tail]} ->
			declaration_teach({class_taught, Comments, StudentNode}, ProtoNode, Tail, Options);
		{_, _, [H | _]} ->
			lex_error(H, keyword_class, {declaration_teach, {class_keyword, StudentNode, ProtoNode}})
	end;

declaration_teach({class_taught, Comments, StudentNode}, ProtoNode, Tokens, Options) ->
	case space(Tokens) of
		{false, _, _} ->
			lex_error(hd(Tokens), space, {declaration_teach, {class_taught, Comments, StudentNode, ProtoNode}});
		{true, MoreComments, [{identifier_type, L, Name} | Tail]} ->
			ClassNode = milang_ast:ast_node(L, Comments ++ MoreComments, {identifier_type, Name}),
			declaration_teach({implies, ClassNode, StudentNode}, ProtoNode, Tail, Options);
		{_, _, [H | _]} ->
			lex_error(H, indentifier_type, {declaration_teach, {class_taught, Comments, StudentNode, ProtoNode}})
	end;

declaration_teach({implies, ClassNode, StudentNode}, ProtoNode, Tokens, Options) ->
	case space(Tokens) of
		{false, _, _} ->
			lex_error(hd(Tokens), space, {declaration_teach, {implies, ClassNode, StudentNode, ProtoNode}});
		{true, _Comments, [{syntax_implies, _, _} | Tail]} ->
			declaration_teach({bindings, [], ClassNode, StudentNode}, ProtoNode, Tail, Options);
		{_, _, [H | _]} ->
			lex_error(H, syntax_implies, {declaration_teach, {implies, ClassNode, StudentNode, ProtoNode}})
	end;

declaration_teach({bindings, Acc, ClassNode, StudentNode}, ProtoNode, Tokens, Options) ->
	case space(Tokens) of
		{_, Comments, [{keyword, L, 'let'} | Tail]} ->
			LetProtoNode = milang_ast:ast_node(L, Comments),
			declaration_teach({bindings, LetProtoNode, Acc, ClassNode, StudentNode}, ProtoNode, Tail, Options);
		{_, _, [{syntax_dot, _, _} | Tail]} ->
			declaration_teach({finish, Acc, ClassNode, StudentNode}, ProtoNode, Tail, Options);
		{_, _, [H | _]} ->
			lex_error(H, [keyword_let, syntax_dot], {declaration_teach, {bindings, Acc, ClassNode, StudentNode, ProtoNode}})
	end;

declaration_teach({bindings, ProtoBinding, Acc, ClassNode, StudentNode}, ProtoNode, Tokens, Options) ->
	case declaration_let(ProtoBinding, Tokens, Options) of
		{ok, BindingNode, NewTokens} ->
			declaration_teach({bindings, [ BindingNode | Acc], ClassNode, StudentNode}, ProtoNode, NewTokens, Options);
		Error ->
			Error
	end;

declaration_teach({finish, ReversedBindings, ClassNode, StudentNode}, ProtoNode, Tokens, _Options) ->
	Bindings = lists:reverse(ReversedBindings),
	Data = milang_ast_teach:new(StudentNode, ClassNode, Bindings),
	Node = milang_ast:transform_data(fun(_) -> Data end, ProtoNode),
	{ok, Node, Tokens}.

% start :
%     open_paren -> (start) -> {close_paren, [Res]}.
%     name -> ({constructor, Name, []}) -> {next_arg, [Res]}
%     _ -> error
% {close_paren, ArgsAcc} :
%     close_paren -> {next_arg, ArgsAcc}
%     _ -> error
% { next_arg, ArgsAcc } :
%     open_paren -> (start) -> {next_arg, [ Res | ArgsAcc]}
%     name -> ({constructor, Name, []}) -> {implies_or_done, [ Res | ArgsAcc]}
% { constructor, Name, ArgsAcc} :
%     open_paren -> (start) -> {constructor, Name, [Res | ArgsAcc]}
%     name -> {constructor, Name, [Res | ArgsAcc]}

type(Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	[{_, L, _} | _] = TokensSansSpace,
	type(start, milang_ast:ast_node(L, Comments, []), TokensSansSpace, Options).

type(GiveProtoNode, Tokens, Options) ->
	ProtoNode = milang_ast:transform_data(fun(_) -> [] end, GiveProtoNode),
	type(start, ProtoNode, Tokens, Options).

type(start, ProtoNode, Tokens, Options) ->
	case Tokens of
		[{identifier_type, L, Name} | Tail] ->
			NameNode = milang_ast:ast_node(L, [], {identifier_type, Name}),
			TypeData = milang_ast_concrete:new(NameNode, []),
			TypeNode = milang_ast:ast_node(L, [], TypeData),
			type({constructor, TypeNode}, ProtoNode, Tail, Options);
		[{identifier_ignored, L, Name} | Tail] ->
			Var = milang_ast:ast_node(L, [], {identifier_ignored, Name}),
			NewNode = milang_ast:transform_data(fun(D) ->
				[ Var | D]
			end, ProtoNode),
			type(after_ignored_var, NewNode, Tail, Options);
		[{identifier_bound, L, Name} | Tail] ->
			Var = milang_ast:ast_node(L, [], {identifier_bound, Name}),
			NewNode = milang_ast:transform_data(fun(D) ->
				[ Var | D]
			end, ProtoNode),
			type(after_ignored_var, NewNode, Tail, Options);
		[{syntax_open, L, subexpression} | Tail] ->
			type({subexpression, milang_ast:ast_node(L, [], [])}, ProtoNode, Tail, Options);
		[H | _] ->
			lex_error(H, [identifier_type, syntax_open, identifier_bound, identifier_type], {type, start})
	end;

type(after_ignored_var, ProtoNode, Tokens, Options) ->
	{HasSpace, _Comments, TokensSansSpace} = space(Tokens),
	case {HasSpace, TokensSansSpace} of
		{true, [{syntax_implies, _, _} | Tail]} ->
			type(restart, ProtoNode, Tail, Options);
		{_, [{syntax_dot, _, _} | _]} ->
			type(finish, ProtoNode, TokensSansSpace, Options);
		{false, [{syntax_implies, _, _} | _]} ->
			lex_error(hd(Tokens), space, {type, {after_ignored_var, ProtoNode}});
		{true, [{identifier_type, _, _} | _]} ->
			lex_error(hd(TokensSansSpace), [syntax_implies, dot, terminator], {type, {after_ignored_var, ProtoNode}});
		{true, [{identifier_bound, _, _} | _]} ->
			lex_error(hd(TokensSansSpace), [syntax_implies, dot, terminator], {type, {after_ignored_var, ProtoNode}});
		{true, [{identifier_ignored, _, _} | _]} ->
			lex_error(hd(TokensSansSpace), [syntax_implies, dot, terminator], {type, {after_ignored_var, ProtoNode}});
		{_, [{syntax_close, _, subexpression} | _]} ->
			type(finish, ProtoNode, TokensSansSpace, Options);
		_ ->
			lex_error(hd(Tokens), [], {type, {after_ignored_var, ProtoNode}})
	end;

type(restart, ProtoNode, Tokens, Options) ->
	case space(Tokens) of
		{true, Comments, [{identifier_type, L, Name} | Tail]} ->
			TypeData = milang_ast_concrete:new(milang_ast:ast_node(L, [], {identifier_type, Name}), []),
			TypeNode = milang_ast:ast_node(L, Comments, TypeData),
			type({constructor, TypeNode}, ProtoNode, Tail, Options);
		{true, Comments, [{identifier_bound, L, Name} | Tail]} ->
			Node = milang_ast:ast_node(L, Comments, milang_ast_identifier:bound(Name)),
			NewNode = milang_ast:transform_data(fun(D) ->
				[Node | D]
			end, ProtoNode),
			type(restart, NewNode, Tail, Options);
		{_, Comments, [{syntax_open, L, subexpression} | Tail]} ->
			type({subexpression, milang_ast:ast_node(L, Comments, [])}, ProtoNode, Tail, Options);
		{_, _, [{syntax_implies, _, _} | Tail]} ->
			type(restart, ProtoNode, Tail, Options);
		{_, _, [{syntax_dot, _, _} | _] = TokensSansSpace} ->
			type(finish, ProtoNode, TokensSansSpace, Options);
		{_, _, [{syntax_close, _, subexpression} | _] = TokensSansSpace} ->
			type(finish, ProtoNode, TokensSansSpace, Options);
		{_, _, TokensSansSpace} ->
			lex_error(hd(TokensSansSpace), [identifier, syntax_open_subexpression], {type, restart})
	end;

type({constructor, TypeNode}, ProtoNode, Tokens, Options) ->
	case space(Tokens) of
		{true, _Comments, [{syntax_implies, _, _} | Tail]} ->
			NewNode = milang_ast:transform_data(fun(D) ->
				OldArgs = milang_ast_concrete:args(D),
				NewArgs = lists:reverse(OldArgs),
				milang_ast_concrete:args(NewArgs, D)
			end, TypeNode),
			NewProto = milang_ast:transform_data(fun(D) ->
				[ NewNode | D ]
			end, ProtoNode),
			type(restart, NewProto, Tail, Options);
		{true, Comments, [{identifier_type, L, Name} | Tail]} ->
			NewNode = milang_ast:transform_data(fun(D) ->
				IdentifierNode = milang_ast:ast_node(L, Comments, milang_ast_identifier:type(Name)),
				OldArgs = milang_ast_concrete:args(D),
				NewArgs = [ IdentifierNode | OldArgs ],
				milang_ast_concrete:args(NewArgs, D)
			end, TypeNode),
			type({constructor, NewNode}, ProtoNode, Tail, Options);
		{true, Comments, [{identifier_bound, L, Name} | Tail]} ->
			NewNode = milang_ast:transform_data(fun(D) ->
				IdentifierNode = milang_ast:ast_node(L, Comments, milang_ast_identifier:bound(Name)),
				OldArgs = milang_ast_concrete:args(D),
				NewArgs = [ IdentifierNode | OldArgs],
				milang_ast_concrete:args(NewArgs, D)
			end, TypeNode),
			type({constructor, NewNode}, ProtoNode, Tail, Options);
		{true, Comments, [{syntax_open, L, subexpression} | Tail]} ->
			SubProto = milang_ast:ast_node(L, Comments, []),
			type({subexpression, SubProto}, ProtoNode, Tail, Options);
		{_, _, TokensSansSpace} ->
			% maybe it's a dot indicating completion? or a close paren?
			% those don't need a space before, and are outside the scope of
			% what the type lexer can determine.
			NewNode = milang_ast:transform_data(fun(D) ->
				OldArgs = milang_ast_concrete:args(D),
				NewArgs = lists:reverse(OldArgs),
				milang_ast_concrete:args(NewArgs, D)
			end, TypeNode),
			NewProto = milang_ast:transform_data(fun(D) ->
				[NewNode | D ]
			end, ProtoNode),
			type(finish, NewProto, TokensSansSpace, Options)
	end;

type(finish, ProtoNode, Tokens, _Options) ->
	case milang_ast:data(ProtoNode) of
		[] ->
			lex_error(undefined, undefined, {type, {finish, empty_type, ProtoNode}});
		[Type] ->
			FinalNode = milang_ast:pre_doc(milang_ast:doc(ProtoNode), Type),
			{ok, FinalNode, Tokens};
		TypesReversed ->
			Types = lists:reverse(TypesReversed),
			Data = milang_ast_signature:new(Types),
			FinalNode = milang_ast:transform_data(fun(_) -> Data end, ProtoNode),
			{ok, FinalNode, Tokens}
	end;

type({subexpression, SubProto}, ProtoNode, Tokens, Options) ->
	{_, MoreComments, TokensSansSpace} = space(Tokens),
	SubProtoFullComments = milang_ast:add_doc(MoreComments, SubProto),
	case type(SubProtoFullComments, TokensSansSpace, Options) of
		{ok, Node, NewTokens} ->
			type({subexpression_close, Node}, ProtoNode, NewTokens, Options);
		Error ->
			Error
	end;

type({subexpression_close, Node}, ProtoNode, Tokens, Options) ->
	case space(Tokens) of
		{_, _, [{syntax_close, _, subexpression} | Tail]} ->
			NewNode = milang_ast:transform_data(fun(D) ->
				[ Node | D ]
			end, ProtoNode),
			type(restart, NewNode, Tail, Options);
		{_, _, TokensSansSpace} ->
			lex_error(hd(TokensSansSpace), syntax_close_subexpression, {type, {subexpression_close, Node}})
	end.


declaration_type(Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	[{_, L, _} | Tail] = TokensSansSpace,
	BaseData = milang_ast_type:new(undefined, [], [], []),
	ProtoNode = milang_ast:ast_node(L, Comments, BaseData),
	declaration_type(ProtoNode, Tail, Options).

declaration_type(ProtoNode, Tokens, Options) ->
	declaration_type(space_before_name, ProtoNode, Tokens, Options).

declaration_type(space_before_name, ProtoNode, Tokens, Options) ->
	case space(Tokens) of
		{false, _, _} ->
			lex_error(hd(Tokens), space, {declaration_type, {space_before_name, ProtoNode}});
		{true, Comments, TokensSansSpace} ->
			NewNode = milang_ast:add_doc(Comments, ProtoNode),
			declaration_type(name, NewNode, TokensSansSpace, Options)
	end;

declaration_type(name, ProtoNode, Tokens, Options) ->
	case identifier_type(Tokens, Options) of
		{ok, NameNode, NewTokens} ->
			NewNode = milang_ast:transform_data(fun(M) -> milang_ast_type:name(NameNode, M) end, ProtoNode),
			declaration_type(got_name, NewNode, NewTokens, Options);
		Error ->
			Error
	end;

declaration_type(got_name, ProtoNode, Tokens, Options) ->
	{HasSpace, Comments, TokensSansSpace} = space(Tokens),
	case {HasSpace, TokensSansSpace} of
		{_, [{syntax_dot, _, _} | Tail]} ->
			{ok, ProtoNode, Tail};
		{false, _} ->
			lex_error(hd(Tokens), [space, dot], {declaration_type, {got_name, ProtoNode}});
		{_, [{syntax_keyword, L, 'when'} | Tail]} ->
			ConstraintsNode = milang_ast:ast_node(L, Comments),
			declaration_type({constraints_start, ConstraintsNode}, ProtoNode, Tail, Options);
		{_, [{syntax_bind, _, _} | Tail]} ->
			declaration_type(start_constructors, ProtoNode, Tail, Options);
		{_, [{identifier_bound, L, Name} | Tail]} ->
			ArgNode = milang_ast:ast_node(L, Comments, {identifier_bound, Name}),
			NewNode = milang_ast:transform_data(fun(M) ->
				OldArgs = milang_ast_type:args(M),
				NewArgs = OldArgs ++ [ArgNode],
				milang_ast_type:args(NewArgs, M)
			end, ProtoNode),
			declaration_type(got_name, NewNode, Tail, Options);
		_ ->
			lex_error(hd(TokensSansSpace), [bind, 'when', identifier_bound], {declaration_type, {got_name_and_space, ProtoNode}})
	end;

declaration_type({constraints_start, CNode}, ProtoNode, Tokens, Options) ->
	case constraints(CNode, Tokens, Options) of
		{ok, NewCNode, NewTokens} ->
			NewNode = milang_ast:transform_data(fun(M) ->
				milang_ast_type:constraints(NewCNode, M)
			end, ProtoNode),
			declaration_type(bind_or_done, NewNode, NewTokens, Options);
		Error ->
			Error
	end;

declaration_type(bind_or_done, ProtoNode, Tokens, Options) ->
	case space(Tokens) of
		{_, _, [{syntax_dot, _, _} | Tail]} ->
			{ok, ProtoNode, Tail};
		{false, _, _} ->
			lex_error(hd(Tokens), space, {declaration_type, {bind_or_done, ProtoNode}});
		{true, _, [{syntax_bind, _, _} | Tail]} ->
			declaration_type(start_constructors, ProtoNode, Tail, Options);
		{_, _, TokensSansSpace} ->
			lex_error(hd(TokensSansSpace), [bind, dot], {declaration_type, {bind_or_done}})
	end;

declaration_type(start_constructors, ProtoNode, Tokens, Options) ->
	case space(Tokens) of
		{false, _, _} ->
			lex_error(hd(Tokens), space, {declaration_type, start_constructors});
		{true, _Comments, TokensSansSpace} ->
			declaration_type(constructors, ProtoNode, TokensSansSpace, Options)
	end;

declaration_type(constructors, ProtoNode, Tokens, Options) ->
	case generic_list(Tokens, Options, fun declaration_constructor/2) of
		{ok, List, NewTokens} ->
			NewNode = milang_ast:transform_data(fun(M) -> milang_ast_type:constructors(List, M) end, ProtoNode),
			declaration_type(dot, NewNode, NewTokens, Options);
		Error ->
			Error
	end;

declaration_type(dot, ProtoNode, Tokens, _Options) ->
	{_, _Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_dot, _, _} | Tail] ->
			{ok, ProtoNode, Tail};
		[T | _] ->
			lex_error(T, dot, {declaration_type, {dot, ProtoNode}})
	end.

constraints(ProtoNode, Tokens, Options) ->
	{_, MoreComments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_open, _, list} | NewTokens] ->
			constraints({next_or_done, []}, milang_ast:add_doc(MoreComments, ProtoNode), NewTokens, Options);
		_ ->
			lex_error(hd(Tokens), syntax_open_list, {constraints, ProtoNode, Tokens, Options})
	end.

constraints({next_or_done, Acc}, ProtoNode, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_element_seperator, L, _} | NewTokens] ->
			EntryNode = milang_ast:ast_node(L, Comments, undefined),
			constraints({next, EntryNode, Acc}, ProtoNode, NewTokens, Options);
		[{syntax_close, _, list} | NewTokens] ->
			NewAcc = lists:reverse(Acc),
			NewNode = milang_ast:transform_data(fun(_) -> milang_ast_constraints:new(NewAcc) end, ProtoNode),
			{ok, NewNode, NewTokens};
		_ ->
			lex_error(hd(TokensSansSpace), [syntax_element_seperator, syntax_close_list], {constraints, {next_or_done, Acc}})
	end;

constraints({next, EntryNode, Acc}, ProtoNode, Tokens, Options) ->
	{_, MoreComments, TokensSansSpace} = space(Tokens),
	case identifier_bound(TokensSansSpace, Options) of
		{ok, Name, NewTokens} ->
			NewEntryNode = milang_ast:add_doc(MoreComments, EntryNode),
			constraints({constraint_for, Name, NewEntryNode, Acc}, ProtoNode, NewTokens, Options);
		Error ->
			Error
	end;

constraints({constraint_for, Name, EntryNode, Acc}, ProtoNode, Tokens, Options) ->
	{MaybeSpace, Comments, TokensSansSpace} = space(Tokens),
	MaybeIdentifer = identifier_type(TokensSansSpace, Options),
	case {MaybeSpace, MaybeIdentifer} of
		{true, {ok, Constraint, NewTokens}} ->
			FullyDocEntryNode = milang_ast:add_doc(Comments, EntryNode),
			FullEntryNode = milang_ast:transform_data(fun(_) -> milang_ast_constraint:new(Name, Constraint) end, FullyDocEntryNode),
			constraints({next_or_done, [FullEntryNode | Acc]}, ProtoNode, NewTokens, Options);
		{true, Error} ->
			Error;
		{false, _} ->
			lex_error(hd(Tokens), space, {constraints, {constraints_for, Name}})
	end.

declaration_constructor(Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{_, L, _} | _] ->
			ProtoNode = milang_ast:ast_node(L, Comments, milang_ast_constructor:new(<<>>, [])),
			declaration_constructor({start, ProtoNode}, TokensSansSpace, Options);
		[T | _] ->
			lex_error(T, identifier_type, {declaration_constructor, start})
	end.

declaration_constructor({start, ProtoNode}, Tokens, Options) ->
	case identifier_type(Tokens, Options) of
		{ok, NameNode, NewTokens} ->
			NewNode = milang_ast:transform_data(fun(M) -> milang_ast_constructor:name(NameNode, M) end, ProtoNode),
			declaration_constructor({arg_or_done, [], NewNode}, NewTokens, Options);
		Error ->
			Error
	end;

declaration_constructor({arg_or_done, ArgAcc, ProtoNode}, Tokens, Options) ->
	{Space, Comments, TokensSansSpace} = space(Tokens),
	case {Space, TokensSansSpace} of
		{true, [{identifier_type, L, Name} | Tail]} ->
			ArgNode = milang_ast:ast_node(L, Comments, {identifier_type, Name}),
			declaration_constructor({arg_or_done, [ArgNode | ArgAcc], ProtoNode}, Tail, Options);
		{true, [{identifier_bound, L, Name} | Tail]} ->
			ArgNode = milang_ast:ast_node(L, Comments, {identifier_bound, Name}),
			declaration_constructor({arg_or_done, [ArgNode | ArgAcc], ProtoNode}, Tail, Options);
		{_, [{syntax_open, _, subexpression} | _]} ->
			declaration_constructor({subexpression, ArgAcc, ProtoNode}, Tokens, Options);
		_ ->
			Args = lists:reverse(ArgAcc),
			Node = milang_ast:transform_data(fun(M) -> milang_ast_constructor:args(Args, M) end, ProtoNode),
			{ok, Node, Tokens}
	end;

%declaration_constructor({type_arg, ArgAcc, ProtoNode}, Tokens, Options) ->
%	{_, Comments, TokensSansSpace} = space(Tokens),
%	case identifier(TokensSansSpace, Options) of
%		{ok, NameNode, NewTokens} ->
%			{_, L, _} = hd(TokensSansSpace),
%			ArgNode = milang_ast:ast_node(L, Comments, #type_concrete{ args = [], name = NameNode }),
%			declaration_constructor({arg_or_done, [ArgNode | ArgAcc], ProtoNode}, NewTokens, Options);
%		Error ->
%			Error
%	end;

declaration_constructor({subexpression, ArgAcc, ProtoNode}, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	[Open | Rest] = TokensSansSpace,
	case to_matching_close(Open, Rest, Options) of
		{ok, Open, Chomped, _Close, NewTokens} ->
			declaration_constructor({function_type_arg, Comments, Chomped, ArgAcc, ProtoNode}, NewTokens, Options);
		Error ->
			Error
	end;

declaration_constructor({function_type_arg, Comments, Chomped, ArgAcc, ProtoNode}, Tokens, Options) ->
	case type(Chomped, Options) of
		{ok, ArgNode, NewTokens} ->
			NewArgNode = milang_ast:pre_doc(Comments, ArgNode),
			declaration_constructor({arg_or_done, [NewArgNode| ArgAcc], ProtoNode}, NewTokens ++ Tokens, Options);
		Error ->
			Error
	end.






to_matching_close({syntax_open, _, _} = Open, Tail, Options) ->
	to_matching_close(Open, Tail, [], Options).

to_matching_close({syntax_open, _, Type} = Open, [{syntax_close, _, Type} = Close | Tail], Acc, _Options) ->
	{ok, Open, lists:reverse(Acc), Close, Tail};

to_matching_close({_, L, Type}, [], _Acc, _Options) ->
	lex_error(L, {syntax_close, Type}, end_of_input);

to_matching_close(Open, [{syntax_open, _, _} = NewOpen | Tail], Acc, Options) ->
	case to_matching_close(NewOpen, Tail, Options) of
		{ok, NewOpen, Chomped, NewClose, NewTail} ->
			NewAcc = [NewClose] ++ (lists:reverse(Chomped)) ++ [NewOpen] ++ Acc,
			to_matching_close(Open, NewTail, NewAcc, Options);
		Error ->
			Error
	end;

to_matching_close(Open, [T | Tail], Acc, Options) ->
	to_matching_close(Open, Tail, [T | Acc], Options).

identifier_bound([{identifier_bound, L, Data} | Tail], _Options) ->
	Node = milang_ast:ast_node(L, [], {identifier_bound, Data}),
	{ok, Node, Tail};
identifier_bound(Tokens, _Options) ->
	lex_error(hd(Tokens), identifier_bound, {identifier_bound, Tokens}).

identifier_type([{identifier_type, L, Data} | Tail], _Options) ->
	Node = milang_ast:ast_node(L, [], {identifier_type, Data}),
	{ok, Node, Tail};
identifier_type(Tokens, _Options) ->
	lex_error(hd(Tokens), identifier_type, {identifier_type, Tokens}).

declaration_module(Location, Comments, Options, Tokens) ->
	ProtoNode = milang_ast:ast_node(Location, Comments, milang_ast_module:new(<<>>)),
	do_declaration_module(need_name, Tokens, Options, ProtoNode).

do_declaration_module(need_name, Tokens, Options, ProtoNode) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	NodeWithComments = milang_ast:add_doc(Comments, ProtoNode),
	case hd(TokensSansSpace) of
		{identifier_bound, _, _} ->
			{ok, NameNode, TokensAfterName} = identifier_bound(TokensSansSpace, Options),
			NodeWithName = milang_ast:transform_data(fun(M) -> milang_ast_module:name(NameNode, M) end, NodeWithComments),
			do_declaration_module(need_dot, TokensAfterName, Options, NodeWithName);
		{identifier_type, _, _} ->
			{ok, NameNode, TokensAfterName} = identifier_type(TokensSansSpace, Options),
			NodeWithName = milang_ast:transform_data(fun(M) -> milang_ast_module:name(NameNode, M) end, NodeWithComments),
			do_declaration_module(need_dot, TokensAfterName, Options, NodeWithName);
		Error ->
			lex_error(Error, [identifier_bound, identifier_type], {declaration_module, {need_name, ProtoNode}})
	end;
do_declaration_module(need_dot, Tokens, _Options, ProtoNode) ->
	{_, _, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_dot, _, _} | NewTokens] ->
			{ok, ProtoNode, NewTokens};
		_ ->
			lex_error(hd(TokensSansSpace), dot, {declaration_module, ProtoNode})
	end.

declaration_import(ProtoNode, Tokens, Options) ->
	declaration_import(pre_name, ProtoNode, Tokens, Options).

declaration_import(pre_name, ProtoNode, Tokens, Options) ->
	case space(Tokens) of
		{true, Comments, TokensSansSpace} ->
			NewNode = milang_ast:add_doc(Comments, ProtoNode),
			declaration_import(name, NewNode, TokensSansSpace, Options);
		_ ->
			lex_error(hd(Tokens), space, {declaration_import, {pre_name, ProtoNode}})
	end;
declaration_import(name, ProtoNode, Tokens, Options) ->
	case hd(Tokens) of
		{identifier_bound, _, _} ->
			{ok, Identifier, NewTokens} = identifier_bound(Tokens, Options),
			declaration_import({post_name, Identifier}, ProtoNode, NewTokens, Options);
		{identifier_type, _, _} ->
			{ok, Identifier, NewTokens} = identifier_type(Tokens, Options),
			declaration_import({post_name, Identifier}, ProtoNode, NewTokens, Options);
		Error ->
			lex_error(Error, [identifier_bound, identifier_type], {declaration_import, {name, ProtoNode}})
	end;

declaration_import({post_name, ComplexName}, ProtoNode, Tokens, _Options) ->
	Name = milang_ast:transform_data(fun
		({_, #{ local := L, module := M}}) -> unicode:characters_to_binary([M,$.,L]);
		({_, N}) -> N
	end, ComplexName),
	{_, _, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_dot, _, _} | NewTokens] ->
			NewNode = milang_ast:transform_data(fun(_) -> milang_ast_import:new(Name) end, ProtoNode),
			{ok, NewNode, NewTokens};
		_ ->
			lex_error(hd(TokensSansSpace), dot, {declaration_import, {post_name, ProtoNode}})
	end.

expression(Tokens, Options) ->
	expression(start, Tokens, Options).

expression(start, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	[{_, L, _} | _] = TokensSansSpace,
	Node = milang_ast:ast_node(L, Comments, milang_ast_infix_series:new(<<>>, [])),
	expression({head, Node}, TokensSansSpace, Options);

expression({head, ProtoNode}, Tokens, Options) ->
	case expression_non_infix(Tokens, Options) of
		{ok, Node, NewTokens} ->
			NewNode = milang_ast:transform_data(fun(M) -> milang_ast_infix_series:head(Node, M) end, ProtoNode),
			expression({infix_op_or_done, NewNode}, NewTokens, Options);
		Error ->
			Error
	end;

expression({infix_op_or_done, ProtoNode}, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_infix_indicator, L, _} | Tail] ->
			InfixProtoNode = milang_ast:ast_node(L, Comments),
			expression({infix_notation, InfixProtoNode, ProtoNode}, Tail, Options);
		_ ->
			?LOG_DEBUG("gave up at: ~p", [hd(TokensSansSpace)]),
			SimpleNode = milang_ast:transform_data(fun(M) ->
				case milang_ast_infix_series:ops(M) of
					[] ->
						HeadNode = milang_ast_infix_series:head(M),
						milang_ast:data(HeadNode);
					OldOps ->
						NewOps = lists:reverse(OldOps),
						milang_ast_infix_series:ops(NewOps, M)
				end
			end, ProtoNode),
			case milang_ast:type(SimpleNode) of
				{ok, infix_series} ->
					{ok, Tree} = milang_ast_infix:as_tree(SimpleNode),
					{ok, Tree, Tokens};
				_ ->
					{ok, SimpleNode, Tokens}
			end
	end;

expression({infix_notation, InfixProtoNode, ProtoNode}, Tokens, Options) ->
	case infix_op(InfixProtoNode, Tokens, Options) of
		{ok, Node, NewTokens} ->
			NewProtoNode = milang_ast:transform_data(fun(M) ->
				OldOps = milang_ast_infix_series:ops(M),
				NewOps = [Node | OldOps ],
				milang_ast_infix_series:ops(NewOps, M)
			end, ProtoNode),
			expression({infix_op_or_done, NewProtoNode}, NewTokens, Options);
		Error ->
			Error
	end.

infix_op(ProtoNode, Tokens, Options) ->
	infix_op(notation, ProtoNode, Tokens, Options).

infix_op(notation, ProtoNode, Tokens, Options) ->
	case infix_notation(Tokens, Options) of
		{ok, Node, NewTokens} ->
			infix_op({right_side_space, Node}, ProtoNode, NewTokens, Options);
		Error ->
			Error
	end;

infix_op({right_side_space, OpNode}, ProtoNode, Tokens, Options) ->
	case space(Tokens) of
		{true, _, _} ->
			infix_op({right_side, OpNode}, ProtoNode, Tokens, Options);
		_ ->
			lex_error(hd(Tokens), space, {infix_op, {right_side_space, OpNode}})
	end;

infix_op({right_side, OpNode}, ProtoNode, Tokens, Options) ->
	case expression_non_infix(Tokens, Options) of
		{ok, ExprNode, NewTokens} ->
			Body = milang_ast_infix_operation:new(OpNode, ExprNode),
			Node = milang_ast:transform_data(fun(_) -> Body end, ProtoNode),
			{ok, Node, NewTokens};
		Error ->
			Error
	end.

infix_notation(Tokens, Options) ->
	infix_notation(start, Tokens, Options).

infix_notation(start, Tokens, Options) ->
	case Tokens of
		[{symbol_infix_left, _, Iodata} | Rest] ->
			Weight = count_characters(Iodata),
			infix_notation({function, left, Weight}, Rest, Options);
		[{identifier_bound, L, Name} | Rest] ->
			NameNode = milang_ast:ast_node(L, [], {identifier_bound, Name}),
			infix_notation({right_or_done, NameNode}, Rest, Options);
		[T | _] ->
			lex_error(T, [symbol_infix_left, identifier_bound], {infix_notation, start})
	end;

infix_notation({function, left, Weight}, Tokens, Options) ->
	case Tokens of
		[{identifier_bound, L, Data} | Rest] ->
			NameNode = milang_ast:ast_node(L, [], {identifier_bound, Data}),
			infix_notation({finish, NameNode, left, Weight}, Rest, Options);
		_ ->
			lex_error(hd(Tokens), identifier_bound, {infix_notation, {function, left, Weight}})
	end;

infix_notation({right_or_done, Name}, Tokens, Options) ->
	case Tokens of
		[{syntax_infix_right, _, Iodata} | Rest] ->
			Weight = count_characters(Iodata),
			infix_notation({finish, Name, right, Weight}, Rest, Options);
		_ ->
			infix_notation({finish, Name, left, 1}, Tokens, Options)
	end;

infix_notation({finish, Func, Assoc, Weight}, Tokens, _Options) ->
	Node = milang_ast_infix_notation:new(Func, Assoc, Weight),
	{ok, Node, Tokens}.

expression_non_infix(Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	[Head | Rest] = TokensSansSpace,
	case Head of
		{identifier_bound, L, Name} ->
			ProtoNode= milang_ast:ast_node(L, Comments),
			NameNode = milang_ast:ast_node(L, [], {identifier_bound, Name}),
			expression_call(NameNode, ProtoNode, Rest, Options);
		{identifier_type, L, Name} ->
			ProtoNode = milang_ast:ast_node(L, Comments),
			NameNode = milang_ast:ast_node(L, [], milang_ast_identifier:type(Name)),
			expression_call(NameNode, ProtoNode, Rest, Options);
		{syntax_open, _, subexpression} ->
			sub_expression(Tokens, Options);
		{literal_string, L, S} ->
			Node = milang_ast:ast_node(L, Comments, {literal_string, S}),
			{ok, Node, Rest};
		{literal_integer, L, I} ->
			Node = milang_ast:ast_node(L, Comments, {literal_integer, I}),
			{ok, Node, Rest};
		{literal_float, L, F} ->
			Node = milang_ast:ast_node(L, Comments, {literal_float, F}),
			{ok, Node, Rest};
		{syntax_open, L, list} ->
			Node = milang_ast:ast_node(L, Comments),
			expression_literal_list(Node, TokensSansSpace, Options);
		%{syntax_open, _, record} ->
		%	expression_record(Tokens, Options);
		{keyword, L, function} ->
			Node = milang_ast:ast_node(L, Comments),
			expression_function(Node, Rest, Options);
		{keyword, L, match} ->
			Node = milang_ast:ast_node(L, Comments),
			expression_match(Node, Rest, Options);
		_ ->
			lex_error(Head, [expression_function, expression_constructor, expression_call, subexpression, literal], expression_non_infix)
	end.

expression_literal_list(ProtoNode, Tokens, Options) ->
	case generic_list(Tokens, Options, fun expression/2) of
		{ok, Items, NewTokens} ->
			Node = milang_ast:transform_data(fun(_) ->
				{literal_list, Items}
			end, ProtoNode),
			{ok, Node, NewTokens};
		Error ->
			Error
	end.

expression_match(ProtoNode, Tokens, Options) ->
	expression_match(space_after_match_keyword, ProtoNode, Tokens, Options).

expression_match(space_after_match_keyword, ProtoNode, Tokens, Options) ->
	{HasSpace, _Comments, _TokensSansSpace} = space(Tokens),
	case HasSpace of
		true ->
			MaybeExpression = expression(Tokens, Options),
			expression_match({expression_to_match, MaybeExpression}, ProtoNode, Tokens, Options);
		false ->
			lex_error(hd(Tokens), [space], {expression_match, {space_after_match_keyword}})
	end;

expression_match({expression_to_match, {error, _} = Error}, _ProtoNode, _Tokens, _Options) ->
	Error;

expression_match({expression_to_match, {ok, ExpressionNode, Tokens}}, ProtoNode, _TooManyTokens, Options) ->
	case space(Tokens) of
		{false, _, _} ->
			lex_error(hd(Tokens), space, {expression_match, {space_after_expression}});
		{true, Comments, [{keyword, _L, 'with'} | Tail]} ->
			expression_match({space_after_with, ExpressionNode, Comments}, ProtoNode, Tail, Options);
		{true, _, TokensSansSpace} ->
			lex_error(hd(TokensSansSpace), ['with'], {expression_match, {after_expression_before_clauses}})
	end;

expression_match({space_after_with, ExprNode, Comments}, ProtoNode, Tokens, Options) ->
	case space(Tokens) of
		{false, _, _} ->
			lex_error(hd(Tokens), space, {expression_match, {space_after_with}});
		{true, MoreComments, TokensSansSpace} ->
			expression_match({first_clause, [Comments, MoreComments], ExprNode}, ProtoNode, TokensSansSpace, Options)
	end;

expression_match({first_clause, Comments, ExprNode}, ProtoNode, Tokens, Options) ->
	case match_clause(Tokens, Options) of
		{ok, MatchNode, NewTokens} ->
			FullyCommentedMatchNode = milang_ast:pre_doc(Comments, MatchNode),
			expression_match({match_clause_or_done, [FullyCommentedMatchNode], ExprNode}, ProtoNode, NewTokens, Options);
		Error ->
			Error
	end;

expression_match({match_clause_or_done, ClauseAcc, ExprNode}, ProtoNode, Tokens, Options) ->
	{_, _Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_dot, _, _} | Tail] ->
			Node = milang_ast:transform_data(fun(_) ->
				milang_ast_match:new(ExprNode, lists:reverse(ClauseAcc))
			end, ProtoNode),
			{ok, Node, Tail};
		_ ->
			expression_match({match_clause, ClauseAcc, ExprNode}, ProtoNode, Tokens, Options)
	end;

expression_match({match_clause, ClauseAcc, ExprNode}, ProtoNode, Tokens, Options) ->
	case match_clause(Tokens, Options) of
		{ok, MatchNode, NewTokens} ->
			NewAcc = [MatchNode | ClauseAcc],
			expression_match({match_clause_or_done, NewAcc, ExprNode}, ProtoNode, NewTokens, Options);
		Error ->
			Error
	end.

match_clause(Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	{_, L, _} = hd(TokensSansSpace),
	ProtoNode = milang_ast:ast_node(L, Comments),
	match_clause(match_head, ProtoNode, TokensSansSpace, Options).

match_clause(match_head, ProtoNode, Tokens, Options) ->
	case match_head(Tokens, Options) of
		{ok, Head, NewTokens} ->
			match_clause({match_body, [], Head}, ProtoNode, NewTokens, Options);
		Error ->
			Error
	end;

match_clause({match_body, BindAcc, Head}, ProtoNode, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{keyword, L, 'let'} | Tail] ->
			LetProto = milang_ast:ast_node(L, Comments),
			case declaration_let(LetProto, Tail, Options) of
				{ok, Node, NewTokens} ->
					match_clause({match_body, [Node | BindAcc], Head}, ProtoNode, NewTokens, Options);
				Error ->
					Error
			end;
		_ ->
			match_clause({match_expression, BindAcc, Head}, ProtoNode, Tokens, Options)
	end;


match_clause({match_expression, Binds, Head}, ProtoNode, Tokens, Options) ->
	case expression(Tokens, Options) of
		{ok, ExprNode, NewTokens} ->
			match_clause({dot, Head, Binds, ExprNode}, ProtoNode, NewTokens, Options);
		Error ->
			Error
	end;

match_clause({dot, Head, Binds, ExprNode}, ProtoNode, Tokens, _Options) ->
	{_, _, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_dot, _, _} | Tail] ->
			Node = milang_ast:transform_data(fun(_) ->
				milang_ast_match_clause:new(Head, lists:reverse(Binds), ExprNode)
			end, ProtoNode),
			{ok, Node, Tail};
		[H | _] ->
			lex_error(H, syntax_dot, {match_clause, {dot, Head, ExprNode}})
	end.

match_head(Tokens, Options) ->
	{_, Comments, [{_, L, _} | _] = TokensSansSpace} = space(Tokens),
	ProtoNode = milang_ast:ast_node(L, Comments),
	match_head(start, ProtoNode, TokensSansSpace, Options).

match_head(start, ProtoNode, Tokens, Options) ->
	Done = fun syntax_implies/4,
	match_head({part, Done}, ProtoNode, Tokens, Options);

match_head({part, Finish}, ProtoNode, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{identifier_ignored, _, Name} | Tail ] ->
			Node = milang_ast:transform_data(fun(_) ->
				{identifier_ignored, Name}
			end, ProtoNode),
			match_head(Finish, Node, Tail, Options);
		[{identifier_bound, _, Name} | Tail] ->
			Node = milang_ast:transform_data(fun(_) ->
				{identifier_bound, Name}
			end, ProtoNode),
			match_head(Finish, Node, Tail, Options);
		[{literal_string, _, S} | Tail] ->
			Node = milang_ast:transform_data(fun(_) ->
				{literal_string, S}
			end, ProtoNode),
			match_head(Finish, Node, Tail, Options);
		[{literal_integer, _, N} | Tail] ->
			Node = milang_ast:transform_data(fun(_) ->
				{literal_integer, N}
			end, ProtoNode),
			match_head(Finish, Node, Tail, Options);
		[{literal_float, _, N} | Tail] ->
			Node = milang_ast:transform_data(fun(_) ->
				{literal_float, N}
			end, ProtoNode),
			match_head(Finish, Node, Tail, Options);
		[{identifier_type, L, _Name} | _Tail] ->
			MatchTypeProtoNode = milang_ast:ast_node(L, Comments),
			case match_type(MatchTypeProtoNode, TokensSansSpace, Options) of
				{ok, Node, NewTokens} ->
					match_head(Finish, Node, NewTokens, Options);
				Error ->
					Error
			end;
		[{syntax_open, L, list} | Tail] ->
			MatchListProtoNode = milang_ast:ast_node(L, Comments),
			case match_list(MatchListProtoNode, Tail, Options) of
				{ok, NewData, NewTokens} ->
					Node = milang_ast:transform_data(fun(_) ->
						NewData
					end, ProtoNode),
					match_head(Finish, Node, NewTokens, Options);
				Error ->
					Error
			end;
		[{syntax_open, L, subexpression} | Tail] ->
			MatchSubProtoNode = milang_ast:ast_node(L, Comments),
			case match_sub(MatchSubProtoNode, Tail, Options) of
				{ok, Node, NewTokens} ->
					match_head(Finish, Node, NewTokens, Options);
				Error ->
					Error
			end;
		[E | _] ->
			lex_error(E, [identifier_bound, identifier_type, identifier_ignored, literal_float, literal_string, literal_integer, syntax_open_list], {match_head, {start, ProtoNode}})
	end;

match_head(Finish, Node, Tokens, Options) ->
	{_, _Comments, TokensSansSpace} = space(Tokens),
	Finish(TokensSansSpace, match_head, Node, Options).

match_type(ProtoNode, Tokens, Options) ->
	[{identifier_type, L, Name} | Tail] = Tokens,
	NameNode = milang_ast:ast_node(L, [], milang_ast_identifier:type(Name)),
	match_type({gather_args, [], NameNode}, ProtoNode, Tail, Options).

match_type({gather_args, ArgAcc, TypeName}, ProtoNode, Tokens, Options) ->
	case space(Tokens) of
		% don't know if we really are done or not for these 3, but not worth
		% exploding over.
		{_, _, [{syntax_implies, _, _} | _] = TokensSansSpace} ->
			match_type({finish, ArgAcc, TypeName}, ProtoNode, TokensSansSpace, Options);
		{_, _, [{syntax_close, _, list} | _] = TokensSansSpace} ->
			match_type({finish, ArgAcc, TypeName}, ProtoNode, TokensSansSpace, Options);
		{_, _, [{syntax_close, _, subexpression} | _] = TokensSansSpace} ->
			match_type({finish, ArgAcc, TypeName}, ProtoNode, TokensSansSpace, Options);
		{false, _, _} ->
			lex_error(hd(Tokens), space, {match_type, {gather_args, ArgAcc, TypeName, ProtoNode}});
		{_, Comments, [{identifier_ignored, L, Name} | Tail ]} ->
			Node = milang_ast:ast_node(L, Comments, {identifier_ignored, Name}),
			NewAcc = [ Node | ArgAcc],
			match_type({gather_args, NewAcc, TypeName}, ProtoNode, Tail, Options);
		{_, Comments, [{identifier_bound, L, Name} | Tail]} ->
			Node = milang_ast:ast_node(L, Comments, {identifier_bound, Name}),
			NewAcc = [ Node | ArgAcc],
			match_type({gather_args, NewAcc, TypeName}, ProtoNode, Tail, Options);
		{_, Comments, [{literal_string, L, S} | Tail]} ->
			Node = milang_ast:ast_node(L, Comments, {literal_string, S}),
			NewAcc = [ Node | ArgAcc],
			match_type({gather_args, NewAcc, TypeName}, ProtoNode, Tail, Options);
		{_, Comments, [{literal_integer, L, N} | Tail]} ->
			Node = milang_ast:ast_node(L, Comments, {literal_integer, N}),
			NewAcc = [ Node | ArgAcc],
			match_type({gather_args, NewAcc, TypeName}, ProtoNode, Tail, Options);
		{_, Comments, [{literal_float, L, N} | Tail]} ->
			Node = milang_ast:ast_node(L, Comments, {literal_float, N}),
			NewAcc = [ Node | ArgAcc],
			match_type({gather_args, NewAcc, TypeName}, ProtoNode, Tail, Options);
		{_, Comments, [{identifier_type, L, Name} | Tail]} ->
			SubProto = milang_ast:ast_node(L, Comments),
			NameNode = milang_ast:ast_node(L, [], milang_ast_identifier:type(Name)),
			{ok, Node, Tail} = match_type({finish, [], NameNode}, SubProto, Tail, Options),
			NewAcc = [ Node | ArgAcc],
			match_type({gather_args, NewAcc, TypeName}, ProtoNode, Tail, Options);
		{_, Comments, [{syntax_open, L, list} | Tail]} ->
			MatchListProtoNode = milang_ast:ast_node(L, Comments),
			case match_list(MatchListProtoNode, Tail, Options) of
				{ok, Node, NewTokens} ->
					NewAcc = [ Node | ArgAcc],
					match_type({gather_args, NewAcc, TypeName}, ProtoNode, NewTokens, Options);
				Error ->
					Error
			end;
		{_, Comments, [{syntax_open, L, subexpression} | Tail]} ->
			MatchSubProtoNode = milang_ast:ast_node(L, Comments),
			case match_sub(MatchSubProtoNode, Tail, Options) of
				{ok, Node, NewTokens} ->
					NewAcc = [Node | ArgAcc],
					match_type({gather_args, NewAcc, TypeName}, ProtoNode, NewTokens, Options);
				Error ->
					Error
			end;
		{_, _, [E | _]} ->
			lex_error(E, [identifier_bound, identifier_type, identifier_ignored, literal_float, literal_string, literal_integer, syntax_open_list], {match_head, {start, ProtoNode}})
	end;

match_type({finish, Args, Name}, ProtoNode, Tokens, _Options) ->
	Node = milang_ast:transform_data(fun(_) ->
		{match_type, Name, lists:reverse(Args)}
	end, ProtoNode),
	{ok, Node, Tokens}.

match_sub(ProtoNode, Tokens, Options) ->
	match_head({part, fun syntax_close_subexpression/4}, ProtoNode, Tokens, Options).

match_list(ProtoNode, Tokens, Options) ->
	match_list({maybe_next_element, []}, ProtoNode, Tokens, Options).

match_list({maybe_next_element, Acc}, ProtoNode, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_element_seperator, L, _} | Tail] ->
			match_list({next_element, L, Comments, Acc}, ProtoNode, Tail, Options);
		[{syntax_cons, _, _} | Tail] ->
			match_list({tail_binding, Comments, Acc}, ProtoNode, Tail, Options);
		[{syntax_close, _, list} | _Tail] ->
			match_list({finish, undefined, Acc}, ProtoNode, TokensSansSpace, Options);
		_ ->
			lex_error(hd(TokensSansSpace), [syntax_element_seperator, syntax_close_list, syntax_cons], {match_list, {maybe_next_element, Acc, ProtoNode}})
	end;

match_list({next_element, L, Comments, Acc}, ProtoNode, Tokens, Options) ->
	Finish = fun list_match_element/4,
	case match_head({part, Finish}, milang_ast:ast_node(L, Comments), Tokens, Options) of
		{ok, Node, NewTokens} ->
			match_list({maybe_next_element, [ Node | Acc]}, ProtoNode, NewTokens, Options);
		Error ->
			Error
	end;

match_list({tail_binding, Comments, Acc}, ProtoNode, Tokens, Options) ->
	case space(Tokens) of
		{false, _, _} ->
			lex_error(hd(Tokens), space, {match_list, {tail_binding, Comments, Acc, ProtoNode}});
		{true, MoreComments, [{identifier_bound, L, Name} | Tail]} ->
			Node = milang_ast:ast_node(L, [Comments, MoreComments], {identifier_bound, Name}),
			match_list({finish, Node, Acc}, ProtoNode, Tail, Options);
		{true, MoreComments, [{identifier_ignored, L, Name} | Tail]} ->
			Node = milang_ast:ast_node(L, [Comments, MoreComments], {identifier_ignored, Name}),
			match_list({finish, Node, Acc}, ProtoNode, Tail, Options);
		{_, _, TokensSansSpace} ->
			lex_error(hd(TokensSansSpace), [identifier_bound, identifier_ignored], {match_list, {tail_binding, Comments, Acc, ProtoNode}})
	end;

match_list({finish, TailBinding, Heads} = State, ProtoNode, Tokens, _Options) ->
	{_, _Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_close, _, list} | Tail] ->
			NewData = case TailBinding of
				undefined ->
					{match_list, lists:reverse(Heads)};
				_ ->
					{match_list_head, lists:reverse(Heads), TailBinding}
			end,
			Node = milang_ast:transform_data(fun(_) ->
				NewData
			end, ProtoNode),
			{ok, Node, Tail};
		[H | _] ->
			lex_error(H, syntax_close_list, {match_list, State })
	end.

syntax_implies(Tokens, Context, Node, _Options) ->
	{_, _Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_implies, _, _} | Tail] ->
			{ok, Node, Tail};
		[H | _] ->
			lex_error(H, syntax_implies, {Context, {syntax_implies, Node}})
	end.

syntax_close_subexpression(Tokens, Context, Node, _Options) ->
	{_, _Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_close, _, subexpression} | Tail] ->
			{ok, Node, Tail};
		[H | _] ->
			lex_error(H, syntax_close_subexpression, {Context, {syntax_close_subexpression, Node}})
	end.

list_match_element(Tokens, Context, Node, _Options) ->
	{_, _Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_element_seperator, _, _} | _Tail] ->
			{ok, Node, Tokens};
		[{syntax_cons, _, _} | _] ->
			{ok, Node, Tokens};
		[{syntax_close, _, list} | _] ->
			{ok, Node, Tokens};
		[H | _] ->
			lex_error(H, [syntax_element_seperator, syntax_conse, syntax_close_list], {Context, {list_match_element, Node}})
	end.


expression_function(ProtoNode, Tokens, Options) ->
	expression_function({arg_or_done, []}, ProtoNode, Tokens, Options).

expression_function({arg_or_done, Args}, ProtoNode, Tokens, Options) ->
	{HasSpace, Comments, TokensSansSpace} = space(Tokens),
	case {HasSpace, TokensSansSpace} of
		{true, [{identifier_bound, L, Data} | Rest]} ->
			VarNode = milang_ast:ast_node(L, Comments, milang_ast_identifier:bound(Data)),
			expression_function({arg_or_done, [ VarNode | Args ]}, ProtoNode, Rest, Options);
		{true, [{identifier_ignored, L, Data} | Rest]} ->
			VarNode = milang_ast:ast_node(L, Comments, milang_ast_identifier:ignored(Data)),
			expression_function({arg_or_done, [VarNode | Args]}, ProtoNode, Rest, Options);
		{_, [{syntax_implies, _, _} | Rest]} ->
			expression_function({binds_or_expression, [], lists:reverse(Args)}, ProtoNode, Rest, Options);
		{false, _} ->
			lex_error(hd(Tokens), space, {expression_function, args_list});
		_ ->
			lex_error(hd(TokensSansSpace), [space, identifier_bound, identifier_ignored, syntax_implies], {expression_function, args_list})
	end;

expression_function({binds_or_expression, BindsAcc, Args}, ProtoNode, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{keyword, L, 'let'} | Rest] ->
			LetNode = milang_ast:ast_node(L, Comments),
			expression_function({bind, LetNode, BindsAcc, Args}, ProtoNode, Rest, Options);
		_ ->
			expression_function({expression, Comments, lists:reverse(BindsAcc), Args}, ProtoNode, Tokens, Options)
	end;

expression_function({bind, LetNode, BindsAcc, Args}, ProtoNode, Tokens, Options) ->
	case declaration_let(LetNode, Tokens, Options) of
		{ok, Node, NewTokens} ->
			expression_function({binds_or_expression, [ Node | BindsAcc], Args}, ProtoNode, NewTokens, Options);
		Error ->
			Error
	end;

expression_function({expression, Comments, Binds, Args}, ProtoNode, Tokens, Options) ->
	case expression(Tokens, Options) of
		{ok, ExprNode, NewTokens} ->
			FullExprNode = milang_ast:pre_doc(Comments, ExprNode),
			expression_function({dot, FullExprNode, Binds, Args}, ProtoNode, NewTokens, Options);
		Error ->
			Error
	end;

expression_function({dot, Expr, Binds, Args}, ProtoNode, Tokens, _Options) ->
	{_, _, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_dot, _, _} | Tail] ->
			Node = milang_ast:transform_data(fun(_) ->
				milang_ast_function:new(Args, Binds, Expr)
			end, ProtoNode),
			{ok, Node, Tail};
		_ ->
			lex_error(hd(TokensSansSpace), [dot], {expression_function, {dot, Expr, Binds, Args}})
	end.

expression_call(Name, ProtoNode, Tokens, Options) ->
	expression_call({space_or_done, Name, [], ProtoNode}, Tokens, Options).

expression_call({space_or_done, Name, Args, ProtoNode}, Tokens, Options) ->
	case space(Tokens) of
		{_, _, [{syntax_infix_indicator, _, _} | _]} ->
			expression_call({finish, Name, Args, ProtoNode}, Tokens, Options);
		{_, _, [{keyword, _, _} | _]} ->
			expression_call({finish, Name, Args, ProtoNode}, Tokens, Options);
		{false, _, TokensSansSpace} ->
			?LOG_DEBUG("guess we're done: ~p", [hd(TokensSansSpace)]),
			expression_call({finish, Name, Args, ProtoNode}, Tokens, Options);
		{true, Comments, TokensSansSpace} ->
			expression_call({arg, Comments, Name, Args, ProtoNode}, TokensSansSpace, Options)
	end;

expression_call({finish, Name, ReversedArgs, ProtoNode}, Tokens, _Options) ->
	ArgsProper = lists:reverse(ReversedArgs),
	Node = milang_ast:transform_data(fun(_) -> milang_ast_call:new(Name, ArgsProper) end, ProtoNode),
	{ok, Node, Tokens};

expression_call({arg, Comments, Name, Args, ProtoNode}, Tokens, Options) ->
	case Tokens of
		[{literal_float, L, F} | Tail] ->
			Arg = milang_ast:ast_node(L, Comments, {literal_float, F}),
			expression_call({space_or_done, Name, [ Arg | Args], ProtoNode}, Tail, Options);
		[{literal_integer, L, I} | Tail] ->
			Arg = milang_ast:ast_node(L, Comments, {literal_integer, I}),
			expression_call({space_or_done, Name, [ Arg | Args ], ProtoNode}, Tail, Options);
		[{literal_string, L, S} | Tail] ->
			Arg = milang_ast:ast_node(L, Comments, {literal_string, S}),
			expression_call({space_or_done, Name, [Arg | Args], ProtoNode}, Tail, Options);
		[{syntax_open, L, list} | _] ->
			ListNode = milang_ast:ast_node(L, Comments),
			expression_call({list, ListNode, Name, Args, ProtoNode}, Tokens, Options);
		[{identifier_bound, L, Data} | Tail] ->
			Node = milang_ast:ast_node(L, Comments, milang_ast_identifier:bound(Data)),
			NewArgs = [ Node | Args],
			expression_call({space_or_done, Name, NewArgs, ProtoNode}, Tail, Options);
		[{identifier_type, L, Data} | Tail] ->
			NameNode = milang_ast:ast_node(L, [], milang_ast_identifier:type(Data)),
			Node = milang_ast:ast_node(L, Comments, milang_ast_call:new(NameNode, [])),
			NewArgs = [ Node | Args ],
			expression_call({space_or_done, Name, NewArgs, ProtoNode}, Tail, Options);
		[{syntax_open, _, subexpression} | _] ->
			case sub_expression(Comments ++ Tokens, Options) of
				{ok, Node, NewTokens} ->
					NewArgs = [ Node | Args ],
					expression_call({space_or_done, Name, NewArgs, ProtoNode}, NewTokens, Options);
				Error ->
					Error
			end;
		_ ->
			ArgsProper = lists:reverse(Args),
			Body = milang_ast_call:new(Name, ArgsProper),
			Node = milang_ast:transform_data(fun(_) -> Body end, ProtoNode),
			{ok, Node, Tokens}
	end;

expression_call({list, ListProtoNode, Name, Args, ProtoNode}, Tokens, Options) ->
	case generic_list(Tokens, Options, fun expression/2) of
		{ok, ListItems, NewTokens} ->
			ListNode = milang_ast:transform_data(fun(_) ->
				{literal_list, ListItems}
			end, ListProtoNode),
			expression_call({space_or_done, Name, [ ListNode | Args], ProtoNode}, NewTokens, Options);
		Error ->
			Error
	end.

sub_expression(Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_open, _, subexpression} | Tail] ->
			sub_expression({sub, Comments}, Tail, Options);
		[T | _] ->
			lex_error(T, syntax_open_subexpression, subexpression)
	end.

sub_expression({sub, Comments}, Tokens, Options) ->
	case expression(Comments ++ Tokens, Options) of
		{ok, Node, NewTokens} ->
			sub_expression({finish, Node}, NewTokens, Options);
		Error ->
			Error
	end;

sub_expression({finish, Node}, Tokens, _Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_close, _, subexpression} | Tail] ->
			{ok, Node, Comments ++ Tail};
		[T | _] ->
			lex_error(T, syntax_close_subexpression, {subexpression, {finish, Node}})
	end.

count_characters(Iodata) ->
	count_characters(unicode:characters_to_binary(Iodata), 0).

count_characters(<<>>, N) ->
	N;
count_characters(<<_/utf8, Rest/binary>>, N) ->
	count_characters(Rest, N + 1).

lex_error(Got, Expected, Context) ->
	{error, #{ got => Got, expected => Expected, context => Context}}.

generic_list(Tokens, Options, ElementFun) ->
	generic_list(need_opener, Tokens, Options, ElementFun, []).

generic_list(need_opener, [{syntax_open, _, list} | Tokens], Options, ElementFun, Acc) ->
	generic_list(start_new_element, Tokens, Options, ElementFun, Acc);

generic_list(start_new_element, Tokens, Options, ElementFun, Acc) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_close, _, list} | Rest] ->
			{ok, lists:reverse(Acc), Comments ++ Rest};
		[{syntax_element_seperator, _, _} | Rest] ->
			generic_list(element, Comments ++ Rest, Options, ElementFun, Acc);
		[Wut | _] ->
			lex_error(Wut, ["]", ","], {generic_list, start_new_element})
	end;

generic_list(element, Tokens, Options, ElementFun, Acc) ->
	case ElementFun(Tokens, Options) of
		{ok, Element, NewTokens} ->
			generic_list(start_new_element, NewTokens, Options, ElementFun, [Element | Acc]);
		Error ->
			Error
	end;

generic_list(State, Tokens, _, _, _) ->
	Head = hd(Tokens),
	lex_error(Head, ["["], {generic_list, State}).



space(Tokens) ->
	space(Tokens, false, []).

space([{whitespace, _, _} | Tokens], _, Acc) ->
	space(Tokens, true, Acc);
space([{comment, _, _} = Token | Tokens], _, Acc) ->
	space(Tokens, true, [Token | Acc]);
space(Tokens, GotSpace, Comments) ->
	{GotSpace, lists:reverse(Comments), Tokens}.
