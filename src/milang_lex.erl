-module(milang_lex).

-include("milang_ast.hrl").
-include("milang_log.hrl").

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

-spec iolist_to_atom(unicode:chardata()) -> atom().
iolist_to_atom(Iolist) ->
	binary_to_atom(unicode:characters_to_binary(Iolist), utf8).

-spec root([ milang_p_token:token() ], options()) -> {error, term()} | {ok, [ milang_ast:ast_node()]}.
root(Tokens, Options) ->
	root(consume_whitespace, Tokens, Options, []).

root(_, [], _Options, Acc) ->
	{ok, lists:reverse(Acc)};
root(S, [{eof, _, _}], Options, Acc) ->
	root(S, [], Options, Acc);
root(consume_whitespace, Tokens, Options, Acc) ->
	{_, Comments, NewTokens} = space(Tokens),
	root({expect_declaration, Comments}, NewTokens, Options, Acc);
root({expect_declaration, Comments}, [{declaration_module, L, _} | Tokens], Options, Acc) ->
	case declaration_module(L, Comments, Options, Tokens) of
		{ok, Declaration, NewTokens} ->
			root(consume_whitespace, NewTokens, Options, [Declaration | Acc]);
		Error ->
			Error
	end;
root({expect_declaration, Comments}, [{declaration_import, _, _} | _] = Tokens, Options, Acc) ->
	case declaration_import(Comments, Options, Tokens) of
		{ok, Declaration, NewTokens} ->
			root(consume_whitespace, NewTokens, Options, [Declaration | Acc]);
		Error ->
			Error
	end;
root({expect_declaration, Comments}, [{declaration_alias, L, _} | Tokens], Options, Acc) ->
	ProtoNode = milang_ast:ast_node(L, Comments, undefined),
	case declaration_alias(ProtoNode, Tokens, Options) of
		{ok, Declaration, NewTokens} ->
			root(consume_whitespace, NewTokens, Options, [Declaration | Acc]);
		Error ->
			Error
	end;
root({expect_declaration, Comments}, [{declaration_type, _, _} = T | Tokens], Options, Acc) ->
	case declaration_type(Comments ++ [T] ++ Tokens, Options) of
		{ok, Declaration, NewTokens} ->
			root(consume_whitespace, NewTokens, Options, [Declaration | Acc]);
		Error ->
			Error
	end;
root({expect_declaration, _Comments}, [{declaration_class, _, _} | _], _Options, _Acc) ->
	{error, {nyi, declaration_class}};
root({expect_declaration, Comments}, Tokens, Options, Acc) ->
	case declaration_spec_or_function(Comments, Tokens, Options) of
		{ok, Declaration, NewTokens} ->
			root(consume_whitespace, NewTokens, Options, [Declaration | Acc]);
		Error ->
			Error
	end.

declaration_alias(#milang_ast{} = ProtoNode, Tokens, Options) ->
	case space(Tokens) of
		{false, _, _} ->
			lex_error(hd(Tokens), space, {declaration_alias, {space_before_name, ProtoNode}});
		{true, Comments, TokensSansSpace} ->
			NewNode = milang_ast:add_doc(Comments, ProtoNode),
			declaration_alias({name, NewNode}, TokensSansSpace, Options)
	end;

declaration_alias({name, ProtoNode}, Tokens, #{ mode := Mode} = Options) ->
	case name_type(Tokens, Options) of
		{ok, {name_upcase, NameShort}, _NewTokens} when is_map(NameShort), Mode =:= module ->
			lex_error(hd(Tokens), [local_type_name], {declaration_alias, {name, ProtoNode}});
		{ok, {name_upcase, _} = Name, NewTokens} ->
			NewNode = milang_ast:transform_data(fun(_) -> #declaration_alias{ name = Name } end, ProtoNode),
			declaration_alias({args_when_or_equal, NewNode}, NewTokens, Options);
		Error ->
			Error
	end;

declaration_alias({args_when_or_equal, ProtoNode}, Tokens, Options) ->
	{HasSpace, Comments, TokensSansSpace} = space(Tokens),
	case {HasSpace, TokensSansSpace} of
		{true, [{syntax_keyword, _, 'when'} | Tail]} ->
			declaration_alias({constraints, ProtoNode}, Comments ++ Tail, Options);
		{false, [{syntax_keyword, _, 'when'} | _]} ->
			lex_error(hd(Tokens), space_before_when, {declaration_alias, {when_or_equal, ProtoNode}});
		{true, [{name_downcase, _, _} | _]} ->
			declaration_alias({args, [], ProtoNode}, Tokens, Options);
		{_, [{syntax_bind, _, _} | Tail]} ->
			declaration_alias({original, ProtoNode}, Comments ++ Tail, Options)
	end;

declaration_alias({args, ArgAcc, ProtoNode}, Tokens, Options) ->
	{GotSpace, Comments, TokensSansSpace} = space(Tokens),
	case {GotSpace, TokensSansSpace} of
		{true, [{name_downcase, L, Iodata} | Tail]} ->
			NameNode = milang_ast:type_variable(milang_ast:ast_node(L, Comments), {name_downcase, iolist_to_atom(Iodata)}),
			NewAcc = [NameNode | ArgAcc],
			declaration_alias({args, NewAcc, ProtoNode}, Tail, Options);
		{false, [{name_downcase, _, _} | _]} ->
			lex_error(hd(Tokens), space, {declaration_alias, {args, ArgAcc, ProtoNode}});
		_ ->
			Args = lists:reverse(ArgAcc),
			NewNode = milang_ast:transform_data(fun(D) -> D#declaration_alias{ args = Args } end, ProtoNode),
			declaration_alias({when_or_equal, NewNode}, Tokens, Options)
	end;

declaration_alias({when_or_equal, ProtoNode}, Tokens, Options) ->
	{GotSpace, Comments, TokensSansSpace} = space(Tokens),
	case {GotSpace, TokensSansSpace} of
		{true, [{syntax_keyword, _, 'when'} | Tail]} ->
			declaration_alias({constraints, ProtoNode}, Comments ++ Tail, Options);
		{false, [{syntax_keyword, _, 'when'} | _]} ->
			lex_error(hd(Tokens), space_before_when, {declaration_alias, {when_or_equal, ProtoNode}});
		{_, [{syntax_bind, _, _} | Tail]} ->
			declaration_alias({original, ProtoNode}, Comments ++ Tail, Options);
		_ ->
			lex_error(hd(TokensSansSpace), [syntax_when, syntax_bind], {declaration_alias, {when_or_equal, ProtoNode}})
	end;

declaration_alias({constraints, ProtoNode}, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_open, _, list} | Tail] = Head ->
			declaration_alias({constraints_do, ProtoNode}, [Head] ++ Comments ++ Tail, Options);
		[Head | _] ->
			lex_error(Head, {syntax_open, list}, {declaration_alias, {constraints, ProtoNode}})
	end;

declaration_alias({constraints_do, ProtoNode}, Tokens, Options) ->
	case generic_list(Tokens, Options, fun constraint/2) of
		{ok, Constraints, NewTokens} ->
			NewNode = milang_ast:transform_data(fun(D) -> D#declaration_alias{ constraints = Constraints} end, ProtoNode),
			declaration_alias({original, NewNode}, NewTokens, Options);
		Error ->
			Error
	end;

declaration_alias({original, ProtoNode}, Tokens, Options) ->
	case type_function(Tokens, Options) of
		{ok, Node, NewTokens} ->
			NewNode = milang_ast:transform_data(fun(D) -> D#declaration_alias{ alias_of = Node } end, ProtoNode),
			declaration_alias({finish, NewNode}, NewTokens, Options);
		Error ->
			Error
	end;

declaration_alias({finish, Node}, Tokens, _Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_dot, _, _} | Tail] ->
			{ok, Node, Comments ++ Tail};
		_ ->
			lex_error(hd(TokensSansSpace), syntax_dot, {declaration_alias, {finsih, Node}})
	end.

declaration_type(Tokens, Options) ->
	declaration_type(start, Tokens, Options).

declaration_type(start, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	[{_, L, _} | Tail] = TokensSansSpace,
	ProtoNode = milang_ast:ast_node(L, Comments, #declaration_type{ args = [], constraints = [], constructors = [] }),
	declaration_type({space_before_name, ProtoNode}, Tail, Options);

declaration_type({space_before_name, ProtoNode}, Tokens, Options) ->
	case space(Tokens) of
		{false, _, _} ->
			lex_error(hd(Tokens), space, {declaration_type, {space_before_name, ProtoNode}});
		{true, Comments, TokensSansSpace} ->
			NewNode = milang_ast:add_doc(Comments, ProtoNode),
			declaration_type({name, NewNode}, TokensSansSpace, Options)
	end;

declaration_type({name, ProtoNode}, Tokens, Options) ->
	case name_type(Tokens, Options) of
		{ok, NameNode, NewTokens} ->
			NewNode = milang_ast:transform_data(fun(M) -> M#declaration_type{ name = NameNode } end, ProtoNode),
			declaration_type({got_name, NewNode}, NewTokens, Options);
		Error ->
			Error
	end;

declaration_type({got_name, ProtoNode}, Tokens, Options) ->
	{HasSpace, Comments, TokensSansSpace} = space(Tokens),
	case {HasSpace, TokensSansSpace} of
		{true, [{syntax_keyword, _, 'when'} | Tail]} ->
			NewNode = milang_ast:add_doc(Comments, ProtoNode),
			declaration_type({constraints_start, NewNode}, Tail, Options);
		{_, [{syntax_keyword, _, 'when'} = T| _]} ->
			lex_error(T, space, {declaration_type, {done_constructors_constraints_space, ProtoNode}});
		{_, [{syntax_dot, _, _} | Tail]} ->
			{ok, ProtoNode, Comments ++ Tail};
		{_, [{syntax_open, _, list} | _]} ->
			declaration_type({constructors, milang_ast:add_doc(Comments, ProtoNode)}, TokensSansSpace, Options);
		{true, [{name_downcase, L, Iodata} | Tail]} ->
			ArgNodeProto = milang_ast:ast_node(L, Comments),
			ArgNode = milang_ast:type_variable(ArgNodeProto, {name_downcase, iolist_to_atom(Iodata)}),
			NewNode = milang_ast:transform_data(fun(M) ->
				M#declaration_type{ args = M#declaration_type.args ++ [ArgNode] }
			end, ProtoNode),
			declaration_type({got_name, NewNode}, Tail, Options);
		{_, [T | _]} ->
			lex_error(T, [constructors, constratints, dot], {got_name, ProtoNode})
	end;

declaration_type({constraints_start, ProtoNode}, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{synax_open, _, list} = Head | Tail ] ->
			declaration_type({constraints, ProtoNode}, [Head] ++ Comments ++ Tail, Options);
		[T | _] ->
			lex_error(T, [list_start], {constraints_start, ProtoNode})
	end;

declaration_type({constraints, ProtoNode}, Tokens, Options) ->
	case generic_list(Tokens, Options, fun constraint/2) of
		{ok, List, NewTokens} ->
			NewNode = milang_ast:transform_data(fun(M) -> M#declaration_type{ constraints = List } end, ProtoNode),
			declaration_type({constructors_or_done, NewNode}, NewTokens, Options);
		Error ->
			Error
	end;

declaration_type({constructors_or_done, ProtoNode}, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_dot, _, _} | Tail] ->
			{ok, ProtoNode, Comments ++ Tail};
		[{syntax_open, _, list} = Head | Tail] ->
			declaration_type({constructors, ProtoNode}, [Head] ++ Comments ++ Tail, Options);
		[T | _] ->
			lex_error(T, [syntax_dot, constructor_list], {declaration_type, {constructors_or_done, ProtoNode}})
	end;

declaration_type({constructors, ProtoNode}, Tokens, Options) ->
	case generic_list(Tokens, Options, fun declaration_constructor/2) of
		{ok, List, NewTokens} ->
			NewNode = milang_ast:transform_data(fun(M) -> M#declaration_type{ constructors = List } end, ProtoNode),
			declaration_type({dot, NewNode}, NewTokens, Options);
		Error ->
			Error
	end;

declaration_type({dot, ProtoNode}, Tokens, _Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_dot, _, _} | Tail] ->
			{ok, ProtoNode, Comments ++ Tail};
		[T | _] ->
			lex_error(T, dot, {dot, ProtoNode})
	end.

constraint(Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	[{_, L, _} | _] = TokensSansSpace,
	ProtoNode = milang_ast:ast_node(L, Comments, #constraint{}),
	constraint({need_name, ProtoNode}, TokensSansSpace, Options).

constraint({need_name, ProtoNode}, Tokens, Options) ->
	case Tokens of
		[{name_downcase, _, Iodata} | Tail] ->
			NewNode = milang_ast:transform_data(fun(D) -> D#constraint{ name = iolist_to_atom(Iodata) } end, ProtoNode),
			constraint({need_assign, NewNode}, Tail, Options);
		[T | _] ->
			lex_error(T, [name_downcase], {constraint, {need_name, ProtoNode}})
	end;

constraint({need_assign, ProtoNode}, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_bind, _, _} | Tail] ->
			constraint({need_type, ProtoNode}, Comments ++ Tail, Options);
		[T | _] ->
			lex_error(T, [syntax_bind], {constraint, {need_assign, ProtoNode}})
	end;

constraint({need_type, ProtoNode}, Tokens, Options) ->
	case type_concrete(Tokens, Options) of
		{ok, Node, NewTokens} ->
			NewNode = milang_ast:transform_data(fun(D) -> D#constraint{ constraint = Node } end, ProtoNode),
			{ok, NewNode, NewTokens};
		Error ->
			Error
	end.

declaration_constructor(Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{name_upcase, L, _} | _] ->
			ProtoNode = milang_ast:ast_node(L, Comments, #constructor{ args = []}),
			declaration_constructor({start, ProtoNode}, TokensSansSpace, Options);
		[T | _] ->
			lex_error(T, name_upcase, {declaration_constructor, start})
	end.

declaration_constructor({start, ProtoNode}, Tokens, Options) ->
	case name_type(Tokens, Options) of
		{ok, NameNode, NewTokens} ->
			NewNode = milang_ast:transform_data(fun(M) -> M#constructor{ name = NameNode } end, ProtoNode),
			declaration_constructor({arg_or_done, [], NewNode}, NewTokens, Options);
		Error ->
			Error
	end;

declaration_constructor({arg_or_done, ArgAcc, ProtoNode}, Tokens, Options) ->
	{Space, Comments, TokensSansSpace} = space(Tokens),
	case {Space, TokensSansSpace} of
		{true, [{name_downcase, L, Iodata} | Tail]} ->
			ArgNodeProto = milang_ast:ast_node(L, Comments),
			ArgNode = milang_ast:type_variable(ArgNodeProto, {name_downcase, iolist_to_atom(Iodata)}),
			declaration_constructor({arg_or_done, [ArgNode | ArgAcc], ProtoNode}, Tail, Options);
		{true, [{name_upcase, _, _} | _]} ->
			declaration_constructor({type_arg, ArgAcc, ProtoNode}, Tokens, Options);
		{_, [{syntax_open, _, subexpression} | _]} ->
			declaration_constructor({subexpression, ArgAcc, ProtoNode}, Tokens, Options);
		_ ->
			Args = lists:reverse(ArgAcc),
			Node = milang_ast:transform_data(fun(M) -> M#constructor{ args = Args} end, ProtoNode),
			{ok, Node, Tokens}
	end;

declaration_constructor({type_arg, ArgAcc, ProtoNode}, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case name_type(TokensSansSpace, Options) of
		{ok, NameNode, NewTokens} ->
			{_, L, _} = hd(TokensSansSpace),
			ArgNode = milang_ast:ast_node(L, Comments, #type_concrete{ args = [], name = NameNode }),
			declaration_constructor({arg_or_done, [ArgNode | ArgAcc], ProtoNode}, NewTokens, Options);
		Error ->
			Error
	end;

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
	case type_function(Chomped, Options) of
		{ok, ArgNode, NewTokens} ->
			NewArgNode = milang_ast:ins_doc(Comments, ArgNode),
			declaration_constructor({arg_or_done, [NewArgNode| ArgAcc], ProtoNode}, NewTokens ++ Tokens, Options);
		Error ->
			Error
	end.

declaration_spec_or_function(Comments, Tokens, Options) ->
	[{_, Location, _} | _] = Tokens,
	declaration_spec_or_function(get_name, Tokens, Options, {Comments, Location}).

declaration_spec_or_function(get_name, Tokens, Options, {Comments, Location}) ->
	case function_name_declaration(Tokens, Options) of
		{ok, Name, NewTokens} ->
			declaration_spec_or_function(spec_or_function, NewTokens, Options, {Comments, Location, Name});
		Error ->
			Error
	end;
declaration_spec_or_function(spec_or_function, Tokens, Options, {Comments, Location, Name}) ->
	{_, MoreComments, TokensSansSpace} = space(Tokens),
	[Head | NewTokens] = TokensSansSpace,
	case Head of
		{syntax_spec, _, _} ->
			declaration_spec_start(Comments, Location, Name, MoreComments ++ NewTokens, Options);
		_ ->
			declaration_function_start(Comments, Location, Name, Tokens, Options)
	end.

function_name_declaration(Tokens, Options) ->
	function_name_declaration(first_part, Tokens, Options).

function_name_declaration(first_part, [{name_downcase, _, Iodata} | Tokens], _Options) ->
	Name = iolist_to_atom(Iodata),
	{ok, {name_downcase, Name}, Tokens};
function_name_declaration(first_part, [{name_symbol_quoted, _, Iodata} | Tokens], _Options) ->
	{ok, {name_symbol, iolist_to_atom(Iodata)}, Tokens};
function_name_declaration(first_part, [{name_upcase, _, _} = T | _], #{ mode := module}) ->
	lex_error(T, [name_downcase, name_symbol_quoted], {function_name_declaration, first_part});
function_name_declaration(first_part, [{name_upcase, _, Iodata} | Tokens], Options) ->
	function_name_declaration({dot, [Iodata]}, Tokens, Options);
function_name_declaration(first_part, [T | _], _Options) ->
	lex_error(T, [name_symbol_quoted, name_upcase, name_downcase], {function_name_declaration, first_part});
function_name_declaration({dot, Acc}, [{syntax_dot, _, _} | Tokens], Options) ->
	function_name_declaration({part_or_done, Acc}, Tokens, Options);
function_name_declaration({part_or_done, Acc}, [{name_upcase, _, Iodata} | Tokens], Options) ->
	function_name_declaration({dot, [Iodata | Acc]}, Tokens, Options);
function_name_declaration({part_or_done, Acc}, [{name_downcase, _, Iodata} | Tokens], _Options) ->
	Local = iolist_to_atom(Iodata),
	Module = iolist_to_atom(lists:join($., lists:reverse(Acc))),
	Name = #{ local => Local, module => Module },
	{ok, {name_downcase, Name}, Tokens};
function_name_declaration({part_or_done, Acc}, [{name_symbol_quoted, _, Iodata} | Tokens], _Options) ->
	Local = iolist_to_atom(Iodata),
	Module = iolist_to_atom(lists:join($., lists:reverse(Acc))),
	Name = #{ local => Local, module => Module },
	{ok, {name_symbol, Name}, Tokens};
function_name_declaration({part_or_done, _} = S, [T | _], _Options) ->
	lex_error(T, [name_upcase, name_downcase, name_symbol_quoted], {function_name_declaration, S}).

declaration_spec_start(Comments, Location, Name, Tokens, Options) ->
	ProtoNode = milang_ast:ast_node(Location, Comments, #declaration_spec{ name = Name }),
	case type_function(Tokens, Options) of
		{ok, TypeNode, NewTokens} ->
			Node = milang_ast:transform_data(fun(M) -> M#declaration_spec{ type = TypeNode } end, ProtoNode),
			declaration_spec_finish(Node, NewTokens, Options);
		Error ->
			Error
	end.

declaration_spec_finish(Node, Tokens, _Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_dot, _, _} | Tail] ->
			{ok, Node, Comments ++ Tail};
		[T | _] ->
			lex_error(T, dot, {declaration_spec, {finish, Node}})
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


type_function(Tokens, Options) ->
	type_function({start_arg, []}, Tokens, Options).

type_function({start_arg, ArgsAcc}, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{name_downcase, L, Iodata} | Rest] ->
			ProtoNode = milang_ast:ast_node(L, Comments),
			Node = milang_ast:type_variable(ProtoNode, {name_downcase, iolist_to_atom(Iodata)}),
			type_function({implies_or_done, [Node | ArgsAcc]}, Rest, Options);
		[{name_upcase, _, _} | _] ->
			type_function({type_concrete, ArgsAcc}, Tokens, Options);
		[{syntax_open, _, subexpression} = Open | Tail] ->
			type_function({open_subexpression, Comments, Open, ArgsAcc}, Tail, Options);
		[{name_underscore, L, Iodata} | Tail] ->
			ProtoNode = milang_ast:ast_node(L, Comments),
			Node = milang_ast:type_variable(ProtoNode, {name_underscore, iolist_to_atom(Iodata)}),
			type_function({implies_or_done, [ Node | ArgsAcc]}, Tail, Options);
		[{syntax_open, _, record} | _] ->
			type_function({type_record, ArgsAcc}, Tokens, Options);
		[T | _] ->
			lex_error(T, [type_variable, type_concrete, subexpression], {type_function, {start_arg, ArgsAcc}})
	end;

type_function({implies_or_done, Args}, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_implies, _, _} | Rest] ->
			type_function({start_arg, Args}, Comments ++ Rest, Options);
		_ ->
			type_function({done, Args}, Tokens, Options)
	end;

type_function({type_record, ArgsAcc}, Tokens, Options) ->
	case type_record(Tokens, Options) of
		{ok, Node, NewTokens} ->
			type_function({implies_or_done, [ Node | ArgsAcc ]}, NewTokens, Options);
		Error ->
			Error
	end;

type_function({type_concrete, ArgsAcc}, Tokens, Options) ->
	case type_concrete(Tokens, Options) of
		{ok, Type, NewTokens} ->
			type_function({implies_or_done, [Type | ArgsAcc]}, NewTokens, Options);
		Error ->
			Error
	end;

type_function({open_subexpression, Comments, Open, ArgsAcc}, Tokens, Options) ->
	case to_matching_close(Open, Tokens, Options) of
		{ok, Open, Chomped, _Close, NewTokens} ->
			type_function({subexpression, Comments, Chomped, ArgsAcc}, NewTokens, Options);
		Error ->
			Error
	end;

type_function({subexpression, Comments, Chomped, ArgsAcc}, Tokens, Options) ->
	case type_function(Chomped, Options) of
		{ok, Type, NewTokens} ->
			NewType = milang_ast:ins_doc(Comments, Type),
			type_function({implies_or_done, [NewType | ArgsAcc]}, NewTokens ++ Tokens, Options);
		Error ->
			Error
	end;

type_function({done, Args}, Tokens, _) ->
	case Args of
		[T] ->
			{ok, T, Tokens};
		_ ->
			[FirstNode | _ ] = All = lists:reverse(Args),
			Location = milang_ast:location(FirstNode),
			Node = milang_ast:ast_node(Location, <<>>, #type_function{ args = All}),
			{ok, Node, Tokens}
	end.

type_concrete(Tokens, Options) ->
	type_concrete(start, Tokens, Options).

type_concrete(start, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{name_upcase, L, _} | _] ->
			ProtoNode = milang_ast:ast_node(L, Comments, #type_concrete{}),
			type_concrete({name, ProtoNode}, TokensSansSpace, Options);
		[{name_downcase, L, Iodata} | Tail] ->
			Node = milang_ast:ast_node(L, Comments, #type_variable{ name = {name_downcase, iolist_to_atom(Iodata)}}),
			{ok, Node, Tail};
		[T | _] ->
			lex_error(T, upcase_name, {type_concrete, start})
	end;
type_concrete({name, ProtoNode}, Tokens, Options) ->
	case name_type(Tokens, Options) of
		{ok, Name, NewTokens} ->
			NewNode = milang_ast:transform_data(fun(M) -> M#type_concrete{ name = Name } end, ProtoNode),
			type_concrete({start_arg, [], NewNode}, NewTokens, Options);
		Error ->
			Error
	end;

type_concrete({start_arg, ArgAcc, ProtoNode}, Tokens, Options) ->
	{HasSpace, Comments, TokensSansSpace} = space(Tokens),
	case {HasSpace, TokensSansSpace} of
		{true, [{name_downcase, L, Iodata} | Rest]} ->
			Node = milang_ast:ast_node(L, Comments, #type_variable{ name = {name_downcase, iolist_to_atom(Iodata)}}),
			type_concrete({start_arg, [Node | ArgAcc], ProtoNode}, Rest, Options);
		{_, [{syntax_open, _, subexpression} = Open | Rest]} ->
			type_concrete({open_subexpression, Comments, Open, ArgAcc, ProtoNode}, Rest, Options);
		{true, [{name_upcase, L, _} | _]} ->
			type_concrete({zero_arg_type, L, Comments, ArgAcc, ProtoNode}, TokensSansSpace, Options);
		{_, _} ->
			Args = lists:reverse(ArgAcc),
			Node = milang_ast:transform_data(fun(M) -> M#type_concrete{ args = Args } end, ProtoNode),
			{ok, Node, Tokens}
	end;

type_concrete({open_subexpression, Comments, Open, ArgAcc, ProtoNode}, Tokens, Options) ->
	case to_matching_close(Open, Tokens, Options) of
		{ok, Open, Chomped, _Close, NewTokens} ->
			type_concrete({subexpression, Comments, Chomped, ArgAcc, ProtoNode}, NewTokens, Options);
		Error ->
			Error
	end;

type_concrete({subexpression, Comments, Chomped, ArgAcc, ProtoNode}, Tokens, Options) ->
	case type_function(Chomped, Options) of
		{ok, TypeProtoNode, NewTokens} ->
			TypeNode = milang_ast:ins_doc(Comments, TypeProtoNode),
			type_concrete({start_arg, [TypeNode | ArgAcc], ProtoNode}, NewTokens ++ Tokens, Options);
		Error ->
			Error
	end;

type_concrete({zero_arg_type, L, Comments, ArgAcc, ProtoNode}, Tokens, Options) ->
	case name_type(Tokens, Options) of
		{ok, Name, NewTokens} ->
			Arg = milang_ast:ast_node(L, Comments, #type_concrete{ name = Name, args = [] }),
			type_concrete({start_arg, [ Arg | ArgAcc], ProtoNode}, NewTokens, Options);
		Error ->
			Error
	end.

type_record(Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_open, L, record} | Tail] ->
			ProtoNode = milang_ast:type_record(milang_ast:ast_node(L, Comments), []),
			type_record({field_start_or_done, ProtoNode}, Tail, Options);
		[T | _] ->
			lex_error(T, syntax_open_record, {type_record, init})
	end.

type_record({field_start_or_done, ProtoNode}, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_element_seperator, _, _} | Tail] ->
			type_record({field_name, ProtoNode}, Comments ++ Tail, Options);
		[{syntax_close, _, record} | Tail] ->
			% while order of record fields doesn't matter from a syntactic or
			% grammer perspective, preserving the order makes testing easier.
			% is is because now if you read in, lex, and to_string out the ast,
			% you'll get the same thing.
			Node = milang_ast:transform_data(fun(D) ->
				D#type_record{ fields = lists:reverse(D#type_record.fields)}
			end, ProtoNode),
			{ok, Node, Comments ++ Tail};
		[T | _] ->
			lex_error(T, [syntax_close_recordd, syntax_element_seperator], {type_record, {field_start_or_done, ProtoNode}})
	end;

type_record({field_name, ProtoNode}, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{name_downcase, L, Iodata} | Tail] ->
			Name = {name_downcase, iolist_to_atom(Iodata)},
			ProtoField = milang_ast:ast_node(L, Comments, #type_record_field{ name = Name }),
			type_record({field_bind, ProtoField, ProtoNode}, Tail, Options);
		[T | _] ->
			lex_error(T, [name_downcase], {type_record, {field_name, ProtoNode}})
	end;

type_record({field_bind, ProtoField, ProtoNode}, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_bind, _, _} | Tail] ->
			type_record({field_type, ProtoField, ProtoNode}, Comments ++ Tail, Options);
		[T | _] ->
			lex_error(T, [syntax_bind], {type_record, {field_bind, ProtoField, ProtoNode}})
	end;

type_record({field_type, ProtoField, ProtoNode}, Tokens, Options) ->
	case type_function(Tokens, Options) of
		{ok, TypeNode, NewTokens} ->
			FieldNode = milang_ast:transform_data(fun(D) ->
				D#type_record_field{ type = TypeNode }
			end, ProtoField),
			NewNode = milang_ast:transform_data(fun(D) ->
				D#type_record{ fields = [ FieldNode | D#type_record.fields ]}
			end, ProtoNode),
			type_record({field_start_or_done, NewNode}, NewTokens, Options);
		Error ->
			Error
	end.

name_type(Tokens, Options) ->
	name_type(first_part, Tokens, Options).

name_type(first_part, Tokens, Options) ->
	case Tokens of
		[{name_upcase, _, Iodata} | Tail] ->
			name_type({dot_or_done, [Iodata]}, Tail, Options);
		[T | _] ->
			lex_error(T, name_upcase, {name_type, first_part})
	end;

name_type({dot_or_done, PartAcc}, Tokens, Options) ->
	case Tokens of
		[{syntax_dot, _, _}, {name_upcase, _, IoData} | Tail] ->
			name_type({dot_or_done, [IoData | PartAcc]}, Tail, Options);
		_ ->
			name_type({done, PartAcc}, Tokens, Options)
	end;

name_type({done, [OnePart]}, Tokens, _Options) ->
	{ok, {name_upcase, iolist_to_atom(OnePart)}, Tokens};

name_type({done, [Local | UnjoinedModule]}, Tokens, _Options) ->
	Name = #{ local => iolist_to_atom(Local)
	, module => iolist_to_atom(lists:join($., lists:reverse(UnjoinedModule)))
	},
	{ok, {name_upcase, Name}, Tokens}.


declaration_function_start(Comments, Location, Name, Tokens, Options) ->
	ProtoNode = milang_ast:ast_node(Location, Comments, #declaration_function{ name = Name, args = [], bindings = []}),
	declaration_function({args_or_implies, []}, Tokens, Options, ProtoNode).

declaration_function({args_or_implies, Acc}, Tokens, Options, ProtoNode) ->
	{WasSpace, Comments, TokensSansSpace} = space(Tokens),
	[Head | Rest] = TokensSansSpace,
	case {WasSpace, Head} of
		{_, {syntax_implies, _, _}} ->
			NewNode = milang_ast:transform_data(fun(M) -> M#declaration_function{ args = lists:reverse(Acc)} end, ProtoNode),
			declaration_function({function_body, []}, Rest, Options, NewNode);
		{true, {name_downcase, L, Iodata}} ->
			NameNode = milang_ast:function_variable(milang_ast:ast_node(L, Comments), {name_downcase, iolist_to_atom(Iodata)}),
			NewAcc = [NameNode | Acc],
			declaration_function({args_or_implies, NewAcc}, Rest, Options, ProtoNode);
		{true, {name_underscore, L, Iodata}} ->
			NameNode = milang_ast:function_variable(milang_ast:ast_node(L, Comments), {name_underscore, iolist_to_atom(Iodata)}),
			NewAcc = [ NameNode | Acc ],
			declaration_function({args_or_implies, NewAcc}, Rest, Options, ProtoNode);
		{true, _} ->
			lex_error(Head, [name_downcase, name_unbound, syntax_implies], {declaration_function, {args_or_implies, Acc, ProtoNode}});
		{false, _} ->
			lex_error(hd(Tokens), space, {declaration_function, {args_or_implies, Acc, ProtoNode}})
	end;

declaration_function({function_body, BindingsAcc}, Tokens, Options, ProtoNode) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	[ Head | Rest] = TokensSansSpace,
	{_, MoreComments, RestSansSpace} = space(Rest),
	case {Head, RestSansSpace} of
		{{name_downcase, _, _} = BindName, [{syntax_bind, _, _} | TrueRest]} ->
			declaration_function({function_body_binding, BindName, Comments ++ MoreComments, BindingsAcc}, TrueRest, Options, ProtoNode);
		_ ->
			NewNode = milang_ast:transform_data(fun(M) -> M#declaration_function{bindings = lists:reverse(BindingsAcc)} end, ProtoNode),
			declaration_function(final_expression, Tokens, Options, NewNode)
	end;

declaration_function({function_body_binding, BindName, BindComments, BindingsAcc}, Tokens, Options, ProtoNode) ->
	case expression(Tokens, Options) of
		{ok, Expression, NewTokens} ->
			{NameType, Location, Iodata} = BindName,
			Binding = milang_ast:ast_node(Location, BindComments, #binding{ name = {NameType, iolist_to_atom(Iodata)}, expression = Expression}),
			declaration_function({binding_comma, [Binding | BindingsAcc]}, NewTokens, Options, ProtoNode);
		Error ->
			Error
	end;

declaration_function({binding_comma, BindingsAcc}, Tokens, Options, ProtoNode) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_element_seperator, _, _} | Rest] ->
			declaration_function({function_body, BindingsAcc}, Comments ++ Rest, Options, ProtoNode);
		[T | _] ->
			lex_error(T, space, {declaration_function, {binding_comma, BindingsAcc, ProtoNode}})
	end;

declaration_function(final_expression, Tokens, Options, ProtoNode) ->
	case expression(Tokens, Options) of
		{ok, Expression, NewTokens} ->
			NewNode = milang_ast:transform_data(fun(M) -> M#declaration_function{ expression = Expression } end, ProtoNode),
			declaration_function(final_dot, NewTokens, Options, NewNode);
		Error ->
			Error
	end;

declaration_function(final_dot, Tokens, _Options, ProtoNode) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_dot, _, _} | Tail] ->
			{ok, ProtoNode, Comments ++ Tail};
		[T | _] ->
			lex_error(T, dot, {declaration_function, {final_dot, ProtoNode}})
	end.


declaration_module(Location, Comments, Options, Tokens) ->
	ProtoNode = milang_ast:ast_node(Location, Comments, #declaration_module{}),
	do_declaration_module(need_name, Tokens, Options, ProtoNode).

do_declaration_module(need_name, Tokens, Options, ProtoNode) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	NodeWithComments = milang_ast:add_doc(Comments, ProtoNode),
	case name_module(TokensSansSpace, Options) of
		{ok, Iolist, TokensAfterName} ->
			NodeWithName = milang_ast:transform_data(fun(M) -> M#declaration_module{ name = iolist_to_atom(Iolist)} end, NodeWithComments),
			do_declaration_module(need_space_before_exposing, TokensAfterName, Options, NodeWithName);
		Error ->
			Error
	end;
do_declaration_module(need_space_before_exposing, Tokens, Options, ProtoNode) ->
	case space(Tokens) of
		{false, _, _} ->
			Head = hd(Tokens),
			lex_error(Head, space, {declaration_module, need_space_before_exposing});
		{true, Comments, NewTokens} ->
			NewNode = milang_ast:add_doc(Comments, ProtoNode),
			do_declaration_module(need_exposing, NewTokens, Options, NewNode)
	end;

do_declaration_module(need_exposing, [{syntax_keyword, _, exposing} | Tokens], Options, ProtoNode) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case generic_list(TokensSansSpace, Options, fun exposing_list_element/2) of
		{ok, List, TokensAfterExposing} ->
			NodeWithComments = milang_ast:add_doc(Comments, ProtoNode),
			NewNode = milang_ast:transform_data(fun(M) -> M#declaration_module{ exposing = List } end, NodeWithComments),
			do_declaration_module(finishing, TokensAfterExposing, Options, NewNode);
		Error ->
			Error
	end;

do_declaration_module(finishing, Tokens, _Options, Node) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_dot, _, _} | NewTokens] ->
			{ok, Node, Comments ++ NewTokens};
		[T |_] ->
			lex_error(T, dot, {declaration_module, finishing})
	end.

name_module(Tokens, Options) ->
	name_module(name_part, Tokens, Options, []).

name_module(name_part, [{name_upcase, _, Part} | NewTokens], Options, Acc) ->
	name_module(dot_or_done, NewTokens, Options, [Part | Acc]);
name_module(dot_or_done, [{syntax_dot, _, _} | Tokens], Options, Acc) ->
	name_module(name_part, Tokens, Options, [$. | Acc]);
name_module(dot_or_done, Tokens, _Options, Acc) ->
	{ok, lists:reverse(Acc), Tokens};
name_module(name_part, [T | _], _Options, _Acc) ->
	lex_error(T, name_upcase, {name_module, name_part}).

exposing_list_element(Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	exposing_list_element(TokensSansSpace, Comments, Options).

exposing_list_element([{name_downcase, _, Iodata} | Tokens], _Comments, _Options) ->
	Node = {name_downcase, iolist_to_atom(Iodata)},
	{ok, Node, Tokens};
exposing_list_element([{name_symbol_quoted, _L, Iodata} | Tokens], _Comments, _Options) ->
	Node = {name_symbol, iolist_to_atom(Iodata)},
	{ok, Node, Tokens};
exposing_list_element([{name_upcase, _L, Iodata} | Tokens], _Comments, _Options) ->
	Node = {name_upcase, iolist_to_atom(Iodata)},
	{ok, Node, Tokens};
exposing_list_element([T | _], _Comments, _Options) ->
	lex_error(T, [name_downcase, name_symbol_quoted, name_upcase], exposing_list_element).

declaration_import(Comments, Options, Tokens) ->
	[{declaration_import, L, _} | Rest] = Tokens,
	ProtoNode = milang_ast:ast_node(L, Comments, #declaration_import{alias = undefined, exposing = []}),
	declaration_import(prename_space, Rest, Options, ProtoNode).

declaration_import(prename_space, Tokens, Options, ProtoNode) ->
	case space(Tokens) of
		{true, Comments, Rest} ->
			NewNode = milang_ast:add_doc(Comments, ProtoNode),
			declaration_import(name, Rest, Options, NewNode);
		{false, _, [T | _]} ->
			lex_error(T, space, {declaration_import, prename_space})
	end;
declaration_import(name, Tokens, Options, ProtoNode) ->
	case name_module(Tokens, Options) of
		{ok, Iolist, Rest} ->
			NewNode = milang_ast:transform_data(fun(M) -> M#declaration_import{ name = iolist_to_atom(Iolist) } end, ProtoNode),
			declaration_import(space_alias_exposing_finished, Rest, Options, NewNode);
		Error ->
			Error
	end;
declaration_import(space_alias_exposing_finished, Tokens, Options, ProtoNode) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	NewNode = milang_ast:add_doc(Comments, ProtoNode),
	declaration_import(alias_exposing_finished, TokensSansSpace, Options, NewNode);
declaration_import(alias_exposing_finished, [{syntax_keyword, _, as} | Rest], Options, ProtoNode) ->
	case space(Rest) of
		{true, Comments, TokensSansSpace} ->
			NewNode = milang_ast:add_doc(Comments, ProtoNode),
			declaration_import(alias_name, TokensSansSpace, Options, NewNode);
		_ ->
			T = hd(Rest),
			lex_error(T, space, {declaration_import, alias_exposing_finished, ProtoNode})
	end;
declaration_import(alias_exposing_finished, [{syntax_dot, _, _} | Rest], _Options, ProtoNode) ->
	{ok, ProtoNode, Rest};
declaration_import(alias_exposing_finished, [{syntax_keyword, _, exposing} | Rest], Options, ProtoNode) ->
	declaration_import(exposing_list, Rest, Options, ProtoNode);
declaration_import(alias_name, [{name_upcase, _, Iodata} | Tokens], Options, ProtoNode) ->
	NewNode = milang_ast:transform_data(fun(M) -> M#declaration_import{ alias = {ok, iolist_to_atom(Iodata)}} end, ProtoNode),
	declaration_import(post_alias_name, Tokens, Options, NewNode);
declaration_import(post_alias_name, Tokens, Options, ProtoNode) ->
	{IsSpace, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_dot, _, _} | Rest] ->
			{ok, ProtoNode, Comments ++ Rest};
		[{syntax_keyword, _, exposing} | Rest] when IsSpace ->
			NewNode = milang_ast:add_doc(Comments, ProtoNode),
			declaration_import(exposing_list, Rest, Options, NewNode);
		_ ->
			T = hd(TokensSansSpace),
			lex_error(T, [exposing, dot, space], {declaration_import, {post_alias_name, ProtoNode}})
	end;
declaration_import(exposing_list, Tokens, Options, ProtoNode) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case generic_list(TokensSansSpace, Options, fun exposing_list_element/2) of
		{ok, List, TokensAfterExposing} ->
			NewNodeSansNewDoc = milang_ast:transform_data(fun(M) -> M#declaration_import{ exposing = List} end, ProtoNode),
			NewNode = milang_ast:add_doc(Comments, NewNodeSansNewDoc),
			declaration_import(finishing, TokensAfterExposing, Options, NewNode);
		Error ->
			Error
	end;
declaration_import(finishing, Tokens, _Options, Node) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	[ Head | Tail] = TokensSansSpace,
	case Head of
		{syntax_dot, _, _} ->
			{ok, Node, Comments ++ Tail};
		_ ->
			lex_error(Head, dot, {declaration_import, {finsihing, Node}})
	end.

expression(Tokens, Options) ->
	expression(start, Tokens, Options).

expression(start, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	[{_, L, _} | _] = TokensSansSpace,
	Node = milang_ast:ast_node(L, Comments, #expression_infix{ infix_ops = []}),
	expression({head, Node}, TokensSansSpace, Options);

expression({head, ProtoNode}, Tokens, Options) ->
	case expression_non_infix(Tokens, Options) of
		{ok, Node, NewTokens} ->
			NewNode = milang_ast:transform_data(fun(M) -> M#expression_infix{ head = Node } end, ProtoNode),
			expression({infix_op_or_done, NewNode}, NewTokens, Options);
		Error ->
			Error
	end;

expression({infix_op_or_done, ProtoNode}, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{name_symbol, L, Iodata} | Rest] ->
			NameNode = {name_symbol, iolist_to_atom(Iodata)},
			ProtoOp = milang_ast:ast_node(L, Comments),
			Op = milang_ast:infix_notation(ProtoOp, NameNode, 1, left),
			ProtoInfix = milang_ast:ast_node(L, Comments, #infix_operation{ operator = Op}),
			expression({infix_op, ProtoInfix, ProtoNode}, Rest, Options);
		[{syntax_infix_left, _, _} | _] ->
			expression({infix_notation, ProtoNode}, Tokens, Options);
		[{syntax_infix_right, _, _} | _] ->
			expression({infix_notation, ProtoNode}, Tokens, Options);
		_ ->
			io:format("~p:~p @ ~p gave up at: ~p", [?MODULE, ?FUNCTION_NAME, ?LINE, hd(TokensSansSpace)]),
			Node = milang_ast:transform_data(fun(M) ->
				M#expression_infix{ infix_ops = lists:reverse(M#expression_infix.infix_ops) }
			end, ProtoNode),
			{ok, Node, Tokens}
	end;

expression({infix_op, ProtoOp, ProtoNode}, Tokens, Options) ->
	case expression_non_infix(Tokens, Options) of
		{ok, Node, NewTokens} ->
			OpNode = milang_ast:transform_data(fun(M) -> M#infix_operation{ expression = Node } end, ProtoOp),
			NewNode = milang_ast:transform_data(fun(M) ->
				M#expression_infix{ infix_ops = [OpNode | M#expression_infix.infix_ops]}
			end, ProtoNode),
			expression({infix_op_or_done, NewNode}, NewTokens, Options);
		Error ->
			Error
	end;

expression({infix_notation, ProtoNode}, Tokens, Options) ->
	case infix_notation(Tokens, Options) of
		{ok, InfixNode, NewTokens} ->
			expression({infix_op, InfixNode, ProtoNode}, NewTokens, Options);
		Error ->
			Error
	end.

infix_notation(Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	infix_notation({start, Comments}, TokensSansSpace, Options).

infix_notation({start, Comments}, Tokens, Options) ->
	case Tokens of
		[{symbol_infix_left, L, Iodata} | Rest] ->
			Weight = count_characters(Iodata),
			ProtoNode = milang_ast:ast_node(L, Comments, #infix_notation{ weight = Weight, assoc = left}),
			infix_notation({function, ProtoNode}, Rest, Options);
		[{symbol_infix_right, L, Iodata} | Rest] ->
			Weight = count_characters(Iodata),
			ProtoNode = milang_ast:ast_node(L, Comments, #infix_notation{ weight = Weight, assoc = right}),
			infix_notation({function, ProtoNode}, Rest, Options);
		[T | _] ->
			lex_error(T, [symbol_infix_right, symbol_infix_left], {infix_notation, {start, Comments}})
	end;

infix_notation({function, ProtoNode}, Tokens, Options) ->
	case Tokens of
		[{name_symbol_quoted, _L, Iodata} | Rest] ->
			NameNode = {name_symbol, iolist_to_atom(Iodata)},
			Node = milang_ast:transform_data(fun(M) -> M#infix_notation{ name = NameNode} end, ProtoNode),
			{ok, Node, Rest};
		[{name_downcase, _L, Iodata} | Rest] ->
			NameNode = {name_function, iolist_to_atom(Iodata)},
			Node = milang_ast:transform_data(fun(M) -> M#infix_notation{ name = NameNode } end, ProtoNode),
			{ok, Node, Rest};
		[{name_upcase, _, _} | _] ->
			infix_notation({remote_function, ProtoNode}, Tokens, Options);
		[T | _] ->
			lex_error(T, [name_symbol_quoted, name_downcase, remote_function_name], {infix_notation, {function, ProtoNode}})
	end;

infix_notation({remote_function, ProtoNode}, Tokens, Options) ->
	case remote_function_name(Tokens, Options) of
		{ok, Name, NewTokens} ->
			Node = milang_ast:transform_data(fun(M) -> M#infix_notation{ name = Name } end, ProtoNode),
			{ok, Node, NewTokens};
		Error ->
			Error
	end.

remote_function_name(Tokens, Options) ->
	remote_function_name(start, Tokens, Options).

remote_function_name(start, [{name_upcase, L, Iodata} | Rest], Options) ->
	remote_function_name({dot, L, [Iodata]}, Rest, Options);

remote_function_name(start, [T | _], _Options) ->
	lex_error(T, name_upcase, {remote_function_name, start});

remote_function_name({dot, L, ModuleAcc}, [{syntax_dot, _, _} | Rest], Options) ->
	remote_function_name({upcase_or_done, L, ModuleAcc}, Rest, Options);

remote_function_name({dot, _L, _} = S, [T | _], _Options) ->
	lex_error(T, dot, {remote_function_name, S});

remote_function_name({upcase_or_done, L, ModuleAcc}, Tokens, Options) ->
	case Tokens of
		[{name_upcase, _, Iodata} | Rest] ->
			remote_function_name({dot, L, [Iodata | ModuleAcc]}, Rest, Options);
		[{name_downcase, _, Iodata} | Rest] ->
			Name = #{ module => iolist_to_atom(lists:join($., lists:reverse(ModuleAcc))),
				local => iolist_to_atom(Iodata)
				},
			Node = {name_downcase, Name},
			{ok, Node, Rest};
		[{name_symbol_quoted, _, Iodata} | Rest] ->
			Name = #{ module => iolist_to_atom(lists:join($., lists:reverse(ModuleAcc))),
				local => iolist_to_atom(Iodata) },
			Node = {name_symbol, Name},
			{ok, Node, Rest};
		[T | _] ->
			lex_error(T, [name_upcase, name_downcase, name_symbol_quoted], {remote_function, {upcase_or_done, L, ModuleAcc}})
	end.

expression_non_infix(Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	[Head | Rest] = TokensSansSpace,
	case Head of
		{name_upcase, _, _} ->
			expression_remote_or_constructor(Tokens, Options);
		{name_downcase, L, Iodata} ->
			NameNode = {name_downcase, iolist_to_atom(Iodata)},
			expression_call(Comments, L, NameNode, Rest, Options);
		{name_symbol_quoted, L, Iodata} ->
			NameNode = {name_symbol, iolist_to_atom(Iodata)},
			expression_call(Comments, L, NameNode, TokensSansSpace, Options);
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
		{syntax_open, _, map} ->
			expression_map(Tokens, Options);
		{syntax_open, _, record} ->
			expression_record(Tokens, Options);
		_ ->
			lex_error(Head, [expression_constructor, expression_call, subexpression, literal], expression_non_infix)
	end.

expression_map(_Tokens, _Options) ->
	{error, {nyi, expression_map}}.

expression_record(Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_open, L, record} | Tail] ->
			ProtoNode = milang_ast:ast_node(L, Comments, #expression_record{}),
			expression_record({post_open, ProtoNode}, Tail, Options);
		[T | _] ->
			lex_error(T, syntax_open_record, {expression_record, init})
	end.

expression_record({post_open, ProtoNode}, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{name_downcase, _, Iodata} | Tail] ->
			Name = {name_downcase, iolist_to_atom(Iodata)},
			expression_record({field_access_or_record_update, Name, ProtoNode}, Tail, Options);
		[{syntax_element_seperator, _, _} | _] ->
			expression_record({field_start_or_done, ProtoNode}, Tokens, Options);
		[{syntax_close, _, record} | Tail] ->
			{ok, ProtoNode, Comments ++ Tail};
		[T | _] ->
			lex_error(T, [name_downcase, syntax_element_seperator, syntax_close_record], {expression_record, {post_open, ProtoNode}})
	end;

expression_record({field_access_or_record_update, Name, ProtoNode}, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_close, _, record} | Tail] ->
			Node = milang_ast:transform_data(fun(_) ->
				#record_field_access{ name = Name }
			end, ProtoNode),
			{ok, Node, Comments ++ Tail};
		[{name_symbol, _, [$|]} | Tail] ->
			Node = milang_ast:transform_data(fun(D) ->
				D#expression_record{ base_reference = Name}
			end, ProtoNode),
			expression_record({field_start_or_done, Node}, Comments ++ Tail, Options);
		[T | _] ->
			lex_error(T, [syntax_close_record, syntax_collection_update], {expression_record, {field_access_or_record_update, Name, ProtoNode}})
	end;

expression_record({field_start_or_done, ProtoNode}, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_element_seperator, _, _} | Tail] ->
			expression_record({field_name, ProtoNode}, Comments ++ Tail, Options);
		[{syntax_close, _, record} | Tail] ->
			{ok, ProtoNode, Comments ++ Tail};
		[T | _] ->
			lex_error(T, [syntax_element_seperator], {expression_record, {field_start_or_done, ProtoNode}})
	end;

expression_record({field_name, ProtoNode}, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{name_downcase, L, Iodata} | Tail] ->
			FieldNode = milang_ast:ast_node(L, Comments, #expression_record_field{ name = {name_downcase, iolist_to_atom(Iodata)}}),
			expression_record({field_bind, FieldNode, ProtoNode}, Tail, Options);
		[T | _] ->
			lex_error(T, name_downcase, {expression_record, {field_name, ProtoNode}})
	end;

expression_record({field_bind, ProtoField, ProtoNode}, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{syntax_bind, _, _} | Tail] ->
			expression_record({field_expression, ProtoField, ProtoNode}, Comments ++ Tail, Options);
		[T | _] ->
			lex_error(T, syntax_bind, {expression_record, {field_bind, ProtoField, ProtoNode}})
	end;

expression_record({field_expression, ProtoField, ProtoNode}, Tokens, Options) ->
	case expression(Tokens, Options) of
		{ok, ExpressionNode, NewTokens} ->
			FieldNode = milang_ast:transform_data(fun(D) ->
				D#expression_record_field{ expression = ExpressionNode }
			end, ProtoField),
			NewNode = milang_ast:transform_data(fun(D) ->
				D#expression_record{ fields = [FieldNode| D#expression_record.fields ]}
			end, ProtoNode),
			expression_record({field_start_or_done, NewNode}, NewTokens, Options);
		Error ->
			Error
	end.

expression_remote_or_constructor(Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	[{_, L, _} | _] = TokensSansSpace,
	case remote_name(TokensSansSpace, Options) of
		{ok, {name_downcase, _} = Name, NewTokens} ->
			expression_call(Comments, L, Name, NewTokens, Options);
		{ok, {name_symbol, _} = Name, NewTokens} ->
			expression_call(Comments, L, Name, NewTokens, Options);
		{ok, {name_upcase, _} = Name, NewTokens} ->
			expression_constructor(Comments, L, Name, NewTokens, Options);
		Error ->
			Error
	end.

expression_constructor(Comments, L, NameNode, Tokens, Options) ->
	ProtoNode = milang_ast:ast_node(L, Comments, #expression_call{ args = []}),
	case do_expression_call({arg_start, [], ProtoNode}, Tokens, Options) of
		{ok, CallNode, NewTokens} ->
			Node = milang_ast:transform_data(fun(M) ->
				#expression_construct{ type = NameNode, args = M#expression_call.args }
			end, CallNode),
			{ok, Node, NewTokens};
		Error ->
			Error
	end.

do_expression_call({arg_start, ArgAcc, ProtoNode}, Tokens, Options) ->
	case space(Tokens) of
		{true, _Comments, _} ->
			do_expression_call({arg, ArgAcc, ProtoNode}, Tokens, Options);
		{false, _, _} ->
			io:format("~s:~s@~p gave up due to no space: ~p.~n", [?MODULE, ?FUNCTION_NAME, ?LINE, hd(Tokens)]),
			Args = lists:reverse(ArgAcc),
			Node = milang_ast:transform_data(fun(M) -> M#expression_call{ args = Args } end, ProtoNode),
			{ok, Node, Tokens}
	end;

do_expression_call({arg, ArgAcc, ProtoNode}, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	case TokensSansSpace of
		[{name_upcase, _, _} | _] ->
			do_expression_call({arg_maybe_remote, ArgAcc, ProtoNode}, Tokens, Options);
		[{name_downcase, L, Iodata} | T] ->
			Name = {name_downcase, iolist_to_atom(Iodata)},
			Arg = milang_ast:ast_node(L, Comments, #expression_call{ function = Name, args = []}),
			do_expression_call({arg_start, [Arg | ArgAcc], ProtoNode}, T, Options);
		[{name_symbol_quoted, L, Iodata} | Tail] ->
			Name = {name_symbol, iolist_to_atom(Iodata)},
			Arg = milang_ast:ast_node(L, Comments, #expression_call{ function = Name, args = []}),
			do_expression_call({arg_start, [Arg | ArgAcc], ProtoNode}, Tail, Options);
		[{literal_string, L, Iodata} | Tail] ->
			Arg = milang_ast:ast_node(L, Comments, {literal_string, Iodata}),
			do_expression_call({arg_start, [Arg | ArgAcc], ProtoNode}, Tail, Options);
		[{literal_integer, L, I} | Tail] ->
			Arg = milang_ast:ast_node(L, Comments, {literal_integer, I}),
			do_expression_call({arg_start, [Arg | ArgAcc], ProtoNode}, Tail, Options);
		[{literal_float, L, F} | Tail] ->
			Arg = milang_ast:ast_node(L, Comments, {literal_float, F}),
			do_expression_call({arg_start, [Arg | ArgAcc], ProtoNode}, Tail, Options);
		_ ->
			io:format("~p:~p@~p gave up at: ~p~n", [?MODULE, ?FUNCTION_NAME, ?LINE, hd(TokensSansSpace)]),
			Node = milang_ast:transform_data(fun(M) -> M#expression_call{ args = lists:reverse(ArgAcc) } end, ProtoNode),
			{ok, Node, Tokens}
	end;

do_expression_call({arg_maybe_remote, ArgAcc, ProtoNode}, Tokens, Options) ->
	{_, Comments, TokensSansSpace} = space(Tokens),
	[{_, L, _} | _] = TokensSansSpace,
	case remote_name(TokensSansSpace, Options) of
		{ok, {name_upcase, _} = Name, NewTokens} ->
			Node = milang_ast:ast_node(L, Comments, #expression_construct{ type = Name, args = []}),
			do_expression_call({arg_start, [Node | ArgAcc], ProtoNode}, NewTokens, Options);
		{ok, {name_downcase, _} = Name, NewTokens} ->
			Node = milang_ast:ast_node(L, Comments, #expression_call{ function = Name, args = []}),
			do_expression_call({arg_start, [Node | ArgAcc], ProtoNode}, NewTokens, Options);
		Error ->
			Error
	end.

expression_call(Comments, L, NameNode, Tokens, Options) ->
	ProtoNode = milang_ast:ast_node(L, Comments, #expression_call{ function = NameNode }),
	do_expression_call({arg_start, [], ProtoNode}, Tokens, Options).

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

remote_name(Tokens, Options) ->
	remote_name(start, Tokens, Options).

remote_name(start, [{name_upcase, L, Iodata} | Tail], Options) ->
	remote_name({dot_then, L, [Iodata]}, Tail, Options);

remote_name(start, [T | _], _Options) ->
	lex_error(T, name_upcase, {remote_name, start});

remote_name({dot_then, L, ModuleAcc}, [{syntax_dot, _, _}, {name_upcase, _, Iodata} | Tail], Options) ->
	remote_name({dot_then, L, [Iodata | ModuleAcc]}, Tail, Options);

remote_name({dot_then, _L, ModuleAcc}, [{syntax_dot, _, _}, {name_downcase, _, Iodata} | Tail], _Options) ->
	Module = iolist_to_atom(lists:join($., lists:reverse(ModuleAcc))),
	Local = iolist_to_atom(Iodata),
	Name = #{ module => Module, local => Local },
	Node = {name_downcase, Name},
	{ok, Node, Tail};

remote_name({dot_then, _L, ModuleAcc}, [{syntax_dot, _, _}, {name_symbol_quoted, _, Iodata} | Tail], _Options) ->
	Module = iolist_to_atom(lists:join($., lists:reverse(ModuleAcc))),
	Local = iolist_to_atom(Iodata),
	Name = #{ module => Module, local => Local },
	Node = {name_symbol, Name},
	{ok, Node, Tail};

remote_name({dot_then, _L, [Type]}, Tokens, _Options) ->
	Node = {name_upcase, iolist_to_atom(Type)},
	{ok, Node, Tokens};

remote_name({dot_then, _L, [Type | ModuleAcc]}, Tokens, _Options) ->
	Module = iolist_to_atom(lists:join($., lists:reverse(ModuleAcc))),
	Local = iolist_to_atom(Type),
	Name = #{ module => Module, local => Local },
	Node = {name_upcase, Name},
	{ok, Node, Tokens};

remote_name(State, [T | _], _Options) ->
	lex_error(T, [dot, [name_upcase, name_downcase, name_symbol_quoted]], {remote_name, State}).

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
