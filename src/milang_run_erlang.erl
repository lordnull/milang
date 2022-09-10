%%% @doc The primary bridge between milang and erlang. This is primarily to
%%% deal with the these differences:
%%% milang                  | erlang
%%% lazy                    | eager
%%% auto-curry              | no-curry
%%% top level bind          | only functions at module level
%%% unified function syntax | fun vs top module level function.
-module(milang_run_erlang).

-include_lib("kernel/include/logger.hrl").

-export([ work/1, as_erlang/1, create_module/2 ]).

%% @doc Given an AST ( `[milang_ast:ast_node/1'] ), create an erlang source code
%% file that be compiled. A function to actually do the writing is supplied so
%% we don't have to keep _everything_ in ram and can do periodic data dumps.
create_module(AST, WriteFun) ->
	ModuleDeclaration = module_declaration(AST),
	ok = WriteFun(ModuleDeclaration),
	ExportDeclaration = export_declaration(AST),
	ok = WriteFun(ExportDeclaration),
	write_functions(AST, WriteFun).

module_declaration(AST) ->
	Filter = fun(Node) ->
		Type = milang_ast:type(Node),
		?LOG_DEBUG("De node: ~p is de type ~p", [Node, Type]),
		Type =:= {ok, module}
	end,
	[ ModuleNode | _] = lists:filter(Filter, AST),
	Data = milang_ast:data(ModuleNode),
	NameNode = milang_ast_module:name(Data),
	{_, NameComplex} = milang_ast:data(NameNode),
	NameStr = identifier_as_string(NameComplex),
	io_lib:format("-module('~s').\n\n", [NameStr]).

export_declaration(AST) ->
	ExportLines = lists:filtermap(fun as_export_entry/1, AST),
	Joined = lists:join($,, ExportLines),
	io_lib:format("-export([~s]).\n\n", [Joined]).

as_export_entry(Node) ->
	IsExpose = case milang_ast:type(Node) of
		{ok, expose} ->
			{ok, Node};
		_ ->
			{error, only_expose}
	end,
	IsSpec = 'Result':and_then(fun(InNode) ->
		Data = milang_ast:data(InNode),
		Declaration = milang_ast_expose:declaration(Data),
		case milang_ast:type(Declaration) of
			{ok, spec} ->
				{ok, Node};
			_ ->
				{error, only_spec} % TODO for now
		end
	end, IsExpose),
	DahFunc = 'Result':and_then(fun(InNode) ->
		Data = milang_ast:data(InNode),
		SpecNode = milang_ast_expose:declaration(Data),
		Spec = milang_ast:data(SpecNode),
		NameNode = milang_ast_spec:name(Spec),
		{_, NameComplex} = milang_ast:data(NameNode),
		FunName = identifier_as_string(NameComplex),
		{ok, FunName}
	end, IsSpec),
	case DahFunc of
		{ok, N} ->
			{true, [N, "/0"]};
		_ ->
			?LOG_DEBUG("Not exportable reason ~p of node ~p", [DahFunc, Node]),
			false
	end.

write_functions([], _WriteFun) ->
	ok;
write_functions([ AST | Tail], WriteFun) ->
	ok = write_function(AST, WriteFun),
	write_functions(Tail, WriteFun).

write_function(AST, WriteFun) ->
	Type = milang_ast:type(AST),
	write_function(Type, AST, WriteFun).

write_function({ok, binding}, AST, WriteFun) ->
	Data = milang_ast:data(AST),
	MatchNode = milang_ast_binding:match(Data),
	MatchData = milang_ast:data(MatchNode),
	case MatchData of
		{_, Name} when is_binary(Name) ->
			WriteFun([io_lib:format("'~s'() ->\n", [Name])]),
			Expression = milang_ast_binding:expression(Data),
			write_function_body(Expression, WriteFun);
		_ ->
			ok
	end;
write_function(_Type, AST, _WriteFun) ->
	?LOG_DEBUG("nyi writing the ast as function: ~p", [AST]),
	ok.

write_function_body(Node, WriteFun) ->
	Type = milang_ast:type(Node),
	Data = milang_ast:data(Node),
	write_function_body(Type, Data, WriteFun).

write_function_body({ok, call}, Call, WriteFun) ->
	Args = lists:map(fun(E) ->
		write_expression(E)
	end, milang_ast_call:args(Call)),
	StackBuilder = case milang_ast_call:function(Call) of
		{identifier_bound, #{ local := L, module := M}} ->
			io_lib:format("'~s':'~s'()", [M, L]);
		{identifier_bound, L} ->
			io_lib:format("'~s'()", [L]);
		Call ->
			""
	end,
	StackArgsJoined = lists:join($,, Args),
	ToWrite = io_lib:format("milang_curry:call(~s, [~s]).~n~n", [StackBuilder, StackArgsJoined]),
	WriteFun(ToWrite);
write_function_body({ok, function}, Function, WriteFun) ->
	Args = milang_ast_function:args(Function),
	Binds = milang_ast_function:binds(Function),
	Expression = milang_ast_function:expression(Function),
	FixedArgs = lists:map(fun extract_match/1, Args),
	JoinedArgs = lists:join($,, FixedArgs),

	PartialFixedBinds = lists:map(fun(B) ->
		Data = milang_ast:data(B),
		MatchNode = milang_ast_binding:match(Data),
		ExprNode = milang_ast_binding:expression(Data),
		{extract_match(MatchNode), write_expression(ExprNode)}
	end, Binds),
	FixedBinds = lists:map(fun({Match, Expr}) ->
		io_lib:format("~s = ~s", [Match, Expr])
	end, PartialFixedBinds),

	FixedExpr = write_expression(Expression),

	FixedBody = case FixedBinds of
		[] -> FixedExpr;
		_ -> lists:join($,, FixedBinds ++ [FixedExpr])
	end,
	FunChars = io_lib:format("fun(~s) -> ~s end", [JoinedArgs, FixedBody]),
	ToWrite = io_lib:format("milang_curry:stack(~s).~n~n", [FunChars]),
	WriteFun(ToWrite);

write_function_body(_Type, Expression, _WriteFun) ->
	?LOG_DEBUG("not yet implmented as function body: ~p", [Expression]),
	ok.

write_expression(Expression) ->
	Type = milang_ast:type(Expression),
	Data = milang_ast:data(Expression),
	write_expression(Type, Data).

%write_expression(infix_series, Data) ->
	% TODO change infix_series into infix_tree during compile step. This will
	% happen after the lexer pass, before the type checker. And because the
	% type checker happens before this step (or will, at least), it will
	% also be here.
write_expression({ok, infix_tree}, Tree) ->
	Notation = milang_ast_infix_tree:notation(Tree),
	Left = milang_ast_infix_tree:left(Tree),
	Right = milang_ast_infix_tree:right(Tree),
	FunctionChars = as_function_name(milang_ast_infix_notation:function(Notation)),
	StackBuilder = [FunctionChars, "()"],
	LeftChars = write_expression(Left),
	RightChars = write_expression(Right),
	curry_call(StackBuilder, [LeftChars, RightChars]);

write_expression({ok, call}, Call) ->
	Function = milang_ast_call:function(Call),
	Args = milang_ast_call:args(Call),
	StackBuilder = [as_function_name(Function), "()"],
	ArgsChars = lists:map(fun write_expression/1, Args),
	curry_call(StackBuilder, ArgsChars);

write_expression({ok, literal_string}, {literal_string, String}) ->
	io_lib:format("( ~p )", [ String ]);

write_expression({ok, literal_integer}, {literal_integer, N}) ->
	io_lib:format(" ( ~p ) ", [N]);

write_expression(_, Data) ->
	?LOG_DEBUG("not yet implemented as pure expression: ~p", [Data]),
	"".


curry_call(StackBuilderChars, Args) ->
	StackBuilder = ["( ", StackBuilderChars, " )"],
	WrappedArgs = lists:map(fun(C) ->
		["( ", C, " )"]
	end, Args),
	JoinedArgs = lists:join($,, WrappedArgs),
	ListedArgs = ["[ ", JoinedArgs, " ]"],
	[ "milang_curry:call( ", StackBuilder, " , ", ListedArgs, " )" ].

as_function_name({identifier_bound, #{ module := M, local := L}}) ->
	["'", M, "':'", L, "'"];
as_function_name({identifier_bound, L}) ->
	["'", L, "'"];
as_function_name({identifier_type, #{ module := M, local := L}}) ->
	["'", M, "':'", "'", L, "'"];
as_function_name({identifier_type, L}) ->
	["'", L, "'"];
as_function_name(#{ module := M, local := L}) ->
	["'", M, "':'", L, "'"];
as_function_name(L) when is_binary(L) ->
	["'", L, "'"];
as_function_name(Name) ->
	?LOG_DEBUG("Likely an invalid function name: ~p", [Name]),
	"'Core':'never'".

extract_match({identifier_bound, #{ module := M, local := L }}) ->
	extract_match(unicode:characters_to_binary(["V_", M, $., L]));
extract_match({identifier_bound, L}) ->
	extract_match(unicode:characters_to_binary(["V_", L]));
extract_match({identifier_ignored, _}) ->
	$_;
extract_match({identifier_type, #{ module := M, local := L}}) ->
	extract_match(unicode:characters_to_binary(["T_", M, $., L]));
extract_match({identifier_type, L}) ->
	extract_match(unicode:characters_to_binary(["T_", L]));
extract_match(Binary) when is_binary(Binary) ->
	{ok, BadBoyFinder} = re:compile("[^a-zA-Z0-9_]+", [unicode, ucp]),
	FirstRun = re:run(Binary, BadBoyFinder, []),
	mutilate_name(Binary, BadBoyFinder, FirstRun);
extract_match(Node) ->
	extract_match(milang_ast:data(Node)).

mutilate_name(Binary, _BadBoyFinder, nomatch) ->
	Binary;
mutilate_name(Binary, BadBoyFinder, {match, [{Start, ByteLen} | _]}) ->
	<<Before:Start/binary, Mutilatable:ByteLen/binary, Rest/binary>> = Binary,
	MutilateList = binary_to_list(Mutilatable),
	MutilatedList = lists:map(fun integer_to_binary/1, MutilateList),
	Mutilation = unicode:characters_to_binary([Before, MutilatedList, Rest]),
	NewMatch = re:run(Mutilation, BadBoyFinder, [{offest, Start}]),
	mutilate_name(Mutilation, BadBoyFinder, NewMatch).

%% @doc Given a term, become eager. If the term is a 0 arity function, it is call
%% and we loop through. Note that yes, this could indeed be an infinite loop.
%% If the result is a function, a milang_curry:stack() is returned. If the
%% result is anything, it is returned.
work(Fun) when is_function(Fun, 0) ->
	work(Fun());
work(Fun) when is_function(Fun) ->
	milang_curry:stack(Fun);
work(Term) ->
	Term.

%% @doc Given a milang ast node and scope, output the erlang code (as humans
%% would read it), to get the right result.
as_erlang(Node) ->
	as_erlang(Node, module).

as_erlang(Node, Scope) ->
	Type = milang_ast:type_simply(Node),
	as_erlang(Type, milang_ast:data(Node), Scope).

as_erlang(binding, Data, module) ->
	MatchNode = milang_ast_binding:match(Data),
	MatchType = milang_ast:type_simply(MatchNode),
	case MatchType of
		identifier_bound ->
			module_function(milang_ast:data(MatchNode), milang_ast_binding:expression(Data));
		_ ->
			{error, {invalid_scope, MatchType, module}}
	end;
as_erlang(Type, _Data, module) ->
	{error, {invalid_scope, module, Type}};
as_erlang(Type, _Data, Scope) ->
	{error, {unsupported_type, Scope, Type}}.

module_function({identifier_bound, Name}, ExpressionNode) ->
	Binding = as_erlang(ExpressionNode, function_body),
	NameStr = identifier_as_string(Name),
	Out = [io_lib:format("'~s'() ->\n", [NameStr]), Binding, $.],
	{ok, Out}.

identifier_as_string(#{ module := M, local := L}) ->
	[M, $., L];
identifier_as_string(L) ->
	L.

%
%			as_erlang(call, milang_ast:data(Node), Scope)
%			milang_ast_call:as_erlang(milang_ast:data(Node), Scope);
%		construct ->
%			milang_ast_construct:as_erlang(milang_ast:data(Node), Scope);
%		function ->
%			milang_ast_function:as_erlang(milang_ast:data(Node), Scope);
%		infix_series ->
%			milang_ast_infix_series:as_erlang(milang_ast:data(Node), Scope);
%		_ ->
%			{error, {nyi, expression, Node, Scope}}
%	end.
%
%
%
%
%
%
%as_erlang(Rec, _Scope = module) ->
%	MatchNode = match(Rec),
%	Expression = expression(Rec),
%	MatchType = milang_ast:type(MatchNode),
%	ExpressionType = milang_ast:type(Expression),
%	case MatchType of
%		identifier_bound ->
%			ExpressionChars = milang_ast_expression:as_erlang(milang_ast:data(Expression), module),
%			{identifier_bound, Name} = milang_ast:data(MatchNode),
%			FuncDefine = io_lib:format("'~s'() ->~n\n", [Name]),
%			{ok, [FuncDefine, ExpressionChars, "\n\n"]};
%		_ ->
%			{error, {nyi, as_erlang, Rec, module}}
%	end.
%
%





%	#declaration_function{ name = Name, args = Args, expression = MaybeInfixBody} = InfixAST#milang_ast.data,
%	{Body, InfixState} = case milang_infix_tree:from_ast(MaybeInfixBody) of
%		{ok, B} ->
%			{B, StatsPreTypeCheck};
%		{error, InfixTreeError} ->
%			{MaybeInfixBody, add_error(InfixAST#milang_ast.location, {invalid_expression, InfixTreeError, MaybeInfixBody}, StatsPreTypeCheck)}
%	end,
%	AST = milang_ast:transform_data(fun(R) ->
%		R#declaration_function{ expression = Body}
%	end, InfixAST),
%	State = type_checking(AST, InfixState),
%	Arity = length(Args),
%	{_, LocalName} = Name,
%	Exported = io_lib:format("'~s'() -> milang_curry:stack(fun ~s/~p).~n~n", [LocalName, LocalName, Arity]),
%	ok = write_to_outfile(State, Exported),
%	ok = ?LOG_DEBUG("ye old args: ~p", [Args]),
%	ArgsList = [milang_arg_to_erlang_arg(A) || A <- Args],
%	ArgsJoined = lists:join($,, ArgsList),
%	FunctionHead = io_lib:format("'~s'(~s) ->~n", [LocalName, ArgsJoined]),
%	ok = write_to_outfile(State, FunctionHead),
%	Frame = #stack_frame{ ast = AST, output_buffer = ".\n"},
%	compile(Body, [Frame | Stack], State);




% from milang_ast_function
%as_erlang(Data, _Scope = module) ->
	% TODO
	% how to turn let ln = System.Print.ln into valid usable erlang.

% from milang_compile.
%	ok = module_preamble(State),
%	Declarations = lists:reverse(State#compile_state.ast_output_buffer),
%	write_declarations(Declarations, State#compile_state.output_file_handle).
%
%module_preamble(NewState) ->
%	ModuleName = NewState#compile_state.module_name,
%	OutputFmt = "-module('~s').~n"
%	"-export([~s]).~n"
%	"-export_type([~s]).~n~n",
%	Functions = NewState#compile_state.function_exports,
%	Types = NewState#compile_state.type_exports,
%	FunctionExportsList = [io_lib:format("'~s'/0~n", [A]) || {_, A} <- Functions],
%	FunctionExprts = lists:join($,, FunctionExportsList),
%	TypeExportsList = [ io_lib:format("'~s'/0~n", [A]) || {_, A} <- Types],
%	TypeExports = lists:join($,, TypeExportsList),
%	Output = io_lib:format(OutputFmt, [ModuleName, FunctionExprts, TypeExports]),
%	write_to_outfile(NewState, Output).
%
%write_declarations([], _) ->
%	ok;
%write_declarations([ Dec | Tail], FileHandle) ->
%	ok = write_declaration(Dec, FileHandle),
%	write_declaration(Tail, FileHandle).
%
%write_declaration(Node, FileHandle) ->
%	case milang_ast:type(Node) of
%		binding ->
%			{ok, Chars} = milang_ast_binding:as_erlang(milang_ast:data(Node), module),
%			io:put_chars(FileHandle, Chars);
%		_ ->
%			?LOG_DEBUG("Not writing node ~p to outfile.", [Node])
%	end
