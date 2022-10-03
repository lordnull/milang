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
	write_functions(AST, milang_scope:new(), WriteFun).

module_declaration(AST) ->
	Filter = fun(Node) ->
		Type = milang_ast:type(Node),
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
		{error, only_expose} ->
			false;
		_ ->
			?LOG_DEBUG("Not exportable reason ~p of node ~p", [DahFunc, Node]),
			false
	end.

write_functions([], _Symbols, _WriteFun) ->
	ok;
write_functions([ AST | Tail], Symbols, WriteFun) ->
	NewSymbols = write_function(AST, Symbols, WriteFun),
	write_functions(Tail, NewSymbols, WriteFun).

write_function(AST, Symbols, WriteFun) ->
	?LOG_DEBUG("Starting write of node ~p", [AST]),
	Type = milang_ast:type(AST),
	write_function(Type, AST, Symbols, WriteFun).

write_function({ok, import}, _AST, Symbols, _WriteFun) ->
	Symbols;
write_function({ok, module}, _AST, Symbols, _WriteFun) ->
	Symbols;
write_function({ok, spec}, AST, Symbols, _WriteFun) ->
	Data = milang_ast:data(AST),
	Name = milang_ast_spec:name(Data),
	{ok, NewSymbols} = milang_scope:insert(Name, function, Symbols),
	NewSymbols;

write_function({ok, binding}, AST, Symbols, WriteFun) ->
	Data = milang_ast:data(AST),
	MatchNode = milang_ast_binding:match(Data),
	MatchData = milang_ast:data(MatchNode),
	case MatchData of
		{_, Name} when is_binary(Name) ->
			WriteFun([io_lib:format("'~s'() ->\n", [Name])]),
			{ok, NewSymbols} = milang_scope:insert(Name, function, Symbols),
			Expression = milang_ast_binding:expression(Data),
			ok = write_function_body(Expression, milang_scope:add_scope(NewSymbols), WriteFun),
			NewSymbols;
		{_, Name} ->
			case milang_scope:insert(Name, function, Symbols) of
				{ok, NewSymbols} ->
					NewSymbols;
				{error, Err} ->
					?LOG_ERROR("Could not add ~p to symbols table due to ~p", [MatchData, Err]),
					Symbols
			end
	end;
write_function({ok, type}, AST, Symbols, WriteFun) ->
	Data = milang_ast:data(AST),
	{_, Name} = NameData = milang_ast:data(milang_ast_type:name(Data)),
	case {is_map(Name), milang_ast_type:constructors(Data)} of
		{true, []} ->
			{ok, NewSymbols} = milang_scope:insert(Name, {type, false}, Symbols),
			NewSymbols;
		{true, ConstructorNodes} ->
			{ok, WithType} = milang_scope:insert(Name, {type, true}, Symbols),
			WithConstructors = lists:foldl(fun(ConstructorNode, Acc) ->
				Constructor = milang_ast:data(ConstructorNode),
				ConstructorNameNode = milang_ast_constructor:name(Constructor),
				{_, ConstructorName} = milang_ast:data(ConstructorNameNode),
				{ok, NewAcc} = milang_scope:insert(ConstructorName, {constructor, Name}, Acc),
				NewAcc
			end, WithType, ConstructorNodes),
			WithConstructors;
		{_, []} ->
			{_, Name} = NameData = milang_ast:data(milang_ast_type:name(Data)),
			TypeName = as_function_name(NameData),
			{ok, NewSymbols} = milang_scope:insert(Name, {type, false}, Symbols),
			ArgNodes = milang_ast_type:args(Data),
			ArgNames = lists:map(fun(ArgNode) ->
				extract_match(milang_ast:data(ArgNode))
			end, ArgNodes),
			JoinedArgs = lists:join(", ", ArgNames),
			ResultTuple = type_tuple(TypeName, ArgNames),
			Fun = io_lib:format("fun( ~s ) -> ~s end~n", [JoinedArgs, ResultTuple]),
			FunctionBody = io_lib:format("milang_curry:stack(~s).~n", [Fun]),
			Function = io_lib:format("~s() -> ~s", [TypeName, FunctionBody]),
			WriteFun(Function),
			NewSymbols;
		{_, Constructors} ->
			WithConstructors = write_constructors(Constructors, Name, Symbols, WriteFun),
			{ok, NewSymbols} = milang_scope:insert(Name, {type, true}, WithConstructors),
			NewSymbols
	end;

write_function({ok, alias}, AST, Symbols, WriteFun) ->
	Data = milang_ast:data(AST),
	% TODO handle the multiple whys an alias can manifest.
	?LOG_ERROR("write and alias, god! ~p", [Data]),
	NameNode = milang_ast_alias:name(Data),
	LocalNameRaw = milang_ast:data(NameNode),
	LocalNameRes = (fun
		({identifier_type, L}) when is_map(L) ->
			'Result':'Err'(cannot_alias_to_remote_name);
		({identifier_type, L}) when is_binary(L) ->
			'Result':'Ok'(L);
		(Wut) ->
			?LOG_ERROR("Cannot alias to given name ~p", [Wut]),
			'Result':'Err'({cannot_alias_with_name, Wut})
	end)(LocalNameRaw),
	?LOG_DEBUG("LocalNameRes: ~p", [LocalNameRes]),
	OriginalNode = milang_ast_alias:original(Data),
	OriginalFoundRes = lookup_for_alias(OriginalNode, Symbols),
	?LOG_DEBUG("OriginalFoundRes: ~p", [OriginalFoundRes]),
	OriginalRes = 'Result':map(fun(SymbolType) ->
		{milang_ast:data(OriginalNode), SymbolType}
	end, OriginalFoundRes),
	?LOG_DEBUG("OriginalRes: ~p", [OriginalRes]),
	ResultConstructors = 'Result':and_then(fun(OriginalFound) ->
		{OriginalData, OriginalSymbol} = OriginalFound,
		OriginalNodeType = milang_ast:type_simply(OriginalNode),
		case {OriginalNodeType, OriginalSymbol} of
			{concrete, {type, false}} ->
				'Result':'Ok'([milang_ast_concrete:name(OriginalData)]);
			{concrete, {type, true}} ->
				OriginalNameNode = milang_ast_concrete:name(OriginalData),
				{_, OriginalName} = milang_ast:data(OriginalNameNode),
				Constructors = milang_scope:foldl(fun(K, Value, Acc) ->
					case Value of
						{constructor, OriginalName} ->
							[K | Acc];
						_ ->
							Acc
					end
				end, [], Symbols),
				'Result':'Ok'(Constructors);
			Wut ->
				?LOG_ERROR("Cannot import consructors since I don't know the type: ~p~n"
					"OriginalData: ~p~n"
					"OriginalSymbol: ~p"
					, [Wut, OriginalData, OriginalSymbol]),
				'Result':'Err'(unsupported_constructor_aliased)
		end
	end, OriginalRes),
	?LOG_DEBUG("ResultConstructors: ~p", [ResultConstructors]),
	_ = 'Result':map(fun(Constructors) ->
		lists:foreach(fun(Constructor) ->
			case Constructor of
				#{ module := M, local := Local } ->
					Text = io_lib:format("'~s'() -> '~s':'~s'().~n~n", [Local, M, Local]),
					WriteFun(Text);
				_ ->
					?LOG_ERROR("Not writing the constructor ~p", [Constructor]),
					ok
			end
		end, Constructors)
	end, ResultConstructors),
	SymbolsTableWithOriginal = 'Result':and_then_n([LocalNameRes, OriginalRes], fun(LocalName, Original) ->
		{OriginalData, _} = Original,
		OriginalName = milang_ast_concrete:name(OriginalData),
		milang_scope:insert(LocalName, {alias, OriginalName}, Symbols)
	end),
	SymbolsWithAllRes = 'Result':and_then_n([ResultConstructors, SymbolsTableWithOriginal], fun(Constructors, InSymbols) ->
		'Result':foldl(fun(Constructor, TableAcc) ->
			case Constructor of
				#{ local := L } ->
					milang_scope:insert(L, Constructor, TableAcc);
				_ ->
					'Result':'Ok'(TableAcc)
			end
		end, InSymbols, Constructors)
	end),
	case SymbolsWithAllRes of
		{error, Err} ->
			?LOG_ERROR("Writing alias failed due to ~p.~n"
				"    AST: ~p~n"
				"    Symbols: ~p"
				, [Err, AST, Symbols]),
			Symbols;
		{ok, NewSymbols} ->
			NewSymbols
	end;

write_function(_Type, AST, Symbols, _WriteFun) ->
	?LOG_ERROR("nyi writing the ast as function: ~p", [AST]),
	Symbols.

lookup_for_alias(OriginalNode, Symbols) ->
	OriginalNodeType = milang_ast:type_simply(OriginalNode),
	case OriginalNodeType of
		concrete ->
			OriginalData = milang_ast:data(OriginalNode),
			OriginalNameNode = milang_ast_concrete:name(OriginalData),
			{_, OriginalName} = milang_ast:data(OriginalNameNode),
			milang_scope:lookup(OriginalName, Symbols);
		_ ->
			{error, unsupported_node_type}
	end.

%import_module([ Node | Tail], Symbols) ->
%	Type = milang_ast:type_simply(Node),
%	NewSymbols = import_module(Type, milang_ast:data(Node), Symbols),
%	import_module(Tail, NewSymbols);
%
%import_module(undefined, Symbols) ->
%	?LOG_ERROR("wut? no import ast!"),
%	Symbols;
%
%import_module(ImportData, Symbols) ->
%	ImportAST = milang_ast_import:imported_ast(ImportData),
%	import_module(ImportAST, Symbols).
%
%import_module(Type, _Data, Symbols) ->
%	?LOG_ERROR("unknonw type ~p for import", [Type]),
%	Symbols.

write_function_body(Node, Symbols, WriteFun) ->
	Type = milang_ast:type(Node),
	Data = milang_ast:data(Node),
	write_function_body(Type, Data, Symbols, WriteFun).

write_function_body({ok, call}, Call, Symbols, WriteFun) ->
	Args = lists:map(fun(E) ->
		write_expression(E, Symbols)
	end, milang_ast_call:args(Call)),
	FunctionNameNode = milang_ast_call:function(Call),
	StackBuilder = case milang_ast:data(FunctionNameNode) of
		{identifier_bound, #{ local := L, module := M}} ->
			io_lib:format("'~s':'~s'()", [M, L]);
		{identifier_bound, L} ->
			{ok, NameType} = milang_scope:lookup(L, Symbols),
			case NameType of
				function ->
					io_lib:format("'~s'()", [L]);
				match ->
					FunName = extract_match({identifier_bound, L}),
					io_lib:format("~s()", [FunName])
			end;
		Call ->
			?LOG_ERROR("Silently skipping this call: ~p", [Call]),
			""
	end,
	StackArgsJoined = lists:join($,, Args),
	ToWrite = io_lib:format("milang_curry:call(~s, [~s]).~n~n", [StackBuilder, StackArgsJoined]),
	WriteFun(ToWrite);
write_function_body({ok, function}, Function, Symbols, WriteFun) ->
	Args = milang_ast_function:args(Function),
	SymbolsWithArgs = lists:foldl(fun(ArgNode, SymbolAcc) ->
		ArgData = milang_ast:data(ArgNode),
		case ArgData of
			{identifier_bound, Name} ->
				{ok, NewScope} = milang_scope:insert(Name, match, SymbolAcc),
				NewScope;
			{identifier_ignored, _} ->
				SymbolAcc;
			_ ->
				?LOG_ERROR("Skipping adding arg data ~p to symbols", [ArgData]),
				SymbolAcc
		end
	end, Symbols, Args),
	Binds = milang_ast_function:binds(Function),
	Expression = milang_ast_function:expression(Function),
	FixedArgs = lists:map(fun extract_match/1, Args),
	JoinedArgs = lists:join($,, FixedArgs),

	{BindsStrs, SymbolsWithBinds} = write_bindings(Binds, SymbolsWithArgs),

	FixedBinds = lists:join(",\n", BindsStrs),
	FixedExpr = write_expression(Expression, SymbolsWithBinds),

	FixedBody = case FixedBinds of
		[] -> FixedExpr;
		_ -> lists:join($,, FixedBinds ++ [FixedExpr])
	end,
	FunChars = io_lib:format("fun(~s) -> ~s end", [JoinedArgs, FixedBody]),
	ToWrite = io_lib:format("milang_curry:stack(~s).~n~n", [FunChars]),
	WriteFun(ToWrite);

write_function_body(_Type, Expression, _Symbols, _WriteFun) ->
	% TODO implement writing a match expression (and other things) out.
	?LOG_ERROR("not yet implmented as function body: ~p", [Expression]),
	ok.

write_bindings(Binds, Symbols) ->
	lists:mapfoldl(fun(B, SymbolsAcc) ->
		write_binding(B, SymbolsAcc)
	end, Symbols, Binds).

write_binding(Binding, Symbols) ->
	Data = milang_ast:data(Binding),
	MatchNode = milang_ast_binding:match(Data),
	ExprNode = milang_ast_binding:expression(Data),
	MatchStr = extract_match(MatchNode),
	ExprStr = write_expression(ExprNode, Symbols),
	Str = io_lib:format("~s = ~s", [MatchStr, ExprStr]),
	{ok, NewSymbols} = case milang_ast:data(MatchNode) of
		{identifier_bound, N} ->
			milang_scope:insert(N, match, Symbols);
		Wut ->
			?LOG_ERROR("Didn't know how to write the binding ~p", [Wut]),
			{ok, Symbols}
	end,
	{Str, NewSymbols}.

write_expression(Expression, Symbols) ->
	Type = milang_ast:type(Expression),
	Data = milang_ast:data(Expression),
	write_expression(Type, Data, Symbols).

%write_expression(infix_series, Data) ->
	% TODO change infix_series into infix_tree during compile step. This will
	% happen after the lexer pass, before the type checker. And because the
	% type checker happens before this step (or will, at least), it will
	% also be here.
write_expression({ok, infix_tree}, Tree, Symbols) ->
	Notation = milang_ast_infix_tree:notation(Tree),
	Left = milang_ast_infix_tree:left(Tree),
	Right = milang_ast_infix_tree:right(Tree),
	FunctionChars = as_function_name(milang_ast_infix_notation:function(Notation)),
	StackBuilder = [FunctionChars, "()"],
	LeftChars = write_expression(Left, Symbols),
	RightChars = write_expression(Right, Symbols),
	curry_call(StackBuilder, [LeftChars, RightChars]);

write_expression({ok, call}, Call, Symbols) ->
	FunctionNode = milang_ast_call:function(Call),
	TrueFunctionName = case milang_ast:data(FunctionNode) of
		{identifier_bound, N} -> N;
		{identifier_type, N} -> N;
		N when is_binary(N) -> N
	end,
	Args = milang_ast_call:args(Call),
	Symbol = milang_scope:lookup(TrueFunctionName, Symbols),
	case {Symbol, Args} of
		{{ok, match}, []} ->
			extract_match(FunctionNode);
		_ ->
			StackBuilder = [as_function_name(milang_ast:data(FunctionNode)), "()"],
			ArgsChars = lists:map(fun(ArgNode) ->
				write_expression(ArgNode, Symbols)
			end, Args),
			curry_call(StackBuilder, ArgsChars)
	end;

write_expression({ok, literal_string}, {literal_string, String}, _Symbols) ->
	io_lib:format("( ~p )", [ String ]);

write_expression({ok, literal_integer}, {literal_integer, N}, _Symbols) ->
	io_lib:format(" ( ~p ) ", [N]);

write_expression({ok, match}, Match, Symbols) ->
	ExpressionNode = milang_ast_match:expression(Match),
	ClauseNodes = milang_ast_match:clauses(Match),
	Expression = write_expression(ExpressionNode, Symbols),
	Clauses = lists:map(fun(ClauseNode) ->
		write_expression(ClauseNode, Symbols)
	end, ClauseNodes),
	JoinedClauses = lists:join(";\n", Clauses),
	io_lib:format("case ( ~s ) of~n~s~nend~n", [Expression, JoinedClauses]);

write_expression({ok, match_clause}, Clause, Symbols) ->
	Match = milang_ast_match_clause:match(Clause),
	Binds = milang_ast_match_clause:binds(Clause),
	Expression = milang_ast_match_clause:expression(Clause),
	MatchStr = extract_match(Match),
	{ok, SymbolsWithMatch} = add_symbols_from_match(milang_ast:data(Match), milang_scope:add_scope(Symbols)),
	{BindsStrs, SymbolsWithBinds} = write_bindings(Binds, SymbolsWithMatch),
	ExpressionStr = write_expression(Expression, SymbolsWithBinds),
	JoinedBindings = lists:join(",\n", BindsStrs),
	io_lib:format("    ~s ->~n~s~n~s", [MatchStr, JoinedBindings, ExpressionStr]);

write_expression({ok, literal_list}, ListLiteral, Symbols) ->
	{literal_list, ElementNodes} = ListLiteral,
	ElementStrs = lists:map(fun(E) ->
		write_expression(E, Symbols)
	end, ElementNodes),
	Joined = lists:join("\n, ", ElementStrs),
	["[ ", Joined, " ]"];

write_expression({ok, identifier_bound}, {identifier_bound, Name}, Symbols) ->
	case milang_scope:lookup(Name, Symbols) of
		{error, not_found} ->
			?LOG_ERROR("Did not find ~s in the symbols table ~p", [Name, Symbols]),
			"";
		{ok, function} ->
			curry_call([$', Name, $', "()"], []);
		{ok, match} ->
			extract_match({identifier_bound, Name});
		Else ->
			?LOG_ERROR("unsure how to write identifier_bound ~s with type ~p as an expression.", [Name, Else]),
			""
	end;

write_expression(_, Data, _Symbols) ->
	?LOG_ERROR("not yet implemented as pure expression: ~p", [Data]),
	"".

write_constructors(Constructors, TypeName, Symbols, WriteFun) ->
	lists:foldl(fun(C, SymbolAcc) ->
		write_constructor(C, TypeName, SymbolAcc, WriteFun)
	end, Symbols, Constructors).

write_constructor(ConstructorNode, TypeName, Symbols, WriteFun) ->
	Data = milang_ast:data(ConstructorNode),
	NameNode = milang_ast_constructor:name(Data),
	ArgNodes = milang_ast_constructor:args(Data),
	Name = as_function_name(milang_ast:data(NameNode)),
	{_, TrueName} = milang_ast:data(NameNode),
	{ok, NewSymbols} = milang_scope:insert(TrueName, {constructor, TypeName}, Symbols),
	Args = lists:map(fun(ArgNode) ->
		case milang_ast:type_simply(ArgNode) of
			identifier_bound ->
				extract_match(milang_ast:data(ArgNode));
			identifier_type ->
				extract_match(milang_ast:data(ArgNode));
			concrete ->
				concrete_as_variable(ArgNode)
		end
	end, ArgNodes),
	JoinedArgs = lists:join(", ", Args),
	Tuple = type_tuple(Name, Args),
	Fun = io_lib:format("fun( ~s ) -> ~s end", [JoinedArgs, Tuple]),
	Curried = curry_call(Fun, []),
	FullFunction = io_lib:format("~s() -> ~s .~n~n", [Name, Curried]),
	WriteFun(FullFunction),
	NewSymbols.

type_tuple(TypeName, []) ->
	TypeName;
type_tuple(TypeName, ArgNames) ->
	Joined = lists:join(", ", [TypeName | ArgNames]),
	io_lib:format("{ ~s }", [Joined]).

concrete_as_variable(Node) ->
	{Line, Col} = milang_ast:location(Node),
	Data = milang_ast:data(Node),
	NameNode = milang_ast_concrete:name(Data),
	NameNodeBase = extract_match(milang_ast:data(NameNode)),
	io_lib:format("~s_~p_~p", [NameNodeBase, Line, Col]).

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
	["'", M, "':'", L, "'"];
as_function_name({identifier_type, L}) ->
	["'", L, "'"];
as_function_name(#{ module := M, local := L}) ->
	["'", M, "':'", L, "'"];
as_function_name(L) when is_binary(L) ->
	["'", L, "'"];
as_function_name(Node) ->
	as_function_name(milang_ast:data(Node)).

extract_match({identifier_bound, #{ module := M, local := L }}) ->
	extract_match(unicode:characters_to_binary(["V_", M, $., L]));
extract_match({identifier_bound, L}) ->
	extract_match(unicode:characters_to_binary(["V_", L]));
extract_match({identifier_ignored, _}) ->
	[$_];
extract_match({identifier_type, #{ module := M, local := L}}) ->
	extract_match(unicode:characters_to_binary(["T_", M, $., L]));
extract_match({identifier_type, <<"True">>}) ->
	<<"'true'">>;
extract_match({identifier_type, <<"False">>}) ->
	<<"'false'">>;
extract_match({identifier_type, L}) ->
	extract_match(unicode:characters_to_binary(["T_", L]));
extract_match(Binary) when is_binary(Binary) ->
	{ok, BadBoyFinder} = re:compile("[^a-zA-Z0-9_]+", [unicode, ucp]),
	FirstRun = re:run(Binary, BadBoyFinder, []),
	mutilate_name(Binary, BadBoyFinder, FirstRun);
extract_match({match_type, ConstructorNameNode, ConstructorMatches}) ->
	{_, ConstructorName} = milang_ast:data(ConstructorNameNode),
	if
		is_binary(ConstructorName) ->
			% it's a local constructor, so we need to build the match manually.
			NameMatch = extract_match(ConstructorNameNode),
			case ConstructorMatches of
				[] ->
					NameMatch;
				_ ->
					Args = lists:map(fun extract_match/1, ConstructorMatches),
					JoinedAll = lists:join(", ", [NameMatch | Args]),
					io_lib:format("{ ~s }", [JoinedAll])
			end;
		is_map(ConstructorName) ->
			% hopefully the compiler has built the beam we need to actually do the
			% match constructor for us. I'm not worried about using up the atom space
			% because this is not code meant to be on a long running server.
			#{ local := LocalStr, module := ModuleStr } = ConstructorName,
			Local = binary_to_atom(LocalStr, utf8),
			Module = binary_to_atom(ModuleStr, utf8),
			Args = lists:map(fun extract_match/1, ConstructorMatches),
			RawTuple = erlang:apply(Module, Local, Args),
			[HeadName | ArgStrs ] = tuple_to_list(RawTuple),
			HeadStr = io_lib:format("'~s'", [HeadName]),
			JoinedAll = lists:join(", ", [HeadStr | ArgStrs]),
			io_lib:format("{ ~s }", [JoinedAll])
	end;
extract_match({match_list, []}) ->
	"[]";
extract_match({match_list, List}) ->
	Strs = lists:map(fun extract_match/1, List),
	Joined = lists:join(", ", Strs),
	[$[, Joined, $]];
extract_match({match_list_head, HeadList, Tail}) ->
	Strs = lists:map(fun extract_match/1, HeadList),
	TailStr = extract_match(Tail),
	JoinedHead = lists:join(", ", Strs),
	[$[, JoinedHead, " | ", TailStr, $]];
extract_match({literal_integer, N}) ->
	io_lib:format(" ~p ", [N]);
extract_match({literal_string, S}) ->
	io_lib:format(" <<\"~s\">> ", [S]);
extract_match({literal_float, F}) ->
	io_lib:format(" ~p ", [F]);

extract_match(Node) ->
	extract_match(milang_ast:data(Node)).

mutilate_name(Binary, _BadBoyFinder, nomatch) ->
	Binary;
mutilate_name(Binary, BadBoyFinder, {match, [{Start, ByteLen} | _]}) ->
	<<Before:Start/binary, Mutilatable:ByteLen/binary, Rest/binary>> = Binary,
	MutilateList = binary_to_list(Mutilatable),
	MutilatedList = lists:map(fun integer_to_binary/1, MutilateList),
	Mutilation = unicode:characters_to_binary([Before, MutilatedList, Rest]),
	NewMatch = re:run(Mutilation, BadBoyFinder, [{offset, Start}]),
	mutilate_name(Mutilation, BadBoyFinder, NewMatch).

add_symbols_from_match({identifier_bound, L}, Scope) ->
	case milang_scope:insert(L, match, Scope) of
		{error, {shadowing, _, match}} ->
			{ok, Scope};
		{error, {shadowing, _, NotMatch}} ->
			?LOG_ERROR("Somehow identifier ~p got pegged as both a match and a ~p", [L, NotMatch]),
			{ok, Scope};
		Ok ->
			Ok
	end;
add_symbols_from_match({identifier_ignored, _}, Scope) ->
	{ok, Scope};
add_symbols_from_match({literal_integer, _}, Scope) ->
	{ok, Scope};
add_symbols_from_match({literal_string, _}, Scope) ->
	{ok, Scope};
add_symbols_from_match({literal_float, _}, Scope) ->
	{ok, Scope};
add_symbols_from_match({match_type, _ConstrutorName, Matches}, Scope) ->
	lists:foldl(fun(Match, {ok, ScopeAcc}) ->
		add_symbols_from_match(Match, ScopeAcc)
	end, {ok, Scope}, Matches);
add_symbols_from_match({match_list_head, HeadList, Tail}, Scope) ->
	{ok, HeadScope} = lists:foldl(fun(Match, {ok, ScopeAcc}) ->
		add_symbols_from_match(Match, ScopeAcc)
	end, {ok, Scope}, HeadList),
	add_symbols_from_match(Tail, HeadScope);
add_symbols_from_match({match_list, List}, Scope) ->
	lists:foldl(fun(Match, {ok, ScopeAcc}) ->
		add_symbols_from_match(Match, ScopeAcc)
	end, {ok, Scope}, List);
add_symbols_from_match(Node, Scope) ->
	add_symbols_from_match(milang_ast:data(Node), Scope).

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
