%% @doc Take an ast and make it real.
-module(milang_compile).

-include_lib("kernel/include/file.hrl").
-include("milang_ast.hrl").
-include("milang_constant_import.hrl").

-export(
	[ compile/2
	]).

-type option()
	:: {input_file_name, binary()}
	|  {output_file_name, binary()}
	|  {search_dirs, [ binary() ]}
	|  {work_dir, binary()}
	|  {compile_mode, program | header | module}
	.
-type options() :: [ option() ].

-export_type([options/0, option/0]).

-record(compile_state, {
	lookup_table = milang_type_validation:new(),
	search_dirs = [],
	work_dir = "./milang-work-dir",
	input_file_name,
	output_file_handle,
	append_output_buffer = [],
	errors = [],
	module_name,
	function_exports = [],
	type_exports = [],
	support_modules = [],
	compile_mode = program
}).

-record(stack_frame, {
	output_buffer = [],
	ast
	}).

always_import_ast() ->
	{ok, AST, <<>>} = parse:it(?DEFAULT_IMPORTS, milang_p:module()),
	AST.

-spec compile(nonempty_list(milang_ast:ast_node()), options()) -> ok.
compile([], Options) when is_list(Options) ->
	error(no_ast);

compile(AST, Options) when is_list(Options) ->
	_ = put(silence, proplists:get_value(silence, Options, false)),
	InputFile = proplists:get_value(input_file_name, Options),
	ok = log("Inputfile: ~s", [InputFile]),
	OutputFile = proplists:get_value(output_file_name, Options),
	ok = log("Outputfile: ~s", [OutputFile]),
	SearchDirs = proplists:get_value(search_dirs, Options, []),
	ok = log("SearchDirs: ~p", [SearchDirs]),
	WorkDir = proplists:get_value(work_dir, Options, <<"./milang-work-dir">>),
	ok = make_dir_or_die(WorkDir),
	ok = log("WorkDir: ~s", [WorkDir]),
	HeadersDir = filename:join([WorkDir, "headers"]),
	ok = make_dir_or_die(HeadersDir),
	ok = write_system_headers(HeadersDir),
	ErlDir = filename:join([WorkDir, "src_erl"]),
	ok = make_dir_or_die(ErlDir),
	ScratchFileName = filename:join([ErlDir, filename:basename((OutputFile ++ ".erl"))]),
	ScratchFileHandle = case file:open(ScratchFileName, [binary, write]) of
		{ok, H} -> H;
		{error, ScratchError} ->
			error({scrach_file_open_failed, ScratchFileName, ScratchError})
	end,
	ok = log("ScratchFileName: ~s", [ScratchFileName]),
	Mode = proplists:get_value(compile_mode, Options, program),
	LintErrors = milang_lint:it(AST, Mode),
	CompileState = #compile_state{
		input_file_name = InputFile,
		output_file_handle = ScratchFileHandle,
		search_dirs = SearchDirs,
		work_dir = WorkDir,
		errors = LintErrors,
		compile_mode = Mode
	},
	FinalState = compile(AST, [], CompileState),
	ok = io:format("Final state: ~p~n", [FinalState]),
	ArchiveOptions =
		[{erl_src_dir, ErlDir}
		,{beam_dir, filename:join([WorkDir, "ebin"])}
		,{support_modules, [ FinalState#compile_state.module_name | FinalState#compile_state.support_modules]}
		,{main_file, FinalState#compile_state.module_name}],
	ok = milang_archive:build(OutputFile, ArchiveOptions).

log(BaseFmt, Args) ->
	io:format(BaseFmt ++ "~n", Args).

make_dir_or_die(Dir) ->
	case file:make_dir(Dir) of
		ok ->
			ok;
		{error, eexist} ->
			ok;
		{error, Error} ->
			error({dir_not_usable, Error})
	end.

write_system_headers(WorkDir) ->
	write_system_headers(WorkDir, code:priv_dir(milang)).

write_system_headers(_WorkDir, {error, Error}) ->
	ok = log("Skipping writing system headers from priv dir, priv dir could not be opened: ~p", [Error]),
	ok;
write_system_headers(WorkDir, PrivDir) ->
	ok = log("priv dir: ~p", [PrivDir]),
	SysConfigHeaders = filelib:wildcard(PrivDir ++ "/**/*.milang-header"),
	ok = log("sys headers: ~p", [SysConfigHeaders]),
	ok = lists:foreach(fun(Source) ->
		ok = maybe_copy_system_header(Source, WorkDir)
	end, SysConfigHeaders).

maybe_copy_system_header(Source, WorkDir) ->
	SourceBase = filename:basename(Source),
	Dest = filename:join(WorkDir, SourceBase),
	SourceCtime = read_ctime_or_die(Source),
	DestCtime = read_ctime(Dest),
	if
		SourceCtime > DestCtime ->
			{ok, _} = R = file:copy(Source, Dest),
			log("Copy complete.~n"
				"    Source: ~p~n"
				"    Dest: ~p~n"
				"    FullR: ~p"
				, [Source, Dest, R]),
			ok;
		true ->
			log("Skipping copy.~n"
				"    Source: ~p~n"
				"    Dest: ~p~n"
				"    SrouceCtime: ~p~n"
				"    DestCtime: ~p"
				, [Source, Dest, SourceCtime, DestCtime]),
			ok
	end.

read_ctime(Filename) ->
	case file:read_file_info(Filename, [{time, posix}]) of
		{ok, Fileinfo} ->
			Fileinfo#file_info.ctime;
		_Error ->
			0
	end.

read_ctime_or_die(Filename) ->
	case file:read_file_info(Filename, [{time, posix}]) of
		{ok, Fileinfo} ->
			Fileinfo#file_info.ctime;
		{error, Error} ->
			error({unable_to_get_file_info, Filename, Error})
	end.

compile([], [], #compile_state{ errors = []} = State) ->
	State;
compile([], [], #compile_state{ errors = Errors}) ->
	error(Errors);
compile([], [AST | NewStack], State) ->
	compile(AST, NewStack, State);
compile([AST | Tail], Stack, State) ->
	compile(AST, [Tail | Stack], State);
compile(#stack_frame{ ast = _AST, output_buffer = Bytes}, [Tail | NewStack], #compile_state{errors = []} = State) ->
	ok = write_to_outfile(State, Bytes),
	compile(Tail, NewStack, State);
compile(#stack_frame{}, [Tail | NewStack], State) ->
	compile(Tail, NewStack, State);
compile(#milang_ast{ type = declaration_module } = Node, Stack, CompileState) ->
	#{ name := Name, exposing := Exports} = Node#milang_ast.data,
	NewState = declaration_module(Name, Exports, CompileState),
	ModuleName = NewState#compile_state.module_name,
	Functions = NewState#compile_state.function_exports,
	Types = NewState#compile_state.type_exports,
	OutputFmt = "-module('~s').~n"
	"-export([~s]).~n"
	"-export_type([~s]).~n~n",
	FunctionExportsList = [io_lib:format("'~s'/0~n", [milang_ast:data(A)]) || A <- Functions],
	FunctionExprts = lists:join($,, FunctionExportsList),
	TypeExportsList = [ io_lib:format("'~s'/0~n", [milang_ast:data(A)]) || A <- Types],
	TypeExports = lists:join($,, TypeExportsList),
	Output = io_lib:format(OutputFmt, [ModuleName, FunctionExprts, TypeExports]),
	ok = write_to_outfile(NewState, Output),
	DefaultImports = always_import_ast(),
	compile(DefaultImports, Stack, NewState);
compile(#milang_ast{ type = declaration_spec } = AST, Stack, State) ->
	NewState = type_checking(AST, State),
	compile([], Stack, NewState);
compile(#milang_ast{ type = declaration_function } = InfixAST, Stack, StatsPreTypeCheck) ->
	#{ name := Name, args := Args, expression := MaybeInfixBody} = InfixAST#milang_ast.data,
	{Body, InfixState} = case milang_infix_tree:from_ast(MaybeInfixBody) of
		{ok, B} ->
			{B, StatsPreTypeCheck};
		{error, InfixTreeError} ->
			{MaybeInfixBody, add_error(InfixAST#milang_ast.location, {invalid_expression, InfixTreeError, MaybeInfixBody}, StatsPreTypeCheck)}
	end,
	AST = InfixAST#milang_ast{ data = maps:put(expression, Body, InfixAST#milang_ast.data)},
	State = type_checking(AST, InfixState),
	Arity = length(Args),
	LocalName = Name#milang_ast.data,
	Exported = io_lib:format("'~s'() -> milang_curry:stack(fun ~s/~p).~n~n", [LocalName, LocalName, Arity]),
	ok = write_to_outfile(State, Exported),
	ok = log("ye old args: ~p", [Args]),
	ArgsList = [milang_arg_to_erlang_arg(A) || A <- Args],
	ArgsJoined = lists:join($,, ArgsList),
	FunctionHead = io_lib:format("'~s'(~s) ->~n", [LocalName, ArgsJoined]),
	ok = write_to_outfile(State, FunctionHead),
	Frame = #stack_frame{ ast = AST, output_buffer = ".\n"},
	compile(Body, [Frame | Stack], State);
compile(#milang_ast{ type = declaration_import } = Dec, Stack, State) ->
	#milang_ast{ location = Location, data = Data} = Dec,
	#{ name := Module } = Data,
	case find_module(Module, State#compile_state.work_dir, State#compile_state.search_dirs) of
		{error, _} ->
			NewState = add_error(Location, {cannot_find_module, Module}, State),
			compile([], Stack, NewState);
		{ok, recompiled} ->
			% I know I'm risking an infite loop here, but for now I
			% want the header file reading to be the source of truth.
			compile(Dec, Stack, State);
		{ok, HeaderFile} ->
			Table = State#compile_state.lookup_table,
			{ok, Header} = milang_parse:file(HeaderFile),
			{ok, NewTable} = milang_type_validation:validate_list(Header, Table),
			{ok, TableWithImport} = milang_type_validation:validate_list([Dec], NewTable),
			OldSupport = State#compile_state.support_modules,
			NewSupport = [ Module | OldSupport ],
			compile([], Stack, State#compile_state{ lookup_table = TableWithImport, support_modules = NewSupport})
	end;
compile(#milang_ast{ type = expression_call } = AST, Stack, State) ->
	#{ name := CallNameAST, args := Args} = AST#milang_ast.data,
	{ModPart, FuncPart} = case CallNameAST of
		#milang_ast{ type = function_name_remote, data = Data} ->
			#{ name := Name, module := ModuleName } = Data,
			{[$', atom_to_binary(ModuleName), $', $:], Name};
		#milang_ast{ type = function_name_local, data = LocalName } ->
			case milang_type_validation:resolve_name({local, LocalName}, State#compile_state.lookup_table) of
				{ok, Resolved} ->
					function_name_to_mf(Resolved);
				{error, notfound} ->
					log("Could not resolve ~s, assuming it's a local call.", [LocalName]),
					{"", LocalName}
			end
	end,
	CommaFrame = #stack_frame{ast = AST, output_buffer = ", "},
	JoinedArgs = lists:join(CommaFrame, Args),
	FinalFrame = #stack_frame{ast = AST, output_buffer = ")\n"},
	NewStack = JoinedArgs ++ [ FinalFrame] ++ Stack,
	Chars = io_lib:format("~s'~s'(", [ModPart, FuncPart]),
	ok = write_to_outfile(State, Chars),
	compile([], NewStack, State);
compile(#milang_ast{ type = literal_string } = Node, Stack, State) ->
	String = Node#milang_ast.data,
	Chars = io_lib:format("<<\"~s\">>", [String]),
	ok = write_to_outfile(State, Chars),
	compile([], Stack, State);
compile(#milang_ast{ type = literal_integer } = Node, Stack, State) ->
	Int = Node#milang_ast.data,
	Chars = integer_to_binary(Int),
	ok = write_to_outfile(State, Chars),
	compile([], Stack, State);
compile(#milang_ast{ type = expression } = Node, Stack, State) ->
	Location = Node#milang_ast.location,
	case milang_infix_tree:from_ast(Node) of
		{ok, FixedAst} ->
			compile(FixedAst, Stack, State);
		{error, Error} ->
			NewState = add_error(Location, {invalid_expression, Error}, State),
			compile([], Stack, NewState)
	end;


%compile({call, _, _, _} = AST, Stack, State) ->
%	NewState = compile_call(AST, State),
%	compile([], Stack, NewState);
%compile({function_name_local, _, _}, Stack, State) ->
%	compile([], Stack, State);
%compile({function_name_remote, _, _, _}, Stack, State) ->
%	compile([], Stack, State);
compile(Wut, Stack, CompileState) ->
	error({nyi, Wut, Stack, CompileState}).

type_checking(AST, State) ->
	case milang_type_validation:validate_list([AST], State#compile_state.lookup_table) of
		{ok, NewTable} ->
			State#compile_state{ lookup_table = NewTable };
		{error, T} ->
			add_error(AST#milang_ast.location, {type_error, T}, State)
	end.

%add_alias(Name, TruePath, State) ->
%	add_to_lookup_table(Name, {alias, TruePath, Name}, State).

% TODO start putting out a function call.
%compile_call(Call, State) ->
%	compile_call(Call, [], State).
%
%compile_call([], [], State) ->
%	State;
%compile_call([], [AST | Tail], State) ->
%	compile(AST, Tail, State);
%compile_call([AST | Tail], Stack, State) ->
%	compile_call(AST, [Tail | Stack], State);
%compile_call({call, _Location, Call, Args}, Stack, State) ->
%	compile_call(Args, [Call | Stack], State);
%compile_call({literal_string, _, _String}, Stack, State) ->
%	compile_call([], Stack, State);
%compile_call({function_name_local, _, _}, Stack, State) ->
%	compile_call([], Stack, State);
%compile_call(Wut, _Stack, State) ->
%	error({call_wut, Wut, State}).

write_to_outfile(#compile_state{ output_file_handle = undefined}, _) -> ok;
write_to_outfile(#compile_state{ output_file_handle = Fd}, Chars) ->
	io:put_chars(Fd, Chars).

milang_arg_to_erlang_arg(AST) when is_tuple(AST) ->
	case milang_ast:type(AST) of
		variable ->
			milang_arg_to_erlang_arg(milang_ast:data(AST));
		_ ->
			error({nyi, argument_type, AST})
	end;
milang_arg_to_erlang_arg(Atom) when is_atom(Atom) ->
	milang_arg_to_erlang_arg(atom_to_binary(Atom));
milang_arg_to_erlang_arg(<<$_, _/binary>> = Arg) ->
	Arg;
milang_arg_to_erlang_arg(Arg) ->
	[$A, Arg].


add_error(Location, Error, State) ->
	OldErrors = State#compile_state.errors,
	NewErrors = [{Location, Error} | OldErrors],
	maybe_close_outfile(State#compile_state{ errors = NewErrors}).

maybe_close_outfile(#compile_state{ output_file_handle = undefined} = State) ->
	State;
maybe_close_outfile(State) ->
	Handle = State#compile_state.output_file_handle,
	ok = file:close(Handle),
	State#compile_state{ output_file_handle = undefined }.

declaration_module(Name, Exports, State) ->
	InputFile = State#compile_state.input_file_name,
	ModuleNameAsString = atom_to_list(Name),
	NewErrors = case filename:rootname(filename:basename(InputFile)) of
		ModuleNameAsString ->
			State#compile_state.errors;
		_ ->
			[{{0,0}, {module_name_file_name_mismatch}} | State#compile_state.errors]
	end,
	PartitionFun = fun(Node) ->
		case milang_ast:type(Node) of
			function_name_local -> true;
			type_name_local -> false
		end
	end,
	{Functions, Types} = lists:partition(PartitionFun, Exports),
	State#compile_state{ module_name = Name, function_exports = Functions, type_exports = Types, errors = NewErrors}.

find_module(ModuleName, WorkDir, SearchDirs) ->
	ModuleNameStr = atom_to_binary(ModuleName),
	HeaderName = unicode:characters_to_binary([ModuleNameStr, ".milang-header"]),
	FullHeaderName = filename:join([WorkDir, "headers", HeaderName]),
	SourceName = unicode:characters_to_binary([ModuleNameStr, ".milang"]),
	MaybeSourceFile = path_find(["." | SearchDirs], SourceName),
	HeaderExist = filelib:is_regular(FullHeaderName),
	case {MaybeSourceFile, HeaderExist} of
		{{ok, Src}, false} ->
			compile_module(Src, WorkDir, SearchDirs);
		{{ok, Src}, true} ->
			case is_dep_outdated(Src, FullHeaderName) of
				true ->
					compile_module(Src, WorkDir, SearchDirs);
				false ->
					{ok, FullHeaderName}
			end;
		{_, true} ->
			{ok, FullHeaderName};
		_ ->
			{error, cannot_find_module}
	end.

is_dep_outdated(DepFile, TargetFile) ->
	{ok, #file_info{ mtime = DepTime}} = file:read_file_info(DepFile),
	{ok, #file_info{ mtime = TargetTime}} = file:read_file_info(TargetFile),
	DepTime > TargetTime.

compile_module(Src, WorkDir, SearchDirs) ->
	% we're spawning so we don't crash the compile process were were created
	% from. We would like to keep as clean/clear errors as we can.
	DoIt = fun() ->
		do_compile_module(Src, WorkDir, SearchDirs)
	end,
	{Pid, Ref} = spawn_monitor(DoIt),
	receive
		{'DOWN', Ref, process, Pid, normal} ->
			{ok, recompiled};
		{'DOWN', Ref, process, Pid, NotNormal} ->
			case NotNormal of
				{error, _} ->
					NotNormal;
				_ ->
					{error, NotNormal}
			end
	end.

do_compile_module(Src, WorkDir, SearchDirs) ->
	AST = case milang_parse:file(Src) of
		{ok, Ok} -> Ok;
		ParseFail -> error(ParseFail)
	end,
	Opts =
		[{search_dirs, SearchDirs}
		,{input_file_name, Src}
		,{work_dir, WorkDir}
		,{compile_mode, module}
		],
	milang_compile:compile(AST, Opts).

path_find(SearchDirs, File) ->
	case file:path_open(SearchDirs, File, [read]) of
		{ok, FD, FullName} ->
			ok = file:close(FD),
			{ok, FullName};
		Error ->
			Error
	end.

function_name_to_mf(#{ module := Module, name := Function}) ->
	{[$', atom_to_binary(Module, utf8), $', $:], Function};
function_name_to_mf({local, Name}) ->
	BinStr = atom_to_binary(Name, utf8),
	case binary:split(BinStr, <<$.>>, [global]) of
		[FunctionPart] ->
			{"", FunctionPart};
		Split ->
			[FunctionPart | ReversedModule] = lists:reverse(Split),
			ModulePart = lists:join($., lists:reverse(ReversedModule)),
			{[$', ModulePart, $', $:], FunctionPart}
	end.
