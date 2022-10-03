%% @doc Take an ast and make it real.
-module(milang_compile).

-include_lib("kernel/include/file.hrl").
-include_lib("kernel/include/logger.hrl").
-include("milang_ast.hrl").
-include("milang_constant_import.hrl").

-export(
	[ compile/2
	, default_imports/0
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
	ast_output_buffer = [],
	function_exports = [],
	type_exports = [],
	support_modules = [],
	compile_mode = program
}).

-record(stack_frame, {
	output_buffer = [],
	ast
	}).

default_imports() -> unicode:characters_to_binary(?DEFAULT_IMPORTS).

always_import_ast() ->
	{ok, Tokens, <<>>} = parse:it(default_imports(), milang_p_token:tokens()),
	{ok, AST} = milang_lex:as_header(Tokens),
	?LOG_DEBUG("always import ast: ~p", [AST]),
	AST.

-spec compile(nonempty_list(milang_ast:ast_node()), options()) -> ok.
compile([], Options) when is_list(Options) ->
	error(no_ast);

compile(AST, Options) when is_list(Options) ->
	DefaultLogLevel = proplists:get_value(default_log_level, Options, info),
	_ = milang_log:set_log_level(DefaultLogLevel),
	InputFile = proplists:get_value(input_file_name, Options),
	ok = ?LOG_DEBUG("Inputfile: ~s", [InputFile]),
	OutputFile = proplists:get_value(output_file_name, Options),
	ok = ?LOG_DEBUG("Outputfile: ~s", [OutputFile]),
	SearchDirs = proplists:get_value(search_dirs, Options, []),
	ok = ?LOG_DEBUG("SearchDirs: ~p", [SearchDirs]),
	WorkDir = proplists:get_value(work_dir, Options, <<"./milang-work-dir">>),
	ok = make_dir_or_die(WorkDir),
	ok = ?LOG_DEBUG("WorkDir: ~s", [WorkDir]),
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
	ok = ?LOG_DEBUG("ScratchFileName: ~s", [ScratchFileName]),
	Mode = proplists:get_value(compile_mode, Options, program),
	LintLogLevel = proplists:get_value(lint_log_level, Options, DefaultLogLevel),
	_ = milang_log:set_log_level(LintLogLevel),
	LintErrors = milang_lint:it(AST, Mode),
	CompileState = #compile_state{
		input_file_name = InputFile,
		output_file_handle = ScratchFileHandle,
		search_dirs = SearchDirs,
		work_dir = WorkDir,
		errors = LintErrors,
		compile_mode = Mode
	},
	CompileLogLevel = proplists:get_value(compile_log_level, Options, DefaultLogLevel),
	_ = milang_log:set_log_level(CompileLogLevel),
	WithAlwaysImports = always_import_ast() ++ AST,
	FinalState = compile(WithAlwaysImports, [], CompileState),
	ok = ?LOG_DEBUG("Final state: ~p~n", [FinalState]),
	WriteModLogLevel = proplists:get_value(write_module_log_level, Options, DefaultLogLevel),
	_ = milang_log:set_log_level(WriteModLogLevel),
	ok = write_module(FinalState),
	ArchiveOptions =
		[{erl_src_dir, ErlDir}
		,{beam_dir, filename:join([WorkDir, "ebin"])}
		,{support_modules, [ FinalState#compile_state.module_name | FinalState#compile_state.support_modules]}
		,{main_file, FinalState#compile_state.module_name}],
	ArchiveLogLevel = proplists:get_value(archive_log_level, Options, DefaultLogLevel),
	_ = milang_log:set_log_level(ArchiveLogLevel),
	ok = milang_archive:build(OutputFile, ArchiveOptions).

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
	ok = ?LOG_DEBUG("Skipping writing system headers from priv dir, priv dir could not be opened: ~p", [Error]),
	ok;
write_system_headers(WorkDir, PrivDir) ->
	ok = ?LOG_DEBUG("priv dir: ~p", [PrivDir]),
	SysConfigHeaders = filelib:wildcard(PrivDir ++ "/**/*.milang-header"),
	ok = ?LOG_DEBUG("sys headers: ~p", [SysConfigHeaders]),
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
			?LOG_DEBUG("Copy complete.~n"
				"    Source: ~p~n"
				"    Dest: ~p~n"
				"    FullR: ~p"
				, [Source, Dest, R]),
			ok;
		true ->
			?LOG_DEBUG("Skipping copy.~n"
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
	OldBuffer = State#compile_state.ast_output_buffer,
	Buffer = lists:reverse(OldBuffer),
	State#compile_state{ ast_output_buffer = Buffer };
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
compile(ASTNode, Stack, CompileState) ->
	case milang_ast:type(ASTNode) of
		undefined ->
			error({unknown_ast, ASTNode});
		{ok, import} ->
			NodeData = milang_ast:data(ASTNode),
			case milang_ast_import:imported_ast(NodeData) of
				undefined ->
					WithSupport = add_support_module(milang_ast:data(ASTNode), CompileState),
					MaybeAST = import_module(ASTNode, WithSupport),
					case MaybeAST of
						{ok, undefined} ->
							?LOG_ERROR("Unable to complete import of ~p due to no ast.", [milang_ast:data(ASTNode)]),
							ErrorState = add_error(milang_ast:location(ASTNode), no_ast_from_imported_module, WithSupport),
							compile([], Stack, ErrorState);
						{ok, ImportAST} ->
							NewNode = milang_ast:transform_data(fun(D) ->
								milang_ast_import:imported_ast(ImportAST, D)
							end, ASTNode),
							compile(ImportAST, Stack, add_node(NewNode, WithSupport));
						{error, Error} ->
							?LOG_ERROR("Unable to complete import.~n    Node: ~p~n    Error: ~p", [ASTNode, Error]),
							ErrorState = add_error(milang_ast:location(ASTNode), Error, WithSupport),
							compile([], Stack, ErrorState)
					end;
				_ ->
					compile([], Stack, CompileState)
			end;
		{ok, alias} ->
			NewState = type_validation(ASTNode, CompileState),
			compile([], Stack, add_node(ASTNode, NewState));
		{ok, binding} ->
			NewState = type_validation(ASTNode, CompileState),
			compile([], Stack, add_node(ASTNode, NewState));
		{ok, module} ->
			Data = milang_ast:data(ASTNode),
			ModuleName = milang_ast_module:name_as_string(Data),
			NewState = CompileState#compile_state{ module_name = ModuleName },
			compile([], Stack, add_node(ASTNode, NewState));
		{ok, expose} ->
			Data = milang_ast:data(ASTNode),
			ExposeWhat = milang_ast_expose:declaration(Data),
			ExposeWhatType = milang_ast:type_simply(ExposeWhat),
			StateWithExports = maybe_add_to_exports(ExposeWhatType, ExposeWhat, CompileState),
			NewState = type_validation(ExposeWhat, StateWithExports),
			compile([], Stack, add_node(ASTNode, NewState));
		{ok, spec} ->
			NewState = type_validation(ASTNode, CompileState),
			compile([], Stack, add_node(ASTNode, NewState));
		{ok, type} ->
			NewState = type_validation(ASTNode, CompileState),
			compile([], Stack, add_node(ASTNode, NewState));
		{ok, class} ->
			NewState = type_validation(ASTNode, CompileState),
			compile([], Stack, add_node(ASTNode, NewState));
		{ok, teach} ->
			NewState = type_validation(ASTNode, CompileState),
			compile([], Stack, add_node(ASTNode, NewState));
		{ok, _T} ->
			error({compile_nyi, ASTNode, Stack, CompileState})
	end.

add_node(Node, State) ->
	OldASTBuffer = State#compile_state.ast_output_buffer,
	NewASTBuffer = [ Node | OldASTBuffer ],
	State#compile_state{ ast_output_buffer = NewASTBuffer }.

type_validation(Node, State) ->
	case milang_type_validation:validate_list([Node], State#compile_state.lookup_table) of
		{ok, NewTable} ->
			State#compile_state{ lookup_table = NewTable };
		{error, T} ->
			add_error(milang_ast:location(Node), {type_error, T}, State)
	end.

add_support_module(ImportData, State) ->
	NameNode = milang_ast_import:name(ImportData),
	NameData = milang_ast:data(NameNode),
	Module = milang_ast_identifier:as_module_name(NameData),
	OldSupports = State#compile_state.support_modules,
	NewSupports = [Module | OldSupports],
	State#compile_state{ support_modules = NewSupports}.

maybe_add_to_exports(spec, Node, State) ->
	Data = milang_ast:data(Node),
	NameNode = milang_ast_spec:name(Data),
	TypeNode = milang_ast_spec:type(Data),
	TrueType = milang_ast:type_simply(TypeNode),
	TypeData = milang_ast:data(TypeNode),
	case TrueType of
		signature ->
			Args = milang_ast_signature:args(TypeData),
			Arity = length(Args) - 1,
			OldExports = State#compile_state.function_exports,
			NewExports = [{NameNode, Arity} | OldExports],
			State#compile_state{ function_exports = NewExports};
		_ ->
			?LOG_DEBUG("Don't know how to export ~p", [TypeNode]),
			State
	end;
maybe_add_to_exports(_, Node, State) ->
	?LOG_DEBUG("Don't know how to export ~p", [Node]),
	State.

write_module(State) ->
	milang_run_erlang:create_module(State#compile_state.ast_output_buffer, fun(Data) ->
		write_to_outfile(State, Data)
	end).

write_to_outfile(#compile_state{ output_file_handle = Fd}, Chars) ->
	io:put_chars(Fd, Chars).



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

%declaration_module(Name, State) ->
%	InputFile = State#compile_state.input_file_name,
%	ModuleNameAsString = case milang_ast:data(Name) of
%		{_, #{ module := M, local := L}} ->
%			unicode:characters_to_binary([M, $., L]);
%		{_, L} ->
%			L
%	end,
%	NewErrors = case filename:rootname(filename:basename(InputFile)) of
%		ModuleNameAsString ->
%			State#compile_state.errors;
%		_ ->
%			[{{0,0}, {module_name_file_name_mismatch}} | State#compile_state.errors]
%	end,
%	State#compile_state{ module_name = ModuleNameAsString, function_exports = [], type_exports = [], errors = NewErrors}.
%

import_module(Node, State) ->
	Data = milang_ast:data(Node),
	NameNode = milang_ast_import:name(Data),
	NameComplex = milang_ast:data(NameNode),
	Module = milang_ast_identifier:as_module_name(NameComplex),
	MaybeFoundHeader = find_module(Module, State#compile_state.work_dir, State#compile_state.search_dirs),
	ContentsRes = 'Result':and_then(fun(File) ->
		file:read_file(File)
	end, MaybeFoundHeader),
	TokensRes = 'Result':and_then(fun(Binary) ->
		case parse:it(Binary, milang_p_token:tokens()) of
			{ok, Tokens, <<>>} ->
				{ok, Tokens};
			{ok, _, LeftOvers} ->
				{error, {left_overs, LeftOvers}};
			Error ->
				Error
		end
	end, ContentsRes),
	'Result':and_then(fun(Tokens) ->
		milang_lex:as_header(Tokens)
	end, TokensRes).

find_module(ModuleName, WorkDir, SearchDirs) ->
	HeaderName = unicode:characters_to_binary([ModuleName, ".milang-header"]),
	FullHeaderName = filename:join([WorkDir, "headers", HeaderName]),
	SourceName = unicode:characters_to_binary([ModuleName, ".milang"]),
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
