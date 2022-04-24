%% @doc Take an ast and make it real.
-module(milang_compile).

-include_lib("kernel/include/file.hrl").

-export(
	[ compile/2
	]).

-type option()
	:: {input_file_name, binary()}
	|  {output_file_name, binary()}
	|  {search_dirs, [ binary() ]}
	|  {work_dir, binary()}
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
	type_exports = []
}).

compile([], Options) when is_list(Options) ->
	error(no_ast);

compile(AST, Options) when is_list(Options) ->
	io:format("what's here:~n~p~n", [file:list_dir(".")]),
	InputFile = proplists:get_value(input_file_name, Options),
	OutputFile = proplists:get_value(output_file_name, Options),
	SearchDirs = proplists:get_value(search_dirs, Options, []),
	WorkDir = proplists:get_value(work_dir, Options, <<"./milang-work-dir">>),
	ok = make_dir_or_die(WorkDir),
	HeadersDir = filename:join([WorkDir, "headers"]),
	ok = make_dir_or_die(HeadersDir),
	ok = write_system_headers(HeadersDir),
	ScratchDir = filename:join([WorkDir, "scratch"]),
	ok = make_dir_or_die(ScratchDir),
	ScratchFileName = filename:join([WorkDir, filename:basename(OutputFile)]),
	ScratchFileHandle = case file:open(ScratchFileName, [binary, write]) of
		{ok, H} -> H;
		{error, ScratchError} ->
			error({scrach_file_open_failed, ScratchFileName, ScratchError})
	end,
	CompileState = #compile_state{
		input_file_name = InputFile,
		output_file_handle = ScratchFileHandle,
		search_dirs = SearchDirs,
		work_dir = WorkDir,
		errors = []
	},
	compile(AST, [], CompileState).

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
	io:format("Skipping writing system headers from priv dir, priv dir could not be opened: ~p~n", [Error]),
	ok;
write_system_headers(WorkDir, PrivDir) ->
	io:format("priv dir: ~p~n", [PrivDir]),
	SysConfigHeaders = filelib:wildcard(PrivDir ++ "/**/*.milang-header"),
	io:format("sys headers: ~p~n", [SysConfigHeaders]),
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
			io:format("Copy complete.~n"
				"    Source: ~p~n"
				"    Dest: ~p~n"
				"    FullR: ~p~n"
				, [Source, Dest, R]),
			ok;
		true ->
			io:format("Skipping copy.~n"
				"    Source: ~p~n"
				"    Dest: ~p~n"
				"    SrouceCtime: ~p~n"
				"    DestCtime: ~p~n"
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

compile([], [], #compile_state{ errors = []}) ->
	ok;
compile([], [], #compile_state{ errors = Errors}) ->
	error(Errors);
compile([], [AST | NewStack], State) ->
	compile(AST, NewStack, State);
compile([AST | Tail], Stack, State) ->
	compile(AST, [Tail | Stack], State);
compile({declaration_module, Location, _, _}, Stack, #compile_state{ module_name = ModName} = State) when ModName =/= undefined ->
	NewState = add_error(Location, multiple_module_declarations, State),
	compile([], Stack, NewState);
compile({declaration_module, _Location, Name, Exports}, Stack, CompileState) ->
	NewState = declaration_module(Name, Exports, CompileState),
	compile([], Stack, NewState);
compile(Declaration, Stack, #compile_state{errors = []} = CompileState) ->
	NewState = add_error(element(2, Declaration), module_declaration_must_be_first, CompileState),
	compile(Declaration, Stack, NewState);
compile({declaration_spec, _Location, Name, Spec}, Stack, State) ->
	NewState = add_to_lookup_table(Name, Spec, State),
	compile([], Stack, NewState);
compile({declaration_function, _Location, Name, Args, Bindings, Body}, Stack, State) ->
	StateWithFunc = maybe_add_function(Name, Args, State),
	ArgsShadowState = add_shadow_errors(Args, StateWithFunc),
	BindingsShadowState = add_shadow_errors(Bindings, ArgsShadowState),
	compile(Body, Stack, BindingsShadowState);
compile({declaration_import, Location, Module, Alias, Imports} = Dec, Stack, State) ->
	case find_module(Module, State#compile_state.work_dir, State#compile_state.search_dirs) of
		{error, _} ->
			NewState = add_error(Location, {cannot_find_module, Module}, State),
			compile([], Stack, NewState);
		{src, Source} ->
			{Pid, Ref} = spawn_monitor(fun() ->
				{ok, AST} = milang_parse:file(Source),
				Opts =
					[{search_dirs, State#compile_state.search_dirs}
					,{input_file_name, Source}
					,{work_dir, State#compile_state.work_dir}
					],
				milang_compile:compile(AST, Opts)
			end),
			receive
				{'DOWN', Ref, process, Pid, normal} ->
					compile(Dec, Stack, State);
				{'DOWN', Ref, process, Pid, NotNormal} ->
					exit(NotNormal)
			end;
		{ok, Header} ->
			NewState = lists:foldl(fun(F, Acc) ->
				F(Acc)
			end, State, [
				fun(S) -> load_module_header(Header, S) end,
				fun(S) -> add_module_alias(Alias, Header, S) end,
				fun(S) -> add_module_imports(Imports, Header, S) end
			]),
			compile([], Stack, NewState)
	end;
compile({call, _, _, _} = AST, Stack, State) ->
	NewState = compile_call(AST, State),
	compile([], Stack, NewState);
compile({function_name_local, _, _}, Stack, State) ->
	compile([], Stack, State);
compile({function_name_remote, _, _, _}, Stack, State) ->
	compile([], Stack, State);
compile(Wut, Stack, CompileState) ->
	error({nyi, Wut, Stack, CompileState}).

%add_alias(Name, TruePath, State) ->
%	add_to_lookup_table(Name, {alias, TruePath, Name}, State).

% TODO start putting out a function call.
compile_call(Call, State) ->
	compile_call(Call, [], State).

compile_call([], [], State) ->
	State;
compile_call([], [AST | Tail], State) ->
	compile(AST, Tail, State);
compile_call([AST | Tail], Stack, State) ->
	compile_call(AST, [Tail | Stack], State);
compile_call({call, _Location, Call, Args}, Stack, State) ->
	compile_call(Args, [Call | Stack], State);
compile_call({literal_string, _, _String}, Stack, State) ->
	compile_call([], Stack, State);
compile_call({function_name_local, _, _}, Stack, State) ->
	compile_call([], Stack, State);
compile_call(Wut, _Stack, State) ->
	error({call_wut, Wut, State}).

maybe_add_function({function_name_local, Location, Name} = AST, _ArgsList, State) ->
	LookupTable = State#compile_state.lookup_table,
	% TODO construct a proper entry to give to the type validator.
	case milang_type_validation:refine(Name, undefined, LookupTable) of
		{ok, NewTable} ->
			State#compile_state{lookup_table = NewTable};
		{error, MisMatch} ->
			io:format("spec/function mismatch~n    Input: ~p~n    Output: ~p", [AST, MisMatch]),
			add_error(Location, {type_mismatch, MisMatch}, State)
	end.

add_shadow_errors(_, State) ->
	State.

add_error(Location, Error, State) ->
	OldErrors = State#compile_state.errors,
	NewErrors = [{Location, Error} | OldErrors],
	State#compile_state{ errors = NewErrors}.

declaration_module(Name, Exports, State) ->
	State#compile_state{ module_name = Name, function_exports = Exports}.

add_to_lookup_table(NameAtomic, TypeInfo, State) ->
	Table = State#compile_state.lookup_table,
	% TODO create actual entry to send to refinement.
	case milang_type_validation:refine(NameAtomic, TypeInfo, Table) of
		{ok, NewTable} ->
			State#compile_state{ lookup_table = NewTable };
		{error, MisMatch} ->
			{_, Location, _} = NameAtomic,
			add_error(Location, {type_mismatch, MisMatch}, State)
	end.


load_module_header(HeaderTerms, State) ->
	LookupTable = State#compile_state.lookup_table,
	NewTable = milang_lookup:import(HeaderTerms, LookupTable),
	State#compile_state{lookup_table = NewTable}.

add_module_alias(undefined, _Header, State) ->
	State;
add_module_alias(Alias, Header, State) ->
	error({alias_nyi, Alias, Header, State}).

add_module_imports(Imports, Header, State) ->
	error({imports_nyi, Imports, Header, State}).

find_module(ModuleName, WorkDir, SearchDirs) ->
	HeaderName = unicode:characters_to_binary([ModuleName, ".milang-header"]),
	FullHeaderName = filename:join([WorkDir, "headers", HeaderName]),
	case file:consult(FullHeaderName) of
		{ok, _} = Ok ->
			Ok;
		_Error ->
			SourceName = unicode:characters_to_binary([ModuleName, ".milang"]),
			case file:path_open(["." | SearchDirs], SourceName, [read]) of
				{ok, Handle, FullName} ->
					ok = file:close(Handle),
					{src, FullName};
				Error ->
					Error
			end
	end.
