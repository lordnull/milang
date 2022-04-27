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

-record(stack_frame, {
	output_buffer = [],
	ast
	}).

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
	ScratchDir = filename:join([WorkDir, "scratch"]),
	ok = make_dir_or_die(ScratchDir),
	ScratchFileName = filename:join([WorkDir, filename:basename(OutputFile)]),
	ScratchFileHandle = case file:open(ScratchFileName, [binary, write]) of
		{ok, H} -> H;
		{error, ScratchError} ->
			error({scrach_file_open_failed, ScratchFileName, ScratchError})
	end,
	ok = log("ScratchFileName: ~s", [ScratchFileName]),
	CompileState = #compile_state{
		input_file_name = InputFile,
		output_file_handle = ScratchFileHandle,
		search_dirs = SearchDirs,
		work_dir = WorkDir,
		errors = []
	},
	FinalState = compile(AST, [], CompileState),
	io:format("Final state: ~p~n", [FinalState]).

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
	OutHandle = State#compile_state.output_file_handle,
	ok = io:put_chars(OutHandle, Bytes),
	compile(Tail, NewStack, State);
compile(#stack_frame{}, [Tail | NewStack], State) ->
	compile(Tail, NewStack, State);
compile({declaration_module, Location, _, _}, Stack, #compile_state{ module_name = ModName} = State) when ModName =/= undefined ->
	NewState = add_error(Location, multiple_module_declarations, State),
	compile([], Stack, NewState);
compile({declaration_module, _Location, Name, Exports}, Stack, CompileState) ->
	NewState = declaration_module(Name, Exports, CompileState),
	ModuleName = NewState#compile_state.module_name,
	Functions = NewState#compile_state.function_exports,
	Types = NewState#compile_state.type_exports,
	OutputFmt = "-module('~s').~n"
	"-export([~s]).~n"
	"-export_type([~s]).~n~n",
	FunctionExportsList = [io_lib:format("'~s'/0~n", [N]) || {_, _, N} <- Functions],
	FunctionExprts = lists:join($,, FunctionExportsList),
	TypeExportsList = [io_lib:format("'~s'/0~n", [N]) || {_, _, N} <- Types],
	TypeExports = lists:join($,, TypeExportsList),
	Output = io_lib:format(OutputFmt, [ModuleName, FunctionExprts, TypeExports]),
	ok = io:put_chars(NewState#compile_state.output_file_handle, Output),
	compile([], Stack, NewState);
compile(Declaration, Stack, #compile_state{module_name = undefined, errors = []} = CompileState) ->
	NewState = add_error(element(2, Declaration), {module_declaration_must_be_first, Declaration}, CompileState),
	compile(Declaration, Stack, NewState);
compile({declaration_spec, _Location, Name, Spec}, Stack, State) ->
	NewState = add_to_lookup_table(Name, Spec, State),
	compile([], Stack, NewState);
compile({declaration_function, _Location, Name, Args, Bindings, Body} = AST, Stack, State) ->
	StateWithFunc = maybe_add_function(Name, Args, State),
	ArgsShadowState = add_shadow_errors(Args, StateWithFunc),
	BindingsShadowState = add_shadow_errors(Bindings, ArgsShadowState),
	OutputFile = BindingsShadowState#compile_state.output_file_handle,
	Arity = length(Args),
	{function_name_local, _, LocalName} = Name,
	Exported = io_lib:format("'~s'() -> milang_curry:stack(fun ~s/~p).~n~n", [LocalName, LocalName, Arity]),
	ok = io:put_chars(OutputFile, Exported),
	ArgsList = [milang_arg_to_erlang_arg(ArgName) || {variable, _, ArgName} <- Args],
	ArgsJoined = lists:join($,, ArgsList),
	FunctionHead = io_lib:format("'~s'(~s) ->~n", [LocalName, ArgsJoined]),
	ok = io:put_chars(OutputFile, FunctionHead),
	Frame = #stack_frame{ ast = AST, output_buffer = ".\n"},
	compile(Body, [Frame | Stack], BindingsShadowState);
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
				fun(S) -> add_module_imports(Module, Imports, Header, S) end
			]),
			compile([], Stack, NewState)
	end;
compile({call, _, CallName, Args} = AST, Stack, State) ->
	{ModPart, FuncPart} = case CallName of
		{function_name_remote, _, ModuleNameAST, Name} ->
			{module_name, _, ModuleName} = ModuleNameAST,
			{[$', ModuleName, $', $:], Name};
		{function_name_local, _, LocalName} ->
			case milang_type_validation:resolve_function_name(binary_to_atom(LocalName, utf8), State#compile_state.lookup_table) of
				{ok, Resolved} ->
					{function, _, M, F, _} = Resolved,
					{[$', atom_to_binary(M), $', $:], F};
				{error, notfound} ->
					log("Could not resolve ~s, assuming it's a local call.", [LocalName]),
					{"", LocalName}
			end
	end,
	CommaFrame = #stack_frame{ast = AST, output_buffer = ", "},
	JoinedArgs = lists:join(CommaFrame, Args),
	FinalFrame = #stack_frame{ast = AST, output_buffer = ")\n"},
	NewStack = JoinedArgs ++ [ FinalFrame] ++ Stack,
	Chars = io_lib:format("~s~s(", [ModPart, FuncPart]),
	ok = io:put_chars(State#compile_state.output_file_handle, Chars),
	compile([], NewStack, State);
compile({literal_string, _, String}, Stack, State) ->
	Chars = io_lib:format("<<\"~s\">>", [String]),
	ok = io:put_chars(State#compile_state.output_file_handle, Chars),
	compile([], Stack, State);
%compile({call, _, _, _} = AST, Stack, State) ->
%	NewState = compile_call(AST, State),
%	compile([], Stack, NewState);
%compile({function_name_local, _, _}, Stack, State) ->
%	compile([], Stack, State);
%compile({function_name_remote, _, _, _}, Stack, State) ->
%	compile([], Stack, State);
compile(Wut, Stack, CompileState) ->
	error({nyi, Wut, Stack, CompileState}).

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

milang_arg_to_erlang_arg(<<$_, _/binary>> = Arg) ->
	Arg;
milang_arg_to_erlang_arg(Arg) ->
	[$A, Arg].

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
	maybe_close_outfile(State#compile_state{ errors = NewErrors}).

maybe_close_outfile(#compile_state{ output_file_handle = undefined} = State) ->
	State;
maybe_close_outfile(State) ->
	Handle = State#compile_state.output_file_handle,
	ok = file:close(Handle),
	State#compile_state{ output_file_handle = undefined }.

declaration_module(Name, Exports, State) ->
	{Functions, Types} = lists:partition(fun({function_name_local, _, _}) ->
		true;
		({type_name_local, _, _}) -> false
	end, Exports),
	State#compile_state{ module_name = Name, function_exports = Functions, type_exports = Types}.

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
	lists:foldl(fun add_header_term/2, State, HeaderTerms).

add_header_term({Name, Entry}, State) ->
	LookupTable = State#compile_state.lookup_table,
	case milang_type_validation:add_entry(Name, Entry, LookupTable) of
		{ok, NewTable} ->
			State#compile_state{lookup_table = NewTable};
		{error, Error} ->
			add_error({0,0}, Error, State)
	end.

add_module_alias(undefined, _Header, State) ->
	State;
add_module_alias(Alias, Header, State) ->
	error({alias_nyi, Alias, Header, State}).

add_module_imports(Module, Imports, _Header, State) ->
	lists:foldl(fun(Import, Acc) ->
		Table = State#compile_state.lookup_table,
		{Location, EntryNameBin, EntryType} = case Import of
			{function_name_local, L, N} ->
				{L, N, function_name_remote};
			{type_name_local, L, N} ->
				{L, N, type_name_remote};
			_ ->
				error({invalid_import, Import})
		end,
		EntryName = binary_to_atom(EntryNameBin, utf8),
		TrueName = binary_to_atom(unicode:characters_to_binary([Module, $., EntryNameBin])),
		Entry = milang_type_validation:alias(EntryName, EntryType, TrueName),
		case milang_type_validation:add_entry(EntryName, Entry, Table) of
			{ok, NewTable} ->
				Acc#compile_state{ lookup_table = NewTable};
			{error, Error} ->
				add_error(Location, {error_adding_lookup, Error}, Acc)
		end
	end, State, Imports).

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
