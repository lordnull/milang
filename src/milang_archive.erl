%%% @doc After the compiler has generated the erlang files, this then takes over.
%%% It takes the generated files created and turns them into beam files. Then,
%%% it creates the bootstrap file, compiles that. Finally, it takes all the beam
%%% files and puts them into a single escript compiled file.
-module(milang_archive).

-export([build/2]).
-export([add_bootstrap/2]).

-include_lib("kernel/include/file.hrl").

-type option()
	:: {erl_src_dir, filename:filename()}
	|  {beam_dir, filename:filename()}
	|  {support_modules, [ atom() ]}
	|  {main_file, atom()}
	.

-spec build(file:filename(), [ option() ]) -> ok.
build(OutputFile, Options) ->
	ErlSrcDir = proplists:get_value(erl_src_dir, Options, "./milang-work-dir/src_erl"),
	ok = io:format("Erlang Source Dir: ~p~n", [ErlSrcDir]),
	ok = make_dir_or_die(ErlSrcDir),
	BeamDir = proplists:get_value(beam_dir, Options, "./milang-work-dir/ebin"),
	ok = io:format("Beam Repository: ~p~n", [BeamDir]),
	ok = make_dir_or_die(BeamDir),
	MainFile = case proplists:get_value(main_file, Options) of
		undefined ->
			main_module_from_output_file(OutputFile);
		AnAtom ->
			AnAtom
	end,
	ok = io:format("Main file: ~s~n", [MainFile]),
	BootstrapPart = add_bootstrap(MainFile, ErlSrcDir),
	ok = io:format("bootstrap built.~n"),
	Curry = code:get_object_code(milang_curry),
	SupportModules = proplists:get_value(support_modules, Options, []),
	io:format("Support Modules: ~p~n", [SupportModules]),
	true = code:add_path(BeamDir),
	RestOfZip = lists:map(fun(ModuleName) ->
		build_beam(ModuleName, ErlSrcDir, BeamDir)
	end, SupportModules),
	Zippable = [fixup_beam_entry(RawTuple) || RawTuple <- [BootstrapPart, Curry | RestOfZip]],
	{ok, Bytes} = escript:create(binary, [shebang, comment, {emu_args, "-escript main milang_bootstrap"}, {archive, Zippable, [memory]}]),
	ok = file:write_file(OutputFile, Bytes),
	ok = ensure_file_executable(OutputFile).

make_dir_or_die(Dir) ->
	case file:make_dir(Dir) of
		ok ->
			ok;
		{error, eexist} ->
			ok;
		{error, Error} ->
			error({dir_not_usable, Error})
	end.

main_module_from_output_file(OutputFile) ->
	Base = filename:basename(OutputFile),
	if
		is_binary(Base) ->
			binary_to_atom(Base);
		is_list(Base) ->
			list_to_atom(Base)
	end.

add_bootstrap(MainModule, WorkDir) ->
	BootstrapFile = unicode:characters_to_list(filename:join([WorkDir, "milang_bootstrap.erl"])),
	ok = io:format("BootstrapFile: ~s~n", [BootstrapFile]),
	ok = file:write_file(BootstrapFile, boostrap_module_src()),
	ok = io:format("MainModule: ~s~n", [MainModule]),
	{ok, _, Beam} = compile:noenv_file(BootstrapFile, [binary, debug_info, {d, 'MILANG_BOOT_MODULE', MainModule}]),
	{"milang_bootstrap.beam", Beam}.

boostrap_module_src() ->
"-module(milang_bootstrap).\n"
"\n"
"-export([main/1]).\n"
"\n"
"main(Args) ->\n"
"	Stack = ?MILANG_BOOT_MODULE:main(),\n"
"	milang_curry:call(Stack, [Args]).\n".

build_beam('Concurrency', _, _) ->
	code:get_object_code('Concurrency');
build_beam('System.Print', _, _) ->
	code:get_object_code('System.Print');
build_beam('Core', _, _) ->
	code:get_object_code('Core');
build_beam(ModuleName, ErlSrc, BeamDir) ->
	MaybeObjectCode = code:get_object_code(ModuleName),
	ErlBaseName = unicode:characters_to_list(io_lib:format("~s.erl", [ModuleName])),
	SrcPath = filename:join(ErlSrc, ErlBaseName),
	MaybeBeamFile = beam_file(MaybeObjectCode, ModuleName),
	RebuiltBeam = rebuild_beam_if_needed(SrcPath, MaybeBeamFile, MaybeObjectCode, BeamDir, ModuleName),
	io:format("Module hunt for ~s:~n"
		"	Erl File: ~s~n"
		"	beam file ~p~n"
		"	rebuilt: ~p~n"
		, [ModuleName, SrcPath, MaybeBeamFile, RebuiltBeam]),
	case old_or_rebuilt(MaybeObjectCode, RebuiltBeam) of
		{error, cannot_find_module} ->
			error({cannot_find_module, ModuleName});
		{error, Wut} ->
			{error, Wut};
		error ->
			{error, cannot_load_object_code, ModuleName};
		ObjCode ->
			ObjCode
	end.

beam_file({ModuleName, _, BeamFile}, ModuleName) ->
	{ok, BeamFile};
beam_file(error, _ModuleName) ->
	{error, no_beam}.

rebuild_beam_if_needed(SrcPath, {error, no_beam}, error, BeamDir, _ModuleName) ->
	compile:file(SrcPath, [{outdir, BeamDir}, debug_info]);
rebuild_beam_if_needed(SrcPath, {ok, BeamFile}, ObjectCode, BeamDir, _ModuleName) ->
	SrcFileInfo = file:read_file_info(SrcPath),
	BeamFileInfo = file:read_file_info(BeamFile),
	case {SrcFileInfo, BeamFileInfo} of
		{ {ok, #file_info{ mtime = SrcTime} }, {ok, #file_info{ mtime = BeamTime } }} when SrcTime < BeamTime ->
			ObjectCode;
		{ {ok, _}, _} ->
			compile:file(SrcPath, [{output_dir, BeamDir}, debug_info]);
		{_, _} ->
			ObjectCode
	end.

old_or_rebuilt(error, error) ->
	{error, cannot_find_module};
old_or_rebuilt(_, {ok, ModuleName}) ->
	code:get_object_code(ModuleName);
old_or_rebuilt(A, A) ->
	A;
old_or_rebuilt(_, Error) ->
	Error.

fixup_beam_entry({Name, BeamCode}) ->
	{fixup_filename(Name), BeamCode};
fixup_beam_entry({_, BeamCode, Name}) ->
	{fixup_filename(Name), BeamCode}.

fixup_filename(Name) ->
	Base = filename:basename(Name),
	Joined = filename:join(["dummy_app_name", "ebin", Base]),
	unicode:characters_to_list(Joined).

ensure_file_executable(OutputFile) ->
	{ok, FileInfo} = file:read_file_info(OutputFile),
	#file_info{mode = Mode} = FileInfo,
	case Mode band 8#00100 of
		0 ->
			NewMode = Mode bor 8#00100,
			NewInfo = FileInfo#file_info{ mode = NewMode },
			file:write_file_info(OutputFile, NewInfo);
		_Not0 ->
			ok
	end.
