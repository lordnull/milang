%%% @doc After the compiler has generated the erlang files, this then takes over.
%%% It takes the generated files created and turns them into beam files. Then,
%%% it creates the bootstrap file, compiles that. Finally, it takes all the beam
%%% files and puts them into a single escript compiled file.
-module(milang_archive).

-export([build/2]).
-export([add_bootstrap/2]).

-include_lib("kernel/include/file.hrl").

build(SourceFiles, Options) ->
	[MainFile | _SupportFiles] = SourceFiles,
	ok = io:format("Main file: ~s~n", [MainFile]),
	WorkDir = proplists:get_value(work_dir, Options, "milang-work-dir"),
	ok = io:format("Work dir: ~s~n", [WorkDir]),
	OutputFile = proplists:get_value(output_file, Options, filename:join(".", filename:basename(MainFile))),
	ok = io:format("OutputFile: ~s~n", [OutputFile]),
	BootstrapPart = add_bootstrap(MainFile, WorkDir),
	ok = io:format("boostrap built.~n"),
	Curry = code:get_object_code(milang_curry),
	RestOfZip = lists:map(fun(Filename) ->
		build_beam(Filename, WorkDir)
	end, SourceFiles),
	Zippable = [fixup_beam_entry(RawTuple) || RawTuple <- [BootstrapPart, Curry | RestOfZip]],
	{ok, Bytes} = escript:create(binary, [shebang, comment, {emu_args, "-escript main milang_bootstrap"}, {archive, Zippable, [memory]}]),
	ok = file:write_file(OutputFile, Bytes),
	ok = ensure_file_executable(OutputFile).

add_bootstrap(MainModule, WorkDir) ->
	BootstrapFile = unicode:characters_to_list(filename:join([WorkDir, "milang_bootstrap.erl"])),
	ok = io:format("BootstrapFile: ~s~n", [BootstrapFile]),
	ok = file:write_file(BootstrapFile, boostrap_module_src()),
	ok = io:format("MainModule: ~s~n", [MainModule]),
	ok = io:format("MainModule as atom: ~s~n", [binary_to_atom(MainModule, utf8)]),
	{ok, _, Beam} = compile:noenv_file(BootstrapFile, [binary, debug_info, {d, 'MILANG_BOOT_MODULE', binary_to_atom(MainModule, utf8)}]),
	{"milang_bootstrap.beam", Beam}.

boostrap_module_src() ->
"-module(milang_bootstrap).\n"
"\n"
"-export([main/1]).\n"
"\n"
"main(Args) ->\n"
"	Stack = ?MILANG_BOOT_MODULE:main(),\n"
"	milang_curry:call(Stack, [Args]).\n".

build_beam(<<"Concurrency">>, _) ->
	code:get_object_code('Concurrency');
build_beam(<<"System.Print">>, _) ->
	code:get_object_code('System.Print');
build_beam(BaseFile, WorkDir) ->
	File = filename:join([WorkDir, BaseFile]),
	ok = io:format("Generated file: ~s~n", [File]),
	SrcFile = unicode:characters_to_list(filename:join([WorkDir, filename:basename(unicode:characters_to_binary([File, ".erl"]))])),
	ok = io:format("as .erl file: ~s~n", [SrcFile]),
	{ok,_} = file:copy(File, SrcFile),
	{ok, _, Beam} = compile:noenv_file(SrcFile, [binary, debug_info]),
	{unicode:characters_to_binary([File, ".beam"]), Beam}.

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
