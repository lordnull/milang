-module(milangc).

-export([main/1]).

main(Args) ->
	case parse_args(Args) of
		{error, just_help} ->
			help(),
			halt(0);
		{error, Wut} ->
			io:format("Given an invalid argument: ~p~n", [Wut]),
			help(),
			halt(1);
		{ok, Options} ->
			ok = setup_work_enviroment(Options),
			compile(Options)
	end.

parse_args(Args) ->
	case getopt:parse(input_args(), Args) of
		{ok, {OptionsAsList, []}} ->
			build_option_map(OptionsAsList);
		{ok, {_, Trailing}} ->
			{error, {trailing_optionsm, Trailing}};
		{error, Error} ->
			{error, Error}
	end.

build_option_map(OptionsAsList) ->
	case proplists:get_value(help, OptionsAsList) of
		undefined ->
			SearchDirs = ["." | get_env_search_dirs()],
			build_option_map(OptionsAsList, #{ search_dirs => lists:reverse(SearchDirs)});
		_ ->
			{error, just_help}
	end.

build_option_map([], #{ input_file := Input } = Opts) ->
	CorrectedSearchDirs = lists:reverse(maps:get(search_dirs, Opts)),
	case find_file(Input, CorrectedSearchDirs) of
		{ok, InputActual} ->
			OutputFile = get_output_file(Opts, InputActual),
			{ok, Opts#{ input_file => InputActual, output_file => OutputFile, search_dirs => CorrectedSearchDirs}};
		{error, notfound} ->
			{error, input_file_not_found}
	end;
build_option_map([], _Opts) ->
	{error, missing_input_file};
build_option_map([{help, _} | Tail], Acc) ->
	build_option_map(Tail, Acc);
build_option_map([{input_file, _} | _], #{ input_file := _ }) ->
	{error, multiple_input_files};
build_option_map([{input_file, In} | Tail], Acc) ->
	build_option_map(Tail, Acc#{ input_file => In });
build_option_map([{output_file, Out} | Tail], Acc) ->
	build_option_map(Tail, Acc#{ output_file => Out});
build_option_map([{search_dir, Dir} | Tail], Acc) ->
	#{ search_dirs := SearchDirsAcc} = Acc,
	build_option_map(Tail, Acc#{ search_dirs => [ Dir | SearchDirsAcc]});
build_option_map([{work_dir, Dir} | Tail], Acc) ->
	build_option_map(Tail, Acc#{ work_dir => Dir});
build_option_map([{output_header_file, DoHeader} | Tail], Acc) ->
	build_option_map(Tail, Acc#{ output_header => DoHeader});
build_option_map([{output_beam_file, DoBeam} | Tail], Acc) ->
	build_option_map(Tail, Acc#{ output_beam => DoBeam});
build_option_map([{executable, DoExe} | Tail], Acc) ->
	build_option_map(Tail, Acc#{executable => DoExe}).

get_env_search_dirs() ->
	case os:getenv("MILANG_SEARCH_DIRS") of
		false ->
			[];
		SomeString ->
			string:split(SomeString, ":", all)
	end.

get_output_file(#{ output_file := Out}, _) ->
	Out;
get_output_file(_Opts, InputFile) ->
	filename:rootname(InputFile, ".milang").

find_file(_BaseName, []) ->
	{error, notfound};
find_file(BaseName, [Dir | Tail]) ->
	Joined = filename:join([Dir, BaseName]),
	case filelib:is_regular(Joined) of
		true ->
			{ok, Joined};
		false ->
			find_file(BaseName, Tail)
	end.

setup_work_enviroment(#{ work_dir := WorkDir}) ->
	ok = make_dir_or_die(WorkDir),
	HeadersDir = filename:join(WorkDir, "headers"),
	ok = make_dir_or_die(HeadersDir),
	Myself = escript:script_name(),
	{ok, ExtractedEscript} = escript:extract(Myself, []),
	Archive = proplists:get_value(archive, ExtractedEscript),
	FolderFun = fun(Filename, _GetInfo, GetBin, ok) ->
		case Filename of
			"system_headers/" ++ HeaderName ->
				case file:write_file(filename:join(HeadersDir, HeaderName), GetBin()) of
					ok ->
						ok;
					{error, Error} ->
						throw({uanble_to_write_system_header, HeaderName, Error})
				end;
			_ ->
				ok
		end
	end,
	{ok, ok} = zip:foldl(FolderFun, ok, {Myself, Archive}),
	ok.

make_dir_or_die(Dir) ->
	case file:make_dir(Dir) of
		ok ->
			ok;
		{error, eexist} ->
			ok;
		{error, Error} ->
			error({dir_not_usable, Error})
	end.

compile(#{ input_file := InFile } = Options) ->
	MaybeTokens = milang_parse:file(InFile),
	MaybeModuleAST = 'Result':and_then(fun(Tokens) ->
		milang_lex:as_module(Tokens)
	end, MaybeTokens),
	MaybeHeaderAST = 'Result':and_then(fun(Tokens) ->
		milang_lex:as_header(Tokens)
	end, MaybeModuleAST),
	_ = 'Result':map(fun(AST) ->
		maybe_create_header(AST, Options)
	end, MaybeHeaderAST),
	_ = 'Result':map(fun(AST) ->
		maybe_create_beam(AST, Options)
	end, MaybeModuleAST),
	_ = 'Result':map(fun(AST) ->
		maybe_create_exe(AST, Options)
	end, MaybeModuleAST),
	case MaybeModuleAST of
		{ok, _} ->
			ok;
		Error ->
			io:format("Parsing error:~n~p~n", [Error]),
			halt(2)
	end.

maybe_create_header(_AST, #{ output_header := false}) ->
	ok;
maybe_create_header(AST, Options) ->
	io:format("Creating header..."),
	#{ work_dir := WorkDir, input_file := InputFile } = Options,
	ShortName = (filename:rootname(filename:basename(InputFile))) ++ ".milang-header",
	HeaderName = filename:join(WorkDir, ShortName),
	{ok, FD} = file:open(HeaderName, [write]),
	milang_header:create_header(AST, FD).

maybe_create_beam(_, _) ->
	io:format("Creating beam not yet supported."),
	ok.

maybe_create_exe(_AST, #{ executable := false}) ->
	ok;
maybe_create_exe(AST, Options) ->
	io:format("Creating executable script..."),
	#{ input_file := InFile, output_file := OutFile, work_dir := WorkDir, search_dirs := SearchDirs } = Options,
	CompileOpts =
		[ {input_file_name, InFile}
		, {output_file_name, OutFile}
		, {search_dirs, SearchDirs}
		, {work_dir, WorkDir}
		],
	milang_compile:compile(AST, CompileOpts).


input_args() ->
	[{input_file, $i, "input-file", string, "The '.milang' file to parse. It should be the entire filename. It will recursively compile files it depends on."}
	,{executable, $e, "executable", {boolean, true}, "Attempt to build an exectuable from the input file, and put it in a file named the same as input, with the extension stripped off."}
	,{executable_file, $f, "executable-file", string, "Same as 'executable', but specify the destination file explicitly"}
	,{output_header_file, $t, "header-file", {boolean, true}, "Build or not build the header file."}
	,{output_beam_file, $b, "beam-file", {boolean, true}, "Build or not build the beam file."}
	,{search_dir, $p, "search-dir", string, "Add the given directory to the list of places to look for source files."}
	,{work_dir, $w, "work-dir", {string, "./milang-work-dir"}, "Where intermidiate files (such as header and beam) go during the build."}
	,{verbose, $v, "verbose", undefined, "If specified, be very talkative during the compile process."}
	,{help, $h, "help", undefined, "Show the help."}
	,{help, $?, "help", undefined, "Show the help."}
	].

help_text() ->
	"\n"
	"Compile a source file into an executable.\n"
	"\n"
	"The full path does not need to point to the source file so long as it is in one of the search-dirs.\n"
	"\n"
	"If either executable or exectuable_file is set, the input file _must_ define a 'main' function that takes a `List String` and returns as `Task Never Unit`.\n"
	"\n"
	"The environment variable MILANG_SEARCH_DIRS is prepended to the search-dir options. The parsing of the variable is simply splitting by colons (:).\n".

help() ->
	getopt:usage(input_args(), "milangc", help_text()).
