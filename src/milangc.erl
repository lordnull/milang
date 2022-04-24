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
			{error, input_file_not_found};
		{error, Posix} ->
			{error, {input_file, Posix}}
	end;
build_option_map([], _Opts) ->
	{error, missing_input_file};
build_option_map([{help, _} | Tail], Acc) ->
	build_option_map(Tail, Acc);
build_option_map([{input_file, _} | _], #{ input_file := _ }) ->
	{error, multiple_input_files};
build_option_map([{output_file, _} | _], #{ output_file := _}) ->
	{error, multiple_output_files};
build_option_map([{input_file, In} | Tail], Acc) ->
	build_option_map(Tail, Acc#{ input_file => In });
build_option_map([{output_file, Out} | Tail], Acc) ->
	build_option_map(Tail, Acc#{ output_file => Out});
build_option_map([{search_dir, Dir} | Tail], Acc) ->
	#{ search_dirs := SearchDirsAcc} = Acc,
	build_option_map(Tail, Acc#{ search_dirs => [ Dir | SearchDirsAcc]}).

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

compile(#{ input_file := InFile, output_file := OutFile, search_dirs := _SearchDirs }) ->
	case milang_parse:file(InFile) of
		{ok, AST} ->
			milang_compile:compile(AST, [{input_file_name, InFile}, {output_file_name, OutFile}]);
		Error ->
			io:format("Parsing error:~n~p~n", [Error]),
			halt(2)
	end.



input_args() ->
	[{input_file, $i, "input-file", string, "The '.milang' file to parse. It should be the entire filename. It will recursively compile files it depends on."}
	,{output_file, $o, "output-file", string, "The exectuable file, and directory to write it to."}
	,{search_dir, $p, "search-dir", string, "Add the given directory to the list of places to look for source files."}
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
	"If no output file is given, the output file is the input file's name with the ext stripped off. It is an error to have no input file.\n"
	"\n"
	"The environment variable MILANG_SEARCH_DIRS is prepended to the search-dir options. The parsing of the variable is simply splitting by colons (:).\n".

help() ->
	getopt:usage(input_args(), "milangc", help_text()).
