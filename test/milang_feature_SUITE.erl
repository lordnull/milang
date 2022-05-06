-module(milang_feature_SUITE).
-behavior(ct_suite).

-include_lib("stdlib/include/assert.hrl").

-compile([export_all]).

all() ->
	[{group, basic}
	].

groups() ->
	[{basic, [], [hello_world_test]}
	].

hello_world_test(Cfg) ->
	DataDir = proplists:get_value(data_dir, Cfg),
	PrivDir = proplists:get_value(priv_dir, Cfg),
	{ok, AST} = milang_parse:file(filename:join([DataDir, "HelloWorld.milang"])),
	OutputFile = filename:join([PrivDir, "HelloWorld"]),
	WorkDir = filename:join([PrivDir, "milang-work-dir"]),
	ok = milang_compile:compile(AST, [{input_file_name, "HelloWorld.milang"},{output_file_name, OutputFile},{work_dir, WorkDir}]),
	Result = os:cmd(OutputFile),
	?assertEqual(Result, "Hello, World!\n").
