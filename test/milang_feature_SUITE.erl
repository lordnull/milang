-module(milang_feature_SUITE).
-behavior(ct_suite).

-include_lib("stdlib/include/assert.hrl").
-include_lib("milang/src/milang_ast.hrl").

-compile([export_all]).

all() ->
	[{group, basic}
	].

groups() ->
	[{basic, [],
		[ hello_world_test
		, type_error_test
		, header_creation_test
		, comment_test
		]}
	].

hello_world_test(Cfg) ->
	DataDir = proplists:get_value(data_dir, Cfg),
	PrivDir = proplists:get_value(priv_dir, Cfg),
	{ok, AST} = milang_parse:file(filename:join([DataDir, "HelloWorld.milang"])),
	OutputFile = filename:join([PrivDir, "HelloWorld"]),
	WorkDir = filename:join([PrivDir, "milang-work-dir"]),
	ok = milang_compile:compile(AST, [{input_file_name, "HelloWorld.milang"},{output_file_name, OutputFile},{work_dir, WorkDir}]),
	Result = os:cmd(OutputFile),
	?assertEqual("Hello, World!\n", Result).

type_error_test(Cfg) ->
	DataDir = proplists:get_value(data_dir, Cfg),
	PrivDir = proplists:get_value(priv_dir, Cfg),
	{ok, AST} = milang_parse:file(filename:join([DataDir, "TypeError.milang"])),
	OutputFile = filename:join([PrivDir, "TypeError"]),
	WorkDir = filename:join([PrivDir, "milang-work-dir"]),
	Got = try milang_compile:compile(AST, [{input_file_name, "TypeError.milang"}, {output_file_name, OutputFile}, {work_dir, WorkDir}]) of
		ok ->
			ok
	catch
		What:Why ->
			{What, Why}
	end,
	?assertNotEqual(ok, Got).


header_creation_test(Cfg) ->
	DataDir = proplists:get_value(data_dir, Cfg),
	PrivDir = proplists:get_value(priv_dir, Cfg),
	{ok, AST} = milang_parse:file(filename:join([DataDir, "CreateHeader.milang"])),
	OutputFile = filename:join([PrivDir, "CreateHeader.milang-header"]),
	WorkDir = filename:join([PrivDir, "milang-work-dir"]),
	{ok, OutDev} = file:open(OutputFile, [write]),
	ok = milang_header:create_header(AST, OutDev),
	ok = file:close(OutDev),
	{ok, HeaderASTWithDoc} = milang_parse:file(OutputFile),
	{ok, ExpectedASTWithDoc} = milang_parse:file(filename:join([DataDir, "CreateHeader.milang-header"])),
	HeaderAST = strip_text_artifacts(HeaderASTWithDoc),
	ExpectedAST = strip_text_artifacts(ExpectedASTWithDoc),
	assert_equal_asts(HeaderAST, ExpectedAST).

assert_equal_asts(AST, AST) ->
	ok;
assert_equal_asts(Got, Expected) ->
	ct:pal("The ast's mismatch. To help use the path you'll see in the error, here they are:"),
	ct:pal("Got: ~p~n", [Got]),
	ct:pal("Expected: ~p~n", [Expected]),
	find_first_difference(Got, Expected, [type_check]).

find_first_difference(Got, Expected, [type_check | Tail]) ->
	if
		is_list(Got), is_list(Expected) ->
			find_first_difference(Got, Expected, [{list, 1} | Tail]);
		is_tuple(Got), is_tuple(Expected) ->
			find_first_difference(Got, Expected, [{tuple, transform} | Tail]);
		is_map(Got), is_map(Expected) ->
			find_first_difference(Got, Expected, [{map, key_check} | Tail]);
		true ->
			error({type_mismatch, #{ got => Got, expected => Expected, path => Tail}})
	end;
find_first_difference([T | Got], [T | Expected], [{ListOrTuple, Nth} | PathTail]) ->
	NewPath = [{ListOrTuple, Nth + 1} | PathTail],
	find_first_difference(Got, Expected, NewPath);
find_first_difference([], [], _Path) ->
	error(should_never_happen);
find_first_difference([], More, Path) ->
	error({expected_more, More, Path});
find_first_difference(Extra, [], Path) ->
	error({too_many, Extra, Path});
find_first_difference([ Got | _], [Expected | _], Path) ->
	find_first_difference(Got, Expected, [type_check| Path]);
find_first_difference(Got, Expected, [{tuple, transform} | Tail]) ->
	GotList = tuple_to_list(Got),
	ExpectedList = tuple_to_list(Expected),
	find_first_difference(GotList, ExpectedList, [{tuple, 1} | Tail]);
find_first_difference(Got, Expected, [{map, key_check} | Tail]) ->
	GotKeys = ordsets:from_list(maps:keys(Got)),
	ExpectedKeys = ordsets:from_list(maps:keys(Expected)),
	Extra = ordsets:subtract(GotKeys, ExpectedKeys),
	Missing = ordsets:subtract(ExpectedKeys, GotKeys),
	case {Extra, Missing} of
		{[], []} ->
			find_first_difference(Got, Expected, [{map, {keys, ExpectedKeys}} | Tail]);
		_ ->
			error({map_mismatch, #{ extra_keys => Extra, missing_keys => Missing, got => Got, expected => Expected }})
	end;
find_first_difference(Got, Expected, [{map, {keys, []}} | Tail]) ->
	error({should_never_happen, Tail, Got, Expected});
find_first_difference(Got, Expected, [{map, {keys, [Key | KeyTail]}} | PathTail]) ->
	GotValue = maps:get(Key, Got),
	ExpectedValue = maps:get(Key, Expected),
	if
		GotValue =:= ExpectedValue ->
			find_first_difference(Got, Expected, [{map, {keys, KeyTail}} | PathTail]);
		true ->
			find_first_difference(GotValue, ExpectedValue, [type_check, {map, Key} | PathTail])
	end.

strip_text_artifacts(#milang_ast{} = Node) ->
	Node#milang_ast{ doc = <<>>, location = {0,0}, data = strip_text_artifacts(Node#milang_ast.data) };
strip_text_artifacts(Data) when is_map(Data) ->
	maps:map(fun(_K, V) ->
		strip_text_artifacts(V)
	end, Data);
strip_text_artifacts(Data) when is_list(Data) ->
	lists:map(fun strip_text_artifacts/1, Data);
strip_text_artifacts(Term) ->
	Term.



comment_test(Cfg) ->
	DataDir = proplists:get_value(data_dir, Cfg),
	PrivDir = proplists:get_value(priv_dir, Cfg),
	{ok, AST} = milang_parse:file(filename:join([DataDir, "CommentTest.milang"])),

	OutputFile = filename:join([PrivDir, "CommentTest"]),
	WorkDir = filename:join([PrivDir, "milang-work-dir"]),
	ok = milang_compile:compile(AST, [{input_file_name, "CommentTest.milang"},{output_file_name, OutputFile},{work_dir, WorkDir}]),
	Result = os:cmd(OutputFile),
	?assertEqual("", Result).
