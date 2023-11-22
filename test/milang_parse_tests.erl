-module(milang_parse_tests).

-include_lib("eunit/include/eunit.hrl").

match_test_() ->
	% {Title, String},
	Inputs =
		[ {"literal integer", <<"match a with\n"
			"	1 -> True.\n"
			"	_ -> False.\n"
			".">>}
		, {"literal float", <<"match a with\n"
			"	1.0 -> True.\n"
			"	_ -> False.\n"
			"	.">>}
		, {"literal string", <<"match a with \"hi\" -> True. \"bye\" -> True. _ -> False. .">>}
		, {"literal empty list", <<"match a with [] -> True..">>}
		, {"literal list, no cons", <<"match a with\n"
			"	[,_] -> 1.\n"
			"	[,_,_] -> 2.\n"
			"	[,_,_,_] -> 3.\n"
			"	_ -> -1..">>}
		, {"literal list, cons", <<"match a with\n"
			"	[,_ ,_ ,_ ,, _] -> 3.\n"
			"	[,_,_,,_] -> 2.\n"
			"	[,_,,_] -> 1.\n"
			"	[] -> 0..">>}
		, {"constructor", <<"match a with\n"
			"	Just a ->\n"
			"		True.\n"
			"	Nothing ->\n"
			"		False.\n"
			".">>}
		, {"record", <<"match a with\n"
			"	{} -> 0.\n"
			"	{ , f1 = _} -> 1.\n"
			"	{,f1 = _,,rest} -> 2.\n"
			".">>}
		],

	MakeTest = fun({Title, String}) ->
		{Title, fun() ->
			Parsed = parse:string(String, milang_parse:match_expression()),
			?assertMatch({ok, _}, Parsed)
		end}
	end,
	lists:map(MakeTest, Inputs).
