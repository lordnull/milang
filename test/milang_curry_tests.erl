-module(milang_curry_tests).

-include_lib("eunit/include/eunit.hrl").

curry_test_() ->
	[ ?_assertEqual(11, milang_curry:stack_and_call(fun(A, B, C) -> A + B + C end, [1,3,7]))
	, fun() ->
		Stacked = milang_curry:stack(fun(Fmt, Args) -> io_lib:format(Fmt, Args) end),
		C1 = Stacked("Hello ~s.~n"),
		C2 = C1([<<"john">>]),
		GotRaw = C2(),
		Got = unicode:characters_to_binary(GotRaw),
		?assertEqual(<<"Hello john.\n">>, Got)
	end
	, fun() ->
		Stacked = milang_curry:stack(fun(A, B, C) -> (A + B) * C end),
		C1 = milang_curry:call(Stacked, [2, 3]),
		Got = milang_curry:call(C1, [7]),
		?assertEqual(35, Got)
	end
	].
