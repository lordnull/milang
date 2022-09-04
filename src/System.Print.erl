-module('System.Print').

-export([ln/1, ln/0]).

ln(String) ->
	_ = io:format("~s~n", [String]),
	{}.

ln() ->
	milang_curry:stack(fun ln/1).

