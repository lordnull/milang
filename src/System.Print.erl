-module('System.Print').

-export([ln/1]).

ln(String) ->
	_ = io:format("~s~n", [String]),
	{}.
