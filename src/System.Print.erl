-module('System.Print').

-export([ln/1]).

ln(String) ->
	fun() ->
		ok = io:format("~s~n", [String]),
		{}
	end.
