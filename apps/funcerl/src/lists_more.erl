%%% @doc Added utility functions for lists that didn't exist in the standard
%%% lib when this module was written.
-module(lists_more).

-export([repeat/2]).

repeat(Count, Element) ->
	[ Element || _ <- lists:seq(1, Count) ].
