-module('Maybe').

-export(['Nothing'/0, 'Some'/0, map/0, and_then/0]).

'Nothing'() ->
	{'Maybe', {'Nothing'}}.

'Some'() ->
	milang_curry:stack(fun 'Some'/1, []).

'Some'(E) ->
	{'Maybe', {'Some', E}}.

map() ->
	milang_curry:stack(fun map/2, []).

map(Mapper, Maybe) ->
	case Maybe of
		{_, {'Nothing'}} ->
			Maybe;
		{_, {'Some', E}} ->
			'Some'(Mapper(E))
	end.

and_then() ->
	milang_curry:stack(fun and_then/2, []).

and_then(AndThen, Maybe) ->
	case Maybe of
		{_, {'Nothing'}} ->
			Maybe;
		{_, {'Some', E}} ->
			AndThen(E)
	end.
