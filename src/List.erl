-module('List').

-export([empty/0, foldl/0, reverse/0, foldr/0, map/0, head/0, tail/0, cons/0, 'Nil'/0, 'Cons'/0, 'Cons'/2]).

'Nil'() -> [].

'Cons'() -> milang_curry:stack(fun 'Cons'/2).

'Cons'(Head, Tail) ->
	[Head | Tail].

empty() ->
	fun() -> [] end.

cons() ->
	milang_curry:stack(fun cons/2).

cons(NewHead, List) ->
	[ NewHead | List ].

foldl() ->
	milang_curry:stack(fun foldl/3).

foldl(FolderCurry, Init, List) ->
	Folder = fun(Element, Acc) ->
		milang_curry:call(FolderCurry, [Element, Acc])
	end,
	lists:foldl(Folder, Init, List).

reverse() ->
	milang_curry:stack(fun lists:reverse/1).

foldr() ->
	milang_curry:stack(fun lists:foldr/3).

map() ->
	milang_curry:stack(fun lists:map/2).

head() ->
	milang_curry:stack(fun head/1).

head([]) ->
	'Maybe':'Nothing'();
head([ E |_]) ->
	'Maybe':'Some'(E).

tail() ->
	milang_curry:stack(fun tail/1).

tail([]) ->
	[];
tail([_ | Tail]) ->
	Tail.
