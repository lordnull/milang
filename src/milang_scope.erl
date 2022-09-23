%%% @doc Add and remove key-value pairs in a scoped context, while allowing
%%% lookup up a key and going into higher level scopes.
-module(milang_scope).

-export([new/0, lookup/2, add_scope/1, pop_scope/1, insert/3, update/3]).

-type scope(Key, Value) :: [ #{ Key => Value } ].

-export_type([scope/2]).

new() -> [#{}].

lookup(_Key, []) ->
	{error, not_found};
lookup(Key, [Scope | Tail]) ->
	case maps:find(Key, Scope) of
		error ->
			lookup(Key, Tail);
		{ok, _} = Ok ->
			Ok
	end.

add_scope(Tail) ->
	[#{} | Tail].

pop_scope([_] = Scope) ->
	Scope;
pop_scope([_ | Tail]) ->
	Tail.

insert(Key, Value, Scope) ->
	case lookup(Key, Scope) of
		{ok, V} ->
			{error, {shadowing, Key, V}};
		{error, not_found} ->
			[ CurrentScope | Tail] = Scope,
			NewCurrent = CurrentScope#{ Key => Value },
			{ok, [ NewCurrent |  Tail]}
	end.

update(Key, Value, Scope) ->
	update(Key, Value, Scope, []).

update(_Key, _Value, [], _) ->
	{error, not_found};
update(Key, Value, [Scope | Tail], NotFoundIn) ->
	case maps:find(Key, Scope) of
		error ->
			update(Key, Value, Tail, [Scope | NotFoundIn]);
		{ok, _Oldvalue} ->
			NewScope = Scope#{ Key => Value },
			NewTail = [NewScope | Tail],
			NewHead = lists:reverse(NotFoundIn),
			{ok, lists:concat(NewHead, NewTail)}
	end.

