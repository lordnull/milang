-module(milang_type_table).

-type symbol() :: unicode:chardata().
-type constraint() :: any | unicode:chardata().
-type data_table() :: #{ symbol() => [ constraint() ]}.
-type spec() :: [ constraint() | spec() ].
-type function_table() :: #{ symbol() => spec() }.
-type scope() :: { function_table(), data_table() }.
-type table() :: nonempty_list( scope() ).

-export_type(
	[ symbol/0
	, constraint/0
	, data_table/0
	, spec/0
	, function_table/0
	, scope/0
	, table/0
	]).

-export(
	[ new/0
	, lookup_data/2
	, lookup_function/2
	, insert_new_data/3
	, insert_new_function/3
	, push_scope/1
	, pop_scope/1
	]).

-spec new() -> table().
new() ->
	[{#{}, #{}}].

-spec lookup_data(symbol(), table()) -> result:result(not_found, [ constraint() ]).
lookup_data(_Key, []) ->
	{error, not_found};
lookup_data(Key, [{_, Data} | Tail]) ->
	case maps:find(Key, Data) of
		error ->
			lookup_data(Key, Tail);
		{ok, _} = Ok ->
			Ok
	end.

-spec lookup_function(symbol(), table()) -> result:result(not_found, spec()).
lookup_function(_Key, []) ->
	{error, not_found};
lookup_function(Key, [{Function, _} | Tail]) ->
	case maps:find(Key, Function) of
		error ->
			lookup_function(Key, Tail);
		{ok, _} = Ok ->
			Ok
	end.

-spec insert_new_data(symbol(), [ constraint()], table()) -> result:result(existing, table()).
insert_new_data(Key, Value, [{Function, OldData} | Tail]) ->
	case maps:find(Key, OldData) of
		error ->
			NewData = maps:put(Key, Value, OldData),
			{ok, [{Function, NewData} | Tail]};
		{ok, _} ->
			{error, existing}
	end.

-spec insert_new_function(symbol(), spec(), table()) -> result:result(exiting, table()).
insert_new_function(Key, Value, [{OldFunction, Data} | Tail]) ->
	case maps:find(Key, OldFunction) of
		error ->
			NewFunction = maps:put(Key, Value, OldFunction),
			{ok, [{NewFunction, Data} | Tail]};
		{ok, _} ->
			{error, existing}
	end.

-spec push_scope(table()) -> table().
push_scope(Table) ->
	[{#{}, #{}} | Table].

-spec pop_scope(table()) -> table().
pop_scope([ _ ] = Table) ->
	Table;
pop_scope([_ | Table]) ->
	Table.
