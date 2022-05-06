-module(milang_p_type).

-export([parse/1]).
-export(
	[ data/0
	, function/0
	, record/0
	, concrete/0
	]).

parse(Subject) ->
	TopP = parse:first_of([concrete(), function(), record(), data()]),
	Recusive = milang_p_atomic:parens(parse:lazy(fun() -> TopP end)),
	Parse = parse:first_of([TopP, Recusive]),
	parse:it(Subject, Parse).

space() ->
	milang_p_atomic:space().

space_opt() ->
	parse:optional(space()).

data() ->
	TailElementP = parse:map(parse:series([space(), data_arg()]), fun([D, A]) ->
		milang_ast:set_doc(D, A)
	end),
	TailP = parse:repeat_until_error(TailElementP),
	HeadP = milang_p_atomic:type_name(),
	SeriesP = parse:series([HeadP, TailP]),
	Tagged = parse:tag(type_data, SeriesP),
	parse:map(Tagged, fun({T, L, [H, Args]}) ->
		Data = #{ name => H, args => Args },
		milang_ast:ast_node(L, <<>>, T, Data)
	end).

data_arg() ->
	parse:first_of(
		[ milang_p_atomic:type_name()
		, milang_p_atomic:variable()
		, record()
		, milang_p_atomic:parens(concrete())
		]).

function() ->
	DirtyTailElementP = parse:series([space_opt(), parse:string(<<"->">>), space_opt(), function_arg()]),
	TailElementP = parse:map(DirtyTailElementP, fun([Doc1, _Arrow, Doc2, Arg]) ->
		MergedDocs = unicode:characters_to_binary([Doc1, "\n", Doc2]),
		milang_ast:set_doc(MergedDocs, Arg)
	end),
	TailP = parse:repeat_until_error(TailElementP),
	SeriesP = parse:series([function_arg(), TailP]),
	Tagged = parse:tag(type_function, SeriesP),
	Merged = parse:map(Tagged, fun({T, L, [Head, Tail]}) ->
		{T, L, [Head | Tail]}
	end),
	parse:map(Merged, fun
		({_T, _L, [OnlyArg]}) -> OnlyArg;
		({T, L, E}) ->
			milang_ast:ast_node(L, <<>>, T, E)
	end).

function_arg() ->
	parse:first_of(
		[ data()
		, record()
		, milang_p_atomic:parens(concrete())
		, milang_p_atomic:variable()
		]).

record() ->
	RecordP = milang_p_atomic:record(record_key(), concrete()),
	Tagged = parse:tag(type_record, RecordP),
	Mapper = fun({T, L, Entries}) ->
		milang_ast:ast_node(L, <<>>, T, Entries)
	end,
	parse:map(Tagged, Mapper).

record_key() ->
	parse:first_of([record_key_string(), record_key_integer()]).

record_key_string() ->
	Downcase = milang_p_atomic:downcase_name(),
	parse:map(Downcase, fun(S) -> binary_to_atom(S, utf8) end).

record_key_integer() ->
	IntRegex = parse:regex("[1-9][0-9]*"),
	parse:map(IntRegex, fun binary_to_integer/1 ).

concrete() ->
	parse:first_of(
		[ parse:lazy(fun function/0)
		, parse:lazy(fun record/0)
		, parse:lazy(fun data/0)
		]).
