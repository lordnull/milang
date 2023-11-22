-module(parse).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export(
	[ success/1
	, fail/1
	, source/0
	, chomp_if/2
	, chomp_while/1
	, chomp_until/1
	, chomp_until_end_or/1
	, end_of_input/0
	, lazy/1
	, and_then/2
	, pipeline/2
	, keep/1
	, discard/1
	, loop/2
	, map/2
	, map_n/2
	, map_err/2
	, row/0
	, col/0
	, byte_offset/0
	, location/0
	, in_context/2
	, backtrackable/1
	, commit/1
	, get_chomped_string/1
	, first_of/1
	, either/2
	%, regex_advanced/3
	%, default_regex_compile/0
	%, default_regex_run/0
	%, regex/1
	, chomp_string/1
	]).
-export(
	[ string/2
	]).

-type parse_step(UserContext, Error, Ok)
	:: {good, _Committed :: boolean(), Ok, parse_ctx:context(UserContext)}
	|  {bad, _Committed :: boolean(), Error, parse_ctx:context(UserContext)}
	.

-type parser(UserContext, Error, Ok) ::
	fun((parse_ctx:context(UserContext)) ->
		parse_step(UserContext, Error, Ok)
	).

-type parse_error(Error, Context) :: #{ location := parse_ctx:location(), byte_offset := parse_ctx:byte_offset(), context := Context, reason := Error }.

-export_type(
	[ parser/3
	, parse_error/2
	]).

-spec string(unicode:chardata(), parser(C, X, A)) -> result:result(parse_error(X, C), A).
string(String, Parser) ->
	InitCtx = parse_ctx:new(String),
	ParseResult = Parser(InitCtx),
	case ParseResult of
		{bad, _, Error, Context} ->
			{error, error_map(Error, Context)};
		{good, _, OK, _} ->
			{ok, OK}
	end.

-spec success(Term) -> parser(term(), none(), Term).
success(Value) ->
	partial:func(fun success/2, [Value]).

success(Value, Context) ->
	{good, false, Value, Context}.

-spec fail(Term) -> parser(term(), Term, none()).
fail(Why) ->
	partial:func(fun fail/2, [Why]).

fail(Why, Context) ->
	{bad, false, Why, Context}.

-spec source() -> parser(term(), none(), unicode:chardata()).
source() ->
	fun source/1.

source(Ctx) ->
	success(parse_ctx:source_string(Ctx), Ctx).

%% @doc backtrackable just means that instead of just failing, we reset our
%% location and offset to where we were when we started in our error.
-spec backtrackable(parser(C, X, A)) -> parser(C, X, A).
backtrackable(Parser) ->
	partial:func(fun backtrackable/2, [Parser]).

backtrackable(Parser, Context) ->
	case Parser(Context) of
		{good, B, V, C} ->
			{good, B, V, C};
		{bad, _, V, C} ->
			{bad, false, V, C}
	end.

-spec commit(V) -> parser(term(), none(), V).
commit(V) ->
	partial:func(fun commit/2, [V]).

commit(V, Ctx) ->
	{good, true, V, Ctx}.

-spec in_context(C, parser(C, X, A)) -> parser(C, X, A).
in_context(UserContext, Parser) ->
	partial:func(fun in_context/3, [UserContext, Parser]).

in_context(UserContext, Parser, Context) ->
	PushedContext = parse_ctx:push_context(Context, UserContext),
	case Parser(PushedContext) of
		{good, B, V, C} ->
			{good, B, V, parse_ctx:pop_context(C)};
		Bad ->
			Bad
	end.

-spec location() -> parser(term(), none(), parse_ctx:location()).
location() ->
	fun(Context) ->
		Location = parse_ctx:location(Context),
		success(Location, Context)
	end.

-spec col() -> parser(term(), none(), parse_ctx:col()).
col() ->
	fun(Context) ->
		{Col, _} = parse_ctx:location(Context),
		success(Col, Context)
	end.

-spec row() -> parser(term(), none(), parse_ctx:row()).
row() ->
	fun(Context) ->
		{_, Row} = parse_ctx:location(Context),
		success(Row, Context)
	end.

-spec byte_offset() -> parser(term(), none(), parse_ctx:byte_offset()).
byte_offset() ->
	fun(Context) ->
		Offset = parse_ctx:byte_offset(Context),
		success(Offset, Context)
	end.

-spec chomp_if(fun((unicode:chardata()) -> boolean()), OnError) -> parser(term(), OnError, {}).
chomp_if(Validator, OnError) ->
	partial:func(fun chomp_if/3, [Validator, OnError]).

chomp_if(Validator, OnError, Ctx) ->
	Source = parse_ctx:source_from_offset(Ctx),
	Grapheme = parse_ctx:get_first_grapheme(Source),
	case Validator(Grapheme) of
		false ->
			{bad, false, OnError, Ctx};
		true ->
			NewCtx = parse_ctx:chomp(Ctx),
			{good, true, {}, NewCtx}
	end.

-ifdef(TEST).
chomp() ->
	parse:chomp_if(compose:always(true), never).
-endif.

-spec chomp_while(fun((unicode:chardata()) -> boolean())) -> parser(term(), none(), {}).
chomp_while(Validator) ->
	partial:func(fun chomp_while/3, [Validator, _ChompCount = 0]).

chomp_while(Predicate, ChompCount, Ctx) ->
	SourceString = parse_ctx:source_from_offset(Ctx),
	Grapheme = parse_ctx:get_first_grapheme(SourceString),
	case Predicate(Grapheme) of
		false ->
			{good, ChompCount > 0, {}, Ctx};
		true ->
			Chomped = parse_ctx:chomp(Ctx),
			chomp_while(Predicate, ChompCount + 1, Chomped)
	end.

-ifdef(TEST).

chomp_while_test_() ->
	DoParse = fun(Subject, Predicate) ->
		parse:string(Subject, parse:get_chomped_string(parse:chomp_while(Predicate)))
	end,
	[ fun() ->
		Got = DoParse(<<"aaa">>, fun(G) -> G =:= <<"a">> end),
		?assertEqual({ok, <<"aaa">>}, Got)
	end
	, fun() ->
		Got = DoParse(<<"aaab">>, fun(G) -> G =:= <<"a">> end),
		?assertEqual({ok, <<"aaa">>}, Got)
	end
	, fun() ->
		Got = DoParse(<<"abababc">>, fun(G) -> G =:= <<"a">> orelse G =:= <<"b">> end),
		?assertEqual({ok, <<"ababab">>}, Got)
	end
	, fun() ->
		Got = DoParse(<<"435x">>, fun(G) -> G =:= <<"a">> end),
		?assertEqual({ok, <<>>}, Got)
	end
	].

-endif.

-spec chomp_until(fun((unicode:chardata()) -> boolean())) -> parser(term(), none(), {}).
chomp_until(Predicate) ->
	chomp_while(fun(G) ->
		not Predicate(G)
	end).

-spec chomp_until_end_or(fun((unicode:chardata()) -> boolean())) -> parser(term(), none(), {}).
chomp_until_end_or(Predicate) ->
	FullPredicate = fun
		(<<>>) ->
			true;
		(G) ->
			Predicate(G)
	end,
	chomp_until(FullPredicate).

-ifdef(TEST).

chomp_until_end_or_test_() ->
	DoParse = fun(Subject, Needle) ->
		parse:string(Subject, parse:get_chomped_string(parse:chomp_until_end_or(fun(G) -> G =:= Needle end)))
	end,
	[ fun() ->
		Got = DoParse(<<"abcdefdone">>, <<"d">>),
		?assertEqual({ok, <<"abc">>}, Got)
	end
	, fun() ->
		Got = DoParse(<<"abcdef">>, <<"d">>),
		?assertMatch({ok, <<"abc">>}, Got)
	end
	, fun() ->
		Got = DoParse(<<"abcdonedef">>, <<"d">>),
		?assertEqual({ok, <<"abc">>}, Got)
	end
	, fun() ->
		Got = DoParse(<<"abcefgh">>, <<"d">>),
		?assertEqual({ok, <<"abcefgh">>}, Got)
	end
	].

-endif.

-spec get_chomped_string(parser(C, X, term())) -> parser(C, X, unicode:chardata()).
get_chomped_string(Parser) ->
	partial:func(fun get_chomped_string/2, [Parser]).

get_chomped_string(Parser, Ctx) ->
	OriginalOffset = parse_ctx:byte_offset(Ctx),
	case Parser(Ctx) of
		{good, Comitted, _Value, NewCtx} ->
			NewOffset = parse_ctx:byte_offset(NewCtx),
			Characters = parse_ctx:between_offsets(OriginalOffset, NewOffset, NewCtx),
			{good, Comitted, Characters, NewCtx};
		{bad, _, _, _} = Step ->
			Step
	end.

-spec and_then(fun((A) -> parser(NewC, X, NewA)), parser(term(), X, A)) -> parser(NewC, X, NewA).
and_then(AndThenner, Parser) ->
	partial:func(fun and_then/3, [AndThenner, Parser]).

and_then(AndThenner, Parser, Context) ->
	FirstParse = Parser(Context),
	case FirstParse of
		{good, Comitted, Value, NewCtx} ->
			NextParser = AndThenner(Value),
			any_commit(Comitted, NextParser(NewCtx));
		{bad, _, _, _} = Step ->
			Step
	end.

any_commit(Comitted, {Goodness, ComittedB, Value, Ctx}) ->
	{Goodness, Comitted orelse ComittedB, Value, Ctx}.

-ifdef(TEST).

and_then_test_() ->
	[ fun() ->
		AndThen = fun(<<"yup">>) -> parse:success(<<"and then">>) end,
		Parser = parse:and_then(AndThen, parse:success(<<"yup">>)),
		Result = parse:string(<<"whatever">>, Parser),
		{ok, <<"and then">>} = Result
	end
	, fun() ->
		AndThen = fun(<<"yup">>) -> parse:fail(<<"and then">>) end,
		Parser = parse:and_then(AndThen, parse:success(<<"yup">>)),
		Result = parse:string(<<"whatever">>, Parser),
		{error, #{location := {1,1}, byte_offset := 0, reason := <<"and then">>}} = Result
	end
	, fun() ->
		AndThen = fun(<<"nope">>) -> parse:success(<<"and then">>) end,
		Parser = parse:and_then(AndThen, parse:fail(<<"nope">>)),
		Result = parse:string(<<"whatever">>, Parser),
		{error, #{location := {1,1}, byte_offset := 0, reason := <<"nope">>}} = Result
	end
	].

-endif.

-spec end_of_input() -> parser(term(), characters_remain, {}).
end_of_input() ->
	fun end_of_input/1.

end_of_input(Context) ->
	Source = parse_ctx:source_from_offset(Context),
	case Source of
		<<>> ->
			success({}, Context);
		_NotEmpty ->
			fail(characters_remain, Context)
	end.

%% @doc Build the parser on demand rather than ahead of time. You _must_ use
%% this when defining self-referential parsers, else you will end up in an
%% infinite loop.
-spec lazy(fun(() -> parser(C, X, A))) -> parser(C, X, A).
lazy(LazyFun) ->
	partial:func(fun lazy/2, [LazyFun]).

lazy(ParserBuilder, Context) ->
	Parser = ParserBuilder(),
	Parser(Context).

-spec map(fun((A) -> B), parser(C, X, A)) -> parser(C, X, B).
map(Mapper, Parser) ->
	and_then(fun(OriginalValue) ->
		success(Mapper(OriginalValue))
	end, Parser).

-spec map_err(fun((X1) -> X2), parser(C, X1, A)) -> parser(C, X2, A).
map_err(Mapper, Parser) ->
	partial:func(fun map_err/3, [Mapper, Parser]).

map_err(Mapper, Parser, Context) ->
	case Parser(Context) of
		{good, _, _, _} = Step ->
			Step;
		{bad, Comitted, Error, Ctx} ->
			{bad, Comitted, Mapper(Error), Ctx}
	end.

-spec either(parser(C, X, A), parser(C, X, A)) -> parser(C, X, A).
either(Parser1, Parser2) ->
	partial:func(fun either/3, [Parser1, Parser2]).

either(Parser1, Parser2, Context) ->
	FirstParse = Parser1(Context),
	case FirstParse of
		{good, _, _, _} = Step ->
			Step;
		{bad, false, _, _} ->
			Parser2(Context);
		{bad, true, _, _} = Step ->
			Step
	end.

-ifdef(TEST).

either_test_() ->
	[ fun() ->
		{ok, 1} = parse:string(<<"hi">>, parse:either(parse:success(1), parse:success(2)))
	end
	, fun() ->
		{ok, {}} = parse:string(<<"hi">>, parse:either(chomp(), parse:success(2)))
	end
	, fun() ->
		{ok, 2} = parse:string(<<"hi">>, parse:either(parse:fail(1), parse:success(2)))
	end
	, fun() ->
		{error, #{ reason := 2}} = parse:string(<<"hi">>, parse:either(parse:fail(1), parse:fail(2)))
	end
	, fun() ->
		{error, #{ reason := 1}} = parse:string(<<"hi">>, parse:either(parse:and_then(compose:always(parse:fail(1)), chomp()), parse:success(never_reached)))
	end
	].

-endif.

-spec first_of([ parser(C, X, A)]) -> parser(C, [X], A).
first_of(Parsers) ->
	partial:func(fun first_of/3, [Parsers, []]).

first_of([], Errors, Ctx) ->
	{bad, false, lists:reverse(Errors), Ctx};
first_of([Parser | Tail], Errors, Ctx) ->
	case Parser(Ctx) of
		{good, _, _, _} = Step ->
			Step;
		{bad, false, Error, _} ->
			first_of(Tail, [Error | Errors], Ctx);
		{bad, true, _, _} = Step ->
			Step
	end.

-ifdef(TEST).

first_of_test_() ->
	[ {"no chomping means despite fail try next", fun() ->
		Parser = parse:first_of(
			[ parse:fail(first)
			, chomp()
			]),
		Got = parse:string(<<"hi">>, Parser),
		?assertEqual({ok, {}}, Got)
	end}
	, {"chomping and failure with no back track means no try next", fun() ->
		FailParser = parse:and_then(fun({}) ->
			fail(fail_after_chomp)
		end, chomp()),
		GoodParser = parse:and_then(fun({}) ->
			success(never_reached)
		end, chomp()),
		Parser = parse:first_of(
			[ FailParser
			, GoodParser
			]),
		Got = parse:string(<<"hi">>, Parser),
		?assertMatch({error, #{ reason := fail_after_chomp}}, Got)
	end}
	, {"chomping and success means we never get to failure", fun() ->
		GoodParser = parse:and_then(fun({}) ->
			success(good)
		end, chomp()),
		BadParser = parse:and_then(fun({}) ->
			fail(never_reached)
		end, chomp()),
		Got = parse:string(<<"hi">>, parse:first_of([GoodParser, BadParser])),
		?assertEqual({ok, good}, Got)
	end}
	, {"chomping and backtrackble failure means we try next", fun() ->
		BadParser = parse:backtrackable(parse:and_then(fun({}) ->
			fail(backtrackable_fail_after_chomp)
		end, chomp())),
		GoodParser = parse:and_then(fun({}) ->
			parse:success(good)
		end, chomp()),
		Got = parse:string(<<"hi">>, parse:first_of([BadParser, GoodParser])),
		?assertEqual({ok, good}, Got)
	end}
	].

-endif.

-spec pipeline(parser(C, X, fun((StateElem) -> State)), [ parser(C, X, {keep, StateElem} | discard) ]) -> parser(C, X, State).
pipeline(InitParser, Parsers) ->
	loop({prime_pipeline, InitParser, Parsers}, fun pipeline/1).

pipeline({prime_pipeline, InitParser, Parsers}) ->
	parse:map(fun(InitParseVal) ->
		{step, {pipeline, InitParseVal, Parsers}}
	end, InitParser);

pipeline({pipeline, LastVal, []}) ->
	parse:success({done, LastVal});

pipeline({pipeline, LastVal, [ Parser | Tail]}) ->
	parse:map(fun
		(discard) ->
			{step, {pipeline, LastVal, Tail}};
		({keep, InVal}) ->
			{step, {pipeline, LastVal(InVal), Tail}}
	end, Parser).

-spec keep(parser(C, X, A)) -> parser(C, X, {keep, A}).
keep(Parser) ->
	parse:map(fun(V) ->
		{keep, V}
	end, Parser).

-spec discard(parser(C, X, any())) -> parser(C, X, discard).
discard(Parser) ->
	parse:map(compose:always(discard), Parser).

%% @doc The same as a pipeline where every value is wrapped in `parse:keep/'.
-spec map_n(fun((...) -> B), [ parser(C, X, term()) ]) -> parser(C, X, B).
map_n(Mapper, Parsers) ->
	PartialMapper = partial:func(Mapper, []),
	Kept = [ parse:keep(P) || P <- Parsers ],
	pipeline(parse:success(PartialMapper), Kept).

-ifdef(TEST).

map_n_test_() ->
	[ ?_assertEqual({ok, 5}, parse:string("hi", parse:map_n(fun(A, B) -> A + B end, [ parse:success(3), parse:success(2)])))
	, ?_assertMatch({error, #{ reason := first}}, parse:string("hi", parse:map_n(fun(_, _) -> {error, map} end, [ parse:fail(first), parse:fail(second)])))
	, ?_assertMatch({error, #{ reason := second}}, parse:string("hi", parse:map_n(fun(_, _) -> {error, map} end, [ parse:success(first), parse:fail(second)])))
	].

-endif.

-spec loop(State, fun((State) -> parser(C, X, {step, State} | {done, A}))) -> parser(C, X, A).
loop(InitState, Stepper) ->
	SeededParser = Stepper(InitState),
	partial:func(fun loop/4, [false, Stepper, SeededParser]).

loop(Comitted, Stepper, Parser, Ctx) ->
	case Parser(Ctx) of
		{good, ComittedStep, {step, NextStep}, NewCtx} ->
			NextParser = Stepper(NextStep),
			loop(Comitted orelse ComittedStep, Stepper, NextParser, NewCtx);
		{good, ComittedStep, {done, Out}, NewCtx} ->
			{good, ComittedStep orelse Comitted, Out, NewCtx};
		{bad, _, _, _} = Bad ->
			any_commit(Comitted, Bad)
	end.

-spec chomp_string(unicode:chardata()) -> parser(term(), {expected, unicode:chardata()}, {}).
chomp_string(Test) ->
	NormalizedTest = unicode:characters_to_nfkc_binary(Test),
	partial:func(fun chomp_string/3, [NormalizedTest, size(NormalizedTest)]).

chomp_string(Test, TestSize, Context) ->
	Subject = parse_ctx:source_from_offset(Context),
	case Subject of
		<<Test:TestSize/binary, _/binary>> ->
			ChompCount = parse_ctx:count_graphemes(Test),
			ChompedCtx = parse_ctx:chomp_n(ChompCount, Context),
			{good, true, {}, ChompedCtx};
		_ ->
			{bad, false, {expected, Test}, Context}
	end.

-ifdef(TEST).

chomp_string_test_() ->
	[ fun() ->
		{ok, {}} = parse:string(<<"hello world">>, parse:chomp_string(<<"hello">>))
	end
	, fun() ->
		{error, #{ reason := {expected, <<"yo">>}}} = parse:string(<<"hello world">>, parse:chomp_string(<<"yo">>))
	end
	, {"chomp is taken into account success", fun() ->
		{ok, {}} = parse:string(<<"hello world">>, parse:and_then(fun(_) ->
			parse:chomp_string(<<"ello">>)
		end, parse:chomp_if(compose:always(true), never)))
	end}
	, {"chomp is taken into account failure", fun() ->
		{error, #{ reason := {expected, <<"hello">>}}} = parse:string(<<"hello world">>, parse:and_then(fun(_) ->
			parse:chomp_string(<<"hello">>)
		end, parse:chomp_if(compose:always(true), never)))
	end}
	].

-endif.

error_map(Reason, Context) ->
	#{ reason => Reason
	,  location => parse_ctx:location(Context)
	,  user_contexts => lists:reverse(parse_ctx:user_context(Context))
	,  byte_offset => parse_ctx:byte_offset(Context)
	,  source_sample => cut_to_newlines(Context)
	}.

cut_to_newlines(Context) ->
	ByteOffset = parse_ctx:byte_offset(Context),
	Source = parse_ctx:source_string(Context),
	<<Before:ByteOffset/binary, After/binary>> = Source,
	BeforeSplits = binary:split(Before, <<"\n">>, [global]),
	CutTail = case binary:split(After, <<"\n">>) of
		[] ->
			[];
		[ Tail | _DoesNotMatter ] ->
			Tail
	end,
	CutHead = trim_befores(BeforeSplits),
	unicode:characters_to_binary([CutHead, CutTail]).

trim_befores([]) ->
	[];
trim_befores([Before]) ->
	Before;
trim_befores([_Head | Tail]) ->
	trim_befores(Tail).


-ifdef(TEST).

swoot_test_() ->
	[ fun() ->
		{ok, {}} = parse:string(<<"whatever">>, parse:success({}))
	end
	, fun() ->
		{error, #{ location := {1,1}, byte_offset := 0, reason := <<"i wanted to">>}} = parse:string(<<"whatever">>, parse:fail(<<"i wanted to">>))
	end
	, fun() ->
		{ok, {}} = parse:string(<<"hello world">>, parse:chomp_if(compose:always(true), never))
	end
	, fun() ->
		{ok, {}} = parse:string(<<>>, parse:end_of_input())
	end
	, fun() ->
		{error, #{ location := {1,1}, byte_offset := 0, reason := characters_remain}} = parse:string(<<"hello world">>, parse:end_of_input())
	end
	, fun() ->
		Mapper = fun(N) -> N * 3 end,
		Parser = parse:map(Mapper, parse:success($a)),
		Result = parse:string(<<"a">>, Parser),
		{ok, $a * 3} = Result
	end
	, fun() ->
		{ok, {}} = parse:string(<<"hello">>, parse:chomp_if(fun(Grapheme) ->
			Grapheme =:= <<$h>>
		end, {expected, $h}))
	end
	, {"chomp_if can fail", fun() ->
		Got = parse:string(<<"yo">>, parse:chomp_if(fun(Grapheme) ->
			Grapheme =:= <<$h>>
		end, {expected, $h})),
		?assertMatch({error, #{ reason := { expected, $h}}}, Got)
	end}
	, {"checking a chomp if: chomped", fun() ->
		Ish = fun(Grapheme) ->
			Grapheme =:= <<$h>>
		end,
		ChompIf = parse:chomp_if(Ish, never),
		{ok, <<$h>>} = parse:string(<<"hello">>, parse:get_chomped_string(ChompIf))
	end}
	, {"utf emoji chomp", fun() ->
		HappyFace = "😀",
		UnicodeHappyFace = unicode:characters_to_nfkc_binary(HappyFace),
		Subject = unicode:characters_to_binary([HappyFace, $a]),
		Parser = parse:chomp_if(compose:always(true), never),
		ChompedString = parse:get_chomped_string(Parser),
		Result = parse:string(Subject, ChompedString),
		?assertMatch({ok, UnicodeHappyFace}, Result)
	end}
	, fun() ->
		DoubleChomp = parse:and_then(fun(_) ->
			parse:chomp_if(compose:always(true), never)
		end, parse:chomp_if(compose:always(true), never)),
		Parser = parse:get_chomped_string(DoubleChomp),
		{ok, <<"he">>} = parse:string(<<"hello">>, Parser)
	end
	, fun() ->
		DoubleChomp = parse:and_then(fun(_) ->
			parse:chomp_if(compose:always(true), never)
		end, parse:chomp_if(compose:always(true), never)),
		Parser = parse:get_chomped_string(DoubleChomp),
		{ok, <<"a">>} = parse:string(<<"a">>, Parser)
	end
	, fun() ->
		{ok, discard} = parse:string(<<"hello">>, parse:discard(parse:chomp_if(compose:always(true), never)))
	end
	].



-endif.