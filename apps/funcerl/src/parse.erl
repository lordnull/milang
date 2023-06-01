-module(parse).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export(
	[ success/1
	, fail/1
	, source/0
	, chomp/0
	, end_of_input/0
	, and_then/2
	, try_recover/2
	%, on_error/2
	, fold/2
	, map/2
	, map_n/2
	, map_err/2
	, row/0
	, col/0
	, offset/0
	, location/0
	, in_context/2
	, backtrackable/1
	, backtrack/0
	, commit/0
	, discard/1
	, peek/1
	, chomp_if/1
	, chomp_n/1
	, first_of/1
	, either/2
	, regex_advanced/3
	, default_regex_compile/0
	, default_regex_run/0
	, regex/1
	, is_string/1
	]).
-export(
	[ string/2
	, string/3
	]).

-type parser(InValue, OutError, OutValue, UserContext) ::
	fun((parse_ctx:context(InValue, UserContext)) ->
		result:result(parse_ctx:context(OutError, UserContext), parse_ctx:context(OutValue, UserContext))).

-type parse_error(Error, Context) :: #{ location := parse_ctx:location(), offset := parse_ctx:offset(), context := Context, reason := Error }.

-spec string(string(), parser(none(), X, A, C)) -> result:result(parse_error(X, C), A).
string(String, Parser) ->
	string(String, Parser, undefined).

-spec string(string(), parser(In, X, A, term()), In) -> result:result(parse_error(X, term()), A).
string(String, Parser, InitUserState) ->
	InitCtx = parse_ctx:new(String, InitUserState),
	ParseResult = Parser(InitCtx),
	IfOkay = result:map_ok(fun(Ctx) ->
		parse_ctx:parse_result(Ctx)
	end, ParseResult),
	result:map_err(fun(Ctx) ->
		error_map(Ctx)
	end, IfOkay).

-spec success(Term) -> parser(term(), none(), Term, Term).
success(Value) ->
	partial:func(fun success/2, [Value]).

success(Value, Context) ->
	{ok, parse_ctx:parse_result(Context, Value)}.

-spec fail(Term) -> parser(term(), Term, none(), Term).
fail(Why) ->
	partial:func(fun fail/2, [Why]).

fail(Why, Context) ->
	{error, parse_ctx:parse_result(Context, Why)}.

-spec source() -> parser(term(), none(), unicode:chardata(), term()).
source() ->
	fun(Ctx) ->
		success(parse_ctx:source_string(Ctx), Ctx)
	end.

-type backtrack_ctx(A, C) :: {backtrackable, A, parse_ctx:context(A, C)}.
-spec backtrackable(parser(A, X, B, C)) -> parser(A, backtrack_ctx(X, C), backtrack_ctx(B, C), C).
backtrackable(Parser) ->
	partial:func(fun backtrackable/2, [Parser]).

backtrackable(Parser, Context) ->
	ParseResult = Parser(Context),
	MappedErr = result:map_err(fun(ErrorCtx) ->
		parse_ctx:map(fun(E) ->
			{backtrackable, E, Context}
		end, ErrorCtx)
	end, ParseResult),
	result:map(fun(GoodCtx) ->
		parse_ctx:map(fun(E) ->
			{backtrackable, E, Context}
		end, GoodCtx)
	end, MappedErr).

-spec backtrack() -> parser(backtrack_ctx(X, C), {no_backtrack, X}, X, C).
backtrack() ->
	fun backtrack/1.

backtrack(Ctx) ->
	CtxValue = parse_ctx:parse_result(Ctx),
	case CtxValue of
		{backtrackable, V, BacktrackCtx} ->
			success(V, BacktrackCtx);
		NotBacktrackable ->
			fail({no_backtrack, NotBacktrackable}, Ctx)
	end.

-spec commit() -> parser(A, {no_backtrack, A}, A, term()).
commit() ->
	fun commit/1.

commit(Ctx) ->
	CtxValue = parse_ctx:parse_result(Ctx),
	case CtxValue of
		{backtrackable, TrueValue, _BacktrackCtx} ->
			success(TrueValue, Ctx);
		NotBacktrackable ->
			fail({no_backtrack, NotBacktrackable}, Ctx)
	end.

-spec in_context(NewC, parser(A, X, B, OldC)) -> parser(A, X, B, maybe_improper_list(NewC, OldC)).
in_context(UserContext, Parser) ->
	partial:func(fun in_context/3, [UserContext, Parser]).

in_context(UserContext, Parser, Context) ->
	PushedContext = parse_ctx:push_context(Context, UserContext),
	ParserResult = Parser(PushedContext),
	result:map_ok(fun(Ctx) ->
		parse_ctx:pop_context(Ctx)
	end, ParserResult).

-spec location() -> parser(term(), none(), parse_ctx:location(), term()).
location() ->
	fun(Context) ->
		Location = parse_ctx:location(Context),
		success(Location, Context)
	end.

-spec col() -> parser(term(), none(), parse_ctx:col(), term()).
col() ->
	fun(Context) ->
		{Col, _} = parse_ctx:location(Context),
		success(Col, Context)
	end.

-spec row() -> parser(term(), none(), parse_ctx:row(), term()).
row() ->
	fun(Context) ->
		{_, Row} = parse_ctx:location(Context),
		success(Row, Context)
	end.

-spec offset() -> parser(term(), none(), parse_ctx:offset(), term()).
offset() ->
	fun(Context) ->
		Offset = parse_ctx:offset(Context),
		success(Offset, Context)
	end.

and_then(AndThenner, Parser) ->
	partial:func(fun and_then/3, [AndThenner, Parser]).

and_then(AndThenner, Parser, Context) ->
	FirstParse = Parser(Context),
	result:and_then(fun(FirstCtxParse) ->
		Value = parse_ctx:parse_result(FirstCtxParse),
		NextParser = AndThenner(Value),
		NextParser(FirstCtxParse)
	end, FirstParse).

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
		{error, #{location := {0,0}, offset := 0, reason := <<"and then">>}} = Result
	end
	, fun() ->
		AndThen = fun(<<"nope">>) -> parse:success(<<"and then">>) end,
		Parser = parse:and_then(AndThen, parse:fail(<<"nope">>)),
		Result = parse:string(<<"whatever">>, Parser),
		{error, #{location := {0,0}, offset := 0, reason := <<"nope">>}} = Result
	end
	].

-endif.

-spec chomp() -> parser(term(), end_of_input, unicode:chardata(), term()).
chomp() ->
	chomp_n(1).

chomp_n(Count) ->
	partial:func(fun chomp_n/2, [Count]).

chomp_n(ChompCount, Context) ->
	SourceString = parse_ctx:source_string(Context),
	{ChompedStr, NewSource} = parse_ctx:chomp_n(ChompCount, SourceString),
	if
		length(ChompedStr) == ChompCount ->
			Updated = parse_ctx:update_location(ChompedStr, NewSource, Context),
			success(ChompedStr, Updated);
		true ->
			fail(end_of_input, Context)
	end.

chomp_if(Character) ->
	partial:func(fun chomp_if/2, [Character]).

chomp_if(Character, Context) ->
	SourceString = parse_ctx:source_string(Context),
	case parse_ctx:chomp_n(1, SourceString) of
		{[Character], NewSource} ->
			Updated = parse_ctx:update_location([Character], NewSource, Context),
			success(Character, Updated);
		_ ->
			fail({expected_character, Character}, Context)
	end.

end_of_input() ->
	fun end_of_input/1.

end_of_input(Context) ->
	SourceString = parse_ctx:source_string(Context),
	case SourceString of
		<<>> ->
			success({}, Context);
		[] ->
			success({}, Context);
		_ ->
			fail(characters_remain, Context)
	end.

map(Mapper, Parser) ->
	and_then(fun(OriginalValue) ->
		success(Mapper(OriginalValue))
	end, Parser).

map_err(Mapper, Parser) ->
	partial:func(fun map_err/3, [Mapper, Parser]).

map_err(Mapper, Parser, Context) ->
	case Parser(Context) of
		{ok, _} = OK -> OK;
		{error, ErrorCtx} ->
			{error, parse_ctx:map(Mapper, ErrorCtx)}
	end.

-spec try_recover(fun((Error) -> result:result(NewError, Out)), parser(In, Error, Out, C)) -> parser(In, NewError, Out, C).
try_recover(Recovery, Parser) ->
	partial:func(fun try_recover/3, [Recovery, Parser]).

try_recover(Recovery, Parser, Context) ->
	RunResult = Parser(Context),
	result:try_recover(fun(ErrorCtx) ->
		%io:format("ErrorCtx: ~p~n", [ErrorCtx]),
		ErrorReason = parse_ctx:parse_result(ErrorCtx),
		RecoveryParser = Recovery(ErrorReason),
		RecoveryParser(ErrorCtx)
	end, RunResult).

-ifdef(TEST).

try_recover_test_() ->
	[ fun() ->
		{ok, 1} = parse:string(<<"hi">>, try_recover(compose:never(), parse:success(1)))
	end
	, fun() ->
		{ok, 2} = parse:string(<<"hi">>, try_recover(fun(N) -> parse:success(N + 1) end, parse:fail(1)))
	end
	, fun() ->
		{error, #{ reason := 2 }} = parse:string(<<"hi">>, try_recover(fun(N) -> parse:fail(N + 1) end, parse:fail(1)))
	end
	].

-endif.

-spec first_of([ parser(A, term(), B, C)]) -> parser(A, no_parsers_matched, B, C).
first_of(Parsers) ->
	partial:func(fun first_of/2, [Parsers]).

first_of([], Context) ->
	fail(no_parsers_matched, Context);
first_of([ Parser | Tail], Context) ->
	ParserResult = Parser(Context),
	result:try_recover(fun(_BadCtx) ->
		first_of(Tail, Context)
	end, ParserResult).

-ifdef(TEST).

first_of_test_() ->
	[ ?_assertEqual({ok, 1}, parse:string("hi", parse:first_of([parse:success(1)])))
	, ?_assertMatch({error, #{ reason := no_parsers_matched}}, parse:string("hi", parse:first_of([parse:fail(first), parse:fail(second)])))
	, ?_assertEqual({ok, 2}, parse:string("hi", parse:first_of([parse:fail(first), parse:success(2)])))
	].

-endif.


-spec either(parser(In, X, B, C), parser(In, X, B, C)) -> parser(In, X, B, C).
either(Parser1, Parser2) ->
	first_of([Parser1, Parser2]).

-ifdef(TEST).

either_test_() ->
	[ fun() ->
		{ok, 1} = parse:string(<<"hi">>, parse:either(parse:success(1), parse:success(2)))
	end
	, fun() ->
		{ok, 2} = parse:string(<<"hi">>, parse:either(parse:fail(1), parse:success(2)))
	end
	, fun() ->
		{error, #{ reason := no_parsers_matched}} = parse:string(<<"hi">>, parse:either(parse:fail(1), parse:fail(2)))
	end
	].

-endif.

-spec fold(fun((A, Accumulator) -> Accumulator), parser(Accumulator, X, A, C)) -> parser(Accumulator, X, Accumulator, C).
fold(Folder, Parser) ->
	partial:func(fun fold/3, [Folder, Parser]).

fold(Folder, Parser, Context) ->
	ParseResult = Parser(Context),
	result:map_ok(fun(Ctx) ->
		parse_ctx:map(fun(ToFoldIn) ->
			FirstAccumulator = parse_ctx:parse_result(Context),
			Folder(ToFoldIn, FirstAccumulator)
		end, Ctx)
	end, ParseResult).


map_n(Mapper, Parsers) ->
	PartialMapper = partial:func(Mapper, []),
	SetMapper = parse:success(PartialMapper),
	parse:and_then(fun(_InitMapper) ->
		partial:func(fun map_n_do/2, [Parsers])
	end, SetMapper).

map_n_do([], Context) ->
	{ok, Context};
map_n_do([Parser | Tail], Context) ->
	ParserResult = Parser(Context),
	%io:format("map_n parser result: ~p~n", [ParserResult]),
	WithNewMapper = result:map_ok(fun(OkCtx) ->
		parse_ctx:map(fun(NewCtx) ->
			OldCtx = parse_ctx:parse_result(Context),
			OldCtx(NewCtx)
		end, OkCtx)
	end, ParserResult),
	result:and_then(fun(NewCtx) ->
		map_n_do(Tail, NewCtx)
	end, WithNewMapper).

-ifdef(TEST).

map_n_test_() ->
	[ ?_assertEqual({ok, 5}, parse:string("hi", parse:map_n(fun(A, B) -> A + B end, [ parse:success(3), parse:success(2)])))
	, ?_assertMatch({error, #{ reason := first}}, parse:string("hi", parse:map_n(fun(_, _) -> {error, map} end, [ parse:fail(first), parse:fail(second)])))
	, ?_assertMatch({error, #{ reason := second}}, parse:string("hi", parse:map_n(fun(_, _) -> {error, map} end, [ parse:success(first), parse:fail(second)])))
	].

-endif.

discard(Parser) ->
	partial:func(fun discard/2, [Parser]).

discard(Parser, Context) ->
	ParseResult = Parser(Context),
	result:map_ok(fun(Ctx) ->
		parse_ctx:parse_result(Ctx, parse_ctx:parse_result(Context))
	end, ParseResult).

default_regex_compile() -> [unicode, anchored, bsr_unicode, ucp].
default_regex_run() -> [{capture, all, binary}].

regex(RegEx) ->
	regex_advanced(default_regex_compile(), default_regex_run(), RegEx).

regex_advanced(CompileOpts, RunOpts, RegEx) ->
	partial:func(fun run_regex/3, [ re:compile(RegEx, [anchored | CompileOpts]), RunOpts]).

run_regex({error, E}, _RunOpts, Context) ->
	fail(E, Context);
run_regex({ok, Compiled}, RunOpts, Context) ->
	Subject = parse_ctx:source_string(Context),
	case re:run(Subject, Compiled, RunOpts) of
		nomatch ->
			fail(nomatch, Context);
		{error, Error} ->
			fail(Error, Context);
		{match, Match} ->
			ChompSize = regex_chomp_size(Match),
			ChompedCtx = parse_ctx:chomp_and_update(Context, ChompSize),
			success(Match, ChompedCtx)
	end.

regex_chomp_size([{Start, End} | _Tail]) ->
	Start + End;
regex_chomp_size([FullResult | _Tail]) when is_list(FullResult) ->
	regex_chomp_size([ list_to_binary(FullResult)]);
regex_chomp_size([FullResult | _Tail]) when is_binary(FullResult) ->
	size(FullResult).


-ifdef(TEST).

regex_test_() ->
	[ fun() ->
		{ok, [<<"453">>, <<"453">>]} = parse:string(<<"453abc">>, regex("([\\d]+)"))
	end
	, fun() ->
		{error, #{ reason := nomatch }} = parse:string(<<"abc123">>, regex("[\\d]="))
	end
	].

-endif.

is_string(Test) ->
	partial:func(fun is_string/3, [Test, size(Test)]).

is_string(Test, TestSize, Context) ->
	Subject = parse_ctx:source_string(Context),
	case Subject of
		<<Test:TestSize/binary, _/binary>> ->
			ChompedCtx = parse_ctx:chomp_and_update(Context, TestSize),
			success(Test, ChompedCtx);
		_ ->
			fail(nomatch, Context)
	end.

-ifdef(TEST).

is_string_test_() ->
	[ fun() ->
		{ok, <<"hello">>} = parse:string(<<"hello world">>, parse:is_string(<<"hello">>))
	end
	, fun() ->
		{error, #{ reason := nomatch }} = parse:string(<<"hello world">>, parse:is_string(<<"yo">>))
	end
	].

-endif.

%% makes backtrackable, and rolls back if it succeeded rather than failure.
peek(Parser) ->
	partial:func(fun peek/2, [Parser]).

peek(Parser, Context) ->
	FirstParseResult = Parser(Context),
	result:map(fun(GoodCtx) ->
		GoodResult = parse_ctx:parse_result(GoodCtx),
		parse_ctx:map(compose:always(GoodResult), Context)
	end, FirstParseResult).

-ifdef(TEST).

peek_test_() ->
	% to really test the peek, we need to see if the location didn't change, so
	% these get a bit complete.
	[ fun() ->
		Peek = peek(chomp_if($a)),
		LocationInstead = parse:and_then(fun($a) ->
			parse:location()
		end, Peek),
		?assertEqual({ok, {0,0}}, parse:string(<<"a greeting">>, LocationInstead))
	end
	, fun() ->
		Peek = peek(chomp_if($b)),
		LocationInstead = parse:and_then(fun(_) -> parse:fail(location_reached) end, Peek),
		?assertMatch({error, #{ reason := {expected_character, $b}}}, parse:string(<<"a greeting">>, LocationInstead))
	end
	].

-endif.

error_map(Context) ->
	#{ reason => parse_ctx:parse_result(Context)
	,  location => parse_ctx:location(Context)
	,  user_contexts => lists:reverse(parse_ctx:user_context(Context))
	,  offset => parse_ctx:offset(Context)
	,  source_sample => cut_to_next_newline(parse_ctx:source_string(Context))
	}.

cut_to_next_newline(Data) ->
	[Line | _Rest] = binary:split(unicode:characters_to_binary(Data), <<"\n">>),
	Line.


-ifdef(TEST).

swoot_test_() ->
	[ fun() ->
		{ok, {}} = parse:string(<<"whatever">>, parse:success({}))
	end
	, fun() ->
		{error, #{ location := {0,0}, offset := 0, reason := <<"i wanted to">>}} = parse:string(<<"whatever">>, parse:fail(<<"i wanted to">>))
	end
	, fun() ->
		{ok, "h"} = parse:string(<<"hello world">>, parse:chomp())
	end
	, fun() ->
		{error, #{ location := {0,0}, offset := 0, reason := end_of_input}} = parse:string(<<>>, parse:chomp())
	end
	, fun() ->
		{ok, {}} = parse:string(<<>>, parse:end_of_input())
	end
	, fun() ->
		{error, #{ location := {0,0}, offset := 0, reason := characters_remain}} = parse:string(<<"hello world">>, parse:end_of_input())
	end
	, fun() ->
		Mapper = fun([N]) -> N * 3 end,
		Parser = parse:map(Mapper, parse:chomp()),
		Result = parse:string(<<"a">>, Parser),
		{ok, $a * 3} = Result
	end
	, fun() ->
		{ok, $h} = parse:string(<<"hello">>, parse:chomp_if($h))
	end
	, fun() ->
		{error, #{ reason := {expected_character, $h}}} = parse:string(<<"yo">>, parse:chomp_if($h))
	end
	, fun() ->
		{ok, "hello"} = parse:string(<<"hello world">>, parse:chomp_n(5))
	end
	, fun() ->
		{error, #{reason := end_of_input}} = parse:string(<<"yo">>, parse:chomp_n(5))
	end
	, fun() ->
		{ok, 5} = parse:string(<<"hello">>, parse:discard(parse:chomp()), 5)
	end
	].



-endif.