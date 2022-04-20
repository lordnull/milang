-module(parse).

-type location() :: {pos_integer(), pos_integer()}.

-type parse_ok(Ok) :: {ok, non_neg_integer(), Ok}.

-type parse_err(Err) :: {err, #{ location := location()} , Err }.

-type parse_terminal(Err, Ok) :: parse_err(Err) | parse_ok(Ok).

-type next(Err, Ok, Acc) :: {next, non_neg_integer(), parser(Err, Ok), Acc}.

-type done(Err, Ok) :: {done, parse_terminal(Err, Ok)}.

-type acc_result(Err, Ok, Acc) :: next(Err, Ok, Acc) | done(Err, Ok).

-type parse_accumulator(Err, Ok, Acc) :: fun((parse_terminal(_, _), Acc) -> acc_result(Err, Ok, Acc)).

-type parse_push(Err, Ok, Acc) :: {push, non_neg_integer(), parser(_, _), parse_accumulator(Err, Ok, Acc), Acc}.

-type parse_non_terminal(Err, Ok, Acc) :: parse_push(Err, Ok, Acc).

-type parse_result(Err, Ok, Acc) :: parse_terminal(Err, Ok) | parse_non_terminal(Err, Ok, Acc).

-type parser(Err, Ok, Acc) :: fun((location(), binary()) -> parse_result(Err, Ok, Acc)).

-type parse_stack_frame(Err, Ok, Acc) :: {unicode:unicode_binary(), location(), parse_accumulator(Err, Ok, Acc), Acc}.

-type parse_stack(Err, Ok, Acc) :: [ parse_stack_frame(Err, Ok, Acc) ].

-type parser(Err, Ok) :: fun((location(), unicode:unicode_binary()) -> parse_result(Err, Ok, _)).

-export_type([parser/2, parser/3]).

-export([it/2]).
-export(
	[ regex/1
	, regex/2
	, peek/1
	, success/1
	, fail/1
	, character/1
	, string/1
	, map/2
	, andThen/2
	, first_of/1
	, series/1
	, repeat_for/2
	, repeat_at_most/2
	, repeat_until_error/1
	, repeat/3
	, chomp/0
	, chomp/1
	, chomp_if/1
	, chomp_while/1
	, chomp_until_end_or/1
	, chomp_until/1
	, tag/2
	, optional/1
	, lazy/1
	]).

-spec it(unicode:unicode_binary(), parser(Err, Ok)) -> parse_err(Err) | parse_ok(Ok).
it(Binary, Parser) ->
	Location = {1, 1},
	Stack = [],
	Result = Parser(Location, Binary),
	AccFun = fun default_acc/2,
	AccState = undefined,
	try parse(Binary, Location, Result, AccFun, AccState, Stack) of
		Out ->
			Out
	catch
		What:Why:Stacktrace ->
			io:format("Parser crash!~n"
				"    Crash: ~p:~p~n"
				"    Stack:~n~p~n"
				"    Binary: ~p~n"
				"    Parser: ~p~n"
				, [What, Why, Stacktrace, Binary, Parser]),
			{error, {parser_crash, What, Why}}
	end.

default_acc(E, _) -> E.

-spec parse(unicode:unicode_binary(), location(), parse_result(Err, Ok, Acc), parse_accumulator(Err, Ok, Acc), Acc, parse_stack(Err, Ok, Acc)) -> parse_err(Err) | parse_ok(Ok).

parse(Binary, Location, {push, Consume, NewParser, NewAccFun, NewAccState}, AccFun, AccState, Stack) ->
	PushItem = {Binary, Location, AccFun, AccState},
	NewStack = [ PushItem | Stack ],
	{NewLocation, NewBinary} = consume(Consume, Location, Binary),
	NewResult = NewParser(NewLocation, NewBinary),
	parse(NewBinary, NewLocation, NewResult, NewAccFun, NewAccState, NewStack);
parse(Binary, Location, RawResult, AccFun, AccState, Stack) ->
	Result = expand_result(RawResult, #{ binary => Binary, location => Location, acc_fun => AccFun, acc_state => AccState, stack => Stack}),
	case AccFun(Result, AccState) of
		{next, Consume, NewParser, NewState} ->
			{NewLocation, NewBinary} = consume(Consume, Location, Binary),
			NewResult = NewParser(NewLocation, NewBinary),
			parse(NewBinary, NewLocation, NewResult, AccFun, NewState, Stack);
		NewResult when Stack =:= [] ->
			finalize(Binary, Location, NewResult);
		NewResult ->
			[{NewBinary, NewLocation, NewAccFun, NewAccState} | NewStack] = Stack,
			parse(NewBinary, NewLocation, NewResult, NewAccFun, NewAccState, NewStack)
	end.

expand_result({error, Map}, _BaseError) when is_map(Map) ->
	{error, Map};
expand_result({error, Reason}, BaseError) ->
	{error, BaseError#{ reason => Reason }};
expand_result(Ok, _BaseError) ->
	Ok.

finalize(Binary, Location, {ok, Consume, Value}) ->
	{_, NewBinary} = consume(Consume, Location, Binary),
	{ok, Value, NewBinary};
finalize(_Binary, _Location, {error, Map} = Error) when is_map(Map) ->
	Error;
finalize(Binary, Location, {error, Reason}) ->
	Error = #{
		binary => Binary,
		location => Location,
		acc_fun => identity,
		acc_state => undefined,
		stack => [],
		reason => Reason
	},
	{error, Error}.

-spec consume(non_neg_integer(), location(), unicode:unicode_binary()) -> {location(), unicode:unicode_binary()}.
consume(HowMuch, {Row, Col}, Binary) ->
	case Binary of
		<<Consumed:HowMuch/binary, Rest/binary>> ->
			NewLocation = update_location(Consumed, Row, Col),
			{NewLocation, Rest};
		_ConsumedItAll ->
			NewLocation = update_location(Binary, Row, Col),
			{NewLocation, <<>>}
	end.

-spec update_location(unicode:unicode_binary(), pos_integer(), pos_integer()) -> location().
update_location(Binary, OldRow, OldCol) ->
	{ok, SplitRe} = re:compile(<<"\\R">>, [bsr_unicode]),
	case re:split(Binary, SplitRe, [{return, binary}]) of
		[Binary] ->
			{OldRow, OldCol + size(Binary)};
		Parts ->
			[LastPart | Rows] = lists:reverse(Parts),
			{OldRow + length(Rows), size(LastPart)}
	end.

-spec andThen(parser(Err, Ok), fun((Ok) -> parser(NextErr, NextOk))) -> parser(NextErr | Err, NextOk).
andThen(Parser, Next) ->
	fun(_, _) ->
		{push, 0, Parser, fun andThenAcc/2, {next, Next}}
	end.

andThenAcc({error, _} = Error, _) ->
	Error;
andThenAcc({ok, C, Value}, {next, Next}) ->
	NextParser = Next(Value),
	{next, C, NextParser, {consumed, C}};
andThenAcc({ok, C, Value}, {consumed, OldConsumed}) ->
	{ok, C + OldConsumed, Value}.

-spec regex(unicode:chardata()) -> parser(nomatch, [ unicode:unicode_binary() ]).
regex(Test) ->
	regex(Test, all).

-spec regex(unicode:chardata(), all | first | all_but_first | none) -> parser(nomatch, [unicode:unicode_binary()]).
regex(Test, CaptureMode) ->
	{ok, Compiled} = re:compile(Test, [anchored, unicode, ucp]),
	fun(_Location, Subject) ->
		regex(Subject, Compiled, CaptureMode)
	end.

regex(Subject, RE, CaptureMode) ->
	case re:run(Subject, RE, [{capture, CaptureMode, binary}]) of
		nomatch ->
			{error, nomatch};
		match ->
			{ok, 0, []};
		{match, Matched} ->
			Size = lists:foldl(fun(B, S) ->
				if
					size(B) > S ->
						size(B);
					true ->
						S
				end
			end, 0, Matched),
			{ok, Size, Matched}
	end.

-spec peek(unicode:chardata()) -> parser(nomatch, unicode:unicode_binary()).
peek(Test) ->
	RegExFun = regex(Test),
	fun(_Location, _Subject) ->
		{push, 0, RegExFun, fun peek_finalize/2, undefined}
	end.

peek_finalize({error, _} = Error, undefined) ->
	Error;
peek_finalize({ok, _, Args}, undefined) ->
	{ok, 0, Args}.

-spec success(Ok) -> parser(none(), Ok).
success(Out) ->
	fun(_, _) ->
		{ok, 0, Out}
	end.

-spec fail(Err) -> parser(Err, none()).
fail(Reason) ->
	fun(_, _) ->
		{error, Reason}
	end.

-spec character(C :: non_neg_integer()) -> parser({expected, C}, C).
character(C) ->
	fun(_, Subject) ->
		character(C, Subject)
	end.

character(C, <<C/utf8, _/binary>>) ->
	{ok, size(<<C>>), C};
character(C, _) ->
	{error, {expected, C}}.

-spec string(unicode:unicode_binary()) -> parser({expected, unicode:unicode_binary()}, unicode:unicode_binary()).
string(Needle) ->
	fun(_, Subject) ->
		NeedleSize = size(Needle),
		string(NeedleSize, Needle, Subject)
	end.

string(NeedleSize, Needle, Haystack) ->
	case Haystack of
		<<Needle:NeedleSize/binary, _/binary>> ->
			{ok, NeedleSize, Needle};
		_NoNeedle ->
			{error, {expected, Needle}}
	end.


-spec map(parser(Err, Ok), fun((Ok) -> NewOk)) -> parser(Err, NewOk).
map(Parser, Mapper) ->
	fun(_Location, _) ->
		{push, 0, Parser, fun map_acc/2, Mapper}
	end.

map_acc({ok, Consumed, Value}, Mapper) ->
	{ok, Consumed, Mapper(Value)};
map_acc(Error, _) ->
	Error.

-spec first_of([parser(_, Ok)]) -> parser(nomatch, Ok).
first_of(Parsers) ->
	fun(_Location, _Subject) ->
		{push, 0, fail(nomatch), fun first_of_acc/2, Parsers}
	end.

first_of_acc({ok, _, _} = Ok, _) ->
	Ok;
first_of_acc({error, _}, []) ->
	{error, nomatch};
first_of_acc({error, _}, [Parser | NewState]) ->
	{next, 0, Parser, NewState}.

combine_oks(Oks) ->
	{Values, TotalC} = lists:mapfoldl(fun({ok, C, V}, A) ->
		{V, A + C}
	end, 0, Oks),
	{ok, TotalC, Values}.

-spec series([parser(Err, Ok)]) -> parser(Err, [Ok]).
series([]) ->
	error(badarg);
series([FirstParser | Parsers]) ->
	fun(_Location, _Subject) ->
		{push, 0, FirstParser, fun series_acc/2, {Parsers, []}}
	end.

series_acc({error, _} = Error, _) ->
	Error;
series_acc({ok, _, _} = Ok, {[], TailResults}) ->
	Results = lists:reverse([Ok | TailResults]),
	combine_oks(Results);
series_acc({ok, C, _} = Ok, {[Parser | Tail], Results}) ->
	{next, C, Parser, {Tail, [Ok | Results]}}.

-spec repeat_for(non_neg_integer(), parser(Err, Ok)) -> parser(Err, [Ok]).
repeat_for(N, Parser) ->
	fun(_, _) ->
		{push, 0, Parser, fun repeat_for_acc/2, {Parser, N, []}}
	end.

repeat_for_acc({ok, C, _} = Ok, {Parser, N, Acc}) ->
	NewAcc = [Ok | Acc],
	NewN = N - 1,
	case NewN of
		0 ->
			combine_oks(lists:reverse(NewAcc));
		_ ->
			{next, C, Parser, {Parser, NewN, NewAcc}}
	end;
repeat_for_acc({error, _} = Error, {_, _, _}) ->
	Error.

-spec optional(parser(Err, Ok)) -> parser(Err, [Ok]).
optional(Parser) ->
	repeat_at_most(1, Parser).

-spec repeat_at_most(non_neg_integer(), parser(Err, Ok)) -> parser(Err, [Ok]).
repeat_at_most(N, Parser) ->
	fun(_, _) ->
		{push, 0, Parser, fun repeat_at_most_acc/2, {Parser, N, N, []}}
	end.

repeat_at_most_acc({ok, _, _}, {_Parser, Max, 0, _}) ->
	{error, {too_many, Max}};
repeat_at_most_acc({ok, C, _} = Ok, {Parser, Max, Left, Acc}) ->
	{next, C, Parser, {Parser, Max, Left - 1, [Ok | Acc]}};
repeat_at_most_acc({error, _}, {_Parser, _Max, _Left, Acc}) ->
	combine_oks(lists:reverse(Acc)).

-spec repeat_until_error(parser(Err, Ok)) -> parser(Err, [Ok]).
repeat_until_error(Parser) ->
	fun(_, _) ->
		{push, 0, Parser, fun repeat_until_error_acc/2, {Parser, []}}
	end.

repeat_until_error_acc({ok, C, _} = Ok, {Parser, Acc}) ->
	{next, C, Parser, {Parser, [Ok | Acc]}};
repeat_until_error_acc({error, _}, {_Parser, Acc}) ->
	combine_oks(lists:reverse(Acc)).

-spec repeat(non_neg_integer(), infinity | non_neg_integer(), parser(Err, Ok)) -> parser(Err, [Ok]).
repeat(Min, infinity, Parser) ->
	MinRepeat = repeat_for(Min, Parser),
	TailRepeat = repeat_until_error(Parser),
	Series = series([MinRepeat, TailRepeat]),
	Mapper = fun([A,B]) -> A ++ B end,
	map(Series, Mapper);
repeat(Min, Max, Parser) ->
	MinRepeat = repeat_for(Min, Parser),
	MaxRepeat = repeat_at_most(Max - Min, Parser),
	Series = series([MinRepeat, MaxRepeat]),
	Mapper = fun([A, B]) -> A ++ B end,
	map(Series, Mapper).

-spec tag(Tag, parser(Err, Ok)) -> parser(Err, {Tag, location(), Ok}).
tag(Tag, Parser) ->
	fun(Location, _) ->
		{push, 0, Parser, fun tag_acc/2, {Tag, Location}}
	end.

tag_acc({error, _} = Wut, _) ->
	Wut;
tag_acc({ok, C, Value}, {Tag, Location}) ->
	{ok, C, {Tag, Location, Value}}.

-spec chomp() -> parser(none(), unicode:unicode_binary()).
chomp() ->
	fun chomp_implementation/2.

-spec chomp(non_neg_integer()) -> parser(none(), unicode:unicode_binary()).
chomp(N) ->
	Repeat = repeat_for(N, chomp()),
	Map = fun unicode:characters_to_binary/1,
	parse:map(Repeat, Map).

-spec chomp_if(fun((char() | unicode:unicode_binary()) -> boolean())) -> parser({not_chompable, char() | unicode:unicode_binary()}, unicode:unicode_binary()).
chomp_if(Predicate) ->
	AndThen = fun(C) ->
		case Predicate(C) of
			true ->
				parse:success(C);
			false ->
				parse:fail({not_chompable, C})
		end
	end,
	andThen(chomp(), AndThen).

-spec chomp_while(fun((char() | unicode:unicode_binary()) -> boolean())) -> parser(none, unicode:unicode_binary()).
chomp_while(Predicate) ->
	fun(_, _) ->
		{push, 0, chomp(), fun chomp_acc/2, {Predicate, []}}
	end.

chomp_acc({ok, Size, C} = Ok, {Predicate, Acc}) ->
	case Predicate(C) of
		true ->
			{next, Size, chomp(), {Predicate, [Ok | Acc]}};
		false ->
			combine_chomped(Acc)
	end;
chomp_acc({error, #{ reason := end_of_input}}, {_, Acc}) ->
	combine_chomped(Acc);
chomp_acc(Error, _) ->
	Error.

-spec chomp_until(unicode:chardata()) -> parser(none(), unicode:unicode_binary()).
chomp_until(TestString) ->
	fun(_, _) ->
		{push, 0, chomp_until_implementation(TestString), fun chomp_until_acc/2, {TestString, []}}
	end.

chomp_until_acc({error, string_found}, {_, Acc}) ->
	combine_chomped(Acc);
chomp_until_acc({ok, C, _} = Ok, {TestString, Acc}) ->
	{next, C, chomp_until_implementation(TestString), {TestString, [Ok | Acc]}};
chomp_until_acc(Else, _) ->
	Else.

chomp_until_implementation(Needle) ->
	fun(Location, Subject) ->
		chomp_until_implementation(Location, Needle, size(Needle), Subject)
	end.

chomp_until_implementation(Location, Needle, NeedleSize, Subject) ->
	case Subject of
		<<Needle:NeedleSize/binary, _/binary>> ->
			{error, string_found};
		_ ->
			chomp_implementation(Location, Subject)
	end.

chomp_implementation(_Location, <<C/utf8, _/binary>>) ->
	{ok, size(<<C/utf8>>), C};
chomp_implementation(_, <<>>) ->
	{error, end_of_input}.

-spec chomp_until_end_or(unicode:chardata()) -> parser(any(), unicode:unicode_binary()).
chomp_until_end_or(TestString) ->
	fun(_, _) ->
		{push, 0, chomp_until_implementation(TestString), fun chomp_until_end_acc/2, {TestString, []}}
	end.

chomp_until_end_acc({error, #{ reason := end_of_input}}, {_, Acc}) ->
	combine_chomped(Acc);
chomp_until_end_acc({error, #{ reason := string_found}}, {_, Acc}) ->
	combine_chomped(Acc);
chomp_until_end_acc({ok, Size, _} = Ok, {TestString, Acc}) ->
	{next, Size, chomp_until_implementation(TestString), {TestString, [Ok | Acc]}};
chomp_until_end_acc(Error, _) ->
	Error.

combine_chomped(Acc) ->
	{ok, Size, List} = combine_oks(lists:reverse(Acc)),
	{ok, Size, unicode:characters_to_binary(List)}.

-spec lazy(fun(() -> parser(Err, Ok))) -> parser(Err, Ok).
lazy(Builder) ->
	fun(_, _) ->
		{push, 0, Builder(), fun lazy_acc/2, undefined}
	end.

lazy_acc(Result, undefined) ->
	Result.
