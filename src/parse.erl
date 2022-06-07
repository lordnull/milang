-module(parse).

-type location() :: {pos_integer(), pos_integer()}.

-type parse_ok(Ok) :: {ok, non_neg_integer(), Ok}.

-type parse_err(Err) :: {error, #{ location := location()} , Err }.

-type parse_terminal(Err, Ok) :: parse_err(Err) | parse_ok(Ok).

-type next(Err, Ok, Acc) :: {next, non_neg_integer(), parser(Err, Ok), Acc}.

-type done(Err, Ok) :: {done, parse_terminal(Err, Ok)}.

-type acc_result(Err, Ok, Acc) :: next(Err, Ok, Acc) | done(Err, Ok).

-type parse_accumulator(Err, Ok, Acc) :: fun((parse_terminal(_, _), Acc) -> acc_result(Err, Ok, Acc)).

-type parse_push(Err, Ok, ContextTag, Acc) :: {push, non_neg_integer(), parser(_, _), {set_tag, ContextTag} | undefined, parse_accumulator(Err, Ok, Acc), Acc}.

-type parse_non_terminal(Err, Ok, ContextTag, Acc) :: parse_push(Err, Ok, ContextTag, Acc).

-type parse_result(Err, Ok, ContextTag, Acc) :: parse_terminal(Err, Ok) | parse_non_terminal(Err, Ok, ContextTag, Acc).

-type parser(Err, Ok, Acc) :: fun((location(), binary()) -> parse_result(Err, Ok, _, Acc)).

-type parse_stack_frame(Err, Ok, ContextTag, Acc) :: {unicode:unicode_binary(), location(), parse_accumulator(Err, Ok, Acc), Acc, parser_context(ContextTag)}.

-type parse_stack(Err, Ok, Tag, Acc) :: [ parse_stack_frame(Err, Ok, Tag, Acc) ].

-type parser(Err, Ok) :: fun((location(), unicode:unicode_binary()) -> parse_result(Err, Ok, _, _)).

-type parser_context(ContextTag) :: #{ tag := ContextTag, mf := {atom(), atom()}, type := remote | local, env := any()}.

-export_type([parser/2, parser/3]).


-export([it/2]).
-export(
	[ regex/1
	, regex/2
	, peek/1
	, test/1
	, set_tag/2
	, success/1
	, fail/1
	, recover/2
	, character/1
	, string/1
	, map/2
	, unless/2
	, andThen/2
	, first_of/1
	, series/1
	, repeat_for/2
	, repeat_at_most/2
	, repeat_until/2
	, repeat_until_error/1
	, repeat_when/2
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
	, end_of_input/0
	]).

-spec it(unicode:unicode_binary(), parser(Err, Ok)) -> parse_err(Err) | {ok, Ok, unicode:unicode_binary()}.
it(Binary, Parser) ->
	Location = {1, 1},
	Stack = [],
	Result = Parser(Location, Binary),
	AccFun = fun default_acc/3,
	AccState = undefined,
	try parse(Binary, Location, Result, AccFun, AccState, create_parser_context(Parser, root), Stack) of
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

-spec default_acc(E, _, _) -> E.
default_acc(E, _, _) -> E.

-spec parse(unicode:unicode_binary(), location(), parse_result(Err, Ok, Tag, Acc), parse_accumulator(Err, Ok, Acc), Acc, parser_context(Tag), parse_stack(Err, Ok, Tag, Acc)) -> parse_err(Err) | {ok, Ok, unicode:unicode_binary()}.

parse(Binary, Location, {push, Consume, NewParser, MaybeNewTag, NewAccFun, NewAccState}, AccFun, AccState, Context, Stack) ->
	PushItem = {Binary, Location, AccFun, AccState, Context},
	NewStack = [ PushItem | Stack ],
	{NewLocation, NewBinary} = consume(Consume, Location, Binary),
	NewTag = case MaybeNewTag of
		undefined ->
			maps:get(tag, Context);
		{set_tag, NT} ->
			NT
	end,
	NewContext = create_parser_context(NewParser, NewTag),
	NewResult = NewParser(NewLocation, NewBinary),
	parse(NewBinary, NewLocation, NewResult, NewAccFun, NewAccState, NewContext, NewStack);
parse(Binary, Location, RawResult, AccFun, AccState, Context, Stack) ->
	Result = expand_result(RawResult, #{ location => Location, parser_context => Context}),
	AccContext = #{ location => Location, tag => maps:get(tag, Context)},
	case AccFun(Result, AccContext, AccState) of
		{next, Consume, NewParser, NewState} ->
			{NewLocation, NewBinary} = consume(Consume, Location, Binary),
			NewContext = create_parser_context(NewParser, maps:get(tag, Context)),
			NewResult = NewParser(NewLocation, NewBinary),
			parse(NewBinary, NewLocation, NewResult, AccFun, NewState, NewContext, Stack);
		NewResult when Stack =:= [] ->
			finalize(Binary, Location, NewResult);
		NewResultSmall ->
			NewResult = expand_result(NewResultSmall, #{ location => Location, parser_context => Context}),
			[{NewBinary, NewLocation, NewAccFun, NewAccState, NewContext} | NewStack] = Stack,
			parse(NewBinary, NewLocation, NewResult, NewAccFun, NewAccState, NewContext, NewStack)
	end.

-spec create_parser_context(parser(_, _), Tag) -> parser_context(Tag).
create_parser_context(Parser, Tag) ->
	InfoProplist = erlang:fun_info(Parser),
	Module = proplists:get_value(module, InfoProplist),
	Name = proplists:get_value(name, InfoProplist),
	Type = proplists:get_value(type, InfoProplist),
	Env = proplists:get_value(env, InfoProplist),
	#{ mf => {Module, Name}, type => Type, env => Env, tag => Tag}.

expand_result({error, SomeError} = Error, _BaseError) when is_map(SomeError) ->
	Error;
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

-spec set_tag(term(), parser(Err, Ok)) -> parser(Err, Ok).
set_tag(Tag, Parser) ->
	fun(_, _) ->
		{push, 0, Parser, {set_tag, Tag}, fun default_acc/3, undefined}
	end.

-spec end_of_input() -> parser(still_input_left, ok).
end_of_input() ->
	fun end_of_input_implementation/2.

end_of_input_implementation(_, <<>>) -> {ok, 0, ok};
end_of_input_implementation(_, _) -> {error, still_input_left}.

-spec andThen(parser(Err, Ok), fun((Ok) -> parser(NextErr, NextOk))) -> parser(NextErr | Err, NextOk).
andThen(Parser, Next) ->
	fun(_, _) ->
		{push, 0, Parser, undefined, fun andThenAcc/3, {next, Next}}
	end.

andThenAcc({error, _} = Error, _, _) ->
	Error;
andThenAcc({ok, C, Value}, _, {next, Next}) ->
	NextParser = Next(Value),
	{next, C, NextParser, {consumed, C}};
andThenAcc({ok, C, Value}, _, {consumed, OldConsumed}) ->
	{ok, C + OldConsumed, Value}.

-spec recover(parser(OldErr, Ok), fun((OldErr) -> parser(NewErr, Ok))) -> parser(NewErr, Ok).
recover(Parser, Recovery) ->
	fun(_, _) ->
		{push, 0, Parser, undefined, fun recover_acc/3, Recovery}
	end.

recover_acc({ok, _, _} = Ok, _, _) ->
	Ok;
recover_acc({error, _} = Error, _, undefined) ->
	Error;
recover_acc({error, Why}, _, Recovery) ->
	{next, 0, Recovery(maps:get(reason, Why)), undefined}.

-spec unless(parser(Err, Ok), fun((Ok) -> {error, NewErr} | ok)) -> parser(Err | NewErr, Ok).
unless(Parser, Predicate) ->
	fun(_Location, _Subject) ->
		{push, 0, Parser, undefined, fun unless_acc/3, Predicate}
	end.

unless_acc({ok, _, Value} = Success, _, Predicate) ->
	case Predicate(Value) of
		{error, _} = Error ->
			Error;
		ok ->
			Success
	end;
unless_acc(Error, _, _Predicate) ->
	Error.

-spec regex(unicode:chardata()) -> parser(nomatch, [ unicode:unicode_binary() ]).
regex(Test) ->
	regex(Test, all).

-spec regex(unicode:chardata(), all | first | all_but_first | none) -> parser(nomatch, [unicode:unicode_binary()]).
regex(Test, CaptureMode) ->
	{ok, Compiled} = re:compile(Test, [anchored, unicode, ucp]),
	fun(_Location, Subject) ->
		regex(Subject, Compiled, CaptureMode, Test)
	end.

regex(Subject, RE, CaptureMode, Test) ->
	case re:run(Subject, RE, [{capture, CaptureMode, binary}]) of
		nomatch ->
			{error, {nomatch, Test}};
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
		{push, 0, RegExFun, undefined, fun peek_finalize/3, undefined}
	end.

peek_finalize({error, _} = Error, _, undefined) ->
	Error;
peek_finalize({ok, _, Args}, _, undefined) ->
	{ok, 0, Args}.

%% @doc Run the parser, but do not consume any characters.
-spec test(parser(Err, Ok)) -> parser(Err, Ok).
test(Parser) ->
	fun(_Location, _Subject) ->
		{push, 0, Parser, undefined, fun test_finalize/3, undefined}
	end.

test_finalize({error, _} = Error, _, undefined) ->
	Error;
test_finalize({ok, _, Args}, _, undefined) ->
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
	{error, {expected, unicode:characters_to_binary([C])}}.

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
		{push, 0, Parser, undefined, fun map_acc/3, Mapper}
	end.

map_acc({ok, Consumed, Value}, _, Mapper) ->
	{ok, Consumed, Mapper(Value)};
map_acc(Error, _, _) ->
	Error.

-spec first_of([parser(_, Ok)]) -> parser(nomatch, Ok).
first_of([]) ->
	fail(no_parsers_defined);
first_of(Parsers) ->
	[ FirstParser | TailParsers] = Parsers,
	fun(_Location, _Subject) ->
		{push, 0, FirstParser, undefined, fun first_of_acc/3, {TailParsers, []}}
	end.

first_of_acc({ok, _, _} = Ok, _, _) ->
	Ok;
first_of_acc({error, Error}, Ctx, {Parsers, Errors}) ->
	#{ location := Location, tag := Tag} = Ctx,
	#{ reason := Reason} = Error,
	MininmalError = #{ location => Location, tag => Tag, reason => Reason },
	NewErrors = [ MininmalError | Errors ],
	case Parsers of
		[] ->
			ErrorsInOrder = lists:reverse(NewErrors),
			{error, ErrorsInOrder};
		[ NextParser | Tail ] ->
			{next, 0, NextParser, {Tail, NewErrors}}
	end.

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
		{push, 0, FirstParser, undefined, fun series_acc/3, {Parsers, []}}
	end.

series_acc({error, Error}, _, {_, ResultsSoFar}) ->
	StepN = length(ResultsSoFar) + 1,
	#{reason := CoreError } = Error,
	NewError = Error#{ reason => {step, StepN, CoreError}},
	{error, NewError};
series_acc({ok, _, _} = Ok, _, {[], TailResults}) ->
	Results = lists:reverse([Ok | TailResults]),
	combine_oks(Results);
series_acc({ok, C, _} = Ok, _, {[Parser | Tail], Results}) ->
	{next, C, Parser, {Tail, [Ok | Results]}}.

-spec repeat_for(non_neg_integer(), parser(Err, Ok)) -> parser(Err, [Ok]).
repeat_for(N, Parser) ->
	fun(_, _) ->
		{push, 0, Parser, undefined, fun repeat_for_acc/3, {Parser, N, []}}
	end.

repeat_for_acc({ok, C, _} = Ok, _, {Parser, N, Acc}) ->
	NewAcc = [Ok | Acc],
	NewN = N - 1,
	case NewN of
		0 ->
			combine_oks(lists:reverse(NewAcc));
		_ ->
			{next, C, Parser, {Parser, NewN, NewAcc}}
	end;
repeat_for_acc({error, _} = Error, _, {_, _, _}) ->
	Error.

-spec optional(parser(Err, Ok)) -> parser(Err, [Ok]).
optional(Parser) ->
	repeat_at_most(1, Parser).

-spec repeat_at_most(non_neg_integer(), parser(Err, Ok)) -> parser(Err, [Ok]).
repeat_at_most(N, Parser) ->
	fun(_, _) ->
		{push, 0, Parser, undefined, fun repeat_at_most_acc/3, {Parser, N, N, []}}
	end.

repeat_at_most_acc({ok, _, _}, _, {_Parser, Max, 0, _}) ->
	{error, {too_many, Max}};
repeat_at_most_acc({ok, C, _} = Ok, _, {Parser, Max, Left, Acc}) ->
	{next, C, Parser, {Parser, Max, Left - 1, [Ok | Acc]}};
repeat_at_most_acc({error, _}, _, {_Parser, _Max, _Left, Acc}) ->
	combine_oks(lists:reverse(Acc)).

-record(repeat_until, {
	parser, acc = [], consumed = 0
}).

-spec repeat_until(parser(ElementError, Ok), parser(TerminalError, _)) -> parser(ElementError | TerminalError, [ Ok ]).
repeat_until(ElementParser, TerminalParser) ->
	InternalElementParser = map(ElementParser, fun(E) -> {element, E} end),
	InternalTerminalParser = map(TerminalParser, fun(T) -> {terminal, T} end),
	ParserProper = first_of([InternalTerminalParser, InternalElementParser]),
	State = #repeat_until{ parser = ParserProper },
	fun(_, _) ->
		{push, 0, ParserProper, undefined, fun repeat_until_acc/3, State}
	end.

repeat_until_acc({ok, C, {terminal, T}}, _, State) ->
	Consumed = C + State#repeat_until.consumed,
	Elements = lists:reverse(State#repeat_until.acc),
	{ok, Consumed, {Elements, T}};
repeat_until_acc({ok, C, {element, E}}, _, State) ->
	NewConsumed = State#repeat_until.consumed + C,
	NewAcc = [E | State#repeat_until.acc],
	NewState = State#repeat_until{ consumed = NewConsumed, acc = NewAcc},
	Parser = NewState#repeat_until.parser,
	{next, C, Parser, NewState};
repeat_until_acc({error, _} = Error, _, _) ->
	Error.

-record(repeat_when, {
	parser, acc = [], consumed = 0
}).

-spec repeat_until_error(parser(_, Ok)) -> parser(none(), [ Ok ]).
repeat_until_error(Parser) ->
	fun(_, _) ->
		{push, 0, Parser, undefined, fun repeat_until_error_acc/3, {Parser, []}}
	end.

repeat_until_error_acc({error, _}, _, {_, Acc}) ->
	combine_oks( lists:reverse(Acc) );
repeat_until_error_acc({ok, C, _} = Ok, _, {Parser, Acc}) ->
	{next, C, Parser, {Parser, [ Ok | Acc]}}.

-spec repeat_when(parser(ElementError, Ok), parser(_, _)) -> parser(ElementError, Ok).
repeat_when(Primary, DoContinue) ->
	RecoveredContinue = recover(DoContinue, fun(Error) ->
		success({error, Error})
	end),
	Parser = series([Primary, RecoveredContinue]),
	State = #repeat_when{ parser = Parser, acc = [], consumed = 0},
	fun(_, _) ->
		{push, 0, Parser, undefined, fun repeat_when_acc/3, State}
	end.

repeat_when_acc({error, _} = Error, _, _) ->
	Error;
repeat_when_acc({ok, C, [Primary, MaybeContinue]}, _, State) ->
	NewConsumed = State#repeat_when.consumed + C,
	AccWithPrimary = [Primary | State#repeat_when.acc],
	case MaybeContinue of
		{error, _AllDone} ->
			Elements = lists:reverse(AccWithPrimary),
			{ok, NewConsumed, Elements};
		SomeValidThing ->
			NewAcc = [ SomeValidThing | AccWithPrimary ],
			NewState = State#repeat_when{ consumed = NewConsumed, acc = NewAcc },
			Parser = NewState#repeat_when.parser,
			{next, C, Parser, NewState}
	end.

-spec repeat(non_neg_integer(), non_neg_integer(), parser(Err, Ok)) -> parser(Err, [Ok]).
repeat(Min, Max, Parser) ->
	MinRepeat = repeat_for(Min, Parser),
	MaxRepeat = repeat_at_most(Max - Min, Parser),
	Series = series([MinRepeat, MaxRepeat]),
	Mapper = fun([A, B]) -> A ++ B end,
	Mapped = map(Series, Mapper),
	RemapError =
		fun
			({step, 2, {too_many, WrongMax}}) ->
				fail({too_many, WrongMax + Min});
			({step, 1, Error}) ->
				fail(Error);
			(Error) ->
				fail(Error)
		end,
	recover(Mapped, RemapError).


-spec tag(Tag, parser(Err, Ok)) -> parser(Err, {Tag, location(), Ok}).
tag(Tag, Parser) ->
	fun(Location, _) ->
		{push, 0, Parser, undefined, fun tag_acc/3, {Tag, Location}}
	end.

tag_acc({error, _} = Wut, _, _TagData) ->
	%io:format("Tag attempt failed.~n    TagData: ~p~n    Error: ~p~n", [_TagData, Wut]),
	Wut;
tag_acc({ok, C, Value}, _, {Tag, Location}) ->
	%io:format("Popping tag~n    Tag: ~p~n    Location:~p~n    Chomped:~p~n    Value: ~p~n", [Tag, Location, C, Value]),
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
		{push, 0, chomp(), undefined, fun chomp_acc/3, {Predicate, []}}
	end.

chomp_acc({ok, Size, C} = Ok, _, {Predicate, Acc}) ->
	case Predicate(C) of
		true ->
			{next, Size, chomp(), {Predicate, [Ok | Acc]}};
		false ->
			combine_chomped(Acc)
	end;
chomp_acc({error, #{ reason := end_of_input}}, _, {_, Acc}) ->
	combine_chomped(Acc);
chomp_acc(Error, _, _) ->
	Error.

-spec chomp_until(unicode:chardata()) -> parser(none(), unicode:unicode_binary()).
chomp_until(TestString) ->
	fun(_, _) ->
		{push, 0, chomp_until_implementation(TestString), undefined, fun chomp_until_acc/3, {TestString, []}}
	end.

chomp_until_acc({error, string_found}, _, {_, Acc}) ->
	combine_chomped(Acc);
chomp_until_acc({ok, C, _} = Ok, _, {TestString, Acc}) ->
	{next, C, chomp_until_implementation(TestString), {TestString, [Ok | Acc]}};
chomp_until_acc(Else, _, _) ->
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
		{push, 0, chomp_until_implementation(TestString), undefined, fun chomp_until_end_acc/3, {TestString, []}}
	end.

chomp_until_end_acc({error, #{ reason := end_of_input}}, _, {_, Acc}) ->
	combine_chomped(Acc);
chomp_until_end_acc({error, #{ reason := string_found}}, _, {_, Acc}) ->
	combine_chomped(Acc);
chomp_until_end_acc({ok, Size, _} = Ok, _, {TestString, Acc}) ->
	{next, Size, chomp_until_implementation(TestString), {TestString, [Ok | Acc]}};
chomp_until_end_acc(Error, _, _) ->
	Error.

combine_chomped(Acc) ->
	{ok, Size, List} = combine_oks(lists:reverse(Acc)),
	{ok, Size, unicode:characters_to_binary(List)}.

-spec lazy(fun(() -> parser(Err, Ok))) -> parser(Err, Ok).
lazy(Builder) ->
	fun(_, _) ->
		{push, 0, Builder(), undefined, fun lazy_acc/3, undefined}
	end.

lazy_acc(Result, _, undefined) ->
	Result.
