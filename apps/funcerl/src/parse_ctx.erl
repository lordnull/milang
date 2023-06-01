%%% @doc A datastructure to hold context about a parse sequence. It's sesperate
%%% from the parse module itself to make the modules shorter and more focused.
%%% @end
-module(parse_ctx).

-type row() :: non_neg_integer().
-type col() :: non_neg_integer().
-type location() :: {row(), col()}.
-type offset() :: non_neg_integer().
-record(context,
	{ source_string :: unicode:chardata()
	, parse_result :: term()
	, user_contexts = [] :: maybe_improper_list(term(), term() | [])
	, location  = {0, 0} :: location()
	, offset = 0 :: non_neg_integer()
	}).

-type context(State, Context) ::
	#context{ parse_result :: State, user_contexts :: maybe_improper_list(Context, Context | []) }.

-export_type(
	[ row/0
	, col/0
	, location/0
	, offset/0
	, context/2
	]).

-export(
	[ new/1
	, new/2
	, source_string/1
	, location/1
	, offset/1
	, parse_result/1
	, parse_result/2
	, user_context/1
	, push_context/2
	, pop_context/1
	, chomp_and_update/2
	, update_location/3
	, map/2
	]).
-export(
	[ chomp_n/2
	]).

-spec new(unicode:chardata()) -> context(undefined, term()).
new(SrcString) ->
	new(SrcString, undefined).

-spec new(unicode:chardata(), V) -> context(V, term()).
new(Str, State) ->
	#context{
		source_string = Str,
		parse_result = State
	}.

-spec source_string(context(term(), term())) -> unicode:chardata().
source_string(Context) ->
	Context#context.source_string.

-spec location(context(term(), term())) -> location().
location(Context) ->
	Context#context.location.

-spec offset(context(term(), term())) -> non_neg_integer().
offset(Ctx) ->
	Ctx#context.offset.

-spec parse_result(context(A, term())) -> A.
parse_result(Context) ->
	Context#context.parse_result.

-spec parse_result(context(term(), C), A) -> context(A, C).
parse_result(Context, Result) ->
	Context#context{ parse_result = Result}.

-spec map(fun((A) -> NextA ), context(A, C)) -> context(NextA, C).
map(Mapper, Context) ->
	InResult = parse_result(Context),
	OutResult = Mapper(InResult),
	parse_result(Context, OutResult).

-spec user_context(context(term(), C)) -> C.
user_context(#context{ user_contexts = UserContext}) ->
	UserContext.

-spec push_context(context(A, C), NewC)  -> context(A, maybe_improper_list(NewC, C)).
push_context(Context, UserContext) ->
	NewUserContext = [UserContext | Context#context.user_contexts],
	Context#context{ user_contexts = NewUserContext}.

-spec pop_context(context(A, maybe_improper_list(term(), T))) -> context(A, T).
pop_context(#context{ user_contexts = []} = Context) ->
	Context;
pop_context(#context{ user_contexts = [ _Popped | NewUserContext ]} = Context) ->
	Context#context{ user_contexts = NewUserContext }.


chomp_and_update(Context, ChompN) ->
	OldSource = source_string(Context),
	{ChompedStr, NewSource} = chomp_n(ChompN, OldSource),
	update_location(ChompedStr, NewSource, Context).

update_location(ChompedStr, NewSource, Context) ->
	OldLocation = location(Context),
	OldOffset = offset(Context),
	{NewLocation, NewOffsetDiff} = do_update_location(OldLocation, OldOffset, ChompedStr),
	Context#context{ source_string = NewSource, location = NewLocation, offset = OldOffset + NewOffsetDiff }.

chomp_n(N, Source) ->
	{_ChompsLeft, SourceLeft, Acc} = chomp_loop(N, Source, []),
	{lists:reverse(Acc), SourceLeft}.

%chomp_loop(ChompsLeft, Source, Acc) ->
chomp_loop(0, Source, Acc) ->
	{0, Source, Acc};
chomp_loop(N, <<>>, Acc) ->
	{N, <<>>, Acc};
chomp_loop(N, [], Acc) ->
	{N, <<>>, Acc};
chomp_loop(N, [ C | Tail], Acc) ->
	{ChompsLeft, NewSrc, NewAcc} = chomp_loop(N, C, Acc),
	case NewSrc of
		<<>> ->
			chomp_loop(ChompsLeft, Tail, NewAcc);
		[] ->
			chomp_loop(ChompsLeft, Tail, NewAcc);
		_ ->
			chomp_loop(ChompsLeft, [NewSrc | Tail], NewAcc)
	end;
chomp_loop(N, C, Acc) when is_integer(C) ->
	chomp_loop(N - 1, [], [ C | Acc]);
chomp_loop(N, <<C/utf8, Tail/binary>>, Acc) ->
	chomp_loop(N - 1, Tail, [C | Acc]).

do_update_location(Location, Offset, <<>>) ->
	{Location, Offset};
do_update_location(Location, Offset, []) ->
	{Location, Offset};
do_update_location(Location, OldOffset, Str) ->
	case re:split(Str, "^\\R", [{newline, any}, bsr_unicode, unicode, ucp]) of
		[Pre, Caputured, Post] ->
			AddedOffset = count_characters([Pre, Caputured]),
			{OldRow, _} = Location,
			NewLocation = {OldRow + 1, 0},
			do_update_location(NewLocation, OldOffset + AddedOffset, Post);
		_NoSplit ->
			{_C, NewStr} = chomp_n(1, Str),
			{OldRow, OldCol} = Location,
			NewLocation = {OldRow, OldCol + 1},
			NewOffset = OldOffset + 1,
			do_update_location(NewLocation, NewOffset, NewStr)
	end.

count_characters(String) ->
	count_characters(String, 0).

count_characters([], Acc) ->
	Acc;
count_characters(<<>>, Acc) ->
	Acc;
count_characters([ SubString | Tail ], Acc) ->
	count_characters(Tail, count_characters(SubString, Acc));
count_characters(<<_C/utf8, Rest/binary>>, Acc) ->
	count_characters(Rest, Acc + 1);
count_characters(N, Acc) when is_integer(N) ->
	Acc + 1.

