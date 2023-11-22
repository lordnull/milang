%%% @doc A datastructure to hold context about a parse sequence. It's sesperate
%%% from the parse module itself to make the modules shorter and more focused.
%%% @end
-module(parse_ctx).

-type row() :: pos_integer().
-type col() :: pos_integer().
-type location() :: {row(), col()}.
-type byte_offset() :: non_neg_integer().
-record(context,
	{ source_string :: unicode:chardata()
	, user_contexts = [] :: [{term(), location()}]
	, location  = {1, 1} :: location()
	, byte_offset = 0 :: non_neg_integer()
	}).

-type context(Context) ::
	#context{ user_contexts :: [ {Context, location()} ]}.

-export_type(
	[ row/0
	, col/0
	, location/0
	, byte_offset/0
	, context/1
	]).

-export(
	[ new/1
	, source_string/1
	, location/1
	, byte_offset/1
	, source_from_offset/1
	, user_context/1
	, push_context/2
	, pop_context/1
	, chomp/1
	, between_offsets/3
	, slice/3
	]).
-export(
	[ chomp_n/2
	, get_first_grapheme/1
	, count_graphemes/1
	]).

-spec new(unicode:chardata()) -> context(term()).
new(SrcString) ->
	#context{
		source_string = unicode:characters_to_nfkc_binary(SrcString)
	}.

-spec source_string(context(term())) -> unicode:chardata().
source_string(Context) ->
	Context#context.source_string.

-spec location(context(term())) -> location().
location(Context) ->
	Context#context.location.

-spec byte_offset(context(term())) -> non_neg_integer().
byte_offset(Ctx) ->
	Ctx#context.byte_offset.

-spec source_from_offset(context(term())) -> unicode:chardata().
source_from_offset(Context) ->
	#context{ byte_offset = Offset, source_string = Source} = Context,
	case Source of
		<<_:Offset/binary, Rest/binary>> ->
			Rest;
		_OffsetTooBig ->
			<<>>
	end.

-spec user_context(context(C)) -> C.
user_context(#context{ user_contexts = UserContext}) ->
	UserContext.

-spec push_context(context(C), C)  -> context(C).
push_context(Context, UserContext) ->
	NewUserContext = [{UserContext, Context#context.location} | Context#context.user_contexts],
	Context#context{ user_contexts = NewUserContext}.

-spec pop_context(context(C)) -> context(C).
pop_context(#context{ user_contexts = []} = Context) ->
	Context;
pop_context(#context{ user_contexts = [ _Popped | NewUserContext ]} = Context) ->
	Context#context{ user_contexts = NewUserContext }.


chomp(Context) ->
	Source = source_string(Context),
	Offset = byte_offset(Context),
	case Source of
		<<_:Offset/binary, Tail/binary>> ->
			update_location(Tail, Context);
		<<_:Offset/binary>> ->
			Context
	end.

chomp_n(0, Context) ->
	Context;
chomp_n(N, Context) ->
	chomp_n(N - 1, chomp(Context)).

%% @doc Take a grapheme from the string and update the row, column, and byte
%% offsets if that grapheme was removed.
update_location(String, Context) ->
	OldLocation = location(Context),
	OldOffset = byte_offset(Context),
	Grapheme = get_first_grapheme(String),
	NewOffsetDiff = size(Grapheme),
	NewLocation = do_update_location(OldLocation, Grapheme),
	Context#context{ location = NewLocation, byte_offset = OldOffset + NewOffsetDiff }.

get_first_grapheme(String) ->
	case re:run(String, "^\\X", [{capture, first, binary}, unicode]) of
		nomatch ->
			<<>>;
		{match, [Grapheme | _]} when is_binary(Grapheme) ->
			Grapheme;
		{match, LikelyError} ->
			io:format("invalid regex to get a grapheme: ~p~n", [LikelyError]),
			<<>>
	end.

count_graphemes(String) ->
	count_graphemes(String, 0).

count_graphemes(String, N) ->
	case get_first_grapheme(String) of
		<<>> ->
			N;
		Grapheme ->
			Size = size(Grapheme),
			<<_:Size/binary, Rest/binary>> = String,
			count_graphemes(Rest, N + 1)
	end.

do_update_location(Location, <<>>) ->
	Location;
do_update_location(Location, Grapheme) ->
	case re:run(unicode:characters_to_binary(Grapheme), "^\\R$", [{newline, any}, bsr_unicode, unicode, ucp]) of
		{match, _} ->
			{OldRow, _} = Location,
			{OldRow + 1, 1};
		nomatch ->
			{OldRow, OldCol} = Location,
			{OldRow, OldCol + 1}
	end.

between_offsets(EndOffset, BeginOffset, Context) when EndOffset > BeginOffset ->
	between_offsets(BeginOffset, EndOffset, Context);
between_offsets(BeginOffset, EndOffset, Context) ->
	Length = EndOffset - BeginOffset,
	slice(BeginOffset, Length, Context).

-spec slice(non_neg_integer(), non_neg_integer(), context(term())) -> unicode:chardata().
slice(Offset, Length, Context) ->
	Source = source_string(Context),
	case Source of
		<<_:Offset/binary, Caputured:Length/binary, _/binary>> ->
			Caputured;
		<<_:Offset/binary, Rest/binary>> ->
			Rest;
		_OffsetTooBig ->
			<<>>
	end.

