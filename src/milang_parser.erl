-module(milang_parser).

-export([run/1]).

-type consumed() :: non_neg_intger().
-type location() :: {pos_integer(), pos_integer()}.

-type parser(Parsed, State) :: fun((binary(), State) -> parse_return(Parsed, State)).

-type parser_init(ReturnAcc) -> fun(() -> ReturnAcc).

-type parser_resume(Parsed, Acc) :: fun((Parsed, Acc) -> {parser(Parsed, Acc), Acc}).

-type parse_return(Parsed, Acc)
    :: {ok, consumed(), Parsed}
    |  {error, term()}
    |  {push, consumed(), parser_resume(Parsed, Acc), parser_init(Acc)}
    |  {next, consumed(), Acc}
    .

-record(frame,
    { state
    , resume
    }).


run(String) ->
    {Parser, State} = root_init(),
    Stack = [],
    run(String, {1,1}, Parser, State, Stack).

run(String, Location, Parser, ParserState, Stack) ->
    case Parser(String, ParserState) of
        {ok, Consumed, Value} when Stack =:= []->
            <<_:Consumed/binary, Rest>> = String,
            {ok, Value, Rest};
        {ok, Consumed, Value} ->
            [ PreviousFrame | NewStack ] = Stack,
            #frame{ state = PreviousState, resume = Resume } = PreviousFrame,
            {NewParser, NewParserState} = Resume(Value, PreviousState),
            {NewLocation, NewString} = consume(Consumed, String),
            run(NewString, NewLocation, NewParser, NewParserState, NewStack);
        {next, Consumed, NewParserState} ->
            {NewLocation, NewString} = consume(Consumed, String),
            run(NewString, NewLocation, Parser, NewParserState, Stack);
        {push, Consumed, Resume, Init} ->
            Frame = #frame{ state = ParserState, resume = Resume },
            {NewLocation, NewString} = consume(Consumed, String),
            {NewParser, NewParserState} = Init(),
            NewState = [Frame | Stack ],
            run(NewString, NewLocation, NewParser, NewParserState, NewStack);
        {error, Wut} ->
            FullError = 
                #{ error => Wut
                ,  location => Location
                ,  string => String
                ,  parser => Parser
                ,  parser_state => ParserState
                ,  stack => Stack
                },
            {error, FullError}
            end.


root_init() ->
    {fun root/2, []}.

root_resume({comment, _}, Acc) ->
    {fun root/2, Acc};

root_resume(Value, Acc) ->
    {fun root/2, [Value | Acc]}.

root_push(TagString, InitFunc) ->
    {push, size(TagString), fun root_resume/2, InitFunc}.

root(<<>>, Acc) ->
    {ok, lists:reverse(Acc)};

root(<<"-module", Rest/binary>>, Acc) ->
    root_push(<<"-module">>, fun declaration_module_init/0);

root(<<"-import", Rest/binary>>, Acc) ->
    root_push(<<"-import">>, fun declaration_import_init/0);

root(<<"-type", Rest/binary>>, Acc) ->
    root_push(<<"-type">>, fun declaration_type_init/0);

root(<<"-class", Rest/binary>>, Acc) ->
    root_push(<<"-class">>, fun declaration_class_init/0);

root(<<"-alias", Rest/binary>>, Acc) ->
    root_push(<<"-alias">>, fun declaration_alias_init/0);

root(<<"{-", Rest/binary>>, Acc) ->
    root_push(<<"{-">>, fun comment_init/0);

root(String, Acc) ->
    case regex(String, whitespace()) of
        {ok, Size, _} ->
            {next, Size, Acc};
        {error, nomatch} ->
            root_push(<<>>, fun declartion_function_or_spec_init/0)
        end.

declaration_module_init() ->
    {declaration_module/2, space_before_name}.

declaration_module(<<"{-", Rest/binary>>, space_before_name) ->
    {push, size(<<"{-">>), declaration_module_resume/2, fun comment_init/0};

declaration_module(String, space_before_name) ->
    case regex(String, whitespace()) of
        {error, nomatch} ->
            {error, expected_comment_or_whitespace};
        {ok, Consumed, _} ->
            {next, Consumed, first_name_part}
        end;

declaration_module(String, first_name_part) ->
    case upcase_name(String) of
        {ok, Consumed, Value} ->
            {next, Consumed, {end_or_dot, [Value]}};
        {error, _} ->
            {error, expected_at_least_one_upcase_name}
        end;

declaration_module(<<$., _/binary>>, {end_or_dot, Acc}) ->
    {next, size(<<$.>>), {name_part, Acc}};

declaration_module(String, {end_or_dot, Acc}) ->
    case whitespace(String) of
        {ok, Consumed, _} ->
            {next, Consumed, }


        variable() ->
	RegEx = parse:regex("[\\p{Ll}_]\\w*", first),
	Tagged = parse:tag(variable, RegEx),
	Mapper = fun({T, L, [V]}) ->
		milang_ast:ast_node(L, <<>>, T, binary_to_atom(V))
	end,
	parse:set_tag(variable, parse:map(Tagged, Mapper)).

whitespace() -> "\\s+".

downcase_name() ->
	RegEx = parse:regex("\\p{Ll}\\w*", first),
	parse:map(RegEx, fun([V]) -> V end).

upcase_name() ->
	RegEx = parse:regex("\\p{Lu}\\w*", first),
	parse:map(RegEx, fun([V]) -> V end).
regex(Subject, REString) ->
	{ok, RE} = re:compile(REString, [anchored, unicode, ucp]),
    regext(Subject, RE, first).

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