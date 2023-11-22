%%% @doc Additional functions to parse arbitrary bits of unicode text. While
%%% some of the functions mimic that in the parse module, that's just it doing
%%% convience things. For example, many of the functions will do chomping for
%%% you, rather than in the parse module with mostly will chomp nothing.
%%% @end.
-module(parse_do).

-include_lib("eunit/include/eunit.hrl").

-export(
	[ chomp_if/1
	, chomp_if/2
	, chomp_while/1
	, chomp_until/1
	, chomp_until_end_or/1
	, tag/2
	, joined_sequence/2
%	, digit/0
%	, whitespace/0
%	, ws/0

	]).

%% @doc Regex version of `parse:chomp_if/1`. Give a regular expression, and
%% chomp the next grapheme if it passes. The regex is always anchored and
%%% unicode. This is because sometimes it's easier to lean on the regex lib and
%% it's ability to check for unicode categories.
chomp_if(Test) ->
	chomp_if(Test, {expected, Test}).

chomp_if(Test, OnError) ->
	parse:chomp_if(regex_validator(Test), OnError).

%% @doc the looping version of above.
chomp_while(Test) ->
	parse:chomp_while(regex_validator(Test)).

chomp_until(Test) ->
	parse:chomp_until(regex_validator(Test)).

chomp_until_end_or(Test) ->
	parse:chomp_until_end_or(regex_validator(Test)).

compile_regex(Test) ->
	CompileOpts = [unicode, anchored, bsr_unicode, ucp],
	re:compile(Test, CompileOpts).

regex_validator(Test) ->
	{ok, Regex} = compile_regex(Test),
	fun(Grapheme) ->
		case re:run([Grapheme], Regex) of
			nomatch ->
				false;
			{match, _} ->
				true
		end
	end.

tag(TagName, Parser) ->
	parse:map_n(fun(Location, UserParser) ->
		{TagName, Location, UserParser}
	end, [parse:location(), Parser]).

joined_sequence(JoinParser, ElementParser) ->
	parse:loop({element, JoinParser, ElementParser, []}, fun joined_sequence/1).

joined_sequence({element, JoinParser, ElementParser, Acc}) ->
	parse:map(fun(Element) ->
		{step, {join_or_done, JoinParser, ElementParser, [Element | Acc]}}
	end, ElementParser);

joined_sequence({join_or_done, JoinParser, ElementParser, Acc}) ->
	MappedJoin = parse:map(fun(Join) ->
		{step, {element, JoinParser, ElementParser, [Join | Acc]}}
	end, JoinParser),
	parse:first_of(
		[ MappedJoin
		, parse:lazy(fun() -> parse:success({done, lists:reverse(Acc)}) end)
		]).
