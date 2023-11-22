-module(parse_tests).

-include_lib("eunit/include/eunit.hrl").

chomp_string_test_() ->
	DoParse = fun(Subject, TestString) ->
		parse:string(Subject, parse:get_chomped_string(parse:chomp_string(TestString)))
	end,
	[ ?_assertMatch({ok, <<"abc">>}, DoParse(<<"abc">>, <<"abc">>))
	, ?_assertMatch({ok, <<"a">>}, DoParse(<<"abc">>, <<"a">>))
	, ?_assertMatch({error, #{ reason := {expected, <<"abc">>}}},DoParse(<<"xyz">>, <<"abc">>))
	, ?_assertMatch({ok, <<"hello world">>}, DoParse(<<"hello world">>, <<"hello world">>))
	, fun() ->
		case unicode:characters_to_binary("😀 👍") of
			String when is_binary(String) ->
				?assertMatch({ok, String}, DoParse(String, String));
			NotString ->
				error({predcondition_failed, NotString})
		end
	end
	, ?_assertMatch({ok, <<"he">>}, DoParse(<<"hello">>, <<"he">>))
	].

pipeline_test_() ->
	[ fun() ->
		Parser = parse:pipeline(parse:success(compose:id()),
			[ parse:keep(parse:success(good))
			]),
		Got = parse:string(<<"any">>, Parser),
		?assertEqual({ok, good}, Got)
	end
	, fun() ->
		Parser = parse:pipeline(parse:success(compose:id()),
			[ parse:discard(parse:chomp_if(compose:always(true), never))
			, parse:keep(parse:chomp_if(compose:always(true), never))
			, parse:discard(parse:chomp_if(compose:always(true), never))
			]),
		Got = parse:string(<<"any">>, Parser),
		?assertEqual({ok, {}}, Got)
	end
	, fun() ->
		ItWorked = parse:success(compose:id()),
		Token = parse:backtrackable(parse:chomp_string(<<"token">>)),
		IsSpace = parse:first_of(
			[ parse:map(compose:always(not_space), parse:backtrackable(parse:chomp_if(fun(C) -> C =/= <<" ">> end, never)))
			, parse:success(is_space)
			]),
		IsSpaceChecked = parse:and_then(fun
			(not_space) ->
				parse:fail({never, space_check_failed});
			(is_space) ->
				parse:commit({})
		end, IsSpace),

		Pipeline = parse:pipeline(ItWorked,
			[ parse:discard(Token)
			, parse:keep(IsSpaceChecked)
			]),

		ChompChar = parse:chomp_if(compose:always(true), never),
		ChompedChar = parse:get_chomped_string(ChompChar),
		FullParser = parse:and_then(fun(_) ->
			ChompedChar
		end, Pipeline),

		Got = parse:string(<<"token ">>, FullParser),
		?assertEqual({ok, <<" ">>}, Got)
	end
	].

first_of_complex_test_() ->
	[ fun() ->
		Inner1 = parse:and_then(fun(_) ->
			parse:fail(first_inner_fail)
		end, parse:chomp_if(compose:always(true), never)),
		Inner2 = parse:and_then(fun(_) ->
			parse:fail(second_inner_fail)
		end, parse:chomp_if(compose:always(true), never)),
		Success = parse:and_then(fun(_) ->
			parse:success(good)
		end, parse:chomp_if(compose:always(true), never)),
		Outer1 = parse:backtrackable(parse:first_of([Inner1, Inner2])),
		Outer2 = parse:first_of([Outer1, Success]),
		Got = parse:string(<<"abcdef">>, Outer2),
		?assertEqual({ok, good}, Got)
	end
	].

loop_success_test_() ->
	ParseA = parse_do:chomp_if("a"),
	ParseB = parse_do:chomp_if("b"),
	Nexter = fun
		(3) ->
			parse:map(fun(_) -> {step, finish_it} end, ParseB);
		(finish_it) ->
			parse:success({done, good});
		(N) ->
			parse:map(fun(_) -> {step, N + 1} end, ParseA)
	end,
	Looper = parse:loop(0, Nexter),
	[ fun() ->
		Got = parse:string(<<"aaab">>, Looper),
		?assertEqual({ok, good}, Got)
	end,
	fun() ->
		Got = parse:string(<<"aaab">>, parse:get_chomped_string(Looper)),
		?assertEqual({ok, <<"aaab">>}, Got)
	end
	].

loop_failable_test_() ->
	ParseA = parse:get_chomped_string(parse_do:chomp_if("a")),
	ParseB = parse:and_then(fun(Str) ->
		case Str of
			<<>> ->
				parse:success(ended);
			<<"b">> ->
				parse:success(ended);
			_ ->
				parse:fail(expected_b_or_end)
		end
	end, parse:get_chomped_string(parse:chomp_if(compose:always(true), never))),
	Nexter = fun
		(3) ->
			parse:map(fun(_) -> {step, finish_it} end, ParseB);
		(finish_it) ->
			parse:success({done, good});
		(N) ->
			parse:map(fun(_) -> {step, N + 1} end, ParseA)
	end,
	Looper = parse:loop(0, Nexter),
	[ fun() ->
		Got = parse:string(<<"aaa">>, Looper),
		?assertEqual({ok, good}, Got)
	end
	, fun() ->
		Got = parse:string(<<"aaab">>, Looper),
		?assertEqual({ok, good}, Got)
	end
	, fun() ->
		Got = parse:string(<<"ab">>, Looper),
		?assertMatch({error, #{reason := {expected, "a"}}}, Got)
	end
	, fun() ->
		Got = parse:string(<<"aaac">>, Looper),
		?assertMatch({error, #{reason := expected_b_or_end}}, Got)
	end
	].
