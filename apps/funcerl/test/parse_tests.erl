-module(parse_tests).

-include_lib("eunit/include/eunit.hrl").

success_fail_and_character_test_() ->
	[ ?_assertEqual({ok, 5}, parse:string(<<"abc">>, parse:success(5)))
	, ?_assertMatch({error, #{ reason := test }}, parse:string(<<"abc">>, parse:fail(test)))
	, ?_assertEqual({ok, $a}, parse:string(<<"abc">>, parse:chomp_if($a)))
	, ?_assertMatch({error, #{ reason := {expected_character, $c}}}, parse:string(<<"abc">>, parse:chomp_if($c)))
	].

try_recover_test_() ->
	[ ?_assertEqual({ok, 5}, parse:string(<<"any">>, parse:try_recover(fun(_) ->
		parse:success(5)
	end, parse:fail(failure))))
	, ?_assertEqual({ok, good}, parse:string(<<"any">>, parse:try_recover(fun
		(failure1) ->
			parse:success(good);
		(failure2) ->
			parse:fail(should_not_get_here)
		end, parse:fail(failure1))))
	, ?_assertMatch({error, #{ reason := remapped_fail}}, parse:string(<<"any">>, parse:try_recover(fun
			(failure1) ->
				parse:fail(should_not_get_here);
			(failure2) ->
				parse:fail(remapped_fail)
		end, parse:fail(failure2))))
	, fun() ->
		InnerFailure = parse:fail(innermost_fail),
		InnerRemap = parse:try_recover(fun(innermost_fail) ->
			parse:fail(inner_recover_fail)
		end, InnerFailure),
		OuterFailure = parse:and_then(compose:never(), InnerRemap),
		OuterRecover = parse:try_recover(fun
			(inner_recover_fail) ->
				parse:success(good);
			(Nope) ->
				parse:fail({not_correct_error, Nope})
		end, OuterFailure),
		?assertEqual({ok, good}, parse:string(<<"any">>, OuterRecover))
	end
	].

is_string_test_() ->
	[ ?_assertMatch({ok, <<"abc">>}, parse:string(<<"abc">>, parse:is_string(<<"abc">>)))
	, ?_assertMatch({ok, <<"a">>}, parse:string(<<"abc">>, parse:is_string(<<"a">>)))
	, ?_assertMatch({error, _}, parse:string(<<"xyz">>, parse:is_string(<<"abc">>)))
	, ?_assertMatch({ok, <<"hello world">>}, parse:string(<<"hello world">>, parse:is_string(<<"hello world">>)))
	, fun() ->
		String = unicode:characters_to_binary("😀 👍"),
		?assertMatch({ok, String}, parse:string(String, parse:is_string(String)))
	end
	].

regex_test_() ->
	[ ?_assertMatch({ok, [<<"a">>]}, parse:string(<<"abc">>, parse:regex("\\w")))
	, ?_assertMatch({ok, [<<"123">>]}, parse:string(<<"123xyz">>, parse:regex("\\d+")))
	, ?_assertMatch({error, #{ reason := nomatch}}, parse:string(<<"abc">>, parse:regex("\\d")))
	, ?_assertMatch({error, #{ reason := nomatch}}, parse:string(<<"abc123">>, parse:regex("\\d")))
	, ?_assertMatch({ok, [<<"hello">>, <<"ello">>, <<"ll">>]}, parse:string(<<"hello">>, parse:regex_advanced(parse:default_regex_compile(), [{capture, all, binary}], "h(e(ll)o)")))
	].

unless_test_d() ->
	[ ?_assertMatch({error, #{ reason := test }}, parse:string(<<"abc">>, parse:unless(parse:chomp(), fun(_) -> {error, test} end)))
	, ?_assertMatch({error, #{ reason := {disallowed, $a}}}, parse:string(<<"abc">>, parse:unless(parse:chomp(), fun($a) -> {error, {disallowed, $a}} end)))
	, ?_assertMatch({ok, $a, <<"bc">>}, parse:string(<<"abc">>, parse:unless(parse:chomp(), fun(_) -> ok end)))
	].

chomp_test_() ->
	[ ?_assertMatch({ok, [$a]}, parse:string(<<"abc">>, parse:chomp()))
	, ?_assertMatch({ok, [$a, $b]}, parse:string(<<"abc">>, parse:chomp_n(2)))
	, ?_assertMatch({ok, $a}, parse:string(<<"abc">>, parse:chomp_if($a)))
	, ?_assertMatch({error, #{ reason := {expected_character, $q}}}, parse:string(<<"abc">>, parse:chomp_if($q)))
	%, ?_assertMatch({ok, <<"a">>, <<"bc">>}, parse:string(<<"abc">>, parse:chomp_while(fun(E) -> E =:= $a end)))
	%, ?_assertMatch({ok, <<"abc">>, <<>>}, parse:string(<<"abc">>, parse:chomp_while(fun(_) -> true end)))
	%, ?_assertMatch({ok, <<"abc">>, <<"xyz">>}, parse:string(<<"abcxyz">>, parse:chomp_until_end_or(<<"xyz">>)))
	%, ?_assertMatch({ok, <<"abc">>, <<>>}, parse:string(<<"abc">>, parse:chomp_until_end_or(<<"xyz">>)))
	%, ?_assertMatch({ok, <<>>, <<"abc">>}, parse:string(<<"abc">>, parse:chomp_while(fun(E) -> E =:= $z end)))
	%, fun() ->
	%	HappyFace = "😀",
	%	Subject = unicode:characters_to_binary([HappyFace, $a]),
	%	ExpectedChomp = unicode:characters_to_binary(HappyFace),
	%	Parser = parse:chomp_while(fun(E) -> E =/= $a end),
	%	Result = parse:string(Subject, Parser),
	%	?assertMatch({ok, ExpectedChomp, <<"a">>}, Result)
	%end
	%, ?_assertMatch({ok, <<"abc">>, <<"defg">>}, parse:string(<<"abcdefg">>, parse:chomp_until(<<"de">>)))
	].

acc_test_d() ->
	SimpleAccumulator =
		fun
			({ok, _, _} = R, _, {[], TailResults}) ->
				Results = [R | TailResults],
				TotalC = lists:foldl(fun({ok, N, _}, A) -> A + N end, 0, Results),
				AllV = [ RV || {ok, _, RV} <- Results],
				{ok, TotalC, lists:reverse(AllV)};
			({error, Wut}, _, _) ->
				{error, Wut};
			({ok, C, _} = R, _, {[Next | Tail], Results}) ->
				{next, C, Next, {Tail, [R | Results]}}
		end,
	Chomp = fun(_Location, <<C/utf8, _/binary>>) ->
		{ok, 1, C};
		(_Location, _NoMatch) ->
			{error, nomatch}
	end,
	Parser = fun(_Location, _Subject) ->
		{push, 0, Chomp, {set_tag, acc_test}, SimpleAccumulator, {[Chomp, Chomp], []}}
	end,
	[ ?_assertMatch({ok, "abc", <<"">>}, parse:string(<<"abc">>, Parser))
	, ?_assertMatch({ok, "bcd", <<"">>}, parse:string(<<"bcd">>, Parser))
	, ?_assertMatch({error, #{ reason := nomatch }}, parse:string(<<"ab">>, Parser))
	].

and_then_test_() ->
	FirstParse = parse:chomp_if($a),
	SecondParse = parse:chomp_if($b),
	Nexter = fun($a) ->
		SecondParse
	end,
	Parse = parse:and_then(Nexter, FirstParse),
	[ ?_assertMatch({ok, $b}, parse:string(<<"abc">>, Parse))
	, ?_assertMatch({error, #{ reason := {expected_character, $b}}}, parse:string(<<"acb">>, Parse))
	, ?_assertMatch({error, #{ reason := {expected_character, $a}}}, parse:string(<<"cba">>, Parse))
	].

first_of_test_() ->
	AParse = parse:chomp_if($a),
	BParse = parse:chomp_if($b),
	CParse = parse:chomp_if($c),
	ABCParse = parse:first_of([AParse, BParse, CParse]),
	CBAParse = parse:first_of([CParse, BParse, AParse]),
	BACParse = parse:first_of([BParse, AParse, CParse]),
	[ ?_assertMatch({ok, $a}, parse:string(<<"abc">>, ABCParse))
	, ?_assertMatch({ok, $a}, parse:string(<<"abc">>, CBAParse))
	, ?_assertMatch({ok, $a}, parse:string(<<"abc">>, BACParse))
	, ?_assertMatch({ok, $c}, parse:string(<<"cba">>, ABCParse))
	, ?_assertMatch({ok, $c}, parse:string(<<"cba">>, CBAParse))
	, ?_assertMatch({ok, $c}, parse:string(<<"cba">>, BACParse))
	, ?_assertMatch({error, #{ reason := no_parsers_matched }}, parse:string(<<"xyz">>, ABCParse))
	].

first_of_complex_test_() ->
	[ fun() ->
		AParse = parse:backtrackable(parse:fail(first_fail)),
		BParse = parse:success(good),
		?assertEqual({ok, good}, parse:string(<<"any">>, parse:first_of([AParse, BParse])))
	end
	, fun() ->
		% the problem here is the backtrackable pushes a backtrack. That
		% backtrack then gets consumed by the 'first_inner_fail' rather than
		% on the first_of(Inners). This means there's no context for the
		% outer first_of to backtrack to.
		%
		% likely need to do away w/ having the backtrack context in the
		% context itself, and have the 'backtrackable' hover the context...
		% somehow.
		Inner1 = parse:fail(first_inner_fail),
		Inner2 = parse:fail(second_inner_fail),
		Outer1 = parse:backtrackable(parse:first_of([Inner1, Inner2])),
		Outer2 = parse:first_of([Outer1, parse:success(good)]),
		?assertEqual({ok, good}, parse:string(<<"any">>, Outer2))
	end
	].

series_test_() ->
	AParse = parse:chomp_if($a),
	BParse = parse:chomp_if($b),
	CParse = parse:chomp_if($c),
	Parse = parse:map_n(fun(AParseR, BParseR, CParseR) ->
		[AParseR, BParseR, CParseR]
	end, [AParse, BParse, CParse]),
	ParseMapped = parse:map(fun([A, B, C]) -> [{A}, {B}, {C}] end, Parse),
	[ ?_assertMatch({ok, "abc"}, parse:string(<<"abc">>, Parse))
	, ?_assertMatch({error, #{ reason := {expected_character, $a}}}, parse:string(<<"cba">>, Parse))
	, ?_assertMatch({ok, [{$a}, {$b}, {$c}]}, parse:string(<<"abc">>, ParseMapped))
	, ?_assertMatch({error, #{ reason := {expected_character, $a}}}, parse:string(<<"cba">>, ParseMapped))
	].

branch_test_() ->
	[ fun() ->
		FirstTest = parse:fail(expect_fail),
		FirstParse = fun(_) -> parse:fail(never_reached) end,
		Branching = parse_do:branch([{FirstTest, FirstParse}]),
		?assertMatch({error, #{ reason := {no_branches_matched, [expect_fail]}}}, parse:string(<<"string">>, Branching))
	end
	, fun() ->
		FirstTest = parse:success(first_ok),
		FirstParse = fun(first_ok) -> parse:success(ok) end,
		SecondTest = parse:fail(never_reached_1),
		SecondParse = fun(never_reached_1) -> parse:fail(never_reached_2) end,
		Branching = parse_do:branch([{FirstTest, FirstParse}, {SecondTest, SecondParse}]),
		Got = parse:string(<<"string">>, Branching),
		?assertMatch({ok, ok}, Got)
	end
	, fun() ->
		FirstTest = parse:fail(first_fail),
		FirstParse = fun(_) -> parse:fail(never_reached) end,
		SecondTest = parse:success(first_ok),
		SecondParse = fun(first_ok) -> parse:fail(expected_fail) end,
		Branching = parse_do:branch([{FirstTest, FirstParse}, {SecondTest, SecondParse}]),
		Got = parse:string(<<"string">>, Branching),
		?assertMatch({error, #{ reason := expected_fail}}, Got)
	end
	].


repeat_test_d() ->
	[ ?_assertMatch({ok, "aaa", <<>>}, parse:string(<<"aaa">>, parse:repeat_for(3, parse:chomp_if($a))))
	, ?_assertMatch({ok, "aa", <<"aa">>}, parse:string(<<"aaaa">>, parse:repeat_for(2, parse:chomp_if($a))))
	, ?_assertMatch({ok, "aaa", <<>>}, parse:string(<<"aaa">>, parse:repeat_at_most(3, parse:chomp_if($a))))
	, ?_assertMatch({error, #{ reason := {expected, <<"a">>}}}, parse:string(<<"abbb">>, parse:repeat_for(2, parse:chomp_if($a))))
	, ?_assertMatch({error, #{ reason := {too_many, 2}}}, parse:string(<<"aaaa">>, parse:repeat_at_most(2, parse:chomp_if($a))))
	, ?_assertMatch({ok, "aaa", <<"bcd">>}, parse:string(<<"aaabcd">>, parse:repeat(2,5,parse:chomp_if($a))))
	, ?_assertMatch({error, #{ reason := {expected, <<"a">>}}}, parse:string(<<"abcd">>, parse:repeat(2, 5, parse:chomp_if($a))))
	, ?_assertMatch({error, #{ reason := {too_many, 5}}}, parse:string(<<"aaaaaabcd">>, parse:repeat(2, 5, parse:chomp_if($a))))
	].

repeat_until_test_() ->
	[ ?_assertMatch({ok, #{ list := "aaa", ended := $b}}, parse:string(<<"aaab">>, parse_do:repeat_until(parse:chomp_if($b), parse:chomp_if($a))))
	, ?_assertMatch({ok, #{ list := "aaa", ended := $c }}, parse:string(<<"aaac">>, parse_do:repeat_until(parse:first_of([parse:chomp_if($b), parse:chomp_if($c)]), parse:chomp_if($a))))
	, ?_assertMatch({error, _}, parse:string(<<"aaad">>, parse_do:repeat_until(parse:first_of([parse:backtrackable(parse:chomp_if($b)), parse:chomp_if($c)]), parse:chomp_if($a))))
	].

repeat_when_test_d() ->
	[ ?_assertMatch({ok, "a.a.a", <<"b">>}, parse:string(<<"a.a.ab">>, parse:repeat_when(parse:chomp_if($a), parse:chomp_if($.))))
	, ?_assertMatch({ok, [[<<"1">>], [<<"+">>], [<<"2">>], [<<"-">>], [<<"3">>]], <<"=0">>}, parse:string(<<"1+2-3=0">>, parse:repeat_when(parse:regex("\\d+"), parse:regex("[\\+\\-]"))))
	, ?_assertMatch({error, _}, parse:string(<<"1+b-3=0">>, parse:repeat_when(parse:regex("\\d+"), parse:regex("[\\+\\-]"))))
	].
