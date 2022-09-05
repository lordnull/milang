-module(parse_tests).

-include_lib("eunit/include/eunit.hrl").

success_fail_and_character_test_() ->
	[ ?_assertEqual({ok, 5, <<"abc">>}, parse:it(<<"abc">>, parse:success(5)))
	, ?_assertMatch({error, #{ reason := test }}, parse:it(<<"abc">>, parse:fail(test)))
	, ?_assertEqual({ok, $a, <<"bc">>}, parse:it(<<"abc">>, parse:character($a)))
	, ?_assertMatch({error, #{ reason := {expected, <<"c">>}}}, parse:it(<<"abc">>, parse:character($c)))
	].

string_test_() ->
	[ ?_assertMatch({ok, <<"abc">>, <<"">>}, parse:it(<<"abc">>, parse:string(<<"abc">>)))
	, ?_assertMatch({ok, <<"a">>, <<"bc">>}, parse:it(<<"abc">>, parse:string(<<"a">>)))
	, ?_assertMatch({error, _}, parse:it(<<"xyz">>, parse:string(<<"abc">>)))
	, ?_assertMatch({ok, <<"hello world">>, <<>>}, parse:it(<<"hello world">>, parse:string(<<"hello world">>)))
	, fun() ->
		String = unicode:characters_to_binary("😀 👍"),
		?assertMatch({ok, String, <<>>}, parse:it(String, parse:string(String)))
	end
	].

regex_test_() ->
	[ ?_assertMatch({ok, [<<"a">>], <<"bc">>}, parse:it(<<"abc">>, parse:regex("\\w")))
	, ?_assertMatch({ok, [<<"123">>], <<"xyz">>}, parse:it(<<"123xyz">>, parse:regex("\\d+")))
	, ?_assertMatch({error, #{ reason := {nomatch, "\\d"}}}, parse:it(<<"abc">>, parse:regex("\\d")))
	, ?_assertMatch({error, #{ reason := {nomatch, "\\d"}}}, parse:it(<<"abc123">>, parse:regex("\\d")))
	, ?_assertMatch({ok, [<<"hello">>, <<"ello">>, <<"ll">>], <<>>}, parse:it(<<"hello">>, parse:regex("h(e(ll)o)")))
	].

unless_test_() ->
	[ ?_assertMatch({error, #{ reason := test }}, parse:it(<<"abc">>, parse:unless(parse:chomp(), fun(_) -> {error, test} end)))
	, ?_assertMatch({error, #{ reason := {disallowed, $a}}}, parse:it(<<"abc">>, parse:unless(parse:chomp(), fun($a) -> {error, {disallowed, $a}} end)))
	, ?_assertMatch({ok, $a, <<"bc">>}, parse:it(<<"abc">>, parse:unless(parse:chomp(), fun(_) -> ok end)))
	].

chomp_test_() ->
	[ ?_assertMatch({ok, $a, <<"bc">>}, parse:it(<<"abc">>, parse:chomp()))
	, ?_assertMatch({ok, <<"ab">>, <<"c">>}, parse:it(<<"abc">>, parse:chomp(2)))
	, ?_assertMatch({ok, $a, <<"bc">>}, parse:it(<<"abc">>, parse:chomp_if(fun(_) -> true end)))
	, ?_assertMatch({error, #{ reason := {not_chompable, $a}}}, parse:it(<<"abc">>, parse:chomp_if(fun(_) -> false end)))
	, ?_assertMatch({ok, <<"a">>, <<"bc">>}, parse:it(<<"abc">>, parse:chomp_while(fun(E) -> E =:= $a end)))
	, ?_assertMatch({ok, <<"abc">>, <<>>}, parse:it(<<"abc">>, parse:chomp_while(fun(_) -> true end)))
	, ?_assertMatch({ok, <<"abc">>, <<"xyz">>}, parse:it(<<"abcxyz">>, parse:chomp_until_end_or(<<"xyz">>)))
	, ?_assertMatch({ok, <<"abc">>, <<>>}, parse:it(<<"abc">>, parse:chomp_until_end_or(<<"xyz">>)))
	, ?_assertMatch({ok, <<>>, <<"abc">>}, parse:it(<<"abc">>, parse:chomp_while(fun(E) -> E =:= $z end)))
	, fun() ->
		HappyFace = "😀",
		Subject = unicode:characters_to_binary([HappyFace, $a]),
		ExpectedChomp = unicode:characters_to_binary(HappyFace),
		Parser = parse:chomp_while(fun(E) -> E =/= $a end),
		Result = parse:it(Subject, Parser),
		?assertMatch({ok, ExpectedChomp, <<"a">>}, Result)
	end
	, ?_assertMatch({ok, <<"abc">>, <<"defg">>}, parse:it(<<"abcdefg">>, parse:chomp_until(<<"de">>)))
	].

peek_test_() ->
	[ ?_assertMatch({ok, [<<"a">>], <<"abc">>}, parse:it(<<"abc">>, parse:peek("[a-z]")))
	, ?_assertMatch({error, #{ reason := {nomatch, "[0-9]" }}}, parse:it(<<"abc">>, parse:peek("[0-9]")))
	].

acc_test_() ->
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
	[ ?_assertMatch({ok, "abc", <<"">>}, parse:it(<<"abc">>, Parser))
	, ?_assertMatch({ok, "bcd", <<"">>}, parse:it(<<"bcd">>, Parser))
	, ?_assertMatch({error, #{ reason := nomatch }}, parse:it(<<"ab">>, Parser))
	].

and_then_test_() ->
	FirstParse = parse:character($a),
	SecondParse = parse:character($b),
	Nexter = fun($a) ->
		SecondParse
	end,
	Parse = parse:andThen(FirstParse, Nexter),
	[ ?_assertMatch({ok, $b, <<"c">>}, parse:it(<<"abc">>, Parse))
	, ?_assertMatch({error, #{ reason := {expected, <<"b">>}}}, parse:it(<<"acb">>, Parse))
	, ?_assertMatch({error, #{ reason := {expected, <<"a">>}}}, parse:it(<<"cba">>, Parse))
	].

first_of_test_() ->
	AParse = parse:character($a),
	BParse = parse:character($b),
	CParse = parse:character($c),
	ABCParse = parse:first_of([AParse, BParse, CParse]),
	CBAParse = parse:first_of([CParse, BParse, AParse]),
	BACParse = parse:first_of([BParse, AParse, CParse]),
	[ ?_assertMatch({ok, $a, <<"bc">>}, parse:it(<<"abc">>, ABCParse))
	, ?_assertMatch({ok, $a, <<"bc">>}, parse:it(<<"abc">>, CBAParse))
	, ?_assertMatch({ok, $a, <<"bc">>}, parse:it(<<"abc">>, BACParse))
	, ?_assertMatch({ok, $c, <<"ba">>}, parse:it(<<"cba">>, ABCParse))
	, ?_assertMatch({ok, $c, <<"ba">>}, parse:it(<<"cba">>, CBAParse))
	, ?_assertMatch({ok, $c, <<"ba">>}, parse:it(<<"cba">>, BACParse))
	, ?_assertMatch({error, #{ reason := [_, _, _] }}, parse:it(<<"xyz">>, ABCParse))
	].

series_test_() ->
	AParse = parse:character($a),
	BParse = parse:character($b),
	CParse = parse:character($c),
	Parse = parse:series([AParse, BParse, CParse]),
	ParseMapped = parse:map(Parse, fun([A, B, C]) -> [{A}, {B}, {C}] end),
	[ ?_assertMatch({ok, "abc", <<>>}, parse:it(<<"abc">>, Parse))
	, ?_assertMatch({error, #{ reason := {step, 1, {expected, <<"a">>}}}}, parse:it(<<"cba">>, Parse))
	, ?_assertMatch({ok, [{$a}, {$b}, {$c}], <<>>}, parse:it(<<"abc">>, ParseMapped))
	, ?_assertMatch({error, #{ reason := {step, 1, {expected, <<"a">>}}}}, parse:it(<<"cba">>, ParseMapped))
	].


repeat_test_() ->
	[ ?_assertMatch({ok, "aaa", <<>>}, parse:it(<<"aaa">>, parse:repeat_for(3, parse:character($a))))
	, ?_assertMatch({ok, "aa", <<"aa">>}, parse:it(<<"aaaa">>, parse:repeat_for(2, parse:character($a))))
	, ?_assertMatch({ok, "aaa", <<>>}, parse:it(<<"aaa">>, parse:repeat_at_most(3, parse:character($a))))
	, ?_assertMatch({error, #{ reason := {expected, <<"a">>}}}, parse:it(<<"abbb">>, parse:repeat_for(2, parse:character($a))))
	, ?_assertMatch({error, #{ reason := {too_many, 2}}}, parse:it(<<"aaaa">>, parse:repeat_at_most(2, parse:character($a))))
	, ?_assertMatch({ok, "aaa", <<"bcd">>}, parse:it(<<"aaabcd">>, parse:repeat(2,5,parse:character($a))))
	, ?_assertMatch({error, #{ reason := {expected, <<"a">>}}}, parse:it(<<"abcd">>, parse:repeat(2, 5, parse:character($a))))
	, ?_assertMatch({error, #{ reason := {too_many, 5}}}, parse:it(<<"aaaaaabcd">>, parse:repeat(2, 5, parse:character($a))))
	].

repeat_until_test_() ->
	[ ?_assertMatch({ok, {"aaa", $b}, <<>>}, parse:it(<<"aaab">>, parse:repeat_until(parse:character($a), parse:character($b))))
	, ?_assertMatch({ok, {"aaa", $c}, <<>>}, parse:it(<<"aaac">>, parse:repeat_until(parse:character($a), parse:first_of([parse:character($b), parse:character($c)]))))
	, ?_assertMatch({error, _}, parse:it(<<"aaad">>, parse:repeat_until(parse:character($a), parse:first_of([parse:character($b), parse:character($c)]))))
	].

repeat_when_test_() ->
	[ ?_assertMatch({ok, "a.a.a", <<"b">>}, parse:it(<<"a.a.ab">>, parse:repeat_when(parse:character($a), parse:character($.))))
	, ?_assertMatch({ok, [[<<"1">>], [<<"+">>], [<<"2">>], [<<"-">>], [<<"3">>]], <<"=0">>}, parse:it(<<"1+2-3=0">>, parse:repeat_when(parse:regex("\\d+"), parse:regex("[\\+\\-]"))))
	, ?_assertMatch({error, _}, parse:it(<<"1+b-3=0">>, parse:repeat_when(parse:regex("\\d+"), parse:regex("[\\+\\-]"))))
	].
