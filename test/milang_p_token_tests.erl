-module(milang_p_token_tests).

-include_lib("eunit/include/eunit.hrl").

comment_test_() ->
	[ {<<"not a comment">>, fun() ->
		?assertMatch({error, #{reason := no_opening}}, parse:string(<<"not a comment">>, milang_p_token:comment()))
	end}
	, {<<"{-no closing">>, fun() ->
		?assertMatch({error, #{ reason := unclosed_comment }}, parse:string(<<"{-no closing">>, milang_p_token:comment()))
	end}
	, {<<"{-a valid comment\nwith newline-}">>, fun() ->
		?assertMatch({ok, <<"a valid comment\nwith newline">>}, parse:string(<<"{-a valid comment\nwith newline-}">>, milang_p_token:comment()))
	end}
	].

keyword_test_() ->
	[ ?_assertMatch({ok, module}, parse:string(<<"module">>, milang_p_token:keyword_module()))
	, ?_assertMatch({ok, module}, parse:string(<<"module   ">>, milang_p_token:keyword_module()))
	, ?_assertEqual({ok, <<"module">>}, parse:string(<<"module   ">>, parse:get_chomped_string(milang_p_token:keyword_module())))
	].

literals_test_() ->
	[ ?_assertMatch({ok, 5}, parse:string(<<"5">>, milang_p_token:literal_integer()))
	, ?_assertMatch({ok, 35}, parse:string(<<"+35">>, milang_p_token:literal_integer()))
	, ?_assertMatch({ok, -345}, parse:string(<<"-345">>, milang_p_token:literal_integer()))
	, ?_assertMatch({ok, 78}, parse:string(<<"78  hi">>, milang_p_token:literal_integer()))
	, ?_assertMatch({ok, 5.3}, parse:string(<<"5.3">>, milang_p_token:literal_float()))
	, ?_assertMatch({ok, -12.003}, parse:string(<<"-12.003">>, milang_p_token:literal_float()))
	, ?_assertMatch({ok, <<"hi">>}, parse:string(<<"\"hi\"">>, milang_p_token:literal_string()))
	, ?_assertMatch({ok, <<"also \"hi\".">>}, parse:string(<<"\"also \\\"hi\\\".\"">>, milang_p_token:literal_string()))
	, ?_assertMatch({ok, <<"and yeah.">>}, parse:string(<<"\"\\and yeah.\"">>, milang_p_token:literal_string()))
	, ?_assertMatch({ok, <<"escaped \\">>}, parse:string(<<"\"escaped \\\\\"">>, milang_p_token:literal_string()))
	].

identifier_test_() ->
	[ {<<"good_local_ident">>, fun() ->
		{ok, <<"good_local_ident">>} = parse:string(<<"good_local_ident">>, milang_p_token:identifier_local())
	end}
	, {<<"goober">>, fun() ->
		{ok, {identifier_bound, <<"goober">>}} = parse:string(<<"goober">>, milang_p_token:identifier_bindable())
	end}
	, {<<"Core.-">>, fun() ->
		{ok, {identifier_bound, #{ local := <<"-">>, module := <<"Core">>}}} = parse:string(<<"Core.-">>, milang_p_token:identifier_bound())
	end}
	, {<<"|">>, fun() ->
		{ok, {identifier_bound, <<"|">>}} = parse:string(<<"|">>, milang_p_token:identifier_bound())
	end}
	, {"then-dot.", fun() ->
		{ok, {identifier_bound, <<"then-dot">>}} = parse:string(<<"then-dot.">>, milang_p_token:identifier_bindable())
	end}
	, {<<"snake_case">>, fun() ->
		{ok, {identifier_bound, <<"snake_case">>}} = parse:string(<<"snake_case">>, milang_p_token:identifier_bound())
	end}
	, {<<"=">>, fun() ->
		{error, #{ reason := { identifier_is_reserved_symbol, <<"=">>}}} = parse:string(<<"=">>, milang_p_token:identifier_bound())
	end}
	, {<<"==">>, fun() ->
		{ok, {identifier_bound, <<"==">>}} = parse:string(<<"==">>, milang_p_token:identifier_bound())
	end}
	, {<<"System.Print.ln">>, fun() ->
		{ok, {identifier_bound, #{ module := <<"System.Print">>, local := <<"ln">>}}} = parse:string(<<"System.Print.ln">>, milang_p_token:identifier_bound())
	end}
	, {<<"module.name.in.declaration.">>, fun() ->
		{ok, {identifier_bound, #{ module := <<"module.name.in">>, local := <<"declaration">>}}} = parse:string(<<"module.name.in.declaration.">>, milang_p_token:identifier_bound())
	end}
	, {<<"last.dot.not.chomped.">>, fun() ->
		Input = <<"last.dot.not.chomped.">>,
		ChompNext = parse:and_then(fun(_) ->
			DotChomp = parse:chomp_if(compose:always(true), no_dot),
			parse:get_chomped_string(DotChomp)
		end, milang_p_token:identifier_bound()),
		Got = parse:string(Input, ChompNext),
		?assertEqual({ok, <<".">>}, Got)
	end}
	, {<<"just.stop.at.last.dot.\n">>, fun() ->
		Input = <<"just.stop.at.last.dot.\n">>,
		FirstGot = parse:string(Input, milang_p_token:identifier_bound()),
		?assertMatch({ok, {identifier_bound, #{ module := <<"just.stop.at.last">>, local := <<"dot">>}}}, FirstGot),
		ChompChomp = parse:get_chomped_string(parse:and_then(fun(_) ->
			parse:chomp_if(compose:always(true), never)
		end, parse:chomp_if(compose:always(true), never))),
		CheckPostId = parse:and_then(fun(_) ->
			ChompChomp
		end, milang_p_token:identifier_bound()),
		?assertEqual({ok, <<".\n">>}, parse:string(Input, CheckPostId))
	end}
	, {<<"3mod.yup">>, fun() ->
		{ok, {identifier_bound, #{ module := <<"3mod">>, local := <<"yup">>}}} = parse:string(<<"3mod.yup">>, milang_p_token:identifier_bound())
	end}
	, {<<"module.3nope">>, fun() ->
		{ok, {identifier_bound, #{ module := <<"module">>, local := <<"3nope">>}}} = parse:string(<<"module.3nope">>, milang_p_token:identifier_bound())
	end}
	, {<<"a.|.4very-weird.>.identifier.==">>, fun() ->

		{ok, {identifier_bound, #{ module := <<"a.|.4very-weird.>.identifier">>, local := <<"==">>}}} = parse:string(<<"a.|.4very-weird.>.identifier.==">>, milang_p_token:identifier_bound())
	end}
	, {<<"_ignored">>, fun() ->
		{ok, {identifier_ignored, <<"_ignored">>}} = parse:string(<<"_ignored">>, milang_p_token:identifier_ignored())
	end}
	, {<<"__also-ignored|">>, fun() ->
		{ok, {identifier_ignored, <<"__also-ignored|">>}} = parse:string(<<"__also-ignored|">>, milang_p_token:identifier_ignored())
	end}
	, {<<"_">>, fun() ->
		{ok, {identifier_ignored, <<"_">>}} = parse:string(<<"_">>, milang_p_token:identifier_bindable())
	end}
	, {<<"_">>, fun() ->
		{ok, {identifier_ignored, <<"_">>}} = parse:string(<<"_">>, milang_p_token:identifier_ignored())
	end}
	, {<<"invalid.single.equal.=">>, fun() ->
		{error, #{ reason := {identifier_is_reserved_symbol, <<"=">>}}} = parse:string(<<"invalid.single.equal.=">>, milang_p_token:identifier_bound())
	end}
	, {<<"+5.ok">>, fun() ->
		{ok, {identifier_bound, #{ local := <<"ok">>, module := <<"+5">>}}} = parse:string("+5.ok", milang_p_token:identifier_bound())
	end}
	, {<<"-5.ok">>, fun() ->
		{ok, {identifier_bound, #{ module := <<"-5">>, local := <<"ok">>}}} = parse:string("-5.ok", milang_p_token:identifier_bound())
	end}
	, {<<"_.ok">>, fun() ->
		{ok, {identifier_bound, #{ module := <<"_">>, local := <<"ok">>}}} = parse:string(<<"_.ok">>, milang_p_token:identifier_bound())
	end}
	, {<<"5.ok">>, fun() ->
		{ok, {identifier_bound, #{ local := <<"ok">>, module := <<"5">>}}} = parse:string(<<"5.ok">>, milang_p_token:identifier_bound()) end} ].