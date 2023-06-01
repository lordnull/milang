-module(milang_p_token_tests).

-include_lib("eunit/include/eunit.hrl").

identifier_test_() ->
	[ {<<"goober">>, fun() ->
		{ok, {identifier_bound, _, <<"goober">>}} = parse:string(<<"goober">>, milang_p_token:identifier_bindable())
	end}
	, {<<"|">>, fun() ->
		{ok, {identifier_bound, _, <<"|">>}} = parse:string(<<"|">>, milang_p_token:identifier_bound())
	end}
	, {"then-dot.", fun() ->
		{ok, {identifier_bound, _, <<"then-dot">>}} = parse:string(<<"then-dot.">>, milang_p_token:identifier_bindable())
	end}
	, {<<"snake_case">>, fun() ->
		{ok, {identifier_bound, _, <<"snake_case">>}} = parse:string(<<"snake_case">>, milang_p_token:identifier_bound())
	end}
	, {<<"=">>, fun() ->
		{error, #{ reason := solo_equals_invalid}} = parse:string(<<"=">>, milang_p_token:identifier_bound())
	end}
	, {<<"==">>, fun() ->
		{ok, {identifier_bound, _, <<"==">>}} = parse:string(<<"==">>, milang_p_token:identifier_bound())
	end}
	, {<<"System.Print.ln">>, fun() ->
		{ok, {identifier_bound, _, #{ module := <<"System.Print">>, local := <<"ln">>}}} = parse:string(<<"System.Print.ln">>, milang_p_token:identifier_bound())
	end}
	, {<<"3mod.yup">>, fun() ->
		{ok, {identifier_bound, _, #{ module := <<"3mod">>, local := <<"yup">>}}} = parse:string(<<"3mod.yup">>, milang_p_token:identifier_bound())
	end}
	, {<<"module.3nope">>, fun() ->
		{ok, {identifier_bound, _, #{ module := <<"module">>, local := <<"3nope">>}}} = parse:string(<<"module.3nope">>, milang_p_token:identifier_bound())
	end}
	, {<<"a.|.4very-weird.>.identifier.==">>, fun() ->

		{ok, {identifier_bound, _, #{ module := <<"a.|.4very-weird.>.identifier">>, local := <<"==">>}}} = parse:string(<<"a.|.4very-weird.>.identifier.==">>, milang_p_token:identifier_bound())
	end}
	, {<<"_ignored">>, fun() ->
		{ok, {identifier_ignored, _, <<"ignored">>}} = parse:string(<<"_ignored">>, milang_p_token:identifier_ignored())
	end}
	, {<<"__also-ignored|">>, fun() ->
		{ok, {identifier_ignored, _, <<"_also-ignored|">>}} = parse:string(<<"__also-ignored|">>, milang_p_token:identifier_ignored())
	end}
	, {<<"invalid.single.equal.=">>, fun() ->
		{error, #{ reason := solo_equals_invalid }} = parse:string(<<"invalid.single.equal.=">>, milang_p_token:identifier_bound())
	end}
	, {<<"+5.ok">>, fun() ->
		{error, #{ reason := identifier_looks_like_an_integer}} = parse:string("+5.ok", milang_p_token:identifier_bound())
	end}
	, {<<"-5.ok">>, fun() ->
		{error, #{ reason := identifier_looks_like_an_integer}} = parse:string("-5.ok", milang_p_token:identifier_bound())
	end}
	, {<<"_.ok">>, fun() ->
		{error, #{ reason := invalid_identifier}} = parse:string(<<"_.ok">>, milang_p_token:identifier_bound())
	end}
	, {<<"5.ok">>, fun() ->
		{error, #{ reason := identifier_looks_like_an_integer}} = parse:string(<<"5.ok">>, milang_p_token:identifier_bound())
	end}
	].
