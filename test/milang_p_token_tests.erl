-module(milang_p_token_tests).

-include_lib("eunit/include/eunit.hrl").

identifier_test_() ->
	[ fun() ->
		{ok, {identifier_bound, _, <<"goober">>}, _} = parse:it(<<"goober">>, milang_p_token:identifier_bindable())
	end
	, fun() ->
		{ok, {identifier_bound, _, <<"|">>}, _} = parse:it(<<"|">>, milang_p_token:identifier_bound())
	end
	, fun() ->
		{ok, {identifier_bound, _, <<"then-dot">>}, <<".">>} = parse:it(<<"then-dot.">>, milang_p_token:identifier_bindable())
	end
	, fun() ->
		{ok, {identifier_bound, _, <<"snake_case">>}, _} = parse:it(<<"snake_case">>, milang_p_token:identifier_bound())
	end
	, fun() ->
		{error, #{ reason := solo_equals_invalid}} = parse:it(<<"=">>, milang_p_token:identifier_bound())
	end
	, fun() ->
		{ok, {identifier_bound, _, #{ module := <<"System.Print">>, local := <<"ln">>}}, _} = parse:it(<<"System.Print.ln">>, milang_p_token:identifier_bindable())
	end
	].
