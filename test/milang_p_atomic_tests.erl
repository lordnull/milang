-module(milang_p_atomic_tests).

-include_lib("eunit/include/eunit.hrl").

atomics_test_() ->
	[ ?_assertMatch(<<>>, string:trim(iolist_to_binary(element(2, milang_p_atomic:parse(<<" \n \r\n  \r\n">>)))))
	, ?_assertMatch({ok, [{docstring, _, <<"yup">>} | _], <<>>}, milang_p_atomic:parse(<<"-doc yup">>))
	, ?_assertMatch({ok, [{docstring, _, <<"yup yup">>} | _], <<>>}, milang_p_atomic:parse(<<"-doc yup yup\n">>))
	, ?_assertMatch({ok, {type_name_local, _, <<"Goober">>}, <<>>}, milang_p_atomic:parse(<<"Goober">>))
	, ?_assertMatch({ok, {type_name_remote, _, {module_name, _, <<"Eating.Goober">>}, <<"Peas3">>}, <<>>}, milang_p_atomic:parse(<<"Eating.Goober.Peas3">>))
	, ?_assertMatch({ok, {function_name_local, _, <<"okay_okay">>}, <<>>}, milang_p_atomic:parse(<<"okay_okay">>))
	, ?_assertMatch({ok, {function_name_remote, _, {module_name, _, <<"Eating.Goober.Peas3">>}, <<"flower">>}, <<>>}, milang_p_atomic:parse(<<"Eating.Goober.Peas3.flower">>))
	, ?_assertMatch({ok, {infix_symbol, _, <<"+">>}, <<>>}, milang_p_atomic:parse(<<"+">>))
	, ?_assertMatch({ok, {left_assoc, _, 1, _}, <<>>}, milang_p_atomic:parse(<<"|>left_first">>))
	, ?_assertMatch({ok, {right_assoc, _, 3, _}, <<>>}, milang_p_atomic:parse(<<"<|||+">>))
	, ?_assertMatch({ok, {function_name_symbol, _, <<"+">>}, <<>>}, milang_p_atomic:parse(<<"'+'">>))
	, ?_assertMatch({ok, {infix_symbol, _, <<"+*=">>}, <<>>}, milang_p_atomic:parse(<<"+*=">>))
	, ?_assertMatch({ok, {function_name_symbol, _, <<"##&">>}, <<>>}, milang_p_atomic:parse(<<"'##&'">>))
	, ?_assertMatch({ok, {function_name_symbol, _, <<"==">>}, <<>>}, milang_p_atomic:parse(<<"'=='">>))
	, ?_assertMatch({ok, {infix_symbol, _, <<"!=">>}, <<>>}, milang_p_atomic:parse(<<"!=">>))
	, ?_assertMatch({error, #{ reason := solo_equals_not_allowed}}, parse:it(<<"=">>, milang_p_atomic:infix()))
	, ?_assertMatch({ok, {type_name_remote, _, {module_name, _, <<"A.B">>}, <<"C">>}, <<".">>}, milang_p_atomic:parse(<<"A.B.C.">>))
	].
