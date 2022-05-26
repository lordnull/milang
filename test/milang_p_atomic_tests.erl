-module(milang_p_atomic_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("milang/src/milang_ast.hrl").

atomics_test_() ->
	[ ?_assertMatch(<<>>, string:trim(iolist_to_binary(element(2, milang_p_atomic:parse(<<" \n \r\n  \r\n">>)))))
	, ?_assertMatch({ok, <<"yup">>, <<>>}, milang_p_atomic:parse(<<"-doc yup">>))
	, ?_assertMatch({ok, <<"yup yup">>, <<>>}, milang_p_atomic:parse(<<"-doc yup yup\n">>))
	, ?_assertMatch({ok, <<"yup\n\n\nyup\n\n\nyup">>, <<>>}, milang_p_atomic:parse(<<"-doc yup\n-doc yup\n-doc yup">>))
	, ?_assertMatch({ok, #milang_ast{type = type_name_local, data = 'Goober'}, <<>>}, milang_p_atomic:parse(<<"Goober">>))
	, ?_assertMatch({ok, #milang_ast{type = type_name_remote, data = #{module := 'Eating.Goober', name := 'Peas3'}}, <<>>}, milang_p_atomic:parse(<<"Eating.Goober.Peas3">>))
	, ?_assertMatch({ok, #milang_ast{type = function_name_local, data = 'okay_okay'}, <<>>}, milang_p_atomic:parse(<<"okay_okay">>))
	, ?_assertMatch({ok, #milang_ast{type = function_name_remote, data = #{module := 'Eating.Goober.Peas3', name := 'flower'}}, <<>>}, milang_p_atomic:parse(<<"Eating.Goober.Peas3.flower">>))
	, ?_assertMatch({ok, #milang_ast{type = infix_symbol, data = '+'}, <<>>}, milang_p_atomic:parse(<<"+">>))
	, ?_assertMatch({ok, #milang_ast{type = infix_notation, data = #{ assoc := left, weight := 1, function := #milang_ast{ type = function_name_local, data = 'left_first'}}}, <<>>}, milang_p_atomic:parse(<<"»left_first"/utf8>>))
	, ?_assertMatch({ok, #milang_ast{type = infix_notation, data = #{ assoc := right, weight := 3, function := #milang_ast{ type = infix_symbol, data = '+'} }}, <<>>}, milang_p_atomic:parse(<<"«««+"/utf8>>))
	, ?_assertMatch({ok, #milang_ast{type = function_name_symbol, data = '+'}, <<>>}, milang_p_atomic:parse(<<"'+'">>))
	, ?_assertMatch({ok, #milang_ast{type = infix_symbol, data = '+*='}, <<>>}, milang_p_atomic:parse(<<"+*=">>))
	, ?_assertMatch({ok, #milang_ast{type = function_name_symbol, data = '##&'}, <<>>}, milang_p_atomic:parse(<<"'##&'">>))
	, ?_assertMatch({ok, #milang_ast{type = function_name_symbol, data = '=='}, <<>>}, milang_p_atomic:parse(<<"'=='">>))
	, ?_assertMatch({ok, #milang_ast{type = infix_symbol, data = '!='}, <<>>}, milang_p_atomic:parse(<<"!=">>))
	, ?_assertMatch({error, #{ reason := {nomatch, _}}}, parse:it(<<"=">>, milang_p_atomic:infix()))
	, ?_assertMatch({ok, #milang_ast{type = type_name_remote, data = #{module := 'A.B', name := 'C'}}, <<".">>}, milang_p_atomic:parse(<<"A.B.C.">>))
	].
