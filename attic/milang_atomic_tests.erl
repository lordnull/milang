-module(milang_atomic_tests).

-include_lib("eunit/include/eunit.hrl").

atomics_test_() ->
	[ ?_assertMatch(<<>>, string:trim(iolist_to_binary(milang_atomic:parse(<<" \n \r\n  \r\n">>))))
	, ?_assertMatch([{docstring, _, <<"yup">>} | _], milang_atomic:parse(<<"-doc yup">>))
	, ?_assertMatch([{docstring, _, <<"yup yup">>} | _], milang_atomic:parse(<<"-doc yup yup\n">>))
	, ?_assertMatch({local_type_name, _, <<"Goober">>}, milang_atomic:parse(<<"Goober">>))
	, ?_assertMatch({remote_type_name, _, <<"Eating.Goober">>, <<"Peas3">>}, milang_atomic:parse(<<"Eating.Goober.Peas3">>))
	, ?_assertMatch({local_function_name, _, <<"okay_okay">>}, milang_atomic:parse(<<"okay_okay">>))
	, ?_assertMatch({remote_function_name, _, {module_name, _, <<"Eating.Goober.Peas3">>}, <<"flower">>}, milang_atomic:parse(<<"Eating.Goober.Peas3.flower">>))
	, ?_assertMatch({function_symbol, _, <<"+">>}, milang_atomic:parse(<<"+">>))
	, ?_assertMatch({left_assoc, _, 1, _}, milang_atomic:parse(<<"|>left_first">>))
	, ?_assertMatch({right_assoc, _, 3, _}, milang_atomic:parse(<<"<|||+">>))
	, ?_assertMatch({function_symbol, _, <<"+">>}, milang_atomic:parse(<<"'+'">>))
	, ?_assertMatch({function_symbol, _, <<"+*=">>}, milang_atomic:parse(<<"+*=">>))
	, ?_assertMatch({function_symbol, _, <<"##&">>}, milang_atomic:parse(<<"'##&'">>))
	, ?_assertMatch({function_symbol, _, <<"==">>}, milang_atomic:parse(<<"'=='">>))
	, ?_assertMatch({function_symbol, _, <<"!=">>}, milang_atomic:parse(<<"!=">>))
	, ?_assertMatch({fail, _}, milang_atomic:parse(<<"=">>))
	].
