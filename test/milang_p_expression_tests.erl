-module(milang_p_expression_tests).

-include_lib("eunit/include/eunit.hrl").

expression_test_d() ->
	[ ?_assertMatch({ok, {literal_list, _, []}, <<>>}, milang_p_expression:parse(<<"[    ]">>))
	, ?_assertMatch({ok, {literal_map, _, []}, <<>>}, milang_p_expression:parse(<<"#{}#">>))
	, ?_assertMatch({ok, {literal_float, _, 5.7}, <<>>}, milang_p_expression:parse(<<"5.7">>))
	, ?_assertMatch({ok, {literal_integer, _, 83}, <<>>}, milang_p_expression:parse(<<"83">>))
	, ?_assertMatch({ok, {literal_string, _, <<"hello!">>}, <<>>}, milang_p_expression:parse(<<"\"hello!\"">>))
	, ?_assertMatch({ok, {literal_list, _, [{literal_integer, _, 5}]}, <<>>}, milang_p_expression:parse(<<"[, 5]">>))
	, fun() ->
		Parsed = milang_p_expression:parse(<<"[, 5, 6 ]">>),
		?assertMatch({ok, {literal_list, _, _}, <<>>}, Parsed),
		{ok, {literal_list, _, Elements}, <<>>} = Parsed,
		?assertMatch([{literal_integer, _, 5}, {literal_integer, _, 6}], Elements)
	end
	, ?_assertMatch({ok, {literal_map, _, [{map_entry, _, {literal_integer, _, 3}, {literal_float, _, 345.899}}]}, <<>>}, milang_p_expression:parse(<<"#{, 3 = 345.899 }#">>))
	, ?_assertMatch({ok, {literal_map, _, [{map_entry, _, {literal_integer, _, 3}, {literal_float, _, 345.899}}]}, <<>>}, milang_p_expression:parse(<<"#{, 3 = 345.899}#">>))
	, ?_assertMatch({ok, {literal_record, _, [{record_field, _, {string, <<"f1">>}, {literal_integer, _, 5}}]}, <<>>}, milang_p_expression:parse(<<"{, f1 = 5}">>))
	, fun() ->
		Parsed = milang_p_expression:parse(<<"{, f1 = a b (c d),flart=\"hi\" }">>),
		?assertMatch({ok, {literal_record, _, _}, <<>>}, Parsed),
		{ok, {literal_record, _, Fields}, <<>>} = Parsed,
		?assertMatch([{record_field, _, {string, <<"f1">>}, _}, {record_field, _, {string, <<"flart">>}, _}], Fields)
	end
	, ?_assertMatch({ok, {call, _, _, _}, <<>>}, milang_p_expression:parse(<<"a 7">>))
	, ?_assertMatch({ok, {call, _, _, _}, <<>>}, milang_p_expression:parse(<<"a [] b Q">>))
	, ?_assertMatch({ok, {call, _, {type_name_local, _, <<"Goober">>}, []}, <<>>}, milang_p_expression:parse(<<"Goober">>))
	, ?_assertMatch({ok, {call, _, _, [_]}, <<>>}, milang_p_expression:parse(<<"Goober.Peas3 a">>))
	, ?_assertMatch({ok, {call, _, _, [_, _]}, <<>>}, milang_p_expression:parse(<<"Goober 5.3 []">>))
	, ?_assertMatch({ok, {call, _, _, [_]}, <<>>}, milang_p_expression:parse(<<"a ( b c d )">>))
	, ?_assertMatch({ok, {call, _, _, [_, _]}, <<>>}, milang_p_expression:parse(<<"a b ( c 97.8 \"hi\" )">>))
	, ?_assertMatch({ok, {call, _, _, []}, <<>>}, milang_p_expression:parse(<<"a">>))
	, ?_assertMatch({ok, {call, _, _, [_, _]}, <<>>}, milang_p_expression:parse(<<"add a b">>))
	, ?_assertMatch({ok, {literal_list, _, [{literal_list, _, [{literal_list, _, []}]}]}, <<>>}, milang_p_expression:parse(<<"[,[,[]]]">>))
	, ?_assertMatch({ok, {expression, _, _, [{{infix_symbol, _, <<"+">>}, _}]}, <<>>}, milang_p_expression:parse(<<"1 + 1">>))
	, fun() ->
		Parsed = milang_p_expression:parse(<<"1 <||add 1">>),
		?assertMatch({ok, _, <<>>}, Parsed),
		{ok, Expression, <<>>} = Parsed,
		?assertMatch({expression, _, {literal_integer, _, 1}, [_]}, Expression),
		{expression, _, _, [OpEntry]} = Expression,
		?assertMatch({{right_assoc, _, 2, {function_name_local, _, <<"add">>}}, {literal_integer, _, 1}}, OpEntry)
	end
	, ?_assertMatch({ok, {expression, _, _, [{{left_assoc, _, 4, _}, _}]}, <<>>}, milang_p_expression:parse(<<"Garbo ||||>Grammer.and \"malloy\"">>))
	, fun() ->
		Parsed = milang_p_expression:parse(<<"not (equal x y)">>),
		?assertMatch({ok, {call, _, _, [_]}, <<>>}, Parsed),
		{ok, {call, _, _, [Arg]}, <<>>} = Parsed,
		?assertMatch({call, _, {function_name_local, _, <<"equal">>}, [_, _]}, Arg),
		{call, _, _, [Arg1, Arg2]} = Arg,
		?assertMatch({function_name_local, _, <<"x">>}, Arg1),
		?assertMatch({function_name_local, _, <<"y">>}, Arg2)
	end
	, fun() ->
		InputString = <<"[
			, a
			, [
			  , 1
			  , 2
			  , 3
			  ]
			, {
			  , f1 = 7
			  , f2 = \"hi\"
			  }
			]">>,
		?assertMatch({ok, {literal_list, _, [_, _, _]}, <<>>}, milang_p_expression:parse(InputString)),
		Parsed = milang_p_expression:parse(InputString),
		{ok, {literal_list, _, [Elem1, Elem2, Elem3]}, <<>>} = Parsed,
		?assertMatch({call, _, _, _}, Elem1),
		?assertMatch({literal_list, _, [_, _, _]}, Elem2),
		?assertMatch({literal_record, _, [_, _]}, Elem3)
	end
	].
