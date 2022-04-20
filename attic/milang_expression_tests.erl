-module(milang_expression_tests).

-include_lib("eunit/include/eunit.hrl").

expression_test_() ->
	[ ?_assertMatch({literal_list, _, []}, milang_expression:parse(<<"[    ]">>))
%	, ?_assertMatch({literal_map, _, []}, milang_expression:parse(<<"#{}#">>))
%	, ?_assertMatch({literal_float, _, 5.7}, milang_expression:parse(<<"5.7">>))
%	, ?_assertMatch({literal_integer, _, 83}, milang_expression:parse(<<"83">>))
%	, ?_assertMatch({literal_string, _, <<"hello!">>}, milang_expression:parse(<<"\"hello!\"">>))
%	, ?_assertMatch({literal_list, _, [{literal_integer, _, 5}]}, milang_expression:parse(<<"[, 5]">>))
%	, ?_assertMatch({literal_list, _, [_, _]}, milang_expression:parse(<<"[, 5, 6 ]">>))
%	, ?_assertMatch({literal_map, _, [{map_entry, _, {literal_integer, _, 3}, {literal_float, _, 345.899}}]}, milang_expression:parse(<<"#{, 3 = 345.899 }#">>))
%	, ?_assertMatch({literal_map, _, [{map_entry, _, {literal_integer, _, 3}, {literal_float, _, 345.899}}]}, milang_expression:parse(<<"#{, 3 := 345.899}#">>))
%	, ?_assertMatch({literal_record, _, [{record_field, _, <<"f1">>, {literal_integer, _, 5}}]}, milang_expression:parse(<<"{, f1 := 5}">>))
%	, ?_assertMatch({literal_record, _, [_, _]}, milang_expression:parse(<<"{, f1 := a b (c d),flart:=\"hi\" }">>))
%	, ?_assertMatch({call, _, _, _}, milang_expression:parse(<<"a 7">>))
%	, ?_assertMatch({call, _, _, _}, milang_expression:parse(<<"a [] b Q">>))
%	, ?_assertMatch({call, _, {local_type_name, _, <<"Goober">>}, []}, milang_expression:parse(<<"Goober">>))
%	, ?_assertMatch({call, _, _, [_]}, milang_expression:parse(<<"Goober.Peas3 a">>))
%	, ?_assertMatch({call, _, _, [_, _]}, milang_expression:parse(<<"Goober 5.3 []">>))
%	, ?_assertMatch({call, _, _, [_]}, milang_expression:parse(<<"a ( b c d )">>))
%	, ?_assertMatch({call, _, _, [_, _]}, milang_expression:parse(<<"a b ( c 97.8 \"hi\" )">>))
%	, ?_assertMatch({call, _, _, []}, milang_expression:parse(<<"a">>))
%	, ?_assertMatch({call, _, _, [_, _]}, milang_expression:parse(<<"add a b">>))
%	, ?_assertMatch({literal_list, _, [{literal_list, _, [{literal_list, _, []}]}]}, milang_expression:parse(<<"[,[,[]]]">>))
%	, ?_assertMatch({expression, _, _, [{{function_symbol, _, <<"+">>}, _}]}, milang_expression:parse(<<"1 + 1">>))
%	, ?_assertMatch({expression, _, {literal_integer, _, 1}, [{{right_assoc, _, 2, {local_function_name, _, <<"add">>}}, {literal_integer, _, 1}}]}, milang_expression:parse(<<"1 <||add 1">>))
%	, ?_assertMatch({expression, _, _, [{{left_assoc, _, 4, _}, _}]}, milang_expression:parse(<<"Garbo ||||>Grammer.and \"malloy\"">>))
%	, ?_assertMatch({call, _, _, [{expression, _, {call, _, _, [_, _]}}]}, milang_expression:parse(<<"not (equal x y)">>))
%	, fun() ->
%		InputString = <<"[
%			, a
%			, [
%			  , 1
%			  , 2
%			  , 3
%			  ]
%			, {
%			  , f1 := 7
%			  , f2 := \"hi\"
%			  }
%			]">>,
%		?assertMatch({literal_list, _, [_, _, _]}, milang_expression:parse(InputString)),
%		Parsed = milang_expression:parse(InputString),
%		{literal_list, _, [Elem1, Elem2, Elem3]} = Parsed,
%		?assertMatch({call, _, _, _}, Elem1),
%		?assertMatch({literal_list, _, [_, _, _]}, Elem2),
%		?assertMatch({literal_record, _, [_, _]}, Elem3)
%	end
	].
