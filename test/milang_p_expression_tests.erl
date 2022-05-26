-module(milang_p_expression_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("milang/src/milang_ast.hrl").

expression_test_() ->
	[ ?_assertMatch({ok, #milang_ast{type = literal_list, data = []}, <<>>}, milang_p_expression:parse(<<"[    ]">>))
	, ?_assertMatch({ok, #milang_ast{type = literal_map, data = []}, <<>>}, milang_p_expression:parse(<<"#{}#">>))
	, ?_assertMatch({ok, #milang_ast{type = literal_float, data = 5.7}, <<>>}, milang_p_expression:parse(<<"5.7">>))
	, ?_assertMatch({ok, #milang_ast{type = literal_integer, data = 83}, <<>>}, milang_p_expression:parse(<<"83">>))
	, ?_assertMatch({ok, #milang_ast{type = literal_string, data = <<"hello!">>}, <<>>}, milang_p_expression:parse(<<"\"hello!\"">>))
	, ?_assertMatch({ok, #milang_ast{type = literal_list, data = [#milang_ast{type = literal_integer, data = 5}]}, <<>>}, milang_p_expression:parse(<<"[, 5]">>))
	, fun() ->
		Parsed = milang_p_expression:parse(<<"[, 5, 6 ]">>),
		?assertMatch({ok, #milang_ast{type = literal_list}, <<>>}, Parsed),
		{ok, #milang_ast{data = Elements}, <<>>} = Parsed,
		?assertMatch([#milang_ast{type = literal_integer, data = 5}, #milang_ast{type = literal_integer, data = 6}], Elements)
	end
	, ?_assertMatch({ok, #milang_ast{type = literal_map, data = [#milang_ast{type = literal_map_entry, data = #{ key := #milang_ast{type = literal_integer, data = 3}, value := #milang_ast{type = literal_float, data = 345.899}}}]}, <<>>}, milang_p_expression:parse(<<"#{, 3 = 345.899 }#">>))
	, ?_assertMatch({ok, #milang_ast{type = literal_map, data = [#milang_ast{type = literal_map_entry, data = #{ key := #milang_ast{type = literal_integer, data = 3}, value := #milang_ast{type = literal_float, data = 345.899}}}]}, <<>>}, milang_p_expression:parse(<<"#{, 3 = 345.899}#">>))
	, ?_assertMatch({ok, #milang_ast{type = literal_record, data = [#milang_ast{type = literal_record_field, data = {f1, #milang_ast{type = literal_integer, data = 5}}}]}, <<>>}, milang_p_expression:parse(<<"{, f1 = 5}">>))
	, fun() ->
		Parsed = milang_p_expression:parse(<<"{, f1 = a b (c d),flart=\"hi\" }">>),
		?assertMatch({ok, #milang_ast{type = literal_record}, <<>>}, Parsed),
		{ok, #milang_ast{data = Fields}, <<>>} = Parsed,
		?assertMatch([_, _], Fields),
		[Field1, Field2] = Fields,
		?assertMatch(#milang_ast{ type = literal_record_field, data = {f1, #milang_ast{ type = expression_call}}}, Field1),
		?assertMatch(#milang_ast{ type = literal_record_field, data = {flart, #milang_ast{ type = literal_string }}}, Field2)
	end
	, ?_assertMatch({ok, #milang_ast{type = expression_call}, <<>>}, milang_p_expression:parse(<<"a 7">>))
	, ?_assertMatch({ok, #milang_ast{type = expression_call}, <<>>}, milang_p_expression:parse(<<"a [] b Q">>))
	, ?_assertMatch({ok, #milang_ast{type = expression_call, data = #{ name := #milang_ast{type = type_name_local, data = 'Goober'}, args := []}}, <<>>}, milang_p_expression:parse(<<"Goober">>))
	, ?_assertMatch({ok, #milang_ast{type = expression_call, data = #{ args := [_]}}, <<>>}, milang_p_expression:parse(<<"Goober.Peas3 a">>))
	, ?_assertMatch({ok, #milang_ast{type = expression_call, data = #{ args := [_, _]}}, <<>>}, milang_p_expression:parse(<<"Goober 5.3 []">>))
	, ?_assertMatch({ok, #milang_ast{type = expression_call, data = #{ args := [_]}}, <<>>}, milang_p_expression:parse(<<"a ( b c d )">>))
	, ?_assertMatch({ok, #milang_ast{type = expression_call, data = #{ args := [_, _]}}, <<>>}, milang_p_expression:parse(<<"a b ( c 97.8 \"hi\" )">>))
	, ?_assertMatch({ok, #milang_ast{type = expression_call, data = #{ args := []}}, <<>>}, milang_p_expression:parse(<<"a">>))
	, ?_assertMatch({ok, #milang_ast{type = expression_call, data = #{ args := [_, _]}}, <<>>}, milang_p_expression:parse(<<"add a b">>))
	, ?_assertMatch({ok, #milang_ast{type = literal_list, data = [#milang_ast{ type = literal_list, data = [#milang_ast{ type = literal_list, data = []}]}]}, <<>>}, milang_p_expression:parse(<<"[,[,[]]]">>))
	, fun() ->
		{ok, Parsed, <<>>} = milang_p_expression:parse(<<"1 + 1">>),
		?assertMatch(#milang_ast{ type = expression }, Parsed),
		#milang_ast{ data = Data} = Parsed,
		?assertMatch(#{ head := _, infix_ops := [{_, _}]}, Data),
		#{ infix_ops := [{Op, _Left}]} = Data,
		?assertMatch(#milang_ast{ type = infix_symbol, data = '+'}, Op)
	end
	, fun() ->
		Parsed = milang_p_expression:parse(<<"1 ««add 1"/utf8>>),
		?assertMatch({ok, _, <<>>}, Parsed),
		{ok, Expression, <<>>} = Parsed,
		?assertMatch(#milang_ast{type = expression, data = #{ head := #milang_ast{ type = literal_integer, data = 1}, infix_ops := [{_,_}]}}, Expression),
		#milang_ast{data = #{ infix_ops := [{Op, Right}]}} = Expression,
		?assertMatch(#milang_ast{ type = infix_notation, data = #{ assoc := right, weight := 2, function := #milang_ast{ type = function_name_local}}}, Op),
		?assertMatch(#milang_ast{ type = literal_integer, data = 1}, Right)
	end
	, fun() ->
		{ok, Parsed, <<>>} = milang_p_expression:parse(<<"Garbo »»»»Grammer.and \"malloy\""/utf8>>),
		?assertMatch(#milang_ast{ type = expression}, Parsed),
		#milang_ast{ data = Data } = Parsed,
		?assertMatch(#{ head := #milang_ast{ type = expression_call } }, Data),
		?assertMatch(#{ infix_ops := [{_,_}]}, Data),
		[{Op, Right}] = maps:get(infix_ops, Data),
		?assertMatch(#milang_ast{ type = infix_notation}, Op),
		OpData = Op#milang_ast.data,
		?assertMatch({ok, left}, maps:find(assoc, OpData)),
		?assertMatch({ok, 4}, maps:find(weight, OpData)),
		?assertMatch({ok, #milang_ast{ type = function_name_remote}}, maps:find(function, OpData)),
		?assertMatch(#milang_ast{ type = literal_string, data = <<"malloy">>}, Right)
	end
	, fun() ->
		Parsed = milang_p_expression:parse(<<"not (equal x y)">>),
		?assertMatch({ok, #milang_ast{type = expression_call, data = #{ args := [_]}}, <<>>}, Parsed),
		{ok, #milang_ast{data = #{ args := [Arg]}}, <<>>} = Parsed,
		?assertMatch(#milang_ast{type = expression_call, data = #{ name := #milang_ast{type = function_name_local, data = 'equal'}, args := [_, _]}}, Arg),
		#milang_ast{data = #{ args := [Arg1, Arg2]}} = Arg,
		?assertMatch(#milang_ast{type = function_name_local, data = x}, Arg1),
		?assertMatch(#milang_ast{type = function_name_local, data = y}, Arg2)
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
		?assertMatch({ok, #milang_ast{type = literal_list, data = [_, _, _]}, <<>>}, milang_p_expression:parse(InputString)),
		Parsed = milang_p_expression:parse(InputString),
		{ok, #milang_ast{data = [Elem1, Elem2, Elem3]}, <<>>} = Parsed,
		?assertMatch(#milang_ast{type = expression_call}, Elem1),
		?assertMatch(#milang_ast{type = literal_list, data = [_, _, _]}, Elem2),
		?assertMatch(#milang_ast{type = literal_record, data = [_, _]}, Elem3)
	end
	].
