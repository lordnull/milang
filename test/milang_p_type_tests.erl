-module(milang_p_type_tests).

-include_lib("eunit/include/eunit.hrl").

the_test_d() ->
	[ ?_assertMatch({ok, {type_data, _, {type_name_local, _, <<"Jim">>}, []}, <<>>}, milang_p_type:parse(<<"Jim">>))
	, ?_assertMatch({ok, {type_data, _, {type_name_remote, _, _, <<"Jim">>}, []}, <<>>}, milang_p_type:parse(<<"Extra.Name.Jim">>))
	, ?_assertMatch({ok, {variable, _, <<"a">>}, <<>>}, milang_p_type:parse(<<"a">>))
	, ?_assertMatch({ok, {type_data, _, _, [{variable, _, <<"quux">>}]}, <<>>}, milang_p_type:parse(<<"Jim quux">>))
	, ?_assertMatch({ok, {type_data, _, _, [{type_name_local, _, _}, {variable, _, <<"quux">>}]}, <<>>}, milang_p_type:parse(<<"Jim Joe quux">>))
	, fun() ->
		ParseResult = milang_p_type:parse(<<"Jim ( Joe a )">>),
		?assertMatch({ok, {type_data, _, _, [_]}, <<>>}, ParseResult),
		{ok, {type_data, _, _, [Arg]}, <<>>} = ParseResult,
		?assertMatch({type_data, _, _, [_]}, Arg),
		{type_data, _, _, [SubArg]} = Arg,
		?assertMatch({variable, _, <<"a">>}, SubArg)
	end
	, ?_assertMatch({ok, {type_function, _, [{variable, _, <<"a">>}, {type_data, _, _, []}]}, <<>>}, milang_p_type:parse(<<"a -> Jim">>))
	, ?_assertMatch({ok, {type_function, _, [{type_function, _, [_, _, _]}, {type_function, _, [_, _]}]}, <<>>}, milang_p_type:parse(<<"(a -> b -> c) -> (a -> c)">>))
	, ?_assertMatch({ok, {type_function, _, [_, _]}, <<>>}, milang_p_type:parse(<<"(a->b)">>))
	, ?_assertMatch({ok, {type_function, _, [_, _]}, <<>>}, milang_p_type:parse(<<"Jim -> (a -> b)">>))
	].
