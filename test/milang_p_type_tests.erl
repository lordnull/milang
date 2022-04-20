-module(milang_p_type_tests).

-include_lib("eunit/include/eunit.hrl").

the_test_() ->
	[ ?_assertMatch({ok, {type_concrete, _, {type_name_local, _, <<"Jim">>}, []}, <<>>}, milang_p_type:parse(<<"Jim">>))
	, ?_assertMatch({ok, {type_concrete, _, {type_name_remote, _, _, <<"Jim">>}, []}, <<>>}, milang_p_type:parse(<<"Extra.Name.Jim">>))
	, ?_assertMatch({ok, {type_variable, _, <<"a">>}, <<>>}, milang_p_type:parse(<<"a">>))
	, ?_assertMatch({ok, {type_concrete, _, _, [{type_variable, _, <<"quux">>}]}, <<>>}, milang_p_type:parse(<<"Jim quux">>))
	, ?_assertMatch({ok, {type_concrete, _, _, [{type_concrete, _, _, []}, {type_variable, _, <<"quux">>}]}, <<>>}, milang_p_type:parse(<<"Jim Joe quux">>))
	, fun() ->
		ParseResult = milang_p_type:parse(<<"Jim ( Joe a )">>),
		?assertMatch({ok, {type_concrete, _, _, [_]}, <<>>}, ParseResult),
		{ok, {type_concrete, _, _, [Arg]}, <<>>} = ParseResult,
		?assertMatch({type_concrete, _, _, [_]}, Arg),
		{type_concrete, _, _, [SubArg]} = Arg,
		?assertMatch({type_variable, _, <<"a">>}, SubArg)
	end
	, ?_assertMatch({ok, {type_function, _, [{type_variable, _, <<"a">>}], {type_concrete, _, _, []}}, <<>>}, milang_p_type:parse(<<"a -> Jim">>))
	, ?_assertMatch({ok, {type_function, _, [{type_function, _, [_, _], _}], {type_function, _, [_], _}}, <<>>}, milang_p_type:parse(<<"(a -> b -> c) -> (a -> c)">>))
	, ?_assertMatch({ok, {type_function, _, [_], _}, <<>>}, milang_p_type:parse(<<"(a->b)">>))
	, ?_assertMatch({ok, {type_function, _, [_], _}, <<>>}, milang_p_type:parse(<<"Jim -> (a -> b)">>))
	].
