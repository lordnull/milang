-module(milang_type_tests).

-include_lib("eunit/include/eunit.hrl").

the_test_() ->
	[ ?_assertMatch({type_concrete, _, {local_type_name, _, <<"Jim">>}, []}, milang_type:parse(<<"Jim">>))
	, ?_assertMatch({type_concrete, _, {remote_type_name, _, <<"Extra.Name">>, <<"Jim">>}, []}, milang_type:parse(<<"Extra.Name.Jim">>))
	, ?_assertMatch({type_variable, _, <<"a">>}, milang_type:parse(<<"a">>))
	, ?_assertMatch({type_concrete, _, _, [{type_variable, _, <<"quux">>}]}, milang_type:parse(<<"Jim quux">>))
	, ?_assertMatch({type_concrete, _, _, [{type_concrete, _, _, []}, {type_variable, _, <<"quux">>}]}, milang_type:parse(<<"Jim Joe quux">>))
	, fun() ->
		ParseResult = milang_type:parse(<<"Jim ( Joe a )">>),
		?assertMatch({type_concrete, _, _, [_]}, ParseResult),
		{type_concrete, _, _, [Arg]} = ParseResult,
		?assertMatch({type_concrete, _, _, [_]}, Arg),
		{type_concrete, _, _, [SubArg]} = Arg,
		?assertMatch({type_variable, _, <<"a">>}, SubArg)
	end
	, ?_assertMatch({type_function, _, [{type_variable, _, <<"a">>}], {type_concrete, _, _, []}}, milang_type:parse(<<"a -> Jim">>))
	, ?_assertMatch({type_function, _, [{type_function, _, [_, _], _}], {type_function, _, [_], _}}, milang_type:parse(<<"(a -> b -> c) -> (a -> c)">>))
	, ?_assertMatch({type_function, _, [_], _}, milang_type:parse(<<"(a->b)">>))
	, ?_assertMatch({type_function, _, [_], _}, milang_type:parse(<<"Jim -> (a -> b)">>))
	].
