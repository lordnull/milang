-module(milang_p_type_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("milang/src/milang_ast.hrl").

the_test_() ->
	[ ?_assertMatch({ok, #milang_ast{type = type_data, data = #{ name := #milang_ast{type = type_name_local, data = 'Jim'}, args := []}}, <<>>}, milang_p_type:parse(<<"Jim">>))
	, ?_assertMatch({ok, #milang_ast{type = type_data, data = #{ name := #milang_ast{type = type_name_remote, data = #{ module := 'Extra.Name', name := 'Jim'}}, args := []}}, <<>>}, milang_p_type:parse(<<"Extra.Name.Jim">>))
	, ?_assertMatch({ok, #milang_ast{type = variable, data = a}, <<>>}, milang_p_type:parse(<<"a">>))
	, fun() ->
		{ok, Parsed, <<>>} = milang_p_type:parse(<<"Jim quux">>),
		?assertMatch(#milang_ast{type = type_data}, Parsed),
		#milang_ast{ data = #{ name := Name, args := Args}} = Parsed,
		?assertMatch(#milang_ast{ type = type_name_local, data = 'Jim'}, Name),
		?assertMatch([ #milang_ast{ type = variable, data = 'quux'}], Args)
	end
	, fun() ->
		{ok, Parsed, <<>>} = milang_p_type:parse(<<"Jim Joe quux">>),
		?assertMatch(#milang_ast{ type = type_data, data = #{ args := [_, _]}}, Parsed),
		#milang_ast{data = #{ name := Name, args := [Arg1, Arg2]}} = Parsed,
		?assertMatch(#milang_ast{ type = type_name_local, data = 'Jim'}, Name),
		?assertMatch(#milang_ast{ type = type_name_local, data = 'Joe'}, Arg1),
		?assertMatch(#milang_ast{ type = variable, data = 'quux'}, Arg2)
	end
	, fun() ->
		ParseResult = milang_p_type:parse(<<"Jim ( Joe a )">>),
		?assertMatch({ok, #milang_ast{type = type_data}, <<>>}, ParseResult),
		{ok, #milang_ast{data = TypeData}, <<>>} = ParseResult,
		?assertMatch(#{ name := #milang_ast{ type = type_name_local, data = 'Jim'}}, TypeData),
		?assertMatch(#{ args := [_]}, TypeData),
		#{ args := [ SubType ]} = TypeData,
		?assertMatch(#milang_ast{ type = type_data }, SubType),
		SubData = SubType#milang_ast.data,
		?assertMatch(#{ name := #milang_ast{ type = type_name_local, data = 'Joe'}}, SubData),
		?assertMatch(#{ args := [ #milang_ast{ type = variable, data = 'a'}]}, SubData)
	end
	, fun() ->
		{ok, Ast, <<>>} = milang_p_type:parse(<<"a -> Jim">>),
		?assertMatch(#milang_ast{ type = type_function, data = [_, _]}, Ast),
		#milang_ast{ data = [Type1, Type2]} = Ast,
		?assertMatch(#milang_ast{ type = variable, data = 'a'}, Type1),
		?assertMatch(#milang_ast{ type = type_data}, Type2),
		#milang_ast{ data = TypeData} = Type2,
		?assertMatch(#{ name := #milang_ast{ type = type_name_local, data = 'Jim'}}, TypeData),
		?assertMatch(#{ args := []}, TypeData)
	end
	, ?_assertMatch({ok, #milang_ast{type = type_function, data = [#milang_ast{type = type_function, data = [_, _, _]}, #milang_ast{type = type_function, data = [_, _]}]}, <<>>}, milang_p_type:parse(<<"(a -> b -> c) -> (a -> c)">>))
	, ?_assertMatch({ok, #milang_ast{type = type_function, data = [_, _]}, <<>>}, milang_p_type:parse(<<"(a->b)">>))
	, ?_assertMatch({ok, #milang_ast{type = type_function, data = [_, _]}, <<>>}, milang_p_type:parse(<<"Jim -> (a -> b)">>))
	].
