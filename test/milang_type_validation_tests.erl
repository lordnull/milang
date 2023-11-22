-module(milang_type_validation_tests).

-include_lib("eunit/include/eunit.hrl").

simplest_type_mismatch_test() ->
	Source = unicode:characters_to_binary(
		"data A."
		"\ndata B."
		"\n"
		"\nspec f = A."
		"\nlet f = B."
		),
	{ok, Ast} = milang_parse:string(Source),
	Got = milang_type_validation:validate_list(Ast),
	?assertNotMatch({ok, _}, Got).

simplest_type_match_test() ->
	Source = unicode:characters_to_binary(
		"data Unit."
		"\n"
		"\nspec f = Unit."
		"\nlet f = Unit."
		),
	{ok, Ast} = milang_parse:string(Source),
	Got = milang_type_validation:validate_list(Ast),
	?assertMatch({ok, _}, Got).

