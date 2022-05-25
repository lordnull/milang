-module(milang_type_validation_tests).

-include_lib("eunit/include/eunit.hrl").

ints_are_not_strings_test_d() ->
	Source = <<
	"func : String.\n"
	"func -> 5."
	>>,
	{ok, [Spec, Func], <<>>} = milang_parse:string(Source),
	EmptyTable = milang_type_validation:new(),
	{ok, UpdatedTable} = milang_type_validation:validate(Spec, EmptyTable),
	Got = milang_type_validation:validate(Func, UpdatedTable),
	?assertNotMatch({ok, _}, Got).
