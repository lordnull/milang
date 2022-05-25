-ifndef(MILANG_AST).
-record(milang_ast, {
	type :: atom(),
	location = {1,1} :: milang_ast:location(),
	doc = <<>> :: milang_ast:doc(),
	data
}).
-define(MILANG_AST, true).
-endif.
