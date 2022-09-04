-module(milang_ast_constructor).

-record(constructor,
	{ name
	, args = []
	}).

-type constructor() :: #constructor{}.
-export_type([constructor/0]).

-export(
	[ new/2
	, name/1, name/2
	, args/1, args/2
	]).

new(N, A) -> #constructor{ name = N, args = A}.

name(R) -> R#constructor.name.
name(N, R) -> R#constructor{ name = N }.

args(R) -> R#constructor.args.
args(A, R) -> R#constructor{ args = A }.
