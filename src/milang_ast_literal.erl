-module(milang_ast_literal).

-type literal()
	:: {float, float()}
	|  {integer, integer()}
	|  {string, unicode:chardata()}
	.

-export_type([literal/0]).

-export([new_float/1, new_integer/1, new_string/1, new/2]).

new_float(N) -> new(float, N).
new_integer(N) -> new(integer, N).
new_string(N) -> new(string, N).

new(Type, N) -> {Type, N}.
