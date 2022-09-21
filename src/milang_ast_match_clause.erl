-module(milang_ast_match_clause).

-type match()
	:: {literal_string, unicode:chardata()}
	|  {literal_integer, integer()}
	|  {literal_float, float()}
	|  {match_list, [ match() ]}
	|  {match_list_head, [ match() ], milang_ast_identifier:identifier()}
	|  {match_type, milang_ast_identifier:identifier(), [ match() ]}
	|  {identifier_ignored, unicode:chardata()}
	|  {identifier_bound, unicode:chardata()}
	.


-record(match_clause,
	{ match
	, binds = []
	, expression
	}).
-type match_clause() :: #match_clause{}.
-export_type([match_clause/0, match/0]).

-export(
	[ new/3
	, 'match'/1, 'match'/2
	, 'binds'/1, 'binds'/2
	, 'expression'/1, 'expression'/2
	]).

new(V0,V1,V2) -> #'match_clause'{'match'=V0,'binds'=V1,'expression'=V2}.

'match'(R) -> R#'match_clause'.'match'.
'match'(V,R) -> R#'match_clause'{ 'match' = V }.

'binds'(R) -> R#'match_clause'.'binds'.
'binds'(V,R) -> R#'match_clause'{ 'binds' = V }.

'expression'(R) -> R#'match_clause'.'expression'.
'expression'(V,R) -> R#'match_clause'{ 'expression' = V }.
