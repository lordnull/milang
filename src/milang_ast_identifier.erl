-module(milang_ast_identifier).

-type name() :: unicode:chardata() | #{ local := unicode:chardata(), module := unicode:chardata() }.
-type bound() :: {identifier_bound, name()}.
-type ignored() :: {identifier_ignored, unicode:chardata()}.
-type type() :: {identifier_type, name()}.


-type milang_identifier()
	:: bound()
	|  ignored()
	|  type()
	.

-export_type([milang_identifier/0, bound/0, ignored/0, type/0]).

-export(
	[ type/1
	, ignored/1
	, bound/1
	, new/2
	, as_module_name/1
	]).

type(N) -> new(identifier_type, N).

ignored(N) -> new(identifier_ignored, N).

bound(N) -> new(identifier_bound, N).

new(T, N) ->
	{T, N}.

as_module_name(#{ local := L, module := M}) ->
	[M, $., L];
as_module_name(M) ->
	M.
