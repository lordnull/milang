-module(milang_ast_identifier).

-type name() :: unicode:chardata() | #{ local := unicode:chardata(), module := unicode:chardata() }.
-type bound() :: {identifier_bound, name()}.
-type ignored() :: {identifier_ignored, unicode:chardata()}.


-type data()
	:: bound()
	|  ignored()
	.
-type ast_node() :: milang_ast:ast_node(data()).

-export_type([data/0, bound/0, ignored/0, ast_node/0]).

-export(
	[ ignored/1
	, bound/1
	, from_token/1
	, new/2
	, as_module_name/1
	]).

ignored(N) -> new(identifier_ignored, N).

bound(N) -> new(identifier_bound, N).

-spec from_token(milang_p_token:identifier_bindable()) -> data().
from_token({identifier_ignored, N}) ->
	new(identifier_ignored, N);
from_token({identifier_bound, N}) ->
	new(identifier_bound, N).

new(T, N) ->
	{T, N}.

as_module_name({identifier_bound, N}) ->
	as_module_name(N);
as_module_name(#{ local := L, module := M}) ->
	[M, $., L];
as_module_name(M) ->
	M.
