-module(milang_ast_module).

-record(?MODULE,
	{ name :: milang_ast_identifier:bound()
	}).

-type data() :: #?MODULE{}.
-type ast_node() :: milang_ast:ast_node(data()).

-export_type([data/0, ast_node/0]).

-export(
	[ new/1
	, 'name'/1, 'name'/2
	, name_as_string/1
	, to_string/2
	]).

new(V0) -> #?MODULE{'name'=V0}.

'name'(R) -> R#?MODULE.'name'.
'name'(V,R) -> R#?MODULE{ 'name' = V }.

name_as_string(R) ->
	{identifier_bound, Name} = R#?MODULE.name,
	case Name of
		#{ module := M, local := L} ->
			unicode:characters_to_binary([M, $., L]);
		_ ->
			Name
	end.

to_string(Data, _Depth) ->
	["module ", name_as_string(Data), ".\n"].