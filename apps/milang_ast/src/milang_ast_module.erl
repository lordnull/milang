-module(milang_ast_module).

-record(module,
	{ name
	}).

-type module_ast() :: #module{}.

-export_type([module_ast/0]).

-export(
	[ new/1
	, 'name'/1, 'name'/2
	, name_as_string/1
	]).

new(V0) -> #'module'{'name'=V0}.

'name'(R) -> R#'module'.'name'.
'name'(V,R) -> R#'module'{ 'name' = V }.

name_as_string(R) ->
	NameNode = R#module.name,
	case milang_ast:data(NameNode) of
		{_, #{ local := L, module := M} } ->
			unicode:characters_to_binary([M, $., L]);
		{_, N} ->
			N
	end.
