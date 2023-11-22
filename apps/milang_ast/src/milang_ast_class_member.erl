-module(milang_ast_class_member).

-record(?MODULE,
	{ type :: 'default' | 'spec'
	, member :: milang_ast_class_member_default:ast_node() | milang_ast_class_member_definition:ast_node()
	}).

-type data() :: #?MODULE{}.
-type ast_node() :: milang_ast:ast_node(data()).

-export_type([data/0, ast_node/0]).

-export(
	[ new/2
	, type/1, type/2
	, member/1, member/2
	, to_string/2
	]).

new(Name, Func) ->
	#?MODULE{ type = Name, member= Func}.

type(R) ->
	R#?MODULE.type.

type(N, R) ->
	R#?MODULE{ type = N}.

member(R) ->
	R#?MODULE.member.

member(F, R) ->
	R#?MODULE{ member = F }.

to_string(Data, Depth) ->
	case type(Data) of
		default ->
			milang_ast:to_string(member(Data), Depth, fun milang_ast_class_member_default:to_string/2);
		spec ->
			milang_ast:to_string(member(Data), Depth, fun milang_ast_class_member_definition:to_string/2)
	end.
