-module(milang_ast_class_member_default).

-record(default,
	{ name
	, function
	}).

-type default() :: #default{}.

-export_type([default/0]).

-export(
	[ new/2
	, name/1, name/2
	, function/1, function/2
	]).

new(Name, Func) ->
	#default{ name = Name, function= Func}.

name(R) ->
	R#default.name.

name(N, R) ->
	R#default{ name = N}.

function(R) ->
	R#default.function.

function(F, R) ->
	R#default{ function = F }.
