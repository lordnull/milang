-module(milang_ast_literal_record).

-record(?MODULE,
	{ base_reference :: maybe:maybe(milang_ast_expression:ast_node())
	, fields = [milang_ast_recored_field:ast_node()]
	}).
-type data() :: #?MODULE{}.
-type ast_node() :: milang_ast:ast_node(data()).

-export_type([ data/0, ast_node/0]).
-export(
	[ new/0, new/1, new/2
	, base_reference/1, base_reference/2
	, fields/1, fields/2
	, to_string/2
	]).

new() ->
	new(undefined).

new(MaybeBase) ->
	new(MaybeBase, []).

new(MaybeBase, Fields) ->
	#?MODULE{ base_reference = MaybeBase, fields = Fields }.

base_reference(Rec) ->
	Rec#?MODULE.base_reference.

base_reference(Ref, Rec) ->
	Rec#?MODULE{ base_reference = Ref}.

fields(Rec) ->
	Rec#?MODULE.fields.

fields(Fields, Rec) ->
	Rec#?MODULE{ fields = Fields}.

to_string(Data, Depth) ->
	FieldsStringUnjoined = lists:map(fun(Field) ->
		milang_ast:to_string(Field, Depth, fun milang_ast_record_field:to_string/2)
	end, fields(Data)),
	FieldsJoined = lists:join("\n", FieldsStringUnjoined),
	BaseString = case base_reference(Data) of
		undefined ->
			[];
		BaseRef ->
			["\n", lists_more:repeat(Depth, "\t"), ",, ", milang_ast:to_string(BaseRef, Depth, fun milang_ast_expression:to_string/2)]
	end,
	["{ ", FieldsJoined, BaseString, "\n", lists_more:repeat("\t", Depth), "}"].
