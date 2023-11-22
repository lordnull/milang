-module(milang_ast_concrete_record).

-record(?MODULE,
	{ fields = [milang_ast_spec_recored_field:ast_node()]
	}).
-type data() :: #?MODULE{}.
-type ast_node() :: milang_ast:ast_node(data()).

-export_type([ data/0, ast_node/0]).
-export(
	[ new/0, new/1
	, fields/1, fields/2
	, to_string/2
	]).

new() ->
	new([]).

new(Fields) ->
	#?MODULE{ fields = Fields }.

fields(Rec) ->
	Rec#?MODULE.fields.

fields(Fields, Rec) ->
	Rec#?MODULE{ fields = Fields}.

to_string(Data, Depth) ->
	FieldsStringUnjoined = lists:map(fun(Field) ->
		milang_ast:to_string(Field, Depth, fun milang_ast_record_field:to_string/2)
	end, fields(Data)),
	FieldsJoined = lists:join("\n", FieldsStringUnjoined),
	["{ ", FieldsJoined, "\n", lists_more:repeat("\t", Depth), "}"].
