-module(milang_ast_expression_record).

-record(record,
	{ base_reference
	, fields = []
	}).

-export(
	[ new/0, new/1
	, base_reference/1, base_reference/2
	, fields/1, fields/2
	]).

new() ->
	new(undefined).

new(MaybeBase) ->
	#record{ base_reference = MaybeBase }.

base_reference(Rec) ->
	Rec#record.base_reference.

base_reference(Ref, Rec) ->
	Rec#record{ base_reference = Ref}.

fields(Rec) ->
	Rec#record.fields.

fields(Fields, Rec) ->
	Rec#record{ fields = Fields}.
