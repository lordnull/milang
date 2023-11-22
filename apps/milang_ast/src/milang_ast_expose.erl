%%% @doc depricated in favor of putting the expose directly on what is exposed.
-module(milang_ast_expose).

-export(
	[ new/1
	, new/2
	, expose_tag/1, expose_tag/2
	, declaration/1, declaration/2
	]).

-record(expose,
	{ declaration % binding (function / constant), type, class, implement, or import.
	, expose_tag = expose
	}).

new(Declaration) ->
	new(Declaration, expose).

new(Declaration, Tag) ->
	#expose{ expose_tag = Tag, declaration = Declaration}.

declaration(Rec) ->
	Rec#expose.declaration.

declaration(Dec, Rec) ->
	Rec#expose{ declaration = Dec }.

expose_tag(Rec) -> Rec#expose.expose_tag.

expose_tag(Tag, Rec) -> Rec#expose{ expose_tag = Tag }.
