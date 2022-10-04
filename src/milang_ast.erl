%%% @doc Module that holds the type / constructor for milang's ast forms.
-module(milang_ast).

-type name() :: unicode:chardata().

-type location() :: {non_neg_integer(), non_neg_integer()}.

-record(milang_ast,
	{ location = {1,1} :: location()
	, docs = <<>> :: unicode:chardata()
	, data
}).

-export(
	[ ast_node/2, ast_node/3
	, set_doc/2, pre_doc/2, add_doc/2, doc/1
	, set_data/2, transform_data/2, data/1
	, location/1
	, type/1, type_simply/1
	]).

-type ast_node(T) :: #milang_ast{ data :: T }.

-export_type(
	[ ast_node/1
	, name/0
	, location/0
	]).

type(#milang_ast{ data = D}) when is_atom(element(1, D)) ->
	{ok, element(1, D)};
type(_) ->
	undefined.

type_simply(N) ->
	case type(N) of
		{ok, T} -> T;
		U -> U
	end.

ast_node(Location, Docs) ->
	ast_node(Location, Docs, undefined).

ast_node(Location, Docs, Data) ->
	#milang_ast{ location = Location, docs = Docs, data = Data}.

-spec location(ast_node(_)) -> location().
location(Node) ->
	Node#milang_ast.location.

-spec doc(ast_node(_)) -> unicode:chardata().
doc(Node) ->
	Node#milang_ast.docs.

-spec data(ast_node(_)) -> term().
data(Node) ->
	Node#milang_ast.data.

-spec set_data(term(), ast_node(_)) -> ast_node(_).
set_data(Data, Node) ->
	Node#milang_ast{ data = Data }.

-spec transform_data(fun((X) -> X), ast_node(_)) -> ast_node(_).
transform_data(Transfomr, Node) ->
	Data = Node#milang_ast.data,
	Node#milang_ast{ data = Transfomr(Data) }.

-spec set_doc(unicode:chardata(), ast_node(_)) -> ast_node(_).
set_doc(Doc, Node) ->
	Node#milang_ast{ docs = Doc}.

-spec add_doc(unicode:chardata(), ast_node(_)) -> ast_node(_).
add_doc(Doc, Node) ->
	Node#milang_ast{ docs = [Node#milang_ast.docs | Doc]}.

-spec pre_doc(unicode:chardata(), ast_node(_)) -> ast_node(_).
pre_doc(Doc, Node) ->
	Node#milang_ast{ docs = [ Doc | Node#milang_ast.docs] }.
