%%% @doc Module that holds the type / constructor for milang's ast forms.
-module(milang_ast).

-type location() :: {pos_integer(), pos_integer()}.
-type doc() :: {comment, location(), unicode:chardata()} | unicode:chardata().
-type docs() :: doc() | maybe_improper_list(doc(), doc()).

-include("milang_ast.hrl").

-type name_interior() :: atom() | #{ local := atom(), module := atom() }.

-type name_downcase() :: {name_downcase, name_interior()}.

-type name_upcase() :: {name_upcase, name_interior()}.

-type name_symbol() :: {name_symbol, name_interior()}.

-type name_underscore() :: {name_underscore, name_interior()}.

-type name()
	:: name_downcase()
	|  name_upcase()
	|  name_symbol()
	|  name_underscore()
	.

-type ast_node(Type) :: #milang_ast{ location :: location(), doc :: docs(), data :: Type }.

-export(
	[ ast_node/2, ast_node/3
	, set_doc/2, add_doc/2, ins_doc/2, doc/1
	, set_data/2, transform_data/2, data/1
	, location/1
	, to_string/1
	]).
-export(
	[ literal_string/2
	, literal_float/2
	, literal_integer/2
	, expression_call/3
	, expression_construct/3
	, infix_notation/4
	, infix_operation/3
	, expression_infix/3
	, expression_record_field/3
	, record_field_access/2
	, expression_record/3
	, declaration_module/3
	, declaration_import/4
	, type_variable/2
	, type_function/2
	, type_concrete/3
	, type_record/2
	, constraint/3
	, constructor/3
	, declaration_type/5
	, declaration_alias/4
	, class_member_definition/3
	, class_member_default/4
	, declaration_class/5
	, declaration_spec/3
	, binding/3
	, declaration_function/5
	, function_variable/2
	]).

-type ast_node() :: ast_node(node_data()).

-type literal_string() :: ast_node({literal_string, unicode:chardata()}).

-type literal_integer() :: ast_node({literal_integer, integer()}).

-type literal_float() :: ast_node({literal_float, float()}).

-type record_field_access() :: #record_field_access{ name :: name_downcase() }.

-type expression_call(ArgType) :: ast_node(#expression_call{ function :: name_downcase() | name_symbol() | record_field_access() , args :: [ ArgType] }).

-type infix_notation() :: ast_node(#infix_notation{ name :: name_downcase() | name_symbol()}).

-type infix_operation(Expression) :: ast_node(#infix_operation{ operator :: infix_notation(), expression :: Expression }).

-type expression_infix(SubExpressionType) :: #expression_infix{head :: SubExpressionType, infix_ops :: [ infix_operation(SubExpressionType) ]}.

-type expression_record_field(SubExpressionType) :: #expression_record_field{ name :: name_downcase(), expression :: SubExpressionType }.

-type expression_record(SubExpressionType) :: #expression_record{ base_reference :: name_downcase(), fields :: [ expression_record_field(SubExpressionType) ]}.

-type declaration_module() :: ast_node(#declaration_module{ exposing :: [ name_downcase() | name_upcase() | name_symbol() ]}).

-type declaration_import() :: ast_node(#declaration_import{ exposing :: [ name_downcase() | name_upcase() | name_symbol() ]}).

-type expression()
	:: literal_string()
	|  literal_float()
	|  literal_integer()
	|  expression_construct(expression())
	|  expression_call(expression())
	|  expression_infix(expression())
	|  expression_record(expression())
	.

-type type_variable() :: ast_node(#type_variable{ name :: name_downcase() | name_underscore() }).

-type function_variable() :: ast_node(#function_variable{ name :: name_downcase() | name_underscore() }).

-type type_function(ArgType) :: ast_node(#type_function{ args :: [ ArgType ]}).

-type type_concrete(ArgType) :: ast_node(#type_concrete{ name :: name_upcase(), args :: [ ArgType ] }).

-type type_record_field(ArgType) :: ast_node(#type_record_field{ name :: name_downcase(), type :: ArgType }).

-type type_record(ArgType) :: ast_node(#type_record{ fields :: [ type_record_field(ArgType) ]}).

-type type_arg() :: type_concrete(type_arg()) | type_variable() | type_function(type_arg()) | type_record(type_arg()).

-type type_concrete() :: type_concrete(type_arg()).

-type type_function() :: type_function(type_arg()).

-type type_record() :: type_record(type_arg()).

-type type_record_field() :: type_record_field(type_arg()).

-type constraint() :: #constraint{ name :: name_downcase(), constraint :: type_concrete(type_arg())}.

-type constructor() :: ast_node(#constructor{ name :: name_upcase, args :: [ type_arg() ]}).

-type declaration_type() :: ast_node(#declaration_type{ name :: name_upcase(), args :: [ name_downcase() ], constraints :: [ constraint() ], constructors :: [ constructor() ]}).

-type declaration_alias() :: ast_node(#declaration_alias{ name :: name_upcase(), args :: [ type_variable() ], alias_of :: type_concrete(type_arg()) | type_function(type_arg())}).

-type class_member_definition() :: ast_node(#class_member_definition{ name :: name_downcase() | name_symbol(), definition :: type_function(type_arg())}).

-type class_member_default() :: ast_node(#class_member_default{ name :: name_downcase() | name_symbol(), args :: [ name_downcase() | name_underscore() ] , expression :: expression()}).

-type class_member() :: class_member_definition() | class_member_default().

-type declaration_class() :: ast_node(#declaration_class{ name :: name_upcase(), constraints :: [ constraint() ], args :: [ type_variable() ], members :: [ class_member() ]}).

-type declaration_spec() :: ast_node(#declaration_spec{ name :: name_downcase() | name_symbol(), type :: type_function() }).

-type binding() :: ast_node(#binding{ name :: name_downcase() | name_underscore(), expression :: expression()}).

-type declaration_function() :: ast_node(#declaration_function{ name :: name_downcase() | name_symbol(), args :: [ function_variable() ], bindings :: [ binding() ], expression :: expression()}).

-type infix_operation() :: infix_operation(expression()).

-type node_data()
	:: declaration_function()
	|  binding()
	|  declaration_spec()
	|  declaration_class()
	|  class_member()
	|  declaration_alias()
	|  declaration_type()
	|  constructor()
	|  constraint()
	|  type_concrete(type_arg())
	|  type_function(type_arg())
	|  type_record(type_arg())
	|  type_record_field(type_arg())
	|  type_variable()
	|  declaration_import()
	|  declaration_module()
	|  expression()
	|  infix_notation()
	|  infix_operation(expression())
	.

-export_type(
	[ ast_node/1, ast_node/0
	, name/0, location/0
	, declaration_function/0
	, binding/0
	, declaration_spec/0
	, declaration_class/0
	, class_member/0
	, declaration_alias/0
	, declaration_type/0
	, constructor/0
	, constraint/0
	, type_concrete/0
	, type_function/0
	, type_record/0
	, type_record_field/0
	, type_variable/0
	, declaration_import/0
	, declaration_module/0
	, expression/0
	, infix_notation/0
	, infix_operation/0
	]).

-spec literal_string(ast_node(_), unicode:chardata()) -> literal_string().
literal_string(Node, String) ->
	set_data({literal_string, String}, Node).

-spec literal_integer(ast_node(_), integer()) -> literal_integer().
literal_integer(Node, Integer) ->
	set_data({literal_integer, Integer}, Node).

-spec literal_float(ast_node(_), float()) -> literal_float().
literal_float(Node, Float) ->
	set_data({literal_float, Float}, Node).

-spec record_field_access(ast_node(), name_downcase()) -> ast_node(record_field_access()).
record_field_access(Node, Name) ->
	set_data(#record_field_access{ name = Name}, Node).

-spec expression_call(ast_node(_), name_downcase() | name_upcase(), [ ArgType ]) -> expression_call(ArgType).
expression_call(Node, Function, Args) ->
	set_data(#expression_call{ function = Function, args = Args}, Node).

-type expression_construct(ArgType) :: ast_node(#expression_construct{ type :: name_upcase(), args :: [ ArgType ]}).
expression_construct(Node, Type, Args) ->
	set_data(#expression_construct{ type = Type, args = Args}, Node).

-spec infix_notation(ast_node(_), name_downcase() | name_symbol(), pos_integer(), left | right) -> infix_notation().
infix_notation(Node, Name, Weight, Assoc) ->
	set_data(#infix_notation{ name = Name, weight = Weight, assoc = Assoc}, Node).

-spec infix_operation(ast_node(_), infix_notation(), Expression) -> infix_operation(Expression).
infix_operation(Node, Operator, Expression) ->
	set_data(#infix_operation{ operator = Operator, expression = Expression}, Node).

-spec expression_infix(ast_node(_), Type, [ infix_operation(Type) ]) -> ast_node(expression_infix(Type)).
expression_infix(Node, Head, Ops) ->
	set_data(#expression_infix{ head = Head, infix_ops = Ops}, Node).

-spec expression_record_field(ast_node(_), name_downcase(), Expression) -> ast_node(expression_record_field(Expression)).
expression_record_field(Node, Name, Expression) ->
	set_data(#expression_record_field{ name = Name, expression = Expression}, Node).

-spec expression_record(ast_node(_), {ok, name_downcase()} | undefined, [ expression_record_field(T) ]) -> ast_node(expression_record(T)).
expression_record(Node, Base, Fields) ->
	set_data(#expression_record{ base_reference = Base, fields = Fields}, Node).

-spec declaration_module(ast_node(_), atom(), [ name_downcase() | name_upcase() | name_symbol() ]) -> declaration_module().
declaration_module(Node, Name, Exposing) ->
	set_data(#declaration_module{ name = Name, exposing = Exposing}, Node).

-spec declaration_import(ast_node(_), atom(), undefined | {ok, atom()}, [ name_downcase() | name_upcase() | name_symbol() ]) -> declaration_import().
declaration_import(Node, Module, Alias, Exposing) ->
	set_data(#declaration_import{ name = Module, alias = Alias, exposing = Exposing}, Node).

-spec type_variable(ast_node(_), name_underscore() | name_downcase()) -> type_variable().
type_variable(Node, Name) ->
	set_data(#type_variable{ name = Name }, Node).

-spec function_variable(ast_node(_), name_underscore() | name_downcase()) -> function_variable().
function_variable(Node, Name) ->
	set_data(#function_variable{ name = Name }, Node).

-spec type_function(ast_node(_), [ T ]) -> ast_node(type_function(T)).
type_function(Node, Args) ->
	set_data(#type_function{ args = Args}, Node).

-spec type_concrete(ast_node(_), name_upcase(), [ T ]) -> ast_node(type_concrete(T)).
type_concrete(Node, Name, Args) ->
	set_data(#type_concrete{ name = Name, args = Args}, Node).

-spec type_record(ast_node(_), [ type_record_field() ]) -> ast_node(type_record()).
type_record(Node, Fields) ->
	set_data(#type_record{ fields = Fields}, Node).

-spec constraint(ast_node(_), name_downcase(), type_concrete(type_arg())) -> ast_node(constraint()).
constraint(Node, Name, Constraint) ->
	set_data(#constraint{ name = Name, constraint = Constraint}, Node).

-spec constructor(ast_node(_), name_upcase(), [ type_arg() ]) -> constructor().
constructor(Node, Name, Args) ->
	set_data(#{ name => Name, args => Args}, Node).

-spec declaration_type(ast_node(_), name_upcase(), [ name_downcase() ], [ constraint() ], [ constructor() ]) -> ast_node(declaration_type()).
declaration_type(Node, Name, Args, Constraints, Constructors) ->
	set_data(#declaration_type{
		name = Name,
		args = Args,
		constraints = Constraints,
		constructors = Constructors
		}, Node).

-spec declaration_alias(ast_node(_), name_upcase(), [ type_variable() ], type_concrete(type_arg()) | type_function(type_arg())) -> declaration_alias().
declaration_alias(Node, Name, Args, Original) ->
	set_data(#declaration_alias{ name = Name, args = Args, alias_of = Original}, Node).

-spec class_member_definition(ast_node(_), name_downcase() | name_symbol(), type_function(type_arg())) -> class_member_definition().
class_member_definition(Node, Name, Definition) ->
	set_data(#class_member_definition{ name = Name, definition = Definition}, Node).

-spec class_member_default(ast_node(_), name_downcase() | name_symbol(), [ name_downcase() | name_underscore() ], expression()) -> class_member_default().
class_member_default(Node, Name, Args, Expression) ->
	set_data(#class_member_default{ name = Name, args = Args, expression = Expression}, Node).

-spec declaration_class(ast_node(_), name_upcase(), [ constraint() ], [ type_variable() ], [ class_member() ]) -> declaration_class().
declaration_class(Node, Name, Constraints, Args, Members) ->
	set_data(#declaration_class{ name = Name, constraints = Constraints, args = Args, members = Members}, Node).

-spec declaration_spec(ast_node(_), name_downcase() | name_symbol(), type_function()) -> declaration_spec().
declaration_spec(Node, Name, Args) ->
	set_data(#declaration_spec{ name = Name, type = Args }, Node).

-spec binding(ast_node(_), name_downcase() | name_underscore(), expression())-> binding().
binding(Node, Name, Expression) ->
	set_data(#binding{ name = Name, expression = Expression}, Node).

-spec declaration_function(ast_node(_), name_downcase() | name_symbol(), [ name_downcase() | name_underscore() ], [ binding() ], expression()) -> declaration_function().
declaration_function(Node, Name, Args, Bindings, Expression) ->
	set_data(#declaration_function{ name = Name, args = Args, bindings = Bindings, expression = Expression}, Node).

-spec ast_node(location(), docs()) -> ast_node(undefined).
ast_node(Location, Doc) ->
	ast_node(Location, Doc, undefined).

-spec ast_node(location(), docs(), Data) -> ast_node(Data).
ast_node(Loc, Doc, Data) ->
	#milang_ast{ location = Loc, doc = Doc, data = Data}.

-spec location(ast_node(_)) -> location().
location(Node) ->
	Node#milang_ast.location.

-spec set_doc(docs(), ast_node(A)) -> ast_node(A).
set_doc(Doc, Node) ->
	Node#milang_ast{ doc = Doc }.

-spec add_doc(docs(), ast_node(A)) -> ast_node(A).
add_doc(Doc, Node) ->
	Node#milang_ast{ doc = [Node#milang_ast.doc | Doc]}.

-spec ins_doc(docs(), ast_node(A)) -> ast_node(A).
ins_doc(Doc, Node) ->
	Node#milang_ast{ doc = [Doc | Node#milang_ast.doc] }.

-spec set_data(NewData, ast_node(_)) -> ast_node(NewData).
set_data(Data, Node) ->
	Node#milang_ast{ data = Data }.

-spec transform_data(fun((OldData) -> NewData), ast_node(OldData)) -> ast_node(NewData).
transform_data(Mapper, Node) ->
	Node#milang_ast{ data = Mapper(Node#milang_ast.data) }.

-spec data(ast_node(D)) -> D.
data(Node) ->
	Node#milang_ast.data.

-spec doc(ast_node(_)) -> docs().
doc(Node) ->
	Node#milang_ast.doc.

%-type declaration_function() :: #milang_ast{ type :: declaration_function, data :: #{ name := atom(), args := [ atom() ], bindings := [ binding() ], expression := expression()}}.
%
%-type binding() :: #milang_ast{ type :: binding, data :: #{ name := atom(), expression := expression() }}.
%
%-type declaration_spec() :: #milang_ast{ type :: declaration_spec, data :: #{ name := atom(), spec := type_function()}}.
%
%-type type_name_local() :: #milang_ast{ type :: type_name_local, data :: atom()}.
%
%-type type_name_remote() :: #milang_ast{ type :: type_name_remote, data :: #{ name := atom(), module := atom()}}.
%
%-type type_name() :: type_name_local() | type_name_remote().
%
%-type infix() :: infix_notation() | infix_symbol().
%
%-type infix_notation() :: #milang_ast{ type :: infix_notation, data :: #{ assoc := 'left' | 'right', weight := pos_integer(), function := atom()}}.
%
%-type infix_symbol() :: #milang_ast{ type :: infix_symbol, data :: atom()}.
%
%-type function_name() :: function_name_local() | function_name_remote() | function_name_symbol().
%
%-type function_name_local() :: #milang_ast{ type :: function_name_local, data :: atom()}.
%
%-type function_name_remote() :: #milang_ast{ type :: function_name_remote, data :: #{ name := atom(), module := atom()}}.
%
%-type function_name_symbol() :: #milang_ast{ type :: function_name_symbol, data :: atom()}.
%
%-type expression() :: #milang_ast{ type :: expression, data :: #{ head := expression_primary(), infix_ops := [{infix(), expression_primary()}]}}.
%
%-type expression_primary() :: expression_call() | expression_literal() | expression_parens().
%
%-type expression_call() :: #milang_ast{ type :: expression_call, data :: #{ name := type_name() | function_name(), args := [ argument() ]}}.
%
%-type expression_literal() :: literal_list() | literal_map() | literal_record() | literal_string() | literal_float() | literal_integer().
%
%-type expression_parens() :: expression_primary().
%
%-type argument() :: expression_literal() | function_name() | type_name() | expression_parens().
%
%-type literal_list() :: #milang_ast{ type :: literal_list, data :: [expression_primary()]}.
%
%-type literal_map() :: #milang_ast{ type :: literal_map, data :: [ literal_map_entry() ]}.
%
%-type literal_map_entry() :: #milang_ast{ type :: literal_map_entry, data :: #{ key := expression_primary(), value := expression_primary()}}.
%
%-type literal_record() :: #milang_ast{ type :: literal_record, data :: [ literal_record_entry() ]}.
%
%-type literal_record_entry() :: #milang_ast{ type :: literal_record_entry, data :: [ {pos_integer() | atom(), expression_primary()}]}.
%
%-type literal_string() :: #milang_ast{ type :: literal_string, data :: unicode:unicode_binary()}.
%
%-type literal_float() :: #milang_ast{ type :: literal_float, data :: float()}.
%
%-type literal_integer() :: #milang_ast{ type :: literal_integer, data :: integer()}.
%
%-type type() :: type_concrete() | type_function() | type_record() | type_data().
%
%-type type_data() :: #milang_ast{ type :: type_data, data :: #{ name := atom(), args := [type_data_arg()]}}.
%
%-type type_data_arg() :: type_name() | variable() | type_record() | type_concrete().
%
%-type type_function() :: #milang_ast{ type :: type_function, data :: [ type_function_arg() ]}.
%
%-type type_function_arg() :: type_data() | type_record() | type_concrete() | variable().
%
%-type type_record() :: #milang_ast{ type :: type_record, data :: [{pos_integer() | atom(), type_concrete()}]}.
%
%-type type_concrete() :: type_data() | type_function() | type_record().
%
%-type variable() :: #milang_ast{ type :: variable, data :: atom()}.
%
%-type ast_node()
%	:: constraints()
%	|  declaration_module()
%	|  declaration_import()
%	|  declaration_alias()
%	|  declaration_type()
%	|  declaration_class()
%	|  class_member()
%	|  class_member_definition()
%	|  class_member_default()
%	|  declaration_function()
%	|  binding()
%	|  declaration_spec()
%	|  type_name_local()
%	|  type_name_remote()
%	|  type_name()
%	|  infix()
%	|  infix_notation()
%	|  infix_symbol()
%	|  function_name()
%	|  function_name_local()
%	|  function_name_remote()
%	|  function_name_symbol()
%	|  expression()
%	|  expression_primary()
%	|  expression_call()
%	|  expression_literal()
%	|  expression_parens()
%	|  argument()
%	|  literal_list()
%	|  literal_map()
%	|  literal_map_entry()
%	|  literal_record()
%	|  literal_record_entry()
%	|  literal_string()
%	|  literal_float()
%	|  literal_integer()
%	|  type()
%	|  type_data()
%	|  type_data_arg()
%	|  type_function()
%	|  type_function_arg()
%	|  type_record()
%	|  type_concrete()
%	|  variable()
%	.
%
%-export_type(
%	[doc/0
%	,location/0
%	,ast_node/0
%	,constraints/0
%	,declaration_module/0
%	,declaration_import/0
%	,declaration_alias/0
%	,declaration_type/0
%	,declaration_class/0
%	,class_member/0
%	,class_member_definition/0
%	,class_member_default/0
%	,declaration_function/0
%	,binding/0
%	,declaration_spec/0
%	,type_name_local/0
%	,type_name_remote/0
%	,type_name/0
%	,infix/0
%	,infix_notation/0
%	,infix_symbol/0
%	,function_name/0
%	,function_name_local/0
%	,function_name_remote/0
%	,function_name_symbol/0
%	,expression/0
%	,expression_primary/0
%	,expression_call/0
%	,expression_literal/0
%	,expression_parens/0
%	,argument/0
%	,literal_list/0
%	,literal_map/0
%	,literal_map_entry/0
%	,literal_record/0
%	,literal_record_entry/0
%	,literal_string/0
%	,literal_float/0
%	,literal_integer/0
%	,type/0
%	,type_data/0
%	,type_data_arg/0
%	,type_function/0
%	,type_function_arg/0
%	,type_record/0
%	,type_concrete/0
%	,variable/0
%	]).
%
%-export(
%	[ast_node/4
%	,constraint/2
%	,constraints/1
%	,declaration_module/4
%	,declaration_import/5
%	,declaration_alias/5
%	,declaration_type/6
%	,declaration_class/6
%	,class_member_definition/4
%	,class_member_default/5
%	,declaration_function/6
%	,binding/4
%	,local_to_remote_name/2
%	]).
%-export(
%	[ set_doc/2
%	, doc/1
%	, add_doc/2
%	, pre_doc/2
%	, set_data/2
%	, transform_data/2
%	, data/1
%	, to_map/1
%	, to_string/1
%	, type/1
%	, location/1
%	]).
%
%local_to_remote_name(Remote, #milang_ast{ type = function_name_local } = Node) ->
%	LocalName = Node#milang_ast.data,
%	NewData = #{ name => LocalName, module => Remote },
%	Node#milang_ast{ type = function_name_remote, data = NewData};
%local_to_remote_name(Remote, #milang_ast{ type = function_name_symbol } = Node) ->
%	LocalName = Node#milang_ast.data,
%	NewData = #{ name => LocalName, module => Remote },
%	Node#milang_ast{ type = function_name_remote, data = NewData};
%local_to_remote_name(Remote, #milang_ast{ type = type_name_local } = Node) ->
%	LocalName = Node#milang_ast.data,
%	NewData = #{ name => LocalName, module => Remote },
%	Node#milang_ast{ type = type_name_remote, data = NewData};
%local_to_remote_name(_Remote, Node) ->
%	Node.
%
%to_a(B) -> binary_to_atom(B, utf8).
%

-spec to_string(ast_node()) -> unicode:chardata().
to_string(Node) ->
	io:format("Das node to_string: ~p~n", [Node]),
	DocString = doc_to_string(milang_ast:doc(Node)),
	ElementString = data_to_string(milang_ast:data(Node)),
	[DocString, ElementString].

doc_to_string({comment, _, Data}) ->
	doc_to_string(Data);
doc_to_string(<<>>) ->
	<<>>;
doc_to_string(Data) when is_binary(Data) ->
	[<<"{-">>, Data, <<"-}\n">>];
doc_to_string(Data) ->
	doc_to_string(unicode:characters_to_binary(Data)).

collection_to_string(IndentDepth, Open, Close, Items) ->
	Tabs = lists:duplicate(IndentDepth, "\t"),
	ElementPrefix = [<<"\n">>, Tabs, <<", ">>],
	Elements = lists:map(fun(Item) ->
		[ElementPrefix, to_string(Item)]
	end, Items),
	[Open, Elements, <<"\n">>, Tabs, Close].

list_to_string(IndentDepth, Items) ->
	collection_to_string(IndentDepth, $[, $], Items).

record_to_string(IndentDepth, Items) ->
	collection_to_string(IndentDepth, ${, $}, Items).

-spec name_to_string(name(), boolean()) -> unicode:chardata().
name_to_string({NameType, #{ module := M, local := L}}, QuoteSymbol) ->
	[atom_to_binary(M, utf8), $., name_to_string({NameType, L}, QuoteSymbol) ];
name_to_string({name_symbol, L}, true) ->
	[$', name_to_string({name_symbol, L}, true), $'];
name_to_string({_, L}, _) ->
	atom_to_binary(L, utf8).

data_to_string(#constraint{ name = Name, constraint = Constraint }) ->
	[ <<"\n\t, ">>, name_to_string(Name, false), <<" = ">>, to_string(Constraint) ];
data_to_string(#constructor{ name = Name, args = Args}) ->
	NameStr = name_to_string(Name, true),
	ArgStrings = [ to_string(A) || A <- Args ],
	lists:join(<<" ">>, [NameStr | ArgStrings]);
data_to_string(#declaration_module{ name = Name, exposing = Exposing }) ->
	Prefix = io_lib:format("-module ~s exposing ", [Name]),
	Suffix = ".\n\n",
	Contents = list_to_string(1, Exposing),
	[Prefix, Contents, Suffix];
data_to_string(#declaration_import{ name = Name, alias = MaybeAlias, exposing = Exposing }) ->
	Prefix = io_lib:format("-import ~s", [Name]),
	Alias = case MaybeAlias of
		undefined -> "";
		{ok, A} -> atom_to_binary(A, utf8)
	end,
	ExposingPrefix = " exposing ",
	Contents = list_to_string(1, Exposing),
	[Prefix, Alias, ExposingPrefix, Contents, ".\n"];
data_to_string(#declaration_alias{ name = Name, constraints = Constraints, args = Args, alias_of = Original}) ->
	NameStr = name_to_string(Name, true),
	ConstraintsSection = case Constraints of
		[] -> [];
		_ -> [" when ", record_to_string(1, Constraints)]
	end,
	ArgsList = lists:map(fun(V) ->
		[" ", to_string(V)]
	end, Args),
	TheOriginal = to_string(Original),
	["-alias", ConstraintsSection, " ", NameStr, ArgsList, " = ", TheOriginal, " .\n\n"];
data_to_string(#declaration_type{ name = Name, args = Args, constraints = Constraints, constructors = Constructors}) ->
	NameStr = name_to_string(Name, false),
	ConstraintSection = case Constraints of
		[] -> [];
		_ -> [" when ", collection_to_string(1, ${, $}, Constraints)]
	end,
	ArgsList = lists:map(fun(E) ->
		[" ", to_string(E)]
	end, Args),
	ConstructorSection = case Constructors of
		[] ->
			[];
		_ ->
			list_to_string(1, Constructors)
	end,
	["-type", ConstraintSection, " ", NameStr, ArgsList, ConstructorSection, ".\n\n"];
data_to_string(#declaration_class{ name = Name, constraints = Constraints, args = Args, members = Members}) ->
	NameStr = io_lib:format("~s", [Name]),
	ConstraintSection = case Constraints of
		[] -> [];
		_ -> [ "when ", list_to_string(1, Constraints)]
	end,
	ArgsList = lists:map(fun(E) -> [" ", to_string(E)] end, Args),
	MembersSection = list_to_string(1, Members),
	["-class", ConstraintSection, " ", NameStr, ArgsList, "[\n", MembersSection, "\n\t].\n\n"];
data_to_string(#class_member_definition{ name = Name, definition = Definition}) ->
	[name_to_string(Name, true), " : ", to_string(Definition)];
data_to_string(#class_member_default{ name = Name, args = Args, expression = Expression}) ->
	ArgsList = lists:map(fun(E) -> [" ", to_string(E)] end, Args),
	[name_to_string(Name, true), ArgsList, " =\n\t", to_string(Expression), "\n\t."];
data_to_string(#declaration_function{ name = Name, args = Args, bindings = Bindings, expression = Expression}) ->
	ArgsList = lists:map(fun(A) -> [" ", to_string(A)] end, Args),
	BindingsList = lists:map(fun(B) -> ["\n\t", to_string(B)] end, Bindings),
	FinalExpression = to_string(Expression),
	[ name_to_string(Name, true), ArgsList, " ->", BindingsList, "\n\t", FinalExpression, "\n\t.\n\n"];
data_to_string(#binding{ name = Name, expression = Expression}) ->
	[ name_to_string(Name, false), " = ", to_string(Expression), " ,"];
data_to_string(#declaration_spec{name = Name, type = Spec}) ->
	[ name_to_string(Name, true), " : ", to_string(Spec), ".\n"];
data_to_string(#infix_notation{ name = {name_symbol, Name}, weight = 1, assoc = left}) when is_atom(Name) ->
	[" ", atom_to_binary(Name, utf8), " "];
data_to_string(#infix_notation{ name = Name, weight = Weight, assoc = Assoc}) ->
	Arrow = case Assoc of
		left -> <<"»"/utf8>>;
		right -> <<"«"/utf8>>
	end,
	Arrows = lists:duplicate(Weight, Arrow),
	FunctionStr = name_to_string(Name, true),
	[" ", Arrows, FunctionStr, " "];
data_to_string(#infix_operation{ operator = Op, expression = Expression}) ->
	ExpressionString = case Expression of
		#expression_infix{} ->
			["( ", data_to_string(Expression), " )"];
		_ ->
			data_to_string(Expression)
	end,
	[data_to_string(Op), ExpressionString];
data_to_string(#expression_infix{ head = Head, infix_ops = Ops}) ->
	HeadString = case Head of
		#expression_infix{} ->
			["( ", data_to_string(Head),  " )"];
		_ ->
			data_to_string(Head)
	end,
	InfixOps = lists:map(fun data_to_string/1, Ops),
	[ HeadString | InfixOps];
data_to_string(#expression_call{ function = Function, args = Args}) ->
	FunctionStr = name_to_string(Function, true),
	ArgListStr = lists:map(fun(A) ->
		E = A#milang_ast.data,
		case E of
			#expression_call{ args = []} ->
				to_string(A);
			#expression_construct{ args = []} ->
				to_string(A);
			{literal_string, _} ->
				to_string(A);
			{literal_float, _} ->
				to_string(A);
			{literal_integer, _} ->
				to_string(A);
			(_) ->
				["( ", to_string(A), " )"]
		end
	end, Args),
	ArgsList = [ [" ", E] || E <- ArgListStr],
	[FunctionStr | ArgsList];
data_to_string(#expression_construct{ type = Type, args = Args }) ->
	TypeStr = name_to_string(Type, false),
	ArgListStr = lists:map(fun(Ast) ->
		E = Ast#milang_ast.data,
		case E of
			#expression_call{ args = []} ->
				[" ", to_string(Ast)];
			#expression_construct{ args = []} ->
				[" ", to_string(Ast)];
			{literal_string, _} ->
				[" ", to_string(Ast)];
			{literal_float, _} ->
				[" ", to_string(Ast)];
			{literal_integer, _} ->
				[" ", to_string(Ast)];
			_ ->
				["( ", to_string(Ast), " )"]
		end
	end, Args),
	[TypeStr | ArgListStr];
data_to_string({literal_string, _, String}) ->
	AsList = unicode:characters_to_list(String),
	EscapeFun = fun
		($\\) -> [$\\, $\\];
		($") -> [$\\, $"];
		(C) -> C
	end,
	EscapedList = lists:map(EscapeFun, AsList),
	[$", EscapedList, $"];
data_to_string({literal_float, _, Data}) ->
	io_lib:format("~w", Data);
data_to_string({literal_integer, _, Data}) ->
	integer_to_binary(Data);
data_to_string(#type_concrete{ name = Name, args = Args} ) ->
	ArgsFun = fun(A) ->
		Data = A#milang_ast.data,
		case Data of
			#type_concrete{args = []} ->
				[" ", to_string(A)];
			#type_concrete{} ->
				[" ( ", to_string(A), " )"];
			#type_function{} ->
				[" ( ", to_string(A), " )"];
			_ ->
				to_string(A)
		end
	end,
	ArgsStr = lists:map(ArgsFun, Args),
	NameStr = name_to_string(Name, false),
	[NameStr, ArgsStr];
data_to_string(#type_function{ args = [D]}) ->
	to_string(D);
data_to_string(#type_function{ args = Args}) ->
	ArgsStr = lists:map(fun(A) ->
		case A#milang_ast.data of
			#type_function{} ->
				["( ", to_string(A), " )"];
			_ ->
				to_string(A)
		end
	end, Args),
	lists:join(" -> ", ArgsStr);
data_to_string(#type_record{ fields = Fields}) ->
	FieldStrs = [ [$,,  to_string(F), "\n"] || F <- Fields ],
	["{\n", FieldStrs, "}"];
data_to_string(#type_record_field{ name = Name, type = Type}) ->
	NameStr = name_to_string(Name, true),
	TypeStr = to_string(Type),
	[NameStr, <<" = ">>, TypeStr];
data_to_string(#type_variable{ name = Name}) ->
	atom_to_binary(Name, utf8);
data_to_string(Wut) ->
	error({to_string, nyi, Wut}).
%
%-spec to_map(ast_node()) -> #{ location := location(), type := atom(), data := term(), doc := unicode:chardata()}.
%to_map(Node) ->
%	#milang_ast{ location = Loc, type = T, data = D, doc = Doc } = Node,
%	#{ location => Loc, type => T, data => D, doc => Doc}.
%
%-spec type(ast_node()) -> atom().
%type(Node) ->
%	Node#milang_ast.type.
%
%-spec location(ast_node()) -> location().
%location(Node) ->
%	Node#milang_ast.location.
%
%-spec doc(ast_node()) -> unicode:chardata().
%doc(Node) ->
%	Node#milang_ast.doc.
%
%-spec data(ast_node()) -> term().
%data(Node) ->
%	Node#milang_ast.data.
%
%-spec set_data(term(), ast_node()) -> ast_node().
%set_data(Data, Node) ->
%	Node#milang_ast{ data = Data }.
%
%-spec transform_data(fun((X) -> X), ast_node()) -> ast_node().
%transform_data(Transfomr, Node) ->
%	Data = Node#milang_ast.data,
%	Node#milang_ast{ data = Transfomr(Data) }.
%
%-spec set_doc(unicode:chardata(), ast_node()) -> ast_node().
%set_doc(Doc, Node) ->
%	Node#milang_ast{ doc = Doc}.
%
%-spec add_doc(unicode:chardata(), ast_node()) -> ast_node().
%add_doc(Doc, Node) ->
%	Node#milang_ast{ doc = [Node#milang_ast.doc | Doc]}.
%
%-spec pre_doc(unicode:chardata(), ast_node()) -> ast_node().
%pre_doc(Doc, Node) ->
%	Node#milang_ast{ doc = [ Doc | Node#milang_ast.doc] }.
%
%-spec constraint(unicode:unicode_binary(), type_concrete()) -> {atom(), type_concrete()}.
%constraint(Name, Entry) ->
%	{to_a(Name), Entry}.
%
%-spec constraints([{unicode:unicode_binary(), type_concrete()}]) -> constraints().
%constraints(List) ->
%	[ constraint(Name, Entry) || {Name, Entry} <- List].
%
%-spec declaration_module(location(), unicode:unicode_binary(), unicode:unicode_binary(), [ type_name_local() | function_name_local()]) -> declaration_module().
%declaration_module(Location, Doc, Name, Exposing) ->
%	Data = #{ name => to_a(Name), exposing => Exposing},
%	#milang_ast{ type = declaration_module, location = Location, doc = Doc, data = Data}.
%
%-spec declaration_import(location(), unicode:unicode_binary(), unicode:unicode_binary(), unicode:unicode_binary() | undefined, [ type_name_local() | function_name_local()]) -> declaration_import().
%declaration_import(Location, Doc, Name, Alias, Exposing) ->
%	Data = #{ name => to_a(Name), alias => to_a(Alias), exposing => Exposing},
%	#milang_ast{ type = declaration_import, location = Location, doc = Doc, data = Data}.
%
%-spec declaration_alias(location(), unicode:unicode_binary(), unicode:unicode_binary(), constraints(), type_concrete()) -> declaration_alias().
%declaration_alias(Location, Doc, Name, Constraints, Original) ->
%	Data = #{ name => to_a(Name), constraints => Constraints, Original => Original },
%	#milang_ast{ type = declaration_alias, location = Location, doc = Doc, data = Data}.
%
%-spec declaration_type(location(), unicode:unicode_binary(), unicode:unicode_binary(), [ variable() ], constraints(), [ type_data() ]) -> declaration_type().
%declaration_type(Location, Doc, Name, Args, Constraints, Constructors) ->
%	Data = #{ name => to_a(Name), args => Args, constraints => Constraints, constructors => Constructors},
%	#milang_ast{ type = declaration_type, location = Location, doc = Doc, data = Data}.
%
%-spec declaration_class(location(),unicode:unicode_binary(),unicode:unicode_binary(),constraints(),[ variable() ], [ class_member() ]) -> declaration_class().
%declaration_class(Location, Doc, Name, Constraints, Args, Members) ->
%	Data = #{ name => to_a(Name), constraints => Constraints, args => Args, members => Members},
%	#milang_ast{ type = declaration_class, location = Location, doc = Doc, data = Data}.
%
%-spec class_member_definition(location(), unicode:unicode_binary(), unicode:unicode_binary(), type_function()) -> class_member_definition().
%class_member_definition(Location, Doc, Name, Definition) ->
%	Data = #{ name => to_a(Name), definition => Definition},
%	#milang_ast{ type = class_member_definition, location = Location, doc = Doc, data = Data}.
%
%-spec class_member_default(location(), unicode:unicode_binary(), unicode:unicode_binary(), [ variable() ], expression()) -> class_member_default().
%class_member_default(Location, Doc, Name, Args, Expression) ->
%	Data = #{ name => to_a(Name), args => Args, expression => Expression },
%	#milang_ast{ type = class_member_default, location = Location, doc = Doc, data = Data}.
%
%-spec declaration_function(location(), unicode:unicode_binary(), unicode:unicode_binary(), [ variable() ], [ binding() ], expression()) -> declaration_function().
%declaration_function(Location, Doc, Name, Args, Bindings, Expression) ->
%	Data = #{ name => to_a(Name), args => Args, bindings => Bindings, expression => Expression },
%	#milang_ast{ type = declaration_function, location = Location, doc = Doc, data = Data}.
%
%-spec binding(location(), unicode:unicode_binary(), unicode:unicode_binary(), expression()) -> binding().
%binding(Location, Doc, Name, Expression) ->
%	Data = #{ name => to_a(Name), expression => Expression },
%	#milang_ast{ type = binding, location = Location, doc = Doc, data = Data}.
%
%-spec ast_node(location(), unicode:unicode_binary(), atom(), _Data) -> ast_node().
%ast_node(Location, Doc, Type, Data) ->
%	#milang_ast{ location = Location, doc = Doc, type = Type, data = Data}.
%
