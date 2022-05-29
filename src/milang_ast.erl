%%% @doc Module that holds the type / constructor for milang's ast forms.
-module(milang_ast).

-type location() :: {pos_integer(), pos_integer()}.
-type doc() :: unicode:unicode_binary().

-include("milang_ast.hrl").

-type constraints() :: [ {atom(), type_concrete() }].

-type declaration_module() :: #milang_ast{ type :: declaration_module, data :: #{ name := atom(), exposing := [ type_name_local() | function_name_local() ]}}.

-type declaration_import() :: #milang_ast{ type :: declaration_import, data :: #{ name := atom(), alias => atom(), exposing := [ type_name_local() | function_name_local()]}}.

-type declaration_alias() :: #milang_ast{ type :: declaration_alias, data :: #{ name := atom(), constraints := constraints(), args := [ variable() ], original := type_concrete()}}.

-type declaration_type() :: #milang_ast{ type :: declaration_type, data :: #{ name := atom(), args := [ atom() ], constraints := constraints(), constructors := [type_data()]}}.

-type declaration_class() :: #milang_ast{ type :: declaration_class, data :: #{ name := atom(), constraints := constraints(), args := [ atom() ], members := [class_member()]}}.

-type class_member() :: class_member_default() | class_member_definition().

-type class_member_definition() :: #milang_ast{ type :: class_member_definition, data :: #{ name := atom(), definition := type_function()}}.

-type class_member_default() :: #milang_ast{ type :: class_member_default, data :: #{ name := atom(), args := [ atom() ], expression := expression()}}.

-type declaration_function() :: #milang_ast{ type :: declaration_function, data :: #{ name := atom(), args := [ atom() ], bindings := [ binding() ], expression := expression()}}.

-type binding() :: #milang_ast{ type :: binding, data :: #{ name := atom(), expression := expression() }}.

-type declaration_spec() :: #milang_ast{ type :: declaration_spec, data :: #{ name := atom(), spec := type_function()}}.

-type type_name_local() :: #milang_ast{ type :: type_name_local, data :: atom()}.

-type type_name_remote() :: #milang_ast{ type :: type_name_remote, data :: #{ name := atom(), module := atom()}}.

-type type_name() :: type_name_local() | type_name_remote().

-type infix() :: infix_notation() | infix_symbol().

-type infix_notation() :: #milang_ast{ type :: infix_notation, data :: #{ assoc := 'left' | 'right', weight := pos_integer(), function := atom()}}.

-type infix_symbol() :: #milang_ast{ type :: infix_symbol, data :: atom()}.

-type function_name() :: function_name_local() | function_name_remote() | function_name_symbol().

-type function_name_local() :: #milang_ast{ type :: function_name_local, data :: atom()}.

-type function_name_remote() :: #milang_ast{ type :: function_name_remote, data :: #{ name := atom(), module := atom()}}.

-type function_name_symbol() :: #milang_ast{ type :: function_name_symbol, data :: atom()}.

-type expression() :: #milang_ast{ type :: expression, data :: #{ head := expression_primary(), infix_ops := [{infix(), expression_primary()}]}}.

-type expression_primary() :: expression_call() | expression_literal() | expression_parens().

-type expression_call() :: #milang_ast{ type :: expression_call, data :: #{ name := type_name() | function_name(), args := [ argument() ]}}.

-type expression_literal() :: literal_list() | literal_map() | literal_record() | literal_string() | literal_float() | literal_integer().

-type expression_parens() :: expression_primary().

-type argument() :: expression_literal() | function_name() | type_name() | expression_parens().

-type literal_list() :: #milang_ast{ type :: literal_list, data :: [expression_primary()]}.

-type literal_map() :: #milang_ast{ type :: literal_map, data :: [ literal_map_entry() ]}.

-type literal_map_entry() :: #milang_ast{ type :: literal_map_entry, data :: #{ key := expression_primary(), value := expression_primary()}}.

-type literal_record() :: #milang_ast{ type :: literal_record, data :: [ literal_record_entry() ]}.

-type literal_record_entry() :: #milang_ast{ type :: literal_record_entry, data :: [ {pos_integer() | atom(), expression_primary()}]}.

-type literal_string() :: #milang_ast{ type :: literal_string, data :: unicode:unicode_binary()}.

-type literal_float() :: #milang_ast{ type :: literal_float, data :: float()}.

-type literal_integer() :: #milang_ast{ type :: literal_integer, data :: integer()}.

-type type() :: type_concrete() | type_function() | type_record() | type_data().

-type type_data() :: #milang_ast{ type :: type_data, data :: #{ name := atom(), args := [type_data_arg()]}}.

-type type_data_arg() :: type_name() | variable() | type_record() | type_concrete().

-type type_function() :: #milang_ast{ type :: type_function, data :: [ type_function_arg() ]}.

-type type_function_arg() :: type_data() | type_record() | type_concrete() | variable().

-type type_record() :: #milang_ast{ type :: type_record, data :: [{pos_integer() | atom(), type_concrete()}]}.

-type type_concrete() :: type_data() | type_function() | type_record().

-type variable() :: #milang_ast{ type :: variable, data :: atom()}.

-type ast_node()
	:: constraints()
	|  declaration_module()
	|  declaration_import()
	|  declaration_alias()
	|  declaration_type()
	|  declaration_class()
	|  class_member()
	|  class_member_definition()
	|  class_member_default()
	|  declaration_function()
	|  binding()
	|  declaration_spec()
	|  type_name_local()
	|  type_name_remote()
	|  type_name()
	|  infix()
	|  infix_notation()
	|  infix_symbol()
	|  function_name()
	|  function_name_local()
	|  function_name_remote()
	|  function_name_symbol()
	|  expression()
	|  expression_primary()
	|  expression_call()
	|  expression_literal()
	|  expression_parens()
	|  argument()
	|  literal_list()
	|  literal_map()
	|  literal_map_entry()
	|  literal_record()
	|  literal_record_entry()
	|  literal_string()
	|  literal_float()
	|  literal_integer()
	|  type()
	|  type_data()
	|  type_data_arg()
	|  type_function()
	|  type_function_arg()
	|  type_record()
	|  type_concrete()
	|  variable()
	.

-export_type(
	[doc/0
	,location/0
	,ast_node/0
	,constraints/0
	,declaration_module/0
	,declaration_import/0
	,declaration_alias/0
	,declaration_type/0
	,declaration_class/0
	,class_member/0
	,class_member_definition/0
	,class_member_default/0
	,declaration_function/0
	,binding/0
	,declaration_spec/0
	,type_name_local/0
	,type_name_remote/0
	,type_name/0
	,infix/0
	,infix_notation/0
	,infix_symbol/0
	,function_name/0
	,function_name_local/0
	,function_name_remote/0
	,function_name_symbol/0
	,expression/0
	,expression_primary/0
	,expression_call/0
	,expression_literal/0
	,expression_parens/0
	,argument/0
	,literal_list/0
	,literal_map/0
	,literal_map_entry/0
	,literal_record/0
	,literal_record_entry/0
	,literal_string/0
	,literal_float/0
	,literal_integer/0
	,type/0
	,type_data/0
	,type_data_arg/0
	,type_function/0
	,type_function_arg/0
	,type_record/0
	,type_concrete/0
	,variable/0
	]).

-export(
	[ast_node/4
	,constraint/2
	,constraints/1
	,declaration_module/4
	,declaration_import/5
	,declaration_alias/5
	,declaration_type/6
	,declaration_class/6
	,class_member_definition/4
	,class_member_default/5
	,declaration_function/6
	,binding/4
	,local_to_remote_name/2
	]).
-export([set_doc/2, to_map/1, to_string/1, type/1, location/1, doc/1, data/1]).

local_to_remote_name(Remote, #milang_ast{ type = function_name_local } = Node) ->
	LocalName = Node#milang_ast.data,
	NewData = #{ name => LocalName, module => Remote },
	Node#milang_ast{ type = function_name_remote, data = NewData};
local_to_remote_name(Remote, #milang_ast{ type = function_name_symbol } = Node) ->
	LocalName = Node#milang_ast.data,
	NewData = #{ name => LocalName, module => Remote },
	Node#milang_ast{ type = function_name_remote, data = NewData};
local_to_remote_name(Remote, #milang_ast{ type = type_name_local } = Node) ->
	LocalName = Node#milang_ast.data,
	NewData = #{ name => LocalName, module => Remote },
	Node#milang_ast{ type = type_name_remote, data = NewData};
local_to_remote_name(_Remote, Node) ->
	Node.

to_a(B) -> binary_to_atom(B, utf8).

to_string(Node) ->
	io:format("Das node to_string: ~p~n", [Node]),
	to_string(milang_ast:type(Node), milang_ast:doc(Node), milang_ast:data(Node)).

to_string(constraints, _, []) ->
	[];
to_string(constraints, _, Constraints) ->
	Prefix = <<" when {\n">>,
	Contents = lists:map(fun({Key, Value}) ->
		io_lib:format("    , ~s = ~s\n", [Key, to_string(Value)])
	end, Constraints),
	Suffix = "\n}\n",
	[Prefix, Contents, Suffix];
to_string(declaration_module, _Doc, Data) ->
	#{ name := Name, exposing := Exposing } = Data,
	Prefix = io_lib:format("-module ~s exposing [", [Name]),
	Suffix = "\n\t].\n\n",
	Contents = lists:map(fun(E) ->
		["\n\t,", to_string(E)]
	end, Exposing),
	[Prefix, Contents, Suffix];
to_string(declaration_import, _, Data) ->
	#{ name := Name, alias := Alias, exposing := Exposing } = Data,
	Prefix = io_lib:format("-import ~s", [Name]),
	Alias = case Alias of
		undefined -> "";
		_ -> io_lib:format(" as ~s", [Alias])
	end,
	ExposingPrefix = " exposing [\n",
	Contents = lists:map(fun(E) ->
		["\n\t, ", to_string(E)]
	end, Exposing),
	Suffix = "\n\t].\n",
	[Prefix, Alias, ExposingPrefix, Contents, Suffix, "\n"];
to_string(declaration_alias, _, Data) ->
	#{ name := Name, constraints := Constraints, args := Args, original := Original} = Data,
	NameStr = to_string(Name),
	ConstraintsSection = to_string(constraints, "", Constraints),
	ArgsList = lists:map(fun(V) ->
		[" ", to_string(V)]
	end, Args),
	TheOriginal = to_string(Original),
	["-alias", ConstraintsSection, " ", NameStr, ArgsList, " = ", TheOriginal, " .\n\n"];
to_string(declaration_type, _, Data) ->
	#{ name := Name, args := Args, constraints := Constraints, constructors := Constructors } = Data,
	NameStr = to_string(Name),
	ConstraintSection = to_string(constraints, "", Constraints),
	ArgsList = lists:map(fun(E) ->
		[" ", to_string(E)]
	end, Args),
	ConstructorSection = case Constructors of
		[] ->
			[];
		_ ->
			[$[, lists:map(fun(E) ->
				["\n\t, ", to_string(E)]
			end, Constructors), "\n\t]"]
	end,
	["-type", ConstraintSection, " ", NameStr, ArgsList, ConstructorSection, ".\n\n"];
to_string(declaration_class, _, Data) ->
	#{ name := Name, constraints := Constraints, args := Args, members := Members } = Data,
	NameStr = io_lib:format("~s", [Name]),
	ConstraintSection = to_string(constraints, "", Constraints),
	ArgsList = lists:map(fun(E) -> [" ", to_string(E)] end, Args),
	MembersSection = lists:map(fun(E) ->
		["\n\t, ", to_string(E)]
	end, Members),
	["-class", ConstraintSection, " ", NameStr, ArgsList, "[\n", MembersSection, "\n\t].\n\n"];
to_string(class_member_definition, _, Data) ->
	#{ name := Name, definition := Def } = Data,
	[io_lib:format("~s ", [Name]), " : ", to_string(Def)];
to_string(class_member_default, _, Data) ->
	#{ name := Name, args := Args, expression := Expression } = Data,
	ArgsList = lists:map(fun(E) -> [" ", to_string(E)] end, Args),
	[io_lib:format("~s ", [Name]), ArgsList, " =\n\t", to_string(Expression), "\n\t."];
to_string(declaration_function, _, Data) ->
	#{ name := Name, args := Args, bidings := Bindings, expression := Expression } = Data,
	ArgsList = lists:map(fun(A) -> [" ", to_string(A)] end, Args),
	BindingsList = lists:map(fun(B) -> ["\n\t", to_string(B)] end, Bindings),
	FinalExpression = to_string(Expression),
	[ io_lib:format("~s", [Name]), ArgsList, " ->", BindingsList, "\n\t", FinalExpression, "\n\t.\n\n"];
to_string(binding, _, Data) ->
	#{ name := Name, expression := Expression } = Data,
	[ io_lib:format("~s", [Name]), " = ", to_string(Expression), " ,"];
to_string(declaration_spec, _, Data) ->
	#{ name := Name, spec := Spec } = Data,
	[ to_string(Name), " : ", to_string(Spec), ".\n"];
to_string(type_name_local, _, Name) ->
	atom_to_binary(Name);
to_string(type_name_remote, _, Data) ->
	#{ name := Name, module := Module } = Data,
	io_lib:format("~s.~s", [Module, Name]);
to_string(infix_notation, _, Data) ->
	#{ assoc := Assoc, weight := Weight, function := Func} = Data,
	Arrow = case Assoc of
		left -> <<"»"/utf8>>;
		right -> <<"«"/utf8>>
	end,
	Arrows = [ Arrow || _ <- lists:seq(1, Weight) ],
	FunctionStr = to_string(Func),
	[" ", Arrows, FunctionStr];
to_string(infix_symbol, _, Name) ->
	atom_to_binary(Name);
to_string(function_name_local, _, Name) ->
	atom_to_binary(Name);
to_string(function_name_remote, _, Data) ->
	#{ name := Local, module := Module } = Data,
	io_lib:format("~s.~s", [Module, Local]);
to_string(function_name_symbol, _, Data) ->
	io_lib:format("'~s'", [Data]);
to_string(expression, _, Data) ->
	#{ head := Head, infix_ops := InfixOps } = Data,
	RawHeadStr = to_string(Head),
	FullHeadStr = case milang_ast:type(Head) of
		expression ->
			[$(, " ", RawHeadStr, " ", $)];
		_ ->
			RawHeadStr
	end,
	InFixing = lists:map(fun({InFix, InFixExpr}) ->
		RawInFixExprStr = to_string(InFixExpr),
		InFixExprStr = case milang_ast:type(InFixExpr) of
			expression ->
				[$(, " ", RawInFixExprStr, " ", $)];
			_ ->
				RawInFixExprStr
		end,
		["\n\t ", to_string(InFix), " ", InFixExprStr]
	end, InfixOps),
	[FullHeadStr, InFixing];
to_string(expression_call, _, Data) ->
	#{ name := Name, args := Args} = Data,
	ArgsList = lists:map(fun(A) ->
		[" ", to_string(A)]
	end, Args),
	[ to_string(Name), ArgsList];
to_string(literal_list, _, Exprs) ->
	["[\n", lists:map(fun(E) -> [",", to_string(E), "\n"] end, Exprs), "]"];
to_string(literal_map, _, Entries) ->
	KVEntries = lists:map(fun(E) ->
		[", ", to_string(E)]
	end, Entries),
	["#{\n", KVEntries, "}#"];
to_string(literal_map_entry, _, Data) ->
	#{ key := KeyExpr, value := ValueExpr } = Data,
	KeyExprStr = case milang_ast:type(KeyExpr) of
		expression ->
			["( ", to_string(KeyExpr), " )"];
		_ ->
			to_string(KeyExpr)
	end,
	ValueExprStr = case milang_ast:type(ValueExpr) of
		expression ->
			["( ", to_string(ValueExpr), " )"];
		_ ->
			to_string(ValueExpr)
	end,
	[", ", KeyExprStr, " = ", ValueExprStr, "\n"];
to_string(literal_record, _, Data) ->
	KVEntries = lists:map(fun(E) ->
		[", ", to_string(E)]
	end, Data),
	["{\n", KVEntries, "}"];
to_string(literal_record_entry, _, {Key, ValueExpr}) ->
	KeyStr = if
		is_atom(Key) ->
			atom_to_binary(Key);
		is_integer(Key) ->
			integer_to_binary(Key)
	end,
	ValueStr = case milang_ast:type(ValueExpr) of
		expression ->
			["( ", to_string(ValueExpr), " )"];
		_ ->
			to_string(ValueExpr)
	end,
	[", ", KeyStr, " = ", ValueStr, "\n"];
to_string(literal_string, _, String) ->
	AsList = unicode:characters_to_list(String),
	EscapeFun = fun
		($\\) -> [$\\, $\\];
		($") -> [$\\, $"];
		(C) -> C
	end,
	EscapedList = lists:map(EscapeFun, AsList),
	[$", EscapedList, $"];
to_string(literal_float, _, Data) ->
	io_lib:format("~w", Data);
to_string(literal_integer, _, Data) ->
	integer_to_binary(Data);
to_string(type_data, _, Data) ->
	#{ name := Name, args := Args } = Data,
	ArgsFun = fun(A) ->
		AType = A#milang_ast.type,
		AData = A#milang_ast.data,
		case {AType, AData} of
			{type_data, #{ args := []}} ->
				["( ", to_string(A), " )"];
			{type_record, _} ->
				to_string(A);
			{variable, _} ->
				to_string(A);
			{type_name_remote, _} ->
				to_string(A);
			{type_name_local, _} ->
				to_string(A);
			_ ->
				["( ", to_string(A), " )"]
		end
	end,
	ArgsUnJoined = lists:map(ArgsFun, Args),
	ArgsList = lists:join(" ", ArgsUnJoined),
	[to_string(Name), " ", ArgsList];
to_string(type_function, _, Data) ->
	MapperFun = fun(A) ->
		case milang_ast:type(A) of
			type_data ->
				to_string(A);
			type_record ->
				to_string(A);
			variable ->
				to_string(A);
			_ ->
				["( ", to_string(A), " )"]
		end
	end,
	Mapped = lists:map(MapperFun, Data),
	lists:join(" -> ", Mapped);
to_string(type_record, _, Data) ->
	Mapper = fun(Node) ->
		["\n", to_string(Node)]
	end,
	Mapped = lists:map(Mapper, Data),
	["{", Mapped, "}"];
to_string(record_field, _, Data) ->
	#{ key := Key, value := Value } = Data,
	KeyString = if
		is_atom(Key) ->
			atom_to_binary(Key);
		is_integer(Key) ->
			integer_to_binary(Key)
	end,
	[", ", KeyString, " = ", to_string(Value) ];
to_string(variable, _, Name) ->
	atom_to_binary(Name);
to_string(Wut, _, _) ->
	error({to_string, nyi, Wut}).

-spec to_map(ast_node()) -> #{ location := location(), type := atom(), data := term(), doc := unicode:chardata()}.
to_map(Node) ->
	#milang_ast{ location = Loc, type = T, data = D, doc = Doc } = Node,
	#{ location => Loc, type => T, data => D, doc => Doc}.

-spec type(ast_node()) -> atom().
type(Node) ->
	Node#milang_ast.type.

-spec location(ast_node()) -> location().
location(Node) ->
	Node#milang_ast.location.

-spec doc(ast_node()) -> unicode:chardata().
doc(Node) ->
	Node#milang_ast.doc.

-spec data(ast_node()) -> term().
data(Node) ->
	Node#milang_ast.data.

-spec set_doc(unicode:chardata(), ast_node()) -> ast_node().
set_doc(Doc, Node) ->
	Node#milang_ast{ doc = unicode:characters_to_binary(Doc)}.

-spec constraint(unicode:unicode_binary(), type_concrete()) -> {atom(), type_concrete()}.
constraint(Name, Entry) ->
	{to_a(Name), Entry}.

-spec constraints([{unicode:unicode_binary(), type_concrete()}]) -> constraints().
constraints(List) ->
	[ constraint(Name, Entry) || {Name, Entry} <- List].

-spec declaration_module(location(), unicode:unicode_binary(), unicode:unicode_binary(), [ type_name_local() | function_name_local()]) -> declaration_module().
declaration_module(Location, Doc, Name, Exposing) ->
	Data = #{ name => to_a(Name), exposing => Exposing},
	#milang_ast{ type = declaration_module, location = Location, doc = Doc, data = Data}.

-spec declaration_import(location(), unicode:unicode_binary(), unicode:unicode_binary(), unicode:unicode_binary() | undefined, [ type_name_local() | function_name_local()]) -> declaration_import().
declaration_import(Location, Doc, Name, Alias, Exposing) ->
	Data = #{ name => to_a(Name), alias => to_a(Alias), exposing => Exposing},
	#milang_ast{ type = declaration_import, location = Location, doc = Doc, data = Data}.

-spec declaration_alias(location(), unicode:unicode_binary(), unicode:unicode_binary(), constraints(), type_concrete()) -> declaration_alias().
declaration_alias(Location, Doc, Name, Constraints, Original) ->
	Data = #{ name => to_a(Name), constraints => Constraints, Original => Original },
	#milang_ast{ type = declaration_alias, location = Location, doc = Doc, data = Data}.

-spec declaration_type(location(), unicode:unicode_binary(), unicode:unicode_binary(), [ variable() ], constraints(), [ type_data() ]) -> declaration_type().
declaration_type(Location, Doc, Name, Args, Constraints, Constructors) ->
	Data = #{ name => to_a(Name), args => Args, constraints => Constraints, constructors => Constructors},
	#milang_ast{ type = declaration_type, location = Location, doc = Doc, data = Data}.

-spec declaration_class(location(),unicode:unicode_binary(),unicode:unicode_binary(),constraints(),[ variable() ], [ class_member() ]) -> declaration_class().
declaration_class(Location, Doc, Name, Constraints, Args, Members) ->
	Data = #{ name => to_a(Name), constraints => Constraints, args => Args, members => Members},
	#milang_ast{ type = declaration_class, location = Location, doc = Doc, data = Data}.

-spec class_member_definition(location(), unicode:unicode_binary(), unicode:unicode_binary(), type_function()) -> class_member_definition().
class_member_definition(Location, Doc, Name, Definition) ->
	Data = #{ name => to_a(Name), definition => Definition},
	#milang_ast{ type = class_member_definition, location = Location, doc = Doc, data = Data}.

-spec class_member_default(location(), unicode:unicode_binary(), unicode:unicode_binary(), [ variable() ], expression()) -> class_member_default().
class_member_default(Location, Doc, Name, Args, Expression) ->
	Data = #{ name => to_a(Name), args => Args, expression => Expression },
	#milang_ast{ type = class_member_default, location = Location, doc = Doc, data = Data}.

-spec declaration_function(location(), unicode:unicode_binary(), unicode:unicode_binary(), [ variable() ], [ binding() ], expression()) -> declaration_function().
declaration_function(Location, Doc, Name, Args, Bindings, Expression) ->
	Data = #{ name => to_a(Name), args => Args, bindings => Bindings, expression => Expression },
	#milang_ast{ type = declaration_function, location = Location, doc = Doc, data = Data}.

-spec binding(location(), unicode:unicode_binary(), unicode:unicode_binary(), expression()) -> binding().
binding(Location, Doc, Name, Expression) ->
	Data = #{ name => to_a(Name), expression => Expression },
	#milang_ast{ type = binding, location = Location, doc = Doc, data = Data}.

-spec ast_node(location(), unicode:unicode_binary(), atom(), _Data) -> ast_node().
ast_node(Location, Doc, Type, Data) ->
	#milang_ast{ location = Location, doc = Doc, type = Type, data = Data}.
