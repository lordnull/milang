-module(milang_delex).

-include_lib("kernel/include/logger.hrl").

-export([string/1]).

string(Node) ->
	string(Node, 0).

string(Node, Depth) ->
	Type = milang_ast:type_simply(Node),
	string(Type, Node, Depth).

string(type, Node, Depth) ->
	Data = milang_ast:data(Node),
	Name = string(milang_ast_type:name(Data), Depth),
	Args = lists:map(fun(ArgNode) ->
		[" ", string(ArgNode)]
	end, milang_ast_type:args(Data)),
	Constraints = constraints_string(milang_ast_type:constraints(Data), Depth),
	Constructors = list(milang_ast_type:constructors(Data), Depth + 1),
	["type ", Name, Args, Constraints, " = ", Constructors, ".\n"];

string(identifier_type, Node, _Depth) ->
	{identifier_type, Data} = milang_ast:data(Node),
	identifier_string(Data);

string(identifier_bound, Node, _Depth) ->
	{identifier_bound, Data} = milang_ast:data(Node),
	identifier_string(Data);

string(identifier_ignored, Node, _Depth) ->
	{identifier_ignored, Data} = milang_ast:data(Node),
	identifier_string(Data);

string(constructor, Node, Depth) ->
	Data = milang_ast:data(Node),
	Name = milang_ast_constructor:name(Data),
	ArgNodes = milang_ast_constructor:args(Data),

	NameStr = string(Name, Depth),

	ArgsStrings = lists:map(fun(ArgNode) ->
		ArgString = string(ArgNode, Depth),
		maybe_parens(ArgString, ArgNode)
	end, ArgNodes),

	ArgsString = lists:join(" ", ArgsStrings),

	[NameStr, " ", ArgsString];

string(alias, Node, Depth) ->
	Data = milang_ast:data(Node),
	Name = milang_ast_alias:name(Data),
	ArgNodes = milang_ast_alias:args(Data),
	ConstraintNodes = milang_ast_alias:constraints(Data),
	OriginalNode = milang_ast_alias:original(Data),

	NameStr = string(Name, Depth),

	ArgsString = lists:map(fun(ArgNode) ->
		ArgString = string(ArgNode, Depth),
		[" ", ArgString]
	end, ArgNodes),

	ConstraintsString = constraints_string(ConstraintNodes, Depth),

	OriginalString = string(OriginalNode, Depth + 1),

	["alias ", NameStr, ArgsString, ConstraintsString, " = ", OriginalString, ".\n"];

string(concrete, Node, Depth) ->
	Data = milang_ast:data(Node),
	Name = milang_ast_concrete:name(Data),
	NameStr = string(Name, Depth),
	ArgNodes = milang_ast_concrete:args(Data),
	ArgsString = lists:map(fun(ArgNode) ->
		ArgStringRaw = string(ArgNode, Depth + 1),
		ArgString = maybe_parens(ArgStringRaw, ArgNode),
		[" ", ArgString]
	end, ArgNodes),
	[NameStr, ArgsString];

string(signature, Node, Depth) ->
	Data = milang_ast:data(Node),
	ArgNodes = milang_ast_signature:args(Data),
	ArgStrings = lists:map(fun(ArgNode) ->
		ArgString = string(ArgNode, Depth + 1),
		maybe_parens(ArgString, ArgNode)
	end, ArgNodes),
	lists:join(" -> ", ArgStrings);

string(spec, Node, Depth) ->
	Data = milang_ast:data(Node),

	NameStr = string(milang_ast_spec:name(Data)),

	ConstraintsString = constraints_string(milang_ast_spec:constraints(Data), Depth),

	TypeString = string(milang_ast_spec:type(Data), Depth + 1),

	["spec ", NameStr , ConstraintsString, " = ", TypeString, ".\n"];

string(undefined, Node, Depth) ->
	try milang_ast:data(Node) of
		D ->
			string(undefined, D, Depth)
	catch
		error:{badrecord, _} ->
			?LOG_ERROR("not yet implmented for delexing: ~p", [Node]),
			""
	end;

string(_, Node, _Depth) ->
	?LOG_ERROR("not yet implmented for delexing: ~p", [Node]),
	"".

identifier_string(#{ module := Module, local := Local}) ->
	[Module, $., Local];
identifier_string(String) when is_binary(String) ->
	String.

list(Nodes, Depth) ->
	Tabs = tabs(Depth),
	Entries = lists:map(fun(Node) ->
		["\n", Tabs, ", ", string(Node, Depth)]
	end, Nodes),
	["[", Entries, "\n", Tabs, "]"].

maybe_parens(String, Node) ->
	case should_parens(milang_ast:type_simply(Node), milang_ast:data(Node)) of
		true ->
			["( ", String, " )"];
		false ->
			String
	end.

should_parens(concrete, _) ->
	true;
should_parens(signature, _) ->
	true;
should_parens(_, _) ->
	false.

tabs(Depth) ->
	["\t" || _ <- lists:seq(1, Depth)].

constraints_string(undefined, _) ->
	[];

constraints_string(Nodes, Depth) ->
	Tabs = tabs(Depth),
	Constraints = lists:map(fun(ConstraintNode) ->
		["\n", Tabs, ", ", string(ConstraintNode)]
	end, Nodes),
	case Constraints of
		[] ->
			"";
		_ -> [" when {", Constraints, "\n", Tabs, "}\n"]
	end.
