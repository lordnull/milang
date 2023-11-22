-module(milang_ast_match_bind).

-record(?MODULE,
	{ match
	}).

-type match_record_field(SubMatch)
	:: milang_ast_identifier:ast_node()
	|  {milang_ast_identifier:ast_node(), SubMatch}
	.

-type match_data()
%	:: {literal_string, unicode:chardata()}
%	|  {literal_integer, integer()}
%	|  {literal_float, float()}
	:: {literal, milang_ast_literal:ast_node()}
	|  {match_list, [ match_data() ]}
	|  {match_list_head, [ match_data()], milang_ast_identifier:ast_node()}
	% the parser will never do a match_bind, and always to match_type.
	% a match_type with no args could be a bind. That's for the type system/
	% linter to figure out.
	|  {match_type, milang_ast_identifier:ast_node(), [ match_data() ]}
	|  {match_bind, milang_ast_identifier:ast_node()}
	|  {match_record, [ match_record_field(match_data()) ]}
	|  {match_record_head, [ match_record_field(match_data())], milang_ast_identifier:ast_node()}
	.
-type data() :: #?MODULE{ match :: match_data() }.
-type ast_node() :: milang_ast:ast_node(data()).


-export_type([data/0, ast_node/0, match_data/0]).

-export(
	[ new/1
	, 'match'/1, 'match'/2
	, to_string/2
	]).

new(V0) -> #?MODULE{'match'=V0}.

'match'(R) -> R#?MODULE.'match'.
'match'(V,R) -> R#?MODULE{ 'match' = V }.

to_string(Data, Depth) ->
	MatchString = match_to_string(match(Data)),
	[ lists_more:repeat(Depth, "\t")
	, MatchString
	].

match_to_string({literal, Literal}) ->
	milang_ast_literal:to_string(Literal);
match_to_string({match_list, []}) ->
	"[]";
match_to_string({match_list, ListElements}) ->
	NoCommaElements = lists:map(fun(Node) ->
		milang_ast:to_string(Node, 0, fun ?MODULE:to_string/2)
	end, ListElements),
	WithCommas = lists:map(fun(Str) ->
		[" , ", Str]
	end, NoCommaElements),
	[$[, WithCommas, " ]"];
match_to_string({match_list_head, Heads, TailBind}) ->
	% because I'm lazy.
	[Opener, Middle, End] = match_to_string({match_list, Heads}),
	[Opener, Middle, " ,, ", milang_ast_identifier:to_string(TailBind), End];
match_to_string({match_type, TypeName, []}) ->
	milang_ast_identifier:to_string(TypeName);
match_to_string({match_type, TypeName, MatchArgs}) ->
	ArgsString = lists:map(fun(Arg) ->
		[" ", match_to_string(Arg)]
	end, MatchArgs),
	[milang_ast_identifier:to_string(TypeName), ArgsString];
match_to_string({match_bind, Bind}) ->
	milang_ast_identifier:to_string(Bind).




