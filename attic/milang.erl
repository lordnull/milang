-module(milang).
-export([parse/1,file/1]).
-define(p_anything,true).
-define(p_charclass,true).
-define(p_choose,true).
-define(p_label,true).
-define(p_not,true).
-define(p_one_or_more,true).
-define(p_optional,true).
-define(p_scan,true).
-define(p_seq,true).
-define(p_string,true).
-define(p_zero_or_more,true).



simple_idx({{line, Line}, {column, Col}}) -> {Line, Col}.

log_rule(RuleName, Node, Message, Args) ->
	log_rule(RuleName, Node, Node, Message, Args).

log_rule(RuleName, Node, Node, Message, Args) ->
	ok = io:format("~s ~s: ~p~n", [RuleName, io_lib:format(Message, Args), Node]),
	Node;
log_rule(RuleName, InNode, OutNode, Message, Args) ->
	ok = io:format("~s ~s: ~p -> ~p~n", [RuleName, io_lib:format(Message, Args), InNode, OutNode]),
	OutNode.

log_rule(RuleName, Node) ->
	log_rule(RuleName, Node, Node, "", []).


-spec file(file:name()) -> any().
file(Filename) -> case file:read_file(Filename) of {ok,Bin} -> parse(Bin); Err -> Err end.

-spec parse(binary() | list()) -> any().
parse(List) when is_list(List) -> parse(unicode:characters_to_binary(List));
parse(Input) when is_binary(Input) ->
  _ = setup_memo(),
  Result = case 'module'(Input,{{line,1},{column,1}}) of
             {AST, <<>>, _Index} -> AST;
             Any -> Any
           end,
  release_memo(), Result.

-spec 'module'(input(), index()) -> parse_result().
'module'(Input, Index) ->
  p(Input, Index, 'module', fun(I,D) -> (p_seq([p_optional(fun 'space'/2), p_zero_or_more(p_seq([fun 'declaration'/2, p_optional(fun 'space'/2)]))]))(I,D) end, fun(Node, _Idx) ->
	[_, DeclarationsWithSpaces] = Node,
	[ Declaration || [Declaration, _] <- DeclarationsWithSpaces]
 end).

-spec 'declaration'(input(), index()) -> parse_result().
'declaration'(Input, Index) ->
  p(Input, Index, 'declaration', fun(I,D) -> (p_choose([fun 'module_declaration'/2, fun 'import_declaration'/2, fun 'alias_declaration'/2, fun 'type_declaration'/2, fun 'class_declaration'/2, fun 'spec_declaration'/2, fun 'function_declaration'/2]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'module_declaration'(input(), index()) -> parse_result().
'module_declaration'(Input, Index) ->
  p(Input, Index, 'module_declaration', fun(I,D) -> (p_seq([p_string(<<"-module">>), fun 'space'/2, p_label('modName', fun 'module_name'/2), fun 'space'/2, p_string(<<"exposing">>), p_optional(fun 'space'/2), p_label('exposing', p_optional(fun 'exposing_list'/2)), p_optional(fun 'space'/2), fun 'declaration_seperator'/2]))(I,D) end, fun(Node, Idx) ->
	Name = proplists:get_value(modName, Node),
	Exports = proplists:get_value(exposing, Node, []),
	{module, simple_idx(Idx), Name, Exports}
 end).

-spec 'import_declaration'(input(), index()) -> parse_result().
'import_declaration'(Input, Index) ->
  p(Input, Index, 'import_declaration', fun(I,D) -> (p_seq([p_string(<<"-import">>), fun 'space'/2, p_label('mod', fun 'type_name'/2), p_optional(p_seq([fun 'space'/2, p_string(<<"as">>), fun 'space'/2, p_label('alias', fun 'upcase_name'/2)])), p_optional(p_seq([fun 'space'/2, p_string(<<"exposing">>), p_optional(fun 'space'/2), p_label('exposing', fun 'exposing_list'/2)])), p_optional(fun 'space'/2), fun 'declaration_seperator'/2]))(I,D) end, fun(Node, Idx) ->
	Mod = proplists:get_value(mod, Node),
	Alias = proplists:get_value(alias, Node),
	Exposing = proplists:get_value(exposing, Node, []),
	{import, simple_idx(Idx), Mod, Alias, Exposing}
 end).

-spec 'alias_declaration'(input(), index()) -> parse_result().
'alias_declaration'(Input, Index) ->
  p(Input, Index, 'alias_declaration', fun(I,D) -> (p_seq([p_string(<<"-alias">>), p_label('constraints', p_optional(p_seq([fun 'space'/2, fun 'constraint_section'/2]))), fun 'space'/2, p_label('name', fun 'upcase_name'/2), p_label('args', p_zero_or_more(p_seq([fun 'space'/2, fun 'downcase_name'/2]))), p_optional(fun 'space'/2), p_string(<<"=">>), p_optional(fun 'space'/2), p_label('actual', fun 'literal_type'/2), p_optional(fun 'space'/2), p_string(<<".">>)]))(I,D) end, fun(Node, Idx) ->
	NewName = proplists:get_value(name, Node),
	Actual = proplists:get_value(actual, Node),
	Constraints = proplists:get_value(constraints, Node, []),
	DirtyArgs = proplists:get_value(args, Node, []),
	Args = [A || [_, A] <- DirtyArgs],
	{alias, simple_idx(Idx), NewName, Args, Constraints, Actual}
 end).

-spec 'type_declaration'(input(), index()) -> parse_result().
'type_declaration'(Input, Index) ->
  p(Input, Index, 'type_declaration', fun(I,D) -> (p_seq([p_string(<<"-type">>), p_label('constraints', p_optional(p_seq([fun 'space'/2, fun 'constraint_section'/2]))), fun 'space'/2, p_label('name', fun 'upcase_name'/2), p_label('args', p_zero_or_more(p_seq([fun 'space'/2, fun 'downcase_name'/2]))), p_label('constructors', p_one_or_more(p_seq([p_optional(fun 'space'/2), fun 'type_constructor'/2]))), p_optional(fun 'space'/2), p_string(<<".">>)]))(I,D) end, fun(Node, Idx) ->
	Name = proplists:get_value(name, Node),
	Constraints = proplists:get_value(constraints, Node, []),
	DirtyArgs = proplists:get_value(args, Node, []),
	Args = [A || [_, A] <- DirtyArgs],
	DirtyConstructors = proplists:get_value(constructors, Node, []),
	Constructors = [C || [_, C] <- DirtyConstructors],
	{type, simple_idx(Idx), Name, Constraints, Args, Constructors}
 end).

-spec 'type_constructor'(input(), index()) -> parse_result().
'type_constructor'(Input, Index) ->
  p(Input, Index, 'type_constructor', fun(I,D) -> (p_seq([p_string(<<"|">>), p_optional(fun 'space'/2), p_label('name', fun 'upcase_name'/2), p_label('args', p_zero_or_more(p_seq([fun 'space'/2, p_choose([fun 'type_primary'/2, fun 'literal_type_record'/2])])))]))(I,D) end, fun(Node, Idx) ->
	Name = proplists:get_value(name, Node),
	DirtyArgs = proplists:get_value(args, Node, []),
	Args = [A || [_, A] <- DirtyArgs],
	{type_constructor, simple_idx(Idx), Name, Args}
 end).

-spec 'literal_type'(input(), index()) -> parse_result().
'literal_type'(Input, Index) ->
  p(Input, Index, 'literal_type', fun(I,D) -> (p_choose([fun 'type_top'/2, fun 'literal_type_record'/2]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'literal_type_record'(input(), index()) -> parse_result().
'literal_type_record'(Input, Index) ->
  p(Input, Index, 'literal_type_record', fun(I,D) -> (p_seq([p_one_or_more(p_seq([p_string(<<";">>), p_optional(fun 'space'/2), fun 'downcase_name'/2, p_optional(fun 'space'/2), p_string(<<":=">>), p_optional(fun 'space'/2), fun 'literal_type'/2, p_optional(fun 'space'/2)])), p_string(<<".">>)]))(I,D) end, fun(Node, Idx) ->
	Fields = [{Name, Type} || [_, _, Name, _, _, _, Type, _] <- Node],
	{literal_type_record, simple_idx(Idx), Fields}
 end).

-spec 'constraint_section'(input(), index()) -> parse_result().
'constraint_section'(Input, Index) ->
  p(Input, Index, 'constraint_section', fun(I,D) -> (p_seq([p_string(<<"{">>), p_one_or_more(p_seq([p_optional(fun 'space'/2), fun 'constraint_item'/2])), p_optional(fun 'space'/2), p_string(<<"}">>)]))(I,D) end, fun(Node, _Idx) ->
	[_, DirtyItems, _, _] = Node,
	[I || [_, I] <- DirtyItems]
 end).

-spec 'constraint_item'(input(), index()) -> parse_result().
'constraint_item'(Input, Index) ->
  p(Input, Index, 'constraint_item', fun(I,D) -> (p_seq([p_string(<<",">>), p_optional(fun 'space'/2), fun 'downcase_name'/2, p_optional(fun 'space'/2), p_string(<<":=">>), p_optional(fun 'space'/2), fun 'type_concrete'/2]))(I,D) end, fun(Node, _Idx) ->
	[{Variable, Constraint} || [_, _, Variable, _, _, _, Constraint] <- Node]
 end).

-spec 'class_declaration'(input(), index()) -> parse_result().
'class_declaration'(Input, Index) ->
  p(Input, Index, 'class_declaration', fun(I,D) -> (p_seq([p_string(<<"-class">>), p_optional(p_seq([p_optional(fun 'space'/2), fun 'constraint_section'/2])), fun 'space'/2, p_label('name', fun 'upcase_name'/2), p_label('args', p_one_or_more(p_seq([fun 'space'/2, fun 'downcase_name'/2]))), p_optional(fun 'space'/2), p_one_or_more(p_seq([fun 'class_member'/2, p_optional(fun 'space'/2)])), p_string(<<".">>)]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'class_member'(input(), index()) -> parse_result().
'class_member'(Input, Index) ->
  p(Input, Index, 'class_member', fun(I,D) -> (p_seq([p_string(<<"|">>), p_optional(fun 'space'/2), p_choose([fun 'class_definition'/2, fun 'class_default'/2])]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'class_definition'(input(), index()) -> parse_result().
'class_definition'(Input, Index) ->
  p(Input, Index, 'class_definition', fun(I,D) -> (p_seq([fun 'bindable_function_name'/2, p_optional(fun 'space'/2), p_string(<<":">>), p_optional(fun 'space'/2), fun 'type_top'/2]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'class_default'(input(), index()) -> parse_result().
'class_default'(Input, Index) ->
  p(Input, Index, 'class_default', fun(I,D) -> (p_seq([fun 'bindable_function_name'/2, p_zero_or_more(p_seq([fun 'space'/2, fun 'downcase_name'/2])), p_optional(fun 'space'/2), p_string(<<":=">>), p_optional(fun 'space'/2), fun 'expression'/2, p_optional(fun 'space'/2), p_string(<<".">>)]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'spec_declaration'(input(), index()) -> parse_result().
'spec_declaration'(Input, Index) ->
  p(Input, Index, 'spec_declaration', fun(I,D) -> (p_seq([p_label('name', fun 'bindable_function_name'/2), p_optional(fun 'space'/2), p_string(<<":">>), p_optional(fun 'space'/2), p_label('type_spec', fun 'type_top'/2), p_optional(fun 'space'/2), p_string(<<".">>)]))(I,D) end, fun(Node, Idx) ->
	Name = proplists:get_value(name, Node),
	Spec = proplists:get_value(type_spec, Node),
	{specification, simple_idx(Idx), Name, Spec}
 end).

-spec 'function_declaration'(input(), index()) -> parse_result().
'function_declaration'(Input, Index) ->
  p(Input, Index, 'function_declaration', fun(I,D) -> (p_seq([p_label('name', fun 'bindable_function_name'/2), p_zero_or_more(p_seq([fun 'space'/2, fun 'variable'/2])), p_optional(fun 'space'/2), p_string(<<":=">>), p_optional(fun 'space'/2), fun 'expression'/2, p_optional(fun 'space'/2), p_string(<<".">>)]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'exposing_list'(input(), index()) -> parse_result().
'exposing_list'(Input, Index) ->
  p(Input, Index, 'exposing_list', fun(I,D) -> (p_seq([p_string(<<"[">>), p_zero_or_more(p_seq([p_optional(fun 'space'/2), fun 'exposing_item'/2])), p_optional(fun 'space'/2), p_string(<<"]">>)]))(I,D) end, fun(Node, Idx) ->
	[_, DirtyElements, _, _] = Node,
	Elements = [E || [_, E] <- DirtyElements],
	{exposing, simple_idx(Idx), Elements}
 end).

-spec 'exposing_item'(input(), index()) -> parse_result().
'exposing_item'(Input, Index) ->
  p(Input, Index, 'exposing_item', fun(I,D) -> (p_seq([p_string(<<",">>), p_optional(fun 'space'/2), p_choose([fun 'upcase_name'/2, fun 'local_function_name'/2])]))(I,D) end, fun(Node, _Idx) ->
	[_, _, Elem] = Node,
	Elem
 end).

-spec 'declaration_seperator'(input(), index()) -> parse_result().
'declaration_seperator'(Input, Index) ->
  p(Input, Index, 'declaration_seperator', fun(I,D) -> (p_string(<<".">>))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'expression'(input(), index()) -> parse_result().
'expression'(Input, Index) ->
  p(Input, Index, 'expression', fun(I,D) -> (p_seq([p_label('head', fun 'expr_primary'/2), p_label('tail', p_zero_or_more(p_seq([p_optional(fun 'space'/2), fun 'infix'/2, p_optional(fun 'space'/2), fun 'expr_primary'/2])))]))(I,D) end, fun(Node, Idx) ->
	log_rule(expression, Node),
	Head = proplists:get_value(head, Node),
	TailWithSpaces = proplists:get_value(tail, Node, []),
	Tail = [{Op, Expr} || [_, Op, _, Expr] <- TailWithSpaces],
	case Tail of
		[] ->
			Head;
		_ ->
			{expression, simple_idx(Idx), Head, Tail}
	end
 end).

-spec 'expr_primary'(input(), index()) -> parse_result().
'expr_primary'(Input, Index) ->
  p(Input, Index, 'expr_primary', fun(I,D) -> (p_choose([fun 'call'/2, fun 'literal'/2, fun 'sub_expression'/2]))(I,D) end, fun(Node, _Idx) ->
	log_rule(expr_primary, Node),
	Node
 end).

-spec 'sub_expression'(input(), index()) -> parse_result().
'sub_expression'(Input, Index) ->
  p(Input, Index, 'sub_expression', fun(I,D) -> (p_seq([p_string(<<"(">>), p_optional(fun 'space'/2), p_label('expr', fun 'expression'/2), p_optional(fun 'space'/2), p_string(<<")">>)]))(I,D) end, fun(Node, Idx) ->
	log_rule(sub_expression, Node),
	Expr = proplists:get_value(expr, Node),
	{expression, simple_idx(Idx), Expr}
 end).

-spec 'call'(input(), index()) -> parse_result().
'call'(Input, Index) ->
  p(Input, Index, 'call', fun(I,D) -> (p_seq([p_label('call', p_choose([fun 'function_name'/2, fun 'type_name'/2])), p_label('args', p_zero_or_more(p_seq([fun 'space'/2, fun 'argument'/2])))]))(I,D) end, fun(Node, Idx) ->
	log_rule(call, Node),
	Call = proplists:get_value(call, Node),
	ArgsWithSpaces = proplists:get_value(args, Node, []),
	Args = [A || [_, A] <- ArgsWithSpaces],
	{call, simple_idx(Idx), Call, Args}
 end).

-spec 'argument'(input(), index()) -> parse_result().
'argument'(Input, Index) ->
  p(Input, Index, 'argument', fun(I,D) -> (p_choose([fun 'literal'/2, fun 'function_name'/2, fun 'type_name'/2, fun 'sub_expression'/2]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'literal'(input(), index()) -> parse_result().
'literal'(Input, Index) ->
  p(Input, Index, 'literal', fun(I,D) -> (p_choose([fun 'literal_list'/2, fun 'literal_map'/2, fun 'literal_record'/2, fun 'literal_string'/2, fun 'literal_number'/2]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'literal_list'(input(), index()) -> parse_result().
'literal_list'(Input, Index) ->
  p(Input, Index, 'literal_list', fun(I,D) -> (p_seq([p_string(<<"[">>), p_label('elements', p_zero_or_more(p_seq([p_optional(fun 'space'/2), fun 'list_element'/2]))), p_optional(fun 'space'/2), p_string(<<"]">>)]))(I,D) end, fun(Node, Idx) ->
	log_rule(?FUNCTION_NAME, Node),
	DirtyElements = proplists:get_value(elements, Node, []),
	Elements = [E || [_, E] <- DirtyElements],
	{literal_list, simple_idx(Idx), Elements}
 end).

-spec 'list_element'(input(), index()) -> parse_result().
'list_element'(Input, Index) ->
  p(Input, Index, 'list_element', fun(I,D) -> (p_seq([p_string(<<",">>), p_optional(fun 'space'/2), fun 'expression'/2]))(I,D) end, fun(Node, _Idx) ->
	log_rule(?FUNCTION_NAME, Node),
	[_, _, Element] = Node,
	Element
 end).

-spec 'literal_map'(input(), index()) -> parse_result().
'literal_map'(Input, Index) ->
  p(Input, Index, 'literal_map', fun(I,D) -> (p_seq([p_string(<<"#{">>), p_zero_or_more(p_seq([p_optional(fun 'space'/2), fun 'map_entry'/2])), p_optional(fun 'space'/2), p_string(<<"}#">>)]))(I,D) end, fun(Node, Idx) ->
	log_rule(?FUNCTION_NAME, Node),
	[_, DirtyFields, _, _] = Node,
	Fields = [F || [_, F] <- DirtyFields],
	{literal_map, simple_idx(Idx), Fields}
 end).

-spec 'map_entry'(input(), index()) -> parse_result().
'map_entry'(Input, Index) ->
  p(Input, Index, 'map_entry', fun(I,D) -> (p_seq([p_string(<<",">>), p_optional(fun 'space'/2), fun 'expression'/2, p_optional(fun 'space'/2), p_string(<<"=">>), p_optional(fun 'space'/2), fun 'expression'/2]))(I,D) end, fun(Node, Idx) ->
	log_rule(?FUNCTION_NAME, Node),
	[_, _, Key, _, _, _, Value] = Node,
	{map_entry, simple_idx(Idx), Key, Value}
 end).

-spec 'literal_record'(input(), index()) -> parse_result().
'literal_record'(Input, Index) ->
  p(Input, Index, 'literal_record', fun(I,D) -> (p_seq([p_string(<<"{">>), p_zero_or_more(p_seq([p_optional(fun 'space'/2), fun 'record_field'/2])), p_optional(fun 'space'/2), p_string(<<"}">>)]))(I,D) end, fun(Node, Idx) ->
	log_rule(?FUNCTION_NAME, Node),
	[_, DirtyFields, _, _] = Node,
	Fields = [F || [_, F] <- DirtyFields],
	{literal_record, simple_idx(Idx), Fields}
 end).

-spec 'record_field'(input(), index()) -> parse_result().
'record_field'(Input, Index) ->
  p(Input, Index, 'record_field', fun(I,D) -> (p_seq([p_string(<<",">>), p_optional(fun 'space'/2), fun 'record_field_key'/2, p_optional(fun 'space'/2), p_string(<<":=">>), p_optional(fun 'space'/2), fun 'expression'/2]))(I,D) end, fun(Node, Idx) ->
	[_, _, Key, _, _, _, Value] = Node,
	{record_field, simple_idx(Idx), Key, Value}
 end).

-spec 'record_field_key'(input(), index()) -> parse_result().
'record_field_key'(Input, Index) ->
  p(Input, Index, 'record_field_key', fun(I,D) -> (p_choose([fun 'downcase_name'/2, p_seq([p_charclass(<<"[1-9]">>), p_zero_or_more(p_charclass(<<"[0-9]">>))])]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'literal_string'(input(), index()) -> parse_result().
'literal_string'(Input, Index) ->
  p(Input, Index, 'literal_string', fun(I,D) -> (p_seq([p_string(<<"\"">>), p_label('string', p_zero_or_more(p_seq([p_not(p_string(<<"\"">>)), p_choose([p_string(<<"\\\\">>), p_string(<<"\\\"">>), p_anything()])]))), p_string(<<"\"">>)]))(I,D) end, fun(Node, Idx) ->
	String = proplists:get_value(string, Node),
	{literal_string, simple_idx(Idx), unicode:characters_to_binary(String)}
 end).

-spec 'literal_number'(input(), index()) -> parse_result().
'literal_number'(Input, Index) ->
  p(Input, Index, 'literal_number', fun(I,D) -> (p_seq([p_optional(p_string(<<"-">>)), p_one_or_more(p_charclass(<<"[0-9]">>)), p_optional(p_seq([p_string(<<".">>), p_one_or_more(p_charclass(<<"[0-9]">>))]))]))(I,D) end, fun(Node, Idx) ->
	case Node of
		[_, _, []] ->
			{literal_integer, simple_idx(Idx), binary_to_integer(unicode:characters_to_binary(Node))};
		[_, _, [_, _]] ->
			{literal_float, simple_idx(Idx), binary_to_float(unicode:characters_to_binary(Node))}
	end
 end).

-spec 'space'(input(), index()) -> parse_result().
'space'(Input, Index) ->
  p(Input, Index, 'space', fun(I,D) -> (p_one_or_more(p_choose([fun 'whitespace'/2, fun 'comment_line'/2])))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'whitespace'(input(), index()) -> parse_result().
'whitespace'(Input, Index) ->
  p(Input, Index, 'whitespace', fun(I,D) -> (p_charclass(<<"[\s\t\n\r]">>))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'comment_line'(input(), index()) -> parse_result().
'comment_line'(Input, Index) ->
  p(Input, Index, 'comment_line', fun(I,D) -> (p_seq([p_string(<<"-doc\s">>), p_label('docstring', p_zero_or_more(p_seq([p_not(p_string(<<"\n">>)), p_anything()])))]))(I,D) end, fun(Node, Idx) ->
	String = proplists:get_value(docstring, Node),
	{docstring, simple_idx(Idx), iolist_to_binary(String)}
 end).

-spec 'type_name'(input(), index()) -> parse_result().
'type_name'(Input, Index) ->
  p(Input, Index, 'type_name', fun(I,D) -> (p_seq([p_label('head', fun 'upcase_name'/2), p_label('tail', p_zero_or_more(p_seq([p_string(<<".">>), fun 'upcase_name'/2])))]))(I,D) end, fun(Node, Idx) ->
	Head = proplists:get_value(head, Node),
	TailWithDots = proplists:get_value(tail, Node, []),
	Tail = [N || [_, N] <- TailWithDots],
	case Tail of
		[] ->
			{local_type_name, simple_idx(Idx), Head};
		_ ->
			ReversedTail = lists:reverse(Tail),
			{[LocalPart], ReversedRemoteTail} = lists:split(1, ReversedTail),
			RemoteParts = [Head | lists:reverse(ReversedRemoteTail)],
			RemotePart = iolist_to_binary(lists:join($., RemoteParts)),
			{remote_type_name, simple_idx(Idx), RemotePart, LocalPart}
	end
 end).

-spec 'module_name'(input(), index()) -> parse_result().
'module_name'(Input, Index) ->
  p(Input, Index, 'module_name', fun(I,D) -> (fun 'type_name'/2)(I,D) end, fun(Node, _Idx) ->
	case Node of
		{_, SimpleIdx, N} ->
			{module_name, SimpleIdx, N};
		{_, SimpleIdx, Path, Local} ->
			{module_name, SimpleIdx, unicode:characters_to_binary([Path, $., Local])}
	end
 end).

-spec 'infix'(input(), index()) -> parse_result().
'infix'(Input, Index) ->
  p(Input, Index, 'infix', fun(I,D) -> (p_choose([fun 'infix_notation'/2, fun 'symbol'/2]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'symbol'(input(), index()) -> parse_result().
'symbol'(Input, Index) ->
  p(Input, Index, 'symbol', fun(I,D) -> (p_choose([fun 'blessed_symbol'/2, p_seq([p_choose([fun 'blessed_symbol'/2, p_string(<<"=">>)]), p_one_or_more(p_choose([fun 'blessed_symbol'/2, p_string(<<"=">>)]))])]))(I,D) end, fun(Node, Idx) ->
	Out = {function_symbol, simple_idx(Idx), unicode:characters_to_binary(Node)},
	log_rule(?FUNCTION_NAME, Node, Out, "", [])
 end).

-spec 'blessed_symbol'(input(), index()) -> parse_result().
'blessed_symbol'(Input, Index) ->
  p(Input, Index, 'blessed_symbol', fun(I,D) -> (p_choose([p_string(<<"+">>), p_string(<<"_">>), p_string(<<"-">>), p_string(<<"*">>), p_string(<<"&">>), p_string(<<"^">>), p_string(<<"%">>), p_string(<<"$">>), p_string(<<"#">>), p_string(<<"@">>), p_string(<<"!">>), p_string(<<"~">>), p_string(<<"`">>), p_seq([p_string(<<"?">>), p_string(<<">">>)]), p_string(<<"<">>), p_string(<<"\/">>)]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'infix_notation'(input(), index()) -> parse_result().
'infix_notation'(Input, Index) ->
  p(Input, Index, 'infix_notation', fun(I,D) -> (p_choose([fun 'left_assoc'/2, fun 'right_assoc'/2]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'left_assoc'(input(), index()) -> parse_result().
'left_assoc'(Input, Index) ->
  p(Input, Index, 'left_assoc', fun(I,D) -> (p_seq([p_label('weight', p_one_or_more(p_string(<<"|">>))), p_string(<<">">>), p_label('name', p_choose([fun 'function_name'/2, fun 'symbol'/2]))]))(I,D) end, fun(Node, Idx) ->
	WeightStr = proplists:get_value(weight, Node),
	Weight = length(WeightStr),
	OpName = proplists:get_value(name, Node),
	{left_assoc, simple_idx(Idx), Weight, OpName}
 end).

-spec 'right_assoc'(input(), index()) -> parse_result().
'right_assoc'(Input, Index) ->
  p(Input, Index, 'right_assoc', fun(I,D) -> (p_seq([p_string(<<"<">>), p_label('weight', p_one_or_more(p_string(<<"|">>))), p_label('name', p_choose([fun 'function_name'/2, fun 'symbol'/2]))]))(I,D) end, fun(Node, Idx) ->
	WeightStr = proplists:get_value(weight, Node),
	Weight = length(WeightStr),
	OpName = proplists:get_value(name, Node),
	{right_assoc, simple_idx(Idx), Weight, OpName}
 end).

-spec 'function_name'(input(), index()) -> parse_result().
'function_name'(Input, Index) ->
  p(Input, Index, 'function_name', fun(I,D) -> (p_choose([fun 'remote_function_name'/2, fun 'local_function_name'/2, fun 'symbol_as_function_name'/2]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'local_function_name'(input(), index()) -> parse_result().
'local_function_name'(Input, Index) ->
  p(Input, Index, 'local_function_name', fun(I,D) -> (fun 'downcase_name'/2)(I,D) end, fun(Node, Idx) ->
	{local_function_name, simple_idx(Idx), Node}
 end).

-spec 'remote_function_name'(input(), index()) -> parse_result().
'remote_function_name'(Input, Index) ->
  p(Input, Index, 'remote_function_name', fun(I,D) -> (p_seq([fun 'module_name'/2, p_string(<<".">>), fun 'downcase_name'/2]))(I,D) end, fun(Node, Idx) ->
	[Module, _, Function] = Node,
	{remote_function_name, simple_idx(Idx), Module, Function}
 end).

-spec 'symbol_as_function_name'(input(), index()) -> parse_result().
'symbol_as_function_name'(Input, Index) ->
  p(Input, Index, 'symbol_as_function_name', fun(I,D) -> (p_seq([p_string(<<"\'">>), fun 'symbol'/2, p_string(<<"\'">>)]))(I,D) end, fun(Node, _Idx) ->
	[_, Out, _] = Node,
	Out
 end).

-spec 'bindable_function_name'(input(), index()) -> parse_result().
'bindable_function_name'(Input, Index) ->
  p(Input, Index, 'bindable_function_name', fun(I,D) -> (p_choose([fun 'local_function_name'/2, fun 'symbol_as_function_name'/2]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'variable'(input(), index()) -> parse_result().
'variable'(Input, Index) ->
  p(Input, Index, 'variable', fun(I,D) -> (fun 'downcase_name'/2)(I,D) end, fun(Node, Idx) ->
	{variable, simple_idx(Idx), Node}
 end).

-spec 'upcase_name'(input(), index()) -> parse_result().
'upcase_name'(Input, Index) ->
  p(Input, Index, 'upcase_name', fun(I,D) -> (p_seq([p_charclass(<<"[A-Z]">>), p_zero_or_more(p_charclass(<<"[a-zA-Z_0-9]">>))]))(I,D) end, fun(Node, _Idx) ->
	iolist_to_binary(Node)
 end).

-spec 'downcase_name'(input(), index()) -> parse_result().
'downcase_name'(Input, Index) ->
  p(Input, Index, 'downcase_name', fun(I,D) -> (p_seq([p_charclass(<<"[a-z]">>), p_zero_or_more(p_charclass(<<"[A-Za-z_0-9]">>))]))(I,D) end, fun(Node, _Idx) ->
	iolist_to_binary(Node)
 end).

-spec 'type_top'(input(), index()) -> parse_result().
'type_top'(Input, Index) ->
  p(Input, Index, 'type_top', fun(I,D) -> (p_seq([fun 'type_primary'/2, p_zero_or_more(p_seq([p_optional(fun 'space'/2), p_string(<<"->">>), p_optional(fun 'space'/2), fun 'type_primary'/2]))]))(I,D) end, fun(Node, Idx) ->
	log_rule(?FUNCTION_NAME, Node),
	case Node of
		[T , []] ->
			T;
		[FirstArg, RawTail] ->
			FixedTail = [TailType || [_, _, _, TailType] <- RawTail],
			ArgsWithReturn = [FirstArg | FixedTail],
			[Return | ReversedArgs] = lists:reverse(ArgsWithReturn),
			Args = lists:reverse(ReversedArgs),
			{type_function, simple_idx(Idx), Args, Return}
	end
 end).

-spec 'type_primary'(input(), index()) -> parse_result().
'type_primary'(Input, Index) ->
  p(Input, Index, 'type_primary', fun(I,D) -> (p_choose([fun 'type_concrete'/2, fun 'type_variable'/2, fun 'sub_type_expression'/2]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'sub_type_expression'(input(), index()) -> parse_result().
'sub_type_expression'(Input, Index) ->
  p(Input, Index, 'sub_type_expression', fun(I,D) -> (p_seq([p_string(<<"(">>), p_optional(fun 'space'/2), fun 'type_top'/2, p_optional(fun 'space'/2), p_string(<<")">>)]))(I,D) end, fun(Node, _Idx) ->
	[_, _, T, _, _] = Node,
	T
 end).

-spec 'type_variable'(input(), index()) -> parse_result().
'type_variable'(Input, Index) ->
  p(Input, Index, 'type_variable', fun(I,D) -> (fun 'variable'/2)(I,D) end, fun(Node, _Idx) ->
	setelement(1, Node, type_variable)
 end).

-spec 'type_concrete'(input(), index()) -> parse_result().
'type_concrete'(Input, Index) ->
  p(Input, Index, 'type_concrete', fun(I,D) -> (p_seq([fun 'type_name'/2, p_zero_or_more(p_seq([fun 'space'/2, fun 'type_arg'/2]))]))(I,D) end, fun(Node, Idx) ->
	[BaseName, ArgsWithSpaces] = Node,
	Args = [Arg || [_, Arg] <- ArgsWithSpaces],
	{type_concrete, simple_idx(Idx), BaseName, Args}
 end).

-spec 'type_arg'(input(), index()) -> parse_result().
'type_arg'(Input, Index) ->
  p(Input, Index, 'type_arg', fun(I,D) -> (p_choose([fun 'type_name'/2, fun 'type_variable'/2, fun 'sub_type_expression'/2]))(I,D) end, fun(Node, Idx) ->
	io:format("blerg type arg node! ~p~n", [Node]),
	case Node of
		[_, _, Type, _, _] ->
			Type;
		{local_type_name, _, _} = Name ->
			{type_concrete, simple_idx(Idx), Name, []};
		{remote_type_name, _, _, _} = Name ->
			{type_concrete, simple_idx(Idx), Name, []};
		_ ->
			Node
	end
 end).



-file("peg_includes.hrl", 1).
-type index() :: {{line, pos_integer()}, {column, pos_integer()}}.
-type input() :: binary().
-type parse_failure() :: {fail, term()}.
-type parse_success() :: {term(), input(), index()}.
-type parse_result() :: parse_failure() | parse_success().
-type parse_fun() :: fun((input(), index()) -> parse_result()).
-type xform_fun() :: fun((input(), index()) -> term()).

-spec p(input(), index(), atom(), parse_fun(), xform_fun()) -> parse_result().
p(Inp, StartIndex, Name, ParseFun, TransformFun) ->
  case get_memo(StartIndex, Name) of      % See if the current reduction is memoized
    {ok, Memo} -> %Memo;                     % If it is, return the stored result
      Memo;
    _ ->                                        % If not, attempt to parse
      Result = case ParseFun(Inp, StartIndex) of
        {fail,_} = Failure ->                       % If it fails, memoize the failure
          Failure;
        {Match, InpRem, NewIndex} ->               % If it passes, transform and memoize the result.
          Transformed = TransformFun(Match, StartIndex),
          {Transformed, InpRem, NewIndex}
      end,
      memoize(StartIndex, Name, Result),
      Result
  end.

-spec setup_memo() -> ets:tid().
setup_memo() ->
  put({parse_memo_table, ?MODULE}, ets:new(?MODULE, [set])).

-spec release_memo() -> true.
release_memo() ->
  ets:delete(memo_table_name()).

-spec memoize(index(), atom(), parse_result()) -> true.
memoize(Index, Name, Result) ->
  Memo = case ets:lookup(memo_table_name(), Index) of
              [] -> [];
              [{Index, Plist}] -> Plist
         end,
  ets:insert(memo_table_name(), {Index, [{Name, Result}|Memo]}).

-spec get_memo(index(), atom()) -> {ok, term()} | {error, not_found}.
get_memo(Index, Name) ->
  case ets:lookup(memo_table_name(), Index) of
    [] -> {error, not_found};
    [{Index, Plist}] ->
      case proplists:lookup(Name, Plist) of
        {Name, Result}  -> {ok, Result};
        _  -> {error, not_found}
      end
    end.

-spec memo_table_name() -> ets:tid().
memo_table_name() ->
    get({parse_memo_table, ?MODULE}).

-ifdef(p_eof).
-spec p_eof() -> parse_fun().
p_eof() ->
  fun(<<>>, Index) -> {eof, [], Index};
     (_, Index) -> {fail, {expected, eof, Index}} end.
-endif.

-ifdef(p_optional).
-spec p_optional(parse_fun()) -> parse_fun().
p_optional(P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} -> {[], Input, Index};
        {_, _, _} = Success -> Success
      end
  end.
-endif.

-ifdef(p_not).
-spec p_not(parse_fun()) -> parse_fun().
p_not(P) ->
  fun(Input, Index)->
      case P(Input,Index) of
        {fail,_} ->
          {[], Input, Index};
        {Result, _, _} -> {fail, {expected, {no_match, Result},Index}}
      end
  end.
-endif.

-ifdef(p_assert).
-spec p_assert(parse_fun()) -> parse_fun().
p_assert(P) ->
  fun(Input,Index) ->
      case P(Input,Index) of
        {fail,_} = Failure-> Failure;
        _ -> {[], Input, Index}
      end
  end.
-endif.

-ifdef(p_seq).
-spec p_seq([parse_fun()]) -> parse_fun().
p_seq(P) ->
  fun(Input, Index) ->
      p_all(P, Input, Index, [])
  end.

-spec p_all([parse_fun()], input(), index(), [term()]) -> parse_result().
p_all([], Inp, Index, Accum ) -> {lists:reverse( Accum ), Inp, Index};
p_all([P|Parsers], Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail, _} = Failure -> Failure;
    {Result, InpRem, NewIndex} -> p_all(Parsers, InpRem, NewIndex, [Result|Accum])
  end.
-endif.

-ifdef(p_choose).
-spec p_choose([parse_fun()]) -> parse_fun().
p_choose(Parsers) ->
  fun(Input, Index) ->
      p_attempt(Parsers, Input, Index, none)
  end.

-spec p_attempt([parse_fun()], input(), index(), none | parse_failure()) -> parse_result().
p_attempt([], _Input, _Index, Failure) -> Failure;
p_attempt([P|Parsers], Input, Index, FirstFailure)->
  case P(Input, Index) of
    {fail, _} = Failure ->
      case FirstFailure of
        none -> p_attempt(Parsers, Input, Index, Failure);
        _ -> p_attempt(Parsers, Input, Index, FirstFailure)
      end;
    Result -> Result
  end.
-endif.

-ifdef(p_zero_or_more).
-spec p_zero_or_more(parse_fun()) -> parse_fun().
p_zero_or_more(P) ->
  fun(Input, Index) ->
      p_scan(P, Input, Index, [])
  end.
-endif.

-ifdef(p_one_or_more).
-spec p_one_or_more(parse_fun()) -> parse_fun().
p_one_or_more(P) ->
  fun(Input, Index)->
      Result = p_scan(P, Input, Index, []),
      case Result of
        {[_|_], _, _} ->
          Result;
        _ ->
          {fail, {expected, Failure, _}} = P(Input,Index),
          {fail, {expected, {at_least_one, Failure}, Index}}
      end
  end.
-endif.

-ifdef(p_label).
-spec p_label(atom(), parse_fun()) -> parse_fun().
p_label(Tag, P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} = Failure ->
           Failure;
        {Result, InpRem, NewIndex} ->
          {{Tag, Result}, InpRem, NewIndex}
      end
  end.
-endif.

-ifdef(p_scan).
-spec p_scan(parse_fun(), input(), index(), [term()]) -> {[term()], input(), index()}.
p_scan(_, <<>>, Index, Accum) -> {lists:reverse(Accum), <<>>, Index};
p_scan(P, Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail,_} -> {lists:reverse(Accum), Inp, Index};
    {Result, InpRem, NewIndex} -> p_scan(P, InpRem, NewIndex, [Result | Accum])
  end.
-endif.

-ifdef(p_string).
-spec p_string(binary()) -> parse_fun().
p_string(S) ->
    Length = erlang:byte_size(S),
    fun(Input, Index) ->
      try
          <<S:Length/binary, Rest/binary>> = Input,
          {S, Rest, p_advance_index(S, Index)}
      catch
          error:{badmatch,_} -> {fail, {expected, {string, S}, Index}}
      end
    end.
-endif.

-ifdef(p_anything).
-spec p_anything() -> parse_fun().
p_anything() ->
  fun(<<>>, Index) -> {fail, {expected, any_character, Index}};
     (Input, Index) when is_binary(Input) ->
          <<C/utf8, Rest/binary>> = Input,
          {<<C/utf8>>, Rest, p_advance_index(<<C/utf8>>, Index)}
  end.
-endif.

-ifdef(p_charclass).
-spec p_charclass(string() | binary()) -> parse_fun().
p_charclass(Class) ->
    {ok, RE} = re:compile(Class, [unicode, dotall]),
    fun(Inp, Index) ->
            case re:run(Inp, RE, [anchored]) of
                {match, [{0, Length}|_]} ->
                    {Head, Tail} = erlang:split_binary(Inp, Length),
                    {Head, Tail, p_advance_index(Head, Index)};
                _ -> {fail, {expected, {character_class, binary_to_list(Class)}, Index}}
            end
    end.
-endif.

-ifdef(p_regexp).
-spec p_regexp(binary()) -> parse_fun().
p_regexp(Regexp) ->
    {ok, RE} = re:compile(Regexp, [unicode, dotall, anchored]),
    fun(Inp, Index) ->
        case re:run(Inp, RE) of
            {match, [{0, Length}|_]} ->
                {Head, Tail} = erlang:split_binary(Inp, Length),
                {Head, Tail, p_advance_index(Head, Index)};
            _ -> {fail, {expected, {regexp, binary_to_list(Regexp)}, Index}}
        end
    end.
-endif.

-ifdef(line).
-spec line(index() | term()) -> pos_integer() | undefined.
line({{line,L},_}) -> L;
line(_) -> undefined.
-endif.

-ifdef(column).
-spec column(index() | term()) -> pos_integer() | undefined.
column({_,{column,C}}) -> C;
column(_) -> undefined.
-endif.

-spec p_advance_index(input() | unicode:charlist() | pos_integer(), index()) -> index().
p_advance_index(MatchedInput, Index) when is_list(MatchedInput) orelse is_binary(MatchedInput)-> % strings
  lists:foldl(fun p_advance_index/2, Index, unicode:characters_to_list(MatchedInput));
p_advance_index(MatchedInput, Index) when is_integer(MatchedInput) -> % single characters
  {{line, Line}, {column, Col}} = Index,
  case MatchedInput of
    $\n -> {{line, Line+1}, {column, 1}};
    _ -> {{line, Line}, {column, Col+1}}
  end.
