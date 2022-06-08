-module(milang_p_atomic).

-export(
	[ parse/1
	]).
-export(
	[ downcase_name/0
	, upcase_name/0
	, space_then/1
	, space_opt_then/1
	, whitespace/0
	, comment_line/0
	, type_name/0
	%, type_name_local/0
	%, type_name_remote/0
	%, module_name_prefix/0
	, variable/0
	, infix/0
	, infix_symbol/0
	, function_name/0
	, function_name_symbol/0
	, function_name_local/0
	, list/1
	, record/2
	, parens/1
	]).

parse(Subject) ->
	Parser = parse:first_of(
		[ space()
		, function_name()
		, type_name()
		, infix()
		, variable()
		%, module_name_prefix()
		]),
	parse:it(Subject, Parser).

parens(Parser) ->
	parse:lazy(fun() ->
		SeriesP = parse:series([parse:character($(), space_opt_then(Parser), space_opt_then(parse:character($)))]),
		parse:set_tag(generic_parens, parse:map(SeriesP, fun([_, Parsed, _]) ->
			Parsed
		end))
	end).

list(ElementParser) ->
	ElementP = parse:lazy(fun() ->
		parse:series([space_opt_then(parse:character($,)), space_opt_then(ElementParser)])
	end),
	EndingP = space_opt_then(parse:character($])),
	ListP = parse:repeat_until(ElementP, EndingP),
	SeriesP = parse:series([parse:character($[), ListP]),
	Mapper = fun([_, {EWithSpaces, _}]) ->
		[E || [_, E] <- EWithSpaces]
	end,
	parse:set_tag(generic_list, parse:map(SeriesP, Mapper)).

record(KeyParser, ValueParser) ->
	ElementP = parse:lazy(fun() ->
		ESeriesP = parse:series([space_opt_then(parse:character($,)), space_opt_then(KeyParser), space_opt_then(parse:character($=)), space_opt_then(ValueParser)]),
		ETagged = parse:tag(record_field, ESeriesP),
		EMapper = fun({T, L, Node}) ->
			[_Comma, Key, _Equals, Value] = Node,
			milang_ast:ast_node(L, <<>>, T, #{ key => Key, value => Value })
		end,
		parse:map(ETagged, EMapper)
	end),
	ElementsP = parse:repeat_until(ElementP, space_opt_then(parse:character($}))),
	SeriesP = parse:series([parse:character(${), ElementsP]),
	Mapper = fun([_, {Elements, _ClosingBrace}]) ->
		Elements
	end,
	parse:set_tag(generic_record, parse:map(SeriesP, Mapper)).

downcase_name() ->
	RegEx = parse:regex("\\p{Ll}\\w*", first),
	parse:map(RegEx, fun([V]) -> V end).

upcase_name() ->
	RegEx = parse:regex("\\p{Lu}\\w*", first),
	parse:map(RegEx, fun([V]) -> V end).

space() ->
	parse:first_of([whitespace(), comment_line()]).

space_then(AfterSpace) ->
	SpaceP = space_opt_then(AfterSpace),
	SeriesP = parse:series([space(), SpaceP]),
	Mapper = fun([_FirstSpace, Terminal]) ->
		Terminal
	end,
	parse:map(SeriesP, Mapper).

space_opt_then(AfterSpace) ->
	RepeatP = parse:repeat_until(space(), AfterSpace),
	Mapper = fun({_Space, Terminal}) ->
		Terminal
	end,
	parse:map(RepeatP, Mapper).

whitespace() ->
	parse:regex("\\s+").

comment_line() ->
	Series = parse:series([parse:string(<<"-doc">>), parse:regex("[ \\t]+"), parse:regex("[^\\n]*", first)]),
	Mapper = fun([_, _, [String]]) ->
		String
	end,
	parse:map(Series, Mapper).

type_name() ->
	parse:first_of([ type_name_remote(), type_name_local() ]).

type_name_remote() ->
	RepeatWhenRevealed = parse:series([parse:character($.), parse:test(upcase_name())]),
	RepeatWhen = parse:map(RepeatWhenRevealed, fun(_) -> ok end),
	Repeat = parse:repeat_when(upcase_name(), RepeatWhen),
	Tagged = parse:tag(undefined, Repeat),
	Mapper = fun
		({_, _, []}) ->
			{error, zero_length_name};
		({_, L, [Part]}) ->
			milang_ast:ast_node(L, <<>>, type_name_local, binary_to_atom(Part));
		({_, L, Parts}) ->
			[Type , _ExtraDot | ReversedModuleParts] = lists:reverse(Parts),
			ModulePartOrOk = lists:reverse(ReversedModuleParts),
			ModulePart = lists:map(fun(ok) -> $.; (E) -> E end, ModulePartOrOk),
			Data = #{ name => binary_to_atom(Type, utf8), module => binary_to_atom(unicode:characters_to_binary(ModulePart)) },
			milang_ast:ast_node(L, <<>>, type_name_remote, Data)
	end,
	parse:set_tag(type_name_remote, parse:map(Tagged, Mapper)).

type_name_local() ->
	Tagged = parse:tag(type_name_local, upcase_name()),
	parse:set_tag(type_name_local, parse:map(Tagged, fun({T, L, N}) ->
		milang_ast:ast_node(L, <<>>, T, binary_to_atom(N, utf8))
	end)).

%module_name_prefix() ->
%	RepeatWhen = parse:series([parse:character($.), parse:test(upcase_name())]),
%	Repeat = parse:repeat_when(upcase_name(), RepeatWhen),
%	Mapper = fun(Iolist) ->
%		unicode:characters_to_binary(Iolist)
%	end,
%	parse:set_tag(module_name_prefix, parse:map(Repeat, Mapper)).

variable() ->
	RegEx = parse:regex("[\\p{Ll}_]\\w*", first),
	Tagged = parse:tag(variable, RegEx),
	Mapper = fun({T, L, [V]}) ->
		milang_ast:ast_node(L, <<>>, T, binary_to_atom(V))
	end,
	parse:set_tag(variable, parse:map(Tagged, Mapper)).

infix() ->
	parse:first_of([ infix_notation(), infix_symbol() ]).

infix_notation() ->
	WeightAndAssoc = parse:regex(<<"«+|»+"/utf8>>, first),
	Function = parse:first_of([function_name(), infix_symbol()]),
	Sequence = parse:series([WeightAndAssoc, Function]),
	Tagged = parse:tag(infix_notation, Sequence),
	Mapper = fun({T, L, [[WeightAndAssocChars], FunctionAst]}) ->
		<<ArrowChar/utf8, _/binary>> = WeightAndAssocChars,
		Weight = round(size(WeightAndAssocChars) / 2), % the « and » are each 2 bytes.
		Assoc = case ArrowChar of
			$« ->
				right;
			$» ->
				left
		end,
		Data = #{ assoc => Assoc, weight => Weight, function => FunctionAst},
		milang_ast:ast_node(L, <<>>, T, Data)
	end,
	parse:set_tag(infix_notation, parse:map(Tagged, Mapper)).

infix_symbol() ->
	DisallowedSet = ordsets:from_list("'\"[]{}(),"),
	AllowedRegexTest = "[\\pS\\pP]+",
	{ok, Compiled} = re:compile(AllowedRegexTest, [unicode, ucp]),
	ChompTest = fun(C) ->
		case ordsets:is_element(C, DisallowedSet) of
			true ->
				false;
			false ->
				case re:run([C], Compiled) of
					{match, _} ->
						true;
					nomatch ->
						false
				end
		end
	end,
	ChompP = parse:chomp_while(ChompTest),
	ValueCheckP = parse:unless(ChompP, fun
		(<<"=">>) ->
			{error, solo_equals_not_allowed};
		(<<".">>) ->
			{error, solo_dot_not_allowed};
		(<<>>) ->
			{error, zero_length_infix_not_valid};
		(_Passing) ->
			ok
	end),
	Tagged = parse:tag(infix_symbol, ValueCheckP),
	Mapped = fun({T, L, B}) ->
		milang_ast:ast_node(L, <<>>, T, binary_to_atom(B, utf8))
	end,
	parse:set_tag(infix_symbol, parse:map(Tagged, Mapped)).

function_name() ->
	parse:set_tag(function_name, parse:first_of([ function_name_local(), function_name_symbol(), function_name_remote()])).

function_name_local() ->
	Tagged = parse:tag(function_name_local, downcase_name()),
	Mapper = fun({T, L, N}) ->
		milang_ast:ast_node(L, <<>>, T, binary_to_atom(N, utf8))
	end,
	parse:set_tag(function_name_local, parse:map(Tagged, Mapper)).

function_name_remote() ->
	LocalNameRaw = parse:first_of([downcase_name(), function_name_symbol()]),
	LocalNamePart = parse:map(LocalNameRaw, fun(Node) ->
		if
			is_binary(Node) ->
				binary_to_atom(Node, utf8);
			is_tuple(Node) ->
				milang_ast:data(Node)
		end
	end),
	Series = parse:repeat_until(parse:series([upcase_name(), parse:character($.)]), LocalNamePart),
	%Series = parse:series([module_name_prefix(), LocalNamePart]),
	Tag = parse:tag(function_name_remote, Series),
	%Mapper = fun({T, L, [M, F]}) ->
	%	Data = #{ name => F, module => binary_to_atom(M) },
	%	milang_ast:ast_node(L, <<>>, T, Data)
	%end,
	Mapper = fun
		({_, _, {[], F}}) ->
			F;
		({T, L, {M, F}}) ->
			JustModuleParts = [MPart || [MPart, _] <- M ],
			ModulePartIodata = lists:join($., JustModuleParts),
			Data = #{ name => F, module => binary_to_atom(unicode:characters_to_binary(ModulePartIodata))},
			milang_ast:ast_node(L, <<>>, T, Data)
	end,
	parse:set_tag(function_name_remote, parse:map(Tag, Mapper)).

function_name_symbol() ->
	Series = parse:series([parse:character($'), infix_symbol(), parse:character($')]),
	Tagged = parse:tag(function_name_symbol, Series),
	Mapper = fun({T, L, [_, SymbolAST, _]}) ->
		#{ data := Symbol } = milang_ast:to_map(SymbolAST),
		milang_ast:ast_node(L, <<>>, T, Symbol)
	end,
	parse:set_tag(function_name_symbol, parse:map(Tagged, Mapper)).
