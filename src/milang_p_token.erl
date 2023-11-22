-module(milang_p_token).

-include_lib("kernel/include/logger.hrl").

-type mi_identifier()
	:: unicode:chardata()
	|  #{ local := unicode:chardata(), module := unicode:chardata() }
	.

-type keyword() :: {keyword, milang_ast:location(), atom()}.
-type whitespace() :: {whitespace, milang_ast:location(), unicode:chardata()}.
-type comment() :: {comment, milang_ast:location(), unicode:chardata()}.
-type identifier_bound() :: {identifier_bound, mi_identifier()}.
-type identifier_ignored() :: {identifier_ignored, mi_identifier()}.
-type identifier_bindable() :: identifier_bound() | identifier_ignored().
-type literal_float() :: {literal_float, milang_ast:location(), float()}.
-type literal_integer() :: {literal_integer, milang_ast:location(), integer()}.
-type literal_number() :: float() | integer().
-type literal_string() :: {literal_string, milang_ast:location(), unicode:chardata()}.
-type syntax_implies() :: {syntax_implies, milang_ast:location(), unicode:chardata()}.
-type syntax_infix_left() :: {syntax_infix_left, milang_ast:location(), unicode:chardata()}.
-type syntax_infix_right() :: {syntax_infix_right, milang_ast:location(), unicode:chardata()}.
-type syntax_infix_indicator() :: {syntax_infix_indicator, milang_ast:location(), unicode:chardata()}.
-type syntax_keyword() :: {syntax_keyword, milang_ast:location(), 'when' | 'exposing' | 'match' | 'as' | 'func'}.
-type syntax_dot() :: {syntax_dot, milang_ast:location(), unicode:chardata()}.
-type syntax_cons() :: {syntax_cons, milang_ast:location(), unicode:chardata()}.
-type syntax_element_seperator() :: {syntax_element_seperator, milang_ast:location(), unicode:chardata()}.
-type syntax_open(T) :: {syntax_open, milang_ast:location(), T}.
-type syntax_close(T) :: {syntax_close, milang_ast:location(), T}.
-type syntax_open_list() :: syntax_open(list).
-type syntax_close_list() :: syntax_close(list).
-type syntax_open_record() :: syntax_open(record).
-type syntax_close_record() :: syntax_close(record).
-type syntax_open_subexpression() :: syntax_open(subexpression).
-type syntax_close_subexpression() :: syntax_close(subexpression).
-type syntax_bind() :: {syntax_bind, milang_ast:location(), unicode:chardata()}.
-type syntax_spec() :: {syntax_spec, milang_ast:location(), unicode:chardata()}.
-type eof() :: {token_name, milang_ast:location(), unicode:chardata()}.

-type token() ::
	  keyword()
	| whitespace()
	| comment()
	| literal_float()
	| literal_integer()
	| literal_string()
	| literal_number()
	| syntax_implies()
	| syntax_infix_left()
	| syntax_infix_right()
	| syntax_keyword()
	| syntax_dot()
	| syntax_cons()
	| syntax_element_seperator()
	| syntax_open_list()
	| syntax_close_list()
	| syntax_open_record()
	| syntax_close_record()
	| syntax_open_subexpression()
	| syntax_close_subexpression()
	| syntax_bind()
	| syntax_spec()
	| identifier_ignored()
	| identifier_bound()
	| identifier_bindable()
	| eof()
	.

-export_type(
	[ token/0
	, identifier_bindable/0
	, identifier_ignored/0
	, identifier_bound/0
	]).

-export(
	[ keyword_module/0
	, keyword_import/0
	, keyword_data/0
	, keyword_alias/0
	, keyword_spec/0
	, keyword_let/0
	, keyword_function/0
	, keyword_match/0
	, keyword_when/0
	, keyword_with/0
	, keyword_class/0
	, keyword_teach/0
	, keyword_as/0
	, keyword_expose_all/0
	, keyword_expose/0
	, whitespace/0
	, comment/0
	, documentation/0
	, any_space/0
	, literal_float/0
	, literal_integer/0
	, literal_number/0
	, literal_string/0
	, record_field_accessor/0
	, identifier_ignored/0
	, identifier_bound/0
	, identifier_bindable/0
	, identifier_local/0
	, identifier/0
	, syntax_bind/0
	, syntax_implies/0
	, syntax_infix_indicator/0
	, any_syntax_infix_indicator/0
	, syntax_dot/0
	, syntax_element_seperator/0
	, syntax_open_list/0
	, syntax_close_list/0
	, syntax_open_record/0
	, syntax_close_record/0
	, syntax_open_subexpression/0
	, syntax_close_subexpression/0
	, syntax_cons/0
	, eof/0
	]).

keyword(AsAtom) ->
	KeywordAsString = atom_to_binary(AsAtom, utf8),
	BacktrackableWord = parse:backtrackable(parse:chomp_string(KeywordAsString)),

	IdentifierCheck = parse:backtrackable(parse_do:chomp_if("[^'\"\\[\\]\\{\\}\\(\\),\\s]")),
	NotKeywordEnder = parse:map(compose:always(not_keyword_ender), IdentifierCheck),

	EndingCheck = parse:first_of(
		[ NotKeywordEnder
		, parse:success(keyword_ender)
		]),
	Final = parse:and_then(fun
		(keyword_ender) ->
			parse:commit(AsAtom);
		(not_keyword_ender) ->
			parse:fail({expected_keyword, AsAtom})
	end, EndingCheck),

	parse:pipeline(parse:success(compose:id()),
		[ parse:discard(BacktrackableWord)
		, parse:keep(Final)
		]).


keyword_module() ->
	keyword(module).

keyword_data() ->
	keyword('data').

keyword_alias() ->
	keyword('alias').

keyword_let() ->
	keyword('let').

keyword_spec() ->
	keyword('spec').

keyword_match() ->
	keyword('match').

keyword_when() ->
	keyword('when').

keyword_with() ->
	keyword('with').

keyword_class() ->
	keyword('class').

keyword_teach() ->
	keyword('teach').

keyword_expose() ->
	keyword('expose').

keyword_expose_all() ->
	keyword('expose all').

keyword_as() ->
	keyword('as').

keyword_function() ->
	keyword('function').

keyword_import() ->
	keyword('import').

-spec whitespace() -> parse:parser(term(), whitespace()).
whitespace() ->
	parse_do:chomp_while("\\s").

-spec documentation() -> parse:parser(term(), comment()).
documentation() ->
	parse:loop({false, []}, fun documentation_loop/1).

documentation_loop({SpaceSeen, DocAcc}) ->
	Whitespace = parse:get_chomped_string(whitespace()),
	FirstOf = parse:first_of(
		[ comment()
		, Whitespace
		]),
	parse:and_then(fun
		(<<>>) when SpaceSeen ->
			parse:success({done, DocAcc});
		(<<>>) when DocAcc =:= [] ->
			parse:fail(no_whitespace_nor_documentation);
		({comment, _, Doc}) ->
			parse:success({step, {true, [DocAcc | Doc]}});
		(_) ->
			parse:success({step, {true, DocAcc}})
	end, FirstOf).

-spec any_space() -> parse:parser(term(), comment()).
any_space() ->
	parse:loop([], fun any_space/1).

any_space(Acc) ->
	FirstOf = parse:first_of(
		[ comment()
		, parse:get_chomped_string(whitespace())
		]),
	parse:map(fun
		({comment, _, D}) ->
			{step, [Acc | D]};
		(<<>>) ->
			{done, Acc};
		(_Space) ->
			{step, Acc}
	end, FirstOf).

-spec comment() -> parse:parser(term(), comment()).
comment() ->
	OpeningRaw = parse:chomp_string(<<"{-">>),
	Opening = parse:map_err(fun(_) ->
		no_opening
	end, OpeningRaw),
	CommentLoop = parse:loop([], fun comment/1),
	parse:and_then(fun(_) ->
		CommentLoop
	end, Opening).

comment(Accumulated) ->
	DoneCheck = parse:map(fun(_) ->
		{done, unicode:characters_to_binary(lists:reverse(Accumulated))}
	end, parse:chomp_string(<<"-}">>)),
	JustChomp = parse:map(fun(C) ->
		{step, [C | Accumulated]}
	end, parse:get_chomped_string(parse:chomp_if(fun(<<>>) -> false; (_) -> true end, unclosed_comment))),
	FirstOfRaw = parse:first_of(
		[ DoneCheck
		, JustChomp
		]),
	parse:map_err(fun(_) ->
		unclosed_comment
	end, FirstOfRaw).

identifier_never_valid() ->
	parse:get_chomped_string(parse_do:chomp_if("[^\\.'\"\\[\\]\\{\\}\\(\\),\\s]", identifer_contains_invalid_characters)).

identifier_ignore_marker() ->
	parse:map(fun(_) ->
		ignore_mark
	end, parse_do:chomp_if("_", not_ignored)).

identifier_number_rep() ->
	parse:map(fun(Char) ->
		{number_part, Char}
	end, parse:get_chomped_string(parse_do:chomp_if("[\\-0-9]"))).

identifier_character() ->
	parse:first_of(
		[ identifier_ignore_marker()
		, identifier_number_rep()
		, identifier_never_valid()
		]).

identifier_next_part() ->
	ParseAfterDot = parse:and_then(fun(_) ->
		identifier_character()
	end, parse_do:chomp_if("\\.", not_dot)),
	parse:backtrackable(ParseAfterDot).

identifier_to_unicode(Characters) ->
	NoTags = lists:map(fun
		(ignore_mark) -> $_;
		({number_part, N}) -> N;
		(C) -> C
	end, Characters),
	case unicode:characters_to_binary(NoTags) of
		{error, _, ErrCar} ->
			{error, {non_unicode_name, ErrCar}};
		B ->
			{ok, B}
	end.

identifier_validate_module_part([]) ->
	{error, module_part_cannot_be_empty};
identifier_validate_module_part(Characters) ->
	identifier_to_unicode(Characters).

identifier_validiate_local_part([ignore_mark | _]) ->
	{error, local_part_cannot_be_ignored};

identifier_validiate_local_part(Characters) ->
	ValidRes = identifier_validate_local(Characters),
	result:map(fun({identifier_bound, I}) ->
		I
	end, ValidRes).

identifier_validate_local([ignore_mark | _] = Characters) ->
	result:map(fun(B) ->
		{identifier_ignored, B}
	end, identifier_to_unicode(Characters));
identifier_validate_local([{number_part, <<"-">>}]) ->
	{ok, {identifier_bound, <<"-">>}};
identifier_validate_local(Characters) ->
	IsNumberPart = fun({number_part, _}) -> true; (_) -> false end,
	IsNumberRes = case lists:all(IsNumberPart, Characters) of
		true ->
			{error, local_part_looks_like_a_number};
		false ->
			{ok, Characters}
	end,

	IsUnicodeRes = result:and_then(fun(C) ->
		identifier_to_unicode(C)
	end, IsNumberRes),

	IsReservedWordRes = result:and_then(fun(C) ->
		Reserved =
		[ <<"=">>
		, <<"->">>
		, <<".">>
		, <<",">>
		, <<",,">>
		, <<"[">>
		, <<"]">>
		, <<"{">>
		, <<"}">>
		, <<"(">>
		, <<")">>
		, <<"{-">>
		, <<"-}">>
		, <<"module">>
		, <<"function">>
		, <<"data">>
		, <<"when">>
		, <<"let">>
		, <<"class">>
		, <<"teach">>
		, <<"import">>
		, <<"spec">>
		, <<"expose">>
		, <<"expose all">>
		],
		case lists:member(C, Reserved) of
			true ->
				{error, {identifier_is_reserved_symbol, C}};
			false ->
				{ok, C}
		end
	end, IsUnicodeRes),

	result:map(fun(C) ->
		{identifier_bound, C}
	end, IsReservedWordRes).

identifier() ->
	parse:loop(init, fun identifier/1).

identifier(init) ->
	parse:map(fun(Start) ->
		{step, {part, [Start], _ModuleParts = []}}
	end, identifier_character());

identifier({part, Characters, ModuleParts}) ->
	NextPart = parse:map(fun(NewFirst) ->
		{step, {part, [NewFirst], [ lists:reverse(Characters) | ModuleParts]}}
	end, identifier_next_part()),
	NextCharacter = parse:map(fun(Char) ->
		{step, {part, [Char | Characters], ModuleParts}}
	end, identifier_character()),
	FinishUp = parse:success({step, {finish, [lists:reverse(Characters) | ModuleParts]}}),
	parse:first_of(
		[ NextPart
		, NextCharacter
		, FinishUp
		]);

identifier({finish, [ OnlyOnePart ]}) ->
	case identifier_validate_local(OnlyOnePart) of
		{ok, Local} ->
			parse:success({done, Local});
		{error, Err} ->
			parse:fail(Err)
	end;

identifier({finish, [ LocalPart | ModulePartsReversed]}) ->
	ModuleParts = lists:reverse(ModulePartsReversed),
	ModulePartsResult = result:and_then_all(fun(ModulePart) ->
		identifier_validate_module_part(ModulePart)
	end, ModuleParts),
	ModulePartResult = result:map(fun(Parts) ->
		unicode:characters_to_binary(lists:join($., Parts))
	end, ModulePartsResult),
	AllRes = result:and_then(fun(ValidModulePart) ->
		result:map(fun(ValidLocalPart) ->
			{identifier_bound, #{ module => ValidModulePart, local => ValidLocalPart}}
		end, identifier_validiate_local_part(LocalPart))
	end, ModulePartResult),
	case AllRes of
		{ok, Good} ->
			parse:success({done, Good});
		{error, Err} ->
			parse:fail(Err)
	end.

identifier_bindable() ->
	identifier().
identifier_bound() ->
	parse:and_then(fun
		({identifier_bound, _} = B) ->
			parse:success(B);
		(_) ->
			parse:fail(invalid_bound_identifier)
	end, identifier()).

identifier_ignored() ->
	FirstChomp = parse_do:chomp_if("_", identifier_not_ignored),
	NextChomps = parse_do:chomp_while("[^\\.'\"\\[\\]\\{\\}\\(\\),\\s]"),
	AllChomps = parse:and_then(fun(_) ->
		NextChomps
	end, FirstChomp),
	AsString = parse:get_chomped_string(AllChomps),
	parse:map(fun(Id) ->
		{identifier_ignored, Id}
	end, AsString).

identifier_local() ->
	parse:and_then(fun
		({identifier_bound, B}) when is_binary(B) ->
			parse:success(B);
		(_) ->
			parse:fail(identifier_not_local)
	end, identifier()).

record_field_accessor() ->
	parse:pipeline(parse:success(fun(Name) ->
		Name
	end),
		[ parse:discard(syntax_open_record())
		, parse:discard(syntax_close_record())
		, parse:keep(identifier_local())
		]).

-spec literal_string() -> parse:parser(term(), literal_string()).
literal_string() ->
	parse:loop(opening_quotes, fun literal_string/1).

literal_string(opening_quotes) ->
	QuoteOpen = parse_do:chomp_if("\"", no_opening_quotes),
	parse:map(fun(_) ->
		{step, {string_chomping, []}}
	end, QuoteOpen);

literal_string({string_chomping, Characters}) ->
	Chomped = parse:chomp_if(fun(<<>>) -> false; (_) -> true end, string_never_closed),
	ChompedStr = parse:get_chomped_string(Chomped),
	parse:map(fun
		(<<"\\">>) ->
			{step, {escaped_grapheme, Characters}};
		(<<"\"">>) ->
			{step, {string_closed, Characters}};
		(C) ->
			{step, {string_chomping, [C | Characters]}}
	end, ChompedStr);

literal_string({escaped_grapheme, Characters}) ->
	Chomped = parse:chomp_if(compose:always(true), never),
	ChompedChar = parse:get_chomped_string(Chomped),
	parse:map(fun(C) ->
		{step, {string_chomping, [C | Characters]}}
	end, ChompedChar);

literal_string({string_closed, Characters}) ->
	String = unicode:characters_to_binary(lists:reverse(Characters)),
	parse:success({done, String}).

-spec literal_float() -> parse:parser(term(), literal_float() | literal_integer()).
literal_float() ->
	literal_number().

-spec literal_integer() -> parse:parser(term(), literal_integer() | literal_float()).
literal_integer() ->
	literal_number().

-spec literal_number() -> parse:parser(term(), literal_integer() | literal_float()).
literal_number() ->
	DetermineSign = parse:first_of(
		[ parse:get_chomped_string(parse:chomp_string(<<"-">>))
		, parse:get_chomped_string(parse:chomp_string(<<"+">>))
		, parse:success(<<"+">>)
		]),
	FirstDigit = parse:get_chomped_string(parse_do:chomp_if("[0-9]", expected_digit)),
	HeadDigits = parse:get_chomped_string(parse_do:chomp_while("[0-9]")),

	% mayhaps we have a float?
	HasTail = parse:and_then(fun(Dot) ->
		parse:map_n(fun(Tail1, TailRest) ->
			[Dot, Tail1, TailRest]
		end, [parse:get_chomped_string(parse_do:chomp_if("[0-9]")), parse:get_chomped_string(parse_do:chomp_while("[0-9]"))])
	end, parse:get_chomped_string(parse:chomp_if(fun(C) -> C =:= <<".">> end, expected_dot))),

	DetermineTail = parse:first_of(
		[ parse:backtrackable(parse:get_chomped_string(HasTail))
		, parse:success([])
		]),

	Final = fun
		(Sign, Digit1, Digits, MaybeTail) ->
			Str = unicode:characters_to_binary([Sign, Digit1, Digits, MaybeTail]),
			case MaybeTail of
				[] ->
					binary_to_integer(Str);
				_ ->
					binary_to_float(Str)
			end
	end,

	parse:map_n(Final,
		[ DetermineSign
		, FirstDigit
		, HeadDigits
		, DetermineTail
		]).


-spec syntax_implies() -> parse:parser(term(), syntax_implies()).
syntax_implies()  ->
	parse:chomp_string(<<"->">>).

-spec syntax_infix_indicator() -> parse:parser(term(), syntax_infix_indicator()).
syntax_infix_indicator() ->
	parse_do:chomp_if("'", {expected_infix_indicator}).

-spec any_syntax_infix_indicator() -> parse:parser(term(), [ syntax_infix_indicator()]).
any_syntax_infix_indicator() ->
	Chomped = parse_do:chomp_while("'"),
	Indicators = parse:get_chomped_string(Chomped),
	parse:map(fun(IndicatorsStr) ->
		parse_ctx:count_graphemes(IndicatorsStr)
	end, Indicators).

-spec syntax_dot() -> parse:parser(term(), syntax_dot()).
syntax_dot() ->
	parse:chomp_string(<<".">>).

syntax_cons() ->
	parse:chomp_string(<<",,">>).

-spec syntax_element_seperator() -> parse:parser(term(), syntax_element_seperator()).
syntax_element_seperator() ->
	parse_do:chomp_if(",", expected_comma).

-type oc_parser() :: char() | unicode:chardata() | parse:parser(any(), any()).
-spec
	syntax_open(oc_parser(), list) -> parse:parser(term(), syntax_open_list())
		; (oc_parser(), record) -> parse:parser(term(), syntax_open_record())
		; (oc_parser(), subexpression) -> parse:parser(term(), syntax_open_subexpression()).

syntax_open(Char, Type) when is_integer(Char) ->
	syntax_open(parse:chomp_if(fun(C) ->
		C =:= <<Char>>
	end, {expected, Type, Char}), Type);

syntax_open(Parser, Type) ->
	parse:map(fun(_) -> Type end, Parser).

-spec
	syntax_close(oc_parser(), list) -> parse:parser(term(), syntax_close_list())
		; (oc_parser(), record) -> parse:parser(term(), syntax_close_record())
		; (oc_parser(), subexpression) -> parse:parser(term(), syntax_close_subexpression()).

syntax_close(Char, Type) when is_integer(Char) ->
	syntax_close(parse:chomp_if(fun(C) ->
		C =:= <<Char>>
	end, {expected, Type, Char}), Type);

syntax_close(Parser, Type) ->
	parse:map(fun(_) -> Type end, Parser).

-spec syntax_open_list() -> parse:parser(term(), syntax_open_list()).
syntax_open_list() ->
	syntax_open($[, list).

-spec syntax_close_list() -> parse:parser(term(), syntax_close_list()).
syntax_close_list() ->
	syntax_close($], list).

-spec syntax_open_record() -> parse:parser(term(), syntax_open_record()).
syntax_open_record() ->
	syntax_open(${, record).

-spec syntax_close_record() -> parse:parser(term(), syntax_close_record()).
syntax_close_record() ->
	syntax_close($}, record).

-spec syntax_open_subexpression() -> parse:parser(term(), syntax_open_subexpression()).
syntax_open_subexpression() ->
	syntax_open($(, subexpression).

-spec syntax_close_subexpression() -> parse:parser(term(), syntax_close_subexpression()).
syntax_close_subexpression() ->
	syntax_close($), subexpression).

-spec syntax_bind() -> parse:parser(term(), syntax_bind()).
syntax_bind() ->
	parse:chomp_if(fun(C) -> C =:= <<"=">> end, not_syntax_bind).

-spec eof() -> parse:parser(term(), eof()).
eof() ->
	parse:end_of_input().
