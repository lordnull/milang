-module(milang_p_token).

-type declaration_module() :: {declaration_module, milang_ast:location(), unicode:chardata()}.
-type declaration_import() :: {declaration_import, milang_ast:location(), unicode:chardata()}.
-type declaration_type() :: {declaration_type, milang_ast:location(), unicode:chardata()}.
-type declaration_alias() :: {declaration_alias, milang_ast:location(), unicode:chardata()}.
-type declaration_class() :: {declaration_class, milang_ast:location(), unicode:chardata()}.
-type whitespace() :: {whitespace, milang_ast:location(), unicode:chardata()}.
-type comment() :: {comment, milang_ast:location(), unicode:chardata()}.
-type name_upcase() :: {name_update, milang_ast:location(), unicode:chardata()}.
-type name_downcase() :: {name_downcase, milang_ast:location(), unicode:chardata()}.
-type name_underscore() :: {name_underscore, milang_ast:location(), unicode:chardata()}.
-type name_symbol() :: {name_symbol, milang_ast:location(), unicode:chardata()}.
-type name_symbol_quoted() :: {name_symbol_quoted, milang_ast:location(), unicode:chardata()}.
-type literal_float() :: {literal_float, milang_ast:location(), float()}.
-type literal_integer() :: {literal_integer, milang_ast:location(), integer()}.
-type literal_string() :: {literal_string, milang_ast:location(), unicode:chardata()}.
-type syntax_implies() :: {syntax_implies, milang_ast:location(), unicode:chardata()}.
-type syntax_infix_left() :: {syntax_infix_left, milang_ast:location(), unicode:chardata()}.
-type syntax_infix_right() :: {syntax_infix_right, milang_ast:location(), unicode:chardata()}.
-type syntax_keyword() :: {syntax_keyword, milang_ast:location(), 'when' | 'exposing' | 'match' | 'as'}.
-type syntax_dot() :: {syntax_dot, milang_ast:location(), unicode:chardata()}.
-type syntax_element_seperator() :: {syntax_element_seperator, milang_ast:location(), unicode:chardata()}.
-type syntax_open(T) :: {syntax_open, milang_ast:location(), T}.
-type syntax_close(T) :: {syntax_close, milang_ast:location(), T}.
-type syntax_open_list() :: syntax_open(list).
-type syntax_close_list() :: syntax_close(list).
-type syntax_open_map() :: syntax_open(map).
-type syntax_close_map() :: syntax_close(map).
-type syntax_open_record() :: syntax_open(record).
-type syntax_close_record() :: syntax_close(record).
-type syntax_open_subexpression() :: syntax_open(subexpression).
-type syntax_close_subexpression() :: syntax_close(subexpression).
-type syntax_bind() :: {syntax_bind, milang_ast:location(), unicode:chardata()}.
-type syntax_spec() :: {syntax_spec, milang_ast:location(), unicode:chardata()}.
-type eof() :: {token_name, milang_ast:location(), unicode:chardata()}.

-type token() ::
	  declaration_module()
	| declaration_import()
	| declaration_type()
	| declaration_alias()
	| declaration_class()
	| whitespace()
	| comment()
	| name_upcase()
	| name_downcase()
	| name_underscore()
	| name_symbol()
	| name_symbol_quoted()
	| literal_float()
	| literal_integer()
	| literal_string()
	| syntax_implies()
	| syntax_infix_left()
	| syntax_infix_right()
	| syntax_keyword()
	| syntax_dot()
	| syntax_element_seperator()
	| syntax_open_list()
	| syntax_close_list()
	| syntax_open_map()
	| syntax_close_map()
	| syntax_open_record()
	| syntax_close_record()
	| syntax_open_subexpression()
	| syntax_close_subexpression()
	| syntax_bind()
	| syntax_spec()
	| eof()
	.

-export_type([ token/0 ]).

-export(
	[ declaration_module/0
	, declaration_import/0
	, declaration_type/0
	, declaration_alias/0
	, declaration_class/0
	, whitespace/0
	, comment/0
	, name_upcase/0
	, name_downcase/0
	, name_underscore/0
	, name_symbol/0
	, name_symbol_quoted/0
	, literal_float/0
	, literal_integer/0
	, literal_string/0
	, syntax_implies/0
	, syntax_infix_left/0
	, syntax_infix_right/0
	, syntax_keyword/0
	, syntax_dot/0
	, syntax_element_seperator/0
	, syntax_open_list/0
	, syntax_close_list/0
	, syntax_open_map/0
	, syntax_close_map/0
	, syntax_open_record/0
	, syntax_close_record/0
	, syntax_open_subexpression/0
	, syntax_close_subexpression/0
	, syntax_bind/0
	, syntax_spec/0
	, eof/0
	, tokens/0
	]).

-spec tokens() -> parse:parser(term(), [ token() ]).
tokens() ->
	FirstOf = parse:first_of(
		[ declaration_module()
		, declaration_import()
		, declaration_type()
		, declaration_alias()
		, declaration_class()
		, whitespace()
		, comment()
		, literal_float()
		, literal_integer()
		, literal_string()
		, syntax_implies()
		, syntax_infix_left()
		, syntax_infix_right()
		, syntax_keyword()
		, syntax_dot()
		, syntax_element_seperator()
		, syntax_open_list()
		, syntax_close_list()
		, syntax_open_map()
		, syntax_close_map()
		, syntax_open_record()
		, syntax_close_record()
		, syntax_open_subexpression()
		, syntax_close_subexpression()
		, syntax_bind()
		, syntax_spec()
		, name_upcase()
		, name_downcase()
		, name_underscore()
		, name_symbol()
		, name_symbol_quoted()
	]),
	UntilEnd = parse:repeat_until(FirstOf, eof()),
	parse:map(UntilEnd, fun({E, End}) ->
		E ++ [End]
	end).

-spec declaration_module() -> parse:parser(term(), declaration_module()).
declaration_module() ->
	parse:tag(declaration_module, parse:string(<<"-module">>)).

-spec declaration_import() -> parse:parser(term(), declaration_import()).
declaration_import() ->
	parse:tag(declaration_import, parse:string(<<"-import">>)).

-spec declaration_type() -> parse:parser(term(), declaration_type).
declaration_type() ->
	parse:tag(declaration_type, parse:string(<<"-type">>)).

-spec declaration_alias() -> parse:parser(term(), declaration_alias()).
declaration_alias() ->
	parse:tag(declaration_alias, parse:string(<<"-alias">>)).

-spec declaration_class() -> parse:parser(term(), declaration_class()).
declaration_class() ->
	parse:tag(declaration_class, parse:string(<<"-class">>)).

-spec whitespace() -> parse:parser(term(), whitespace()).
whitespace() ->
	parse:tag(whitespace, parse:regex("\\s+")).

-spec comment() -> parse:parser(term(), comment()).
comment() ->
	Ending = parse:string(<<"-}">>),
	MaybeNested = parse:first_of([parse:lazy(fun comment/0), parse:chomp()]),
	Repeat = parse:repeat_until(MaybeNested, Ending),
	Series = parse:series([parse:string(<<"{-">>), Repeat]),
	Mapper = fun([_, {Doc, _}]) ->
		JustChars = lists:map(fun(D) ->
			case D of
				{_, _, C} -> ["{-", C, "-}"];
				_ -> D
			end
		end, Doc),
		unicode:characters_to_binary(JustChars)
	end,
	Mapped = parse:map(Series, Mapper),
	parse:tag(comment, Mapped).

-spec name_downcase() -> parse:parser(term(), name_downcase()).
name_downcase() ->
	RegEx = parse:regex("\\p{Ll}[\\w]*", first),
	parse:tag(name_downcase, RegEx).

-spec name_upcase() -> parse:parser(term(), name_upcase()).
name_upcase() ->
	RegEx = parse:regex("\\p{Lu}[\\w]*", first),
	parse:tag(name_upcase, RegEx).

-spec name_underscore() -> parse:parser(term(), name_underscore()).
name_underscore() ->
	RegEx = parse:regex("_([\\p{Ll}][\\w]*)?", first),
	parse:tag(name_underscore, RegEx).

-spec name_symbol() -> parse:parser(term(), name_symbol()).
name_symbol() ->
	DisallowedSet = ordsets:from_list("'\"[]{}(),«»:_"),
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
	parse:tag(name_symbol, ValueCheckP).

-spec name_symbol_quoted() -> parse:parser(term(), name_symbol_quoted()).
name_symbol_quoted() ->
	Series = parse:series([parse:character($'), name_symbol(), parse:character($')]),
	Tagged = parse:tag(name_symbol_quoted, Series),
	parse:map(Tagged, fun({T, L, [_, S, _]}) ->
		{_, _, N} = S,
		{T, L, N}
	end).

-spec literal_string() -> parse:parser(term(), literal_string()).
literal_string() ->
	% I apologize for all the slashes.
	% The original does not need to escape the double quotes. Nor did it need to
	% escape the backslashes from another parser before it could get to the
	% regex parser.
	% from https://stackoverflow.com/questions/249791/regex-for-quoted-string-with-escaping-quotes:
	%     /"(?:[^"\\]|\\.)*"
	% I've also extended it so it captures the inner string.
	Regex = <<"\"((?:[^\"\\\\]|\\\\.)*)\"">>,
	ParsedRegex = parse:regex(Regex),
	MappedRegex = parse:map(ParsedRegex, fun([_, S]) ->
		S
	end),
	parse:tag(literal_string, MappedRegex).

-spec literal_float() -> parse:parser(term(), literal_float()).
literal_float() ->
	Sign = parse:optional(parse:first_of([parse:character($-), parse:character($+)])),
	NumberRegex = parse:regex("[0-9]+"),
	Series = parse:series([Sign, NumberRegex, parse:character($.), NumberRegex]),
	Tagged = parse:tag(literal_float, Series),
	parse:map(Tagged, fun({T, L, Chars}) ->
		AsString = unicode:characters_to_binary(Chars),
		AsFloat = binary_to_float(AsString),
		{T, L, AsFloat}
	end).

-spec literal_integer() -> parse:parser(term(), literal_integer()).
literal_integer() ->
	Sign = parse:optional(parse:first_of([parse:character($-), parse:character($+)])),
	NumberRegex = parse:regex("[0-9]+"),
	Series = parse:series([Sign, NumberRegex]),
	Tagged = parse:tag(literal_integer, Series),
	parse:map(Tagged, fun({T, L, Chars}) ->
		AsBinary = unicode:characters_to_binary(Chars),
		AsInt = binary_to_integer(AsBinary, 10),
		{T, L, AsInt}
	end).

-spec syntax_implies() -> parse:parser(term(), syntax_implies()).
syntax_implies()  ->
	parse:tag(syntax_implies, parse:string(<<"->">>)).

-spec syntax_infix_left() -> parse:parser(term(), syntax_infix_left()).
syntax_infix_left() ->
	parse:tag(syntax_infix_left, parse:regex(<<"»+"/utf8>>, first)).

-spec syntax_infix_right() -> parse:parser(term(), syntax_infix_right()).
syntax_infix_right() ->
	parse:tag(syntax_infix_right, parse:regex(<<"«+"/utf8>>, first)).

-spec syntax_keyword() -> parse:parser(term(), syntax_keyword()).
syntax_keyword() ->
	Tagged = parse:tag(syntax_keyword, parse:regex(<<"(when|match|exposing|as)">>, first)),
	parse:map(Tagged, fun({T, L, Iolist}) ->
		{T, L, binary_to_atom(unicode:characters_to_binary(Iolist), utf8)}
	end).

-spec syntax_dot() -> parse:parser(term(), syntax_dot()).
syntax_dot() ->
	parse:tag(syntax_dot, parse:character($.)).

-spec syntax_element_seperator() -> parse:parser(term(), syntax_element_seperator()).
syntax_element_seperator() ->
	parse:tag(syntax_element_seperator, parse:character($,)).

-type oc_parser() :: char() | unicode:chardata() | parse:parser(any(), any()).
-spec
	syntax_open(oc_parser(), list) -> parse:parser(term(), syntax_open_list())
		; (oc_parser(), map) -> parse:parser(term(), syntax_open_map())
		; (oc_parser(), record) -> parse:parser(term(), syntax_open_record())
		; (oc_parser(), subexpression) -> parse:parser(term(), syntax_open_subexpression()).

syntax_open(Char, Type) when is_integer(Char) ->
	syntax_open(parse:character(Char), Type);

syntax_open(Bin, Type) when is_binary(Bin) ->
	syntax_open(parse:string(Bin), Type);

syntax_open(Parser, Type) ->
	Mapped = parse:map(Parser, fun(_) -> Type end),
	parse:tag(syntax_open, Mapped).

-spec
	syntax_close(oc_parser(), list) -> parse:parser(term(), syntax_close_list())
		; (oc_parser(), map) -> parse:parser(term(), syntax_close_map())
		; (oc_parser(), record) -> parse:parser(term(), syntax_close_record())
		; (oc_parser(), subexpression) -> parse:parser(term(), syntax_close_subexpression()).

syntax_close(Char, Type) when is_integer(Char) ->
	syntax_close(parse:character(Char), Type);

syntax_close(Bin, Type) when is_binary(Bin) ->
	syntax_close(parse:string(Bin), Type);

syntax_close(Parser, Type) ->
	Mapped = parse:map(Parser, fun(_) -> Type end),
	parse:tag(syntax_close, Mapped).

-spec syntax_open_list() -> parse:parser(term(), syntax_open_list()).
syntax_open_list() ->
	syntax_open($[, list).

-spec syntax_close_list() -> parse:parser(term(), syntax_close_list()).
syntax_close_list() ->
	syntax_close($], list).

-spec syntax_open_map() -> parse:parser(term(), syntax_open_map()).
syntax_open_map() ->
	syntax_open(<<"{=">>, map).

-spec syntax_close_map() -> parse:parser(term(), syntax_close_map()).
syntax_close_map() ->
	syntax_close(<<"=}">>, map).

-spec syntax_open_record() -> parse:parser(term(), syntax_open_record()).
syntax_open_record() ->
	syntax_open(<<"{">>, record).

-spec syntax_close_record() -> parse:parser(term(), syntax_close_record()).
syntax_close_record() ->
	syntax_close(<<"}">>, record).

-spec syntax_open_subexpression() -> parse:parser(term(), syntax_open_subexpression()).
syntax_open_subexpression() ->
	syntax_open($(, subexpression).

-spec syntax_close_subexpression() -> parse:parser(term(), syntax_close_subexpression()).
syntax_close_subexpression() ->
	syntax_close($), subexpression).

-spec syntax_bind() -> parse:parser(term(), syntax_bind()).
syntax_bind() ->
	parse:tag(syntax_bind, parse:character($=)).

-spec syntax_spec() -> parse:parser(term(), syntax_spec()).
syntax_spec() ->
	parse:tag(syntax_spec, parse:character($:)).

-spec eof() -> parse:parser(term(), eof()).
eof() ->
	parse:tag(eof, parse:end_of_input()).
