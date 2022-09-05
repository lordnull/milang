-module(milang_p_token).

-include_lib("kernel/include/logger.hrl").

-type mi_identifier()
	:: unicode:chardata()
	|  #{ local := unicode:chardata(), module := unicode:chardata() }
	.

-type keyword() :: {keyword, milang_ast:location(), atom()}.
-type whitespace() :: {whitespace, milang_ast:location(), unicode:chardata()}.
-type comment() :: {comment, milang_ast:location(), unicode:chardata()}.
-type identifier_bound() :: {identifier, milang_ast:location(), mi_identifier()}.
-type identifier_ignored() :: {identifier_ignored, milang_ast:location(), mi_identifier()}.
-type identifier_type() :: {identifier_type, milang_ast:location(), mi_identifier()}.
-type literal_float() :: {literal_float, milang_ast:location(), float()}.
-type literal_integer() :: {literal_integer, milang_ast:location(), integer()}.
-type literal_string() :: {literal_string, milang_ast:location(), unicode:chardata()}.
-type syntax_implies() :: {syntax_implies, milang_ast:location(), unicode:chardata()}.
-type syntax_infix_left() :: {syntax_infix_left, milang_ast:location(), unicode:chardata()}.
-type syntax_infix_right() :: {syntax_infix_right, milang_ast:location(), unicode:chardata()}.
-type syntax_infix_indicator() :: {syntax_infix_indicator, milang_ast:location(), unicode:chardata()}.
-type syntax_keyword() :: {syntax_keyword, milang_ast:location(), 'when' | 'exposing' | 'match' | 'as' | 'func'}.
-type syntax_dot() :: {syntax_dot, milang_ast:location(), unicode:chardata()}.
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
	| syntax_implies()
	| syntax_infix_left()
	| syntax_infix_right()
	| syntax_keyword()
	| syntax_dot()
	| syntax_element_seperator()
	| syntax_open_list()
	| syntax_close_list()
	| syntax_open_record()
	| syntax_close_record()
	| syntax_open_subexpression()
	| syntax_close_subexpression()
	| syntax_bind()
	| syntax_spec()
	| identifier_type()
	| identifier_ignored()
	| identifier_bound()
	| eof()
	.

-export_type([ token/0 ]).

-export(
	[ keyword_module/0
	, keyword_import/0
	, keyword_alias/0
	, keyword_spec/0
	, keyword_let/0
	, keyword_function/0
	, keyword_match/0
	, keyword_when/0
	, keyword_class/0
	, keyword_implements/0
	, keyword_as/0
	, whitespace/0
	, comment/0
	, literal_float/0
	, literal_integer/0
	, literal_string/0
	, identifier_ignored/0
	, identifier_type/0
	, identifier_bound/0
	, identifier_bindable/0
	, syntax_bind/0
	, syntax_implies/0
	, syntax_infix_left/0
	, syntax_infix_right/0
	, syntax_infix_indicator/0
	, syntax_dot/0
	, syntax_element_seperator/0
	, syntax_open_list/0
	, syntax_close_list/0
	, syntax_open_record/0
	, syntax_close_record/0
	, syntax_open_subexpression/0
	, syntax_close_subexpression/0
	, eof/0
	, tokens/0
	]).

-spec tokens() -> parse:parser(term(), [ token() ]).
tokens() ->
	FirstOf = parse:first_of(
		[ keyword_module()
		, keyword_import()
		, keyword_alias()
		, keyword_type()
		, keyword_spec()
		, keyword_let()
		, keyword_function()
		, keyword_match()
		, keyword_when()
		, keyword_class()
		, keyword_as()
		, keyword_implements()
		, keyword_expose()
		, keyword_expose_all()
		, whitespace()
		, comment()
		, literal_float()
		, literal_integer()
		, literal_string()
		, syntax_bind()
		, syntax_implies()
		, syntax_infix_left()
		, syntax_infix_right()
		, syntax_infix_indicator()
		, syntax_dot()
		, syntax_element_seperator()
		, syntax_open_list()
		, syntax_close_list()
		, syntax_open_record()
		, syntax_close_record()
		, syntax_open_subexpression()
		, syntax_close_subexpression()
		, identifier_bound()
		, identifier_ignored()
		, identifier_type()
		]),
	UntilEnd = parse:repeat_until(FirstOf, eof()),
	parse:map(UntilEnd, fun({E, End}) ->
		E ++ [End]
	end).

keyword(AsAtom) ->
	FollowedBy = parse:first_of([ comment(), whitespace() ]),
	Test = parse:test(FollowedBy),
	Keyword = parse:string(atom_to_binary(AsAtom, utf8)),
	Series = parse:series([Keyword, Test]),
	Mapped = parse:map(Series, fun([_, _]) ->
		AsAtom
	end),
	parse:tag(keyword, Mapped).

keyword_module() ->
	keyword(module).

keyword_type() ->
	keyword('type').

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

keyword_class() ->
	keyword('class').

keyword_implements() ->
	keyword('implements').

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
	parse:tag(whitespace, parse:regex("\\s+")).

-spec comment() -> parse:parser(term(), comment()).
comment() ->
	Repeat = parse:chomp_until(<<" -}">>),
	Series = parse:series([parse:string(<<"{- ">>), Repeat, parse:string(<<" -}">>)]),
	Mapper = fun([_, Doc, _]) ->
		unicode:characters_to_binary(Doc)
	end,
	Mapped = parse:map(Series, Mapper),
	parse:tag(comment, Mapped).

identifier_type() ->
	Regex = "[\\p{Lu}][^'\"\\[\\]\\{\\}\\(\\),«»\\.\\s]*",
	identifier(Regex, identifier_type).


identifier_bound() ->
	Regex = "[^'\"\\[\\]\\{\\}\\(\\),«»_\\.\\s\\d\\p{Lu}][^'\"\\[\\]\\{\\}\\(\\),«»\\.\\s]*",
	identifier(Regex, identifier_bound).

identifier(Regex, Tag) ->
	ModulePartRegex = "([^'\"\\[\\]\\{\\}\\(\\),«»\\.\\s]+\\.)*",
	FullRegex = [ModulePartRegex, Regex],
	ParseBase = parse:regex(FullRegex, first),
	Identifier = parse:map(ParseBase, fun(Iolist) ->
		AsBinary = unicode:characters_to_binary(Iolist),
		Split = string:split(AsBinary, ".", all),
		[Local | RemoteReversed] = lists:reverse(Split),
		case lists:join($., lists:reverse(RemoteReversed)) of
			[] ->
				Local;
			Remote ->
				#{ local => Local, module => unicode:characters_to_binary(Remote) }
		end
	end),
	Tagged = parse:tag(Tag, Identifier),
	parse:andThen(Tagged, fun
		({_, _, <<"=">>}) ->
			parse:fail(solo_equals_invalid);
		({_, _, #{ local := <<"=">>}}) ->
			parse:fail(solo_equals_invalid);
		(V) ->
			parse:success(V)
	end).

identifier_ignored() ->
	Regex = "_[^'\"\\[\\]\\{\\}\\(\\),«»_\\.\\s]*",
	ParseBase = parse:regex(Regex, first),
	parse:tag(identifier_ignored, ParseBase).

identifier_bindable() ->
	parse:first_of([ identifier_bound(), identifier_ignored() ]).

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


-spec syntax_infix_indicator() -> parse:parser(term(), syntax_infix_indicator()).
syntax_infix_indicator() ->
	parse:tag(syntax_infix_indicator, parse:character($')).

-spec syntax_dot() -> parse:parser(term(), syntax_dot()).
syntax_dot() ->
	parse:tag(syntax_dot, parse:character($.)).

-spec syntax_element_seperator() -> parse:parser(term(), syntax_element_seperator()).
syntax_element_seperator() ->
	parse:tag(syntax_element_seperator, parse:character($,)).

-type oc_parser() :: char() | unicode:chardata() | parse:parser(any(), any()).
-spec
	syntax_open(oc_parser(), list) -> parse:parser(term(), syntax_open_list())
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

-spec eof() -> parse:parser(term(), eof()).
eof() ->
	parse:tag(eof, parse:end_of_input()).
