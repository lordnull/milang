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
-type literal_float() :: {literal_float, milang_ast:location(), float()}.
-type literal_integer() :: {literal_integer, milang_ast:location(), integer()}.
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
	, keyword_with/0
	, keyword_class/0
	, keyword_teach/0
	, keyword_as/0
	, whitespace/0
	, comment/0
	, literal_float/0
	, literal_integer/0
	, literal_string/0
	, identifier_ignored/0
	, identifier_bound/0
	, identifier_bindable/0
	, syntax_bind/0
	, syntax_implies/0
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
		, keyword_data()
		, keyword_spec()
		, keyword_let()
		, keyword_function()
		, keyword_match()
		, keyword_when()
		, keyword_with()
		, keyword_class()
		, keyword_as()
		, keyword_teach()
		, keyword_expose_all()
		, keyword_expose()
		, whitespace()
		, comment()
		, literal_float()
		, literal_integer()
		, literal_string()
		, syntax_bind()
		, syntax_implies()
		, syntax_infix_indicator()
		, syntax_dot()
		, syntax_cons()
		, syntax_element_seperator()
		, syntax_open_list()
		, syntax_close_list()
		, syntax_open_record()
		, syntax_close_record()
		, syntax_open_subexpression()
		, syntax_close_subexpression()
		, identifier_bound()
		, identifier_ignored()
		]),
	UntilEnd = parse:in_context(root, parse_do:repeat_until(eof(), FirstOf)),
	parse:map(fun(#{ list := Elements, ended := End }) ->
		Elements ++ [End]
	end, UntilEnd).

grammer_space(AsAtom) ->
	FollowedBy = parse:first_of(
		[ comment()
		, whitespace()
		, eof()
		, parse:chomp_if($.)
		, parse:is_string(<<",,">>)
		, parse:chomp_if($,)
		, parse:chomp_if($()
		, parse:chomp_if($))
		, parse:chomp_if($[)
		, parse:chomp_if($])
		, parse:chomp_if(${)
		, parse:chomp_if($})
		]),
	Test = parse:peek(FollowedBy),
	Keyword = parse:is_string(atom_to_binary(AsAtom, utf8)),
	parse:map_n(fun(_, _) ->
		AsAtom
	end, [Keyword, Test]).

keyword(AsAtom) ->
	parse:in_context(keyword, parse_do:tag(keyword, grammer_space(AsAtom))).

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
	parse:in_context(whitespace, parse_do:tag(whitespace, parse:regex("\\s+"))).

-spec comment() -> parse:parser(term(), comment()).
comment() ->
	Characters = parse_do:enclosed(parse:is_string(<<"{-">>), parse:is_string(<<"-}">>), parse:chomp()),
	parse:in_context(comment, parse_do:tag(comment, Characters)).


identifier_local() ->
	NeverValid = "\\.'\"\\[\\]\\{\\}\\(\\),\\s",
	ValidFirstCharacters = ["[^_", NeverValid, "]"],
	ValidTailCharacters = ["[^", NeverValid, "]"],
	Regex = [ValidFirstCharacters, ValidTailCharacters, "*"],
	RegexParse = parse:map_err(fun(nomatch) ->
		invalid_identifier
	end, parse:regex(Regex)),
	Validated = parse:in_context(validating_identifier, parse:and_then(fun
		([<<"=">>]) ->
			parse:fail(solo_equals_invalid);
		(SomeString) ->
			IntegerRegex = "(-|\\+)?[0-9]+$",
			case re:run(SomeString, IntegerRegex, [anchored]) of
				nomatch ->
					parse:success(SomeString);
				{match, _} ->
					parse:fail(identifier_looks_like_an_integer)
			end
	end, RegexParse)),
	Squished = parse:map(fun unicode:characters_to_binary/1, Validated),
	parse:in_context(identifier_local, Squished).

identifier_bound() ->
	InContext = parse:in_context(identifier_bound, identifier_bound(get_local, [])),
	parse_do:tag(identifier_bound, InContext).

identifier_bound(get_local, Acc) ->
	parse:and_then(fun(Local) ->
		io:format("get_local ~p~n", [Local]),
		identifier_bound(dot_or_space, [Local | Acc])
	end, identifier_local());

identifier_bound(dot_or_space, Acc) ->
	DotTest = parse:chomp_if($.),
	IfDot = fun(_) ->
		io:format("dot_or_space, is dot~n", []),
		identifier_bound(space_or_local, Acc)
	end,

	SpaceTest = whitespace(),
	IfSpace = fun(_) ->
		io:format("dot_or_space, is space~n", []),
		identifier_bound(finalize, Acc)
	end,

	EndOfInput = parse:end_of_input(),
	IfEndOfInput = fun(_) ->
		io:format("dot_or_space, is eof~n", []),
		identifier_bound(finalize, Acc)
	end,

	Branching = parse_do:branch(
		[ { DotTest, IfDot}
		, { SpaceTest, IfSpace}
		, { EndOfInput, IfEndOfInput}
		]),
	parse:map_err(fun
		({no_branches_matched, _}) ->
			expected_dot_or_space;
		(Error) ->
			Error
	end, Branching);

identifier_bound(space_or_local, Acc) ->
	SpaceTest = whitespace(),
	IfSpace = fun(_) ->
		io:format("space_or_local, is space~n", []),
		identifier_bound(finalize, Acc)
	end,
	Space = {SpaceTest, IfSpace},

	EndOfInputTest = parse:end_of_input(),
	IfEndOfInput = fun(_) ->
		io:format("space_or_local, is end of input~n", []),
		identifier_bound(finalize, Acc)
	end,
	EndOfInput = { EndOfInputTest, IfEndOfInput},

	Else = {parse:success(ok), fun(ok) ->
		io:format("space_or_local, trying local as is not space~n", []),
		identifier_bound(get_local, Acc)
	end},

	Branching = parse_do:branch([ Space, EndOfInput, Else ]),
	parse:map_err(fun
		({no_branches_matched, [LocalBranch | _]}) ->
			LocalBranch;
		(Error) ->
			Error
	end, Branching);

identifier_bound(finalize, []) ->
	io:format("finalize none~n"),
	parse:fail(no_parts);
identifier_bound(finalize, [JustOne]) ->
	io:format("finalize, just one: ~p", [JustOne]),
	parse:success(JustOne);
identifier_bound(finalize, [Local | RevModule]) ->
	io:format("finalized local ~p and module parts ~p~n", [Local, RevModule]),
	Module = lists:join($., lists:reverse(RevModule)),
	parse:success(#{ local => Local, module => unicode:characters_to_binary(Module)}).

identifier_ignored() ->
	GetIgnoreMark = parse:chomp_if($_),
	GetIdent = parse:and_then(fun(_) ->
		identifier_regex()
	end, GetIgnoreMark),
	parse:in_context(identifier_ignored, parse_do:tag(identifier_ignored, GetIdent)).

identifier_regex() ->
	Regex = "[^'\"\\[\\]\\{\\}\\(\\),\\.\\s]*",
	parse:map(fun([W]) ->
		W
	end, parse:regex(Regex)).

identifier_bindable() ->
	parse:first_of(
		[ parse_do:tag(identifier_bound, identifier_local())
		, identifier_ignored()
		]).

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
	MappedRegex = parse:map(fun([_, S]) ->
		S
	end, ParsedRegex),
	parse:in_context(literal_string, parse_do:tag(literal_string, MappedRegex)).

-spec literal_float() -> parse:parser(term(), literal_float()).
literal_float() ->
	Regex = "((-|\\+)?[0-9]+\\.[0-9]+)",
	InCtx = parse:in_context(?FUNCTION_NAME, parse:regex(Regex)),
	AsFloat = parse:map(fun([FullMatch, _]) ->
		AsString = unicode:characters_to_binary(FullMatch),
		binary_to_float(AsString)
	end, InCtx),
	parse_do:tag(literal_float, AsFloat).

-spec literal_integer() -> parse:parser(term(), literal_integer()).
literal_integer() ->
	Regex = "((-|\\+)?[0-9]+)",
	InCtx = parse:in_context(?FUNCTION_NAME, parse:regex(Regex)),
	AsInteger = parse:map(fun([FullMatch, _]) ->
		AsBinary = unicode:characters_to_binary(FullMatch),
		binary_to_integer(AsBinary)
	end, InCtx),
	parse_do:tag(literal_integer, AsInteger).

-spec syntax_implies() -> parse:parser(term(), syntax_implies()).
syntax_implies()  ->
	parse:in_context(syntax_implies, parse_do:tag(syntax_implies, grammer_space('->'))).

-spec syntax_infix_indicator() -> parse:parser(term(), syntax_infix_indicator()).
syntax_infix_indicator() ->
	parse:in_context(?FUNCTION_NAME, parse_do:tag(syntax_infix_indicator, parse:chomp_if($'))).

-spec syntax_dot() -> parse:parser(term(), syntax_dot()).
syntax_dot() ->
	parse:in_context(?FUNCTION_NAME, parse_do:tag(syntax_dot, grammer_space('.'))).

syntax_cons() ->
	parse:in_context(?FUNCTION_NAME, parse_do:tag(syntax_cons, parse:is_string(<<",,">>))).

-spec syntax_element_seperator() -> parse:parser(term(), syntax_element_seperator()).
syntax_element_seperator() ->
	parse:in_context(?FUNCTION_NAME, parse_do:tag(syntax_element_seperator, parse:chomp_if($,))).

-type oc_parser() :: char() | unicode:chardata() | parse:parser(any(), any()).
-spec
	syntax_open(oc_parser(), list) -> parse:parser(term(), syntax_open_list())
		; (oc_parser(), record) -> parse:parser(term(), syntax_open_record())
		; (oc_parser(), subexpression) -> parse:parser(term(), syntax_open_subexpression()).

syntax_open(Char, Type) when is_integer(Char) ->
	syntax_open(parse:chomp_if(Char), Type);

syntax_open(Bin, Type) when is_binary(Bin) ->
	syntax_open(parse:is_string(Bin), Type);

syntax_open(Parser, Type) ->
	Mapped = parse:map(fun(_) -> Type end, Parser),
	parse_do:tag(syntax_open, Mapped).

-spec
	syntax_close(oc_parser(), list) -> parse:parser(term(), syntax_close_list())
		; (oc_parser(), record) -> parse:parser(term(), syntax_close_record())
		; (oc_parser(), subexpression) -> parse:parser(term(), syntax_close_subexpression()).

syntax_close(Char, Type) when is_integer(Char) ->
	syntax_close(parse:chomp_if(Char), Type);

syntax_close(Bin, Type) when is_binary(Bin) ->
	syntax_close(parse:is_string(Bin), Type);

syntax_close(Parser, Type) ->
	Mapped = parse:map(fun(_) -> Type end, Parser),
	parse_do:tag(syntax_close, Mapped).

-spec syntax_open_list() -> parse:parser(term(), syntax_open_list()).
syntax_open_list() ->
	parse:in_context(?FUNCTION_NAME, syntax_open($[, list)).

-spec syntax_close_list() -> parse:parser(term(), syntax_close_list()).
syntax_close_list() ->
	parse:in_context(?FUNCTION_NAME, syntax_close($], list)).

-spec syntax_open_record() -> parse:parser(term(), syntax_open_record()).
syntax_open_record() ->
	parse:in_context(?FUNCTION_NAME, syntax_open(<<"{">>, record)).

-spec syntax_close_record() -> parse:parser(term(), syntax_close_record()).
syntax_close_record() ->
	parse:in_context(?FUNCTION_NAME, syntax_close(<<"}">>, record)).

-spec syntax_open_subexpression() -> parse:parser(term(), syntax_open_subexpression()).
syntax_open_subexpression() ->
	parse:in_context(?FUNCTION_NAME, syntax_open($(, subexpression)).

-spec syntax_close_subexpression() -> parse:parser(term(), syntax_close_subexpression()).
syntax_close_subexpression() ->
	parse:in_context(?FUNCTION_NAME, syntax_close($), subexpression)).

-spec syntax_bind() -> parse:parser(term(), syntax_bind()).
syntax_bind() ->
	parse:in_context(?FUNCTION_NAME, parse_do:tag(syntax_bind, grammer_space('='))).

-spec eof() -> parse:parser(term(), eof()).
eof() ->
	parse:in_context(?FUNCTION_NAME, parse_do:tag(eof, parse:end_of_input())).
