-module(milang_parse).

-export([file/1, string/1]).

-type milang_ast() :: [ milang_ast:ast_node() ].

-spec string(unicode:unicode_binary()) -> {ok, milang_ast(), unicode:unicode_binary()} | {error, tuple()}.
string(Binary) ->
	parse:it(Binary, milang_p:module()).

-spec file(file:filename()) -> {ok, milang_ast()} | {error, tuple()}.
file(Filename) ->
	case file:open(Filename, [read, binary]) of
		{ok, Handle} ->
			parse_file(Handle);
		{error, Error} ->
			{error, {invalid_file, Error}}
	end.

-define(chunk_size, 10000).

-spec parse_file(file:io_device()) -> {ok, milang_ast()} | {error, tuple()}.
parse_file(Handle) ->
	parse_file("", Handle, _AstAcc = [], _StringLeft = <<>>).

-spec parse_file(unicode:unicode_binary() | io:server_no_data() | string(), file:io_device(), milang_ast(), unicode:unicode_binary()) -> {ok, milang_ast()} | {error, term()}.
parse_file(eof, _Handle, AstAcc, <<>>) ->
	{ok, AstAcc};
parse_file(eof, _Handle, _AstAcc, LeftOverChars) ->
	{error, {trailing_characters, LeftOverChars}};
parse_file({error, Error}, _Handle, _AstAcc, _LeftOverChars) ->
	{error, {read_error, Error}};
parse_file(Data, Handle, AstAcc, LeftOverChars) ->
	String = unicode:characters_to_binary([Data, LeftOverChars]),
	{ok, NewParts, NewLeftOverChars} = string(String),
	NewRead = io:get_chars(Handle, <<>>, ?chunk_size),
	parse_file(NewRead, Handle, AstAcc ++ NewParts, NewLeftOverChars).
