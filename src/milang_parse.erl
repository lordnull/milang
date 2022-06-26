-module(milang_parse).

-export([file/1, string/1]).

-type milang_ast() :: [ milang_ast:ast_node() ].

-spec string(unicode:unicode_binary()) -> {ok, milang_ast(), unicode:unicode_binary()} | {error, term()}.
string(Binary) ->
	parse:it(Binary, milang_p_token:tokens()).

-spec file(file:filename()) -> {ok, milang_ast()} | {error, term()}.
file(Filename) ->
	case file:open(Filename, [read, binary]) of
		{ok, Handle} ->
			parse_file(Handle);
		{error, Error} ->
			{error, {invalid_file, Error}}
	end.

-define(chunk_size, 10000).

-spec parse_file(file:io_device()) -> {ok, milang_ast()} | {error, term()}.
parse_file(Handle) ->
	InitalRead = io:get_chars(Handle, <<>>, ?chunk_size),
	parse_file(InitalRead, Handle, _TokenAcc = [], _StringLeft = <<>>).

-spec parse_file(binary() | unicode:unicode_binary() | string() | io:server_no_data(), file:io_device(), milang_ast(), unicode:chardata()) -> {ok, milang_ast()} | {error, term()}.
parse_file(eof, _Handle, AstAcc, <<>>) ->
	{ok, AstAcc};
parse_file(eof, _Handle, AstAcc, LeftOverChars) ->
	String = unicode:characters_to_binary(LeftOverChars),
	case string(String) of
		{ok, NewParts, <<>>} ->
			{ok, NewParts ++ AstAcc};
		{ok, _NewParts, NewLeftOverChars} ->
			{error, {trailing_characters, NewLeftOverChars}};
		Error ->
			Error
	end;
parse_file({error, Error}, _Handle, _AstAcc, _LeftOverChars) ->
	{error, {read_error, Error}};
parse_file(Data, Handle, AstAcc, LeftOverChars) ->
	String = unicode:characters_to_binary([Data, LeftOverChars]),
	case string(String) of
		{ok, NewParts, NewLeftOverChars} ->
			NewRead = io:get_chars(Handle, <<>>, ?chunk_size),
			parse_file(NewRead, Handle, AstAcc ++ NewParts, NewLeftOverChars);
		{error, _} ->
			NewRead = io:get_chars(Handle, <<>>, ?chunk_size),
			parse_file(NewRead, Handle, AstAcc, [Data, LeftOverChars])
	end.
