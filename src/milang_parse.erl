-module(milang_parse).

-export([file/1, string/1]).

string(Binary) ->
	parse:it(Binary, milang_p:module()).

file(Filename) ->
	case file:open(Filename, [read, binary]) of
		{ok, Handle} ->
			parse_file(Handle);
		{error, Error} ->
			{error, {invalid_file, Error}}
	end.

-define(chunk_size, 10000).

parse_file(Handle) ->
	ReadResult = io:get_chars(Handle, <<>>, ?chunk_size),
	parse_file(ReadResult, Handle, _AstAcc = [], _StringLeft = <<>>).

parse_file(eof, _Handle, AstAcc, <<>>) ->
	{ok, AstAcc};
parse_file(eof, _Handle, _AstAcc, LeftOverChars) ->
	{error, {trailing_characters, LeftOverChars}};
parse_file({error, Error}, _Handle, _AstAcc, _LeftOverChars) ->
	{error, {read_error, Error}};
parse_file(Data, Handle, AstAcc, LeftOverChars) ->
	String = unicode:characters_to_binary([Data, LeftOverChars]),
	case string(String) of
		{ok, NewParts, NewLeftOverChars} ->
			NewRead = io:get_chars(Handle, <<>>, ?chunk_size),
			parse_file(NewRead, Handle, AstAcc ++ NewParts, NewLeftOverChars);
		{error, Error} ->
			{error, {parse_error, Error}}
	end.
