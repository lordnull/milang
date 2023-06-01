-module(milang_parse).

-include_lib("kernel/include/logger.hrl").

-export([file/1, string/1]).

-type milang_ast() :: [ milang_ast:ast_node() ].

-spec string(unicode:unicode_binary()) -> {ok, milang_ast(), unicode:unicode_binary()} | {error, term()}.
string(Binary) ->
	parse:string(Binary, milang_p_token:tokens()).

-spec file(file:filename()) -> {ok, milang_ast()} | {error, term()}.
file(Filename) ->
	case file:open(Filename, [read, binary]) of
		{ok, Handle} ->
			?LOG_DEBUG("Beginning parse of file ~s", [Filename]),
			parse_file(Handle);
		{error, Error} ->
			?LOG_DEBUG("Could not open file ~s for parsing due to ~p", [Filename, Error]),
			{error, {invalid_file, Error}}
	end.

-define(chunk_size, 10000).

-spec parse_file(file:io_device()) -> {ok, milang_ast()} | {error, term()}.
parse_file(Handle) ->
	InitalRead = io:get_chars(Handle, <<>>, ?chunk_size),
	parse_file(InitalRead, Handle, _DataAcc = []).

-spec parse_file(binary() | unicode:unicode_binary() | string() | io:server_no_data(), file:io_device(), unicode:chardata()) -> {ok, milang_ast()} | {error, term()}.
parse_file(eof, _Handle, FileContents) ->
	case string(unicode:characters_to_binary(FileContents)) of
		{ok, _} = Ok ->
			?LOG_DEBUG("Parse complete for file."),
			Ok;
		Error ->
			?LOG_DEBUG("Error parsing data already read from the file: ~p", [Error]),
			Error
	end;
parse_file({error, ReadError}, _Handle, _FileContents) ->
	?LOG_DEBUG("Error reading the file: ~p", [ReadError]),
	{error, {read_error, ReadError}};
parse_file(Data, Handle, FileContents) ->
	NewRead = io:get_chars(Handle, <<>>, ?chunk_size),
	?LOG_DEBUG("Reading next chunk of data..."),
	parse_file(NewRead, Handle, [Data | FileContents]).
