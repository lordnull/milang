#! /usr/bin/env escript
%% -*- erlang -*-
%%! -escript main milang_boostrap

-module(milang_bootstrap).

-export([main/1]).

-ifndef(BOOT_MODULE).
-define(BOOT_MODULE, 'HelloWorld').
-endif.

main(Args) ->
	Stack = ?BOOT_MODULE:main(),
	milang_curry:call(Stack, [Args]).

