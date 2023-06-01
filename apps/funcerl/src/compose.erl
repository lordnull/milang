%%% @doc because why not? It takes 2 funs and can either forward pipe or backward
%%% pipe them.
%%% @end.
-module(compose).

-export(
	[ pipe_to/2
	, pipe_from/2
	, always/1
	, never/0
	, id/0
	, debug/2
	]).

%% @doc Returns a fun/1 that takes the value, gives it to FirstFun, and then
%% takes that value and gives it to SecondFun.
%%
%% ```fun(Val) ->
%%        SecondFun(FirstFun(Val))
%% end.
%% ```
%% @end.
-spec pipe_to(fun((A) -> B), fun((B) -> C)) -> fun((A) -> C).
pipe_to(FirstFun, SecondFun) ->
	fun(Val) ->
		SecondFun(FirstFun(Val))
	end.

%% @doc Returns a fun/1 that takes the value, gives it to SecondFun, and then
%% takes that value and gives it to FirstFun.
%%
%% ```fun(Val) ->
%%        FirstFun(SecondFun(Val))
%% end.
%% ```
%% @end.
-spec pipe_from(fun((B) -> C), fun((A) -> B)) -> fun((A) -> C).
pipe_from(FirstFun, SecondFun) ->
	fun(Val) ->
		FirstFun(SecondFun(Val))
	end.

%% @doc Returns a fun that returns a constant value no matter the argument given.
%% @end.
-spec always(A) -> fun((term()) -> A).
always(A) ->
	fun(_) -> A end.

%% @doc Returns a function that causes an error if called. Primarily useful in
%% unit tests to ensure a code-path is never followed.
%% @end
-spec never() -> fun((term()) -> no_return()).
never() ->
	fun(_) -> error(forbidden_function_called) end.

%% @doc Returns a function that returns whatever value is given to it.
%% @end.
-spec id() -> fun((A) -> A).
id() ->
	fun(A) -> A end.

%% @doc Takes the value and prints a message to standard out with the value
%% appended. It then returns the value passed in. An easy way to debug values
%% without needing to add new variables or lines.
%% @end
-spec debug(iodata(), A) -> A.
debug(Msg, A) ->
	ok = io:format(unicode:characters_to_binary([Msg, ": ~p~n"]), [A]),
	A.
