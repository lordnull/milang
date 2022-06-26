-module(milang_log).

-type log_level() :: debug | info | error | never | non_neg_integer().

-type log_info() :: #{
	module := atom(),
	function := atom(),
	line := pos_integer(),
	arity := non_neg_integer() }.

-export_type([ log_level/0, log_info/0 ]).

-export(
	[ set_log_level/1
	, it/3, it/4
	]).

log_level_to_int(N) when is_integer(N) ->
	N;
log_level_to_int(never) ->
	0;
log_level_to_int(undefined) ->
	0;
log_level_to_int(debug) ->
	50;
log_level_to_int(info) ->
	25;
log_level_to_int(error) ->
	10.

log_level_for_human(10) ->
	error;
log_level_for_human(25) ->
	info;
log_level_for_human(50) ->
	debug;
log_level_for_human(0) ->
	never;
log_level_for_human(N) ->
	N.

-spec set_log_level(log_level()) -> log_level().
set_log_level(LogLevel) ->
	OldLogLevel = put(milang_compile_log_level, LogLevel),
	log_level_for_human(OldLogLevel).

-spec it(log_level(), log_info(), string()) -> 'ok'.
it(LogAtLevel, Info, Format) ->
	it(LogAtLevel, Info, Format, []).

-spec it(log_level(), log_info(), string(), list()) -> 'ok'.
it(LogAtLevel, Info, Format, Args) ->
	LogAtInt = log_level_to_int(LogAtLevel),
	MaxLogLevel = log_level_to_int(get(milang_compile_log_level)),
	if
		LogAtInt =< MaxLogLevel ->
			do_log(LogAtLevel, Info, Format, Args);
		true ->
			ok
	end.

do_log(LogAtLevel, Info, Format, Args) ->
	#{ module := Module, line := Line, function := Function, arity := Arity } = Info,
	PreFormat = "(~p) [~p] ~s:~s/~p  ",
	PreArgs = [LogAtLevel, Line, Module, Function, Arity],
	FullFormat = PreFormat ++ Format,
	FullArgs = PreArgs ++ Args,
	io:format(FullFormat, FullArgs).
