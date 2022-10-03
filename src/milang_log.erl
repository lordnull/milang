-module(milang_log).

-export(
	[ set_log_level/1
	]).

-spec set_log_level(logger:level()) -> ok | {error, term()}.
set_log_level(LogLevel) ->
	case logger:update_formatter_config(default, template, [level, " ", time, " ", file, ":", line, " ", msg, "\n"]) of
		ok ->
			OldPrimaryConfig = logger:get_primary_config(),
			#{ level := OldLevel } = OldPrimaryConfig,
			case logger:update_primary_config(#{ level => LogLevel }) of
				ok ->
					{ok, OldLevel};
				E ->
					E
			end;
		E ->
			E
	end.
