-module('Concurrency').

-export([do/1]).

do(Task) ->
	{Pid, Ref} = do_task(Task),
	reap_task(Pid, Ref).

do_task(Fun) ->
	Parent = self(),
	spawn_monitor(fun() ->
		Value = Fun(),
		Parent ! {'$task_complete', self(), Value}
	end).

reap_task(Pid, Ref) ->
	receive
		{'DOWN', Ref, process, Pid, normal} ->
			receive
				{'$task_complete', Pid, Value} ->
					{ok, Value}
			after
				0 ->
					{error, timeout}
			end;
		{'DOWN', Ref, process, Pid, Why} ->
			{error, Why}
	end.
