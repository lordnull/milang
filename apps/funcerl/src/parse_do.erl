%%% @doc Additional functions to parse arbitrary bits of unicode text. While
%%% some of the functions mimic that in the parse module, that's just it doing
%%% convience things. For example, many of the functions will do chomping for
%%% you, rather than in the parse module with mostly will chomp nothing.
%%% @end.
-module(parse_do).

-export(
	[ noop/0
	, repeat_until/2
	, enclosed/3
	, tag/2
	, grapheme/0
	, optional/1
	, branch/1
%	, digit/0
%	, whitespace/0
%	, ws/0

	]).

noop() -> fun noop/1.

noop(Ctx) -> {ok, Ctx}.


%% @doc Check the end parser, and if it passes, we're done. Otherwise, keep
%% running the same given parser.
repeat_until(EndParse, ElementParse) ->
	TaggedEnd = parse:map(fun(V) ->
		{done, V}
	end, EndParse),
	TaggedElement = parse:map(fun(V) ->
		{continue, V}
	end, ElementParse),
	repeat_loop(TaggedEnd, TaggedElement, []).

repeat_loop(EndParse, ElementParse, Acc) ->
	Firsty = parse:first_of([EndParse, ElementParse]),
	parse:and_then(fun
		({done, EndVal}) ->
			parse:success( #{ list => lists:reverse(Acc), ended => EndVal } );
		({continue, Element}) ->
			repeat_loop(EndParse, ElementParse, [Element | Acc])
	end, Firsty).

%% @doc a specific type of repeat where we have a certain end we want, and then
%% ditch that end.
enclosed(StartParse, EndParse, ElementParse) ->
	Repeated = parse:and_then(fun(_) ->
		repeat_until(EndParse, ElementParse)
	end, StartParse),
	parse:map(fun(#{ list := L }) -> L end, Repeated).

tag(TagName, Parser) ->
	parse:map_n(fun(Location, UserParser) ->
		{TagName, Location, UserParser}
	end, [parse:location(), Parser]).

%% @doc A group of utf8 characters that will appear as a single character to a
%% reader.
grapheme() ->
	parse:regex("\\X").

%% @doc returns either an empty list, or a list of exactly 1 containing the
%% result of the parser. Essentially a faster way to do a recover.
optional(Parser) ->
	Mapped = parse:map(fun(V) ->
		[V]
	end, Parser),
	parse:try_recover(fun(_) ->
		{ok, []}
	end, Mapped).

%% @doc A more efficient first_of/1. Rather than making an entire parse branch
%% backtrackable, instead you provide a more performant parser for each which
%% can be used to choose which parse to continue traveling down. This is useful
%% if the branches could require a lot of parsing only for an error to make it
%% unfeasible to coninue, even on other paths.
branch(Branches) ->
	io:format("banches: ~p~n", [Branches]),
	branch_loop(Branches, _BranchErrors = []).

% TODO keep a record of the errors so when we get to a 'no_branches_matched',
% maybe whatever called this can map the error to something more reasonable.
branch_loop([], BranchErrors) ->
	io:format("loop done: ~p.~n", [BranchErrors]),
	parse:fail({no_branches_matched, BranchErrors});
branch_loop([{TestParse, AndThen} | Tail], BranchErrors) ->
	BacktrackableParse = parse:backtrackable(TestParse),

	ErrorHandler = fun(Error) ->
		io:format("try next branch due to ~p: ~p~n", [Error, Tail]),
		Backtrack = parse:backtrack(),
		parse:and_then(fun(Backtracked) ->
			io:format("backtrack value: ~p~n", [Backtracked]),
			branch_loop(Tail, [Backtracked | BranchErrors])
		end, Backtrack)
	end,

	GoingDownBranch = parse:and_then(fun(_) ->
		Comitted = parse:commit(),
		parse:and_then(AndThen, Comitted)
	end, BacktrackableParse),

	Recovery = parse:try_recover(ErrorHandler, GoingDownBranch),
	parse:map_err(fun
		({no_backtrack, Why}) ->
			Why;
		(Why) ->
			Why
	end, Recovery).
