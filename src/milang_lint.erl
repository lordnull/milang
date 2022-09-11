%%% @doc Things that, while syntactically correct, and may not _explicitely_
%%% compiler failures, are still "wrong" in the opinion of the language.
-module(milang_lint).

-include("milang_ast.hrl").

-export([it/2]).

%-record(state, {
%	errors = [],
%	defined_names = ordsets:new(),
%	names_needing_spec = ordsets:new(),
%	names_needing_body = ordsets:new(),
%	declaration_module_first_linted = false
%}).

-type mode() :: 'program' | 'header' | 'module'.
-spec it([milang_ast:ast_node()], mode()) -> [{milang_ast:location(), term()}].
it(_Ast, _Mode) ->
	% TODO actually implement the linter.
	[].

% TODO
% things to lint:
% * module name matches file path
% * unused types
% * unused binds
% * unused args
% * reused type names
% * reused bind names
% * non-exhaustive matches
% * spec with no function ( module compile only)


%it(AST, Mode) ->
%	do_lint(AST, Mode, #state{}).
%
%do_lint([], _Mode, State) ->
%	NeedSpecErrors = lists:map(fun(Name) ->
%		{{0, 0}, {exposed_but_unspecced, Name}}
%	end, State#state.names_needing_spec),
%	NeedBodyErrors = lists:map(fun(Name) ->
%		{{0,0}, {specced_or_exposed_but_not_defined, Name}}
%	end, State#state.names_needing_body),
%	lists:concat([NeedSpecErrors, NeedBodyErrors, State#state.errors]);
%
%do_lint([Node | _] = AST, Mode, #state{declaration_module_first_linted = false} = State) ->
%	MidState = State#state{ declaration_module_first_linted = true },
%	case Node#milang_ast.type of
%		declaration_module ->
%			do_lint(AST, Mode, MidState);
%		_NotModule ->
%			NewState = add_error(Node#milang_ast.location, module_declaration_not_first, MidState),
%			do_lint(AST, Mode, NewState)
%	end;
%
%do_lint([#milang_ast{ type = declaration_module } = Node | Tail], Mode, State) ->
%	NewState = declaration_module(Node, Mode, State),
%	do_lint(Tail, Mode, NewState);
%
%do_lint([ Node | Tail], Mode, State) ->
%	ok = io:format("lint not yet implemented in mode ~s for ~p~n", [Mode, Node]),
%	do_lint(Tail, Mode, State).
%
%add_error(Location, Term, #state{ errors = Errors} = State) ->
%	NewErrors = [{Location, Term} | Errors],
%	State#state{ errors = NewErrors }.
%
%declaration_module(Node, program, State) ->
%	MidState = declaration_module(Node, module, State),
%	case ordsets:is_element('main', State#state.defined_names) of
%		false ->
%			add_error(Node#milang_ast.location, missing_main, MidState);
%		true ->
%			MidState
%	end;
%
%declaration_module(Node, module, State) ->
%	MidState = declaration_module(Node, header, State),
%
%	#{exposing := Exposing } = Node#milang_ast.data,
%	NamesNeedingBody = fun(NameAST, Acc) ->
%		Name = NameAST#milang_ast.data,
%		NewSet = ordsets:add_element(Name, Acc#state.names_needing_body),
%		Acc#state{ names_needing_body = NewSet }
%	end,
%	lists:foldl(NamesNeedingBody, MidState, Exposing);
%
%declaration_module(Node, header, State) ->
%	#{ exposing := Exposing } = Node#milang_ast.data,
%
%	NamesOnlyOnce = fun(NameAST, {Checking, Acc}) ->
%		Name = NameAST#milang_ast.data,
%		case ordsets:is_element(Name, Checking) of
%			true ->
%				{Checking, add_error(NameAST#milang_ast.location, repeated_exposing, Acc)};
%			false ->
%				{ordsets:add_element(Name, Checking), Acc}
%		end
%	end,
%
%	NamesNeedingSpec = fun(NameAST, Acc) ->
%		Name = NameAST#milang_ast.data,
%		NewSet = ordsets:add_element(Name, Acc#state.names_needing_spec),
%		Acc#state{ names_needing_spec = NewSet }
%	end,
%
%	{_, OnceCheck} = lists:foldl(NamesOnlyOnce, {ordsets:new(), State}, Exposing),
%	NeedSpec = lists:foldl(NamesNeedingSpec, OnceCheck, Exposing),
%
%	NeedSpec.
%
