-module(milang_ui_package_tree).

-behavior(wx_object).
-include_lib("wx/include/wx.hrl").

-export([new/2]).
-export(
	[ init/1
	, terminate/2
	, code_change/3
	, handle_info/2
	, handle_call/3
	, handle_cast/2
	, handle_event/2
	]).

new(Parent, Packages) ->
	wx_object:start_link(?MODULE, #{parent => Parent, packages => Packages}, []).

-record(state, { parent, packages = []}).

init(#{ parent := Parent, packages := Packages }) ->

	Panel = wxPanel:new(Parent, []),

	ok = lists:foreach(fun(P) ->
		milang_ui_package_tree_item:new(Panel, P)
	end, Packages),

	{Panel, #state{ parent = Parent, packages = Packages }}.

handle_call(Msg, _From, State) ->
	{reply, {error, {invalid, Msg}}, State}.

handle_event(Event, State) ->
	logger:debug("Dropped event ~p.", [Event]),
	{noreply, State}.

handle_cast(Cast, State) ->
	logger:debug("Dropped cast: ~p", [Cast]),
	{noreply, State}.

handle_info(Msg, State) ->
	logger:debug("Dropped msg: ~p", [Msg]),
	{noreply, State}.

terminate(_Why, _State) -> ok.

code_change(_Version, State, _Options) ->
	{ok, State}.
