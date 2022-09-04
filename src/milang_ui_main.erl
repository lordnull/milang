-module(milang_ui_main).

-include_lib("wx/include/wx.hrl").

-behavior(gen_server).

-export(
	[ start_link/0
	]).
-export(
	[ init/1
	, handle_call/3
	, handle_cast/2
	, handle_info/2
	, terminate/2
	, code_change/3
	]).

start_link() ->
	logger:set_primary_config(level, debug),
	gen_server:start_link(?MODULE, {}, []).


-record(state, { wx_env, wx_things = [] }).

init(_) ->
	WxEnv = wx:new(),

	DefaultModule = milang_module:new("Test Module"),

	XrcObject = wxXmlResource:get(),
	wxXmlResource:initAllHandlers(XrcObject),

	PrivDir = case code:priv_dir(milang) of
		{error, _} ->
			"priv";
		D ->
			D
	end,

	UiXrcs = filename:join([PrivDir, "ui_xrc", "MiEditor.xrc"]),

	case wxXmlResource:load(XrcObject, UiXrcs) of
		false ->
			error({unable_to_load_ui, UiXrcs});
		true ->
			ok
	end,

	Editor = milang_ui_editor:new(DefaultModule),

	WxThings = [Editor],

	State = #state{ wx_env = WxEnv, wx_things = WxThings },

	{ok, State}.

handle_call(Msg, _From, State) ->
	{reply, {error, {invalid, Msg}}, State}.

handle_cast(_, State) ->
	{noreply, State}.

handle_info(_, State) ->
	{noreply, State}.

terminate(_Why, _State) -> ok.

code_change(_Version, State, _Options) ->
	{ok, State}.
