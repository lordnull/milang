-module(milang_ui_editor).
-behavior(wx_object).
-include_lib("wx/include/wx.hrl").

-export([new/1]).
-export(
	[ init/1
	, terminate/2
	, code_change/3
	, handle_info/2
	, handle_call/3
	, handle_cast/2
	, handle_event/2
	]).

new(Module) ->
	wx_object:start_link(?MODULE, #{module => Module}, []).

-record(state,
	{ module
	}).

init(#{ module := Module}) ->
	State = #state{ module = Module},

	XrcObject = wxXmlResource:get(),
	Frame = wxXmlResource:loadFrame(XrcObject, wx:null(), "MiEditor"),

	PackageTreePanel = wxXmlResource:xrcctrl(Frame, "PackageTreePanel", wxPanel),

	PackageSizer = wxWindow:getSizer(PackageTreePanel),

	Packages = milang_package:load_all_packages(),

	ok = build_package_tree(PackageTreePanel, PackageSizer, Packages),

	_ = wxFrame:show(Frame),
	{Frame, State}.

build_package_tree(Panel, Sizer, Packages) ->
	lists:foreach(fun(Package) ->
		PackageUi = milang_ui_tree_part:new(Panel, milang_package:name(Package)),
		wxSizer:add(Sizer, PackageUi, [{proportion, 0}, {flag, ?wxEXPAND}]),
		{NextPanel, NextSizer} = milang_ui_tree_part:get_items_parent(PackageUi),
		ok = build_package_tree(NextPanel, NextSizer, milang_package:packages(Package)),
		build_module_tree(NextPanel, NextSizer, milang_package:modules(Package))
	end, Packages).

build_module_tree(Panel, Sizer, Modules) ->
	lists:foreach(fun(Module) ->
		ModuleUi = milang_ui_tree_part:new(Panel, milang_package:name(Module)),
		wxSizer:add(Sizer, ModuleUi, [{proportion, 0}, {flag, ?wxEXPAND}]),
		{NextPanel, NextSizer} = milang_ui_tree_part:get_items_parent(ModuleUi),
		ok = lists:foreach(fun(Type) ->
			Text = wxStaticText:new(NextPanel, ?wxID_ANY, milang_package:name(Type)),
			wxSizer:add(NextSizer, Text)
		end, milang_package:types(Module)),
		lists:foreach(fun(Func) ->
			Text = wxStaticText:new(NextPanel, ?wxID_ANY, milang_package:name(Func)),
			wxSizer:add(NextSizer, Text)
		end, milang_package:functions(Module))
	end, Modules).

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
