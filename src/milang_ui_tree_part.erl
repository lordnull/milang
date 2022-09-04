-module(milang_ui_tree_part).

-behavior(wx_object).
-include_lib("wx/include/wx.hrl").
-include_lib("kernel/include/logger.hrl").

-export(
	[ new/2
	, expand/1, collapse/1
	, add_handler/3
	, remove_handler/2
	, get_items_parent/1
	]).
-export(
	[ init/1
	, terminate/2
	, code_change/3
	, handle_info/2
	, handle_call/3
	, handle_cast/2
	, handle_event/2
	]).

new(Parent, SectionName) ->
	wx_object:start_link(?MODULE, #{section_name => SectionName, parent => Parent}, []).

expand(Obj) ->
	cast_collapsed(Obj, false).

collapse(Obj) ->
	cast_collapsed(Obj, true).

cast_collapsed(Obj, IsCollapsed) ->
	wx_object:cast(Obj, {set_is_collapsed, IsCollapsed}).

add_handler(Obj, EventType, Callback) ->
	wx_object:call(Obj, {add_event_handler, EventType, Callback}).

remove_handler(Obj, HandlerKey) ->
	wx_object:call(Obj, {remove_handler, HandlerKey}).

get_items_parent(Obj) ->
	wx_object:call(Obj, {get_items_parent}).


-record(state, {is_expanded = true, items_panel, items_sizer, parent }).

init(#{ parent := Parent, section_name := SectionName }) ->


	XrcObject = wxXmlResource:get(),

	Panel = wxXmlResource:loadPanel(XrcObject, Parent, "MiExpandoPanel"),

	CollapseToggle = wxXmlResource:xrcctrl(Panel, "SectionExpanded", wxCheckBox),

	wxCheckBox:setLabel(CollapseToggle, SectionName),

	ItemsPanel = wxXmlResource:xrcctrl(Panel, "ItemsPanel", wxPanel),
	ItemsSizer = wxWindow:getSizer(ItemsPanel),


	State = #state{
		is_expanded = true,
		items_panel = ItemsPanel,
		items_sizer = ItemsSizer,
		parent = Parent
	},
	{Panel, State}.

handle_call({get_items_parent}, _From, State) ->
	Reply = {State#state.items_panel, State#state.items_sizer},
	{reply, Reply, State};

handle_call(Msg, _From, State) ->
	{reply, {error, {invalid, Msg}}, State}.

handle_event(Event, State) ->
	?LOG_DEBUG("Dropped event ~p.", [Event]),
	{noreply, State}.

handle_cast(Cast, State) ->
	?LOG_DEBUG("Dropped cast: ~p", [Cast]),
	{noreply, State}.

handle_info(Msg, State) ->
	?LOG_DEBUG("Dropped msg: ~p", [Msg]),
	{noreply, State}.

terminate(_Why, _State) -> ok.

code_change(_Version, State, _Options) ->
	{ok, State}.
