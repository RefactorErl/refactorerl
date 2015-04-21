%%% -*- coding: latin-1 -*-

%%% This file is part of RefactorErl.
%%%
%%% RefactorErl is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as published
%%% by the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% RefactorErl is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public License
%%% along with RefactorErl.  If not, see <http://plc.inf.elte.hu/erlang/>.
%%%
%%% The Original Code is RefactorErl.
%%%
%%% The Initial Developer of the Original Code is Eötvös Loránd University.
%%% Portions created  by Eötvös Loránd University and ELTE-Soft Ltd.
%%% are Copyright 2007-2013 Eötvös Loránd University, ELTE-Soft Ltd.
%%% and Ericsson Hungary. All Rights Reserved.

%%% @doc This is the main module of RefactorErl Wx interface. It creates the
%%% main window, handles the main "notebook" (pages for different functions)
%%% and provides some functionality for calling referl_wx_logic's functions and
%%% error/exception handling. <br />
%%% It implements a so-called "Referl Wx event handling" (See handle_info)
%%%  events: <br />
%%%      -file_drop <br />
%%%      -file_add <br />
%%%      -add_dir_finished <br />
%%%      -file_drop_finished <br />
%%%      -add_env_finished <br />
%%%      -del_env_finished <br />
%%% It also implements a "Referl wx query handling" (See handle_info and
%%% referl_wx_queries) <br />
%%% It routes query requests from Referl Wx interface modules to one
%%% of its children (referl_wx_queries)
%%% @author Gabor Hosszu

-module(referl_wx).
-include_lib("referl_core/include/core_export.hrl").
-include_lib("referl_gen/include/gen_export.hrl").
-include_lib("wx/include/wx.hrl").
-include("wx.hrl").

-behaviour(wx_object).

-export([start/0, start/1, init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).
-export([execute_logic_ui_call/2, handle_result/1, handle_result/3,
         handle_result/2, handle_result/4]).
-export([infowindow_main/2, infowindow_main/3, statusbar_notification/3]).
-export([hide_n_show/3, activate_btns/3, enable_btns/2]).
-export([make_placeholder/3, get_selected/1, get_all/1, set_stc_style/1,
         set_scrollrate/1]).

-record(state,
	{
	  wx_elem, %% wx_elements
	  sess_inf %% session info
	 }).
-record(wx_elements,
	{
	  parent,
	  server,
	  notebook,
	  stbar,		%% Statusbar
	  pages,
    browsers
	}).
-record(session_info, %% session info
	{
	  username,
	  access_to_db
	 }).

-define(mainNoteBookId, 1001).
-define(mainFrame, "main_Frame_1000").

%% @doc Starts Wx Interface
start() ->
	UserName = atom_to_list(node()),
  start(UserName).

%% @doc Starts Wx Interface with a username
start([UserName]) when is_atom(UserName) ->
  UserName2 = atom_to_list(UserName),
  wx_object:start(?MODULE,
                      {UserName2, true}, []);
start(UserName=[U | _ ]) when is_list(UserName) and is_integer(U) ->
  wx_object:start(?MODULE,
                      {UserName, false}, []);
start(_) -> {error, "A non-empty string is needed for username."}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
init(Params) ->
        Server = wx:new([{silent_start, true}]),
        process_flag(trap_exit, false),
        wx:batch(fun() -> do_init(Server, Params) end).
do_init(Server, {UserName, NoShell}) ->
  put(noshell, NoShell),
  Title = "RefactorErl (user: " ++ UserName ++ ")",
  %% Setup global fonts, etc.
  GlobalFont = wxFont:new(8, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL,
                                                     ?wxFONTFLAG_DEFAULT),
  Frame = wxFrame:new(wx:null(), -1, Title, [{style, ?wxDEFAULT_FRAME_STYLE
                                          }, {pos, {100, 100}}]),
 	wxFrame:setName(Frame, ?mainFrame),

  Panel = wxPanel:new(Frame, []),

  wxWindow:setFont(Panel, GlobalFont),
  %% Setup sizers
  MainSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, []),
  Notebook = wxNotebook:new(Panel, ?mainNoteBookId, [{style, ?wxBK_DEFAULT}]),

	%% Setup wxStatusBar
  StatusBar = wxFrame:createStatusBar(Frame, []),
  wxStatusBar:setFieldsCount(StatusBar, 1),
  wxStatusBar:setStatusText(StatusBar, "Welcome!", [{number, 0}]),
  wxSizer:add(MainSizer, Notebook, [{proportion, 1},
				      {flag, ?wxEXPAND}]),

  wxPanel:setSizer(Panel, MainSizer),

  %% CONNECT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  wxNotebook:connect(Notebook, command_notebook_page_changed,
       [{skip, true}]), % {skip, true} has to be set on windows
  wxFrame:connect(Frame, close_window),
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %% INIT
  {Init, [Pages, _Props, DbAccess]} =
  try
    put(fatal, {stop, db_busyc}), %% if defined, no modals will show up on errors,
    %% and db_busy exception will be thrown
    {DbAccess0, RestrictedMode0} = ?HandleRes(?ExecLCall(init, [self()])),
    Props0 = [{user, UserName}, {main_pid, self()},
	      {restricted_mode, RestrictedMode0}],
    Pages0 = add_pages_to_notebook(Notebook, Props0),
  	db_access_changed(DbAccess0, Pages0),
    erase(fatal),
    {ok, [Pages0, Props0, DbAccess0]}
  catch
    _:{error, Error} ->  {{error, Error}, [undefined, undefined, undefined]}
  end,
  case Init of
    {error,db_busy} ->
        ignore;
    {error, Reason} ->
        {stop, Reason};
    _ ->
        %% MAC RELATED
        %% Cause of Mac this stuff comes here...
        wxSizer:layout(MainSizer),
        wxFrame:layout(Frame),
        wxFrame:show(Frame),
        wxFrame:maximize(Frame),
        wxFrame:sendSizeEvent(Frame),
      	WxElem = #wx_elements{ parent=Frame, server=Server,
                               browsers=[], notebook = Notebook,
                               stbar=StatusBar, pages=Pages },
      	SessInf = #session_info{ username=UserName,
                                 access_to_db = DbAccess },
        {Frame, #state{ wx_elem=WxElem, sess_inf=SessInf}}
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Behaviour

%% @private
handle_event(#wx{event=#wxClose{}},
                    State = #state{ wx_elem=#wx_elements{ parent = Frame }}) ->
    wxWindow:destroy(Frame),
    {stop, normal, State};

handle_event(#wx{id=?mainNoteBookId,
                 event = #wxNotebook{type = command_notebook_page_changed}},
	     State = #state{}) ->
    {noreply,State};
handle_event(_Ev = #wx{}, State = #state{}) ->
    {noreply,State}.

%% @private
%% Jobinfo received from RefactorErl (reflib_ui operations etc.)
handle_info({global,jobinfo,{{_Type,modifier,SState,_},Msg}},
			State = #state{ wx_elem=WxElem }) when SState==started ->
	DbAccess = false,
	db_access_changed(DbAccess, WxElem#wx_elements.pages),
	statusbar_notification(self(), Msg, ?stbar_Notif_Timeout),
    {noreply,State#state{ sess_inf=#session_info{ access_to_db=DbAccess } }};

handle_info({global,jobinfo,{{Tp,modifier,SState,Ent},Msg}},
			State = #state{ wx_elem=WxElem }) when SState==finished ->
	DbAccess = true,
    Info = case Ent of
             [I|_] -> I;
             _     -> none
           end,
    Chen = WxElem#wx_elements.pages,
    case Tp of
        add -> send_children(Chen, {event, add_dir_finished,Info});
        add_dir -> send_children(Chen, {event, add_dir_finished,Info});
        drop_files -> send_children(Chen, {event, file_drop_finished,Info});
        drop -> send_children(Chen, {event, file_drop_finished,Info});
        drop_dir -> send_children(Chen, {event, file_drop_finished,Info});
        reset -> send_children(Chen, {event, file_drop_finished,Info}),
                 send_children(Chen, {event, reset,Info});
        add_env -> send_children(Chen, {event, add_env_finished,Info});
        del_env_val -> send_children(Chen, {event, del_env_finished,Info});
        _       -> ok
    end,
	db_access_changed(DbAccess, WxElem#wx_elements.pages),
	statusbar_notification(self(), Msg, ?stbar_Notif_Timeout),
    {noreply,State#state{ sess_inf=#session_info{ access_to_db=DbAccess } }};

handle_info({global,jobinfo,_Info={{Job,_,_SState,Args},Msg}},
			 State = #state{ wx_elem=WxElem }) ->
    Chen = WxElem#wx_elements.pages,
    case {Job, Args} of
      {transform, [semantic_query, _]}  ->
              send_children(Chen, {event, semantic_query, none});
      _                                 -> ok
    end,
	  statusbar_notification(self(), Msg, ?stbar_Notif_Timeout),
    {noreply,State};
handle_info({global,statusinfo,Info}, State=#state{ wx_elem=WxElem }) ->
    Children = WxElem#wx_elements.pages,
    handle_changes(proplists:get_value(change,Info),Children),
    {noreply,State};

handle_info({request_for_query, {RequesterPid, RequesterObj}, QStr, File, Pos},
             State = #state{  wx_elem=WxElem           }) ->
    Children = WxElem#wx_elements.pages,
    QueryHandler = proplists:get_value(referl_wx_queries, Children, no_child),
    Notebook = WxElem#wx_elements.notebook,
    case QueryHandler of
        no_child -> ok;
        _Ref     -> wx_object:get_pid(QueryHandler) !
                    {request_for_query, {RequesterPid, RequesterObj},
                                                     QStr, File, Pos},
                    wxNotebook:setSelection(Notebook, 1)
    end,
    {noreply, State};

handle_info({request_for_dupcode_search, Algorithm, Mod, Start, End},
             State = #state{  wx_elem=WxElem           }) ->
    Children = WxElem#wx_elements.pages,
    DupHandler = proplists:get_value(referl_wx_dupcode, Children, no_child),
    Notebook = WxElem#wx_elements.notebook,
    case DupHandler of
        no_child -> ok;
        _Ref     -> wx_object:get_pid(DupHandler) !
                    {request_for_dupcode_search, Algorithm, Mod, Start, End},
                    wxNotebook:setSelection(Notebook, 3)
    end,
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply,State}.

%% @private
handle_call({stbar_not, Txt}, _From, State=#state{ wx_elem = WxE }) ->
  StatusBar = wxFrame:getStatusBar(WxE#wx_elements.parent),
  CurrentTxt = wxStatusBar:getStatusText(StatusBar),
  case CurrentTxt of
    Txt -> ok;
    _   -> wxStatusBar:setStatusText(StatusBar, Txt)
  end,
  {reply,ok,State};

handle_call({stbar_clear, ToClearTxt}, _From, State=#state{ wx_elem = WxE }) ->
  StatusBar = wxFrame:getStatusBar(WxE#wx_elements.parent),
  CurrentTxt = wxStatusBar:getStatusText(StatusBar),
  case CurrentTxt of
    ToClearTxt -> wxStatusBar:setStatusText(StatusBar, "");
    _   -> ok
  end,
  {reply,ok,State};

handle_call(_Msg, _From, State) ->
    {reply,ok,State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply,State}.

%% @private
code_change(_, _, State) ->
    {stop, ignore, State}.

%% @private
terminate(_Reason, _State=#state{ wx_elem=WxElem }) ->
	?Logic:terminate(),
  timer:sleep(200),
  wx:destroy(),
  get(noshell) == true andalso begin init:stop() end,
  Children = WxElem#wx_elements.pages,
  [ exit(wx_object:get_pid(Child), kill) || {_, Child} <- Children ],
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exported
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Executing a ?Logic UI call with exception handling
execute_logic_ui_call(FunName, Args) ->
    handle_exceptions(?Logic, FunName, Args).

handle_exceptions(Mod, Fun, Args) ->
	try
		apply(Mod, Fun, Args)
	catch
		request_denied -> handle_exception(request_denied);
    {query_exception, Error} -> handle_exception({query_exception, Error})
	 % T:E -> ?d({T,E}), ok
  end.

%% @doc Handle results after executing a call
%% The standard modules of the RefErl Wx UI
%%  - give a result or
%%  - give a wxError tuple ({error, ErrorTxt}) or
%%  - give an exception which must be handled with handle_exceptions and conver-
%%    ted to a proper {exception, Type, ErrorTxt} tuple
%% If the PROCESS DICTIONARY key "fatal" is defined, then no
%% popup is shown, but an error is thrown: thrown(FatalsKey)
handle_result(R, ErrReturn) ->
  handle_result(R, fun(I, _A) -> I end, [], ErrReturn).
handle_result(R) ->
  handle_result(R, fun(I, _A) -> I end, []).
handle_result(R, HandlerFun, HandlerArgs) ->
  handle_result(R, HandlerFun, HandlerArgs, ok).
handle_result(R, HandlerFun, HandlerArgs, ErrReturn) ->
  Excep = get(fatal),
  PopUp = Excep == undefined,
	case R of
		{error, Error} -> PopUp andalso ?wxMain:infowindow_main(Error, "Error", ?wxICON_ERROR),
                      not PopUp andalso throw(Excep),
                      ErrReturn;
    {exception, _Type, ErrMsg} ->
                      PopUp andalso ?wxMain:infowindow_main(ErrMsg, "Error", ?wxICON_ERROR),
                      not PopUp andalso throw(Excep),
                      ErrReturn;
    Res            -> HandlerFun(Res, HandlerArgs)
	end.

%% @doc General functionality for buttons
%% Hide some buttons and show some.
hide_n_show(Sizer, ToHide, ToShow) ->
    activate_btns(Sizer, ToHide, false),
    activate_btns(Sizer, ToShow, true).
%% @doc Activate/deactivate (show/hide) buttons
activate_btns(Sizer, Buttons, Bool) ->
    Fun =
    fun(Button) ->
        wxSizer:show(Sizer, Button, [{show, Bool}])
    end,
    wx:foreach(Fun, Buttons).
%% @doc Enable/disable buttons
enable_btns(Buttons, Bool) ->
    Fun =
    fun(Button) ->
        wxWindow:enable(Button, [{enable, Bool}])
    end,
    wx:foreach(Fun, Buttons).
%% @doc Make a dummy widget (placeholder) with given width, height
make_placeholder(Parent, W, H) ->
    Win = wxWindow:new(Parent, ?wxID_ANY, [{style, ?wxBORDER_NONE},
					   {size, {W,H}}]),
    Win.

%% @doc Get selected Items/ItemID-s of a list control
%% (recursive algorithm, no easier way to do)
get_selected(ListCtrl) ->
    get_items(ListCtrl, -1, [], [{state, ?wxLIST_STATE_SELECTED}]).

get_all(ListCtrl) ->
    get_items(ListCtrl, -1, [], []).

get_items(ListCtrl, Next, Acc, Props) ->
    Item = wxListCtrl:getNextItem(ListCtrl, Next, [{geometry, ?wxLIST_NEXT_ALL}]
                                              ++ Props),
    case Item of
         -1 -> Acc;
         _  -> get_items(ListCtrl, Item, [ Item | Acc ], Props)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_exception(request_denied) ->
	% infowindow(Frame, "Request denied: job server is busy.", "ERROR"),
	{exception, request_denied, "The job server is busy!"};

handle_exception({query_exception, Error}) ->
	{exception, query_exception, Error};

handle_exception(E) ->
	{exception, unknown_exception, "Error: " ++ io_lib:write(E)}.

add_pages_to_notebook(Notebook, Props) ->
    ModsToStart = [{"File browser", referl_wx_filebrowser},
                   {"Queries", referl_wx_queries},
                   {"Dependency graph", referl_wx_depgraph},
                   {"Duplicated code", referl_wx_dupcode},
                   {"Investigations", referl_wx_inv}],
    %%               {"Database graph", referl_wx_dbgraph}],
    GenFun = fun({Title, Mod}) ->
                           Panel = wxScrolledWindow:new(Notebook, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),
                           wxScrolledWindow:setScrollRate(Panel, 5, 5),
                           wxNotebook:addPage(Notebook, Panel, Title, []),
                           Pid = check_start(Mod:start(
                                  [{parent, Panel}] ++ Props)),
                           {Mod, Pid}
                           end,
    Panels = wx:map(GenFun, ModsToStart),
    Panels.

%% @doc Set scroll rate with "global" values
set_scrollrate(Panel) ->
  wxWindow:fitInside(Panel),
  wxScrolledWindow:setScrollRate(Panel, 5, 5),
  wxFrame:refresh(Panel).

%% Checks whether a page started normally, or not
check_start(Return) ->
  case Return of
      {error, Error}  -> throw({error, Error});
      ignore          -> throw({error, db_busy});
      WxObj           -> WxObj
  end.

%% @doc Shows notification in statusbar for a given time and sleeps if it
%% contains anything alse than ""
statusbar_notification(MainPid, Txt, Time) ->
  WxEnv = wx:get_env(),
  ToSpawn =
    fun() ->
        link(MainPid),
        wx:set_env(WxEnv),
        try
          wx_object:call(MainPid, {stbar_not, Txt}),
          if Time == ?INFINITY -> ok;
            true -> timer:sleep(Time),
                    wx_object:call(MainPid, {stbar_clear, Txt}),
                    ok
          end
        catch
          _:_ -> ok
        end
    end,
  spawn(ToSpawn).

%% @doc Show info window.
infowindow(Parent, Infotext, Title, Style) ->
    DErr = wxMessageDialog:new(
              Parent,
              Infotext,
              [{caption, Title},{style, Style}]),
    wxWindow:centre(DErr),
    wxDialog:showModal(DErr).

%% @doc Show info window (main window is the parent).
infowindow_main(Infotext, Title) ->
  infowindow_main(Infotext, Title, ?wxOK).

%% @doc Show info window (main window is the parent).
infowindow_main(Infotext, Title, Style) ->  % wx:null()
	infowindow(wxWindow:findWindowByName(?mainFrame), Infotext, Title, Style).

%% Informate children that the db access changed to true | false.
db_access_changed(DbAccess, Children) ->
    send_children(Children, {event, db_access, DbAccess}).

handle_changes(undefined, _) -> ok;
handle_changes(Chs, Children) ->
    [ begin
        Present = proplists:get_value(present,Props,true),
        if Present -> send_children(Children, {event, file_add, FName});
           true    -> send_children(Children, {event, file_drop, FName})
        end
      end  || {FName, Props} <- Chs ].

send_children(Children, Msg) ->
    [ wx_object:get_pid(Child) ! Msg ||
                              {_Mod, Child} <- Children ].

%% @doc Set a default style for a StyledTextCtrl
set_stc_style(STC) ->
    wxStyledTextCtrl:setMarginWidth(STC, ?wxSTC_MARGIN_NUMBER, 0),
    wxStyledTextCtrl:setUseHorizontalScrollBar(STC, false),
    wxStyledTextCtrl:autoCompSetTypeSeparator(STC, ?SEP_INT),
    wxStyledTextCtrl:styleSetSize(STC, ?wxSTC_STYLE_DEFAULT, 10).
