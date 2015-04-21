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

%%% @doc This module provides a code browser with "query bubles". The code
%%% browser can be started as a standalone window (frame), or embedded, or
%%% as a code snippet browser (the offset of the snippet in the containing file
%%% must be set) 
%%% The code browser provides syntax highlight, numbered lines, and switchable
%%% query bubbles (on/off).
%%% This module handles browser events (file drop or add) in order to show
%%% the latest content of the files. (or switch of queries if dropped)
%%%
%%% @author Gabor Hosszu

-module(referl_wx_codebrowser).
-author("Gábor Hosszú").
-behaviour(wx_object).

-export([start_browser/2, start_browser_window/2,
         init/1, terminate/2,  code_change/3,
         handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).
-export([load_code/2, load_snippet/3, enable/2, unload_code/1,
         highlight_positions/2]).
 
-include_lib("referl_core/include/core_export.hrl").
-include_lib("wx/include/wx.hrl").
-include("wx.hrl").
-include("ui.hrl").
 
 -define(stc, wxStyledTextCtrl).

-record(state,
{
 parent,
 access_to_db,
 menu,      %% PopUp menu for right clicks
 bub_parent, %% parent of the bubles (if windowed then the frame, if not, 
             %% then the parent)
 handler, % an event handler (wx_object reference)
 textctrl, % main textcontrol
 bub_mode, % true if we there are clickable query bubles, false if not
 bubble, %% this field contains a query bubble if there is one activated (or no_bub)
 fpath , %% no_file if no file loaded
 offset  %% When a snippet (a part of a file) is loaded, the user must give the
         %% start position of it in the file, and also provide the snippet itself
         %% as a string. The position is needed for the queries.
    }).

-define(runPosQuery, 9100).
-define(runPredefQuery, 9200).

-define(DUPSEARCH_ID_MX, 9900).
-define(DUPSEARCH_ID_ST, 9901).
-define(DUPSEARCH_ID_SWM, 9902).
-define(DUPSEARCH_ID_MXF, 9903).
-define(DUPSEARCH_ID_FST, 9904).
    
%% @doc When starting standalone Parent should be e.g a Panel
%% When starting windowed Parent should be the Wx Server
%% QHandler is a Query Handler (wx_object PID) 
%% (e.g a running referl_wx_queries object's PID) which can handle
%% query info messages (request to run a query)
%% A handler module implements an "Event Handler", which means that it
%% handles {request_for_query, {RequesterPid, RequesterObj}, QStr, File, Pos}
%% and {request_for_dupcode_search, ModuleName, StartPos, EndPos}
%% info messages coming from the Code Browser (referl_wx_codebrowser)
start_browser(Parent, EvtHandler) ->
    wx_object:start_link(?MODULE, {Parent, no_window, EvtHandler}, []).
 
start_browser_window(Parent, EvtHandler) ->
    wx_object:start_link(?MODULE, {Parent, windowed, EvtHandler}, []).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
init({Parent, Mode, EvtHandler}) ->
        wx:batch(fun() -> do_init({Parent, Mode, EvtHandler}) end).
 
do_init({Parent, no_window, EvtHandler}) ->
    {TextCtrl, PopupMenu} = init_browser(Parent),  

    {TextCtrl, #state{parent=Parent, handler=EvtHandler, textctrl=TextCtrl,
             bub_parent=Parent, bubble=no_bub, fpath=no_file,
             bub_mode=true, access_to_db=true, menu=PopupMenu}};
    
do_init({Parent, windowed, EvtHandler}) ->
    CodeFrame = wxFrame:new(Parent, ?wxID_ANY, "Code browser", 
                                                         [{size,{700, 550}}, 
                                                          {pos, {150, 150}}]),
    Panel = wxPanel:new(CodeFrame, []),
    {TextCtrl, PopupMenu} = init_browser(Panel),
    MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(MainSizer, TextCtrl, 
                           [{border, 10}, {flag, ?wxEXPAND}, {proportion, 1}]),
    wxPanel:setSizer(Panel, MainSizer),
    
    wxFrame:connect(CodeFrame, close_window),
    wxFrame:show(CodeFrame),
    {CodeFrame, #state{parent=Parent, handler=EvtHandler, textctrl=TextCtrl,
          bub_parent=CodeFrame, bubble=no_bub, fpath=no_file,
          bub_mode=true, access_to_db=true, menu=PopupMenu}}.

init_browser(Panel) ->
    TextControl = new_code_area(Panel),
    ?stc:connect(TextControl, stc_hotspot_click),
    ?stc:usePopUp(TextControl, false),
    PopupMenu = wxMenu:new(),
    wxMenu:append(PopupMenu, ?DUPSEARCH_ID_MX, "Duplicates in selection... (matrix)"),
    wxMenu:append(PopupMenu, ?DUPSEARCH_ID_ST, "Duplicates in selection... (suffix_tree)"),
    wxMenu:append(PopupMenu, ?DUPSEARCH_ID_SWM, "Duplicates in selection... (sw_metrics)"),
    wxMenu:append(PopupMenu, ?DUPSEARCH_ID_MXF, "Duplicates in selection... (matrix_filtered)"),
    wxMenu:append(PopupMenu, ?DUPSEARCH_ID_FST, "Duplicates in selection... (filtered_suffix_tree)"),
    wxWindow:connect(Panel, context_menu),
    wxMenu:connect(PopupMenu, command_menu_selected),
    {TextControl, PopupMenu}.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
%% Close event (bubble or browser)
handle_event(#wx{event=#wxClose{}, obj=Obj}, 
            State = #state{bub_parent = Frame, bubble=MaybeBub}) ->
    case MaybeBub of
        {Bub, _, _, _} ->   wxWindow:destroy(Bub);
        _              ->   ok
    end,
    case Obj of
        Frame ->
            wxWindow:destroy(State#state.menu),
            wxWindow:destroy(Frame),
            {stop, normal, State};
        _     ->
            {noreply, State#state{ bubble=no_bub }}
    end;

%% Right click
handle_event(#wx{event=#wxContextMenu{}, obj=_Obj}, 
             State = #state{ parent=Panel, menu=PopupMenu }) ->
    wxWindow:popupMenu(Panel, PopupMenu),
    {noreply, State#state{}};

handle_event(#wx{event=#wxCommand{ type=command_menu_selected }, obj=_Obj, id=Id}, 
             State = #state{ textctrl=TextCtrl, menu=_PopupMenu,
                             handler=DupHandler }) ->
    Algorithm = case Id of
        ?DUPSEARCH_ID_MX -> matrix;
        ?DUPSEARCH_ID_ST -> suffix_tree;
        ?DUPSEARCH_ID_SWM -> sw_metrics;
        ?DUPSEARCH_ID_MXF -> matrixfilter;
        ?DUPSEARCH_ID_FST -> filtered_suffix_tree
    end,
    StartPos = ?stc:getSelectionStart(TextCtrl),
    EndPos = ?stc:getSelectionEnd(TextCtrl),
    ModulePath = State#state.fpath,
    if StartPos =< EndPos ->
               ask_for_dupsearch(DupHandler, Algorithm, ModulePath, StartPos, EndPos);
       true -> ok
    end,
    {noreply, State#state{}};

%% Instantiation of bubbles when clicking on a hotspot (in code)
handle_event(#wx{event=#wxStyledText{ type=stc_hotspot_click, 
             position=Pos }}, State = #state{ fpath=File, offset=Offset,
                                              bub_mode=true, 
                                              access_to_db=true}) ->
    {X, Y} = wx_misc:getMousePosition(),
    case State#state.bubble of
        no_bub                 -> nothing_to_do;
        {MiniFrame, _, _, _}   -> wxWindow:destroy(MiniFrame);
        X                      -> ok
    end,
    Bubble=new_bubble(State#state.bub_parent, X, Y, File, Pos, Offset),
    {noreply, State#state{ bubble = Bubble }};

%% Button clicks
handle_event(#wx{event = #wxCommand{type = command_button_clicked},  id=Id,
                obj=Obj   },  State = #state{ bubble={_, TextCtrl, File, Pos},
                         handler=Handler, offset=Offset }) -> 
    case Id of
        ?runPosQuery  -> QStr = ?Logic:trim_whitespace(
                                            wxStyledTextCtrl:getText(TextCtrl)),
                         ask_for_query(QStr, File, Pos+Offset, Handler, Obj);
        ?runPredefQuery ->
                         QStr = wxToolTip:getTip(wxButton:getToolTip(Obj)),
                         ask_for_query(QStr, File, Pos+Offset, Handler, Obj);
        _             -> ok
    end,
    {MiniFrame, _, _, _} = State#state.bubble,
    wxWindow:destroy(MiniFrame),
    Bub = no_bub,
    {noreply,State#state{ bubble=Bub }};

%% Autocompletion
handle_event(#wx{event = #wxStyledText{type = stc_charadded}},
      State = #state{ bubble={_, TextCtrl, _File, _Pos}                 }) ->
    CursorPos = wxStyledTextCtrl:getAnchor(TextCtrl),
    Txt = wxStyledTextCtrl:getTextRange(TextCtrl, 0, CursorPos),
    Res = ?HandleRes(?ExecLCall( autocomplete, [Txt]), [] ),
    {CompleteList0, _ReplaceWith} = lists:unzip(Res),
    CompleteList = string:join(CompleteList0, ?SEP),
    L = ?HandleRes(?ExecLCall( calc_ac_len_from_clist, [Txt, CompleteList0]), [] ),
    CompleteList =/= [] andalso 
        wxStyledTextCtrl:autoCompShow(TextCtrl, L, CompleteList),
    {noreply,State};

%% Closing windows when user clicks next to the query bubble
%% It's a trick: we get the actually focused window, and cast it as a wxButton
%% (casting is NOT the C++ casting! See wxErlang docs.) and check if it is one 
%% of the buttons of our query bubble
%% One of the Btns is the query input styled text control itself.
handle_event(#wx{event=#wxFocus{type=kill_focus}, userData=Btns }, 
                                State = #state{ }) ->
    BtnObj = wx:typeCast(wxWindow:findFocus(),wxButton),
    AutoComp = case State#state.bubble of
        no_bub           -> false;
        {_, Txt, _, _}   -> wxStyledTextCtrl:autoCompActive(Txt)
    end, 
    HasFocus = ((lists:member(BtnObj,Btns)) or AutoComp),                    
    Bub =
    case State#state.bubble of
        no_bub                 -> State#state.bubble;
        {MiniFrame, _, _, _}  when not HasFocus
                               -> wxWindow:destroy(MiniFrame),
                                  no_bub;
        _                      -> State#state.bubble
    end,
    {noreply, State#state{ bubble = Bub }};

handle_event(#wx{}, State = #state{}) ->
    {noreply, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply,State}.

%% @private
%% DB access
handle_info({browser_event, db_access, Bool}, State=#state{bub_mode=true}) ->
    Bub = codebrowser_enabler(State, Bool),
    {noreply, State#state{ bubble=Bub, access_to_db=Bool }};

handle_info({browser_event, db_access, Bool}, State=#state{bub_mode=false}) ->
    {noreply, State#state{ access_to_db=Bool }};

%% Callbacks handled as normal gen_server callbacks
handle_info(stop, State) ->
    {stop, ignore, State};

handle_info({browser_event, file_drop, Filepath}, 
        State=#state{ fpath=Filepath }) ->
    Bub = codebrowser_enabler(State, false),
    {noreply, State#state{ bub_mode=false, bubble=Bub }};

handle_info({browser_event, file_add, Filepath}, 
        State=#state{textctrl = TextCtrl, fpath=Filepath}) ->
    ?stc:setMarginType(TextCtrl, 0, ?wxSTC_MARGIN_NUMBER),
    load_code_to_txtctrl(undload_code_from_txtctrl(TextCtrl), Filepath, no_pos),
    {noreply, State#state{ bub_mode=true }};

handle_info({browser_event, reset, _}, 
        State=#state{textctrl = _TextCtrl }) ->
    Bub = codebrowser_enabler(State, false),
    {noreply, State#state{ bub_mode=false, bubble=Bub }};

handle_info(_Msg, State) ->
    {noreply, State}.

%% @private
%% Enabling/disabling bubles
handle_call({enable, true}, _From, State=#state{ }) ->
    codebrowser_enabler(State, true),
    {reply, ok, State#state{ bub_mode=true }};
handle_call({enable, false}, _From, State=#state{ }) ->
    Bub = codebrowser_enabler(State, false),
    {reply, ok, State#state{ bub_mode=false, bubble=Bub }};

%% Loading codes
handle_call({load_code, {Filepath, Pos}}, _From, 
                                          State=#state{textctrl = TextCtrl}) ->
    ?stc:setMarginType(TextCtrl, 0, ?wxSTC_MARGIN_NUMBER),
    case load_code_to_txtctrl(undload_code_from_txtctrl(TextCtrl), 
                              Filepath, Pos) of
        TextCtrl -> codebrowser_enabler(State, true),
                    {reply, ok, State#state{ fpath = Filepath,
                                             offset= 0 , bub_mode=true }};
        Err      -> {reply, Err, State}
    end;

%% Loadings code snippets
handle_call({load_snippet,Snippet, Filepath, Offset, Pos, _Line, Range},
                                 _From, State=#state{textctrl = TextCtrl}) ->
    % ?stc:setMarginType(TextCtrl, 0, ?wxSTC_MARGIN_NUMBER),
    % SnippetPLusLines = lists:flatten(lists:duplicate(0, "\n")) ++ Snippet,
    case load_string_to_txtctrl(undload_code_from_txtctrl(TextCtrl), 
                                                    Snippet, Pos) of
        TextCtrl -> 
                    ?stc:setMarginWidth(TextCtrl, 0, 0),
                    codebrowser_enabler(State, true),
                    set_permanent_selection(TextCtrl, 
                                    remove_offset(Offset, Range)),
                    {reply, ok, State#state{ fpath = Filepath,
                                             offset= Offset, bub_mode=true }};
        Err      -> {reply, Err, State}
    end;

%% Unloading code
handle_call({unload_code}, _From, State=#state{textctrl = TextCtrl}) ->
    undload_code_from_txtctrl(TextCtrl), 
    {reply, ok, State#state{ fpath = no_file, offset = 0 }};

%% Highlighting positions (list)
handle_call({highlight, PosList}, _From, State=#state{textctrl = TextCtrl}) ->
    %clear_permanent(TextCtrl),
    %?stc:styleResetDefault(TextCtrl),
    clear_permanent(TextCtrl),
    set_permanent_selections(TextCtrl, PosList), 
    {reply, ok, State};

handle_call(Msg, _From, State) ->
    {reply, {error, "Unknown call: " ++ io_lib:write(Msg)}, State}.
 
%% @private
code_change(_, _, State) ->  %%this is just a stub
    {stop, ignore, State}.
 
%% @private
terminate(_Reason, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exported functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Loads code to the browser or gives a wxError tuple
%% Props is a proplist containing key 'file'
%% Optional keys: pos (scalar position {From, To})
load_code(Browser, Props) -> 
    Filepath = proplists:get_value(file, Props, no_file),
    case Filepath of
        no_file -> ?wxErr("No filepath given!");
        _         -> Pos = proplists:get_value(pos, Props, no_pos),
                     Pid = wx_object:get_pid(wx:typeCast(Browser, wxWindow)),
                     case wx_object:call(Pid, {load_code, {Filepath, Pos}}) of
                        ok -> ok;
                        {error, enoent}
                               -> ?wxErr("No such file or directory: " 
                                            ++ Filepath);
                        _Error -> ?wxErr("Error when loading file.")
                    end
    end.

%% @doc Unload code
unload_code(Browser) -> 
    Pid = wx_object:get_pid(wx:typeCast(Browser, wxWindow)),
    wx_object:call(Pid, {unload_code}).

%% @doc  Props is a proplist containing key 'file' and 'startpos', where the snippet
%% starts
%% Optional keys:
%%                pos (scalar position {From, To}) - offset for queries
%%                range (scalar position {From, To}) - permanent selection
%% Loads snippet to the browser or gives a wxError tuple
%% Snippet is a (preloaded/user-given) text from a file. If the file doesnt con
%% tain this snippet, queries will always fail. If the user-given offset (pos) 
%% is wrong, queries will also fail.
load_snippet(Browser, Snippet, Props) ->
    Filepath = proplists:get_value(file, Props, no_file),
    Offset = proplists:get_value(startpos, Props, no_pos),
    Pos = proplists:get_value(pos, Props, no_pos),
    Line = proplists:get_value(line, Props, 1),
    Range = proplists:get_value(range, Props, no_pos),
    if (Filepath == no_file) or (Offset == no_pos) ->
            ?wxErr("Error when snippet: no file or offset given.");
        true ->
            Pid = wx_object:get_pid(wx:typeCast(Browser, wxWindow)),
            wx_object:call(Pid, 
                {load_snippet, Snippet, Filepath, Offset, Pos, Line, Range})
    end.

%% @doc Enable / disable browser
enable(Browser, Bool) ->
    Pid = wx_object:get_pid(wx:typeCast(Browser, wxWindow)),
    wx_object:call(Pid, {enable, Bool}).

%% @doc Highlight positions
highlight_positions(Browser, PosList) ->
    Pid = wx_object:get_pid(wx:typeCast(Browser, wxWindow)),
    wx_object:call(Pid, {highlight, PosList}).   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new_code_area(Parent) ->
    TextControl = ?stc:new(Parent),
    code_area(TextControl).

code_area(TextControl) ->

    ?stc:styleClearAll(TextControl),
    ?stc:setLexer(TextControl, ?wxSTC_LEX_ERLANG),
    ?stc:textWidth(TextControl, ?wxSTC_STYLE_DEFAULT, "9"),
    ?stc:setMarginWidth(TextControl, 0, 0),
    ?stc:setMarginWidth(TextControl, 1, 0),

    ?stc:setSelectionMode(TextControl, ?wxSTC_SEL_LINES),
    %%?stc:hideSelection(TextControl, true),

    set_styles(TextControl, true),
    ?stc:setKeyWords(TextControl, 0, keyWords()),
    %% Scrolling
    Policy = ?wxSTC_CARET_SLOP bor ?wxSTC_CARET_JUMPS bor ?wxSTC_CARET_EVEN, 
    ?stc:setYCaretPolicy(TextControl, Policy, 3),
    ?stc:setVisiblePolicy(TextControl, Policy, 3),
    ?stc:setReadOnly(TextControl, true),
    TextControl.

load_code_to_txtctrl(TextControl, Filepath, Pos) ->
    case read_file(Filepath) of
        {ok, Code} ->
            load_string_to_txtctrl(TextControl, Code, Pos);
        Err ->
            Err
    end.

load_string_to_txtctrl(TextControl, Code, Pos) when is_list(Code) ->
    load_string_to_txtctrl(TextControl, list_to_binary(Code), Pos);

load_string_to_txtctrl(TextControl, Code, Pos) when is_binary(Code) ->
    ?stc:setReadOnly(TextControl, false),
    ?stc:setTextRaw(TextControl, <<Code/binary, 0:8>>),
    ?stc:convertEOLs(TextControl, ?wxSTC_EOL_LF),
    ?stc:setEOLMode(TextControl, ?wxSTC_EOL_LF),
    set_margins(TextControl),
    ?stc:setReadOnly(TextControl, true),
    highlight_area(TextControl, pos_conversion_helper(Pos)),

    TextControl.

read_file(Filepath) -> %% TODO rewrite is_client and move to wxMain
    %% in wxMain a call could handle is_client and there would be no exceptions
    %%
    IsClient = ?HandleRes( ?ExecLCall(is_client, []), true),
    case IsClient of
        true  -> ?HandleRes( ?ExecLCall(cat_file, [Filepath]), []);
        false -> fc_server_read_file(Filepath);
		 %% file:read_file(Filepath);
        _Err  -> IsClient
    end.

fc_server_read_file(Filepath) ->
   Res = ?FileCacheMan:get_file_content(Filepath),
   if is_list(Res)   -> {ok, list_to_binary(Res)};
      true           -> file_read_error
   end.

highlight_area(_TextControl, no_pos) -> ok;
highlight_area(TextControl, {From, To}) ->
    ?stc:setSelection(TextControl, From, To).

undload_code_from_txtctrl(TextControl) ->
    ?stc:setReadOnly(TextControl, false),
    ?stc:setTextRaw(TextControl, <<0:8>>),
    ?stc:setReadOnly(TextControl, true),
    TextControl.

keyWords() ->
    L = ["after","begin","case","try","cond","catch","andalso","orelse",
	 "end","fun","if","let","of","query","receive","when","bnot","not",
	 "div","rem","band","and","bor","bxor","bsl","bsr","or","xor", 
     "module", "import", "include", "include_lib", "vsn", "export"],
    lists:flatten([K ++ " " || K <- L] ++ [0]).

%% Opens a 'Query bubble' (Wx Miniframe)
%% Connects it to the close event handler, proper button click handlers,
%% and kill_focus handler (with some tricks, see kill_focus handler)
new_bubble(Parent, X, Y, File, Pos, Offset) ->
    case calc_predefined_queries(File, Pos+Offset) of
        {result, Predef} -> wx:batch( fun() ->
                            new_bubble_fun(Parent, X, Y, File, Pos, Predef) end);
        Err              -> ?HandleRes(Err), 
                            no_bub
    end.

new_bubble_fun(Parent, X, Y, File, Pos, Predefined) -> 
    MiniFrame = wxMiniFrame:new(Parent, ?wxID_ANY, "Query bubble", [{style,
								    ?wxDEFAULT_FRAME_STYLE bor
								    ?wxFRAME_FLOAT_ON_PARENT},
                                    {pos, {X, Y}}]),
    Panel = wxPanel:new(MiniFrame, []),
    Sz = wxBoxSizer:new(?wxVERTICAL),
    TextCtrl = wxStyledTextCtrl:new(Panel, [{size, {180, 30}}]),
    wxStyledTextCtrl:setMarginWidth(TextCtrl, ?wxSTC_MARGIN_NUMBER, 0),
    wxStyledTextCtrl:setUseHorizontalScrollBar(TextCtrl, false),
    wxStyledTextCtrl:autoCompSetSeparator(TextCtrl, ?SEP_INT),

    RunQBtn = wxButton:new(Panel, ?runPosQuery, 
                                    [{label,"Run query"}, {size, {100, 23}}]),
    wxSizer:addSpacer(Sz, 30),
    wxSizer:add(Sz, TextCtrl, [{flag, ?wxCENTRE bor ?wxALL}]),
    wxSizer:addSpacer(Sz, 12),
    wxSizer:add(Sz, RunQBtn, [{flag, ?wxCENTRE bor ?wxALL}]),
    wxSizer:addSpacer(Sz, 12),
    PreBtns = add_predef_q_buttons(Panel, Sz, Predefined),
    wxSizer:addSpacer(Sz, 5),
    % H = 25 * (length(PreBtns) + 1) + 105,

    wxPanel:setSizer(Panel, Sz),
   % {_,H} = wxWindow:getBestSize(Panel),
   % wxMiniFrame:setSize(MiniFrame, {200,H}),
    wxSizer:setSizeHints(Sz, MiniFrame),

    %% CONNECT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    wxWindow:connect(MiniFrame, stc_charadded),
    wxFrame:connect(MiniFrame, close_window),
    wxFrame:connect(RunQBtn, command_button_clicked),
    %% this is a trick to get when the bubble losts focus
    Items = [RunQBtn, wx:typeCast(TextCtrl, wxButton) |PreBtns],
    wxFrame:connect(TextCtrl, kill_focus, [{userData, Items}]), 
    wxWindow:setFocus(TextCtrl), %% Set focus to the query input
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    wxMiniFrame:show(MiniFrame),
    {MiniFrame, TextCtrl, File, Pos}.

calc_predefined_queries(File, Pos) ->
    case ?ExecLCall(definition, [File, Pos]) of
        {error, Error} -> {error, Error};
        {_, Type} -> {result, ?ExecLCall(get_predef_queries, [Type])};
        none      -> {result, []};
        ErrExcp   -> ErrExcp
    end.

%% Add predefined query buttons and return them in a list
add_predef_q_buttons(Panel, Sz, Predef) ->
    FunToRun =
    fun({Title,QStr}) ->
        B = wxButton:new(Panel, ?runPredefQuery, 
                                [{label, Title}, {size, {130, 23}}]),
        wxButton:setToolTip(B, QStr),
        wxSizer:add(Sz, B, [{flag, ?wxCENTRE bor ?wxALL}]),
        wxFrame:connect(B, command_button_clicked),
        B
    end,
    wx:map(FunToRun, Predef).

ask_for_query(QStr, File, Pos, Handler, Obj) ->
    Handler ! {request_for_query, {self(), Obj}, QStr, File, Pos}.

ask_for_dupsearch(DupHandler, Algorithm, ModuleName, StartPos, EndPos) ->
    DupHandler ! {request_for_dupcode_search, Algorithm, ModuleName, StartPos, EndPos}.

%% There is a little difference between scalar positions used by RefactorErl
%% and scalar selection positions used by WxWidgets
%% In RefErl N means the Nth character
%% In Wx the parameter 'From' means the position BEFORE the character, 
%% not the position of the character
pos_conversion_helper({From, To}) -> {From - 1, To};
pos_conversion_helper(Pos) -> Pos.
highlight_conversion_helper({From, To}) -> {From - 1, To - 1};
highlight_conversion_helper(Pos) -> Pos.

%% Set font styles, font foreground and hotspot (on or off)
set_styles(TextControl,HotSpotBool) ->
    FixedFont = wxFont:new(10, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL,[]),
    ?stc:styleSetFont(TextControl, ?wxSTC_STYLE_DEFAULT, FixedFont),
    Styles = erl_styles(),
    SetStyle = fun({Style, Color}) ->
               ?stc:styleSetFont(TextControl, Style, FixedFont),
               ?stc:styleSetForeground(TextControl, Style, Color),
               ?stc:styleSetHotSpot(TextControl, Style, HotSpotBool)
           end,
    [SetStyle(Style) || Style <- Styles].

erl_styles() ->
          [{?wxSTC_ERLANG_DEFAULT,  {0,0,0}},
           {?wxSTC_ERLANG_COMMENT,  {160,53,35}},
           {?wxSTC_ERLANG_VARIABLE, {150,100,40}},
           {?wxSTC_ERLANG_NUMBER,   {5,5,100}},
           {?wxSTC_ERLANG_KEYWORD,  {130,40,172}},
           {?wxSTC_ERLANG_STRING,   {170,45,132}},
           {?wxSTC_ERLANG_OPERATOR, {30,0,0}},
           {?wxSTC_ERLANG_ATOM,     {0,0,0}},
           {?wxSTC_ERLANG_FUNCTION_NAME, {255,0,0}}, %% export list
           {?wxSTC_ERLANG_CHARACTER,{236,155,172}},
           {?wxSTC_ERLANG_MACRO,    {40,144,170}},
           {?wxSTC_ERLANG_RECORD,   {40,100,20}},
           {?wxSTC_ERLANG_SEPARATOR,{0,0,0}},
           {?wxSTC_ERLANG_NODE_NAME,{0,255,0}}].

set_permanent_selection(_TextCtrl, no_pos) -> ok;
set_permanent_selection(TextCtrl, {From, To}) ->
    set_permanent_selections(TextCtrl, [{From, To}]).

set_permanent_selections(_TextCtrl, []) -> ok;
set_permanent_selections(TextCtrl, List) ->
    ?stc:indicatorSetStyle(TextCtrl, 2, ?wxSTC_INDIC_ROUNDBOX),
    P = ?stc:getEndStyled(TextCtrl),
    set_permanent_selections_(TextCtrl, List),
    ?stc:startStyling(TextCtrl, P, 31).

set_permanent_selections_(_TextCtrl, []) -> ok;
set_permanent_selections_(TextCtrl, [{From, To} | Tail]) ->
    {From2, To2} = highlight_conversion_helper({From, To}),
    Cnt = To2 - From2 + 1,
    ?stc:startStyling(TextCtrl, From2, ?wxSTC_INDICS_MASK),
    ?stc:setStyling(TextCtrl, Cnt, ?wxSTC_INDIC2_MASK),
    set_permanent_selections_(TextCtrl, Tail).

set_margins(TextControl) ->
    Lines = ?stc:getLineCount(TextControl),
    Sz = trunc(math:log10(Lines))+1,
    LW = ?stc:textWidth(TextControl, 
                        ?wxSTC_STYLE_LINENUMBER, lists:duplicate(Sz, $9)),
    
    ?stc:setMarginWidth(TextControl, 0, LW+5),
    ?stc:setMarginSensitive(TextControl, 0, true).

clear_permanent(TextCtrl) ->
    ?stc:indicatorSetStyle(TextCtrl, 1, ?wxSTC_INDIC_HIDDEN),
    Cnt = ?stc:getLength(TextCtrl),
    ?stc:startStyling(TextCtrl, 0, ?wxSTC_INDICS_MASK),
    ?stc:setStyling(TextCtrl, Cnt, ?wxSTC_INDIC1_MASK).

remove_offset(_Offset, no_pos) -> no_pos;
remove_offset(Offset, {From, To}) -> {From-Offset,To-Offset}.

codebrowser_enabler(State, false) ->
    set_styles(State#state.textctrl, false),
    case State#state.bubble of
        no_bub                 -> no_bub;
        {MiniFrame, _, _, _}   -> wxWindow:destroy(MiniFrame),
                                  no_bub
    end;

codebrowser_enabler(State, true) ->
    set_styles(State#state.textctrl, true),
    no_bub.
