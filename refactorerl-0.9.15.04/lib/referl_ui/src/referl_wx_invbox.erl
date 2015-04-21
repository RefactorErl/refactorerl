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

%%% @doc This module provides a new Wx graphical item, called invbox. There are
%%% two types of it mentioned as invbox and memobox. The first is able to dis-
%%% play code snippets and start queries from there and the second is for remin-
%%% ders , notes or comments. 
%%% Such a box can be dragged (moving it on the screen). There are buttons on it
%%% to hide it, give it a name, show its built-in menu, start a query.
%%% An invbox communicates with its parent wx_object using info messages, these
%%% are called "inv_request" messages, and are sent/handled like other info
%%% messages.
%%% @author Gabor Hosszu

-module(referl_wx_invbox).
-author("Gábor Hosszú").
-behaviour(wx_object).
 
-export([new/12, new/11, new/6, get_text/1, 
         get_file_data/1, moveTo/2, disable/1, get_name/1, if_hidden/1,
         hide/2, set_name/2, get_pos/1]).
-export([start/1, init/1, terminate/2,  code_change/3,
         handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).
 
-include_lib("referl_core/include/core_export.hrl").
-include_lib("referl_gen/include/gen_export.hrl").
-include_lib("wx/include/wx.hrl").
-include("wx.hrl").

-define(addMemoBtn, 7101).
-define(delBtn,     7102).
-define(movBtn,     7103).
-define(newBtn,     7104).
-define(hideBtn,    7110).
-define(menuBtn,    7115).
 
-record(state,
{
 parent,
 pos,       %% When dragging, it is the difference 
            %% between the position of the mouse and the dragged item
 mytype,    %% Invbox or memobox
 filedata,  %% This is a redundant field, it contains loaded file name, offset, 
            %% startline, htext (from htextbox)
 self,
 hidden,    %% Whether we hide or show this box
 hidebtn,   %% Button for hiding the box
 dim,       %% The initial dimensions of the box
 namebox,   %% Stores the name in case of invboxes
 menu,      %% Tuple of IsMenuShown (bool), main sizer, menu sizer, menu btn
 textsz,    %% Sizer of textcontrol
 htextbox,  %% Static textbox, shown when the box is in hidden state
 drag,
 brw,       %% Code browser or text control for memos
 pid        %% Query handler and investigation handler to 
            %% which queries and inv requests should be routed
    }).

%% @doc Start a new investigation box
new(Parent, W, H, Text, Filepath, Offset, Line, Pid, Range, HText, ToolTip) ->
    new(Parent, W, H, Text, Filepath, Offset, Line, Pid, 
                                true, Range, HText, ToolTip).
new(Parent, W, H, Text, Filepath, Offset, Line, Pid, BubMode, Range, 
                                                            HText, ToolTip) ->
    Obj = start({invbox, Parent, W,   H,       Text,  Filepath, 
                 Offset, Line,   Pid, BubMode, Range, HText, ToolTip}),
    wxFrame:show(Obj),
    Obj.
new(Parent, W, H, Pid, Text, ToolTip) ->
    Obj = start({memobox, Parent, W, H, Pid, Text, ToolTip}),
    wxFrame:show(Obj),
    Obj.

%% @private
start({invbox, Parent, W, H, Text, Filepath, Offset, Line, Pid, BubMode, Range, 
                                                        HText,ToolTip}) ->
    wx_object:start_link(?MODULE, {invbox, Parent, W, H, Text, Filepath,
                        Offset, Line, Pid, BubMode, Range, HText, ToolTip},[]);

start({memobox, Parent, W, H, Pid, Text, ToolTip}) ->
    wx_object:start_link(?MODULE, {memobox, Parent, W, H, Pid, 
                                            Text, ToolTip}, []).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
init({invbox, Parent, W, H, Text, Filepath, Offset, Line, Pid, BubMode, Range, 
                                                            HText, ToolTip}) ->
    wx:batch(fun() -> do_init(Parent, W,    H,   Text,    Filepath,  Offset, 
                              Line, Pid, BubMode, Range, HText, ToolTip) end);
init({memobox, Parent, W, H, Pid, Text, ToolTip}) ->
    wx:batch(fun() -> do_init(Parent, W, H, Pid, Text, ToolTip) end).
 
do_init(Parent, W, H, Text, Filepath, Offset, Line, Pid, BubMode, Range, 
                                                         HText, ToolTip) ->
    Box = wxPanel:new(Parent, [{size, {W, H}},
                              {style, ?wxBORDER_SUNKEN}]),
    P = wxScrolledWindow:calcScrolledPosition(Parent,wxPanel:getPosition(Box)),
    Sz = wxBoxSizer:new(?wxVERTICAL), % Box, []),
    TitleSz = wxBoxSizer:new(?wxHORIZONTAL),
    TitleSz2 = wxBoxSizer:new(?wxVERTICAL),
    BtnSz = wxBoxSizer:new(?wxVERTICAL),
    BtnSz1 = wxBoxSizer:new(?wxHORIZONTAL),
    TxtSz = wxBoxSizer:new(?wxVERTICAL),
    Brw = ?wxCB:start_browser(Box, self()),
    W0 = round(( W - 4)/2),  %% FONT SIZE?? TODO
    S =  {size, {W0,21}},
    ?wxCB:load_snippet(Brw, Text, [{file, Filepath}, {startpos, Offset},
                                   {line, Line},     {range,     Range}]),
    not BubMode andalso ?wxCB:enable(Brw, false),
    FileData = {Filepath, Offset, Line, Range, HText},
    HideBtn = wxButton:new(Box, ?hideBtn, [{label,"Hide"},  {size, {50,23}}]),
    MenuBtn = wxButton:new(Box, ?menuBtn, [{label,"V"},  {size, {20,23}}]),
    AddBtn = wxButton:new(Box, ?addMemoBtn, [{label,"Add memo"}, S]),
    DelBtn = wxButton:new(Box, ?delBtn, [{label,"Delete"}, S]),
    BtnSz2 = wxBoxSizer:new(?wxHORIZONTAL),
    MovBtn = wxButton:new(Box, ?movBtn, [{label,"Move to new"}, S]),
    NewBtn = wxButton:new(Box, ?newBtn, [{label,"New investigation"}, S]),
    set_tooltips(AddBtn, DelBtn, MovBtn, NewBtn),
    NameBox = wxTextCtrl:new(Box, ?wxID_ANY, [{size, {W0,22}}]),
    HTextBox = wxStaticText:new(Box, ?wxID_ANY, HText),
    wxSizer:add(TitleSz, NameBox),
    wxSizer:add(TitleSz, MenuBtn),
    
    wxSizer:add(TitleSz, TitleSz2, [{proportion,1},{flag,?wxEXPAND bor ?wxALL}]),
    wxSizer:add(TitleSz2, HideBtn),%, [{flag, ?wxALIGN_RIGHT}]),

    wxSizer:add(BtnSz1, AddBtn),
    wxSizer:add(BtnSz1, DelBtn),
    wxSizer:add(BtnSz2, MovBtn),
    wxSizer:add(BtnSz2, NewBtn),
    wxSizer:addSpacer(Sz, 5),
    wxSizer:add(Sz, TitleSz, [{flag, ?wxEXPAND bor ?wxALL}]),
    wxSizer:add(BtnSz, BtnSz1),
    wxSizer:add(BtnSz, BtnSz2),
    wxSizer:add(Sz, BtnSz),
    wxSizer:add(Sz, HTextBox, [{border,2},{flag,?wxALL}]),
    wxSizer:add(TxtSz,Brw,[{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sz,TxtSz,[{flag, ?wxEXPAND},{proportion, 1}]),
    wxSizer:addSpacer(Sz, 3),
    
    
    wxWindow:hide(HTextBox),
    wxPanel:setSizer(Box, Sz),
    wxSizer:layout(Sz),
    wxWindow:layout(Box),
    wxWindow:setToolTip(Box, ToolTip),
    show_menu(false, Sz, BtnSz), %% Firstly, we hide the menu
    wxFrame:show(Box),
    
    %% CONNECT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    wxPanel:connect(Box, left_down),
    wxPanel:connect(Box, left_up),
    wxPanel:connect(Box, motion),
    wxPanel:connect(AddBtn, command_button_clicked),
    wxPanel:connect(DelBtn, command_button_clicked),
    wxPanel:connect(MovBtn, command_button_clicked),
    wxPanel:connect(NewBtn, command_button_clicked),
    wxPanel:connect(MenuBtn, command_button_clicked),
    wxPanel:connect(HideBtn, command_button_clicked),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    {Box, #state{parent=Parent, self=Box, pos=P, drag=false, brw=Brw, 
            pid=Pid, mytype=invbox, filedata=FileData, hidden=false,
            dim={W,H}, namebox=NameBox, menu={false, Sz, BtnSz, MenuBtn},
            htextbox=HTextBox, hidebtn=HideBtn, textsz=TxtSz }}.

do_init(Parent, W, H, Pid, Text, ToolTip) ->
    Box = wxPanel:new(Parent, [{size, {W, H}},
                              {style, ?wxBORDER_SUNKEN}]),
    P = wxScrolledWindow:calcScrolledPosition(Parent,wxPanel:getPosition(Box)),
    Sz = wxBoxSizer:new(?wxVERTICAL), % Box, []),
    TxtSz = wxBoxSizer:new(?wxVERTICAL),
    W0 = W - 4,
    DelBtn = wxButton:new(Box, ?delBtn, [{label,"Delete"}, 
                                                    {size, {round(W0/2),23}}]),
    Txt = wxTextCtrl:new(Box, ?wxID_ANY, [{style, ?wxTE_PROCESS_ENTER bor
                                                  ?wxTE_PROCESS_TAB bor 
                                                  ?wxTE_MULTILINE}]),
    wxSizer:add(TxtSz,Txt, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxTextCtrl:setValue(Txt, Text),
    wxSizer:addSpacer(Sz, 5),
    wxSizer:add(Sz,DelBtn, []),
    wxSizer:add(Sz,TxtSz,[{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:addSpacer(Sz, 5),
    wxWindow:setToolTip(Box, ToolTip),
    wxPanel:setSizer(Box, Sz),
    wxSizer:layout(Sz),
    wxWindow:layout(Box),
    wxFrame:show(Box),
    %% CONNECT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    wxPanel:connect(Box, left_down),
    wxPanel:connect(Box, left_up),
    wxPanel:connect(Box, motion),
    wxPanel:connect(DelBtn, command_button_clicked),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {Box, #state{parent=Parent, self=Box, pos=P, drag=false, brw=Txt, pid=Pid, 
                 mytype=memobox, hidden=false, dim={W,H}, textsz=TxtSz}}.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
%% Hiding
handle_event(#wx{event = #wxCommand{type=command_button_clicked},  id=?hideBtn},
                State = #state{ brw=Brw, self=Self, hidden=Hide, dim={W,H},
                           htextbox=HTB, menu={_Shown, Sz, Menu, MenuBtn},
                           hidebtn=HideBtn, parent=Parent, textsz=TxtSz }) ->
    hide_itself(Self, Brw, HideBtn, MenuBtn, HTB, Sz, TxtSz,
                                                         Menu, not Hide, W, H),
    wxFrame:refresh(Parent),
    {noreply, State#state{ hidden=(not Hide), menu={false, Sz, Menu, MenuBtn}}};

%% Menu
handle_event(#wx{event = #wxCommand{type=command_button_clicked},  id=?menuBtn},
             State=#state{ hidden=Hidden, menu={Shown, Sz, Menu, MenuBtn},
                           self=Box, dim={W,_H} }) ->
    show_menu(not Shown, Sz, Menu),
    {_,BH} = wxWindow:getBestSize(Box),
    Hidden andalso wxWindow:setSize(Box,{W,BH}),
    wxFrame:refresh(State#state.parent),
    {noreply, State#state{ menu={(not Shown), Sz, Menu, MenuBtn} } };

%% Other button clicks
handle_event(#wx{event = #wxCommand{type=command_button_clicked},  id=Id},
                            State = #state{ pid=Pid, self=Self }) ->
    case Id of
        ?addMemoBtn -> request_memo(Pid, self(), Self);
        ?delBtn     -> request_inv(Pid, self(), Self, del_subtree);
        ?movBtn     -> request_inv(Pid, self(), Self, mov_to_new);
        ?newBtn     -> request_inv(Pid, self(), Self, new_inv);
        _ -> ok
    end,
    {noreply, State#state{ } };

%% In these calls we make the box movable by the user
%% Left mouse down/up, and mouse motions
handle_event(#wx{ event=#wxMouse{ type=left_down } }, 
    State = #state{ pos=_, self=Box, parent=Parent }) ->
    {X0,Y0} = wxPanel:getPosition(Box),
    {X, Y} = wxWindow:screenToClient(Parent, wx_misc:getMousePosition()),
    wxPanel:captureMouse(Box),
    wxFrame:refresh(Parent),
    {noreply, State#state{ pos={X-X0,Y-Y0}, drag=true } };

handle_event(#wx{ event=#wxMouse{ type=left_up } }, 
    State = #state{ self=Box, parent=Parent }) ->
    wxPanel:releaseMouse(Box),
    wxFrame:refresh(Parent),
    P2 = wxPanel:getPosition(Box),
    {noreply, State#state{ pos=P2, drag=false } };

handle_event(#wx{ event=#wxMouse{ type=motion } }, 
    State = #state{ self=Box, pos={X0,Y0}, drag=Drag, parent=Parent }) ->
    case Drag of
        true ->  
            {X, Y} = wx_misc:getMousePosition(),
            P = wxWindow:screenToClient(Parent, {X-X0,Y-Y0}),
            wxPanel:move(Box,P),
            wxFrame:refresh(Parent),
            {noreply, State#state{ }};
        false -> 
            {noreply, State}
    end;

handle_event(#wx{}, State = #state{}) ->
    {noreply, State}.

%% @private    
handle_cast(_Msg, State) ->
    {noreply,State}.

%% @private
%% Query requests (must be sent to the query handler which was given in new/11)
handle_info({request_for_query, {_RqPid, _}, QStr, File, Pos}, State) ->
    Obj = State#state.self,
    State#state.pid ! {request_for_query, {self(), Obj}, QStr, File, Pos},
    {noreply, State};
     
%% Callbacks handled as normal gen_server callbacks
handle_info(_Msg, State) ->
    {noreply, State}.

%% @private
%% Getting invbox text
handle_call(get_text, _From, State = #state{ brw=Brw }) ->
    Txt = 
    case State#state.mytype of
        invbox -> wxStyledTextCtrl:getText(Brw);
        memobox -> wxTextCtrl:getValue(Brw)
    end,
    {reply, Txt, State};

%% Getting file data
handle_call(get_file_data, _From, State = #state{}) ->
    {reply, State#state.filedata, State};

%% Getting/setting name of the box
handle_call(get_name, _From, State = #state{ namebox=Namebox }) -> 
    Name =  if Namebox =/= undefined -> wxTextCtrl:getValue(Namebox);
               true                  -> "Memobox"
    end,
    {reply, Name, State};
handle_call({set_name, Name}, _From, State = #state{ namebox=Namebox }) ->
    if Namebox =/= undefined -> wxTextCtrl:setValue(Namebox, Name);
       true                  -> ok
    end,
    {reply, ok, State#state{}};

%% If it is a hidden box or not
handle_call(if_hidden, _From, State = #state{}) ->
    {reply, State#state.hidden, State};
handle_call({hide, Bool}, _From, State=#state{ self=Box, brw=Brw, dim={W,H}, 
                            htextbox=HTB, menu={_Shown, Sz, Menu, MBtn},
                            hidebtn=HideBtn, parent=Parent, textsz=TxtSz }) ->
    hide_itself(Box, Brw, HideBtn, MBtn, HTB, Sz, TxtSz, Menu, Bool, W, H),
    wxFrame:refresh(Parent),
    {reply, ok, State#state{ hidden=Bool, menu={false, Sz, Menu, MBtn} }};

%% Moving to a given position
handle_call({moveTo, Pos}, _From, State = #state{ self=Box, parent=_Parent }) ->
    wxPanel:move(Box, Pos),
    {reply, ok, State#state{ pos=Pos }};

%% Getting the actual position of the box
handle_call(get_pos, _From, State = #state{ self=Box, parent=_Parent }) ->
    {reply, wxPanel:getPosition(Box), State};

%% Enabling/disabling the box
handle_call({enable, Bool}, _From, State = #state{ brw=Brw }) ->
     case State#state.mytype of
        invbox -> ?wxCB:enable(Brw, Bool);
        memobox -> wxWindow:enable(Brw, [{enable, Bool}])
    end,   
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {reply, {error, nyi}, State}.
 
%% @private
code_change(_, _, State) ->  %%this is just a stub
    {stop, ignore, State}.
 
%% @private
terminate(_Reason, _State) ->
    ok.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Interface:
%% @doc Get the text contained by the investigation box
get_text(Box) ->
    wx_object:call(Box, get_text).

%% @doc Get file data contained by the investigation box
get_file_data(Box) ->
    wx_object:call(Box, get_file_data).

%% @doc Move the investigation box
moveTo(Box, Pos) ->
    wx_object:call(Box, {moveTo, Pos}).

%% @doc Disable the investigation box
disable(Box) ->
    wx_object:call(Box, {enable, false}).

%% @doc Get the name of the investigation box
get_name(Box) ->
    wx_object:call(Box, get_name).

%% @doc Show if the investigation box is hidden
if_hidden(Box) ->
    wx_object:call(Box, if_hidden).

%% @doc Set the name of the investigation box
set_name(Box, Name) ->
    wx_object:call(Box, {set_name, Name}).

%% @doc Hide the investigation box
hide(Box, Bool) ->
    wx_object:call(Box, {hide, Bool}).

%% @doc Get the position of the investigation box
get_pos(Box) ->
    wx_object:call(Box, get_pos).

%% Request something from the investigation module
%% These are called when the user clicks one of the buttons on the investigation
%% box.
%% RqPid is the pid of the requester box. RqObj is the reference of it.
%% Pid is the pid of the investigation handler module.

request_memo(Pid, RqPid, RqObj) ->
    Pid ! {inv_request, req_for_memo, {RqPid, RqObj}}.

request_inv(Pid, RqPid, RqObj, Req) ->
    Pid ! {inv_request, Req, {RqPid, RqObj}}.

%% Hide/show an invbox
%% W and H are its original dimensions
hide_itself(Box, Browser, HideBtn, _MenuBtn, HTextBox, Sz, TxtSz,
                                                Menu, Bool, W, H) ->
    %wxStyledTextCtrl:setUseHorizontalScrollBar(Browser,not Bool),
    %wxStyledTextCtrl:setUseVerticalScrollBar(Browser,not Bool),
    wxWindow:show(HTextBox, [{show, Bool}]),
    wxWindow:show(Browser,  [{show, not Bool}]),
    % Menu enabling: wxButton:enable(MenuBtn, [{enable, not Bool}]),
    wxSizer:hide(Sz, Menu, [{recursive, true}]),
    if Bool -> wxButton:setLabel(HideBtn,"Show"),
               wxSizer:hide(Sz, TxtSz, [{recursive, true}]),
               {_,BH} = wxWindow:getBestSize(Box),
               wxWindow:setSize(Box,{W,BH});
       true -> wxButton:setLabel(HideBtn,"Hide"),
               wxSizer:show(Sz, TxtSz, [{recursive, true}]),
               wxWindow:setSize(Box,{W,H})
    end,
    wxSizer:layout(Sz).

show_menu(false, Sz, Menu) ->
    wxSizer:hide(Sz, Menu, [{recursive, true}]),
    wxSizer:layout(Sz);
show_menu(true, Sz, Menu) ->
    wxSizer:show(Sz, Menu, [{recursive, true}]),
    wxSizer:layout(Sz).

set_tooltips(AddBtn, DelBtn, MovBtn, NewBtn) ->
    wxWindow:setToolTip(AddBtn, "Add a new box for comments"),
    wxWindow:setToolTip(DelBtn, "Delete the subtree of this box"),
    wxWindow:setToolTip(MovBtn, "Move this box and its subtree "
                                "to a new investigation"),
    wxWindow:setToolTip(NewBtn, "Start a new investigation with this box as "
                                "its root").