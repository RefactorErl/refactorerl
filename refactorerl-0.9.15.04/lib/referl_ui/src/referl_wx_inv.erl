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

%%% @doc This module provides an interface for Emacs. It is intended to be
%%% `start'ed from the command line using `-noshell'. It reads Erlang terms
%%% from the standard input, and passes them to an interpreter function
%%% (implemented by a gen_server). Responses are written to the standard
%%% output in the form required by the Emacs RefactorErl minor mode.
%%%
%%% It is possible to run this interface on the same node as the main server,
%%% but distributed operation is also supported, when the input loop has its
%%% own dedicated Erlang node. This enables to have an Erlang shell running on
%%% the server node for development.
%%%
%%% @author Gabor Hosszu

-module(referl_wx_inv).
-author("Gábor Hosszú").
-behaviour(wx_object).
 
-export([start/1, init/1, terminate/2,  code_change/3,
         handle_info/2, handle_call/3, handle_cast/2, handle_event/2, 
         handle_sync_event/3]).
 
-include_lib("referl_core/include/core_export.hrl").
-include_lib("referl_gen/include/gen_export.hrl").
-include_lib("wx/include/wx.hrl").
-include("wx.hrl").

%%% @doc This is the investigation handler module. It may get requests from invboxes 
%%% etc. as info messages. See referl_wx_invbox for further information about
%%% this.
%%% This module implements a query handling and a Referl Wx Event handling.
%%%
%%% Notes:
%%% Inv_status is kind of Finite State Machine.
%%% When started, the state is run. When set start node and clicked run
%%% the state is start. When chosen the first node (root, one of the clauses
%%% of the fun set before) the state is started and after the first investi-
%%% gation step, the state is investigation.
%%%
%%% We use some trees in this module, which can be found in referl_wx_logic. 
%%% One of them is a graph which we store in our state and use to drawn the lines 
%%% between nodes, reference nodes (inv/memoboxes) etc. The key of this graph is
%%% a so-called "inv state object", which is made by one of the following  
%%% functions: make_inv_state_object, make_labeled_object
%%% This key contains the pid, the wx reference and the label of a box, so we can
%%% draw a tree using these relatively fast. Two inv state object keys are consi-
%%% dered to be equal when their pids are equal (see function fst/0). In this
%%% graph, the data belonging to graphnodes is a RefactorErl node or the atom memo
%%%
%%% The second graph is a data tree generated from the previous one mentioned above.
%%% This one is stored on the disc in dets, and can be loaded up. 
%%% This tree contains "pid-like ids" (cause they were real pids before saving
%%% to dets) and invnode records as data.
%%% See load investigation functions for details, where this module widely uses 
%%% tree_transformation-s (see the function tree_transformation) to convert a 
%%% tree stored in dets to the drawable one. Here, we also use put/get process 
%%% dictionary functions.
%%% See: state.curr_obj, query handler, referl_wx_invbox
%%% This tree can be converted back to the first tree type mentioned above. 
 
-record(state,
{
 parent,
 access_to_db,
 invarea,
 invname,    %% Name of the current investigation
 invhash,    %% Hash of the current investigation
 reslist,
 invlist,    %% List of investigations which can be loaded
 typech,     %% wxChoice, my or all investigations
 disbtns,    %% Buttons which should be enabled/disabled according to 
             %% investigation selected
 btns,       %% Every button
 inp,
 user,
 inv_status, %% run | start | started | investigation (FSM)
 infolabs,
 objs,       %% investigation boxes in a graph structure (current investigation)
 curr_obj,   %% The actual investigation box (from which the latest query was 
             %% received) and a pid of its Wx_object
 curr_qstr,  %% The actual query with which we progress in our investigation
 qstr_wt,    %% Widget to show the actual query string
 buttsz,     %% Sizer of buttons which must be refreshed when the query text/loaded inv
             %% changes
 sav_status  %% saved | unsaved - This flag shows if there are modifications
    }).
 
-define(runBtn, 6101).
-define(addBtn, 6102).
-define(savBtn, 6103).
-define(loadBtn, 6104).
-define(shrBtn, 6105).
-define(sasBtn, 6106).
-define(delBtn, 6107).

-define(typeCh, 6201).

-define(typeAll, 0).
-define(typeMy,  1).

-define(dropTxt, "Would you like to drop the current investigation, and load another?"
                 " All unsaved changes will be lost.").
-define(overTxt, "Would you like to overwrite the existing investigation?").
-define(hashChTxt, "The database changed since the beginning of the investigation"
                    ", so the investigation boxes are not clickable.").
-define(noResTxt, "The query gave an empty result.").

-define(memW, 200).
-define(memH, 100).
-define(invW, 250).
-define(invH, 150).

%% @private
start(Props) ->
    wx_object:start_link(?MODULE, Props, []).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
init(Props) ->
        wx:batch(fun() -> do_init(Props) end).
 
do_init(Props) ->
    Panel = proplists:get_value(parent, Props),
    User = proplists:get_value(user, Props),
    %% MAC related
    %% StaticBox "definition"
    ButtSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, []), % {label, "wxButton"}
    InvAreaSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, []),   
    Size = {size, {160,26}},
    Btns = [
    B01 = wxButton:new(Panel, ?runBtn, [{label,"Run new investigation"}, Size]),
    B02 = wxButton:new(Panel, ?addBtn, [{label,"Add to investigation"}, Size]),
    B03 = wxButton:new(Panel, ?savBtn, [{label,"Save investigation"}, Size]),
    B06 = wxButton:new(Panel, ?sasBtn, [{label,"Save as..."}, Size]),
    B04 = wxButton:new(Panel, ?loadBtn, [{label,"Load investigation"}, Size]),
    B05 = wxButton:new(Panel, ?shrBtn, [{label,"Share investigation"}, Size]),
    B07 = wxButton:new(Panel, ?delBtn, [{label,"Delete"}, Size]) ],

    %% B06 = wxButton:new(Panel, ?shrAllBtn, [{label,"Share with every user"}]),
    
    Static = wxStaticText:new(Panel, ?wxID_ANY, "Current investigation:", []),
    ResTxt = wxStaticText:new(Panel, ?wxID_ANY, "Results of latest query:", []),
    StFun = wxStaticText:new(Panel, ?wxID_ANY, "Starting function (M:F/A):", []),
    CurrQstr = wxStaticText:new(Panel, ?wxID_ANY, "", []),
    NameLab = wxStaticText:new(Panel, ?wxID_ANY, "", []),
    IfSaved = wxStaticText:new(Panel, ?wxID_ANY, "", []),
    InfoLabs={NameLab, IfSaved},
    Sz = wxBoxSizer:new(?wxHORIZONTAL),
    NameSz = wxBoxSizer:new(?wxHORIZONTAL),
    Expand  = fun(X) -> [{border, X}, {flag, ?wxALIGN_CENTER bor ?wxALL}] end,
    Center  = [{flag, ?wxALIGN_CENTER}],
    InvArea = wxScrolledWindow:new(Panel, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),
    wxFrame:setVirtualSize(InvArea, 2000, 2000),
    wxScrolledWindow:setScrollRate(InvArea, 5, 5),
    wxPanel:setBackgroundColour(InvArea, ?wxWHITE),

    StartInp = wxStyledTextCtrl:new(Panel, [{size, {200, 20}}]),
    ResList = wxListBox:new(Panel, ?wxID_ANY, [{size, {200, 100}}]),
    TypeCh = wxChoice:new(Panel, ?typeCh, [{choices, ["All investigations",
                                                      "My investigations"]}]),
    wxChoice:setSelection(TypeCh, ?typeAll),
    InvList = wxListBox:new(Panel, ?wxID_ANY, [{size, {200, 100}}]),
    set_stc_style(StartInp),
    wxSizer:add(ButtSz,B01, Expand(4)),
    wxSizer:add(ButtSz,StFun, Expand(4)),
    wxSizer:add(ButtSz, StartInp, Center),
    wxSizer:addSpacer(ButtSz,25),    
    wxSizer:add(ButtSz,B02, Center),
    wxSizer:add(ButtSz,ResTxt, Expand(4)),
    wxSizer:add(ButtSz,CurrQstr, Expand(4)),
    wxSizer:add(ButtSz,ResList, Center),
    wxSizer:addSpacer(ButtSz,25),
    wxSizer:add(ButtSz,B03,Center),
    wxSizer:add(ButtSz,B06,Center),
    wxSizer:add(ButtSz,Static,Expand(10)),
    wxSizer:add(NameSz,NameLab,Expand(0)), 
    wxSizer:add(NameSz,IfSaved,Expand(0)),
    wxSizer:add(ButtSz,NameSz,Expand(0)),
    wxSizer:addSpacer(ButtSz,25),
    wxSizer:add(ButtSz,TypeCh),
    wxSizer:add(ButtSz,InvList,Center),
    wxSizer:add(ButtSz,B04,Center),
    wxSizer:add(ButtSz,B05,Center),
    wxSizer:add(ButtSz,B07,Center),
    wxSizer:add(InvAreaSz,InvArea, [{proportion, 1}, {flag, ?wxEXPAND}]),
   
    wxSizer:add(Sz, ButtSz, [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sz, InvAreaSz, [{proportion, 1}, {flag, ?wxEXPAND}]),

    Empty = ?ExecLCall(tree_new, []),

    %% CONNECT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    wxWindow:connect(StartInp, stc_charadded),
    wxPanel:connect(InvArea, paint, [callback]),
    wxWindow:connect(Panel, command_button_clicked),
    wxWindow:connect(TypeCh, command_choice_selected),
    wxWindow:connect(InvList, command_listbox_selected),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    %% INIT
    put(fatal, ignore), %% if defined, no modals will show up on errors, 
    %% and db_busy exception will be thrown
    CurrHash = db_hash(), %% needs DB
    %% Firstly, we show all investigations
    fill_inv_list(InvList,User,?typeAll), %% needs DB
    %% Display name of the unnamed investigation
    display_inv_info(InfoLabs, undefined, unsaved, ButtSz), 
    erase(fatal),

    %% This part was modified to support mac
    wxPanel:setSizer(Panel, Sz),
    wxWindow:fit(Panel),
    wxSizer:layout(Sz),
    
    {Panel, #state{ parent=Panel, invname=undefined, invarea=InvArea,
                    reslist=ResList, user=User, inv_status=run, objs=Empty,
                    invlist=InvList, typech=TypeCh, disbtns={B07, B05},
                    infolabs=InfoLabs, inp=StartInp, invhash=CurrHash,
                    curr_obj=no_obj, qstr_wt=CurrQstr, buttsz=ButtSz,
                    sav_status=unsaved, btns=Btns, curr_qstr=root }}.

set_stc_style(STC) ->
    wxStyledTextCtrl:setMarginWidth(STC, ?wxSTC_MARGIN_NUMBER, 0),
    wxStyledTextCtrl:setUseHorizontalScrollBar(STC, false),
    wxStyledTextCtrl:autoCompSetTypeSeparator(STC, ?SEP_INT),
    wxStyledTextCtrl:styleSetSize(STC, ?wxSTC_STYLE_DEFAULT, 10). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
%% Painting graph edges
handle_sync_event(#wx{event = #wxPaint{} }, _, 
    #state{ invarea=InvArea, objs=Tree }) ->
        DC = wxPaintDC:new(InvArea),
        if length(Tree) > 1 -> tree_draw(DC, Tree),
                               ok;
                       true -> ok
        end,
        wxPaintDC:destroy(DC).

add_node_and_edge(_Tree, no_obj, NxtObj, Label, Node) ->
    NxtObj2 = make_labeled_obj(NxtObj, Label),
    ?ExecLCall(tree_root,[NxtObj2, Node]);
add_node_and_edge(Tree, CurrObj, NxtObj, Label, Node) ->
    NxtObj2 = make_labeled_obj(NxtObj, Label),
    ?ExecLCall(tree_add_node, [Tree, CurrObj, NxtObj2, Node, fst()]).

%% @private
%% On button click we do the proper action
handle_event(#wx{event = #wxCommand{type = command_button_clicked},  id=Id},
         State=#state{ invarea=InvArea, reslist=ResLst, inp=Inp, objs=Tree,
                       curr_obj=CurrObj, inv_status=Status0, curr_qstr=QStr,
                       parent=Panel, typech=TypeCh, invname=CurrName, user=User,
                       invlist=InvList, infolabs=InfoLabs, invhash=CurrHash,
                       buttsz=ButtSz, qstr_wt=QstrWt, sav_status=CurrSvSt,
                       btns=[_,AddB|_] })->
    Empty = ?ExecLCall(tree_new,[]),
    RealId = if ((Id == ?savBtn) and (CurrName == undefined)) -> ?sasBtn;
                true                                          -> Id
             end,
    case RealId of
        ?runBtn ->
            case ask_user(Panel,?dropTxt,Tree=/=Empty) of
                yes ->
                    true = wxPanel:disconnect(InvArea, paint, [callback]),
                    wxPanel:destroyChildren(InvArea),
                    run(ResLst, wxStyledTextCtrl:getText(Inp)),
                    Hash = db_hash(),
                    display_inv_info(InfoLabs, undefined, unsaved, ButtSz),
                    set_current_qstr(QstrWt, "", ButtSz),
                    restrict_btns(true, [AddB]),
                    wxPanel:connect(InvArea, paint, [callback]),
                    wxPanel:refresh(InvArea),
                    {noreply,State#state{ inv_status=start, curr_obj=no_obj, 
                            invname=undefined, objs=Empty, invhash=Hash,
                            curr_qstr=root, sav_status=unsaved }};
                _         ->
                    {noreply,State#state{}}
            end;
        ?addBtn when Status0 =/= started ->
            case wxListBox:getSelection(ResLst) of
                -1 -> {noreply,State#state{ }};
                N  -> {Node, Start, Pos, Filepath, Range, MFA} 
                                         = wxListBox:getClientData(ResLst,N),
                      %% We use the pid as search key in the graph
                      {{_, _},{{Line,_},_}} = Pos,
                      InvObj = add_inv_node(InvArea,Filepath,CurrObj,
                                            Node,Start,Line,Range,MFA,QStr),
                      InvObjPid = wx_object:get_pid(InvObj),
                      NxtObj = make_inv_state_obj(InvObjPid, InvObj),
                      Status=case Status0 of
                        start -> started;
                        investigation -> investigation
                      end,
                      Tree2 = add_node_and_edge(Tree,CurrObj,NxtObj,QStr,Node),
                      display_inv_info(InfoLabs, CurrName, unsaved, ButtSz),
                      wxPanel:refresh(InvArea),
                      {noreply,State#state{ inv_status=Status, objs=Tree2,
                                            sav_status=unsaved }}
            end;
        ?sasBtn ->
            SaveableTree = 
              ?ExecLCall(tree_transformation,[Tree, id_tr(), data_tr(InvArea)]),
            {Name, SvSt} = get_unique_name_and_save(Panel, User, CurrHash, 
                                            SaveableTree, CurrName, CurrSvSt),
            display_inv_info(InfoLabs, Name, SvSt, ButtSz),
            fill_inv_list(InvList,User,wxChoice:getSelection(TypeCh)),
            {noreply,State#state{ invname=Name, sav_status=SvSt }};
        ?savBtn when CurrName =/= undefined ->
            SaveableTree = 
              ?ExecLCall(tree_transformation,[Tree, id_tr(), data_tr(InvArea)]),
            {Name, SvSt} = save_investigation(CurrName, User, CurrHash, 
                                              SaveableTree, CurrSvSt),
            display_inv_info(InfoLabs, Name, SvSt, ButtSz),
            {noreply,State#state{ invname=Name, sav_status=SvSt }};
        ?loadBtn ->
            case wxListBox:getSelection(InvList) of
                -1 -> {noreply,State#state{ }};
                N  -> %% Trick: disconnect our painter fun, not to get exceptions
                      %% cause of the non-existing boxes
                    case ask_user(Panel,?dropTxt,Tree =/= Empty) of
                        yes ->
                            wxPanel:disconnect(InvArea, paint, [callback]),
                            wxPanel:destroyChildren(InvArea),
                            wxListBox:clear(ResLst),
                            {Tree2, Name, Hash} = 
                            load_inv_tree(
                                wxListBox:getClientData(InvList,N),InvArea),
                            display_inv_info(InfoLabs, Name, saved, ButtSz),
                            set_current_qstr(QstrWt, "", ButtSz),
                            restrict_btns(true, [AddB]),
                            wxPanel:connect(InvArea, paint, [callback]),
                            wxPanel:refresh(InvArea),
                            {noreply,State#state{ inv_status=investigation,
                                     objs=Tree2, invname=Name, invhash=Hash,
                                     sav_status=saved }};
                        _ ->
                            {noreply,State#state{}}
                    end
            end;
        ?shrBtn ->
            share_investigation(Panel, InvList, wxListBox:getSelection(InvList)),
            fill_inv_list(InvList,User,wxChoice:getSelection(TypeCh)),
            {noreply,State#state{ }};
        ?delBtn ->
            IName =
            delete_investigation(InvList, wxListBox:getSelection(InvList), User),
            fill_inv_list(InvList,User,wxChoice:getSelection(TypeCh)),
            {NewName, NewSavStatus} =
            if IName == CurrName -> display_inv_info(InfoLabs, undefined, 
                                                         unsaved, ButtSz),
                                    {undefined, unsaved};
               true -> {CurrName, CurrSvSt}
            end,
            {noreply,State#state{ invname=NewName, sav_status=NewSavStatus }};
        _       ->
            {noreply,State#state{ }}
    end;

%% Changing All/My inv lists
handle_event(#wx{event = #wxCommand{type = command_choice_selected}},
         State = #state{ typech=TypeCh, user=User, invlist=InvList }) ->
    fill_inv_list(InvList,User,wxChoice:getSelection(TypeCh)),
    {noreply,State};
handle_event(#wx{event = #wxCommand{type = command_listbox_selected, 
                                    commandInt=-1 } }, State) ->
    {noreply,State};
%% Enabling deletion/sharing according to current selection
handle_event(#wx{event = #wxCommand{type = command_listbox_selected}},
        State = #state{ user=User, invlist=InvList, disbtns={DelBtn,ShrBtn}}) ->
    N = wxListBox:getSelection(InvList),
    K =wxListBox:getClientData(InvList,N),
    {_, Users} = K, 
    Bool = lists:member(User, Users),
    ?wxMain:enable_btns([DelBtn, ShrBtn], Bool),
    {noreply,State};
%% Autocompletion
handle_event(#wx{event = #wxStyledText{type = stc_charadded}, obj=STC },
                                                        State = #state{  }) ->
    Txt = wxStyledTextCtrl:getText(STC),
    Res = ?HandleRes(?ExecLCall( autocomplete_funmod, [Txt,func]), [] ),
    CompleteList = string:join(Res, ?SEP),
    L = length(lists:takewhile(fun(Ch) -> Ch =/= $. end, lists:reverse(Txt))),
    wxStyledTextCtrl:autoCompShow(STC, L, CompleteList),
    {noreply,State};
%% Other messages
handle_event(#wx{}, State = #state{}) ->
    {noreply, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply,State}.

%% @private
%% DB access
handle_info({event, db_access, false},State=#state{ btns=Btns }) ->
    ?wxMain:enable_btns(Btns, false),
    {noreply, State#state{ access_to_db=false }};

handle_info({event, db_access, true},State=#state{ btns=Btns=[_, AddB | _],
                                         objs=Tree, invhash=CurrHash }) ->
    ?wxMain:enable_btns(Btns, true),
    Hash = db_hash(),
    if Hash =/= CurrHash ->
            ?ExecLCall( tree_traversal, [Tree,disable_node()] ),
            restrict_btns(false, [AddB]),
            Tree =/= ?ExecLCall( tree_new, [] ) andalso
             ?wxMain:infowindow_main(?hashChTxt,"Older investigation");
       true -> ok
    end,
    {noreply, State#state{ access_to_db=true }};

%% Query handling functionality
handle_info({request_for_query, RqTuple, QStr, File, Pos}, 
    State = #state{ qstr_wt=QstrWt, buttsz=ButtSz, 
                     user=User, reslist=ListBox    }) ->
    Res = ?HandleRes(?ExecLCall(run_query_or_skel_node, 
                                [User, QStr, {File, Pos}]), [{nodes, []}]),
    Nodes = proplists:get_value(nodes, Res, []),
    run_on_nodes(ListBox, Nodes),
    CurrObj = make_inv_state_obj(RqTuple),
    set_current_qstr(QstrWt, QStr, ButtSz),
    {noreply, State#state{ curr_obj=CurrObj, curr_qstr=QStr,
                           inv_status=investigation }}; 

%% Investigation requests (received from inv boxes)
handle_info({inv_request, req_for_memo, {RqPid, RqObj}}, 
            State = #state{ invarea=InvArea, objs=Tree, buttsz=ButtSz,
                            infolabs=InfLb, invname=CurrName }) ->
    MemoToObj = make_inv_state_obj(RqPid, RqObj),
    {MW, MH} = { round(?invW/2), round(?invH/2)},
    {PX,PY} = ?wxIB:get_pos(RqPid),
    Memo = ?wxIB:new(InvArea, ?memW, ?memH, self(), "", "Memo"),
    ?wxIB:moveTo(Memo, {PX+MW, PY+MH}),
    MemoPid = wx_object:get_pid(Memo),
    MemoObj = make_inv_state_obj(MemoPid, Memo),
    Tree2 = add_node_and_edge(Tree, MemoToObj, MemoObj, "Memo", memo),
    display_inv_info(InfLb, CurrName, unsaved, ButtSz),
    wxPanel:refresh(InvArea),
    {noreply, State#state{ objs=Tree2, sav_status=unsaved }}; 

handle_info({inv_request, Req, {RqPid, RqObj}}, 
            State = #state{ invarea=InvArea, objs=Tree, reslist=ResList,
                            qstr_wt=QstrWt, buttsz=ButtSz, infolabs=InfLb,
                            invname=CurrName }) ->
    ClickedObj = make_inv_state_obj(RqPid, RqObj),
    DelFun = fun({_, InvBox, _}) -> wxPanel:destroy(InvBox) end,
    Ask = (Req =/= del_subtree),
    case ask_user(State#state.parent,?dropTxt,Ask) of
        yes ->
            Tree2 =
            case Req of
                del_subtree -> {NewTree, Deleted} = 
                                ?ExecLCall( tree_delete_subtree,
                                            [Tree,ClickedObj,fst()] ),
                                wx:foreach(DelFun,Deleted),
                                wxListBox:clear(ResList),
                                NewTree;
                mov_to_new  -> {NewTree, Dropped} = ?ExecLCall(tree_get_subtree,
                                                       [Tree,ClickedObj,fst()]),
                                wx:foreach(DelFun,Dropped),
                                wxListBox:clear(ResList),
                                NewTree;
                new_inv     -> {NewTree, Dropped} = 
                                ?ExecLCall(tree_node_to_new_tree,
                                           [Tree,ClickedObj,fst()]),
                                wx:foreach(DelFun,Dropped),
                                wxListBox:clear(ResList),
                                NewTree;                            
                _           -> Tree
            end,
            wxPanel:refresh(InvArea),
            display_inv_info(InfLb, CurrName, unsaved, ButtSz),
            set_current_qstr(QstrWt, "", ButtSz),
            {noreply, State#state{ objs=Tree2, sav_status=unsaved }}; 
        _ -> 
            {noreply, State#state{}}
    end;

%% Callbacks handled as normal gen_server callbacks
handle_info(_Msg, State) ->
    {noreply, State}.
 
%% @private
handle_call(_Msg, _From, State) ->
    {reply, {error, nyi}, State}.
 
%% @private
code_change(_, _, State) ->  %%this is just a stub
    {stop, ignore, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Investigation structure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% edge() = no_edge | #invedge{}
% node() = { edge(), #invnode{}, invgraph() }
% invgraph() = [ node() ]

run(ListBox, Txt) ->
    FileNList =
    case ?HandleRes( ?ExecLCall( get_function, [Txt]), [] ) of
        [] -> [];
        X  -> {File, List} = ?ExecLCall( get_clauses_as_container, [X] ),
              lists:map(fun(I) -> {File, I} end, List)
    end, 
    result_list(ListBox, FileNList).

run_on_nodes(ListBox, Nodes) ->
    List =
    [ begin
       FPath = ?HandleRes(?ExecLCall( get_file_path, [Node] ), error),
       Cont  = ?HandleRes(?ExecLCall( get_container, [Node] ), error),
       if ((FPath == error) or (Cont == error)) -> error;
          true -> {FPath, Cont}
       end
      end
       || Node <- Nodes, is_tuple(Node) ],
    List2 = lists:filter(fun(A) -> A=/=error end, List),
    result_list(ListBox, List2).

result_list(ListBox, List) ->
    List == [] andalso ?wxMain:infowindow_main(?noResTxt, "No result"),
    FunToRun =
    fun({File, {Node, StartPos, Pos={_, {{L,_C},_}}, Range}}) ->
        MFA = txt(Node),
        Txt = MFA ++ " (" ++ 
              filename:basename(File) ++ ": " ++ io_lib:write(L) ++ ")",
        wxListBox:insertItems(ListBox, [Txt], 0),
        wxListBox:setClientData(ListBox, 0, 
                                {Node,StartPos, Pos, File, Range, MFA});
        (_) -> may_indicate_some_internal_error
    end,
    wx:foreach(fun(X) ->
                wxListBox:delete(ListBox,X)
               end, lists:reverse(lists:seq(0,wxListBox:getCount(ListBox)))),
    wx:foreach(FunToRun, List).

add_inv_node(InvArea, FPath, ParentObj, ClOrFormNd, Start, Line,
                                        {Range, _}, MFA, QueryStr) ->
    Parent = inv_state_obj_to_pid(ParentObj),
    Text = get_clause_text(ClOrFormNd),
    Offs = Start - 1,
    Obj = ?wxIB:new(InvArea,?invW,?invH,Text,FPath,Offs,
                    Line,self(),Range,MFA,make_tooltip(QueryStr)),
    {MW, MH} = { round(?invW/2), round(?invH/2)},
    {PX, PY} = parent_pos(Parent),
    ?wxIB:moveTo(Obj, {PX + MW, PY + MH}),
    Obj.


parent_pos(no_obj) -> {0, 0};
parent_pos(Parent) ->
    ?wxIB:get_pos(Parent).

get_clause_text(ClauseOrFormNode) -> ?ExecLCall( node_text, [ClauseOrFormNode] ).
txt(Node) -> ?ExecLCall( node_to_text, [Node] ).

%% Make an "inv state object" from the pid, the reference (and label) of an
%% inv/memobox (this will be a key in the tree which we use to have a structure
%% the element of which are drawable relatively fast as a tree)
make_inv_state_obj({RqPid, RqObj}) -> {RqPid, RqObj, no_label}.
make_inv_state_obj(RqPid, RqObj) -> {RqPid, RqObj, no_label}.
make_labeled_obj(_StObj={Pid, Obj, _}, Label) -> {Pid, Obj, Label}.
make_labeled_obj(Pid, Obj, Label) -> {Pid, Obj, Label}.
inv_state_obj_to_pid({Pid, _, _}) -> Pid;
inv_state_obj_to_pid(_) -> no_obj.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Drawing connections
tree_draw(DC, Tree) ->
    wx:batch(
        fun() ->
            Pen0 = wxPen:new({0,0,0}),
            wxPen:setWidth(Pen0, 1),
            wxDC:setPen(DC, Pen0),
            wxDC:setBrush(DC, ?wxTRANSPARENT_BRUSH),
            wxDC:clear(DC),
            tree_draw_(DC, Tree)
        end).

tree_draw_(_DC, []) -> ok;
tree_draw_(DC, [{_Id,_,_,[]} | Tail]) -> tree_draw_(DC, Tail);
tree_draw_(DC, [{{_, Id, _},_,_,Children} | Tail]) ->
    P1 = get_connection_point(Id),
    [ begin 
        P2 = get_connection_point(C),
        {O, {W, H}} = info_of_box(C),
        {MX,MY} = midpoint(P1, P2),
        ArrInfo = ?ExecLCall(arrow_info,[P2, P1, O, W, H, 8]),
        P3 = {MX,MY+10},
        wxDC:drawText(DC, L, P3),
        DiscPs = lists:map(fun({X,Y}) -> {round(X), round(Y)} end, 
                                ArrInfo ++ [hd(ArrInfo)]),
        wxDC:drawLines(DC, DiscPs),
        wxDC:drawLine(DC, P1, P2) 
      end           || {_, C, L} <- Children ],
    tree_draw_(DC, Tail).

get_connection_point(Box) ->
    {X0, Y0} = wxPanel:getPosition(Box),
    {W, H}   = wxPanel:getSize(Box),
    {round(X0+(W/2)),round(Y0+(H/2))}. 

midpoint({X1, Y1}, {X2, Y2}) ->
    {round((X1+X2)/2), round((Y1+Y2)/2)}.

info_of_box(Box) ->
    {wxPanel:getPosition(Box), wxPanel:getSize(Box)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

forced_save(Name, User, Hash, Inv) ->
    ?ExecLCall( save_investigation, [Name, User, Hash, Inv]),
    Name.

save_investigation(Name, User, Hash, Inv, CurrSvSt) ->
    Handler =
    fun({save_error, overwrite}, Name0) -> 
                {forced_save(Name0, User, Hash, Inv), saved};
       ({save_error, name_already_in_use}, Name0) -> 
                ?wxMain:infowindow_main("You can't overwrite the investigation"
                                   " of another user!", "Name already in use."), 
                {Name0, CurrSvSt};
       (_,Name0) ->
                {Name0, saved}
    end,
    ?HandleRes( ?ExecLCall( save_investigation_safe, [Name, User, Hash, Inv]),
                                                    Handler, Name, undefined ).    

get_unique_name_and_save(Parent, User, Hash, Inv, CurrInvName, CurrSvSt) ->
    Handler = 
    fun({save_error, overwrite}, Name) ->
            case ask_user(Parent,?overTxt,true) of
                yes -> {forced_save(Name, User, Hash, Inv), saved};
                _   -> {undefined, CurrSvSt} %% cancelled
            end;
        ({save_error, name_already_in_use}, _) ->
            ?wxMain:infowindow_main("The choosen name is already in use."
                                " Try another one!", "Name already in use."),
            get_unique_name_and_save(Parent, User, Hash, 
                                     Inv, CurrInvName, CurrSvSt);
        (_,Name) -> {Name, saved}
              end,
    Dialog =
    wxTextEntryDialog:new(Parent, 
                          "Please give your investigation a unique name.", []),
    case wxDialog:showModal(Dialog) of
        ?wxID_OK -> Name = wxTextEntryDialog:getValue(Dialog),
                    ?HandleRes( ?ExecLCall( save_investigation_safe, 
                      [Name, User, Hash, Inv]),
                      Handler, Name, undefined );
        _        -> {CurrInvName, CurrSvSt} %% cancelled
    end.

%% Fill the investigation list with available investigations
fill_inv_list(InvList,User,Type) ->
    wxListBox:clear(InvList),
    Invs = case Type of
            ?typeAll -> ?HandleRes( ?ExecLCall( list_investigations, [] ), [] );
            ?typeMy  -> ?HandleRes( ?ExecLCall( list_investigations,[User]), []);
            _        -> ?HandleRes( ?ExecLCall( list_investigations,[User]), [])
           end,
    FunToRun = fun({Name, Users}) ->
                     wxListBox:append(InvList, Name, {Name, Users})
               end,
    lists:map(FunToRun, Invs).

%% Loads a saved investigation using two transformations on the saved tree to
%% get a displayable tree. (and some information about it like its name)
%% Warning: this function uses process dictionary and stores a key-value pair
%% named dict which is a dictionary itself. The first part of the transformation
%% fills this dictionary.

load_inv_tree({Name, _Users},InvArea) -> load_inv_tree(Name,InvArea);
load_inv_tree(Name,InvArea) ->
    InvTree = ?HandleRes( ?ExecLCall( load_investigation, [Name] ), [] ),
    Hash = InvTree#inv.hash,
    CurrHash = db_hash(),
    BubMode = CurrHash == Hash,
    not BubMode andalso ?wxMain:infowindow_main(?hashChTxt,"Older investigation"),
    Tree0 = InvTree#inv.invdata,
    Dict = dict:new(),
    put(dict,Dict),
    Tree1= ?ExecLCall( tree_transformation, 
                    [Tree0, ?IdFun,display_inv_data(InvArea, self(), BubMode)]),
    Dict2 = get(dict),
    Tree2= ?ExecLCall( tree_transformation,
                       [Tree1, refresh_id(Dict2), id_fun2()]),
    put(dict,undefined),
    {Tree2, Name, Hash}.

%% Share investigation with another user (shows input box)
share_investigation(_Parent, _InvList, -1) -> no_item_selected;
share_investigation(Parent, InvList, N) -> 
    {Name, _} = wxListBox:getClientData(InvList,N),
    Dialog =
    wxTextEntryDialog:new(Parent, "Please enter the name of the user "
                          "you would like to share this investigation with.",[]),
    case wxDialog:showModal(Dialog) of
        ?wxID_OK -> User = wxTextEntryDialog:getValue(Dialog),
                    ?HandleRes( ?ExecLCall( share_investigation, [Name, User]));
        _        -> cancelled
    end.

%% Delete an investigation and return its name (e.g. so we can check if the
%% current one was deleted or not)
delete_investigation(_InvList, -1, _User) -> undefined;
delete_investigation(InvList, N, User) -> 
    {Name, _} = wxListBox:getClientData(InvList,N),
    case ?HandleRes( ?ExecLCall( delete_investigation, [Name, User]), error) of
            error -> undefined;
            _     -> Name
    end.

%% Display information about the current investigation like its name
display_inv_info({NameLab, _IfSaved}, undefined, _SaveStatus, Sizer) ->
    wxStaticText:setLabel(NameLab, "Unsaved investigation"),
    wxSizer:layout(Sizer);
    %% proplist to be able to show more info later if needed
display_inv_info({NameLab, IfSaved}, Name, SaveStatus, Sizer) -> 
    wxStaticText:setLabel(NameLab, Name),
    if SaveStatus == unsaved -> wxStaticText:setLabel(IfSaved, "*");
       true -> wxStaticText:setLabel(IfSaved, "")
    end,
    wxSizer:layout(Sizer).

set_current_qstr(QstrWt, QStr, ButtSz) ->
    wxStaticText:setLabel(QstrWt, QStr),
    wxSizer:layout(ButtSz).

%% Ask userabout something: yes or no
ask_user(_,_,false) -> yes;
ask_user(Parent,Txt,true) ->
    YesNoDial = wxMessageDialog:new(Parent, Txt,[{style, ?wxYES_NO}]),
    case wxDialog:showModal(YesNoDial) of
        ?wxID_YES -> yes;
        _         -> no 
    end.

%% Db hash helper
db_hash() ->
    ?HandleRes( ?ExecLCall( get_database_hash, [] ) ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Tree and its transformation

%% Here we use this tree in a special way: we have tuples as key with important
%% objects like the invbox, its pid, etc and define a function fst() which says
%% T1 and T2 are equal when they contain the same pid

fst() -> fun({Pid1, _, _}, {Pid2, _, _}) -> Pid1 == Pid2;
            (_,_) -> false end.

id_tr() -> fun({Pid,_Obj,_Label}) -> Pid;
              (no_parent) -> no_parent end.
data_tr(InvArea) -> 
                fun({Pid,Obj,Label}, memo) ->
                  #invnode{ name="Memo", 
                    show=?wxIB:if_hidden(Pid), 
                   node=memo, label=Label,
                   memo=true, text=?wxIB:get_text(Pid), 
                   pos=wxScrolledWindow:calcUnscrolledPosition(InvArea,
                                                wxPanel:getPosition(Obj)) };
                ({Pid,Obj,Label}, Node) -> FD = ?wxIB:get_file_data(Pid),
                  #invnode{ name=?wxIB:get_name(Pid),
                   show=(not ?wxIB:if_hidden(Pid)),
                   node=Node, label=Label,
                   memo=false, text=?wxIB:get_text(Pid), 
                   pos=wxScrolledWindow:calcUnscrolledPosition(InvArea,
                                                wxPanel:getPosition(Obj)), 
                   file_data=FD }   
             end.
disable_node() -> fun({_Pid,_Obj,_Label}, memo) -> memo;
                     ({_Pid,Obj,_Label}, Node) -> ?wxIB:disable(Obj),
                                                  Node
                  end.

%% Transforming back a saved tree to a displayable tree in two steps
%% Warning: these functions use the process dictionary, and they suppose that
%% there is a dictionary key dict, which is a key-value dict.
%% Using this method, it is possible to convert the tree in 2N steps

%% (1.) conversion step
%% Data conversion from invnode records to referl node or memo
%% This function also generates the invboxes on the display as a side effect
display_inv_data(InvArea, Pid, BubMode) -> 
    fun(OldIdPid, InvNode) ->
        if InvNode#invnode.memo ->
            Box = ?wxIB:new(InvArea, ?memW, ?memH, Pid,
                            InvNode#invnode.text,  "Memo"),
            SP = wxScrolledWindow:calcScrolledPosition(InvArea,
                                                       InvNode#invnode.pos),
            ?wxIB:moveTo(Box, SP),
            BoxPid = wx_object:get_pid(Box),
            Obj = make_labeled_obj(BoxPid,Box,InvNode#invnode.label),
            put(dict, dict:store(OldIdPid, Obj, get(dict))),
            InvNode#invnode.node;
        true ->
            {Filepath,Offset,Line,Range,MFA} = InvNode#invnode.file_data,
            Box = ?wxIB:new(InvArea,?invW,?invH, InvNode#invnode.text, Filepath,
                    Offset, Line, Pid, BubMode, Range, MFA, 
                        make_tooltip(InvNode#invnode.label)),
            SP = wxScrolledWindow:calcScrolledPosition(InvArea,
                                                       InvNode#invnode.pos),
            ?wxIB:moveTo(Box, SP),
            ?wxIB:set_name(Box, InvNode#invnode.name),
            ?wxIB:hide(Box, not InvNode#invnode.show),
            BoxPid = wx_object:get_pid(Box),
            Obj = make_labeled_obj(BoxPid,Box,InvNode#invnode.label),
            put(dict, dict:store(OldIdPid, Obj, get(dict))),
            InvNode#invnode.node
        end
    end.

%% (1.) conversion step
%% ?IdFun fun(I) -> I end.

%% (2.) conversion step
%% Id conversion from "pid-like ids" to real "inv state objects" which are the
%% keys of the tree used while drawing ("pid-like ids" are the keys of the tree
%% stored on the disc)
refresh_id(Dict) -> 
    fun(no_parent) -> no_parent;
       (OldIdPid) -> element(2,dict:find(OldIdPid, Dict)) end.

%% (2.) conversion step
%% Do nothing Data Transformation
id_fun2() -> fun(_I1,I2) -> I2 end.

%% Making a tooltip from a query string which was used to reach the InvBox
%% and the clause of it (by querying)
make_tooltip(root) -> "ROOT";
make_tooltip(undefined) -> "UNDEFINED";
make_tooltip(Str) -> "-> " ++ Str.

%% This function should be called in order to restrict/reenable buttons
%% when database hash changed, and the investigation cannot be continued
restrict_btns(Bool, Btns) ->
    ?wxMain:enable_btns(Btns, Bool).
