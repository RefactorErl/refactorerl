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
 
%%% @doc This module provides interface to dependency graphs. They can be drawn
%%% as a wx graphical object, SVG or "smart graph". The wx graph is clickable
%%% (open nodes-funs-mods in a code browser), searchable (label search), fil-
%%% terable (node ranking based on node degrees and node type, importance;
%%% gnode), zoomable. <br />
%%% It uses referl_wx_logic's graphical/drawing functions effectively, and 
%%% transforms a graphviz plain-ext representation to a list of nodes/edges and
%%% then to an easily drawable graph. (See referl_wx_logic for details)
%%% @author Gabor Hosszu

-module(referl_wx_depgraph).
-author("Gábor Hosszú").
-behaviour(wx_object).

-export([start/1, init/1, terminate/2,  code_change/3,
handle_info/2, handle_call/3, handle_cast/2, handle_event/2, handle_sync_event/3]).
 
-include_lib("wx/include/wx.hrl").
-include_lib("referl_core/include/core_export.hrl").
-include("wx.hrl").
-compile(export_all).
-record(state,
{
 parent,
 access_to_db,
 graphp,
 graph,
 browsers,
 evthandler,
 optionsz,   %% Sizer for options (on the left side of the panel)
 optmainsz,  %% Sizer which contains the optionsz
 widgets,    %% Dinamically changed option widgets
 chs,        %% Level changer (radio box) and type changer
 zoom,       %% Zoom contains the current scale of the current graph
 minrank,    %% Minimal rank for nodes to show (current value)
 ranking,    %% Ranking, list contain ranks and intervals
 searchws,   %% Searchwidgets, a tuple containing a search txtctrl and a runbtn, 
             %% the next btn and latest results (list), and a number: which element
             %% of the list is the current one shown?
 sldrs,
 runbs,      %% Tuple of buttons: run draw smart, draw svg
 drawws,     %% Widgets for drawing, pens, fonts, brushes
 disbtns     %% Non-dinamically generated buttons, that are enabled and disabled
             %% according to current level
    }).
 
-define(nodeButton, 111111).
-define(modgroupLvl, 0).
-define(modLvl, 1).
-define(funLvl, 2).

-define(allType, 0).
-define(cycleType, 1).

-define(runBtn, 111112).
-define(runNDSBtn, 111113).
-define(nextNDSBtn, 111114).
-define(drawJSBtn, 111115).
-define(drawSVGBtn,  111116).
-define(printGBtn,   111117).

-define(typeCh, 111210).
-define(levelCh, 111211).

-define(rankSldr, 111301).
-define(zoomSldr, 111302).

-define(firstZoomVal, 1).
-define(firstRankVal, 5).

-define(empRes, "The result graph is empty"). %% Message when the graph is empty

%% os_key is put into the process dictionary.
%% It shows which OS we use, because there are some OS-specific parts near
%% drawing. See do_init, draw_node_button
-define(OS, os_key).
-define(Win,   win_os).
-define(Other, other_os).

%% TODO:
%% - duplicated code near run

%% @private
start(Props) ->
    wx_object:start_link(?MODULE, Props, [{}]).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
init(Props) ->
        wx:batch(fun() -> do_init(Props) end).
 
do_init(Props) ->
    Panel = proplists:get_value(parent, Props),
    EvtHandler = proplists:get_value(main_pid, Props),
    Sz = wxBoxSizer:new(?wxHORIZONTAL),
    %% MAC RELATED
    %% StaticBox "definition" must be before controls...
    OptionsSz = wxBoxSizer:new(?wxVERTICAL),
    OptionsMainSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, []),
    GraphAreaSz = wxBoxSizer:new(?wxVERTICAL),
    GraphOptSz = wxBoxSizer:new(?wxHORIZONTAL),
    GraphP = wxScrolledWindow:new(Panel, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),
    wxWindow:setToolTip(GraphP, "Left click on nodes to show node information, "
                                " and right click, to start dependency analysis"
                                " from that node (filters the graph)"),
    %% Mac related
    Win = string:str(string:to_lower(wx_misc:getOsDescription()), "windows") > 0,
    Mac = string:str(string:to_lower(wx_misc:getOsDescription()), "mac") > 0,
    if (Win or Mac) ->  
               put(?OS, ?Win),
               wxWindow:setBackgroundStyle(GraphP, ?wxBG_STYLE_SYSTEM);
       true -> put(?OS, ?Other),
               wxWindow:setBackgroundStyle(GraphP, ?wxBG_STYLE_CUSTOM)
    end,
    Level = wxRadioBox:new(Panel, ?levelCh, "Level", ?wxDefaultPosition, 
                         {200,100}, ["Module group", "Module", "Function"]),
    S = {size, {150, 22}},
    Run   = wxButton:new(Panel, ?runBtn, [{label,"Run and draw"},S]),
    DrawJava = wxButton:new(Panel, ?drawJSBtn, [{label,"Draw smart graph"},S]),
    DrawSVG = wxButton:new(Panel, ?drawSVGBtn, [{label,"Draw SVG graph"},S]),
    PrintGraph = wxButton:new(Panel, ?printGBtn, 
                                    [{label,"Print dependencies"},S]),
    NodeSearch = wxStyledTextCtrl:new(Panel, [{size, {100, 18}}]),
    ?wxMain:set_stc_style(NodeSearch),
    RunNDS   = wxButton:new(Panel, ?runNDSBtn, [{label,"Search for:"}]),
    NextBtn   = wxButton:new(Panel, ?nextNDSBtn, [{label,"Next..."}]),
    wxWindow:disable(NextBtn),
    ZoomSldr = wxSlider:new(Panel, ?zoomSldr, ?firstZoomVal, 1, 5,
            [{style, ?wxSL_HORIZONTAL},
             {size, {70,40}}]),
    RankSldr = wxSlider:new(Panel, ?rankSldr, ?firstRankVal, 1, 5,
            [{style, ?wxSL_HORIZONTAL},{size, {70,40}}]),
    Sldrs = {ZoomSldr, RankSldr},
    DisBs = [DrawJava,PrintGraph],
    CnA = [{flag, ?wxCENTRE bor ?wxALL}],
    SrcWs = {NodeSearch,RunNDS,NextBtn,[],none},
    wxSizer:add(OptionsMainSz, Level, []),
    TCh = add_type_changer(Panel, OptionsMainSz),
    wxSizer:add(OptionsMainSz, OptionsSz, []),
    wxSizer:addSpacer(OptionsMainSz,10),
    wxSizer:add(OptionsMainSz, Run),
    wxSizer:add(OptionsMainSz, DrawJava),
    wxSizer:add(OptionsMainSz, DrawSVG),
    wxSizer:add(OptionsMainSz, PrintGraph),
    wxSizer:addSpacer(GraphOptSz, 40),
    wxSizer:add(GraphOptSz, RunNDS, CnA),
    wxSizer:add(GraphOptSz, NodeSearch, CnA),
    wxSizer:add(GraphOptSz, NextBtn, CnA),
    wxSizer:addSpacer(GraphOptSz, 40),
    wxSizer:add(GraphOptSz, wxStaticText:new(Panel, ?wxID_ANY, "Zoom:"), CnA),
    wxSizer:add(GraphOptSz, ZoomSldr, CnA),
    wxSizer:addSpacer(GraphOptSz, 20),
    wxSizer:add(GraphOptSz, wxStaticText:new(Panel, 
                            ?wxID_ANY, "Node ranking (minimum value):"), CnA),
    wxSizer:add(GraphOptSz, RankSldr, CnA),
    wxSizer:add(OptionsMainSz, ?wxMain:make_placeholder(Panel,200,1)),
    wxSizer:add(GraphAreaSz,GraphOptSz, [{flag, ?wxEXPAND}]),
    wxSizer:add(GraphAreaSz,GraphP, 
        [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sz, OptionsMainSz, [{flag, ?wxEXPAND}, {border, 10}]),
    wxSizer:add(Sz, GraphAreaSz, [{flag, ?wxEXPAND bor ?wxALL}, {proportion, 1}]),

    wxPanel:setSizer(Panel, Sz),
    wxSizer:layout(Sz),
    %% CONNECT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% wxPanel:connect(GraphP, mousewheel),
    wxPanel:connect(ZoomSldr, scroll_thumbrelease),%%scroll_changed),
    wxPanel:connect(RankSldr, scroll_thumbrelease),%%scroll_changed),
    wxPanel:connect(GraphP, paint, [callback]),
    wxPanel:connect(Panel, command_radiobox_selected),
    wxPanel:connect(Panel, command_button_clicked), 
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    %% Fonts, brushes, and pens can be carried in state, no need to recreate 
    %% them in every step
    %% Tools for edges are called EdgeTools in the code
    %% Tools for buttons are called DrawTools
    Font = wxFont:new(10, ?wxDEFAULT, ?wxNORMAL, 1),
    BrushesForShapes = [BB=wxBrush:new({0,0,0}),wxBrush:new({100,100,100}),
                        wxBrush:new({200,200,200})],
    TB = wxBrush:new({0,0,0}, [{style, ?wxTRANSPARENT}]),
    EdgePen = wxPen:new({140,130,134}),
    wxPen:setWidth(EdgePen, 2),
    Pen = wxPen:new(?wxWHITE),
    FocusPen = wxPen:new(?wxRED, [{width, 3}]),
    disable_btns(DisBs,?modgroupLvl),
    DrawWs = {Font, BrushesForShapes, {BB, EdgePen}, {TB, {Pen, FocusPen}}},
    %%%%%%%

    %% INIT
    put(fatal, ignore), %% if defined, no modals will show up on errors, 
    %% and db_busy exception will be thrown
    Widgets = add_option_widgets(Panel,OptionsSz,?modgroupLvl), %% needs DB
    wxSizer:layout(Sz),
    erase(fatal),

    {Panel, #state{parent=Panel, graphp=GraphP, graph=no_graph,  zoom=undefined,
                   evthandler=EvtHandler, browsers=[], widgets=Widgets,
                   optionsz=OptionsSz, optmainsz=OptionsMainSz, chs={Level, TCh},
                   minrank=20, searchws=SrcWs, sldrs=Sldrs, disbtns=DisBs,
                   runbs={Run,DrawJava,DrawSVG,PrintGraph}, drawws=DrawWs }}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
%% Painting the graph
handle_sync_event(#wx{event = #wxPaint{}}, _wxObj, #state{graphp=GraphP, 
           graph=Graph, zoom=Zoom, minrank=MR,
           drawws={Font, Brushes, EdgeTools, _DrawTools} }) ->
    {DC,Canvas} = create_dc(GraphP),
    case Graph of
        {_,_, _, Nodes, Edges} ->
            wx:batch( 
                fun() ->
                    {FW, FH} = wxWindow:getSize(GraphP),
                    wxDC:setClippingRegion(DC,{0,0,FW,FH}),
                    create_edges(Zoom,Canvas,GraphP, Edges,MR,EdgeTools),
                    create_graph_nodes(Zoom,Canvas,GraphP,Nodes,MR,Font,Brushes)
                end);
        no_graph        ->
                ok
    end,
    %% There should be a check if it is R14B or lower
    ?wxGraphicsContext_destroy(Canvas), 
    wxPaintDC:destroy(DC),
    ok.

%% @private
%% Slider events (zoom, ranking)
handle_event(#wx{event=#wxScroll{type =scroll_thumbrelease, commandInt=Zoom2},
             id=?zoomSldr },
        State=#state{ zoom=Zoom, graph=Graph, graphp=GraphP, minrank=MR,
                      drawws={_, _, _, DrawTls}, disbtns=_DisBtns })
             when Graph=/=no_graph ->
        if Zoom =/= Zoom2 -> total_graph_refresh(Zoom2,Graph,GraphP,MR,DrawTls);
           true -> ok
        end,
        {noreply, State#state{ zoom=Zoom2 }};

handle_event(#wx{event=#wxScroll{type =scroll_thumbrelease, commandInt=Ind},
             id=?rankSldr },
        State=#state{ zoom=Zoom, graph=Graph, graphp=GraphP, minrank=MR,
                      drawws={_, _, _, DrawTls} })
             when Graph=/=no_graph ->
        {_,_, Ranking, _, _} = Graph,
        MR2 = get_curr_min_rank(Ranking,Ind),
        if MR =/= MR2 -> total_graph_refresh(Zoom,Graph,GraphP,MR2,DrawTls);
           true -> ok
        end,
        {noreply, State#state{ minrank=MR2 }};

%% Dinamically adding/removing widgets according to current dep level
handle_event(#wx{event=#wxCommand{type = command_radiobox_selected, 
        commandInt=ChNum}, id=?levelCh },
        State=#state{ parent=Panel, optionsz=OptionsSz, widgets=W0 }) -> 
        Widgets = 
        wx:batch( fun() ->
                    delete_old_widgets(Panel,OptionsSz,W0),
                    W=add_option_widgets(Panel,OptionsSz,ChNum),
                    wxSizer:layout(State#state.optmainsz),
                    disable_btns(State#state.disbtns,ChNum),
                    W
                  end  ),
        ?wxMain:set_scrollrate(Panel),
        {noreply, State#state{ widgets=Widgets }};

%% Closing a browser
handle_event(#wx{event=#wxClose{ }, userData=Br },  State) ->
        BrNew = lists:delete(Br, State#state.browsers),
        wxFrame:destroy(Br),
        {noreply, State#state{ browsers=BrNew }};

%% Right mouse button click on a node (start dep anal from there)
handle_event(#wx{event = #wxMouse{type = right_down}, userData=Data },
            State=#state{ graphp=GraphP, chs={LvlCh,TpCh},  zoom=ActZoom,
            minrank=ActMinRank, sldrs={ZoomSldr,RankSldr}, runbs={Run,B2,B3,B4},
            drawws={_, _, _, DrawTls}, disbtns=DisBs})->
        AllBs = [Run,B2,B3,B4],
        MP = State#state.evthandler,
        case Data of
            {Node={_,_,_}, _} ->
                CurrLevel = wxRadioBox:getSelection(LvlCh),
                CurrType = wxRadioBox:getSelection(TpCh),
                {FW, FH} = wxWindow:getSize(GraphP),
                Opts = gnode_only_options(Node),
                Level = level(CurrLevel),
                Type  = type(CurrType),
                case run(Level, Type, GraphP, FW, FH, Opts, Run, AllBs, MP) of
                    {UserZoom, Graph, Ranking, MinR} ->
                        wxScrolledWindow:setScrollRate(GraphP, 5, 5),
                        set_proper_sldr_val(ZoomSldr,RankSldr,UserZoom,?firstRankVal),
                        disable_btns(DisBs, CurrLevel),
                        total_graph_refresh(UserZoom,Graph,GraphP,MinR,DrawTls),
                        {noreply,State#state{ graph=Graph, zoom=UserZoom, 
                        minrank=MinR, ranking=Ranking }};
                    no_graph ->
                        disable_btns(DisBs, CurrLevel),
                        total_graph_refresh(ActZoom,no_graph,GraphP,
                                            ActMinRank,DrawTls),
                        {noreply,State#state{ graph=no_graph }}
                end;
            _ ->
                {noreply,State#state{}}
        end;

%% Button click handling
handle_event(#wx{event = #wxCommand{type = command_button_clicked},  id=Id,
            obj=_Obj, userData=Data  },
            State=#state{ evthandler=EvtHandler, parent=Parent, browsers=Brs,
            graphp=GraphP, chs={LvlCh,TpCh}, widgets=Ws, graph=ActGraph,
            zoom=ActZoom, ranking=ActRanking, minrank=ActMinRank,
            sldrs={ZoomSldr,RankSldr}, runbs={RunB,B2,B3,B4}, searchws=Sws,
            drawws={_, _, _, DrawTls}, disbtns=DisBs })->
    AllBs = [RunB,B2,B3,B4],
    MP = State#state.evthandler,
    case Id of
        ?runBtn      -> 
                CurrLevel = wxRadioBox:getSelection(LvlCh),
                CurrType = wxRadioBox:getSelection(TpCh),
                {FW, FH} = wxWindow:getSize(GraphP),
                {Opts, Level, Type} = gather_params(Ws,CurrLevel,CurrType),
                case run(Level, Type, GraphP, FW, FH, Opts, RunB, AllBs, MP) of
                    {UserZoom, Graph, Ranking, MinR} -> %% NO NEED OF UserZoom and MinR
                        wxScrolledWindow:setScrollRate(GraphP, 5, 5),
                        set_proper_sldr_val(ZoomSldr,RankSldr,UserZoom,?firstRankVal),
                        disable_btns(DisBs, CurrLevel),
                        total_graph_refresh(UserZoom,Graph,GraphP,MinR,DrawTls),
                        {noreply,State#state{ graph=Graph, zoom=UserZoom, 
                        minrank=MinR, ranking=Ranking }};
                    no_graph ->
                        disable_btns(DisBs, CurrLevel),
                        total_graph_refresh(ActZoom,no_graph,
                                            GraphP,ActMinRank,DrawTls),
                        {noreply,State#state{ graph=no_graph }}
                end;
        ?nodeButton  -> 
            case graph_node_click(Data) of
                ok ->       
                    {noreply,State#state{}};
                {no_file, _} -> 
                    {noreply,State#state{}};   
                {File, Pos} ->
                    Browser = ?wxCB:start_browser_window(Parent, EvtHandler),
                    wxFrame:connect(Browser, close_window,[{userData,Browser}]),
                    ?HandleRes(?wxCB:load_code(Browser,[{file,File},{pos,Pos}])),
                    {noreply,State#state{ browsers=[Browser|Brs] }}
            end;                        
        ?runNDSBtn when ActGraph=/=no_graph ->
            {NodeSearch,RunNDS,NextBtn,_,_} = Sws,
            case wxStyledTextCtrl:getText(NodeSearch) of
                ""  -> wxWindow:disable(NextBtn),
                       {noreply,State#state{}};
                TXT -> {_,_,_,Nodes,_} = ActGraph,
                       Items = lists:filter(fun(T) ->
                                                Label = element(4,T),
                                                string:str(Label, TXT) > 0 end, Nodes),
                       case Items of
                        [] -> {noreply,State#state{}};
                        [{_,Rank,_,_,XX,YY,_,_,_,_} | _]=L ->
                            Sws2={NodeSearch,RunNDS,NextBtn,L,1},
                            wxWindow:enable(NextBtn),
                            {NewMinRnk, NewZoom, _RankIndex} = 
                            search_res(GraphP, ActZoom, XX, YY, ActMinRank,
                                       Rank, ActRanking, ActGraph, ZoomSldr,
                                       RankSldr, TXT, DrawTls),
                            {noreply, State#state{ zoom=NewZoom, minrank=NewMinRnk,
                                                                    searchws=Sws2 }}
                       end
            end;
        ?nextNDSBtn -> 
            {NodeSearch,RunNDS,NextBtn,L,Curr} = Sws,
            NowCurr = (Curr) rem length(L) + 1,
            Sws2 = {NodeSearch,RunNDS,NextBtn,L,NowCurr},
            {_,Rank,_,TXT,XX,YY,_,_,_,_} = lists:nth(NowCurr,L),
            {NewMinRnk, NewZoom, _RankIndex} = 
            search_res(GraphP,ActZoom,XX,YY,ActMinRank,Rank,ActRanking,ActGraph,
                                                 ZoomSldr,RankSldr,TXT,DrawTls),
            {noreply, State#state{ zoom=NewZoom, minrank=NewMinRnk, searchws=Sws2 }};
        ?drawJSBtn ->
            CurrLevel = wxRadioBox:getSelection(LvlCh),
            CurrType = wxRadioBox:getSelection(TpCh),
            draw_js_graph(Ws,CurrLevel,CurrType,AllBs,DisBs,MP),
            {noreply,State#state{}};
        ?drawSVGBtn ->
            CurrLevel = wxRadioBox:getSelection(LvlCh),
            CurrType = wxRadioBox:getSelection(TpCh),
            draw_svg_graph(Ws,CurrLevel,CurrType,AllBs,DisBs,MP),
            {noreply,State#state{}};
        ?printGBtn ->
            CurrLevel = wxRadioBox:getSelection(LvlCh),
            CurrType = wxRadioBox:getSelection(TpCh),
            print_graph(Ws,CurrLevel,CurrType,AllBs,DisBs,MP),
            {noreply,State#state{}};
        _   -> {noreply,State#state{}}
    end;

%% Autocomplete for starting node input
handle_event(#wx{event = #wxStyledText{type = stc_charadded}, obj=STC },
      State = #state{   chs={LvlCh, _} }) ->
    Txt = wxStyledTextCtrl:getText(STC),
    Type =
    case wxRadioBox:getSelection(LvlCh) of 
        ?funLvl -> func;
        ?modLvl -> mod;
        _       -> other
    end,
    Res = ?HandleRes(?ExecLCall( autocomplete_funmod, [Txt,Type]), [] ),
    CompleteList = string:join(Res, ?SEP),
    L = length(lists:takewhile(fun(Ch) -> Ch =/= $. end, lists:reverse(Txt))),
    wxStyledTextCtrl:autoCompShow(STC, L, CompleteList),
    {noreply,State};

handle_event(_WX = #wx{}, State = #state{}) ->
    {noreply, State}.
    
%% @private
handle_cast(_Msg, State) ->
    {noreply,State}.

%% @private
%% DB access
handle_info({event, db_access, Bool},State=#state{ runbs={B1,B2,B3,B4},
                                     disbtns=DisBs , chs={Level, _}  }) ->
    ?wxMain:enable_btns([B1,B2,B3,B4], Bool),
    Bool andalso disable_btns(DisBs, wxRadioBox:getSelection(Level)),
    send_browser_event(State#state.browsers,db_access,Bool),
    {noreply, State#state{ access_to_db=Bool }};

%% Callbacks handled as normal gen_server callbacks
handle_info({event, Evt, Info}, State) when Info =/= none ->
    send_browser_event(State#state.browsers,Evt,Info),
    {noreply, State};

handle_info({event, _Evt, _FilePath}, State) ->
    {LvlCh, _} = State#state.chs,
    wxRadioBox:getSelection(LvlCh) == ?modgroupLvl andalso 
        begin
            {_, _, [ListBox | _ ]} = State#state.widgets,
            add_current_dirs(ListBox)
        end,
    {noreply, State};

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

handle_add_correct_regexp(Input,ListW) ->
    Txt = wxStyledTextCtrl:getText(Input),
    case re:compile(Txt) of
        {ok, _} ->  wxListBox:insertItems(ListW, [Txt], 0);
        _       ->  ?wxMain:infowindow_main("Bad regular expression.", 
                                            "Bad regexp")
    end.

%% Handler for general list widgets
%% Can be used as a callback in wxFrame:connect
handle_add(Input,ListW) ->
    wxListBox:insertItems(ListW, [wxStyledTextCtrl:getText(Input)], 0).

handle_del(ListW) ->
    case wxListBox:getSelection(ListW) of 
        ?wxNOT_FOUND -> ok;
        N            -> wxListBox:delete(ListW, N)
    end.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Drawing graphical elements
%% Cp - center point, X,Y - position, R - radius, S - style information (shape)
%% MinRank - the actual minimal rank, Canvas - wxGraphicsContext

create_graph_nodes(Scale, Canvas,Parent, Nodes, MinRank, Font, BrushesForShapes) ->
    wxGraphicsContext:setFont(Canvas, Font, {0,0,0}),
    BrushesForShapes2 = lists:map(fun(Br) -> 
                                    wxGraphicsContext:createBrush(Canvas, Br)
                                  end,
                                  BrushesForShapes),
    wxFont:setPointSize(Font, text_height_scale(Scale)),
    GFont = wxGraphicsContext:createFont(Canvas, Font),
    wxGraphicsContext:setFont(Canvas, GFont),
    [ create_node(Scale, Canvas, Parent, X, Y, Label, W, H, R, S, Rank, MinRank,
                  BrushesForShapes2) 
                        || {_, Rank, _, Label, X, Y, R, W, H, S} <- Nodes ],
    wxGraphicsObject:destroy(GFont).

create_node_buttons(_, _, no_graph, _, _) -> no_graph;
create_node_buttons(Scale, Parent, {_,_,_,Nodes, _Edges}, MinRank, DrwTools) ->
    [ create_node_button(Scale, Parent, X, Y, Label, NodeRep, W, H, R, Rank,
                         MinRank, DrwTools) 
                        || {_, Rank, NodeRep, Label, X, Y, R, W, H, _} <- Nodes ].

create_edges(Scale, Canvas,Parent, Edges, MinRank, EdgeTools) ->
    [ create_edge(Scale, Canvas, Parent, Ps, Lab, LX, LY, Style, Arr, Rank, 
                  MinRank, EdgeTools) 
                        || {_, Rank, Ps, Lab, LX, LY, Style,Arr} <- Edges ].

create_node_button(Sc, Parent, CX, CY, Label, NodeRep, _W, _H, R, Rank,MinRank,
                   DrwTools) ->
    if Rank >= MinRank ->
       draw_node_button(Parent, ?F(CX*Sc), ?F(CY*Sc), {CX,CY}, Label, 
                        NodeRep, ?F(R*Sc), ?F(R*Sc), ?F(R*Sc), DrwTools);
     true -> 
       ok
    end.
create_node(Sc, Canvas, Parent, CX, CY, Label, W, H, R, Style, Rank,
            MinRank, BrushesForShapes) ->
    if Rank >= MinRank ->
       draw_node(Canvas, Parent, ?F(Sc*CX), ?F(Sc*CY), Label, ?F(Sc*W), 
                ?F(Sc*H), ?F(Sc*R), Style, BrushesForShapes);
     true -> 
       ok
    end.

draw_node_button(Parent, CX, CY, Ref, Label, NodeRep, W, H, R, DrwTools) ->
    BButt = 
    case get(?OS) of
        ?Other -> draw_node_button_lin(Parent, CX, CY, W, H, R, DrwTools);
        _    -> draw_node_button_all(Parent, CX, CY, R)
    end,
    wxChoice:setToolTip(BButt, Label),
    wxButton:setName(BButt, Label++io_lib:write(Ref)),
    wxFrame:connect(BButt, right_down, [{userData, {NodeRep, Label}}]),
    wxFrame:connect(BButt, command_button_clicked, [{userData, NodeRep}]).    

draw_node_button_all(Parent, CX, CY, R) -> 
    O =  math:sqrt((R*R)/2),
    {X2,Y2} = {round(CX-O),round(CY-O)},
    Oo = round(2*O),
    wxButton:new(Parent, ?nodeButton, %% -4
            [{pos,calc_scrolled_pos(Parent, X2,Y2)},
             {style,?wxBORDER_NONE},{size,{Oo,Oo}}]).

draw_node_button_lin(Parent, CX, CY, _OrigW, _OrigH, R, DrwTools) ->
    W = H = ?C(?NodeVsButtonRatio * R),
    {Brush, Pens} = DrwTools,
    Cp = ?ExecLCall(center, [W, H]),
    {X,Y} = calc_xy_from_cp({CX, CY}, W, H), 
    Pic = draw_btn_pic(W,H,Cp,R,normal, Brush, Pens),
    FocusPic = draw_btn_pic(W,H,Cp,R,focuspic, Brush, Pens),
    BButt = wxBitmapButton:new(Parent, ?nodeButton, Pic, %% -4
            [{pos,calc_scrolled_pos(Parent, X-7,Y-7)},
             {style,?wxBORDER_NONE},{size,{W+15,H+15}}]),
    %% wxButton:setBackgroundStyle(BButt, ?wxBG_STYLE_SYSTEM),
    wxBitmapButton:setBitmapFocus(BButt, FocusPic),
    BButt.

draw_btn_pic(W,H,Cp,R, Type, Brush, {Pen, FocusPen}) ->
    Pic = wxBitmap:new(W+1,H+1),
    Mask = wxBitmap:new(W+1,H+1),
    wxPen:setWidth(Pen, 1),
    DC0 = wxMemoryDC:new(),
    wxMemoryDC:selectObject(DC0, Mask),
    wxDC:setBrush(DC0,Brush),
    wxDC:setBackground(DC0, ?wxBLACK_BRUSH),
    wxDC:clear(DC0),
    wxDC:setPen(DC0, Pen),
    wxDC:drawCircle(DC0, Cp, R),
    Type == focuspic andalso begin  wxPen:setWidth(Pen, 3),
                                    wxDC:setPen(DC0, Pen),
                                    wxDC:drawRectangle(DC0, {0,0}, {W,H}) end,
    wxMemoryDC:destroy(DC0),
    DC = wxMemoryDC:new(),
    wxBitmap:setMask(Pic, wxMask:new(Mask, ?wxBLACK)),
    wxMemoryDC:selectObject(DC, Pic),
    wxDC:setBackground(DC, ?wxBLACK_BRUSH),
    wxDC:clear(DC),
    Type == focuspic andalso 
                     begin  wxDC:setPen(DC, FocusPen),
                            wxDC:drawRectangle(DC, {0,0}, {W,H}) end,
    wxMemoryDC:destroy(DC),
    Pic.

%% In the following functions, moveToPoint always needs floats (cause of
%% Erl R14)

draw_node(Canvas, Parent, CX, CY, Lab, _W, _H, R, Style, BrushesForShapes) ->
    wxGraphicsContext:setBrush(Canvas, shape_to_brush(Style, BrushesForShapes)),
    Path = wxGraphicsContext:createPath(Canvas),
    {SCX, SCY} = calc_scrolled_pos(Parent, CX, CY),
    wxGraphicsPath:addCircle(Path, 1.0 * SCX, 1.0 * SCY, 1.0 * R),
    wxGraphicsContext:drawPath(Canvas, Path),
    wxGraphicsContext:drawText(Canvas, Lab, SCX-R, SCY+R).   
create_edge(Scale, Canvas, Parent, Ps, Lab, LX, LY, 
                                        Colour, Arr, Rank,MinRank,EdgeTools) ->
    if Rank >= MinRank ->
       draw_edge(Scale, Canvas, Parent, Ps, Lab, LX, LY, Colour, Arr, EdgeTools);
     true -> 
       ok
    end.   

draw_edge(Sc, Canvas, Parent, Ps, _Lab, _LX, _LY, 
                                    {Type,_Colour}, Arr, EdgeTools) ->
    {Brush0, Pen0} = EdgeTools,
    case Type of
        "dotted" -> wxPen:setStyle(Pen0, ?wxDOT);%% important for opaque
        _        -> wxPen:setStyle(Pen0, ?wxSOLID)
    end,
    wxGraphicsContext:setPen(Canvas, Pen0),
    Brush = wxGraphicsContext:createBrush(Canvas, Brush0),
    wxGraphicsContext:setBrush(Canvas, Brush),
    Path = wxGraphicsContext:createPath(Canvas),
    CalcFun = fun({X,Y}) -> 
                    {XF, YF} = calc_scrolled_pos(Parent,?F(Sc*X),?F(Sc*Y)),
                    { 1.0 * XF, 1.0 * YF} 
              end,
    DrawablePaths = ?ExecLCall(pairs2,[Ps, CalcFun]),
    Fun = fun({{X1, Y1}, {X, Y}}) ->
                wxGraphicsPath:moveToPoint(Path, 1.0 * X1, 1.0 * Y1),
                wxGraphicsPath:addLineToPoint(Path,  1.0 * X, 1.0 * Y)
          end,
    wx:map(Fun, DrawablePaths),
    wxGraphicsContext:strokePath(Canvas, Path),
    create_arrow(Canvas, lists:map(CalcFun,Arr)).

%% In Erl R14 P1, P2 and P3 MUST contain floats
create_arrow(Canvas, [P1, P2, P3]) ->
    Path = wxGraphicsContext:createPath(Canvas),
    wxGraphicsPath:moveToPoint(Path, P1),
    wxGraphicsPath:addLineToPoint(Path, P2),
    wxGraphicsPath:moveToPoint(Path, P2),
    wxGraphicsPath:addLineToPoint(Path, P3),
    wxGraphicsPath:moveToPoint(Path, P3),
    wxGraphicsPath:addLineToPoint(Path, P1),
    wxGraphicsContext:strokePath(Canvas, Path).

create_dc(Parent) ->
    DC = wxPaintDC:new(Parent),
    Canvas = wxGraphicsContext:create(DC),
    {DC, Canvas}.

%% Set proper text height for the actual scaling (1...5)
text_height_scale(Sc) ->
    12+2*(Sc-1).

%% Totally refresh the graph readd buttons and redraw edges.
%% DrawTools are the pens, brushes, etc needed for drawing nodes
total_graph_refresh(Zoom,Graph,GraphP,MinRank,DrawTls) ->
    wx:batch( 
      fun() ->
        wxWindow:destroyChildren(GraphP),
        create_node_buttons(Zoom, GraphP, Graph, MinRank, DrawTls),
        case Graph of
            {W, H, _, _, _} -> wxFrame:setVirtualSize(GraphP,
                                ?C(W*Zoom)+30,?C(H*Zoom)+30);
            _ -> ok
        end,
        wxFrame:refresh(GraphP)
      end).

%% Set rank and zoom for a node to be visible
%% DrawTools are the pens, brushes, etc needed for drawing nodes
make_node_visible(MinRank,Rank,Ranking,Zoom,Graph,GraphP,DrawTls) ->
    {{{NewMinRank, _}, _}, Index} 
        = hd(lists:filter(fun({{{A,_},_}, _}) ->
                            (A =< Rank) end,
                          lists:zip(Ranking,
                                    lists:seq(1,length(Ranking))))),
    case NewMinRank =/= MinRank of
        true -> total_graph_refresh(Zoom,Graph,GraphP,NewMinRank,DrawTls);
        false -> ok
    end,
    {NewMinRank, Zoom, Index}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Node click events
graph_node_click(N={_,_,_}) -> ?d(k),
    case ?HandleRes(?ExecLCall( node_file_pos, [N] )) of
        {no_file, _} -> graph_node_click("no data available in the database");
        {File, Pos}  -> {File, Pos};
        _            -> ok
    end;
    
graph_node_click(Element) ->
    ?wxMain:infowindow_main("Node data: " ++ 
                            ?Logic:write(decode_node_data(Element)), 
                            "Node information"),
    ok.

decode_node_data(fb) -> "module group";
decode_node_data(Element) -> Element.

%%%%% Options

%% Add different types of widgets dinamically
add_option_widgets(Panel,OptionsSz,?modgroupLvl) ->
    {RLab, RInp} 
        = add_input(Panel, OptionsSz, "Define modulegroup by regexp:"), 
    MGList = add_list_widget(Panel,OptionsSz,RInp,"Modulegroups", 
                             fun handle_add_correct_regexp/2, 150),  
    ListWidget = hd(MGList),
    add_current_dirs(ListWidget), 
    {RInp, RLab, MGList};

add_option_widgets(Panel,OptionsSz,?modLvl) -> 
    add_funmod_ws(Panel, OptionsSz, "module");

add_option_widgets(Panel,OptionsSz,?funLvl) -> 
    add_funmod_ws(Panel, OptionsSz, "function").

%% Add widgets for fun/mod depanal
add_funmod_ws(Panel, OptionsSz, L) ->
    {StartLab, Input} 
        = add_input(Panel, OptionsSz, "Add this to a list:"),
    Titles = ["Starting nodes", "Exclude these "++L++"s", 
              "Exclude children of these "++L++"s", 
              "Exclude these libs", "Connections"],
    wxSizer:addSpacer(OptionsSz,10),
    ChkBox = add_otp_exclude_chkbox(Panel,OptionsSz),
    InputLists = [begin
                    wxSizer:addSpacer(OptionsSz,10),
                    add_list_widget(Panel,OptionsSz,Input,T,50)
                  end || T <- Titles ],
    list_to_tuple([Input, StartLab, ChkBox] ++ InputLists).

%% Add an input and a label for it (with text L)
add_input(Parent, Sz, L) ->
    Lab = wxStaticText:new(Parent, ?wxID_ANY, L),
    Inp = wxStyledTextCtrl:new(Parent, [{size, {200, 18}}]),
    ?wxMain:set_stc_style(Inp),
    wxSizer:add(Sz,Lab, []),
    wxSizer:add(Sz,Inp, []),
    wxWindow:connect(Inp, stc_charadded),
    {Lab, Inp}.

%% Add a type changer radio box
add_type_changer(Panel,Sz) -> 
    TypeCh = wxRadioBox:new(Panel, ?typeCh, "Type", ?wxDefaultPosition, 
                         {200,70}, ["All", "Cicles"]),
    wxSizer:add(Sz,TypeCh, []),
    TypeCh.

%% Add an OTP exlusion checkbox
add_otp_exclude_chkbox(Panel,Sz) ->
    ChkBox = wxCheckBox:new(Panel, ?wxID_ANY, "Exclude OTP"),
    wxSizer:add(Sz,ChkBox, []),
    ChkBox.

%% Add a list widget to Sz sizer and Input (input for adding elements)
add_list_widget(Parent,Sz,Input,L,Heigth) ->
    add_list_widget(Parent,Sz,Input,L,fun handle_add/2,Heigth).
add_list_widget(Parent,Sz,Input,L,AddHandler,Heigth) ->
    Label = wxStaticText:new(Parent, ?wxID_ANY, L),
    ListW = wxListBox:new(Parent, ?wxID_ANY, [{size, {200, Heigth}}]),
    S = {size, {100, 22}},
    AddB = wxButton:new(Parent, ?wxID_ANY, [{label, "Add"}, S]),
    DelB = wxButton:new(Parent, ?wxID_ANY, [{label, "Delete"},S]),
    Sz2 = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(Sz,Label),
    wxSizer:add(Sz2,AddB),
    wxSizer:add(Sz2,DelB),
    wxSizer:add(Sz,Sz2),    
    wxSizer:add(Sz,ListW, []),
    wxFrame:connect(AddB, command_button_clicked, 
                  [{callback, fun(_,_)-> AddHandler(Input,ListW) end}]),
    wxFrame:connect(DelB, command_button_clicked, 
                    [{callback, fun(_,_)-> handle_del(ListW) end}]),
    [ListW, AddB, DelB, Label].

%% Delete widgets dinamically from a W0 widgetlist
delete_old_widgets(_Panel,OptionsSz,W0) ->
    WL = lists:flatten(tuple_to_list(W0)),
    [ begin
        case W of
            _ ->    wxSizer:detach(OptionsSz,W),
                    wxWindow:destroy(W)
        end
      end  || W <- WL ],
    [ wxSizer:remove(OptionsSz, S) || S <- lists:reverse( 
           lists:seq( 1, length( wxSizer:getChildren(OptionsSz) ) ) ) ].

%% Enable / disable permanent buttons according to current dep-level
disable_btns(Btns, ?modgroupLvl) ->
    ?wxMain:enable_btns(Btns, false);
disable_btns(Btns, _) ->
    ?wxMain:enable_btns(Btns, true).

%% Add currently loaded directories to a listbox widget (e. g. module group list)
add_current_dirs(ListBox) ->
    wxListBox:clear(ListBox),
    Dirs = ?HandleRes(?ExecLCall( get_loaded_directories, [] ), []),
    wxListBox:insertItems(ListBox, Dirs, 0).

%% Set the proper slider values (non-manual zooming)
set_proper_sldr_val(ZoomSldr,RankSldr,Zoom,RankIndex) ->
    wxSlider:setValue(ZoomSldr,Zoom),
    wxSlider:setValue(RankSldr,RankIndex).

%% Run a dependency analysis and draw its graph
run(Level, Type, GraphP, FrameW, FrameH, Opts, _RunB, AllBs, Main) ->
    ?wxMain:enable_btns(AllBs, false),
    ?wxMain:statusbar_notification(Main,"Running dependency analysis...", 
                                                                ?INFINITY),
    GraphCalcFun = 
        fun() -> 
            run_(Level, Type, GraphP, FrameW, FrameH, Opts)
        end,
    Res = ?HandleRes( 
        ?ExecLCall( cached_dep_analysis, [Level,Type,Opts,GraphCalcFun] )),
    case Res of
        {_, {_, _, _, [], _}, _, _} -> ?wxMain:infowindow_main(?empRes,?empRes);
        _                           -> ok
    end,
    ?wxMain:enable_btns(AllBs, true),
    ?wxMain:statusbar_notification(Main,"Dependency analysis finished.", 
                                                                ?INFINITY),
    Res.

%% Graph calc fun for dependency analysis (see Logic)
run_(Level, Type, _GraphP, FrameW, FrameH, Opts) ->
    Handler = 
        fun(FilePath,_) ->
            case ?HandleRes(?ExecLCall(get_graph,[FilePath]), no_graph )  of
                Graph0={W,H,_, _Nodes,_} ->
                    UserScale = calc_scaling(W,H,FrameW,FrameH),
                    Graph={_,_,Ranking,_,_}=
                     ?ExecLCall(to_inner_graph,[Graph0, UserScale,20,20]),
                    MinR = get_curr_min_rank(Ranking,?firstRankVal),
                    Zoom = ?firstZoomVal,
                    {Zoom, Graph, Ranking, MinR};
                no_graph ->
                    no_graph
            end
        end,
    ?HandleRes( 
        ?ExecLCall( dep_analysis, [Level, Type, Opts ]), Handler, [], no_graph ).

%% Get level from the number of the current level chooser widget
level(?funLvl) -> func;
level(?modLvl) -> mod;
level(?modgroupLvl) -> mb.

%% Get type from the number of the current type chooser widget
type(?cycleType) -> cyclic;
type(?allType) -> all.

%% Give a list of options containing a gnode only
gnode_only_options(Node) ->
    [{starting_nodes, [Node]}].

%% Gather options (regexp / exception,leaves,otp) from widgets
gather_options(Ws) ->
    case Ws of
        {_,_,ModGrList} ->
			[{groups, lists:map(fun get_group/1, list_from_listwidget(ModGrList))}];
        {_, _, OtpBox, StartInp, ExcList, LvsList, ExclLibs, Conns} ->
                            [{starting_nodes,  list_from_listwidget(StartInp)},
                             {exclude,         list_from_listwidget(ExcList)},
                             {exclude_lib,     list_from_listwidget(ExclLibs)},
                             {connection,      list_from_listwidget(Conns)},
                             {exclude_children,list_from_listwidget(LvsList)},
                             {exclude_otp,     otpbox(OtpBox)}];
        _ -> []
    end.

%% Get proper groups parameter
get_group(List) ->
	Group = string:tokens(List, " ,"),
	if length(Group) == 1 -> [G] = Group, G;
		true -> Group
	end.

%% Gather parameters using widgets
gather_params(Ws,CurrLevel,CurrType) ->
   {gather_options(Ws), level(CurrLevel), type(CurrType)}.

%% Get a list of tuples from a list box
list_from_listwidget([ListBox,_,_,_]) -> list_from_listwidget(ListBox);
list_from_listwidget(ListBox) ->
    [ wxListBox:getString(ListBox,N) || 
                        N <- lists:seq(0,wxListBox:getCount(ListBox)-1) ].

%% Include otp checked/unchecked
otpbox(OtpBox) ->
    wxCheckBox:isChecked(OtpBox).

%% Convert text written into textbox to a gnode parameter
input_to_gnode(Input) ->
    case string:strip(wxStyledTextCtrl:getText(Input)) of
            ""  -> {starting_nodes, []};
            X   -> {starting_nodes,[X]}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Send an event to the code browsers started from this module
send_browser_event(Browsers, Evt, Info) ->
    [ wx_object:get_pid(Br) ! {browser_event, Evt, Info} 
                                                    || Br <- Browsers ].
%% Get the current minimal rank from the ranking
get_curr_min_rank(Ranking,Nth) -> 
    {{A,_},_} = lists:nth(Nth,Ranking),
    A.

%% Complex function to show the next result of the search, now every data of the
%% next result is an argument of the function
search_res(GraphP,ActZoom,XX,YY,ActMinRank,Rank,
                  ActRanking,ActGraph,ZoomSldr,RankSldr,TXT,DrawTools) ->
    {NewMinRnk, NewZoom, RankIndex} = 
        make_node_visible(ActMinRank, Rank, ActRanking, ActZoom,
                         ActGraph, GraphP, DrawTools),
    set_proper_sldr_val(ZoomSldr,RankSldr,NewZoom,RankIndex),
    Btn = wxWindow:findWindowByName(TXT ++ io_lib:write({XX,YY})),
    wxWindow:setFocus(Btn),
    {NewMinRnk, NewZoom, RankIndex}.

%% In the following functions, MP is the pid of main module
%% Draw a smart graph and open it
draw_js_graph(Ws,CurrLevel,CurrType,AllBtns,DisBs,MP) ->
    Fun = fun(Res) -> wx_misc:launchDefaultBrowser(Res) end,
    prepare_and_run_(Ws,CurrLevel,CurrType,AllBtns,DisBs,draw_java_graph,Fun,MP).

%% Draw an SVG graph and open it
draw_svg_graph(Ws,CurrLevel,CurrType,AllBtns,DisBs,MP) ->
    Fun = fun(Res) -> wx_misc:launchDefaultBrowser(Res) end,
    prepare_and_run_(Ws,CurrLevel,CurrType,AllBtns,DisBs,draw_svg_graph,Fun,MP).

print_graph(Ws,CurrLevel,CurrType,AllBtns,DisBs,MP) ->
    Fun = fun(Res) -> show_printed(Res) end,
    prepare_and_run_(Ws,CurrLevel,CurrType,AllBtns,DisBs,print_dep_graph,Fun,MP).

show_printed(Result) -> 
    S = io_lib:format("~p", [Result]), 
    ?wxMain:infowindow_main(S,"Dependencies").

prepare_and_run_(Ws,CurrLevel,CurrType,AllBtns,DisBs,LCall,Handler,Main) ->
    {Opts,Lvl,Type} = gather_params(Ws,CurrLevel,CurrType),
    ?wxMain:enable_btns(AllBtns, false),
    ?wxMain:statusbar_notification(Main,"Running dependency analysis...",
                                                                ?INFINITY),
    case ?HandleRes(?ExecLCall(LCall, [Lvl,Type,Opts]), none) of
        none -> ?wxMain:infowindow_main("Error during analysis","Error");
        Res  -> ?wxMain:statusbar_notification(Main, "Dependency analysis"
                                                        " finished.",?INFINITY),
                Handler(Res)
    end,
    ?wxMain:enable_btns(AllBtns, true),
    disable_btns(DisBs, CurrLevel). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Translation, graphics, helpers

%% Calculate a scaling using which the graph exactly fits an area of FrameW*FrameH
calc_scaling(W,H,FrameW,FrameH) ->
    PW = W * ?INCH,
    PH = H * ?INCH,
    case FrameW > FrameH of
        true -> calc_scaling(PW,FrameW);
        false -> calc_scaling(PH,FrameH)
    end,
    min(calc_scaling(PW,FrameW),calc_scaling(PH,FrameH)).

calc_scaling(Val,FrameVal) ->
    case Val > FrameVal of
        true -> FrameVal/Val * ?INCH;
        false -> ?INCH
    end.

%% Calculate upper left corner from center point
calc_xy_from_cp({CX, CY}, W, H) ->  {?F(CX - W/2), ?F(CY - H/2)}.

%% Calculate the scrolled position of a widget
calc_scrolled_pos(ScWin, X, Y) ->
    wxScrolledWindow:calcScrolledPosition(ScWin, X, Y).

%% Convert a shape (given by dep.graph module) to a brush/colour
%% This function takes the proper element of a brush list according to
%% the shape given by GraphViz
shape_to_brush({triangle,_}, BrushesForShapes) -> lists:nth(1,BrushesForShapes);
shape_to_brush({box,_}, BrushesForShapes) -> lists:nth(2,BrushesForShapes);
shape_to_brush({hexagon,_}, BrushesForShapes) -> lists:nth(3,BrushesForShapes);
shape_to_brush({_,_}, _) -> ?wxTRANSPARENT_BRUSH.
