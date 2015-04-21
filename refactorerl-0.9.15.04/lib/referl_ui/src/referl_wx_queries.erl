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

%%% @doc This is the query GUI of RefErl Wx. It implements a GUI where the user
%%% can start and manage queries and skeletons, and browse their results (cached).
%%%
%%% This module also implements a "Query Handler", which means that it
%%% handles {request_for_query, RequesterPid, QStr, File, Pos} info messages
%%% coming from a Code Browser (referl_wx_codebrowser)
%%% @author Gabor Hosszu

-module(referl_wx_queries).
-author("Gábor Hosszú").
-behaviour(wx_object).
 
-export([start/1, init/1, terminate/2,  code_change/3,
         handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).
 
-include_lib("referl_core/include/core_export.hrl").
-include_lib("wx/include/wx.hrl").
-include("wx.hrl").
-include("ui.hrl").
 
-record(state,
{
 parent,
 access_to_db,
 notebook,
 browser,
 qinput,          %% Query input
 queries,         %% ListCtrl for queries/skeletons
 lastrestools,    %% Tuple: Main sizer, last result sizer, last result tree
 flist,           %% Files list
 scrbox,          %% Searchbox (input)
 btns,            %% Buttons
 %% btns_to_disable, %% Buttons to disable when running queries
 user,
 runlist,         %% ListCtrl for running queries
 ch,              %% List type chooser (my/all queries, skeletons)
 grpbyinput       %% Input for group by expressions
    }).

%% Autocompletion list separator is defined in wx.hrl

-define(runQButton, 3101).
-define(delQButton, 3102).
-define(killQButton, 3103).
-define(modQButton, 3104).
-define(savSButton, 3105).

-define(runQPage, 2). %% Running queries' page
-define(myQsPage, 0). %% Queries list page
-define(fsPage,   3). %% Files list page

-define(myQueries, 0). %% myQueries list
-define(allQueries, 1). %% allQueries list
-define(skeletons, 2). %% skeletons list

-define(modalId, 3995).

-define(FLID, 3801). %% ID of FileListCtrl

%% @private
start(Props) ->
    wx_object:start_link(?MODULE, Props, []).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
init(Props) ->
        wx:batch(fun() -> do_init(Props) end).
 
do_init(Props) ->
    User = proplists:get_value(user, Props),
    Panel = proplists:get_value(parent, Props),


    %% MAC related
    %% StaticBox "definition" comes before controls
    QueryTabsSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, []),
    TxtArea1Sz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, []),
    QueryTabs = wxNotebook:new(Panel, 3001, [{style, ?wxBK_DEFAULT}]),
    QueryPref = wxPanel:new(QueryTabs, []), %%
    RunBoxSz = wxStaticBoxSizer:new(?wxVERTICAL, QueryPref, [{label, "Query"}]),
    QBoxSz = wxStaticBoxSizer:new(?wxVERTICAL, QueryPref, 
                                               [{label, "Latest queries"}]),
    %%

    LastRes = wxPanel:new(QueryTabs, []), %%
    RunningQs = wxPanel:new(QueryTabs, []), %%
    Files = wxPanel:new(QueryTabs, []), %%
    FilterTxt = wxStaticText:new(Files, ?wxID_ANY, "Filter:"),

    wxNotebook:addPage(QueryTabs, QueryPref, "Queries", []),
    wxNotebook:addPage(QueryTabs, LastRes, "Last Results", []),   
    wxNotebook:addPage(QueryTabs, RunningQs, "Running queries", []),   
    wxNotebook:addPage(QueryTabs, Files, "Files in DB", []),
    Fix = {size, {150, 30}},
    BtnFlags = [{border, 4}, {flag, ?wxCENTRE bor ?wxALL}],
    B01 = wxButton:new(QueryPref, ?runQButton, [{label,"Run"}, Fix]),
    B02 = wxButton:new(QueryPref, ?delQButton, [{label,"Delete"}, Fix]),
    B03 = wxButton:new(QueryPref, ?modQButton, [{label,"Modify skeleton"}, Fix]),
    B04 = wxButton:new(QueryPref, ?savSButton, [{label,"Save as skeleton"}, Fix]),

    Choices = ["My queries", "All queries", "Skeletons"],
    ListChooser = wxChoice:new(QueryPref, 4, [{choices, Choices}]),
    wxChoice:setToolTip(ListChooser, 
                                 "Here you can choose which items to display"),

    GroupByTxt = wxStaticText:new(QueryPref, ?wxID_ANY, "Group by:"),
    QueryInput = wxStyledTextCtrl:new(QueryPref, [{size, {350, 48}}]),
    setup_stc(QueryInput),
    GroupByInput = wxStyledTextCtrl:new(QueryPref, [{size, {350, 20}}]),
    setup_stc(GroupByInput),

    QueriesList = wxListBox:new(QueryPref, 3301, [{style, ?wxLB_SINGLE},
                                         {size, {350, 200}}]),
    wxChoice:setToolTip(QueriesList, 
                                 "Double click to run a query!"),

    LastResTree = wxTreeCtrl:new(LastRes, [{style, ?wxTR_DEFAULT_STYLE bor
                                               ?wxTR_HAS_VARIABLE_ROW_HEIGHT}]),

    RunningList = wxListCtrl:new(RunningQs, [{style, ?wxLC_REPORT bor
                                                     ?wxLC_SINGLE_SEL},
                                               {size, {180, 200}}]),

    SearchBox = wxStyledTextCtrl:new(Files, [{size, {100, 22}}]),
    SrcSz = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(SrcSz, FilterTxt, BtnFlags),
    wxSizer:add(SrcSz, SearchBox, BtnFlags),

    setup_stc(SearchBox),
    FilesList = wxListCtrl:new(Files,[{style,?wxLC_REPORT bor ?wxLC_SINGLE_SEL},
                                      {size, {180, 200}}, {winid, ?FLID}]),

    setup_rqlist(RunningList),
    setup_fslist(FilesList),
    B10 = wxButton:new(RunningQs, ?killQButton, [{label,"Kill query"}, Fix]),
    
    Browser = referl_wx_codebrowser:start_browser(Panel, self()),
    
    Expand  = [{flag, ?wxEXPAND}],
    Sz = wxBoxSizer:new(?wxHORIZONTAL),
    QueryPrefSz = wxBoxSizer:new(?wxVERTICAL),
    LastResSz = wxBoxSizer:new(?wxVERTICAL),   
    FilesSz = wxBoxSizer:new(?wxVERTICAL),   
    RQSz = wxBoxSizer:new(?wxVERTICAL), 
    LsTools = {Sz, LastResSz, LastRes, LastResTree}, 

    wxSizer:add(QueryTabsSz,QueryTabs, [{proportion, 1}] ++ Expand),  
    wxSizer:add(QueryTabsSz, ?wxMain:make_placeholder(Panel, 350,1), 
                                                            [{proportion, 0}]),   

    wxSizer:add(RunBoxSz, B01, BtnFlags),
    wxSizer:add(RunBoxSz, B04, BtnFlags),
    wxSizer:add(RunBoxSz,QueryInput, BtnFlags),
    wxSizer:add(RunBoxSz,GroupByTxt, BtnFlags),
    wxSizer:add(QueryPrefSz, RunBoxSz, BtnFlags),
    wxSizer:add(RunBoxSz,GroupByInput, BtnFlags),
    wxSizer:addSpacer(QueryPrefSz, 20),
    wxSizer:add(QBoxSz,B02, BtnFlags),
    wxSizer:add(QBoxSz,B03, BtnFlags),     
    wxSizer:add(QBoxSz, ListChooser, [{border, 6}, {flag, ?wxLEFT}]),
    wxSizer:add(QBoxSz, QueriesList, BtnFlags),
    wxSizer:add(QueryPrefSz, QBoxSz, BtnFlags),
    wxSizer:add(LastResSz, LastResTree, [{proportion, 1}] ++ Expand),
    wxSizer:add(RQSz, RunningList, [{proportion, 1}] ++ Expand),
    wxSizer:add(RQSz, B10, BtnFlags),
    wxSizer:add(FilesSz, SrcSz, BtnFlags),
    wxSizer:add(FilesSz, FilesList, [{proportion, 1}] ++ Expand),
    wxSizer:add(TxtArea1Sz, Browser, [{proportion, 1}] ++ Expand),
    
    wxSizer:add(Sz, QueryTabsSz, [{proportion, 1}] ++ Expand),
    wxSizer:add(Sz, TxtArea1Sz, [{proportion, 7}] ++ Expand),
    wxPanel:setSizer(QueryPref, QueryPrefSz),
    wxPanel:setSizer(LastRes, LastResSz),
    wxPanel:setSizer(RunningQs, RQSz),
    wxPanel:setSizer(Files, FilesSz),
    wxPanel:setSizer(Panel, Sz),

    %% CONNECT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    wxWindow:connect(SearchBox, stc_change),
    wxWindow:connect(QueryInput, stc_charadded),
    wxWindow:connect(QueryInput, stc_autocomp_selection),
    wxWindow:connect(Panel, command_choice_selected),
    wxWindow:connect(Panel, command_button_clicked),
    wxWindow:connect(Panel, command_listbox_doubleclicked),
    wxWindow:connect(Panel, command_listbox_selected),
    wxWindow:connect(Panel, command_tree_item_activated),
    wxWindow:connect(Panel, command_list_item_activated),
    wxNotebook:connect(QueryTabs, command_notebook_page_changed,
               [{skip, true}]),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    %% INIT
    %% The first list we can see is "My queries", so we have to initialize it
    put(fatal, ignore), %% if defined, no modals will show up on errors, 
    %% and db_busy exception will be thrown
    wxChoice:setSelection(ListChooser, ?myQueries),
    Type = item_type_from_selection(User, ?myQueries),
    list_query_items(QueriesList, Type), %% needs DB
    active_proper_btns_for_list({B01, B02, B03}, Type),
    erase(fatal),

    %% This part was modified to support mac
    wxPanel:setSizer(Panel, Sz),
    wxWindow:fit(Panel),
    wxSizer:layout(Sz),

    {Panel, #state{parent=Panel, user=User, access_to_db=true, flist=FilesList,
            notebook=QueryTabs, browser=Browser, qinput=QueryInput,
            queries=QueriesList, lastrestools=LsTools,
            runlist=RunningList, ch=ListChooser, btns={B01, B02, B03},
            scrbox=SearchBox, grpbyinput=GroupByInput}}.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
%% On listbox selection we decide if the del/mod buttons must be active/deactive
%% The first clause is active when we change to different list type
%% (my/all/skeletons)
handle_event(#wx{event = #wxCommand{type = command_listbox_selected, 
                                    commandInt=-1 } }, State) ->
    {noreply,State};
handle_event(#wx{event = #wxCommand{type = command_listbox_selected, 
         cmdString=_PrettyQStr} },
         State = #state{ queries=QueriesList, btns={_RunB,DelB,ModB},
                         user=User, ch=Ch }) ->
    ActualListType = wxChoice:getSelection(Ch),
    case ActualListType of
        ?myQueries  -> ok;
        X           ->
                {1, [Selected]} = wxListBox:getSelections(QueriesList),
                %% Getting client data described at list_query_items definition
                UserData = wxListBox:getClientData(QueriesList, Selected),
                Users = 
                case X of
                    ?allQueries -> get_user_data_users(UserData);
                    ?skeletons  -> [get_skel_owner(UserData)]
                end,
                Bool = lists:member(User,Users),
                ?wxMain:enable_btns([DelB], Bool),
                if Bool -> wxButton:setLabel(ModB, "Modify skeleton");
                   true -> wxButton:setLabel(ModB, "Skeleton info")
                end
    end,
    {noreply,State};

%% On listbox doubleclick we run the clicked query
handle_event(#wx{event = #wxCommand{type = command_listbox_doubleclicked, 
         cmdString=_PrettyQStr} },
         State = #state{ queries=QueriesList, lastrestools={_,_,LastRes,_},
                         ch=Ch,
                         user=User, runlist=RunningQList, parent=Panel }) ->
    Selected = case wxListBox:getSelections(QueriesList) of
        {1, [S]} -> S;
        {0, _  } -> -1
    end,
    ActualListType = wxChoice:getSelection(Ch),
    Selected =/= -1 andalso
    case ActualListType of
        ?skeletons -> 
            UserData = wxListBox:getClientData(QueriesList, Selected),
            Name = get_skel_name(UserData),
            SkelCall =  ?HandleRes(?ExecLCall(skeleton_call_format, [Name]), []),
            case ask_for_skel_args(Panel, SkelCall) of
                cancelled -> cancelled;
                Call      -> run_query(User, QueriesList, LastRes, {Call, none}, 
                                        RunningQList, no_file, no_pos) 
            end;                   
        _          ->
                    %% Getting client data described at list_queries definition
                    UserData = wxListBox:getClientData(QueriesList, Selected),
                    QStrAndGroupBy = get_user_data_qstr_n_grpby(UserData),
                    {_, File, Pos, _} = get_user_data_qkey(UserData),
                    run_query(User, QueriesList, LastRes, QStrAndGroupBy, 
                              RunningQList, File, Pos)
    end, 
    {noreply,State};

%% On notebook page change (tab change) we list the proper items
%% running queries / my/all queries / skeletons / filelist
handle_event(#wx{event = #wxNotebook{type = command_notebook_page_changed}},
      State = #state{ notebook=Notebook, runlist=RunningQList, user=User, 
                      queries=QBox, ch=Ch, flist=FilesList, scrbox=SearchBox,
                      access_to_db=true}) ->
        %% sometimes gave an exception
        Selection = try wxNotebook:getSelection(Notebook) catch _:_ -> none end,
        case Selection of
        ?runQPage  -> list_running_queries(RunningQList);
        ?myQsPage  -> list_query_items(QBox, 
                       item_type_from_selection(User,wxChoice:getSelection(Ch)));
        ?fsPage    -> ?wxFB:list_files_in_db(FilesList,filter(SearchBox));
        _          -> ok
        end,
    {noreply,State};

handle_event(#wx{ event=#wxList{ type=command_list_item_activated,
      itemIndex=Item }, id=?FLID }, 
      State = #state{ browser=Browser, flist=FilesList }) ->
    FilePath = (?wxFB:listctrl_nth_row_getter(FilesList,2))(Item),
    Data = [{file, FilePath}],
    ?HandleRes( ?wxCB:load_code(Browser, Data) ),
    {noreply,State};

%% On button click we do the proper action 
handle_event(#wx{event = #wxCommand{type = command_button_clicked},  id=Id},
         State = #state{ queries=QueriesList, qinput=QInput, grpbyinput=GrpByInput,
                         lastrestools={_,_,LastRes,_}, ch=Ch,
                         user=User, runlist=RunningQList, parent=Panel }) ->
    case Id of
        ?runQButton  -> run_query(User, QueriesList, LastRes, {input, QInput, GrpByInput}, 
                                 RunningQList, no_file, no_pos);
        ?delQButton  -> delete_item(QueriesList,User,wxChoice:getSelection(Ch));
        ?killQButton -> kill_query(User,RunningQList),
                        list_running_queries(RunningQList);
        ?modQButton  -> item_info(Panel,QueriesList,User,
                                wxChoice:getSelection(Ch));
        ?savSButton  -> save_as_skeleton(Panel, QueriesList, User, QInput,
                                         wxChoice:getSelection(Ch));
        _            -> ok
    end,
    {noreply,State};

%% On choice selection change (my/all/skeleton chooser) we list the proper items
handle_event(#wx{event = #wxCommand{type = command_choice_selected} },
         State = #state{ queries=QueriesList, user=User, ch=Ch, btns=Btns }) ->
    ItemType = item_type_from_selection(User, wxChoice:getSelection(Ch)),
    active_proper_btns_for_list(Btns, ItemType),
    list_query_items(QueriesList, ItemType),
    {noreply,State};

%% On tree item click (query result tree) we load the proper code (and highlight)
handle_event(#wx{event = #wxTree{type = command_tree_item_activated, item=Item}},
                        State = #state{lastrestools={_,_,_,Tree}, 
                        browser=Browser }) ->
    Data = wxTreeCtrl:getItemData(Tree, Item),
    case Data of 
        [] -> ?wxMain:infowindow_main("There is no code in the database " 
                                      "related to this item.", "No code available");
        _  -> handle_tree_item_click(Tree, Item, Data, Browser)
    end,
    {noreply,State#state{}};

%% Listing files
handle_event(#wx{event = #wxStyledText{type = stc_change} },
      State = #state{ flist=FilesList, scrbox=SearchBox, access_to_db=true }) ->
    ?wxFB:list_files_in_db(FilesList,filter(SearchBox)),
    {noreply,State};

%% Autocomplete
handle_event(#wx{event = #wxStyledText{type = Type}, obj=QInput },
                 State = #state{ qinput=QInput, access_to_db=true }) 
                when ((Type == stc_charadded) or (Type == stc_autocomp_selection)) ->
    CursorPos = wxStyledTextCtrl:getAnchor(QInput),
    Txt = wxStyledTextCtrl:getTextRange(QInput, 0, CursorPos),
    Res = ?HandleRes(?ExecLCall( autocomplete, [Txt]), [] ),
    {CompleteList0, _ReplaceWith} = lists:unzip(Res),
    CompleteList = string:join(CompleteList0, ?SEP),
    L = ?HandleRes(?ExecLCall( calc_ac_len_from_clist, [Txt, CompleteList0]), [] ),
    CompleteList =/= [] andalso 
        wxStyledTextCtrl:autoCompShow(QInput, L, CompleteList),
    {noreply,State};

%% Anything else (other events)
handle_event(#wx{}, State = #state{}) ->
    {noreply, State}.

%% @private    
handle_cast(_Msg, State) ->
    {noreply,State}.

%% @private
%% Callbacks handled as normal gen_server callbacks
%% Query request handling (See referl_codebrowser for more information)
handle_info({request_for_query, {_RequesterPid,_RequesterObj}, QStr, File, Pos}, 
             State = #state{ queries=QueriesList,
                lastrestools={_,_,LastRes,_},
                         user=User, runlist=RunningQList }) ->
    run_query(User, QueriesList, LastRes, {QStr, none}, RunningQList, File, Pos),      
    {noreply, State};

%% DB access
handle_info({event, db_access, Bool},State=#state{ user=User, btns={RunB,DelB,ModB} }) ->
    if Bool ->
        Type = item_type_from_selection(User, ?myQueries),
        active_proper_btns_for_list({RunB,DelB,ModB}, Type);
      true ->
        ?wxMain:enable_btns([RunB, DelB, ModB], Bool)
    end,
    wx_object:get_pid(State#state.browser) ! {browser_event, db_access, Bool},
    {noreply, State#state{ access_to_db=Bool }};

handle_info({event, Evt, Info}, State=#state{ flist=FilesList, 
                                              scrbox=SearchBox    }) ->
    wx_object:get_pid(State#state.browser) ! {browser_event, Evt, Info},
    NotBusy = ?ExecLCall( can_read_db, [] ),
    if ((Evt == file_drop_finished) or (Evt == add_dir_finished)) 
        and NotBusy -> ?wxFB:list_files_in_db(FilesList,filter(SearchBox));
       (Evt == semantic_query)
        and NotBusy -> list_running_queries(State#state.runlist);
       true         -> ok
    end,
    {noreply, State};

handle_info({new_result_tree, NewTree}, 
            State=#state{ lastrestools={Sz, LastResSz, LastRes, Tree} }) ->
    wxSizer:detach(LastResSz, Tree),
    wxTreeCtrl:destroy(Tree),   
    wxSizer:add(LastResSz, NewTree, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:layout(LastResSz),
    wxSizer:layout(Sz),
    wxNotebook:setSelection(State#state.notebook, 1),
    {noreply, State#state{ lastrestools={Sz,LastResSz,LastRes,NewTree} }};

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
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Running "Items" (Item means query or skeleton)
%% IStr is a Query string or a Skeleton
%% Written into QInput
run_query(User, QueriesList, LastRes, {input, Input, GrpByInput}, RunningQList, File, Pos) ->
    IStr = ?Logic:trim_whitespace(wxStyledTextCtrl:getText(Input)),
    GroupByText = ?Logic:trim_whitespace(wxStyledTextCtrl:getText(GrpByInput)),
    GroupBy = case GroupByText of
                    ""          -> none;
                    GroupByExpr -> list_to_integer_safe(GroupByExpr)
              end,
    run_query(User, QueriesList, LastRes, {IStr, GroupBy}, RunningQList, File, Pos);


%% Runs a query, and handles its results using QueriesList
%% listbox (Wx item)
%% 
run_query(User, QueriesList, LastRes, {IStr, GroupBy}, RunningQList, File, Pos) ->
    WxEnv = wx:get_env(),
    %% ?wxMain:enable_btns(BtnsToDisable, false),
    MyPid = self(),
    ToSpawn =
    fun() ->
        wx:set_env(WxEnv),
        ?HandleRes(?ExecLCall(run_query_or_skel, [User, IStr, {File, Pos}, GroupBy]), 
                              fun query_handler/2,{{IStr, QueriesList, LastRes,
                                MyPid},                          RunningQList})
        %% ?Logic:can_read_db() andalso 
        %%      ?wxMain:enable_btns(BtnsToDisable, true)
    end,
    spawn_link(ToSpawn).

%% Handle query result, display it, and refresh running queries
query_handler(Res, {Args, RunningQList}) ->
    wx:batch( fun() ->
    list_running_queries(RunningQList),
     query_handler(Res, Args) end );

%% appending items to the tree and handling different types of results
query_handler(no_query, _) -> ok;
query_handler(Res, {QTxt, QueriesList, LastRes, MyPid}) ->
    %wxTreeCtrl:deleteAllItems(Tree),
    Tree =  wxTreeCtrl:new(LastRes, [{style, ?wxTR_DEFAULT_STYLE bor
                                       ?wxTR_HAS_VARIABLE_ROW_HEIGHT}]),
    RootId = wxTreeCtrl:addRoot(Tree, "Results of " ++ QTxt),
    query_res_handler(Res, {QTxt, QueriesList, Tree}, RootId),
    wxTreeCtrl:expand(Tree,RootId),
    wxTreeCtrl:sortChildren(Tree, RootId),
    MyPid ! {new_result_tree, Tree}.

query_res_handler([], {_QTxt, _QueriesList, _Tree}, _RootId) ->
    ok;

query_res_handler([{group_by,{nopos,"No Result"},list,[]}], QWidgets, RootId) ->
    query_res_handler([], QWidgets, RootId);

query_res_handler([{list,[]}], QWidgets, RootId) ->
    query_res_handler([], QWidgets, RootId);

query_res_handler([{eq, Name, Value}], {_IStr, _QueriesList, Tree}, RootId) ->
    Txt = io_lib:format("~p = ~p", [Name,Value]),
    Item = wxTreeCtrl:appendItem(Tree, RootId, Txt),
    wxTreeCtrl:setItemTextColour(Tree, Item, {100, 100, 100}),
    wxTreeCtrl:expand(Tree,Item);

query_res_handler([{list,L}], {_IStr, _QueriesList, Tree}, RootId) ->
    Fun =
    fun({{File, Pos1, Pos2}, Txt}) ->
            Item = wxTreeCtrl:appendItem(Tree, RootId, Txt),
            wxTreeCtrl:expand(Tree,Item),
            wxTreeCtrl:setItemData(Tree, Item, [{file, File}, 
                                                {pos, {Pos1, Pos2}}]),
            wxTreeCtrl:setItemBold(Tree, Item);
        ({nopos, Txt}) ->
            Item = wxTreeCtrl:appendItem(Tree, RootId, Txt),
            wxTreeCtrl:setItemTextColour(Tree, Item, {100, 100, 100})
    end,
    wx:foreach(Fun, L);

query_res_handler([{group_by, {_Pos, Text}, propertylist, PropName, PropertyList} | Tail ], 
                                          {IStr, QueriesList, Tree}, RootId) ->
    Sub = wxTreeCtrl:appendItem(Tree, RootId, Text),
    PropertyStringList = [ if is_list(Item) -> Item;
                              true -> io_lib:write(Item) end || Item <- PropertyList ],
    PropertyString =  lists:flatten(string:join(PropertyStringList,", ")),
    Txt = io_lib:format("~p = ~s", [PropName,PropertyString]),
    Item = wxTreeCtrl:appendItem(Tree, Sub, Txt),
    wxTreeCtrl:setItemTextColour(Tree, Item, {100, 100, 100}),
    wxTreeCtrl:expand(Tree,Sub),
    query_res_handler(Tail, {IStr, QueriesList, Tree}, RootId);

query_res_handler([{group_by, {_Pos, Text}, eq, Name, Value} | Tail ], 
                                          {IStr, QueriesList, Tree}, RootId) ->
    Sub = wxTreeCtrl:appendItem(Tree, RootId, Text),
    Format = if is_atom(Name) -> "~p = ~p";
                true -> "~s = ~p"
             end,
    Txt = io_lib:format(Format, [Name,Value]),
    Item = wxTreeCtrl:appendItem(Tree, Sub, Txt),
    wxTreeCtrl:setItemTextColour(Tree, Item, {100, 100, 100}),
    wxTreeCtrl:expand(Tree,Sub),
    query_res_handler(Tail, {IStr, QueriesList, Tree}, RootId);

query_res_handler([{group_by, {_Position, Text}, list, L} | Tail ], 
                                          {IStr, QueriesList, Tree}, RootId) ->
    Sub = wxTreeCtrl:appendItem(Tree, RootId, Text),
    query_res_handler([{list,L}], {IStr, QueriesList, Tree}, Sub),
    wxTreeCtrl:expand(Tree,Sub),
    query_res_handler(Tail, {IStr, QueriesList, Tree}, RootId);

query_res_handler([{chain, L, _PostWS} | Tail ], 
                                          {IStr, QueriesList, Tree}, RootId) ->
    Sub = wxTreeCtrl:appendItem(Tree, RootId, "Chain"),
    query_res_handler([{list,L}], {IStr, QueriesList, Tree}, Sub),
    wxTreeCtrl:expand(Tree,Sub),
    query_res_handler(Tail, {IStr, QueriesList, Tree}, RootId);

query_res_handler([ Term | Tail ], {IStr, QueriesList, Tree}, RootId) ->
    Txt = io_lib:write("Error: " ++ Term),
    Item = wxTreeCtrl:appendItem(Tree, RootId, Txt),
    wxTreeCtrl:setItemTextColour(Tree, Item, {100, 100, 100}),
    query_res_handler(Tail, {IStr, QueriesList, Tree}, RootId).

%% List running queries in a Wx ListCtrl
list_running_queries(ListCtrl) ->
    Rqs = ?HandleRes(?ExecLCall(get_running_queries, []), []),
    wxListCtrl:deleteAllItems(ListCtrl),
    ItemCnt = length(Rqs) - 1,
    %% removed cause of Mac: wxListCtrl:setItemCount(ListCtrl, ItemCnt),
    IndNRqs = lists:zip(lists:seq(0,ItemCnt), Rqs),
    FunToRun = 
    fun({Index, #rq_info{user=U,qid=Q,qstr=QS}}) ->
            wxListCtrl:insertItem(ListCtrl, Index, ""),
            wxListCtrl:setItem(ListCtrl, Index, 0, io_lib:write(Q)),
            wxListCtrl:setItem(ListCtrl, Index, 1, QS),
            wxListCtrl:setItem(ListCtrl, Index, 2, U)
    end,
    wx:foreach(FunToRun, IndNRqs),
    ?wxFB:proper_column_sizes(ListCtrl),
    ok.

%% Setup Running Queries list initial style
setup_rqlist(RunningQList) -> 
    wxListCtrl:insertColumn(RunningQList, 0, "QId"),
    wxListCtrl:insertColumn(RunningQList, 1, "Query"),
    wxListCtrl:insertColumn(RunningQList, 2, "User").

%% Setup Files list initial style
setup_fslist(RunningQList) -> 
    wxListCtrl:insertColumn(RunningQList, 0, ""),
    wxListCtrl:insertColumn(RunningQList, 1, "Filename"),
    wxListCtrl:insertColumn(RunningQList, 2, "Path").

%% Kill query, if it is ours (the query of the current user)
%% Performs error handling if it is a query of another user
kill_query(User,RunningQList) ->
    case ?wxMain:get_selected(RunningQList) of
        [SId | _ ] ->
            Item0 = wxListItem:new(), 
            wxListItem:setId(Item0, SId),
            wxListItem:setColumn(Item0, 2), 
            wxListCtrl:getItem(RunningQList, Item0),
            UserTxt = wxListItem:getText(Item0),
            wxListItem:setColumn(Item0, 0), 
            wxListCtrl:getItem(RunningQList, Item0),
            QId = list_to_integer(wxListItem:getText(Item0)),
            if UserTxt == User ->
                    spawn_link( fun() ->
                                   ?HandleRes( ?ExecLCall( kill_query,  [QId] ) )
                                end );
               true -> 
                    ?wxMain:infowindow_main("This query belongs to another user!", 
                                            "Error")
            end;
        []      -> cancelled
    end.      
     
%% List the User's/All queries or skeletons to a listbox
%% Add data to the listbox items: {QueryStr, {SafeQStr, File, Pos}}
%% Make a fancy string to be displayed as query name
%% Add user data to each list element
list_query_items(QueriesListBox, skeletons) ->
    Ss = ?HandleRes( ?ExecLCall( list_skeletons, [] ), [] ),
    wxListBox:clear(QueriesListBox), 
    FunToRun =
    fun(R={Name, Body, Owner, _ParamCardinality, _Comment}) ->
        FancyTitle = 
        case Name of
            "" -> Body ++ " (" ++ Owner ++ ") ";
            _  -> Name ++ " (" ++ Owner ++ ") "
        end,
        wxListBox:append(QueriesListBox, FancyTitle, 
                                         make_user_data(skeletons, R))
    end,
    wx:foreach(FunToRun, Ss);
list_query_items(QueriesListBox, {queries, User}) ->
    Qs = ?HandleRes(?ExecLCall( get_users_queries,  [User] ), []),
    wxListBox:clear(QueriesListBox),
    FunToRun =
    fun(Rec) ->
        FancyTitle = Rec#q_info.qstr ++ " " ++
                    get_query_filepos_string(Rec#q_info.key),
        wxListBox:append(QueriesListBox, FancyTitle, 
                                         make_user_data(queries, Rec))
    end,
    wx:foreach(FunToRun, Qs).

item_type_from_selection(User, ?myQueries)  -> {queries, User};
item_type_from_selection(_,    ?allQueries) -> {queries,  all};
item_type_from_selection(_,    ?skeletons)  -> skeletons;
item_type_from_selection(User, _)  -> {queries, User}.

%% User data handling for list items (skeletons/my/all queries)
make_user_data(queries, Rec) -> 
    {_, _, _, GrpBy} = Rec#q_info.key,
    {Rec#q_info.qstr, Rec#q_info.key, Rec#q_info.users, GrpBy};
make_user_data(skeletons, {Name, Body, Owner, _ParamCardinality, Comment}) ->
    {Name, Body, Comment, Owner}.
get_user_data_qstr_n_grpby({QStr, _, _, GroupBy}) -> {QStr, GroupBy}.
get_user_data_qkey({_, SafeQ, _, _}) -> SafeQ.
get_user_data_users({_, _, Users, _}) -> Users.
get_skel_name({Name, _, _, _}) -> Name.
get_skel_owner({_, _, _, Owner}) -> Owner.
get_skel_body({_, Body, _, _}) -> Body.
get_skel_cmt({_, _, Cmt, _}) -> Cmt.


%% Delete query/skeleton from QueriesList (ListBox wx item)
%% (We also refresh the list)
%% ChosenList is ?skeletons or ?my/allQueries
delete_item(QueriesListBox, User, ChosenList) ->
    Refresh = query_list_refresh(QueriesListBox, User, ChosenList),
    case wxListBox:getSelections(QueriesListBox) of
            {0, _}          -> ok;
            {1, [Selected]} ->  
                UserData = wxListBox:getClientData(QueriesListBox, Selected),
                delete_item(User, UserData, ChosenList, Refresh, Selected)
    end.
delete_item(_User, UserData, ?skeletons, Refresh, _Selected) -> 
                Name = get_skel_name(UserData),
                ?HandleRes(?ExecLCall(delete_skeleton, [Name]),
                            Refresh, []);
delete_item(User, UserData, _, Refresh, _Selected) ->
                QKey = get_user_data_qkey(UserData),
                ?HandleRes(?ExecLCall(delete_user_query, [QKey, User]),
                            Refresh, []).
%% Editable or non-editable info window (modal) for skeletons
item_info(Parent, QueriesListBox, User, ?skeletons) -> 
    case wxListBox:getSelections(QueriesListBox) of
            {0, _}          -> ok;
            {1, [Selected]} ->  
                UData = wxListBox:getClientData(QueriesListBox, Selected),
                Owner = get_skel_owner(UData),
                skeleton_dialog(Parent, QueriesListBox, User, Owner, UData)
    end;
item_info(_Parent, _QueriesListBox, _User, _ChosenList) -> ok.

%% Save as a skeleton
save_as_skeleton(Parent, QueriesList, User, QInput, CurrList) -> 
    Body = ?Logic:trim_whitespace(wxStyledTextCtrl:getText(QInput)),
    skel_save_window(Parent, QueriesList, User, Body, CurrList).
%% Save window for the function before this
skel_save_window(_, _, _, "", _CurrList) -> cancelled;
skel_save_window(Parent, QueriesList, User, Body, CurrList) ->
    Dialog = wxTextEntryDialog:new(Parent, 
                                   "Please type in the name of the skeleton!",
                                   [{caption, "Name of the skeleton"}]),
    case wxDialog:showModal(Dialog) of
        ?wxID_OK -> Name = wxTextEntryDialog:getValue(Dialog),
                    Refresh = query_list_refresh(QueriesList, User, CurrList),
                    ?HandleRes(?ExecLCall(save_skeleton,[Name, Body, User]),
                                Refresh, []);
        _        -> cancelled
    end.

%% Ask the user to give the skeleton arguments
ask_for_skel_args(Parent, SkelCall) ->
    Dialog = wxTextEntryDialog:new(Parent, 
                                "Please fill in the arguments of the skeleton!",
                                 [{caption, "Arguments of the skeleton"}]),
    wxTextEntryDialog:setValue(Dialog, SkelCall),
    case wxDialog:showModal(Dialog) of
        ?wxID_OK -> wxTextEntryDialog:getValue(Dialog);
        _        -> cancelled
    end.    

%% Handle click
%% Item is the clicked item, Data is a proplist, holding filename and position
%% values, browser is a codebrowser window
handle_tree_item_click(_Tree, _Item, Data, Browser) ->
    case db_data_check(Data) of
        true -> ?HandleRes( ?wxCB:load_code(Browser, Data) );
        _    -> no_data
    end.
db_data_check(Data) -> 
    File = proplists:get_value(file, Data),
    ?ExecLCall( is_file_alive, [File] ) .

%% Activate proper buttons.
%% When listing queries no modify button needed.
active_proper_btns_for_list({_, Del, Mod}, {queries, _}) ->
    ?wxMain:enable_btns([Del], true),
    ?wxMain:enable_btns([Mod], false);
active_proper_btns_for_list({_, Del, Mod}, skeletons) ->
    ?wxMain:enable_btns([Del, Mod], true).

%% Skeleton dialogs.
%% When User == Owner, the user can edit the skeleton, else he can only get info
%% about it.
%% Create a modal dialog where the user can update skeletons:
skeleton_dialog(Parent, QListBox, Owner, Owner, UserData)->
    wx:batch( 
    fun() ->
        {Dial, BodyInp, CmtInp} =
         create_skeleton_modal_dialog(Parent, UserData),
        Name = get_skel_name(UserData),
        case wxDialog:showModal(Dial) of
            ?wxID_OK -> 
                    NewBody = wxTextCtrl:getValue(BodyInp),
                    NewCmt  = wxTextCtrl:getValue(CmtInp),
                    Refresh = query_list_refresh(QListBox, Owner, ?skeletons),
                    ?HandleRes(?ExecLCall(update_skeleton, 
                               [Name, NewBody, Owner, NewCmt]), Refresh, []);
            _        -> cancelled
        end
    end);
%% Create a modal dialog where the user can check skeleton properties:
skeleton_dialog(Parent, _QListBox, _User, _Owner, UserData) ->
    wx:batch( 
    fun() ->
        {Dial, BodyInp, CmtInp} =
         create_skeleton_modal_dialog(Parent, UserData),
        ?wxMain:enable_btns([BodyInp, CmtInp], false),
        case wxDialog:showModal(Dial) of
            ?wxID_OK -> ok;
            _        -> cancelled
        end
    end).    

%% Create a modal dialog where the user can update skeletons
%% Returns {Dialog, BodyInput, CommentInput}
%% TODO: no update if no change
create_skeleton_modal_dialog(Parent, UserData) ->
    Dial = wxDialog:new(Parent, ?modalId, "Skeleton properties"),
    wxDialog:setSize(Dial, 320, 400),
    % _Owner = get_skel_owner(UserData),
    Name = get_skel_name(UserData),
    Body = get_skel_body(UserData),
    CmtTxt = get_skel_cmt(UserData),
    NameSt = wxStaticText:new(Dial, ?modalId + 1, Name),
    BodyLab = wxStaticText:new(Dial, ?modalId + 4, "Body:"),
    CmtLab = wxStaticText:new(Dial, ?modalId + 5, "Comment:"),
    Inp2 = wxTextCtrl:new(Dial, ?modalId + 2, [{size, {300, 90}},
                                               {style, ?wxTE_MULTILINE}]),
    Cmt = wxTextCtrl:new(Dial, ?modalId + 3, [{size, {300, 90}},
                                              {style, ?wxTE_MULTILINE}]),
    Sz2 = wxDialog:createButtonSizer(Dial, ?wxOK bor ?wxCANCEL),
    DSz = wxBoxSizer:new(?wxVERTICAL),
    BtnFlags = [{border, 10}, {flag, ?wxLEFT bor ?wxALL}],
    Sz = wxBoxSizer:new(?wxVERTICAL),
    wxBoxSizer:add(DSz, NameSt, BtnFlags),
    wxBoxSizer:add(DSz, BodyLab, BtnFlags),
    wxBoxSizer:add(DSz, Inp2, BtnFlags), 
    wxBoxSizer:add(DSz, CmtLab, BtnFlags),
    wxBoxSizer:add(DSz, Cmt, BtnFlags),   
    wxSizer:add(Sz, DSz, [{flag, ?wxEXPAND}, {proportion, 7}]),  
    wxSizer:add(Sz, Sz2, [{proportion, 1}]),  
    wxPanel:setSizer(Dial, Sz),
    wxSizer:layout(Sz),
    wxTextCtrl:setValue(Inp2,Body),
    wxTextCtrl:setValue(Cmt,CmtTxt),
    {Dial, Inp2, Cmt}.

%% Get a refresh fun for items in the current list (query or skeleton list)
query_list_refresh(QueriesListBox, User, ChosenList) ->
    fun(_, _) -> 
        list_query_items(QueriesListBox, 
                        item_type_from_selection(User, ChosenList)) 
    end.

%% Get a fancy string from a SafeQuery tuple
get_query_filepos_string({_, no_file, no_pos, GrpBy}) -> groupby_str(GrpBy);
get_query_filepos_string({_, File, Pos, GrpBy}) -> 
    filename:basename(File) ++ ":" ++ integer_to_list(Pos) ++ groupby_str(GrpBy).

groupby_str(none) -> "";
groupby_str(GrpBy) -> " grouped by " ++ integer_to_list(GrpBy).

%% Setup a styled text control
setup_stc(STC) ->
    wxStyledTextCtrl:setMarginWidth(STC, ?wxSTC_MARGIN_NUMBER, 0),
    wxStyledTextCtrl:setUseHorizontalScrollBar(STC, false),
    wxStyledTextCtrl:autoCompSetSeparator(STC, ?SEP_INT).

filter(SearchBox) ->
    Txt = wxStyledTextCtrl:getText(SearchBox),
    fun(Entities) ->
        lists:filter(fun(FPath) ->
                        STR = string:str(FPath,Txt),
                        if  Txt == ""          -> true;
                            STR > 0            -> true;
                            true               -> false
                        end
                    end, Entities) end.

list_to_integer_safe(GroupByExpr) ->
    try
        list_to_integer(GroupByExpr)
    catch
        _:badarg -> -1
    end.
