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

%%% @doc This module implements the file browser and administration interface
%%% of RefErl Wx. It also provides some functionality for handling listcontrols,
%%% and filling them with files found in Db. It implements event handling for
%%% "RefErl Wx events" sent by the main module, and uses Handler functions for
%%% visualization of file addig / dropping (percentage).
%%% @author Gabor Hosszu

-module(referl_wx_filebrowser).
-author("Gábor Hosszú").
-behaviour(wx_object).
 
-export([start/1, init/1, terminate/2,  code_change/3,
handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-export([list_files_in_db/2, listctrl_nth_row_getter/2,
         proper_column_sizes/1]). 

-include_lib("referl_core/include/core_export.hrl").
-include_lib("wx/include/wx.hrl").
-include("wx.hrl").
 
-record(state,
{
 parent,
 access_to_db,
 notebook,
 wxlists,   %% wx list controls for listing {files in db, errors, appbases}
 buttonsz,  %% button sizer
 file_only_btns,
 app_only_btns,
 other_btns,
 browsers,    %% Active browsers
 evthandler   %% Query and event handler object (wxMain)
              %% - query handler for opened file browsers
}).

-define(addFileButton, 2101).
-define(addAppbaseButton, 2103).
-define(addIncludeButton, 2105).
-define(addDirButton, 2102).
-define(dropFileButton, 2111).
-define(delEnvButton, 2112).
-define(resetDbButton, 2113).
-define(synchDbButton, 2114).
-define(precacheButton, 2115).
-define(forceRefButton, 2116).


-define(filePage, 0).
-define(errorPage, 1).
-define(appbPage, 2).

%% @private
start(Props) ->
    wx_object:start_link(?MODULE, Props, []).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
init(Props) ->
        wx:batch(fun() -> do_init(Props) end).
 
do_init(Props) ->
    Panel = proplists:get_value(parent, Props),
    Restricted = proplists:get_value(restricted_mode, Props),
    EvtHandler = proplists:get_value(main_pid, Props),
	ButtSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, []), % {label, "wxButton"}	    

    Fix = {size, {120, 30}},
    AllBtns =   
    [B01 = wxButton:new(Panel, ?addFileButton, [{label,"Add file"}, Fix]),
     B02 = wxButton:new(Panel, ?addDirButton, [{label,"Add dir"}, Fix]),
     B03 = wxButton:new(Panel, ?addAppbaseButton, 
                                               [{label,"Add appbase"}, Fix]),
     B04 = wxButton:new(Panel, ?addIncludeButton, 
                                               [{label,"Add include"}, Fix]),
     B11 = wxButton:new(Panel, ?dropFileButton, [{label,"Drop file"}, Fix]),
     B12 = wxButton:new(Panel, ?delEnvButton, 
                                               [{label,"Delete env"}, Fix]),
     B13 = wxButton:new(Panel, ?forceRefButton, 
                                        [{label,"Force file refresh"}, Fix]),
     B21 = wxButton:new(Panel, ?resetDbButton,
                                              [{label,"Reset DB"}, Fix]),
     B22 = wxButton:new(Panel, ?synchDbButton,
                                              [{label,"Synchronize DB"}, Fix]),
     B23 = wxButton:new(Panel, ?precacheButton, 
                                              [{label,"Precache files"}, Fix])],
    
    {FileOnlyBtns, AppOnlyBtns, OtherBtns, RestrictedBtns} =
    if Restricted -> { [B13], [], [B22, B23], 
                       [B01, B02, B03, B04, B11, B12, B21] };
       true       -> { [B11, B13], [B12], [B01, B02, B03, B04, B21, B22, B23],
                       [] }
    end,

    Notebook1 = wxNotebook:new(Panel, 2001, [{style, ?wxBK_DEFAULT}]),
    Style = [{style, ?wxLC_REPORT}], % bor ?wxLC_SINGLE_SEL}],
    FileListCtrl = wxListCtrl:new(Notebook1, Style), %% ?filePage
    setup_filelist(FileListCtrl),
    ErrListCtrl = wxListCtrl:new(Notebook1, Style), %% ?errorPage
    setup_errorlist(ErrListCtrl),
    AppbListCtrl = wxListCtrl:new(Notebook1, 
            [{style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL}]), %% ?appbPage
    setup_appblist(AppbListCtrl),

    wxNotebook:addPage(Notebook1, FileListCtrl, "Files in DB", []),
    wxNotebook:addPage(Notebook1, ErrListCtrl, "Errors", []),    
    wxNotebook:addPage(Notebook1, AppbListCtrl, "Configuration parameters", []),        

    Sz = wxBoxSizer:new(?wxHORIZONTAL),
    Expand  = [{flag, ?wxEXPAND bor ?wxALL}],
    %%Notebook1Sz = wxBoxSizer:new(?wxVERTICAL),
    
    BtnFlags = [{flag, ?wxRESERVE_SPACE_EVEN_IF_HIDDEN},
                {border, 6}, {flag, ?wxALL}], %%  bor ?wxEXPAND
    AddFun =
    fun(Button) ->
        wxSizer:add(ButtSz,Button, BtnFlags)
    end,
    wx:foreach(AddFun, AllBtns),

    %%wxSizer:add(Notebook1Sz,Notebook1, [{proportion, 1}] ++ Expand),
    
    wxSizer:add(Sz, ButtSz, [{proportion, 0}] ++ Expand),
    wxSizer:add(Sz, Notebook1, [{proportion, 1}] ++ Expand),


    %% CONNECT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    wxWindow:connect(Panel, command_list_item_activated),
    wxWindow:connect(Panel, command_button_clicked),
    wxNotebook:connect(Notebook1, command_notebook_page_changed,
		       [{skip, true}]),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    %% MAC RELATED
    wxPanel:setSizer(Panel, Sz),
    wxWindow:fit(Panel),
    wxSizer:layout(Sz),

    %% INIT
    %% On startup, the filelist (?filePage) is loaded
    put(fatal, ignore), %% if defined, no modals will show up on errors, 
    %% and db_busy exception will be thrown
    disable_restricted(RestrictedBtns),
    list_files_in_db(FileListCtrl), %% needs DB
    ?wxMain:hide_n_show(ButtSz, AppOnlyBtns, FileOnlyBtns),
    erase(fatal), 

    {Panel, #state{parent=Panel, notebook=Notebook1, access_to_db=true,
                   buttonsz=ButtSz, evthandler=EvtHandler, browsers=[],
                   file_only_btns=FileOnlyBtns, app_only_btns=AppOnlyBtns, 
                   other_btns=OtherBtns,
                   wxlists={FileListCtrl, ErrListCtrl, AppbListCtrl} }}.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
%% We connect every browser's event handler's close event to this
%% in order to be able to delete the browser from the browser list
%% This is kind of redefinition because the codebrowser has a default behaviour
%% for close events
handle_event(#wx{event=#wxClose{ }, userData=Br },  State) ->
        BrNew = lists:delete(Br, State#state.browsers),
        wxFrame:destroy(Br),
        {noreply, State#state{ browsers=BrNew }};

%% Files / errors / configuration parameters (notebook pages)
handle_event(#wx{event = #wxNotebook{type = command_notebook_page_changed}},
      State=#state{ file_only_btns=FileOnlyBtns, app_only_btns=AppOnlyBtns,
                    access_to_db=DB }) ->
    ButtSz  = State#state.buttonsz,
        Current = wxNotebook:getCurrentPage(State#state.notebook),
        ListCtrl = wx:typeCast(Current, wxListCtrl),
        case wxNotebook:getSelection(State#state.notebook) of
        ?filePage  -> if DB -> list_files_in_db(ListCtrl);
                         true -> show_busy(ListCtrl)
                      end,
                      ?wxMain:hide_n_show(ButtSz, AppOnlyBtns, FileOnlyBtns);
        ?errorPage -> ?wxMain:hide_n_show(ButtSz, AppOnlyBtns++FileOnlyBtns, []),
                      if DB -> list_errors(ListCtrl);
                         true -> show_busy(ListCtrl)
                      end;
        ?appbPage  -> if DB -> list_appbase_nodes(ListCtrl);
                         true -> show_busy(ListCtrl)
                      end,
                      ?wxMain:hide_n_show(ButtSz, FileOnlyBtns, AppOnlyBtns)
        end,
    {noreply,State};

%% Button clicks
handle_event(#wx{id=Id, event=#wxCommand{ type=command_button_clicked } }, 
        State = #state{ wxlists={FileListCtrl, _ErrListCtrl, AppbListCtrl} }) ->
    Buttons = State#state.file_only_btns ++ State#state.app_only_btns  ++
              State#state.other_btns,
    Parent = State#state.parent,
    Browsers = State#state.browsers,
	case Id of
		?addFileButton    -> add_file(Parent, Buttons);
		?addAppbaseButton -> add_appbase(Parent, Buttons);
		?addDirButton     -> add_dir(Parent, Buttons);
        ?dropFileButton   -> drop_file(FileListCtrl, Buttons);
        ?delEnvButton     -> del_env(AppbListCtrl, Buttons);
        ?resetDbButton    -> reset_db(FileListCtrl);
        ?synchDbButton    -> synch_db(FileListCtrl);
        ?addIncludeButton -> add_include(Parent, Buttons);
        ?precacheButton   -> precache_files(FileListCtrl, Buttons);
        ?forceRefButton   -> force_refresh(FileListCtrl, Buttons, Browsers)
	end,
    {noreply,State};

%% Filelist or errorlist item clicks (codebrowser opening)
handle_event(#wx{ event=#wxList{ type=command_list_item_activated,
         itemIndex=Item }},   State = #state{ wxlists={FLCtrl, ErrCtrl, _},
         evthandler=EvtHandler, parent=Parent, browsers=Brs,
         access_to_db=true }) ->
    case wxNotebook:getSelection(State#state.notebook) of
        ?filePage  -> Browser = ?wxCB:start_browser_window(Parent, EvtHandler),
                      wxFrame:connect(Browser,close_window,[{userData,Browser}]),
                      FilePath = (listctrl_nth_row_getter(FLCtrl,2))(Item),
                      Data = [{file, FilePath}],
                      ?HandleRes( ?wxCB:load_code(Browser, Data) ),
                      {noreply,State#state{ browsers=[Browser|Brs] }};
        ?errorPage -> Browser = ?wxCB:start_browser_window(Parent, EvtHandler),
                      wxFrame:connect(Browser,close_window,[{userData,Browser}]),
                      ExtPath = (listctrl_nth_row_getter(ErrCtrl,3))(Item), 
                      {FilePath, Pos} = parse_extpath(ExtPath),
                      Data = [{file, FilePath}, {pos, Pos}],
                      ?HandleRes( ?wxCB:load_code(Browser, Data) ),
                      {noreply,State#state{ browsers=[Browser|Brs] }};
        _          -> {noreply,State}
    end;

handle_event(#wx{}, State = #state{}) ->
    {noreply, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply,State}.

%% @private
%% Callbacks handled as normal gen_server callbacks
handle_info({event, db_access, Bool}, State) -> 
    Buttons = State#state.file_only_btns ++
              State#state.app_only_btns  ++
              State#state.other_btns,
    ?wxMain:enable_btns(Buttons, Bool),
    send_browser_event(State#state.browsers, db_access, Bool),
    {noreply, State#state{ access_to_db=Bool }};
%% Handler messages from add/drop handleenvenvrs
handle_info({handler, {Op, File, Percent}}, State) ->
    Txt =
    case Op of
        drop -> "Dropping "++ filename:basename(File) ++ " "
                    ++ integer_to_list(round(Percent * 100)) ++ "%";
        add  -> "Adding "++ filename:basename(File) ++ " "
                    ++ integer_to_list(round(Percent * 100)) ++ "%";
        _    -> "Operation: " ++ atom_to_list(Op)
    end,
    ?wxMain:statusbar_notification(State#state.evthandler,Txt, 
                                       ?stbar_ShownUp_Timeout),
    {noreply, State};
handle_info({event, Evt, Info}, 
                            State=#state{ wxlists={FLCtrl, _, AppBaseCtrl} }) ->
    Info =/= none andalso
            send_browser_event(State#state.browsers, Evt, Info),
    %% NotBusy = not .... !!!!
    NotBusy = ?ExecLCall( can_read_db, [] ),
    ((Evt == file_drop_finished) or (Evt == add_dir_finished)) 
        and NotBusy andalso list_files_in_db(FLCtrl),
    ((Evt == add_env_finished) or (Evt == del_env_finished)) 
        and NotBusy andalso list_appbase_nodes(AppBaseCtrl),
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
    
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Top-level functions (GUI-item functionality)

%% The following db modifier functions have 
%% a Buttons parameter: Buttons to enable/disable 

%% Add directory
add_dir(Parent, Buttons) ->
    gen_add(fun get_dirpath/2, add_dir, Parent, Buttons, fun() -> nothing end).
%% Add file
add_file(Parent, Buttons) -> 
    gen_add(fun get_filepath/2, add_dir, Parent, Buttons, fun() -> nothing end).
%% Add an appbase node
add_appbase(Parent, Buttons) -> 
    gen_add(fun get_dirpath/2, add_appbase,Parent, Buttons,fun() -> nothing end).
%% Add an include node
add_include(Parent, Buttons) ->
    gen_add(fun get_dirpath/2, add_include,Parent, Buttons,fun() -> nothing end).
%% Add something with Request (using the Path got with GetPathFun), 
%% and refresh with RefreshFun
%% Refresh means for example the update of the filelist after adding a file
%% Start a generic add process
gen_add(GetPathFun, Request, Parent, Buttons, RefreshFun) ->
    WxEnv = wx:get_env(),
    Self = self(),
    ToSpawn =
    fun() ->
        wx:set_env(WxEnv),
        ?wxMain:enable_btns(Buttons, false),
	    IsClient = ?HandleRes( ?ExecLCall(is_client, []), true ),
        Handler = make_load_handler(Self, Request),
	    case GetPathFun(Parent, IsClient) of
		    cancelled -> ok;
		    Path      -> ?HandleRes(?ExecLCall(Request, [Path,Handler])),
					     RefreshFun()
        end,
        ?wxMain:enable_btns(Buttons, true)
    end,
    erlang:spawn_link(ToSpawn).

%% Drop a file / files from the db and then refresh filelist (wx listcontrol)
drop_file(FileListCtrl, Buttons) -> 
    Self =self(),
    gen_drop(Buttons, FileListCtrl,
            fun() -> nothing end,
            fun(Entities) ->  
                Handler = make_drop_handler(Self, drop_file),
                Files = [ File || [_,_,File] <- Entities ],
                ?HandleRes(?ExecLCall(drop_dir, [Files,Handler])) end).

%% Delete appbase env / envs
del_env(AppbListCtrl, Buttons) -> 
    Self = self(), 
    gen_drop(Buttons, AppbListCtrl,                                 
            fun() -> nothing end,
            fun(Entities) -> 
                Handler = make_drop_handler(Self, del_env),
                [ ?HandleRes(?ExecLCall(del_env,
                     [Entity,list_to_atom(Type),Handler]),[])
                    || [_, Type, Entity] <- Entities ] end).

%% Start a generic dropping process
gen_drop(Buttons, ListCtrl, RefreshFun, DropFun) ->
    WxEnv = wx:get_env(),
    ToSpawn =
    fun() ->
        wx:set_env(WxEnv),
        ?wxMain:enable_btns(Buttons, false),
        Selected = ?wxMain:get_selected(ListCtrl),
        FunToRun = listctrl_full_row_getter(ListCtrl),
        Entities = wx:map(FunToRun, Selected),
        DropFun(Entities),
        RefreshFun(),
        ?wxMain:enable_btns(Buttons, true)
    end,
    erlang:spawn_link(ToSpawn).

precache_files(FileListCtrl, Buttons) ->
    WxEnv = wx:get_env(),
    AllItems = ?wxMain:get_all(FileListCtrl),
    FunToRun = listctrl_nth_row_getter(FileListCtrl,2),
    Files = wx:map(FunToRun, AllItems),
    Dirs = lists:usort([ filename:dirname(File) || File <- Files ]),
    ToSpawn =
    fun() ->
        wx:set_env(WxEnv),
        ?wxMain:enable_btns(Buttons, false),
        ?HandleRes(?ExecLCall(precache_files, [Dirs])),
        ?wxMain:enable_btns(Buttons, true)
    end,
    erlang:spawn_link(ToSpawn).

force_refresh(FileListCtrl, Buttons, Browsers) -> 
    WxEnv = wx:get_env(),
    Selected = ?wxMain:get_selected(FileListCtrl),
    FunToRun = listctrl_nth_row_getter(FileListCtrl,2),
    Files = wx:map(FunToRun, Selected),
    ToSpawn =
    fun() ->
        wx:set_env(WxEnv),
        ?wxMain:enable_btns(Buttons, false),
        ?HandleRes(?ExecLCall(force_refresh_files, [Files])),
        [ send_browser_event(Browsers, file_add, File) || File <- Files ],
        ?wxMain:enable_btns(Buttons, true)
    end,
    erlang:spawn_link(ToSpawn).  

list_files_in_db(FileListCtrl) -> list_files_in_db(FileListCtrl,?IdFun).
%% @doc List files in a Wx list control
list_files_in_db(FileListCtrl,Filter) ->
    list_entities_from_db(FileListCtrl, Filter, get_loaded_files_list, 
                                            file_cols_add(FileListCtrl)).
list_appbase_nodes(AppbListCtrl) -> 
    list_entities_from_db(AppbListCtrl, ?IdFun, get_envs,
                                            appb_cols_add(AppbListCtrl)).

%% ListingRequest is a UI request which we get a list of entities with.
list_entities_from_db(ListCtrl, Filter, ListingRequest, ListAddFun) ->
    wxListCtrl:enable(ListCtrl), 
    EntityList = ?ExecLCall(ListingRequest, []),
    ?HandleRes(EntityList,fun list_entities_from_db_helper/2,
               {ListCtrl, Filter, ListAddFun}).
list_entities_from_db_helper(EntityList, {ListCtrl, Filter, ListAddFun}) -> 
    EList2 = Filter(EntityList),
    wxListCtrl:deleteAllItems(ListCtrl),
    ItemCnt = length(EList2) - 1,
    %% removed cause of Mac: wxListCtrl:setItemCount(ListCtrl, ItemCnt),
    Indexes = lists:seq(0,ItemCnt),
    IndexNItem = lists:zip(Indexes, lists:sort(EList2)),
    wx:foreach(ListAddFun, IndexNItem),
    proper_column_sizes(ListCtrl).

file_cols_add(ListCtrl) ->
    fun({Index, Item}) ->
        wxListCtrl:insertItem(ListCtrl, Index, ""),
        wxListCtrl:setItem(ListCtrl, Index, 0, erlang:integer_to_list(Index+1)),
        FullPth = ?Logic:write(Item),
        FName = filename:basename(FullPth),
        wxListCtrl:setItem(ListCtrl, Index, 1, FName),
        wxListCtrl:setItem(ListCtrl, Index, 2, FullPth)
    end.

appb_cols_add(ListCtrl) ->
    fun({Index, {Type, Item}}) ->
        wxListCtrl:insertItem(ListCtrl, Index, ""),
        wxListCtrl:setItem(ListCtrl, Index, 0, erlang:integer_to_list(Index+1)),
        wxListCtrl:setItem(ListCtrl, Index, 1, Type),
        wxListCtrl:setItem(ListCtrl, Index, 2, ?Logic:write(Item))
    end.
    
list_errors(ErrPanel) -> 
    wxListCtrl:enable(ErrPanel),
    ErrList = ?ExecLCall(get_error_forms_in_db, []),
    ?HandleRes(ErrList, fun list_errors_helper/2, ErrPanel).
list_errors_helper(ErrList, ErrPanel) ->
    WxEnv = wx:get_env(),
    ToSpawn =
    fun() ->
        wx:set_env(WxEnv),
        wxListCtrl:deleteAllItems(ErrPanel),
        ItemCnt = length(ErrList) - 1,
        %% removed cause of Mac:  wxListCtrl:setItemCount(ErrPanel, ItemCnt),
        Indexes = lists:seq(0,ItemCnt),
        IndexNItem = lists:zip(Indexes, lists:sort(ErrList)),
        FunToRun =
        fun({Index, {Path, P1, Length, Err}}) ->
            ExtPath = Path ++ " (position " ++ io_lib:write(P1)
                           ++ "-" ++ io_lib:write(P1+Length-1) ++ ")",
            wxListCtrl:insertItem(ErrPanel, Index, ""),
            wxListCtrl:setItem(ErrPanel, Index, 0, integer_to_list(Index+1)),
            wxListCtrl:setItem(ErrPanel, Index, 1, filename:basename(Path)),
            wxListCtrl:setItem(ErrPanel, Index, 2, Err),
            wxListCtrl:setItem(ErrPanel, Index, 3, ExtPath)
        end,
        wx:foreach(FunToRun, IndexNItem)
    end,
    erlang:spawn_link(ToSpawn).   

show_busy(_ListCtrl) ->
    ?wxMain:infowindow_main("The tool is busy, try to view the elements of"
                            " the list later.", "Cannot list elements").

reset_db(FileListCtrl) ->
    Dialog =     
    wxSingleChoiceDialog:new(FileListCtrl, 
    "Do you really want to reset the database?", 
    "Reset database", ["Yes, in absolute mode",
                       "Yes, in relative mode",
                       "No"]),
    wxSingleChoiceDialog:showModal(Dialog) == ?wxID_OK andalso
      case wxSingleChoiceDialog:getSelection(Dialog) of
          0 -> reset_db_helper(FileListCtrl, abs, "absolute");
          1 -> reset_db_helper(FileListCtrl, rel, "relative");
          2 -> cancelled
      end.

reset_db_helper(FileListCtrl, PosMode, PosTxt) ->
    ?HandleRes( ?ExecLCall(reset_db, [PosMode]), 
                fun(_,_) ->
                        ?wxMain:infowindow_main("The database has "
                            "been reseted. Current positioning mode: "
                             ++ PosTxt, "Database reset")
                end, []),
    list_files_in_db(FileListCtrl).   

synch_db(FileListCtrl) ->
    ?HandleRes( ?ExecLCall(synch_db, []) ),
    list_files_in_db(FileListCtrl).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers

%% Get filepath.
%%
get_filepath(Parent, IsClient) ->
   get_path(Parent, IsClient, wxFileDialog).

get_dirpath(Parent, IsClient) ->
   get_path(Parent, IsClient, wxDirDialog).

get_path(Parent, false, Mod) ->
    FileD = Mod:new(
              Parent,
              []),
    case Mod:showModal(FileD) of
		?wxID_OK -> Mod:getPath(FileD);
		_		 -> cancelled
	end;
get_path(Parent, true, _Mod) ->
    TextD = wxTextEntryDialog:new(
              Parent,
			  "Path on the server:",
              []),
	case wxTextEntryDialog:showModal(TextD) of
		?wxID_OK -> wxTextEntryDialog:getValue(TextD);
		_		 -> cancelled
	end;
get_path(_Parent, _UIerror, _Mod) -> cancelled.


%% Setup the style of the filelist
setup_filelist(FileList) -> 
    setup_list(FileList, 3, ["", "Filename", "Filepath"]). 
setup_appblist(AppbList) -> 
    setup_list(AppbList, 3, ["", "Type", "Path"]).

setup_list(List, Cols, Titles) ->
    Cnt = lists:seq(0,Cols-1),
    [ begin
        wxListCtrl:insertColumn(List, Num, lists:nth(Num+1,Titles))
      end || Num <- Cnt ].

setup_errorlist(ErrorList) -> 
    wxListCtrl:insertColumn(ErrorList, 0, ""),
    wxListCtrl:insertColumn(ErrorList, 1, "Filename"),
    wxListCtrl:insertColumn(ErrorList, 2, "Error"),
    wxListCtrl:insertColumn(ErrorList, 3, "Path"),
    wxListCtrl:setColumnWidth(ErrorList, 0, 40),
    wxListCtrl:setColumnWidth(ErrorList, 1, 150),
    wxListCtrl:setColumnWidth(ErrorList, 2, 530),
    wxListCtrl:setColumnWidth(ErrorList, 3, 530).


make_load_handler(SrvPid, add_dir) ->
    fun({_, progress, {add, _File, _, _Max}}) ->
            ok;
       ({_, progress, {add, File, Percent, _, _, _}}) ->
            SrvPid ! {handler, {add, File, Percent}};
       (_) ->
            ok
    end;
make_load_handler(_SrvPid, _) -> ?IdFun.
make_drop_handler(SrvPid, drop_file) ->
    fun({_, progress, {drop, _File, _, _Max}}) ->
            ok;
       ({_, progress, {drop, File, Percent, _, _, _}}) ->
            SrvPid ! {handler, {drop, File, Percent}};
       (_) ->
            ok
    end;
make_drop_handler(_SrvPid, _) -> ?IdFun.

%% @doc Get the nth row of a ListCtrl (in a specific column)
listctrl_nth_row_getter(ListCtrl, Column) ->
    fun(S) ->
        Item0 = wxListItem:new(), 
        wxListItem:setId(Item0, S),
        wxListItem:setColumn(Item0, Column), 
        wxListItem:setMask(Item0, ?wxLIST_MASK_TEXT),
        wxListCtrl:getItem(ListCtrl, Item0),
        wxListItem:getText(Item0) end.

listctrl_full_row_getter(ListCtrl) -> 
    fun(S) ->
        Item0 = wxListItem:new(), 
        Cols = lists:seq(0,wxListCtrl:getColumnCount(ListCtrl)-1),
        [ begin
            wxListItem:setId(Item0, S),
            wxListItem:setColumn(Item0, I), 
            wxListItem:setMask(Item0, ?wxLIST_MASK_TEXT),
            wxListCtrl:getItem(ListCtrl, Item0),
            wxListItem:getText(Item0)
          end || I <- Cols ]
        end.

send_browser_event(Browsers, Evt, Info) ->
    [ wx_object:get_pid(Br) ! {browser_event, Evt, Info} 
                                                    || Br <- Browsers ].

%% Under Wx, it is not possible to add client data to a list ctrl row.
%% We had to add this data explicitly, written as a string. This
%% function gets back data with some parsing.
parse_extpath(ExtPath) ->
    [FPath, _PosTxt, Pos0] = string:tokens(ExtPath, " "),
    Pos = lists:sublist(Pos0, length(Pos0) - 1), %% remove ")" 
    [P1,P2] = string:tokens(Pos, "-"),
    {FPath, {list_to_integer(P1), list_to_integer(P2)}}.

%% @doc Set proper column sizes for a ListCtrl
proper_column_sizes(ListCtrl) ->
    ColumnNum = wxListCtrl:getColumnCount(ListCtrl),
    Num = wxListCtrl:getItemCount(ListCtrl),
    Size = 
    if Num > 0 -> ?wxLIST_AUTOSIZE;
       true    -> ?wxLIST_AUTOSIZE_USEHEADER
    end,
    [ wxListCtrl:setColumnWidth(ListCtrl, I, Size) ||
       I <- lists:seq(0,ColumnNum - 1)  ].

%% Disable restricted buttons
disable_restricted(RestrictedBtns) ->
    [ wxButton:disable(Btn) || Btn <- RestrictedBtns ].
