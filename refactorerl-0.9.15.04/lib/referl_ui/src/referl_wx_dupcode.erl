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

%%% @doc This module is a GUI for duplicated code analysis
%%% @author Gabor Hosszu

-module(referl_wx_dupcode).
-author("Gábor Hosszú").
-behaviour(wx_object).
 
-export([start/1, init/1, terminate/2,  code_change/3, filter_empty_params/1,
         handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).
 
-include_lib("referl_core/include/core_export.hrl").
-include_lib("wx/include/wx.hrl").
-include("wx.hrl").
-include("ui.hrl").
 
-record(state,
{
 parent,
 main_pid,
 access_to_db,
 runb,
 group_ch,
 choice,
 codebrs,
 codechs,
 algch,      %% Algorithm chooser
 fileinp,    %% Input field for filename or regexp
 filelist,
 nodes1,     %% RefactorErl nodes of the currently selected codes (const_var_diff)
 nodes2,
 avail_algs, %% Available duplicated code algorithms
 sizer,
 widgets,
 outer_sizer,
 dupname_widget
    }).

-define(runButton, 4010).
-define(difButton, 4013).
-define(loadButton, 4014).

-define(grpChooser, 4101).
-define(code1Chooser, 4102).
-define(code2Chooser, 4103).

-define(algChooser, 4503).
 
%% @private
start(Props) ->
    wx_object:start_link(?MODULE, Props, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
init(Props) ->
        wx:batch(fun() -> do_init(Props) end).
 
do_init(Props) ->
    Panel = proplists:get_value(parent, Props),
    MainPid = proplists:get_value(main_pid, Props, self()),

    %% MAC RELATED
    %% On Mac, wxStaticBoxSizers must be "defined" before the controls
    SetupWSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Setup"}]),

    RunB = wxButton:new(Panel, ?runButton, [{label,"Run analysis"},
                                            {size, {150, 25}}]),
    LoadB = wxButton:new(Panel, ?loadButton, [{label,"Load results..."},
                                            {size, {150, 25}}]),
    %%DifB = wxButton:new(Panel, ?difButton, [{label,"Show diffs"},
    %%                                        {size, {150, 25}}]),
    CodeGroupCh = wxChoice:new(Panel, ?grpChooser, [{size, {210, 25}}]),
    CH1 = wxChoice:new(Panel, ?code1Chooser, [{size, {100, 25}}]),
    CH2 = wxChoice:new(Panel, ?code2Chooser, [{size, {100, 25}}]),
    CB1 = referl_wx_codebrowser:start_browser(Panel, MainPid),
    CB2 = referl_wx_codebrowser:start_browser(Panel, MainPid),


    DupcodeAlgs = ?ExecLCall(get_dupcode_algorithms, []),
    AlgsWKeys = [ {proplists:get_value(label, AlgData), {AlgKey, AlgData}} 
                    || {AlgKey, AlgData} <- DupcodeAlgs ],

    Algs = [ AlgLabel || {AlgLabel, _} <- AlgsWKeys ],

    AlgCh  = wxComboBox:new(Panel, ?algChooser, [{value, hd(Algs)}, 
                                          {choices, Algs}, 
                                          {size, {210, 35}}]),

    AlgoTxt = wxStaticText:new(Panel, 4519, "Algorithm", []),
    CB1TxT = wxStaticText:new(Panel, 4515, "Left browser and right browser", []),
    DupcodeTxt = wxStaticText:new(Panel, 4520, "No dupcode loaded", []),
    %% CB2TxT = wxStaticText:new(Panel, 4515, "Right browser", []),
    CodeGroupTxT = wxStaticText:new(Panel, 4516, "Group", []),

    Expand  = [{flag, ?wxEXPAND}, {proportion, 1}],

    MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
        
    SetPaneSz = wxBoxSizer:new(?wxVERTICAL),
    CodeCH12Sz = wxBoxSizer:new(?wxHORIZONTAL),
    % CodeCHTxt12Sz = wxBoxSizer:new(?wxHORIZONTAL),
    TextCtrlSz = wxBoxSizer:new(?wxHORIZONTAL),
    
    BtnFlags = [{border, 6}, {flag, ?wxCENTRE bor ?wxALL}],
    wxSizer:add(SetupWSz, AlgoTxt),
    wxSizer:add(SetupWSz, AlgCh),
    wxSizer:add(SetPaneSz, SetupWSz, [{flag, ?wxEXPAND}]),
    wxSizer:addSpacer(SetPaneSz, 10),
    wxSizer:add(SetPaneSz, RunB, BtnFlags),
    wxSizer:add(SetPaneSz, DupcodeTxt, BtnFlags),
    wxSizer:add(SetPaneSz, LoadB, BtnFlags), 
    wxSizer:add(SetPaneSz, CodeGroupTxT, BtnFlags),
    wxSizer:add(SetPaneSz, CodeGroupCh, BtnFlags),
    wxSizer:add(CodeCH12Sz, CH1, BtnFlags),
    wxSizer:add(CodeCH12Sz, CH2, BtnFlags),
    wxSizer:add(SetPaneSz, CB1TxT, BtnFlags),
    wxSizer:add(SetPaneSz, CodeCH12Sz, BtnFlags),
    wxSizer:add(SetPaneSz, ?wxMain:make_placeholder(Panel, 200, 1), []),
    wxSizer:add(TextCtrlSz, CB1, Expand),
    wxSizer:add(TextCtrlSz, CB2, Expand),
    wxSizer:add(MainSizer, SetPaneSz, 
                           [{border, 10}, {flag, ?wxEXPAND}, {proportion, 0}]),
    wxSizer:add(MainSizer, TextCtrlSz, 
                           [{border, 10}, {flag, ?wxEXPAND}, {proportion, 1}]),
    

    ?wxMain:enable_btns([CH1, CH2, CodeGroupCh], false),

    {Key, _} = proplists:get_value(hd(Algs), AlgsWKeys),
    WidgetsData = ?ExecLCall(get_dupcode_algorithm_data, [Key]),
    Widgets = place_dupcode_widgets(Panel, SetupWSz, WidgetsData),

    %% CONNECT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    wxWindow:connect(Panel, command_button_clicked),
    wxWindow:connect(Panel, command_choice_selected),
    wxWindow:connect(Panel, command_combobox_selected),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    %% MAC RELATED
    wxPanel:setSizer(Panel, MainSizer),
    wxWindow:fit(Panel),
    wxSizer:fit(SetPaneSz, Panel),
    wxSizer:layout(MainSizer),

    {Panel, #state{parent=Panel, runb=RunB, group_ch=CodeGroupCh, choice=none,
                   codebrs={CB1, CB2}, codechs={CH1, CH2}, filelist=none,
                   fileinp=none, algch=AlgCh, dupname_widget=DupcodeTxt,
                   access_to_db=true, main_pid=MainPid, avail_algs=AlgsWKeys,
                   sizer=SetupWSz, widgets=Widgets, outer_sizer=SetPaneSz}}.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
%% Choosing code group
handle_event(#wx{id=?algChooser,event=#wxCommand{type=command_combobox_selected} }, 
         State = #state{ avail_algs=Algs, algch=AlgCh, widgets=Ws,
                         parent=Panel, sizer=Sz, outer_sizer=Outer }) ->
    {Key, _} = get_selected_alg_data(AlgCh, Algs),
    WidgetsData = ?ExecLCall(get_dupcode_algorithm_data, [Key]),
    [ begin
        wxSizer:detach(Sz,W),
        wxWindow:destroy(W)
      end || {_, _, _, W} <- Ws ],
    NewWs = place_dupcode_widgets(Panel, Sz, WidgetsData),
    wxSizer:layout(Sz),
    wxSizer:layout(Outer),
    {noreply,State#state{ widgets=NewWs }};

%% Choosing code group
handle_event(#wx{id=?grpChooser,event=#wxCommand{type=command_choice_selected} }, 
         State = #state{ group_ch=CodeGroupCh, choice=Ch, codechs=CCs}) ->
        WxCh = wxChoice:getSelection(CodeGroupCh),
        if Ch =/= WxCh ->
                   Group = wxChoice:getClientData(CodeGroupCh, WxCh),
                   handle_dup_group_load(Group, CCs);

           true -> ok
        end,
    {noreply,State#state{ choice=WxCh }};

%% Choosing code 1 and code 2 
handle_event(#wx{id=?code1Chooser,
         event=#wxCommand{ type=command_choice_selected } }, 
         State = #state{ codebrs={CB1, _CB2}, codechs={CH1, _CH2}}) ->
        Nodes = load_code(CB1,CH1),
    {noreply,State#state{ nodes1 = Nodes }};
handle_event(#wx{id=?code2Chooser,
         event=#wxCommand{ type=command_choice_selected } }, 
         State = #state{ codebrs={_CB1, CB2}, codechs={_CH1, CH2}}) ->
        Nodes = load_code(CB2,CH2),
    {noreply,State#state{ nodes2 = Nodes }};

%% Button clicks
handle_event(#wx{id=Id, event=#wxCommand{ type=command_button_clicked } }, 
      State = #state{ runb=RunB, group_ch=CodeGroupCh, codechs=CHs={CH1, CH2},
                      widgets=Widgets, choice=Ch, algch=AlgCh,
                      nodes1=Nodes1, nodes2=Nodes2, codebrs={CB1, CB2}, 
                      dupname_widget=DupcodeTxt }) ->
    Ch2 =
	case Id of
		?runButton    -> Btns = [RunB, CodeGroupCh, CH1, CH2],
                         run_dupcode_anal(Btns, CodeGroupCh, CHs, AlgCh,
                                          Widgets,
                                          State#state.main_pid, 
                                          State#state.avail_algs,
                                          DupcodeTxt),
                         none;
        ?difButton    -> show_diff(CB1, CB2, Nodes1, Nodes2),
                         Ch;
        ?loadButton   -> load_old_result(State#state.parent, CodeGroupCh, 
                                         CHs, DupcodeTxt),
                         Ch;
        _             -> Ch
	end,
    {noreply,State#state{ choice=Ch2 }};

handle_event(#wx{}, State = #state{}) ->
    {noreply, State}.
    
%% @private
handle_cast(_Msg, State) ->
    {noreply,State}.

%% @private
%% DB access
handle_info({event, db_access, Bool},State=#state{ runb=RunB }) ->
    ?wxMain:enable_btns([RunB], Bool),
    {Br1, Br2} = State#state.codebrs,
    Brws = [Br1, Br2],
    send_browser_event(Brws,db_access,Bool),
    {noreply, State#state{ access_to_db=Bool }};

handle_info({event, Evt, Info}, State) when Info =/= none ->
    {Br1, Br2} = State#state.codebrs,
    Brws = [Br1, Br2],
    send_browser_event(Brws,Evt,Info),
    {noreply, State};

handle_info({request_for_dupcode_search, Algorithm, ModulePath, StartPos, EndPos}, 
      State = #state{ runb=RunB, group_ch=CodeGroupCh, codechs=CHs={CH1, CH2},
                      dupname_widget=DupcodeTxt, 
                      main_pid=Main }) ->
    Btns = [RunB, CodeGroupCh, CH1, CH2],
    ?wxMain:enable_btns(Btns, false),
    ?wxMain:statusbar_notification(Main, "Duplicated code analysis started.", 
                                                    ?stbar_Notif_Timeout),
    ?HandleRes( ?ExecLCall(get_dupcode_by_pos, [ModulePath, Algorithm,
                                                StartPos, EndPos]), 
                         fun handle_dup_result/2, {CodeGroupCh, CHs, 
                                                   DupcodeTxt} ),
    ?wxMain:statusbar_notification(Main, "Duplicated code analysis finished.", 
                                                    ?stbar_Notif_Timeout),
    ?wxMain:infowindow_main("Duplicated code analysis finished", "Info"),
    ?wxMain:enable_btns(Btns, true), 
    {noreply, State#state{ choice=none }};
     
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
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Run duplicated code analisys
%% L1 and L2 are the two dup code choice lists
run_dupcode_anal(Btns, GroupChooser, Lists, AlgCh, Widgets, Main, DupcodeAlgs,
                 DupcodeTxt) -> 
    {AlgKey, _} = get_selected_alg_data(AlgCh, DupcodeAlgs),
    Params0 = [{algorithm, AlgKey}, {format, ui_format}, {postype, scalar}],
    case get_params(Widgets) of
        error -> none;
        Params1 ->
            Params = Params0 ++ Params1,
            FilteredParams = filter_empty_params(Params),
            run_dupcode_anal(Btns, GroupChooser, Lists, Main, FilteredParams, DupcodeTxt)
    end.

run_dupcode_anal(Btns, GroupChooser, {L1, L2}, Main, Params, DupcodeTxt) ->
    ?wxMain:enable_btns(Btns, false),
    ?wxMain:statusbar_notification(Main, "Duplicated code analysis started.", 
                                                    ?stbar_Notif_Timeout),
    ?HandleRes( ?ExecLCall(search_initial_clones, [Params]), 
                         fun handle_dup_result/2, {GroupChooser, {L1, L2}, 
                                                   DupcodeTxt} ),
    ?wxMain:statusbar_notification(Main, "Duplicated code analysis finished.", 
                                                    ?stbar_Notif_Timeout),
    ?wxMain:infowindow_main("Duplicated code analysis finished", "Info"),
    ?wxMain:enable_btns(Btns, true).

%% Handler for duplicated code analisys results
handle_dup_result({Name, Res}, {GroupChooser, {L1, L2}, DupcodeTxt}) ->
    set_dupname(?Logic:write(Name), DupcodeTxt),
    fill_choice_list(GroupChooser, Res, "Duplicate group"),
    handle_dup_group_load([], {L1, L2}).

%% Proplist of a code ("dup")
%% Fill L1 and L2 code choice lists
handle_dup_group_load(Group, {L1, L2}) ->
    fill_code_choice_list(L1, Group),
    fill_code_choice_list(L2, Group).

fill_code_choice_list(CodeChooser, Group) ->
    fill_choice_list(CodeChooser, Group, "Code").

fill_choice_list(WxChoice, Items, Title) ->
    wxChoice:clear(WxChoice),
    NumNRes = lists:zip(lists:seq(1,length(Items)), Items),
    FunToRun =
    fun({Index, Item}) ->
        Text = Title ++ " " ++ integer_to_list(Index),
        wxChoice:append(WxChoice, Text, Item)
    end,
    wx:foreach(FunToRun, NumNRes).   

load_code(CB,CH) ->
    Chosen = wxChoice:getSelection(CH),
    Data = wxChoice:getClientData(CH, Chosen),
    File = proplists:get_value(filepath, Data),
    St   = proplists:get_value(startpos, Data),
    End  = proplists:get_value(endpos, Data),
    Props = [{file, File}, {pos, {St, End}}],
    referl_wx_codebrowser:load_code(CB, Props),
    proplists:get_value(nodes, Data, []).

get_selected_alg_data(AlgCh, DupcodeAlgs) ->
    SelectedOption = wxComboBox:getValue(AlgCh),
    proplists:get_value(SelectedOption, DupcodeAlgs).

show_diff(_, _, undefined, undefined) -> ok;
show_diff(Browser1, Browser2, Nodes1, Nodes2) ->
    R = ?ExecLCall( get_const_var_diff, [Nodes1, Nodes2] ),
    case R of
        {[], []}     -> ?wxMain:infowindow_main("There are no differences.",
                                                "No differences"),
                        ok;
        {Pos1, Pos2} -> ScPos1 = [ Sc || {Sc, _} <- Pos1 ],
                        ScPos2 = [ Sc || {Sc, _} <- Pos2 ],
                        ?wxCB:highlight_positions(Browser1, ScPos1),
                        ?wxCB:highlight_positions(Browser2, ScPos2);
        _            -> error
    end.

%% Send an event to the code browsers started from this module
send_browser_event(Browsers, Evt, Info) ->
    [ wx_object:get_pid(Br) ! {browser_event, Evt, Info} 
                                                    || Br <- Browsers ].

place_dupcode_widgets(Parent, Sizer, WidgetsData) ->
    lists:flatten([ place_widget(Parent, Sizer, Widget) || Widget <- WidgetsData ]).

place_widget(Parent, Sizer, {_, Props}) ->
    Key = proplists:get_value(key, Props),
    Label = proplists:get_value(label, Props),
    Type = proplists:get_value(type, Props),
    EnumType = proplists:get_value(enumtype, Props, none),
    Default = proplists:get_value(default, Props),
    place_widget(Parent, Sizer, Type, Props, Default, Label, Key, EnumType).

place_widget(Parent, Sizer, enum, Props, Default, Label, Key, EnumType) ->
    Options = proplists:get_value(options, Props),
    LabelTxt = wxStaticText:new(Parent, ?wxID_ANY, Label, []),
    OptionsStrings = [ ?Logic:write(A) || A <- Options ],
    ComboBox = wxComboBox:new(Parent, ?wxID_ANY, [{value, ?Logic:write(Default)}, 
                                                  {choices, OptionsStrings}, 
                                                  {size, {210, 35}}]),
    wxSizer:add(Sizer, LabelTxt),
    wxSizer:add(Sizer, ComboBox),
    [{label, notype, nokey, LabelTxt}, {combobox, {enum, EnumType}, Key, ComboBox}];

place_widget(Parent, Sizer, atoms, _Props, Default, Label, Key, _) ->
    LabelTxt = wxStaticText:new(Parent, ?wxID_ANY, Label ++ " (separated with commas)", []),
    Size = get_proper_input_size(atoms),
    Input = wxTextCtrl:new(Parent, ?wxID_ANY, [{size, Size}]),
    wxTextCtrl:setValue(Input, ?Logic:write(Default)),
    wxSizer:add(Sizer, LabelTxt),
    wxSizer:add(Sizer, Input),
    [{label, notype, nokey, LabelTxt}, {input, atoms, Key, Input}];

place_widget(Parent, Sizer, boolean, _Props, _Default, Label, Key, _) ->
    LabelTxt = wxStaticText:new(Parent, ?wxID_ANY, Label, []),
    ChkBox = wxCheckBox:new(Parent, ?wxID_ANY, Label),
    wxSizer:add(Sizer, LabelTxt),
    wxSizer:add(Sizer, ChkBox),    
    [{label, notype, nokey, LabelTxt}, {checkbox, boolean, Key, ChkBox}];

%% atom, float, integer
place_widget(Parent, Sizer, Type, _, Default, Label, Key, _) ->
    LabelTxt = wxStaticText:new(Parent, ?wxID_ANY, Label, []),
    Size = get_proper_input_size(atoms),
    Input = wxTextCtrl:new(Parent, ?wxID_ANY, [{size, Size}]),
    wxTextCtrl:setValue(Input, ?Logic:write(Default)),
    wxSizer:add(Sizer, LabelTxt),
    wxSizer:add(Sizer, Input),    
    [{label, notype, nokey, LabelTxt}, {input, Type, Key, Input}].

get_params(Widgets) ->
    Params =
        lists:flatten([
        try
         get_param(W)
        catch
            _:badarg -> {_,Type,Key,_} = W,
                        ?wxMain:infowindow_main("Problem with parameter types: "
                                                ++ ?Logic:write(Type) ++ " needed for "
                                                ++ ?Logic:write(Key)
                                                 , "Error"),
                        error;
            _:_      -> []
        end || W <- Widgets ]),
    Errors =
    lists:filter(fun(error) -> true;
                    (_)     -> false end,
                Params),
    if length(Errors) == 0 -> Params;
       true                -> error
    end.


get_param({label, _, _, _}) ->
    [];

get_param({input, atoms, Key, Input}) ->
    String = wxTextCtrl:getValue(Input),
    Value = split_to_atomlist(String),
    {Key, Value};

get_param({input, Type, Key, Input}) ->
    String = wxTextCtrl:getValue(Input),
    Value = value_of_string(String, Type),
    {Key, Value};

get_param({checkbox, boolean, Key, CheckBox}) ->
    {Key, wxCheckBox:isChecked(CheckBox)};

get_param({combobox, {enum, EnumType}, Key, ComboBox}) ->
    String = wxComboBox:getValue(ComboBox),
    Value = value_of_string(String, EnumType),
    {Key, Value}.

split_to_atomlist(String) ->
    Strings = string:tokens(String, ","),
    [ list_to_atom(string:strip(S)) || S <- Strings ].

value_of_string(String, Type) ->
    case Type of
        float   -> ?Logic:list_to_num(String);
        string  -> String;
        integer -> list_to_integer(String);
        _Other  -> list_to_atom(String)
    end.

get_proper_input_size(atoms) -> {250, 25};
get_proper_input_size(atom) ->  {250, 25};
get_proper_input_size(_) ->     {40,  25}.

filter_empty_params(Params) ->
    Filter =
    fun({_, ''}) -> false;
       ({_, ""}) -> false;
       ([])      -> false;
       (_)       -> true end,
    lists:filter(Filter, Params).

load_old_result(Parent, GroupChooser, {L1, L2}, DupcodeTxt) ->
    Btns = [GroupChooser, L1, L2],
    ?wxMain:enable_btns(Btns, false),
    Dialog = wxTextEntryDialog:new(Parent, 
                    "Please enter the name of the result..."),
    case wxTextEntryDialog:showModal(Dialog) of
        ?wxID_OK -> Name = list_to_atom(wxTextEntryDialog:getValue(Dialog)),
                    Result = ?ExecLCall(get_dupcode_result, [Name]),
                    case Result of
                        {_, undefined} -> ?wxMain:infowindow_main("The duplicated code analysis result with that name does not exist.", "Error");
                        _              -> handle_dup_result(Result, {GroupChooser, {L1, L2}, DupcodeTxt}),
                                          atom_to_list(Name)
                    end;
        _        -> ok
    end,
    ?wxMain:enable_btns(Btns, true).

set_dupname(Name, DupcodeTxt) ->
    wxStaticText:setLabel(DupcodeTxt, Name).
