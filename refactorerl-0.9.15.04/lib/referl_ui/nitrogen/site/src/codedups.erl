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

%%% @author Tamas Hoffman <hoffmantamas@caesar.elte.hu>

%% -*- mode: nitrogen -*-

-module (codedups).
-compile(export_all).
-include_lib("referl_ui/include/nitrogen.hrl").
-include_lib("nitrogen/include/wf.hrl").
-include_lib("kernel/include/file.hrl").
-include("records.hrl").
-vsn("$Rev: 12252 $ ").

-define(MAX_CHR_OF_LINE,25).

%%% ============================================================================
%%% HTML output

main() -> 
    wf:redirect("/main").

title() -> "RefactorErl Queries".

menu() ->
    FileLabel=#label{text="Filepath or regexp:"},
    FileText=#textbox_autocomplete{id=filetextbox,style="width:190px",tag=mods},
    FileAddButton=#button{id=filebutton,
                          text="Add to list",
                          postback=add_file,
                          class=referl_button},
    FileareaLabel=#label{text="Files to run analysis on:"},
    FileTextarea=#textarea{style="height:80px; width:100%",id=file_list},
    Param1Label=#label{text="Min length:"},
    Param1=#textbox{style="width:30px",id=param1},
    Param2Label=#label{text="Min number of dups:"},
    Param2=#textbox{style="width:30px",id=param2},
    Param3Label=#label{text="Overlap amount:"},
    Param3=#textbox{style="width:30px",id=param3},
    wf:set(param1,"64"),
    wf:set(param2,"2"),
    wf:set(param3,"0"),

    RunDCAnalBtn=#button{id=run_dupcodeanal, 
                text="Run duplicate code\nanalysis", 
                postback=run_dupcodeanal, 
                class=[referl_button,need_db]},
    ResultPanel=#panel{id=dupcode_result},
    

    %only accept a numeric value
    ValidatorFun = fun(ToAtom)->
                   wf:wire(run_dupcodeanal,
                           ToAtom,
                           #validate { validators=[#is_integer{text=
                                       "Please, enter a numeric value!"}]})
                   end,
    [ValidatorFun(Elem) || Elem <- [param1, param2, param3]],

    
    #table{rows=[
        #tablerow{cells=#tablecell{colspan=2,body=FileLabel}},
        #tablerow{cells=#tablecell{colspan=2,body=FileText}},
        #tablerow{cells=#tablecell{colspan=2,body=FileAddButton}},
        #tablerow{cells=#tablecell{colspan=2,body=#br{}}},
        #tablerow{cells=#tablecell{colspan=2,body=FileareaLabel}},
        #tablerow{cells=#tablecell{colspan=2,body=FileTextarea}},
        #tablerow{cells=[#tablecell{body=Param1Label},#tablecell{body=Param1}]},
        #tablerow{cells=[#tablecell{body=Param2Label},#tablecell{body=Param2}]},
        #tablerow{cells=[#tablecell{body=Param3Label},#tablecell{body=Param3}]},
        #tablerow{cells=#tablecell{colspan=2,body=#br{}}},
        #tablerow{cells=#tablecell{colspan=2,body=RunDCAnalBtn}},
        #tablerow{cells=#tablecell{colspan=2,body=#br{}}},
        #tablerow{cells=#tablecell{colspan=2,body=ResultPanel}}
                ]}.

sourcecode_browsers() ->
    ResultList1=#dropdown{id=resultlist1},
    DiffButton=#hidden{id=diff_button},
    ResultList2=#dropdown{id=resultlist2},
    SourceView1=[#tablecell{body=#panel{id=linenums1,body=[],class=["lnums"]}},
                   #tablecell{body=#panel{id=source1}}],
    SourceView2=[#tablecell{body=#panel{id=linenums2,body=[],class=["lnums"]}},
                   #tablecell{body=#panel{id=source2}}],
    Filename1=#tablecell{body=#panel{id=filename1,body=[],class=centered},
                         colspan=2},
    Filename2=#tablecell{body=#panel{id=filename2,body=[],class=centered},
                         colspan=2},
    Panel1=#panel{id=scrollable1,class=["dupcode_column1"],body=SourceView1},
    Panel2=#panel{id=scrollable2,class=["dupcode_column2"],body=SourceView2},
    [#table{class=[fullwidth],rows=[#tablerow{cells=[
                #tablecell{class=[quarterwidth],body=ResultList1},
                #tablecell{class=[quarterwidth,alignright],body=DiffButton},
                #tablecell{class=[quarterwidth],body=ResultList2},
                #tablecell{class=[quarterwidth],body="&nbsp;"}]},
            #tablerow{cells=[Filename1,Filename2]}]},Panel1,Panel2].
    
logout() ->
    nitrogen_lib:logout_link(wf:user()).

%%% ============================================================================
%%% Handlers for postback events    
event(logout) ->
    nitrogen_lib:logout(wf:user());

event({add_to_list,From,To}) ->
    Text=get_text(From),
    List=wf:q(To),
    if
        (Text/="") and ((List==undefined) or (List=="")) -> wf:set(To,Text);
        Text/="" -> wf:set(To,List++"\n"++Text);
        true -> ok
    end;

event(add_file) ->
    event({add_to_list,filetextbox,file_list});

event(run_dupcodeanal) ->
    wf:wire(run_dupcodeanal,#hide{}),
    wf:replace(dupcode_result,#panel{id=dupcode_result,
                                     body=["Running analysis, please wait.."]}),
    FileList=string:tokens(get_text(file_list),"\n"),
    P1=list_to_integer(get_text(param1)),
    P2=list_to_integer(get_text(param2)),
    P3=list_to_integer(get_text(param3)),
    Fun=fun() -> 
                case ?NITRO_SERVICES:search_initial_clones([{files,FileList},
                                                            {minlen,P1},
                                                            {minnum,P2},
                                                            {overlap,P3}]) of
                    {result, Dups} -> Dups;
                    E = {error, _} -> E
                end
    end,
    wf:continue({continue, dupcodeanal}, Fun, 60*60*1000);

event({show_diff,Nodes1,Nodes2}) ->
    wf:wire(#script{script="window.getSelection().removeAllRanges();"}),
    %Diffs=refusr_dupcode:const_var_diff(Nodes1,Nodes2),
    Diffs = case ?NITRO_SERVICES:get_const_var_diff(Nodes1, Nodes2) of
                {result, Result} -> Result;
                {error, _} -> []
            end,
    {CDiffs,VDiffs}=case Diffs of
        [] -> {[],[]};
        {A,B} -> {A,B}
    end,
    NodeDiffs1=[N1 || {N1,_}<-CDiffs]++[N1 || {N1,_}<-VDiffs],
    NodeDiffs2=[N2 || {_,N2}<-CDiffs]++[N2 || {_,N2}<-VDiffs],
    show_nodes(NodeDiffs1,1),
    show_nodes(NodeDiffs2,2);

event({selected_group,Items}) ->
    ItemList=parse_items(Items,1),
    wf:replace(resultlist1,#dropdown{id=resultlist1,
                                     options=ItemList,
                                     html_encode=true,
                                     postback={load_source,1}}),
    wf:replace(resultlist2,#dropdown{id=resultlist2,
                                     options=ItemList,
                                     html_encode=true,
                                     postback={load_source,2}}),
    wf:set(resultlist1,convert_position_to_string(hd(Items))),
    wf:set(resultlist2,convert_position_to_string(hd(tl(Items)))),
    wf:continue({continue, showfirstcodes}, fun() -> ok end, 60*60*1000);

event({load_source,Tab})->
    event({load_source,Tab,wf:q("resultlist"++integer_to_list(Tab))});

event({load_source,Tab,String})->
    L=parse_string(String),
    File=proplists:get_value(filepath,L),
    {SL,SC}=proplists:get_value(startpos,L),
    {EL,EC}=proplists:get_value(endpos,L),
    case Tab of
        1 -> wf:replace(diff_button,
            #button{id=diff_button,
                    text="Show differences",
                    postback={show_diff,
                        proplists:get_value(nodes,L),
                        proplists:get_value(nodes,
                            parse_string(wf:q("resultlist2")))},
                    class=referl_button_small});
        2 -> wf:replace(diff_button,
            #button{id=diff_button,
                    text="Show differences",
                    postback={show_diff,
                        proplists:get_value(nodes,
                            parse_string(wf:q("resultlist1"))),
                        proplists:get_value(nodes,L)},
                    class=referl_button_small})
    end,
    referl_htmlserver:generate(File),
    Fun=fun() -> referl_htmlserver:getdata(File) end,
    wf:replace("linenums"++integer_to_list(Tab),
        #panel{id="linenums"++integer_to_list(Tab),body=[],class=["lnums"]}),
    wf:replace("filename"++integer_to_list(Tab),
        #panel{id="filename"++integer_to_list(Tab),body=File,class=centered}),
    wf:session("filename"++integer_to_list(Tab),File),
    TabName="source"++integer_to_list(Tab),
    wf:replace(TabName,#panel{id=TabName,body="Loading file, please wait..."}),
    wf:wire(TabName,#hide{}),
    wf:wire(TabName,#appear{}),
    wf:continue({continue, generate, Tab, {SL,SC,EL,EC}}, Fun, 60*60*1000).

%%% ============================================================================
%%% Handlers for continous events    

continue({continue, generate, Tab, {SL,SC,EL,EC}}, Text) ->
    TabName="source"++integer_to_list(Tab),
    wf:replace(TabName,#panel{id=TabName,body=Text,class=[monosp]}),
    wf:wire(#script{script="generatelinenums(obj('"++TabName++"'),
        obj('linenums"++integer_to_list(Tab)++"'));"}),
    show_result({SL,SC,EL,EC},Tab);

continue({continue, showfirstcodes}, ok) ->
    event({load_source,1}),
    event({load_source,2});

continue({continue, dupcodeanal}, {error,E}) ->
    wf:wire(run_dupcodeanal,#show{}),
    wf:wire(#notification{type=error, text=E}),
    wf:replace(dupcode_result,#panel{id=dupcode_result,body=[E]});

continue({continue, dupcodeanal}, []) ->
    wf:wire(run_dupcodeanal,#show{}),
    wf:replace(dupcode_result,#panel{id=dupcode_result,body="No result."});

continue({continue, dupcodeanal}, Result) ->
    wf:wire(run_dupcodeanal,#show{}),
    wf:replace(dupcode_result,#panel{id=dupcode_result,body=[["Result:",#br{}]
        ++parse_result(Result,1)]}).

%%% ============================================================================
%%% autocomplete

autocomplete_enter_event(SearchTerm, _Tag) ->
    Mods=?NITRO_SERVICES:get_mods(),
    DataM=lists:map(fun(Elem)-> 
       {struct, [{id, list_to_binary(Elem)}, 
                 {label, list_to_binary(Elem)}, 
                 {value, list_to_binary(Elem)}]} 
        end,Mods),
    List = [{struct,[{id, Id }, {label, Label}, {value, Value}]} || 
            {struct,[{id, Id }, {label, Label}, {value, Value}]} <- DataM, 
            string:str(string:to_lower(binary_to_list(Label)), 
                       string:to_lower(SearchTerm)) > 0],                        
    mochijson2:encode(List).

autocomplete_select_event({struct,[{<<"id">>,_},{<<"value">>,Value}]},_Tag)->
    wf:flash(Value),
    ok.

%%% ============================================================================
%%% Helper functions

%% @doc Highlight given interval in given tab
show_result({StartLn,StartCol,EndLn,EndCol},Tab) ->
    %io:format("~p ~p ~p ~p",[StartLn,StartCol,EndLn,EndCol]),
    wf:wire(#script{script="
        highlight(obj('"++"source"++integer_to_list(Tab)++"'),
        obj('"++"scrollable"++integer_to_list(Tab)++"'),
        "++integer_to_list(StartLn)++","++integer_to_list(StartCol)++",
        "++integer_to_list(EndLn)++","++integer_to_list(EndCol+1)++");"}).

%% @doc Hightlight given nodes in given tab
show_nodes(Nodes,Tab) ->
    Intervals=lists:map(fun(Node) ->
                ?NITRO_SERVICES:get_node_position(Node)
        end,Nodes),

    Starts=[integer_to_list(S) || {S,_}<-Intervals],
    Ends=[integer_to_list(E) || {_,E}<-Intervals],
    
    wf:wire(#script{script="
    try{
        highlight2(obj('"++"source"++integer_to_list(Tab)++"'),
                  obj('"++"scrollable"++integer_to_list(Tab)++"'),"
        ++lists:flatten(io_lib:format("~p",[Starts]))++","
        ++lists:flatten(io_lib:format("~p",[Ends]))++");
    }catch(ex){}"}).

%% @doc Converts string into an erlang term
parse_string(S) ->
    {ok,Scanned,_}=erl_scan:string(S++"."),
    {ok,Parsed}=erl_parse:parse_exprs(Scanned),
    {value,L,_}=erl_eval:exprs(Parsed,[]),
    L.

%% @doc Get string from a html element
get_text(Id) ->
    Result=wf:q(Id),
    if
        Result==undefined -> "";
        true -> Result
    end.

%% @doc Converts list of result groups into a list of links
parse_result([],_) -> [];
parse_result([L|LS],Num) ->
    [#link{ id="dc_group"++integer_to_list(Num),
            body="Duplicate code group "++integer_to_list(Num),
            title="Browse group",
            postback={selected_group,L}},#br{}]
    ++parse_result(LS,Num+1).

%% @doc Converts result list into listbox items
parse_items([],_) -> [];
parse_items([L|LS],Num) ->
    [#option{ text="Code "++integer_to_list(Num), 
              value=convert_position_to_string(L),
              selected=false}]
    ++parse_items(LS,Num+1).

%% @doc Convert an erlang term into string
convert_position_to_string(L) ->
    lists:flatten(io_lib:format("~p", [L])).
