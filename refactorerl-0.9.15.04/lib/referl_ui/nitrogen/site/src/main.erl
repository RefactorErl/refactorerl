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

%%% @author Viktoria Fordos <v@fviktoria.hu>

%% -*- mode: nitrogen -*-

-module (main).
-compile(export_all).
-include_lib("referl_ui/include/nitrogen.hrl").
-include_lib("nitrogen/include/wf.hrl").
-include_lib("kernel/include/file.hrl").
-include("records.hrl").
-vsn("$Rev: 12345 $ ").

-define(MAX_CHR_OF_LINE,40).
-define(Query(Label,Query),#link{class=mylink,
                                text=Label, 
                                postback={predefquery,Query,Node}},#br{}).

%%% ============================================================================
%%% HTML output

main() -> 
    wf:session(selected_source, undefined),
    wf:session(selected_source_hash, -1),
    wf:session(selectednode_data, undefined),
    wf:session(currentjump_nodes, undefined),
    wf:session(currentjump_num, 0),
    wf:session(click_node,undefined),
    wf:session(query_history,[]),
    wf:session(query_history_current,0),
    wf:session(history_jumpnode,0),
    nitrogen_lib:main("main.html").

title() -> "RefactorErl Queries".

other()->
    HideButton=#link{id=tab_hider,
                     class=tab_hider,
                     postback=hide_tabs,
                     body="<", 
                     title = "Click to hide tabs"},
    HideButton2=#link{id=filetab_hider,
                      class=filetab_hider,
                      postback=hide_filetab,
                      body=">", 
                      title = "Click to hide file browser"},
    HiddenData=#hidden{id=hiddendata,text=""},
    HiddenButton=#link{id=hiddenbutton,body="",postback=node_clicked},
    HiddenButton2=#link{id=hiddenbutton_startfrompos,body="",
                       postback=start_from_pos},
    [HideButton,HideButton2,HiddenData,HiddenButton,HiddenButton2].

query_editor()->
    JumpTable=#table{id=jumptable},
    Nav=#tablecell{body=[
                #button{id=history_back, 
                        text="<-", 
                        postback=history_back, 
                        class=[referl_button_small]},#br{},
                #button{id=history_forward, 
                        text="->", 
                        postback=history_forward, 
                        class=[referl_button_small]}]},
    History=#tablecell{body=#panel{id=history_list}},
    HistoryTable=#table{rows=[#tablerow{cells=[History,Nav]}]},
    ContextMenu=#panel{id=contextmenu,body=[],class=contextmenu},
    Table=#table{class=width_100,rows=[#tablerow{cells=[
        #tablecell{body=[ContextMenu]},
        #tablecell{body=[JumpTable]},
        #tablecell{id=curfilename,class=[menuitem,width_100],body=[]},        
        #tablecell{body=[HistoryTable]}
                    ]}]},
    update_history_buttons(),
    replace_curfile(undefined),
    Table.

jumptable() ->
    PrevBtn=#button{id=jump_next_button, 
                text="Prev", 
                postback=jumpprev,
                class=[referl_button]},
    NextBtn=#button{id=jump_next_button, 
                text="Next", 
                postback=jumpnext,
                class=[referl_button]},
    #table{id=jumptable,rows=[
        #tablerow{cells=[
            #tablecell{body=[PrevBtn]},
            #tablecell{body=[NextBtn]}
                        ]},
        #tablerow{cells=[
            #tablecell{id=jumpcounter,body=["..."],
                       colspan=2,class=[menuitem]}
                        ]}]}.

replace_curfile(undefined) ->
    wf:replace(curfilename,
        #tablecell{id=curfilename,class=[menuitem,width_100],
                   body=["Current file: No file shown."]});

replace_curfile(FileName) ->
    wf:replace(curfilename,
        #tablecell{id=curfilename,class=[menuitem,width_100],
                   body=["Current file: "++FileName]}).

query_result()->
    #panel{id=query_result_placeholder,body=[]}.

query_result_source_code()->
    wf:session(selected_source, undefined),
    wf:session(last_source, undefined),
    wf:session(selected_source_hash, -1),
    [#panel{id=linenums,body=[],class=["lnums"]},
     #panel{id=query_result_source_code_placeholder,body=[]}].

executed_query()->
    #panel{id=executed_query_placeholder,body=[]}.

warnings()->
    Warning= ?NITRO_SERVICES:get_possible_warning(),
    case Warning of
        no_warning -> "";
        _ ->#panel{id=warning_messages_placeholder,body=Warning}
    end.

queries()->
    wf:comet(fun() -> background_update() end),
    SaveSkelBtn=#button{id=save_skeleton_button, 
                text="Save as skeleton", 
                postback=save_skeleton_req, 
                class=[referl_button]},
    RunNewQBtn=#button{id=new_query_button, 
                text="Run", 
                postback=run_new_query_node, 
                class=[referl_button]},
    HideBtn=#link{id=hide_query_button, 
                text="^", title="Hide query box",
                postback=hide_querybox,
                class=mylink},
    BtnTable=#table{rows=[#tablerow{cells=[#tablecell{body=[RunNewQBtn]},
                                           #tablecell{body=[SaveSkelBtn]}]}]},
    TextBox=#textarea_autocomplete{ id=query_box, 
                                    tag="", 
                                    text="",
                                    style="max-height:50px; 
                                           min-height:50px; height:50px",
                                    minLength=2, 
                                    delay=300, 
                                    html_encode=true, 
                                    class=[width_300],
                                    next=new_query_button},
    wf:wire(new_query_button, 
            query_box, 
            #validate { validators=[#is_required { text="Required." }]}),
    wf:wire(save_skeleton_button, 
            query_box, 
            #validate { validators=[#is_required { text="Required." }]}),
    
    Form=#panel{id=placeholder,
                 body=[HideBtn,"<br/>",
                        BtnTable,
                        TextBox,
                        "<br/>",
                        #panel{id=skeleton_message_panel_placeholder, body=""},
                        #dropdown{id=query_owner,
                                  options=[
                                            #option{ text="My queries", 
                                                     value="my", 
                                                     selected=true },
                                            #option{ text="All queries", 
                                                     value="all", 
                                                     selected=false },
                                            #option{ text="Skeletons", 
                                                     value="skel", 
                                                     selected=false }
                                          ],
                                  html_encode=true,
                                  postback=load_list
                                 },
                         #panel{id=running_queries_placeholder,body=[]},
                         #panel{id=queries_list_panel,
                                body=[
                                      #table{id=query_list,
                                             class="query_list_table", 
                                             rows=[]}
                                      ]
                                }
                         ]},
    event(load_list),
    Form.
    
logout()->
    nitrogen_lib:logout_link(wf:user()).

get_db_root_dir()->
    TL= ?NITRO_CORE:get_file_browser_loaded_files_root(),
    if 
        length(TL) == 1 -> TL;
        true -> "__multi_dirs__"
    end.

browser_type_select()->
    #table{rows=[#tablerow{cells=[
        #tablecell{body=["Filter: ",
            #textbox{id=file_filter,text="",class="file_filter"}]},
        #tablecell{body=[#link{id=show_html_button,
                               body=" ",
                               postback=show_file_links}]}
                                 ]}]}.

file_browser()->    
    [#panel{id=file_browser_placeholder,
           body=[#panel{id=file_browser_db_panel,
                        body="<div id='file_browser_db'></div>"}]}].

skeleton_message_panel()->
    [#panel{id=skeleton_message_panel_placeholder, body=""}].

%%% ============================================================================
%%% autocomplete

autocomplete_enter_event(SearchTerm, _Tag) ->
    PosQueries= ?NITRO_SERVICES:do_autocomplete(SearchTerm),
    Data=lists:map(fun({Label,Value})-> 
                          {struct, [{id, list_to_binary(Value)}, 
                                    {label, list_to_binary(Label)} , 
                                    {value, list_to_binary(Value) }]} 
                   end,PosQueries),
    mochijson2:encode(Data).

autocomplete_select_event({struct,[{<<"id">>,_},{<<"value">>, Value}]},_Tag) ->
    wf:flash(Value),
    ok.

%%% ============================================================================
%%% Node query functions

%% @doc Execute given query from given node
execute_query(Query,{PNode,Node},Type) ->
    Fun=check_query_start(Query,{PNode,Node},Type),
    wf:replace(executed_query_placeholder,
               #panel{id=executed_query_placeholder,
                      body=[#p{body=?NITRO_CORE:whitespaces_to_html(
                    Query),
                               class="query_title"},
                            #br{},
                            #p{body="Running query, please wait.."},
                            #br{}]}),
    wf:replace(query_result_placeholder, query_result()),
    wf:replace(jumptable,#table{id=jumptable}),
    cleanup(),
    show_last_result_tab(),
    wf:continue({continue, nodequery, {Query,{PNode,Node,Type/=none},Type}}, Fun, 60*60*1000).

%% @doc Checks if query is valid from given node
check_query_start(Query,{PNode,Node},Type) ->
    ValidStarts=case Type of
        macrodef -> ["@expr","@expression","@macro"];
        mod -> ["@expr","@expression","@file","@module","@mod"];
        recdef -> ["@expr","@expression","@record","@rec"];
        recfielddef -> ["@expr","@expression","@recfield","@field"];
        var -> ["@expr","@expression","@variable","@var"];
        funappl -> ["@expr","@expression","@function","@fun"];
        fundef -> ["@expr","@expression","@function","@fun", "@clause"];
        recexpr -> ["@expr","@expression","@record","@rec"];
        recfield -> ["@expr","@expression","@recfield","@field"];
        atom -> ["@expr","@expression"];
        _ -> []
    end,
    QueryStart=case string:tokens(Query,".") of
        [Head|_] -> Head;
        [] -> ""
    end,
    StartIsValid=lists:any(fun(X) -> QueryStart==X end, ValidStarts),
    User=wf:user(),
    if
        (Type==none) or (StartIsValid)->
        try
            fun() -> ?NITRO_SERVICES:execute_query({unknown,{Query, nodequery, 
                {PNode,Node,Type/=none}},User}) end
        catch
            request_denied -> 
                fun() -> {error,"Request to database is denied."} end
        end;
        true -> 
                fun() -> {error,"'"++QueryStart
                            ++"' is an illegal selector for this node." } 
                end
    end.

%%% ============================================================================
%%% Visualisation functions

%% @doc Select or highlight given node    
show_result(Node) ->
    {_,NodeT,NodeId}=Node,
    NodeTypeText=atom_to_list(NodeT),
    NodeIdText=integer_to_list(NodeId),
    cleanup(),
    wf:wire(#script{script="
    try{
        unhighlight(document.getElementById('mainwrapper'));
        elem=document.getElementsByName('jump_"
            ++NodeTypeText++NodeIdText++"')[0];
        if(elem!=null)
        {
            showcontext=false;
            scrolltonode(elem);
            if(elem.tagName=='A')
            {
                $(elem).click();
            }
            else
            {
                $(elem).addClass('selectednode');
                lastcolored=elem;
                selectednode=elem;
                cd.value='"++NodeTypeText++"|"++NodeIdText++"';
                $(cb).click();
            }
        }
    }catch(ex){}"}),
    %If node has no link attached, try to highlight it
    wf:continue({continue,check_if_node_exists}, fun() -> Node end, 60*60*1000).

%% @doc Show file and select given node
jump_to_node(Node) ->
    wf:replace(jumptable,jumptable()),
    wf:replace(jumpcounter,
        #tablecell{id=jumpcounter,body=[
                        integer_to_list(wf:session(currentjump_cur))
                        ++" of "
                        ++integer_to_list(wf:session(currentjump_num))],
                   colspan=2,class=[menuitem]}),
    Curfile=wf:session(last_source),
    Filepath=?NITRO_SERVICES:get_file(Node),
    if
    Filepath/="" ->
        if
            Curfile/=Filepath ->
                wf:wire(#script{script="lastcolored=null;"}),
                wf:session(click_node,Node),
                event({show_selection_links,Filepath});
            true ->
                wf:session(node_exists,undefined),
                show_result(Node)
        end,
        wf:session(selected_source, Filepath),
        wf:session(selected_source_hash,?NITRO_SERVICES:get_filehash(Filepath)),
        wf:session(selectednode_data,undefined),
        replace_curfile(Filepath);
    true -> wf:wire(#notification{type=error,text="Cannot find node."})
    end.

%% @doc Select current query's result at given index
select_result(Num) ->
    NumString=integer_to_list(Num),
    wf:replace(jumpcounter,
        #tablecell{id=jumpcounter,body=[
                        NumString++" of "
                        ++integer_to_list(wf:session(currentjump_num))],
                   colspan=2,
                   class=[menuitem]}),
    wf:session(currentjump_cur,Num),
    wf:wire(#script{script="
        try{
            if(lastcoloredresult!=null) 
                $(lastcoloredresult).removeClass('selected_queryresult')
            $(obj('query_result"++
                NumString++"')).addClass('selected_queryresult');
            lastcoloredresult=obj('query_result"++NumString++"');
        }catch(ex){}"}).      

%% @doc Clean things associated with a selected node
cleanup() ->
    wf:wire(#script{script="
        try{
            if(lastcolored!=null) 
            {
                $(lastcolored).removeClass('selectednode');
                var children=lastcolored.childNodes;
                for(var i=0;i<children.length;i++) 
                    $(children[i]).removeClass('selectednode');
            }
            lastcolored=null;
            $(obj('contextmenu')).css('display','none');
        }catch(ex){}"}),
    wf:session(selectednode_data, undefined).

%% @doc Enable/disable history buttons according to their position
update_history_buttons() ->
    Options=parse_history(wf:session(query_history),1,[]),
    wf:replace(history_list,#dropdown{id=history_list,
                                      options=Options,style="max-width: 300px",
                                      postback=load_history}),
    Hc=wf:session(query_history_current),
    Hl=length(wf:session(query_history)),
    if
        Hc=<1 ->
            wf:replace(history_back,#button{id=history_back, 
                text="<-",
                class=[referl_button_disabled]});
        true ->
            wf:replace(history_back,#button{id=history_back, 
                text="<-",
                postback=history_back, 
                class=[referl_button_small]})
    end,
    if
        Hc==Hl ->
            wf:replace(history_forward,#button{id=history_forward, 
                text="->", 
                class=[referl_button_disabled]});
        true ->
            wf:replace(history_forward,#button{id=history_forward, 
                text="->", 
                postback=history_forward, 
                class=[referl_button_small]})
    end.

%% @doc Convert history elements into listbox options
parse_history([],Num,Sum) when Num==1 -> 
    [#option{text="Query History",value=0,selected=true}|Sum];
parse_history([],_,Sum) -> 
    Sum;
parse_history([{Q,N}|EL],Num,Sum) -> 
    parse_history(EL,Num+1,
    case N of 
    {_,{_,{_,_,_}=Node,_}} ->
            [#option{text=Q++" FROM "
                   ++ ?NITRO_SERVICES:node_to_text(Node),
            value=Num,
            selected=wf:session(query_history_current)==Num}|Sum];
    {undefined,_} -> [#option{text=Q,
            value=Num,
            selected=wf:session(query_history_current)==Num}|Sum];
    {_,undefined} -> [#option{text=Q,
            value=Num,
            selected=wf:session(query_history_current)==Num}|Sum];
    {F,P} -> [#option{text=Q++" FROM "
                   ++ filename:basename(F) ++ " Pos:"++integer_to_list(P),
            value=Num,
            selected=wf:session(query_history_current)==Num}|Sum]
    end).

%%% ============================================================================
%%% Handlers for postback events    

event(close_context) ->
    wf:wire(#script{script="$(obj('contextmenu')).css('display','none');"});

event(hide_tabs) ->
    wf:wire(#script{script="
        $('#tabs').hide();
        resizerow(true);"}),
    wf:replace(tab_hider,
               #link{id=tab_shower,
                     class=tab_hider,
                     postback=show_tabs,
                     body=">",
                     title = "Click to show tabs"});
    
event(show_tabs) ->
    wf:wire(#script{script="
        $('#tabs').show();
        resizerow(true);"}),
    wf:replace(tab_shower,
               #link{id=tab_hider,
                     class=tab_hider,
                     postback=hide_tabs,
                     body="<",
                     title = "Click to hide tabs"});
    
event(hide_filetab) ->
    wf:wire(#script{script="
        $('#filetab').hide();
        resizerow(false);"}),
    wf:replace(filetab_hider,
               #link{id=filetab_shower,
                     class=filetab_hider,
                     postback=show_filetab,
                     body="<",
                     title = "Click to show file browser"});
    
event(show_filetab) ->
    wf:wire(#script{script="
        $('#filetab').show();
        resizerow(false);"}),
    wf:replace(filetab_shower,
               #link{id=filetab_hider,
                     class=filetab_hider,
                     postback=hide_filetab,
                     body=">",
                     title = "Click to hide file browser"});

event(hide_querybox) ->
    wf:replace(hide_query_button,#link{id=show_query_button, 
                text="v", title="Show query box",
                postback=show_querybox,
                class=mylink}),
    wf:wire(save_skeleton_button,#hide{}),
    wf:wire(new_query_button,#hide{}),
    wf:wire(query_box,#hide{});
    
event(show_querybox) ->
    wf:replace(show_query_button,#link{id=hide_query_button, 
                text="^", title="Hide query box",
                postback=hide_querybox,
                class=mylink}),
    wf:wire(save_skeleton_button,#show{}),
    wf:wire(new_query_button,#show{}),
    wf:wire(query_box,#show{});

event(history_back) -> 
    wf:session(query_history_current,wf:session(query_history_current)-1),
    Ok=wf:session(query_history_current)>0,
    if
        Ok -> event(load_current_history);
        true -> wf:session(query_history_current,1)
    end;

event(history_forward) ->
    wf:session(query_history_current,wf:session(query_history_current)+1),
    Ok=length(wf:session(query_history))>=wf:session(query_history_current),
    if
        Ok -> event(load_current_history);
        true -> 
            wf:session(query_history_current,length(wf:session(query_history)))
    end;

event(load_history) ->
    Num=hd(wf:q(history_list)),
    wf:session(query_history_current,Num),
    event(load_current_history);
    
event(load_current_history) ->
    update_history_buttons(),
    wf:wire(#script{script="
        obj('history_back').disabled=true;
        obj('history_forward').disabled=true;
        obj('history_list').disabled=true;"}),
    wf:session(history_jumpnode,wf:session(history_jumpnode)+1),
    case lists:nth(wf:session(query_history_current),
                   wf:session(query_history)) of
    {Query,{nodequery,{_,Node,_}}} ->
        event({predefquery,Query,Node});
    {Q,{F,P}} ->
        wf:session(selected_source,F),
        wf:session(selected_source_hash, ?NITRO_SERVICES:get_filehash(F)),
        wf:session(selected_position,P),
        cleanup(),
        event({run_new_query,Q})
    end;

event({query_jump_to_node,Node,Num})->
    select_result(Num),
    jump_to_node(Node);

event(jumpprev)->
    Nextnum=wf:session(currentjump_cur)-1,
    Maxnum=wf:session(currentjump_num),
    if
        Maxnum==0 -> Jumpnum=1;
        Nextnum=<0 -> Jumpnum=Maxnum;
        true -> Jumpnum=Nextnum
    end,
    wf:session(currentjump_cur,Jumpnum),
    wf:wire(#script{script="
        try{
            $(obj('query_result"++integer_to_list(Jumpnum)++"')).click();
        }catch(ex){}"});

event(jumpnext)->
    Nextnum=wf:session(currentjump_cur)+1,
    Maxnum=wf:session(currentjump_num),
    if
        Nextnum>Maxnum -> Jumpnum=1;
        true -> Jumpnum=Nextnum
    end,
    wf:session(currentjump_cur,Jumpnum),
    wf:wire(#script{script="
        try{
            $(obj('query_result"++integer_to_list(Jumpnum)++"')).click();
        }catch(ex){}"});

event(start_from_pos)->
    case wf:session(disablethings) of
    undefined ->
        cleanup(),
        TextBox=#textarea_autocomplete{ id=context_query_box,
                                        tag="", 
                                        text="",
                                        style="height:18px;",
                                        minLength=2, 
                                        delay=300, 
                                        html_encode=true},
        Button=#button{id=pos_query_run,
                       text="Run",
                       postback=startfrom_context,
                       class=referl_button},
        Table1=#table{rows=[#tablerow{
                   cells=[#tablecell{body=#link{text="X",
                       postback=close_context,
                       class=pad_right}},
        #tablecell{body="<strong>Start query from this position</strong>"}]}]},
        Table2=#table{rows=[#tablerow{
                   cells=[#tablecell{body=TextBox},#tablecell{body=Button}]}]},
        wf:replace(contextmenu,
             #panel{id=contextmenu,
                body=[Table1,Table2],
                class=contextmenu}),
        wf:wire(pos_query_run, 
            context_query_box, 
            #validate { validators=[#is_required { text="Required." }]}),
        [X,Y]=string:tokens(wf:q(hiddendata), "|"),
        wf:wire(#script{script="$(obj('contextmenu')).css('left',"++X++");
        $(obj('contextmenu')).css('top',"++Y++");
        $(obj('contextmenu')).css('display','inline');"});
    _ -> ok
    end;
            
event(startfrom_context)->
    Query=wf:q(context_query_box),
    event({run_new_query,Query});

event({predefquery,"@file.references",Node})->
    Mod=filename:basename(?NITRO_SERVICES:get_file(Node),".erl"),
    event({docustomquery,Node,"mods.funs.body.esub[.function[mod="++Mod++"]]",none});

event({predefquery,Query,Node})->
    event({docustomquery,Node,Query,none});

event({docustomquery,Node})->
    Query=wf:q(customqueries),
    Type=?NITRO_SERVICES:get_type(Node),
    event({docustomquery,Node,Query,Type});

event({docustomquery_context,Node})->
    Query=wf:q(context_query_box),
    Type=?NITRO_SERVICES:get_type(Node),
    event({docustomquery,Node,Query,Type});

event({docustomquery,Node,Q,Type})->
    Query=?NITRO_SERVICES:convert_query_if_skel(Q),
    NotExprQuery=lists:sublist(Query,5)/="@expr",
    if
        (Query/=[]) andalso (hd(Query)/=$@) -> 
            event({run_new_query,Query});
        Query=="" ->
            ok;
        NotExprQuery ->
            PNode=?NITRO_SERVICES:get_proper_node(Node),
            case PNode of
                error ->  wf:wire(#script{script="
                            obj('history_back').disabled=false;
                            obj('history_forward').disabled=false;
                            obj('history_list').disabled=false;"}),
                          wf:wire(#notification{type=error, 
                                      text="Node does not exist anymore."});
                deny -> wf:wire(#script{script="
                            obj('history_back').disabled=false;
                            obj('history_forward').disabled=false;
                            obj('history_list').disabled=false;"}),
                        wf:wire(#notification{type=error, 
                                      text="Access to database was denied."});
                _ -> execute_query(Query,{PNode,Node},Type)
            end;
        true -> 
            execute_query(Query,{Node,Node},Type)
    end;

event(node_clicked)->
    wf:wire(#script{script="window.getSelection().removeAllRanges();"}),
    wf:session(node_exists,true),
    [D1,D2]=string:tokens(wf:q(hiddendata), "|"),
    TT=list_to_existing_atom(D1),
    Id=list_to_integer(D2),
    Node={'$gn',TT,Id},
    Type=?NITRO_SERVICES:get_type(Node),
    CurrentSel=wf:session(selectednode_data),
    if
    CurrentSel==Node -> 
        cleanup();
    true ->
        wf:wire(#script{script="
            try{
                elem=selectednode;
                if(lastcolored!=null)
                {
                    $(lastcolored).removeClass('selectednode');
                    var children=lastcolored.childNodes;
                    for(var i=0;i<children.length;i++) 
                        $(children[i]).removeClass('selectednode');
                }
                $(elem).addClass('selectednode');
                var children=elem.childNodes;
                for(var i=0;i<children.length;i++) 
                    $(children[i]).addClass('selectednode');
                lastcolored=elem;
            }catch(ex){}"}),
        Rows=case wf:session(disablethings) of
            undefined -> get_predef_queries(Node,Type,queries);
            _ -> []
        end,
        wf:replace(contextmenu,
            #panel{id=contextmenu,body=Rows,class=contextmenu}),
        wf:wire(context_run, 
            context_query_box, 
            #validate { validators=[#is_required { text="Required." }]}),
        wf:wire(#script{script="positionContextMenu(selectednode);"}),
        wf:session(selectednode_data,Node)
    end;

event({use_skeleton,{Name, _Body, _Owner, _Card}})->
    Call=?NITRO_SERVICES:skeleton_call_format(Name), %% SKEL
    show_queries_tab(),
    wf:set(query_box, Call);

event({delete_skeleton,{Name, _Body, _Owner, _Card}})->
    ?NITRO_SERVICES:delete_skeleton(Name), %% SKEL
    event(load_list);

event({edit_skeleton, Skel={_Name, Body, _Owner, _Card}})->
    UpdateSkelBtn=#button{id=save_skeleton_button, 
            text="Update skeleton", 
            postback={update_skeleton, Skel}, 
            class=[referl_button]},
    wf:set(query_box, Body),
    wf:replace(save_skeleton_button, UpdateSkelBtn),
    wf:wire(save_skeleton_button, 
            query_box, 
            #validate { validators=[#is_required { text="Required." }]});

event({update_skeleton, {Name, _Body, _Owner, _Card}})->
    NewBody=wf:q(query_box),
    SaveSkelBtn=#button{id=save_skeleton_button, 
            text="Save as skeleton", 
            postback=save_skeleton_req, 
            class=[referl_button]},
    wf:replace(save_skeleton_button, SaveSkelBtn),
    wf:wire(save_skeleton_button, 
                    query_box, 
                    #validate{validators=[#is_required { text="Required." }]}),
    message_box_hide(skeleton_message_panel_placeholder,
                              fun()->skeleton_message_panel() end),
    case ?NITRO_SERVICES:update_skeleton(Name, NewBody, %% SKEL
            wf:user()) of
        ok ->
            event(load_list);
        {error, Msg}->
            Content=#panel{body="Error: "++Msg, class=["centered", "error"]},
            wf:wire(#notification{type=error,text="Error: "++Msg}),
            message_box(Content, skeleton_message_panel_placeholder, 
                fun()->ok end)
    end;

event(save_skeleton)->
    SkelName=wf:q(skel_name),
    SkelBody=wf:q(query_box),
    case ?NITRO_SERVICES:save_skeleton(SkelName, SkelBody, %% SKEL
            wf:user()) of
        ok ->
            event(load_list),
            message_box_hide(skeleton_message_panel_placeholder,
                fun()->skeleton_message_panel() end);
        {error, Msg}->
            Content=#panel{body="Error: "++Msg, class=["centered", "error"]},
            wf:wire(#notification{type=error,text="Error: "++Msg}),
            message_box(Content, skeleton_message_panel_placeholder, 
                fun()->ok end)
    end;

event(save_skeleton_req)->
    Content=#panel{body=["Save skeleton as:", 
                         #textbox{id=skel_name, next=new_skel_button}, 
                         #button{id=new_skel_button, 
                                 text="Save", 
                                 postback=save_skeleton, 
                                 class=[referl_button]}], 
                   class="centered"},
    message_box(Content, skeleton_message_panel_placeholder, 
        fun()->ok end),
    wf:wire(new_skel_button, 
            skel_name, 
            #validate { validators=[#is_required { text="Required." }]});
    
event(logout) ->
    nitrogen_lib:logout(wf:user());

event({run_new_query,Q})->
    wf:replace(jumptable,#table{id=jumptable}),
    cleanup(),
    %selected file as starting point
    F=wf:session(selected_source),
    %selected position as starting point
    P=wf:session(selected_position),
    Pos=case {is_integer(P), is_list(P), is_atom(P)} of
        {true, false, false} -> P;
        {false, true, false} -> list_to_integer(P);
        {false, false, true} when P/=undefined->
            list_to_integer(atom_to_list(P))+1;
        {false, false, true} ->undefined
    end,             
    wf:replace(executed_query_placeholder,
               #panel{id=executed_query_placeholder,
                      body=[#p{body=?NITRO_CORE:whitespaces_to_html(
                    Q),
                               class="query_title"},
                            #br{},
                            #p{body="Running query, please wait.."},
                            #br{}]}),
    wf:replace(query_result_placeholder, query_result()),
    {File, Position}=case string:chr(Q,$@) of
                         0 -> {undefined, undefined};
                         _ when (F/=undefined) and (Pos/=undefined) -> {F,Pos};
                         _ -> {undefined, undefined}
                     end,
    show_last_result_tab(),
    User=wf:user(),
    Fun = fun() -> 
              ?NITRO_SERVICES:execute_query({unknown,{Q, File, Position},User}) 
          end,
    wf:continue({continue, posquery, {Q, File, Position}}, Fun, 60*60*1000);

event({run_query,SafeQuery,Q})-> 
    wf:replace(jumptable,#table{id=jumptable}),
    cleanup(),
    wf:set(query_box, Q),
    wf:replace(query_result_source_code_placeholder,
               query_result_source_code()),
    wf:replace(executed_query_placeholder,
               #panel{id=executed_query_placeholder,
                      body=[#p{body=?NITRO_CORE:whitespaces_to_html(
                    Q),
                               class="query_title"},
                            #br{},
                            #p{body="Running query, please wait.."},
                            #br{}]}),
    wf:replace(query_result_placeholder, 
               #panel{id=query_result_placeholder,body=[]}),
    show_last_result_tab(),
    User=wf:user(),
    Fun = fun() -> 
              ?NITRO_SERVICES:execute_query({prev_query,SafeQuery,User}) 
          end,
    {_, File, Pos} = SafeQuery,
    wf:continue({continue, posquery, {Q, File, Pos}}, Fun, 60*60*1000);

event({run_query_node,SafeQuery,Q})->
    wf:replace(jumptable,#table{id=jumptable}),
    cleanup(),
    wf:set(query_box, Q),
    wf:replace(linenums,#panel{id=linenums}),
    wf:replace(query_result_source_code_placeholder,
               query_result_source_code()),
    wf:replace(executed_query_placeholder,
               #panel{id=executed_query_placeholder,
                      body=[#p{body=?NITRO_CORE:whitespaces_to_html(
                    Q),
                               class="query_title"},
                            #br{},
                            #p{body="Running query, please wait.."},
                            #br{}]}),
    wf:replace(query_result_placeholder, 
               #panel{id=query_result_placeholder,body=[]}),
    show_last_result_tab(),
    User=wf:user(),
    Fun = fun() -> 
              ?NITRO_SERVICES:execute_query({prev_query,SafeQuery,User}) 
          end,
    {_, File, Pos} = SafeQuery,
    wf:continue({continue, posquery, {Q, File, Pos}}, Fun, 60*60*1000);

event({delete_query,SafeQuery})->
    case wf:role(admin) andalso wf:q(query_owner)=="all" of
        true -> ?NITRO_SERVICES:delete_from_qtab(SafeQuery,admin);
        false ->
            ?NITRO_SERVICES:delete_from_qtab(SafeQuery,
                wf:user())
    end,
    event(load_list);

event({abort_query,QueryId})->
    Message=case ?NITRO_SERVICES:kill_query(QueryId) of
        ok -> #p{body="Query is aborted successfully."};
        not_found -> #p{body="Query isn't found, sorry."}
    end,
    wf:replace(running_queries_placeholder,
               #panel{id=running_queries_placeholder,
                      body=[Message]});

event(run_new_query_node)->
    RunFromNode = (wf:session(selectednode_data)/=undefined),
    Q=wf:q(query_box),
    if
        RunFromNode ->
            Node=wf:session(selectednode_data), 
            Type=?NITRO_SERVICES:get_type(Node),
            event({docustomquery,Node,Q,Type});
        true ->
            cleanup(),
            event({run_new_query,Q})
    end;

event({show_selection,File, SP, StartPos, EndPos})->
    event({show_selection,File, SP, StartPos, EndPos, none});
event({show_selection,File, SP, Start, End, Num})->
    case Num of
        none -> ok;
        _ -> select_result(Num)
    end,
    wf:session(selected_file, File),
    wf:session(selected_position, SP),
    wf:session(selected_startpos, Start),
    wf:session(selected_endpos, End),
    event(show_file_links);

event(show_file_links)->
    File=wf:session(selected_file),
    case File of
        undefined -> ok;
        _ ->
            case lists:suffix("/", File) of
                true ->
                    ok;
                false ->
                    replace_curfile(File),
                    cleanup(),
                    event({show_selection_links,File})
            end
    end;

event({show_selection_links,File})->
    LastFile=wf:session(last_source),
    wf:session(last_source, File),
    wf:session(selected_source, File),
    wf:session(selected_source_hash, ?NITRO_SERVICES:get_filehash(File)),
    if
        File==LastFile -> 
            Fun = fun() -> same end;
        true ->
            referl_htmlserver:generate(File),
            Fun=fun() -> referl_htmlserver:getdata(File) end,
            wf:replace(linenums,#panel{id=linenums,body=[],class=["lnums"]}),
            wf:replace(query_result_source_code_placeholder,
                      #panel{id=query_result_source_code_placeholder,
                             body="Loading file, please wait..."}),
            wf:wire(query_result_source_code_placeholder,#hide{}),
            wf:wire(query_result_source_code_placeholder,#appear{})
    end,
    wf:continue({continue, generate}, Fun, 60*60*1000);

event(load_list)->
    Who=wf:q(query_owner),
    wf:session(group_helper,0),
    {List,LoadListFun}=case Who of
        undefined->
            {?NITRO_SERVICES:query_list_from_qtab(wf:user()),
             load_list_function()};
        "my"->
            {?NITRO_SERVICES:query_list_from_qtab(wf:user()),
             load_list_function()};
        "all"->
            {?NITRO_SERVICES:query_list_from_qtab("all"),
            load_list_function()};
        "skel"->
            {?NITRO_SERVICES:list_skeletons(), %% SKEL
            skeletons_table_fun()}
    end,
    Rows=case lists:foldl(LoadListFun, [], List) of
            [] -> 
            EmptyListCell=#tablecell{body=[]},
            [#tablerow{cells=[EmptyListCell]}];
            R -> R
         end,
    wf:replace(query_list,#table{id=query_list,rows=Rows}),
    wf:wire(a,#hide{});

event({show_comment_editor, SelfId, RowId})->
    wf:wire(RowId, #appear{speed=50}),
    Cell=#tablecell{id=SelfId,
                    body=[#link {text="C",
                                class="delete_query_a", 
                                title = "Edit comments of the query", 
                                postback={hide_comment_editor,SelfId,RowId}}],
                    align="left",
                    valign="middle",
                    class=["query_list_tablecell","width_15", "white_border"]},
    wf:replace(SelfId, Cell);

event({hide_comment_editor, SelfId, RowId})->
    wf:wire(RowId, #fade{speed=50}),
    Cell=#tablecell{id=SelfId,
                    body=[#link {text="C",
                                class="delete_query_a", 
                                title = "Edit comments of the query", 
                                postback={show_comment_editor,SelfId,RowId}}],
                    align="left",
                    valign="middle",
                    class=["query_list_tablecell","width_15", "white_border"]},
    wf:replace(SelfId, Cell);

event({save_comment, {sem_query, SafeQuery}, CommentTextBoxId})->
    NewComment=case wf:q(CommentTextBoxId) of
        undefined -> "";
        R -> R
    end,
    ?NITRO_SERVICES:update_prev_query_comment(SafeQuery,
        NewComment),
    event(load_list);

event({save_comment, {skeleton, SkelName}, CommentTextBoxId})->
    NewComment=case wf:q(CommentTextBoxId) of
        undefined -> "";
        R -> R
    end,
    ?NITRO_SERVICES:update_prev_skeleton_comment(SkelName, %% SKEL
        NewComment),
    event(load_list);

event({show_help,TempIdLink,TempIdCell,TempIdRow})->
    HelpLink=#link {id=TempIdLink,
                    text="?",
                    class="delete_query_a", 
                    title = "Hide information", 
                    postback={hide_help,
                              TempIdLink, 
                              TempIdCell,
                              TempIdRow}},
    wf:replace(TempIdLink,HelpLink),
    wf:wire(TempIdRow, #appear{speed=50});

event({hide_help,TempIdLink,TempIdCell,TempIdRow})->
    HelpLink=#link {id=TempIdLink,
                    text="?",
                    class="delete_query_a", 
                    title = "Show information", 
                    postback={show_help,
                              TempIdLink, 
                              TempIdCell,
                              TempIdRow}},
    wf:replace(TempIdLink,HelpLink),
    wf:wire(TempIdRow, #fade{speed=50});

event({show_errors,Message})->
    HideErrorsLink=#link{id=hide_errors_link, 
                         text=Message, 
                         postback={hide_errors,Message}, 
                         class="error_link"},
    wf:replace(show_errors_link,HideErrorsLink),
    wf:wire(warning_table, #appear{speed=100});

event({hide_errors,Message})->
    ShowErrorsLink=#link{id=show_errors_link, 
                         text=Message, 
                         postback={show_errors,Message}, 
                         class="error_link"},
    wf:replace(hide_errors_link,ShowErrorsLink),
    wf:wire(warning_table, #fade{speed=100});

event({alias_query, Alias, SafeQuery, FstCellId, FstCell,FourthCellId})->
    TBId=wf:temp_id(),
    TextB=#textbox { id=TBId, text=Alias},
    SaveB=#button{  text="Save", 
                    postback={save_alias, SafeQuery, TBId}, 
                    class=referl_button },
    Panel=#panel{body=[TextB, SaveB]},
    Cell=#tablecell{id=FstCellId,
                    body=[Panel],
                    align="left",
                    valign="middle",
                    class=["query_list_tablecell", "white_border"]},
    wf:replace(FstCellId, Cell),
    FourthCell=#tablecell{id=FourthCellId,
                          body=[#link {text="E",
                                    class="delete_query_a", 
                                    title = "Cancel", 
                                    postback={cancel_alias, 
                                              Alias, 
                                              SafeQuery, 
                                              FstCellId, 
                                              FstCell,
                                              FourthCellId}}],
                          align="left",
                          valign="middle",
                          class=["query_list_tablecell","width_15",
                                 "white_border"]},
    wf:replace(FourthCellId, FourthCell);

event({cancel_alias, Alias, SafeQuery, FstCellId, FstCell,FourthCellId})->
    FourthCell=#tablecell{id=FourthCellId,
                          body=[#link {text="E",
                                    class="delete_query_a", 
                                    title = "Assign a name to the query", 
                                    postback={alias_query, 
                                              Alias, 
                                              SafeQuery, 
                                              FstCellId, 
                                              FstCell,
                                              FourthCellId}}],
                          align="left",
                          valign="middle",
                          class=["query_list_tablecell","width_15" , 
                                 "white_border"]},
    wf:replace(FourthCellId, FourthCell),
    wf:replace(FstCellId,FstCell);

event({save_alias, SafeQuery, TBId})->
    case wf:q(TBId) of
        undefined -> 
            wf:wire(#alert{text="Please, type the name of the query 
                                 into the textbox!"});
        Name when length(Name)>0->
            case ?NITRO_SERVICES:set_alias(SafeQuery, Name) of
                                       ok -> event(load_list);
                                       {error, E} -> wf:wire(#alert{text=E})
                                   end;
        _ -> 
            wf:wire(#notification{type=warning,
                text="Please, type the name of the query into the textbox!"})
    end.

%%% ============================================================================
%%% Handlers for continous events    

continue({continue, jump}, {error,Query,Error}) ->
    wf:replace(jumptable,#table{id=jumptable}),
    query_result_handler({Query,0,0},{{result, no_result},
                         {warning,no_warning},{error,Error}});
continue({continue, jump}, {FilteredNodes=[C|_],Query,Type,StartNode}) 
    when is_integer(C)-> %Metric result
    Result=[{group_by,{nopos,"Result"},list,
        [{result,FilteredNodes}]}],
    query_result_handler({Query,0,0},{{result, Result},
                         {warning,no_warning},{error,no_error}}),    
    if
        Type/=none ->
            referl_htmlserver:add_query(Type,Query),
            Hash = ?NITRO_HELPER:get_database_hash(),
            ?NITRO_SERVICES:update_tab_if_needed({Query, nodequery, StartNode},Query,
                                                 wf:user(),Query,
                                                 FilteredNodes, Hash),
            event(load_list);
        true -> ok
    end,
    update_history(Query,StartNode),
    wf:replace(executed_query_placeholder,
               #panel{id=executed_query_placeholder,
                      body=[#p{body=?NITRO_CORE:whitespaces_to_html(Query),
                      class="query_title"},
                          #p{body=["Starting node: ",
                               #link{id=query_result0,
                                     postback={query_jump_to_node,StartNode,0},
                                     body=?NITRO_SERVICES:node_to_text(
                                        StartNode)}
                                  ]}]}),
    wf:session(currentjump_num,0),
    wf:session(currentjump_cur,0),
    wf:replace(jumptable,jumptable()),
    wf:replace(jumpcounter,#tablecell{id=jumpcounter,
                                      body=["0 of 0"],
                                      colspan=2,
                                      class=[menuitem]});
continue({continue, jump}, {FilteredNodes,Query,Type,StartNode}) ->
    FileList=lists:keysort(2,lists:map(
                fun(X) -> {X, ?NITRO_SERVICES:get_file(X)} end,
                FilteredNodes)),
    GroupedList=group_files(FileList),
    Result=lists:map(fun(X) ->
        InnerList=lists:map(fun(Y) ->
                    {{node,?NITRO_SERVICES:get_node_linenum(Y),Y},
                    ?NITRO_SERVICES:node_to_text(Y)}
                end,
                tl(X)),
        {group_by,{nopos,lists:last(string:tokens (hd(X), "/"))},list,InnerList} 
                     end, GroupedList),
    query_result_handler({Query,0,0},{{result, Result},
                         {warning,no_warning},{error,no_error}}), 
    wf:replace(executed_query_placeholder,
               #panel{id=executed_query_placeholder,
                      body=[#p{body=?NITRO_CORE:whitespaces_to_html(Query),
                      class="query_title"},
                          #p{body=["Starting node: ",
                               #link{id=query_result0,
                                     postback={query_jump_to_node,StartNode,0},
                                     body=?NITRO_SERVICES:node_to_text(
                                        StartNode)}
                                  ]}]}),
    if
        Type/=none ->
            referl_htmlserver:add_query(Type,Query),
            ?NITRO_SERVICES:update_tab_if_needed({Query, nodequery, StartNode},Query,
                                                 wf:user(),Query,Result,
                                                 referl_misc:database_hash()),
            event(load_list);
        true -> ok
    end, 
    update_history(Query,StartNode),
    wf:wire(#script{script="
        try{$(obj('query_result1')).click();}catch(ex){}"}),
    HistoryJumpnode=wf:session(history_jumpnode),
    if
        HistoryJumpnode/=undefined -> jump_to_node(HistoryJumpnode);
        true -> ok
    end;

continue({continue, generate}, same) ->
    StartPos=wf:session(selected_startpos),
    case StartPos of
        undefined -> ok;
        {SL,SP} ->
            {EL,EP}=wf:session(selected_endpos),
            wf:wire(#script{script="
                highlight_lc(obj('query_result_source_code_placeholder'),"
                ++integer_to_list(SL)++","++integer_to_list(SP)++","
                ++integer_to_list(EL)++","++integer_to_list(EP+1)++");"});
        _ ->
            wf:session(selected_position, StartPos), 
            wf:wire(#script{script="
                highlight(obj('query_result_source_code_placeholder'),"
                ++integer_to_list(StartPos)++","
                ++integer_to_list(wf:session(selected_endpos))++");"})
    end,
    wf:session(selected_startpos,undefined);

continue({continue, generate}, Text) ->
    wf:replace(query_result_source_code_placeholder,
                #panel{id=query_result_source_code_placeholder,
                    body=Text,class=[monosp]}),
    continue({continue, generate}, same),
    wf:wire(#script{script=
        "generatelinenums(obj('query_result_source_code_placeholder'),
            obj('linenums'));"}),
    Node=wf:session(click_node),
    if
        Node==undefined -> ok;
        true ->
            show_result(Node),
            wf:session(click_node,undefined)
    end;

continue({continue, check_if_node_exists}, Node) ->
    NodeExists=wf:session(node_exists),
    if
        NodeExists/=true ->
            {Start,End}= ?NITRO_SERVICES:get_node_position(Node),
            wf:wire(#script{script="
            try{
                highlight(obj('query_result_source_code_placeholder'),"
                ++integer_to_list(Start+1)++","++integer_to_list(End+1)++");
            }catch(ex){}"});
        true -> wf:session(node_exists,undefined)
    end;

continue({continue, nodequery, {Q,StartNode,_}}, timeout) ->
    update_history(Q,{nodequery,StartNode}),
    query_result_handler({Q,nodequery,StartNode},timeout),
    event(load_list);

continue({continue, nodequery, {Q,StartNode,Type}}, Result) ->
    query_result_handler({Q,nodequery,StartNode},Result),
    event(load_list),
    if
        Type/=none -> referl_htmlserver:add_query(Type,Q);
        true -> ok
    end,
    case Result of
        {error,_} -> ok;
        {_,_,{error,E}} when E/=no_error -> ok;
        _ -> update_history(Q,{nodequery,StartNode})
    end,
    wf:wire(#script{script="
        try{$(obj('query_result1')).click();}catch(ex){}"});


continue({continue, posquery,{Q,File,StartPos}}, timeout) ->
    wf:replace(jumptable,jumptable()),
    query_result_handler({Q,File,StartPos},timeout),
    update_history(Q,{File,StartPos}),
    event(load_list);
    
continue({continue, posquery, {Q,File,StartPos}}, Result) ->
    wf:replace(jumptable,jumptable()),
    query_result_handler({Q,File,StartPos},Result),
    event(load_list),
    update_history(Q,{File,StartPos}),
    wf:wire(#script{script="try{$(obj('query_result1')).click();}catch(ex){}"}).
    
%%% ============================================================================
%%% Helper functions

%% @doc Update history with new query
update_history(Query,StartNode) ->
    Current=wf:session(query_history_current),
    Update=wf:session(history_jumpnode)==0,
    InMiddle=Current/=length(wf:session(query_history)),
    if
        (Update) and (not InMiddle) ->
            wf:session(query_history,wf:session(query_history)
                ++[{Query,StartNode}]),
            wf:session(query_history_current,Current+1),
            update_history_buttons();
        (Update) and (InMiddle) ->
            wf:session(query_history,
                lists:sublist(wf:session(query_history),Current)
                ++[{Query,StartNode}]),
            wf:session(query_history_current,Current+1),
            update_history_buttons();
        not Update ->
            wf:session(history_jumpnode,wf:session(history_jumpnode)-1),
            wf:wire(#script{script="
                obj('history_back').disabled=false;
                obj('history_forward').disabled=false;
                obj('history_list').disabled=false;"})
    end.

%% @doc Get custom query list for given type
get_custom_queries(Type) ->
    [#option{text="Choose previous query",value=" ",selected=true} |
    lists:map(
        fun(X) -> #option{text=X,value=X,selected=false} end,
        referl_htmlserver:get_queries(Type))].

show_queries_tab()->
    wf:wire(#script{script="$('#tabs').tabs('select', 0);"}).

show_last_result_tab()->
    wf:wire(#script{script="$('#tabs').tabs('select', 1);"}).

message_box(Content)->
    message_box(Content, executed_query_placeholder, 
        fun()->show_last_result_tab() end).

message_box(Content, What, Fun)->
    Cell=#tablecell{body=Content, 
                    class=["graph_controll_tablecell", "red_border"]},
    Table=#table{ rows=[#tablerow{cells=[Cell]}], class="query_result_table"},
    Panel=#panel{id=What, body=[Table], class=["width_100"]},
    wf:replace(What, Panel),
    Fun().

message_box_hide()->
    message_box_hide(executed_query_placeholder, fun()->executed_query() end).

message_box_hide(What, Fun)->
    wf:wire(What, #hide {}),
    wf:replace(What, Fun()),
    wf:wire(What, #show {}).

skeletons_table()->
    Skeletons= ?NITRO_SERVICES:list_skeletons(), %% SKEL
    Rows=case lists:foldl(skeletons_table_fun(), [], Skeletons) of
        [] -> EmptyListCell=#tablecell{body=[]},
              [#tablerow{cells=[EmptyListCell]}];
        R -> R
    end,
    #table{class="query_list_table", 
                 rows=Rows}.

skeletons_table_fun()->
    fun({Name, Body, Owner, ParamCardinality, Comment}, AccIn)->
           Skel={Name, Body, Owner, ParamCardinality},
           SkelLabel=split_label_into_rows(lists:flatten(Name++"/"++
               io_lib:format("~p",[ParamCardinality]))),
           Cell1=#tablecell{body=[#link {body=SkelLabel,
                                    title = "Use skeleton", 
                                    postback={use_skeleton,Skel}}],
                            align="left",
                            valign="middle",
                            colspan=5,
                            class=["query_list_tablecell", "white_border"]},
            
            InfoStr="Name: "++Name++"<br/>"++
                    "Body: "++Body++"<br/>"++
                    "Owner: "++Owner++"<br/>",
            CommentInfo=if (length(Comment)>0)->
                             "Comments: "++Comment++"<br/>";
                        true -> "Comments: -- <br/>"
            end,
            {HelpCell, InfoRow}=generate_help(InfoStr++CommentInfo),
            EditCell=case wf:user() of
                        Owner ->#tablecell{body=[#link {text="E",
                                    class="delete_query_a", 
                                    title = "Edit skeleton", 
                                    postback={edit_skeleton,Skel}}],
                                    align="left",
                                    valign="middle",
                                    class=["query_list_tablecell","width_15", 
                                           "white_border"]};
                        _ ->#tablecell{body=[], 
                                       class=["width_15", "white_border"]}
                     end,
            DeleteCell=case wf:user() of
                         Owner ->#tablecell{body=[#link {text="X",
                                    class="delete_query_a", 
                                    title = "Delete skeleton", 
                                    postback={delete_skeleton,Skel}}],
                                    align="left",
                                    valign="middle",
                                    class=["query_list_tablecell","width_15", 
                                           "white_border"]};
                        _ ->#tablecell{body=[], 
                                       class=["width_15", "white_border"]}
                     end,
           {RowId,CommentEditorRow}=generate_comment_editor({skeleton, Name},
                                                            Comment),
           FifthCellId=wf:temp_id(),
           FifthCell=#tablecell{id=FifthCellId,
                                body=[#link {text="C",
                                class="delete_query_a", 
                                title = "Edit comments of the skeleton", 
                                postback={show_comment_editor,
                                          FifthCellId, RowId }}],
                                align="left",
                                valign="middle",
                                class=["query_list_tablecell","width_15", 
                                       "white_border"]},
            wf:session(group_helper,wf:session(group_helper)+1),
            GH=integer_to_list(wf:session(group_helper)),
           [#tablerow{class=[grouph,"group"++GH],
                cells=[Cell1]},
            #tablerow{id=a,class=[groupd,"group"++GH],
                cells=[#tablecell{class="width_15"},
                       HelpCell, DeleteCell, EditCell, FifthCell]},
            InfoRow, 
            CommentEditorRow]++AccIn
    end.

generate_help(InfoStr)->
    TempIdCell=wf:temp_id(),
    TempIdRow=wf:temp_id(),
    TempIdLink=wf:temp_id(),
    HelpLink=#link {id=TempIdLink,
                    text="?",
                    class="delete_query_a", 
                    title = "Show information", 
                    postback={show_help,TempIdLink, TempIdCell,TempIdRow}},
    HelpCell=#tablecell{body=[HelpLink],
                      align="left",
                      valign="middle",
                      class=["query_list_tablecell","width_15","white_border"]},

    Cell=#tablecell{id=TempIdCell,
                    colspan=5,
                     body=[#p{body=InfoStr}],
                     class=["info_tablecell", "red_border"]},
    InfoRow=#tablerow{id=TempIdRow,
                      cells=[Cell],
                      style="display:none;"},
    {HelpCell, InfoRow}.

background_update()->
    timer:sleep(1000),
    Queries= ?NITRO_SERVICES:get_running_queries(wf:user()),
    Table=case lists:foldl(queries_table_fun(), [], Queries) of
        [] -> #table{rows=#tablerow{cells=#tablecell{body=[]}}};
        R -> #table{class="query_list_table",rows=R}
    end,
    wf:replace(running_queries_placeholder,
               #panel{id=running_queries_placeholder,
                      body=[Table]}),
    wf:flush(),
    background_update().

queries_table_fun()->
    fun({QueryId, QueryStr,Ownership}, AccIn)->
           Cell1=#tablecell{body=split_label_into_rows(QueryStr),
                            align="left",
                            valign="middle",
                            class=["query_list_tablecell", "white_border",
                                   "running_query"]},
           Cell2=case Ownership of
                     own->
                         #tablecell{body=[#link {text="X",
                                    class="delete_query_a", 
                                    title = "Abort query", 
                                    postback={abort_query,QueryId}}],
                                    align="left",
                                    valign="middle",
                                    class=["query_list_tablecell","width_15"]};
                     extraneous->
                         #tablecell{body="",
                                    align="left",
                                    valign="middle",
                                    class=["query_list_tablecell","width_15"]}
                 end,
           [#tablerow{cells=[Cell1, Cell2]}]++AccIn
    end.

query_result_handler({Q,_,_},timeout)->
    wf:replace(executed_query_placeholder,
               #panel{id=executed_query_placeholder,
                      body=[#p{body=?NITRO_CORE:whitespaces_to_html(Q),
                               class="query_title"},
                            #br{},
                            #p{body="The execution of the query has
                                     exceeded the timeout limit.",
                               class="error"},
                            #br{}]}),
    wf:wire(#notification{type=error,
                text="The execution of the query has
                      exceeded the timeout limit."}),
    wf:replace(query_result_placeholder, 
               #panel{id=query_result_placeholder, body=[]});

query_result_handler({Q,_,_},{error,E})->
    wf:replace(executed_query_placeholder,
               #panel{id=executed_query_placeholder,
                      body=[#p{body=?NITRO_CORE:whitespaces_to_html(Q),
                               class="query_title"},
                            #br{},
                            #p{body=E,
                               class="error"},
                            #br{}]}),
    wf:wire(#notification{type=error, text=E}),
    wf:replace(query_result_placeholder, 
               #panel{id=query_result_placeholder,body=[]});

query_result_handler({Q,_,_},{{result, no_result},
    {warning,no_warning},{error,no_error}})->
    wf:replace(executed_query_placeholder,
               #panel{id=executed_query_placeholder,
                      body=[#p{body=?NITRO_CORE:whitespaces_to_html(Q),
                               class="query_title"},
                            #br{},
                            #p{body="nitrogen_helper crashed",
                               class="error"},
                            #br{}]}),
    wf:wire(#notification{type=error, text="nitrogen_helper crashed"}),
    wf:replace(query_result_placeholder, 
               #panel{id=query_result_placeholder, body=[]});

query_result_handler({Q,_,_},{{result, no_result},
    {warning,no_warning},{error,E}})->
    wf:replace(executed_query_placeholder,
               #panel{id=executed_query_placeholder,
                      body=[#p{body=?NITRO_CORE:whitespaces_to_html(Q),
                               class="query_title"},
                            #br{},
                            #p{body=E,
                               class="error"},
                            #br{}]}),
    wf:wire(#notification{type=error, text=E}),
    wf:replace(query_result_placeholder, 
               #panel{id=query_result_placeholder,body=[]});

query_result_handler({Q,File,StartPos},{{result, Result},
    {warning,no_warning},{error,no_error}})->
    if 
        File==nodequery ->
            {_,StartNode,_}=StartPos,
            wf:replace(executed_query_placeholder,
               #panel{id=executed_query_placeholder,
                      body=[#p{body=?NITRO_CORE:whitespaces_to_html(Q),
                      class="query_title"},
                          #p{body=["Starting node: ",
                               #link{id=query_result0,
                                     postback={query_jump_to_node,StartNode,0},
                                     body=?NITRO_SERVICES:node_to_text(
                                        StartNode)}
                                  ]}]});
        is_list(File) ->
            wf:replace(executed_query_placeholder,
               #panel{id=executed_query_placeholder,
                      body=[#p{body=?NITRO_CORE:whitespaces_to_html(Q),
                               class="query_title"},
                            #p{body=#link {id="query_result0",
                                           text="Starting position", 
                                           postback={show_selection, File,
                                        StartPos, StartPos, StartPos+1, 0}}}]});
        true -> wf:replace(executed_query_placeholder,
               #panel{id=executed_query_placeholder,
                      body=#p{body=?NITRO_CORE:whitespaces_to_html(Q),
                               class="query_title"}})
    end,
    wf:replace(query_result_placeholder,
               #panel{id=query_result_placeholder,
                      body=[#table{id=query_result_table,
                                   class="queries_result_table",
                                   rows=html_from_result(Result)},
                            #br{}]});

query_result_handler({Q,File,StartPos},{{result, Result},
    {warning,Warning},{error,no_error}})->
    if 
        File==nodequery ->
            {_,StartNode,_}=StartPos,
            wf:replace(executed_query_placeholder,
               #panel{id=executed_query_placeholder,
                      body=[#p{body=?NITRO_CORE:whitespaces_to_html(Q),
                      class="query_title"},
                          #p{body=["Starting node: ",
                               #link{id=query_result0,
                                     postback={query_jump_to_node,StartNode,0},
                                     body=?NITRO_SERVICES:node_to_text(
                                        StartNode)}
                                  ]}]});
        is_list(File) ->
            wf:replace(executed_query_placeholder,
               #panel{id=executed_query_placeholder,
                      body=[#p{body=?NITRO_CORE:whitespaces_to_html(Q),
                               class="query_title"},
                            #p{body=#link {id="query_result0",
                                           text="Starting position", 
                                           postback={show_selection, File, 
                                        StartPos, StartPos, StartPos+1, 0}}}]});
        true -> wf:replace(executed_query_placeholder,
               #panel{id=executed_query_placeholder,
                      body=#p{body=?NITRO_CORE:whitespaces_to_html(Q),
                               class="query_title"}})
    end,
    wf:wire(#notification{type=warning, text=Warning}),
    wf:replace(warning_messages_placeholder,
               #panel{id=warning_messages_placeholder,
                      body=Warning}),
    wf:replace(query_result_placeholder,
               #panel{id=query_result_placeholder,
                      body=[#table{id=query_result_table,
                                   class="queries_result_table",
                                   rows=html_from_result(Result)},
                            #br{}]}).

warning_handler({Message, Warning}) when is_list(Warning)->
    [#link{id=show_errors_link, 
           text=Message, 
           postback={show_errors,Message}, 
           class="error_link"},
     #table{id=warning_table,
           class="queries_result_table",
           rows=lists:foldl(fun({File, StartPos, Length,ErrorMessage},Acc)->
                        Link=#link {text=File,
                               title = "Show", 
                               postback={show_selection, 
                                         File,
                                         StartPos, 
                                         StartPos, 
                                         StartPos+Length-1}},
                        Cell1=#tablecell{body=[Link],
                                        align="left",
                                        valign="middle",
                                        class="queries_result_tablecell"},
                        Cell2=#tablecell{body=ErrorMessage,
                                        align="left",
                                        valign="middle",
                                        class="queries_result_tablecell"},
                        Acc++[#tablerow{cells=[Cell1,Cell2]}]
                            end, [], Warning),
         style="display:none;"}];

warning_handler(_)->"".

load_list_function()->
    fun(E, AccIn)->
    CurUser=wf:user(),
    {SafeQuery={SQ,File, Pos},Q,Alias, Comment, Users}=E,
    ThrdCell=case lists:member(CurUser, Users) orelse wf:role(admin) of
        true ->
            #tablecell{body=[#link {text="X",
                                    class="delete_query_a", 
                                    title = "Delete query", 
                                    postback={delete_query,SafeQuery}}],
                                    align="left",
                                    valign="middle",
                                    class=["query_list_tablecell","width_15", 
                                           "white_border"]};
        false ->#tablecell{body=[], class=["width_15", "white_border"]}
    end,
    FstCellId=wf:temp_id(),
   
            FstCell=#tablecell{id=FstCellId,
                   body=[#link{body=split_label_into_rows(Alias),
                                    title = "Run query",
                                    postback={run_query,SafeQuery,Q}}],
                        align="left",
                        valign="middle",
                        colspan=5,
                        class=["query_list_tablecell", "white_border"]},
    FourthCellId=wf:temp_id(),
    FourthCell=#tablecell{id=FourthCellId,
                          body=[#link {text="N",
                                    class="delete_query_a", 
                                    title = "Assign a name to the query", 
                                    postback={alias_query, 
                                              Alias, 
                                              SafeQuery, 
                                              FstCellId, 
                                              FstCell,
                                              FourthCellId}}],
                          align="left",
                          valign="middle",
                          class=["query_list_tablecell","width_15", 
                                 "white_border"]},
    
    {RowId,CommentEditorRow}=generate_comment_editor({sem_query,SafeQuery},
                                                     Comment),
    FifthCellId=wf:temp_id(),
    FifthCell=#tablecell{id=FifthCellId,
                          body=[#link {text="C",
                                    class="delete_query_a", 
                                    title = "Edit comments of the query", 
                                    postback={show_comment_editor,
                                              FifthCellId, RowId }}],
                          align="left",
                          valign="middle",
                          class=["query_list_tablecell", 
                                 "white_border"]},
    
    QInfo=if 
            (is_list(File) and is_integer(Pos)) -> 
                "Query string: "++SQ++"<br/>"++
                "File: "++File++"<br/> Starting Position: "++
                wf:to_list(Pos)++"<br/>";
            File==nodequery ->
                {_,Node,_}=Pos,
                "Query string: "++SQ++"<br/>"++
                        "File: "++ ?NITRO_SERVICES:get_file(Node)++
                        "<br/> Starting Node: "++
                        ?NITRO_SERVICES:node_to_text(Node)++"<br/>";
            true -> "Query string: "++SQ++"<br/>"
          end,
    CommentInfo=if (length(Comment)>0)->
                   "Comments: "++Comment++"<br/>";
               true -> "Comments: -- <br/>"
            end,
    {SndCell, InfoRow}=generate_help(QInfo++CommentInfo),

    wf:session(group_helper,wf:session(group_helper)+1),
    GH=integer_to_list(wf:session(group_helper)),
    [#tablerow{class=[grouph,"group"++GH],
            cells=[FstCell]},
     #tablerow{id=a,class=[groupd,"group"++GH],
            cells=[#tablecell{class="width_15"},
                   SndCell,ThrdCell,FourthCell, FifthCell]},
     InfoRow, 
     CommentEditorRow]++AccIn
    end.

generate_comment_editor(ID={_Type, _Identifier},Comment)->
    TempId=wf:temp_id(),
    Cell=#tablecell{body=[
                     #textarea{id=TempId, text=Comment, style="width:100%"},
                     #button{text="Save Comment", 
                             postback={save_comment, ID, TempId},
                             class=[referl_button]}],
                    align="left",
                    valign="middle",
                    colspan="5",
                    class=["query_list_tablecell", "white_border"]},
    RowId=wf:temp_id(),
    {RowId, #tablerow{id=RowId, cells=[Cell], style="display:none;"}}.

split_label_into_rows(Query) when length(Query)<?MAX_CHR_OF_LINE ->
    [Query];
split_label_into_rows(Query)->
    {List1,List2}=lists:split(?MAX_CHR_OF_LINE,Query),
    lists:flatten(List1++"<br/>"++split_label_into_rows(List2)).

html_from_result(Table) ->
    %io:format("~p",[Table]),
    wf:session(result_helper,0),
    wf:session(group_helper,0),
    TableElements = to_table(Table),
    if 
        TableElements/=[] ->
            wf:session(currentjump_nodes,undefined),
            wf:session(currentjump_num,wf:session(result_helper)),
            wf:session(currentjump_cur,0);
        true -> 
            wf:replace(jumptable,#table{id=jumptable}),
            wf:session(selected_source, undefined),
            wf:session(selected_source_hash, -1),
            wf:session(last_source, undefined),
            wf:replace(linenums,#panel{id=linenums,body=[],class=["lnums"]}),
            wf:replace(query_result_source_code_placeholder,
                      #panel{id=query_result_source_code_placeholder,
                             body=[]}),
            replace_curfile(undefined),
            wf:session(currentjump_nodes,undefined)
    end,

    case TableElements of
        [] ->[#tablerow{cells=[#tablecell{body=[#p{body="No result"}],
                                          align="left",
                                          valign="middle",
                                          class="queries_result_tablecell"}]}];
        _ ->TableElements
    end.

to_table([]) -> [];

to_table([{group_by,{nopos,"No Result"},list,[]}]) -> [];
%selection without groups, empty result
to_table([{list,[]}]) -> [];

%statistic result
to_table([C={eq,_Text1,_Text2}]) ->
    [#tablerow{cells=[#tablecell{body=[#p{body=get_text(C)}],
                                 align="left",
                                 valign="middle",
                                 class="queries_result_tablecell"}]}];

%selection without groups
to_table([{list,L}]) ->
    to_table({list,L});

%used for selection without groups
to_table({list,L}) ->
    case L of
%not used case clause
        [] ->[#tablerow{cells=[#tablecell{body=[#p{body="No result"}],
                                          align="left",
                                          valign="middle",
                                          class="queries_result_tablecell"}]}];
        _  -> [#tablerow{cells=[#tablecell{body=[to_html({list,L})],
                                           align="left",
                                           valign="middle",
                                           class="queries_result_tablecell"}]}] 
    end;

%grouped selection
to_table([{group_by, Entity, list, List}|Tail]) ->
    to_table([{group_by, Entity},{list, List}| Tail]);

%not used
to_table([{group_by, Entity, list, Type, List}|Tail]) ->
    to_table([{group_by, Entity},{list, Type, List}| Tail]);

%not used
to_table([{group_by, PrevType, Entity, list, Type, List}|Tail]) ->
    to_table([{group_by, PrevType, Entity},{list, Type, List}| Tail]);

%not used
to_table([{group_by, PrevType, Entity, eq, Type, PropVal}|Tail]) ->
    to_table([{group_by, PrevType, Entity}, {eq, Type, PropVal}|Tail]);

%property_query
to_table([{group_by, Entity, eq, Type, PropVal}|Tail]) ->
    IdText=integer_to_list(wf:session(group_helper)),
    wf:session(group_helper,wf:session(group_helper)+1),
    Ordering=if 
        IdText=="0" -> #tablerow{cells=[
                       #tablecell{body="Name <a href='javascript:void(0);' 
                                    onclick='sortcolumns(1)'>v</a>
                                    <a href='javascript:void(0);' 
                                    onclick='sortcolumns(2)'>^</a>",
                                  align="left",
                                  valign="middle"},
                       #tablecell{body="Value <a href='javascript:void(0);' 
                                   onclick='sortcolumns(3)'>v</a>
                                   <a href='javascript:void(0);' 
                                   onclick='sortcolumns(4)'>^</a>",
                                  align="right",
                                  valign="middle"}]};
        true -> #span{}
    end,
        
    [Ordering,#tablerow{id="metricresult_"++IdText,
                        cells=[#tablecell{body=[to_html(Entity)],
                                 id="metricresult1_"++IdText,
                                 align="left",
                                 valign="middle",
                                 class="queries_result_tablecell2"},
                      #tablecell{body=[get_text({eq, Type, PropVal})],
                                 id="metricresult2_"++IdText,
                                 align="right",
                                 valign="middle",
                                 class="queries_result_tablecell2"}]}
    |to_table(Tail)];

%composite_property_query
to_table([{group_by, Entity, propertylist, Name, PropVals}|Tail]) ->
    IdText=integer_to_list(wf:session(group_helper)),
    wf:session(group_helper,wf:session(group_helper)+1),
    Ordering=if 
        IdText=="0" -> #tablerow{cells=[
                       #tablecell{body="Name <a href='javascript:void(0);' 
                                    onclick='sortcolumns(1)'>v</a>
                                    <a href='javascript:void(0);' 
                                    onclick='sortcolumns(2)'>^</a>",
                                  align="left",
                                  valign="middle"},
                       #tablecell{body=atom_to_list(Name),
                                  align="right",
                                  valign="middle"}]};
        true -> #span{}
    end,
    HtmlPropVals = [get_text({eq, val, Val})++"<br />" || Val <- PropVals],
        
    [Ordering,#tablerow{id="metricresult_"++IdText,
                        cells=[#tablecell{body=[to_html(Entity)],
                                 id="metricresult1_"++IdText,
                                 align="left",
                                 valign="middle",
                                 class="queries_result_tablecell2"},
                      #tablecell{body=lists:flatten(HtmlPropVals),
                                 id="metricresult2_"++IdText,
                                 align="right",
                                 valign="middle",
                                 class="queries_result_tablecell2"}]}
    |to_table(Tail)];

%used for grouped selection
to_table([{group_by,Row1},Row2|Tail]) ->
    wf:session(group_helper,wf:session(group_helper)+1),
    Group=integer_to_list(wf:session(group_helper)),
    {list,List}=Row2,
    R2={list,lists:keysort(2,List)},
    [#tablerow{cells=[#tablecell{body=[to_html(Row1)],
                                 align="left",
                                 valign="middle",
                                 class=["queries_result_tablecell1","head",
                                        "group"++Group]},
                      #tablecell{body=#link{body="Hide", 
                                            class=["head","headtxt",
                                                   "group"++Group]},
                                 align="right",
                                 valign="middle",
                                 class=["queries_result_tablecell1","head",
                                        "group"++Group]}]},
    #tablerow{cells=#tablecell{colspan=2,body=[to_html(R2)],
                                 align="left",
                                 valign="middle",
                                 class=["queries_result_tablecell2","data",
                                        "group"++Group]}}
    |to_table(Tail)];

%iteration/closure
to_table([{chain,L,End}|Tail]) ->
    [#tablerow{cells=[#tablecell{body=[to_html({chainList,L}),
                                       to_html({chain_end,End})],
                                 align="left",
                                 valign="middle",
                                 class="queries_result_tablecell"}]}
    |to_table(Tail)];

%probably not used, below function could cover
to_table(R=[C|_]) when is_list(R) and is_integer(C)->
    [#tablerow{cells=[#tablecell{body=[#p{body=R}],
                                 align="left",
                                 valign="middle",
                                 class="queries_result_tablecell"}]}];

%fallback, probably not used
to_table(R) ->
    [#tablerow{cells=[#tablecell{body=[#p{body=io_lib:format("~p",[R])}],
                                 align="left",
                                 valign="middle",
                                 class="queries_result_tablecell"}]}].

to_html({list,[]}) -> [];
to_html({list,[H]}) -> to_html(H);
to_html({list,[H|T]}) -> [to_html(H),"<br/>",to_html({list,T})];

to_html({chainList,[]}) -> [];
to_html({chainList,[H]}) -> to_html({H,{comma,no}});
to_html({chainList,[H|T]}) -> [to_html({H,{comma,yes}}),to_html({chainList,T})];

to_html({chain_end,"\n"}) -> [];
to_html({chain_end,"*\n"}) -> "*";

to_html({X,{comma,IsComma}}) ->
    Separator = case IsComma of
                    yes -> ", ";
                    no -> ""
                end,
    case get_pos(X) of
        nopos -> #span{ text=get_text(X) ++ Separator};
        {node,StartLine,Node} ->
            wf:session(result_helper,wf:session(result_helper)+1),
            Type=?NITRO_SERVICES:get_type(Node),
            LineText=if 
                Type/=mod -> "   Line "++integer_to_list(StartLine)++": ";
                true -> "" 
            end,
            #link{id="query_result"++integer_to_list(wf:session(result_helper)),
                  text=LineText++get_text(X)++Separator, 
                  title = "Show", 
                  postback={query_jump_to_node,Node,wf:session(result_helper)}};
        {File,{SP,{StartLine,SC}=Start},{_,{EL,EC}=End}} ->
            LineText=if
            (StartLine==1) andalso (SC==1) andalso (EL==1) andalso (EC==1) ->"";
            true ->  "   Line "++integer_to_list(StartLine)++": "
            end,
            wf:session(result_helper,wf:session(result_helper)+1),
            #link{id="query_result"++integer_to_list(wf:session(result_helper)),
                  text=LineText++get_text(X)++Separator, 
                  title = "Show", 
                  postback={show_selection, File, SP, Start, 
                            End, wf:session(result_helper)}}
    end;

to_html(X) ->
    case get_pos(X) of
        nopos ->#span{ text=get_text(X)};
        result ->
            wf:session(result_helper,wf:session(result_helper)+1),
            #link{id="query_result"++integer_to_list(wf:session(result_helper)),
                  text=get_text(X), 
                  title = "Show"};
        {node,StartLine,Node} ->
            wf:session(result_helper,wf:session(result_helper)+1),
            Type=?NITRO_SERVICES:get_type(Node),
            LineText=if 
                Type/=mod -> "   Line "++integer_to_list(StartLine)++": ";
                true -> "" 
            end,
            #link{id="query_result"++integer_to_list(wf:session(result_helper)),
                  text=LineText++get_text(X), 
                  title = "Show", 
                  postback={query_jump_to_node,Node,wf:session(result_helper)}};
        {File,{SP,{StartLine,SC}=Start},{_,{EL,EC}=End}} ->
            LineText=if
            (StartLine==1) andalso (SC==1) andalso (EL==1) andalso (EC==1) ->"";
            true ->  "   Line "++integer_to_list(StartLine)++": "
            end,
            wf:session(result_helper,wf:session(result_helper)+1),
            #link{id="query_result"++integer_to_list(wf:session(result_helper)),
                  text=LineText++get_text(X), 
                  title = "Show", 
                  postback={show_selection, File, SP, Start, 
                            End, wf:session(result_helper)}}
    end.

get_text({group_by,{_Pos,Text}}) when is_list(Text)-> Text;
get_text({group_by,{_Pos,Text}}) -> io_lib:format("~p",[Text]);

get_text({_Pos,Text}) when is_list(Text)-> Text;
get_text({_Pos,Text}) -> io_lib:format("~p",[Text]);

get_text({eq,Text1,Text2}) when is_list(Text1) and is_list(Text2)-> 
    Text2;
get_text({eq,_,Text2}) ->
    io_lib:format("~p",[Text2]);
get_text(_) -> "notext".

get_pos({group_by,{{File,StartPos,EndPos},_Text}}) ->
    get_pos({{File,StartPos,EndPos},_Text});
get_pos({{File, 1, 1}, _Text}) -> 
	{File, {1,{1,1}}, {1,{1,1}}};
get_pos({{File,StartPos,EndPos},_Text}) ->
    {File, StartPos, EndPos};
get_pos(_) -> nopos.

group_files([]) -> [["No Result"]];
group_files([{Data,File}|ES]) ->
    group_files(ES,[File,Data],[],File).

group_files([],InnerSum,Sum,_) -> Sum++[InnerSum];
group_files([{Data,File}|ES],InnerSum,Sum,LastFile) when File==LastFile ->
    group_files(ES,InnerSum++[Data],Sum,File);
group_files([{Data,File}|ES],InnerSum,Sum,_) ->
    group_files(ES,[File,Data],Sum++[InnerSum],File).

update_source() ->
    File=wf:session(selected_source),
    OldHash=wf:session(selected_source_hash),
    CurHash=?NITRO_SERVICES:get_filehash(File),
    wf:wire(#script{script="
    $('#file_browser_db').remove();
	$(obj('file_browser_db_panel')).append('<div id=file_browser_db></div>');
    $('#file_browser_db').fileTree({
        root: '"++get_db_root_dir()++"',
        script: 'ajax_handler?mod=db',
        expandSpeed: 500,
        collapseSpeed: 500
    },function(file){
        $.post('ajax_handler', { selected_file: file} );
        $(obj('show_html_button')).click();
    });"}),
    if
        OldHash/=-1 andalso CurHash/=-1 andalso OldHash/=CurHash ->
            wf:session(selected_source_hash,CurHash),
            wf:session(selected_source,undefined),
            wf:session(last_source, undefined),
            event({show_selection_links,File});
        true -> ok
    end.

%% @doc Generate predefined query list for given type
%% Invbutton states what extra button the list will have
get_predef_queries(Node,Type,InvButton) ->
    TextBox=#textarea_autocomplete{ id=context_query_box,
                                    tag="", 
                                    text="",
                                    style="height:18px;",
                                    minLength=2, 
                                    delay=300, 
                                    html_encode=true},
    Button=#button{id=context_run,
                   text="Run",
                   postback={docustomquery_context,Node},
                   class=referl_button},
    {_,T,Id}=try
        ?NITRO_SERVICES:get_container(Node)
    catch
        _ -> {0,error,0}
    end,
    NodeString=atom_to_list(T)++"|"++integer_to_list(Id),
    Buttons=case InvButton of
        queries -> [#tablerow{cells=#tablecell{body=#dropdown{id=customqueries,
                             options=get_custom_queries(Type),
                             postback={docustomquery,Node}}}},
        
                    #tablerow{cells=#tablecell{colspan=2,body=
                    #link{text="Start investigation",
                         url="javascript:newWindow('investigations?start="++
                             NodeString++"')"}}}];
        invs -> []
    end,
    if
        T==denied -> wf:wire(#notification{type=error, 
                              text="Access to database was denied."}),[];
        T==error -> wf:wire(#notification{type=error, 
                              text="Node does not exist anymore."}),[];
        true ->
    [get_predef_queries_(Node,Type),
     #table{rows=[#tablerow{
         cells=[#tablecell{body=TextBox},#tablecell{body=Button}]}]++Buttons}]
    end.

get_predef_queries_(Node,var) ->
    [?Query("Var References","@var.references"),
     ?Query("Var Binding","@var.bindings"),
     ?Query("Var Origin","@expr.origin"),
     ?Query("Var Reach","@expr.reach")];    
get_predef_queries_(Node,funappl) ->        
    [?Query("Function References","@fun.references"),
     ?Query("Function Definition","@expr.funs")];
get_predef_queries_(Node,fundef) ->
    [?Query("Function References","@fun.references")];
get_predef_queries_(Node,mod) ->
    [?Query("Functions","@file.funs"),
     ?Query("Module References","@file.references"),
     ?Query("Records","@file.records")];
get_predef_queries_(Node,macrodef) ->
    [?Query("Macro References","@macro.references")];
get_predef_queries_(Node,recdef) ->
    [?Query("Record References","@record.references")];
get_predef_queries_(Node,recfielddef) ->
    [?Query("Field References","@field.references")];
get_predef_queries_(Node,recexpr) ->    
    [?Query("Record References","@record.references"),
     ?Query("Record Definition","@expr.records"),
     ?Query("All Field References","@record.fields.references")];
get_predef_queries_(Node,recfield) ->
    [?Query("Field References","@field.references")];
get_predef_queries_(_,_) ->
    [].
