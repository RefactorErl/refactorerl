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

-module (investigations).
-compile(export_all).
-include_lib("referl_ui/include/nitrogen.hrl").
-include_lib("nitrogen/include/wf.hrl").
-include_lib("kernel/include/file.hrl").
-include("records.hrl").
-vsn("$Rev: 9568 $ ").

-define(Query(Label,Query),#link{class=mylink,
                                text=Label, 
                                postback={predefquery,Query,Node}},#br{}).
                                
-record(wdata, {label,startnode,container,name,hidden=false,text,ann=""}).

%%% ============================================================================
%%% HTML output

main() -> 
    wf:session(selectednode_data, undefined),
    wf:session(current_window, undefined),
    wf:session(query_startwindow, undefined),
    wf:session(current_query, undefined),
    wf:session(investigation,[]),
    wf:session(nodetext,""),
    wf:session(inv_name,"Unnamed"),
    wf:session(current_inv,""),
    wf:session(modified,false),
    nitrogen_lib:main("investigations.html").

title() -> "RefactorErl Queries".

other() ->
    HiddenData=#hidden{id=hiddendata,text=""},
    HiddenButton=#link{id=hiddenbutton,body="",postback=node_clicked},
    #table{id=otherdiv,class=width_100,rows=[#tablerow{cells=[
        #tablecell{body=HiddenData},
        #tablecell{body=HiddenButton}]}]}.

menu() ->
    wf:wire(save_button, 
            savename, 
            #validate { validators=[#is_required { text="Required." }]}),
    StartLabel=#label{text="Starting function (m:f/a):"},
    StartButton=#table{rows=#tablerow{cells=[
        #tablecell{body=#textbox_autocomplete{id=start_textbox,
                                              tag=funs,
                                              style="width:150px"}},
        #tablecell{body=#button{id=start_button,
                        text="Start inv.",
                        class=need_db,
                        postback=start_investigation}}]}},
    SaveButton=#table{rows=#tablerow{cells=[
        #tablecell{body=#textbox{id=savename,style="width:150px"}},
        #tablecell{body=#button{id=save_button,text="Save",
        postback=save_investigation}}]}},
    Rename=#table{rows=#tablerow{cells=[
        #tablecell{body=#label{id=invname,text="Unnamed"}},
        #tablecell{body="&nbsp;"},
        #tablecell{body=#button{id=rename_inv,
                                text="Rename",
                                postback=rename_inv}}]}},
    #table{class=width_100,rows=[
        #tablerow{cells=#tablecell{body=Rename}},
        #tablerow{cells=#tablecell{body=StartLabel}},
        #tablerow{cells=#tablecell{body=StartButton}},
        #tablerow{cells=#tablecell{body=SaveButton}}
    ]}.
    
saved_invs() ->
    Dropdown=#dropdown{id=inv_owner,
                       options=[
                                #option{ text="My investigations", 
                                         value="my", 
                                         selected=true },
                                #option{ text="All investigations", 
                                         value="all", 
                                         selected=false }
                                ],
                       html_encode=true,
                       postback=load_invs
                      },
    Browser=#panel{id=invs_list,body=[]},
    event(load_invs),
    #table{class=width_100,rows=[
        #tablerow{cells=#tablecell{body=Dropdown}},
        #tablerow{cells=#tablecell{body=Browser}}
    ]}.
    
last_result() ->
    Result1=#panel{id=executed_query_placeholder,body=""},
    Result2=#panel{id=query_result_placeholder,body=""},
    #table{class=width_100,rows=[
        #tablerow{cells=#tablecell{body=Result1}},
        #tablerow{cells=#tablecell{body=Result2}}
    ]}.
    
investigations() -> 
    #panel{id=mainwrapper,body=#span{id=contextmenu,text="",class=contextmenu}}.
    
get_startnode() ->
    wf:set(savename,""),
    wf:session(current_inv,""),
    Start=wf:q(start),
    case Start of
        undefined -> ok;
        _ ->
            [D1,D2]=string:tokens(Start, "|"),
            TT=list_to_existing_atom(D1),
            Id=list_to_integer(D2),
            Clause={'$gn',TT,Id},
            add_window(Clause,0,Clause,"")
    end,
    [].
    
logout()->
    nitrogen_lib:logout_link(wf:user()).

%%% ============================================================================
%%% Handlers for postback events

event(logout)->
    nitrogen_lib:logout(wf:user());

event({hide_group,Num})->
    NumS=integer_to_list(Num),
    wf:wire("group"++NumS,#hide{}),
    wf:replace("hider"++NumS,
        #link{id="hider"++NumS,text="Show",postback={show_group,Num}});
    
event({show_group,Num})->
    NumS=integer_to_list(Num),
    wf:wire("group"++NumS,#show{}),
    wf:replace("hider"++NumS,
        #link{id="hider"++NumS,text="Hide",postback={hide_group,Num}});

event({popup_yes,Fun}) ->
    wf:remove(popup),
    Fun();
    
event(popup_no) ->
    wf:remove(popup),
    wf:session(modified,true);

event(rename_inv) ->
    wf:replace(invname,#textbox{id=invname,postback=rename_inv_final}),
    wf:wire(#script{script="$(obj('invname')).focus();"}),
    wf:wire(rename_inv,#hide{});
    
event(rename_inv_final) ->
    NewName=wf:q(invname),
    wf:session(inv_name,NewName),
    wf:replace(invname,#label{id=invname,text=NewName}),
    wf:wire(rename_inv,#show{});

event(start_investigation) ->
    Fun=fun() ->
    wf:session(modified,false),
    wf:set(savename,""),
    wf:session(current_inv,""),
    reset_windows(),
    StartFunction=wf:q(start_textbox),
    Result=re:split(StartFunction,"([:/])",[{return,list},group]),
    case Result of
        [[Module,":"],[Function,"/"],[Arity]] when length(Module)>0, 
                length(Function)>0, length(Arity)>0 ->
            Fun=try
                    ?NITRO_SERVICES:get_function(Module,Function,Arity)
                catch
                    _:_ -> []
                end,
            if
                Fun==[] -> wf:wire(#notification{type=error, 
                                               text="Function doesn't exist!"});
                true -> 
                    wf:session(current_window,0),
                    
                    execute_query("@fun.clause",{hd(Fun),hd(Fun)},fundef)
            end;
        _ -> wf:wire(#notification{type=error, text="Bad function format!"})
    end
    end,
    make_popup_window("The current investigation has been modified, "++
        "are you sure you want to start a new one?",Fun);

event({docustomquery_context,Node})->
    Query=wf:q(context_query_box),
    Type=?NITRO_SERVICES:get_type(Node),
    event({docustomquery,Node,Query,Type});

event({predefquery,Query,Node})->
    event({docustomquery,Node,Query,none});

event({docustomquery,Node,Query,Type})->
    NotExprQuery=lists:sublist(Query,5)/="@expr",
    if
        (Query/=[]) andalso (hd(Query)/=$@) -> 
           wf:wire(#notification{type=warning, 
                text="No global queries allowed!"});
        Query=="" ->
            ok;
        NotExprQuery ->
            PNode=?NITRO_SERVICES:get_proper_node(Node),
            execute_query(Query,{PNode,Node},Type);
        true -> 
            execute_query(Query,{Node,Node},Type)
    end;

event({query_jump_to_node,Node,Num})->
    Inv=wf:session(investigation),
    ButtonId="'query_result"++integer_to_list(Num)++"'",
    wf:wire(#script{script="
        try{
            if(lastcoloredresult!=null) 
                $(lastcoloredresult).removeClass('selected_queryresult')
            $(obj("++ButtonId++")).addClass('selected_queryresult');
            lastcoloredresult=obj("++ButtonId++");
        }catch(ex){}"}),
    WindowNum=tree_get_lastid(Inv),
    WindowId="wi"++integer_to_list(WindowNum),
    try
        CNode=?NITRO_SERVICES:get_container(Node),
        WindowBody=try
            ?NITRO_SERVICES:generate_node(CNode,WindowNum)
        catch
            request_denied -> "Database currently in use."
        end,
        WindowData=tree_get_node_data(Inv,WindowNum),
        wf:session(investigation,
            tree_modify_node(Inv,WindowNum,WindowData#wdata{container=CNode,
                                                            text=WindowBody})),
        wf:replace(WindowId,#panel{id=WindowId,body=WindowBody}),
        StartLine=integer_to_list(
            ?NITRO_SERVICES:get_node_linenum_comment(CNode)),
        wf:wire(#script{script=
        "generatelinenums(obj('"++WindowId++"'),
        obj('linenums"++integer_to_list(WindowNum)++"'),"++StartLine++");"}),
        show_result(Node,WindowNum)
    catch
        request_denied -> 
        wf:wire(#notification{type=error,text="Database is currently in use."})
    end;

event(node_clicked)->
    [D1,D2,Window]=string:tokens(wf:q(hiddendata), "|"),
    TT=list_to_existing_atom(D1),
    Id=list_to_integer(D2),
    Node={'$gn',TT,Id},
    Type=?NITRO_SERVICES:get_type(Node),
    CurrentSel=wf:session(selectednode_data),
    CurrentWin=wf:session(current_window),
    if
    (CurrentSel==Node) and (Window==CurrentWin) -> 
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
            $.post('ajax_handler', { nodetext: $(elem).text()} );
        }catch(ex){}"}),
        Rows=case wf:session(disablethings) of
            undefined -> main:get_predef_queries(Node,Type,invs);
            _ -> []
        end,
        wf:replace(contextmenu,
            #panel{id=contextmenu,body=Rows,class=contextmenu}),
        wf:wire(context_run, 
            context_query_box, 
            #validate { validators=[#is_required { text="Required." }]}),
        wf:wire(#script{script="positionContextMenu(selectednode,false);"}),
        wf:session(current_window,list_to_integer(Window)),
        wf:session(selectednode_data,Node)
    end;
    
event(save_investigation) ->
    FileName=wf:q(savename),
    event({save_investigation,FileName,fun() -> ok end});
    
event({save_investigation,FileName,EndFun}) ->
    WouldOverwrite=overwrites_inv(FileName),
    if
        WouldOverwrite ->  wf:wire(#notification{type=warning,
            text="An investigation with the given name already exists!"});
        true ->
        wf:session(current_inv,FileName),
        wf:session(modified,false),
        delete_empty_window(),
        Fun=fun(Tree) ->
                ?NITRO_SERVICES:insert_into_itab(wf:user(),FileName,
                {wf:session(inv_name),?NITRO_HELPER:get_database_hash(),Tree}),
                event(load_invs),
                EndFun()
            end,
        put_positions_in_tree(wf:session(investigation),Fun)
    end;

event({load_investigation,User,File}) ->
    Fun=fun() ->
    wf:session(modified,false),
    wf:set(savename,File),
    wf:session(current_inv,File),
    CurrentHash=?NITRO_HELPER:get_database_hash(),
    {Name,Hash,Investigations}=?NITRO_SERVICES:get_from_itab(User,File),
    if
        Hash/=CurrentHash -> wf:wire(#notification{type=warning,
            text="Database has changed since last visit, "++
            "investigation might not be usable."});
        true -> ok
    end,
    reset_windows(),
    wf:replace(invname,#label{id=invname,text=Name}),
    put_windows(Investigations)
    end,
    make_popup_window("The current investigation has been modified, "++
    "are you sure you want to load a new one?",Fun);
    
event({window_options,Window}) ->
    cleanup(),
    Inv=wf:session(investigation),
    WindowId="w"++integer_to_list(Window),
    wf:replace(contextmenu,#panel{id=contextmenu,body=[],class=contextmenu}),
    {_,_,Data,Children}=tree_get_node(Inv,Window),
    HaveKeepButton=(Window==tree_get_lastid(Inv)) 
                and (wf:session(current_query)/=undefined) 
                and (wf:session(current_window)/=1)
                and (Data#wdata.container/=empty),
    if
        HaveKeepButton ->
            wf:insert_bottom(contextmenu, 
                #button{text="New window",class=inv_button,
                    postback={window_keep,Window}}),
            wf:insert_bottom(contextmenu,#br{});
        true ->
            ok
    end,
    
    HaveDeleteButton=(Children==[]) 
                and (Data#wdata.container/=empty),
    if
        HaveDeleteButton ->
            wf:insert_bottom(contextmenu, 
                #button{text="Delete window",class=inv_button,
                    postback={window_delete,Window}}),
            wf:insert_bottom(contextmenu,#br{});
        true ->
            ok
    end,
    if
        (Data#wdata.hidden) and (Data#wdata.hidden/=deny)->
            wf:insert_bottom(contextmenu, 
                #button{text="Show contents",class=inv_button,
                    postback={window_show,Window}}),
            wf:insert_bottom(contextmenu,#br{});
        (not Data#wdata.hidden) and (Data#wdata.hidden/=deny) ->
            wf:insert_bottom(contextmenu, 
                #button{text="Hide contents",class=inv_button,
                    postback={window_hide,Window}}),
            wf:insert_bottom(contextmenu,#br{});
        true -> ok
    end,
    if 
        Data#wdata.ann=="" ->
            wf:insert_bottom(contextmenu, 
                #button{text="Add annotation",class=inv_button,
                        postback={window_add_ann,Window}});
        true ->
            wf:insert_bottom(contextmenu, 
                #button{text="Edit annotation",class=inv_button,
                        postback={window_edit_ann,Window}})
    end,
    wf:insert_bottom(contextmenu,#br{}),
    wf:insert_bottom(contextmenu, 
        #button{text="Move to new investigation",class=inv_button,
                postback={window_moveto_new,Window}}),
    wf:insert_bottom(contextmenu,#br{}),
    wf:insert_bottom(contextmenu, 
        #button{text="Rename",class=inv_button,
                postback={window_rename,Window}}),
    wf:insert_bottom(contextmenu,#br{}),
    case {Data#wdata.container,Data#wdata.hidden/=deny} of
        {{_,_,_},true} ->  
            wf:insert_bottom(contextmenu, 
                #button{text="Start new inv. from here",
                        class=[need_db,inv_button],
                        postback={window_newinv,Window}});
        _ -> ok
    end,
    wf:wire(#script{script="positionContextMenu(obj('"++WindowId++"'),true,26);"});

event({window_add_ann,Window}) ->
    wf:wire(#script{script="$(obj('contextmenu')).css('display','none');"}),
    add_ann_window(Window,""),
    event({window_edit_ann,Window});
    
event({window_edit_ann,Window}) ->
    wf:wire(#script{script="$(obj('contextmenu')).css('display','none');"}),
    WindowId="anni"++integer_to_list(Window),
    wf:replace(WindowId,#panel{id=WindowId,body=[#textarea{id=WindowId++"a"},
        #button{text="Ok",postback={window_edit_ann_final,Window}}]});
    
event({window_edit_ann_final,Window}) ->
    wf:session(modified,true),
    Inv=wf:session(investigation),
    Data=tree_get_node_data(Inv,Window),
    WindowId="ann"++integer_to_list(Window),
    InnerWindowId="anni"++integer_to_list(Window),
    NewAnn=?NITRO_CORE:whitespaces_to_html(wf:q(InnerWindowId++"a")),
    if
        NewAnn=="" ->
            wf:wire(#script{script=
                "jsPlumb.detachAllConnections($(obj('"++WindowId++"')));"}),
                wf:remove(WindowId);
        true -> 
            wf:replace(InnerWindowId,#panel{id=InnerWindowId,class=drg,body=NewAnn}),
            wf:wire(#script{script="jsPlumb.repaintEverything();"})
    end,
    wf:session(investigation,
        tree_modify_node(Inv,Window,Data#wdata{ann=NewAnn}));

event({window_newinv,Window}) ->
    Fun=fun()->
    Inv=wf:session(investigation),
    Data=tree_get_node_data(Inv,Window),
    wf:wire(#script{script="$(obj('contextmenu')).css('display','none');"}),
    wf:set(savename,""),
    wf:session(current_inv,""),
    reset_windows(),
    add_window(Data#wdata.container,0,Data#wdata.container,"")
    end,
    make_popup_window("The current investigation has been modified, "++
        "are you sure you want to start a new one?",Fun);

event({window_delete,Window}) ->
    wf:session(modified,true),
    wf:wire(#script{script="$(obj('contextmenu')).css('display','none');"}),
    delete_window(Window);
    
event({window_rename,Window}) ->
    LabelName="wn"++integer_to_list(Window),
    wf:wire(#script{script="$(obj('contextmenu')).css('display','none');"}),
    wf:replace(LabelName,#textbox{id=LabelName,
                                  postback={window_rename_final,Window}});
                                  
event({window_rename_final,Window}) ->
    wf:session(modified,true),
    LabelName="wn"++integer_to_list(Window),
    NewName=wf:q(LabelName),
    wf:wire(#script{script="$(obj('contextmenu')).css('display','none');"}),
    Inv=wf:session(investigation),
    WindowData=tree_get_node_data(Inv,Window),
    wf:session(investigation,
        tree_modify_node(Inv,Window,WindowData#wdata{name=NewName})),
    wf:replace(LabelName,#label{id=LabelName,text=NewName});                                

event({window_moveto_new,Window}) ->
    LabelName="wn"++integer_to_list(Window),
    wf:wire(#script{script="$(obj('contextmenu')).css('display','none');"}),
    wf:replace(LabelName,[#label{id=tmplab,text="Save new investigation as:"},
                          #textbox{id=LabelName,
                                   postback={window_moveto_new_final,Window}}]);

event({window_moveto_new_final,Window}) ->
    LabelName="wn"++integer_to_list(Window),
    NewName=getnametext(LabelName,""),
    WouldOverwrite=overwrites_inv(NewName,even_current),
    if
        NewName=="" -> ok;
        WouldOverwrite -> wf:wire(#notification{type=warning,
            text="An investigation with the given name already exists!"});
        true ->
        wf:replace(tmplab,#label{id=tmplab,text="Save old investigation as:"}),
        wf:replace(LabelName,#textbox{id=LabelName,
                        postback={window_moveto_new_final2,Window,NewName}})
    end;

event({window_moveto_new_final2,Window,NewName}) ->
    LabelName="wn"++integer_to_list(Window),
    OldName=getnametext(LabelName,""),
    WouldOverwrite=overwrites_inv(OldName,even_current),
    if
        OldName=="" -> ok;
        WouldOverwrite -> wf:wire(#notification{type=warning,
            text="An investigation with the given name already exists!"});
        true -> event({moveto_new_investigation,Window,OldName,NewName})
    end;

event({moveto_new_investigation,Window,OldName,NewName}) ->
    NewTree=tree_get_subtree(wf:session(investigation),Window),
    RemainingTree=tree_delete_subtree(wf:session(investigation),Window),
    WindowData=tree_get_node_data(RemainingTree,Window),
    wf:session(investigation,
        tree_modify_node(RemainingTree,Window,
            WindowData#wdata{container={investigation,{wf:user(),NewName}}})),
    event({save_investigation,OldName,fun() -> 
        wf:session(investigation,NewTree),
        event({save_investigation,NewName,
        fun() -> event({load_investigation,wf:user(),NewName}) end}) 
                                      end});

event({window_hide,Window}) ->
    Inv=wf:session(investigation),
    WindowText=integer_to_list(Window),
    WindowId="wi"++WindowText,
    wf:wire(#script{script="$(obj('contextmenu')).css('display','none');"}),
    #wdata{container=CN,hidden=H}=tree_get_node_data(Inv,Window),
    if H/=deny ->
    case CN of 
        {_,_,_} ->
    WindowBody=?NITRO_SERVICES:node_to_text(CN),
    wf:replace("hb"++WindowText,
        #button{id="hb"++WindowText,text="Show",postback={window_show,Window}}),
    wf:replace(WindowId,#panel{id=WindowId,body=WindowBody}),
    StartLine=integer_to_list(
        ?NITRO_SERVICES:get_node_linenum_comment(CN)),
    wf:wire(#script{script=
        "generatelinenums(obj('wi"++integer_to_list(Window)++"'),
            obj('linenums"++integer_to_list(Window)++"'),"++StartLine++");"}),
    WindowData=tree_get_node_data(Inv,Window),
    wf:session(investigation,
        tree_modify_node(Inv,Window,WindowData#wdata{hidden=true})),
    wf:wire(#script{script="
        $(obj('w"++WindowText++"')).width('auto');
        $(obj('w"++WindowText++"')).height('auto');
        $(obj('wd"++WindowText++"')).width('auto');
        $(obj('wd"++WindowText++"')).height('auto');
        repaintEverything();"});
        _ -> ok
    end;
    true -> ok
    end;

event({window_show,Window}) ->
    Inv=wf:session(investigation),
    WindowText=integer_to_list(Window),
    wf:wire(#script{script="$(obj('contextmenu')).css('display','none');"}),
    WindowId="wi"++WindowText,
    {_,_,WindowData,Children}=tree_get_node(Inv,Window),
    case WindowData#wdata.container of 
        {_,_,_} ->
    WindowBody=try
        ?NITRO_SERVICES:generate_node(WindowData#wdata.container,Window)
    catch
        request_denied -> "Request to database was denied!"
    end,
    StartLine=integer_to_list(
        ?NITRO_SERVICES:get_node_linenum_comment(WindowData#wdata.container)),
    wf:replace("hb"++WindowText,
        #button{id="hb"++WindowText,text="Hide",postback={window_hide,Window}}),
    wf:replace(WindowId,#panel{id=WindowId,body=WindowBody}),
    wf:wire(#script{script=
        "generatelinenums(obj('wi"++integer_to_list(Window)++"'),
            obj('linenums"++integer_to_list(Window)++"'),"++StartLine++");"}),
    wf:session(investigation,
        tree_modify_node(Inv,Window,WindowData#wdata{hidden=false})),
    [rehighlight_nodes(Inv,W,Window) || W <- Children],
    wf:wire(#script{script="repaintEverything();"});
        _ -> ok
    end;
    
event({window_keep,_}) ->
    wf:wire(#script{script="$(obj('contextmenu')).css('display','none');"}),
    cleanup(),
    {StartNode,Query,Text}=wf:session(current_query),
    add_window(empty,wf:session(query_startwindow),StartNode,
               Text++"<br/>"++Query);

event({delete_inv,User,Name}) ->
    ?NITRO_SERVICES:delete_from_itab(User,Name),
    event(load_invs);

event(load_invs) ->
    Who=wf:q(inv_owner),
    Result=case Who of
        "all"->
            ?NITRO_SERVICES:inv_list_from_itab("all");
        _ ->
            ?NITRO_SERVICES:inv_list_from_itab(wf:user())
    end,
    wf:replace(invs_list,
    #table{id=invs_list,class=width_100,rows=lists:map(fun({User,Name})->
        CanDelete=(User==wf:user()) orelse (wf:role(admin)),
        #tablerow{cells=[#tablecell{style="padding:5px",body=
                    #link{body=Name,postback={load_investigation,User,Name}}},
                        #tablecell{align="right",body=if
                            CanDelete ->
                                #link{class=delete_query_a,
                                        text="X",
                                        postback={delete_inv,User,Name}};
                            true -> 
                                #span{class=referl_button_disabled,text="X"}
                            end}]}
                                       end,Result)}).

%%% ============================================================================
%%% Autocomplete

autocomplete_enter_event(SearchTerm, funs) ->
    Funs=case ?NITRO_SERVICES:execute_system_query("mods.funs", 
                                                   {query_display_opt,
                                                    [],
                                                    needed_pattern,$:}) of
        {result, no_result} -> [];
        {result, ResultFuns} -> ResultFuns
    end,    
    DataF=lists:map(fun(Elem)-> 
                           {struct, [{id, list_to_binary(Elem)}, 
                                     {label, list_to_binary(Elem)} , 
                                     {value, list_to_binary(Elem) }]} 
                    end,Funs),
    List = [{struct,[{id, Id }, {label, Label}, {value, Value}]} || 
            {struct,[{id, Id }, {label, Label}, {value, Value}]} <- DataF, 
            string:str(string:to_lower(binary_to_list(Label)), 
                       string:to_lower(SearchTerm)) > 0],
    mochijson2:encode(List);

autocomplete_enter_event(SearchTerm, _Tag) ->
    PosQueries=?NITRO_SERVICES:do_autocomplete(SearchTerm),
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
%%% Handlers for continous events 

continue({continue, put_positions},{{NodeId,Parent,Data,List},NS,Fun,Sum})->
    Res=string:tokens(wf:q(hiddendata), "|"),
    NewSum=case Res of
    [X,Y,W,H] ->
        Sum++[{NodeId,Parent,Data,X,Y,W,H,{0,0},List}];
    [X,Y,W,H,AX,AY] ->
        Sum++[{NodeId,Parent,Data,X,Y,W,H,{AX,AY},List}]
    end,
    put_positions_in_tree_(NS,Fun,NewSum);

continue({continue, jump}, {error,Query,Error}) ->
    main:query_result_handler({Query,0,0},{{result, no_result},
                              {warning,no_warning},{error,Error}});
continue({continue, jump}, {FilteredNodes=[C|_],Query,Type,StartNode}) 
    when is_integer(C)->
   Result=[{group_by,{nopos,"Result"},list,
        [{result,FilteredNodes}]}],
    main:query_result_handler({Query,0,0},{{result, Result},
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
                                  ]}]});
continue({continue, jump}, {FilteredNodes,Query,_,StartNode}) ->
    wf:wire(#script{script="$('#inv_tabs').tabs('select', 1);"}),
    FileList=lists:keysort(2,lists:map(
                fun(X) -> {X, ?NITRO_SERVICES:get_file(X)} end,
                FilteredNodes)),
    GroupedList=main:group_files(FileList),
    Result=lists:map(fun(X) ->
        InnerList=lists:map(fun(Y) ->
                    {{node,?NITRO_SERVICES:get_node_linenum(Y),Y},
                    ?NITRO_SERVICES:node_to_text(Y)}
                end,
                tl(X)),
        {group_by,{nopos,lists:last(string:tokens (hd(X), "/"))},list,InnerList} 
                     end, GroupedList),
    wf:session(query_startwindow,wf:session(current_window)),
    wf:session(current_query,{StartNode,Query,wf:session(nodetext)}),
    main:query_result_handler({Query,0,0},{{result, Result},
                              {warning,no_warning},{error,no_error}}),
    if
        FilteredNodes==[] -> ok;
        true ->
            CNode=?NITRO_SERVICES:get_container(hd(FilteredNodes)),
            add_window(CNode,wf:session(query_startwindow),StartNode,
                       wf:session(nodetext)++"<br/>"++Query),
            wf:wire(#script{script="
                try{$(obj('query_result1')).click();}catch(ex){}"})
    end;
    
continue({continue, nodequery, {Q,StartNode,_}}, timeout) ->
    main:query_result_handler({Q,nodequery,StartNode},timeout);

continue({continue, nodequery, {Query,StartNode,_}}, Result) ->
    wf:session(query_startwindow,wf:session(current_window)),
    wf:session(current_query,{StartNode,Query,wf:session(nodetext)}),
    main:query_result_handler({Query,0,0},Result),
    {{result,R},_,_}=Result,
    if
        R==[] -> ok;
        true ->
            add_window(empty,wf:session(query_startwindow),StartNode,
                       wf:session(nodetext)++"<br/>"++Query)
            %wf:wire(#script{script="
                %try{$(obj('query_result1')).click();}catch(ex){}"})
    end.

%%% ============================================================================
%%% Helper functions

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

%% @doc Show result node in given window
show_result(Node,Window) ->
    {_,NodeT,NodeId}=Node,
    NodeTypeText=atom_to_list(NodeT),
    NodeIdText=integer_to_list(NodeId),
    wf:wire(#script{script="
    try{
        var elem;
        var window=obj('wi"++integer_to_list(Window)++"');
        var elems=document.getElementsByName('jump_"
            ++NodeTypeText++NodeIdText++"');
        for(var i=0;i<elems.length;i++) if(jQuery.contains(window,elems[i])) 
            {elem=elems[i];break;}
        if(elem!=null)
        {
            showcontext=false;
            scrolltonode(elem,window);
            if(elem.tagName=='A')
            {
                $(elem).click();
            }
            else
            {
                $(elem).addClass('selectednode');
                lastcolored=elem;
                selectednode=elem;
                cd.value='"++NodeTypeText++"|"++NodeIdText
                           ++"|"++integer_to_list(Window)++"';
                $(cb).click();
            }
        }
    }catch(ex){}"}).

%% @doc Execute given query from given node
execute_query(Query,{PNode,Node},Type) ->
    wf:wire(#script{script="$('#inv_tabs').tabs('select', 1);"}),
    delete_empty_window(),
    Fun=main:check_query_start(Query,{PNode,Node},none),
    cleanup(),
    wf:replace(query_result_placeholder, main:query_result()),
    wf:replace(executed_query_placeholder,
               #panel{id=executed_query_placeholder,
                      body=[#p{body=?NITRO_CORE:whitespaces_to_html(Query),
                               class="query_title"},
                            #br{},
                            #p{body="Running query, please wait.."},
                            #br{}]}),
   wf:continue({continue, nodequery, {Query,Node,Type}}, Fun, 60*60*1000).

%%% ============================================================================
%%% Window management

%% @doc Add a new investigaton window with ContainerNode's contents.
%% If Parent is not 0, connect it to given parent window's StartNode, 
%% with given label
add_window(CNode,Parent,{_,Type,NId}=StartNode,Label) ->
    wf:session(modified,true),
    NextId=tree_get_lastid(wf:session(investigation))+1,
    IdText=integer_to_list(NextId),
    NodeId="jump_"++atom_to_list(Type)++integer_to_list(NId),
    ParentId="w"++integer_to_list(Parent),
    WindowId="w"++integer_to_list(NextId),
    DivId="wd"++integer_to_list(NextId),
    WindowText=case CNode of 
        {_,_,_} -> 
            try
                ?NITRO_SERVICES:generate_node(CNode,NextId)
            catch
                request_denied -> "Request to database was denied!"
            end;
        {investigation,{User,FileName}} -> 
            [FileName++".inv",
             #br{},
             #link{text="View this investigation",
                   postback={load_investigation,User,FileName}}];
        _ -> "&nbsp;<br />&nbsp;"
    end,
    
    NewDiv=#panel{id=WindowId,class=inv_window,body=#table{rows=[
        #tablerow{class=drg,cells=[
            #tablecell{class=width_15,body=#button{text="Options",
                                    postback={window_options,NextId}}},
            #tablecell{class=width_15,body=#button{id="hb"++IdText,text="Hide",
                                                postback={window_hide,NextId}}},
            #tablecell{body=#label{id="wn"++IdText,
                                   text="Window"++IdText}}]},
        #tablerow{cells=[
            #tablecell{colspan=3,body=#panel{
                       id=DivId,
                       class=inv_window_inner,
                       body=#table{class=[drg,all_100],rows=#tablerow{cells=[
                            #tablecell{body=#panel{id="linenums"++IdText,
                                                   body=[],
                                                   class=inv_lnums}},
                            #tablecell{body=#panel{id="wi"++IdText,
                                                   body=WindowText}}]}}}}]}]}},
    wf:insert_bottom(mainwrapper, NewDiv),
    case CNode of 
        {_,_,_} ->
            StartLine=integer_to_list(
                ?NITRO_SERVICES:get_node_linenum_comment(CNode)),
            wf:wire(#script{script=
            "generatelinenums(obj('wi"++IdText++"'),
            obj('linenums"++IdText++"'),"++StartLine++");"});
        _ -> ok
    end,
    if
        Parent>0 ->
            wf:wire(#script{script="
                positionNewWindow(obj('"++WindowId++"'),obj('"++ParentId++"'));
                jsPlumb.connect({source:obj('"++ParentId++"'),
                                 target:obj('"++WindowId++"'),
                anchors:[ calculateRelativePosition(obj('"++ParentId++"'),
                    '"++NodeId++"'),[0,0.5,-1,0] ],
                overlays:[ [ 'PlainArrow', 
                    { width:20, length:20, location:1, cssClass:'inv_arrow' }],
                    [ 'Label', { label:'"++Label++"', 
                    location:0.6, cssClass:'inv_label' } ]]});
                    repaintEverything();"});
        true -> wf:wire(#script{script="
                $(obj('"++WindowId++"')).css('left',10);
                $(obj('"++WindowId++"')).css('top',30);"})   
    end,
    
    wf:wire(#script{script="
        setDragResize(obj('"++WindowId++"'),obj('"++DivId++"'),true);"}),
    
    WindowData=#wdata{label=Label,startnode=StartNode,container=CNode,
                      text=WindowText,name="Window"++integer_to_list(NextId)},
    wf:session(investigation,tree_add_node(wf:session(investigation),
        Parent,WindowData)).
        
add_window_custom(CNode,Parent,{_,Type,NId}=StartNode,Label,
                  Id,X,Y,W,H,Text,Hidden,Name,Ann,{AX,AY}) ->
    IdText=integer_to_list(Id),
    NodeId="jump_"++atom_to_list(Type)++integer_to_list(NId),
    ParentId="w"++integer_to_list(Parent),
    WindowId="w"++IdText,
    DivId="wd"++IdText,
    {CanHide,WindowText}=case CNode of 
        {_,_,_} -> 
            try
                TypeTest=?NITRO_SERVICES:get_type(CNode),
                if
                    TypeTest==error -> 
                        {deny,Text};
                    Hidden -> 
                        {Hidden,?NITRO_SERVICES:node_to_text(CNode)};
                    true -> 
                        {Hidden,?NITRO_SERVICES:generate_node(CNode,Id)}
                end
            catch
                _ -> {deny,Text}
            end;
        {investigation,{User,FileName}} -> 
            {Hidden,
            [FileName++".inv",
             #br{},
             #link{text="View this investigation",
                   postback={load_investigation,User,FileName}}]};
        _ -> {Hidden,"&nbsp;"}
    end,
    HideButton=if
        CanHide/=true -> #tablecell{class=width_15,
                             body=#button{id="hb"++IdText,
                                          text="Hide",
                                          postback={window_hide,Id}}};
        true -> #tablecell{class=width_15,
                           body=#button{id="hb"++IdText,
                                        text="Show",
                                        postback={window_show,Id}}}
    end,
    NewDiv=#panel{id=WindowId,class=inv_window,body=#table{rows=[
        #tablerow{class=drg,cells=[
            #tablecell{class=width_15,
                     body=#button{text="Options",postback={window_options,Id}}},
                     HideButton,
            #tablecell{body=#label{id="wn"++IdText,text=Name}}]},
        #tablerow{cells=[
            #tablecell{colspan=3,body=#panel{
                       id=DivId,
                       class=inv_window_inner,
                       body=#table{class=[drg,all_100],rows=#tablerow{cells=[
                            #tablecell{body=#panel{id="linenums"++IdText,
                                                   body=[],
                                                   class=inv_lnums}},
                            #tablecell{body=#panel{id="wi"++IdText,
                                                   body=WindowText}}]}}}}]}]}},
    wf:insert_bottom(mainwrapper, NewDiv),
    case CNode of 
        {_,_,_} ->
            StartLine=integer_to_list(
                ?NITRO_SERVICES:get_node_linenum_comment(CNode)),
            wf:wire(#script{script=
            "generatelinenums(obj('wi"++IdText++"'),
            obj('linenums"++IdText++"'),"++StartLine++");"});
        _ -> ok
    end,
    wf:wire(#script{script="
        $(obj('"++WindowId++"')).width("++W++");
        $(obj('"++WindowId++"')).height("++H++");
        $(obj('"++DivId++"')).width("++W++"-20);
        $(obj('"++DivId++"')).height("++H++"-24);
        $(obj('"++DivId++"')).css('max-height',"++H++");
        $(obj('"++WindowId++"')).css('left',"++X++");
        $(obj('"++WindowId++"')).css('top',"++Y++");"}),
    if
        Parent>0 ->
            wf:wire(#script{script="
                jsPlumb.connect({source:obj('"++ParentId++"'),
                                 target:obj('"++WindowId++"'),
                anchors:[ calculateRelativePosition(obj('"++ParentId++"'),
                    '"++NodeId++"'),[0,0.5,-1,0] ],
                overlays:[ [ 'PlainArrow', 
                { width:20, length:20, location:1, cssClass:'inv_arrow' }],
                [ 'Label', { label:'"++Label++"', 
                    location:0.5, cssClass:'inv_label' } ]]});"});
        true -> ok       
    end,
    
    wf:wire(#script{script="
        setDragResize(obj('"++WindowId++"'),obj('"++DivId++"'),true);"}),
    
    WindowData=#wdata{label=Label,startnode=StartNode,container=CNode,
                      text=Text,hidden=CanHide,name=Name,ann=Ann},
    wf:session(investigation,tree_add_node_(wf:session(investigation),
        Parent,Id,WindowData)),
    if
        Ann /="" -> 
            AnnId="ann"++IdText,
            add_ann_window(Id,Ann),
            wf:wire(#script{script="
            $(obj('"++AnnId++"')).css('left',"++AX++");
            $(obj('"++AnnId++"')).css('top',"++AY++");
            jsPlumb.repaint($(obj('"++AnnId++"')));"});
        true -> ok
    end.


add_ann_window(Parent,Text) ->
    ParentId="w"++integer_to_list(Parent),
    WindowId="ann"++integer_to_list(Parent),
    NewDiv=#panel{id=WindowId,class=inv_annwindow,
                  body=#panel{id="anni"++integer_to_list(Parent),
                              class=drg,
                              body=Text}},
    wf:insert_bottom(mainwrapper, NewDiv),
    wf:wire(#script{script="
        $(obj('"++ParentId++"')).attr('name',"++integer_to_list(Parent)++");
        positionNewWindow(obj('"++WindowId++"'),obj('"++ParentId++"'));
                jsPlumb.connect({source:obj('"++ParentId++"'),
                                 target:obj('"++WindowId++"'),
                anchors:[ [0.5,1,0,1],[0.5,0,0,-1] ],overlays: [[ 'PlainArrow', 
                { width:1, length:1, location:1, cssClass:'inv_arrow' }]]});
        setDragResize(obj('"++WindowId++"'),obj('"++WindowId++"'),false);"}).

%% @doc Delete window that has given id
delete_window(Window) ->
    cleanup(),
    WindowId="w"++integer_to_list(Window),
    {_,Parent,Data,_}=tree_get_node(wf:session(investigation),Window),
    wf:session(investigation,
               tree_delete_node(wf:session(investigation),Window)),
    if
        Parent/=0 ->
            {_,_,_,Children}=tree_get_node(wf:session(investigation),Parent),
            StartNodes=[(tree_get_node_data(
                        wf:session(investigation),W))#wdata.startnode 
                            || W <- Children],
            RemoveHighLight=not lists:member(Data#wdata.startnode,StartNodes),
            ParentId="wi"++integer_to_list(Parent),
            {_,Type,NId}=Data#wdata.startnode,
            NodeId="jump_"++atom_to_list(Type)++integer_to_list(NId),
            if
                RemoveHighLight ->
                    wf:wire(#script{script="
                    nodes=document.getElementsByName('"++NodeId++"');
                    for(var i=0;i<nodes.length;i++) 
                        if(jQuery.contains(obj('"++ParentId++"'),nodes[i])) 
                        {
                            node=nodes[i];
                            break;
                        }
                    $(node).removeClass('inv_startnode');"});
                true -> ok
            end,
            wf:wire(#script{script=
                "jsPlumb.detachAllConnections($(obj('"++WindowId++"')));"});
    true -> ok
    end,
    wf:remove(WindowId).

%% @doc Remove all windows and connections
reset_windows() ->
    wf:session(investigation,[]),
    wf:session(current_window, undefined),
    wf:session(query_startwindow, undefined),
    wf:session(current_query, undefined),
    cleanup(),
    wf:replace(executed_query_placeholder,
               #panel{id=executed_query_placeholder}),
    wf:replace(query_result_placeholder,
               #panel{id=query_result_placeholder}),
    wf:replace(mainwrapper,#panel{id=mainwrapper,
        body=#span{id=contextmenu,text="",class=contextmenu}}),
    wf:wire(#script{script="jsPlumb.detachEveryConnection();czoom=1;zoom=1;"}).

%% @doc Create windows from given list
put_windows([]) -> ok;
put_windows([{Id,Parent,Data,X,Y,W,H,AnnPos,_}|NS]) ->
    add_window_custom(Data#wdata.container,Parent,Data#wdata.startnode,
                     Data#wdata.label,Id,X,Y,W,H,Data#wdata.text,
                     Data#wdata.hidden,Data#wdata.name,Data#wdata.ann,AnnPos),
    put_windows(NS).
    

%%% ============================================================================
%%% Functions for tree data structure 

%% @doc Add node with given id and data to tree
tree_add_node(Tree,Id,Data) -> 
    tree_add_node_(Tree,Id,tree_get_lastid(Tree)+1,Data).
tree_add_node_([],_,Id,Data) -> [{Id,0,Data,[]}];
tree_add_node_([{NodeId,Parent,NodeData,NodeList}|NS],Id,NewId,Data) 
when Id==NodeId ->
    NewNodeList=NodeList++[NewId],
    [{NodeId,Parent,NodeData,NewNodeList}|NS++[{NewId,NodeId,Data,[]}]];
tree_add_node_([N|NS],Id,NewId,Data) -> [N|tree_add_node_(NS,Id,NewId,Data)].

%% @doc Get node with given id from tree
tree_get_node([],_) -> {0,0,notfound,[]};
tree_get_node([{NodeId,_,_,_}=N|_],Id) when Id==NodeId -> N;
tree_get_node([_|NS],Id) -> tree_get_node(NS,Id).

%% @doc Get data of node with given id from tree
tree_get_node_data([],_) -> #wdata{container=notfound};
tree_get_node_data([{NodeId,_,Data,_}|_],Id) when Id==NodeId -> Data;
tree_get_node_data([_|NS],Id) -> tree_get_node_data(NS,Id).

%% @doc Get all id numbers in tree
tree_get_ids(Tree) -> tree_get_ids_(Tree,[]).
tree_get_ids_([],Ids) -> lists:reverse(Ids);
tree_get_ids_([{Id,_,_,_}|NS],Ids) -> tree_get_ids_(NS,[Id|Ids]).

%% @doc Get highest id number in tree
tree_get_lastid(Tree) -> tree_get_lastid_(Tree,0).
tree_get_lastid_([],Max) -> Max;
tree_get_lastid_([{Id,_,_,_}|NS],Max) when Id>Max -> tree_get_lastid_(NS,Id);
tree_get_lastid_([_|NS],Max)-> tree_get_lastid_(NS,Max).

%% @doc Delete node with given id from tree, removing all connections as well
tree_delete_node(Tree,Id) ->
    {_,Parent,_,_}=tree_get_node(Tree,Id),
    tree_delete_node_(Tree,Id,Parent).
tree_delete_node_([],_,_) -> [];
tree_delete_node_([{NodeId,_,_,_}|NS],Id,Parent) when Id==NodeId -> 
    tree_delete_node_(NS,Id,Parent);
tree_delete_node_([{NodeId,NParent,NodeData,NodeList}|NS],Id,Parent) 
when Parent==NodeId -> 
    NewList=lists:delete(Id,NodeList),
    [{NodeId,NParent,NodeData,NewList}|tree_delete_node_(NS,Id,Parent)];
tree_delete_node_([N|NS],Id,Parent) -> [N|tree_delete_node_(NS,Id,Parent)].

%% @doc Modify data in node with given id in tree
tree_modify_node([],_,_) -> [];
tree_modify_node([{NodeId,Parent,_,List}|NS],Id,NewData) when Id==NodeId -> 
    NewNode={NodeId,Parent,NewData,List},
    [NewNode|tree_modify_node(NS,Id,NewData)];
tree_modify_node([N|NS],Id,NewData) -> [N|tree_modify_node(NS,Id,NewData)].

%% @doc Get subtree starting from given node in tree
tree_get_subtree(Tree,Id) -> 
    {_,_,Data,NodeList}=tree_get_node(Tree,Id),
    [{Id,0,Data,NodeList}|tree_get_subtree_(Tree,NodeList,[])].
tree_get_subtree_([],_,NewTree) -> NewTree;
tree_get_subtree_([{NodeId,_,_,List}=N|NS],Ids,NewTree) ->
    IsMember=lists:member(NodeId,Ids),
    if
        IsMember -> tree_get_subtree_(NS,Ids++List,NewTree++[N]);
        true -> tree_get_subtree_(NS,Ids,NewTree)
    end.
    
%% @doc Delete subtree starting from given node in tree excluding that node
tree_delete_subtree(Tree,Id) ->
    {_,_,_,NodeList}=tree_get_node(Tree,Id),
    tree_delete_subtree_(Tree,NodeList,Tree).
tree_delete_subtree_(_,[],NewTree) -> NewTree;
tree_delete_subtree_([{NodeId,_,_,List}|NS],Ids,NewTree) ->
    IsMember=lists:member(NodeId,Ids),
    if
        IsMember -> tree_delete_subtree_(NS,lists:delete(NodeId,Ids)
                    ++List,tree_delete_node(NewTree,NodeId));
        true -> tree_delete_subtree_(NS,Ids,NewTree)
    end.

%%% ============================================================================
%%% Helper functions

%% @doc Delete the empty window if there is one
delete_empty_window() ->
    LastId=tree_get_lastid(wf:session(investigation)),
    #wdata{container=CN}=tree_get_node_data(wf:session(investigation),LastId),
    if
        CN==empty ->
            delete_window(LastId);
        true ->
            ok
    end.

%% @doc Put window position information in nodes
put_positions_in_tree(Tree,Fun) ->
    put_positions_in_tree_(Tree,Fun,[]).
put_positions_in_tree_([],Fun,Sum) -> Fun(Sum);
put_positions_in_tree_([{NodeId,_,_,_}=N|NS],Fun,Sum) ->
    #wdata{ann=Ann}=tree_get_node_data(wf:session(investigation),NodeId),
    ParentId="w"++integer_to_list(NodeId),
    if
        Ann=="" ->
            wf:wire(#script{script="
                x=parseInt($(obj('"++ParentId++"')).css('left'),10);
                y=parseInt($(obj('"++ParentId++"')).css('top'),10);
                w=$(obj('"++ParentId++"')).width();
                h=$(obj('"++ParentId++"')).height();
                obj('hiddendata').value=x+'|'+y+'|'+w+'|'+h;"});
        true ->
            AnnId="ann"++integer_to_list(NodeId),
            wf:wire(#script{script="
                x=parseInt($(obj('"++ParentId++"')).css('left'),10);
                y=parseInt($(obj('"++ParentId++"')).css('top'),10);
                w=$(obj('"++ParentId++"')).width();
                h=$(obj('"++ParentId++"')).height();
                ax=parseInt($(obj('"++AnnId++"')).css('left'),10);
                ay=parseInt($(obj('"++AnnId++"')).css('top'),10);
                obj('hiddendata').value=x+'|'+y+'|'+w+'|'+h+'|'+ax+'|'+ay;"})
    end,
    wf:continue({continue,put_positions},fun()->{N,NS,Fun,Sum} end, 60*60*1000).

%% @doc Highlight W's startnode in StartW
rehighlight_nodes(Inv,W,StartW) ->
    #wdata{startnode={_,T,Id}}=tree_get_node_data(Inv,W),
    wf:wire(#script{script="
        var elem=null;
        var window=obj('wi"++integer_to_list(StartW)++"');
        var elems=document.getElementsByName('jump_"++
            atom_to_list(T)++integer_to_list(Id)++"');
        for(var i=0;i<elems.length;i++) if(jQuery.contains(window,elems[i])) 
            {elem=elems[i];break;}
        $(elem).addClass('inv_startnode');"}).

%% @doc Get text from textbox, if empty set it to default
getnametext(Id,Default) ->
    Text=wf:q(Id),
    if
        Text==undefined orelse Text==[] -> Default;
        true -> Text
    end.

%% @doc Creates a popup window with specified text, that executes the 
%% given function if 'Yes' is clicked 
make_popup_window(Text,Fun) ->
    IsModified=wf:session(modified),
    if
        IsModified==true ->
            wf:session(modified,popup),
            PopUp=#panel{id=popup,body=[#panel{class=popup_restrict},
            #panel{class=[popup,centered],
                body=[#label{text="Warning!"},#br{},#label{text=Text},
            #button{text="Yes",postback={popup_yes,Fun}},
            #button{text="No",postback=popup_no}]}]},
            wf:insert_bottom(otherdiv, PopUp);
        true -> Fun()
    end.

%% @doc Checks if given name would overwrite an existing investigation
overwrites_inv(Name) ->
    Current=wf:session(current_inv),
    Res=?NITRO_SERVICES:get_from_itab(wf:user(),Name),
    if
        Res==not_found -> false;
        Current==Name -> false;
        true -> true
    end.
    
overwrites_inv(Name,even_current) ->
    Res=?NITRO_SERVICES:get_from_itab(wf:user(),Name),
    if
        Res==not_found -> false;
        true -> true
    end.
