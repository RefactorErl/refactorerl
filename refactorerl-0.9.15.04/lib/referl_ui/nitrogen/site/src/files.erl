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

-module (files).
-compile(export_all).
-include_lib("referl_ui/include/nitrogen.hrl").
-include_lib("nitrogen/include/wf.hrl").
-include_lib("kernel/include/file.hrl").
-include("records.hrl").
-vsn("$Rev: 9568 $ ").
-define(PRINTER_ID, nitrogen_printer).

%%% ============================================================================
%%% HTML output

main() ->
    start_progress_printer(),
    nitrogen_lib:main("files.html").

title() -> "RefactorErl Queries".

logout()->
    nitrogen_lib:logout_link(wf:user()).

get_server_root_dir()->
    TL= ?NITRO_CORE:get_file_browser_server_root(),
    if 
        length(TL) == 1 -> TL;
        true -> "__multi_dirs__"
    end.

get_db_root_dir()->
    TL= ?NITRO_CORE:get_file_browser_loaded_files_root(),
    if 
        length(TL) == 1 -> TL;
        true -> "__multi_dirs__"
    end.

env_nodes()->
    case wf:role(admin) of
        true ->
             #panel{id=envs_placeholder, body=#link{id=show_envs_link, 
               text="List all of the 'appbase' environment nodes", 
               postback=show_envs, 
               class="env_link"}};
        false -> "You don't have permission to access this page."
    end.

show_file_section()->
    #panel{id=show_sorce_panel, body=""}.

browser_type_select()->
    case wf:role(admin) of
    true ->
    Options=case wf:session(browser_type_dropdown) of
                undefined ->[#option{ text="Browse server", 
                                      value="server", 
                                      selected=true },
                             #option{ text="Browse loaded files", 
                                      value="db", 
                                      selected=false }];
                "server"  ->[#option{ text="Browse server", 
                                      value="server", 
                                      selected=true },
                             #option{ text="Browse loaded files", 
                                      value="db", 
                                      selected=false }];
                "db"      ->[#option{ text="Browse server", 
                                      value="server", 
                                      selected=false },
                             #option{ text="Browse loaded files", 
                                      value="db", 
                                      selected=true }]
            end,
    wf:set(browser_type_dropdown,"server"),
    InnerCell=[#panel{id=buttons_placeholder,
                      body=[#link{id=show_file_button, 
                                  body=[#image{image="/images/show_source.png", 
                                               alt="Show source", 
                                               class=no_decoration}], 
                                  postback=show_file,
                                  title = "Show source",
                                  class=no_decoration},
                            #link{id=add_file_button, 
                                  body=[#image{image="/images/database_add.png", 
                                               alt="Add file", 
                                               class=[need_db,no_decoration]}], 
                                  postback={add_file,add},
                                  title = "Add file to database",
                                  class=no_decoration, 
                                  actions=get_progress_event(
                                            "Adding file(s) to database ..")},
                            #link{id=sync_file_button, 
                                  body=[#image{image="/images/reload.png", 
                                               alt="Reload", 
                                               class=[need_db,no_decoration]}], 
                                  postback={add_file,sync},
                                  title = "Reload file to database",
                                  class=no_decoration,
                                  actions=get_progress_event(
                                           "Reloading file(s) to database ..")},
                            #link{id=delete_file_button, 
                                  body=[#image{image=
                                                  "/images/database_remove.png", 
                                               alt="Drop file", 
                                               class=[need_db,no_decoration]}], 
                                  postback=delete_file, 
                                  title = "Drop file from database",
                                  class=no_decoration},
                            #link{id=set_env_button, 
                                  body=[#image{image="/images/set_env.png", 
                                               alt="Add as an appbase node", 
                                               class=[need_db,no_decoration]}], 
                                  postback=add_env,
                                  title = "Add as an appbase node",
                                  class=no_decoration},
                            #link{id=generate_button, 
                                  body=[#image{image="/images/show_links.png", 
                                               alt="Generate html", 
                                               class=[need_db,no_decoration]}], 
                                  postback=generate,
                                  title = "Generate html",
                                  class=no_decoration},
                            #link{id=db_sync_button, 
                                 body=[#image{image="/images/database_sync.png", 
                                               alt="Synchronize database", 
                                               class=[need_db,no_decoration]}], 
                                  postback=db_sync,
                                  title = "Synchronize database",
                                  class=no_decoration,
                                  actions=get_progress_event(
                                            "Synchronizing database ..")},
                            #panel{id=delete_confirm}]}],
    Body=#table{rows=[#tablerow{
                            cells=[#tablecell{body=[
                                        #dropdown{id=browser_type_dropdown,
                                                  options=Options,
                                                  html_encode=true,
                                                  postback=load_file_browser}]},
                                   #tablecell{body=InnerCell}]}]},
    Body;
    false -> #br{}
    end.

message_panel()->
    MessageBox=[#panel { class=[flash_content, centered],
                        id=progress_message_panel,
                        body=""}, 
               #panel { class=[flash_content, centered],
                        id=progress_panel,
                        body="<div id='progressbar'></div>" }],
    StatusRow = #tablerow{cells=[#tablecell{body=[#panel{class=[flash_content, 
                                                                centered, bold],
                                                          id=wait_message_panel, 
                                                          body="" }
                                                  ]++MessageBox}]},
    
    InnerPanel = #panel { id=wait_message_flash_inner, 
                          class=flash, 
                          actions=#show { target=wait_message_flash, 
                                          effect=blind, 
                                          speed=10 }, 
                          body=[#link{class=flash_close_button, 
                                      text="Close", 
                                      actions=#event{ type=click, 
                                                      target=wait_message_flash, 
                                                      actions=
                                                         #hide{effect=blind,
                                                               speed=400}}},
                                #table{rows=[StatusRow]}]},
    WaitPanel=#panel { id=wait_message_flash, 
                       style="display:none;", 
                       body=InnerPanel},
    wf:wire(wait_message_flash_inner, #hide{}),
    wf:wire(wait_message_flash, #hide{}),
    
    case wf:session(flash_message) of
        undefined -> ok;
        Message   -> wf:session(flash_message,undefined),
                     wf:continue({continue, message},fun()->Message end,
                        60*60*1000)
    end,
    [WaitPanel,#panel{id=message_panel,body=[#flash{}]}].

file_browser()->
    case wf:role(admin) of
    true ->    
    Body=[#panel{id=file_browser_placeholder,
           body=[#checkbox{id=ext_checkbox,text="Show only .erl and .hrl files",
                           class="ext_checkbox"},
                "<br />Filter: ",
                #textbox{id=file_filter,text="",class="file_filter"},
                #panel{id=file_browser_db_panel,
                        body="<div id='file_browser_db'></div>"},
                #panel{id=file_browser_server_panel,
                        body="<div id='file_browser_server'></div>"}]}],
    case wf:session(browser_type_dropdown) of
        undefined -> show_panel("server");
        Panel -> show_panel(Panel)
    end,
    Body;
    false -> #br{}
    end.

%%% ============================================================================
%%% Handlers for postback events    

continue({continue, message}, Message) ->
    wf:wire(#notification{type=notification, text=Message});

continue({continue, generate}, {Files,Fun}) ->
    Count=length(Files),
    CurCount=wf:session(gencount),
    wf:session(gencount,wf:session(gencount)+1),
    if
        CurCount==Count ->
           need_to_update("Successfully generated "++wf:session(selected_file));
        true ->
            Fun({"Generating: "++lists:nth(CurCount+1,Files)++
            " ("++integer_to_list(CurCount)++"/"++
            integer_to_list(Count)++")",round(wf:session(gencount)/Count*100)})
    end.

count(File,List) ->
    IsFile=case file:read_file_info(File) of
                {ok, #file_info{type=directory}} -> 
                    false;
                {error,_} -> 
                     E=filename:extension(File),
                     if
                        E=="" -> false;
                        true -> true
                     end;
                {_, _} ->
                    true
           end,
    case IsFile of
        false -> 
            lists:foldr(fun(X,S) -> 
                                X++S 
                        end,
                        [],
                        lists:map(fun(X) ->
                                          count(filename:join(File,X),List) 
                                  end,
                    ?NITRO_CORE:get_loaded_files_list(File)));
        true ->
            [File]
    end.

event(generate)->
    File=wf:session(selected_file),
    case File of
        undefined -> 
             wf:wire(#notification{type=warning, 
                text="Please select a file first!"});
        _ ->
            Files=count(File,[]),
            wf:wire(wait_message_flash_inner, #show{}),
            wf:wire(wait_message_flash, #show{}),
            PFun=start_progress_printer(),
            wf:session(gencount,1),
            PFun({"Generating: "++hd(Files)++" (1/"++
            integer_to_list(length(Files))++")",
                round(wf:session(gencount)/length(Files)*100)}),
            lists:map(fun(X) ->
                Fun=fun() ->referl_htmlserver:generate_call(X),{Files,PFun} end,
                wf:continue({continue, generate}, Fun, 60*60*1000) end,Files)
    end;    

event(db_sync)->
    wf:wire(wait_message_flash_inner, #show{}),
    wf:wire(wait_message_flash, #show{}),
    PrinterFun=start_progress_printer(),
    case ?NITRO_SERVICES:synchronize(PrinterFun) of
        {result, _}->
            wf:wire(wait_message_flash, #hide{effect=blind, speed=400}),
            wf:wire(#notification{type=notification, 
                text="Successfully synchronized the database."});
        {error, Error} ->
            wf:wire(wait_message_flash, #hide{effect=blind, speed=400}),
            need_to_update(Error)
    end;

event(delete_file)->
    wf:replace(delete_confirm,
    #table{id=delete_confirm,
           rows=[#tablerow{cells=#tablecell{colspan=2,body="Are you sure?"}},
                 #tablerow{cells=[
                    #tablecell{body=#button{text="Yes", 
                                        postback=delete_confirm,
                                        actions=get_progress_event(
                                          "Dropping file(s) from database..")}},
                    #tablecell{body=#button{text="No", postback=delete_cancel}}
                                 ]}]});

event(delete_cancel)->
    wf:replace(delete_confirm,#panel{id=delete_confirm});

event(delete_confirm) ->
    File=wf:session(selected_file),
    case File of
        undefined -> wf:wire(#notification{type=warning, 
                     text="Please select a file first."});
        _ ->wf:wire(wait_message_flash, #hide{effect=blind, speed=400}),
            IsFile=case file:read_file_info(File) of
                        {ok, #file_info{type=directory}} -> 
                            false;
                        {error,_} -> 
                             E=filename:extension(File),
                             if
                                E=="" -> false;
                                true -> true
                             end;
                        {_, _} ->
                            true
                   end,
            if
                not IsFile ->
                    lists:map(fun(X)->
                            wf:session(selected_file,filename:join(File,X)),
                            event(delete_confirm)
                        end,
                        ?NITRO_CORE:get_loaded_files_list(File)),
                    wf:wire(wait_message_flash, #hide{effect=blind, speed=400}),
                    need_to_update(
                                         "Successfully dropped the folder "++
                                             File++
                                             " from database.");
                IsFile ->
                    PrinterFun=start_progress_printer(),
                    case ?NITRO_SERVICES:drop_from_db(File,
                            PrinterFun) of
                         {result, _}-> need_to_update(
                                         "Successfully dropped "++
                                             File++
                                             " from database.");
                         {error, Error} ->
                            wf:wire(#notification{type=error,text=Error})
                     end
            end
    end;

event({add_file,Event})->
    File=wf:session(selected_file),
    case File of
        undefined -> wf:wire(#notification{type=warning, 
                        text="Please select a file first."});
        _ ->wf:wire(wait_message_flash, #hide{effect=blind, speed=400}),
            PrinterFun=start_progress_printer(),
            case ?NITRO_SERVICES:add_to_db(File, PrinterFun) of
                 {result, _}->    
                     case Event of
                         add -> need_to_update("Successfully added "++
                                                   File++
                                                   " to database.");
                         sync ->wf:wire(#notification{type=notification, 
                          text="Successfully reloaded "++File++" to database."})
                     end;
                {error, Error} ->need_to_update(Error)
             end
    end;

event(show_file)->
    File=wf:session(selected_file),
    case File of
        undefined -> wf:wire(#notification{type=warning, 
                          text="Please select a file first."});
        _ ->
            case ?NITRO_SERVICES:get_source(File) of
            {data, Data} ->
                wf:replace(show_sorce_panel,
                           #panel{id=show_sorce_panel,
                                  body=[#p{body=File, class="query_title"},
                                        #textarea{id=source_code_textarea,
                                                  text=Data,
                                                  html_encode=true,
                                                  class="source_code_ta"}]}),
                wf:wire(#resize_textarea { targetId=source_code_textarea, 
                                           maxWidth=80});
            {error, Error} ->
                wf:replace(show_sorce_panel,#panel{id=show_sorce_panel,
                                                   body=[#p{id=error_in_load,
                                                            body=Error,
                                                            class="error"}]})
            end
    end;
    
event(load_file_browser)->
    case wf:session(disablethings) of
        undefined -> show_panel(wf:q(browser_type_dropdown));
        true -> ok
    end;

event(show_envs)->
    case wf:session(disablethings) of
    undefined -> 
        Envs=?NITRO_SERVICES:get_appbases(),
        T=case Envs of
            {error,Err} ->
                #panel{id=envs_table,class="query_result_table",
                       body="<br />"++Err};
            _ -> #table{id=envs_table,
               class="query_result_table",
               rows=lists:foldl(fun(Path,Acc)->
                    Cell1=#tablecell{body=#p{body=Path},
                                    align="left",
                                    valign="middle",
                                    class=["query_result_tablecell",
                                            "text_85"]},
                    Link=#link {text="X",
                                class="delete_query_a", 
                                title = "Delete the value of the appbase node", 
                                postback={del_env,
                                          Path}},
                    Cell2=#tablecell{body=Link,
                                    align="left",
                                    valign="middle",
                                    class="query_result_tablecell"},
                    Acc++[#tablerow{cells=[Cell1,Cell2]}]
                                end, [], Envs),
             style="display:none;"}
        end,
        
        wf:insert_bottom(envs_placeholder,T),


        HideEnvsLink=#link{id=hide_envs_link,
                           text="Hide appbase nodes", 
                           postback=hide_envs, 
                           class="env_link"},
        wf:replace(show_envs_link,HideEnvsLink),
        wf:wire(envs_table, #appear{speed=100});
    true -> ok
    end;

event(hide_envs)->
    ShowEnvsLink=#link{id=show_envs_link, 
                       text="List all of the appbase nodes", 
                       postback=show_envs, 
                       class="env_link"},
    wf:replace(hide_envs_link,ShowEnvsLink),
    wf:wire(envs_table, #fade{speed=100}),
    wf:remove(envs_table);

event({del_env, Path})->
    case ?NITRO_SERVICES:del_appbase(Path) of
        ok -> wf:wire(#notification{type=notification, 
                    text="Successfully deleted "++Path++
                         " from the values of the appbase node."});
        not_found ->wf:wire(#notification{type=notification, 
                        text=Path++" is not set as an appbase node."});
        {error, Message} -> wf:wire(#notification{type=error, text=Message})
    end,
    wf:replace(envs_placeholder,env_nodes());

event(add_env)->
    File=wf:session(selected_file),
    case File of
        undefined -> wf:wire(#notification{type=warning, 
                text="Please select a file first."});
        _ ->
            case ?NITRO_SERVICES:add_appbase(File) of
                ok ->wf:wire(#notification{type=notification, 
                     text=File++" is added as an appbase node successfully."});
                {error,Error} -> wf:wire(#notification{type=error, text=Error})
             end
    end,
    wf:replace(envs_placeholder,env_nodes());

event(logout) ->
    nitrogen_lib:logout(wf:user()).

%%% ============================================================================
%%% Hepler functions
show_panel("server")->
    wf:wire(file_browser_db_panel, #hide {}),
    wf:wire(file_browser_server_panel, #show {}),
    wf:wire(sync_file_button, #hide {}),
    wf:wire(delete_file_button, #hide {}),
    wf:wire(add_file_button, #show {}),
    wf:wire(show_file_button, #show {}),
    wf:wire(generate_button, #hide {}),
    wf:wire(db_sync_button, #hide {});

show_panel("db")->
    wf:wire(file_browser_server_panel, #hide {}),
    wf:wire(file_browser_db_panel, #show {}),
    wf:wire(sync_file_button, #show {}),
    wf:wire(delete_file_button, #show {}),
    wf:wire(add_file_button, #hide {}),
    wf:wire(show_file_button, #show {}),
    wf:wire(generate_button, #show {}),
    wf:wire(db_sync_button, #show {}).

need_to_update(Message)->
    case ?NITRO_SERVICES:need_to_update() of
        false -> ok;
        true -> wf:session(flash_message,Message),
                wf:session(browser_type_dropdown,wf:q(browser_type_dropdown)),
                wf:redirect("/files")
    end.

get_progress_event(Label)->
    Event = #event { target=wait_message_panel, type=click },
    Actions=[#show{target=wait_message_flash_inner, speed=0},
             #show{target=spinner_progress, speed=0},
             #show {target=wait_message_flash, effect=blind, speed=10 },
             #set{value=[Label]}],
    Event#event{actions=Actions}.

start_progress_printer()->
    {ok,Pid}=wf:comet(fun() -> progress_loop() end, ?PRINTER_ID),
    fun(Data)-> Pid!Data end.

progress_loop()->
    receive 
        {Data=[C|_], Percent} when is_integer(C)->
            wf:wire(#script{script="$('#progressbar').progressbar({ value: "
                            ++io_lib:format("~p",[round(Percent)])++" });"}),
            Panel=#panel { class=flash_content,
                     id=progress_message_panel,
                      body="<p>"++Data++"</p>" }  ,          
            wf:replace(progress_message_panel, Panel),
            wf:flush(),
            progress_loop();

         _->progress_loop()
    end.
