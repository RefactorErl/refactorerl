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

-module(referl_ui_nitrogen_core).

-export([start_nitrogen/0, start_nitrogen/1, stop_nitrogen/0]).
-export([get_nitrogen_index/0, get_nitrogen_root/0, 
         get_nitrogen_apps_dir/0, get_nitrogen_site_dir/0]).
-export([get_file_browser_server_root/0, get_images_root/0, 
         get_restricted_mode/0, get_file_browser_loaded_files_root/0]).
-export([get_loaded_files_list/1, get_loaded_directories/0]).
-export([whitespaces_to_html/1]).
-export([make_ui_request/1, make_ui_request/2]).

-define(NROOT,"nitrogen").
-define(NSITE,?NROOT++"/site").
-define(NAPPS,?NROOT++"/apps").

-define(LocalA(T,E,A), {T,{?LocalError(E,A), 
                           ?Error:error_text(?LocalError(E,A))}}).
-define(LocalError2(E,A), ?LocalA(error,E,A)).
-define(LocalAbort2(E,A), ?LocalA(abort,E,A)).
-vsn("$Rev: 13083 $ ").

-include("ui.hrl").
-include_lib("referl_core/include/core_export.hrl").

%%% ============================================================================
%%% Start, configure and stop nitrogen framework

%% @doc Starts nitrogen framework
start_nitrogen() ->
    start_nitrogen([]).
    
%% @doc Starts nitrogen framework
start_nitrogen(["from_script", YPath, YName, YPort, YListen, 
               ImgRoot, RestrMode | ["no_path"]]) ->
    start_nitrogen(YPath, YName, YPort, YListen, "no_path", 
		   ImgRoot, RestrMode);

start_nitrogen(["from_script", YPath, YName, YPort, YListen, 
               ImgRoot, RestrMode | ["no_path" | BrowserRoot]]) ->
    start_nitrogen(YPath, YName, YPort, YListen, BrowserRoot,
		   ImgRoot, RestrMode);

start_nitrogen(PropList) ->
    YPort = proplists:get_value(yaws_port,PropList,"8001"),
    YListen = proplists:get_value(yaws_listen,PropList,"127.0.0.1"),
    YName = proplists:get_value(yaws_name,PropList,"localhost"),
    YPath = proplists:get_value(yaws_path,PropList,"no_path"),
    BRoot = proplists:get_value(browser_root,PropList,"no_path"),
    IRoot = proplists:get_value(images_dir,PropList,"no_path"),
    ImgRoot=case IRoot of
            "no_path" -> mnesia:system_info(directory);
            Dir -> Dir
            end,
    RestrictedMode = proplists:get_value(restricted_mode,PropList,false),
    start_nitrogen(YPath, YName, YPort, YListen, BRoot, ImgRoot,
        RestrictedMode).

start_nitrogen(YPath, YName, YPort, YListen, BrowserRoot, ImgRoot, RestrMode)->
    try    
        ?NITRO_HELPER:load_needed(),
        ?NITRO_HELPER:init_fbtab(),
        ?NITRO_HELPER:init_rqtab(),
        ?NITRO_HELPER:set_file_browser_docroot(BrowserRoot),
        ?NITRO_HELPER:set_image_root(ImgRoot),
        ?NITRO_HELPER:set_restricted_mode(RestrMode),
        PropList=?NITRO_HELPER:make_proplist(YPath, YName, YPort, YListen),
        application:start(nprocreg),
        web_helper:start_yaws(PropList)
    catch
        error: {badmatch,{error,{already_started,yaws}}} ->
            exit("Yaws and probably Nitrogen have been already started.. \n"++
                  "Nitrogen cannot be started now.");
        RefErr ->
            io:format(?Error:error_text(RefErr) ++ "~n"),
            %io:format("~p~n",[RefErr]),
            exit("Cannot start nitrogen.")
    end.
    
%% @doc Stop nitrogen framework
stop_nitrogen()->
    ?NITRO_HELPER:close_qtab(),
    application:stop(yaws).

%%% ============================================================================
%%% Getters for nitrogen framework

%% @doc Returns nitrogen's docroot path
%% @spec get_nitrogen_index()-> string()
get_nitrogen_index()->
    filename:join([?NITRO_HELPER:get_base(),?NSITE,"static"]).

%% @doc Returns nitrogen's root path
%% @spec get_nitrogen_root()-> string()
get_nitrogen_root()->
    filename:join([?NITRO_HELPER:get_base(),?NROOT]).

%% @doc Returns nitrogen's apps path
%% @spec get_nitrogen_apps_dir()-> string()
get_nitrogen_apps_dir()->
    filename:join([?NITRO_HELPER:get_base(),?NAPPS]).

%% @doc Returns nitrogen's site path
%% @spec get_nitrogen_site_dir()-> string()
get_nitrogen_site_dir()->
    filename:join([?NITRO_HELPER:get_base(),?NSITE]).

%% @doc Returns the root dir(s) of browsable files in server
%% @spec get_file_browser_server_root()-> [string()]
get_file_browser_server_root()->
    [{_,Result}]= ?NITRO_HELPER:find_in_fbtab("server_docroot"),
    Result.

%% @doc Returns the root dir(s) of images
%% @spec get_images_root()-> [string()]
get_images_root()->
    [{_,Result}]= ?NITRO_HELPER:find_in_fbtab("images_docroot"),
    Result.

%% @doc Returns the restricted mode flag.
%% @spec get_restricted_mode() -> atom()
get_restricted_mode() ->
    [{_,Result}]= ?NITRO_HELPER:find_in_fbtab("restricted_mode"),
    list_to_existing_atom(Result).

%% @doc Returns the root dir(s) of browsable files
%% which previously loaded to database
%% @spec get_file_browser_loaded_files_root()-> [string()]
get_file_browser_loaded_files_root()->
    [{_,Result}]= ?NITRO_HELPER:find_in_fbtab("db_docroot"),
    Result.

%% @doc Returns a list of filenames, where file's parent is the given argument,
%% and file's been loaded to database 
%% @spec get_loaded_files_list(string())-> [string()]
get_loaded_files_list(Parent)->
    FileList=try
        [reflib_file:path(F) || F<-?NITRO_HELPER:query_request([file])]
    catch
        request_denied -> []
    end,
    if
        FileList==[] -> [];
        true ->
            Folders=lists:usort(lists:map(fun(File)->
                case lists:prefix(Parent, File) of
                    true -> FolderSplit=string:tokens(lists:sublist(File,
                                length(Parent)+1,length(File)), "/"),
                            ConcFolder=lists:map(fun(X)-> X++"/" end, 
                                FolderSplit),
                            if
                                length(ConcFolder)==1 -> hd(ConcFolder);
                                true -> lists:flatten(lists:reverse(
                                    lists:nthtail(1,lists:reverse(ConcFolder))))
                            end;
                    false -> []
                end 
                                          end, FileList)),
            
            lists:map(fun(F)->lists:sublist(F,length(F)-1) end,
                [F || F <- Folders, F/=[]])
    end.

%% @doc Returns the list of the loaded directories
%% @spec get_loaded_directories() -> [] | [string()]
get_loaded_directories()->
    Result = make_ui_request({dir_sort}),
    case Result of
        {error, _}-> [];
        {ok, {error,_}}-> [];
        {ok, Dirs} -> [Dir || {Dir, _Mods}<-Dirs, is_list(Dir), Dir/="Other"]
    end.
    
%%% ============================================================================
%%% Whitespace conversion

%% @doc Turns whitespaces into HTML tags.
%% @spec whitespaces_to_html(string()) -> string()
whitespaces_to_html(String) ->
    lists:map(fun(E)->
        if 
            E == 10 -> "<br/>";
            E == 9  -> "&nbsp;&nbsp;";
            E == 32 -> "&nbsp;";
            true -> E
        end
    end, String).

%%% ============================================================================
%%% UI requests

make_ui_request(UIArgs)->
    make_ui_request(UIArgs, []).

make_ui_request(UIArgs, Args)->
    %make request to UI router
    ReqID = ?UI:getid(),
    User = proplists:get_value(user, Args, nobody),
    QStr = proplists:get_value(querystr, Args, ""),
    NeedToAdministrate = 
        case User of
            nobody -> 
                false;
            _ when QStr /= "" -> 
                ?NITRO_HELPER:insert_to_rqtab(ReqID, User, no_id, QStr),
                true;
            _ -> 
                false
        end,
    case ?UI:request(ReqID,UIArgs) of
        deny -> {error, {deny, "The request was denied by the job server."++
                                "Please, try again later."}};
        ok -> ui_loop(ReqID, Args, NeedToAdministrate)
    end.

ui_loop(ReqID, Args, NeedToAdministrate)->
    receive
        {ReqID, reply, R} ->
            NeedToAdministrate andalso 
                ?NITRO_HELPER:delete_from_rqtab({ReqID, '_','_','_'}),
            R;        
        {ReqID, query_id, QueryId}->
            NeedToAdministrate andalso
                begin
                    Result= ?NITRO_HELPER:find_in_rqtab_by_reqid(ReqID),
                    case Result of
                        [{ReqID, User, _, QStr}]->
                            ?NITRO_HELPER:insert_to_rqtab(ReqID, User,
                                                          QueryId, QStr);
                        _ -> ok
                    end
                end,
            ui_loop(ReqID, Args, NeedToAdministrate);       
        {ReqID, progress, {add, _File, _, _Max}} ->
            ui_loop(ReqID, Args, NeedToAdministrate);
        {ReqID, progress, {drop, _File, _, _Max}} ->
            ui_loop(ReqID, Args, NeedToAdministrate);
        {ReqID, progress, {add, File, Percent, FormCount, FormMax, KBps}} ->
            ?NITRO_HELPER:remote_print({File, Percent, FormCount, FormMax,KBps},
                                       Args),
            ui_loop(ReqID, Args, NeedToAdministrate);
        {ReqID, progress, {drop, File, _Percent, FormCount, FormMax, KBps}} ->
            ?NITRO_HELPER:remote_print({File, FormCount/FormMax, FormCount,
                                        FormMax, KBps},Args),
            ui_loop(ReqID, Args, NeedToAdministrate)
    end.
