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
-module (ajax_handler).
-compile(export_all).
-include_lib("referl_ui/include/nitrogen.hrl").
-include_lib("nitrogen/include/wf.hrl").
-include_lib("kernel/include/file.hrl").
-vsn("$Rev: 13086 $ ").


main() ->
    case wf:role(auth_users) of
        true ->
            #template { file=?NITRO_CORE:get_nitrogen_site_dir()++
                                 "/templates/ajax_handler.html" };
        false ->
            wf:header("WWW-Authenticate", "HTTP/1.1 403 Forbidden"),
            wf:status_code(403),
            "<strong>Access Denied!</strong>"
    end.

get_content()->
    case wf:q(filter) of
        undefined ->
    case wf:q(dir) of
        undefined -> "";
        "__multi_dirs__" ->
            case wf:q(mod) of
                undefined -> "";
                "db"->TopLevelDirs=
                          get_starting_folders(
                              ?NITRO_CORE:get_file_browser_loaded_files_root()),
                      LiList=get_li_list("",TopLevelDirs),
                      "<ul class='jqueryFileTree' style='display: none;'>"++
                          LiList++
                          "</ul>";
                "server"->TopLevelDirs=
                                get_starting_folders(
                                    ?NITRO_CORE:get_file_browser_server_root(
                                        )),
                          LiList=get_li_list("",TopLevelDirs),
                          "<ul class='jqueryFileTree' style='display: none;'>"++
                              LiList++"</ul>";
                "invs"->
                          LiList=get_li_list(mnesia:system_info(directory),
                              ["investigations"]),
                          "<ul class='jqueryFileTree' style='display: none;'>"++
                              LiList++"</ul>"
            end;
            RootDirUrlEncoded->RootDir=wf_convert:url_decode(RootDirUrlEncoded),
            case wf:q(mod) of 
                "db"->LiList=
                       get_li_list(RootDir,
                           get_starting_folders(
                           ?NITRO_CORE:get_loaded_files_list(RootDir),noslash)),
                      "<ul class='jqueryFileTree' style='display: none;'>"
                       ++LiList++"</ul>";
                _ ->
                   case listdir_sorted(RootDir) of
                       {ok,FileNames}->
                           LiList=get_li_list(RootDir,FileNames),
                           "<ul class='jqueryFileTree' style='display: none;'>"
                            ++LiList++"</ul>";
                       {error, _}->""
                   end
            end
    end;
        Filter ->
            case wf:q(mod) of
                undefined -> "";
                "db"->TopLevelDirs=
                        ?NITRO_CORE:get_file_browser_loaded_files_root(),
                      LiList=get_filtered_list("",Filter,TopLevelDirs,db),
                      "<ul class='jqueryFileTree' style='display: none;'>"++
                          LiList++"</ul>";
                "server"->
                    TopLevelDirs=?NITRO_CORE:get_file_browser_server_root(),
                    LiList=get_filtered_list("",Filter,TopLevelDirs,server),
                          "<ul class='jqueryFileTree' style='display: none;'>"++
                              LiList++"</ul>"
            end
    end.

get_starting_folders(Folders,noslash) ->
    GroupedFolders=group_files(lists:map(fun(F) -> 
                                            {F,hd(filename:split(F))} 
                                         end,Folders)),
    lists:map(fun([F|FS]) -> 
                  get_largest_prefix(F,F,FS) 
              end,GroupedFolders).
              
get_starting_folders(Folders) ->
    GroupedFolders=group_files(lists:map(fun(F) -> 
                        {F,filename:join(lists:sublist(filename:split(F),2))} end, Folders)),
     lists:map(fun([F|FS]) -> 
                  get_largest_prefix(F,F,FS) 
              end,GroupedFolders).

get_largest_prefix(_,LastPrefix,[]) -> LastPrefix;
get_largest_prefix(Prefix,LastPrefix,Folders) ->
    GoodPrefix=lists:all(fun(F) -> lists:prefix(Prefix, F) end,Folders),
    if
	GoodPrefix==false -> LastPrefix;
        hd(Folders)==Prefix -> Prefix;
        GoodPrefix==true -> 
            First=hd(Folders),            
            NextPrefix=filename:join(Prefix,
                lists:nth(
                    length(filename:split(Prefix))+1,filename:split(First))),
            get_largest_prefix(NextPrefix,Prefix,Folders)
    end.

myhd([]) -> [];
myhd([E|_]) -> E.

group_files([]) -> [];
group_files([{Data,File}|ES]) ->
    group_files(ES,[File,Data],[],File).

group_files([],InnerSum,Sum,_) -> Sum++[InnerSum];
group_files([{Data,File}|ES],InnerSum,Sum,LastFile) when File==LastFile ->
    group_files(ES,InnerSum++[Data],Sum,File);
group_files([{Data,File}|ES],InnerSum,Sum,_) ->
    group_files(ES,[File,Data],Sum++[InnerSum],File).

receive_ajax()->
    case wf:q(dir) of
        undefined ->wf:session(selected_file,wf:q(selected_file)),
                    "";
        _ -> ""
    end,
    case wf:q(window_position) of
        undefined -> "";
        _ -> wf:session(window_position,wf:q(window_position))
    end,
    case wf:q(nodetext) of
        undefined -> "";
        _ -> wf:session(nodetext,wf:q(nodetext))
    end,
    case wf:q(selected_position) of
        undefined -> "";
        _ -> wf:session(selected_position, wf:q(selected_position)),
             ""
    end.

get_li_list(RootDir,FileNames)->
    lists:foldr(fun(X, List) -> 
            IsFile=case file:read_file_info(filename:join(RootDir,X)) of
                    {ok, #file_info{type=directory}} -> 
                        false;
                    {error,_} -> 
                         E=filename:extension(X),
                         if
                            E=="" -> false;
                            true -> true
                         end;
                    {_, _} ->
                        true
            end,
            Elem=if
                IsFile ->
                    R=filename:extension(X),
                    Ext=if 
                            hd(R) == 46 -> lists:nthtail(1,R);
                            true -> X
                        end,
                    OnlyErls=wf:q(checked)=="true",
                    if
                        (R==".erl") or (R==".hrl") or not (OnlyErls) ->
                            "<li class='file ext_"++ 
                            Ext ++
                            "'><a href='#' rel='"++
                            filename:join(RootDir,X)++
                            "'>"++
                            lists:last(string:tokens (X, "/"))++
                            "</a></li>";
                        true -> ""
                    end;
                not IsFile ->
                    "<li class='directory collapsed'><a href='#' rel='"++
                        filename:join(RootDir,X)++
                        "/'>"++
                        X++
                        "</a></li>"
            end,
            Elem ++ List
                end, [], FileNames).

get_filtered_list(RootDir,Filter,FileNames,ListType)-> 
    lists:foldr(fun(X, List) -> 
        IsFile=case file:read_file_info(filename:join(RootDir,X)) of
                    {ok, #file_info{type=directory}} -> 
                        false;
                    {error,_} -> 
                         E=filename:extension(X),
                         if
                            E=="" -> false;
                            true -> true
                         end;
                    {_, _} ->
                        true
            end,
        Elem=if
                IsFile ->
                    R=filename:extension(X),
                    Matches=filter(Filter,lists:last(string:tokens (X, "/"))),
                    OnlyErls=wf:q(checked)=="true",
                    if
                        (Matches) and ((R==".erl")  
                        or (R==".hrl") or not (OnlyErls)) -> 
                            Ext=if 
                                    hd(R) == 46 -> lists:nthtail(1,R);
                                    true -> X
                                end,
                            "<li class='file ext_"++ 
                                Ext ++
                                "'><a href='#' rel='"++
                                filename:join(RootDir,X)++
                                "'>"++
                                lists:last(string:tokens (X, "/"))++
                                %filename:join(RootDir,X)++
                                "</a></li>";
                        true -> ""
                    end;
                not IsFile ->
                    if
                        ListType==db ->
                            get_filtered_list(X,Filter,
                                ?NITRO_CORE:get_loaded_files_list(X),ListType);
                        true ->
                            {ok,Files}=listdir_sorted(filename:join(RootDir,X)),
                            get_filtered_list(X,Filter,Files,ListType)
                    end
            end,
        Elem ++ List
                end, [], FileNames).


filter(Filter,File) ->
    Regexp=lists:flatten(lists:map(fun(X) -> 
    if 
        X==$* -> ".*";
        X==$. -> "\\.";
        X==$? -> ".";
        true -> X 
    end end,"*"++Filter++"*")),
    try
    case re:run(File, Regexp, []) of
        {match,[{S,F}]} when (S==0) and (F==length(File)) -> true;
        _ -> false
    end
    catch
    _ -> false
    end.

listdir_sorted(RootDir) ->
    {State,FileNames}=file:list_dir(RootDir),
    if
    State==error -> 
        {ok,[]};
    true ->
        F1=mysort(lists:filter(
            fun(X) -> 
                L=file:read_file_info(filename:join(RootDir,X)),
                case L of {ok, #file_info{type=directory}} ->true; _ ->false end
            end,FileNames)),
        F2=mysort(lists:filter(
            fun(X) -> 
                L=file:read_file_info(filename:join(RootDir,X)),
                case L of {ok, #file_info{type=regular}} -> true; _ -> false end
            end,FileNames)),
        F3=mysort(lists:filter(
            fun(X) -> 
                L=file:read_file_info(filename:join(RootDir,X)),
                case L of {error, _} -> true; _ -> false end
            end,FileNames)),
        {ok,F1++F2++F3}
    end.

mysort([]) ->
    [];
mysort([H | T]) -> 
    mysort([ X || X <- T, string:to_lower(X) < string:to_lower(H) ]) 
    ++ [H] 
    ++ mysort([ X || X <- T, string:to_lower(X) >= string:to_lower(H) ]).
