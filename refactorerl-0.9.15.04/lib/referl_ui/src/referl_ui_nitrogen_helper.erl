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

-module(referl_ui_nitrogen_helper).

-export([init_qtab/0, close_qtab/0, insert_to_qtab/7, find_in_qtab/1, 
         find_in_qtab_by_pattern/1, delete_from_qtab/1]).
-export([init_rqtab/0, insert_to_rqtab/4, find_in_rqtab_by_reqid/1, 
         find_in_rqtab_by_user/1, get_rqtab_elements/0, delete_from_rqtab/1]).
-export([init_fbtab/0, insert_to_fbtab/2, find_in_fbtab/1]).
-export([get_database_hash/0, is_database_changed/1]).
-export([error_handler/1, is_error_in_database/0, make_error_message/1, 
         get_error_forms_in_database/0]).
-export([insert_to_itab/3, find_in_itab_by_pattern/1, delete_from_itab/1]).
-export([set_image_root/1, set_restricted_mode/1, set_file_browser_docroot/1, 
         calculate_root/1]).
-export([list_to_term/1, filtered_query/1, query_filter_fun/0, remote_print/2, 
         get_base/0, load_needed/0, make_proplist/4]).
-export([calculate_result/2, calculate_result/3, calculate_result/4]).
-export([%generate_dep_graph_fun_level/1, generate_dep_graph_mod_level/1,
         get_result_list/2,generate_dep_graph/1]).
-export([query_request/1, query_request/2, query_request/3, data_request/1,
         syn_request/1, syn_request2/3, generate_node_request/2]).
%% Used by the Wx interface:
-export([get_errors/1]).

-define(TAB, 
    filename:join([?MISC:data_dir(),
                   "query_table_v6"++ 
                   ?MISC:graph_based_postfix()])).
-define(FB_TAB, 
    filename:join([?MISC:data_dir(),
                   "file_browser_table"++ 
                   ?MISC:graph_based_postfix()])).
-define(RQ_TAB, 
    filename:join([?MISC:data_dir(),
                   "running_queries_table"++ 
                   ?MISC:graph_based_postfix()])).                   
-define(I_TAB, 
    filename:join([?MISC:data_dir(),
                   "investigations_table"++
                   ?MISC:graph_based_postfix()])).

-define(NROOT,"nitrogen").
-define(NSITE,?NROOT++"/site").
-define(NAPPS,?NROOT++"/apps").
-vsn("$Rev: 12595 $ ").

-include("ui.hrl").
%-include_lib("referl_core/include/core_export.hrl").

%% @doc Initialize query_table_v6
%% @spec init_qtab() -> ok
%% @deprecated
init_qtab() ->
    dets:open_file(?TAB,[]).

%% @doc Close query_table_v6
%% @spec close_qtab() -> ok
%% @deprecated
close_qtab() ->
    dets:close(?TAB).

%%% ============================================================================
%%% Low- level operations on 'running_queries_table' dets table
init_rqtab()->
    dets:open_file(?RQ_TAB,[]),
    dets:delete_all_objects(?RQ_TAB),
    dets:close(?RQ_TAB).

insert_to_rqtab(ReqID,User, QueryId, QStr) ->
    dets:open_file(?RQ_TAB,[]),
    dets:insert(?RQ_TAB,{ReqID,User,QueryId, QStr}),
    dets:close(?RQ_TAB).

find_in_rqtab_by_reqid(ReqID) ->
    dets:open_file(?RQ_TAB,[]),
    Result=dets:match_object(?RQ_TAB,{ReqID,'_','_','_'}),
    dets:close(?RQ_TAB),
    Result.

find_in_rqtab_by_user(User) ->
    dets:open_file(?RQ_TAB,[]),
    Result=dets:match_object(?RQ_TAB,{'_',User,'_','_'}),
    dets:close(?RQ_TAB),
    Result.

get_rqtab_elements()->
    dets:open_file(?RQ_TAB,[]),
    Result=dets:match_object(?RQ_TAB,{'_','_','_','_'}),
    dets:close(?RQ_TAB),
    Result.
    
delete_from_rqtab(RowPattern) ->
    dets:open_file(?RQ_TAB,[]),
    dets:match_delete(?RQ_TAB,RowPattern),
    dets:close(?RQ_TAB).

%%% ============================================================================
%%% Low- level operations on 'file_browser_table' dets table
insert_to_fbtab(Key,Value) ->
    dets:open_file(?FB_TAB,[]),
    dets:insert(?FB_TAB,{Key,Value}),
    dets:close(?FB_TAB).

find_in_fbtab(Key) ->
    dets:open_file(?FB_TAB,[]),
    Result=dets:match_object(?FB_TAB,{Key,'_'}),
    dets:close(?FB_TAB),
    Result.

init_fbtab()->
    dets:open_file(?FB_TAB,[]),
    dets:delete_all_objects(?FB_TAB),
    dets:close(?FB_TAB).

%%% ============================================================================
%%% Low- level operations on 'query_table_v6' dets table
insert_to_qtab(SafeQuery=[C|_],Query, Alias, Comment, Result, User, Hash) 
    when is_integer(C)->
    dets:open_file(?TAB,[]),
    insert_to_qtab({SafeQuery, undefined, undefined}, 
                    Query, Alias, Comment, Result, User, Hash),
    dets:close(?TAB);

insert_to_qtab(SafeQuery={_,_,_}, Query, Alias, Comment, Result, User, Hash) ->
    dets:open_file(?TAB,[]),
    dets:insert(?TAB, {SafeQuery, Query, Alias, Comment, Result, User, Hash}),
    dets:close(?TAB).

find_in_qtab(SafeQuery) ->
    dets:open_file(?TAB,[]),
    Result=dets:lookup(?TAB,SafeQuery),
    dets:close(?TAB),
    Result.

find_in_qtab_by_pattern(Pattern)->
    dets:open_file(?TAB,[]),
    Result=dets:match_object(?TAB,Pattern),
    dets:close(?TAB),
    Result.

delete_from_qtab(SafeQuery) ->
    dets:open_file(?TAB,[]),
    dets:delete(?TAB,SafeQuery),
    dets:close(?TAB).
    
%%% ============================================================================
%%% Low- level operations on 'investigations_table' dets table
insert_to_itab(User,Name,Data) ->
    dets:open_file(?I_TAB,[]),
    dets:insert(?I_TAB,{{User, Name}, Data}),
    dets:close(?I_TAB).

find_in_itab_by_pattern(Pattern) ->
    dets:open_file(?I_TAB,[]),
    Result=dets:match_object(?I_TAB,Pattern),
    dets:close(?I_TAB),
    Result.
    
delete_from_itab(RowPattern) ->
    dets:open_file(?I_TAB,[]),
    dets:match_delete(?I_TAB,RowPattern),
    dets:close(?I_TAB).

%%% ============================================================================
%%% Nitrogen setters

set_image_root(ImgRoot=[C|_]) when is_integer(C) ->
    insert_to_fbtab("images_docroot",ImgRoot);

set_image_root(ImgRoot) when is_atom(ImgRoot) ->
    set_image_root(atom_to_list(ImgRoot)).

set_restricted_mode(RM=[C|_]) when is_integer(C) ->
    insert_to_fbtab("restricted_mode",RM);

set_restricted_mode(RM) when is_atom(RM) ->
    insert_to_fbtab("restricted_mode", atom_to_list(RM)).

set_file_browser_docroot(no_path) ->
    set_file_browser_docroot("no_path");

set_file_browser_docroot("no_path")->
    Root=calculate_file_browser_serverroot(),
    set_file_browser_docroot(Root, no_path);

set_file_browser_docroot(BrowserRoot) when is_atom(BrowserRoot) ->
    set_file_browser_docroot([atom_to_list(BrowserRoot)], path);

set_file_browser_docroot(BrowserRoot=[C|_]) when is_integer(C) ->
    set_file_browser_docroot([BrowserRoot],path);

set_file_browser_docroot(BrowserRoot=[C|_]) when is_list(C) ->
    set_file_browser_docroot(BrowserRoot,path);

set_file_browser_docroot(_)->
    throw(?RefErr0r(
          "Bad argument given at startup~n Bad parameter: broswer root~n")).

set_file_browser_docroot(BrowserRoot=[C|_], path) when is_integer(C)->
    set_file_browser_docroot([BrowserRoot], path);

set_file_browser_docroot(BrowserRoot=[C|_], path) when is_list(C)->
    insert_to_fbtab("server_docroot",BrowserRoot),
    insert_to_fbtab("db_docroot", calculate_file_browser_dbroot());

set_file_browser_docroot(_, path)  ->
    throw(?RefErr0r(
          "Bad argument given at startup~n Bad parameter: broswer root~n"));

set_file_browser_docroot(BrowserRoot, no_path)->
    insert_to_fbtab("server_docroot",BrowserRoot),
    insert_to_fbtab("db_docroot",BrowserRoot).

calculate_file_browser_serverroot()->
    calculate_file_browser_dbroot().

calculate_file_browser_dbroot()->
    %Gets filelist from UI router
    {result, FileList}=?NITRO_SERVICES:execute_system_query("files",
                                         {query_display_opt,
                                          [{positions, scalar}],
                                          needed_pattern,$:}),
    %Gets top level directories
    RootDir=calculate_root(lists:map(fun(File)->
                                         lists:sublist(File,length(File)-1) 
                                     end,FileList)),
    
    case RootDir of
        [] -> case file:get_cwd() of
                  {ok,Dir} -> [Dir];
                  {error, Error}->
                      throw(?RefErr0r("Error during init: " ++ 
                                      io_lib:format("~p", [Error])))
              end;
        _  -> RootDir
    end.

get_errors(Forms) when is_list(Forms)->
    ErrorFiles=query_request(Forms,[{form, back}]),
    lists:foldl(fun({File, Form}, Acc)->
        {Index, First}=case syn_request2(File, form, Form) of
            1 -> {1,true};
            Int -> {Int-1,false}
        end,
        {StartPos,Length} = case First of
            true->
                [Token]=query_request(Form, [{flex, last}]),
                case query_request(?Token,pos,Token) of
                    {EPos,_} ->
                    %%  Len= ?Form:form_length(Form),
                    %%  Len=(data_request(Form))#form.length,
                        Len = form_length_request(Form),
                        {EPos-Len,Len};
                    not_found -> {not_found, 0}
                end;
            false->
                PrevForm=query_request(File, ?File:form(Index)),
                [Token]=query_request(PrevForm, [{flex, last}]),
                case query_request(?Token,pos,Token) of
                      {SPos, _} ->
                   %%     Len=(data_request(Form))#form.length
                          Len = form_length_request(Form)
                             +query_request(?Token,len,
                                            query_request(?Token,data,Token)),
                        {SPos+1,Len-1}; %% 2 ????
                    not_found -> {not_found, 0}
                end
            end,
        Tag=(data_request(Form))#form.tag,
        case {ErrorMessage=make_error_message(Tag),StartPos} of
            {"", _} -> Acc;
            {_, not_found} -> Acc;
            _ -> Acc++[{query_request(?File,path,File), 
                        StartPos, 
                        Length,
                        ErrorMessage}]
        end
    end, [], lists:zip(ErrorFiles, Forms));
                
get_errors([])->[];

get_errors(_)->[].

%%% ============================================================================
%%% UI router helper functions

query_request(Arg1) ->
    case ?NITRO_CORE:make_ui_request({graph_query, ?Query, exec, [Arg1]}) of
        {error, {deny,_}} -> throw(request_denied);
        {error, E} -> throw(E);
        {ok,R} -> R
    end.
query_request(Arg1,Arg2) ->
    case ?NITRO_CORE:make_ui_request({graph_query, ?Query,exec, [Arg1,Arg2]}) of
        {error, {deny,_}} -> throw(request_denied);
        {error, E} -> throw(E);
        {ok,R} -> R
    end.
query_request(Mod,Fun,Arg) ->
    case ?NITRO_CORE:make_ui_request({graph_query, Mod, Fun, [Arg]}) of
        {error, {deny,_}} -> throw(request_denied);
        {error, E} -> throw(E);
        {ok,R} -> R
    end.
data_request(Arg) ->
    case ?NITRO_CORE:make_ui_request({graph_data, Arg}) of
        {error, {deny,_}} -> throw(request_denied);
        {error, E} -> throw(E);
        {ok,R} -> R
    end.
syn_request(Arg) ->
    case ?NITRO_CORE:make_ui_request({syn_leaves, Arg}) of
        {error, {deny,_}} -> throw(request_denied);
        {error, E} -> throw(E);
        {ok,R} -> R
    end.
syn_request2(Arg1, Arg2, Arg3) ->
    case ?NITRO_CORE:make_ui_request({syn_index, Arg1, Arg2, Arg3}) of
        {error, {deny,_}} -> throw(request_denied);
        {error, E} -> throw(E);
        {ok,R} -> R
    end.
    
generate_node_request(Arg1, Arg2) ->
    case ?NITRO_CORE:make_ui_request({html_generate_node, Arg1, Arg2}) of
        {error, {deny,_}} -> throw(request_denied);
        {error, E} -> throw(E);
        {ok,R} -> R
    end.
    
form_length_request(Arg) ->
    case ?NITRO_CORE:make_ui_request({form_length, Arg}) of
        {error, {deny,_}} -> throw(request_denied);
        {error, E} -> throw(E);
        {ok,R} -> R
    end.
%%% ============================================================================
%%% Helper functions

generate_dep_graph(Options)->
    ?NITRO_CORE:make_ui_request({draw_dep_graph, Options}).

%generate_dep_graph_fun_level(Options)    ->
%    Opts=proplists:delete(type, Options),
%    case proplists:get_value(type, Options) of
%        all -> ?NITRO_CORE:make_ui_request({cyclic_fun_draw, Opts});
%        cycles ->
%            ?NITRO_CORE:make_ui_request({cyclic_fun_draw_cycles, Opts});
%        none -> case proplists:get_value(gnode, Options) of
%                    undefined -> {error, "Bad arguments were given."};
%                    _ -> ?NITRO_CORE:make_ui_request({cyclic_fun_draw, Opts})
%                end;
%        _ -> {error, "Bad arguments were given."}
%    end.

%generate_dep_graph_mod_level(Options) ->
%    Opts=proplists:delete(type, Options),
%    case proplists:get_value(type, Options) of
%        all -> 
%            ?NITRO_CORE:make_ui_request({cyclic_mod_draw, Opts});
%        cycles ->
%            ?NITRO_CORE:make_ui_request({cyclic_mod_draw_cycles, Opts});
%        none -> case proplists:get_value(gnode, Options) of
%                    undefined -> {error, "Bad arguments were given."};
%                    _ -> ?NITRO_CORE:make_ui_request({cyclic_mod_draw, Opts})
%                end;
%        _ -> {error, "Bad arguments were given."}
%    end.

get_result_list(QueryResult,NeededPattern)->
    ResultList=string:tokens(QueryResult," \t\n"),
    case NeededPattern of
        none -> {result,ResultList};
        _ -> {result, lists:filter(fun(E)->
                                       case string:chr(E,NeededPattern) of
                                          0 -> false;
                                          _ -> true 
                                       end 
                                   end,ResultList)}
    end.

remote_print({File, Percent, FormCount, FormMax, KBps}, Args)->
    KBpsTxt=case KBps of
        0.0 -> "";
        0 -> "";
        _ -> ?MISC:format("~5.2f kB/s", [(0.0 + KBps)])
    end,
    Data=io_lib:format("File: ~s (~p / ~p) ~s", 
        [File, FormCount, FormMax, KBpsTxt]),
    remote_print({lists:flatten(Data), Percent*100}, Args);

remote_print(Data, Args)->
    RemotePrinterFun=proplists:get_value(remote_printer_fun, Args, 
                                         fun(_)->ok end),
    spawn(fun()->RemotePrinterFun(Data) end),
    ok.

get_base()->
    Path0 = filename:split(filename:dirname(code:which(?NITRO_CORE))),
    Path =filename:join(lists:sublist(Path0,length(Path0)-1)),
    Path.
    
load_needed() ->
    Base=get_base(),
    %Needed to start Nitrogen
    case code:add_path(filename:join([Base,?NAPPS,"nprocreg/ebin"])) of        
        true -> ok;
        {error,_} -> throw(?RefError(file_notdir,
                                     [filename:join([Base,
                                                     ?NAPPS,
                                                     "nprocreg/ebin"])]))
    end,
    case code:add_path(filename:join([Base,?NAPPS,"simple_bridge/ebin"])) of
        true -> ok;
        {error,_} -> throw(?RefError(file_notdir,
                                     [filename:join([Base,
                                                    ?NAPPS,
                                                    "simple_bridge/ebin"])]))
    end,
    case code:add_path(filename:join([Base,?NAPPS,"nitrogen/ebin"])) of
        true -> ok;
        {error,_} -> throw(?RefError(file_notdir,
                                    [filename:join([Base,
                                                    ?NAPPS,
                                                    "nitrogen/ebin"])]))
    end,
    case code:add_path(filename:join([Base,?NAPPS,"simple_bridge/include"])) of
        true -> ok;
        {error,_} -> throw(?RefError(file_notdir,
                                     [filename:join([Base,
                                                    ?NAPPS,
                                                    "simple_bridge/include"])]))
    end,
    case code:add_path(filename:join([Base,?NAPPS,"nitrogen/include"])) of
        true -> ok;
        {error,_} -> throw(?RefError(file_notdir,
                                    [filename:join([Base,
                                                    ?NAPPS,
                                                    "nitrogen/include"])]))
    end,
    case code:ensure_loaded(nprocreg) of
        {error,_} -> throw(?RefErr0r(nprocreg_not_loaded));
        _ -> ok
    end,
    
    %Nitrogen framework use this low-level module to load 
    %the requested page's main modul in some circumstances, 
    %so we should add the page's ebin folder
    {ok,LoadPath}=erl_prim_loader:get_path(),
    erl_prim_loader:set_path(LoadPath++[filename:join([Base,?NSITE,"ebin"])]),
    ok.

make_proplist(YPath, YName, YPort, YListen) ->
    try
    YawsPathProp = {yaws_path,YPath},
    YawsNameProp = {yaws_name,YName},
    YawsPortProp = {yaws_port, web_helper:convert_list_to_integer(YPort)},
    YawsListenProp = {yaws_listen, web_helper:string_to_ip(YListen)},
    YawsIndexFunModuleProp = {yaws_index_fun_module, ?NITRO_CORE},
    YawsIndexFunNameProp = {yaws_index_fun_name, get_nitrogen_index},
    YawsPostSize = {yaws_partial_post_size, 1024*1024},
    YawsAppMod = {yaws_app_mod,   [{"/", nitrogen_yaws}] },
    NitrogenProp = {nitrogen_prop,with_nitrogen},
    AllProp = [YawsNameProp,YawsPortProp,YawsListenProp,YawsIndexFunModuleProp,
               YawsIndexFunNameProp,YawsAppMod,NitrogenProp,YawsPostSize],
    case YPath of
        "no_path" -> AllProp;
        _ -> [YawsPathProp|AllProp]
    end
    catch
        RefErr ->
            io:format(?Error:error_text(RefErr) ++ "~n"),
            exit("Cannot start nitrogen.")
    end.
    
%% @doc Returns with the result of the given query
calculate_result(Q,DisplayOpt)->
    calculate_result(Q,DisplayOpt, []).

calculate_result(Q,DisplayOpt, StartOpt) when is_list(StartOpt)->
    calculate_result(Q,DisplayOpt, StartOpt, []).
calculate_result(Q,DisplayOpt, StartOpt, Options) 
  when is_list(StartOpt) andalso is_list(Options)->
    SendBackQueryId=proplists:get_value(save_query_id, Options, false),
    User=proplists:get_value(user, Options, nobody),
    Req = {transform,semantic_query,
           [{ask_missing,false},
            {send_back_query_id, SendBackQueryId},
            {querystr,Q},
            {display_opt,DisplayOpt},
            {start_opt,StartOpt}]},
    case ?NITRO_CORE:make_ui_request(Req,[{user,User},{querystr,Q}]) of
        {ok, Result} -> Result;
        M -> M
    end.

get_database_hash() ->
    case ?NITRO_CORE:make_ui_request({database_hash}) of
        {ok, Hash} -> Hash;
        _ -> -1
    end.

error_handler(warning) ->
    "Parse errors. The forms which contain errors are not clickable.";

error_handler({warning, Errors}) ->
    {"Warning: the database contains file(s) with error(s).", Errors};

error_handler(E) ->
    case E of
        {abort, M} -> reflib_error:error_text(M);
        {error, M} -> "Fatal error: " ++ io_lib:format("~p",[M])
    end.

is_error_in_database() ->
    try
        case query_request(?Query:seq([file],?File:error_forms())) of
            [] -> false;
            _ -> true
        end
    catch 
        request_denied -> false
    end.

make_error_message(Tag)->
    try
    case Tag of
        {1,Mess} -> Mess;
        {no_include_file, Name} -> 
            io_lib:format("Include file not found: ~p", [Name]);
        {include_lib, application_not_specified, Name} ->
            io_lib:format("Application not specified -include lib: ~p", [Name]);
        {unknown_env_in_include, [Name]}->
            io_lib:format("Unknown env in include: ~p", [Name]);
        {include_error, IncName, Reason} ->
            io_lib:format("Include error in ~p. Reason: ~p",[IncName,Reason]);
        _ -> ""
    end
    catch
        _Type : _Error ->
            ""
    end.

is_database_changed(Hash1) ->
    Hash1 /= get_database_hash().

list_to_term(String) ->
    {ok, T, _} = erl_scan:string(String++"."),
    case erl_parse:parse_term(T) of
        {ok, Term} -> Term;
        {error, Error} -> Error
    end.
  
get_error_forms_in_database() ->
    try
        case query_request(?Query:seq([file],?File:error_forms())) of
            [] -> [];
            ErrorForms -> get_errors(ErrorForms)
        end
    catch
        request_denied -> []
    end.

%% @doc Return the filter function,
%% which need to remove whitespaces from queries.
query_filter_fun()->
    fun(E)->
        if 
            E == 10 -> false;
            E == 9  -> false;
            E == 32 -> false;
            true -> true
        end
    end.

%% @doc Replaces group of whitespace in the given query with 1 blank char.
filtered_query([]) -> [];
filtered_query(Query=[C|_]) when is_integer(C) ->
    {ok, Mp}=re:compile("(\s|\t|\n)+",[]),
    re:replace(Query,Mp," ",[{return, list}, global]).

calculate_root(FileList=[C|_]) when is_list(C)->
    lists:usort(lists:map(fun(E) -> filename:dirname(E) end, FileList));

calculate_root([])->[].
