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

-module(referl_ui_nitrogen_services).

-export([execute_query/1,execute_system_query/2, update_tab_if_needed/6, 
         set_alias/2]).
-export([delete_from_qtab/2, query_list_from_qtab/1, 
         update_prev_query_comment/2]).
-export([inv_list_from_itab/1, delete_from_itab/2, get_from_itab/2,
         insert_into_itab/3]).
-export([get_running_queries/0, get_running_queries/1, stop_running_queries/1, 
         kill_query/1]).
-export([get_appbases/0, del_appbase/1, add_appbase/1]).
-export([add_to_db/2, drop_from_db/2, need_to_update/0, get_source/1]).
-export([synchronize/1]).
-export([get_possible_warning/0, add_possible_warning/0]).
-export([svg/2, generate_dependency_graph/2, print_dependency_graph/2,
         generate_fb_graph/3, delete_dependency_graphs_files/1,
         generate_smart_graph/2]).
-export([search_initial_clones/1, get_const_var_diff/2]).
-export([do_autocomplete/1]).
-export([get_node_position/1, get_node_linenum/1, get_node_linenum_comment/1,
         position_to_linenum/2, generate_node/2, get_container/1,
         node_to_text/1, node_query_fun/4, get_file/1, get_type/1, only_text/1,
         get_function/3, get_proper_node/1, filter_nodes/1, get_clauses/1,
         get_mods/0,get_funs/0, get_filehash/1, convert_query_if_skel/1]).

%% Skeleton-interface
-export([update_skeleton/3, update_prev_skeleton_comment/2, delete_skeleton/1,
         skeleton_call_format/1, save_skeleton/3, list_skeletons/0, 
         evaluate_skeleton/3, evaluate_skeleton/4, try_parse_skeleton/2,
         determine_sq_request_type/1, do_autocomplete_skeleton/1]).
%%%%%%%%%%%%%%%%%%%%%%%%

-vsn("$Rev: 12595 $ ").

-include("ui.hrl").
-include_lib("referl_core/include/core_export.hrl").

%%% @type safequery() = {SQueryString, File, Position}
%%%   SQueryString = string()
%%%   File = string()
%%%   Position = integer().

%%% ============================================================================
%%% Operations on 'query_table_v6' dets table

%% @doc Returns previous queries from the given user in the query_table_v6
%% @spec query_list_from_qtab( string() )->[] | [ Item :: term() ]
%% Item = {SafeQuery, Query, Alias, Users}
%%   SafeQuery = safequery()
%%   Alias = string()
%%   Users = [ string() ]
query_list_from_qtab(Usr) ->
    L = ?NITRO_HELPER:find_in_qtab_by_pattern(
            {'_','_','_','_','_','_','_'}),
    L2 = case Usr of
            "all" -> L;
            _ -> lists:filter(
                    fun({_SafeQuery,_Q,_A,_C,_R,Users,_H}) ->
                        lists:member(Usr,Users)
                    end, L)
         end,
    lists:keysort(3,lists:map(
                    fun ({SafeQuery,Q,A,C,_R,U,_H})->
                            {SafeQuery,Q,A,C,U} 
                    end, L2)).

update_prev_query_comment(SafeQuery, NewComment)->
    [{_SafeQuery,Query,Alias,_Comment,Result,UserList,Hash}] =
        ?NITRO_HELPER:find_in_qtab(SafeQuery),
    ?NITRO_HELPER:insert_to_qtab(SafeQuery,Query,Alias,NewComment,
        Result,UserList,Hash).

%% @doc Deletes the given query from the given user in the query_table_v6
%% @spec delete_from_qtab({SQueryString, File, Positon},string()) -> ok
delete_from_qtab(SafeQuery,admin) ->
    [{SafeQuery,_Query,_Alias,_Comment,_Result,_UserList,_Hash}] =
        ?NITRO_HELPER:find_in_qtab(SafeQuery),
    ?NITRO_HELPER:delete_from_qtab(SafeQuery);

delete_from_qtab(SafeQuery,Usr) ->
    [{SafeQuery,Query,Alias,Comment,Result,UserList,Hash}] =
        ?NITRO_HELPER:find_in_qtab(SafeQuery),
    UserList2 = lists:delete(Usr,UserList),
    case UserList2 of
        [] -> ?NITRO_HELPER:delete_from_qtab(SafeQuery);
        _ ->
            ?NITRO_HELPER:insert_to_qtab(SafeQuery,Query,Alias,
                Comment,
                Result,UserList2,Hash)
    end.
    
%%% ============================================================================
%%% Operations on 'investigations_table' dets table

%% @doc Returns previous queries from the given user in the query_table_v6
%% @spec inv_list_from_itab( string() )->[ string() ]
inv_list_from_itab(User) ->
    L = case User of
            "all" -> ?NITRO_HELPER:find_in_itab_by_pattern({'_','_'});
            _ -> ?NITRO_HELPER:find_in_itab_by_pattern({{User,'_'},'_'})
         end,
    [{Usr,Name} || {{Usr,Name},_} <- L].

%% @doc Deletes the given investigation from the 'investigations table'
%% @spec delete_from_itab(string(),string()) -> ok
delete_from_itab(User,Name) ->
    ?NITRO_HELPER:delete_from_itab({{User,Name},'_'}).

%% @doc Gets investigation data from 'investigations table'
%% @spec get_from_itab(string(),string()) -> term()
get_from_itab(User,Name) ->
    Res=?NITRO_HELPER:find_in_itab_by_pattern({{User,Name},'_'}),
    if
        Res==[] -> not_found;
        true -> 
            [{{_,_},Data}]=Res, 
            Data
    end.
    
%% @doc Inserts the given investigation into 'investigations table'
%% @spec insert_into_itab(string(),string(),term()) -> ok
insert_into_itab(User,Name,Data) ->
    ?NITRO_HELPER:insert_to_itab(User,Name,Data).

%%% ============================================================================
%%% Semantic Queries

%% @doc Returns the given query's result
%% and administrate the action if necessary
%% @spec execute_query({QueryType, Query, User} | 
%%                      {QueryType, SafeQuery, User} |
%%                      {QueryType, Query, Alias, User} | 
%%                      {QueryType, SafeQuery, Alias, User} |
%%                      noquery) -> {Result, Warning, Error}
%%   QueryType = 'query' | prev_query | unknown
%%   Query = string()
%%   User = string()
%%   Alias = string()
%%   SafeQuery = safequery()
%%   Result = {result, no_result | [term()]}
%%   Warning = {warning, no_warning | string()}
%%   Error = {error, no_error | string()}
execute_query({_,[],_}) ->
    execute_query(noquery);

execute_query({_,[],_,_}) ->
    execute_query(noquery);

execute_query({'query',Q=[C|_],User}) when is_integer(C)->
    execute_query({'query',{Q, undefined, undefined},User});

execute_query({'query',{Query, File, Pos},User}) ->
    ?NITRO_HELPER:init_qtab(),
    SafeQuery=lists:filter(?NITRO_HELPER:query_filter_fun(), Query),
    case ?NITRO_HELPER:find_in_qtab({SafeQuery, File, Pos}) of
        [_] -> execute_query({prev_query,{SafeQuery, File, Pos},User});
        [] -> execute_query({'query',{Query, File, Pos}, Query, User})
    end;

execute_query({'query',Q=[C|_], Alias, User}) when is_integer(C)->
    execute_query({'query',{Q, undefined, undefined},Alias, User});

execute_query({'query',{Query, File, Pos}, Alias, User}) ->
    ?NITRO_HELPER:init_qtab(),
    SafeQuery=lists:filter(?NITRO_HELPER:query_filter_fun(), Query),
    {StartOpt,Output,Save}=case {File, Pos} of
                 {[C|_],P} when is_integer(C) andalso is_integer(P) -> 
                     {[{file, File},{position, Pos}],formattednodes,true};
                 {nodequery,{PNode,Node,_Saveable}} ->
                    case get_type(Node) of
                        denied -> {error,other,false};
                        error -> {error,other,false};
                        _ -> {[{node_list,[PNode]}],formattednodes,true}
                    end;
                 _ -> {[],formattednodes,true}
             end,
    if
        StartOpt==error -> {{result, no_result},{warning,no_warning},
            {error, "Node does not exist anymore."}};
        true ->
            case ?NITRO_HELPER:calculate_result(Query,
                                [{positions,both}, {output,Output}], 
                                StartOpt,[{save_query_id, true}, {user, User}]) of
                {result, Result} ->
                    [R0] = ?MISC:pgetu([result],Result),
                    R = io_lib:format("~p",[R0]),
                    Hash = ?NITRO_HELPER:get_database_hash(),
                    (not Save) orelse (update_tab_if_needed({SafeQuery,File,Pos},
                                                            Query,User,Alias,R,Hash)),
                    {{result,R0},
                    {warning, get_possible_warning()},
                    {error,no_error}};
                E ->
                    {{result, no_result},{warning,no_warning},
                    {error, ?NITRO_HELPER:error_handler(E)}}
            end
    end;

execute_query({prev_query,SQ={_Query, File, Pos},U}) ->
    ?NITRO_HELPER:init_qtab(),
    [{_SQ,Q,A,_C,R,_U, H}] = ?NITRO_HELPER:find_in_qtab(SQ),
    case ?NITRO_HELPER:is_database_changed(H) of
        true -> execute_query({'query', {Q, File, Pos}, A, U});
        false ->
            update_tab_if_needed(SQ,Q,U,A,R,H),
            {{result, ?NITRO_HELPER:list_to_term(lists:flatten(R))},
             {warning, get_possible_warning()},
             {error,no_error}}
    end;

execute_query({unknown,QData={Query, File, Pos},User})->
    case determine_sq_request_type(Query) of
        sem_query -> execute_query({'query',{Query, File, Pos},User});
        skeleton -> try_parse_skeleton(QData, User)%;
        %_ -> ErrorStr="Error: Type could not determined.",
        %    {{result, no_result},{warning,no_warning},{error,ErrorStr}}
    end;

execute_query(noquery) ->
    [{result,[]}].

%% @doc Returns the given query's result in list
%% @spec execute_system_query(string(),
%%                            {query_display_opt,
%%                            proplist(),
%%                            needed_pattern,
%%                            char()}) -> [] | [string()]
execute_system_query(Query,{query_display_opt,Option,needed_pattern,Pattern})->
    R=case ?NITRO_HELPER:calculate_result(Query,Option) of
        {result,Res} -> Res;
        _ -> [{result,""}]
    end,
    [Result] = ?MISC:pgetu([result],R),
    ?NITRO_HELPER:get_result_list(Result,Pattern).
    
%% @doc Administrate query_table_v6: if User has not owned the query,
%% add User to owner list.
update_tab_if_needed(SafeQuery,Query,User,Alias,Result,Hash)->
    {Users, Comment} = case ?NITRO_HELPER:find_in_qtab(SafeQuery) of
                [] -> {[User], ""};
                [{_SQ,_Q,_A,C,_R,UL,_H}] ->{lists:usort([User|UL]), C}
    end,
    ?NITRO_HELPER:insert_to_qtab(SafeQuery,Query,Alias,Comment,
        Result,Users,Hash).

%% @doc Set the given name as alias for the given query
%% @spec set_alias(safequery(), string) -> ok | {error, string()}
set_alias(SafeQuery, Alias=[I|_]) when is_integer(I)->
    case ?NITRO_HELPER:find_in_qtab(SafeQuery) of
        [] -> {error, "Query was not found."};
        [{_SQ,Q,_A,C,R,Us,H}] ->
            ?NITRO_HELPER:insert_to_qtab(SafeQuery,Q,Alias,C,R,Us,H),
            ok
    end.

%%% ============================================================================
%%% Running queries
%% @doc Returns the list of the currently running queries.
%% @spec get_running_queries()-> [] | [{QueryId, QueryString}]
%%  QueryId = number()
%%  QueryString = string()
get_running_queries()->
    RunningQueries=case ?NITRO_CORE:make_ui_request({get_running_queries, default_output}) of
                       {error, _ } -> [];
                       {ok, R} ->  R
                   end,
    [{QId,proplists:get_value(querystr,Args)} || 
     {QId, _Pid, _Mod, Args} <- RunningQueries].

%% @doc Returns the list of the currently running queries and marks queries
%% which belong to the given user.
%% @spec get_running_queries(User)-> [] | [{QueryId, QueryString, Ownership}]
%%  User = string()
%%  QueryId = number()
%%  QueryString = string()
%%  Ownership = own | extraneous
get_running_queries(User=[C|_]) when is_integer(C)->
    lists:foldl(fun ({_,Usr,QueryId, QStr},AccIn) when Usr =:=User->
                        AccIn++[{ QueryId, QStr, own}];
                    ({_,_,QueryId, QStr},AccIn) ->
                        AccIn++[{ QueryId, QStr, extraneous}]
                end, [], ?NITRO_HELPER:get_rqtab_elements());

get_running_queries(_)->[].

%% @doc Kill all of the running queries which belong to the given user
%% @stop_running_queries(string()|atom())-> ok
stop_running_queries(User)->
    [kill_query(QueryID) ||
    {_,_,QueryID,_} <-?NITRO_HELPER:find_in_rqtab_by_user(User)],
    ok.

%% @doc Kills the query which belongs to the given query id.
%% @spec kill_query(number()) -> not_found | ok
kill_query(QueryId) when is_integer(QueryId)->
    case ?NITRO_CORE:make_ui_request({kill_query,QueryId}) of
        {error, _ } -> not_found;
        {ok, R} ->
            ?NITRO_HELPER:delete_from_rqtab({'_','_',QueryId,'_'}),
            R
    end;

kill_query(_)->
    not_found.
    
%%% ============================================================================
%%% Autocomplete semantic queries

%% @doc Returns the possible completions of the given query string.
%% @spec do_autocomplete(string()) -> [] | [{CompletedQS, Completion}]
%% CompletedQS = string()
%% Completion = string()
do_autocomplete(QueryString) when is_atom(QueryString) ->
    do_autocomplete(atom_to_list(QueryString));

do_autocomplete(QueryString)->
    List1=
    try
        List=refusr_ac:run(?NITRO_HELPER:filtered_query(QueryString)),
        [{lists:flatten(InCompleteEnt++Completion), 
          lists:flatten(QueryString++Completion)} ||
          {InCompleteEnt,Completion=[C|_]}<-List, 
          is_list(InCompleteEnt),is_integer(C)]
    catch 
        _:_ -> []
    end,
    List2=
    try
            do_autocomplete_skeleton(QueryString)
    catch 
        _:_ -> []
    end,
    lists:keysort(2, List1++List2).
    
%%% ============================================================================
%%% Enviroment Nodes

%% @doc Returns all of the appbase nodes
%% @spec get_appbases() -> [] | [string()] | {error, string()}
get_appbases()->
    case ?NITRO_CORE:make_ui_request({get_env, appbase}) of
        {ok, Result} -> Result;
        {error, {_Code, Message}} -> {error, "Error: "++ Message}
    end.

%% @doc Deletes the given appbase node
%% @spec del_appbase(string()) -> ok | not_found | {error, string()}
del_appbase(Path)->
    case ?NITRO_CORE:make_ui_request({del_env_val, appbase, Path}) of
        {ok, [ok]} -> ok;
        {ok, []} -> not_found;
        {error, {_Code, Message}} -> {error, "Error: "++ Message}
    end.

%% @doc Adds the given appbase node
%% @spec add_appbase(string()) -> ok | {error, string()}
add_appbase(Path=[C|_]) when is_integer(C) ->
    case filelib:is_dir(Path) of
        true ->
            case ?NITRO_CORE:make_ui_request({add_env, appbase, Path}) of
                {error, {_Code, Message}} -> {error, "Error: "++ Message};
                _ -> ok
            end;
        false -> {error, Path++" is not a directory."}
    end.

%%% ============================================================================
%%% Database's operations

%% @doc Add the given file, or directory to database
%% @spec add_to_db(string(), fun()) -> {result, ok} | {error, string()}
add_to_db(File=[C|_], PrinterFun) 
    when is_integer(C) andalso is_function(PrinterFun)->
    case io_lib:deep_char_list(File) of
        true ->
            case ?NITRO_CORE:make_ui_request({add_dir,File},
                    [{remote_printer_fun, PrinterFun}]) of
                {error, {_Code, Msg}} ->{error, "Error: " ++ Msg};
                R -> {result, R}
            end;
        false ->
            {error,"Error: bad file argument given"}
    end;
add_to_db(_,_)->
    {error, "Bad arguments given"}.

%% @doc Drop the given file from database
%% @spec drop_from_db(string(),PrinterFun::fun())->{result,ok}|{error, string()}
drop_from_db(File=[C|_], PrinterFun) 
    when is_integer(C) andalso is_function(PrinterFun)->
    case ?NITRO_CORE:make_ui_request({drop,File},
            [{remote_printer_fun, PrinterFun}]) of
        {error, {_Code, Msg}} ->{error, "Error: " ++ Msg};
        R -> {result, R}
    end;
drop_from_db(_,_)->
    {error, "Bad arguments given"}.

%% @doc Updates different root directory lists
%% @spec need_to_update() -> true
need_to_update() ->
    wf:session(db_hash,?NITRO_HELPER:get_database_hash()),
    OldServerDocRoot = ?NITRO_CORE:get_file_browser_server_root(),
    case OldServerDocRoot ==
        ?NITRO_CORE:get_file_browser_loaded_files_root() of
        true -> ?NITRO_HELPER:set_file_browser_docroot("no_path");
        false -> ?NITRO_HELPER:set_file_browser_docroot(OldServerDocRoot)
    end,
    true.

%% @doc Returns warning message if database contains files with error forms
%% @spec get_possible_warning() -> no_warning | string()
get_possible_warning() ->
    case ?NITRO_HELPER:is_error_in_database() of
        false -> no_warning;
        true -> ?NITRO_HELPER:error_handler(warning)
    end.


%% @doc Returns the list of errors if database contains any file with error form
%% @spec add_possible_warning() -> no_warning | {warning, Warning}
%% Warning = {FilePath :: string(), 
%%            StartPos :: integer(), 
%%            Length :: integer(),
%%            ErrorMessage :: string()}
add_possible_warning() ->
    case ?NITRO_HELPER:get_error_forms_in_database() of
        [] -> no_warning;
        Errors -> ?NITRO_HELPER:error_handler({warning, Errors})
    end.
    
%% @doc Starts database synchronization
%% @spec synchronize(PrinterFun::fun()) -> 
%%  {result, any()} | {error, Error :: string()}
synchronize(PrinterFun) when is_function(PrinterFun)->
    case ?NITRO_CORE:make_ui_request({synchronize},
                                     [{remote_printer_fun, PrinterFun}]) of
        {error, {_Code, Msg}} ->{error, "Error: " ++ Msg};
        R -> {result, R}
    end.

%%% ============================================================================
%%% Dependency graph
%% @doc Converts to SVG from the given .dot file
%% @spec svg(string(), lists()) -> ok | {error, string()}
svg(Source=[A|_], Target=[B|_]) when is_list(Source) andalso is_list(Target)
  andalso is_integer(A) andalso is_integer(B)->
    case file:read_file_info(Source) of
        {ok, _ } -> ok;
        {error,E} -> throw(?RefError(file_not_exists,[Source,E]))
    end,
    Res = os:cmd("dot -Tsvg "++Source++" -o"++Target),
    case Res of
        [] -> ok;
        _  -> throw(?RefError(os_error,[Res]))
    end.

%% @doc Generates dependency graph in module or function level 
%% with the given options
%% @spec generate_dependency_graph(proplist(),string()) -> 
%% {error, string()} | 
%% {string(), string()}
generate_dependency_graph(Options,User) when is_atom(User)->
    generate_dependency_graph(Options,atom_to_list(User));

generate_dependency_graph(Options,User) when is_list(User)->
    FileName=User++".dot",
    [{_,TargetDir}]= ?NITRO_HELPER:find_in_fbtab("images_docroot"),
    DotName=filename:join([TargetDir,FileName]),
    XOptions=Options++[{file_path, DotName},{output,simple_dot}],
%    Result = case proplists:get_value(level, XOptions) of
%        mod -> ?NITRO_HELPER:generate_dep_graph_mod_level(
%            proplists:delete(level, XOptions));
%        func -> ?NITRO_HELPER:generate_dep_graph_fun_level(
%            proplists:delete(level, XOptions))   
%    end,
    Result = ?NITRO_HELPER:generate_dep_graph(XOptions),
    case Result of
        {error, {_Code, Msg}} ->{error, "Error: " ++ Msg};
        E = {error, [C|_]} when is_integer(C) -> E;
        {ok, _} -> 
            try
                SvgName=filename:join([TargetDir,User++".svg"]),
                svg(DotName,SvgName),
                {DotName, SvgName}
            catch
                Err={M_,_,_} when is_atom(M_) -> {error, ?Error:error_text(Err)}
            end
    end;

generate_dependency_graph(_Options,_User) ->
    {error, "Bad argument was given"}.

%% @doc Generates dependecy graph in module or function level with the
%% given options
%% @spec print_dependency_graph(proplist(),string()) -> 
%% {error, string()} | 
%% {string(), string()}
print_dependency_graph(Options,User) when is_atom(User)->
    print_dependency_graph(Options,atom_to_list(User));

print_dependency_graph(Options,User) when is_list(User)->
%    Result = case proplists:get_value(level, Options) of
%        mod -> case proplists:get_value(gnode, Options) of
%                   undefined ->
%                       ?NITRO_CORE:make_ui_request({cyclic_mod_get_deps});
%                   Modules ->
%                       ?NITRO_CORE:make_ui_request({cyclic_mod_get_deps,
%                                                    Modules})
%               end;
%        func -> case proplists:get_value(gnode, Options) of
%                   undefined ->
%                       ?NITRO_CORE:make_ui_request({cyclic_fun_get_deps});
%                   Modules ->
%                       ?NITRO_CORE:make_ui_request({cyclic_fun_get_deps,
%                                                    Modules})
%               end
%    end,
    XOptions = Options ++ [{output,name_terms}],
    Result = ?NITRO_CORE:make_ui_request({draw_dep_graph,XOptions}),
    case Result of
        {ok, R} -> {result, R};
        {error, {_Code, Msg}} -> {error, "Error: " ++ Msg}
    end;

print_dependency_graph(_Options,_User) ->
    {error, "Bad argument was given"}.

%% @doc Generates dependency graph in functionblock level with the given options
%% @spec generate_fb_graph(proplist(), atom(), string()) -> 
%% {DotName, SvgName} | {error, ErrorMessage}
%% DotName = string()
%% SvgName = string()
%% ErrorMessage = string()
generate_fb_graph(Subjects, Type, User) when is_list(Subjects) andalso is_list(User)->
    FileName=User++".dot",
    [{_,TargetDir}]= ?NITRO_HELPER:find_in_fbtab("images_docroot"),
    DotName=filename:join([TargetDir,FileName]),
%    Options=[{regexp, Subjects}, {type, draw}, {dot, DotName}],
    Options=[{groups,Subjects}, 
                {file_path, DotName},{output,simple_dot},
                {level,mb}, {type, Type}],
    case ?NITRO_CORE:make_ui_request({draw_dep_graph, Options}) of
        {error, {_Code, Msg}} -> {error, "Error: " ++ Msg};
        E = {error, [C|_]} when is_integer(C) -> E;
        {ok, _} ->
            try
                SvgName=filename:join([TargetDir,User++".svg"]),
                svg(DotName,SvgName),
                {DotName, SvgName}
            catch
                Err={M,_,_} when is_atom(M) ->{error, ?Error:error_text(Err)}
            end
    end;

generate_fb_graph(_Options, _Type, _User) ->
    {error, "Bad argument was given"}.

generate_smart_graph(Options, User) when is_list(Options) andalso is_list(User)->
    case ?NITRO_CORE:make_ui_request({generate_smart_graph, Options}) of
        {error, {_Code, Msg}} -> {error, "Error: " ++ Msg};
        E = {error, [C|_]} when is_integer(C) -> E;
        {ok, {result, JSCode}} ->
            {result, JSCode}
    end;

generate_smart_graph(_, _)->
    {error, "Bad argument was given"}.    
    
%% @doc Deletes all of the files which are associated to the user
%% @spec delete_dependency_graphs_files(string()) -> ok
delete_dependency_graphs_files(User) when is_atom(User)->
    delete_dependency_graphs_files(atom_to_list(User));

delete_dependency_graphs_files(User) when is_list(User)->
    [{_,TargetDir}]= ?NITRO_HELPER:find_in_fbtab("images_docroot"),
    case file:list_dir(TargetDir) of
        {ok,FileNames}->
            lists:foreach(fun(Elem) ->
                             case lists:prefix(User++"_",Elem) 
                             andalso (lists:suffix(".dot",Elem) 
                             orelse lists:suffix(".svg",Elem)) of
                                true -> 
                                    file:delete(
                                        filename:join([TargetDir, Elem]));
                                false -> ok
                             end
                          end, FileNames);
        _ -> ok
    end.
    
%%% ============================================================================
%%% Duplicate code functions

%% @spec search_initial_clones(proplist()) -> {error, string()} | {result,any()}
%% @doc Makes request for searching initial clones 
search_initial_clones(Options)->
    case ?NITRO_CORE:make_ui_request(
            {duplicated_code_initial_clone, Options}) of
        {error, _ } -> {error, "Bad parameters."};
        {ok, {_, Duplicates}} -> {result, Duplicates}
    end.

%% @spec get_const_var_diff(node(), node()) -> {error,string()} | {result,any()}
%% @doc Makes request for calculating the difference between two given nodes.
get_const_var_diff(Node1, Node2)->
    case ?NITRO_CORE:make_ui_request(
            {duplicated_code_const_var_diff, Node1, Node2}) of
        {error, _} -> {error, "Bad parameters."};
        {ok, Result} ->  {result, Result}
    end.

%%% ============================================================================
%%% Queries and node operations

%% @doc Return start and end position of given node
get_node_position(Node) ->
    try
        Tokens=?NITRO_HELPER:syn_request(Node),
        {Start,_}=?NITRO_HELPER:query_request(?Token,pos,hd(Tokens)),
        {_,End}=?NITRO_HELPER:query_request(?Token,pos,lists:last(Tokens)),
        {Start-1,End}
    catch
        request_denied -> 0
    end.

%% @doc Returns line of given node
get_node_linenum(Node) ->
    try
        {{Line,_},{_,_}}=?NITRO_HELPER:query_request(?Token,linecol,
            hd(?NITRO_HELPER:syn_request(Node))),
        Line
    catch
        request_denied -> 0
    end.
    
%% @doc Returns line of given node at the start of it's comments
get_node_linenum_comment(Node) ->
    try
        Tokens=?NITRO_HELPER:syn_request(Node),
        #lex{data=T=#token{}}=?NITRO_HELPER:data_request(hd(Tokens)),
        WsLines=countlines(T#token.prews,0),
        {{Line,_},{_,_}}=?NITRO_HELPER:query_request(?Token,linecol,hd(Tokens)),
        Line-WsLines
    catch
        _:_ -> 0
    end.
    
countlines([],Sum) ->    Sum;
countlines([C|CS],Sum) when C==10-> countlines(CS,Sum+1); 
countlines([_|CS],Sum) -> countlines(CS,Sum).

%% @doc Converts given start position to line number in a file
position_to_linenum(File,StartPos) ->
    {Lines,_}=?Token:pos2lc(file:read_file(?File:abs_path(File)),StartPos),
    Lines.

%% @doc Converts then given node's contents to plain text
node_to_text(Node) ->
    try 
        ?NITRO_HELPER:data_request(Node),
        case Node of
            {_,clause,_} -> 
                FuncNode=?NITRO_HELPER:query_request(Node,
                    ?Query:seq([?Clause:form(),?Form:func()])),
                if
                    FuncNode==[] -> "";
                    true -> function_text(hd(FuncNode))
                end;
            {_,func,_} -> 
                function_text(Node);
            {_,module,_} -> 
                atom_to_list(?NITRO_HELPER:query_request(?Mod,name,Node));
            _ ->  only_text(Node)
        end
    catch
        request_denied -> "Database was in use.";
        _:_ -> "Node does not exist anymore."
    end.

get_clauses(Fun) ->
    ?NITRO_HELPER:query_request(Fun, ?Query:seq(?Fun:definition(), 
                                                ?Form:clauses())).

%% @doc Converts a function node to m:f/a format
function_text(FuncNode) ->
    atom_to_list(?NITRO_HELPER:query_request(?Mod,name,
                 hd(?NITRO_HELPER:query_request(FuncNode,?Fun:module()))))
    ++":"++atom_to_list(?NITRO_HELPER:query_request(
        ?Fun,name,FuncNode))
    ++"/"++integer_to_list(?NITRO_HELPER:query_request(
        ?Fun,arity,FuncNode)).

%% @doc Fun for queries starting from a node
node_query_fun(Query,{PNode,Node},Type,User) ->
fun() ->
    if
        Query=="@file.references" -> %custom query
            Error=[],
            Nodes=lists:flatten([?NITRO_HELPER:query_request(X,
                [{esub,back},{esub,back}]) || 
                X <- ?NITRO_HELPER:query_request(PNode,?Mod:references())]);
        true ->
            Req = {transform,semantic_query,
                  [{ask_missing,false},
                    {send_back_query_id, true},
                    {querystr,Query},
                    {display_opt,[{output,nodes}]},
                    {start_opt,[{node_list,[PNode]}]}]},
                case ?NITRO_CORE:make_ui_request(Req,
                    [{user,User},{querystr,Query}]) of
                {ok, {result,[{result,Result}]}} -> Error=[];
                {ok, {abort,{_,Error}}} -> Result=[{nodes,error}];
                {ok, {error,Err}} -> 
                    Error="Fatal error: "
                    ++lists:flatten(io_lib:format("~p",[Err])),
                        Result=[{nodes,error}];
                _ -> Error="Unknown error.",Result=[{nodes,error}]
            end,
            Nodes=proplists:get_value(nodes,Result)
    end,
    if
        Nodes==error ->
            {error,Query,Error};
        is_integer(hd(Nodes)) ->
            {integer_to_list(hd(Nodes)),Query,Type,Node};
        is_atom(hd(Nodes)) ->
            {atom_to_list(hd(Nodes)),Query,Type,Node};
        is_list(hd(Nodes)) andalso is_integer(hd(hd(Nodes))) ->
            {hd(Nodes),Query,Type,Node};
        true ->
            {filter_nodes(Nodes),Query,Type,Node}
    end
end.

%% @doc Get file in which the node is
get_file(Node) ->
    try
        FirstToken=hd(?NITRO_HELPER:syn_request(Node)),
        FileNode=hd(?NITRO_HELPER:query_request(FirstToken,?Token:file())),
        ?NITRO_HELPER:query_request(?File,path,FileNode)
    catch
        _:_-> "" 
    end.

%% @doc Get the container (clause/form) of a node
get_container({_,form,_}=Node) -> Node;
get_container({_,clause,_}=Node) -> Node;
get_container({_,typexp,_}=Node) -> 
    try
        hd(?NITRO_HELPER:query_request(Node,[{tattr,back}]))
    catch
        request_denied -> {0,denied,0};
        bad_node -> {0,bad_node,0};
        {bad_node,_} -> {0,bad_node,0}
    end;
get_container(Node) ->
    try
        PE=?NITRO_HELPER:query_request(Node,
            ?Query:seq([?Expr:clause(),?Clause:funcl()])),
        if
            PE==[] -> 
                PE2=?NITRO_HELPER:query_request(Node,
                    ?Query:seq([?Expr:nameof(),?Clause:form()])),
                if
                    PE2==[] -> 
                        FirstToken=hd(?NITRO_HELPER:syn_request(Node)),
                        hd(?NITRO_HELPER:query_request(FirstToken,
                                                       ?Token:form()));
                    true -> hd(PE2)
                end;
            true -> hd(PE)
        end
    catch
        request_denied -> {0,denied,0};
        bad_node -> {0,bad_node,0};
        {bad_node,_} -> {0,bad_node,0}
    end.

%% @doc Get type of node
get_type(Node) ->
    try
        NodeData=?NITRO_HELPER:data_request(Node),
        case NodeData of
            #form{type=module}        -> mod;
            #form{type=record}        -> recdef;
            #typexp{type=spec_field}  -> recfielddef;
            #form{type=macro}         -> macrodef;
            #expr{type=application}   -> funappl;
            #expr{type=implicit_fun}  -> funappl;
            #expr{type=funref}        -> funappl;
            #clause{type=fundef}      -> fundef;
            #expr{type=variable}      -> var;
            #expr{type=record_expr}   -> recexpr;
            #expr{type=record_field}  -> recfield;
            #expr{type=record_access} -> recexpr;
            #expr{type=record_update} -> recexpr;
            #expr{type=atom} ->
                Path=?NITRO_HELPER:query_request(Node,?Expr:fields()),
                if
                    Path/=[]          -> recfield;
                    true              -> atom
                end;
            _                         -> none
        end
    catch
        request_denied -> denied;
        _ -> error
    end.

%% @doc Get the semantic node of a syntax node
get_proper_node(Node) ->
    try
        Type=get_type(Node),
        case Type of
            macrodef -> N=[Node];
            mod -> N=?NITRO_HELPER:query_request(Node,?Form:module());
            recdef -> N=?NITRO_HELPER:query_request(Node,?Form:record());
            recfielddef -> N=?NITRO_HELPER:query_request(Node,[{fielddef,1}]);
            var -> N=?NITRO_HELPER:query_request(Node,?Expr:variables());
            funappl -> N=?NITRO_HELPER:query_request(Node,
                        ?Query:any(?Expr:function(),?Expr:dynfunction()));
            fundef -> N=?NITRO_HELPER:query_request(Node,
                        ?Query:seq([?Clause:form(),?Form:func()]));
            recexpr -> N=?NITRO_HELPER:query_request(Node,?Expr:record());
            recfield -> N=?NITRO_HELPER:query_request(Node,?Expr:field());
            atom -> N=[Node];
            _ -> N=[]
        end,
        if
            N==[] -> error;
            true -> hd(N)
        end
    catch
        request_denied -> deny
    end.

%% @doc Manage the result of a query
filter_nodes([Node|Nodes]) -> 
    try
        filter_nodes_([Node|Nodes],true)
    catch
        request_denied -> error
    end;
filter_nodes([]) -> [].
filter_nodes_([Node|Nodes],First) ->
    case Node of
        {_,func,_} ->
            N=?NITRO_HELPER:query_request(Node,
                  ?Query:seq([?Fun:definition(),
                              ?Form:clauses()]));
        {_,record,_} ->
            N=?NITRO_HELPER:query_request(Node,?Rec:form());
        {_,typexp,_} ->
            N=?NITRO_HELPER:query_request(Node,[{flex, back},{form, back}]);
        {_,module,_} ->
            N=?NITRO_HELPER:query_request(Node,
                ?Query:seq([?Mod:file(),?File:form(1)]));
        {_,_,_} -> 
            N=[Node];
        _ -> N=?NITRO_HELPER:query_request(
            ?Query:seq([?Mod:find(Node),?Mod:file(),?File:form(1)]))
    end,
    if
        N==[] -> filter_nodes_(Nodes,First);
        true ->
            File=get_file(hd(N)),
            Type=get_type(hd(N)),
                if
                    (File/="") and (Type/=none) ->
                        %if
                            %First==true ->
                                %referl_htmlserver:generate(File);
                            %true -> ok
                        %end, 
                        N++filter_nodes_(Nodes,false);
                    (File/="") -> 
                        N++filter_nodes_(Nodes,First);
                    true -> 
                        filter_nodes_(Nodes,First)
                end
    end;
filter_nodes_([],_) -> [].

%% @doc Returns text without whitespaces
only_text(Node) ->
    try
        [[T#token.text] || Token <- ?NITRO_HELPER:syn_request(Node), 
        #lex{data=T=#token{}} <- [?NITRO_HELPER:data_request(Token)]]
    catch
        request_denied -> ""
    end.
    
%% @doc Get function node from m:f/a
get_function(Module,Function,Arity) ->
	?NITRO_HELPER:query_request(?Query:seq([?Mod:find(list_to_existing_atom(Module)),
		?Fun:find(list_to_existing_atom(Function),list_to_integer(Arity))])).
        
%% @doc Generate html for contents of node
generate_node(Node,Window) ->
    ?NITRO_HELPER:generate_node_request(Node,Window).
    
get_mods() ->
    try
        [atom_to_list(?Mod:name(M)) || 
            M<-?NITRO_HELPER:query_request(?Mod:all())]
    catch
        request_denied -> []
    end.

get_funs() ->
    try
        [function_text(F) || F<-?NITRO_HELPER:query_request(
            ?Query:seq([?Mod:all(),?Mod:locals_all()]))]
    catch
        request_denied -> []
    end.
    
get_filehash(Filename) ->
    try
        Files=?NITRO_HELPER:query_request(?File:find(Filename)),
        if 
            Files == [] -> -1;
            true -> ?NITRO_HELPER:query_request(?File,hash,hd(Files))
        end
    catch
        request_denied -> -1
    end.
    
convert_query_if_skel(Query) ->
    case ?NITRO_SKELETONS:determine_sq_request_type(Query) of
        sem_query -> Query;       
        skeleton -> 
            {ok, Elements, _}=erl_scan:string(Query),
            {_,_, SkelName}=hd(Elements),
            ParamsList=re:split(Query,"[``]",[{return,list}]),
            Parameters=case length(ParamsList) of
                1 ->[]; %skeleton can contain 0 parameters
                _ -> ?NITRO_SKELETONS:even_list_elements(ParamsList)
            end,
            ?NITRO_SKELETONS:evaluate_skeleton(atom_to_list(SkelName), 
                                               Parameters,wf:user(),onlyconvert)
    end.

%%% ============================================================================
%%% Skeletons

%% @doc Replaces the body and the owner of the skeleton, 
%% which is identified by the given name,
%% with the given new body and with the given owner.
%% @spec update_skeleton(Name::string(), NewBody::string(), Owner::string())->
%% ok | {error, ErrorMessage::string()}
update_skeleton(Name=[A|_],NewBody=[B|_],Owner=[C|_]) when is_integer(A) 
    andalso is_integer(B) andalso is_integer(C)  ->
    case ?NITRO_CORE:make_ui_request({update_skeleton, 
                                      Name, NewBody, Owner}) of
                       {ok, R} ->  R;
                       E       ->  E
    end.

%% @doc Updates the comment of a previously stored skeleton based on 
%% the given skeleton name.
%% @spec update_prev_skeleton_comment(Name::string(), NewComment::string())-> ok
update_prev_skeleton_comment(Name=[A|_], NewComment) when is_integer(A)
    andalso is_list(NewComment)->
    case ?NITRO_CORE:make_ui_request({update_prev_skeleton_comment, 
                                      Name, NewComment}) of
                       {ok, R} ->  R;
                       E       ->  E
    end.

%% @doc Deletes the skeleton which is identified by the given name
%% @spec delete_skeleton(Name::string())-> ok
delete_skeleton(Name)  ->
    case ?NITRO_CORE:make_ui_request({delete_skeleton, Name}) of
                       {ok, R} ->  R;
                       E       ->  E
    end.       

%% @doc Composes a valid skeleton call based on the given skeleton name.
%% @spec skeleton_call_format(Name::string())->string()
skeleton_call_format(Name=[C|_]) when is_integer(C) ->
    case ?NITRO_CORE:make_ui_request({skeleton_call_format, Name}) of
                       {ok, R} ->  R;
                       E       ->  E
    end.

%% @doc Saves a new skeleton from the given parameters.
%% @spec save_skeleton(Name::string(), Body::string(), Owner::string())->
%% ok | {error, ErrorMessage::string()}
save_skeleton(Name=[A|_], Body=[B|_], Owner=[C|_]) when is_integer(A)
    andalso is_integer(B) andalso is_integer(C) ->
    case ?NITRO_CORE:make_ui_request({save_skeleton, Name, Body, Owner}) of
                       {ok, R} ->  R;
                       E       ->  E
    end.


%% @doc Returns a list of the data of the stored skeletons.
%% @spec list_skeletons()->[]|[Skeleton]
%% Skeleton = {Name, Body, Owner, ParamCardinality, Comment}
%% Name = string()
%% Body = string()
%% Owner = string()
%% ParamCardinality = integer()
%% Comment = string()
list_skeletons() ->
    case ?NITRO_CORE:make_ui_request({list_skeletons}) of
                       {ok, R} ->  R;
                            E  ->  E
    end.         

%% @doc Evaluate the skeleton, which is identified by the given name, with the
%% given actual parameters than try to execute it as a semantic query string.
%% @spec evaluate_skeleton(Name::string(), Parameters, User::string()) ->
%% {Result, Warning, Error} | {error, Error}
%%   Result = {result, no_result | [term()]}
%%   Warning = {warning, no_warning | string()}
%%   Error = {error, no_error | string()}
%%   Parameters = []| [string()]
evaluate_skeleton(Name=[A|_], Parameters, User=[B|_]) when is_integer(A) 
  andalso is_integer(B) andalso is_list(Parameters)->
    case ?NITRO_CORE:make_ui_request({evaluate_skeleton, Name,
                                      Parameters, User}) of
        {ok, {error, Error}}         -> 
                    {{result, no_result}, {warning,no_warning}, {error, Error}};
        {ok, {'query', Query, User}} ->
                    ?NITRO_SERVICES:execute_query({'query', Query, User});
        {error, Error}                   ->
                                    {error, Error}
    end.

evaluate_skeleton(Name=[A|_], Parameters, _User, onlyconvert)
     when is_integer(A) andalso is_list(Parameters)->
    case ?NITRO_CORE:make_ui_request({evaluate_skeleton, Name,
                                      Parameters, dummy, onlyconvert}) of
                       {ok, R} -> R;
                       E       -> E
    end.


try_parse_skeleton(T={_Call, _File, _Pos}, User)  ->
    case ?NITRO_CORE:make_ui_request({try_parse_skeleton, T, User}) of
          {ok, {error, Error}} ->  
                    {{result, no_result}, {warning,no_warning}, {error, Error}};       
                       {ok, R} ->  execute_query(R);
                       E       ->  E
    end. 
        
determine_sq_request_type(Call=[C|_]) when is_integer(C)->
    case ?NITRO_CORE:make_ui_request({determine_sq_request_type, Call}) of
                       {ok, R} ->  R;
                       E       ->  E
    end.

do_autocomplete_skeleton(Call)  ->
    case ?NITRO_CORE:make_ui_request({do_autocomplete_skeleton, Call}) of
                       {ok, R} ->  R;
                       E       ->  E
    end.

%%% ============================================================================
%%% Helper functions for Nitrogen framework

%% @doc Returns file contents of the given filename
%% @spec get_source(string())-> {data, string()} | {error, string()}
get_source(FileName)->
    case file:open(FileName,[read]) of
        {ok,_IODevice} ->
            case file:read_file(FileName) of
                {ok, Data} -> {data,Data};
                {error,Reason} -> {error, "Error: " ++ Reason}
            end;
        {error,Reason} ->
            {error, io_lib:format("Error: ~p",[Reason])}
    end.
