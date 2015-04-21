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

%%% @author Matyas Kuti

-module(referl_qt).

-compile(export_all).

%% Includes
-include("ui.hrl").
-include_lib("referl_core/include/core_export.hrl").

%% Macros
-define( DBHASH, get_database_hash() ).
-define( ERROR(E), case E of 
    {{_M, _T, _D}, ErrTxt}      
        -> {error, ErrTxt}; 
    {_,ErrTxt}                  
        -> {error, ErrTxt};
    ErrTxt when is_list(ErrTxt)   
        -> {error, ErrTxt}; 
    _    
        -> {error, "Unknown error!"} end).

-define( STRERROR(E), {error, E} ).

%% Records
-record(q_info, {safeq, qstr, users, result, hash}).
-record(inv, {name, hash, users, invdata}).
-record(invnode, {name, show, node, text, label, memo, file_data, pos}).

%% @doc Start the Qt Gui with the node name as username.
%% @spec start() -> ok
start() ->
    UserName = atom_to_list(node()),
    start(UserName).

%% @doc Start the Qt Gui with the given username.
%% @spec start(string()) -> ok
start(UserName) ->
    NodeName = node(),
    User = get_user(UserName),
    Pid = spawn( fun() -> start_(User) end ),
    case whereis(referl_qt) of
        undefined -> 
            register(referl_qt, Pid);
        _ ->
            exit(whereis(referl_qt), kill),
            unregister(referl_qt),
            register(referl_qt, Pid)
    end,
    ?UI:add_msg_handler(Pid), 
    Binary = filename:dirname(code:which(?MODULE)) ++ "/../qt/bin/RefErlQt",
    case filelib:is_file(Binary) of
        false ->
            io:format("Qt GUI is not available. Make sure you compile the tool with the command: bin/referl -built tool -qt_gui~n"),
            exit(whereis(referl_qt), kill),
            unregister(referl_qt),
            case init:get_argument(noshell) of
                {ok, _} -> init:stop();
                _ -> ok
            end;
        true ->
            StartGuiCommand = Binary ++ " " ++ atom_to_list(NodeName) ++ " " ++ atom_to_list( erlang:get_cookie() ), 
            spawn( fun() -> io:format("~p~n", [os:cmd(StartGuiCommand)]) end ),
            ok
    end.

start_(UserName) ->
    receive
        {GuiPid, pid} ->
            GuiPid ! {ok, UserName},
            handle_gui_request(is_client, GuiPid),
            handle_gui_request(dupcode_algorithm_data, GuiPid),
            loop(GuiPid)
    end.

get_user([S | _]=UserName) ->
    if 
       is_integer(S) -> UserName;
       true ->  atom_to_list(S)
    end.

%% @doc Loop
%% @spec loop(atom()) -> ok
loop(GuiPid) ->
    receive
        terminate -> handle_gui_request(terminate, GuiPid);
        Request ->
            try
                handle_gui_request(Request, GuiPid)
            catch
                request_denied -> 
                    GuiPid ! ?STRERROR("The database is busy. Request denied.");
                _:_ ->
                    GuiPid ! fatal
            end,
            loop(GuiPid)
    end.

%% Gui request handlers

%Database and file
handle_gui_request({reset_db, PositioningMode}, GuiPid) ->
    Success = reset_db(PositioningMode),
    GuiPid ! {reset_db, Success};
    
handle_gui_request({add_appbase, Path}, GuiPid) ->
    GuiPid ! {add_appbase, add_appbase(Path)};
    
handle_gui_request({add_include, Path}, GuiPid) ->
    GuiPid ! {add_include, add_include(Path)};
    
handle_gui_request({del_env, Path, Type}, GuiPid) ->
    GuiPid ! {del_env, del_env(Path, Type)};
    
handle_gui_request({drop_file, Path}, GuiPid) ->
    GuiPid ! {drop_file, drop_file(Path, GuiPid)};
    
handle_gui_request({add_dir, Path}, GuiPid) ->
    GuiPid ! {add_dir, add_dir(Path, GuiPid)};
    
handle_gui_request({drop_dir, Paths}, GuiPid) ->
    GuiPid ! {drop_dir, drop_dir(Paths, GuiPid)};
        
handle_gui_request({cat_file, Path}, GuiPid) ->
    GuiPid ! {cat_file, Path, cat_file(Path)};

handle_gui_request(files, GuiPid) -> 
    GuiPid ! {files, get_file_list()};
    
handle_gui_request(errors, GuiPid) ->
    GuiPid ! {errors, get_error_list()};

handle_gui_request(sync_db, GuiPid)-> 
    GuiPid ! sync_db();

handle_gui_request(get_envs, GuiPid) ->
    GuiPid ! {get_envs, get_envs()};
    
handle_gui_request(modules, GuiPid) ->
    GuiPid ! {modules, get_modules()};

handle_gui_request(functions, GuiPid) -> 
    GuiPid ! {functions, get_functions()};
    
handle_gui_request(db_hash, GuiPid) ->
    GuiPid ! {db_hash, ?DBHASH};

handle_gui_request(is_client, GuiPid) ->
    GuiPid ! {is_client, ?MISC:is_client()};

%Dependency graph
handle_gui_request({draw_svg, Path, Options}, GuiPid) ->
    GuiPid ! {draw_svg, draw_svg(Path, Options)};

handle_gui_request({print_graph, _Path, Options}, GuiPid) ->
    GuiPid ! {print_graph, print_graph(Options)};

handle_gui_request({get_graph, _Path, Options}, GuiPid) ->
    GuiPid ! {get_graph, get_graph(Options)};
    
%Queries
handle_gui_request({queries, UserName}, GuiPid) ->
    R = {queries, get_users_query_list(UserName)},
    GuiPid ! R;
    
handle_gui_request({run_query, UserName, QStr, File, Pos}, GuiPid) ->
    spawn( fun() -> 
        GuiPid ! {run_query, 
            run_query_or_skeleton(UserName, QStr, {File, Pos}, GuiPid), 
            QStr}
        end 
    );
    
handle_gui_request({delete_query, QStr, File, Pos}, GuiPid) ->
    GuiPid ! {delete_query, delete_from_qtab({QStr, File, Pos})};

handle_gui_request({kill_query, QueryID}, GuiPid) ->
    GuiPid ! {kill_query, kill_query(QueryID)};
    
handle_gui_request(running_queries, GuiPid) ->
    GuiPid ! {running_queries, running_queries()};
    
handle_gui_request(skeletons, GuiPid) ->
    GuiPid ! {skeletons, list_skeletons()};

handle_gui_request({save_skeleton, Name, Skeleton, Comment, UserName}, GuiPid) ->
    GuiPid ! {save_skeleton, 
        save_skeleton(Name, Skeleton, Comment, UserName), Name};
    
handle_gui_request({delete_skeleton, Name}, GuiPid) ->
    GuiPid ! {delete_skeleton, delete_skeleton(Name), Name};

handle_gui_request({modify_skeleton, Name, Skeleton, Comment, User}, GuiPid) ->
    GuiPid ! {modify_skeleton, 
        update_skeleton(Name, Skeleton, Comment, User), Name};
    
handle_gui_request({skeleton_call_format, Name}, GuiPid) ->
    GuiPid ! {skeleton_call_format, skeleton_call_format(Name)};

handle_gui_request({autocomplete_query, Str}, GuiPid) ->
    GuiPid ! {autocomplete_query, autocomplete(Str)};
    
handle_gui_request({predef_query, Path, Position}, GuiPid) ->
    GuiPid ! {predef_query, get_predef_queries(Path, Position)};

%Duplicated code
handle_gui_request({dupcode_search, Options}, GuiPid) -> 
    spawn( 
        fun() ->
            try
                GuiPid ! {dupcode_search, run_dupcode_analysis(Options)}
            catch
                request_denied -> 
                    GuiPid ! ?STRERROR("The database is busy. Request denied.");
                _:_ ->
                    GuiPid ! fatal
            end
        end 
    );

handle_gui_request(dupcode_algorithm_data, GuiPid) ->
    GuiPid ! {dupcode_algorithm_data, get_dupcode_algorithm_data()};

handle_gui_request({previous_dupcode_search, Name}, GuiPid) ->
    spawn( 
        fun() ->
            try
                GuiPid ! {dupcode_search, run_previous_dupcode_analysis(Name)}
            catch
                request_denied -> 
                    GuiPid ! ?STRERROR("The database is busy. Request denied.");
                _:_ ->
                    GuiPid ! fatal
            end
        end
    );

handle_gui_request(previous_dupcode_names, GuiPid) ->
    GuiPid ! {previous_dupcode_names, get_dupcode_result_names()};

handle_gui_request({selected_dupcode, Algorithm, FilePath, Start, End}, GuiPid) ->
    spawn( 
        fun() ->
            
            try
                GuiPid ! {dupcode_search, run_selected_dupcode(Algorithm, FilePath, Start, End)}
            catch
                request_denied -> 
                    GuiPid ! ?STRERROR("The database is busy. Request denied.");
                _:_ ->
                    GuiPid ! fatal
            end
        end 
    );

%Investigations
handle_gui_request(investigations, GuiPid) ->
    GuiPid ! {investigations, list_investigations()};
    
handle_gui_request({load_investigation, Name}, GuiPid) ->
    GuiPid ! {load_investigation, load_investigation(Name)};
    
handle_gui_request({start_investigation, Module, Function, Arity, User}, GuiPid) ->
    spawn( fun() ->
            GuiPid ! {load_investigation, 
                start_investigation(Module, Function, Arity, User)} 
        end);

handle_gui_request({investigation_memo, ParentId}, GuiPid) ->
    spawn( fun() -> 
            GuiPid ! {investigation_memo, investigation_memo(ParentId)} 
        end );

handle_gui_request({investigation_query, ParentId, QueryStr, File, Position, User}, GuiPid) ->
    spawn( fun() -> 
            GuiPid ! {investigation_query, 
            investigation_query(ParentId, QueryStr, 
                                File, Position, User)} 
        end);

handle_gui_request({delete_investigation, Name, User}, GuiPid) ->
    GuiPid ! {delete_investigation, 
        delete_investigation(Name, User), Name};
    
handle_gui_request({share_investigation, Name, User}, GuiPid) ->
    GuiPid ! {share_investigation, 
        share_investigation(Name, User), Name, User};

handle_gui_request({save_investigation, Name, User, Hash, InvData}, GuiPid) ->
    GuiPid ! {save_investigation, 
        save_investigation_safe(Name, User, Hash, InvData), Name};

handle_gui_request(terminate, GuiPid) -> 
    GuiPid ! terminate,
    case ?MISC:is_client() of
        false -> ?UI:del_msg_handler(self())
    end,
    case init:get_argument(noshell) of
        {ok, _} -> init:stop();
        _ -> ok
    end;

handle_gui_request({global,jobinfo,
        {{_,modifier,finished,
                  _}, _ }} , GuiPid) -> 
    handle_gui_request(files, GuiPid),
    handle_gui_request(modules, GuiPid),
    handle_gui_request(errors, GuiPid),
    handle_gui_request(get_envs, GuiPid),
    handle_gui_request(functions, GuiPid),
    handle_gui_request(db_hash, GuiPid);

handle_gui_request(_, _) -> ok.

%%% ============================================================================

%%% Database related functions - modifications and getters

%% Adding and dropping functions return the path of the handled object(s) on
%% success.

%% @doc Return a files contents
%% @spec cat_file(string()) -> string()
cat_file(Path) ->
    case ui({cat_file, Path}) of
        {error, Error} -> ?ERROR(Error);
        Content -> {ok, lists:flatten(Content)} 
    end.

%% @doc Return the list of environments with their types
%% @spec get_envs() -> [ {atom(), term()} ]
get_envs() ->
    AppBases = [ {appbase, I} || I <- ui({get_env, appbase}) ],
    Includes = [ {include, I} || I <- ui({get_env, include}) ],
    AppBases ++ Includes.

%% @doc Delete an environment from the database
%% @spec del_env(string(), atom()) -> string() | {error, string()}
del_env(Path, Type) ->
    case ui({del_env_val, Type, Path}) of
        [] -> [];
        [ok] -> Path;
        {error, Error} -> ?ERROR(Error)
    end.

%% @doc Add an appbase environment to the database
%% @spec add_appbase(string()) -> string() | {error, string()}
add_appbase(Path) ->
    add_env(Path, appbase).

%% @doc Add an include environment to the database
%% @spec add_include(string()) -> string() | {error, string()}
add_include(Path) ->
    add_env(Path, include).

%% @doc Add an environment to the database of the given type
%% @spec add_env(string(), atom()) -> string() |  {error, string()}
add_env(Path, Type) ->
    case ui({add_env, Type, Path}) of
        {error, Error} -> ?ERROR(Error);
        _ -> Path
    end.

%% @doc Add a directory of files to the database
%% @spec add_dir(string(), pid()) -> string() | {error, string()}
add_dir(Path, GuiPid) ->
    case ui( {add_dir, Path}, GuiPid) of
        {error, Error} -> ?ERROR(Error);
        _ -> Path
    end.

%% @doc Drop a file from the database
%% @spec drop_file(string(), pid()) -> [string()] | {error, string()}
drop_file(Path, GuiPid) ->
    drop_dir([Path], GuiPid).

%% @doc Drop a directory from the database
%% @spec drop_dir(string(), pid()) -> [string()] | {error, string()}
drop_dir(Paths, GuiPid) ->
    case ui({drop_files, Paths}, GuiPid) of
        {error, {{reflib_error,file_not_present,_},_}} -> Paths; 
        {error, Error} -> ?ERROR(Error);
        _ -> Paths
    end.

%% @doc Return the list of loaded files' paths
%% @spec get_file_list() -> [string()] | deny
get_file_list() -> 
    try
        [
            begin
                case File of
                    {error, Error} -> ?ERROR(Error);
                    _ -> query_request(?File, path, [File]) 
                end
            end || File <- ui({graph_query, ?Query, exec, [[file]]})
        ]
    catch
        request_denied -> deny
    end.

%% @doc Return the list of loaded directories' paths
%% @spec get_directory_list() -> [string()] | deny
get_directory_list() ->
    try
        case ui({dir_sort}) of
            {error, _} -> [];
            Directories -> [ Dir || {Dir, _} 
                <- Directories, is_list(Dir), Dir =/= "Other"] 
        end
    catch
        request_denied -> deny
    end.

%% @doc Return the list of errors present in the database
%% @spec get_error_list() -> [{string(), integer(), integer(), string()}] | deny
get_error_list() ->
    ErrorQuery = ?Query:seq([file], ?File:error_forms()),
    try
        case query_request(ErrorQuery) of
            [] -> [];
            ErrorForms -> referl_ui_nitrogen_helper:get_errors(ErrorForms) 
        end
    catch
        request_denied -> deny
    end.

%% @doc Reset the database in the given positioning mode (rel or abs)
%% @spec reset_db(rel | abs) -> ok | {error, string()}
reset_db(PositioningMode) ->
    case ui( {reset, PositioningMode} ) of
        [] -> ok;
        {error, Error} -> ?ERROR(Error)
    end.

%% @doc Syncronise the database
%% @spec sync_db() -> ok | {error, string()}
sync_db() ->
    ui({synchronize}).

%% @doc Returns if the database is readable
%% @spec can_read_db() -> boolean()
can_read_db() ->
    ?UI:can_read_db(). 

%% @doc Return the database hash
%% @spec get_database_hash() -> term()
get_database_hash() ->
    case ui({database_hash}) of
        {error, Error} -> ?ERROR(Error);
        Hash -> Hash
    end.

%% @doc Returns if the database has changed by comparing the given hash
%% to the current one
%% @spec is_database_changed(term()) -> boolean()
is_database_changed(Hash) ->
    Hash /= get_database_hash().

%% @doc Return the names of the loaded modules
%% @spec get_modules() -> [string()]
get_modules() ->
    try
        [atom_to_list(query_request(?Mod,name,M)) || 
            M <-query_request(?Mod:all())]
    catch
        request_denied -> []
    end.

%% @doc Return the names of the loaded functions
%% @spec get_functions() -> [string()]
get_functions() ->
    try
        [function_text(F) || F <- query_request(
            ?Query:seq([?Mod:all(),?Mod:locals_all()]))]
    catch
        request_denied -> []
    end.
    

%% @doc Return the file names of the loaded modules
%% @spec get_modpath() -> [string()]
get_modpath() ->
    try
	    lists:usort([ filename:basename(?File:path(Fnode))
	         || Fnode <- query_request(?Query:seq([?File:all()]))])
	catch
	    request_denied -> []
    end.    


%%% ============================================================================

%%% Dependency graph drawing functions

%% The functions return the path of the generated files

%% @doc Download the path from the server if refactorerl is in client mode and
%% return the path of the downloaded file, else return the current path
%% @spec get_file_if_needed(string()) -> string() | {error, string()}
get_file_if_needed(FilePath) ->
	    IsClient = ?MISC:is_client(),
	    if IsClient ->
	            case ui({get_file_from_server, FilePath}) of
	                {error, Error} ->
	                    ?ERROR(Error);
	                Binary ->
	                    Name = filename:basename(FilePath),
	                    LocalPath = ?MISC:data_dir() ++ "/"  ++ Name,
	                    ok = file:write_file(LocalPath, Binary),
	                    LocalPath
	            end;
	        true -> FilePath
	    end.

%% @doc Run dependency analysis on the database with the given parameters
%% @spec dependency_analysis(proplist()) -> 
%% string() | {error, string()}
dependency_analysis(Options) ->
    DotPath = 
        filename:join( 
            ?MISC:data_dir(), 
            "latest_dependency_res" ++ ?MISC:graph_based_postfix()
        ),
    Opts = Options ++ [{file_path, DotPath}],
    R = ui({draw_dep_graph, Opts}),
    case R of
        {error, Error} -> ?ERROR(Error);
        _ -> DotPath
    end.

%% @doc Generate an SVG image of the dependency graph with the given parameters
%% @spec draw_svg(string(), porplist()) -> 
%% string() | {error, string()}
draw_svg(Path, Options) ->
    try
        R = dependency_analysis(Options),
        case R of
            {error, Error} -> ?ERROR(Error);
            DotPath -> 
                    case get_file_if_needed(DotPath) of
                        {error, Error} -> ?ERROR(Error);
                        File ->
                            Cmd = "dot -Tsvg -o"++Path++" "++File,  
                            [] = os:cmd(Cmd),
                            Path
                    end            
        end
    catch
        request_denied -> ?STRERROR("The database is busy. Request denied.");
        _:_ -> ?STRERROR("Error during SVG creation. 
                Check GraphViz isntallation and dot command availabilty!")
    end.

%% @doc Generate plain text .dot file representation of the dependency graph
%% @spec print_graph(proplist()) -> string() | {error, string()}
print_graph(Options) ->
    try
        R = dependency_analysis(Options),
    case R of
        {error, Error} -> ?ERROR(Error);
        DotPath -> 
            case get_file_if_needed(DotPath) of
                    {error, Err1} -> {error, Err1};
                    File ->
                        Cmd = "dot -Tplain-ext " ++ DotPath ++ " -o " 
                            ++ DotPath ++ "_gen.dot", 
                        case os:cmd(Cmd) of
                            "Warning" ++ _ -> ok;
                            [] -> ok;
                            Err2 -> throw({cmd_error, Err2})
                        end,
                        DotPath ++ "_gen.dot"
            end   
    end
    catch
        {cmd_error, Err3} -> 
            {error, "Error when executing dot command: " ++ Err3};
        request_denied -> ?STRERROR("The database is busy. Request denied.");
        _:_ -> ?STRERROR("Unknown error!")
    end.

get_graph(Options) ->
    Opts = Options ++ [{output, terms}],
    Result = ui({draw_dep_graph, Opts}),
    case Result of
        {error, Error} -> 
            ?ERROR(Error);
        _ ->
            Nodes = proplists:get_value(nodes, Result),
            Edges = proplists:get_value(links, Result),
            NormalizedNodes = lists:map(fun(NodeInfoList) -> 
                                            Id = proplists:get_value(id, NodeInfoList),
                                            Label = proplists:get_value(name, NodeInfoList),
                                            NodeType = proplists:get_value(type, NodeInfoList),
                                            {Id, Label, NodeType}
                                        end,
                                        Nodes),
            NormalizedEdges = lists:map(fun(EdgeInfoList) ->
                                            Source = proplists:get_value(source, EdgeInfoList),
                                            Target = proplists:get_value(target, EdgeInfoList),
                                            {Source, Target}
                                        end,
                                        Edges),
            {NormalizedNodes, NormalizedEdges}
    end.


%%% ============================================================================
%%% Queries

%% @doc Run a query or skeleton using node output
%% @spec run_query_or_skeleton_node(string(), string(), 
%%          {string() | no_file, string() | no_pos}) -> [node()] | {error, string()}
run_query_or_skeleton_node(UserName, QStr, {File, Pos}) ->
    DisplayOpt = [{positions, scalar}, {output, nodes}],
    try
        run_query_or_skeleton(UserName, QStr, true, 
                DisplayOpt, {File, Pos}, nopid)
    catch
        request_denied -> ?STRERROR("The database is busy. Request denied.")
    end.

%% @doc Run a query or skeleton
%% @spec run_query_or_skeleton(string(), string(), 
%%          {string() | no_file, string() | no_pos}, pid()) 
%%              -> term() | {error, string()}
run_query_or_skeleton(UserName, QStr, {File, Pos}, GuiPid) ->
    try
    DisplayOpt = [{positions, scalar}, {output, other}],
    QRes = run_query_or_skeleton(UserName, QStr, true, 
            DisplayOpt, {File, Pos}, GuiPid),
    preprocess_query_result(QRes)
    catch
        request_denied -> ?STRERROR("The database is busy. Request denied.")
    end.

run_query_or_skeleton(UserName, QStr, SendBackID, 
                        DisplayOpt, {File, Pos}, GuiPid) ->
    case ui({determine_sq_request_type, QStr}) of
        sem_query ->
            run_query(UserName, QStr, SendBackID, 
                        DisplayOpt, {File, Pos}, GuiPid);
        skeleton ->
            try_parse_n_run_skeleton({QStr, File, Pos}, UserName, 
                                        SendBackID, DisplayOpt, GuiPid);
        {error, Error} -> ?ERROR(Error)
    end.

try_parse_n_run_skeleton(T={_QStr, File, Pos}, UserName, 
                            SendBackID, DisplayOpt, GuiPid)  ->
    case ui({try_parse_skeleton, T, UserName}) of
        {error, Error} -> 
            ?ERROR(Error);
        {_, QStr, _} -> 
            run_query(UserName, QStr, SendBackID, 
                        DisplayOpt, {File, Pos}, GuiPid)
    end. 

run_query(UserName, QStr, SendBackID, DisplayOpt, {File, Pos}, GuiPid) ->
    StartOpt = case {File, Pos} of
        {no_file, no_pos} -> [];
        _ -> [{file, File}, {position, Pos}]
    end,
    SafeQuery = {QStr, File, Pos},
    Hash = ?DBHASH,
    RefreshFun = 
        fun(Res)-> 
            if
                GuiPid =/= nopid ->
                    update_qtab_if_needed(SafeQuery, QStr, UserName, Res,Hash);
                true -> ok
            end
        end,
    case find_in_qtab(SafeQuery) of
        [] -> 
            run_query_helper(QStr, SendBackID, DisplayOpt, 
                                StartOpt, RefreshFun, UserName, GuiPid);
        [Rs] ->
            case is_previous_query(SafeQuery, UserName, Rs) of
                false -> 
                    run_query_helper(QStr, SendBackID, DisplayOpt, 
                                        StartOpt, RefreshFun, UserName, GuiPid);
                true ->
                    update_qtab_if_needed(SafeQuery, QStr, UserName, 
                        Rs#q_info.result, Hash),
                    Rs#q_info.result
            end
    end.

run_query_helper(QStr, SendBackID, DisplayOpt, 
                    StartOpt, RefreshFun, _UserName, GuiPid) ->
    case ui({transform, semantic_query, 
            [ {ask_missing, false}, 
            {send_back_query_id, SendBackID}, 
            {querystr, QStr}, 
            {display_opt, DisplayOpt}, 
            {start_opt, StartOpt} ]}, GuiPid) of
        {error, Error} -> ?ERROR(Error);
        {result, Result} -> 
            [Result_] = ?MISC:pgetu([result],Result),
            RefreshFun(Result_),
            Result_;
        {abort, {_, Error}} -> ?ERROR(Error)
    end.

determine_sq_request_type(Call=[C|_]) when is_integer(C) ->
    case ui({determine_sq_request_type, Call}) of
        {error, Error} -> ?ERROR(Error);
        Result -> Result
    end.

is_previous_query({_QStr, _File, _Pos}, _User, Res) ->
    not is_database_changed(Res#q_info.hash).

%% @doc Get the list of running queries (id and query string tuples)
%% @spec running_queries() -> [{integer(), string()}]
running_queries() ->
    QList = ui({get_running_queries, default_output}),
    lists:map( fun({QId, _, _, OptList}) -> 
                    {QId, proplists:get_value(querystr, OptList)} 
                end , QList).

%% @doc Kill the query with the given ID
%% @spec kill_query(integer()) -> ok | {error, string()}
kill_query(QueryID) when is_integer(QueryID) ->
    case ui({kill_query, QueryID}) of
        ok -> ok;
        not_found ->
            {error, "Query not found."};
        _ -> 
            {error, "Unknown error occoured while tring to kill the query."}
    end.

%% @doc List the queries of a user
%% @spec get_users_queries(string()) -> [q_info()] 
get_users_queries(User) ->
    QList = find_in_qtab_by_pattern({'_','_','_','_','_','_'}),
    case User of
        all -> QList;
        _ ->
            lists:filter(
                fun(Rec) -> lists:member(User,Rec#q_info.users) end,
                QList)
    end.

%% @doc List the queries of a user
%% @spec get_users_query_list(string()) -> 
%%          [{string(), string() | no_file, integer() | no_pos}] 
get_users_query_list(User) ->
    lists:map(fun(Rec) -> Rec#q_info.safeq end, get_users_queries(User)).

do_autocomplete_skeleton(Call) ->
    case ui({do_autocomplete_skeleton, Call}) of
        {error, _ } -> [];
        Res         -> Res
    end.

%% @doc List autocompletes for a query or skeleton
%% @spec autocomplete(string()) -> [{string(), string()}]
autocomplete(Call) ->
    List1=
    try
        List = refusr_ac:run(Call),
        [ { lists:flatten( InCompleteEnt ++ Completion ), 
           lists:flatten( Call ++ Completion ) } ||
          { InCompleteEnt, Completion = [ C | _ ] } <- List, 
          is_list( InCompleteEnt ), is_integer(C) ]
    catch 
        _:_ -> []
    end,
    List2 =
        try
            do_autocomplete_skeleton(Call)
        catch 
            _:_ -> []
        end,
    lists:keysort(2, List1 ++ List2).

%%% Skeletons

%% @doc List saved skeletons
%% @spec list_skeletons() -> 
%%          [{string(), string(), string(), integer(), string()}]
list_skeletons() ->
    case ui({list_skeletons}) of
        {error, Error} -> ?ERROR(Error);
        Result -> Result
    end.

save_skeleton(Name, Body, Comment, User) ->
    case ui({save_skeleton, Name, Body, User}) of
        {error, Error} -> ?ERROR(Error);
        _ -> ui({update_prev_skeleton_comment, Name, Comment})
    end.

delete_skeleton(Name) ->
    ui({delete_skeleton, Name}).

skeleton_call_format(Name) ->
    ui({skeleton_call_format, Name}).

update_skeleton(Name, NewBody, NewComment, User) ->
    case ui({update_skeleton, Name, NewBody, User}) of
	    {error, Error} -> ?ERROR(Error);
	    _ -> ui({update_prev_skeleton_comment, Name, NewComment})
    end.

%% Tables

insert_to_qtab(Rec) ->
    ui({insert_to_qtab, [Rec]}).

insert_to_qtab(SafeQuery={_,_,_}, QStr, Res, Users, Hash) ->
    ui({insert_to_qtab, SafeQuery, QStr, Res, Users, Hash}).

update_qtab_if_needed(SafeQuery, QStr, User, Result, Hash) ->
    ui({update_qtab_if_needed, SafeQuery, QStr, User, Result, Hash}).

find_in_qtab(SafeQuery) ->
    ui({find_in_qtab, SafeQuery}). 

find_in_qtab_by_pattern(Pattern)->
    ui({find_in_qtab_by_pattern, Pattern}).

delete_from_qtab(SafeQuery) ->
    ui({delete_from_qtab, SafeQuery}).

%%% Preprocess query result

preprocess_query_result(Result) ->
    case Result of
        {error, Error} -> ?ERROR(Error);
        _ -> preprocess_query_result_(Result)
    end.

preprocess_query_result_(Result) ->
    case Result of
        [] -> [];
        [ {group_by, {Position, Text}, list, List} | Tail ] ->
            [{{Position, Text}, preprocess_query_result_(List)}] 
                ++ preprocess_query_result_(Tail);
        [ {group_by, {Position, Text}, eq, Name, Value} | Tail ] ->
            [{{Position, Text}, eq, Name, Value, []}] 
                ++ preprocess_query_result_(Tail);
        [ {list, List} | Tail ] -> 
            preprocess_query_result_(List) ++ preprocess_query_result_(Tail);
        [ {Position, Text} | Tail ] ->
            [{{Position, Text}, []}] ++ preprocess_query_result_(Tail);
        [ {chain, List, _} | Tail ] -> 
            preprocess_query_result_(List) ++ preprocess_query_result_(Tail)
    end.

%%% Predefined queries

get_predef_queries(Path, Position) ->
    try
        Node = refusr_predef:get_node_and_type(Path, Position),
        case Node of
            {error, _} -> [];
            {_, Type} ->
                refusr_predef:get_predef_queries(Type, Path, Position)
        end
    catch
        _ -> []
    end.

%%% ============================================================================
%%% Investigations

%% @doc Convert a node to Mod:Fun/Arity format
%% @spec function_text(node()) -> string()
function_text(FunNode) ->
    ui({function_text, FunNode}).

get_file_path(Node) ->
    try
        FirstToken = hd(ui({syn_leaves, Node})),
        FileNode = hd(query_request(FirstToken,?Token:file())),
        query_request(?File,path,FileNode)
    catch
        _:_-> "" 
    end.

node_text(Node) ->
    case ui({syn_tree_text, Node}) of
        {error, _} -> "";
        Text        -> lists:flatten(Text)
    end.

node_to_text(Node) ->
    try 
        ui({graph_data, Node}),
        case Node of
            {_,clause,_} -> 
                FuncNode=query_request(Node,?Query:seq([?Clause:form(),?Form:func()])),
                if
                    FuncNode==[] -> "";
                    true -> function_text(hd(FuncNode))
                end;
            {_,func,_} -> 
                function_text(Node);
            {_,module,_} -> 
                atom_to_list(query_request(?Mod,name,Node));
            _ ->  node_text(Node)
        end
    catch
        _:_ -> "" 
    end.

get_clauses(FunNode) ->
    query_request(FunNode, ?Query:seq(?Fun:definition(), ?Form:clauses())).

get_clauses_as_container(FunNode) ->
    File = fundef_file(FunNode),
    Clauses = get_clauses(FunNode),
    {File, [ begin Pos=node_pos(C),
                   {C, start_pos(C), Pos, Pos} 
             end        || C <- Clauses ]}.

%% @doc Get a function node from module, name and arity
%% @spec get_function(string(), string(), integer()) -> node()
get_function(Module,Function,Arity) ->
    query_request(?Query:seq([?Mod:find(list_to_atom(Module)),
        ?Fun:find(list_to_atom(Function), Arity)])).

get_container(Node) ->
    case ui({get_container, Node}) of
        {bad_node, _, _, _} -> {error, "RefactorErl internal error: bad node."};
        {error, E}          -> {error, ?ERROR(E)};
        Result              -> Result
    end.

fundef_file(Node) ->
    FunModQ = ?Fun:module(),
    case query_request(Node, FunModQ) of
        [Mod|_] -> mod_file(Mod);
        _       -> no_file
    end.

mod_file(Mod) ->
    case query_request(Mod, ?Mod:file()) of
        [File|_] -> query_request(?File,path,File);
        _        -> no_file
    end.

start_pos(Node) ->
    ui({start_pos, Node}).

%% @doc Return the position of the given node
%% @spec node_pos(node()) -> term()
node_pos(Node) ->
    ui({node_pos, Node}).

%% @doc Return all investigations
%% @spec list_investigations() -> term()
list_investigations() ->
    get_all_invtab_name_user_pairs().

%% @doc List a user's investigations
%% éist_investigations(string()) -> [term()]
list_investigations(User) ->
    All = get_all_invtab_name_user_pairs(),
    [ I || I = { _ , Users } <- All, lists:member(User,Users) ].

%% @doc Start a new investigation from a function
%% @spec start_investigation(string(), string(), integer(), string()) -> inv()
start_investigation(Module, Function, Arity, User) ->
    case get_function(Module, Function, Arity) of
        [] -> ?STRERROR("There is no such function!");
        [FunNode] ->
            [GraphNode | _] = get_clauses(FunNode), 
            Text = node_text(GraphNode),
            {FilePath, _} = get_clauses_as_container(FunNode),
            {
                _, PositionOffset, { 
                                        {
                                            StartPosition, EndPosition
                                        }, 
                                        { 
                                            { LineNumber, _ }, _
                                        }
                                   }, _ 
            } = get_container(GraphNode),
            ItemLabel = node_to_text(GraphNode),
            FileData = { FilePath, PositionOffset-1, 
                        LineNumber, {StartPosition, EndPosition}, ItemLabel },
            DbHash = ?DBHASH,
            Pid = pid_to_list(self()),
            Users = [User],
            Name = "",
            Children = [],
            InvNode = #invnode{name = "", show = true, node = GraphNode, 
                                text = Text, label = root, memo = false, 
                                file_data = FileData, pos = {0,0}},
            Inv = #inv{ name = Name, hash = DbHash, users = Users, 
                        invdata = [ {Pid, "no_parent", InvNode, Children} ] },
            Inv
    end.

%% @doc Create a new investigation memo
%% @spec investigation_memo(string()) -> {string(), string()}
investigation_memo(ParentId) ->
    Pid = pid_to_list(self()),
    {Pid, ParentId}.

%% @doc Run a query with a list of investigation nodes as results
%% @spec investigation_query(string(), string(), string(), integer(), string()) 
%%  -> [term()] | {error, string()}
investigation_query(ParentId, QueryStr, File, Position, User) ->
    try
        Result = run_query_or_skeleton_node(User, QueryStr, {File, Position}),
        [{nodes, Nodes} | _] = Result,
        lists:map( fun(Node) -> convert_node(Node, ParentId, QueryStr) end ,Nodes)
    catch
        request_denied -> ?STRERROR("The database is busy. Request denied.");
        _:_ -> ?STRERROR("No proper result!")
    end.

%% @doc Convert a node to an investigation node
%% @spec convert_node(node(), string(), string()) -> term()
convert_node(Node, ParentId, QueryStr) ->
    Pid = spawn( fun() -> ok end ),
    Id = pid_to_list(Pid),
    Children = [],
    {
        GraphNode, PositionOffset, { 
                                {
                                    StartPosition, EndPosition
                                }, 
                                { 
                                    { LineNumber, _ }, _
                                }
                           }, _ 
    } = get_container(Node),
    Text = node_text(GraphNode),
    ItemLabel = node_to_text(GraphNode),
    FilePath = get_file_path(GraphNode),
    FileData = { FilePath, PositionOffset-1, LineNumber, 
                    {StartPosition, EndPosition}, ItemLabel },
    InvNode = #invnode{ name = "", show = true, node = GraphNode, 
                        text = Text , label = QueryStr, memo = false, 
                        file_data = FileData, pos = {0,0}},
    {Id, ParentId, InvNode, Children}.

%% @doc Save an investigation checking if there are any others with the same name
%% @spec save_investigation_safe(string(), string(), term(), term()) 
%%  -> ok | {error, string()}
save_investigation_safe(Name, User, Hash, InvData) ->
    case get_from_invtab(Name) of
        not_found -> save_investigation(Name, User, Hash, InvData);
        Inv  -> IsMember = lists:member(User,Inv#inv.users),
                     if IsMember -> save_investigation(Name, User, Hash, InvData);
                        true     -> ?STRERROR("This name is already in use!")
                     end
    end.

%% @doc Save an investigation
%% @spec save_investigation(string(), string(), term(), term()) -> ok
save_investigation(Name, User, Hash, InvData) ->
    NewData = lists:map(fun(X) -> strings_to_pids(X) end, InvData),
    Inv = #inv{ name = Name, hash = Hash, users = [User], invdata = NewData },
    insert_to_invtab(Inv).

%% @doc Load an investigation with the given name
%% @spec load_investigation(string()) -> inv()
load_investigation(Name) ->
    Inv = get_from_invtab(Name),
    Data = Inv#inv.invdata,
    NewData = lists:map(fun(X) -> pids_to_string(X) end, Data),
    Inv#inv{invdata = NewData}.

pids_to_string(InvElem) ->
    {Id, Parent, Node, Children} = InvElem,
    P = parent_to_string(Parent),
    {pid_to_list(Id), P, Node, lists:map(fun(X) -> pid_to_list(X) end, Children)}.

parent_to_string(Parent) ->
    if
        is_atom(Parent) ->
            atom_to_list(Parent);
        true ->
            pid_to_list(Parent)
    end.

strings_to_pids(InvElem) ->
    {Id, Parent, Node, Children} = InvElem,
    P = string_to_parent(Parent),
    {list_to_pid(Id), P, Node, lists:map(fun(X) -> list_to_pid(X) end, Children)}.

string_to_parent(ParentStr) ->
    if 
        ParentStr == "no_parent" -> list_to_atom(ParentStr);
        true -> list_to_pid(ParentStr)
    end.

%% @doc Share an investigation
%% @spec share_investigation(string(), string()) -> ok | {error, string()}
share_investigation(Name, User) ->
    case get_from_invtab(Name) of
        not_found -> ?STRERROR("This investigation doesn't exist!");
        InvNData  -> 
            IsMember = lists:member(User,InvNData#inv.users),
            if IsMember -> {error, "The user already owns this investigation."};
                true     -> Users = InvNData#inv.users,
                            InvNData2 = InvNData#inv{ users=[User|Users] },
                            insert_to_invtab(InvNData2)
                end
    end.

%% @doc Delete an investigation
%% @spec delete_investigation(string(), string()) -> ok | {error, string()}
delete_investigation(Name, User) ->
    case get_from_invtab(Name) of
        not_found -> {error, "This investigation doesn't exist!"};
        InvNData  -> 
            IsMember = lists:member(User,InvNData#inv.users),
            if IsMember ->
                    delete_from_invtab(Name);
               true     -> 
                    {error, "This investigation belongs to another user!"}
            end
    end.

%%% Tables

get_all_invtab_name_user_pairs() ->
    ui({get_all_invtab_name_user_pairs}).

insert_to_invtab(Rec) ->
    ui({insert_to_invtab, Rec}).

delete_from_invtab(Name) ->
    ui({delete_from_invtab, Name}).

get_from_invtab(Name) ->
    ui({get_from_invtab, Name}).

%%% ============================================================================
%%% Duplicated Code Analysis

get_dupcode_algorithms() ->
    Algorithms = ui({get_algorithms}),
    Keys = lists:map( fun( {_, Props} ) -> 
        proplists:get_value(key, Props) end , Algorithms ),
    Labels = lists:map( fun( {_, Props} ) -> 
        proplists:get_value(label, Props) end , Algorithms ),
    lists:zip(Keys, Labels).

get_dupcode_algorithm_data() ->
    Algorithms = get_dupcode_algorithms(),
    lists:map( fun({Key, Label}) -> 
        {Key, Label, ui({get_algorithm_data, Key}) }  end , Algorithms).

get_dupcode_algorithm_data(AlgorithmKey) ->
    AlgoritmsData = get_dupcode_algorithm_data(),
    lists:filter( fun({Key, _, _}) -> Key == AlgorithmKey end, AlgoritmsData).

run_dupcode_analysis(Options) ->
    ScalarOptions = Options ++ [{postype, scalar},{format, file_and_loc}],
    Result = ui({clone_identifierl, ScalarOptions}),
    transform_dupcode_result(Result).

transform_dupcode_result(Result) ->
    case Result of
        {error, Err} -> ?ERROR(Err);
        _ ->
            [_, _, {clone_name, Name}, {detected_clones, Clones}, {output_file_path, Path}] = Result,
            UnnumberedClones = lists:map(fun(C) -> {_Num, Content} = C, Content end, Clones),
            {Name, Path, UnnumberedClones}
    end.

get_dupcode_result_names() ->
    Results = ui({get_all_dupcode_result}),
    Names = lists:flatten( lists:map( fun(Res) -> [NameList | _ ] = Res, NameList end , Results) ),
    Names.

run_previous_dupcode_analysis(Name) ->
    Names = get_dupcode_result_names(),
    Results = ui({get_all_dupcode_result}),
    case lists:member(Name, Names) of
        false -> ?STRERROR("No previous result with the name " ++ atom_to_list(Name));
        true -> 
            [Result] = lists:filter(fun(Res) -> [NameList | _ ] = Res, lists:member(Name, NameList) end, Results),
            [_, Parameters, _] = Result,
            run_dupcode_analysis(Parameters)
    end.

run_selected_dupcode(Algorithm, FilePath, Start, End) ->
    Result = ui({clone_identifierl_by_pos, Algorithm, FilePath, Start, End, scalar, file_and_loc}),
    transform_dupcode_result(Result).

%%% ============================================================================

%%% UI requests

%% Functions to make requests to the reflib_ui_router (?UI)

ui(NameArgs) ->
    ReqId = ?UI:getid(),
    request(ReqId, NameArgs, fun() -> ui_loop(ReqId) end).

ui(NameArgs, GuiPid) ->
    ReqId = ?UI:getid(),
    request(ReqId, NameArgs, fun() -> ui_loop(ReqId, GuiPid) end).

ui_loop(ReqId) -> 
    receive
        {ReqId, reply, R} -> 
            case R of
                {ok, Result} -> Result;
                {error, Error} -> ?ERROR(Error)
            end
    end.

ui_loop(ReqId, GuiPid) -> 
    receive
        {ReqId, reply, R} -> 
            case R of
                {ok, Result} -> Result;
                {error, Error} -> ?ERROR(Error)
            end;
        {ReqId, query_id, _QueryId} -> 
            if 
                GuiPid =/= nopid ->
                    GuiPid ! {running_queries, running_queries()};
                true -> ok
            end,
            ui_loop(ReqId, GuiPid);
        {ReqId, progress, Progress} -> 
            GuiPid ! { progress,  Progress},
            ui_loop(ReqId, GuiPid)
    end.

request(ReqId, NameArgs, FunToRun) ->
    case ?UI:request(ReqId, NameArgs) of
        ok -> FunToRun();
        deny -> throw(request_denied)
    end.

query_request(Argument) ->
    case ui( {graph_query, ?Query, exec, [Argument]} ) of
        {error, Error} -> ?ERROR(Error);
        Result -> Result
    end.

query_request(Argument1, Argument2) ->
    case ui( {graph_query, ?Query, exec, [Argument1, Argument2]} ) of
        {error, Error} -> ?ERROR(Error);
        Result -> Result
    end.

query_request(Module, Function, Argument) when is_list(Argument) ->
    case ui( {graph_query, Module, Function, Argument} ) of
        {error, Error} -> ?ERROR(Error);
        Result -> Result
    end;

query_request(Module,Function,Argument) -> 
    query_request(Module,Function,[Argument]).
