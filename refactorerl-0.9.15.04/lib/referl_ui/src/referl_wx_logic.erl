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

%%% @doc This module throws the following exeptions: <br />
%%% - request_denied  ~ your query request was denied by the job server 
%%%                     (it is busy) <br />
%%% - {query_exception, Error} ~ some error occured during the execution of a 
%%%     more complex query/query sequence/operation (which contains several 
%%      queries), this may be a graph inconsistence <br />
%%% @author Gabor Hosszu

-module(referl_wx_logic).
-compile(export_all).
 
%% Exports
-export([init/1, terminate/0]).
-export([is_client/0, can_read_db/0]).
%% UI calls
-export([add_dir/2, add_appbase/2, add_include/2,
         del_env/3, get_envs/0, 
         get_loaded_files_list/0, is_file_alive/1, 
         get_loaded_directories/0]).
-export([drop_dir/2, get_file_status_info/1, error_info/1, reset_db/1, 
         get_error_forms_in_db/0]).
-export([precache_files/1, force_refresh_files/1]).
-export([synch_db/0, cat_file/1, get_database_hash/0]).
-export([run_query_or_skel/4, run_query_or_skel/5, kill_query/1, 
         run_query_or_skel_node/3]).
-export([search_initial_clones/1, get_const_var_diff/2, get_dupcode_algorithms/0,
         get_dupcode_algorithm_data/1, get_dupcode_result/1,
         get_dupcode_by_pos/4]).
-export([list_skeletons/0,save_skeleton/3, skeleton_call_format/1]).
-export([update_skeleton/4]).
-export([autocomplete/1, calc_ac_len_from_clist/2, get_funs/0, get_mods/0, 
         autocomplete_funmod/2]).
-export([definition/2, get_predef_queries/1]).
-export([get_running_queries/0, get_users_queries/1, delete_user_query/2,
         delete_skeleton/1]).
-export([node_file_pos/1, get_function/1, get_clauses/1, 
         get_clauses_as_container/1, get_file_path/1]).
-export([get_container/1]).
-export([dep_analysis/3,cached_dep_analysis/4, draw_java_graph/3, 
         draw_svg_graph/3, print_dep_graph/3]).
-export([node_text/1, node_to_text/1]).
-export([save_investigation/4, load_investigation/1, list_investigations/0,
         save_investigation_safe/4, share_investigation/2, 
         list_investigations/1, delete_investigation/2]).
-export([tree_new/0,tree_root/2,tree_add_node/5,tree_node_to_new_tree/3,
         tree_get_node/3, tree_delete_subtree/3, tree_delete_node/3,
         tree_get_subtree/3, tree_transformation/3, tree_traversal/2]).
-export([arrow_info/4, arrow_info/2, arrow_info/3, get_intersection_point/4,
         get_intersection_point/5, arrow_info/6, center/2, radius/2]).
-export([get_graph/1]).
-export([to_inner_graph/4]).
-export([trim_whitespace/1,write/1,pairs2/2,floor/1,ceil/1,pairs/2,
         list_to_num/1,num_to_list/1]).

%% UI router
-export([update_qtab_if_needed_ui/5, insert_to_qtab_ui/5, insert_to_qtab_ui/1, 
         find_in_qtab_ui/1, find_in_qtab_by_pattern_ui/1,
         delete_from_qtab_ui/1]).
-export([deptab_dets_insert_ui/1,
         deptab_match_opts_ui/1, deptab_delete_ui/1, deptab_delete_all_ui/0]).
-export([insert_to_invtab_ui/1, delete_from_invtab_ui/1, get_from_invtab_ui/1,
         get_all_invtab_name_user_pairs_ui/0]).

%% Includes
-include("ui.hrl").
-include("wx.hrl").
-include_lib("referl_core/include/core_export.hrl"). 

%%% ============================================================================
%%% DETS table definitions

-define(QTAB, 
    filename:join([?MISC:data_dir(),
                   "wx_query_table_"++ 
                   ?MISC:graph_based_postfix()])).
-define(RQTAB, 
    filename:join([?MISC:data_dir(),
                   "wx_running_queries_table"
		   ])).

-define(DEPTAB, 
    filename:join([?MISC:data_dir(),
                   "wx_dependency_graphs_table"++ 
                   ?MISC:graph_based_postfix()])).

-define(INVTAB, 
    filename:join([?MISC:data_dir(),
                   "wx_investigation_table"++ 
                   ?MISC:graph_based_postfix()])).

%%% ============================================================================
%%% Exported functions

%% @doc Initalization
%% @spec init(atom())-> boolean()
init(Pid) -> %% TODO ASK!!!
%% why?
%    Jobinfo=jinfo, %wf:session(jobinfo),
%    if
%        Jobinfo/=undefined ->
%            ?UI:del_msg_handler(Jobinfo);
%        true -> ok
%    end,
    DataDir = ?MISC:data_dir(),
    DummyNDataDir = filename:join(DataDir,"dummy_file"),
    case filelib:ensure_dir(DummyNDataDir) of
        {error, Error} -> ?wxError(Error);
        ok             -> reset_rqtab(),
                          ?UI:add_msg_handler(Pid),
                          {?UI:can_read_db(), ?UI:is_restricted_mode()}
    end.

%% @doc Termination
%% @spec terminate()-> ok
terminate() ->
    not is_client() andalso deptab_delete_all(),
	?UI:del_msg_handler(self()).

%% @doc Check if RefErl runs in client mode
%% @spec is_client() -> boolean()
is_client() ->
	?MISC:is_client().

%%% ============================================================================
%%% UI calls

%% Adding/dropping funs
%% When succesful, they return the path of the handled object

%% @doc Add a directory
%% @spec add_dir(string(), fun()) -> string()
add_dir(Path, Handler) ->
    case ui({add_dir, Path}, Handler) of
		{error, Error} -> ?wxError(Error);
		_			   -> Path
	end.

%% @doc Drop files
%% @spec drop_dir([string()], fun()) -> [string()]
drop_dir(Paths, Handler) ->
    case ui({drop_files, Paths}, Handler) of
        {error, {{reflib_error,file_not_present,_},_}} -> Paths;
		{error, Error} -> ?wxError(Error);
		_			   -> Paths
	end.

%% @doc Returns all of the include and appbase nodes
%% @spec get_envs() -> [{string(), term()}]
get_envs()->
    AppBases = [ {"appbase", I} || I <- ui({get_env, appbase}) ],
    Includes = [ {"include", I} || I <- ui({get_env, include}) ],
    AppBases ++ Includes.

%% @doc Deletes the given environmental node
%% @spec del_env(term(), term(), fun()) -> term() | wx_error()
del_env(Value, Type, _Handler)->
    case ui({del_env_val, Type, Value}) of
        [] -> ?wxErr("The given environmental node cannot be found");
        [ok] -> Value;
        {error, Error} -> ?wxError(Error)
    end.

%% @doc Adds the given appbase node
%% @spec add_appbase(string(), fun()) -> string() | wx_error()
add_appbase(Path=[C|_], _Handler) when is_integer(C) ->
    add_env(Path, appbase, _Handler).

%% @doc Adds the given include node
%% @spec add_include(string(), fun()) -> string() | wx_error()
add_include(Path=[C|_], _Handler) when is_integer(C) ->
    add_env(Path, include, _Handler).

add_env(Path=[C|_], Type, _Handler) when is_integer(C) -> 
    case ui({add_env, Type, Path}) of
        {error, Error} -> ?wxError(Error);
        _              -> Path
    end.

%% @doc Gets loaded files in a list
%% @spec get_loaded_files_list() -> [string()] | wx_error()
get_loaded_files_list()->
    _LoadedFilesList=
	try
        [ begin 
			case F of
				{error, Error} -> ?wxError(Error);
				_			   -> query_request(?File,path,[F])
			end
		  end || F <- ui({graph_query, ?Query, exec, [[file]]}) ]
    catch
        request_denied -> ?wxErr("Failed to get loaded files. "
                                   "The job server is busy.")
    end.

%% @doc Gets error forms
%% @spec get_error_forms_in_db() -> [term()]
get_error_forms_in_db()->
	ErrQuery = ?SEQ([file],?File:error_forms()),
    case query_request(ErrQuery) of
            [] -> 
                [];
            ErrForms -> 
                referl_ui_nitrogen_helper:get_errors(ErrForms)
    end.

%% @doc Get the loaded directories
%% @spec get_loaded_directories() -> [string()]
get_loaded_directories()->
    SortedDirs = ui({dir_sort}),
    case SortedDirs of
        {error, _}-> [];
        Dirs -> [ Dir || {Dir, _} <- Dirs, is_list(Dir), Dir =/= "Other"]
    end.

%% @doc Get file status for a filelist
%% @spec get_file_status_info([string()]) -> term()
get_file_status_info(FileList)->
    ui({status_info, FileList}).

%% @doc Get error info for a filelist
%% @spec error_info([string()]) -> term()
error_info(FileList)->
	StatusInfo = get_file_status_info(FileList),
	case StatusInfo of
		{error, Error} -> ?wxError(Error);
		_			   ->
							FilterFun = 
							fun({_Path, Props}) ->
								proplists:is_defined(error, Props)
							end,
							ErrorList = lists:filter(FilterFun, StatusInfo),
							Fun =
							fun({Path, Props}) ->
								{filename:basename(Path), 
								decode_error(proplists:get_value(error, Props)), 
								Path}
							end,
							lists:map(Fun, ErrorList)
	end.

%% @doc Resets database with given PosMode	
%% @spec reset_db(term()) -> ok | wx_error()
reset_db(PosMode) ->
    ui({reset, PosMode}).

%% @doc Synchronizes database
%% @spec synch_db() -> ok | wx_error()
synch_db() ->
    ui({synchronize}).

%% @doc Returns the textual representation of a modfile
%% Returns {ok, Code} or wxError (like file:read_file/1)
%% @spec cat_file(string()) -> {ok, string()} | wx_error()
cat_file(Filepath) -> 
    case ui({cat_file, Filepath}) of
        {error, Error} -> ?wxError(Error);
        Res            -> {ok, erlang:list_to_binary(lists:flatten(Res))}
    end.

is_file_alive(File) ->
    FileQuery =  query_request(?File,find,[File]),
    case query_request(FileQuery) of
        [_T|_] -> true;
        _      -> false
    end.

%% @doc Gets if database hash changed
%% @spec is_database_changed(term()) -> boolean()
is_database_changed(Hash1) ->
    Hash1 /= get_database_hash().   

%% @doc Gets database hash
%% @spec get_database_hash() -> term()
get_database_hash() ->
    case ui({database_hash}) of
        {error, Error} -> ?wxError(Error);
        Hash            -> Hash
    end.

%% @doc Returns if the DB if readable
%% @spec can_read_db() -> boolean()
can_read_db() -> ?UI:can_read_db().

%% @doc Precache a list of files (path).
%% @spec precache_files([string()]) -> ok
precache_files(Files) ->
    [ ui({generate_all, File}) || File <- Files ].

force_refresh_files(Files) ->
    [ ?FileCacheMan:force_generate(File) || File <- Files ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Investigations / querying / general

get_file_path(Node) ->
    try
        FirstToken = hd(ui({syn_leaves, Node})),
        FileNode = hd(query_request(FirstToken,?Token:file())),
        query_request(?File,path,FileNode)
    catch
        _:_-> "" 
    end.

%% @doc Get modules and funs (for e.g. for autocompletion)
%% @spec autocomplete_funmod(string(), term()) -> [string()]
autocomplete_funmod(Txt, Type) ->
    List0 =
    case Type of
        mod     -> get_mods();
        func    -> get_funs();
        modpath -> get_modpath();
        _   -> []
    end,
    [ L || L <- List0, string:str(L,Txt) > 0 ].

get_mods() ->
    try
        [atom_to_list(query_request(?Mod,name,M)) || 
            M <-query_request(?Mod:all())]
    catch
        request_denied -> []
    end.

get_funs() ->
    try
        [function_text(F) || F<- query_request(
            ?SEQ([?Mod:all(),?Mod:locals_all()]))]
    catch
        request_denied -> []
    end.

get_modpath() ->
    try
        lists:usort([ filename:basename(?File:path(Fnode))
             || Fnode <- query_request(?SEQ([?File:all()]))])
    catch
        request_denied -> []
    end.

%% @doc Converts a function node to the "canonical" M:F/A format
%% @spec function_text(node()) -> string()
function_text(FunNode) ->
    ui({function_text, FunNode}).

%% @doc Get function node for a string M:F/A
%% @spec get_function(string(), string(), string()) -> node()
get_function(Module,Function,Arity) ->
    query_request(?SEQ([?Mod:find(list_to_atom(Module)),
        ?Fun:find(list_to_atom(Function),list_to_integer(Arity))])).

get_function(Text) ->
    MFA = re:split(Text,"([:/])",[{return,list},group]),
    case MFA of
        [[M,":"], [F,"/"], [A]] when 
                length(M)>=1, length(F)>=1, length(A)>=1 ->
            FunNode=try
                    get_function(M,F,A)
                catch
                    _:_ -> []
                end,
            if  FunNode==[] -> ?wxErr("Function doesn't exists.");
                true        -> FunNode
            end;
        _ -> ?wxErr("Bad function format!")
    end.

%% @doc Get clauses of a function node
%% @spec get_clauses(node()) -> [node()]
get_clauses(FunNode) ->
    query_request(FunNode, ?SEQ(?Fun:definition(), ?Form:clauses())).

%% @doc Get text for a node (from code)
%% @spec node_text(node()) -> string()
node_text(Node) ->
    case ui({syn_tree_text, Node}) of
        {error, _} -> "";
        Txt        -> lists:flatten(Txt)
            % if hd(Txt2) == 10 -> tl(Txt2);
            %    true             -> Txt2
            % end
    end.

node_to_text(Node) ->
    try 
        ui({graph_data, Node}),
        case Node of
            {_,clause,_} -> 
                FuncNode=query_request(Node,?SEQ([?Clause:form(),?Form:func()])),
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
     %%   request_denied -> "Database was in use.";
        _:_ -> "???" 
    end.

%% @doc Get containing clause / form for a node the position
%% of the container and the position of the node
%% {ClorFormNode, ClorFormStartScalar, ClorFormPos, NodePos}
%% @spec get_container(node()) -> [container()] | wx_error()
get_container(Node) ->
    case ui({get_container, Node}) of
        {bad_node, _, _, _} -> ?wxErr("RefactorErl internal error: bad node.");
        {error, E}          -> ?wxError(E);
        Result              -> Result
    end.

%% @doc Get clauses and position information for them
%% This function returns list of tuples 
%% {Clause, StartPos, ClausePos, ClausePos} like get_container/1
%% @spec get_clauses_as_container(node()) -> [container()] | wx_error()
get_clauses_as_container(FunNode) ->
    File = fundef_file(FunNode),
    Clauses = get_clauses(FunNode),
    {File, [ begin Pos=node_pos(C),
                   {C, start_pos(C), Pos, Pos} 
             end        || C <- Clauses ]}.

%% @doc Save an investigation and give an error message if it already exists
%% or the name is already used
%% @spec save_investigation_safe(string(), string(), hash(), investigation()) -> 
%%  {save_error, overwrite} | {save_error, name_already_in_use} | ok
save_investigation_safe(Name, User, Hash, Inv) ->
    case get_from_invtab(Name) of
        not_found -> save_investigation(Name, User, Hash, Inv);
        InvNData  -> IsMember = lists:member(User,InvNData#inv.users),
                     if IsMember -> {save_error, overwrite};
                        true     -> {save_error, name_already_in_use}
                     end
    end.

%% @doc Save an investigation
%% @spec save_investigation(string(), string(), hash(), investigation()) -> ok
save_investigation(Name, User, Hash, Inv) ->
    InvNData = #inv{ name=Name, hash=Hash, users=[User], invdata=Inv },
    insert_to_invtab(InvNData).

%% @doc Load an investigation
%% @spec load_investigation(string()) -> investigation()
load_investigation(Name) ->
    get_from_invtab(Name).

%% @doc List all investigations (as name and user pairs)
%% @spec list_investigations() -> [{string(), string()}]
list_investigations() ->
    get_all_invtab_name_user_pairs().

list_investigations(User) ->
    All = get_all_invtab_name_user_pairs(),
    [ I || I={_,Users} <- All, lists:member(User,Users) ].

%% @doc Share an investigation
%% @spec share_investigation(string(), string()) -> ok | wx_error()
share_investigation(Name, User) ->
    case get_from_invtab(Name) of
        not_found -> ?wxErr("This investigation doesn't exist!");
        InvNData  -> 
            IsMember = lists:member(User,InvNData#inv.users),
            if IsMember -> ?wxErr("The user already owns this investigation.");
                true     -> Users = InvNData#inv.users,
                            InvNData2 = InvNData#inv{ users=[User|Users] },
                            insert_to_invtab(InvNData2)
                end
    end.

%% @doc Delete an investigation
%% @spec delete_investigation(string(), string()) -> ok | wx_error()
delete_investigation(Name, User) ->
    case get_from_invtab(Name) of
        not_found -> ?wxErr("This investigation doesn't exist!");
        InvNData  -> 
            IsMember = lists:member(User,InvNData#inv.users),
            if IsMember ->
                    delete_from_invtab(Name);
               %    Users = InvNData#inv.users,
               %    Users2 = lists:delete(User,Users),
               %    case Users2 of
               %         [] -> delete_from_invtab(Name);
               %         _  -> InvNData2 = InvNData#inv{ users=Users2 },
               %               insert_to_invtab(InvNData2)
               %    end;
               true     -> 
                    ?wxErr("This investigation belongs to another user!")
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Querying

%% @doc Run a query or a skeleton with node output
%% @spec run_query_or_skel_node(string(), string(), {string(), integer()}) -> 
%%       ok | wx_error()
run_query_or_skel_node(_, "", _) -> [];
run_query_or_skel_node(CurrUser, Qstr, {File, Pos}) ->
    DisplayOpt = [{positions,scalar}, {output,nodes}],
    RefreshFun = ?IdFun,
    StartOpt   = [{file, File},{position, Pos}],
    run_query_helper(Qstr, true, DisplayOpt, StartOpt, RefreshFun, CurrUser).

%% @doc Run a query or a skeleton
%% @spec run_query_or_skel(string(), string(), 
%%                         {string(), integer()}, atom() ) -> 
%%       ok | wx_error()
run_query_or_skel(_, "", _, _) -> no_query;
run_query_or_skel(CurrUser, Qstr, {File, Pos}, GroupBy0) ->
    GroupBy = case GroupBy0 of
        none -> [];
        _    -> [{groupby, GroupBy0}]
    end,
    DisplayOpt = [{positions,scalar}, {output,other}] ++ GroupBy,
    run_query_or_skel(CurrUser, Qstr, true, DisplayOpt, {File, Pos}).
run_query_or_skel(_, "", _, _, {_, _}) -> no_query;
run_query_or_skel(User, Query, SBQueryId, DisplayOpt, {File, Pos}) ->
    case determine_sq_request_type(Query) of
        sem_query ->
            run_query(User, Query, SBQueryId, DisplayOpt, {File, Pos});
        skeleton ->
            try_parse_n_run_skeleton({Query,File,Pos}, 
                                     User, SBQueryId, DisplayOpt);
        {error, Err} -> 
            {error, Err}
    end.      

try_parse_n_run_skeleton(T={_Qstr, File, Pos}, User, SBQueryId, DPOpt)  ->
    case ui({try_parse_skeleton, T, User}) of
        {error, Err} -> {error, Err};
        {_, QStr, _} -> run_query(User, QStr, SBQueryId, DPOpt, {File, Pos})
    end. 

determine_sq_request_type(Call=[C|_]) when is_integer(C)->
    case ui({determine_sq_request_type, Call}) of
        {error, Err} -> {error, Err};
        R            -> R
    end.

run_query(CurrUser, Qstr, SendBackQueryId, DisplayOpt, {File, Pos}) ->
    StartOpt = case {File, Pos} of
        {no_file, no_pos} -> [];
        _                 -> [{file, File},{position, Pos}]
    end,
    GroupBy = proplists:get_value(groupby, DisplayOpt, none),
    SafeQuery=make_safe(Qstr),
    QueryKey = {SafeQuery, File, Pos, GroupBy},
    Hash = ?DBHASH,
    RefreshCache = 
        fun(Res)-> update_qtab_if_needed(QueryKey,Qstr,CurrUser,Res,Hash)  end,
    LLL = find_in_qtab_by_pattern({'_',QueryKey,'_','_','_','_'}),
    case LLL of
        []   -> run_query_helper(Qstr, SendBackQueryId, DisplayOpt, 
                                 StartOpt, RefreshCache, CurrUser);
        [Rs] -> case is_previous_query(QueryKey, CurrUser, Rs) of
                  false ->
                    run_query_helper(Qstr, SendBackQueryId, DisplayOpt, 
                                    StartOpt, RefreshCache, CurrUser);
                  true -> 
                    update_qtab_if_needed(QueryKey,Qstr,CurrUser,
                                                        Rs#q_info.result,Hash),
                    Rs#q_info.result
               end                     
    end.

run_query_helper(Qstr, SendBackQueryId, DisplayOpt, StartOpt, RefreshFun,
                                                        CurrUser) ->
    case ui({transform,semantic_query,
           [{ask_missing,false},
            {send_back_query_id, SendBackQueryId},
            {querystr,Qstr},
            {display_opt,DisplayOpt},
            {start_opt,StartOpt}]}, ?IdFun, CurrUser, Qstr)   of
       {error, Error}       -> ?wxError(Error);
       {result, R}          -> [R0] = ?MISC:pgetu([result],R),
                               RefreshFun(R0),
                               R0;
       {abort, {_, Error}}  -> ?wxError(Error) 
    end.

is_previous_query({_SafeQuery, _File, _Pos, _GrpBy}, _User, Res) ->
    not is_database_changed(Res#q_info.hash).

%% @doc Kill query.
kill_query(QueryId) when is_integer(QueryId)->
    case ui({kill_query,QueryId}) of
        not_found       -> ?wxErr("Query not found");
        ok              ->
            delete_from_rqtab({'_','_','_',QueryId,'_'}),
            ok;
        _               -> ?wxErr("Some error occured when the"
                                  " program tried to kill the query")
    end.

get_users_queries(User) ->  %% Depends on the q_info record
    L   = find_in_qtab_by_pattern({'_','_','_','_','_','_'}),
    _L2 = case User of
            all -> L;
            _ -> lists:filter(
                    fun(Rec) ->
                        lists:member(User,Rec#q_info.users)
                    end, L)
         end.

delete_user_query(QueryKey, User) ->
    [Rec] = find_in_qtab(QueryKey),
    UserList = Rec#q_info.users,
    UserList2 = lists:delete(User,UserList),
    case UserList2 of
        UserList -> ?wxErr("This query belongs to another user!");
        [] -> delete_from_qtab(QueryKey);
        _ ->
            insert_to_qtab(Rec#q_info{ users = UserList2 })
    end.

%%%%%%%%%%%%%%%%%%%%%%%
%% Skeleton interface

%% @doc List skeletons
%% @spec list_skeletons() -> [string()] | wx_error()
list_skeletons() ->
    case ui({list_skeletons}) of
        {error, E} -> ?wxError(E);
             R     -> R
    end. 

%% @doc Save a skeleton
%% @spec save_skeleton(string(), string(), string()) -> 
%%       [string()] | wx_error()
save_skeleton(Name, Body, User) ->
    ui({save_skeleton, Name, Body, User}).

%% @doc Ask for skeleton call format
%% @spec skeleton_call_format(string()) -> string() | wx_error()
skeleton_call_format(Name) ->
    ui({skeleton_call_format, Name}).

%% @doc Delete a skeleton
%% @spec delete_skeleton(string()) -> 
%%       ok | wx_error()
delete_skeleton(Name) ->
    ui({delete_skeleton, Name}).

%% @doc Update a skeleton
%% @spec update_skeleton(string(), string(), string(), string()) -> 
%%       ok | wx_error()
update_skeleton(Name, NewBody, Owner, NewCmt) ->
    case ui({update_skeleton, Name, NewBody, Owner}) of
        {error, Err} -> ?wxError(Err);
        _            -> ui({update_prev_skeleton_comment, Name, NewCmt})
    end.

do_autocomplete_skeleton(Call) ->
    case ui({do_autocomplete_skeleton, Call}) of
        {error, _ } -> [];
        Res         -> Res
    end.

%% @doc Autocompletion of queries and skeletons
%% @spec autocomplete(string()) -> string()
autocomplete(Call) ->
    List1=
    try
        List=refusr_ac:run(Call),
        [ {lists:flatten(InCompleteEnt++Completion), 
           lists:flatten(Call++Completion)}   ||
          {InCompleteEnt,Completion=[C|_]} <-  List, 
          is_list(InCompleteEnt), is_integer(C)       ]
    catch 
        _:_ -> []
    end,
    List2=
    try
            do_autocomplete_skeleton(Call)
    catch 
        _:_ -> []
    end,
    lists:keysort(2, List1++List2).

calc_ac_len_from_clist(_, _CompleteList=[]) ->
    0;
calc_ac_len_from_clist(B, _CompleteList=[RefCompl|_]) ->
    calc_ac_len(B, RefCompl).

calc_ac_len(B, S) ->
    RevB = lists:reverse(B),
    RevS = lists:reverse(S),
    AllTail = [ lists:nthtail(N, RevS) || N <- lists:seq(1,length(S)) ],
    check_len(RevB, AllTail, 0).

check_len(_, [], L) -> L;
check_len(RevB, [E|Tail], L) ->
    IsPrefix = lists:prefix(E, RevB),
    if IsPrefix ->
        check_len(RevB, Tail, max(L, length(E)));
    true ->
        check_len(RevB, Tail, L)
    end.

%%%%%%%%%%%%%%%%%%%%%%%
%% Query bubbles / queries from a position

%% @doc Get node type
%% @spec definition(string(), integer()) -> node() | wx_error()
definition(FilePath, Pos) ->
    case ui({definition, FilePath, Pos}) of
        {error, Error} -> ?wxError(Error);
        Result         -> Result
    end.

%% @doc The type of "frequently used queries" we have to show
%% @spec node_type(node()) -> atom()
node_type(Node) ->
    ui({node_type, Node}).

%% @doc Get predefined queries
%% @spec get_predef_queries(atom()) -> [{string(), string()}]
get_predef_queries(variable) ->
    [{"Var References","@var.references"},
     {"Var Binding","@var.bindings"},
     {"Var Origin","@expr.origin"},
     {"Var Reach","@expr.reach"}];    
get_predef_queries(funappl) ->        
    [ {"Function References","@fun.references"},
      {"Function Definition","@def"}];
get_predef_queries(fundef) ->
    [{"Function References","@fun.references"}];
get_predef_queries(mod) ->
    [{"Functions","@file.funs"},
     {"Exported Functions","@file.exports"},
     {"Module References","@file.references"},
     {"Records","@file.records"}];
get_predef_queries(macrodef) ->
    [{"Macro References","@macro.references"},
     {"Macro Definition","@def"},
     {"Expand all references", "@macro.all_refs.macro_value"}];
get_predef_queries(recdef) ->
    [{"Record References","@record.references"},
     {"All field References","@record.fields.references"}];
get_predef_queries(recfielddef) ->
    [{"Field References","@field.references"},
     {"All Field References","@field.record.references"}];
get_predef_queries(recexpr) ->    
    [{"Record References","@record.references"},
     {"Record Definition","@expr.records"},
     {"All Field References","@record.fields.references"}];
get_predef_queries(recfield) ->
    [{"Field References","@field.references"},
     {"All Field References","@field.record.references"}];
get_predef_queries(_) ->
    [].


%% @doc Get file and position for  a Fun/Mod node
%% @spec node_file_pos(node()) -> {string(), integer()} | wx_error()
node_file_pos(Node) ->
    GeneralType = case node_type(Node) of
        module -> module;
        fundef -> func;
        funappl -> func
    end,
    case GeneralType of
        module  -> Pos = no_pos,
                File = mod_file(Node),
                {File, Pos};
        func    -> Pos = fundef_pos(Node),
                File = fundef_file(Node),
                {File, Pos};
        K    -> ?wxErr("Unknown node")
    end.

fundef_pos(Node) ->
    ui({fundef_pos, Node}).

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

node_pos(Node) ->
    ui({node_pos, Node}).

start_pos(Node) ->
    ui({start_pos, Node}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Dependency analysis

%% Run a dependency analysis. Opt contains the PROPER options for the actual
%% analysis level and type (it can also be empty)
%% CLMode is the complex label mode (on or off) which is a labelling using
%% tuples (this can provide extra node information in dot files)

%% @doc Run a dependency analysis
%% No complex labels
%% @spec regular_dep_analysis(term(), term(), proplist()) -> term()
regular_dep_analysis(Level, Type, Opts) -> 
    dep_analysis(Level, Type, undefined, Opts, false).

%% @doc Run a dependency analysis
%% Complex labels
%% @spec dep_analysis(term(), term(), proplist()) -> term()
dep_analysis(Level, Type, Opts) -> 
    dep_analysis(Level, Type, undefined, Opts, true).
dep_analysis(Level, Type, undefined, Opts, CLMode) ->
    FPath = 
    filename:join([?MISC:data_dir(), 
                    ?wxInvFileName++?MISC:graph_based_postfix()]),
    dep_analysis(Level, Type, FPath, Opts, CLMode);

dep_analysis(Level, Type, FilePath, Opts, ClMode) ->
    CL = if ClMode -> [{output, complex_dot},{dotpars,[{splines,"ortho"},{ratio,"0.05"},{nodesep,"4"}]}];
            true   -> [{output, simple_dot}]
    end,
    Options = CL ++ [{file_path, FilePath}, {level,Level}, {type,Type}] ++ Opts,
    case ui({draw_dep_graph, Options}) of
        {error, Error}  -> ?wxError(Error);
        _               -> FilePath
    end.

%% @doc Cached dependency analysis
%% Uses the inner graph representation defined in referl_wx_depgraph
%% Runs GraphCalcFun to calculate a graph if no graph present in the DB
%% @spec cached_dep_analysis(term(), term(), proplist(), fun()) -> term()
cached_dep_analysis(Level,Type,Opts,GraphCalcFun) ->
    find_graph_in_deptab(Level, Type, Opts, GraphCalcFun).

%% @doc Draw a JS graph
%% @spec draw_java_graph(term(), term(), proplist()) ->
%%       string() | wx_error()
draw_java_graph(Level,Type,Opts) ->
    O = [{level, Level}, {type, Type}] ++ Opts,
    SGOpts = [{output_type, js_with_html}, {dependency_options, O}, 
              {dependency_level, Level}],
    case ui({generate_smart_graph, SGOpts}) of
        {result, FilePath} -> get_file_if_needed(FilePath);
        _                  -> ?wxErr("Error during analisys")
    end.

%% @doc Draw an SVG graph
%% @spec draw_svg_graph(term(), term(), proplist()) ->
%%       string() | wx_error()
draw_svg_graph(Level,Type,Opts) ->
    try
        FilePath = regular_dep_analysis(Level, Type, Opts),
        MyFile = case get_file_if_needed(FilePath) of
            {error, Error} -> throw({gen_error, Error});
            MF -> MF
        end,
        SVGPath = MyFile++".svg",
        Cmd = "dot -Tsvg -o"++SVGPath++" "++MyFile,
        [] = os:cmd(Cmd),
        SVGPath
    catch 
        {gen_error, Err} -> ?wxError(Err);
        _:_ -> ?wxErr("Error during analisys")
    end.

get_file_if_needed(FilePath) ->
    IsClient = is_client(),
    if IsClient ->
            case ui({get_file_from_server, FilePath}) of
                {error, Error} ->
                    ?wxError(Error);
                Binary ->
                    Name = filename:basename(FilePath),
                    LocalPath = ?MISC:data_dir() ++ "/"  ++ Name,
                    ok = file:write_file(LocalPath, Binary),
                    LocalPath
            end;
        true -> FilePath
    end.

%% @doc Print a dependency graph (as a term)
%% @spec print_dep_graph(term(), term(), proplist()) ->
%%       term() | wx_error()
print_dep_graph(Level, Type, Options) ->
    io:format("~p~n~n",[Options]),    
    Opts = [{level,Level},{type,Type},{output,name_terms}] ++ Options,
    case ?NITRO_CORE:make_ui_request({draw_dep_graph,Opts}) of
        {error, Error} -> ?wxError(Error);
        {ok, R}        -> R;
        R              -> R
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Duplicated code analysis

%% @doc Makes request for searching for initial clones 
%% @spec search_initial_clones(proplist()) ->
%%       [term()] | wx_error()
search_initial_clones(Options)->
    case ui({clone_identifierl, Options}) of
        {error, {{refusr_clone_identifierl,empty_db,_},_}} 
                                    -> ?wxErr("Database is empty.");
        {error, {{_,_,_},E}}        -> ?wxErr("Duplicated code error: " ++ write(E));
        Results                     -> Res = proplists:get_value(detected_clones, Results),
                                       Name = proplists:get_value(clone_name, Results),
                                       {write(Name), Res}
    end.
%% [{files,FileList}, {minlen,P1}, {minnum,P2}, {overlap,P3}, {posopt, scalar}]

%% @doc Makes request for calculating the difference between two given nodes.
%% @spec get_const_var_diff(node(), node()) ->
%%       {[{integer(), integer()}], [{integer(), integer()}]}
get_const_var_diff(Node1, Node2)->
    Diffs =
    case ui({duplicated_code_const_var_diff, Node1, Node2}) of
        {error, _ } -> [];
        Res         -> Res
    end,
    {CDiffs,VDiffs} = case Diffs of
                 [] -> {[],[]};
              {A,B} -> {A,B}
    end,
    NodeDiffs1 = [N1 || {N1,_}<-CDiffs] ++ [N1 || {N1,_}<-VDiffs],
    NodeDiffs2 = [N2 || {_,N2}<-CDiffs] ++ [N2 || {_,N2}<-VDiffs],
    Fun = fun(Node) -> node_pos(Node) end,
    Intervals1 = lists:map(Fun,NodeDiffs1),
    Intervals2 = lists:map(Fun,NodeDiffs2),
    {Intervals1, Intervals2}.

get_dupcode_algorithms() ->
    case ui({get_algorithms}) of
        {error, _ } -> [];
        Res         -> Res
    end.

get_dupcode_algorithm_data(Algorithm) ->
    case ui({get_algorithm_data, Algorithm}) of
        {error, _ } -> [];
        Res         -> Res
    end.

get_dupcode_result(Name) ->
    case ui({get_dupcode_result, Name, ui_format, scalar}) of
        {error, _ } -> {Name, undefined};
        Res         -> {Name, proplists:get_value(detected_clones, Res)}
    end.

get_dupcode_by_pos(ModulePath, Algorithm, Start, End) ->
    case ui({clone_identifierl_by_pos, Algorithm, ModulePath, Start, End, scalar, ui_format}) of
        {error, E } -> ?wxError(E);
        Res         -> Name = proplists:get_value(clone_name, Res),
                       {Name, proplists:get_value(detected_clones, Res)}
    end.       

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Get running queries (as QueryID and QueryString pairs)
%% @spec get_running_queries() -> [{term(), string()}]
get_running_queries() ->
    RQTab = get_rqtab_elements(),
    All = get_queries(),
    merge_queries(RQTab, All).

get_queries() ->
    try 
        Running = ui({get_running_queries, default_output}),
        Result = [ begin 
                       { QueryId, _, _, Opt} = AQuery,
                       QueryStr = proplists:get_value(querystr, Opt),
                       {QueryId, QueryStr} end || AQuery <- Running ],
        Result
    catch 
        _:_ -> []
    end.

merge_queries(RQTab, All) ->
    merge_queries_(RQTab, All, []).

merge_queries_(RQTab, [{QId, QStr} | Tail], Acc) ->
    QRec = get_rq_element(QId, RQTab),
    IsElement = QRec =/= none,
    if IsElement -> merge_queries_(RQTab, Tail, [  QRec | Acc]);
       true -> merge_queries_(RQTab, Tail, [ #rq_info{ user="", qid=QId, 
                                               qstr=QStr, reqid=none  } |Acc])
    end;

merge_queries_(_RQTab, [], Acc) -> Acc.

get_rq_element(QId, [QRec=#rq_info{ qid=QId } | _Tail]) -> QRec;
get_rq_element(QId, [ _ | Tail ]) -> get_rq_element(QId, Tail);
get_rq_element(_QId, []) -> none.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Table operations

%%%% General  

gen_insert(Name, Rec) ->
    dets:open_file(Name,[{keypos, 2}]),
    ok = dets:insert(Name,Rec),
    dets:close(Name).

%%% Top level
update_qtab_if_needed(QueryKey,Query,User,Result,Hash)->
    ui({update_qtab_if_needed, QueryKey,Query,User,Result,Hash}).

insert_to_qtab(Rec) ->
    ui({insert_to_qtab, [Rec]}).

insert_to_qtab(QueryKey={_,_,_,_}, Query, Res, Users, Hash) ->
    ui({insert_to_qtab, QueryKey, Query, Res, Users, Hash}).

find_in_qtab(QueryKey) ->
    ui({find_in_qtab, QueryKey}).

find_in_qtab_by_pattern(Pattern)->
    ui({find_in_qtab_by_pattern, Pattern}).

delete_from_qtab(QueryKey) ->
    ui({delete_from_qtab, QueryKey}).
  
update_qtab_if_needed_ui(QueryKey,Query,User,Result,Hash)->
    try
        Users = case find_in_qtab_by_pattern_ui({'_',QueryKey,'_','_','_','_'}) of
                    [] ->
                        [User];
                    [Rec] ->
                        lists:usort([User|Rec#q_info.users])
        end,
        insert_to_qtab_ui(QueryKey,Query,Result,Users,Hash)
    catch
        _:_ -> ok
    end.

%%%% Query TABle
insert_to_qtab_ui(Rec) -> gen_insert(?QTAB, Rec).
insert_to_qtab_ui(QueryKey={_,_,_,_}, Query, Res, Users, Hash) ->
    Rec=#q_info{key=QueryKey,qstr=Query,users=Users,result=Res,hash=Hash},
    gen_insert(?QTAB, Rec).

find_in_qtab_ui(QueryKey) -> 
    try
        dets:open_file(?QTAB,[{keypos, 2}]),
        Result=dets:lookup(?QTAB,QueryKey),
        dets:close(?QTAB),
        Result
    catch
        _:_ -> []
    end.

find_in_qtab_by_pattern_ui(Pattern)->
    try
        dets:open_file(?QTAB,[{keypos, 2}]),
        Result=dets:match_object(?QTAB,Pattern),
        dets:close(?QTAB),
        Result
    catch
        _:_ -> []
    end.

delete_from_qtab_ui(QueryKey) ->
    dets:open_file(?QTAB,[{keypos, 2}]),
    dets:delete(?QTAB,QueryKey),
    dets:close(?QTAB).

%%% Low- level operations on 'wx_running_queries_table' dets table
%%% #record_name{record_field=something,_='_'}

insert_to_rqtab(Rec) -> gen_insert(?RQTAB, Rec).
insert_to_rqtab(ReqID, User, QueryId, QStr) ->
    Rec = #rq_info{user=User,qid=QueryId,qstr=QStr,reqid=ReqID},
    gen_insert(?RQTAB, Rec).

find_in_rqtab_by_reqid(ReqID) ->
    dets:open_file(?RQTAB,[{keypos, 2}]),
    Result=dets:match_object(?RQTAB,{'_', ReqID,'_','_','_'}),
    dets:close(?RQTAB),
    Result.

get_rqtab_elements()->
    dets:open_file(?RQTAB,[{keypos, 2}]),
    Result=dets:match_object(?RQTAB,{'_','_','_','_','_'}),
    dets:close(?RQTAB),
    Result.
    
delete_from_rqtab(RowPattern) ->
    dets:open_file(?RQTAB,[{keypos, 2}]),
    dets:match_delete(?RQTAB,RowPattern),
    dets:close(?RQTAB).

reset_rqtab() ->
    dets:open_file(?RQTAB,[{keypos, 2}]),
    dets:delete_all_objects(?RQTAB),
    dets:close(?RQTAB). 

%% DEPTAB

deptab_dets_insert(Rec) ->
    ui({deptab_dets_insert, Rec}).

deptab_match_opts(Opts) ->
    ui({deptab_match_opts, Opts}).

deptab_delete(Opts) ->
    ui({deptab_delete, Opts}).

deptab_delete_all() ->
    ui({deptab_delete_all}).


deptab_dets_insert_ui(Rec) ->
    dets:open_file(?DEPTAB,[{keypos, 2}]),
    dets:insert(?DEPTAB,Rec),
    dets:close(?DEPTAB).

deptab_match_opts_ui(Opts) ->
    dets:open_file(?DEPTAB,[{keypos, 2}]),
    Res=dets:match_object(?DEPTAB,{'_',Opts,'_','_'}),
    dets:close(?DEPTAB),
    Res.

deptab_delete_ui(Opts) ->
    dets:open_file(?DEPTAB,[{keypos, 2}]),
    dets:delete(?DEPTAB,Opts),
    dets:close(?DEPTAB).

find_graph_in_deptab(Level,Type,Opts0,FunToRun) ->
    Opts = [Level,Type] ++ Opts0,
    Res = 
    case deptab_match_opts(Opts) of
        [] -> ActHash = ?DBHASH,
              deptab_insertion(Level,Type,Opts,FunToRun,ActHash);
        [{_,_,DbHash,Graph1}] -> 
              ActHash = ?DBHASH,
              case DbHash of
                ActHash -> Graph1;
                _       -> deptab_delete(Opts),
                           deptab_insertion(Level,Type,Opts,FunToRun,ActHash)
              end
    end,
    Res.  

deptab_insertion(_Level,_Type,Opts,FunToRun,Hash) ->
    case FunToRun() of
        no_graph -> 
            no_graph;
        Graph    ->
            Rec = #deprec{ opts= Opts, hash=Hash, graph=Graph },
            deptab_dets_insert(Rec),
            Graph
    end.

deptab_delete_all_ui() ->
    dets:open_file(?DEPTAB,[{keypos, 2}]),
    dets:delete_all_objects(?DEPTAB),
    dets:close(?DEPTAB).

%% INVTAB

insert_to_invtab(Rec) ->
    ui({insert_to_invtab, Rec}).

delete_from_invtab(Name) ->
    ui({delete_from_invtab, Name}).

get_from_invtab(Name) ->
    ui({get_from_invtab, Name}).

get_all_invtab_name_user_pairs() ->
    ui({get_all_invtab_name_user_pairs}).

insert_to_invtab_ui(Rec) -> gen_insert(?INVTAB, Rec).

delete_from_invtab_ui(Name) ->
    dets:open_file(?INVTAB,[{keypos, 2}]),
    dets:match_delete(?INVTAB,{'_', Name, '_', '_', '_'}),
    dets:close(?INVTAB).

get_from_invtab_ui(Name) ->
    dets:open_file(?INVTAB,[{keypos, 2}]),
    Result = case dets:match_object(?INVTAB,{'_',Name,'_','_','_'}) of
                    [H|_] -> H;
                    _     -> not_found
             end,
    dets:close(?INVTAB),
    Result.

get_all_invtab_name_user_pairs_ui() ->
    try
        dets:open_file(?INVTAB,[{keypos, 2}]),
        Result = dets:match_object(?INVTAB,{'_','_','_','_','_'}),
        dets:close(?INVTAB),
        lists:map(fun(Item) -> {Item#inv.name, Item#inv.users}  end,
                  Result)
    catch
        _:_ -> []
    end.

%%% ============================================================================
%%% Functions to use reflib_ui_router (UI)

query_request(Arg1) ->
    case ui({graph_query, ?Query, exec, [Arg1]}) of
        {error, E} -> ?wxError(E);
                 R -> R
    end.

query_request(Arg1,Arg2) ->
    case ui({graph_query, ?Query,exec, [Arg1,Arg2]}) of
        {error, E} -> ?wxError(E);
                 R -> R
    end.

query_request(Mod,Fun,Arg) when is_list(Arg) ->
    case ui({graph_query, Mod, Fun, Arg}) of
        {error, E} -> ?wxError(E);
                 R -> R
    end;
query_request(Mod,Fun,Arg) -> query_request(Mod,Fun,[Arg]).

ui(NameArgs)-> ui(NameArgs,?IdFun).
ui(NameArgs,Handler)-> ui(NameArgs,Handler,nobody,dummy).
ui(NameArgs,Handler,User,QStr)->
    ReqID = ?UI:getid(),
    (User =/= nobody) andalso
        insert_to_rqtab(ReqID, User, no_id, QStr),
    Fun = fun() -> ui_loop(ReqID,Handler,User =/= nobody) end,
    request(ReqID,NameArgs,Fun).

ui_loop(ReqID, Handler, NeedToAdministrate)->
    receive
        {ReqID, reply, R} -> 
            NeedToAdministrate andalso 
                delete_from_rqtab({'_', ReqID, '_','_','_'}),
             case R of
                {ok, Res} -> Res;
                {error, Error} -> {error, Error}
             end;
        {ReqID, query_id, QueryId}->
            NeedToAdministrate andalso
                begin
                    case find_in_rqtab_by_reqid(ReqID) of
                        [Rec]->
                            insert_to_rqtab(Rec#rq_info{ qid=QueryId });
                        _ -> ok
                    end
                end,
            ui_loop(ReqID, Handler, NeedToAdministrate);  
      H={ReqID, _, _} ->
             Handler(H),
             ui_loop(ReqID, Handler, NeedToAdministrate)       
    end.
 
request(ReqID,NameArgs,FunToRun) ->
    case ?UI:request(ReqID,NameArgs) of
        ok -> FunToRun();
        deny -> throw(request_denied)
    end.

%%% ============================================================================
%%% Tree data structure

%% @doc Create an empty tree where Id-s are compared by a user given 
%% "equality function" (Eq)
%% A tree node is the following: {Id,ParentId,Data,ListOfChildrenIds}
%% @spec tree_new() -> tree()
tree_new() -> [].

%% @doc Create a tree root
%% @spec tree_root(term(),term()) -> tree()
tree_root(Id,Data) -> [{Id,no_parent,Data,[]}].

%% @doc Add a node to the tree as a child of Parent
%% @spec tree_add_node(tree(),term(),term(),term(),fun()) -> tree()
tree_add_node(Tree,Parent,Id,Data,Eq) -> 
    tree_add_node_(Tree,Parent,Id,Data,Eq).
tree_add_node_([],_,Id,Data,_Eq) -> [{Id,no_parent,Data,[]}];
tree_add_node_([N={NodeId,Par,NodeData,NodeList}|NS],Parent,NewId,Data,Eq) ->
    case Eq(NodeId,Parent) of
        true -> NewNodeList=NodeList++[NewId],
                [{NodeId,Par,NodeData,NewNodeList}|NS++[{NewId,NodeId,Data,[]}]];
        _    -> [N|tree_add_node_(NS,Parent,NewId,Data,Eq)]
    end.

%% @doc Makes a new tree from a node (not from its subtree)
%% @spec tree_node_to_new_tree(tree(),term(),fun()) -> tree()
tree_node_to_new_tree(Tree,Id,Eq) ->
    {_,_,Data,_}=tree_get_node(Tree,Id,Eq),
    NewTree = [{Id,no_parent,Data,[]}],
    CollectFun = fun({NodeId,_,_,_}) ->
                    not Eq(NodeId, Id) end,
    Deleted = lists:filter(CollectFun, Tree),
    DeletedIds = lists:map(fun({NodeId,_,_,_}) -> NodeId end, Deleted),
    {NewTree, DeletedIds}.

%% @doc Get node with the given Id from the tree
%% @spec tree_get_node(tree(),term(),fun()) -> tree_node()
tree_get_node(_Tree=[],_Id,_) -> notfound;
tree_get_node([{NodeId,_,_,_}=N|NS],Id,Eq) -> 
    case Eq(NodeId, Id) of
        true -> N;
        _    -> tree_get_node(NS,Id,Eq)
    end.
%% @doc Delete subtree, gives a {NewTree, DeletedIds} pair
%% @spec tree_delete_subtree(tree(),term(),fun()) -> tree()
tree_delete_subtree(Tree,Id,Eq) ->
    {_,_,_,NodeList}=tree_get_node(Tree,Id,Eq),
    Tree2 = tree_delete_node(Tree,Id,Eq),
    tree_delete_subtree_(Tree2,NodeList,Tree2,Eq,[Id]).
tree_delete_subtree_(_,[],NewTree,_Eq,DelIds) -> {NewTree, DelIds};
tree_delete_subtree_([{NId,_,_,List}|NS],Ids,NewTree,Eq,DelIds) ->
    IsMember=list_member(NId,Ids,Eq),
    if
        IsMember -> tree_delete_subtree_(NS,list_delete(NId,Ids,Eq)
                    ++List,tree_delete_node(NewTree,NId,Eq),Eq,[NId|DelIds]);
        true -> tree_delete_subtree_(NS,Ids,NewTree,Eq,DelIds)
    end.

%% List functions reimplemented using Eq equivalence function
list_member(Obj,[H|T],Eq) ->
    case Eq(H,Obj) of
       true      -> true;
       _         -> list_member(Obj,T,Eq)
    end;
list_member(_,[],_) -> false.
list_delete(Item, [H|Rest], Eq) -> 
    case Eq(Item,H) of
       true       -> Rest;
       _          -> [H|list_delete(Item, Rest, Eq)]
    end;
list_delete(_, [], _) -> [].

%% @doc Delete node with given id from tree, removing all connections as well
%% This function does not remove the subtree of the node. so it may return an
%% inconsistent tree if used not carefully.
%% @spec tree_delete_node(tree(),term(),fun()) -> tree()
tree_delete_node(Tree,Id,Eq) ->
    {_,Parent,_,_}=tree_get_node(Tree,Id,Eq),
    tree_delete_node_(Tree,Id,Parent,Eq).
tree_delete_node_([],_,_,_) -> [];
tree_delete_node_([N={NodeId,NParent,NodeData,NodeList}|NS],Id,Parent,Eq) -> 
    case Eq(NodeId,Id) of
        true -> tree_delete_node_(NS,Id,Parent,Eq);
        _    -> case Eq(Parent,NodeId) of 
                    true -> 
                        NewList=list_delete(Id,NodeList,Eq),
                        [{NodeId,NParent,NodeData,NewList}|
                         tree_delete_node_(NS,Id,Parent,Eq)];
                    _    ->
                        [N|tree_delete_node_(NS,Id,Parent,Eq)]
                end
    end.

%% @doc Get subtree starting from given node in tree
%% This function also converts the given Id to a root and gives a list of
%% nodes which were dropped: {NewTree, Dropped}
%% @spec tree_get_subtree(tree(),term(),fun()) -> tree()
tree_get_subtree(Tree,Id,Eq) -> 
    {_,_,Data,NodeList}=tree_get_node(Tree,Id,Eq),
    tree_get_subtree_(Tree,NodeList,Id,[{Id,no_parent,Data,NodeList}],Eq,[]).
tree_get_subtree_([],_,_,NewTree,_Eq,Dropped) -> {NewTree, Dropped};
tree_get_subtree_([{NodeId,_,_,List}=N|NS],Ids,RootId,NewTree,Eq,Dropped) ->
    IsMember=list_member(NodeId,Ids,Eq),
    IsRoot  = Eq(RootId,NodeId),
    if
        IsMember -> tree_get_subtree_(NS, Ids ++ List, RootId,
                                         NewTree ++ [N], Eq, Dropped);
        IsRoot -> tree_get_subtree_(NS,Ids,RootId,NewTree,Eq,Dropped);
        true -> tree_get_subtree_(NS,Ids,RootId,NewTree,Eq,[NodeId|Dropped])
    end.

%% @doc Tree transformation with 1-1 transformation function for node Ids, parent ids
%% children ids, node data
%% @spec tree_tr_(tree(),fun(),fun(),fun(),fun(),[]) -> tree()
tree_tr_([{NodeId,ParentId,NodeData,List}|NS],TrF1,TrF2,TrF3,DataFun,Acc) ->
    List2 = [ TrF3(Item) || Item <- List ],
    N2 = {TrF1(NodeId),TrF2(ParentId),DataFun(NodeId,NodeData),List2},
    tree_tr_(NS,TrF1,TrF2,TrF3,DataFun,[ N2 | Acc ]);
tree_tr_([],_,_,_,_,Acc) -> lists:reverse(Acc).

%% @doc Traversal of a tree (with a data transformation function)
%% @spec tree_traversal(tree(),fun()) -> tree()
tree_traversal(Tree,DataFun) -> 
    tree_tr_(Tree,?IdFun,?IdFun,?IdFun,DataFun,[]).
%% @doc Tree transformation with a transformation function for node properties
%% like Id, parent Id, Children Id, and one for node data
%% @spec tree_transformation(tree(),fun(),fun()) -> tree()
tree_transformation(Tree,TrFun,DataFun) -> 
    tree_tr_(Tree,TrFun,TrFun,TrFun,DataFun,[]).

%%%
%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Convert a graphviz plain-ext representation to list representation :
%% A graph: {Width, Height, Scale, Nodes, Edges}
%% A node:  {Name, {NodeData, Label, CenterX, CenterY, 
%%                        Radius, Width, Height, Style}}
%% An edge: {{From, To}, {Points, Label, LabelPointX, LabelPointY, 
%%                                              Style, ArrayPoints}}
%%
-define(Wider, 1).

%% @doc Get a graphviz plain-ext graph in list representation
%% @spec get_graph(string() | binary()) -> list_graph()
get_graph(FilePath) when is_list(FilePath) ->
    try 
        Cmd = "dot -Tplain-ext "++FilePath++" -o "++FilePath++"_gen.dot",
        case os:cmd(Cmd) of
            "Warning:" ++  _   -> ok;
            []                 -> ok;
            Er                  -> throw({cmd_error, Er})
        end,
        case file:read_file(FilePath ++ "_gen.dot") of
            {ok, Binary} -> get_graph(Binary);
            _            -> throw(file_error)
        end
    catch
        file_error -> ?wxErr("Error when reading the dot file.");
        {cmd_error, Er0}  -> 
            ?wxErr("Error when trying to execute graphviz (dot): "
                     ++ Er0);
        {parse_error, Error} -> ?wxError(Error);
        _:_ -> no_graph
    end;

 get_graph(Binary) when is_binary(Binary) ->
    try
        GraphData=binary_to_list(Binary),
        Lines=
          real_lines(string:tokens(lists:filter(fun(A) -> A =/= $\r end,
                                                GraphData),"\n")),
        {_,Sc,W,H}=list_to_tuple(string:tokens(hd(Lines)," ")),
        Width=list_to_num(W),
        Height=list_to_num(H),
        Scale=list_to_num(Sc),
        Nodes=[parse_graphnode_line(E) || E<-Lines, hd(E)==$n],
        Edges=[parse_graphedge_line(E,Nodes) || E<-Lines, hd(E)==$e],
        {Width * ?Wider, Height, Scale, Nodes, Edges}
    catch
        X:Y -> throw({parse_error, "Graph error: " 
                                    ++ io_lib:write(X) ++ io_lib:format(", ~p",[Y])})
    end.

parse_graphnode_line(E) ->
    try
        L=string:tokens(E," "),
        [_, Name, CX, CY, W, H, _ | _ ] = L,        
        CLabel = lists:nth(7,L),       
        NW = list_to_num(W), 
        NH = list_to_num(H),
        StyleColor = lists:last(L),
        {Shape,Label,NodeData} = parse_complex_label(CLabel),
        Style = {Shape,StyleColor},
        R = radius(NW,NH),
        {Name, {NodeData, Label, list_to_num(CX) * ?Wider, 
                list_to_num(CY), R, NW, NH, Style}}
    catch
        X:Y -> throw({parse_error, "Graphnode line error: " 
                                     ++ io_lib:write(X) ++ io_lib:write(Y)})
    end.

parse_graphedge_line(E,Nodes) ->
    try 
        L=string:tokens(E," "),
        [_, From, To, N | Tail ] = L,
        Points0 = pairs(lists:sublist(Tail,2*list_to_num(N)),fun list_to_num/1),
        Points = lists:map(fun({X,Y}) -> {?Wider*X,Y} end, Points0),
        {Label, LX, LY} =
        case lists:nthtail(4 + 2 * list_to_num(N), L) of
            [Label0, LX0, LY0 | _] -> {Label0, LX0, LY0};
                                _  -> {no_label, dummy, dummy}
        end,
        Style = {lists:nth(length(L)-1,L), lists:last(L)},
        _ToNode = {_,{_,_,CX,CY,R,_,_, _}} = lists:keyfind(To, 1, Nodes), 
        _FromNode = {_,{_,_,CX2,CY2,R2,_,_, _}} = lists:keyfind(From, 1, Nodes),
        LastPoint = lists:last(Points),
        NearbyPoints = create_nearby_points(CX, CY, LastPoint, R),
        NearestPoint = case NearbyPoints of
                [] -> LastPoint;
                _  -> lists:last(NearbyPoints)
        end,
        ArrInfo = arrow_info(CX,CY,R,NearestPoint),
        StartPoint = get_intersection_point(CX2,CY2,R2,hd(Points)),
        Points2 = [StartPoint|Points] ++ NearbyPoints,
        {{From, To}, 
         {Points2, Label, list_to_num(LX), list_to_num(LY), Style, ArrInfo}}
    catch
        X:Y -> throw({parse_error, "Graphedge line error: "
                                     ++ io_lib:write(X) ++ io_lib:write(Y)})
    end.

real_lines(Lines) -> real_lines(Lines, []).

real_lines([L1, L2 | Tail], Acc) ->
    case lists:last(L1) of
        $\\ -> real_lines([L1 ++ L2 | Tail], Acc);
        _   -> real_lines([L2 | Tail], [L1 | Acc])
    end;

real_lines([L1], Acc) -> lists:reverse([L1 | Acc]);
real_lines([], Acc) -> lists:reverse(Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Depgraph tree representation to an easily drawable inner representation
%% Graph = {Width, Height, Ranking, Nodes, Edges}
%% Edge = {{From,To}, Rank, Points, Label, LabelPointX, LabelPointY, 
%%                                                Style, ArrowPoints}
%% Node = {Name,Rank,NoDt,Label,X,Y,Radius,Width,Height}

%% @doc Convert a list graph representation to an inner graph representation 
%% It is possible to translate and scale the list graph
%% @spec to_inner_graph(list_graph(), number(), number(), number()) -> 
%%        inner_graph()
to_inner_graph(no_graph, _, _, _) -> no_graph;
to_inner_graph({Width, Height, Scale, Nodes, Edges}, UScal, UTrX, UTrY) ->
    put(max_rank, 0),
    Scal = Scale * UScal,
    NodeNum = length(Nodes),
    Nodes2 = [ inner_node(N,Scal,UTrX,UTrY,dict:from_list(Edges),NodeNum) 
                                                            || N <- Nodes ],
    Ranking0 = create_intervals(get(max_rank)),
    Ranking1 =
    lists:foldl(fun({_,Rank,_,_,_,_,_,_,_,_},Ranking) -> 
                        add_rank_cnt(Rank,Ranking) end,
                Ranking0, Nodes2),
    Ranking = drop_intervals(Ranking1,NodeNum),
    Edges2 = [ inner_edge(E,Scal,UTrX,UTrY,Nodes2) || E <- Edges ],
    {tr_(Width,Scal,UTrX),tr_(Height,Scal,UTrY),Ranking,Nodes2, Edges2}.

inner_node({N,{NoDt,L,X,Y,R,W,H,S}},Sc,UTrX,_UTrY,EdgesDict,NodeNum) -> 
    Pred = fun({K,_},_) when K =:= N -> true;
              ({_,M},_) when M =:= N -> true;
              (_,    _) -> false end,
    Rank = create_rank(NoDt, dict:size(dict:filter(Pred, EdgesDict)), NodeNum),
    put(max_rank, max(get(max_rank),Rank)),
    {N,Rank,NoDt,L,tr_(X,Sc,UTrX),tr_(Y,Sc,UTrX),
    tr_(R,Sc,0),tr_(W,Sc,0),tr_(H,Sc,0),S}.

inner_edge({{From, To},{Ps,L,LX,LY,S,Arr}},Sc,UTrX,UTrY,Nodes) -> 
    ToNode = lists:keyfind(To, 1, Nodes), 
    FromNode = lists:keyfind(From, 1, Nodes),
    Rank = min(element(2,ToNode),element(2,FromNode)), %% element 2 is the rank
    {{From,To}, Rank, tr_(Ps,Sc,{UTrX,UTrY}), 
     L, tr_(LX,Sc,UTrX), tr_(LY,Sc,UTrY), S, tr_(Arr,Sc,{UTrX,UTrY})}.

%% Translation and scaling, ranking

%% Translate and scale {X,Y} or a List of points, or a number
tr_({X,Y},Sc,{TrnslX,TranslY})  -> {tr_(X,Sc,TrnslX), tr_(Y,Sc,TranslY)};
tr_(L,Sc,{TrnslX,TranslY}) when is_list(L) ->
    lists:map(fun({X,Y})->
                {tr_(X,Sc,TrnslX), tr_(Y,Sc,TranslY)}
              end, L);
tr_(N,Sc,Traslation) when is_number(N) and is_number(Sc) ->
    N * Sc + Traslation;
tr_(dummy,_,_) ->
    dummy.

%% Create a rank for a node according to its importance and in/out degree
create_rank({_,Type,_}, Degree, NodeNum) -> create_rank(Type, Degree, NodeNum);
create_rank(Type, Degree, NodeNum) ->
    case Type of
        root -> Degree + 3*NodeNum;
        module -> Degree + NodeNum;
        func   -> Degree;
        _      -> Degree + 2*NodeNum
    end.
%% Create intervals from 0 to Max (Number of intervals = ?G)
create_intervals(Max) ->
    lists:map(fun(N) -> 
                {{Max-N*Max/?G, Max-(N-1)*Max/?G},0} end, lists:seq(1, ?G)).

%% Drop (nearly) empty ranking intervals
drop_intervals(IntV,NodeNum) ->
    lists:reverse(
    element(1,
    lists:mapfoldl(fun(I,no_item) -> {I,I};
                  ({_,A},Acc) when (A < 1/10 *NodeNum) -> {Acc,Acc};
                  (I,_Acc) -> {I,I}
               end,
               no_item,
               lists:reverse(IntV)))).
%% Increase the number of elements in a rank interval of a ranking
add_rank_cnt(Rank,Ranking) ->
    {Key, Cnt} = hd(lists:filter(fun({{A,_},_}) ->
                            (A =< Rank) end,
                        Ranking)),
    lists:keyreplace(Key,1,Ranking,{Key,Cnt+1}).

%% Parse a complex label given as a string
%% Convert a string to an erlang term (tuple)
parse_complex_label(CLabel) ->    
    STR1 =
    case erl_scan:string(CLabel) of
        {ok, [{string,1,STR}], _} -> STR;
        _           -> throw(parse_complex_label_error)
    end,    
    {ok, Tokens, _} = erl_scan:string(STR1++"."),
    {ok,Term} = erl_parse:parse_term(Tokens),
    Shape = proplists:get_value(shape,Term),
    Label = proplists:get_value(name,Term),
    NodeData = proplists:get_value(node,Term),   
    {Shape, Label, NodeData}.

get_clabel(MaybeCLabel) -> get_clabel(MaybeCLabel,[]).
get_clabel([Item | T],Acc) when is_list(Item) ->
    Part = try
      lists:sublist(lists:reverse(Item), 2)
    catch
      _:_ -> none
    end,
    case Part  of
        "\"}" -> lists:flatten([Acc|Item]);
        _  -> get_clabel(T,[Acc|Item])
    end;
get_clabel([Item | T],Acc) -> get_clabel(T,[Acc|Item]);
get_clabel(_,_Acc) -> "". 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Drawing helper functions (calculations)

%% @doc  Get an arrow (three point, they form an arrow when connected)
%% @spec arrow_info(point(), point(), number()) -> [point()]
arrow_info({X0, Y0},_ControlPoint={CX, CY}, Distance) ->
    {NormX, NormY} = dir_vector({CX-X0, CY-Y0}),
    P1 = add({X0,Y0}, mult_vector(-1 * Distance, {NormX, NormY})),
    P2 = add({X0,Y0}, mult_vector(Distance, {NormX, NormY})),
    arrow_info(P1, P2).
%% @doc  Get an arrow using its point
%% @spec arrow_info(point(), point()) -> [point()]
arrow_info({X0, Y0},{X1, Y1}) ->
    calc_arrow_left_right_points(X0, Y0, X1, Y1) ++ [{X1,Y1}].
%% @doc  Get an arrow (pointing to the center of a circle)
%% @spec arrow_info(number(), number(), number(), point()) -> [point()]
arrow_info(CX,CY,R,{X0, Y0}) ->
    {Xi, Yi} = get_intersection_point(CX,CY,R, {X0, Y0}),
    calc_arrow_left_right_points(X0, Y0, Xi, Yi) ++ [{Xi,Yi}].
%% @doc  Get an arrow (pointing to a rectangle)
%% @spec arrow_info(point(), point(), point(), number(), number(), number()) -> 
%%       [point()]
arrow_info({Ax,Ay}, {Bx,By}, {Ox, Oy}, W, H, Distance) ->
    {Xi, Yi} = get_intersection_point({Ax,Ay}, {Bx,By}, {Ox, Oy}, W, H),
    {NormX, NormY} = dir_vector({Ax-Xi, Ay-Yi}),
    {Px, Py} = add({Xi,Yi}, mult_vector(-1 * Distance, {NormX, NormY})),
    calc_arrow_left_right_points(Px, Py, Xi, Yi) ++ [{Xi,Yi}].

%% @doc Intersection point of a rectangle and a line 
%% A is the center of the rect and a point of the line, B is a point of the line
%% O is the position of the rect (upper left corner), W is the width, H is the height
%% @spec get_intersection_point(point(), point(), point(), number(), number()) ->
%%       point()
get_intersection_point({Ax,Ay}, {Bx,By}, {Ox, Oy}, W, H) ->
    M = try (By - Ay)/(Bx - Ax) catch
            _:_ -> inf
    end,
    if  (M == inf) ->
            if By < Ay -> {Ox + (W/2), Oy};
               true    -> {Ox + (W/2), Oy + H}
            end;
        ((H/W =< M) or ((-1*(H/W)) >= M)) ->
            if By < Ay -> {(Oy - Ay + M * Ax) / M, Oy};
               true    -> {(Oy + H - Ay + M * Ax) / M, Oy + H}
            end;
       ((H/W  >= M) and (-1*(H/W) =< M)) ->
            if Bx < Ax -> {Ox, M * Ox - M * Ax + Ay};
               true    -> {Ox + W, M * (Ox + W) - M * Ax + Ay}
            end
    end.

%% @doc Get the intersection point of a circle at {CX, CY}, with radius R
%% and a vector starting from ControlPoint to {CX, CY} 
%% @spec get_intersection_point(number(), number(), number(), point()) ->
%%       point()
get_intersection_point(CX,CY,R,ControlPoint) ->
    {X0,Y0} = ControlPoint,
    {_Xi, _Yi} = calc_intersection_point(X0, Y0, CX, CY, R).

%% @doc X0,Y0 is the last point of the path
%% CX,CY is the center point
%% W,H are the width and height of the node
%% @spec calc_intersection_point(number(), number(), number(), number(), number()) ->
%%       point()
calc_intersection_point(X0, Y0, X0, CY, R) ->
    if CY > Y0 -> {X0, CY - R};
       true    -> {X0, CY + R}
    end;
    
calc_intersection_point(X0, Y0, CX, CY, R) -> 
    M = (CY - Y0) / (CX - X0),
    EqA = math:pow(M,2) + 1,
    EqB = -2 * math:pow(M,2) * CX  - 2 * CX,
    EqC = math:pow(M,2) * math:pow(CX,2) - math:pow(R,2) + math:pow(CX,2),
    DiscR = math:sqrt(math:pow(EqB,2) - 4 * EqA * EqC),
    RX1 = (-1 * EqB + DiscR) / (2 * EqA),
    RX2 = (-1 * EqB - DiscR) / (2 * EqA),
    RY1 = M * RX1 + (-1 * M * CX + CY),
    RY2 = M * RX2 + (-1 * M * CX + CY),
    case len(RX1-X0,RY1-Y0) < len(RX2-X0,RY2-Y0) of
        true  -> {RX1,RY1};
        false -> {RX2,RY2}
    end.

%% @doc X0,Y0 is the last point of the path
%% CX,CY is the center point
%% W,H are the width and height of the node
%% @spec create_nearby_points(number(), number(), {number(), number()}, number()) ->
%%       point()
create_nearby_points(CX, CY, _LastPoint={LPX, LPY}, R) ->
    RefDist = 3 * R,
    Dist = len(CX-LPX, CY-LPY),
    if Dist > RefDist ->
            Diff = Dist - RefDist,
            Dir = dir_vector({CX-LPX, CY-LPY}),
            Translation = mult_vector(Diff, Dir),
            [tr_({LPX, LPY}, 1, Translation)];
       true -> []
    end.

%% @doc Xi,Yi is the intersection point
%% X0,Y0 is the last point of the path
%% @spec calc_arrow_left_right_points(number(), number(), number(), number()) ->
%%       [point()]
calc_arrow_left_right_points(X0, Y0, Xi, Yi) ->
    {NormX, NormY} = perpendicular_dir_vector({Xi, Yi}, {X0, Y0}),
    M = len(Xi-X0,Yi-Y0),
    Distance = ((2 / (math:sqrt(3)) * M) / 2) * 1.5,
    P1 = add({X0,Y0}, mult_vector(Distance, {NormX, NormY})),
    P2 = add({X0,Y0}, mult_vector(-1 * Distance, {NormX, NormY})),
    [P1, P2].

%% @doc Distance from origin for a given point
%% @spec len(number(), number()) -> number()
len(X,Y) ->
    math:sqrt(math:pow(X,2) + math:pow(Y,2)).

%% @doc Addition of vectors
%% @spec add(point(), point()) -> point()
add({X1,Y1},{X2,Y2}) -> {X1+X2, Y1+Y2}.

%% @doc Multiplicate a vector with a given scalar
%% @spec mult_vector(number(), point()) -> point()
mult_vector(Scal,{X,Y}) -> {Scal*X, Scal*Y}.

%% @doc Direction vector from a point (just a normalization) 
%% (the direction of the line given by the origo and a point)
%% @spec dir_vector(point()) -> point()
dir_vector({DirX, DirY}) ->
    L = len(DirX,DirY),
    {DirX/L, DirY/L}.

%% @doc Perpendicular direction vector from two point 
%% @spec perpendicular_dir_vector(point(), point()) -> point()
perpendicular_dir_vector({Xi, Yi}, {X0, Y0}) ->
    {DirX, DirY} = {Yi - Y0, X0 - Xi},
    dir_vector({DirX, DirY}).

%% @doc Center of a rectangle
%% @spec center(number(), number()) -> point()
center(W, H) -> {floor(W/2), floor(H/2)}.

%% @doc Radius of a ellipse (with a given width and height)
%% @spec radius(number(), number()) -> number()
radius(W, H) -> min(W/2, H/2).



%%% ============================================================================
%%% Implementation

decode_error(ErrList) ->
	Errs =
	[ case Err of
		[L] when is_list(L) -> L;
		[ {nexttoken, T} | _ ] -> "Ends with " ++ T;
		_					-> "Error..."
	  end
	||	Err <- ErrList ],
	string:join(Errs, ", ").

%% @doc Remove unwanted characters from a string (making SafeQueries)
make_safe(Query) -> _SafeQuery = lists:filter(query_filter_fun(), Query).

query_filter_fun()->
    fun(E)->
        if 
            E == 10 -> false;
            E == 9  -> false;
            E == 32 -> false;
            true -> true
        end
    end.

%% USEFUL

%% @doc Trim whitespaces
%% @spec trim_whitespace(string()) -> string()
trim_whitespace(Input) ->
    trim(Input).
trim(String) when is_list(String) ->
    String2 = lists:dropwhile(fun is_whitespace/1, String),
    lists:reverse(lists:dropwhile(fun is_whitespace/1, lists:reverse(String2))).
is_whitespace($\s) -> true;
is_whitespace($\t) -> true;
is_whitespace($\n) -> true;
is_whitespace($\r) -> true;
is_whitespace(_Else) -> false.

%% @doc Term to string conversion
%% @spec write(term()) -> string()
write(Term) when is_list(Term) ->
    Term;
write(Term) when is_number(Term) ->
    io_lib:write(Term);
write(Term) ->
    io_lib:write(Term).

%% Conversion

%% @doc List to number conversion
%% @spec list_to_num(list()) -> number()
list_to_num(L) when is_list(L) ->
    case string:to_float(L) of
        {error,no_float} -> list_to_integer(L);
        {F,_} -> F
    end;
list_to_num(L) -> L.

%% @doc Number to list conversion
%% @spec num_to_list(number()) -> list()
num_to_list(N) ->
    try
        float_to_list(N)
    catch
        _:_ -> integer_to_list(N)
    end.

%% @doc Making pairs from a list (with a function for transformations)
%% @spec pairs([term()], fun()) -> [{term(), term()}]
pairs(L,F) -> pairs(L, F, []).
pairs([ X, Y | Tail ], F, Acc) -> pairs(Tail, F, [{F(X),F(Y)} | Acc]);
pairs([], _, Acc) -> lists:reverse(Acc).

pairs2(L,F) -> pairs2(L,F,[]).
pairs2([P1,P2],F,Acc) -> lists:reverse([{F(P1),F(P2)}|Acc]); 
pairs2([P1,P2|Tail],F,Acc) -> pairs2([P2|Tail], F, [{F(P1),F(P2)}|Acc]);
pairs2([P1],F,Acc) -> lists:reverse([{F(P1),F(P1)}|Acc]);
pairs2([],_F,Acc) -> lists:reverse(Acc).

%% @doc Floor of a number
%% @spec floor(number()) -> integer()
floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

%% @doc Ceil of a number
%% @spec ceil(number()) -> integer()
ceil(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.
