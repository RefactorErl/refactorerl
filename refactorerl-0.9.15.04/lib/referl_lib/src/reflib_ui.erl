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

%%% @doc This module provides a bridge between user interfaces and system
%%% functionality. A user interface requests operations and receives results
%%% through this module. Requests are asychronously handled, the results are
%%% returned through a `gen_event' event manager. Events can occur
%%% independently of operations, a user interface probably wants to handle
%%% these as well.
%%%
%%% Operations requests are sent using the exported functions of this module.
%%% See the description of the functions below.
%%%
%%% == Message types ==
%%%
%%% To receive event messages, a standard `gen_event' callback module must be
%%% registered by calling {@link add_msg_handler/2}. The following event
%%% messages may be sent to the callback modules:
%%%
%%% <dl>
%%%
%%% <dt>{@type {status, Message::string()@}}</dt> <dd>A textual message that
%%% informs the user of the tool's status.</dd>
%%%
%%% <dt>{@type {error, Message::string()@}}</dt> <dd>A textual error message
%%% that describes an internal error situation of the tool.</dd>
%%%
%%% <dt>{@type {add, Files::[Path::path()]@}}</dt> <dd>The files specified by
%%% the `Files' list are added (or maybe re-added) to the database.</dd>
%%%
%%% <dt>{@type {drop, Files::[Path::path()]@}}</dt> <dd>The files specified by
%%% the `Files' list are removed from the database.</dd>
%%%
%%% <dt>{@type {invalid, Path::path()@}}</dt> <dd>The file specified by `Path'
%%% is added to the database, but it is invalid (contains errors).</dd>
%%%
%%% <dt>{@type {reload, Files::[Path::path()]@}}</dt> <dd>The files specified by
%%% the `Files' list are modified by the tool (and should be reloaded from disk
%%% by the UI).</dd>
%%%
%%% <dt>{@type {rename, {From::path(), To::path()@}@}}</dt> <dd>The file
%%% `From' is changed to have the name `To' (and this changes should be
%%% followed by the UI).</dd>
%%%
%%% <dt>{@type {showconfig, Config::[{Key::atom(), Value@}]@}}</dt> <dd>This
%%% message describes the currect runtime configuration of the tool. Currently
%%% used key values are `appbase', `include', and `output'. See also {@link
%%% saveconfig/3}.</dd>
%%%
%%% <dt>{@type {filelist, Files::[{Path::path(), Error::atom()@}]@}}</dt>
%%% <dd>This message contains the files that are currently stored in the
%%% database, and whether they haver errors or not.</dd>
%%%
%%% <dt>{@type {filepos,
%%% FilePos::[{Path::path(),SatartPos::{integer(), integer()@},
%%% EndPos::{integer(), integer()@}@}]@}}</dt> <dd>This message
%%% contains a list of starting and ending position and their containing
%%% file</dd>
%%%
%%% <dt>{@type {funlist, Functions::[{Name::atom(),
%%% Arity::integer()@}]@}}</dt> <dd>This message contains a list of functions.
%%% <small>TODO: This is rather specific to the move function refactoring, and
%%% should be improved.</small></dd>
%%%
%%% <dt>{@type {recordlist, Records::[Name::atom()]@}}</dt> <dd>This message
%%% contains a list of records. <small>TODO: this is rather specific to the
%%% move record refactoring, and should be improved.</small></dd>
%%%
%%% <dt>{@type {macrolist, Macros::[Name::atom()|string()]@}}</dt> <dd>This
%%% message contains a list of macros. <small>TODO: this is rather specific
%%% to the move macro refactoring, and should be improved.</small></dd>
%%%
%%% <dt>{@type {question, {Id::integer(), Details::proplist()@}@}}</dt>
%%% <dd>Contains a question that must be answered by {@link reply/2} or
%%% cancelled by {@link cancel/1}. <small>TODO: `Details' are to be
%%% specified.</small></dd>
%%%
%%% <dt>{@type {uifinished, Operation::atom()@}}</dt> <dd>The spawning of the
%%% specified operation is complete on the UI side.</dd>
%%%
%%% <dt>{@type {trfinished, ok@}}</dt> <dd>An operation is finished
%%% on the transform side.</dd>
%%%
%%% <dt>{@type {progress, {add|drop, Path::path(), Count::integer(),
%%% Max::integer()@}@}}</dt> <dd>Progress report of a file (re)loading or
%%% dropping operation. `Max' is the maximal number of steps, `Count' is the
%%% number of currently finished steps.</dd>
%%%
%%% </dl>
%%%
%%% @todo actualize comments
%%% @todo finish filelist
%%% @todo transform
%%%
%%% A quick overview of the new message format.
%%% Unicast:
%%%  {ReqID, progress, todo()} |
%%%  {ReqID, reply, {error,Reason} | {ok,any()}}
%%%  {ReqID, question, {NewID,todo()}}
%%%  {ReqID, answer, todo()}
%%% Broadcast:
%%%  {B,statusinfo,StatusData}
%%%   where StatusData=
%%%    {shutdown,Reason} |
%%%    {reset,Reason} |
%%%    {change,FileChange}
%%%     where FileChange = [{Filename,[{rename,New}    |
%%%                                    {content,true}  |
%%%                                    {present,true|false}|
%%%                                    {error,Errors}  |
%%%                                    {lastmod,todo()}|
%%%                                    {type,todo()} ]}]
%%%      where Errors = [todo()]
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>

-module(reflib_ui).
-vsn("$Rev: 1479$ ").

%%% ----------------------------------------------------------------------------
%%% Client exports

%%% Database control
-export([stop/1, reset/1, reset/2, clean/1, add/2, drop/2, backup/2, ls_backups/1,
         backup_info/2, current_posmode/1, saveconfig/4]).
-export([create_graph/2, rename_graph/3, ls_graphs/1, actual_graph/1, restore/2,
         undo/2, load_graph/2, delete_graph/2, delete_all_graphs/1]).
-export([load_beam/4, add_dir/2, generate_dir/2, drop_dir/2, drop_files/2]).
-export([add_by_emakefile/2, load_configuration/2, unload_configuration/2]).
-export([cat_file/2, cat_fun/4, cat_recmac/3]).
-export([syn_leaves/2, syn_index/4, syn_class/2, syn_tree_text/2,
         syn_node_type/2, syn_first_leaf/2, syn_last_leaf/2]).
-export([synchronize/1, database_hash/1, file_hash/2]).
% Analysers
-export([anal_dyn/1, clean_dyn/1, anal_dyn_spec/1, anal_message/1]).

%%% HTML generation
-export([html_generate_node/3, html_generate/2]).

%%% Status queries
-export([status/2, showconfig/1, filelist/1, status_info/2, system_info/1,
         data_dir/1]).

%%% Enviromental variables
-export([get_envs/1, get_env/2, add_env/3, del_env/2, del_env_val/3, set_env/3]).

%%% Smart graph
-export([generate_smart_graph/2]).

%%% File Cache Server
-export([generate_all/2]).

%%% Directory sorting
-export([dir_sort/1, dir_sort_get_dirs/2, dir_sort_get_mods/2]).

%%% Function block anal.s
-export([refusr_fb_regexp_re/2]).

%%% Interface Layers
-export([if_layers_show_insults/3, if_layers_draw/3, 
         if_layers_draw/4, if_layers_check_layered_architecture/3]).

%%% Duplicated code
-export([duplicated_code_const_var_diff/3, get_all_dupcode_result/1,
        get_dupcode_group/4,
        get_dupcode_result/4, get_algorithms/1, get_algorithm_data/2]).

%%% Clone IdentifiErl
-export([clone_identifierl/2, clone_identifierl_by_pos/6,
         clone_identifierl_by_pos/7]).

%%% Semantic queries
-export([draw/3, get_running_queries/2, kill_query/2]).

%%% Internal queries
-export([funlist/2, recordlist/2, macrolist/2]).

%%% Refactoring
-export([transform/3, reply/3, cancel/2]).

%%% Clustering
-export([cl_options/2, run_cl/4, cl_refresh/1, cl_prev_clustering/1]).

%%% Metrics
-export([metric_mode/1, metric_mode/2, metric_mode/3]).

%% Callbacks
-export([error_text/2]).

%% Helpers
-export([action_module_name/1, is_transformation/1, get_file_from_server/2,
         do_depanal_and_transfer/3]).

%% Graph querying
-export([graph_query/4, graph_data/2, form_length/2, function_positions/2]).

%% Skeleton
-export([update_skeleton/4, update_prev_skeleton_comment/3, delete_skeleton/2,
         skeleton_call_format/2, save_skeleton/4, list_skeletons/1, 
         evaluate_skeleton/4, evaluate_skeleton/5, try_parse_skeleton/3,
         determine_sq_request_type/2, do_autocomplete_skeleton/2]).
%% Deadcode
-export([deadcode/2,deadcode/3,deadcode_interface/4]).

%% Wx interface
-export([start_wx/1, start_wx/2, fundef_pos/2, start_pos/2, node_pos/2, 
                definition/3, node_type/2, get_container/2, function_text/2]).
-export([get_all_invtab_name_user_pairs/1, get_from_invtab/2,
        delete_from_invtab/2, insert_to_invtab/2, delete_from_qtab/2,
        find_in_qtab_by_pattern/2, find_in_qtab/2, insert_to_qtab/6,
        insert_to_qtab/2, update_qtab_if_needed/6, deptab_delete_all/1,
        deptab_delete/2, deptab_match_opts/2, deptab_dets_insert/2]).
-export([act_graph/1]).

%% Dependency graph
-export([draw_dep_graph/2]).

-include("lib.hrl").
-include_lib("kernel/include/file.hrl").


%%% ----------------------------------------------------------------------------
%%% UI protocol description

% each function invocation must return with exactly one of the following:
-define(NoReply,   noreply).
-define(Dat(X),    {ok,X}).
-define(OK,        ?Dat([])).
-define(ERR(R),    {error,R}).
-define(LErr(R),   ?LocalErr(R,[])).
-define(LocalErr(R,L), ?ERR(?LocalError(R,L))).
-define(RefErr(R,L),   ?ERR(?RefError(R,L))).

% the following can be sent anytime:
send_progress(MCB, Op, File, Percent, FormCount, FormMax, KBps) ->
    (MCB#msg_cb.unicast)(progress,{Op, File, Percent, FormCount, FormMax, KBps}).

send_change(MCB,Change) ->
    (MCB#msg_cb.broadcast)(statusinfo,[{change,Change}]).

send_shutdown(MCB) ->
    (MCB#msg_cb.broadcast)(statusinfo,[{shutdown,"manual shutdown initiated"}]).

send_reset(MCB) ->
    (MCB#msg_cb.broadcast)(statusinfo,[{reset,"manual restart initiated"}]).

send_difference(MCB, DiffFile)->
    (MCB#msg_cb.broadcast)(statusinfo,[{difference,
                                        DiffFile++": "++
                                        "The content of the database is inconsistent "++
                                        "with the content of the file on the disk."}]).

% possible formats of changes
-define(PresentCorrect(File),
        {File, [{present,true}]}).
-define(PresentCorrectTM(File,Type,LastMod),
        {File, [{present,true},
                {type, Type}, {lastmod, LastMod}]}).
-define(PresentError(File,Errors),
        {File, [{error,Errors}]}). %,{present,true}
-define(PresentErrorTM(File,Type,LastMod,Errors),
        {File, [{present,true}, {error,Errors},
                {type, Type}, {lastmod, LastMod}]}).
-define(NotPresent(File),
        {File, [{present,false}]}).
-define(Modified(File),
        {File, [{content,true}]}).
-define(Renamed(OldPath,NewPath),
        {OldPath, [{rename,NewPath}]}).
-define(AddedCorrect(File),     ?PresentCorrect(File)).
-define(AddedError(File,Error), ?PresentError(File,Error)).
-define(Dropped(File),          ?NotPresent(File)).


%%% ----------------------------------------------------------------------------
%%% types

%%% @type path() = string(). The absolute path of a file.


%%% ============================================================================
%%% Error texts

error_text(wrangler_dir, []) ->
    ["Please set Wrangler's ",
     "installation directory, and restart RefactorErl"];
error_text(cl_ui_refresh, []) ->
    ["cl_ui refresh: something wrong"];
error_text(invalid_checkpoint_number, []) ->
    error_text(no_backup, []);
error_text(no_backup, []) ->
    ["Need a backup to do it"];
error_text(nothing_changed, []) ->
    ["Nothing has changed since the last backup"];
error_text(invalid_path_number, []) ->
    ["An invalid path number has been specified"];
error_text(undef, []) ->
    ["undefined function"];
error_text(noresult, []) ->
    ["no result available"];
error_text(trfail, []) ->
    ["initiation of the transformation failed"];
error_text(no_dups, []) ->
    ["Wrangler did not find any ",
     "duplicated code fragments"];
error_text(not_found, [Path])->
    ["No module or BEAM could be handled from \"", Path, "\""];
error_text(not_found_emakefile, [Path])->
    ["No EMakefile could be handled from \"", Path, "\""];
error_text(bad_emakefile, [Path, ErrorMsg])->
    ["Processing failed for Emakefile \"", Path, "\", the error: ",ErrorMsg];
error_text(somebad, [])->
    ["Processing failed for some files, check the errors"];
error_text(graph_already_exist, [Name])->
    ["A graph already exists with the given name: \'", ?MISC:to_list(Name), "\'"];
error_text(graph_not_exist, [OldName, _NewName])->
    error_text(graph_not_exist, [OldName]);
error_text(graph_already_exist, [_OldName, NewName])->
    error_text(graph_already_exist, [NewName]);
error_text(graph_not_exist, [Name])->
    ["The graph you have specified does not exist: \'", ?MISC:to_list(Name), "\'"];
error_text(invalid_name, [_OldName, NewName])->
    error_text(invalid_name, [NewName]);
error_text(invalid_name, [Name])->
    ["The name you have specified is invalid: \'", ?MISC:to_list(Name), "\'"];
error_text(graph_is_in_use, [Name])->
    ["The graph you have specified is in use,",
     " and cannot be deleted now: \'", ?MISC:to_list(Name), "\'"];
error_text(graph_load_fail, [Name]) ->
    ["The graph \'" ++?MISC:to_list(Name) ++ "\' has been loaded, but with an empty database, because it may be crashed!"];
error_text(invalid_backup, [Backup]) ->
    ["The backup or the checkpoint number you have specified is invalid: \'",
      ?MISC:to_list(Backup), "\'"];
error_text(corrupted_backup, [Backup])->
    ["The given backup is corrupted and has been renamed (has a 'corrupted_' prefix): \'",
      ?MISC:to_list(Backup), "\'\n",
      "The database remains the same."];
error_text(env_denied, [Env])->
    ["The given key (",
     ?MISC:to_list(Env),
     ") is the key of a system-level enviromental node. ",
     "These nodes can not be written or read by users."];
error_text(graph_query_denied, [])->
    "The given query is not allowed.".


%@todo refactor to ?LocalError
%error_message({no_include_file, File}) ->
%    ?MISC:format("Include file \"~s\" not found", [File]);
%error_message({some_proc, []})->
%    ?MISC:format("Not all files were processed", []);
%error_message({none_proc, []})->
%    ?MISC:format("No files were processed", []);
%error_message(Error) ->
%    ?MISC:format("Error: ~p", [Error]).



%%% ============================================================================
%%% Standard UI exports
%% @spec stop(#msg_cb{}) -> ok
%% @doc Stops the RefactorErl server.
stop(MCB) ->
    send_shutdown(MCB),
    init:stop(),
    ?OK.
% "RefactorErl server is shutting down..."

%% @spec status(#msg_cb{}, path()) -> ok
%% @doc Requests information about the status of `File'. The result is a
%% message of type `add', `invalid', or `drop'.
status(_MCB, FileName) when is_list(FileName) ->
    case ?Query:exec(?File:find(FileName)) of
        [File] ->
            case ?Query:exec(File, ?File:error_forms()) of
                [] -> ?Dat([?PresentCorrect(FileName)]);
                E  -> ?Dat([?PresentError(FileName,
                                          decode_error_form(File,E))])
            end;
        []  -> ?Dat([?NotPresent(FileName)])
    end.


%% @spec add(#msg_cb{}, path()) -> ok
%% @doc Adds `File' to the database.
%% @todo maybe introduce a `reload' or `update' message
add(MCB, FileName) when is_list(FileName) ->
    save_database(do_add_file(MCB,FileName)).

%% @spec drop(#msg_cb{}, path()) -> ok
%% @doc Drops `File' from the database.
drop(MCB, FileName) when is_list(FileName) ->
    save_database(do_drop_file(MCB,FileName)).

%% @spec drop_files(#msg_cb{}, [path()]) -> ok
%% @doc Drops the given files from the database.
drop_files(MCB, FileNamesList) when is_list(FileNamesList) ->
    Res = [ do_drop_file(MCB,FileName) || FileName <- FileNamesList ],
    Return = case lists:filter(fun(I) -> I =/= ?OK end, Res) of
                    [] -> ?OK;
                    Ls -> hd(Ls)
    end,
    save_database(Return).

%% @spec add_dir(#msg_cb{}, path()) -> ok
%% @doc Adds recursively to the database starting from `File'.
add_dir(MCB,FileName) when is_list(FileName) ->
    save_database(add_flat(recurse_erl(MCB, FileName, fun add_filedir/2))).

%% @spec add_by_emakefile(#msg_cb{}, path()) -> ok
%% @doc Adds recursively to the database, 
%% reads configuration from the given Emakefile.
%% If the Emakefile contains relative paths, then the absolute paths are made
%% using the directory of the Emakefile as base.
add_by_emakefile(MCB,Files=[C|_]) when is_list(Files) andalso is_list(C)->
    unpack_results([add_by_emakefile(MCB, File) || File <- Files]);
add_by_emakefile(MCB,FileName=[C|_]) when is_list(FileName) andalso is_integer(C)->
    handle_emakefile(MCB, FileName, all);
add_by_emakefile(_,BadFileName) ->  
    throw(?LocalError(not_found_emakefile, [?MISC:any_to_string(BadFileName)])).

%% @spec load_configuration(#msg_cb{}, path()) -> ok
%% @doc Reads the configuration from the given Emakefile and stores it in the DB.
%% If the Emakefile contains relative paths, then the absolute paths are made
%% using the directory of the Emakefile as base.
load_configuration(MCB,Files=[C|_]) when is_list(Files) andalso is_list(C)->
    unpack_results([load_configuration(MCB, File) || File <- Files]);
load_configuration(MCB,FileName=[C|_]) when is_list(FileName) andalso is_integer(C)->
    handle_emakefile(MCB, FileName, load);
load_configuration(_, BadFileName) ->
    throw(?LocalError(not_found_emakefile, [?MISC:any_to_string(BadFileName)])).

%% @spec unload_configuration(#msg_cb{}, path()) -> ok
%% @doc Reads the configuration from the given Emakefile and removes them from the DB.
%% If the Emakefile contains relative paths, then the absolute paths are made
%% using the directory of the Emakefile as base.
unload_configuration(MCB,Files=[C|_]) when is_list(Files) andalso is_list(C)->
    unpack_results([unload_configuration(MCB, File) || File <- Files]);
unload_configuration(MCB,FileName=[C|_]) when is_list(FileName) andalso is_integer(C)->
    handle_emakefile(MCB, FileName, unload);
unload_configuration(_, BadFileName) ->
    throw(?LocalError(not_found_emakefile, [?MISC:any_to_string(BadFileName)])).    

%% @spec generate_dir(#msg_cb{}, path()) -> ok
%% @doc Generates html recursively starting from `File'.
generate_dir(MCB,FileName) when is_list(FileName) ->
    add_flat(recurse_erl(MCB,FileName, fun generate_filedir/2)).

%% @spec drop_dir(#msg_cb{}, path()) -> ok
%% @doc Drops recursively to the database starting from `File'.
drop_dir(MCB,FileName) when is_list(FileName) ->
    case filelib:is_dir(FileName) of
        true -> drop_filedir(MCB, FileName);
        false -> case ?MISC:is_erl(FileName) of
                    true->do_drop_file(MCB, FileName);
                    false -> ok
                 end
    end,
    save_database(ok),
    ?OK.
%    case filelib:is_regular(FileName) of
%        true ->
%            add_flat(recurse_erl(MCB,FileName, fun drop_filedir/2));
%        false ->
%            drop_filedir(MCB, FileName)
%    end.

%% @spec load_beam(#msg_cb{}, path(), path(), boolean()) -> ok
%% @doc Loads a BEAM file compiled with `debug_info' and saves the result
load_beam(MCB,FileName,TargetDir,ToSave)
  when is_list(FileName), is_list(TargetDir), is_boolean(ToSave) ->
    case refcore_loadbeam:start(FileName,TargetDir,ToSave) of
        {error, Reason} ->
            send_change(MCB,[?AddedError(FileName,[])]), %@todo
            ?LErr(Reason);
        {ok, File} ->
            Add = [?File:path(FN) || FN <- ?Query:exec(File, ?File:includes())],
            send_change(MCB,[?AddedCorrect(F) || F <- Add]),
            ?OK
    end.

%% @spec draw(#msg_cb{}, path(),
%%            integer() | atom() | list(integer() | atom())) -> ok
%% @doc Creates a `.dot' drawing of the graph in the database, and saves it in
%% `File'. The contents of the graph are filtered according to `Filter'; the
%% value `1' means no filtering, then different numbers select different
%% filters. Also, filters might be given by atom keywords.
draw(_MCB, File, Type) ->
    Filter = convert_filter(Type),
    ok = ?DRAW_GRAPH:draw_graph(File, Filter),
    ?OK.

%% @spec showconfig(#msg_cb{}) -> ok
%% @doc Requests configuration information. The result is sent in a message of
%% type `showconfig'.
showconfig(_MCB) ->
    ?Dat([{Name, Value} ||
              Env <- ?Query:exec([env]),
              #env{name=Name, value=Value} <- [?Graph:data(Env)]]).

%% @spec system_info(#msg_cb{}) -> {ok, proplist()}
%% @doc Returns information about the RefactorErl system.
system_info(_MCB) ->
    ?Dat([{db_mode,     ?Graph:get_dbmod()},
          {pos_mode,    ?FileMan:get_pmod()},
          {appbase,     ?Syn:get_env(appbase)},
          {include,     ?Syn:get_env(include)},
          {output,    ?Syn:get_env(output)},
          {data_dir,    ?MISC:data_dir()},
          {referl_base, get_base()},
          {server_name, ?REFERL_NODE}]).

get_base()->
    Path0 = filename:split(filename:dirname(code:which(?MODULE))),
    filename:join(lists:sublist(Path0,length(Path0)-3)).

%% @spec filelist(#msg_cb{}) -> ok
%% @doc Requests a list of the files in the database. The result is sent in a
%% message of type `filelist'.
filelist(_MCB) ->
    filelist_parallel().
%   filelist_seq:
%    Files = ?Query:exec([file]), %@todo
%    FileStats =
%        [ case ?Query:exec(FileNode, ?File:error_forms()) of
%              [] -> ?PresentCorrect(FilePath);
%              E  -> ?PresentError(FilePath,decode_error_form(FileNode,E))
%          end || FileNode <- Files, FilePath <- [?File:path(FileNode)]],
%    ?Dat(FileStats).

%% @spec reset(#msg_cb{}) -> ok
%% @doc Resets the database.
reset(MCB) ->
    send_reset(MCB),
    referl_htmlserver:reset(),
    ?FileCacheMan:reset(),
    ?Graph:reset_schema(),
    save_database(ok),
    ?OK.

%% @spec reset(#msg_cb{}, rel | abs ) -> ok
%% @doc Resets the database, and changes the positioning mode of the server.

reset(MCB,rel) ->
    reset(MCB),
    change_posmode(rel),
    ?OK;
    
reset(MCB,abs) ->
    reset(MCB),
    change_posmode(abs),
    ?OK;

reset(MCB,_) ->
    reset(MCB),
    ?OK.

change_posmode(PosMode) ->
    ?FileMan:restart(PosMode).

current_posmode(_MCB) ->
    PosMode = ?FileMan:get_pmod(),
    ?Dat(PosMode).

clean(_MCB) ->
    ?Graph:clean(),
    ?OK.

synchronize(MCB)->
    WorkList = [{FilePath, LastMod} || 
       File <- ?Query:exec([file]), 
       LastMod <- [calendar:now_to_datetime((?ESG:data(File))#file.lastmod)],
       FilePath <- [?File:path(File)]],
    [reload_if_modified(MCB, WorkElem) || WorkElem <- WorkList],
    save_database(?OK).

database_hash(_MCB) ->
    ?Dat(?MISC:database_hash()).

file_hash(_MCB, File) ->
    ?Dat(?MISC:file_hash(File)).
    
anal_message(_MCB) ->
    ?Dat(save_database(refanal_message:analyse())).
    
anal_dyn(_MCB) ->
    ?Dat(save_database(refanal_dynfun:analyse())).
    
clean_dyn(_MCB) ->
    ?Dat(save_database(refanal_dynfun:clean())).

anal_dyn_spec(_MCB) ->
    refanal_list:analyse(),
    ?Dat(save_database(refanal_dynfun:analyse())).

%% @spec get_running_queries(#msg_cb{}, default_output | str_output) -> {ok, [] | [any()]}
%% @doc Returns the list of the currently running semmantic queries. 
%% default_output means, that every information is returned about the queries,
%% str_output means, that only a list of strings with the names of the queries is returned
get_running_queries(_MCB, default_output)-> 
            ?Dat(?Transform:get_running_queries());
get_running_queries(_MCB, str_output)->
    Running = ?Transform:get_running_queries(),
    Result = [ begin 
                   { QueryId, _, _, Opt} = AQuery,
                   QueryStr = proplists:get_value(querystr, Opt),
                   lists:concat([QueryStr, " - ", QueryId])
                    end || AQuery <- Running ],
    ?Dat(Result).

%% @spec kill_query(#msg_cb{}, QueryId::integer()) -> {ok, ok | not_found}
%% @doc Aborts the query which is identified by the given query id.
kill_query(_MCB, QueryId)->
    ?Dat(?Transform:kill_query(QueryId)).

%% @spec cat_file(#msg_cb{}, atom() | string()) -> {ok, string()} | {error, any()}
%% @doc Returns the textual content of the given file.
cat_file(_MCB, ModFile)->
    try
        ?Dat(?Syn:tree_text(get_filenode(ModFile)))
    catch
        Err = {_,_,_}-> ?ERR(Err);
        E -> ?LocalErr(E, [])
    end.

%% @spec cat_fun(#msg_cb{}, atom() | string(), atom(), integer()) -> 
%%  {ok, string()} | {error, any()}
%% @doc Returns the textual content of the given function.
cat_fun(_MCB, ModFile, FunName, Arity)->
    try      
        Mod = get_modnode(ModFile),
        Fun = ?Query:exec1([Mod],
                           ?Query:seq([?Fun:find(FunName,Arity),
                                       ?Fun:definition()]),
                           ?RefError(fun_not_found,[FunName,Arity])),
        ?Dat(?Syn:tree_text(Fun))
    catch
        Err = {_,_,_}-> ?ERR(Err);
        E -> ?LocalErr(E, [])
    end. 

%% @spec cat_recmac(#msg_cb{}, atom() | string(), atom() | string()) -> 
%%  {ok, string()} | {error, any()}
%% @doc Returns the textual definition of the given record or macro.
cat_recmac(_MCB, ModFile, RecMac)->
    try
        File = get_filenode(ModFile),
        getrecmactext(File, RecMac)
    catch
        Err = {_,_,_}-> ?ERR(Err);
        E -> ?LocalErr(E, [])
    end.

%% @spec syn_leaves(#msg_cb{}, node()) -> [node()]
%% @doc Returns the leaves of the syntactical subtree that starts at `Node',
%% in the correct syntactical order.
%% @see refcore_syntax:leaves/1
syn_leaves(_MCB, Node)->
    ?Dat(?Syn:leaves(Node)).

%% @spec syn_last_leaf(#msg_cb{}, node()) -> [node()]
%% @doc Returns the last leaf of subtree
%% @see refcore_syntax:syn_last_leaf/1
syn_last_leaf(_MCB, Node)->
    LLFun = ?Syn:last_leaf(),
    ?Dat(LLFun(Node)).

%% @spec syn_first_leaf(#msg_cb{}, node()) -> [node()]
%% @doc Returns the first leaf of subtree
%% @see refcore_syntax:syn_first_leaf/1
syn_first_leaf(_MCB, Node)->
    FLFun = ?Syn:first_leaf(),
    ?Dat(FLFun(Node)).

%% @spec syn_index(#msg_cb{}, node(), atom(), node()) -> integer()
%% @doc Returns the index of a link.
%% @see refcore_syntax:index/3
syn_index(_MCB, Parent, Link, Child)->
    ?Dat(?Syn:index(Parent, Link, Child)).
    
%% @spec syn_class(#msg_cb{}, node()) -> atom()
%% @doc Returns the class of a node.
%% @see refcore_syntax:class/1
syn_class(_MCB, Node)->
    ?Dat(?Syn:class(Node)).
    
%% @spec syn_tree_text(#msg_cb{}, node()) -> Chars
%%       Chars = [char() | Chars]
%% @doc Returns the tree text of a node.
%% @see refcore_syntax:tree_text/1
syn_tree_text(_MCB, Node)->
    ?Dat(?Syn:tree_text(Node)).

%% @spec syn_node_type(#msg_cb{}, node()) -> atom()
%% @doc Returns the node type
%% @see refcore_syntax:node_type/1
syn_node_type(_MCB, Node)->
    ?Dat(?Syn:node_type(Node)).

%%% ============================================================================
%%% HTML generation
%% @spec html_generate(#msg_cb{}, string()) -> string()
%% @doc Returns a html text with links on specific nodes.
html_generate(_MCB, Filename)->
    ?Dat(referl_htmlgen:generate(Filename)).

% @todo doc!
html_generate_node(_MCB, Node, Window)->
    ?Dat(referl_htmlgen:generate_node(Node, Window)).

%%% ============================================================================
%%% Graph querying
%% @spec graph_query(#msg_cb{}, atom(), atom(), list()) -> {ok, any()} | {error, any()}
%% @doc Returns the result of the given query. Allowed types of the queries can
%% be found in ?MODULE:controlled_graph_query/3.
graph_query(_MCB, Mod, Fun, Args)->
    ?Dat(controlled_graph_query(Mod, Fun, Args)).

%% @spec graph_data(#msg_cb{}, node()) -> {ok, any()} | {error, any()}
%% @doc Returns the data of the given node.
%% @see refcore_graph:data/1
graph_data(_MCB, Node)->
    ?Dat(?Graph:data(Node)).
    
%% @spec form_length(#msg_cb{}, node()) -> {ok, any()} | {error, any()}
%% @doc Returns the form length (the way we calculate it depends on the current
%% positioning mode)  
form_length(_MCB, Form) ->
    ?Dat(?Token:form_length(Form)).
    
%%% ============================================================================
%%% Transformation interface

-define(SQ,refusr_sq).
-define(MQ,refusr_metrics).
-define(CL,refcl_main).


%% @spec transform(#msg_cb{}, atom(), proplist()) -> ok
%% @doc Initiates a transformation and waits for it to end.
%% Returns the result of the said transformation.
%% `Args'  is a proplist that contains the arguments of the transformation.
%% If it is a real transformation (not a query/clustering),
%% saves the database upon its succesful execution. If the execution fails then 
%% rolls back the changes.
%% @see reflib_transform
%% @see reflib_args
transform(MCB, Action, Args) ->
    Mod = action_module_name(Action),
    case is_transformation(Mod) of
	%% this creates a hidden backup used exclusively for undo, except for
	%% mnesia, since that can have only one checkpoint at a time
	true -> ok = ?Graph:save(before_transformation),
                real_transform(MCB, Mod, Args);
        false -> transform2(MCB, Mod, Args)
    end.

transform2(MCB, Mod, Args) ->
    try
        ?Dat(?Transform:do(MCB, Mod, Args))
    catch
        throw:_ ->
            ?LErr(trfail);
        error:_ ->
            ?LErr(trfail)
    end.


real_transform(MCB, Mod, Args) ->
    try
        TrRes = ?Transform:do(MCB, Mod, Args),
        case is_success(TrRes) of
            true -> save_database(ok);
            false ->  is_tuple(TrRes) andalso 
                          element(1, TrRes) == error andalso 
                          rollback_bad_transformation(MCB)
        end,
        ?Dat(TrRes)
    catch
        throw:_ ->
            rollback_bad_transformation(MCB),
            ?LErr(trfail);
        error:_ ->
            rollback_bad_transformation(MCB),
            ?LErr(trfail)
    end.

rollback_bad_transformation(MCB)->
    %% undo does the same as a backup loading and it is used
    %% to roll back any changes since the save(before_transform)
    load(MCB, undo, [], true).

%% Returns whether the module name designates a transformation
%% (as opposed to a query or clustering).
is_transformation(?MQ) -> false;
is_transformation(?SQ) -> false;
is_transformation(?CL) -> false;
is_transformation(_)   -> true.

%% Returns whether the transformation result indicates success.
is_success({abort, _}) -> false;
is_success({deny, _})  -> false;
is_success({error, _}) -> false;
is_success(_)          -> true.

%% Returns the module name for an action (refactoring, query or clustering).
action_module_name(metric_query)   -> ?MQ;
action_module_name(semantic_query) -> ?SQ;
action_module_name(clustering)     -> ?CL;
action_module_name(Refac)          -> list_to_atom("reftr_"++atom_to_list(Refac)).


%% @spec reply(#msg_cb{}, integer(), term()) -> ok
%% @doc Provides a reply to a previously asked question.
%% @see reflib_transform:reply/2
reply(_MCB, Id, Reply) ->
    noreply = ?Transform:reply(Id, Reply),
    ?NoReply.

%% @spec cancel(#msg_cb{}, integer()) -> ok
%% @doc Cancels a previously asked question.
%% @see reflib_transform:cancel/1
cancel(_MCB, Id) ->
    noreply = ?Transform:cancel(Id),
    ?NoReply.


%%% ----------------------------------------------------------------------------
%%% Movings

%% @spec funlist(#msg_cb{}, path()) -> ok
%% @doc Requests the list of functions defined in `File'. The result is
%% returned in a message of type `funlist'.
%% @todo error handling?!
funlist(_MCB, File) ->
    ?Dat([{?Fun:name(F), ?Fun:arity(F)} ||
             F <- ?Query:exec(
                     ?Query:seq([?File:find(File),
                                 ?File:module(),
                                 ?Mod:locals()]))]).

%% @spec recordlist(#msg_cb{}, path()) -> ok
%% @doc Requests the list of records defined in `File'. The result is
%% returned in a message of type `recordlist'.
recordlist(_MCB, File) ->
    ?Dat([?Rec:name(R) ||
             R <- ?Query:exec(
                     ?Query:seq(?File:find(File), ?File:records()))]).

%% @spec macrolist(#msg_cb{}, path()) -> ok
%% @doc Requests the list of macros defined in `File'. The result is
%% returned in a message of type `macrolist'.
macrolist(_MCB, File) ->
    ?Dat([?Macro:name(M) ||
             M <- ?Query:exec(
                     ?Query:seq(?File:find(File), ?File:macros()))]).

%%% ----------------------------------------------------------------------------
%%% Clustering

%% @spec cl_options(#msg_cb{}, atom()) -> ok
%% @doc Requests the options of the given clustering algorithm.
cl_options(_MCB, Alg) ->
    OptList = cl_ui:cl_options(Alg),
    Ls = [[A,B] || {A,B} <- OptList],
    ?Dat([Alg]++Ls).

%% @spec cl_refresh(#msg_cb{}) -> ok
%% @doc Refreshes the Emacs clustering interface.
cl_refresh(_MCB) ->
    case cl_ui:refresh() of
        {cl_ui, recreated} ->
            ?OK;
        _ ->
            ?LErr(cl_ui_refresh) %@todo
    end.
% "cl_ui refresh: cleaned"

%% @spec run_cl(#msg_cb{}, proplist(), atom(), atom()) -> ok
%% @doc Invokes {@link cl_ui:run/1}. See the options there.
run_cl(_MCB, Opt, Alg, Create) ->
    {Clustering, FittNum} = cl_ui:run({Opt, Alg, Create}),
    ?Dat([{result,Clustering},
          {flist,?MISC:format("~w",[FittNum])}]).
% "Clustering algorithm finished."

cl_prev_clustering(_MCB) ->
   ?Dat(cl_interface:load_previous_clustering()).

%%% ============================================================================
%%% File Cache Server

generate_all(_MCB, Directory) ->
    F = fun(ActionFun) ->
            recurse_erl(_MCB, Directory, ActionFun) end,
    ?Dat(?FileCacheMan:generate_all(F)).

%%% ============================================================================
%%% Backup

backup(_MCB, CommitLog) ->
    ?Graph:backup(CommitLog).

ls_backups(_MCB) ->
    ?Dat(?Graph:ls_backups()).

backup_info(_MCB, Backup) ->
    Result = ?Graph:backup_info(Backup),
    case {is_tuple(Result), size(Result)} of
        {true, 3} ->
            ?Dat(Result);
        _ ->
            {Error, _} = Result,
            ?LocalErr(Error, [Backup])
    end.

restore(MCB, Backup) ->
    load(MCB, restore, [Backup], false).

undo(MCB, _File) ->
    load(MCB, undo, []).

load(MCB, Fun, Args)->
    load(MCB, Fun, Args, true).

load(MCB, Fun, Args, WriteToDisk) ->
    send_reset(MCB),
    FilesBefore = files_with_lastmod(),
    try apply(?Graph, Fun, Args) of
        ok  ->
            FilesAfter = files_with_lastmod(),
            ModifiedFiles =
                [ begin
                      case WriteToDisk of
                          true -> ok = ?FileMan:save_file(File);
                          false -> ok
                      end,
                      ?File:path(File) end ||
                    {File, LastMod} <- FilesAfter,
                    lists:keysearch(File, 1, FilesBefore) =/= LastMod ],
            case WriteToDisk of
                true -> send_change(MCB,[?Modified(F) || F <- ModifiedFiles]);
                false -> send_difference(MCB, [F || F <- ModifiedFiles])
            end,
            ?OK;
        {Error, _} ->
            ?LocalErr(Error, Args);
        Error ->
            ?LocalErr(Error, Args)
    catch
        throw:Msg -> ?LErr(Msg)
    end.


create_graph(_MCB, Name) ->
    graph_op(create_graph, [Name]).

rename_graph(_MCB, OldName, NewName) ->
    graph_op(rename_graph, [OldName, NewName]).

ls_graphs(_MCB) ->
    graph_op(ls_graphs, []).

actual_graph(_MCB) ->
    graph_op(actual_graph, []).

load_graph(MCB, Name) ->
    send_reset(MCB),
    graph_op(load_graph, [Name]).

delete_graph(_MCB, Name) ->
    graph_op(delete_graph, [Name]).

delete_all_graphs(MCB) ->
    send_reset(MCB),
    graph_op(delete_all_graphs, []).

graph_op(FunAtom, Args) ->
    case apply(?Graph, FunAtom, Args) of
        ok ->
            ?OK;
        Result when is_list(Result) or is_atom(Result) ->
            ?Dat(Result);
        Error when is_tuple(Error) ->
            ?LocalErr(element(1, Error), Args);
        Error ->
            ?LocalErr(Error, Args)
    end.

%%% ----------------------------------------------------------------------------
%%% Attributes of the error form

%% @spec decode_error_form(#file{}, ErrList) -> ok
%% @doc This function can collect information about error forms.
decode_error_form(FileNode, ErrorForms) when is_list(ErrorForms) ->
    [decode_error_form(FileNode, ErrorForm) || ErrorForm <- ErrorForms];
decode_error_form(FileNode, ErrorForm) ->
    #form{tag=Tag} = ?Graph:data(ErrorForm),
    case Tag of
        Err = {no_include_file, _MissingInc} ->
            [Err];
        {include_error,_Includer, Err = {no_include_file,_Included}} ->
            [Err];
        {1, ErrTxt} when is_list(ErrTxt) ->
            ParseErrorMsg =
                re:replace(ErrTxt, "^[^:]+:([0-9:\\-]+) (Parse error before .+)",
                           "\\1 \\2", [{return, list}]),
            case ParseErrorMsg == ErrTxt of
                false -> [ParseErrorMsg];
                true  -> decode_possible_token_error(FileNode, Tag)
            end;
        _ ->
            decode_possible_token_error(FileNode, Tag)
    end.

decode_possible_token_error(FileNode, Tag) ->
    Token = paarse(Tag),
    {Position, Text} =
        case Token of
            [] ->
                N = ?File:length(FileNode),
                {{N,N}, "EOF"};
            _ ->
               {?Token:pos(Token), ?Token:text(Token)}
        end,
    [{nexttokentext, Text}, {position, Position}].


%%% ----------------------------------------------------------------------------
%%% Handling file status information

%% @spec status_info(#msg_cb{}, [string()]) -> ok
%%
%% @doc This function collets information about files loaded in the
%%      RefactorErl database.
%%      Parameter of the function: `[]' means that the function find all
%%      files form the database.
%%      When the parameter is a list of the file paths or it is one file path
%%      in a list, the function is collecting information about these files
%%      only.
%%      The message of the function is a `FileStatusList' that is a
%%      [proplist()] contains information about the file(s).
%%
%%      Elements of the `FileStatusList':
%%
%%      {file,string()}          The path and the name of the file
%%      {error,Errors}           The file contains errors: list @todo
%%      {type,atom()}            The type of the file:
%%                               `none'|`module'|`include'
%%      {lastmod,int()|atom()}   The last modification of th file. The
%%                               default value is `undefined'
%%      {present,boolean()}      Status of the file:  @todo
status_info(_MCB, FileList) ->
    ?Dat(file_status_info(FileList)).


%%% ----------------------------------------------------------------------------
%%% Handling enviromental variables

%% @spec get_envs(#msg_cb{}) -> {ok, [{atom(), [any()]}]}
%% @doc Returns the user-level environment settings (appbase, output etc.) 
%% as a proplist.
get_envs(_MCB) ->
    ?Dat([ Env 
         || { Key, _ } = Env <- ?Syn:get_envs(), 
            lists:member(Key, user_level_envs_keylist())]).

%% @spec get_env(#msg_cb{}, atom()) -> {ok, [any()]}
%% @doc Returns the value of the user-level environment settings 
%% (appbase, output etc.) as a list.
get_env(_MCB, Name) when is_atom(Name)->
    case lists:member(Name, user_level_envs_keylist()) of
        true ->
            ?Dat(?Syn:get_env(Name));
        false ->
            throw(?LocalError(env_denied, [Name]))
    end.

%% @spec add_env(#msg_cb{}, atom(), any()) -> {ok, ok}
%% @doc Adds a new user-level environment node if it does not already exists.
%% @see refcore_syntax:add_env/2
add_env(_MCB, Name, Value) when is_atom(Name)->
    case lists:member(Name, user_level_envs_keylist()) of
        true ->
            ?Dat(?Syn:add_env(Name, Value));
        false ->
            throw(?LocalError(env_denied, [Name]))
    end.

%% @spec del_env(#msg_cb{}, atom()) -> {ok, [any()]}
%% @doc Deletes user-level enviromental node, which have the given name.
%% @see refcore_syntax:del_env/1
del_env(_MCB, Name) when is_atom(Name)->
    case lists:member(Name, user_level_envs_keylist()) of
        true ->
            ?Dat(?Syn:del_env(Name));
        false ->
            throw(?LocalError(env_denied, [Name]))
    end.

%% @spec del_env_val(#msg_cb{}, atom(), any()) -> {ok, [any()]}
%% @doc Deletes user-level environment node entries of name `Name' 
%% which are associated with the value `Value'.
%% @see refcore_syntax:del_env_val/2
del_env_val(_MCB, Name, Value) when is_atom(Name)->
    case lists:member(Name, user_level_envs_keylist()) of
        true ->
            ?Dat(?Syn:del_env_val(Name, Value));
        false ->
            throw(?LocalError(env_denied, [Name]))
    end.
    
%% @spec set_env(#msg_cb{}, atom(), any()) -> {ok, ok}
%% @doc Sets the value of a user-level environment node.
%% @see refcore_syntax:set_env/2
set_env(_MCB, Name, Value) when is_atom(Name)->
    case lists:member(Name, user_level_envs_keylist()) of
        true ->
            ?Dat(?Syn:set_env(Name, Value));
        false ->
            throw(?LocalError(env_denied, [Name]))
    end.

user_level_envs_keylist()->
    [output,  % where to write
     appbase, % appbase paths
     include, % include paths
     def,     % environmentally set macro definitions for a macro name
     env_var  % the environment var. of the path @see refcore_preproc:real_path/1
     ].

%%% ----------------------------------------------------------------------------
%%% Smart graph

%% @spec generate_smart_graph(#msg_cb{}, Proplist)-> {ok, Result} | {error, Error :: any()}
%% Result = {result, JSCode::string() } | {result, HTMLPath::string()}
%% @doc Generate the JS code of the smart graph or the complete HTML page of the 
%% smart graph.
generate_smart_graph(_MCB, Proplist)->
    ?Dat(refusr_smart_graph:generate(Proplist)).

%%% ----------------------------------------------------------------------------
%%% Directory sorting

%% @doc Sorts every module according to their consisting directories.
%% @spec dir_sort(#msg_cb{}) -> {ok, Sorted | Error}
%% Sorted = [{Directory, Modules}]
%% Directory = string()
%% Modules = [atom()]
%% Error = {error, no_modules_in_database} | {error, no_list_given} | {error, bad_list}
dir_sort(_MCB)->
    ?Dat(refusr_dir_sort:sort()).

%% @spec dir_sort_get_dirs(#msg_cb{}, ParList)-> {ok,Sorted | Error}
%% Sorted = [{Directory, Modules}]
%% Directory = string()
%% Modules = [atom()]
%%
%% ParList = [Module]
%% Module = node() | atom()
%%
%% Error = {error, no_list_given}| {error, bad_list}
%% @doc Sorts the modules (which can be given either as nodes or with
%% their names as atoms) according to their directories. 
dir_sort_get_dirs(_MCB, ParList)->
    ?Dat(refusr_dir_sort:get_dirs(ParList)).

%% @spec dir_sort_get_mods(#msg_cb{},Options::proplists())-> [{DirPath, ModList}] | Error
%% DirPath = string()
%% ModList = [atom()]
%% Error = {error, no_directory_given} |
%%     {error, no_such_directory_in_the_database} | 
%%     {error, sorting_directories}
%% @doc Gives back the modules belonging to the given directory list.
dir_sort_get_mods(_MCB, Options)->
    ?Dat(refusr_dir_sort:get_mods(Options)).

%%% ----------------------------------------------------------------------------
%%% Function block anal.s

%% @spec refusr_fb_regexp_re(#msg_cb{},Options::proplists()) -> {ok, any()}
%% @doc Operates on functional blocks filtered by regular expressions.
refusr_fb_regexp_re(_MCB, Options)->
    ?Dat(refusr_fb_regexp:re(Options)).

%%% ----------------------------------------------------------------------------
%%% Interface layers

%% @spec if_layers_show_insults(#msg_cb{}, ModList, ArchList) -> {ok, any()}
%% ModList = [{LayerName::atom(), [Module::atom()]}]
%% ArchList = [{LayerName1::atom(), LayerName2::atom()}]
%% @doc The function checks wether in an architecture defined by the 
%% input are functions, that insult the hierarchy or not. 
%% The result will be in a human readable format.
if_layers_show_insults(_MCB, ModList, ArchList)->
    ?Dat(refusr_layer:show_insults(ModList, ArchList)).

%% @spec if_layers_check_layered_architecture(#msg_cb{}, ModList, ArchList) -> 
%% {ok, any()}
%% ModList = [{LayerName::atom(), [Module::atom()]}]
%% ArchList = [{LayerName1::atom(), LayerName2::atom()}]
%% @doc The function checks wether in an architecture defined by the 
%% input are functions, that insult the hierarchy or not.
if_layers_check_layered_architecture(_MCB, Modlist, ArchList)->
    ?Dat(refusr_layer:check_layered_architecture(Modlist, ArchList)).

%% @spec if_layers_draw(#msg_cb{}, LayerList::lists(), ArchList::lists()) -> 
%% {ok, any()}
%% LayerList = [{LayerName::atom(), [Paths::string()]}]
%% ArchList = [{LayerName1::atom(), LayerName2::atom()}]
%% @doc Visualisation of layers, representing group of layers and insulting functions.
if_layers_draw(_MCB, LayerList, ArchList)->
    ?Dat(refusr_layer:draw(LayerList, ArchList)).

%% @spec if_layers_draw(#msg_cb{}, LayerList::lists(), 
%%                      ArchList::lists(), DotName::string()) -> {ok, any()}
%% @doc Same as {@link if_layers_draw/3}, only the user can give the name of the
%%  generated .dot file.
if_layers_draw(_MCB, LayerList, ArchList, DotName)->
    ?Dat(refusr_layer:draw(LayerList, ArchList, DotName)).

%% @spec duplicated_code_const_var_diff(#msg_cb{}, node(), node()) -> {ok, any()}
%% @doc  Calculates the different variables and constants,
%% which are originated from the given nodes.
duplicated_code_const_var_diff(_MCB, Node1, Node2)->
    ?Dat(refusr_clone_identifierl_suffix_tree:const_var_diff(Node1, Node2)).

%% @spec get_all_dupcode_result(#msg_cb{}) -> {ok, any()}
%% @doc  Returns every stored dupcode result.
get_all_dupcode_result(_MCB) ->
    ?Dat(refusr_clone_identifierl_lib:get_results_dets()).

%% @spec get_dupcode_group(#msg_cb{}, atom(), integer(), atom()) -> {ok, any()}
%% @doc  Returns the Name stored dupcode result's
%%       GroupNumber-th group in Format format.
get_dupcode_group(_MCB, Name, GroupNumber, Format) ->
    ?Dat(refusr_clone_identifierl_lib:get_group_dets(Name, GroupNumber, Format)).
    

%% @spec get_dupcode_result(#msg_cb{}, atom(), atom(), atom()) -> {ok, any()}
%% @doc  Returns the Name stored dupcode result's
%% in Format format.
get_dupcode_result(_MCB, Name, Format, PosType)->
    try 
	    Result = refusr_clone_identifierl_lib:match_name_dets(Name, Format, PosType),
	    case Result of
		     {true, Res} -> ?Dat(Res);
		     _ -> ?Dat([])
	    end
    catch
        error:{badmatch, []} -> ?Dat([])
    end.

%% @spec get_algorithms(#msg_cb{}) -> {ok, any()}
%% @doc  Returns the implemented clone identifierl algorithms and their names.
get_algorithms(_MCB) ->
    ?Dat(refusr_clone_identifierl_lib:get_algorithms()).

%% @spec get_algorithm_data(#msg_cb{}, atom()) -> {ok, any()}
%% @doc  Returns every parameter used by the Algorithm clone idnetifierl algorithm.
get_algorithm_data(_MCB, Algorithm) when is_atom(Algorithm) ->
    ?Dat(refusr_clone_identifierl_lib:get_algorithm_data(Algorithm)).

%probably unused now
clone_identifierl_by_pos(_MCB, FilePath, Start, End, PosType, Format) ->
    Options = [{algorithm, matrix}, {positions,[{FilePath, Start, End}]},
               {postype, PosType}, {format, Format}],
    ?Dat(?CloneIdentifiErl:get_clones(Options)).

clone_identifierl_by_pos(_MCB, Algorithm, FilePath, Start, End, PosType, Format) ->
    Options = [{algorithm, Algorithm}, {positions,[{FilePath, Start, End}]},
               {postype, PosType}, {format, Format}],
    ?Dat(?CloneIdentifiErl:get_clones(Options)).

%% @spec clone_identifierl(#msg_cb{}, proplist()) -> {ok, any()}
%% Options = [Option]
%% Option = {files, string()} | {minlen, integer()} | 
%% {minnum, integer()} | {overlap, integer()} |
%% {output, Filename::string()} | {name, Name::atom()} |
%% {max_invlaid_seq_length, Isl :: integer()} |
%% {diff_limit, DiffLimit :: float()} | {metric, Metric :: atom()} |
%% {subject, Subject :: atom()} | {method, Method :: atom()} |
%% {algorithm, Alg :: atom()} | {unit, Unit :: atom()} |
%% {max_rank, integer()} | {cache, bool()} |
%% {enforce, bool()} | {format, atom()} | {output, atom()}
%% @doc Searches for syntactically identical or similar codes.
clone_identifierl(_MCB, Options)->
    ?Dat(?CloneIdentifiErl:get_clones(Options)).

%%% ============================================================================
%%% Private implementation

%% @doc Parser for the error messages
paarse({_,Mesg})->
    case
        re:run(Mesg, "{{.+},{.+}}", [{capture, first}]) of
        {match, [{F, L}]} ->
            SToken = string:substr(Mesg, F+1, L),
            {ok, STerm, _}= erl_scan:string(SToken++"."),
            {ok, Tken} = erl_parse:parse_term(STerm),
            {_,Token} = Tken, Token;
        _ -> []
    end;
paarse(_) ->
    [].

%%% ----------------------------------------------------------------------------
handle_emakefile(MCB, EmakefilePath, Cmd) ->
        case file:consult(EmakefilePath) of
        {ok, Apps} ->
            save_database(load_by_apps_conf(MCB, filename:dirname(EmakefilePath), Cmd, Apps));
        {error, enoent }->
            throw(?LocalError(not_found_emakefile, [EmakefilePath]));
        {error, Err }->
            throw(?LocalError(bad_emakefile, [EmakefilePath, file:format_error(Err)]))
    end.

load_by_apps_conf(MCB, EmakefilePath, Cmd, Apps)->
    load_by_apps_conf(MCB, EmakefilePath, Cmd, Apps, []).

load_by_apps_conf(_, _, _, [], Results)->
    unpack_results(Results);

load_by_apps_conf(MCB, EmakefilePath, Cmd, [{Mods = [M|_], Options}| Apps], Result0) 
  when is_atom(M) andalso is_list(Options)->
     ModNameFun = fun(AtomMod) when is_atom(AtomMod)->
                         atom_to_list(AtomMod)++".erl";
                     (Mod)->
                         Mod
                  end,
     ModStrs = [ ModNameFun(Mod) || Mod <- Mods],
    load_by_apps_conf(MCB, EmakefilePath, Cmd, [{ModStrs, Options}] ++ Apps, Result0);

load_by_apps_conf(MCB, EmakefilePath, Cmd, [{Module, Options}| Apps], Result0) 
  when is_atom(Module) andalso is_list(Options)->
    ModStr = atom_to_list(Module)++".erl",
    load_by_apps_conf(MCB, EmakefilePath, Cmd, [{ModStr, Options} | Apps], Result0);

load_by_apps_conf(MCB, EmakefilePath, Cmd, [{PathOrModList=[C|_], Options}| Apps], Result0) 
  when not is_atom(C) andalso is_list(Options) andalso is_atom(Cmd)->
    {FileList, DirForAppbaseCalc} = 
        determine_params_for_load(PathOrModList, EmakefilePath),
    % init
    ((Cmd =:= load) or (Cmd =:= all)) andalso
        begin
            set_dirs_appbase(MCB, DirForAppbaseCalc),
            handle_includes(MCB, set, EmakefilePath, Options),
            handle_macros(MCB, set, Options)
        end,
    % add it
    Result = case Cmd of
                 all ->
                     add_flat([recurse_erl(MCB, File, fun add_filedir/2) 
                              || File <- FileList]);
                 _ ->
                     {ok,[]}
             end,
    % remove conf
    ((Cmd =:= unload) or (Cmd =:= all)) andalso
        begin
            handle_includes(MCB, unset, EmakefilePath, Options),
            handle_macros(MCB, unset, Options)
        end,
    load_by_apps_conf(MCB, EmakefilePath, Cmd, Apps, Result0++[Result]).

determine_params_for_load(PathOrModList, EmakefilePath) 
  when is_list(hd(PathOrModList)) ->
    DeepFileList = [filelib:wildcard(filename:absname(File, EmakefilePath))
                    || File <- PathOrModList],
    FileList = [File || List<- DeepFileList, File <- List],
    Result = {FileList, EmakefilePath},
    case Result of
        {[],_} ->
            throw(?LocalError(not_found,[PathOrModList]));
%        {_,[]} ->
%            throw(?LocalError(not_found,[PathOrModList]));
        _ ->
            Result
    end;
determine_params_for_load(PathOrModList, EmakefilePath) ->
    case filelib:wildcard(filename:absname(PathOrModList, EmakefilePath)) of
        [] -> 
            throw(?LocalError(not_found,[PathOrModList]));
        [Directory] ->
            {[Directory], Directory};
        [File | _] ->
            Dir = filename:dirname(File),
            {[Dir], Dir}
    end.

set_dirs_appbase(MCB, Directory)->
    Splitted = filename:split(Directory),
    Appbase = 
        case lists:last(Splitted) of
            "src" ->
                filename:join(lists:sublist(Splitted,length(Splitted)-2));
            _ ->
                filename:join(lists:sublist(Splitted,length(Splitted)-1))
        end,
    add_env(MCB, appbase, Appbase).

handle_includes(MCB, set, EmakefilePath, Options)->
    [ add_env(MCB, include, filename:absname(Path, EmakefilePath)) 
        || {i, Path} <- Options];
handle_includes(MCB, unset, EmakefilePath, Options)->
    [ del_env_val(MCB, include, filename:absname(Path, EmakefilePath)) 
        || {i, Path} <- Options].

handle_macros(MCB, set, Options)->
    [ add_env(MCB, def, {?MISC:to_list(MacName), MacVal}) 
        || {d, MacName, MacVal} <- Options],
    [ add_env(MCB, def, ?MISC:to_list(MacName)) || {d, MacName} <- Options];
handle_macros(MCB, unset, Options)->
    [ del_env_val(MCB, def, {?MISC:to_list(MacName), MacVal}) 
        || {d, MacName, MacVal} <- Options],
    [ del_env_val(MCB, def, ?MISC:to_list(MacName)) || {d, MacName} <- Options].

add_filedir(MCB,File) ->
    case ?MISC:is_erl(File) of
        true  ->
            do_add_file(MCB,File);
        false ->
            true = ?MISC:is_beam(File),
            do_load_beam(MCB,File)
    end.

generate_filedir(_,File) ->
    case ?MISC:is_erl(File) of
        true  ->
            {referl_htmlserver:generate_call(File),ok};
        false ->
            {ok,error}
    end.

drop_filedir(MCB,File)->
    case file:list_dir(File) of
        {ok, FileNames} -> 
            [drop_dir(MCB, filename:join([File, Name])) || 
                Name <- FileNames ];
        {error, _} -> ?OK
    end.

do_add_file(MCB,FileName)->
    case ?FileMan:add_file(FileName,
                           [update, {progress, progress(MCB,add)}]) of
        {error, Reason} ->
            send_change(MCB,[?AddedError(FileName,[])]), %@todo
            ?LErr(Reason);
        {file, File} ->
            Nodes = ?Query:exec(File, ?File:includes()),
            Paths = [?File:path(FN) || FN <- Nodes],
            send_change(MCB,[?AddedCorrect(F) || F <- Paths]),
            ?Dat(Nodes)
    end.

do_drop_file(MCB,FileName)->
    case ?Query:exec(?File:find(FileName)) of
        [File] ->
            Drop =
                [?File:path(FN) || FN <- ?Query:exec(File, ?File:included())],
            ?FileMan:drop_file(File, [{progress, progress(MCB,drop)}]),
            referl_htmlserver:drop(FileName),
            send_change(MCB,[?Dropped(F) || F <- Drop]),
            ?OK;
        [] ->
            ?RefErr(file_not_present,[FileName])
    end.

%% @doc Traverse a complete directory recursively while doing the action
%% specified on all "*.erl" and "*.beam" files each folder contains.
recurse_erl(MCB,Start=[_|_], Action) when is_function(Action,2) ->
    Files = erl_beam(MCB,Start),
    FileNumber = length(Files),
    SendFun = MCB#msg_cb.unicast,
    IndexedFiles = lists:zip(Files,lists:seq(1,FileNumber)),
    _Result = [begin Res = Action(MCB,F),
                     SendFun(progress, {completed, FileNumber, Indx}),
                     Res     end || {F, Indx} <- IndexedFiles ].
% @todo
    %% case Result /= [] of
    %%     true ->
    %%         case lists:any(fun(X)->X end, Result) of
    %%             false ->
    %%                 message(status, "~s", [error_message({none_proc,[]})]);
    %%             true ->
    %%                 case lists:any(fun(X)->not X end, Result) of
    %%                     true ->
    %%                         message(status, "~s",
    %%                                 [error_message({some_proc,[]})]);
    %%                     false ->
    %%                         ok
    %%                 end
    %%         end;
    %%     false ->
    %%         ok
    %% end.

%% @doc Traverse a complete directory recursively to collect
%% all "*.erl" and "*.beam" files each folder contains.
erl_beam(_MCB,Start) ->
    Files = ?MISC:find_files(
           filename:absname(Start),
           ?MISC:'or'(fun ?MISC:is_erl/1, fun ?MISC:is_beam/1)),
    case Files of
    [] ->
        throw(?LocalError(not_found,[Start]));
    _ ->
        FileMod = [{filename:rootname(filename:basename(F)), F}
               || F <- Files],
        {Erl,Beam0} = lists:partition(
                fun({_M,F})-> ?MISC:is_erl(F) end, FileMod),
        {ErlMod,_ErlFile} = lists:unzip(Erl),
        Beam = ?MISC:pdel(ErlMod, Beam0),
        All = Erl ++ Beam,
        [F || {_,F} <- All]
    end.

do_load_beam(MCB,File) ->
    load_beam(MCB,File,filename:dirname(File),false).

add_flat(Results0)->
    Results = lists:flatten(Results0),
    case lists:all(fun({ok,_})->true; % ?OK, ?Dat
                      (_)->false end,
                   Results) of
        true->
            Unpack = [N || {ok,N} <- Results], % ?OK, ?Dat
            ?Dat(lists:flatten(Unpack));
        false->
            ?LErr(somebad)
    end.

unpack_results(Results) ->
    Unpack = [N || {ok,N} <- Results],
    ?Dat(lists:flatten(Unpack)).

%%% ----------------------------------------------------------------------------

progress(MCB,Op) ->
    fun
        (File, Percent, FormCount, FormMax, KBps) ->
            send_progress(MCB, Op, File, Percent, FormCount, FormMax, KBps)
    end.

%% @doc Filters can be given either by their atomic abbreviations
%%      or by numeric codes.
convert_filter([]) -> all;
convert_filter(Types) when is_list(Types) -> [convert_filter(T) || T <- Types];
convert_filter(Type) when is_atom(Type) -> Type;
convert_filter(2) -> sem;
convert_filter(3) -> ctx;
convert_filter(4) -> syn;
convert_filter(5) -> synlex;
convert_filter(6) -> lex;
convert_filter(7) -> inp;
convert_filter(8) -> not_lex;
convert_filter(_) -> all.


files_with_lastmod() ->
    [{File, LastMod}
     || File <- ?Query:exec([file]),
        LastMod <- [(?ESG:data(File))#file.lastmod]].

file_status_info([])->
    Files = ?Query:exec([file]),
    [file_stat(FileName, FileNode) ||
        FileNode <- Files, FileName <- [?File:path(FileNode)]];

file_status_info(FileList)-> %@todo
    lists:map(
      fun(FileName) ->
              case ?Query:exec(?File:find(FileName)) of
                  [] ->
                      ?NotPresent(FileName);
                  [FileNode] ->
                      file_stat(FileName,FileNode)
              end
      end,
      FileList).

file_stat(Path,FileNode)->
    Type = ?File:type(FileNode),
    #file{lastmod=LastMod} = ?Graph:data(FileNode),
    case ?Query:exec(FileNode, [{form, {type, '==', error}}]) of
        [] -> ?PresentCorrectTM(Path, Type, LastMod);
        E  -> ?PresentErrorTM(Path, Type, LastMod,
                              decode_error_form(FileNode,E))
    end.

get_filenode(ModFile) ->
    File = mod2file(ModFile),
    ?Query:exec1(?File:find(File),?RefError(file_not_present,[File])).

mod2file([]) ->
    throw(?LocalError(no_file,[]));
mod2file(File) when is_list(File) ->
    filename:absname(File);
mod2file(Mod) when is_atom(Mod) ->
    FileNodes =
        ?Query:exec(?Query:seq([
            ?Mod:find(Mod),
            ?Mod:file() ])),
    case FileNodes of
        [File] ->
            ?File:path(File);
        [] ->
            throw(?RefError(mod_not_found, [Mod]));
        [_|_] ->
            throw(?RefError(ambiguous_mod, [Mod]));
        X      -> throw(?LocalError(internal_unexpected, [X]))
    end.

get_modnode(ModFile) ->
    Mod = file2mod(ModFile),
    ?Query:exec1(?Mod:find(Mod),?RefError(mod_not_found,[Mod])).

file2mod([]) ->
    throw(?LocalError(no_file,[]));
file2mod(Mod) when is_atom(Mod) ->
    Mod;
file2mod(Fil) when is_list(Fil) ->
    File = filename:absname(Fil),
    FileNode = ?Query:exec1(?File:find(File),
                            ?RefError(file_not_present, [File])),
    ModNode  = ?Query:exec1([FileNode], ?File:module(),
                            ?RefError(file_not_module,  [File])),
    ?Mod:name(ModNode).

getrecmactext(File, RecMac)->
    QE   = fun(L)-> ?Query:exec([File], ?Query:seq(L)) end,
    Obj  =
        case QE([?Rec:find(RecMac),?Rec:form()]) of
             [] ->
                 M = QE([?Macro:find(RecMac)]),
                 case (M==[]) and (is_atom(RecMac)) of
                     true  -> QE([?Macro:find(atom_to_list(RecMac))]);
                     %notice the difference between
                     %atom_to_list('PA') and io_lib:write('PA')
                     false -> M
                 end;
             R  -> R
        end,
    case Obj of
        []  -> throw(?RefError(mac_not_found,[RecMac]));
        [O] -> ?Syn:tree_text(O)
    end.

filelist_parallel()->
    Files = ?Query:exec([file]),
    ReceiverPid = produce_receiver(self(), length(Files)),
    [spawn(fun()->
           FilePath = ?File:path(FileNode),
           Result = 
              case ?Query:exec(FileNode, ?File:error_forms()) of
              [] -> 
                  ?PresentCorrect(FilePath);
              E  -> 
                  ?PresentError(FilePath,
                        decode_error_form(FileNode,E))
              end,
           ReceiverPid ! {worker_result, Result} 
        end) || FileNode <- Files ],
    receive
        {receiver_finished, R} ->?Dat(R)
    end.

produce_receiver(ParentPid, CWorker) when is_pid(ParentPid) ->
    spawn(fun()->
                  Result = receive_loop(CWorker, []),
                  ParentPid ! {receiver_finished, Result}
          end).

receive_loop(0, Acc)->
    Acc;
receive_loop(CWorker, AccIn) when is_integer(CWorker) andalso CWorker>0 ->
    Result = receive
        {worker_result, R} -> R
    end,
    receive_loop(CWorker-1, AccIn ++ [Result]).

%@todo: sure?
controlled_graph_query(?Query, Fun, Args)->
    apply(?Query, Fun, Args);
%@todo: sure?
controlled_graph_query(?Args, Fun, Args)->
    apply(?Args, Fun, Args);
controlled_graph_query(?File, Fun, Args)->
    apply(?File, Fun, Args);
controlled_graph_query(?Form, Fun, Args)->
    apply(?Form, Fun, Args);
controlled_graph_query(?Clause, Fun, Args)->
    apply(?Clause, Fun, Args);
controlled_graph_query(?Expr, Fun, Args)->
    apply(?Expr, Fun, Args);
controlled_graph_query(?Mod, Fun, Args)->
    apply(?Mod, Fun, Args);
controlled_graph_query(?Fun, Fun, Args)->
    apply(?Fun, Fun, Args);
controlled_graph_query(?Dynfun, Fun, Args)->
    apply(?Dynfun, Fun, Args);
controlled_graph_query(?Var, Fun, Args)->
    apply(?Var, Fun, Args);
controlled_graph_query(?Rec, Fun, Args)->
    apply(?Rec, Fun, Args);
controlled_graph_query(?RecField, Fun, Args)->
    apply(?RecField, Fun, Args);
controlled_graph_query(?Macro, Fun, Args)->
    apply(?Macro, Fun, Args);
controlled_graph_query(?Token, Fun, Args)->
    apply(?Token, Fun, Args);
controlled_graph_query(_,_,_)->
    throw(?LocalError(graph_query_denied, [])).


save_database(Result)->
    ?Graph:save(database),
    Result.

reload_if_modified(MCB, {FilePath, LastModDB})->
    case file:read_file_info(FilePath) of
        {ok, #file_info{mtime = LstMdFLoc}} ->
            LastModFile=hd(calendar:local_time_to_universal_time_dst(LstMdFLoc)),
            case compare_universal_datetimes(LastModFile, LastModDB) of
                1 -> do_add_file(MCB, FilePath);
                _ -> nothing_todo
            end;
        _  -> not_loaded
    end.
       
compare_universal_datetimes(Date1, Date2)->
    GS1 = calendar:datetime_to_gregorian_seconds(Date1),
    GS2 = calendar:datetime_to_gregorian_seconds(Date2),
    case GS1 == GS2 of
        true -> 0;
        false -> case GS1 < GS2 of
                    true -> -1;
                    false -> 1
                 end
   end.

%%% ============================================================================

metric_mode(_MCB) -> ?Dat(metricmode_()).
metric_mode(_MCB, Enable) -> metric_mode(_MCB, Enable, nostate).
metric_mode(_MCB, Enable, State) ->
    Res = case Enable of
        true ->  metricmode_(on, State);
        false -> metricmode_(off, State);
        show ->  not metricmode_() andalso metricmode_(on, State),
                 all_metric_errors(State)
    end,
    ?Dat(Res).

%%% ============================================================================

update_skeleton(_MCB, Name=[A|_],NewBody=[B|_],Owner=[C|_]) when is_integer(A) 
    andalso is_integer(B) andalso is_integer(C)  ->
    ?Dat(refusr_skeletons:update_skeleton(Name, NewBody, Owner)).

update_prev_skeleton_comment(_MCB, Name=[A|_], NewComment) when is_integer(A)
    andalso is_list(NewComment)->
    ?Dat(refusr_skeletons:update_prev_skeleton_comment(Name, NewComment)).

delete_skeleton(_MCB, Name)  ->
    ?Dat(refusr_skeletons:delete_skeleton(Name)).         

skeleton_call_format(_MCB, Name=[C|_]) when is_integer(C) ->
    ?Dat(refusr_skeletons:skeleton_call_format(Name)).

save_skeleton(_MCB, Name=[A|_], Body=[B|_], Owner=[C|_]) when is_integer(A)
    andalso is_integer(B) andalso is_integer(C) ->
    ?Dat(refusr_skeletons:save_skeleton(Name, Body, Owner)).

list_skeletons(_MCB) ->
    ?Dat(refusr_skeletons:list_skeletons()).         

evaluate_skeleton(_MCB, Name=[A|_], Parameters, User=[B|_]) when is_integer(A) 
  andalso is_integer(B) andalso is_list(Parameters)->
    ?Dat(refusr_skeletons:evaluate_skeleton(Name, Parameters, User)).

evaluate_skeleton(_MCB, Name=[A|_], Parameters, User, onlyconvert)
     when is_integer(A) andalso is_list(Parameters)->
    ?Dat(refusr_skeletons:evaluate_skeleton(Name, Parameters, 
                                                User, onlyconvert)).

try_parse_skeleton(_MCB, {Call, _File, _Pos}, User)  ->
    ?Dat(refusr_skeletons:try_parse_skeleton({Call, _File, _Pos}, User)). 
        
determine_sq_request_type(_MCB, Call=[C|_]) when is_integer(C)->
    ?Dat(refusr_skeletons:determine_sq_request_type(Call)).

do_autocomplete_skeleton(_MCB, Call)  ->
    ?Dat(refusr_skeletons:do_autocomplete_skeleton(Call)).

metricmode_() -> refanal_metrics_helper:metricmode().
metricmode_(Enable, nostate) -> refanal_metrics_helper:metricmode(Enable);
metricmode_(Enable, State) -> refanal_metrics_helper:metricmode(Enable, State).
all_metric_errors(nostate) -> refanal_metrics_helper:all_metric_errors();
all_metric_errors(State) -> refanal_metrics_helper:all_metric_errors(State).

get_file_from_server(_MCB, Path) ->
    case file:read_file(Path) of
        {ok, Binary} -> ?Dat(Binary);
        {error, Reason} -> ?ERR(Reason)
    end.

start_wx(_MCB) ->
    ?Dat(can_be_started).

start_wx(_MCB, _UserName) ->
    ?Dat(can_be_started).

%% This function returns the containing clause/form of a node
%% and some additional information which may be needed when running
%% investigations:
%% {FCNode :: node(),   StartPos :: integer(), 
%%  FCNodePos :: pos(), NodePos :: pos()}
%% FCNode is a form or a clause (clause, if available)
%% StartPos is a scalar, start pos. of the cl/form minus the length
%%  of its prewhitespace
%% FCNodePos is a {ScalarPos, LCPos} tuple with the begin/end position
%%  of the function/clause node
%% NodePos is a {ScalarPos, LCPos} tuple with the begin/end position
%%  of the argument node

get_container(_MCB, {_,file,_}=Node) -> %% It returns the first form
    FirstToken=hd(?Syn:leaves(Node)),
    ND = hd(?Query:exec(FirstToken, ?Token:form())),
    ?Dat({ND, start_pos(ND), node_pos(ND), node_pos(Node)});

get_container(_MCB, {_,form,_}=Node) ->
    P = node_pos(Node),
    ?Dat({Node, start_pos(Node), P, P});

get_container(_MCB, {_,clause,_}=Node) ->
    P = node_pos(Node),
    ?Dat({Node, start_pos(Node), P, P});

get_container(_MCB, {_,typexp,_}=Node) -> %% tsub?
    N=hd(?Query:exec(Node,[{tattr,back}])),
    ?Dat({N, start_pos(Node), node_pos(N), node_pos(Node)});

get_container(_MCB, Node={_,field,_}) -> %% defs form
    ND = hd(?Query:exec(Node, ?RecField:form())),
    P = node_pos(ND),
    ?Dat({ND, start_pos(ND), P, P});

get_container(_MCB, Node={_,func,_}) ->
    ND = case ?Query:exec(Node, ?Fun:definition()) of
        [] -> hd(?Query:exec(Node, ?Fun:applications()));
        [H|_] -> H
    end,
    P = node_pos(ND),
    ?Dat({ND, start_pos(ND), P, P});

get_container(_MCB, Node={_,variable,_}) ->
    ND = hd(?Query:exec(Node, ?Var:clause())),
    P = node_pos(ND),
    ?Dat({ND, start_pos(ND), P, P});

get_container(_MCB, Node={_,lex,_}) ->
    ND = case ?Query:exec(Node, ?Token:clause()) of
                [] -> hd(?Query:exec(Node, ?Token:form()));
                [H|_] -> H
    end,
    ?Dat({ND, start_pos(ND), node_pos(ND), node_pos(Node)});

get_container(_MCB, Node={_,record,_}) -> %% defs form
    ND = hd(?Query:exec(Node, ?Rec:form())),
    P = node_pos(ND),
    ?Dat({ND, start_pos(ND), P, P});

get_container(_MCB, Node={_,expr,_}) ->
    try 
        PE=?Query:exec(Node,
                ?Query:any([
                    ?Query:seq([?Expr:clause(),?Clause:funcl()]),
                    ?Expr:attrib_form()])),
        ND =
        if
            PE==[] -> 
                PE2=?Query:exec(Node,
                        ?Query:seq([?Expr:nameof(),?Clause:form()])),
                if
                    PE2==[] -> 
                            FirstToken=hd(?Syn:leaves(Node)),
                            hd(?Query:exec(FirstToken, ?Token:form()));
                    true -> hd(PE2)
                end;
            true -> hd(PE)
        end,
        ?Dat({ND, start_pos(ND), node_pos(ND), node_pos(Node)})
    catch
        _:_ -> ?Dat({bad_node, no_pos, no_pos, no_pos})
    end;

get_container(_MCB, _) -> ?Dat({bad_node, no_pos, no_pos, no_pos}).

node_pos(_MCB, Node) ->
    [First] = (?Syn:first_leaf())(Node),
    [Last] = (?Syn:last_leaf())(Node),
    FNode = ?Query:exec(First, ?Token:file()),
    {{BegS, _}, {BegL, _}} = ?Token:pos(FNode, First, both),
    {{_, EndS}, {_, EndL}} = ?Token:pos(FNode, Last, both),
    ?Dat({{BegS, EndS}, {BegL, EndL}}).

node_pos(Node) -> 
    {ok, Res} = node_pos(dummy, Node),
    Res.

start_pos(_MCB, Node) ->
    [First] = (?Syn:first_leaf())(Node),
    {Beg, _}  = ?Token:pos(First),
    T = (?Graph:data(First))#lex.data,
    Pre = (T)#token.prews,
    ?Dat(Beg - length(Pre)).

start_pos(Node) -> 
    {ok, Res} = start_pos(dummy, Node),
    Res.

%% This function searches for the definition of a funnode
%% and returns its scalar begin/end position as a tuple
%% In case of error, it returns no_pos (if not a funnode)
fundef_pos(_MCB, Node) ->
    Res =
    case ?Query:exec(Node,?Fun:definition()) of
            [Def|_] ->  [First] = (?Syn:first_leaf())(Def),
                        [Last] = (?Syn:last_leaf())(Def),
                        {Beg, _}  = ?Token:pos(First),
                        {_, End}  = ?Token:pos(Last),
                        {Beg, End};
            []       -> no_pos;
            _        -> no_pos %% Maybe error message
        end,
    ?Dat(Res).

%% Converts a function node to the "canonical" M:F/A format
function_text(_MCB, FunNode) ->
    Res =
    atom_to_list(?Mod:name(hd(?Query:exec(FunNode,?Fun:module())))) 
     ++ ":"
     ++ atom_to_list(?Fun:name(FunNode)) ++ "/"
     ++ integer_to_list(?Fun:arity(FunNode)),
     ?Dat(Res).

%% Get node for a position in a file
definition(_MCB, FilePath, Pos) ->
    try 
        FileQuery =  ?File:find(FilePath),
        FileTokenFun = ?File:token(Pos),
        Token =
        case ?Query:exec(?Query:seq(FileQuery,FileTokenFun)) of
                [T | _]    -> T;
                Any        -> report_definition_error(Any, FileQuery)
        end,
        Entity =
            ?Query:exec(
               Token,
               ?Query:any(
                  [ [{llex,back}, mref],                             %macro ref
                    ?Query:seq(?Token:expr(), ?Expr:variables()),    %var
                    ?Query:seq(?Token:expr(), ?Expr:field()),        %field ref
                    ?Query:seq(?Token:expr(), ?Expr:record()),       %recref
                    ?Query:seq(?Token:typexp(), ?Expr:fielddef()),   %field def
                    ?Query:seq(?Token:expr(), ?Expr:module()),       %modq
                    ?Query:seq([?Token:expr(), ?Expr:parent(), ?Expr:function()]),
                    ?Query:seq([?Token:expr(), ?Expr:parent(),
                                ?Expr:parent(), ?Expr:function()]),  %funref infix
                    ?Query:seq([?Token:expr(), ?Expr:nameof(),
                                ?Clause:form(), ?Form:func()]),      %fundef
                    [{{flex,back}, {type, '==', macro}}],            %macro form
                    ?Query:seq(?Token:form(), ?Form:record()) ] ) ), %rec form
        Ent =
        case Entity of
                   []       -> none;
                   [Node|_] -> {Node, node_type(Node)};
                   Error    -> Error
        end,
        ?Dat(Ent)
    catch
        {error, Err} -> ?ERR(Err)
    end.

report_definition_error(ReturnValue, FileQuery) ->
    case {ReturnValue, FileQuery} of
         {{error, Error1}, _} -> throw({error, Error1});
         {[], []}             -> throw({error, "File not found in database"});
         {[], _}              -> throw({error, "The given position does not contain any token"});
         _                    -> throw({error, "Some internal error occured"})
    end.

%% Node type for predefined queries (in Wx) and for node definition
node_type(_MCB, Node) ->
    NodeData = ?Graph:data(Node),
    Type =
    case NodeData of
        #form{type=module}        -> mod;
        #form{type=record}        -> recdef;
        #typexp{type=spec_field}  -> recfielddef;
        #form{type=macro}         -> macrodef;
        #expr{type=application}   -> funappl;
        #expr{type=implicit_fun}  -> funappl;
        #expr{type=funref}        -> funappl;
        #clause{type=fundef}      -> fundef;
        #expr{type=variable}      -> variable;
        #expr{type=record_expr}   -> recexpr;
        #expr{type=record_field}  -> recfield;
        #expr{type=record_access} -> recexpr;
        #expr{type=record_update} -> recexpr;
        #expr{type=atom} ->
            Path=?Query:exec(Node,?Expr:fields()),
            if
                Path/=[]          -> recfield;
                true              -> atom
            end;
        #variable{}               -> variable;
        #module{}                 -> module;
        #record{}                 -> recdef;
        #field{}                  -> recfield;
        #func{}                   -> funappl;
        _                         -> none
    end,
    ?Dat(Type).

node_type(Node) -> 
    {ok, R} = node_type(dummy, Node),
    R.

act_graph(_MCB) ->
    ?Dat(?Graph:actual_graph()).

deptab_dets_insert(_MCB, Rec) ->
    ?Dat(referl_wx_logic:deptab_dets_insert_ui(Rec)).

deptab_match_opts(_MCB, Opts) ->
    ?Dat(referl_wx_logic:deptab_match_opts_ui(Opts)).

deptab_delete(_MCB, Opts) ->
    ?Dat(referl_wx_logic:deptab_delete_ui(Opts)).

deptab_delete_all(_MCB) ->
    ?Dat(referl_wx_logic:deptab_delete_all_ui()).

update_qtab_if_needed(_MCB, SafeT,Query,User,Result,Hash)->
    ?Dat(referl_wx_logic:update_qtab_if_needed_ui(SafeT,Query,
                                                User,Result,Hash)).

insert_to_qtab(_MCB, Rec) ->
    ?Dat(referl_wx_logic:insert_to_qtab_ui(Rec)).

insert_to_qtab(_MCB, SafeQuery={_,_,_}, Query, Res, Users, Hash) ->
    ?Dat(referl_wx_logic:insert_to_qtab_ui(SafeQuery, Query, Res, 
                                                      Users, Hash)).

find_in_qtab(_MCB, SafeQuery) ->
    ?Dat(referl_wx_logic:find_in_qtab_ui(SafeQuery)).

find_in_qtab_by_pattern(_MCB, Pattern)->
    ?Dat(referl_wx_logic:find_in_qtab_by_pattern_ui(Pattern)).

delete_from_qtab(_MCB, SafeQuery) ->
    ?Dat(referl_wx_logic:delete_from_qtab_ui(SafeQuery)).

insert_to_invtab(_MCB, Rec) ->
    ?Dat(referl_wx_logic:insert_to_invtab_ui(Rec)).

delete_from_invtab(_MCB, Name) ->
    ?Dat(referl_wx_logic:delete_from_invtab_ui(Name)).

get_from_invtab(_MCB, Name) ->
    ?Dat(referl_wx_logic:get_from_invtab_ui(Name)).

get_all_invtab_name_user_pairs(_MCB) ->
    ?Dat(referl_wx_logic:get_all_invtab_name_user_pairs_ui()).

%%% ============================================================================

deadcode(_MCB,Files) -> 
    ?Dat(refusr_deadcode:start(Files)).
deadcode(_MCB,Files,Mode) -> 
    ?Dat(refusr_deadcode:start(Files,Mode)).
deadcode_interface(_MCB,Include,Exclude,Mode) -> 
    ?Dat(refusr_deadcode:check_interface(Include,Exclude,Mode)).

function_positions(_MCB, Function)->
   [File, FirstToken, LastToken] = 
        ?Query:exec(Function, 
                    ?Query:seq(?Fun:definition(),
                                ?Query:all([ ?Form:file(),
                                             ?Syn:first_leaf(),
                                             ?Syn:last_leaf() ]))),
    GetPos = fun(Token)->?Token:get_both_pos(File, Token) end,
    ?Dat({File, GetPos(FirstToken), GetPos(LastToken)}).

%%% ============================================================================
draw_dep_graph(_MCB,PropList) ->
    ?Dat(refusr_dep_graph:draw(PropList)).
    
data_dir(_MCB) ->
	?Dat(?MISC:data_dir()).

do_depanal_and_transfer(_MCB, Arglist, Filename) ->
    DataDir = ?MISC:data_dir(),
    FullFilePath = filename:join([DataDir, Filename]),
    FullFilePathDOT = FullFilePath ++ ".dot",
    DepArglist = Arglist ++ [{file_path, FullFilePathDOT}],
    case apply(?MODULE, draw_dep_graph, [no_mcb, DepArglist]) of
        {ok, Data} -> case file:read_file(FullFilePathDOT) of
                        {ok, Binary}    -> ?Dat({Data, binary_to_list(Binary)});
                        {error, Reason} -> ?ERR(Reason)
                      end;
        E          -> E
    end.

%% @spec saveconfig(#msg_cb{}, [path()], [path()], path() | original) -> ok 
%% @doc Modifies the runtime configuration of the tool. 
%% <ul> 
%% <li>`App' is a list of directories that are searched for applications for 
%%   `include_lib' directives</li> 
%% <li>`Inc' is a list of directories that are searched for `include' 
%%   directives</li> 
%% <li>`Out' is the output directory of the tool. Its value can be `original', 
%%   which means files should be overwritten, or a name of a directory, which 
%%   means modified files should be saved there.</li> 
%% </ul> 
%% @todo If this option is still necessary, it should use ?Graph:save_envs/0. 
saveconfig(_MCB, AppDirs, IncDirs, OutDir) -> 
    ?Syn:del_envs(), 
    [?Syn:create_env(appbase, Dir) || Dir <- AppDirs], 
    [?Syn:create_env(include, Dir) || Dir <- IncDirs], 
    ?Syn:create_env(output, OutDir), 
    ?OK. 

