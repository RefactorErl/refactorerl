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

%%% @doc Graph storage server. This is a NIF based implementation of
%%% the semantical graph server interface.
%%%
%%% @author Peter Felker <felker.peter88@gmail.com>
%%% @author Andras Nemeth <andras.b.nemeth@ericsson.com>

-module(refdb_nif).
-vsn("$Rev: 12311 $"). %% "
-behaviour(refcore_gendb).

%%% ============================================================================
%%% imports

-include("core.hrl").
-include("refcore_gendb.hrl").

%%% ============================================================================
%%% Compiler options

-define(use_binary_wrapper, true).

%%% ============================================================================
%%% Exports

%% Client exports
-export([%start_link/1,
         init/1,
         code_change/2,
         terminate/2,
	 create_class/1,
	 erase_class/1,
	 is_parallel_exec_supported/0,
	 is_path_supported/0]).

-export([erase_nodes/0,
         root/0, create/1, create_prot/1, update/2, delete/1, data/1,
         mklink/3, mklink_prot/3, rmlink/3, links/1, path/2, index/3,
         set_prop/3, get_prop/2, get_props/1, del_prop/2,
         remove_garbage/0, back_links/1]).

-export([backup/0, backup/1, restore/1, ls_backups/0, backup_info/1,
         undo/0, redo/0, clean/0]).

-export([save/1, create_graph/1, rename_graph/2, ls_graphs/0, actual_graph/0,
         load_graph/1, delete_graph/1, delete_all_graphs/0]).

-export([int_path/3]).

% only for current nif compatibility
-export([is_protected_node/1,reset_schema/0]).

%%% ============================================================================
%%% Debug

% the following are temporarily moved here from core_export.hrl

%% Prettyprints the value of a single expression with module/line information.
% -define(u2(Name, X), begin io:format("~4w ~s~n ~12s: ~p~n", [?LINE, atom_to_list(?MODULE), Name, X]), ?Graph:sleep(), X end).
% -define(u(X), ?u2(??X, X)).

%%% ============================================================================
%%% Structures

%% @type rnode().
%%  Represents a node in the graph.

%% @type data() = tuple().
%%  Represents the class and attributes of a node. This is essentially a
%%  record, the name of the record (or the first element of the tuple) is
%%  the class name, and the fields are the attributes.

%% @type path() = [PathElem]
%%       PathElem = Tag | {Tag, Index} | {Tag, Filter} | {intersect, Node, Tag}
%%                | Union | {PathElem | path(), unique}
%%       Union = [PathElem | path()]
%%       Tag = atom() | {atom(), back}
%%       Index = integer() | last | {integer(), last} | {integer(), integer()}
%%       Filter = {Filter, and, Filter} | {Filter, or, Filter}
%%              | {not, Filter} | {Attrib, Op, term()}
%%       Attrib = atom()
%%       Op = '==' | '/=' | '=<' | '>=' | '<' | '>'.
%% Indexes start at 1. `{Start, End}' interval means indexes `Ind' that satisfy
%% `Start =< Ind < End'.

%% @type schema() = [ClassDef]
%%       ClassDef = {ClassName::atom(), [Attrib::atom()], [Link]}
%%       Link = {Tag::atom(), ClassName::atom()}.
%%  Describes the schema of the graph.

-record(dbState, {has_schema  :: boolean() | schema_error,
                  schema      :: tuple(),
                  schema_list :: list()}).

is_parallel_exec_supported() ->
    false.

is_path_supported() ->
    false.

%% @private
init(_InitArgs) ->
    case ?otpversion < 14 of
        true ->
            io:format("(probably) incompatible release ~p. At least R14B is required.", [erlang:system_info(otp_release)]),
            exit(-1);
        _ ->
            ok
    end,
    filelib:ensure_dir(filename:join(mnesia:system_info(directory), "dummy")),
    load_nif(application:get_env(?APPLICATION_NAME, nif_graph_nif_is_loaded)),
    {ok, #dbState{has_schema = false}}.

%% at schema reset, the db is re-initialized. But to be 'compatible' with the
%% current release of the ever-changing nif implementation, it have to be skipped
%% with checking 'nif_graph_nif_is_loaded' in the application envs, it is okay for now.
load_nif({ok, true}) ->
    ok;
load_nif(_) ->
    %process_flag(trap_exit, true),
    Nif = filename:join(code:lib_dir(?APPLICATION_NAME, priv), "nif_graph"),
    load_nif(Nif, get_datastore() ++ [mnesia:system_info(directory)]),
    application:set_env(?APPLICATION_NAME, nif_graph_nif_is_loaded, true).

%% @doc Tries to get back the graph and persistence object pointers in a list.
%% If something is wrong (connection with NIF has not been established, etc),
%% then returns [0, 0]. In the list, the first element is the graph pointer,
%% while the second one is the persistence object pointer. If all of these are 0,
%% then a new persistence and graph object will be created at C++ level.
get_datastore() ->
    try
        nif_get_datastore()
    catch
        _ -> [0, 0]
    end.

load_nif(Path, LoadInfo) ->
    try
        ok = erlang:load_nif(Path, LoadInfo)
    catch
        _:_ ->
            try
                ok = erlang:load_nif(Path, LoadInfo)
            catch
                _:_ ->
                    io:format("~n"
                              "Error: RefactorErl cannot find the nif_graph shared library!~n"
                              "Solutions:~n"
                              "-Try to rebuild the tool.~n"
                              "-Install a g++ compiler (at least g++ 4.3.6) and then rebuild the tool.~n"
                              " (It is important that your PATH environment has to contain g++)~n"
                              "-Add write permission to <path-to-RefactorErl>/lib/referl_core/priv/ folder~n"
                              " and then rebuild the tool.~n"),
                    halt()
            end,
            io:format("The last used graph has been loaded, "
                      "but with an empty database, because it may be crashed!~n")
    end.

%% @private

-define(HandleNifError(Instruction, St),
            try
                Instruction
            catch
                Error -> {result, Error, St}
            end).

%% handle_operation({get_schema}, _Extra,
%%                #dbState{schema_list = SchemaList} = State) ->
%%     {result, SchemaList, State};
%% handle_operation({schema, Schema}, _Extra, State) ->
%%     ?HandleNifError(handle_schema(Schema, State), State);
%% handle_operation({reset_schema}, _Extra, #dbState{schema = Schema} = State) ->
%%     ?HandleNifError(handle_reset_schema(Schema), State);
%% handle_operation(Request, _Extra, #dbState{has_schema = true,
%%                                          schema = Schema}
%%                = State) ->
%%     Result = handle_req(Request, Schema),
%%     {result, Result, State};
%% handle_operation(_Request, _Extra, no_schema) ->
%%     {stop, no_schema, no_schema, no_schema};
%% handle_operation(_Request, _Extra, #dbState{has_schema = schema_error} = State) ->
%%     {stop, schema_error, schema_error, State}.

code_change(_OldVsn, _State) ->
    no_code_change.

terminate(_Reason, _State) ->
    ok.

%% handle_req({root}, _Schema) ->
%%     handle_root();

%% handle_req({create, Data, Prot}, Schema) ->
%%     handle_create(Data, Prot, Schema);

%% handle_req({update, Node, Data}, _Schema) ->
%%     handle_update(Data, Node);

%% handle_req({delete, Node}, _Schema) ->
%%     handle_delete(Node);

%% handle_req({data,   Node}, _Schema) ->
%%     handle_data(Node);

%% handle_req({mklink, From, Tag, To, Prot}, Schema) ->
%%     handle_mklink(From, Tag, To, Prot, Schema);

%% handle_req({rmlink, From, Tag, To}, Schema) ->
%%     handle_rmlink(From, Tag, To, Schema);

%% handle_req({links,  Node}, Schema) ->
%%     handle_links(Node, Schema);

%% handle_req({back_links,  Node}, Schema) ->
%%     handle_back_links(Node, Schema);

%% handle_req({path,   Node, Path},  Schema) ->
%%     handle_path(Node, Path, Schema);

%% handle_req({index,  From, Tag, To},  Schema) ->
%%     handle_index(From, Tag, To, Schema);

%% handle_req({setp,   Node,Key,Val}, _Schema) ->
%%     handle_setp(Node, Key, Val);

%% handle_req({getp,   Node, Key}, _Schema) ->
%%     handle_getp(Node, Key);

%% handle_req({getp,   Node}, _Schema) ->
%%     handle_getp(Node);

%% handle_req({delp,   Node, Key}, _Schema) ->
%%     handle_delp(Node, Key);

%% handle_req({erase_nodes}, _Schema) ->
%%     handle_erase();

%% %handle_req({reset_schema},  Schema) ->
%% %    handle_reset_schema(Schema);

%% handle_req({is_protected_node, Node}, _Schema) ->
%%     handle_is_protected_node(Node);

%% handle_req({backup, CommitLog},  Schema) ->
%%     handle_backup(CommitLog, Schema);

%% handle_req({ls_backups}, _Schema) ->
%%     handle_ls_backups();

%% handle_req({backup_info, Backup}, _Schema) ->
%%     handle_backup_info(Backup);

%% handle_req({undo}, Schema) ->
%%     handle_undo(Schema);

%% handle_req({remove_garbage}, _Schema) ->
%%     handle_remove_garbage();

%% handle_req({clean}, _Schema) ->
%%     handle_clean();

%% handle_req({create_graph, Name}, _Schema) ->
%%     handle_create_graph(Name);

%% handle_req({rename_graph, OldName, NewName}, _Schema) ->
%%     handle_rename_graph(OldName, NewName);

%% handle_req({ls_graphs}, _Schema) ->
%%     handle_ls_graphs();

%% handle_req({actual_graph}, _Schema) ->
%%     handle_actual_graph();

%% handle_req({load_graph, Name}, _Schema) ->
%%     handle_load_graph(Name);

%% handle_req({delete_graph, Name}, _Schema) ->
%%     handle_delete_graph(Name);

%% handle_req({delete_all_graphs}, _Schema) ->
%%     handle_delete_all_graphs();

%% handle_req({save, FileName}, Schema) ->
%%     handle_save(FileName, Schema);

%% handle_req({restore, Backup}, Schema) ->
%%     handle_restore(Backup, Schema);

%% handle_req(_, _Schema) ->
%%     throw(request_not_implemented).


%%% ----------------------------------------------------------------------------
%%% Schema operations

erase_nodes() ->
    ok.

%% handle_reset_schema(Schema) ->
%%     save_envs(Schema),
%%     propagate_reset_to_servers(),
%%     ResetSchema = reset_schema(Schema),
%%     restore_envs(Schema),
%%     ResetSchema.

create_class(_Class) ->
    ok.

erase_class(root) -> %% cheat
    int_reset_schema();
erase_class(_Class) ->
    ok.

%%% ----------------------------------------------------------------------------
%%% Node operations

root() ->
    {ok, {?NODETAG, root, int_root()}}.

data({?NODETAG, Class, Id}) ->
    case int_data(Id) of
        {bad_node, _} ->
            {error, bad_node};
        Result ->
            case get_class(Result) =:= Class of
                true -> {ok, Result};
                false -> {error, bad_node}
            end
    end.

is_protected_node({?NODETAG, _Class, Id}) ->
    {ok, int_is_protected_node(Id)}.

create(Data) ->
    Class = get_class(Data),
    ?IS_VALID_CLASS(Class),
    {ok, {?NODETAG, Class, int_create(Data, false)}}.

create_prot(Data) ->
    Class = get_class(Data),
    ?IS_VALID_CLASS(Class),
    {ok, {?NODETAG, Class, int_create(Data, true)}}.

update({?NODETAG, NodeClass, NodeId}, Data) ->
    case get_class(Data) =:= NodeClass of
        true ->
            case int_update(NodeId, Data) of
                ok ->
                    ok;
                {protected_node, _Id} ->
                    {ok, protected_node};
                {bad_node, _Id} ->
                    {error, bad_node}
            end;
        false ->
            {error, {bad_class, NodeClass}}
    end.

delete({?NODETAG, _Class, Id}) ->
    case int_delete(Id) of
        ok ->
            ok;
        {bad_node, Id} ->
            ok;
        {protected_node, _Id} ->
            {ok, protected_node}
    end.

%%% ----------------------------------------------------------------------------
%%% Link modifications

mklink(FNode, TagInfo, TNode) ->
    mklink_c(FNode, TagInfo, TNode, false).

mklink_prot(FNode, TagInfo, TNode) ->
    mklink_c(FNode, TagInfo, TNode, true).

mklink_c(FNode = {?NODETAG, FClass, FId}, TagInfo,
	 TNode = {?NODETAG, _TClass, TId}, IsProtected) ->
    case TagInfo of
        {Tag, Idx} when is_atom(Tag) -> ok;
        Tag when is_atom(Tag)        -> Idx = last
    end,

    try
        _C = ?GET_NEXT_CLASS_FWD(FClass, Tag),
        case int_mklink(FId, Tag, Idx, TId, IsProtected) of
            ok ->
                ok;
            {bad_node, FId} ->
                {error, {bad_node, FNode}};
            {bad_node, TId} ->
                {error, {bad_node, TNode}}
        end
    catch
       throw:_ -> {error, {bad_link, FNode, TagInfo, TNode}}
    end.

rmlink(FNode = {?NODETAG, FClass, FId}, Tag,
       TNode = {?NODETAG, _TClass, TId}) ->
    try
        _C = ?GET_NEXT_CLASS_FWD(FClass, Tag),

        case int_rmlink(FId, Tag, TId) of
            ok ->
                ok;
            {protected_link, _, _, _} ->
                {ok, protected_link};
            _ ->
                {error, {not_exists, FNode, Tag, TNode}}
        end
    catch
        throw:_ -> {error, {bad_link, FNode, Tag, TNode}}
    end.

%%% ----------------------------------------------------------------------------
%%% Link queries

links({?NODETAG, _NodeClass, NodeId}) ->
    check_links(int_links(NodeId)).

back_links({?NODETAG, _NodeClass, NodeId}) ->
    check_links(int_back_links(NodeId)).

check_links(Links) ->
    case is_list(Links) of
        true ->
            NodeList = [{Tag, {?NODETAG, int_class(Id), Id}}
                        || {Tag, Id} <- lists:usort(Links)],
            {ok, NodeList};
        false ->
            {ok, []}
    end.

%% this part have moved to gendb. only 'int_path' is kept here
%% FIXME: gendb needs a more generic path!
%path(Node, Path) ->
    %throw({error, path_not_supported}).
    %?HANDLE_PATH_PLEASE(Node, Path).
path(Node = {?NODETAG, Class, Id}, Path) ->
    try
        case do_path(Path, [Id], Class) of
            {error, bad_node} ->
                {ok, []};
            {error, bad_node, _} ->
                {ok, []};
            Result ->
                {ok, [{?NODETAG, int_class(ResultId), ResultId}
                        || ResultId <- Result]}
        end
    catch
        error:badarg -> {ok, []};
        throw:{overreaching_path_idx,_,_} -> {ok, []};
        throw:Msg -> {error, {Msg, Node, Path}}
    end.

index(FNode = {?NODETAG, FClass, FId}, Tag,
      TNode = {?NODETAG, _TClass, TId}) ->
    try
        _C = ?GET_NEXT_CLASS_FWD(FClass, Tag),
        {ok, int_index(FId, Tag, TId)}
    catch
        throw:_ -> {error, {bad_link, FNode, Tag, TNode}}
    end.

%%% ----------------------------------------------------------------------------
%%% Persistence operations

backup() ->
    backup("DefaultCommitLog").

backup(CommitLog) ->
    {ok, {ok, int_backup(CommitLog)}}.

ls_backups() ->
    {ok, int_ls_backups()}.

backup_info(Backup) ->
    {ok, int_backup_info(Backup)}.

undo() ->
    {ok, int_undo()}.

redo() -> % FIXME
    throw({error, not_implemented}).

remove_garbage() ->
    {ok, int_remove_garbage()}.

clean() ->
    int_delete_all_backups().

create_graph(Name) ->
    {ok, int_create_graph(Name)}.

rename_graph(OldName, NewName) ->
    graph_op(fun int_rename_graph/2, [OldName, NewName]).

ls_graphs() ->
    graph_op(fun int_ls_graphs/0, []).

actual_graph() ->
    graph_op(fun int_actual_graph/0, []).

load_graph(Name) ->
    graph_op(fun int_load_graph/1, [Name]).

delete_graph(Name) ->
    graph_op(fun int_delete_graph/1, [Name]).

delete_all_graphs() ->
    graph_op(fun int_delete_all_graphs/0, []).

save(FileName) ->
    {ok, int_save(FileName)}.

restore(Backup) ->
    {ok, int_restore(Backup)}.

graph_op(Fun, Args) ->
    case apply(Fun, Args) of
        ok -> ok;
        Error -> {ok, Error}
    end.

%%% ----------------------------------------------------------------------------
%%% Property functions

set_prop({?NODETAG, _Class, _Id}, _Key, _Value) ->
    ok.

get_prop({?NODETAG, _Class, _Id}, _Key) ->
    {ok, []}.

get_props({?NODETAG, _Class, _Id}) ->
    {ok, []}.

del_prop({?NODETAG, _Class, _Id}, _Key) ->
    ok.

%%% ----------------------------------------------------------------------------
%%% Implementation details

%% Notify all servers about the pending reset that require it.
propagate_reset_to_servers() ->
    refcore_funprop:reset().

%% Resets the schema to the one given in the parameter.
reset_schema()->
    reset_schema(dummy).
reset_schema(_Schema) ->
    int_reset_schema().%,
%    {ok, InitSt} = init(dummy_param),
%    handle_schema([], InitSt#dbState{has_schema=true,
%                                    schema=Schema}).

%% handle_schema(SchemaList, St = #dbState{has_schema=false}) ->
%%     SchemaETS = ets:new(referl_schema, [bag]),
%%     SchemaAttrETS = ets:new(referl_schema_attrs, [bag]),
%%     [ets:insert(SchemaETS, {FromClass, Tag, ToClass}) ||
%%         {FromClass, _Attribs, SLinks} <- SchemaList,
%%         {Tag, ToClass} <- SLinks],
%%     [ets:insert(SchemaAttrETS, {FromClass, Attribs}) ||
%%         {FromClass, Attribs, _} <- SchemaList],
%%     {result, {ok, init}, St#dbState{has_schema=true,
%%                             schema={SchemaETS, SchemaAttrETS},
%%                             schema_list=SchemaList}};
%% handle_schema(_SchemaList, St = #dbState{has_schema=true,
%%                                         schema=Schema}) ->
%%     restore_envs(Schema),
%%     {result, {ok, match}, St}.

%% %% Saves the environment configuration to `EnvConfFile'.
%% save_envs(Schema) ->
%%     EnvConfFile = "refactorerl.emacs.configuration",
%%     {ok, Root} = handle_root(),
%%     {ok, Envs} = handle_path(Root, [env], Schema),
%%     EnvDatas = [Data || Env <- Envs, {ok, Data} <- [handle_data(Env)]],
%%     case EnvDatas of
%%         [] ->
%%             no_envs_saved;
%%         _ ->
%%             {ok, Dev} = file:open(EnvConfFile, [write]),
%%             io:format(Dev, "~p.~n", [EnvDatas]),
%%             file:close(Dev)
%%     end.

%% %% Restores the environment nodes saved by `save_envs/0'.
%% restore_envs(Schema) ->
%%     {ok, Root}    = handle_root(),
%%     {ok, OldEnvs} = handle_path(Root, [env], Schema),
%%     case OldEnvs of
%%         [] ->
%%             EnvConfFile = "refactorerl.emacs.configuration",
%%             Envs =
%%                 case filelib:is_file(EnvConfFile) of
%%                     true ->
%%                         {ok, [Envs2]} = file:consult(EnvConfFile),
%%                         Envs2;
%%                     false ->
%%                         [#env{name=appbase, value=code:lib_dir()},
%%                          #env{name=output, value=original}]
%%                 end,
%%             {ok, Root} = handle_root(),
%%             EnvNodes = [handle_create(Env, false, Schema) || Env <- Envs],
%%             [handle_mklink(Root, env, Env, false, Schema) || {ok, Env} <- EnvNodes];
%%         _ ->
%%             envs_already_present
%%     end.


%% Takes `Nodes' that belong to `Class' and returns the list of nodes
%% that are reached following the links described in the first argument.
%% Note that while traversing backwards links produce unordered lists,
%% forward links are currently forced to be ordered.
%% Most forward links would be unordered if not for the mentioned
%% technical restriction (except for links in the syntax tree,
%% and some semantic links such as some data flow ones);
%% currently, a quickfix is applied.
%% Note: `UFwdTag' means that the forward tag is logically unordered.
do_path([], Nodes, _) ->
    Nodes;
do_path([UFwdTags|Rest], Nodes, Class) when is_list(UFwdTags) ->
    NewNodess = [ufwd_int_path(Nodes, UFwdTag) || UFwdTag <- UFwdTags],
    NewNodes  = ?MISC:fast_usort(lists:flatten(NewNodess)),
    do_path(Rest, NewNodes, Class);
do_path([{UFwdTag, unique}|Rest], Nodes, Class) ->
    NewNodes  = ufwd_int_path(Nodes, UFwdTag),
    do_path(Rest, NewNodes, Class);
do_path([{intersect, {?NODETAG, _, TestNode}, TestPathElem}|Rest], Nodes, Class) ->
    {Tag, RevDir, _}   = get_tag_dir_filter(TestPathElem),
    Dir                = dir_reverse(RevDir),
    {class, TestClass} = ?GET_NEXT_CLASS(Dir, Class, Tag),
    Tests              = do_path([TestPathElem], [TestNode], TestClass),
    NewNodes           = ?MISC:intersect(Tests, Nodes),
    do_path(Rest, NewNodes, Class);
do_path([{Tag, {FromIdx, ToIdx}}|Rest], Nodes, Class) when is_integer(FromIdx), is_integer(ToIdx) ->
    {class, NextClass} = ?GET_NEXT_CLASS_FWD(Class, Tag),
    Nodes2             = do_path([Tag], Nodes, Class),
    NewNodes           = lists:sublist(Nodes2, FromIdx, ToIdx - FromIdx),
    do_path(Rest, NewNodes, NextClass);
do_path([Elem|Rest], Nodes, Class) ->
    {Tag, Dir, Filter} = get_tag_dir_filter(Elem),
    {class, NextClass} = ?GET_NEXT_CLASS(Dir, Class, Tag),

    Nodes2   = int_path(Nodes, Dir, Tag),
    FNodes2  = filter_ptrs(Filter, Nodes2, NextClass),
    NewNodes =
        case Dir of
            fwd  -> [To || {_, _, To} <- ?MISC:fast_usort(FNodes2)];
            back -> FNodes2
        end,
    do_path(Rest, NewNodes, NextClass).

dir_reverse(fwd)  -> back;
dir_reverse(back) -> fwd.

%% Note: in case of `intersect', filters are disregarded.
get_tag_dir_filter({{Tag, back}, Filt}) when is_atom(Tag) -> {Tag, back, Filt};
get_tag_dir_filter({Tag,  back}       ) when is_atom(Tag) -> {Tag, back, empty_filter};
get_tag_dir_filter({Tag,         Filt}) when is_atom(Tag) -> {Tag, fwd,  Filt};
get_tag_dir_filter(Tag                ) when is_atom(Tag) -> {Tag, fwd,  empty_filter};
get_tag_dir_filter(Elem               )                   -> throw({bad_path, Elem}).


get_class(Data) ->
    element(1, Data).

%% Filters a set of nodes. Where `FromIdxTo' is used, the traversed link is ordered.
filter_ptrs(empty_filter, Nodes, _) ->
    Nodes;
filter_ptrs(_, [], _) ->
    [];
filter_ptrs(Idx, FromIdxTos, _) when is_integer(Idx) ->
    [FromIdxTo || {_, Idx2, _} = FromIdxTo <- FromIdxTos, Idx == Idx2];
filter_ptrs(last, FromIdxTos, _) ->
    filter_last_fromidxtos(?MISC:fast_usort(FromIdxTos));
filter_ptrs({Idx, last}, Nodes, _) when is_integer(Idx) ->
    {_, Part2} = lists:split(Idx, Nodes),
    Part2;
filter_ptrs({Idx1, Idx2}, Nodes, _) when is_integer(Idx1), is_integer(Idx2) ->
    lists:sublist(Nodes, Idx1, Idx2 - Idx1);
filter_ptrs(Filter, Nodes, NextClass) ->
    Attrs = ?GET_CLASS_ATTRS(NextClass),
    [Node || Node <- Nodes, data_filter(Filter, xdata(Node), Attrs)].

%% The argument list has to be sorted. The function returns a list that
%% contains only the tuples where the index is the largest value for `From'.
filter_last_fromidxtos([]) ->
    [];
filter_last_fromidxtos(Fs = [_]) ->
    Fs;
filter_last_fromidxtos([F1 = {From1, _, _}| FITs = [{From2, _, _}|_]]) when From1 =/= From2 ->
    [F1|filter_last_fromidxtos(FITs)];
filter_last_fromidxtos([_|FITs]) ->
    filter_last_fromidxtos(FITs).


%% Returns the data of a node. For technical reasons,
%% the node may be in a tuple in case of a forward link.
xdata({_From, _Idx, To}) -> int_data(To);
xdata(Node)              -> int_data(Node).


data_filter({'not', Filter}, Data, Attrs) ->
    not data_filter(Filter, Data, Attrs);
data_filter({Filt1, 'and', Filt2}, Data, Attrs) ->
    data_filter(Filt1, Data, Attrs) andalso data_filter(Filt2, Data, Attrs);
data_filter({Filt1, 'or', Filt2}, Data, Attrs) ->
    data_filter(Filt1, Data, Attrs) orelse data_filter(Filt2, Data, Attrs);
data_filter({Attr, Op, Value}, Data, Attrs) when is_atom(Attr) ->
    OpF = get_op_fun(Op),
    Ind = indexof(Attr, Attrs) + 1,
    Ind =:= 0 andalso
        throw({bad_attribute, Attr}),
    OpF(element(Ind, Data), Value);
data_filter(Cond, _, _) ->
    throw({bad_condition, Cond}).

get_op_fun('==') -> fun(A,B) -> A =:= B end;
get_op_fun('/=') -> fun(A,B) -> A =/= B end;
get_op_fun('<' ) -> fun(A,B) -> A <   B end;
get_op_fun('=<') -> fun(A,B) -> A =<  B end;
get_op_fun('>' ) -> fun(A,B) -> A >   B end;
get_op_fun('>=') -> fun(A,B) -> A >=  B end.



%% todo Move to ?List
indexof(El, Lst) -> indexof(El, Lst, 1).

indexof(_,  [],        _)   -> -1;
indexof(El, [El | _],  Ind) -> Ind;
indexof(El, [_  | Tl], Ind) -> indexof(El, Tl, Ind+1).


%%% ============================================================================
%%% NIF calls

%% These macros are required in older versions of Erlang/OTP
%% that had no interface for transferring atoms and terms.
-ifdef(use_binary_wrapper).
    -define(term2gterm(X), term_to_binary(X)).
    -define(gterm2term(X), binary_to_term(X)).
    -define(atom2gatom(X), atom_to_binary(X, latin1)).
    -define(gatom2atom(X), binary_to_atom(X)).
-else.
    -define(term2gterm(X),      X).
    -define(gterm2term(X),      X).
    -define(atom2gatom(X),      X).
    -define(gatom2atom(X),      X).
-endif.

%int_is_protected_link(NodeId1, Tag, NodeId2) ->
%    nif_is_protected_link(NodeId1, ?atom2gatom(Tag), NodeId2).

int_is_protected_node(NodeId) ->
    nif_is_protected_node(NodeId).

int_root() ->
    nif_root().

int_create(Data, IsProtected) ->
    nif_create(?term2gterm(Data), ?atom2gatom(IsProtected)).

int_update(Node, Data) ->
    nif_update(Node, ?term2gterm(Data)).

int_data(Node) ->
    Result = nif_data(Node),
    case is_binary(Result) of
        true ->
            ?gterm2term(Result);
        false ->
            Result
    end.

int_delete(Node) ->
    nif_delete(Node).

int_mklink(FNode, Tag, last, TNode, IsProtected) ->
    nif_mklink(FNode, ?atom2gatom(Tag), -1, TNode, ?atom2gatom(IsProtected));

int_mklink(FNode, Tag, Idx, TNode, IsProtected) ->
    nif_mklink(FNode, ?atom2gatom(Tag), Idx, TNode, ?atom2gatom(IsProtected)).

int_rmlink(FNode, Tag, TNode) ->
    nif_rmlink(FNode, ?atom2gatom(Tag), TNode).

int_remove_garbage() ->
    nif_remove_garbage().

%% Traverses `Tag' in the forward direction and returns the resulting nodes.
%% `Tag' is expected to be an unordered tag.
ufwd_int_path(Nodes, Tag) ->
    NIFAtom = ?atom2gatom(Tag),
    ?MISC:fast_usort(lists:concat([nif_back_path(From, NIFAtom) || From <- Nodes])).

%% Returns a list of nodes that can be found when traversing `Tag' from `Froms',
%% together with `From' and `Idx'.
%% In case of a `back' link, returns an unordered list of resulting nodes.
int_path(Froms, fwd, Tag) ->
    NIFAtom = ?atom2gatom(Tag),
    [{From, Idx, To}
        ||  From      <- Froms,
            {To, Idx} <- ?MISC:index_list(nif_fwd_path(From, NIFAtom))];
int_path(Froms, back, Tag) ->
    NIFAtom = ?atom2gatom(Tag),
    lists:concat([nif_back_path(From, NIFAtom) || From <- Froms]).

int_links(Node) ->
    nif_links(Node).

int_back_links(Node) ->
    nif_back_links(Node).

int_class(Node) ->
    get_class(int_data(Node)).

int_index(FNode, Tag, TNode) ->
    nif_index(FNode, ?atom2gatom(Tag), TNode).

int_reset_schema() ->
    nif_reset_schema().

int_ls_backups() ->
    lists:reverse(nif_ls_backups()).

int_backup_info(Backup) ->
    nif_backup_info(Backup).

int_delete_all_backups() ->
    nif_delete_all_backups().

int_backup(CommitLog) ->
    save(fun nif_backup/1, [CommitLog]).

int_save(FileName) ->
    save(fun nif_save/1, [FileName]).

int_restore(BackupName) ->
    load(fun nif_restore/1, [BackupName]).

int_undo() ->
    load(fun nif_undo/0, []).

save(Fun, Args) ->
    %save_envs(Schema),
    ?SAVE_ENVS,
    Ret = apply(Fun, Args),
    %restore_envs(Schema),
    ?RESTORE_ENVS,
    Ret.

load(Fun, Args) ->
    %save_envs(Schema),
    ?SAVE_ENVS,
    % every load operation resets the graph.
    propagate_reset_to_servers(),
    Ret = apply(Fun, Args),
    %restore_envs(Schema),
    ?RESTORE_ENVS,
    Ret.

int_create_graph(Name) ->
    nif_create_graph(Name).

int_rename_graph(OldName, NewName) ->
    nif_rename_graph(OldName, NewName).

int_ls_graphs() ->
    nif_ls_graphs().

int_actual_graph() ->
    nif_actual_graph().

int_load_graph(Name) ->
    propagate_reset_to_servers(),
    nif_load_graph(Name).

int_delete_graph(Name) ->
    nif_delete_graph(Name).

int_delete_all_graphs() ->
    propagate_reset_to_servers(),
    nif_delete_all_graphs().


%%% ----------------------------------------------------------------------------
%%% NIF stubs

-define(NifLoadError, throw(nif_is_not_loaded)).

nif_is_protected_node(_NodeId)          -> ?NifLoadError.
%nif_is_protected_link(_NodeId1, _Tag,
%                      _NodeId2)         -> ?NifLoadError.
nif_root()                              -> ?NifLoadError.
nif_create(_Data, _IsProtected)         -> ?NifLoadError.
nif_update(_Node, _Data)                -> ?NifLoadError.
nif_delete(_Node)                       -> ?NifLoadError.
nif_data(_Node)                         -> ?NifLoadError.
nif_mklink(_FNode, _Tag, _Idx, _TNode,
           _IsProtected)                -> ?NifLoadError.
nif_rmlink(_FNode, _Tag, _TNode)        -> ?NifLoadError.
nif_remove_garbage()                    -> ?NifLoadError.
nif_fwd_path(_Nodes, _Tag)              -> ?NifLoadError.
nif_back_path(_Nodes, _Tag)             -> ?NifLoadError.
nif_links(_Node)                        -> ?NifLoadError.
nif_back_links(_Node)                   -> ?NifLoadError.
nif_index(_FNode, _Tag, _TNode)         -> ?NifLoadError.
nif_get_datastore()                     -> ?NifLoadError.
nif_reset_schema()                      -> ?NifLoadError.
nif_backup(_CommitLog)                  -> ?NifLoadError.
nif_backup_info(_Backup)                -> ?NifLoadError.
nif_ls_backups()                        -> ?NifLoadError.
nif_delete_all_backups()                -> ?NifLoadError.
nif_undo()                              -> ?NifLoadError.
nif_save(_FileName)                     -> ?NifLoadError.
nif_restore(_Backup)                    -> ?NifLoadError.
nif_create_graph(_Name)                 -> ?NifLoadError.
nif_rename_graph(_OldName, _NewName)    -> ?NifLoadError.
nif_ls_graphs()                         -> ?NifLoadError.
nif_actual_graph()                      -> ?NifLoadError.
nif_load_graph(_Name)                   -> ?NifLoadError.
nif_delete_graph(_Name)                 -> ?NifLoadError.
nif_delete_all_graphs()                 -> ?NifLoadError.
