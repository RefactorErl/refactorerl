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

%%% @doc Graph storage server. This is a mnesia based implementation of
%%% the semantical graph server interface.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Andras Nemeth <andras.b.nemeth@ericsson.com>

-module(refcore_graph).
-vsn("$Rev: 12913 $"). %% "

%%% ============================================================================
%%% Exports

%% Client exports
-export([schema/1, get_schema/0, reset_schema/0,
         erase_nodes/0,
         root/0, create/1, create_prot/1, update/2, delete/1, data/1, class/1,
         mklink/3, mklink_prot/3, rmlink/3, links/1, path/2, index/3,
         set_prop/3, get_prop/2, get_props/1, del_prop/2,
         remove_garbage/0, back_links/1]).

%% Compatibility interface to refcore_db
-export([backup/0, backup/1, restore/1, ls_backups/0, backup_info/1,
         undo/0, redo/0, clean/0]).
-export([is_gnode/1]).

-export([get_dbmod/0]).

-export([save_envs/0]).

-export([save/1, create_graph/1, rename_graph/2, ls_graphs/0, actual_graph/0,
         load_graph/1, delete_graph/1, delete_all_graphs/0]).

%%% ============================================================================
%%% Client functions

-include("core.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(GRAPH_TIMEOUT, infinity).

-define(GraphOp(Request,Fun),
        begin
            %Request = list_to_tuple([Req | Args]),
            %case gen_server:call(?GRAPH_SERVER, Request, ?GRAPH_TIMEOUT) of
            case refcore_gendb:Fun(Request, ?GRAPH_TIMEOUT) of
                ok             -> ok;
                {ok, Reply}    -> Reply;
                {error, Error} -> erlang:error(Error, Request)
            end
        end).

-define(SyncGraphOp(Request),
    ?GraphOp(Request, sync_graph_op)).

-define(AsyncGraphOp(Request),
    ?GraphOp(Request, async_graph_op)).

%% @type rnode().
%%  Represents a node in the graph.

%% @type data() = tuple().
%%  Represents the class and attributes of a node. This is essentially a
%%  record, the name of the record (or the first element of the tuple) is
%%  the class name, and the fields are the attributes.

%% @type path() = [PathElem]
%%       PathElem = Tag | {Tag, Index} | {Tag, Filter} | {intersect, Node, Tag}
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

%% @spec schema(schema()) -> init | match | mismatch
%% @doc Initialises the server with a given schema. Checks whether the
%% persistent storage has the same schema as given here, and initialises it
%% if neccessary.
schema(Schema) ->
    %gen_server:call(?GRAPH_SERVER, {schema, Schema}, ?GRAPH_TIMEOUT).
    ?SyncGraphOp([schema, Schema]).

%% @spec get_schema() -> schema()
%% @doc Initialises the server with a given schema. Checks whether the
%% persistent storage has the same schema as given here, and initialises it
%% if neccessary.
get_schema() ->
    %gen_server:call(?GRAPH_SERVER, {get_schema}, ?GRAPH_TIMEOUT).
    ?SyncGraphOp([get_schema]).

%% @spec reset_schema() -> ok
%% @doc This function can be used to erase the current graph schema when a new
%% schema has to be used. Erases all data from the database.
%% @see erase_graph/0
reset_schema() ->
    %gen_server:call(?GRAPH_SERVER, {reset_schema}, ?GRAPH_TIMEOUT).
    ?SyncGraphOp([reset_schema]).

%% @spec erase_nodes() -> ok
%% @doc This function erases the contents of the database while retaining the
%% schema. This is a synchronous operation (opposed to {@link reset_schema/0}).
erase_nodes() ->
    ?SyncGraphOp([erase_nodes]).

%% @spec root() -> rnode()
%% @doc Returns the root node.
root() ->
    ?AsyncGraphOp([root]).

%% @spec create(data()) -> rnode()
%% @doc Creates a new node. The class and attributes of the node are given by
%% Data.
create(Data) when is_tuple(Data) ->
    ?AsyncGraphOp([create, Data]).

%% @spec create_prot(data()) -> rnode()
%% @doc Creates a protected new node.
create_prot(Data) when is_tuple(Data) ->
    ?AsyncGraphOp([create_prot, Data]).

%% @spec update(rnode(), data()) -> ok
%% @doc Updates the attributes of a node. The new attributes are given by
%% Data, which must have the same class as Node.
update(Node, Data) when ?IS_NODE(Node) ->
    ?SyncGraphOp([update, Node, Data]).

%% @spec delete(rnode()) -> ok
%% @doc Deletes the given node.
delete(Node) when ?IS_NODE(Node) ->
    ?SyncGraphOp([delete, Node]).

%% @spec data(rnode()) -> data()
%% @doc Returns the data associated with a node.
data(Node) when ?IS_NODE(Node) ->
    ?AsyncGraphOp([data, Node]).

%% @spec class(rnode()) -> atom()
%% @doc Returns the node class of the node. This is equivalent to
%% `element(1, data(Node))' (but may be faster).
class({?NODETAG, Class, _Id}) ->
    Class.

%% @spec mklink(rnode(), atom() | {atom(), integer()}, rnode()) -> ok
%% @doc Creates a link between two nodes.
mklink(From, Tag, To) when ?IS_NODE(From), ?IS_NODE(To),
                           is_atom(Tag); is_tuple(Tag) ->
    ?SyncGraphOp([mklink, From, Tag, To]).

%% @spec mklink_prot(rnode(), atom() | {atom(), integer()}, rnode()) -> ok
%% @doc Creates a link between two nodes.
mklink_prot(From, Tag, To) when ?IS_NODE(From), ?IS_NODE(To),
                           is_atom(Tag); is_tuple(Tag) ->
    ?SyncGraphOp([mklink_prot, From, Tag, To]).

%% @spec rmlink(rnode(), atom(), rnode()) -> ok
%% @doc Removes a link between two nodes.
rmlink(From, Tag, To) when ?IS_NODE(From), ?IS_NODE(To), is_atom(Tag) ->
    ?SyncGraphOp([rmlink, From, Tag, To]).

remove_garbage() ->
    ?SyncGraphOp([remove_garbage]).

%% @spec links(rnode()) -> [{atom(), rnode()}]
%% @doc Returns the links starting from a node.
links(Node) when ?IS_NODE(Node) ->
    ?AsyncGraphOp([links, Node]).

back_links(Node) when ?IS_NODE(Node) ->
    ?AsyncGraphOp([back_links, Node]).

%% @spec path(rnode(), path()) -> [rnode()]
%% @doc Evaluates a path expression starting from Node, and returns the
%% resulting nodes.
path(Node, []) -> [Node];
path(Node, Path) when ?IS_NODE(Node), is_list(Path) ->
    ?AsyncGraphOp([path, Node, Path]).

%% @spec index(rnode(), atom(), rnode()) -> integer() | none
%% @doc Returns the index of a link. If there is a link between `From' and `To'
%% with a tag `Tag', then the result is the index of this link among the links
%% with tag `Tag'. Otherwise, the result is `none'.
index(From, Tag, To) when ?IS_NODE(From), ?IS_NODE(To), is_atom(Tag) ->
    ?AsyncGraphOp([index, From, Tag, To]).

%% @spec set_prop(rnode(), atom(), term()) -> ok
%% @doc Set a node property.
set_prop(Node, Key, Value) when ?IS_NODE(Node), is_atom(Key) ->
    ?SyncGraphOp([set_prop, Node, Key, Value]).

%% @spec get_prop(rnode(), atom()) -> {ok, term()} | undefined
%% @doc Get the value of a node property.
get_prop(Node, Key) when ?IS_NODE(Node), is_atom(Key) ->
    ?AsyncGraphOp([get_prop, Node, Key]).

%% @spec get_props(rnode()) -> [{atom(), term()}]
%% @doc Get all properties of a node.
get_props(Node) when ?IS_NODE(Node) ->
    ?AsyncGraphOp([get_props, Node]).

%% @spec del_prop(rnode(), atom()) -> ok
%% @doc Remove a property of a node.
del_prop(Node, Key) when ?IS_NODE(Node), is_atom(Key) ->
    ?SyncGraphOp([del_prop, Node, Key]).

%% @spec backup() -> {ok,backupfile} | error
%% @doc Creates a new checkpoint from mnesia tables and returns the
%% name of the checkpoint.
backup() ->
    ?SyncGraphOp([backup]).

%% @spec backup(string()) -> any()
%% @doc Works as {@link ri:backup/0}, but you can attach a commit-log
%% to the backup.
backup(CommitLog) ->
    ?SyncGraphOp([backup, CommitLog]).

%% @doc Returns a list from the created backups.
ls_backups() ->
    ?AsyncGraphOp([ls_backups]).

%% @spec backup_info(atom() | integer() | string()) -> any()
%% @doc Returns information about the given backup. That information
%% is actually a tuple, which first element is the backup name,
%% the second a string that contains, the time of creation of the backup,
%% and the third is the commit-log. If the commit-log has not been
%% specified, then the third element will be an empty string. 
backup_info(Backup) when is_atom(Backup) or is_integer(Backup) ->
    ?AsyncGraphOp([backup_info, Backup]);
backup_info(Backup) when is_list(Backup) ->
    ?AsyncGraphOp([backup_info, list_to_atom(Backup)]).

%% @spec save(atom() | string()) -> any()
%% @doc Saves the actual graph's state to the given file.
%% Note that you must specify only the name of the file,
%% the function always saves the state to the current graph's folder.
save(FileName) when is_atom(FileName) ->
    Hash = ?FileMan:db_hash(),
    ?FileMan:set_db_hash(Hash),
    ?SyncGraphOp([save, FileName]);
save(FileName) when is_list(FileName) ->
    Hash = ?FileMan:db_hash(),
    ?FileMan:set_db_hash(Hash),
    ?SyncGraphOp([save, list_to_atom(FileName)]).

save_envs() ->
    ?SyncGraphOp([save_envs]).

%% @spec restore(atom() | integer() | string()) -> any()
%% @doc Restores the given backup.
restore(Backup) when is_atom(Backup) or is_integer(Backup) ->
    ?SyncGraphOp([restore, Backup]);
restore(Backup) when is_list(Backup) ->
    ?SyncGraphOp([restore, list_to_atom(Backup)]).

%% @spec undo() -> undo_is_ok | invalid_checkpoint_number
%% @doc Restores the previous state of the database from the last
%% checkpoint. The actual checkpoint numbers are
%% stored in the database.
undo() ->
    %?Db:undo().
    ?SyncGraphOp([undo]).

%% @spec redo() -> restored | invalid_checkpoint_number
%% @doc Restores the next state of the database from the last
%% checkpoint. The actual checkpoint numbers are
%% stored in the database.
redo() ->
    %?Db:redo().
    ?SyncGraphOp([redo]).

%% @spec clean() -> cleaned_all_backups
%% @doc Cleans all backup files from the mnesia storage
clean()->
    %?Db:clean().
    ?SyncGraphOp([clean]).

%% @spec is_gnode(term()) -> boolean()
%% @doc  Returns whether the argument is the representation of a graph node.
is_gnode({'$gn', _, _}) -> true;
is_gnode(_)             -> false.

%% @spec create_graph(atom()) -> any()
%% @doc Creates a graph, with name. That graph can be
%% restored later with the load_graph/1 function.
create_graph(Name) ->
    ?SyncGraphOp([create_graph, Name]).

%% @spec rename_graph(atom(), atom()) -> any()
%% @doc Renames a graph.
rename_graph(OldName, NewName) ->
    ?SyncGraphOp([rename_graph, OldName, NewName]).

%% @spec ls_graphs() -> list()
%% @doc Lists the created graphs.
ls_graphs() ->
     ?AsyncGraphOp([ls_graphs]).

%% @spec actual_graph() -> atom()
%% @doc Returns the name atom of the actual graph.
actual_graph() ->
     ?AsyncGraphOp([actual_graph]).

%% @spec load_graph(atom()) -> any()
%% @doc Restores a previously created graph. The only parameter is the
%% graph's name.
load_graph(Name) ->
    ?SyncGraphOp([load_graph, Name]).

%% @spec delete_graph(atom()) -> any()
%% @doc Deletes the given graph. The only parameter is the
%% graph's name.
delete_graph(Name) ->
    ?SyncGraphOp([delete_graph, Name]).

%% @spec delete_all_graphs() -> ok
%% @doc Removes all graphs.
delete_all_graphs() ->
    ?SyncGraphOp([delete_all_graphs]).

%% @spec get_dbmod() -> refdb_nif | refdb_mnesia | refdb_kyotomini
%% @doc Returns the name of the currently used database module. 
get_dbmod() ->
    ?RefDb:get_dbmod().

%%% ============================================================================
%%% Implementation
%%%
%%% For each node class, there are two mnesia tables:
%%%  - one with the name of the class that stores the attributes
%%%  - one with '$lnk' appended to the class name that stores link information
%%%
%%% Every node has an integer ID, which is unique in its class. A node
%%% is represented by a {?NODETAG, Class, Id} tuple.
%%%
%%% The attribute table contains tuples of the form {Id, Data}, the key
%%% is the first element.
%%%
%%% The link table contains tuples of the form
%%%   {Class, {From, Tag}, Index, To},
%%% where Class is the name of the table, From and To are ID-s, Tag is the
%%% link tag and Index is the index of the link among the links from the
%%% same node and with the same tag. The (non-unique) key is the {From, Tag}
%%% element, and there is a mnesia index on the To element for backward
%%% links.


%%% ----------------------------------------------------------------------------
%%% Data types

%% Class table: stores the attribute names for every node class
%% -record(class, {name, attribs, links}).
%% Target table: stores the link target class name for every starting class,
%% link tag pair
%% -record(target, {start, next}).
%% Nextid table: stores the next available ID for every node class
%% -record(nextid, {class, id}).
