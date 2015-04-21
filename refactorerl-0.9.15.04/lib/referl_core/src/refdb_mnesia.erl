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

-module(refdb_mnesia).
-vsn("$Rev: 12253 $"). %% "
-behaviour(refcore_gendb).

%%% ----------------------------------------------------------------------------
%%% macros

%%% ----------------------------------------------------------------------------
%%% Exports

-export([%start_link/1,
         init/1,
         is_path_supported/0,
         is_parallel_exec_supported/0,
         create_class/1,
         erase_class/1,
         code_change/2,
         terminate/2]).


-export([%start_link/0,
         %schema/1, get_schema/0, reset_schema/0,
         erase_nodes/0,
         root/0, create/1, create_prot/1, update/2, delete/1, data/1, %class/1,
         mklink/3, mklink_prot/3, rmlink/3, links/1, path/2, index/3,
         set_prop/3, get_prop/2, get_props/1, del_prop/2,
         remove_garbage/0, back_links/1]).

%% Compatibility interface to refcore_db
-export([backup/0, backup/1, restore/1, ls_backups/0, backup_info/1,
         undo/0, redo/0, clean/0]).
%-export([is_gnode/1]).

-export([save/1, create_graph/1, rename_graph/2, ls_graphs/0, actual_graph/0,
         load_graph/1, delete_graph/1, delete_all_graphs/0]).

%% internal handlers
%-export([req_handler_loop/1]).

%%% ----------------------------------------------------------------------------
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

%% Nextid table: stores the next available ID for every node class
-record(nextid, {class, id}).

%%% ----------------------------------------------------------------------------
%%% Callback functions

-define(Exec(Body), exec(fun() -> Body end)).

-record(dbState, {has_schema      :: boolean() | schema_error,
                  schema          :: tuple(),
                  schema_list     :: list(),
                  req_handler_pid :: pid()}).

-include("core.hrl").
-include("refcore_gendb.hrl").
-include_lib("stdlib/include/qlc.hrl").

is_path_supported() ->
    true.

is_parallel_exec_supported() ->
    true.

%% @private
init(_) ->
    process_flag(trap_exit, true),
    case has_right_to_use_mnesia(node()) of
        true ->Init = init_mnesia(),
               HasSchema = has_schema(Init),
               HasSchema orelse create_db_classes(),
               {ok, #dbState{has_schema=HasSchema}};
        false -> {stop, {no_right, no_right_message()}}
    end.

%% @spec init_mnesia() -> empty | exists
%% @doc Initializes the database connection, creates a new database if
%% necessary.
init_mnesia() ->
        case mnesia:table_info(schema, storage_type) of
        ram_copies ->
            %% The schema should be stored on disc
            mnesia:change_table_copy_type(schema, node(), disc_copies),

            %% checkpoint counter table (needs a better name)
            mnesia:create_table(counter, [{ram_copies,[node()]}]),
            init_counter(),

            empty;

        disc_copies ->
            mnesia:wait_for_tables(mnesia:system_info(tables),infinity),
            delete_end_backups({1,up}),
            init_counter(),
            exists
    end.

has_right_to_use_mnesia(Node)->
    lists:member(Node,mnesia:system_info(db_nodes)).

no_right_message()->
    io_lib:format("The Mnesia data directory (~s) can only be used from the following node(s): ~p.~n",
                  [mnesia:system_info(directory), mnesia:system_info(db_nodes)]).

has_schema(Init) ->
    %Tabs = [class, target, nextid, protected],
    Tabs = [nextid, protected],
    ClassesExist = lists:all(fun(Tab) -> table_exists(Tab) end, Tabs),
    Init == exists andalso ClassesExist.

create_db_classes() ->
    create_table(nextid, record_info(fields, nextid), []),
    create_table(protected, record_info(fields, protected), [{type, bag}]).

%% @spec table_exists(atom()) -> bool()
%% @doc Checks if `Table' is available.
table_exists(Table) ->
    try mnesia:table_info(Table, storage_type) of
        disc_copies ->
            mnesia:wait_for_tables([Table], infinity),
            true;
        _ ->
            true
    catch
        exit:{aborted, {no_exists, _, _}} ->
            false
    end.

%% @spec create_table(atom(), [atom()], [Opt]) -> ok
%% @doc Create a new table. `Opt' may be (almost) any mnesia table option.
create_table(Name, Attribs, Opts) ->
    {atomic, ok} = mnesia:create_table(Name,
                                       [{attributes, Attribs},
                                        {disc_copies, [node()]} | Opts]).

%% @spec exec(() -> term()) -> term()
%% @doc Executes `Fun' in a mnesia activity context.
exec(Fun) -> exec(Fun, wr).

%% @spec exec(() -> term(), rd | wr) -> term()
%% @doc Executes `Fun' in the given type of mnesia activity context. `wr'
%% means the transaction may read and write (or should be protected by
%% simultaneous writes), `rd' means the transaction is read only and there may
%% be no simultaneous writes.
exec(Fun, wr) ->
    mnesia:async_dirty(Fun);
exec(Fun, rd) ->
    mnesia:ets(Fun).

code_change(_OldVsn, _State) ->
    no_code_change.

%% @private
terminate(_Reason, _State) ->
    %%delete_end_backups({1,up}),
    ok.


%%% ----------------------------------------------------------------------------
%%% Schema handling
%%% This part was moved to refcore_gendb

create_class(Class) ->
    table_exists(Class) orelse Class =:= root % do not create root table
        orelse create_table(Class, [id, attribs, props], []),
    table_exists(linktab(Class))
        orelse create_table(linktab(Class),
                          [id, ind, to],
                          [{index, [to]}, {type, bag}]).

erase_nodes() ->
    Q = qlc:q([{Name, linktab(Name)} ||
                  Name <- ?GET_LIST_OF_CLASSES,
                  Name =/= root, Name =/= '$hash']),
    ?Exec(lists:foreach(fun erase_class/1, qlc:e(Q))),
    ok.

erase_class(Class) ->
    Class =:= root orelse erase_tab(Class),
    erase_tab(linktab(Class)),
    ?Exec(mnesia:delete({nextid, Class})).

erase_tab(Tab) ->
    ?Exec(lists:foreach(fun mnesia:delete/1,
                        [{Tab, Key} || Key <- mnesia:all_keys(Tab)])).

%%% ----------------------------------------------------------------------------
%%% Node operations

root() ->
    {ok, {?NODETAG, root, 0}}.

data({?NODETAG, root, _}) ->
    {ok, {root}};
data({?NODETAG, Class, Id}) ->
    case ?Exec(mnesia:read(Class, Id, read)) of
        [{_,_,Data,_}] -> {ok, Data};
        []             -> {error, bad_node}
    end.

create_prot(Data) ->
    Result = {ok, {?NODETAG, Class, Id}}
           = create_int(Data),
    protect_node(Class, Id),
    Result.

create(Data) ->
    create_int(Data).

create_int(Data) ->
    Class = element(1, Data),
    Id = create_node(Class, Data),
    {ok, {?NODETAG, Class, Id}}.

create_node(Class, Data) ->
    Id = mnesia:dirty_update_counter(nextid, Class, 1),
    ?Exec(mnesia:write({Class, Id, Data, []})),
    Id.

update(Node = {?NODETAG, Class, Id}, Data) ->
    case {is_protected_node(Node), data(Node)} of
        {true,_} ->
            % todo Reply with something other than 'ok'
            %      if the node is protected?
            ok;
        {false, {error, bad_node}} ->
            {error, bad_node};
        {false, _} ->
            if
                element(1, Data)=:=Class ->
                    ?Exec(
                       begin
                           [{Class, Id, _, Props}] = mnesia:read({Class, Id}),
                           mnesia:write({Class, Id, Data, Props})
                       end),
                    ok;
                true ->
                    {error, {bad_class, Class}}
            end
    end.

delete(Node = {?NODETAG, Class, Id}) ->
    ?Exec(
        begin
            case mnesia:read({Class, Id}) of
                [{Class, Id, _, _Props}] -> ok;
                Other ->
                    throw({bad_delete, Node, Other})
            end,


            mnesia:delete({Class, Id}),
            delete_forward_links(Class, Id),
            delete_backward_links(Class, Id)
        end),
    ok.

delete_forward_links(Class, Id) ->
    Tags = [Tag || {Tag, _} <- ?GET_NEXT_CLASSES_FWD(Class)],
    %Tags = [Tag || {Tag, _} <- link_targets(Class, fwd)],
    [ok = mnesia:delete({linktab(Class), {Id, Tag}}) ||
        Tag <- Tags].

delete_backward_links(Class, Id) ->
    Links =
        lists:foldl(
          fun
              ({FTag, FCl}, Lnk) ->
                  orddict:append(FCl, FTag, Lnk)
          end,
          [],
          ?GET_NEXT_CLASSES_BACK(Class)),
          %link_targets(Class, back)),

    Reorders =
        [begin
            mnesia:delete_object(Rec),
            {linktab(FCl), From}
         end ||
            {FCl, Tags}             <- Links,
            Rec = {_,From = {_,FTag}, _,_} <-
                mnesia:index_match_object({linktab(FCl), '_', '_', Id}, 4),
            lists:member(FTag, Tags)],
    [begin
        Nodes = mnesia:match_object({CLnk, From, '_', '_'}),
        [ begin
            mnesia:delete_object(Obj),
            mnesia:write({CLnk, {FId, Tag}, Idx2, To})
          end
            ||  {Obj = {_CLnk, {FId, Tag}, Idx, To}, Idx2}  <- ?MISC:index_list(Nodes),
                Idx =/= Idx2]
     end ||
        {CLnk, From} <- lists:usort(Reorders)].


%%% ----------------------------------------------------------------------------
%%% Link queries

%% Note: the links need reordering, as Mnesia does not keep the order.
%% Since the links are unique, a unique sort is applicable.
links({?NODETAG, Class, Id}) ->
    T = [{{Id, Tag}, TCl} || {Tag, TCl} <- ?GET_NEXT_CLASSES_FWD(Class)],
    Q = qlc:q([ {Tag, {?NODETAG, TCl, To}} ||
                  {ST={_,Tag}, TCl} <- T,
                  {_, FL, _Ind, To} <- mnesia:table(linktab(Class)),
                  ST =:= FL]),
    Run = ?Exec(qlc:e(Q)),
    Reordered = ?MISC:fast_usort(Run),
    {ok, Reordered}.

back_links(_Node) -> % TODO
    {ok, {not_implemented, ?MODULE, handle_back_links}}.

index({?NODETAG, FCl, FId}, Tag, {?NODETAG, TCl, TId}) ->
    case ?GET_NEXT_CLASS_FWD(FCl, Tag) of
    %case link_target(FCl, Tag, fwd) of
        {class, TCl} ->
            case link_index(FCl, FId, Tag, TId) of
                []    -> {ok, none};
                [Idx] -> {ok, Idx};
                Multi when is_list(Multi) -> {ok, Multi}
            end;
        _ ->
            {error, bad_link}
    end.

path({?NODETAG, Class, Id}, Path) ->
    try compile_path(Path, Class, [Id]) of
        Query ->
            {ok, ?Exec(qlc:e(Query))}
    catch
        throw:Msg ->
            {error, Msg}
    end.

%% Returns existing link indexes (normally only zero or one)
link_index(FCl, FId, Tag, TId) ->
    Q = qlc:q([Idx || {_, Key, Idx, To} <- mnesia:table(linktab(FCl)),
                      Key =:= {FId, Tag},
                      To =:= TId]),
    ?Exec(qlc:e(Q)).


%%% ----------------------------------------------------------------------------
%%% Link modifications

%% mklink
mklink(From = {?NODETAG, FCl, FId}, TagInfo, To = {?NODETAG, TCl, TId}) ->
    {Tag, Idx} =
        if
            is_atom(TagInfo) -> {TagInfo, last};
            true             -> TagInfo
        end,
    %case link_target(FCl, Tag, fwd) of
    case ?GET_NEXT_CLASS_FWD(FCl, Tag) of
        {class, TCl} ->
            ?Exec(
               case {node_exists(FCl, FId), node_exists(TCl, TId)} of
                   {true, true} ->
                       do_mklink(FCl, FId, Tag, Idx, TId),
                       ok;
                   {false, true} ->
                       {error, {bad_node, From}};
                   {true, false} ->
                       {error, {bad_node, To}};
                   {false, false} ->
                       {error, {bad_nodes, From, To}}
               end);
        _ ->
            {error, bad_link}
       end.

do_mklink(FCl, FId, Tag, Idx, TId) ->
    Absent = link_index(FCl, FId, Tag, TId) =:= [],
    if
        not Absent -> ok;
        Idx =:= last ->
            append_link(linktab(FCl), FId, TId, Tag);
        true ->
            insert_link(linktab(FCl), FId, TId, Tag, Idx)
    end.

node_exists(root, 0)   -> true;
node_exists(Class, Id) -> mnesia:read(Class, Id) =/= [].

%% Only creates the new link
append_link(Links, From, To, Tag) ->
    Idx = length(mnesia:read(Links, {From, Tag}, read)) + 1,
    mnesia:write({Links, {From, Tag}, Idx, To}).

%% Replaces the set of links completely with the new set
insert_link(Links, From, To, Tag, Idx) ->
    LinkList = mnesia:read(Links, {From,Tag}, read),
    NewLinks =
        [ Lnk || Lnk = {_, _, I, _} <- LinkList, I < Idx ]++
        [ {Links, {From, Tag}, Idx, To} ] ++
        [ {L, F, I+1, T} || {L, F, I, T} <- LinkList, I >= Idx ],
    mnesia_replace_links(From, Tag, Links, NewLinks).

mklink_prot(From = {?NODETAG, FCl, FId}, Tag, To = {?NODETAG, _, TId}) ->
    case mklink(From, Tag, To) of
        ok ->
            protect_link(FCl, FId, Tag, TId),
            ok;
        Error -> Error
    end.

%% rmlink
rmlink(From = {?NODETAG, FCl, FId}, Tag, To = {?NODETAG, TCl, TId}) ->
    case is_protected_link(FCl, FId, Tag, TId) of
        true ->
            % todo Reply with something other than 'ok'
            %      if the node is protected?
            ok;
        false ->
            %case link_target(FCl, Tag, fwd) of
            case ?GET_NEXT_CLASS_FWD(FCl, Tag) of
                {class, TCl} ->
                    ?Exec(remove_link(linktab(FCl), FId, TId, Tag));
                _ ->
                    {error, {bad_link, From, Tag, To}}
            end
    end.

%% Replaces the set of links completely
remove_link(OldLinks, From, To, Tag) ->
    LinkList = mnesia:read(OldLinks, {From, Tag}),
    case  [I || {_, _, I, T} <- LinkList, T == To] of
        [Idx] ->
            NewLinks =
                [ Lnk || Lnk = {_, _, I, _} <- LinkList, I < Idx ] ++
                [ {L, F, I-1, T} ||
                    {L, F, I, T} <- LinkList, I > Idx ],
            mnesia_replace_links(From, Tag, OldLinks, NewLinks),
            ok;
        [] ->
            {error, not_exists};
        _ ->
            throw({multiple_links, Tag})
    end.

%% Removes the old links from Mnesia and inserts the new ones instead.
mnesia_replace_links(From, Tag, OldLinks, NewLinks) ->
    mnesia:delete({OldLinks, {From,Tag}}),
    [mnesia:write(Lnk) || Lnk <- NewLinks].

remove_garbage() ->
    {ok, {not_implemented, ?MODULE, handle_remove_garbage}}.

%%% ----------------------------------------------------------------------------
%%% Property functions

set_prop({?NODETAG, Class, Id}, Key, Value) ->
    ?Exec(
       case node_exists(Class, Id) of
           true ->
               [{Class, Id, Data, Props}] = mnesia:read({Class, Id}),
               NewProps = [{Key, Value} | proplists:delete(Key, Props)],
               mnesia:write({Class, Id, Data, NewProps}),
               ok;
           false ->
               {error, bad_node}
       end).

get_prop({?NODETAG, Class, Id}, Key) ->
    ?Exec(
       case node_exists(Class, Id) of
           true ->
               [{Class, Id, _Data, Props}] = mnesia:read({Class, Id}),
               Value =
                   case proplists:lookup(Key, Props) of
                       none -> undefined;
                       {Key, Val} -> {ok, Val}
                   end,
               {ok, Value};
           false ->
               {error, bad_node}
       end).

get_props({?NODETAG, Class, Id}) ->
    ?Exec(
       case node_exists(Class, Id) of
           true ->
               [{Class, Id, _Data, Props}] = mnesia:read({Class, Id}),
               {ok, lists:usort(Props)};
           false ->
               {error, bad_node}
       end).

del_prop({?NODETAG, Class, Id}, Key) ->
    ?Exec(
       case node_exists(Class, Id) of
           true ->
               [{Class, Id, Data, Props}] = mnesia:read({Class, Id}),
               NewProps = proplists:delete(Key, Props),
               mnesia:write({Class, Id, Data, NewProps}),
               ok;
           false ->
               {error, bad_node}
       end).

%%% ----------------------------------------------------------------------------
%%% Path compiler

%% Returns the list of node IDs that are reachable from `Ids' through
%% the path described in the first argument.
%% The IDs at all steps belong to `Class' (they represent {'$gn', Class, Id} nodes).
compile_path([], Class, Ids) ->
    qlc:q([{?NODETAG, Class, Id} || Id <- Ids]);
compile_path([S={intersect, {?NODETAG, SCl, SId}, Step} | Rest], Class, Ids) ->
    {Tag, Dir} = get_tag_dir(Step),

    ?GET_NEXT_CLASS(Dir, SCl, Tag) =:= {class, Class} orelse
        throw({bad_class, S, Class}),

    Ids2 =
        case Dir of
            fwd  -> sort_fwd_links(fwd_intersect(SCl, SId, Tag, Ids));
            back -> back_intersect(Class, SId, Ids, Tag)
        end,
    compile_path(Rest, Class, Ids2);
compile_path([Elem | Rest], Class, Ids) ->
    {Tag, Dir, Filter} = get_tag_dir_filter(Elem),
    {class, NextClass} = ?GET_NEXT_CLASS(Dir, Class, Tag),
    Cond               = get_cond(Dir, Class, Tag, Filter, NextClass),

    Ids2 =
        case Dir of
            fwd  -> sort_fwd_links(fwd_one_step(Tag, Ids, Class, Cond));
            back -> back_one_step(NextClass, Ids, Tag, Cond)
        end,
    compile_path(Rest, NextClass, Ids2).

%% In case of forward links, the result ids are sorted,
%% because the order of links has to be kept during top-down traversals,
%% and Mnesia does not keep them sorted automatically.
sort_fwd_links(FromIdxTos) ->
    [ToId || {_From, _Idx, ToId} <- ?MISC:fast_usort(?Exec(qlc:e(FromIdxTos)))].


get_tag_dir(Step) when is_atom(Step) -> {Step, fwd};
get_tag_dir(StepDir = {_, _})        -> StepDir.

fwd_intersect(SCl, SId, Tag, Ids) ->
    qlc:q([{From, Idx, Id}
            || {_, From, Idx, To} <- mnesia:table(linktab(SCl)),
               From =:= {SId, Tag},
               Id <- Ids,
               Id =:= To]).

fwd_one_step(Tag, Ids, Class, Cond) ->
    Keys = qlc:q([{{Id, Tag}} || Id <- Ids]),
	qlc:q([{K1, Idx, To} || {K1} <- Keys,
                            {_, K2, Idx, To} <- mnesia:table(linktab(Class)), 
                            K1 =:= K2, 
                            Cond(element(1, K1), Idx, To)]). 

back_intersect(Class, SId, Ids, Tag) ->
    qlc:q(
      [Id || {_, From, _,To} <- mnesia:table(linktab(Class)),
             SId =:= To,
             Id <- Ids,
             From =:= {Id, Tag}]).

back_one_step(NextClass, Ids, Tag, Cond) ->
    Keys = qlc:q([{Id} || Id <- Ids]),
    qlc:q([From || {_, {From, T}, _, To} <- mnesia:table(linktab(NextClass)),
                   {Id} <- Keys,
                   To =:= Id,
                   T =:= Tag,
                   Cond(Id, -1, From)]).


get_tag_dir_filter({{Tag, back}, Filt}) when is_atom(Tag) -> {Tag, back, Filt};
get_tag_dir_filter({Tag,  back}       ) when is_atom(Tag) -> {Tag, back, {}};
get_tag_dir_filter({Tag,         Filt}) when is_atom(Tag) -> {Tag, fwd,  Filt};
get_tag_dir_filter(Tag                ) when is_atom(Tag) -> {Tag, fwd,  {}};
get_tag_dir_filter(Elem)                                  -> throw({bad_path, Elem}).

get_cond(Dir, Class, Tag, Filter, NextClass) ->
    case Filter of
        {} ->
            fun(_Fr, _Ind, _Id) -> true end;
        Index when is_integer(Index), Dir =:= fwd ->
            fun(_Fr, Idx, _Id) -> Idx =:= Index end;
        last when Dir =:= fwd ->
            fun(Fr, Idx, _Id) ->
                Read = mnesia:read({linktab(Class), {Fr, Tag}}),
                Idx =:= length(Read)
            end;
        {Ind1, last} when is_integer(Ind1), Dir =:= fwd ->
            fun(_Fr, Idx, _Id) -> Idx >= Ind1 end;
        {Ind1, Ind2} when is_integer(Ind1), is_integer(Ind2), Dir =:= fwd ->
            fun(_Fr, Idx, _Id) -> Idx >= Ind1 andalso Idx < Ind2 end;
        _ ->
            compile_filter(?GET_CLASS_ATTRS(NextClass), NextClass, Filter)
    end.


compile_filter(AttrInfo, Attrs, {'not', Filter}) ->
    F = compile_filter(AttrInfo, Attrs, Filter),
    fun(Fr, Idx, Id) -> not F(Fr, Idx, Id) end;
compile_filter(AttrInfo, Attrs, {Filt1, Op, Filt2})
  when Op =:= 'and' orelse Op =:= 'or' ->
    F1 = compile_filter(AttrInfo, Attrs, Filt1),
    F2 = compile_filter(AttrInfo, Attrs, Filt2),
    case Op of
        'and' -> fun(Fr, Idx, Id) -> F1(Fr,Idx,Id) andalso F2(Fr,Idx,Id) end;
        'or' ->  fun(Fr, Idx, Id) -> F1(Fr,Idx,Id) orelse  F2(Fr,Idx,Id) end
    end;
compile_filter(AttrInfo, Attrs, {Attr, Op, Value})
  when is_atom(Attr) ->
    OpFun = op_to_fun(Op),
    Idx   = indexof(Attr, AttrInfo) + 1,

    Idx =:= 0 andalso
        throw({bad_attribute, Attr}),

    fun (_Fr, _Ind, Id) ->
            [{_,_,Data,_}] = mnesia:read(Attrs, Id, read),
            OpFun(element(Idx, Data), Value)
    end;
compile_filter(_, _, Cond) ->
    throw({bad_condition, Cond}).

op_to_fun('==') -> fun(A,B) -> A =:= B end;
op_to_fun('/=') -> fun(A,B) -> A =/= B end;
op_to_fun('<' ) -> fun(A,B) -> A <   B end;
op_to_fun('=<') -> fun(A,B) -> A =<  B end;
op_to_fun('>' ) -> fun(A,B) -> A >   B end;
op_to_fun('>=') -> fun(A,B) -> A >=  B end.

%% TODO move to ?MISC
indexof(El, Lst) -> indexof(El, Lst, 1).

%% TODO move to ?MISC
indexof(_,  [],        _)   -> -1;
indexof(El, [El | _],  Idx) -> Idx;
indexof(El, [_  | Tl], Idx) -> indexof(El, Tl, Idx+1).

%% TODO: this would probably be more efficient with a table stored in the
%% process state
linktab(Class) ->
    list_to_atom(atom_to_list(Class)++"$lnk").

%%% ----------------------------------------------------------------------------
%%% Node protection

protect_node(Class, Id) ->
    ?Exec(mnesia:dirty_write(#protected{type=node, infos={Class, Id}})).

protect_link(FCl, FId, Tag, TId) ->
    ?Exec(mnesia:dirty_write(#protected{type=link, infos={FCl, FId, Tag, TId}})).

is_protected_node({'$gn', Class, Id}) ->
    NodeInfo = #protected{type=node, infos={Class, Id}},
    [NodeInfo] == ?Exec(mnesia:match_object(NodeInfo)).

is_protected_link(FCl, FId, Tag, TId) ->
    LinkInfo = #protected{type=link, infos={FCl, FId, Tag, TId}},
    [LinkInfo] == ?Exec(mnesia:match_object(LinkInfo)).

%%% ----------------------------------------------------------------------------
%%% Schema queries

%%% ----------------------------------------------------------------------------
%%% backup / restore / undo / redo

backup() ->
    backup(dummy_log).

backup(_) ->
    {ok, checkpoint()}.

save(before_transformation) ->
    case backup() of
	{ok, _} -> ok;
	Else -> Else
    end;
save(_FileName) ->
    {ok, {not_implemented, ?MODULE, handle_save}}.

restore(Count) ->
    case restore_checkpoint(Count) of
        {atomic, _} -> ok;
        Else -> Else
    end.

ls_backups() ->
    list_checkpoints().

backup_info(_Backup) -> % TODO
    {ok, {not_implemented, ?MODULE, handle_backup_info}}.

undo() ->
    {ok, undo_int()}.

undo_int() ->
    IdAkt = mnesia_idakt(),
    Id    = mnesia_id(),
    if
        IdAkt > 0 ->
            restore_checkpoint(Id),
            mnesia:dirty_write({counter, 1, IdAkt - 1}),
            ok;
        true ->
            invalid_checkpoint_number
    end.

redo() ->
    {ok, redo_int()}.

redo_int() ->
    IdAkt = mnesia_idakt(),
    Id    = mnesia_id(),
    if
        IdAkt < Id ->
            restore_checkpoint(IdAkt + 1),
            mnesia:dirty_write({counter, 1, IdAkt + 1}),
            ok;
        true ->
            invalid_checkpoint_number
    end.

clean() ->
    Id = mnesia_id(),
    delete_all_backups(Id).

%%% ---------------------------------------------------------------------------
%%% Checkpoints

-define(NAME, backup).


init_counter() ->
    mnesia:dirty_write({counter,0,0}),
    mnesia:dirty_write({counter,1,0}).

list_checkpoints() ->
    {ok, [get_backup_name(?NAME, N) || N <- lists:seq(1, checkpoint_count())]}. % fixme: lists:seq(1,0) is possible

checkpoint()->
    %%temporary function call
    %% TODO does the above comment mean that this function call is to be removed? why?
    delete_all_backups(1),
    Name = get_backup_name(?NAME, 1),

    Tab   = mnesia:system_info(tables),
    Args  = [{name,Name},{max, Tab},
             {allow_remote,true},{ram_overrides_dump,true}],
    catch mnesia:activate_checkpoint(Args),
    Dir   = mnesia:system_info(directory),
    File  = filename:absname_join(Dir,Name),
    catch mnesia:backup_checkpoint(Name,File),
    mnesia:deactivate_checkpoint(Name),
    mnesia:dirty_write({counter, 0, checkpoint_count() + 1}),
    IdAkt = mnesia_idakt(),
    mnesia:dirty_write({counter, 1, IdAkt + 1}),
    {ok, Name}.

delete_all_backups(0)->
    mnesia:dirty_write({counter, 0, 0}),
    mnesia:dirty_write({counter, 1, 0}),
    {ok, backups_deleted};
delete_all_backups(BackupId)->
    {Name, Id} = get_backup_info(backup, BackupId),
    file:delete(filename:absname_join(mnesia:system_info(directory),Name)),
    delete_all_backups(Id - 1).


delete_end_backups({BackupId, up})->
    {Name, Id} = get_backup_info(backup, BackupId),
    Directory  = mnesia:system_info(directory),
    File       = filename:absname_join(Directory,Name),
    case filelib:is_file(File) of
        true ->
            file:delete(
              filename:absname_join(mnesia:system_info(directory),Name)),
            Dir = up;
        _ ->
            Dir = down
    end,
    delete_end_backups({Id + 1, Dir});
delete_end_backups({Id, down})->
    {Id - 1, deleted}.

restore_checkpoint(BackupInfo)->
    {Name, VsnNum} = get_backup_info(?NAME, BackupInfo),
    Dir            = mnesia:system_info(directory),
    File           = filename:absname_join(Dir,Name),
    mnesia:dirty_write({counter, 1, VsnNum + 1}),
    catch mnesia:restore(File,[{recreate_tables, []},
                               {skip_tables, [counter]}]).


%% Returns the number of checkpoints in the system.
checkpoint_count() ->
    mnesia_id().

%% Returns the assembled backup name.
get_backup_name(Prefix, Vsn) ->
    {Name, _Vsn} = get_backup_info(Prefix, Vsn),
    Name.

%% Combines a name and a version number into a string.
%% The "version number" can be given as an atom;
%% the last number of the textual form of this atom is used.
get_backup_info(Prefix, VsnNum) when is_integer(VsnNum) ->
    Name = ?MISC:format("~p.~p", [Prefix, VsnNum]),
    {Name, VsnNum};
get_backup_info(Prefix, VsnAtom) when is_atom(VsnAtom) ->
    case re:run(atom_to_list(VsnAtom), "[0-9]+", [global,{capture, all, list}]) of
        {match, Matches = [_|_]} ->
            [NumberString] = lists:last(Matches),
            Name = ?MISC:format("~p.~s", [Prefix, NumberString]),
            {Name, list_to_integer(NumberString)};
        _ ->
            throw({backup_has_no_number, VsnAtom})
    end.

mnesia_idakt() ->
    [{_, _, IdAkt}] = mnesia:dirty_read({counter,1}),
    IdAkt.

mnesia_id() ->
    [{_, _, Id}] = mnesia:dirty_read({counter,0}),
    Id.

%%% ---------------------------------------------------------------------------
%%% other unimplemented stuff

create_graph(_Name) ->
    %{stop, {not_implemented, ?MODULE, handle_create_graph}}.
    multi_graph_error().
rename_graph(_OName, _NName) ->
    %{stop, {not_implemented, ?MODULE, handle_rename_graph}}.
    multi_graph_error().
ls_graphs() ->
    %{stop, {not_implemented, ?MODULE, handle_ls_graphs}}.
    multi_graph_error().
actual_graph() ->
    %{stop, {not_implemented, ?MODULE, handle_actual_graph}}.
    multi_graph_error().
load_graph(_Name) ->
    %{stop, {not_implemented, ?MODULE, handle_load_graph}}.
    multi_graph_error().
delete_graph(_Name) ->
    %{stop, {not_implemented, ?MODULE, handle_delete_graph}}.
    multi_graph_error().
delete_all_graphs() ->
    %{stop, {not_implemented, ?MODULE, handle_delete_all_graphs}}.
    multi_graph_error().

multi_graph_error() ->
    {ok, "In mnesia there can be only one graph"}.
