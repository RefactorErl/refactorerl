%% -*- coding: latin-1 -*-

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

%%% @doc The module implements `ets' table analysis. It traverses the
%%% SPG and collects `ets' manipulation information, such table
%%% creation, reading and writing. The analysis assumes that process
%%% analysis has been already executed, and the necessary process
%%% information is available in the SPG and the communication
%%% graph. It extends the graphs with `ets' semantic information.
%%%
%%% @author Melinda Toth <toth_m@inf.elte.hu>

-module(refanal_ets).
-vsn("$Rev: 11223 $ ").
-behaviour(refcore_anal).

%% callback functions
-export([schema/0, externs/1, insert/4, remove/4, update/2]).

-export([add_ets_read_write/0, clean/0]).

-include("core.hrl").

-define(ProcLib, refanal_proc_lib).
-define(PT, processes).

%%% @private
schema() ->
    [{ets_tab, 
      record_info(fields, ets_tab), 
      [{ets_ref, expr}, {ets_def, expr}]},
     {root, [{ets_tab, ets_tab}]}].

%%% @private
externs(_) -> [].

%%% @private
insert(Parent, _Pre, {_Tag, _Child}, _Post) -> 
    case ?Anal:data(Parent) of
        #expr{type = application} ->
            [Fun] = ?Graph:path(Parent, [{fundef, back}]),
            [Mod] = ?Graph:path(Fun, [{func, back}]),
            case {?Graph:data(Fun), ?Graph:data(Mod)} of
                {#func{name=new}, #module{name=ets}} -> 
                    Ets = ?Graph:create(#ets_tab{names=[]}),
                    ?ProcLib:ensure_link(Ets, ets_def, Parent);
                _ -> ok
            end;
        _ -> ok
    end.


%%% @private
remove(_Parent, _Pre, {_Tag, _Child}, _Post) -> ok.

%%% @private
update(_Node,_Data) -> ok.

%% analyse()->
%%     create_ets_tabs_refs().

-spec add_ets_read_write() -> ok.
%% @doc The function discovers the `ets' manipulations and extends both
%% SPG and process communication graph with this information.
add_ets_read_write() ->
%    ets:new(proc_com_tab, [named_table]),
    create_ets_tabs_refs(),
    add_read_write(read, {select, 2}),
    add_read_write(read, {match, 2}),
    add_read_write(write, {insert, 2}).

-spec clean() -> ok.
%% @doc Removes semantic information of `ets' tables from the SPG and
%% communication graph.
clean() ->
    %% cleaning up ets information from the SPG
    EtsTabs = ?Graph:path(?Graph:root(), [ets_tab]),
    [?Graph:rmlink(Tab, Link, To) || Tab <- EtsTabs, 
                                     {Link, To} <- ?Graph:links(Tab)],
    [?Graph:rmlink(From, Link, Tab) || Tab <- EtsTabs, 
                                       {Link, From} <- ?Graph:links(Tab)],
    lists:foreach(fun ?Graph:delete/1, EtsTabs),

    %% cleaning up information from the communication graph
    case ets:info(?PT) of
        undefined -> ok;
        _         ->
            ets:match_delete(?PT, {{'_', {'_', ets_tab, '_'}}, '_', '_', '_'})
    end,
    ok.

add_read_write(Type, {Name, Arity})->
    Fun = ?Query:exec(?Query:seq([?Mod:find(ets), 
                                  ?Mod:local(Name, Arity)])),
    FunRefs = ?Query:exec(Fun, ?Fun:applications()),
    List = lists:flatten([get_ets_proc(FunRef, Type) || FunRef <- FunRefs]),
    lists:foreach(fun({Procs, Ets, Data}) ->
                          [begin
                              ?ProcLib:ensure_link(Proc, Type, Ets),
                              ets:insert(?PT, 
                                         {{Proc, Ets},
                                          ?ProcLib:label(?Graph:data(Proc)),
                                          {Type, ?Syn:flat_text(Data)},
                                          ?ProcLib:label(?Graph:data(Ets))})
                           end  || Proc <- Procs]
                  end, List).

get_ets_proc(Expr, _Type) ->
    Procs = ?ProcLib:get_proc(Expr),
    case {?Graph:path(Expr, [{ets_ref, back}]), 
          ?Graph:path(Expr, [{esub, 2}, {esub, 2}])} of
%% todo: fix it the ary is not always 2
        {[Ets], [Data]} ->  [{Procs, Ets, Data}];
        _ -> []
    end.

create_ets_tabs_refs() -> 
%% TODO: create dummy ets_tab node for unidentified ets refernces
    NewFun = ?Query:exec(?Query:seq([?Mod:find(ets), ?Mod:local(new, 2)])),
    NewRefs = ?Query:exec(NewFun, ?Fun:applications()),
    RefList = [{Ref, is_tabref(reach(Ref))--[Ref]} || Ref <- NewRefs],
    NamePs  = ?Query:exec(NewRefs, 
                          ?Query:seq([?Expr:child(2), ?Expr:child(1)])),
    NamePsOrigin = lists:zip(NewRefs, [origin(Name) || Name <- NamePs]),
    NameRefList = 
        [{New, is_tabref(reach(RefR))--[New]} || {New, RefR} <- NamePsOrigin],
    Names = [{New, lists:map(fun ?ProcLib:atom_value/1, 
                             lists:filter(fun ?ProcLib:is_atom_expr/1,
                                          Orig))} 
             || {New, Orig} <- NamePsOrigin],
    AtomRefs = [{New, is_tabref(reach(?ProcLib:atomnodes(Name)))--[New]} 
                       %% not so efficient...
                 || {New, Name} <- Names],
    References =  merge(RefList, NameRefList, AtomRefs, Names),
    lists:foreach(fun({N, R, Name}) -> 
%% todo handle when References == []
                    Node = ?ProcLib:ensure_node(#ets_tab{names=Name}),
                    ?ProcLib:ensure_link(?Graph:root(), ets_tab, Node),
                    ?ProcLib:ensure_link(Node, ets_def, N),
                    [?ProcLib:ensure_link(Node, ets_ref, To) || To <- R],
                          [ets:insert(?PT, {{P, Node}, 
                                            ?ProcLib:label(?Graph:data(P)), 
                                      create, 
                                      ?ProcLib:label(?Graph:data(Node))}) 
                     || P <- ?ProcLib:get_proc(N)]
                  end, References),
    {RefList, NameRefList, AtomRefs, Names, References}.

merge([{N, H1} | L1], [{N, H2} | L2], [{N, H3} | L3], [{N, H4} | L4]) ->
    [ {N, H1++H2++H3, H4}| merge(L1, L2, L3, L4)];
merge(_, _, _, _) ->
    [].

is_tabref(L) ->
    Fun =  fun(N) ->
                   [{_, ArgL}] = ?Syn:parent(N),
                   [{_, App}]  = ?Syn:parent(ArgL),
                   ets_parent(App, ?Syn:class(App))
           end,
    lists:append(lists:map(Fun, L)).

ets_parent(E, expr) ->
    case ?Query:exec(E, ?Query:seq([?Expr:function(), ?Fun:module()])) of 
    %% dynfun???
        []  -> [];
        [M] -> case ?Mod:name(M) of
                   ets -> [E];
                   _   -> []
               end
    end;
ets_parent(_, _) ->
    [].


reach(N) when is_list(N) -> 
    ?ProcLib:run(fun() -> ?Dataflow:reach(N, [{back, false}]) end);
reach(N) ->
    ?ProcLib:run(fun() -> ?Dataflow:reach([N], [{back, false}]) end).

origin(N) when is_list(N) -> 
    ?ProcLib:run(fun() -> ?Dataflow:reach(N, [{back, true}]) end);
origin(N) ->
    ?ProcLib:run(fun() -> ?Dataflow:reach([N], [{back, true}]) end).

