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

%%% @doc This module can convert the graph (stored in the Mnesia database) to
%%% graph that is stored it ets (this "ets graph" is called "egraph").
%%%
%%% @todo Write the precise type of filter_fun()
%%% @todo The filter function part of the refac_draw_graph module should be put
%%% into a new module, so it could be used with this module, as well.
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(reflib_egraph).
-vsn("$Rev: 9568 $").

-export([create_egraph/0, create_egraph/1]).

-include("lib.hrl").

%%% @type egraph() = ets({node(), {data(), ListOfLinks::[{tag(),node()}]}}).
%%%
%%% A graph that represents the graph that is in the Mnesia database of the
%%% tool.
%%% The types `node()', `data()' and `tag()' are described in the documentation
%%% of the `refac_graph' module.

%% @spec create_egraph() -> egraph()
%%
%% @doc Converts the graph in the database into egraph with the default options.
%% Same as `create_egraph([])'.
create_egraph() ->
    create_egraph([]).

%% @spec create_egraph(atom()) -> egraph()
%%
%% @doc Converts the graph in the database into egraph with edge filtering.
%% To not use filtering, invoke this function with `all' as `Filter'.
%%
%% Options:
%% <ul>
%%      <li>`ets_name': the name of the ets table that will be created.
%%          Default value: `egraph'.</li>
%%      <li>`filter': the filtering that will be used.
%%          Default value: `all'.</li>
%% </ul>
%%
%% @todo Maybe also the back edges should be included in the graph?
create_egraph(Options) ->
    Opts = cl_utils:proplist_update(create_egraph_default(),Options),
    E = ets:new(proplists:get_value(ets_name,Opts),[]),
    create_egraph(E, ?Graph:root(),
                  filter(proplists:get_value(filter,Opts))),
    E.

create_egraph_default() ->
    [{ets_name,egraph},
     {filter,all}].

%% @spec create_egraph(egraph(), node(), filter_fun()) -> ok
%%
%% @doc Adds `Node' and all the connected nodes recursively to `Ets' 
%% (it does not add a node if it is already in `Ets').
create_egraph(Ets, Node, Filter) ->
    case ets:lookup(Ets, Node) of
        [] ->
            Data = ?Graph:data(Node),
            Links = [Link || Link = {Tag, _To} <- ?Graph:links(Node),
                             Filter(element(1, Data), Tag)],
            ets:insert(Ets, {Node, {Data, Links}}),
            lists:foldl(fun ({_Tag, To}, _) ->
                                create_egraph(Ets, To, Filter)
                        end, 
                        ok,
                        Links),
            ok;
        _ -> 
            ok
    end.

filter(all) ->
    fun all_links/2.

all_links(_, _) -> 
    true.
