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
%%% ============================================================================
%%% Module information

%%% @doc Functions to call igraph.

%%% @author Szabo Bence <szbtadi@caesar.elte.hu>

-module(refusr_igraph).
-vsn("$Rev$").

-include("user.hrl").
-include("refusr_clone_identifierl.hrl").

-export([maximal_cliques_erlang/1,maximal_cliques_to_file/1, maximal_cliques/1]).

% @private
-export([search_maximal_cliques/1, search_maximal_cliques_to_file/1]).

-define(IGRAPH_NAME,
        filename:join([?MISC:data_dir(), "clone_identifierl_igraph"++
                        ?Lib:current_time_str()])).

-ifdef(igraph).

-on_load(load_my_nifs/0).

load_my_nifs() ->
        case nif_load() of
        {error, Rnle} -> throw(Rnle);
        ok -> ok
    end.

nif_load() ->
    Nif = filename:join(code:lib_dir(referl_user, priv), "igraph"),
    erlang:load_nif(Nif, 0).


%% @doc Calls the Igraph source file to calculate the maximal cliques.
maximal_cliques_to_file(ClonesAsEdges) ->
    EdgeString = lists:flatmap(fun(K) -> io_lib:format("~p ",[K]) end, ClonesAsEdges),
    
    FileName = ?IGRAPH_NAME,
    {ok,IODev} = file:open(FileName, [write]),
    io:put_chars(IODev, EdgeString),
    ok = file:close(IODev),
    
    search_maximal_cliques_to_file(FileName),
    {ok, ParsedList} = file:consult(FileName),
    file:delete(FileName),
    ParsedList.

maximal_cliques(IgraphList) ->
    search_maximal_cliques(IgraphList).

-else.

%% Calls the erlang implementation of maximal clique search.
maximal_cliques_to_file(ClonesAsEdges) ->
    maximal_cliques_erlang(ClonesAsEdges).

maximal_cliques(ClonesAsEdges) ->
    maximal_cliques_erlang(ClonesAsEdges).

-endif.

%% @doc Creates and undirected graph and claculates the maximal cliques.
maximal_cliques_erlang([]) -> [];
maximal_cliques_erlang(ClonesAsEdges) ->
    OrdEdges = ordsets:from_list(ClonesAsEdges),
    Edges = ets:new(max_cliq_ets,[set,public]),
    insert_to_ets(Edges, ClonesAsEdges),
    Result = bk_std_gp(Edges,[],OrdEdges,[],[]),
    ets:delete(Edges),
    lists:usort(Result).


%% @doc Inserts the edges to an ets table to form an undirected graph.
insert_to_ets(Name, []) -> Name;
insert_to_ets(Name, [From, To | Edges]) ->
    case ets:lookup(Name, From) of
        [] -> ets:insert(Name, {From, [To]});
        [{_,Edge}] -> ets:update_element(Name, From, {2, Edge ++ [To]})
    end,
    case ets:lookup(Name, To) of
        [] -> ets:insert(Name, {To, [From]});
        [{_,Edge2}] -> ets:update_element(Name, To, {2, Edge2 ++ [From]})
    end,
    insert_to_ets(Name, Edges).


%% @doc Returns the neighbours of a Node form the ets table.
get_neighbours(Ets, Node) -> 
    ordsets:from_list(
        ets:lookup_element(Ets, Node, 2)).


% Standard BK algorithm
% bk_std(_G, Clique, [], [], Res) ->
%    [Clique] ++ Res;
% bk_std(_G, _Clique, [], _Not, Res) -> Res;
% bk_std(G, Clique, [Cand | Cands], Not, Res) ->
%     NodeNeighbours = get_neighbours(G, Cand),
%     NewClique = ordsets:add_element(Cand, Clique),
%     NewCand = ordsets:intersection(Cands, NodeNeighbours),
%     NewNot = ordsets:intersection(Not, NodeNeighbours),

%     Res1 = bk_std(G, NewClique, NewCand, NewNot, Res),
%     NNot = ordsets:add_element(Cand, Not),

%     bk_std(G, Clique, Cands, NNot, Res1).
    

% Standarb BK algorithm with greedy pivot selection
bk_std_gp(_G,R,[],[],Res) ->
    [R] ++ Res;
bk_std_gp(G,R,P,X,Res) ->
    Pivot = select_greedy_pivot(G,P,X),
    NewP = ordsets:subtract(P, ordsets:from_list(get_neighbours(G,Pivot))),
    bk0_std_gp(G,R,P,NewP,X,Pivot,Res).

bk0_std_gp(_G,_R,_P,[],_X,_Pivot,Res) -> Res;
bk0_std_gp(G,R,P,[H|Ps],X,Pivot,Res) ->
    DP = ordsets:del_element(H, P),
    OrdNeighbours = ordsets:from_list(get_neighbours(G, H)),
    NewR = ordsets:add_element(H, R),
    NewP = ordsets:intersection(DP, OrdNeighbours),
    NewX = ordsets:intersection(X, OrdNeighbours),

    Res1 = bk_std_gp(G,NewR,NewP,NewX,Res),
    NX = ordsets:add_element(H, X),

    bk0_std_gp(G,R,DP,Ps,NX,Pivot, Res1).


select_greedy_pivot(G,P,X) ->
    PUX = ordsets:union(P,X),
    Degrees =
        lists:map(fun(K) ->
            Deg = length(ordsets:intersection(
                     ordsets:from_list(get_neighbours(G, K)),
                     P)),
            {Deg,K}
        end, PUX),
    
    {_, Id} = lists:max(Degrees),
    Id.


search_maximal_cliques_to_file(_InputFilePath) ->
    "Not loaded!".
search_maximal_cliques(_IgraphList) ->
    "Not loaded!".


