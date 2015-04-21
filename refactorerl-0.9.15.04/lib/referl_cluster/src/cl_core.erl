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
%%% The Initial Developer of the Original Code is E�tv�s Lor�nd University.
%%% Portions created  by E�tv�s Lor�nd University and ELTE-Soft Ltd.
%%% are Copyright 2007-2013 E�tv�s Lor�nd University, ELTE-Soft Ltd.
%%% and Ericsson Hungary. All Rights Reserved.

%%% @doc Generic clustering implementation. Implements the main algorithmic
%%% steps of clustering and some specific clustering methods.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Aniko Nagyne Vig <viganiko@inf.elte.hu>
%%% @author Hanna Kollo <khi@inf.elte.hu>

-module(cl_core).
-vsn("$Rev: 9609 $").

-include_lib("../include/cluster.hrl").

-export([attribs/2, filter/3, transform/2, transform2/2, distances/2,
         agglom_dist/2, agglom_attr/3]).
-export([dist_to_file/2]).

%%% @type dist_fun() = (entity(), [{attr(), value()}],
%%%                     entity(), [{attr(), value()}]) -> number().

%%% @type merge_fun() = (NewLst, [{attr(), value()}],
%%%                      {entity(), [{attr(), value()}],
%%%                       entity(), [{attr(), value()}]}) -> 
%%%                     [{attr(), value()}].

%% @spec attribs([{entity(), [{attr(), value()}]}], value()) -> attribs()
%%
%% @doc Builds an attribute matrix from sparse attribute data.
attribs(Data, Default) ->
    Flat = [{E, A, V} || {E, AV} <- Data, {A, V} <- AV],
    Ent = lists:usort([E || {E, _, _} <- Flat]),
    Att = lists:usort([A || {_, A, _} <- Flat]),
    Empty = ?Matrix:new(Ent, Att, Default),
    lists:foldl(fun({E, A, V}, M) -> ?Matrix:set(E, A, V, M) end, Empty, Flat).

%% @spec filter(attribs(), [(entity(), [{attr(), value()}]) -> bool()],
%%                         [(attr(), [{entity(), value()}]) -> bool()]) ->
%%              attribs()
%%
%% @doc Filters the contents of an attribute matrix using the specified
%% entity and attribute filters.
filter(Attribs, EntityFilt, AttribFilt) ->
    A1 = lists:foldl(fun filter_rows/2, Attribs, EntityFilt),
    lists:foldl(fun filter_cols/2, A1, AttribFilt).

filter_rows(Filter, Attribs) ->
    filter(Filter, Attribs,
           fun ?Matrix:get_row/2, fun ?Matrix:del_row/2,
           ?Matrix:rows(Attribs)).

filter_cols(Filter, Attribs) ->
    filter(Filter, Attribs,
           fun ?Matrix:get_col/2, fun ?Matrix:del_col/2,
           ?Matrix:cols(Attribs)).

filter(Filter, Matrix, GetElem, DelElem, Elements) ->
    lists:foldl(
      fun(Elem, M) ->
              case Filter(Elem, GetElem(Elem, M)) of
                  true -> DelElem(Elem, M);
                  false -> M
              end
      end,
      Matrix, Elements).

%% @spec transform(attribs(), MapFun) -> attribs()
%%           MapFun = (value1()) -> value2()
%%
%% @doc Transforms the contents of an attribute matrix to another
%% representation using the given mapping.
transform(Attribs, MapFun) ->
    ?Matrix:transform(MapFun, Attribs).

%% @spec transform2(attribs(), MapFun) -> attribs()
%%           MapFun = (attrib(), entity(), value1()) -> value2()
%%
%% @doc Transforms the contents of an attribute matrix to another
%% representation using the given mapping.
transform2(Attribs, MapFun) ->
    ?Matrix:transform2(MapFun, Attribs).

%% @spec distances(attribs(), dist_fun()) -> distances()
%%
%% @doc Calculates the entity distance matrix from an attribute matrix using
%% the given distance function.
distances(Attribs, DistFun) ->
    Ent = ?Matrix:rows(Attribs),
    Rows = [{E, ?Matrix:get_row(E, Attribs)} || E <- Ent],
    Empty = ?Matrix:new(Ent, Ent, []),
    calc_distances(Rows, Empty, DistFun).

calc_distances([], Dists, _) ->
    Dists;
calc_distances([{Ent, Attrs}|Tail], Dists, DistFun) ->
    calc_distances(
      Tail,
      calc_distances(Ent, Attrs, Tail, Dists, DistFun),
      DistFun).

calc_distances(From, FromAttrs, ToList, OrigDists, DistFun) ->
    lists:foldl(
      fun ({To, ToAttrs}, Dists) ->
              Dist = DistFun(From, FromAttrs, To, ToAttrs),
              D1 = ?Matrix:set_ets(From, To, Dist, Dists, all),
              ?Matrix:set_ets(To, From, Dist, D1, all)
      end,
      OrigDists, ToList).

%% @spec agglom_dist(distances(), GroupFun) -> Clusters
%%       GroupFun = (Gr1, Gr2, Other, Gr1Gr2Dst, Gr1ODst, Gr2ODst) -> NewDist
%%       Clusters = [entity()|Clusters]
%%
%% @doc Performs clustering using the agglomerative algorithm, starting from
%% a given entity distance matrix.
agglom_dist(Dists, DistGroupFun) ->
    {Data, Clusters} = agglom(Dists, dst_start(Dists, DistGroupFun),
                              fun dst_new_group/3, fun dst_group_dist/4),
    dst_result(Data, Clusters).

-record(aggdst, {groups, orig, grpfun}).

dst_start(Dists, DistGroupFun) ->
    #aggdst{groups=dict:from_list([{Ent, [Ent]} || Ent <- ?Matrix:rows(Dists)]),
            orig=dict:new(),
            grpfun=DistGroupFun}.

dst_new_group(#aggdst{groups=Groups, orig=Orig}=Data, Gr1, Gr2) ->
    New = dict:size(Groups) + 1,
    Gr1Lst = dict:fetch(Gr1, Groups),
    Gr2Lst = dict:fetch(Gr2, Groups),
    NewGroups = dict:store(New, Gr1Lst ++ Gr2Lst, Groups),
    NewOrig = dict:store(New, {Gr1, length(Gr1Lst), Gr2, length(Gr2Lst)}, Orig),
    {New, Data#aggdst{groups=NewGroups, orig=NewOrig}}.

dst_group_dist(New, Grp,
               #aggdst{groups=Groups, orig=Orig, grpfun=DistFun}, Dists) ->
    {Gr1, Gr1Len, Gr2, Gr2Len} = dict:fetch(New, Orig),
    DistFun(Gr1Len, Gr2Len, length(dict:fetch(Grp, Groups)),
            ?Matrix:get(Gr1, Gr2, Dists),
            ?Matrix:get(Gr1, Grp, Dists),
            ?Matrix:get(Gr2, Grp, Dists)).

dst_result(#aggdst{groups=Groups}, Clusters) ->
    [ [ dict:fetch(Grp, Groups) || Grp <- Cls] || Cls <- Clusters].

%% @spec agglom_attr(attribs(), dist_fun(), merge_fun()) -> Clusters
%%       Clusters = [entity()]
%%
%% @doc Performs clustering using the agglomerative algorithm, starting from
%% a given attribute matrix, calculating entity distances using this matrix.
agglom_attr(Attribs, DistFun, MergeFun) ->
    case ?Matrix:rows(Attribs) of
        [] ->
            [];
        _ ->
            {Data, Clusters} = agglom(distances(Attribs, DistFun),
                attr_start(Attribs, DistFun, MergeFun),
                fun attr_new_group/3,
                fun attr_group_dist/4),
            attr_result(Data, Clusters)
    end.

-record(aggattr, {attrs, groups, distfun, mergefun}).

attr_start(Attribs, DistFun, MergeFun) ->
    #aggattr{attrs=Attribs,
             groups=dict:from_list([{Ent, [Ent]} || Ent <- ?Matrix:rows(Attribs)]),
             distfun=DistFun, mergefun=MergeFun}.

attr_new_group(#aggattr{attrs=Attribs, groups=Groups, mergefun=MergeFun}=Data,
               Gr1, Gr2) ->
    New = dict:size(Groups) + 1,
    NewLst = dict:fetch(Gr1, Groups) ++ dict:fetch(Gr2, Groups),
    NewGroups = dict:store(New, NewLst, Groups),
    NewAttrs = ?Matrix:add_row(New, Attribs),
    VeryNewAttrs = ?Matrix:add_col(New, NewAttrs),
    FilledAttrs =
        lists:foldl(fun({Col, Val}, Attr) ->
            ?Matrix:set_ets(New, Col, Val, Attr, all),
            ?Matrix:set_ets(Col, 
                        New, 
                        ?Matrix:change_value(Val, 
						                     func,
                                             sumFuns(Col, NewLst, Attr)),
                        Attr,
                        all)
        end,
                    VeryNewAttrs,
                    MergeFun(NewLst,
                             [?Matrix:get_row(Gr, Attribs) || Gr <- NewLst],
                             {Gr1, ?Matrix:get_row(Gr1, Attribs),
                              Gr2, ?Matrix:get_row(Gr2, Attribs)})),
    {New, Data#aggattr{attrs=FilledAttrs, groups=NewGroups}}.
    
sumFuns(Col, News, Attr) ->
    lists:foldl(fun(Gr, Acc) -> 
        Acc + ?Matrix:get_from_ets(Col, Gr, Attr, func) end, 0, News).

attr_group_dist(New, Grp,
                #aggattr{attrs=Attribs, distfun=DistFun}, _Dists) ->
    DistFun(Grp, ?Matrix:get_row(Grp, Attribs),
            New, ?Matrix:get_row(New, Attribs)).

attr_result(#aggattr{groups=Groups}, Clusters) ->
    [ [ dict:fetch(Grp, Groups) || Grp <- Cls] || Cls <- Clusters].

%%% ======================================================================
%%% Generic agglomerative algorithm

agglom(Dists, GroupData, GroupFun, DistFun) ->
    Result = agglom(?Matrix:rows(Dists), Dists, GroupData, GroupFun, DistFun, []),
    ?Matrix:delete(Dists),
    Result.

agglom([_], _, GroupData, _, _, Clusters) -> {GroupData, Clusters};
agglom(Groups, Dists, GroupData, GroupFun, DistFun, Clusters) ->
    % Find two groups to be merged
    {Gr1, Gr2, _} = find_min(Dists),
    % Create new group
    {New, NewGroupData} = GroupFun(GroupData, Gr1, Gr2),
    NewGroups = [New | Groups -- [Gr1, Gr2]],
    % Calculate distances for the new group
    NewDists = group_dists(New, NewGroups, NewGroupData, Dists, DistFun),
    CleanDists = remove_groups([Gr1, Gr2], NewDists),
    agglom(NewGroups, CleanDists, NewGroupData,
           GroupFun, DistFun,
           [NewGroups | Clusters]).

group_dists(New, Groups, GroupData, OrigDists, DistFun) ->
    GrownDists = ?Matrix:add_row(New, ?Matrix:add_col(New, OrigDists)),
    lists:foldl(
      fun(Grp, Dists) ->
              NewDist = DistFun(New, Grp, GroupData, Dists),
              D2 = ?Matrix:set_ets(New, Grp, NewDist, Dists, all),
              ?Matrix:set_ets(Grp, New, NewDist, D2, all)
      end,
      GrownDists,
      Groups -- [New]).

find_min(Matrix) ->
    lists:foldl(
      fun(Col, Min0) ->
              ?Matrix:fold_col(
                 fun
                 (Row, Val, undefined) ->
                                   {Row, Col, Val};
                 (Row, Val, {_, _, MinVal}) when Val < MinVal ->
                                   {Row, Col, Val};
                 (_, _, Min) ->
                                   Min
                           end,
                 Min0, Col, Matrix)
      end,
      undefined, ?Matrix:cols(Matrix)).

remove_groups(Groups, Dists) ->
    lists:foldl(fun (G, D) -> ?Matrix:del_row(G, ?Matrix:del_col(G, D)) end,
                Dists, Groups).

%%% ======================================================================
%%% R frontend

%% In R, you can use the following command to perform K-means:
%% kmeans(read.table(file("<<filename>>"), header = TRUE, row.names = 1), <<K>>)

%% @spec dist_to_file(distances(), string()) -> atom()
%%
%% @doc Writes the distace matrix to a text file, which can be used by R.
dist_to_file(Dists, FileName) ->
    {ok, S} = file:open(FileName, [write]),
    lists:foreach(fun(ColName) -> io:format(S, "\t~s", [atom_to_list(ColName)])
                  end,
                  ?Matrix:cols(Dists)),
    lists:foreach(fun(RowName) -> 
                          io:format(S, "~n~s", [RowName]),
                          lists:foreach(
                            fun(Attrib) ->
                                    io:format(S, "\t~w", [valueofattr(Attrib)])
                            end,
                            ?Matrix:get_row(RowName, Dists)) 
                  end,
                  ?Matrix:rows(Dists)),
    file:close(S),
    ok.

valueofattr({_, []}) -> 0;
valueofattr({_, N}) -> N.

