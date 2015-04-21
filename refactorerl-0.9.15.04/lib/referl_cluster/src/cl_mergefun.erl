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

%%% @doc This module contains functions that calculate the virtual attribute
%%% vector of an entity group from the attribute vectors of the groups'
%%% contents.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(cl_mergefun).
-vsn("$Rev: 10531 $").

-export([sum/3, avg/3, smart/3, print_merged_clusters/3]).

-include_lib("referl_cluster/include/cluster.hrl").

%% @type merge_fun() = (NewLst,[{attr(), value()}],
%%                      {entity(), [{attr(), value()}],
%%                       entity(), [{attr(), value()}]}) -> [{attr(), value()}]

%% @doc Calculate virtual attributes by summing the original attribute values.
sum(_, AttrLst,_) ->
    mapn(
      fun(Attrs) ->
              {Attr, _} = hd(Attrs),
              {Attr, lists:sum([V || {A,V} <- Attrs, A == Attr])}
      end, AttrLst).

%% @doc Calculate virtual attributes by taking the average of the original
%% attribute values.
avg(_, AttrLst,_) ->
    mapn(
      fun(Attrs) ->
              {Attr, _} = hd(Attrs),
              Vals = [V || {A,V} <- Attrs, A == Attr],
              {Attr, lists:sum(Vals) / length(Vals)}
      end, AttrLst).

mapn(_, []) -> [];
mapn(Fun, AttrLst) ->
    [ Fun([Head || [Head|_] <- AttrLst]) |
      mapn(Fun, [Tail || [_|Tail] <- AttrLst, Tail /= []])].

%% @spec print_merged_clusters(info_fun(),merge_fun(),writable_device()) ->
%%           merge_fun()
%%
%%       info_fun() = (entity(), [{attr(), value()}],
%%                     entity(), [{attr(), value()}]) -> term()
%%
%% @doc Generates a merge function that wraps another merge function
%% (`MergeFun').
%% It works as if `MergeFun' was the merge function, but in addition it prints
%% information about the merged clusters.
%% `Infofun' is the function that converts the attributes of the clusters to
%% information that will be printed. It can be, for example, a distance
%% function.
%%
%% Example for usage:
%% ```
%% DistFun = cl_distfun:weight_gen(cl_distfun:pow_size_fun_gen(0.5)),
%% MergeFun = cl_mergefun:print_merged_clusters(DistFun,
%%                                              fun cl_mergefun:smart/3,
%%                                              {stdout,[]}),
%% cl_interface:run_cluster([{distfun, DistFun}, {mergefun, MergeFun}])
%% '''
%% A possible output:
%% ```
%% Updating and loading the attribute matrix...
%% Filtering the attribute matrix...
%% Obtaining the distance function and merge function...
%% Calculating the clusters...
%% 0.873485, [lib], [a1]
%% 0.706144, [a2], [a1,lib]
%% 1.40541, [a3], [a1,a2,lib]
%% Clustering finished.
%% [[[a3,a2,lib,a1]],[[a2,lib,a1],[a3]],[[lib,a1],[a3],[a2]]]
%% '''
print_merged_clusters(_,MergeFun,_) ->
    fun(NewLst, AttrLst,{Gr1,A1,Gr2,A2}) ->
%            D = Infofun(Gr1,A1,Gr2,A2),
%            ?ClOut:fwrite(W,"~p, ",[D]),
%            ?ClOut:fwrite(W,"~p, ",[proplists:get_value(entities,A1,[])]),
%            ?ClOut:fwrite(W,"~p~n",[proplists:get_value(entities,A2,[])]),
            MergeFun(NewLst, AttrLst,{Gr1,A1,Gr2,A2})
    end.

smart(_, AttrLst,_) ->
    mapn(
      fun(Attrs) ->
          {Attr, _} = hd(Attrs),
          lists:foldr(fun({_, Val1},{_, Val2}) ->
               {Attr,
                   ?Matrix:itemize({?Matrix:get_from_value(Val1, func)+
                       ?Matrix:get_from_value(Val2, func),
                   sets:to_list(sets:intersection(
                       sets:from_list(?Matrix:get_from_value(Val1, rec)),
                           sets:from_list(?Matrix:get_from_value(Val2, rec)))),
                   sets:to_list(sets:intersection(
                      sets:from_list(?Matrix:get_from_value(Val1, macro)),
                         sets:from_list(?Matrix:get_from_value(Val2, macro))))})}
                   end, {Attr, ?Matrix:get_default()}, Attrs)
      end, AttrLst).