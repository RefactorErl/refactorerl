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

%%% @doc Print the results of module clustering into dot files.
%%%
%%% @author Kornel Horvath <kornel@inf.elte.hu>

-module(cl_misc).
-vsn("$Rev$").

-include_lib("referl_cluster/include/cluster.hrl").

%% =============================================================================
%% Exports

%% Tuples
-export([swap/1]).
%% List
-export([list_cnt/1]).



%% =============================================================================
%% Tuples

%% @spec swap(tuple() | [tuple()]) -> tuple() | [tuple()]
%% @doc  Swap the elements of 2-tuples.
swap({A,B}) -> {B,A};
swap(TupleList) when is_list(TupleList) -> [swap(T) || T<-TupleList].



%% =============================================================================
%% List

%% @spec list_cnt(List::[term()]) -> [{term(), Count::integer()}]
%% @doc  Return a sorted list of elments of `List' where same elements are
%%       present only once. The elements are zipped into a tuple with their
%%       frequency.
list_cnt(List) when is_list(List) ->
    dict:to_list(lists:foldl(fun(Elem,D) -> dict:update_counter(Elem,1,D) end,
                             dict:new(), List)).


