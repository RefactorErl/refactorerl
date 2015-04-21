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

%%% @doc This is a test module for module `cl_kmeans'.
%%%
%%% @author Petra Krizsai <krizsai@inf.elte.hu>

-module(reftest_cl_kmeans).
-vsn("$Rev: 9568 $").

-export([test/0]).

-include_lib("referl_cluster/include/cluster.hrl").

%% @spec test() -> ok
%% @doc It tests the modul and returns ok if the test passed.
test() ->
    test_mydist(),
    ok = test_calc_min_dist(),
    ok = test_calc_clusters(),
    ok = test_transform_ets(),
    test_calc_new_centroid(),
    test_calc_centroids(),
    test_run_cluster().


test_run_cluster() ->
    A = create_test_attribs(), 
    _Dict = cl_kmeans:run_cluster([{k, 2}, 
                                  {mergefun, fun smart/3}, 
                                  {distfun, fun mydist/4}, 
                                  {entitylist, [a, b, c]}, 
                                  {stoppingcriteria, {unchanged, 4}}, 
                                  {format, dict}],
                                 A),
   % io:format("Result: ~p", [dict:to_list(Dict)])
    ok.

%%  x  y  z
%%a 1  0  0
%%b 0  0  0
%%c 1  1  1
create_test_attribs() ->
    M = cl_matrix:new([a, b, c], [x, y, z], 0),
    M2 = cl_matrix:set(a, x, 1, M),
    M3 = cl_matrix:set(c, x, 1, M2),
    M4 = cl_matrix:set(c, y, 1, M3),
    cl_matrix:set(c, z, 1, M4).

test_calc_min_dist() ->
    M = create_test_attribs(),
    Centroids = [a, c],
    {a, 1} = cl_kmeans:calc_min_dist(b, Centroids, M, fun mydist/4),
    cl_matrix:delete(M),
    ok.

test_calc_clusters() ->
    M = create_test_attribs(),
    Centroids = [a, c],
    Ets = cl_kmeans:calc_clusters(Centroids, M, fun mydist/4, [a, b, c]),
    [{a, a}] = ets:lookup(Ets, a),
    [{b, a}] = ets:lookup(Ets, b),
    [{c, c}] = ets:lookup(Ets, c),
    ets:delete(Ets),
    cl_matrix:delete(M),
    ok.

mydist(_E1, Attr1, _E2, Attr2) ->
    ZippedAttr = lists:zip(Attr1, Attr2),
    lists:foldl(
        fun({{_Entity, V1},{_Entity, V2}}, Sum) ->
            case V1/=V2 of
                true ->
                    Sum + 1;
                false ->
                    Sum
            end
        end, 0, ZippedAttr).

test_mydist() ->
    Attr1 = [{a, 1}, {b, 0}, {c, 1}],
    Attr2 = [{a, 0}, {b, 0}, {c, 0}],
    2 = mydist(undefined, Attr1, undefined, Attr2).

test_transform_ets() ->
    Ets = ets:new(ets, []),
    ets:insert(Ets, {m1, c1}),
    ets:insert(Ets, {m2, c1}),
    ets:insert(Ets, {m3, c2}),
    ets:insert(Ets, {m4, c2}),
    D = cl_kmeans:transform_ets(Ets),
    [m1, m2] = lists:sort(dict:fetch(c1, D)),
    [m3, m4] = lists:sort(dict:fetch(c2, D)),
    ok.

smart(_, AttrLst,_) ->
    cl_mergefun:mapn(
      fun(Attrs) ->
              {Attr, _} = hd(Attrs),
              Vals = [V || {A,V} <- Attrs, A == Attr],
              {Attr,lists:sum(Vals)/length(Vals)}
      end, AttrLst).

test_calc_new_centroid() ->
    A = create_test_attribs(),
    true = (lists:sort([{x, 2/3}, {y, 1/3}, {z, 1/3}]) ==
        lists:sort(cl_kmeans:calc_new_centroid([a, b, c], A, fun smart/3))),
    cl_matrix:delete(A).

test_calc_centroids() ->
    M = create_test_attribs(),
    D = dict:from_list([{a, [a, b, c]}]),

    {Centroids, Matrix} = cl_kmeans:calc_centroids
                            (M, D, fun smart/3),
    [4] = Centroids,
    true = (lists:sort([{x, 2/3}, {y, 1/3}, {z, 1/3}]) ==
        lists:sort(cl_matrix:get_row(4, Matrix))),
    cl_matrix:delete(M),
    ok.
