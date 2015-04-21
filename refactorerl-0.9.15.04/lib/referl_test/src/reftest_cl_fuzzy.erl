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

%%% @doc This is a test module for module `cl_fuzzy_cmeans'.
%%%
%%% @author Petra Krizsai <krizsai@inf.elte.hu>

-module(reftest_cl_fuzzy).
-vsn("$Rev: 9568 $").

-export([test/0]).

-include_lib("referl_cluster/include/cluster.hrl").

%% @spec test() -> ok
%% @doc It tests the modul and returns ok if the test passed.
test() ->
     test_calc_coeff_matrix(),
     test_convert_to_non_fuzzy(),
     test_calc_centroids(),
     test_run_cluster(),
     ok.

test_calc_coeff_matrix() ->
    Attribs = create_test_attribs(),
    Centroids = [c, d],
    Entities = [a, b],
    cl_fuzzy_c_means:calc_coeff_matrix(Centroids, Entities, fun cl_distfun:euclidian/4, Attribs, 2).

test_convert_to_non_fuzzy() ->
    Attribs = create_test_attribs(),
    Centroids = [c, d],
    Entities = [a, b],
    Coeff = cl_fuzzy_c_means:calc_coeff_matrix(Centroids, Entities, fun cl_distfun:euclidian/4, Attribs, 2),
    Ets = cl_fuzzy_c_means:convert_to_nonfuzzy(Centroids, Coeff),
    ets:tab2list(Ets).

test_calc_centroids() ->
    Attribs = create_test_attribs(),
    Centroids = [c, d],
    Entities = [a, b],
    Coeff = cl_fuzzy_c_means:calc_coeff_matrix(Centroids, Entities, fun cl_distfun:euclidian/4, Attribs, 2),
    cl_fuzzy_c_means:calc_centroids(Attribs, Coeff, Entities, Centroids, 2).

test_run_cluster() ->
    Attribs = create_test_attribs(),
    cl_fuzzy_c_means:run_cluster([{k, 2},
                                  {distfun, fun cl_distfun:euclidian/4},
                                  {stoppingcriteria, {iterations, 10}},
                                  {m, 2},
                                  {format, matrix},
                                  {entitylist, [c, d]},
                                  {initcentroids, [a, b]}], Attribs).

%%  x  y
%%a 1  10
%%b 1  4
create_test_attribs() ->
    M = cl_matrix:new([a, b, c, d], [x, y], 0),
    M2 = cl_matrix:set(a, x, 1, M),
    M3 = cl_matrix:set(b, x, 2, M2),
    M4 = cl_matrix:set(c, y, 1, M3),
    cl_matrix:set(d, y, 2, M4).

% todo Dialyzer claims this function is unused.
%      Remove if true, refute otherwise.
% mydist(_E1, Attr1, _E2, Attr2) ->
%     ZippedAttr = lists:zip(Attr1, Attr2),
%     lists:foldl(
%         fun({{_Entity, V1},{_Entity, V2}}, Sum) ->
%             case V1/=V2 of
%                 true ->
%                     Sum + 1;
%                 false ->
%                     Sum
%             end
%         end, 0, ZippedAttr).
