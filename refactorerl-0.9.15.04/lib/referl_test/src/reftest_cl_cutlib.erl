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

%%% @doc This is a test module for module `cl_cutlib'.
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(reftest_cl_cutlib).
-vsn("$Rev: 9568 $").

-export([test/0]).

%% @spec test() -> ok
%%
%% @doc It tests the modul and returns ok if the test passed.
test() ->
    ok = test_cut_lib_1(),
    ok = test_cut_lib_2().

%% @doc Contains only functions, does not contain macros and records.
test_cut_lib_1() ->
    %% test input
    MA =
        [{a1,[{entities,[a1]},
              {size,1},
              {{fun_attr,lib,f2,0},1}]},
         {lib,[{entities,[lib]},
               {size,1},
               {{fun_attr,lib,f6,0},2},
               {{fun_attr,lib,f7,0},2},
               {{fun_attr,lib,f2,0},1},
               {{fun_attr,lib,f3,0},2}]},
         {a2,[{entities,[a2]},
              {size,1},
              {{fun_attr,lib,f5,0},1},
              {{fun_attr,lib,f1,0},1},
              {{fun_attr,lib,f2,0},1}]},
         {a3,[{entities,[a3]},
              {size,1},
              {{fun_attr,lib,f4,0},1},
              {{fun_attr,lib,f5,0},1}]}],
    ME = 0,
    FA =
        [{{fun_attr,lib,f4,0},
          [{entities,[{fun_attr,lib,f4,0}]},
           {size,1},
           {{fun_attr,lib,f6,0},1},
           {{fun_attr,lib,f7,0},1}]},
         {{fun_attr,lib,f5,0},
          [{entities,[{fun_attr,lib,f5,0}]},{size,1}]},
         {{fun_attr,lib,f6,0},
          [{entities,[{fun_attr,lib,f6,0}]},{size,1}]},
         {{fun_attr,a2,f,0},
          [{entities,[{fun_attr,a2,f,0}]},
           {size,1},
           {{fun_attr,lib,f5,0},1},
           {{fun_attr,lib,f1,0},1},
           {{fun_attr,lib,f2,0},1}]},
         {{fun_attr,lib,f7,0},
          [{entities,[{fun_attr,lib,f7,0}]},{size,1}]},
         {{fun_attr,a3,f,0},
          [{entities,[{fun_attr,a3,f,0}]},
           {size,1},
           {{fun_attr,lib,f4,0},1},
           {{fun_attr,lib,f5,0},1}]},
         {{fun_attr,lib,f1,0},
          [{entities,[{fun_attr,lib,f1,0}]},
           {size,1},
           {{fun_attr,lib,f6,0},1},
           {{fun_attr,lib,f2,0},1},
           {{fun_attr,lib,f3,0},1}]},
         {{fun_attr,lib,f2,0},
          [{entities,[{fun_attr,lib,f2,0}]},
           {size,1},
           {{fun_attr,lib,f3,0},1}]},
         {{fun_attr,lib,f3,0},
          [{entities,[{fun_attr,lib,f3,0}]},
           {size,1},
           {{fun_attr,lib,f7,0},1}]},
         {{fun_attr,a1,f,0},
          [{entities,[{fun_attr,a1,f,0}]},
           {size,1},
           {{fun_attr,lib,f2,0},1}]}],
    FE = 0,
    Objects = [{fun_attr,lib,f1,0},
               {fun_attr,lib,f2,0},
               {fun_attr,lib,f3,0},
               {fun_attr,lib,f4,0},
               {fun_attr,lib,f5,0},
               {fun_attr,lib,f6,0},
               {fun_attr,lib,f7,0}],
    Clusters = [[a1,a2],[a3]],

    %% expected test output
    ExpectedResult =
        {[{fun_attr,lib,f5,0},
          {fun_attr,lib,f6,0},
          {fun_attr,lib,f7,0}],
         [{0,[a1,a2],
           [
            {fun_attr,lib,f1,0},
            {fun_attr,lib,f2,0},
            {fun_attr,lib,f3,0}]},
          {1,[a3],[{fun_attr,lib,f4,0}]}]},

    %% test
    ModAttribs = cl_core:attribs(MA,ME),
    FunAttribs = cl_core:attribs(FA,FE),

    Result = cut_lib_2(ModAttribs,FunAttribs,Clusters,Objects),
    true = (sort_cutlib_result_nice(Result) ==
            sort_cutlib_result_nice(ExpectedResult)),

    %% cleanup
    cl_matrix:delete(ModAttribs),
    cl_matrix:delete(FunAttribs),
    ok.

test_cut_lib_2() ->
    MA = [{a1,[{entities,[a1]},
               {size,1},
               {{rec_attr,"/a/x/ref/trunk/test/h.hrl",rh1},1},
               {{rec_attr,"/a/x/ref/trunk/test/h.hrl",r_a1},1},
               {{fun_attr,lib,f2,0},1}]},
          {lib,[{entities,[lib]},
                {size,1},
                {{rec_attr,"/a/x/ref/trunk/test/lib.erl",r3},1},
                {{rec_attr,"/a/x/ref/trunk/test/h.hrl",rh2},2},
                {{fun_attr,lib,f6,0},2},
                {{fun_attr,lib,f7,0},2},
                {{rec_attr,"/a/x/ref/trunk/test/lib.erl",r2},2},
                {{rec_attr,"/a/x/ref/trunk/test/h.hrl",rh1},2},
                {{rec_attr,"/a/x/ref/trunk/test/lib.erl",r1},2},
                {{fun_attr,lib,f2,0},1},
                {{fun_attr,lib,f3,0},2}]},
          {a2,[{entities,[a2]},
               {size,1},
               {{fun_attr,lib,f5,0},1},
               {{fun_attr,lib,f1,0},1},
               {{fun_attr,lib,f2,0},1}]},
          {a3,[{entities,[a3]},
               {size,1},
               {{fun_attr,lib,f4,0},1},
               {{fun_attr,lib,f5,0},1}]}],
    ME = 0,
    FA = 
        [{{fun_attr,lib,f4,0},
          [{entities,[{fun_attr,lib,f4,0}]},
           {size,1},
           {{rec_attr,"/a/x/ref/trunk/test/lib.erl",r3},1},
           {{rec_attr,"/a/x/ref/trunk/test/h.hrl",rh2},1},
           {{fun_attr,lib,f6,0},1},
           {{fun_attr,lib,f7,0},1},
           {{rec_attr,"/a/x/ref/trunk/test/lib.erl",r2},1}]},
         {{fun_attr,lib,f5,0},
          [{entities,[{fun_attr,lib,f5,0}]},{size,1}]},
         {{fun_attr,lib,f6,0},
          [{entities,[{fun_attr,lib,f6,0}]},{size,1}]},
         {{fun_attr,a2,f,0},
          [{entities,[{fun_attr,a2,f,0}]},
           {size,1},
           {{fun_attr,lib,f5,0},1},
           {{fun_attr,lib,f1,0},1},
           {{fun_attr,lib,f2,0},1}]},
         {{fun_attr,lib,f7,0},
          [{entities,[{fun_attr,lib,f7,0}]},{size,1}]},
         {{fun_attr,a3,f,0},
          [{entities,[{fun_attr,a3,f,0}]},
           {size,1},
           {{fun_attr,lib,f4,0},1},
           {{fun_attr,lib,f5,0},1}]},
         {{fun_attr,lib,f1,0},
          [{entities,[{fun_attr,lib,f1,0}]},
           {size,1},
           {{rec_attr,"/a/x/ref/trunk/test/h.hrl",rh2},1},
           {{fun_attr,lib,f6,0},1},
           {{rec_attr,"/a/x/ref/trunk/test/lib.erl",r2},1},
           {{rec_attr,"/a/x/ref/trunk/test/h.hrl",rh1},1},
           {{rec_attr,"/a/x/ref/trunk/test/lib.erl",r1},1},
           {{fun_attr,lib,f2,0},1},
           {{fun_attr,lib,f3,0},1}]},
         {{fun_attr,lib,f2,0},
          [{entities,[{fun_attr,lib,f2,0}]},
           {size,1},
           {{rec_attr,"/a/x/ref/trunk/test/h.hrl",rh1},1},
           {{rec_attr,"/a/x/ref/trunk/test/lib.erl",r1},1},
           {{fun_attr,lib,f3,0},1}]},
         {{fun_attr,lib,f3,0},
          [{entities,[{fun_attr,lib,f3,0}]},
           {size,1},
           {{fun_attr,lib,f7,0},1}]},
         {{fun_attr,a1,f,0},
          [{entities,[{fun_attr,a1,f,0}]},
           {size,1},
           {{rec_attr,"/a/x/ref/trunk/test/h.hrl",rh1},1},
           {{rec_attr,"/a/x/ref/trunk/test/h.hrl",r_a1},1},
           {{fun_attr,lib,f2,0},1}]}],
    FE = 0,
    Objects = [{fun_attr,lib,f1,0},
               {fun_attr,lib,f2,0},
               {fun_attr,lib,f3,0},
               {fun_attr,lib,f4,0},
               {fun_attr,lib,f5,0},
               {fun_attr,lib,f6,0},
               {fun_attr,lib,f7,0},
               {rec_attr,"/a/x/ref/trunk/test/lib.erl",r1},
               {rec_attr,"/a/x/ref/trunk/test/lib.erl",r2},
               {rec_attr,"/a/x/ref/trunk/test/lib.erl",r3},
               {rec_attr,"/a/x/ref/trunk/test/h.hrl",rh1},
               {rec_attr,"/a/x/ref/trunk/test/h.hrl",rh2},
               {rec_attr,"/a/x/ref/trunk/test/h.hrl",rh3},
               {rec_attr,"/a/x/ref/trunk/test/h.hrl",r_a1}],
    Clusters = [[a1,a2],[a3]],

    ExpectedResult =
        {[{fun_attr,lib,f5,0},
          {fun_attr,lib,f6,0},
          {fun_attr,lib,f7,0},
          {rec_attr,"/a/x/ref/trunk/test/lib.erl",r2},
          {rec_attr,"/a/x/ref/trunk/test/h.hrl",rh2},
          {rec_attr,"/a/x/ref/trunk/test/h.hrl",rh3}],
         [{0,[a1,a2],
           [{rec_attr,"/a/x/ref/trunk/test/lib.erl",r1},
            {rec_attr,"/a/x/ref/trunk/test/h.hrl",r_a1},
            {rec_attr,"/a/x/ref/trunk/test/h.hrl",rh1},
            {fun_attr,lib,f1,0},
            {fun_attr,lib,f2,0},
            {fun_attr,lib,f3,0}]},
          {1,[a3],
           [{fun_attr,lib,f4,0},
            {rec_attr,"/a/x/ref/trunk/test/lib.erl",r3}]}]},

    Clusters = [[a1,a2],[a3]],

    ModAttribs = cl_core:attribs(MA,ME),
    FunAttribs = cl_core:attribs(FA,FE),

    Result = cut_lib_2(ModAttribs,FunAttribs,Clusters,Objects),
    true = (sort_cutlib_result_nice(Result) ==
            sort_cutlib_result_nice(ExpectedResult)),

    %% cleanup
    cl_matrix:delete(ModAttribs),
    cl_matrix:delete(FunAttribs),
    ok.

sort_cutlib_result_nice(R) ->
    test:sort(R,{sort,[sort,{leave,sort,sort}]}).

cut_lib_2(ModAttribs,FunAttribs,Clusters,Objects) ->
    ClustersData = cl_cutlib:clusters_to_clusters_data(Clusters),
    Graph = cl_cutlib:create_graph(ModAttribs,FunAttribs,ClustersData,Objects),
    ClusterIds = cl_cutlib:clusters_data_to_cluster_ids(ClustersData),
    CutResult = cl_cutlib:do_cut(Graph,ClusterIds),
    cl_cutlib:cut_result_to_nice({CutResult,ClustersData}).

