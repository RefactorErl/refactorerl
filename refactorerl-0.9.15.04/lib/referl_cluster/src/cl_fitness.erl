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

%%% @doc Defines measures for determining the fitness of a clustering result
%%% The clustering result is passed as a parameter, and it is suppesed to
%%% refer to the acual content of the database.
%%%
%%% @todo Correct the documentation: ets -> mnesia
%%%
%%% @author Hanna Kollo <khi@inf.elte.hu>
%%% @author Laszlo Budai <budail@caesar.elte.hu>

% call: cl_fitness:fitness(Clustering,FitnessOptions)

-module(cl_fitness).
-vsn("$Rev: 10531 $").

-include_lib("referl_cluster/include/cluster.hrl").

%% =============================================================================
%% Exports

-export([fitness/2, fitness/3, fitness_default/0]).

%% =============================================================================

%% @spec fitness(Clusters, FunMatrix::matrix()) -> float()
%%       Clusters = [Cluster]
%%       Cluster = [ClusteringEntity]
%% @doc Calculates the fitness of the clustering by analysing cluster-inner and
%%      inter-cluster connections.
%% @see  fitness/2
fitness(Clusters, FunMatrix) ->
    fitness(Clusters, fitness_default(), FunMatrix).


%% @spec fitness_default() ->
%%           DefaultSettings::[{Property::atom(), Value::term()}]
%% @doc  Default settings for {@link fitness}.
fitness_default() ->
    [{mq, first_version}, {entity_type, module}, {entities, #which_entities{}}].


%% @spec fitness(Clusters,
%%               Options::[{Key::atom(), Value::term()}],
%%               FunMatrix::matrix()) -> float()
%%       Clusters = [Cluster]
%%       Cluster = [ClusteringEntity]
%%
%% @doc Calculates the fitness of the clustering by analysing
%% cluster-inner and inter-cluster connections.
%% The matrix FunMatrix provides the connection matrix for function alpha.
%%
%% Options:
%% <ul>
%%   <li>`mq': name of the used measurement. `first_version' is the TurboMQ,
%%       `second_version' is the BasicMQ.</li>
%%   <li>`entities': a `which_entities' record which controll the calculating
%%        the fitness value</li>
%% </ul>
fitness(Clusters, Options, Matrix) ->
    case proplists:get_value(mq, Options, first_version) of
        first_version ->
            core(Clusters, Matrix,
                 proplists:get_value(entities, Options, #which_entities{}));
        second_version ->
            core2(Clusters, Matrix,
                  proplists:get_value(entities, Options, #which_entities{}))
    end.


%% internal
%% first version of MQ
core(Clusters, Matrix, Params) ->
    lists:foldl(
           fun(Cluster, Acc) ->
                   cluster_factor(Clusters,
                                  Cluster,
                                  Matrix,
                                  Params)
                       + Acc
           end, 0, Clusters).

%% internal
%% second version of MQ
core2(Clusters, Matrix, Params) ->
    K = length(Clusters),
    if K == 1 ->
            intra_conn(lists:nth(1, Clusters), Matrix, Params);
       true ->
            Intra =
                lists:foldl(fun(Cluster, Acc) ->
                                    Acc + intra_conn(Cluster, Matrix, Params)
                            end, 0, Clusters),
            Inter =
                lists:foldl(
                  fun(Cluster1, Acc1) ->
                          Acc1 +
                              lists:foldl(
                                fun(Cluster2, Acc2) ->
                                        Acc2 + inter_conn(Cluster1,
                                                          Cluster2,
                                                          Matrix,
                                                          Params)
                                end, 0, Clusters)
                  end, 0, Clusters),
            MQ = 1/K * Intra - 2/(K*(K-1)) * Inter,
            MQ
    end.

inter_conn(Cluster1, Cluster2, Matrix,
          #which_entities{funs=true, recs=false, macros=false}) ->
    inter_conn_fun(Cluster1, Cluster2, Matrix).


intra_conn(Cluster, Matrix,
          #which_entities{funs=true, recs=false, macros=false}) ->
    intra_conn_fun(Cluster, Matrix).

intra_conn_fun(Cluster, Table) ->
    N = length(Cluster),
    mu(Cluster, Table)/(N*N) .

inter_conn_fun(Cluster1, Cluster2, Matrix) ->
    N1 = length(Cluster1),
    N2 = length(Cluster2),
    if Cluster1 == Cluster2 ->
            0;
       true ->
            epsilon(Cluster1, Cluster2, Matrix)/(2*N1*N2)
    end.

%% @doc Calculates the cluster factor of a cluster by analysing
%% internal cohesion and inter-cluster connection strength.
cluster_factor(
  Clusters, Cluster,
  Matrix,
  #which_entities{funs=true, recs=true, macros=false}) ->
    cluster_factor_all(Clusters, Cluster, Matrix);
cluster_factor(
  Clusters, Cluster,
  Matrix,
  #which_entities{funs=true, recs=false, macros=false}) ->
    cluster_factor_fun(Clusters, Cluster, Matrix);
cluster_factor(
  Clusters, Cluster,
  Matrix,
  #which_entities{funs=false, recs=true, macros=false}) ->
    cluster_factor_rec(Clusters, Cluster, Matrix).

cluster_factor_all(Clusters, Cluster, Matrix) ->
    Mu = mu(Cluster, Matrix) + mu2(Cluster, Matrix),
    SumEpsilon =
    lists:foldl(fun(Cl, Acc) ->
                        if Cl /= Cluster ->
                                %?d({epsilon2(Cl, Cluster,Matrix), Cluster, Cl}),
                                Acc +
                                    epsilon(Cluster, Cl, Matrix) +
                                    epsilon(Cl, Cluster, Matrix) +
                                    epsilon2(Cluster, Cl, Matrix);
                           true ->
                                Acc
                        end
                end, 0, Clusters),
    %?d({Cluster, Mu, SumEpsilon}),
    Nomination = 2*Mu + SumEpsilon,

    if
        Nomination == 0 -> 0;
        true -> 2*Mu/Nomination
    end.

cluster_factor_fun(Clusters, Cluster, Matrix) ->
    Mu = mu(Cluster, Matrix),
    SumEpsilon = lists:foldl(fun (Cl, Acc) ->
                        if Cl /= Cluster ->
                                Acc +
                                    epsilon(Cluster, Cl, Matrix) +
                                    epsilon(Cl, Cluster, Matrix);
                           true ->
                                Acc
                        end
                end, 0, Clusters),

    Nomination = 2*Mu + SumEpsilon,

    if
        Nomination == 0 -> 0;
        true -> 2*Mu/Nomination
    end.

cluster_factor_rec(Clusters, Cluster, Matrix) ->
    Mu2 = mu2(Cluster, Matrix),
    SumEpsilon2 = lists:foldl(fun (Cl, Acc) ->
                        if Cl /= Cluster ->
                                Acc +
                                    epsilon2(Cluster, Cl, Matrix);
                           true ->
                                Acc
                        end
                end, 0, Clusters),

    Nomination = 2*Mu2 + SumEpsilon2,

    if
        Nomination == 0 -> 0;
        true -> 2*Mu2/Nomination
    end.

%% Cluster : [Module]
%% Matrix: ets({CallerMod, CalledMod}, {FunCallCount,RecCount,MacroCount})
mu(Cluster, Matrix) ->
    epsilon(Cluster, Cluster, Matrix).

%% Cluster1 : [Module]
%% Cluster2 : [Module]
%% Matrix: ets({CallerMod, CalledMod}, {FunCallCount,RecCount,MacroCount})
epsilon(Cluster1, Cluster2, Matrix) ->
    lists:foldl(
      fun(Module1, Acc1) ->
              Acc1 + lists:foldl(
                fun(Module2, Acc2) ->
                        if Module1 /= Module2 ->
                                alpha(Module1, Module2, Matrix) + Acc2;
                           true -> Acc2
                        end
                end, 0, Cluster2)
      end, 0, Cluster1).

%% internal
alpha(Module1, Module2, Matrix) ->
    case ?Matrix:get_from_ets(Module1, Module2,Matrix,func) of
        0 -> 0;
        _ -> 1
    end.

%% Cluster : [Module]
%% Matrix: dets({CallerMod, CalledMod}, {FunCallCount,RecCount,MacroCount})
mu2(Cluster, Matrix) ->
    epsilon2(Cluster, Cluster, Matrix)/2. %% division due to commutatvity

%% Cluster1 : [Module]
%% Cluster2 : [Module]
%% Matrix: dets({CallerMod, CalledMod}, {FunCallCount,RecCount,MacroCount})
epsilon2(Cluster1, Cluster2, Matrix) ->
    lists:foldl(
      fun(Module1, Acc1) ->
              Acc1 + lists:foldl(
                fun(Module2, Acc2) ->
                        if Module1 /= Module2 ->
                                alpha2(Module1, Module2, Matrix) + Acc2;
                           true -> Acc2
                        end
                end, 0, Cluster2)
      end, 0, Cluster1).

%internal
alpha2(Module1, Module2, Matrix) ->
    case ?Matrix:get_from_ets(Module1, Module2, Matrix, rec) of
        [] -> 0;
        _ -> 1
    end.

