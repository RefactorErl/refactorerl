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

%%% @doc Implements a genetic algorithm that determines a promising module
%%% clustering, form the modules which are in the database.
%%% The value of the clustering is calculated by the
%%% {@link cl_fitness:fitness/2} function.
%%%
%%% @author Hanna Kollo <khi@inf.elte.hu>

-module(cl_genetic).
-vsn("$Rev: 9609 $"). %"

-compile({no_auto_import, [min/2, max/2]}).

-export([ga/1,
         ga/3,
         ga_default/0]).

-include_lib("referl_cluster/include/cluster.hrl").

-record(global, {entities, connections, output, options}).

%% @doc Runs genetic algoritm with default parameters.
ga(FunMatrix) ->
    ga(stdout, [{population_size, 12},
                {iterations, 10},
                {mutation_rate, 0.9},
                {crossover_rate, 0.7},
                {elite_count, 2},
                {max_cluster_size, 5},
                {max_start_cluster_size, 2}], FunMatrix).

%% @doc Runs genetic algoritm with given parameters.
ga(_, Options, FunMatrix) ->
    %{W, C} = ?ClPrint:open(Output),
    FilesNum = length(?Query:exec([file])),
    Options2 = ?ClUtils:proplist_update(ga_default(), Options),
    Data = ?Matrix:rows(FunMatrix),
    Result = if FilesNum >= 2 ->
	                core(#global{entities=Data, 
	                             connections=FunMatrix, 
	%                             output=W, 
	                             options=Options2});
	            true -> []
	         end,
    %?ClPrint:close(C),
    Result.
    
%% @doc Returns back the default parameters of genetic algorithm.
ga_default() ->
    [{population_size, 12},
     {iterations, 10},
     {mutation_rate, 0.9},
     {crossover_rate, 0.7},
     {elite_count, 2},
     {max_cluster_size, 5},
     {max_start_cluster_size, 2}].


core(Global) ->
    PopulationSize = proplists:get_value(population_size,Global#global.options),
    Iterations = proplists:get_value(iterations,Global#global.options),
    %?ClOut:fwrite(Global#global.output, "Starting genetic algorithm...~n"),
    %?ClOut:fwrite(Global#global.output,
    %              "---------------------------------------------------~n"),
    L = chromosome_length(Global),
    Population = random_population(Global, PopulationSize, L),
    ga(Global, Population, L, 1,Iterations).

ga(Global, Population, Length, N, N) ->
    %?ClOut:fwrite(Global#global.output, "Iteration no. ~p~n", [N]),
    %PopFit = 
        population_fitness(Global, Population, Length),
    %{TmpList, _} = lists:unzip(PopFit),
    %?ClOut:fwrite(Global#global.output, "Fitnesses = ~p~n", [TmpList]),
    lists:map(fun (Elem) -> phenotype(Global, Elem, Length) end, Population);
                    
ga(Global, Population, Length, K, N) ->
    %?ClOut:fwrite(Global#global.output, "Iteration no. ~p~n", [K]),
    NewPopulation = create_new_population(Global, Population, Length),
    ga(Global, NewPopulation, Length, K+1, N).

create_new_population(Global, OldPopulation, Length) ->
    EliteCount = proplists:get_value(elite_count,Global#global.options),
    PopulationSize = proplists:get_value(population_size,Global#global.options),
    PopFit = population_fitness(Global, OldPopulation, Length),
    %{TmpList, _} = lists:unzip(PopFit),
    %?ClOut:fwrite(Global#global.output, "Fitnesses = ~p~n", [TmpList]),
    PopLevel = population_leveled(Global, PopFit, Length),
    Elites = elites(Global, PopFit, EliteCount),
    NewPopulation =
    select_new_population(Global, Elites, PopFit, PopLevel, Length, 0,
                          PopulationSize-EliteCount),
    NewPopulation.

elites(Global, PopFit, EliteCount) ->
    elites(Global, [], PopFit, EliteCount).

elites(_Global, Elites, _, 0) ->
    Elites;
elites(Global, Elites, [{_F, Chromosome} | RestPop], K) ->
    elites(Global, [Chromosome | Elites], RestPop, K-1).

population_fitness(Global, Population, Length) ->
    L = lists:map(
      fun(Chromosome) ->
              Phenotype = phenotype(Global, Chromosome, Length),
              Fitness =
                  ?ClFitness:fitness(
                    Phenotype,
                    [{mq, first_version},
                     {entity_type, 
                         ?Matrix:get_entities(Global#global.connections)},
                     {entities,
                      #which_entities{funs=true, recs=false, macros=false}}
                    ], Global#global.connections),
              {Fitness, Chromosome}
      end, Population),
    lists:reverse(lists:sort(L)).

population_leveled(_Global, PopFit, Length) ->
    SumFitness = sum_fitness(PopFit),
    if SumFitness == 0 ->
            {PopLevel, _} =
                lists:mapfoldl(
                  fun({_Fitness, Chromosome}, Acc) ->
                          {{1/Length + Acc, Chromosome}, 1/Length + Acc}
                  end, 0, PopFit);
       true ->
            SortPop = lists:sort(PopFit),
            {PopLevel, _} =
                lists:mapfoldl(
                  fun({Fitness, Chromosome}, Acc) ->
                          X = (Fitness + Acc)/SumFitness,
                          {{X, Chromosome}, Fitness + Acc}
                  end, 0, SortPop)
    end,
    PopLevel.


sum_fitness(PopFit) ->
    lists:foldl(fun({F, _C}, Acc) -> F + Acc end, 0, PopFit).

choose_chromosome(_Global, PopFit, upperquarter) ->
    N = round(length(PopFit)/4),
    R = random:uniform(N),
    {_, Chromosome} = lists:nth(R, PopFit),
    Chromosome.

select_new_population(Global, PartPopulation, PopFit, PopLevel, Length, K, N) ->
    C1 = choose_chromosome(Global, PopFit, upperquarter),
    C2 = choose_chromosome(Global, PopFit, upperquarter),
    {C3, C4} = perform_crossover(Global, C1, C2, Length),
    {C5, C6} = perform_mutation(Global, C3, C4, Length),
    if K + 2 < N ->
            NewPopulation = [C5, C6 |PartPopulation],
            select_new_population(Global,
                                  NewPopulation,
                                  PopFit,
                                  PopLevel,
                                  Length,
                                  K+2,
                                  N);
       K + 2 == N ->
            NewPopulation = [C5, C6 | PartPopulation],
            NewPopulation;
       K + 1 == N ->
            NewPopulation = [C5 | PartPopulation],
            NewPopulation
    end.

perform_crossover(Global, C1, C2, Length) ->
    CrossoverRate = proplists:get_value(crossover_rate,Global#global.options),
    CrossoverP = random:uniform(),
    if CrossoverP < CrossoverRate ->
            C3 = crossover(Global, C1, C2, Length),
            C4 = crossover(Global, C1, C2, Length);
       true ->
            C3 = C1,
            C4 = C2
    end,
    {C3, C4}.

perform_mutation(Global, C3, C4, Length) ->
    MutationRate = proplists:get_value(mutation_rate,Global#global.options),
    MutationP = random:uniform(),
    if MutationP < MutationRate ->
            C5 = mutation(Global, C3, Length),
            C6 = mutation(Global, C4, Length);
       true ->
            C5 = C3,
            C6 = C4
    end,
    {C5, C6}.

empty_clusters(Length) ->
    L = empty_clusters([], Length),
    dict:from_list(L).

empty_clusters(List, 0) ->
    List;
empty_clusters(List, K) ->
    empty_clusters([{K, []} | List], K-1).

phenotype(_Global, Chromosome, Length) ->
    %?d(_Global#global.entities),
    L = dict:to_list(Chromosome),
    {_ ,List} = lists:unzip(L),
    Clusters = empty_clusters(Length),
    {C3, _} =
    lists:foldl(
      fun(Elem, {C, Index}) ->
              Entity = lists:nth(Index, _Global#global.entities),
              %?d(Entity),
              C2 = dict:update(
                     Elem, fun(Data) ->
                                   [Entity | Data]
                           end, C),
              {C2, Index + 1}
      end, {Clusters, 1}, List),
    {_ , ClusterList} = lists:unzip(dict:to_list(C3)),
    Phenotype = lists:filter(fun([]) -> false; (_) -> true end, ClusterList),
    Phenotype.

chromosome_length(Global) ->
    length(Global#global.entities).

random_population(Global, PopulationSize, ChromosomeLength) ->
    random_population(Global, [], PopulationSize, PopulationSize,
                      ChromosomeLength).

random_population(_Global, List, 0, _, _) ->
    List;
random_population(Global, List, K, PopulationSize, ChromosomeLength) ->
    MaxClusterSize = proplists:get_value(max_cluster_size,
                                         Global#global.options),
    MaxStartClusterSize = proplists:get_value(max_start_cluster_size,
                                              Global#global.options),
    R = random_chromosome(Global, ChromosomeLength,
                          min(MaxClusterSize, MaxStartClusterSize)),

    random_population(Global, [R | List], K-1, PopulationSize,
                      ChromosomeLength).

random_chromosome(Global, ChromosomeLength, Range) ->
    L = random_chromosome(Global, [], ChromosomeLength, ChromosomeLength,
                          Range),
    dict:from_list(L).

random_chromosome(_Global, List, 0, _, _) ->
    List;
random_chromosome(Global, List, K, N, Range) ->
    R = random:uniform(Range),
    random_chromosome(Global, [{K, R} | List], K-1, N, Range).

mutation(Global, Chromosome, Length) ->
    MaxClusterSize = proplists:get_value(max_cluster_size,Global#global.options),
    Point = random:uniform(Length),
    R = random:uniform(min(Length, MaxClusterSize)),
    dict:update(Point, fun(_Value) -> R end, Chromosome).

crossover(_Global, Chromosome1, Chromosome2, Length) ->
    Point = random:uniform(Length),
    dict:merge(fun(Key, Value1, Value2) ->
                       if Key < Point ->
                               Value1;
                          true ->
                               Value2
                       end
               end, Chromosome1, Chromosome2).

min(X, Y) ->
    if X < Y ->
            X;
       true ->
            Y
    end.

