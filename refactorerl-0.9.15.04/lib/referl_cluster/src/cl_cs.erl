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

%%% @doc This module contains tests and examples of the clustering module.
%%%
%%% @author Laszlo Budai <budail@caesar.elte.hu>

-module(cl_cs).
-export([weight_vs_call_sum/1, all_algorithms/1, genetic/1, test/0]).
-include_lib("../include/cluster.hrl").

%% @type path() - A string describing the path of a group of files.

%% @spec weight_vs_call_sum(path()) -> any()
%%
%% @doc This function demonstrates the difference between the call sum and
%%      the weight distance function. It loads the files described in the
%%      parameter to the database and runs both algorithms.
weight_vs_call_sum(Path) ->
    ri:reset(),
    io:format("Loading files ~n"),
    ri:add(Path),
    io:format("Running clusterings ~n"),
    io:format("--- Weight --- ~n"),
    W = cl_interface:run_cluster([{distfun,weight}]),
    io:format("--- Call Sum --- ~n"),
    C = cl_interface:run_cluster([{distfun,call_sum}]),
    W_Fitt = cl_interface:fitness([{clusterings,W}]),
    C_Fitt = cl_interface:fitness([{clusterings,C}]),

    io:format("--- Results --- ~n"),
    io:format("--- Weight --- ~n"),
    io:format("~p ~n ~p ~n", [W, W_Fitt]),
    io:format("--- ----- --- ~n"),
    io:format("--- Call Sum --- ~n"),
    io:format("--- ----- --- ~n"),
    io:format("~p ~n ~p ~n", [C, C_Fitt]).

%% @spec all_algorithms(path()) -> any()
%%
%% @doc This function shows the difference between all the clustering algorithms
%%      available in RefactorErl. It loads the files described in the
%%      parameter to the database and runs all of the algorithms.
all_algorithms(Path) ->
    ri:reset(),
    io:format("Load files ~n"),
    ri:add(Path),
    io:format("Run clusterings ~n"),
    io:format("--- Weight --- ~n"),
    W = cl_interface:run_cluster([{distfun,weight}]),
    io:format("--- Call Sum --- ~n"),
    C = cl_interface:run_cluster([{distfun,call_sum}]),
    io:format("--- Genetic --- ~n"),
    G = cl_interface:run_cluster([{alg,genetic}]),
    io:format("--- K-means --- ~n"),
    K = cl_interface:run_cluster([{alg,kmeans}]),

    io:format("--- Results --- ~n"),
    io:format("--- Weight --- ~n"),
    io:format("~p ~n", [W]),
    io:format("--- ----- --- ~n"),
    io:format("--- Call Sum --- ~n"),
    io:format("--- ----- --- ~n"),
    io:format("~p ~n", [C]),
    io:format("--- ----- --- ~n"),
    io:format("--- Genetic --- ~n"),
    io:format("--- ----- --- ~n"),
    io:format("~p ~n", [G]),
    io:format("--- ----- --- ~n"),
    io:format("--- K-means --- ~n"),
    io:format("--- ----- --- ~n"),
    io:format("~p ~n", [K]).


%% @spec genetic(integer()) -> any()
%%
%% @doc This function tests the genetic algorithm. It runs the genetic algorithm
%%      various parameters, and returns the fitness of the best result
%%      and the run time.
genetic(N) ->
    ParamList = [{A,B} || A <- lists:seq(10,50,10), B <- lists:seq(10,50,10)],
    lists:map(
        fun({A, B}) ->
            {_,SecStart,MSecStart} = now(),
            EV = threeParameterGenetic(N, A, B),
            {_,SecEnd, MSecEnd} = now(),
            TimeElapsed = ((SecEnd*1000000+MSecEnd) - (SecStart*1000000-MSecStart))/1000000,
            {{A, B},{round(EV*1000)/1000, round(TimeElapsed*1000)/1000}}
    end, ParamList).

threeParameterGenetic(N, IterNum, PopNum) ->
    ?d({IterNum, PopNum}),
    {_, F} = run_genetic(N, IterNum, PopNum),
    OccList = lists:foldl(fun(Fit, List) ->
				case lists:keyfind(Fit, 1, List) of
				    false -> lists:append([{Fit, 1}],List);
				    {_, Num} -> lists:keyreplace(Fit, 1, List, {Fit,Num+1})
				end
    end, [], F),
    SortedOccList = lists:reverse(lists:keysort(2, OccList)),
    PercentList = lists:map(fun({Fitt, Num}) ->
        {Fitt, Num/N}
    end, SortedOccList),
    lists:sum([Ft*P || {Ft,P} <- PercentList]).

run_genetic(N, I, P) ->
    run_genetic(N, I, P, [], []).

run_genetic(0, _, _, R, F) -> {R, F};
run_genetic(N, I, P, R, F) ->
    R1 = cl_interface:run_cluster([{alg,genetic},{entities,function},{iterations,I},
                                   {population_size,P}]),
    F1 = cl_interface:fitness([{clusterings,R1},{fitness_options,[{entity_type,function}]}]),
    run_genetic(N-1, I, P, R ++ [lists:last(R1)], F ++ [lists:last(F1)]).

%% @spec test() -> ok
%%
%% @doc This function tests the clustering algorithms enlisted in the
%%      first generator of ArgList. It runs the algorithm with testing
%%      different (sensible) values for 13 clustering parameters.
%%      This function is for debugging purpose only, it crashes on error.
test() ->
    ArgList = [{Alg, Ent, DistFun, TransformFun, Population, Mutation, MaxClSize,
          OnlyBest, Decompose, Headers, Store, LibLim, ShowGoodness}

    || Alg <- [agglom_attr, genetic], Ent <- [module, function],
       DistFun <- [call_sum, weight], TransformFun <- [none, zero_one],
       Population <- [10,15], Mutation <- [0.5, 0.7],
       MaxClSize <- [2, 4], OnlyBest <- [yes, no], Decompose <- [yes, no],
       Headers <- [[".hrl"],["g.hrl"]], Store <- [yes, no], LibLim <- [3, 5],
       ShowGoodness <- [yes, no]],

    SeqEnd = round(math:pow(2,13)),
    CountList = lists:seq(1, SeqEnd),
    LabeledArgList = lists:zip(CountList, ArgList),

    lists:map(
        fun({Count, ParamTuple}) -> ?d(Count), test(ParamTuple)
    end, LabeledArgList),

    ok.

test({Alg, Ent, DistFun, TransformFun, Population, Mutation, MaxClSize,
          OnlyBest, Decompose, Headers, Store, LibLim, ShowGoodness}) ->
	?ClInterface:run_full_cluster([{algorithm, Alg},{entities, Ent},
										{cluster_options,[{distfun, DistFun},
														  {transformfun, TransformFun},
														  {population_size,Population},
														  {mutation_rate,Mutation},
														  {max_cluster_size,MaxClSize}]},
										{only_best, OnlyBest},
										{decompose, Decompose},
										{decompose_options,[{headers,Headers}]},
										{store_results,Store},
										{library_limit,LibLim},
										{show_goodness, ShowGoodness}]).


















