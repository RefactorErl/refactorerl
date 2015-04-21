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

%%% @doc This module provides interface functions for clustering. The different
%%% user interfaces can use this transformation to calculate clustering. There
%%% are two algorithms implemented for clustering:
%%% <ul>
%%%     <li>Agglomerative algorithm: which can be used for module and function
%%%         clustering too.</li>
%%%     <li>Genetic algorithm: which can be used for module clustering </li>
%%% </ul>
%%%
%%% @author Roland Kiraly <kiralyroland@inf.elte.hu>
%%% @author Gabor Horvath <tyros3@gmail.com>

-module(refcl_main).
-vsn("$Rev: 9612 $"). %"

-export([prepare/1, format_decomp/1]).

-include_lib("referl_cluster/include/cluster.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%%======================================================================
%%% Prepare

%% @spec prepare(proplist()) -> clustering_result()
%%
%% @doc This function handles the communication with higher level
%%      interfaces of the program. First, it asks the parameters
%%      described in the Args proplist, then, it runs the clustering
%%      algorithm with the given parameters, and returns the result.
prepare(Args) ->
	Algorithm = ?Args:algorithm(Args),
	Entity = ?Args:entity(Args),
	NewArgs = proplists:delete(algorithm, proplists:delete(entities, Args)) ++
              [{algorithm, Algorithm}, {entities, Entity}],
    
    Decomp = case Entity of
		module -> ?Args:decomposition(NewArgs);
		function -> no
	end,
	
	NewClusterArgs = ?Args:cluster_options(NewArgs),
	
	{LibraryLimit, NewDecompArgs} = case Decomp of
	    yes -> 
	        {?Args:library_limit(NewArgs),
	        ?Args:decomp_options(NewArgs)};
	    no -> 
	        {-1,[]}
	end,
	
	OnlyBest = ?Args:only_best(NewArgs),
	ShowGoodness = ?Args:show_goodness(NewArgs),
	StoreResults = ?Args:store_results(NewArgs),
	
	FuncParamList = 
	[{algorithm, Algorithm},
	 {entities, Entity},
	 {cluster_options, NewClusterArgs},
	 {decompose, Decomp},
	 {decompose_options, NewDecompArgs},
	 {show_goodness, ShowGoodness},
	 {only_best, OnlyBest},
	 {library_limit, LibraryLimit},
	 {store_results, StoreResults}],
	 
	fun() -> 
        {Clusterings, Goodnesses, Decomposition} = 
                                   ?ClInterface:run_full_cluster(FuncParamList),
        
        Decor = "=========================",
        Info = "\r\n===Clustering results:===\r\n",
        WholeText = Decor ++ Info ++ Decor,
        Res = [[{format,info},{text,WholeText}]],
        ClusterOutput = [add_to_result(Cluster) || Cluster <- Clusterings],
        FitnessOutput = case ShowGoodness of
            yes -> 
				[[{format,info},{text,"=== Fitness Numbers: ==="}],
								  add_to_result(Goodnesses)]; 
		    no -> 
		        ""
		end,
        DecompOutput = case Decomposition =/= none of
            true ->  [[{format, info},{text,"=== Decomposition: ==="}],
                      [{format, info},{text,format_decomp(Decomposition)}]];
            false -> ""
        end,
        Result = Res ++ ClusterOutput ++ FitnessOutput ++ DecompOutput,

        case proplists:get_value(ask_missing, Args) of
            true -> ?Transform:question(Result);
            false -> ok
        end,

        [Clusterings, Goodnesses, Decomposition]
    end.

%%%======================================================================
%%% Private functions

add_to_result(List) ->
    Text = io_lib:fwrite("~p", [List]),
    [{format,info},{text,Text}].

format_decomp(Modules) ->
     MappedModules = lists:map(fun({File,{Staying, MoveList}}) ->
         lists:flatten(io_lib:format(
             "Filename: ~s \r\n     Can't move: ~p \r\n",[File,Staying])) ++
         format_decomp_one(MoveList)
     end, Modules),
     lists:concat(MappedModules).
     
format_decomp_one(List) ->
    format_decomp_one([],List,0,length(List)).
    
format_decomp_one(InfoString, _, N, N) -> InfoString;
format_decomp_one(InfoString, [{_, Cl, Moving} | MoveTail], K, N) ->
    Obj = lists:flatten(io_lib:format(
                                "     Objects: ~p move to cluster: ~p \r\n",
                                [Moving, Cl])),
    NewIS = InfoString ++ Obj,
    format_decomp_one(NewIS, MoveTail, K+1, N).
