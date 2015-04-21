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

%%% @doc Interface module of the clustering and related modules.
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>
%%% @author Hanna Kollo <khi@inf.elte.hu>
%%% @author Petra Krizsai <krizsai@inf.elte.hu>
%%% @author Laszlo Budai <budail@caesar.elte.hu>

-module(cl_interface).
-vsn("$Rev: 10531 $").

-export([run_cluster/0, run_cluster/1, run_cluster_default/0,
         run_cluster_default/1, run_cluster_labels/0, run_cluster_labels/1,
         get_libs/0, get_libs/1, get_libs_default/0, get_libs_labels/0,
         cut_libs/1, cut_libs_default/0, cut_libs_labels/0,
         fitness/1, fitness_default/0, fitness_labels/0, weight/1,
         run_full_cluster/1, run_full_cluster/0, load_previous_clustering/0,
         run_full_cluster_default/0, run_full_cluster_labels/0,
         weight_default/0, weight_labels/0]).

-import(proplists, [get_value/2]).
-include_lib("../include/cluster.hrl").

-define(DataPath(Name), filename:join(?MISC:data_dir(),Name)
                        ++?MISC:graph_based_postfix()).

%%% ============================================================================
%%% Default values for function options
%%%
%%% Every function in this module have a proplist as a parameter.
%%% These proplists contain the actual parameters of the function, so
%%% it is important to give default value to every variable initialized by
%%% these proplists.
%%%
%% Function: {@link run_cluster/1}.
run_cluster_default() ->
    run_cluster_default(alg) ++
    run_cluster_default(all) ++
    run_cluster_default(agglom_attr) ++
    run_cluster_default(genetic) ++
    run_cluster_default(kmeans).

%% Function: {@link run_cluster/1}.
%%
%% The returned options:
%% <ul>
%%     <li>If `Part' is the name of an algorithm (`agglom_attr' or `genetic'),
%%         the returned options are the options specific to that algorithm.</li>
%%     <li>If `Part' is `all', the returned options are the options that are
%%         common for all algorithm (except for the `alg' options, which is not
%%         returned if `Part' is `all')</li>
%%     <li>If `Part' is `alg', the returned option is `alg'.</li>
%% </ul>
run_cluster_default(alg) ->
    [{alg, agglom_attr}];
run_cluster_default(all) ->
    [{modules, undefined},
     {functions, undefined},
     {log_output, stdout},
     {print_options, [{output,null}]},
     {entities, module}];
run_cluster_default(agglom_attr) ->
    [{skip_modules, undefined},
     {skip_functions, undefined},
     {transformfun, none},
     {distfun, weight},
     {anti_gravity, 0.5},
     {mergefun, smart}];
run_cluster_default(genetic) ->
    [{population_size, 12},
     {iterations, 10},
     {mutation_rate, 0.9},
     {crossover_rate, 0.7},
     {elite_count, 2},
     {max_cluster_size, 5},
     {max_start_cluster_size, 2}];
run_cluster_default(kmeans) ->
    [{stoppingcriteria, {unchanged, 10}},
     {mergefun, smart},
     {distfun, weight},
     {anti_gravity, 0},
     {k, 3}].

%% Function: {@link get_libs/1}.
get_libs_default() ->
    [{limit, 5}].

%% Function: {@link fitness/1}.
fitness_default() ->
    [{clusterings, no_default},
     {fitness_options, ?ClFitness:fitness_default()}].

%% Function: {@link cut_libs/1}.
cut_libs_default() ->
    [{clustering, no_default},
     {modules, []},
     {headers, []},
     {log_output, stdout},
     {print_options, [{output, null}]}].

%% Function: {@link weight/1}.
weight_default() ->
    [{clusterings, no_default},
     {entities, modules}].

%% Function: {@link run_all_cluster/1}.
run_full_cluster_default() ->
    [{algorithm, agglom_attr},
     {cluster_options, run_cluster_default()},
     {only_best, no},
     {show_goodness, yes},
     {decompose, no},
     {decompose_options, cut_libs_default()},
     {library_limit, get_libs_default()},
     {store_results, no}].

%%% ============================================================================
%%% Labels for function options

%% Function: {@link run_cluster/1}
run_cluster_labels() ->
    run_cluster_labels(alg) ++
    run_cluster_labels(all) ++
    run_cluster_labels(agglom_attr) ++
    run_cluster_labels(genetic).

%% Function: {@link run_cluster/1}
run_cluster_labels(alg) ->
    [{alg, "Algorithm"}];
run_cluster_labels(all) ->
    [{modules, "Modules to cluster"},
     {functions, "Functions to cluster"},
     {log_output, "Log output"},
     {print_options, "Print options"}];
run_cluster_labels(agglom_attr) ->
    [{skip_modules, "Modules to skip (separators: space or comma)"},
     {skip_functions, "Functions to skip (separators: space or comma)"},
     {transformfun, "Transform function"},
     {distfun, "Distance function"},
     {anti_gravity, "Antigravity"},
     {mergefun, "Merge Function"}];
run_cluster_labels(genetic) ->
    [{population_size, "Population size"},
     {iterations, "Iterations"},
     {mutation_rate, "Mutation rate"},
     {crossover_rate, "Crossover rate"},
     {elite_count, "Elite count"},
     {max_cluster_size, "Maximum cluster size"},
     {max_start_cluster_size, "Maximum start-cluster-size"}].

%% Function: {@link get_libs/1}.
get_libs_labels() ->
    [{limit, "Limit"}].

%% Function: {@link cut_libs/1}.
cut_libs_labels() ->
    [{clustering, "Clustering"},
     {modules, "Modules"},
     {headers, "Headers"},
     {log_output, "Log output"},
     {print_options, "Print options"}].

%% Function: {@link fitness/1}.
fitness_labels() ->
    [{clusterings, "Clusterings"},
     {fitness_options, "Fitness options"}].

%% Function: {@link weight/1}.
weight_labels() ->
    [{clustering, "Clusterings"},
     {entities, "Entities"}].

%% Function: {@link run_full_cluster/1}.
run_full_cluster_labels() ->
    [{algorithm, "Algorithm"},
     {cluster_options, "Clustering options"},
     {only_best, "Show only the best clustering"},
     {show_goodness, "Show goodness values"},
     {decompose, "Offer decomposition"},
     {decompose_options, "Decomposition options"},
     {library_limit, "Minimum call number for library modules"},
     {store_results, "Store the result of the clustering"}].

%%% ============================================================================
%%% Interface functions for the clustering module
%%%
%% @spec run_cluster() -> ok
%%
%% @doc Runs the clustering with the default options.
%% Same as `run_cluster([])'.
run_cluster() ->
    run_cluster([]).

%% @spec run_cluster(proplist()) -> [[[mod_name()]]]
%%
%% @doc Runs the clustering with the given options.
%%
%% Options (every option has a default value):
%% <ul>
%%     <li>`entities::(module | function)': it specifies whether modules
%%         or functions should be clustered.
%%         The default is `module'.</li>
%%     <li>`alg::(agglom_attr | genetic | kmeans)': the algorithm to be used.
%%         The default is `agglom_attr'.</li>
%%     <li>`modules::(undefined | [atom()])': the list of modules that should be
%%         clustered.
%%         The default value is `undefined'.</li>
%%     <li>`skip_modules::(undefined | [atom()])': the list of modules that
%%         should not be clustered.
%%         The default value is `undefined'.
%%         Either modules or skip_modules or both should be undefined.
%%         If both are undefined, all modules will be clustered.</li>
%%     <li>`functions::(undefined | [atom()])': the list of functions that
%%         should be clustered.
%%         The default value is `undefined'.</li>
%%     <li>`skip_functions::(undefined | [fun_attr()])': the list of functions that
%%         should not be clustered.
%%         The default value is `undefined'.
%%         Either functions or skip_functions or both should be undefined.
%%         If both are undefined, all functions will be clustered.</li>
%%     <li>`log_output::output_ref()': it specifies where the `cl_out:fwrite'
%%         messages about the execution should be printed.
%%         The default is `stdout'.</li>
%%     <li>`print_options::proplist()':
%%         it specifies how the clustering should be printed.
%%         The `PrintOptions' is forwarded to the `print:print_clusters'
%%         function.
%%         The default is `[{output,null}]'.
%%         A sensible value is
%% ```
%% [{output, {process, cl_out:start_section_process(
%%                         cl_out:file_name_gen("output", ".txt"))}}]
%% '''
%%         which will write the output into files `"output1.txt"', `"output2.txt"'
%%         etc. </li>
%% </ul>
%%
%% Options specific to the `agglom_attr' algorithm:
%% <ul>
%%     <li>`transformfun': a function that transforms the attribute matrix
%%         before running the clustering.
%%         It can be `zero_one' or `undefined'.
%%         `zero_one' means that the weights that are positive will be
%%         transformed to 1.
%%         `undefined' means that no transformation will be performed.
%%         The default value is `undefined'.</li>
%%     <li>`distfun': a distance function.
%%         It can be `call_sum', `weight' or a function reference.
%%         The default value is `weight'.</li>
%%     <li>`anti_gravity': if the distance function works with anti_gravity
%%         (like the `weight' distance function), then the weight of that can be
%%         given here.
%%         It is a float (or integer) that can vary between 0 and 1.
%%         (If 0, there is no anti_gravity, if 1, the anti_gravity is very
%%         strong.)
%%         The default value is 0.5.</li>
%%     <li>`mergefun': a merge function.
%%         It can be `smart' or a function reference.
%%         The default value is `smart', which is equivalent to
%%         `fun cl_mergefun:smart/3'.</li>
%% </ul>
%%
%% Options specific to the `genetic' algorithm:
%% <ul>
%%     <li>`population_size': The number of chromosomes in an iteration.
%%         Default: 12.</li>
%%     <li>`iterations': The number of iteration in the algorithm.
%%         Default: 10.</li>
%%     <li>`mutation_rate': The probability of mutation.
%%         Default: 0.9.</li>
%%     <li>`crossover_rate': The probability of crossover.
%%         Default: 0.7.</li>
%%     <li>`elite_count': The number of chromosomes that survive their
%%         generation because of being the fittest.
%%         Default: 2.</li>
%%     <li>`max_cluster_size': Maximum number of clusters allowed.</li>
%%     <li>`max_start_cluster_size': Maximum number of clusters allowed at
%%         startup.</li>
%% </ul>
%%
%% Options specific to the `kmeans' algorithm:
%% See the documentation of `cl_kmeans'.
%%
%% Example for `Options':
%% ```
%% [{alg, agglom_attr},
%%  {skip_modules, [lib]},
%%  {distfun, weight},
%%  {mergefun, smart},
%%  {anti_gravity, 0.3}]
%% '''
run_cluster(Options) ->
    Opts = ?ClUtils:proplist_update(run_cluster_default(), Options),
    Matrix = ?Matrix:load_attribute_matrix(get_value(entities, Opts)),
    FilesNum = length(?Query:exec([file])),
%    {W, C} = ?ClPrint:open(get_value(log_output, Opts)),
	W = 1,
    Clusterings =
        case get_value(alg, Opts) of
            Alg when Alg == agglom_attr; Alg == kmeans ->
                run_cluster_alg(W, Opts, Matrix);
            genetic when FilesNum >= 2 ->
                ?ClGenetic:ga(W, Opts, Matrix);
            _ -> []
        end,

    case get_value(print_options,Opts) of
        [{output,null}] ->
            ok;
        PrintOptions ->
            %?ClOut:fwrite(W, "Printing the clusterings...~n"),
            ?ClPrint:print_clusterings(Clusterings, PrintOptions)
    end,

    %?ClPrint:fwrite(W, "Clustering finished.~n"),
    %?ClPrint:close(C),
    ?Matrix:close(Matrix),
    Clusterings.

%% @spec run_cluster_alg(any(), output_ref(), proplist()) -> [[[mod_name()]]]
%%
%% @doc Runs the given clustering algorithm.
run_cluster_alg(_, Options, FunMatrix) ->
%    {W, C} = ?ClPrint:open(Output),
    %?ClOut:fwrite(W, "Updating and loading the attribute matrix...~n"),
    %?ClOut:fwrite(W,"~p~n",[Options]),
    Entities = get_value(entities, Options),
    Attribs = FunMatrix,

    %?ClOut:fwrite(W, "Filtering the attribute matrix...~n"),
    SkipEntities =
        case Entities of
            module ->
                skip_modules;
            function ->
                skip_functions
        end,

    EntFilter =
        case {get_value(modules, Options),
              get_value(SkipEntities, Options)} of
            {undefined, undefined} ->
                [];
            {EntitiesToCluster, undefined} ->
                [?ClUtils:leave(EntitiesToCluster)];
            {undefined, EntitiesToSkip} ->
                [?ClUtils:ignore(EntitiesToSkip)];
            {_, _} ->
                throw("One of '" ++ atom_to_list(Entities) ++ "' and '" ++
                      atom_to_list(SkipEntities) ++ "' must be" ++
                      "undefined, but neither is.")
        end,
    FilteredAttribs = ?ClCore:filter(Attribs, EntFilter, []),

    TransformedAttribs =
        case get_value(transformfun, Options) of
            none ->
                FilteredAttribs;
            zero_one ->
    %            ?ClOut:fwrite(W, "Transforming the attribute matrix...~n"),
                ?ClCore:transform2(FilteredAttribs,
                                   fun ?ClUtils:transform_to_01/3)
        end,

    %?ClOut:fwrite(W, "Obtaining the distance function and merge function...~n"),
    DistFun =
        case get_value(distfun, Options) of
            weight ->
                AntiGravity = get_value(anti_gravity, Options),
                case Entities of
                    module ->
                        ?ClDistfun:weight_gen(
                            ?ClDistfun:pow_size_fun_gen(AntiGravity));
                    function ->
                        ?ClDistfun:generate_fun_common_refs(AntiGravity)
                end;
            call_sum ->
                fun ?ClDistfun:call_sum/4;
            Fun when is_function(Fun) ->
                Fun
        end,

    MergeFun =
       case get_value(mergefun, Options) of
           smart ->
               fun ?ClMergefun:smart/3;
           Fun2 when is_function(Fun2) ->
               Fun2
       end,

    Algorithm = get_value(alg, Options),
    %?ClOut:fwrite(W, "Calculating the clusters...~n"),
    Result =
        case Algorithm of
           agglom_attr ->
               ?ClCore:agglom_attr(TransformedAttribs, DistFun, MergeFun);
            kmeans ->
                Options2 =
                    ?ClUtils:proplist_update(
                        Options,
                        [{mergefun, MergeFun},
                         {entitylist, ?Matrix:rows(TransformedAttribs)},
                         {distfun, DistFun}]),

                % TODO Calling set_selfusage in one branch will make the results
                % of k-means better; we could do something similar in the other
                % branch. Probably this set_selfusage things should be done more
                % elegantly.
                TransformedAttribs2 =
                    case Entities of
                        module ->
                            TransformedAttribs;
                        function ->
                            set_selfusage(TransformedAttribs)
                    end,

                ?ClKmeans:run_cluster(Options2, TransformedAttribs2)
        end,

    %?ClPrint:close(C),
    Result.

%% @spec set_selfusage(attribs()) -> attribs()
%%
%% @doc Sets self usage in the matrix. It means that for all rows that are also
%% columns, the value of the cell that corresponds to that row and column will
%% be set to 1 (which means it uses itself).
set_selfusage(Attribs) ->
    Rows = ?Matrix:rows(Attribs),
%    Cols = cl_matrix:cols(Attribs),
    lists:foldl(
      fun(Row, Attribs2) ->
%              Attribs3 =
%                  %% Could be done more efficiently; `Cols' could be a `dict' or
%                  %% an `ets' table.
%                  case lists:member(Row, Cols) of
%                      true ->
%                          Attribs;
%                      false ->
%                          cl_matrix:add_col(Row, Attribs2)
%                  end,
%              cl_matrix:set(Row, Row, 1, Attribs3)
            Def = ?Matrix:get_from_value(?Matrix:get_default(), func),
            case ?Matrix:get_from_ets(Row, Row, Attribs2, func) of
                Def -> ?Matrix:set_ets(Row, Row, 1, Attribs2, func);
                _ -> Attribs
            end
        end, Attribs, Rows).

%% @spec get_libs() -> [mod_name()]
%%
%% @doc Returns modules that are thought to be library modules with the default
%% options.
%% Same as `get_libs([])'.
get_libs() ->
    get_libs([]).

%% @spec get_libs(proplist()) -> [mod_name()]
%%
%% @doc Returns modules that are thought to be library modules.
%%
%% Options:
%% <ul>
%%     <li>`limit': an integer. Modules which are used by at least `limit' other
%%         modules, will be returned as library modules.
%%         The default value is 5.</li>
%% </ul>
get_libs(Options) ->
    Opts = ?ClUtils:proplist_update(get_libs_default(), Options),
    Attribs = ?Matrix:load_attribute_matrix(module),

    Libs = ?ClAttr:get_library_modules(
             Attribs,
             get_value(limit, Opts)),
    ?Matrix:close(Attribs),
    Libs.

%% @spec cut_libs(proplist()) -> cuts_result_nice()
%%
%%           cuts_result_nice() =
%%               [{FileName::string(),
%%                {ObjectsNotMoved::[graph_object()],
%%                 ObjectsMoved::[{ClusterId::number(),
%%                                 Cluster::[mod_name()],
%%                                 Objects::[graph_object()]}]}}]
%%           graph_object() = fun_attr() | rec_attr() | macro_attr()
%%
%% @doc Decomposes all the given modules and hrl files.
%%
%% Options:
%% <ul>
%%     <li>`clustering': the clustering, according to which the modules and
%%         headers should be decomposed.
%%         There is no default value, it must be given.</li>
%%     <li>`modules': modules that should be decomposed.
%%         The default is `[]'.</li>
%%     <li>`headers': postfix of headers that should be decomposed.
%%         The default is `[]'.
%%         E.g. if it is `["h.hrl"]', then `"h.hrl"' and "`oh.hrl'" will be, but
%%         `"ho.hrl"' will not be decomposed.</li>
%%     <li>`log_output::output_ref()': it specifies where the ?ClOut:fwrite
%%          messages about the execution should be printed.
%%         The default is `stdout'.</li>
%%     <li>`print_options::proplist()':
%%         it specifies how the decomposition should be printed.
%%         The `PrintOptions' is forwarded to the `print:print_cuts' function.
%%         The default is `[{output,null}]'.</li>
%% </ul>
cut_libs(Options) ->
    Opts = ?ClUtils:proplist_update(cut_libs_default(), Options),
    Clustering = ?ClUtils:get_defined_value(clustering, Opts),
    %{W, C} = ?ClPrint:open(get_value(log_output, Opts)),

    %?ClOut:fwrite(W, "Updating and loading the attribute matrices...~n"),
    %Matrix = ?Matrix:load_attribute_matrix(module),

    %?ClOut:fwrite(W, "Calculating the decomposition...~n"),
    Cuts = ?ClCutlib:cut_libs_all(Clustering,
                                  get_value(modules, Opts),
                                  get_value(headers, Opts)),

    case get_value(print_options,Opts) of
        [{output,null}] ->
            ok;
        PrintOptions ->
    %        ?ClOut:fwrite(W, "Printing the decomposition...~n"),
            ?ClPrint:print_cuts(Cuts, PrintOptions)
    end,

    %?ClOut:fwrite(W, "Decomposition finished.~n"),
    %?ClPrint:close(C),
    Cuts.

%% @spec fitness(proplist()) -> [number()]
%%
%% @doc Calculates the fitness value of the given clusterings.
%%
%% Options:
%% <ul>
%%     <li>`clusterings::[[mod_name()]]': the list of clusterings, whose fitness
%%         value should be calculated.
%%         This option is necessary, there is no default value.</li>
%%     <li>`fitness_options::proplist()': this option speficies how to calculate
%%         the fitness value. The value of this option will be passed to the
%%         {@link cl_fitness:fitness/2} function.
%%         The default is `[]'.</li>
%% </ul>
fitness(Options) ->
    Opts = ?ClUtils:proplist_update(fitness_default(), Options),
    Clusterings = get_value(clusterings, Opts),
    FitnessOptions = get_value(fitness_options, Opts),
    Matrix = ?Matrix:load_attribute_matrix(get_value(entity_type,
                                                     FitnessOptions)),
    Val = [ ?ClFitness:fitness(Clustering, FitnessOptions, Matrix) ||
        Clustering <- Clusterings ],
    ?Matrix:delete(Matrix),
    Val.

%% @spec weight(proplist()) -> [number()]
%%
%% @doc Calculates the goodness  a clustering, which uses
%%      weight distance function.
%%
%% Options:
%% <ul>
%%     <li>`clusterings::[[mod_name()]]': the list of clusterings, whose fitness
%%         value should be calculated.
%%         This option is necessary, there is no default value.</li>
%%     <li>`entities::function | module': entities of the given clustering</li>
%% </ul>
weight(Options) ->
    Opts = ?ClUtils:proplist_update(weight_default(), Options),
    Clusterings = get_value(clusterings, Opts),
    case Clusterings of
        [] -> [];
        _ ->
			[EntList] = lists:nth(1,Clusterings),
			Entities = get_value(entities, Opts),
			Matrix = ?Matrix:load_attribute_matrix(Entities),

			AllConnectionList = common_neighbour_count(EntList, EntList, Matrix),

			FitList = lists:map(fun(Clustering) ->
				lists:foldl(fun(Cluster, Acc) ->
					ConnList = common_neighbour_count(Cluster, EntList, Matrix),
					AvgList = lists:map(fun({Ent, {FVal, RVal}}) ->
						{_,{FAll, RAll}} = lists:keyfind(Ent, 1, AllConnectionList),
						case FAll + RAll of
							0 -> 0;
							_ -> (FVal + RVal) / (FAll + RAll)
						end
					end, ConnList),
					Acc + lists:sum(AvgList)/length(AvgList)
				end, 0, Clustering)
			end, Clusterings),

			?Matrix:delete(Matrix),

			FitList
	end.

common_neighbour_count(EntityList, AllEntities, Matrix) ->
    lists:foldr(fun(Ent1, AccList) ->
        EntValues =
        lists:foldr(fun(Ent2, {FConn, RConn}) ->
            case Ent1 =/= Ent2 of
                false -> {FConn, RConn};
                true -> {FVal, RVal} =
                    common_neighbour_count_for_two(Ent1, Ent2, AllEntities,
                                                   Matrix),
                    {FConn + FVal, RConn + RVal}
            end
        end, {0,0}, EntityList),
        AccList ++ [{Ent1, EntValues}]
    end, [], EntityList).

common_neighbour_count_for_two(Ent1, Ent2, AllEntities, Matrix) ->
    FunSum =
    lists:sum(
        lists:map(fun(Ent3) ->
            case ?Matrix:get_from_ets(Ent1, Ent3, Matrix, func) > 0 andalso
                 ?Matrix:get_from_ets(Ent2, Ent3, Matrix, func) > 0 of
                false -> 0;
                true -> 1
            end
        end, AllEntities)),
    RecSum =
    lists:sum(
        lists:map(fun(Ent3) ->
            case length(?Matrix:get_from_ets(Ent1, Ent3, Matrix, rec)) > 0
                 andalso
                 length(?Matrix:get_from_ets(Ent2, Ent3, Matrix, rec)) > 0 of
                false -> 0;
                true -> 1
            end
    end, AllEntities)),
    {FunSum, RecSum}.

run_full_cluster() ->
    run_full_cluster([]).

%% @spec run_full_cluster(proplist()) -> empty()
%%
%% @doc Calls every clustering step required by the user, and prints the
%%      neccesary info on the standard output (for now).
%%
%% Options:
%% <ul>
%%      <li> `algorithm': Desired clustering algorithm
%%            (agglom_attr | genetic | kmeans; default: agglom_attr)</li>
%%      <li> `cluster_options': Options for the clustering</li>
%%      <li> `only_best': Choses the best clustering and leaves out the rest.
%%            (yes | no; default: no)</li>
%%      <li> `show_goodness': Shows the "fitness" of each clustering shown.
%%           (yes | no; default: yes)</li>
%%      <li> `decompose': Offers a decomposition of the modules according
%%                     to the clustering (yes | no; default: no).
%%                     This options leaves out library modules from clustering!</li>
%%      <li>  `library_limit': If a module recieves this much incoming calls,
%%                     that is considered a library module.</li>
%%      <li> `decompose_options': Options for module decomposition</li>
%%      <li> `store_results': The user can store the result of the clustering
%%            in a dets table. (yes | no; default: no)</li>
%% </ul>
run_full_cluster(Options) ->
    Opts = ?ClUtils:proplist_update(run_full_cluster_default(), Options),
    Algorithm = get_value(algorithm, Opts),
    Entities = proplists:get_value(entities, Options, module),
    ClusteringOptions = [{alg, Algorithm},{output,null},{entities, Entities}]
                            ++ get_value(cluster_options, Opts),
    DistFun = proplists:get_value(distfun, ClusteringOptions, weight),
    LibraryLimit = get_value(library_limit, Opts),
    OnlyBest = get_value(only_best, Opts),
    ShowGoodness = get_value(show_goodness, Opts),
    Decompose = get_value(decompose, Opts),
    DecomposeOptions = get_value(decompose_options, Opts),
    StoreTable = get_value(store_results, Opts),

    case LibraryLimit of
        -1 ->
            SkipThese = [],
            FinalClusteringOptions = ClusteringOptions;
        _ ->
            SkipThese = get_libs([{limit,LibraryLimit}]),
            case proplists:get_value(skip_modules,ClusteringOptions,undefined) of
                undefined -> ok;
                _ -> proplists:delete(skip_modules, ClusteringOptions)
            end,
           FinalClusteringOptions = [{skip_modules,SkipThese}]
                                    ++ ClusteringOptions
    end,
    Clusterings = run_cluster(FinalClusteringOptions),
    Fitnesses =
        case Algorithm of
            agglom_attr ->
                case DistFun of
                    weight -> weight([{clusterings, Clusterings},{entities, Entities}]);
                    call_sum -> fitness([{clusterings, Clusterings},
                          {fitness_options,[{entity_type,Entities}]}])
                end;
            _ -> fitness([{clusterings, Clusterings},
                          {fitness_options,[{entity_type,Entities}]}])
        end,

    case Clusterings of
        [] -> {[],[],none};
        _ ->
			case OnlyBest of
				yes -> {Cl, Fit} =
				           select_best_clustering(Clusterings, Fitnesses);
				no -> {Cl, Fit} = {Clusterings, Fitnesses}
			end,

			case length(Cl) of
				1 -> Clustering = Cl;
				_ -> {Clustering,_} =
				         select_best_clustering(Clusterings, Fitnesses)
			end,

			case Decompose =:= yes andalso Entities =/= function of
				true ->
					DO1 = case proplists:get_value(clustering,
					                               DecomposeOptions,
					                               none) of
					   none -> DecomposeOptions;
					   _ -> proplists:delete(clustering, DecomposeOptions)
					end,
					DO2 = case proplists:get_value(modules,
					                               DecomposeOptions,
					                               none) of
					   none -> DO1;
					   _ -> proplists:delete(modules, DO1)
					end,
					DOpts = DO2
								++ [{clustering, Clustering},
								    {modules,SkipThese}],
					Decomp = cut_libs(DOpts);
				false -> Decomp = none
			end,

			case StoreTable of
				no -> ok;
				yes ->
					% Write to dets table
					TableName = ?DataPath("saved"),
					dets:open_file(TableName, [{file,TableName}]),
					dets:delete_all_objects(TableName),
					TupleList = [{clusterings, Cl},
								 {chosen_clustering, Clustering},
								 {goodnesses, Fit},
								 {decomposition, Decomp}],
					dets:insert(TableName, TupleList),
					% Write to scriptable file
					{_, IoDev} = file:open(?DataPath("scriptable"),[write]),
					file:write(IoDev, io_lib:format("~p. \r\n",[TupleList])),
					file:close(IoDev),
					% Write readable file,
					{_,IoDev2} = file:open(?DataPath("readable"),[append]),
					case OnlyBest of
						no ->
							?ClPrint:print_clusterings(Clusterings,
													   [{output,
													     ?DataPath("readable")},
														{write_mode, write},
														{entities, Entities}]);
						yes -> file:write(IoDev2, "")
					end,

					file:write(IoDev2, io_lib:format("~70c \r\n","=")),
					?ClPrint:print_clustering(Clustering,
											   [{output,?DataPath("readable")},
												{write_mode, append},
												{entities, Entities}]),

					file:write(IoDev2, io_lib:format("~70c \r\n","=")),
					case Decomp of
						none -> ok;
						_ -> ?ClPrint:print_cuts(Decomp,
											[{output,?DataPath("readable")}])
					end,
					file:close(IoDev2)
			end,

			case ShowGoodness of
				yes -> {Cl, Fit, Decomp};
				no -> {Cl, [], Decomp}
			end
	end.

select_best_clustering(Clusterings, Fitnesses) ->
    select_best_clustering(Clusterings, Fitnesses, [], 0).

select_best_clustering([], [], BestCl, BestFit) -> {BestCl, BestFit};
select_best_clustering([Cl | Cls], [Fit | Fitnesses], BestCl, BestFit) ->
    case Fit >= BestFit of
        true -> select_best_clustering(Cls, Fitnesses, Cl, Fit);
        false -> select_best_clustering(Cls, Fitnesses, BestCl, BestFit)
    end.

%% @spec load_previous_clustering() -> [proplist()]
%% @doc Loads a saved clustering of the current graph.
load_previous_clustering() ->
    TableName = ?DataPath("saved"),
    dets:open_file(TableName,[]),
    [{_,Cl}] = dets:lookup(TableName, clusterings),
    [{_,Best}] = dets:lookup(TableName, chosen_clustering),
    [{_,Goodnesses}] = dets:lookup(TableName, goodnesses),
    [{_,Decomposition}] = dets:lookup(TableName, decomposition),

    [{clusterings, Cl},{best, Best}, {goodnesses, Goodnesses},{decomposition, Decomposition}].
