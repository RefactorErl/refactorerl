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

%%% @doc This module can be used to decompose the library modules that remain
%%% after clustering.
%%% Probably the easiest way to use this library is to use the
%%% {@link cl_interface:cut_libs/1} function to invoke the calculations, and the
%%% {@link cl_print:print_cuts/2} function to print the results.
%%%
%%% The structure of the module is described in the pdf documentation.
%%% `create_graph' creates the graph, using `user_objects' and `user_clusters'.
%%% `do_cut' sorts the objects into the clusters, using `move_object_to_cluster'
%%% and `try_to_move_object_to_cluster'.
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>
%%% @author Laszlo Budai <budail@caesar.elte.hu>

-module(cl_cutlib).
-vsn("$Rev: 9609 $").

-export([collect_files/1, collect_files_by_modnames/1,
         collect_files_by_filenames/1,
         collect_files_by_filenames_end/1]).
-export([cut_lib/4, cut_lib_result_data/4, cut_libs/4, cut_libs_result_data/4,
         cut_libs_all/3, cut_libs_all_result_data/3,
         clusters_to_clusters_data/1, create_graph/4,
         clusters_data_to_cluster_ids/1, do_cut/2, cut_result_to_nice/1,
         cut_result_list_to_nice/1]).

-include_lib("referl_cluster/include/cluster.hrl").

%%% @type fun_attr() = #fun_attr{}.

%%% @type rec_attr() = #rec_attr{}.

%%% @type macro_attr() = #macro_attr{}.

%%% @type mod_attribs() = matrix().
%%%
%%% Attribute matrix that contains modules as entities.

%%% @type fun_attribs() = matrix().
%%%
%%% Attribute matrix that contains functions as entities.

%%% @type attr() = fun_attr() | rec_attr() | term().
%%%
%%% An attribute of the attribute matrix.

%%% @type mod_name() = string().
%%%
%%% The name of a module.

%%% @type file_name() = string().
%%%
%%% Absolute path to a file.

%%% @type cluster_id() = number().

%%% @type clusters_data() =
%%% {ModToClusterId::ordered_dictionary(mod_name(), cluster_id()),
%%%  ClusterIdToCluster::ordered_dictionary(cluster_id(), [mod_name()])}.
%%%
%%% The representation of the clustering that can be used effectively by this
%%% module.
%%% `ModToClusterId' assigns the cluster's id to each module of the cluster.
%%% `ClusterIdToCluster' assigns the list of clusters to each cluster's id.

%%% @type graph_object() = fun_attr() | rec_attr() | macro_attr().
%%%
%%% The graph contains module clusters and functions, macros, records.
%%% The functions, macros records are called objects, because they have to be
%%% sorted into the clusters.

%%% @type graph_node() = graph_object() | cluster_id().

%%% @type caller_to_callees_dict() = dictionary(graph_node(),set(graph_node())).

%%% @type callee_to_callers_dict() = dictionary(graph_node(),set(graph_node())).

%%% @type cut() = dictionary(cluster_id(), set(graph_object())).
%%%
%%% Represents a possible decomposition of the modules which are wanted to be
%%% decomposed.

%%% @type cut_result() =
%%% {ObjectsNotMoved::[graph_object()],
%%%  ObjectsMoved::[{Cluster::cluster_id(), Objects::[graph_object()]}]}.
%%%
%%% Represents a possible decomposition of the modules which are wanted to be
%%% decomposed.
%%% Objects in `ObjectsNotMoved' remained in their original place.
%%% Objects in `ObjectsMoved' were moved; objects `Objects' were moved into
%%% `Cluster'.

%%% @type cut_result_nice() =
%%% {ObjectsNotMoved::[graph_object()],
%%%  ObjectsMoved::[{ClusterId::cluster_id(),
%%%                  Cluster::[mod_name()],
%%%                  Objects::[graph_object()]}]}.
%%%
%%% Represents a possible decomposition of the modules which are wanted to be
%%% decomposed.
%%% Objects in `ObjectsNotMoved' remained in their original place.
%%% Objects in `ObjectsMoved' were moved; objects `Objects' were moved into
%%% `Cluster'.
%%% The difference between cut_result() and cut_result_nice() is that in the
%%% former the clusters are represented only by their id, while in the latter
%%% they are represented also by their contents (i.e. the list of the modules
%%% that they contain).

%%% @type cuts_result() = [{file(), cut_result()}].
%%%
%%% Represents the decomposition of several files.

%%% @type cuts_result_nice() = [{file_name(), cut_result_nice()}].
%%%
%%% Represents the decomposition of several files.

%% @todo: move to another module - maybe it exists somewhere...
file_to_filename(File) ->
    ?File:path(File).

%% @spec collect_files((file()) -> boolean()) -> [file()]
%%
%% @doc Returns the files specified by `Filter'.
%% A file will be contained by the result list if `Filter' returned true with
%% it as an argument.
collect_files(Filter) ->
    lists:foldl(
      fun(File, Files) ->
              case Filter(File) of
                  true -> [File|Files];
                  false -> Files
              end
      end,
      [],
      ?Query:exec([file])).

%% @spec collect_files_by_modnames([mod_name()]) -> [file()]
%%
%% @doc Returns the files that belong to `Modules'.
collect_files_by_modnames(Modules) ->
    ModulesOrd = ordsets:from_list(Modules),
    collect_files(
      fun(File) ->
              case ?Query:exec(File, ?File:module()) of
                  [] -> % there is no module in this file
                      false;
                  [ModNode] ->
                      ModName = ?Mod:name(ModNode),
                      ordsets:is_element(ModName, ModulesOrd)
              end
      end).

%% @spec collect_files_by_filenames([file_name()]) -> [file()]
%%
%% @doc Returns the files that belong to the given file names.
collect_files_by_filenames(FileNames) ->
    FileNamesOrd = ordsets:from_list(FileNames),
    collect_files(
      fun(File) ->
              CurrentFileName = ?File:path(File),
              ordsets:is_element(CurrentFileName, FileNamesOrd)
%            case ?File:path(File) of
%                CurrentFileName ->
%                    ordsets:is_element(CurrentFileName, FileNamesOrd);
%                _ ->
%                    false
%            end
      end).

%% @spec collect_files_by_filenames_end([string()]) -> [file()]
%%
%% @doc Returns the files that ends with the given string.
collect_files_by_filenames_end(FileNameEnds) ->
    collect_files(
      fun(File) ->
              CurrentFileName = ?File:path(File),
              lists:any(match_end(CurrentFileName), FileNameEnds)
%              case ?File:path(File) of
%                  CurrentFileName ->
%                      lists:any(match_end(CurrentFileName), FileNameEnds);
%                  _ ->
%                      false
%              end
      end).

match_end(FileName) ->
    fun(FileNameEnd) ->
            case length(FileName)-length(FileNameEnd)+1 of
                N when N>=0 -> string:substr(FileName, N) == FileNameEnd;
                _ -> false
            end
    end.

%% @spec contents_of_files([file()]) -> [graph_object()]
%%
%% @doc Returns the objects that belong to the files.
contents_of_files(Files) ->
    ContentsTable = ets:new(contents_of_files, []),
    lists:foldl(
      fun(File, _) ->
              Links =
                  %% TODO Is there a nicer way to do this?
                  ?Query:exec(File, ?File:macros())++
                  ?Query:exec(File, ?File:records())++
                  ?Query:exec(File, ?Query:seq(?File:module(), ?Mod:locals())),

              lists:foldl(
                fun(Node, _) ->
                        NewObject =
                            case ?Syn:class(Node) of
                                func ->
                                    %% We only want the functions that have definition
                                    case ?Query:exec(Node, ?Fun:definition()) of
                                        [] -> undefined;
                                        _ -> ?ClAttr:fun_to_fun_attr(Node)
                                    end;
                                form -> case ?Macro:name(Node) of
                                        "MODULE"    -> undefined;
                                        _           -> %?d(Node),
                                            collect_macro_info(Node)
                                                               
                                        end;
                                record  -> collect_rec_info(Node)
                            end,

                        case NewObject of
                            undefined -> undefined;
                            _ -> ets:insert(ContentsTable, {NewObject})
                        end
                end,
                undefined,
                Links)
      end,
      undefined,
      Files),
    ContentsList = [ Obj || {Obj} <- ets:tab2list(ContentsTable)],
    ets:delete(ContentsTable),
    ContentsList.
    
%% @spec collect_rec_info([node()]) -> {rec_attr(), [fun_attr()]}
%%
%% @doc Returns a tuple. The first element contains a record in a rec_attr
%%      record, the second elements shows, which functions use this record.
collect_rec_info(Node) ->
    {?ClAttr:rec_to_rec_attr(Node), 
                    sets:from_list(
                        lists:map(fun(Nd) -> ?ClAttr:fun_to_fun_attr(Nd) 
                        end,  ?Query:exec(Node,
                              ?Query:seq([?Rec:references(),
                                          ?Expr:clause(),
                                          ?Clause:form(),
                                          ?Form:func()]))))}.
               
%% @spec collect_macro_info([node()]) -> {macro_attr(), [fun_attr()]}
%%
%% @doc Returns a tuple. The first element contains a macro in a macro_attr
%%      record, the second elements shows, which functions use this macro.                   
collect_macro_info(Node) ->
    ObjRefList = ?Macro:refs(Node),
    MacroToFunList = case length(ObjRefList) of
        0 -> [];
        _ -> lists:map(
        fun(Nd) ->
            case ?Syn:node_type(Nd) of
                expr -> 
                    Nodes = ?Query:exec(Nd, ?Query:seq([?Expr:clause(),
                                            ?Clause:form(), ?Form:func()])),
                    case length(Nodes) of 
                        0 -> ok;
                        _ -> ?ClAttr:fun_to_fun_attr(hd(Nodes))
                    end;
                clause -> 
                    Nodes = ?Query:exec(Nd, ?Query:seq([?Clause:form(),
                                                        ?Form:func()])),
                    case length(Nodes) of 
                        0 -> ok;
                        _ -> ?ClAttr:fun_to_fun_attr(hd(Nodes))
                    end;
                form -> 
                    Nodes = ?Query:exec(Nd, ?Form:func()),
                    case ?Graph:is_gnode(Nodes) of
                        false -> ok;
                        true -> ?ClAttr:fun_to_fun_attr(hd(Nodes))
                    end
            end
        end, hd(ObjRefList))
    end,
    
    RetList = lists:foldr(
        fun(Elem, Acc) -> 
            case Elem of 
                ok -> Acc; 
                RealElem -> Acc ++ [RealElem] 
             end
     end, [], MacroToFunList),
    
    {?ClAttr:macro_to_macro_attr(Node),sets:from_list(RetList)}.

%% @spec cut_libs_all([[mod_name()]], [mod_name()], [string()]) ->
%%           cuts_result_nice()
%%
%% @doc Decomposes all the given modules and hrl files.
%% Each files will be decomposed separately.
cut_libs_all(Clusters, Libs, HrlNames) ->
    cut_libs_all_general(Clusters, Libs, HrlNames, fun cut_libs/4).

%% @spec cut_libs_all_result_data([[mod_name()]], [mod_name()], [string()]) ->
%%           {cuts_result(), clusters_data()}
%%
%% @doc Decomposes all the given modules and hrl files.
%% Each files will be decomposed separately.
cut_libs_all_result_data(Clusters, Libs, HrlNames) ->
    cut_libs_all_general(Clusters, Libs, HrlNames, fun cut_libs_result_data/4).

%% @spec cut_libs_all_general([[mod_name()]], [mod_name()], [string()],
%%           (mod_attribs(), fun_attribs(), [[mod_name()]], [file()]) -> Cut) ->
%%           Cut
%%
%% @doc Decomposes all the given modules and hrl files.
%% Each files will be decomposed separately.
cut_libs_all_general(Clusters, Libs, HrlNames, CutFun) ->

    %% creating attributes matricies
    ModAttribs = ?Matrix:load_attribute_matrix(module),
    FunAttribs = ?Matrix:load_attribute_matrix(function),
    ModAttribs2 = ?ClCore:filter(ModAttribs, [?ClUtils:ignore(Libs)], []),

    %% collecting files to be clustered
    FilesToBeClustered =
        collect_files_by_modnames(Libs)++
        collect_files_by_filenames_end(HrlNames),

    %% actual cutting
    Cut = CutFun(ModAttribs, FunAttribs, Clusters, FilesToBeClustered),

    %% deleting tables
    ?Matrix:delete(ModAttribs2),
    ?Matrix:delete(FunAttribs),
    Cut.

%% @spec cut_lib(mod_attribs(), fun_attribs(), [[mod_name()]], [file()]) ->
%%           cut_result_nice()
%%
%% @doc Returns the decomposition of the given files.
%% The files will be decomposed together.
cut_lib(ModAttribs, FunAttribs, Clusters, Files) ->
    CutResultData = cut_lib_result_data(ModAttribs, FunAttribs, Clusters,Files),
    cut_result_to_nice(CutResultData).

%% @spec cut_lib_result_data(mod_attribs(), fun_attribs(), [[mod_name()]],
%%           [file()]) ->
%%           {cut_result(), clusters_data()}
%%
%% @doc Returns the decomposition of the given files.
%% The files will be decomposed together.
cut_lib_result_data(ModAttribs, FunAttribs, Clusters, Files) ->
    Objects = contents_of_files(Files),
    ClustersData = clusters_to_clusters_data(Clusters),
    Graph = create_graph(ModAttribs, FunAttribs, ClustersData, Objects),
    ClusterIds = clusters_data_to_cluster_ids(ClustersData),
    CutResult = do_cut(Graph, ClusterIds),
    {CutResult, ClustersData}.

%% @spec cut_libs(mod_attribs(), fun_attribs(), [[mod_name()]], [file()]) ->
%%           cuts_result_nice()
%%
%% @doc Returns the decomposition of the given files.
%% Each files will be decomposed separately.
cut_libs(ModAttribs, FunAttribs, Clusters, Files) ->
    CutResultData = cut_libs_result_data(ModAttribs, FunAttribs,Clusters,Files),
    cut_result_list_to_nice(CutResultData).

%% @spec cut_libs_result_data(mod_attribs(), fun_attribs(), [[mod_name()]],
%%           [file()]) ->
%%           {cuts_result(), clusters_data()}
%%
%% @doc Returns the decomposition of the given files.
%% Each files will be decomposed separately.
cut_libs_result_data(ModAttribs, FunAttribs, Clusters, Files) ->
    ClustersData = clusters_to_clusters_data(Clusters),
    ClusterIds = clusters_data_to_cluster_ids(ClustersData),

    {[{File, begin Objects = contents_of_files([File]),
                  Graph = create_graph(ModAttribs, FunAttribs,
                                       ClustersData, Objects),
                  do_cut(Graph, ClusterIds)
            end} || File <- Files], ClustersData}.

%% @spec cut_result_list_to_nice({cuts_result(), clusters_data()}) ->
%%           cuts_result_nice()
%%
%% @doc The result of the cut contains cluster id-s; the output of this function
%% contains the clusters themselves as lists.
cut_result_list_to_nice({CutResultList, ClustersData}) ->
    [ {file_to_filename(File), cut_result_to_nice({CutResult, ClustersData})} ||
        {File, CutResult}<- CutResultList ].

%% @spec clusters_to_clusters_data([[mod_name()]]) -> clusters_data()
%%
%% @doc Transforms the clustering to another data structure.
%% `NumberOfClusters' contains the number of clusters.
clusters_to_clusters_data(Clusters) ->
    %% Acc: {id_of_next_cluster, ClusterIdToClusterAcc}
    {_N, ClusterIdToCluster} =
        lists:foldl(
          fun(Cluster, {Id, ClusterIdToClusterAcc}) ->
                  {Id+1,
                   [{Id, Cluster} | ClusterIdToClusterAcc]}
          end,
          {0, []},
          Clusters),
    ModToClusterIdDict =
        orddict:from_list(
          [{Mod, Id} || {Id, Cluster} <- ClusterIdToCluster, Mod <- Cluster]),
    {ModToClusterIdDict, orddict:from_list(ClusterIdToCluster)}.

%% @spec clusters_data_to_cluster_ids(clusters_data()) -> [cluster_id()]
%%
%% @doc Returns the list of id-s of the clusters.
clusters_data_to_cluster_ids({_, ClusterIdToCluster}) ->
    orddict:fetch_keys(ClusterIdToCluster).

%% @spec create_graph(mod_attribs(), fun_attribs(), clusters_data(),
%%                    [graph_object()]) ->
%%           {caller_to_callees_dict(), callee_to_callers_dict()}
%%
%% @doc Creates the graph on which the algorithm will work.
%% `Objects': objects that have to be assigned to clusters.
create_graph(_, FunAttribs, Clusters, Objects) ->
    %% RE: CallerToCalleesDict
    %% ER: CalleeToCallersDict
    ObjectsOrd = ordsets:from_list(Objects),
    ER =
        dict:from_list(
          [{Object,
            sets:union(
              user_objects(FunAttribs, Object, ObjectsOrd),
              user_clusters(FunAttribs, Object, Clusters))} ||
              Object <- Objects]),
    RE =
        dict:fold(
          fun(Callee, Callers, ERAcc1) ->
                  sets:fold(
                    fun(Caller, ERAcc2) ->
                            %?d({Caller, Callee}),
                            set_dict:add(Caller, Callee, ERAcc2)
                    end,
                    ERAcc1,
                    Callers)
          end,
          dict:new(),
          ER),
    {RE, ER}.

%% @spec user_objects(fun_attribs(), graph_object(),
%%                    ordered_set(graph_object())) -> set(graph_node())
%%
%% @doc Returns the objects that use `Object' and are present in the
%% `ObjectsOrd' set.
%% It will not contain the `Object' even if it uses itself.
%%
%% @todo add macros?
user_objects(FunAttribs, Object, ObjectsOrd) ->
    case is_record(Object,fun_attr) of
		true -> 
			L2 = ?Matrix:fold_col(
				fun (Caller, MatrixValue, L) ->
				   % ?d({Caller, Object, MatrixValue}),
				    case ?Matrix:get_from_value(MatrixValue, func) > 0 of
						true -> 
						    case ordsets:is_element(Caller, ObjectsOrd) andalso
									Caller/=Object of
								true -> [Caller|L];
								false -> L
							end;
						false ->  L
					end
				end,
				[],
				Object,
				FunAttribs),
			sets:from_list(L2);
		false ->
			{_, ConnectionData} = Object,
			Objects = sets:from_list(ordsets:to_list(ObjectsOrd)),
			Val = sets:intersection(ConnectionData,Objects),
			Val
	end.

%% @spec user_clusters(mod_attribs(), graph_object(), clusters_data()) ->
%%           set(graph_node())
%%
%% @doc Returns the clusters that use `Object'.
user_clusters(FunAttribs, Object, 
              {ModToClusterIdDict, _}) ->
    case is_record(Object,fun_attr) of
        true ->
            ?Matrix:fold_col(
              fun (Caller, MatrixValue, S) ->
                  case ?Matrix:get_from_value(MatrixValue,func) > 0 of
                      true -> 
                          case orddict:is_key(Caller#fun_attr.mod, 
                                            ModToClusterIdDict) of
                              true ->
                                  Cluster = orddict:fetch(
                                                Caller#fun_attr.mod, 
                                                ModToClusterIdDict),
                                  sets:add_element(Cluster, S);
                              false ->
                                  S
                          end;
                      false -> S
                  end
              end,
              sets:new(),
              Object,
              FunAttribs);
        false ->
            {_, ConnectionData} = Object,
            ConnectionList = sets:to_list(ConnectionData),
            S = lists:foldl(fun(F, Acc) ->
                case orddict:is_key(F#fun_attr.mod, ModToClusterIdDict) of
                    true ->
                        {_,ClusterId} = lists:keyfind(F#fun_attr.mod,1,ModToClusterIdDict),
                        sets:add_element(ClusterId, Acc);
                    false -> Acc
                end
            end, sets:new(), ConnectionList),
            S
    end.

%% @spec do_cut({caller_to_callees_dict(), callee_to_callers_dict()},
%%              [cluster_id()]) -> cut_result()
%%
%% @doc Returns the decomposition of the modules.
do_cut(Graph, ClusterIds) ->
    do_cut(Graph, dict:new(), ClusterIds).

%% @spec do_cut({caller_to_callees_dict(), callee_to_callers_dict()}, cut(),
%%              ClusterIds::[cluster_id()]) -> cut_result()
%%
%% @doc Tries to sort the objects of the graph to the clusters in `ClusterIds'.
do_cut({_RE, ER}, Cut, []) ->
    {[Object || {Object, _} <- dict:to_list(ER)], set_dict:to_list(Cut)};
do_cut({RE, ER}, Cut, [ClusterId|ClusterIdsTail]=ClusterIds) ->
    %% `Change': true if changes happened
    %% `RE': CallerToCalleesDict
    %% `ER': CalleeToCallersDict
    {Change, NewRE, NewER, NewCut} =
        case dict:find(ClusterId, RE) of
            {ok, Callees} ->
                %?d({ClusterId, sets:to_list(Callees)}),
                sets:fold(
                  fun(Object, Acc) ->
                          try_to_move_object_to_cluster(Object, Acc, ClusterId)
                  end,
                  {false, RE, ER, Cut},
                  Callees);
            error ->
                {false, RE, ER, Cut}
        end,
    %% `Change': if there were some changes in the graph (Change=true), i.e. we
    %% managed to put new modules to the cluster, we have to consider the same
    %% cluster in the next turn again(NewN=N), because we might find some now
    %% modules again. If there were not any changes (Change=false), we should go
    %% to and examine the next cluster (NewN=N-1).
    NewClusterIds = case Change of
                        true -> ClusterIds;
                        false -> ClusterIdsTail
                    end,
    do_cut({NewRE, NewER}, NewCut, NewClusterIds).

%% @spec try_to_move_object_to_cluster(
%%           fun_attr(),
%%           {bool(), caller_to_callees_dict(),
%%            callee_to_callers_dict(), cut()},
%%            cluster_id()) ->
%%           {bool(), caller_to_callees_dict(),
%%            callee_to_callers_dict(), cut()}
%%
%% @doc Check whether `Object' is used only by cluster `ClusterId', and if it is
%% true, it moves `Object' to that cluster.
try_to_move_object_to_cluster(Object, {_Change, RE, ER, Cut}=Args, ClusterId) ->
    case dict:find(Object, ER) of
        {ok, Callers} ->
            case sets:size(Callers) == 1 of
                true ->
                    {RE2, ER2, Cut2} =
                        move_object_to_cluster(ClusterId, Object, RE, ER, Cut),
                    {true, RE2, ER2, Cut2};
                false ->
                    Args
            end;
        error ->
            Args
    end.

%% @spec move_object_to_cluster(
%%           cluster_id(), fun_attr(),
%%           caller_to_callees_dict(), callee_to_callers_dict(),
%%           cut()) ->
%%           {caller_to_callees_dict(),
%%            callee_to_callers_dict(),
%%            cut()}
%%
%% @doc Moves `Object' to `ClusterId' cluster.
move_object_to_cluster(ClusterId, Object, RE, ER, Cut) ->
    %% adding the object to the cluster's set
    CutNew = set_dict:add(ClusterId, Object, Cut),

    %% `Object'->other edges will become `Cluster'->other edges
    {RE3, ER3} =
        case dict:find(Object, RE) of
            {ok, Callees} ->
                sets:fold(
                  fun(Callee, {RE2, ER2}) ->
                          {set_dict:add(ClusterId, Callee, RE2),
                           set_dict:add(
                             Callee, ClusterId,
                             set_dict:remove(Callee, Object, ER2))}
                  end,
                  {RE, ER},
                  Callees);
            error ->
                {RE, ER}
        end,

    %% removing `Object' from the cluster's callees and from RE
    RE4 = set_dict:remove(ClusterId, Object, RE3),
    RENew = dict:erase(Object, RE4),
    ERNew = dict:erase(Object, ER3),
    {RENew, ERNew, CutNew}.

%% @spec cut_result_to_nice({cut_result(),clusters_data()}) -> cut_result_nice()
%%
%% @doc The result of the cut contains cluster id-s; the output of this function
%% contains also the clusters themselves as lists.
cut_result_to_nice({{Objects_not_moved, Objects_moved},
                    {_, ClusterIdToClusterDict}}) ->
    {clear_objects(Objects_not_moved),
     [ {ClusterId, 
        orddict:fetch(ClusterId, ClusterIdToClusterDict),
        clear_objects(Objects)}  
    || {ClusterId, Objects} <- Objects_moved]}.
         
clear_objects(Objects) ->
    lists:map(fun(Obj) ->
        case is_record(Obj,fun_attr) of
            false -> 
                element(1, Obj);
            true -> 
                Obj
        end
    end, Objects).

