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
%%% ============================================================================
%%% Module information

%%% @doc Identifying code clones based on a matrix (caching version)
%%% @todo Integrate this module with refusr_clone_identifierl_tlexprs_clones

%%% @author Viktoria Fordos <f-viktoria@elte.hu>

-module(refusr_clone_identifierl_tlexprs_clones_with_cache).
-vsn("$Rev: 9316 $"). 
-include("user.hrl").
-include("refusr_clone_identifierl.hrl").

% interface
-export([get_clones/0, get_clones/1, get_specific_clones/1, default_options/0]).


% external symbol for clone item
-define(nil_clone_item, nothing).
%%% ============================================================================
%%% Interface

get_clones()->
    get_clones(default_options()).


get_specific_clones(Exprs)->
    get_clones([{subject, Exprs},
                 {metric, dice_sorensen},
                 {method, quick},
                 {diff_limit, 0.1},
                 {max_invalid_seq_length, 1},
                 {alphabet_opts, []}]).

default_options()->
    ?TLEClones:default_options().


get_clones(Options) when is_list(Options) ->
    search(Options).
    

%%% ============================================================================
%%% Controllers

search(Options)->
    {Storage, NumOfUnits} = ?Lib:make_unit_storage(store, Options),
    Clones = find_clones({Storage, NumOfUnits}, Options),
    ?Lib:free_storage(Storage),
    FilteredClones = ?TLEClones:filter_out_clones(Clones, Options),
    GroupedClones = ?Lib:group_clones(FilteredClones), 
    [{analysed_candidates_num, (NumOfUnits-1)*NumOfUnits/2},
     {detected_clones_num, length(GroupedClones)},
     {detected_clones, GroupedClones}].

find_clones(Storage, Options)->
    case proplists:get_value(subject, Options) of
        all ->
            find_all_clones(Storage, Options);
        _ ->
            ?TLEClones:find_specific_clones(Storage, Options)
    end.

find_all_clones(Storage, Options) ->
    case proplists:get_value(method, Options) of
        quick ->
            find_all_clones_quickly(Storage, Options);
        standard ->
            find_all_clones_generally(Storage, Options)
    end.

find_all_clones_quickly({Storage, NUmOfUnits}, Options) ->
    Diags = [ lists:zip(lists:seq(I,NUmOfUnits),lists:seq(1, NUmOfUnits-I+1)) 
            || I <- lists:seq(2, NUmOfUnits) ],
    find_all_clones_quickly(Diags, Storage, Options).

find_all_clones_quickly(Diags, Storage, Options) ->
    MetricStorage = ?Metric:make_cache(?metric_tab, #metric_values.key), 
    WorkerOpts = extract_worker_params(Options),
    MakeProperArgs = fun(Arg)-> [Arg, Storage, WorkerOpts] end,
    Result = ?MISC:parallelise(Diags, ?TLEClones, diags_worker, MakeProperArgs, low),
    ?Metric:free_cache(MetricStorage),
    {_, DeepDupsLists} = lists:unzip(Result),
    [Dup || DeepDups<-DeepDupsLists, Dup<-DeepDups].

find_all_clones_generally(_, _) ->
    %TODO
    not_implemented.

%%% ============================================================================
%%% Implementation of the quick method (based on diags)

extract_worker_params(Options) ->
    % m(s1,s2) = 1, when s1 == s2 
    MetricFun = case proplists:get_value(metric, Options) of
                    leveinstein ->
                        fun(E1, E2) 
                             when length(E1)>length(E2) -> 
                               1 - refusr_strm:levenshtein(E1,E2) / length(E1);
                           (E1, E2) ->
                               1- refusr_strm:levenshtein(E1,E2) / length(E2)
                        end;
                    dice_sorensen ->
                        fun(E1, E2)-> 
                               refusr_strm:dice_sorensen(E1,E2) 
                        end;
                    Fun when is_function(Fun, 2) ->
                        Fun
    end,
    UserMetricFun = fun(K1, K2)-> 
                           metric_value({K1,K2}, MetricFun) 
                    end, 
    DiffLimit = 1 - proplists:get_value(diff_limit, Options),
    MaxInvalidSeqLength  = proplists:get_value(max_invalid_seq_length, Options),
    {UserMetricFun, DiffLimit, MaxInvalidSeqLength}.

metric_value({Key, Key}, MetricFun)-> 
    metric_value([Key, Key], MetricFun); 
metric_value({Key1, Key2}, MetricFun)-> 
    metric_value(lists:sort([Key1, Key2]), MetricFun); 
metric_value(Keys=[K1,K2], MetricFun) when is_list(Keys), is_function(MetricFun, 2)-> 
    ?Metric:cache_val(?metric_tab, Keys, #metric_values.metric_val,
                      fun()-> #metric_values{key=Keys} end,
                      fun()-> MetricFun(K1,K2) end).