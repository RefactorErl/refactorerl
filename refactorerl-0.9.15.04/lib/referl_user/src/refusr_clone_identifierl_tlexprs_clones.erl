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

%%% @doc Identifying code clones based on a matrix
%%% @todo Integrate this module with refusr_clone_identifierl_tlexprs_clones_with_cache

%%% @author Viktoria Fordos <f-viktoria@elte.hu>

-module(refusr_clone_identifierl_tlexprs_clones).
-vsn("$Rev: 9316 $").
-include("user.hrl").
-include("refusr_clone_identifierl.hrl").

% interface
-export([get_clones/0, get_clones/1, get_specific_clones/1, default_options/0,
         filter_out_clones/2, find_specific_clones/2]).
% internal - for paralellising
-export([diags_worker/3, main_filter_worker/3]).

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
    [{subject, all},
     {metric, dice_sorensen},
     {method, quick},
     {diff_limit, 0.1},
     {max_invalid_seq_length, 0},
     {alphabet_opts, []}].

get_clones(Options) when is_list(Options) -> 
    search(Options).


%%% ============================================================================
%%% Controllers

search(Options)->
    {Storage, NumOfUnits} = ?Lib:make_unit_storage(store, Options),
    Clones = find_clones({Storage, NumOfUnits}, Options),
    ?Lib:free_storage(Storage),
    FilteredClones = filter_out_clones(Clones, Options),
    GroupedClones = ?Lib:group_clones(FilteredClones),

    [{analysed_candidates_num, (NumOfUnits-1)*NumOfUnits/2},
     {detected_clones_num, length(GroupedClones)},
     {detected_clones, GroupedClones}].

find_clones(Storage, Options)-> 
    case proplists:get_value(subject, Options, undefined) of
        all -> 
            find_all_clones(Storage, Options);
        _ -> 
            find_specific_clones(Storage, Options)
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

find_specific_clones(Storage, Options)->
    case proplists:get_value(method, Options) of
        quick ->
            find_specific_clones_quickly(Storage, Options);
        standard ->
            find_specific_clones_generally(Storage, Options)
    end.

find_specific_clones_quickly({Storage, NUmOfUnits}, Options)->
    Subjects = proplists:get_value(subject, Options),
    MatchSpec = ?Lib:make_match_spec(Subjects),
    SubjectsWithIndex = lists:keysort(1,ets:select(Storage, MatchSpec)),
    %TODO: check the subjects, that these prop.s hold:
    % - have same parent
    % - form a sequence (no missing elements!)
    MinM = element(1, hd(SubjectsWithIndex)),
    MaxM = element(1, lists:last(SubjectsWithIndex)),
    % the first rows have special diags.
    Diags =  [lists:zip(lists:seq(1, 1+(MaxM-(MinM+I))), lists:seq(MinM+I, MaxM))
        || I <- lists:seq(1, MaxM - MinM)] ++
    % 'normal' diags
    [lists:zip(lists:seq(I, I+(MaxM - MinM)), lists:seq(MinM, MaxM))
            || I <- lists:seq(1, NUmOfUnits-(MaxM - MinM))] --
    % the main diagonal is not needed
    [[{I,I} || I <- lists:seq(MinM, MaxM)]] ++
    % the last rows have special diags.
    [lists:zip(lists:seq(I, NUmOfUnits), lists:seq(MinM, MinM + (NUmOfUnits-I)))
            || I <- lists:seq(NUmOfUnits-(MaxM - MinM)+1, NUmOfUnits)],
    find_all_clones_quickly(Diags, Storage, Options).

find_all_clones_quickly(Diags, Storage, Options) ->
    WorkerOpts = extract_worker_params(Options),
    MakeProperArgs = fun(Arg)-> [Arg, Storage, WorkerOpts] end,
    Result = ?MISC:parallelise(Diags, ?MODULE, diags_worker, MakeProperArgs),
    {_, DeepDupsLists} = lists:unzip(Result),
    [Dup || DeepDups<-DeepDupsLists, Dup<-DeepDups].

find_all_clones_generally(_, _) ->
    %TODO
    not_implemented.

find_specific_clones_generally(_, _) ->
    %TODO
    not_implemented.


%%% ============================================================================
%%% Clone filtering
filter_out_clones(Clones, Options)->
     Storage = ?Metric:make_cache(?filter_tab, #filter_values.node),
     DiffLimit = 1 - proplists:get_value(diff_limit, Options),
     MaxInvalidSeqLength  = proplists:get_value(max_invalid_seq_length, Options),
     MakeProperArgs = fun(Arg)-> [Arg, {DiffLimit, MaxInvalidSeqLength}, Storage] end,
     Result = ?MISC:parallelise(Clones, ?MODULE, main_filter_worker, MakeProperArgs),
     {_, DeepFilteredClones} = lists:unzip(Result),
     ?Metric:free_cache(Storage),
     [ Cl || DeepClones <- DeepFilteredClones, DeepClones /= [], Cl<- DeepClones].

 main_filter_worker(Clones, Opts, Storage)->
    main_filter_worker(Clones, Opts, Storage, []).

 main_filter_worker([],_, _Storage, Results) ->
     Results;
 main_filter_worker([Clone | Clones], Opts, Storage, Results) ->
     case filter_conditions(Clone, Opts, Storage) of
         false ->
             main_filter_worker(Clones, Opts, Storage, Results ++ [[Clone]]);
         NewClones when is_list(NewClones) ->
             main_filter_worker(Clones, Opts, Storage, Results ++ [NewClones]);
         true ->
             main_filter_worker(Clones, Opts, Storage, Results ++ [[]])
     end.

filter_conditions([Clone],_, Storage)->
    #clone_item{items = [Unit1, Unit2]} = Clone,
    eval_conditions({Unit1, Unit2}, one_element, Storage);
filter_conditions(Clones, Opts, Storage)->
    filter_and_trim(Clones, Opts, [], [], 0, Storage).

filter_and_trim([], Opts, Currents, Results, InvalidSeqLength, Storage)
  when (length(Currents) - InvalidSeqLength) == 1 ->
    MayBeResult = [hd(Currents)],
    case filter_conditions(MayBeResult, Opts, Storage) of
                true when Results /= []->
                    Results;
                true ->
                    true;
                false ->
                    Results ++ [MayBeResult]
    end;
filter_and_trim([], _, Currents, Results, InvalidSeqLength, _Storage) ->
    case lists:sublist(Currents, 1, max(0, length(Currents) - InvalidSeqLength)) of
        [] when Results /= []->
            Results;
        [] ->
            true;
        Last ->
            Results ++ [Last]
    end;
filter_and_trim([ClI = #clone_item{items = [Unit1, Unit2], score=Score} | Clones],
                Opts = {DiffLimit, MaxInvalidSeqLength}, Currents, Results,
                InvalidSeqLength, Storage) ->
    IsGood = Score >= DiffLimit andalso
                 not(eval_conditions({Unit1, Unit2}, multi_element, Storage)),
    case IsGood of
        true ->
            filter_and_trim(Clones, Opts, Currents++[ClI], Results, 0, Storage);
        false when InvalidSeqLength < MaxInvalidSeqLength andalso
                               Currents /= [] ->
            filter_and_trim(Clones, Opts, Currents++[ClI], Results, InvalidSeqLength + 1 , Storage);
        false when (length(Currents) - InvalidSeqLength) > 1 ->
            filter_and_trim(Clones, Opts, [], Results ++
                                [lists:sublist(Currents, 1,
                                               length(Currents) - InvalidSeqLength)],
                            0, Storage);
        % In this case, the one-element filters are needed to be used.
        false when (length(Currents) - InvalidSeqLength) == 1 ->
            MayBeResult = [hd(Currents)],
            case filter_conditions(MayBeResult, Opts, Storage) of
                true ->
                    filter_and_trim(Clones, Opts, [], Results, 0, Storage);
                false ->
                    filter_and_trim(Clones, Opts, [], Results ++ [MayBeResult], 0, Storage)
            end;
        false ->
            filter_and_trim(Clones, Opts, [], Results, 0, Storage)
    end.

eval_conditions(Params, one_element, Storage)->
    eval_conditions(Params, Storage, ?Metric:filter(filters_for_one_element_clones, Storage),false);
eval_conditions(Params, multi_element, Storage)->
    eval_conditions(Params, Storage, ?Metric:filter(filters_for_multi_element_clones, Storage),false).

eval_conditions(_, _, _, true)->
    true;
eval_conditions(_, _, [], Result)->
    Result;
eval_conditions(Par={ Unit1, Unit2}, Storage, [Filter | Filters], _)->
    #filtering_metric{key=Key, calc_fun=CalcFun, arbitrate_fun=ArbFun} = Filter,
    Val1 = ?Metric:filter_value(Unit1, Key, CalcFun, Storage),
    Val2 = ?Metric:filter_value(Unit2, Key, CalcFun, Storage),
    Result = ArbFun({Val1, Val2}),
    eval_conditions(Par, Storage, Filters, Result).
%%% ============================================================================
%%% Implementation of the quick method (based on diags)

extract_worker_params(Options) ->
    % m(s1,s2) = 1, when s1 == s2
    MetricFun = case proplists:get_value(metric, Options) of
                    leveinstein ->
                        fun(E1, E2) when length(E1)>length(E2) ->
                               1 - refusr_strm:levenshtein(E1,E2) / length(E1);
                           (E1, E2) ->
                               1- refusr_strm:levenshtein(E1,E2) / length(E2)
                        end;
                    dice_sorensen ->
                        fun(E1, E2)-> refusr_strm:dice_sorensen(E1,E2) end;
                    Fun when is_function(Fun, 2) ->
                        Fun
                end,
    DiffLimit = 1 - proplists:get_value(diff_limit, Options),
    MaxInvalidSeqLength  = proplists:get_value(max_invalid_seq_length, Options),
    {MetricFun, DiffLimit, MaxInvalidSeqLength}.

diags_worker(Diagonals, Storage, Opts)->
    diags_worker(Diagonals, Storage, Opts, []).

diags_worker([], _, _, Results)->
    Results;
diags_worker([Diagonal|Diagonals], Storage, Opts, Results)->
    diags_worker(Diagonals, Storage, Opts,
                 Results++[diag_worker(Diagonal, Storage, Opts)]).

diag_worker(Diagonal, Storage, Opts) when length(Diagonal) > 0 ->
    diag_worker(Diagonal, Storage, Opts, [], ?nil_clone_item, [], 0);
diag_worker(_, _, _)->
    [].

diag_worker([], _, _, UnClassified, _, Clones, InvalidSeqLength) ->
    case lists:sublist(UnClassified, 1, max(0, length(UnClassified) - InvalidSeqLength)) of
        [] ->
            Clones;
        LastClone ->
            Clones ++ [LastClone]
    end;

diag_worker([{K1, K2} | Diagonal], Storage,
            Opts = {MetricFun, DiffLimit, MaxInvalidSeqLength},
            UnClassified, LastClone, Clones, InvalidSeqLength) ->
    %Insert and look-up times in tables of type set, bag and duplicate_bag are constant,
    %regardless of the size of the table.
    Unit1 = ets:lookup_element(Storage, K1, ?unit_data_pos),
    Unit2 = ets:lookup_element(Storage, K2, ?unit_data_pos),
    CanFormClone = can_form_clone(LastClone, Unit1, Unit2),
    Score = MetricFun(Unit1#unit.alphabet, Unit2#unit.alphabet),
    MaybeClone = #clone_item{items = [Unit1, Unit2], score = Score},
    case {Score >= DiffLimit, CanFormClone} of
        {true, true} ->
            diag_worker(Diagonal, Storage, Opts, UnClassified++[MaybeClone],
                        MaybeClone, Clones, 0);
        {true, false} when (length(UnClassified) - InvalidSeqLength) > 0 ->
            diag_worker(Diagonal, Storage, Opts, [MaybeClone],MaybeClone,
                        Clones ++
                            [lists:sublist(UnClassified, 1,
                                           length(UnClassified) - InvalidSeqLength)],
                        0 );
        {true, false} ->
            diag_worker(Diagonal, Storage, Opts, [MaybeClone], MaybeClone, Clones,
                        0 );
        {false, true} when InvalidSeqLength < MaxInvalidSeqLength andalso
                               UnClassified /= [] ->
            diag_worker(Diagonal, Storage, Opts, UnClassified++[MaybeClone],
                         MaybeClone, Clones, InvalidSeqLength + 1 );
        {false, _} when (length(UnClassified) - InvalidSeqLength) > 0 ->
            diag_worker(Diagonal, Storage, Opts, [], ?nil_clone_item,
                         Clones ++
                             [lists:sublist(UnClassified, 1,
                                            length(UnClassified) - InvalidSeqLength)],
                         0 );
        {false, _} ->
            diag_worker(Diagonal, Storage, Opts, [], ?nil_clone_item, Clones, 0 )
    end.

% determine whether the new candidates can belong to the same clone
can_form_clone(#clone_item{items = [#unit{parent = P1}, #unit{parent = P2}]},
               #unit{parent = P1}, #unit{parent = P2})->
    true;
can_form_clone(?nil_clone_item, _, _)->
    true;
can_form_clone(_, _, _)->
    false.