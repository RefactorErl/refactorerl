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

%%% @doc A duplicated code search based on suffix_tree but with different filtering.

%%% @author Szabo Bence <szbtadi@caesar.elte.hu>

-module(refusr_clone_identifierl_filtered_suffix_tree).
-vsn("$Rev: 11297 $").
-include("user.hrl").
-include("refusr_clone_identifierl.hrl").

-export([get_clones/0, get_clones/1, default_options/0]).

% @private
-export([clique_worker/3,  inv_seq_loop/5, generate_groups/2, filter_worker/4]).

-record(group_item, {rows , cols}).

%%% ============================================================================
%%% Interface

get_clones()->
    get_clones(default_options()).

get_clones(Options) when is_list(Options) ->
    search(Options).

default_options() ->
    ?STree:default_options() ++ 
     [{max_invalid_seq_length, 0}].


search(Options) ->
    CloneResult = ?STree:get_clones(Options), 
    Clones = proplists:get_value(detected_clones, CloneResult),
    NoLexClones =
        lists:filter(fun(Clone) ->
            lists:any(fun(#clone_item{items = Items}) ->
                lists:all(fun(J)-> ?Graph:class(J#unit.id) /= lex end, Items)
            end, Clone)
        end, Clones),
    NormClones = ?Lib:to_tlexpr_clones(NoLexClones, fstree),

    Storage = ?Metric:make_cache(?filter_tab, #filter_values.node),
    FilteredClones = filter_out_clones(NormClones, Options, Storage),

    [{analysed_candidates_num, no_data},
      {detected_clones_num, length(FilteredClones)},
      {detected_clones, FilteredClones}].


filter_out_clones(Clones, Options, Storage) ->
    ZipClones = lists:zip(Clones,lists:seq(1,length(Clones))),
    MakeProperArgs = fun(Arg) -> [Arg, Options, [], Storage] end,
    _FilteredClones =
        ?MISC:parallelise(ZipClones, ?MODULE, filter_worker, MakeProperArgs, false, normal).


%% @doc Main filtering unit.
filter_worker([], _, Res, _) -> Res;
filter_worker([{Clone,N} | Clones], Options, Res, Storage) ->
    Length = length(Clone),
    MaxInvalidSeqLength = proplists:get_value(max_invalid_seq_length, Options),
    Cliques = get_cliques(Clone, N, Length, Storage),
    Isl = lists:reverse(lists:seq(0, MaxInvalidSeqLength)),

    MakeProperArgs = fun(Arg) -> [Cliques, Arg, N, Storage, []] end,
    SGS2 = ?MISC:parallelise(Isl, ?MODULE, inv_seq_loop, MakeProperArgs, false, low),
    SGS2E = make_max_ordset(SGS2++[Cliques]),
    SGSn = get_sgsn(SGS2E, N, Storage, Length),

    ets:match_delete(Storage, {N,'$1'}),
    MakeProperArgs1 = fun(Arg) -> [Arg, Clone] end,
    GroupedClones = ?MISC:parallelise(SGSn, ?MODULE, generate_groups, MakeProperArgs1, false, low),

    Minlen = proplists:get_value(minlen, Options),
    Minnum = proplists:get_value(minnum, Options),
    FilterLength = filter_by_length(GroupedClones,Minlen,Minnum),
    filter_worker(Clones, Options, FilterLength++Res, Storage).


%% @doc Loops through the InvalidSequenceLength list and groups.
inv_seq_loop(_, [], _, _, Res) -> Res;
inv_seq_loop(Cliques, [Isl | Tail], Id, Storage, Res) ->
    NewRes = inv_seq_group(Cliques, Isl, Id, Storage, []),
    inv_seq_loop(Cliques, Tail, Id, Storage, [NewRes|Res]).


%% @doc Groups the cliques for a specific InvalidSequenceLength.
inv_seq_group([], _, _, _, Res) -> Res;
inv_seq_group([Clique | Tail], Isl, Id, Storage, Res) ->
    #group_item{rows = Rows, cols = Cols} = Clique,
    Cands = ordsets:filter(
                fun(#group_item{rows = HRows, cols = HCols}) ->
                    lists:min(HRows) - lists:max(Rows) == Isl+1 andalso
                    ordsets:size(ordsets:intersection(Cols,HCols)) >= 2 
            end, Tail),

    Preworked = [pre(Clique, Cand, Id, Storage) || Cand<-Cands],
    NewClique = [union_and_intersect(Clique, [PCand]) || PCand<-lists:flatten(Preworked)],
    NewRes = insert_to_res(Res, NewClique),
    inv_seq_group(Tail, Isl, Id, Storage, NewRes).


%% @doc Filters clones by lenght.
filter_by_length([], _, _) -> [];
filter_by_length([Clone | Clones], Minlen, Minnum) ->
    Length = lists:foldl(fun(X, Sum) ->
                    Items = X#clone_item.items,
                    First = hd(Items),
                    length(First#unit.alphabet) + Sum
                 end, 0, Clone),

    case Length < Minlen of
        true -> filter_by_length(Clones, Minlen, Minnum);
        _ -> [Clone] ++ filter_by_length(Clones, Minlen, Minnum)
    end.


%% @doc Makes a maximal ordset from the list.
make_max_ordset(Res) -> 
    make_max_ordset(Res, []).

make_max_ordset([], Res) -> Res;
make_max_ordset([Head | Tail], Res) ->
    NewRes = insert_to_res(Head, Res),
    make_max_ordset(Tail, lists:flatten(NewRes)).


%% @doc Inserts elements to Res if the conditions are good.
insert_to_res([], Res) -> Res;
insert_to_res([H|T], Res) ->
    #group_item{rows = Rows, cols = Cols} = H,
    CanGoIn = lists:any(
                fun(#group_item{rows = KRows, cols = KCols}) -> 
                    ordsets:is_subset(Rows, KRows) andalso
                    ordsets:is_subset(Cols, KCols)
                end, T++Res),
    NewRes = case CanGoIn of
            false -> FilteredRes =
                         lists:filter(
                            fun(#group_item{rows = KRows, cols = KCols}) -> 
                                not(ordsets:is_subset(KRows, Rows)
                                andalso ordsets:is_subset(KCols, Cols))
                            end, Res),
                     ordsets:add_element(H,FilteredRes);
            _ -> Res
        end,
    insert_to_res(T, NewRes).


%% @doc Get the cliques of the Clones and returns them as group_item.
get_cliques(Clones, Id, Length, Storage) ->
    CloneNums = lists:zip(Clones, lists:seq(1, Length)),
    MakeProperArgs = fun(Arg)-> [Arg, Id, Storage] end,
    Result = ?MISC:parallelise(CloneNums, ?MODULE, clique_worker, MakeProperArgs, false, low),
    lists:flatten(Result).


%% @doc Worker unit of the filtering process.
clique_worker([], _, _) -> [];
clique_worker([{Clone, N}| Tail], Id, Storage) ->
    Items = Clone#clone_item.items,
    ZippedItems = lists:zip(Items, lists:seq(1,length(Items))),
    lists:map(fun({#unit{id = Node},M})-> 
        ets:insert(Storage,{Node,{Id,N,M}}) end, ZippedItems),
    ItemPairs = combos(2, ZippedItems),
    FilteredPairs = filtermap(fun(K) -> filter_return_id(K, Storage) end, ItemPairs),

    IgrpahList = lists:flatten(FilteredPairs),
    Cliques = 
        case length(Items)*(length(Items)-1) == length(IgrpahList) of
            true -> [lists:seq(1,length(Items))];
            _ -> ?Igraph:maximal_cliques(IgrpahList)
        end,
    OrdCliques = ordsets:from_list(Cliques),

    FullOrdCliques =
        lists:map(
            fun(K) ->
                #group_item{rows = [N], cols = ordsets:from_list(K)}
            end, OrdCliques),
    [FullOrdCliques]++clique_worker(Tail, Id, Storage).


%% @doc Calculates the filter value on a pair of items.
filter_return_id([{I1,Id1}, {I2,Id2}], Storage) ->
    case eval_conditions([I1,I2], Storage, one_element) of
        false -> {true, [Id1,Id2]};
        _ -> false
    end.


%% @doc Iterates throught Max..0 interval and connects the partresults if possible.
get_sgsn(SGSn, _, _, Max) when Max == 0 -> SGSn;
get_sgsn(SGSn, Id, Storage, Max) -> 
    SGSnE = seq_group(SGSn, Id, Storage, []),
    NewSGS = make_max_ordset([SGSn]++SGSnE),
    get_sgsn(NewSGS, Id, Storage, Max-1).

%% @doc Does an iteration of glueing the SGSn-s.
seq_group(Cliques, Id, Storage, Res) ->
    NewRes = seq_group0(Cliques, Id, Storage, []),
    Res++[NewRes].
seq_group0([], _, _, Res) -> Res;
seq_group0([Clique | Tail], Id, Storage, Res) ->
    #group_item{rows = Rows, cols = Cols} = Clique,
    Cands = ordsets:filter(
                fun(#group_item{rows = HRows, cols = HCols}) ->
                    ordsets:intersection(HRows, Rows) /= [] andalso
                    ordsets:size(ordsets:intersection(Cols, HCols)) >= 2 andalso
                    (not ordsets:is_subset(HRows,Rows) andalso not ordsets:is_subset(Rows,HRows))
            end, Tail),

    Preworked = [pre(Clique, Cand, Id, Storage) || Cand<-Cands],
    NewClique = [union_and_intersect(Clique, [PCand]) || PCand<-lists:flatten(Preworked)],
    NewRes = insert_to_res(Res, NewClique),
    seq_group0(Tail, Id, Storage, NewRes).

%% @doc Creates a group item by intersecting the cols and make min..max rows.
union_and_intersect(Comp, Cands) ->
    Full = ordsets:add_element(Comp,Cands),
    First = hd(Full), 
    MinRow = hd(First#group_item.rows),
    Last = lists:last(Full),
    MaxRow = lists:last(Last#group_item.rows),
    NewRow = lists:seq(MinRow, MaxRow),
    NewCol = intersect_cols(Full),
    NewGroupItem = #group_item{rows = NewRow, cols = NewCol},
    NewGroupItem.


%% @doc Intersects the list of group_item-s cols.
intersect_cols([H|T]) ->
    Cols = H#group_item.cols,
    intersect_cols(T,Cols).
intersect_cols([], Result) -> Result;
intersect_cols([H|T], Result) ->
    Cols = H#group_item.cols,
    intersect_cols(T, ordsets:intersection(Cols,Result)).


%% @doc Generates clone groups from group_items.
generate_groups(SGSnc, Clones) -> 
    [generate_group(Group, Clones) || Group <- SGSnc].

%% @doc Generates clone group from a gorup_item.
generate_group(Group, Clones) ->
    #group_item{rows = Rows, cols = Cols} = Group,
    get_ci_from_row(Rows,Cols, Clones).

%% @doc Gets the clone_group by rows.
get_ci_from_row([],_,_) -> [];
get_ci_from_row([H|T], Cols, Clones) ->
    ActRow = lists:nth(H, Clones),
    ActRowItems = ActRow#clone_item.items,
    NewItems = lists:map(fun(K) -> lists:nth(K, ActRowItems) end, Cols),
    [#clone_item{items = NewItems}] ++ get_ci_from_row(T, Cols, Clones).



%% @doc Generates every combinations of the list to K length lists.
combos(1, L) -> [[X] || X <-L];
combos(_, []) -> []; % is this clause correct?
combos(K, L) when K == length(L) -> [L];
combos(K, [H|T]) ->
    [[H | Subcombos] || Subcombos <- combos(K-1, T)]
    ++(combos(K, T)).


%% @doc Evaluates the filtering result to pair.
eval_conditions(Params, Storage, one_element)-> 
    eval_conditions(Params, Storage, ?Metric:filter(filters_for_one_element_clones_light, Storage),false).

eval_conditions(_, _, _,true)->
    true;
eval_conditions(_, _, [], Result)->
    Result;
eval_conditions([Unit1,Unit2]=Items, Storage, [Filter | Filters], _)-> 
    #filtering_metric{key=Key, calc_fun=CalcFun, arbitrate_fun=ArbFun} = Filter,
    Val1 = ?Metric:filter_value(Unit1, Key, CalcFun, Storage),
    Val2 = ?Metric:filter_value(Unit2, Key, CalcFun, Storage),
    Result = ArbFun({Val1, Val2}),
    eval_conditions(Items, Storage, Filters, Result).


%% @doc Filtermap function from Erlang lists module.
filtermap(F, [Hd|Tail]) ->
    case F(Hd) of
        {true,Val} ->
            [Val|filtermap(F, Tail)];
        false ->
            filtermap(F, Tail)
    end;
filtermap(F, []) when is_function(F, 1) -> [].


pre(Clique, Cand, Id, Storage) ->
    CandCols = Cand#group_item.cols,
    CandRows =
        case length(Clique#group_item.rows) > 1 of
            true -> ordsets:subtract(Cand#group_item.rows, Clique#group_item.rows);
            _ -> Cand#group_item.rows
        end,
    pre0([Clique], CandRows, CandCols, Id, Storage).

pre0(Cliques, [], _, _, _) -> Cliques;
pre0(Cliques, [Row | Rows], CandCols, Id, Storage) ->
    Result = 
        lists:flatmap(fun(Clique)->
            NewRows = Clique#group_item.rows++[Row],
            Cols = ordsets:intersection(Clique#group_item.cols, CandCols),
            GroupItem = #group_item{rows = [Row], cols = Cols},
            Glued = prework(Clique, GroupItem, Id, Storage, []),
            [#group_item{rows = NewRows, cols = Col} || Col<-Glued, length(Col) > 1]
        end, Cliques),
    pre0(Result,Rows,CandCols,Id, Storage).

prework(_, #group_item{cols = []}, _, _, Res) -> Res;
prework(Clique, Cand, Id, Storage, Res) ->
    #group_item{cols = [H|T], rows = Row} = Cand,
    {Result,Rem} =
        lists:partition(fun(Col) -> 
            prework0(Clique#group_item{cols = [H,Col]},
                     #group_item{rows = Row, cols = [H,Col]},
                     Id, Storage)
        end, T),
    prework(Clique,Cand#group_item{cols = Rem}, Id, Storage, [[H]++Result|Res]).
    
    

prework0(Clique, Cand, Id, Storage) ->
    CExpr = get_exprs_from_group_item(Clique, Id, Storage),
    CandExpr = get_exprs_from_group_item(Cand, Id, Storage),
    CandVars = get_var_pairs(CandExpr, Storage, []),
    CBinds = get_var_binds(CExpr, Storage, []),
    check_bind_integrity(CandVars, CBinds).

    
get_exprs_from_group_item(#group_item{rows = Rows, cols = Cols}, Id, Storage)->
    [ [ets:lookup_element(Storage, {Id,Row,Col}, 1) || Col <- Cols]
        || Row <- Rows].



get_var_pairs([], _, Res) -> Res;
get_var_pairs([Exprs | CExprs], Storage, Res) ->
    #filtering_metric{key=Key} =
        hd(?Metric:filter(filters_for_one_element_clones_light, Storage)),
    [First,Second] = Exprs,
    %Queried later when looking for bindings.
    {_, Vars1, _} = ets:lookup_element(Storage, First, Key),
    {_, Vars2, _} = ets:lookup_element(Storage, Second, Key),
    Pairs = pair_vars(Vars1, Vars2, []),
    get_var_pairs(CExprs, Storage, Pairs++Res).


pair_vars([], [], Res) -> Res;
pair_vars([{[],_}|Vars1], [{[],_}|Vars2], Res) ->
    pair_vars(Vars1, Vars2, Res);
pair_vars([{Var1,_}|Vars1], [{[],_}|Vars2], Res) ->
    pair_vars(Vars1, Vars2, [{Var1,no_var}|Res]);
pair_vars([{[],_}|Vars1], [{Var2,_}|Vars2], Res) ->
    pair_vars(Vars1, Vars2, [{no_var,Var2}|Res]);
pair_vars([{Var1,_}|Vars1], [{Var2,_}|Vars2], Res) ->
    pair_vars(Vars1, Vars2, [{Var1,Var2}|Res]).


get_var_binds([], _, Res) -> Res;
get_var_binds([Exprs|CExprs], Storage, Res) ->
    #filtering_metric{key=Key} =
        hd(?Metric:filter(filters_for_one_element_clones_light, Storage)),
    [First,Second] = Exprs,
    % Queried before when looking for variables
    {Binds1, _, _} = ets:lookup_element(Storage, First, Key),
    {Binds2, _, _} = ets:lookup_element(Storage, Second, Key),
    R = pair_vars(Binds1, Binds2,[]),
    get_var_binds(CExprs, Storage, R++Res).


check_bind_integrity(_,[]) -> true;
check_bind_integrity(Vars,[{Bind1,Bind2} | Binds]) ->
    BindedVars =
        [{Var1,Var2} || {Var1,Var2} <- Vars, Bind1 == Var1 orelse Bind2 == Var2],
    case lists:usort(BindedVars) of
        [{Bind1, Bind2}] -> check_bind_integrity(Vars, Binds);
        [] -> check_bind_integrity(Vars, Binds);
        _ -> false
    end.
