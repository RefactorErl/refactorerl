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

%%% @doc A duplicated code search based on the matrix algorithm but with 
%%%		 different filtering.

%%% @author First Szabo Bence <szbtadi@caesar.elte.hu>

-module(refusr_clone_identifierl_matrixfilter).
-vsn("$Rev$").
-include("user.hrl").
-include("refusr_clone_identifierl.hrl").

-export([get_clones/0, get_clones/1, default_options/0]).

%%% ============================================================================
%%% Interface
get_clones() -> 
	get_clones(default_options()).


default_options()->
    ?FunClones:default_options()++?TLEClones:default_options().

get_clones(Options) when is_list(Options) ->
    {NumOfUnits,Storage} = ?FunClones:make_storage(Options),
    Clones = ?FunClones:find_clones(NumOfUnits, Storage, Options),
    ?FunClones:free_storage(Storage),
    ListClones = [[Clone] || Clone <- Clones],
    MakeProperArgs = fun(Arg)-> [Arg, mfilter] end,
    NormClones = ?MISC:parallelise(ListClones, ?Lib, to_tlexpr_clones, MakeProperArgs, false, normal),
    TLEClones = trim_at_clause(NormClones, []),
    FilteredClones = ?TLEClones:filter_out_clones(TLEClones, Options),
    GroupedClones = ?Lib:group_clones(FilteredClones),

    [{analysed_candidates_num, NumOfUnits*(NumOfUnits-1) /2},
    {detected_clones_num, length(GroupedClones)},
    {detected_clones, GroupedClones}].

trim_at_clause([Clone | Clones], Res) when length(Clone) == 1 ->
    trim_at_clause(Clones, [Clone | Res]);
trim_at_clause([], Res) ->
    lists:reverse(Res);
trim_at_clause([[] | Clones], Res) -> 
    trim_at_clause(Clones, Res);
trim_at_clause([Clone | Clones], Res) ->
    Head = hd(Clone),
    HeadUnit = hd(Head#clone_item.items),
    {CutClone,Remains} =
        lists:splitwith(
            fun(#clone_item{items = [Unit,_]}) ->
                HeadUnit#unit.parent == Unit#unit.parent end, Clone),
    trim_at_clause([Remains|Clones], [CutClone|Res]).