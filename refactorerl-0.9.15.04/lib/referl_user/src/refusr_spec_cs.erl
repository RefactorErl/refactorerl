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

%%% @doc

%%% == Implementation status ==
%%% This feature is _not_ fully implemented.

%%% @author Gabor Olah <olikas.g@gmail.com>
-module(refusr_spec_cs).

-vsn("$Rev: $"). %for emacs"

-include("user.hrl").

-include("spec.hrl").

-export([new/0, new/1, add/3, concat/2]).


new() ->
    {conj, ordsets:new()}.

new(OldSet) ->
    OldSet.



add(Set1, Way, Set2 = {_Way2, _Set2}) ->
    add(Set1, Way, [Set2]);
add({conj, OldSet}, conj, Cons) when is_list(Cons) ->
    {conj, ordsets:union(OldSet, ordsets:from_list(Cons))};
add({conj, OldSet}, disj, Cons) when is_list(Cons) ->
    {conj,
     ordsets:add_element({disj,
                          ordsets:from_list(Cons)}, OldSet)};
add({disj, _OldSet} = Cs, conj, Cons) when is_list(Cons) ->
    {conj,
     ordsets:add_element(Cs,
                         ordsets:from_list(Cons))};
add({disj, OldSet}, disj, Cons) when is_list(Cons) ->
    {disj, ordsets:union(ordsets:from_list(Cons), OldSet)}.



%% add({conj, OldSet}, conj, Cons) when is_list(Cons) ->
%%     {conj, ordsets:union(OldSet, ordsets:from_list(Cons))};
%% add({conj, OldSet}, conj, {Way, Set}) ->
%%     add({conj, OldSet}, conj, Cons);

%% add({conj, OldSet}, disj, Cons) when is_list(Cons) ->
%%     {conj,
%%      ordsets:add_element({disj,
%%                           ordsets:from_list(Cons)}, OldSet)};
%% add({conj, OldSet}, disj, Cons) ->
%%     add({conj, OldSet}, disj, [Cons]);



%% add({disj, _OldSet} = Cs, conj, Cons) when is_list(Cons) ->
%%     {conj,
%%      ordsets:add_element(Cs,
%%                          ordsets:from_list(Cons))};
%% add({disj, OldSet}, conj, Cons) ->
%%     add({disj, OldSet}, conj, [Cons]);

%% add({disj, OldSet}, disj, Cons) when is_list(Cons) ->
%%     {disj, ordsets:union(ordsets:from_list(Cons), OldSet)};
%% add({disj, OldSet}, disj, Cons) ->
%%     add({disj, OldSet}, disj, [Cons]).


concat([], _Way) when (_Way =:= conj) ; (_Way =:= disj) ->
    new();
concat([CS], _Way) when (_Way =:= conj) ; (_Way =:= disj) ->
    CS;
concat(Consets, Way) when is_list(Consets) and
                          ((Way =:= conj) or (Way =:= disj)) ->
    Fun = fun(C, Acc) -> ?CS:add(Acc, Way, C) end,
    lists:foldl(Fun, ?CS:new(), Consets).





