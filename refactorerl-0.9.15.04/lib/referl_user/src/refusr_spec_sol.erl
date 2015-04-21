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

%%% @doc The module provides the type Solution. A solution is a mapping
%%% beetween nodes representing type variables and concrete types. A node
%%% can have only one type. Nodes the type of which cannot be reffered are
%%% represented by none() type.

%%% == Implementation status ==
%%% This feature is _not_ fully implemented.

%%% @author Gabor Olah <olikas.g@gmail.com>
-module(refusr_spec_sol).

-vsn("$Rev: 9568 $"). %for emacs"

-include("user.hrl").

-include("spec.hrl").

-export([new/0, new/1]).
-export([bottom/0]).
-export([update/4]).
-export([get_type/2]).
-export([merge/1, merge/2]).

%% @doc Creates a new solution.
new() ->
    orddict:new().

%% @doc Colones the solution.
new(OldSol) ->
    OldSol.
    %orddict:from_list( orddict:to_list( OldSol ) ).

%% @doc returns a bottom solution indicationg a type clash.
bottom() ->
    bottom.


%% @doc Creates a new solution by updating the type of Node. If Way is
%% 'intersect' the result will be the subtype of NewType and the type of
%% Node. If Way is 'union' than the resulting type will be the union of
%% the old and the new type.
update(bottom, _, _, _) ->
    bottom;
update(Sol, ?TV(Node), Way, NewType) ->
    update(Sol, Node, Way, NewType);
update(Sol,
       #type{kind='funsig', value=#funsigvalue{args=ArgsA, retval=RetValA}},
       Way,
       #type{kind='funsig', value=#funsigvalue{args=ArgsB, retval=RetValB}}) ->
    Fun = fun({TypeA, TypeB}, AccSol) ->
                  update(AccSol, TypeA, Way, TypeB)
          end,
    Sol1 = lists:foldl(Fun, Sol, lists:zip(ArgsA, ArgsB)),
    _Sol2 = update(Sol1, RetValA, Way, RetValB);
update(Sol,
       #type{kind='list', value=TypeA},
       Way,
       #type{kind='list', value=TypeB}) when TypeA =/= nil , TypeB =/= nil ->
    update(Sol, TypeA, Way, TypeB);
update(Sol,
       #type{kind='tuple', value=TypesA},
       Way,
       #type{kind='tuple', value=TypesB}) ->
    %?d(TypesA),
    %?d(TypesB),
    Fun = fun({TA, TB}, AccSol) ->
                  update(AccSol, TA, Way, TB)
          end,
    lists:foldl(Fun, Sol, lists:zip(TypesA, TypesB));
update(Sol,
       #type{kind='union', value=TypesA},
       Way,
       #type{kind='union', value=TypesB}) when length(TypesA) =:= length(TypesB)->
    %?d(TypesA),
    %?d(TypesB),
    Fun = fun({TA, TB}, AccSol) ->
                  update(AccSol, TA, Way, TB)
          end,
    lists:foldl(Fun, Sol, lists:zip(TypesA, TypesB));
update(Sol, Node, Way, NewType) when Way == 'intersect'; Way == 'union' ->
    case orddict:find(Node, Sol) of
        {ok, OldType} ->
            %% Node is included in the solution, so we should update
            %% it according to Way and store the calculated type in
            %% the solution.
            T = calc_new_type(Way, OldType, NewType),
            _NewSol = orddict:store(Node, T, Sol);
        error ->
            %% This Node has not been included in the solution, so
            %% so we should add it with Type
            _NewSol = orddict:store(Node, NewType, Sol)
    end.


calc_new_type(intersect, _OldType, NewType) ->
    %% ST = ?SLIB:is_subtype(NewType, OldType),
    %% ST#st_state.type;
    NewType;
calc_new_type(union, OldType, NewType) ->
    ?SLIB:t_union([OldType, NewType]).

%% @doc Returns the type of Node if included otherwise returns 'node_missing'.
get_type(bottom, _Node) ->
    {error, bottom};
get_type(Sol, Node) ->
    case orddict:find(Node, Sol) of
        {ok, Type} ->
            {ok, Type};
        error ->
            {error, node_missing}
    end.

%% @doc Merges two solutions by adding different elements to Sol1 and updating
%% existing element with union.
merge(Sol1, Sol2) ->
    Fun = fun(Node, Type, AccSol) ->
                  %?d([Node, Type]),
                  update(AccSol, Node, union, Type)
          end,
    orddict:fold(Fun, Sol1, Sol2).

%% @doc Merges a list of solution by aggregating the solutions.
merge([]) ->
    [];
merge([Sol]) ->
    Sol;
merge(Sols) when is_list(Sols) ->
    Fun = fun(Sol, Acc) ->
                  merge(Acc, Sol)
          end,
    lists:foldl(Fun, new(), Sols).


