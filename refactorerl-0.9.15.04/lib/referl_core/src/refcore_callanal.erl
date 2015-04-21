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

%%% @doc Collection of functions supporting dynamic call analysis.
%%%
%%% @author Dániel Horpácsi <daniel_h@inf.elte.hu>

-module(refcore_callanal).

-include("core.hrl").

-type gnode(A) :: {atom(), A, integer()}.

-type e_atom() :: { gnode(expr), atom() }.
-type id() :: e_atom()
            | tuple(atom())
            | undefined
            | list( id() ).

-define(reach, reach_1st).

-export_type([id/0]).

-export([lookup_IDs/3, lookup_ID/2]).
-export([listcons_length/1]).

%% =============================================================================
%% Call type and callee ID recognition

is_cons_expr(Node) -> (?Graph:data(Node))#expr.type == cons.
is_atom_expr(Node) -> (?Graph:data(Node))#expr.type == atom.
%% is_fun_expr(Node) -> (?Graph:data(Node))#expr.type == implicit_fun.
atom_value(Node) ->  #expr{type = atom, value = Val} = ?Graph:data(Node), Val.

lookup_IDs(ModRef, FunRef, U) -> {lookup_ID(ModRef, U), lookup_ID(FunRef, U)}.

lookup_ID(Ref, U) -> lookup_ID(Ref, ?Graph:data(Ref), U).

-spec lookup_ID(gnode(expr), #expr{}, any()) -> id().
lookup_ID(N, #expr{type=atom, value=Name}, _) -> {N, Name};
lookup_ID(N, #expr{}, U)                      -> lookup_ID_via_dataflow(N, U);
lookup_ID(_, _, U)                            -> U.
lookup_ID_via_dataflow(N, U) ->
    Ns = ?Dataflow:?reach([N], [back], true),
    Is_Atom_Expr = fun(M) -> ?Graph:class(M) == expr andalso is_atom_expr(M) end,
    {Atoms, Rest} = lists:partition(Is_Atom_Expr, Ns),
    %% length(Atoms) > 1 andalso ?MultRefWarning,
    case {[{M, atom_value(M)} || M <- Atoms], Rest} of
        {[], _ } -> U;
        {L , []} -> L;
        {L , _ } -> [U | L]
    end.

%% =============================================================================

-type simple_list_length()      :: integer()
                                 | {1}.
-type proper_list_cons_length() :: [integer()]
                                 | [{integer()}].
-type list_cons_length()        :: proper_list_cons_length()
                                 | incalculable.

-spec sum_lengths(simple_list_length(), list_cons_length()) -> proper_list_cons_length().
sum_lengths({LL}, incalculable) when is_integer(LL) -> [{LL}];
sum_lengths( LL , incalculable) when is_integer(LL) -> [{LL}];
sum_lengths({LL}, {CL}) when is_integer(LL), is_integer(CL) -> [{LL + CL}];
sum_lengths( LL , {CL}) when is_integer(LL), is_integer(CL) -> [{LL + CL}];
sum_lengths({LL},  CL ) when is_integer(LL), is_integer(CL) -> [{LL + CL}];
sum_lengths( LL ,  CL ) when is_integer(LL), is_integer(CL) -> [ LL + CL ];
sum_lengths( LL ,  CLs) when (is_integer(LL) orelse is_tuple(LL)), is_list(CLs) ->
    lists:append([sum_lengths(LL, CL) || CL <- CLs, is_integer(CL)
                                             orelse is_tuple(CL)
                                             orelse CL == incalculable]).

-spec listcons_length(gnode(expr)) -> list_cons_length().
listcons_length(ListExpr) -> listcons_length(ListExpr, ?Graph:data(ListExpr)).

listcons_length(ListExpr, #expr{type=cons}) ->
    case ?Query:exec(ListExpr, ?Expr:children()) of
        [] -> [0];
        [Head] -> [list_length(Head)];
        [Head, Tail] ->
            case lists:member(Tail, loop(ListExpr))  of
                %% eliminating infinite loops -- ? should be refined ?
                true  -> sum_lengths(list_length(Head), incalculable);
                false -> sum_lengths(list_length(Head), listcons_length(Tail))
            end
    end;
listcons_length(N, #expr{}) ->
    Ns = ?Dataflow:?reach([N], [back], true),
    L1 = [N2 || N2 <- Ns, N2 /= N, ?Graph:class(N2) == expr],
    {L2, L3} = lists:partition(fun is_cons_expr/1, L1),
    if L2 == [] orelse L3 /= [] -> incalculable; %% TODO is this correct?
       true  -> lists:append(lists:map(fun listcons_length/1, L2))
    end;
listcons_length(_ListExpr, _Data) -> incalculable.

-spec list_length(gnode(expr)) -> simple_list_length().
list_length(ListExpr) -> list_length(ListExpr, ?Graph:data(ListExpr)).
list_length(ListExpr, #expr{type = list}) ->
    length(?Query:exec(ListExpr, ?Expr:children()));
list_length(_ListExpr, _Data) ->
    {1}.

loop(Expr) ->
    ?Graph:path(Expr, [flow]) ++
    ?Graph:path(Expr, [flow, flow]) ++   %% eliminating 'fret' inf. loops
    ?Graph:path(Expr, [ret]) ++
    ?Graph:path(Expr, [flow, ret]).
