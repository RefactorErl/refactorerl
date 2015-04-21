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

%%% @doc High level function specification-clause-related operations. This
%%% module contains functions that expect a specclause semantical node as
%%% their parameter (or return a query that expects a specclause semantical
%%% node as starting point).
%%%
%%% @author Denes Peteri <petden@caesar.elte.hu>

-module(reflib_spec_clause).
-vsn("$Rev$").

%% =============================================================================
%% Exports

-export([specpar/2]).
-export([all_typerefs/1, all_typerefs/2, named_typerefs/1, named_typerefs/2, index/1]).
-export([spec/0, definition/0, specguard/0, specpars/0, specret/0]).

-include("lib.hrl").

%% =============================================================================
%% Properties

%% @spec specpar(node(#specclause{}), integer()) -> [node(#specparam{})]
%% @doc Index-th (not return) parameter (specpar) of the specification.
specpar(Clause, Index) ->
    ?ESG:path(Clause, [{specpar, Index}]).

%% @spec specparams(node(#specclause{})) -> [node()]
%% @doc All of the specparam nodes under Clause (specpars and specrets).
specparams(Clause) ->
    ?ESG:path(Clause, specpars()) ++ ?ESG:path(Clause, specret()).

%% @spec named_typerefs(node(#specclause{})) -> [node(#namedtype{})]
%% @doc Returns the namedtypes used by this node.
named_typerefs(Clause) ->
    Guards = ?ESG:path(Clause, specguard()),
    Params = specparams(Clause),
    lists:append([?SpecGuard:named_typerefs(Guard) || Guard <- Guards]) ++
        lists:append([?SpecParam:named_typerefs(Param) || Param <- Params]).

%% @spec named_typerefs(node(#specclause{}), integer() | infinite) ->
%%     [node(#namedtype{})]
%% @doc Returns the namedtypes referenced by this node within a given Depth.
%% 0 depth means direct references. `infinite' is the same as named_typerefs/1.
named_typerefs(Clause, Depth)->
    Guards = ?ESG:path(Clause, specguard()),
    Params = specparams(Clause),
    lists:append([?SpecGuard:named_typerefs(Guard, Depth) || Guard <- Guards]) ++
        lists:append([?SpecParam:named_typerefs(Param, Depth) || Param <- Params]).

%% @spec all_typerefs(node(#specclause{})) -> [node(#typexp{})]
%% @doc Returns the (typexp) types used in this node.
all_typerefs(Clause) ->
    Guards = ?ESG:path(Clause, specguard()),
    Params = specparams(Clause),
    lists:append([?SpecGuard:all_typerefs(Guard) || Guard <- Guards]) ++
        lists:append([?SpecParam:all_typerefs(Param) || Param <- Params]).

%% @spec all_typerefs(node(#specclause{}), integer() | infinite) ->
%%     [node(#typexp{})]
%% @doc Returns the (typexp) types referenced by this node within a given Depth.
%% 0 depth means direct references. `infinite' is the same as all_subtypes/1.
all_typerefs(Clause, Depth)->
    Guards = ?ESG:path(Clause, specguard()),
    Params = specparams(Clause),
    lists:append([?SpecGuard:all_typerefs(Guard, Depth) || Guard <- Guards]) ++
        lists:append([?SpecParam:all_typerefs(Param, Depth) || Param <- Params]).

%% @spec index(node(#specclause{})) -> integer()
%% @doc Returns the index of the node under the spec.
index(Clause) ->
    [Spec] = ?Query:exec(Clause, spec()),
    ?ESG:index(Spec, specclause, Clause).

%% =============================================================================
%% Queries

%% @spec spec() -> query(#specclause{}, #spec{})
%% @doc The spec the cluase belongs to.
spec() -> [{specclause, back}].

%% @spec definition() -> query(#specclause{}, #typexp{})
%% @doc The defining typexp.
definition() -> [{specclausedef, back}].

%% @spec specguard() -> query(#specclause{}, #specguard{})
%% @doc Returns the specguard, if the clause has one.
specguard() -> [specguard].

%% @spec specpars() -> query(#specclause{}, #specparam{})
%% @doc Returns all of the parameters (not the return parameters though).
specpars() -> [specpar].

%% @spec specret() -> query(#specclause{}, #specparam{})
%% @doc Returns the return parameter.
specret() -> [specret].

