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

%%% @doc High level function specification-guard-related operations. This
%%% module contains functions that expect a specguard semantical node as
%%% their parameter (or return a query that expects a specguard semantical
%%% node as starting point).
%%%
%%% @author Denes Peteri <petden@caesar.elte.hu>

-module(reflib_spec_guard).
-vsn("$Rev$").

%% =============================================================================
%% Exports

-export([guards/1, value/1]).
-export([all_typerefs/1, all_typerefs/2, named_typerefs/1, named_typerefs/2]).
-export([specclause/0, definition/0]).

-include("lib.hrl").

%% =============================================================================
%% Properties

%% same as named_subtypes
%% @spec named_typerefs(node(#specguard{})) -> [node(#namedtype{})]
%% @doc Returns the namedtypes used by this node.
named_typerefs(Guard) ->
    ?Type:named_typerefs_def(Guard, definition()).

%% @spec named_typerefs(node(#specguard{}), integer() | infinite) ->
%%     [node(#namedtype{})]
%% @doc Returns the namedtypes referenced by this node within a given Depth.
%% 0 depth means direct references. `infinite' is the same as named_typerefs/1.
named_typerefs(Guard, infinite) -> named_typerefs(Guard);
named_typerefs(Guard, Depth) when is_integer(Depth)->
    RootTypexps = ?ESG:path(Guard, definition()),
    lists:append([?Typexp:named_typerefs(Typexp, Depth) || Typexp <- RootTypexps]).

%% @spec all_typerefs(node(#specguard{})) -> [node(#typexp{})]
%% @doc Returns the (typexp) types used in this node.
all_typerefs(Guard) ->
    ?Type:all_typerefs_def(Guard, definition()).

%% @spec all_typerefs(node(#specguard{}), integer() | infinite) ->
%%     [node(#typexp{})]
%% @doc Returns the (typexp) types referenced by this node within a given Depth.
%% 0 depth means direct references. `infinite' is the same as all_subtypes/1.
all_typerefs(Guard, infinite) -> all_typerefs(Guard);
all_typerefs(Guard, Depth) when is_integer(Depth)->
    RootTypexps = ?ESG:path(Guard, definition()),
    lists:append([?Typexp:all_typerefs(Typexp, Depth) || Typexp <- RootTypexps]).

%% @spec guards(node(#specguard{})) -> [node(#typexp{})]
%% @doc Returns all of the guard-like subtype constraints in the specguard.
guards(Guard) ->
    case ?ESG:path(Guard, definition()) of
        [Typexp] ->
            case (?ESG:data(Typexp))#typexp.type of
                vardef -> [Typexp];
                guardlist -> ?ESG:path(Typexp, [tsub]);
                _ -> []
            end;
        _ -> []
    end.

%% @spec value(node(#specguard{})) -> string()
%% @doc The text representation of the specguard.
value(Guard) ->
    (?ESG:data(Guard))#specguard.value.

%% =============================================================================
%% Queries

%% @spec specclause() -> query(#specguard{}, #specclause{})
%% @doc The clause the guard belongs to.
specclause() -> [{specguard, back}].

%% @spec definition() -> query(#specguard{}, #typexp{})
%% @doc The defining typexp.
definition() -> [{specguarddef, back}].

