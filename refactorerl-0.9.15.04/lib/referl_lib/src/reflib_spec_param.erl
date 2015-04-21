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

%%% @doc High level function specification-parameter-related operations. This
%%% module contains functions that expect a specparam semantical node as
%%% their parameter (or return a query that expects a specparam semantical
%%% node as starting point).
%%%
%%% @author Denes Peteri <petden@caesar.elte.hu>

-module(reflib_spec_param).
-vsn("$Rev$").

%% =============================================================================
%% Exports

-export([specclause/1, value/1, name/1, param_type/1, is_ret/1, is_par/1, index/1]).
-export([all_typerefs/1, all_typerefs/2, named_typerefs/1, named_typerefs/2]).
-export([definition/0, reference/0]).

-include("lib.hrl").

%% =============================================================================
%% Properties

%% @spec named_typerefs(node(#specparam{})) -> [node(#namedtype{})]
%% @doc Returns the namedtypes used by this node.
named_typerefs(Param) ->
    ?Type:named_typerefs_def(Param, definition()).

%% @spec named_typerefs(node(#specparam{}), integer() | infinite) ->
%%     [node(#namedtype{})]
%% @doc Returns the namedtypes referenced by this node within a given Depth.
%% 0 depth means direct references. `infinite' is the same as named_typerefs/1.
named_typerefs(Param, infinite) -> named_typerefs(Param);
named_typerefs(Param, Depth) when is_integer(Depth)->
    RootTypexps = ?ESG:path(Param, definition()),
    lists:append([?Typexp:named_typerefs(Typexp, Depth) || Typexp <- RootTypexps]).

%% @spec all_typerefs(node(#specparam{})) -> [node(#typexp{})]
%% @doc Returns the (typexp) types used in this node.
all_typerefs(Param) ->
    ?Type:all_typerefs_def(Param, definition()).

%% @spec all_typerefs(node(#specparam{}), integer() | infinite) ->
%%     [node(#typexp{})]
%% @doc Returns the (typexp) types referenced by this node within a given Depth.
%% 0 depth means direct references. `infinite' is the same as all_subtypes/1.
all_typerefs(Param, infinite) -> all_typerefs(Param);
all_typerefs(Param, Depth) when is_integer(Depth)->
    RootTypexps = ?ESG:path(Param, definition()),
    lists:append([?Typexp:all_typerefs(Typexp, Depth) || Typexp <- RootTypexps]).

%% @spec index(node(#specparam{})) -> integer()
%% @doc Returns the index of the node under the spec-clause.
index(Param) ->
    [Clause] = specclause(Param),
    case is_ret(Param) of
        true -> 1;
        _ -> ?ESG:index(Clause, specpar, Param)
    end.

%% @spec specclause(node(#specparam{})) -> [node(#specclause{})]
%% @doc The clause the specparam is in.
specclause(Param) ->
    ?ESG:path(Param, [{specpar, back}]) ++
        ?ESG:path(Param, [{specret, back}]).

%% @spec value(node(#specparam{})) -> string()
%% @doc The text representation of the specparam.
value(Param) ->
    (?ESG:data(Param))#specparam.value.

%% @spec name(node(#specparam{})) -> string() | undefined
%% @doc If the parameter has a name, returns it. Returns undefined otherwise.
name(Param) ->
    (?ESG:data(Param))#specparam.name.

%% @spec param_type(node(#specparam{})) -> specpar | specret
%% @doc `specret' if the param is a return parameter, `specret' if it's an
%% input parameter.
param_type(Param) ->
    (?ESG:data(Param))#specparam.type.

%% @spec is_ret(node(#specparam{})) -> boolean()
%% @doc True if the parameter is a return (output) parameter.
is_ret(Param) ->
    case param_type(Param) of
        specret -> true;
        _ -> false
    end.

%% @spec is_par(node(#specparam{})) -> boolean()
%% @doc True if the parameter is a input parameter.
is_par(Param) ->
    not(is_ret(Param)).

%% =============================================================================
%% Queries

%% @spec definition() -> query(#specparam{}, #typexp{})
%% @doc The defining typexp.
definition() -> [{specparamdef, back}].

%% @spec reference() -> query(#specparam{}, #expr{})
%% @doc The referencing `fpar' or `fret' expression.
reference() -> [{specparamref, back}].

