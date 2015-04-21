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

%%% @doc High level type parameter related operations. This module contains
%%% functions that expect a namedtypeparam semantical node as their parameter
%%% (or return a query that expects a namedtypeparam semantical node as
%%% starting point).
%%%
%%% @author Denes Peteri <petden@caesar.elte.hu>

-module(reflib_type_param).
-vsn("$Rev$"). % for emacs "

%% =============================================================================
%% Exports

-export([name/1, deftype/1, index/1, isjoker/1]).
-export([all_subtypes/1, all_subtypes/2]).
-export([definition/0, type/0, references/0]).

-include("lib.hrl").

%% =============================================================================
%% Typeparam related properties

%% @spec name(node(#namedtypeparam{})) -> list() | atom()
%% @doc Returns the name of the typeparam.
name(Typeparam) -> (?ESG:data(Typeparam))#namedtypeparam.name.

%% @spec deftype(node(#namedtypeparam{})) -> atom()
%% @doc Returns the type of the typeparam variable.
deftype(Typeparam) -> (?ESG:data(Typeparam))#namedtypeparam.type.

%% @spec index(node(#namedtypeparam{})) -> integer()
%% @doc The index of the parameter in the type argument list.
index(Typeparam) ->
    [Type] = ?Query:exec(Typeparam, type()),
    ?ESG:index(Type, typeparam, Typeparam).

%% @spec isjoker(node(#namedtypeparam{})) -> bool()
%% @doc Is the typeparam variable joker?
isjoker(Typeparam) ->
    case deftype(Typeparam) of
        joker -> true;
        _ -> false
    end.

%% typeparams don't have named subtypes; they can only be variables.

%% @spec all_subtypes(node(#namedtypeparam{})) -> [node(#typexp{})]
%% @doc Returns the (typexp) types used in this node.
all_subtypes(Typeparam) ->
    ?Type:all_typerefs_def(Typeparam, definition()).

%% @spec all_subtypes(node(#namedtypeparam{}), integer() | infinite) ->
%%     [node(#typexp{})]
%% @doc Returns the (typexp) types referenced by this node within a given Depth.
%% 0 depth means direct references. `infinite' is the same as all_subtypes/1.
all_subtypes(Typeparam, infinite) -> all_subtypes(Typeparam);
all_subtypes(Typeparam, Depth) when is_integer(Depth)->
    RootTypexps = ?ESG:path(Typeparam, definition()),
    lists:append([?Typexp:all_typerefs(Typexp, Depth) || Typexp <- RootTypexps]).

%% =============================================================================
%% Queries starting from typeparams

%% @spec definition() -> query(#namedtypeparam{}, #typexp{})
%% @doc The defining typexp.
definition() ->
    [{typeparamdef,back}].

%% @spec type() -> query(#namedtypeparam{}, #namedtype{})
%% @doc The named type the parameter belongs to.
type() ->
    [{typeparam, back}].

%% @spec references() -> query(#namedtypeparam{}, #typexp{})
%% @doc The typexp nodes referencing the parameter.
references() ->
    [{typeparamref, back}].
