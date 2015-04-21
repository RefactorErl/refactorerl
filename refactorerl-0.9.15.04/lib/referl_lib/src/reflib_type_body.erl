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

-module(reflib_type_body).
-vsn("$Rev$").

%% =============================================================================
%% Exports

-export([value/1]).
-export([named_subtypes/1, named_subtypes/2, all_subtypes/1, all_subtypes/2]).
-export([definition/0, type/0]).

-include("lib.hrl").

%% =============================================================================
%% Properties

%% @spec value(node(#namedtypebody{})) -> string()
%% @doc The text representation of the named type body.
value(Typebody) ->
    (?ESG:data(Typebody))#namedtypebody.value.

%% @spec named_subtypes(node(#namedtypebody{})) -> [node(#namedtype{})]
%% @doc Returns the namedtypes used by this node.
named_subtypes(Typebody) ->
    ?Type:named_typerefs_def(Typebody, definition()).

%% @spec named_subtypes(node(#namedtypebody{}), integer() | infinite) ->
%%     [node(#namedtype{})]
%% @doc Returns the namedtypes referenced by this node within a given Depth.
%% 0 depth means direct references. `infinite' is the same as named_subtypes/1.
named_subtypes(Typebody, infinite) -> named_subtypes(Typebody);
named_subtypes(Typebody, Depth) when is_integer(Depth)->
    RootTypexps = ?ESG:path(Typebody, definition()),
    lists:append([?Typexp:named_typerefs(Typexp, Depth) ||
                  Typexp <- RootTypexps]).

%% @spec all_subtypes(node(#namedtypebody{})) -> [node(#typexp{})]
%% @doc Returns the (typexp) types used in this node.
all_subtypes(Typebody) ->
    ?Type:all_typerefs_def(Typebody, definition()).

%% @spec all_subtypes(node(#namedtypebody{}), integer() | infinite) ->
%%     [node(#typexp{})]
%% @doc Returns the (typexp) types referenced by this node within a given Depth.
%% 0 depth means direct references. `infinite' is the same as all_subtypes/1.
all_subtypes(Typebody, infinite) -> all_subtypes(Typebody);
all_subtypes(Typebody, Depth) when is_integer(Depth)->
    RootTypexps = ?ESG:path(Typebody, definition()),
    lists:append([?Typexp:all_typerefs(Typexp, Depth) || Typexp <- RootTypexps]).

%% =============================================================================
%% Queries

%% @spec definition() -> query(#namedtypebody{}, #typexp{})
%% @doc The defining typexp of the body.
definition() ->
    [{typebodydef, back}].

%% @spec type() -> query(#namedtypebody{}, #namedtype{})
%% @doc The type the body belongs to.
type() ->
    [{typebody, back}].

