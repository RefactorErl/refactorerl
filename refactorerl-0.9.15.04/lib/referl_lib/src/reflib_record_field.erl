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

%%% @doc Record field properties and record field based queries

-module(reflib_record_field).
-vsn("$Rev: 4100 $"). % for emacs"

%% =============================================================================
%% Exports

-export([file/0, recorddef/0, name/1, form/0, references/0]).
-export([all_typerefs/1, all_typerefs/2, named_typerefs/1, named_typerefs/2]).

-include("lib.hrl").

%% =============================================================================


%% @spec name(node(#field{})) -> atom()
%% @doc The name of the field object
name(Field) ->
    (?Graph:data(Field))#field.name.

%% @spec file() -> query(#field{}, #file{})
%% @doc The result query returns the file that defines the record
file() ->
    [{field, back}, {record, back}].

%% @spec form() -> query(#field{}, #form{})
%% @doc The result query returns the form that defines the record
form() ->
    [{field, back}, {recdef, back}].

%% @spec references() -> query(#field{}, #expr{})
%% @doc The result query returns the references to the record field
references() ->
    [{fieldref, back}].

%% @spec recorddef() -> query(#field{}, #form{})
%% @doc The result query returns the defining record of the record field
recorddef() ->
    [{field, back}].

%% @spec named_typerefs(node(#field{})) -> [node(#namedtype{})]
%% @doc Returns the namedtypes used by this node.
named_typerefs(Field) ->
    ?Type:named_typerefs_def(Field, [{fielddef, back}]).

%% @spec named_typerefs(node(#field{}), integer() | infinite) ->
%%     [node(#namedtype{})]
%% @doc Returns the namedtypes referenced by this node within a given Depth.
%% 0 depth means direct references. 'infinite' is the same as named_typerefs/1.
named_typerefs(Field, infinite) -> named_typerefs(Field);
named_typerefs(Field, Depth) when is_integer(Depth)->
    RootTypexps = ?ESG:path(Field, [{fielddef, back}]),
    lists:append([?Typexp:named_typerefs(Typexp, Depth) || Typexp <- RootTypexps]).

%% @spec all_typerefs(node(#field{})) -> [node(#typexp{})]
%% @doc Returns the (typexp) types used in this node.
all_typerefs(Field) ->
    ?Type:all_typerefs_def(Field, [{fielddef, back}]).

%% @spec all_typerefs(node(#field{}), integer() | infinite) ->
%%     [node(#typexp{})]
%% @doc Returns the (typexp) types referenced by this node within a given Depth.
%% 0 depth means direct references. 'infinite' is the same as all_subtypes/1.
all_typerefs(Field, infinite) -> all_typerefs(Field);
all_typerefs(Field, Depth) when is_integer(Depth)->
    RootTypexps = ?ESG:path(Field, [{fielddef, back}]),
    lists:append([?Typexp:all_typerefs(Typexp, Depth) || Typexp <- RootTypexps]).

