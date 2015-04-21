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

%%% @doc Record properties and record based queries

-module(reflib_record).
-vsn("$Rev: 10567 $ ").

%% =============================================================================
%% Exports

-export([find/1, fields/0, field/1, name/1, file/0, form/0, references/0]).
-export([all_typerefs/1, all_typerefs/2, named_typerefs/1, named_typerefs/2]).

-include("lib.hrl").

%% =============================================================================


%% @spec find(atom()) -> query(#file{}, #record{})
%% @doc The result query returns the record with name `Name'
find(Name) ->
    [{record, {name, '==', Name}}].


%% @spec fields() -> query(#record{}, #field{})
%% @doc The result query returns the fields of the record
fields() ->
    [field].


%% @spec field(atom()) -> query(#record{}, #field{})
%% @doc The result query returns the record's field `Field'
field(Field) ->
    [{field, {name, '==', Field}}].


%% @spec name(node(#record{})) -> atom()
%% @doc The name of the record object
name(Record) ->
    (?Graph:data(Record))#record.name.

%% @spec file() -> query(#record{}, #file{})
%% @doc The result query returns the file that defines the record
file() ->
    [{record, back}].

%% @spec form() -> query(#record{}, #form{})
%% @doc The result query returns the form that defines the record
form() ->
    [{recdef, back}].

%% @spec references() -> query(#record{}, #expression{})
%% @doc The result query returns every reference to the record
references() ->
    [{recref, back}].

%% @spec named_typerefs(node(#record{})) -> [node(#namedtype{})]
%% @doc Returns the namedtypes used by this node.
named_typerefs(Record) ->
    Fields = ?ESG:path(Record, fields()),
    lists:append([?RecField:named_typerefs(Field) || Field <- Fields]).

%% @spec named_typerefs(node(#record{}), integer() | infinite) ->
%%     [node(#namedtype{})]
%% @doc Returns the namedtypes referenced by this node within a given Depth.
%% 0 depth means direct references. 'infinite' is the same as named_typerefs/1.
named_typerefs(Record, Depth)->
    Fields = ?ESG:path(Record, fields()),
    lists:append([?RecField:named_typerefs(Field, Depth) || Field <- Fields]).

%% @spec all_typerefs(node(#record{})) -> [node(#typexp{})]
%% @doc Returns the (typexp) types used in this node.
all_typerefs(Record) ->
    Fields = ?ESG:path(Record, fields()),
    lists:append([?RecField:all_typerefs(Field) || Field <- Fields]).

%% @spec all_typerefs(node(#record{}), integer() | infinite) ->
%%     [node(#typexp{})]
%% @doc Returns the (typexp) types referenced by this node within a given Depth.
%% 0 depth means direct references. 'infinite' is the same as all_subtypes/1.
all_typerefs(Record, Depth)->
    Fields = ?ESG:path(Record, fields()),
    lists:append([?RecField:all_typerefs(Field, Depth) || Field <- Fields]).

