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

%%% @doc High level function specification-related operations. This module
%%% contains functions that expect a spec semantical node as their parameter
%%% (or return a query that expects a spec semantical node as starting point).
%%%
%%% @author Denes Peteri <petden@caesar.elte.hu>

-module(reflib_spec).
-vsn("$Rev$"). % for emacs "

%% =============================================================================
%% Exports

-export([name/1, arity/1]).
-export([clause/2, has_func/1]).
-export([all_typerefs/1, all_typerefs/2, named_typerefs/1, named_typerefs/2]).
-export([find/2]).
-export([definition/0, def_file/0, module/0, references/0]).
-export([clauses/0, returntypes/0]).

-include("lib.hrl").

%% =============================================================================
%% Spec related properties

%% @spec name(node(#spec{})) -> atom()
%% @doc Returns the name of the spec.
name(Spec) -> (?ESG:data(Spec))#spec.name.

%% @spec arity(node(#spec{})) -> integer()
%% @doc Returns the arity of the spec.
arity(Spec) -> (?ESG:data(Spec))#spec.arity.

%% @spec clause(node(#spec{}), integer()) -> [node(#specclause{})]
%% @doc The Index-th clause.
clause(Spec, Index) ->
    ?ESG:path(Spec, [{specclause, Index}]).

%% @spec named_typerefs(node(#spec{})) -> [node(#namedtype{})]
%% @doc Returns the namedtypes used by this node.
named_typerefs(Spec) ->
    Clauses = ?ESG:path(Spec, clauses()),
    lists:append([?SpecClause:named_typerefs(Clause) || Clause <- Clauses]).

%% @spec named_typerefs(node(#spec{}), integer() | infinite) ->
%%     [node(#namedtype{})]
%% @doc Returns the namedtypes referenced by this node within a given Depth.
%% 0 depth means direct references. `infinite' is the same as named_typerefs/1.
named_typerefs(Spec, Depth)->
    Clauses = ?ESG:path(Spec, clauses()),
    lists:append([?SpecClause:named_typerefs(Clause, Depth) ||
                  Clause <- Clauses]).

%% @spec all_typerefs(node(#spec{})) -> [node(#typexp{})]
%% @doc Returns the (typexp) types used in this node.
all_typerefs(Spec) ->
    Clauses = ?ESG:path(Spec, clauses()),
    lists:append([?SpecClause:all_typerefs(Clause) || Clause <- Clauses]).

%% @spec all_typerefs(node(#spec{}), integer() | infinite) ->
%%     [node(#typexp{})]
%% @doc Returns the (typexp) types referenced by this node within a given Depth.
%% 0 depth means direct references. `infinite' is the same as all_subtypes/1.
all_typerefs(Spec, Depth)->
    Clauses = ?ESG:path(Spec, clauses()),
    lists:append([?SpecClause:all_typerefs(Clause, Depth) || Clause <- Clauses]).

%% @spec has_func(node(#spec{})) -> bool()
%% @doc Returns `true' if the specification has a function.
has_func(Spec) ->
    case ?ESG:path(Spec, references()) of
        [] -> false;
        _ -> true
    end.

%% =============================================================================
%% Queries starting from specs

%% @spec find(atom(), integer()) -> query(#module{}, #spec{})
%% @doc The result query returns the spec with name `Name' and arity `Arity'.
find(Name, Arity) ->
    [{type, {{name, '==', Name}, 'and', {arity, '==', Arity}}}].

%% @spec definition() -> query(#spec{}, #form{})
%% @doc The defining form.
definition() ->
    [{specdef, back}].

%% @spec def_file() -> query(#spec{}, #file{})
%% @doc The file, the spec is defined in.
def_file() ->
    definition() ++ [{form, back}].

%% @spec module() -> query(#spec{}, #module{})
%% @doc The spec pertains to the returned module.
%% If the spec form has a `module_qualifier', the spec node will be under the
%% module referenced there. Otherwised it will be under the module, it is
%% defined in.
module() ->
    [{spec, back}].

%% @spec references() -> query(#spec{}, #func{})
%% @doc Returns the functions referencing this spec.
references() ->
    [{specref, back}].

%% @spec clauses() -> query(#spec{}, #specclause{})
%% @doc Returns the clauses of the specification.
clauses() ->
    [specclause].

%% @spec returntypes() -> query(#spec{}, #typexp{})
%% @doc Possible return types of the specification.
returntypes() ->
    ?Query:seq([
        clauses(),
        ?SpecClause:specret(),
        ?SpecParam:definition()]).


