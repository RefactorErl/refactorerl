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

%%% @doc This module contains functions that return a query that expects a form
%%% semantical node as starting point.
%%%
%%% @author Istvan Bozo <bozo_i@inf.elte.hu>

-module(reflib_form).
-vsn("$Rev: 10567 $ ").

%% =============================================================================
%% Exports

-export([type/1]).

%% Properties
-export([hash/1]).
-export([form_length/1]).

%% Queries
-export([clauses/0, clause/1, file/0]).
-export([module/0, func/0, spec/0, type/0, record/0, exprs/0, expr/1, deep_exprs/0, macros/0,
         records/0]).

-include("lib.hrl").

%%% ============================================================================
%%% Form related properties

%% @spec type(node()) -> atom()
%% @doc Returns the type of a form as an atom. The returned atom is
%% the type (e.g. export, import) by module attributes and `macro' by
%% macro definition forms.
type(Form) ->
    #form{type=Type, tag=Tag} = ?Graph:data(Form),
    case Type of
        define -> macro;
        attrib -> Tag;
        _      -> Type
    end.

hash(Form) ->
    (?ESG:data(Form))#form.hash.

%% @spec form_length(node()) -> integer()
%% @doc Returns the form length (the way we calculate it depends on the current
%% positioning mode)  
form_length(Form) ->
    ?Token:form_length(Form).

%%% ============================================================================
%%% Form related queries

%% @spec module() -> query(#form{}, #module{})
%% @doc The result query returns the module node which contains the form.
module() ->
    [{form, back}, moddef].

%% @spec func() -> query(#form{}, #func{})
%% @doc The result query returns the function defined by the form.
func() ->
    [fundef].

%% @spec spec() -> query(#form{}, #spec{})
%% @doc The result query returns the specification defined by the form.
spec() ->
    [specdef].

%% @spec type() -> query(#form{}, #namedtype{})
%% @doc The result query returns the type defined by the form.
type() ->
    [typedef].

%% @spec record() -> query(#form{}, #record{})
%% @doc The result query returns the record defined by the form.
record() ->
    [recdef].

%% @spec clauses() -> query(#form{}, #clause{})
%% @doc The result query returns the clauses of the form.
clauses() ->
    [funcl].

%% @spec clause(integer()) -> query(#form{}, #clause{})
%% @doc The result query returns the clause of the form with the given `I'
%% index.
clause(I) ->
    [{funcl, I}].

%% @spec file() -> query(#form{}, #file{})
%% @doc The result query returns the file node which contains the form.
file() -> [{form, back}].

%% @spec macros() -> query(#form{}, #form{})
%% @doc The result query returns the macro definitions of the macros used in
%% the form. In the result list any macro definition may occure in multiple
%% number. It is applicable for any form, except the macro definition forms
%% (in this case the result always will be an empty list).
macros() ->
    ?Query:any(
       ?Query:seq([
                   ?Form:clauses(),
                   ?Query:all(?Clause:exprs(), ?Clause:name()),
                   ?Expr:deep_sub(),
                   [{elex,1},{llex,1},{mref,1}]]),
       ?Query:seq([
                   exprs(),
                   ?Expr:deep_sub(),
                   [{elex,1},{llex,1},{mref,1}]])).

%% @spec records() -> query(#form{}, #record{})
%% @doc The result query returns the record definitions of the records used in
%% the form. In the result list any record definition may occure in multiple
%% number.
records() ->
    ?Query:any(
       ?Query:seq([
                   ?Form:clauses(),
                   ?Clause:exprs(),
                   ?Expr:records()]),
       ?Query:seq([
                   exprs(),
                   ?Expr:deep_sub(),
                   ?Expr:records()])).


%% @spec exprs() -> query(#form{}, #expr{})
%% @doc The result query returns subexpressions directly under the form
%% (mainly useful in attribute forms)
exprs() -> [eattr].

%% @spec expr(integer()) -> query(#form{}, #expr{})
%% @doc The result query returns the `I'th subexpression directly under the form
%% (mainly useful in attribute forms)
expr(I) -> [{eattr, I}].

%% @spec deep_exprs() -> query(#form{}, #expr{})
%% @doc The result query returns subexpressions under the form
%%      (either directly or by subexpressions).
deep_exprs() ->
    ?Query:all( exprs(),
                ?Query:seq([clauses(), ?Clause:exprs(), ?Expr:deep_sub()])).
