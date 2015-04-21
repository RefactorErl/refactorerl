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
%%% The Initial Developer of the Original Code is E�tv�s Lor�nd University.
%%% Portions created  by E�tv�s Lor�nd University and ELTE-Soft Ltd.
%%% are Copyright 2007-2013 E�tv�s Lor�nd University, ELTE-Soft Ltd.
%%% and Ericsson Hungary. All Rights Reserved.

%%% @doc This module contains functions that return a query that expects a
%%% clause semantical node as starting point.
%%%
%%% @author Istvan Bozo <bozo_i@inf.elte.hu>

-module(reflib_clause).
-vsn("$Rev: 10280 $ ").

-include("lib.hrl").

%% =============================================================================
%% Exports

%% Queries
-export([var/1, type/1]).
-export([name/0, patterns/0, pattern/1, guard/0, funcl/0,
         body/0, body/1, exprs/0, expr/1, scope/0, subscopes/0, form/0,
         variables/0, variable/1]).
-export([is_same_clause/1, is_clause/1]).

%% =============================================================================
%% Clause properties

%% @spec var(node())-> atom()
%% @doc Returns the variable visibility class.
var(Clause) -> (?ESG:data(Clause))#clause.var.

%% @spec type(node())-> atom()
%% @doc Returns the type of a clause.
type(Clause) -> (?ESG:data(Clause))#clause.type.

%% =============================================================================
%% Queries starting from clause nodes

%% @spec name() -> query(#clause{}, #expr{})
%% @doc The result query returns the name expressions of the clause.
name() ->
    [name].

%% @spec patterns() -> query(#clause{}, #expr{})
%% @doc The result query returns the patterns of the clause.
patterns() -> [pattern].

%% @spec pattern(integer()) -> query(#clause{}, #expr{})
%% @doc The result query returns the `I'th pattern of the clause.
pattern(I) ->
    [{pattern,I}].

%% @spec guard() -> query(#clause{}, #expr{})
%% @doc The result query returns the guard expressions of the clause.
guard() -> [guard].

%% @spec body() -> query(#clause{}, #expr{})
%% @doc The result query returns the top-level expressions from the body of the
%% clause.
body() -> [body].

%% @spec body(integer()) -> query(#clause{}, #expr{})
%% @doc The result query returns the `I'th top-level expression from the body
%% of the clause.
body(I) -> [{body, I}].

%% @spec exprs() -> query(#clause{}, #expr{})
%% @doc The result query returns the top-level expressions of the clause.
exprs() ->
    [visib].

%% @spec expr(integer()) -> query(#clause{}, #expr{})
%% @doc The result query returns the `I'th top level expression of the clause.
expr(I) ->
    [{visib, I}].

%% @spec scope() -> query(#clause{}, #clause{})
%% @doc The result query returns the scope of the clause.
scope() ->
    [scope].

%% @spec subscopes() -> query(#clause{}, #clause{})
%% @doc The result query returns the direct subsclopes of a scope clause.
subscopes() ->
    [{scope, back}].

%% @spec funcl() -> query(#clause{}, #clause{})
%% @doc The result query returns the function clause which contains the clause.
funcl()->
    ?Query:seq(scope(), [functx]).

%% @spec form() -> query(#clause{}, #form{})
%% @doc The result query returns the form which contains the clause.
form() ->
    ?Query:seq(funcl(), [{funcl,back}]).

%% @spec variables() -> query(#clause{}, #variable{})
%% @doc The result query returns the variables that are defined in the scope
%% that contains the clause.
variables() ->
    [scope, vardef].

%% @spec variable(string()) -> query(#clause{}, #variable{})
%% @doc The result query returns the variable `VarName' that is defined in the
%% scope of the clause.
variable(VarName) ->
    [scope, {vardef, {name, '==', VarName}}].
%% =============================================================================
%% Clause related functions
is_clause(MaybeClause)->
    ?Syn:class(MaybeClause) == clause.

%% @spec is_same_clause({#clause{}, #clause{}}) -> bool()
%% @doc  Returns whether the two clauses are the same, disregarding whitespace.
is_same_clause({C, C}) ->
    true;
is_same_clause({C1, C2}) ->
    type(C1) == type(C2) andalso clause_diffs(C1, C2) == [].

clause_diffs(C1, C2) ->
    Infos1 = clause_data(C1),
    Infos2 = clause_data(C2),
    [ d ||  {X1, X2} <- lists:zip(Infos1, Infos2),
            length(X1) /= length(X2) orelse
            not lists_all(fun ?Expr:is_same_expr/1, lists:zip(X1, X2))].

%% todo What is the difference between this function and lists:all/2?
%% todo Should be moved to ?MISC.
lists_all(_Fun, []) -> true;
lists_all(Fun, [X|Xs]) ->
    case Fun(X) of
        true  -> lists_all(Fun, Xs);
        false -> false
    end.

%% Collects data about the clause.
clause_data(Clause) ->
    Patterns   = ?Query:exec([Clause], patterns()),
    Guards     = ?Query:exec([Clause], guard()),
    Body       = ?Query:exec([Clause], body()),
    [Patterns, Guards, Body].
