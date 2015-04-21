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

%%% ============================================================================
%%% Module information

%%% @doc This module implements the ``merge expression duplicates'' transformation.
%%%
%%% <h2>Parameters</h2>
%%% <ol>
%%% <li>
%%%     An expression in the Erlang file whose duplicates are to be merged.
%%%     Currently it is specified by
%%%         the file name and
%%%         two positions in the file that delimit the expression.
%%% </li><li>
%%%     A name for the variable that will bind the value of the expression.
%%%     This is a string that must conform to the requirement on variables names of
%%%     the Erlang language.
%%% </li></ol>
%%%
%%% <h2>Side conditions</h2>
%%% <ol>
%%% <li>
%%%     The expression cannot be substituted if any of its subexpressions
%%%     have side effects.
%%% </li><li>
%%%     The transformation cannot be executed if the expression is
%%%     in the head of a list comprehension,
%%%     in a pattern or
%%%     in a guard expression.
%%% </li><li>
%%%     If the expression occurs in a generator expression, it should not contain
%%%     variables that are bound by generator patterns.
%%% </li><li>
%%%     The given variable name should not already exist in the given scope
%%%     in order to avoid name clashes.
%%% </li><li>
%%%     The transformation should not destroy variable bindings.
%%% </li></ol>
%%%
%%%
%%% @author Robert Kitlei <kitlei@inf.elte.hu>

-module(reftr_merge).
-vsn("$Rev: 12913 $").

%% Callbacks
-export([prepare/1, error_text/2]).

-include("user.hrl").

%%% ============================================================================
%%% Errors

%% @private
error_text(no_inst, []) ->
    % This should be impossible
    ["No instance of the expression is available for refactoring"];
error_text(dirty_fun, []) ->
    ["The selection has functions with possible side effects"];
error_text(message_passing, []) ->
    ["The selection contains message passing"];
error_text(has_var_binding, BoundVars) ->
    VarNames = [?Var:name(V) || V <- BoundVars],
    ["The selection contains bindings for ",
     ?MISC:plural("variable", VarNames), " ",
     ?MISC:flatjoin(VarNames, ", ")].

%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args) ->
    Exprs         = ?Args:expr_range(Args),
    ?Check(1 == length(Exprs), ?RefError(token_parent, [expr])),
    [Expr]        = Exprs,

    Disqual  = instance_disqualifiers(Expr),
    ?Check( [] == Disqual, ?RefError(bad_kind, Disqual)),

    Insts = instances(Expr),
    ?Check([] /= Insts, ?LocalError(no_inst, [])),
    ?Check(not ?Expr:has_message_passing(Expr),
           ?LocalError(message_passing, [])),
    ?MISC:is_sideeffect_check_on() andalso
        ?Check(not ?Expr:has_dirty_fun(Expr),
               ?LocalError(dirty_fun, [])),

    BoundVars = ?Query:exec(Expr, ?Expr:varbinds()),
    ?Check( [] == BoundVars, ?LocalError(has_var_binding, BoundVars)),

    VarNames = [?Var:name(V) || V <- ?Query:exec(Expr, ?Expr:scope_varbinds())],
    % todo Add transformation info
    VarName  = ?Args:ask(Args, varname, fun cc_varname/2, fun cc_error/3, VarNames),

    % @todo Warn if expression is only instance.
    InstParents         = [{?Syn:parent(Inst), Inst} || Inst <- Insts],
    InsLoc = {TopCl, _} = insertion_location(Insts),

    ?Transform:touch(TopCl),
    [fun() ->
            MatchExpr = new_match_expr(VarName, Expr),
            insert_new_match_expr(MatchExpr, InsLoc),
            []
     end,
     [fun([]) -> %@todo bkil remove []
        change_instance(VarName, Parent, Inst)
      end || {[{_Link, Parent}], Inst} <- InstParents]].

%%% ============================================================================
%%% Transformation

%% @doc Creates the new match expression.
new_match_expr(NewName, Expr) ->
    CopyOrig = ?ESG:copy(Expr),
    {_, ExprCopy} = lists:keyfind(Expr, 1, CopyOrig),
    ?Syn:construct({match_expr, {var, NewName}, ExprCopy}).

%% @doc Inserts the new match expression into its new location.
%% @todo Replace the original expression if necessary.
insert_new_match_expr(MatchExpr, {TopCl, PrevBody}) ->
    InsFun =
        fun(Body) ->
            {PrevBody, Body2} = lists:split(length(PrevBody), Body),
            PrevBody ++ [MatchExpr|Body2]
        end,
    ?Syn:replace(TopCl, body, InsFun).

%% @doc Replaces the instance of the expression with a new variable.
%% @todo Remove superfluous parentheses.
change_instance(NewVarName, Parent, Inst) ->
    NewVar = ?Syn:construct({var, NewVarName}),
    ?Syn:replace(Parent, {node, Inst}, [NewVar]),
    NewVar.


%%% ============================================================================
%%% Checks

cc_varname(VarName, ScVarNames) ->
    ?Check(not lists:member(VarName, ScVarNames), ?RefError(var_exists, VarName)),
    VarName.

cc_error(?RefError(var_exists, VarName), VarName, _ScVarNames) ->
    ?MISC:format("Variable ~p already exists.", [VarName]).

%%% ============================================================================
%%% Implementation

%% @doc Returns all instances of the expression.
%% Includes the original expression as well.
instances(Expr) ->
    ExprType = ?Expr:role(Expr),
    [ E ||  E <- ?Query:exec([Expr], ?Query:seq([   ?Expr:clause(),
                                                    ?Clause:form(),
                                                    ?Form:deep_exprs()])),
            ExprType == ?Expr:role(E),
            ?Expr:is_same_expr({E, Expr}),
            [] == instance_disqualifiers(E) ].

%% @doc Returns the list of conditions that disqualify the expression
%% from being allowed as an instance.
%% The list is empty if the expression can be allowed as an instance.
instance_disqualifiers(Expr) ->
    UpKinds = [?Expr:type(PathNode)
                    || {_, PathNode} <- lists:reverse(?Syn:root_path(Expr)),
                        ?Expr:is_expr(PathNode) ],

    {UpLinks, _} = lists:unzip(?Syn:root_path(Expr)),

    Invalidating    = [ {list_gen,  lists:member(list_gen, UpKinds)},
                        {guard,     lists:member(guard, UpLinks)},
                        {pattern,   lists:member(pattern, UpLinks)},
                        {list_comp, lists:member(list_comp, UpKinds)} ],

    [Invalid || {Invalid, true} <- Invalidating].

%% @doc Determines the insertion point of the new match expression.
%% Returns the insertion parent clause and the previos elements in its body.
insertion_location(Insts) ->
    TopCl             = lowest_clause(common_parent_clause(Insts)),
    BodyExprs         = ?Query:exec([TopCl], ?Clause:body()),
    BodyExprsWithInst = body_exprs_with_nodes(Insts, TopCl),
    OutsideBoundVars  =
        ?MISC:flatsort(
            [?Query:exec([Inst], ?Expr:varrefs())  || Inst <- Insts] --
            [?Query:exec([Inst], ?Expr:varbinds()) || Inst <- Insts]),
    {BodyBeforeInsts, _BodyInstsPart} =
        lists:splitwith(
            fun(BodyExpr) -> not lists:member(BodyExpr, BodyExprsWithInst) end,
            BodyExprs),
    {BodyVarMatches, _BodyRest} =
        var_bound_body_exprs(OutsideBoundVars, BodyBeforeInsts, TopCl),
    {TopCl, BodyVarMatches}.


%% @doc Separates the prefix of the body expressions that binds
%% variables in the given list.
%% The bindings inside an instance are not included.
var_bound_body_exprs(Vars, BodyExprs, TopCl) ->
    Bindings      = [?Query:exec([Var], ?Expr:binding_vars()) || Var <- Vars],
    UBindings     = ?MISC:flatsort(Bindings),
    BodyVarExprs  = body_exprs_with_nodes(UBindings, TopCl),
    var_bound_body_exprs2(BodyVarExprs, [], BodyExprs).

%% @doc Searches the body until all of the bindings are found.
var_bound_body_exprs2(_, Collected, []) ->
    {lists:reverse(Collected), []};
var_bound_body_exprs2([], Collected, BodyExprs) ->
    {lists:reverse(Collected), BodyExprs};
var_bound_body_exprs2(BodyVarExprs, Collected, [BodyExpr|BodyExprs]) ->
    var_bound_body_exprs2(BodyVarExprs -- [BodyExpr], [BodyExpr|Collected], BodyExprs).

%% @doc Expressions immediately below the top clause that contain an instance.
body_exprs_with_nodes(Nodes, TopCl) ->
    NodesWithLinks =
        [ lists:dropwhile(fun({_, PathNode}) -> PathNode /= TopCl end,
                            ?Syn:root_path(Node))
            || Node <- Nodes],
    [ Node  || [{_, _}, {_, Node}|_] <- NodesWithLinks].

%% @doc Returns the lowest clause in the path.
lowest_clause(Path) ->
    [Cl|_] = [C ||  {_, C} <- lists:reverse(Path),
                    ?Syn:node_type(C) == clause],
    Cl.

%% @doc Returns the lowest clause that is ancestor to all of the expressions.
common_parent_clause([Expr]) ->
    ?Syn:root_path(Expr);
common_parent_clause(Exprs) ->
    [Path1|Paths] = [?Syn:root_path(Expr) || Expr <- Exprs],
    lists:foldl(fun ?MISC:common_prefix/2, Path1, Paths).
