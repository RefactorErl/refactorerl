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

%%% ============================================================================
%%% Module information

%%% @doc In this refactoring all instances of a variable are replaced
%%% with its bound value in that region where the variable is
%%% visible. The variable can be left out where its value is not used.

%%% == Parameters ==
%%% <ul>
%%% <li>A variable (see {@link reflib_args:variable/1}).</li>
%%% </ul>

%%% == Conditions of applicability ==
%%% <ul>
%%% <li>The variable has exactly one binding occurrence on the left hand side of
%%%   a pattern matching expression, and not a part of a compound pattern.</li>
%%% <li>The expression bound to the variable has no side effects.</li>
%%% <li>Every variable of the expression is visible (that is, not shadowed) at
%%%   every occurrence of the variable to be eliminated.</li>
%%% </li>

%%% == Transformation steps and compensations ==
%%% <ol>
%%% <li>Substitute every occurrence of the variable with the expression
%%%   bound to it at its binding occurrence, with parentheses around the
%%%   expression.</li>
%%% <li>If the result of the match expression that binds the variable is
%%%   discarded, remove the whole match expression. Otherwise, replace the match
%%%   expression with its right hand side.</li>
%%% </ol>

%%% == Implementation status ==
%%% This refactoring is fully implemented.

%%% @author Daniel Drienyovszky <monogram@inf.elte.hu>

-module(reftr_elim_var).
-vsn("$Rev: 12913 $"). % for emacs"

%% Callbacks
-export([prepare/1, error_text/2]).

-include("user.hrl").

%%% ============================================================================
%%% Errors

%% @private
error_text(bindings, [VarName]) ->
    ["Variable ", VarName, " has multiple bindings"];
error_text(function, [VarName]) ->
    ["Variable ", VarName, " is a function parameter"];
error_text(bind_not_in_match_expr, [_VarName]) ->
    ["Only variables bound in match expressions can be eliminated"];
error_text(side_effect, [VarName]) ->
    ["The definition of ", VarName, " has side effects"];
error_text(macro, [VarName]) ->
    ["Variable ", VarName, " is used in a macro body"];
error_text(shadowed_vars, [ShadowedVars]) ->
    ["Free variables of the definition would become shadowed: ", ShadowedVars].

%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args) ->
    Var     = ?Args:variable(Args, eliminate),
    VarName = ?Var:name(Var),
    Pattern = ?Query:exec1([Var], ?Var:bindings(),
                           ?LocalError(bindings, [VarName])),
    Match = ?Query:exec1([Pattern], ?Expr:parent(),
                         ?LocalError(function, [VarName])),
    ?Check(?Expr:type(Match) =:= match_expr,
           ?LocalError(bind_not_in_match_expr, [VarName])),
    case ?Query:exec([Match], ?Expr:children()) of
        [Pattern, Def] ->
                [Pattern, Def];
        [Def, Pattern] ->
            [Def, Pattern]
    end,
    ?Check(not lists:any(fun ?Fun:is_dirty/1,
                         ?Query:exec([Def], ?Expr:functions())),
           ?LocalError(side_effect, [VarName])),
    FVars = free_vars(Def),
    Refs = ?Query:exec([Var], ?Var:references()),
    ?Check(not lists:any(fun is_from_macro/1, Refs),
           ?LocalError(macro, [VarName])),
    ShadowedVars = lists:concat([FVars -- visible_vars(Ref) || Ref <- Refs]),
    ShadowedVarNames = lists:map(fun ?Var:name/1, ShadowedVars),
    ?Check(ShadowedVars == [],
           ?LocalError(shadowed_vars, [ShadowedVarNames])),
    DefUsed = def_is_used(Match),
    [fun () ->
             [begin
                 DefCopy = enclose(copy(Def)),
                 replace(R, DefCopy),
                 DefCopy
              end || R <- Refs]
     end,
     fun (Replacements) ->
             ?Transform:touch(Match),
             if not DefUsed -> delete(Match);
                true        -> replace(Match, enclose(copy(Def)))
             end,
             Replacements
     end].

%%% ----------------------------------------------------------------------------
%%% prepare helpers

free_vars(E) ->
    F = ?Query:exec([E], ?Expr:varrefs()),
    B = ?Query:exec([E], ?Expr:varbinds()),
    lists:usort(F) -- B.

visible_vars(E) ->
    ?Query:exec([E], ?Query:seq(?Expr:clause(), ?Clause:variables())).

is_from_macro(E) ->
    [] /= ?Query:exec([E], [{elex, {data, '==', virtual}}]).

def_is_used(Match) ->
    Body = ?Query:exec([Match], ?Query:seq(?Expr:clause(), ?Clause:exprs())),
    FunParam = ?Query:exec([Match], ?Query:seq(?Expr:clause(), ?Clause:patterns())),
    case lists:member(Match, Body) of
        false ->
            true;
        true ->
            case lists:last(Body) of
                Match ->
                    true;
                _ ->
                    case lists:member(Match,FunParam) of
                        true ->
                            true;
                        false ->
                            false
                    end
            end
    end.

%%% ----------------------------------------------------------------------------
%%% transform helpers

copy(Node) ->
    proplists:get_value(Node, ?Syn:copy(Node)).

enclose(Node) ->
    ?Syn:construct({paren, Node}).

replace(From, To) ->
    [{_, Parent}] = ?Syn:parent(From),
    [New] = ?Syn:replace(Parent, {node, From}, [To]),
    New.

delete(Node) ->
    [{_, Parent}] = ?Syn:parent(Node),
    ?Syn:replace(Parent, {node, Node}, []).
