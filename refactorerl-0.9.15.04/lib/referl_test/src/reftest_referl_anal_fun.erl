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

%%% @doc Unit test for {@link referl_anal_fun}.
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>

-module(reftest_referl_anal_fun).
-vsn("$Rev: 9568 $ ").

-compile([export_all]).

-include("test.hrl").

files() ->
    [{module, "m1.erl",
      "-module(m1).          \n"
      "-export([f/0, g/0]).  \n"
      "f() -> g(), fun f/0.  \n"
      "g() -> ok.            \n"
     }].

test_modify_clause_name() ->
    M1 = ?Query:exec1(?Mod:find(m1), mod_not_found),
    F  = ?Query:exec1(M1, ?Fun:find(f, 0), fun_not_found),
    FF = ?Query:exec1(F, ?Fun:definition(), form_not_found),
    FC = ?Query:exec1(FF, ?Form:clause(1), clause_not_found),
    NameExpr = ?Query:exec1(FC, ?Clause:name(), clause_name_not_found),

    ?Syn:replace(NameExpr, {elex, 1}, ["newfunname"]),
    ?ESG:finalize(),

    [NewF] = ?Query:exec(M1, ?Fun:find(newfunname, 0)),
    [NewF] = ?Query:exec(FF, ?Form:func()),

    ?Syn:replace(NameExpr, {elex, 1}, ["f"]),
    ?ESG:finalize(),
    ok.

test_modify_clause_patterns() ->
    M1 = ?Query:exec1(?Mod:find(m1), mod_not_found),
    F  = ?Query:exec1(M1, ?Fun:find(f, 0), fun_not_found),
    FF = ?Query:exec1(F, ?Fun:definition(), form_not_found),
    FC = ?Query:exec1(FF, ?Form:clause(1), clause_not_found),

    NewPattern = ?Syn:create(#expr{role=pattern,type=variable,value="X"},["X"]),
    ?Syn:replace(FC, {pattern, 0, 0}, [NewPattern]),
    ?ESG:finalize(),

    [NewF] = ?Query:exec(M1, ?Fun:find(f, 1)),
    [NewF] = ?Query:exec(FF, ?Form:func()),

    ?Syn:replace(FC, {node, NewPattern}, []),
    ?ESG:finalize(),

    [F] = ?Query:exec(FF, ?Form:func()),

    ok.

test_modify_application_name() ->
    M1 = ?Query:exec1(?Mod:find(m1), mod_not_found),
    G  = ?Query:exec1(M1, ?Fun:find(g, 0), fun_not_found),
    [App|_] = ?Query:exec(G, ?Fun:applications()),
    [NameExpr|_] = ?Query:exec(App, ?Expr:children()),

    ?Syn:replace(NameExpr, {elex, 1}, ["newfunname"]),
    ?ESG:finalize(),

    [NewG] = ?Query:exec(M1, ?Fun:find(newfunname, 0)),
    [NewG] = ?Query:exec(App, ?Expr:function()),
    [App]  = ?Query:exec(NewG, ?Fun:applications()),

    ?Syn:replace(NameExpr, {elex, 1}, ["g"]),
    ?ESG:finalize(),
    ok.

test_modify_application_args() ->
    M1 = ?Query:exec1(?Mod:find(m1), mod_not_found),
    G  = ?Query:exec1(M1, ?Fun:find(g, 0), fun_not_found),
    [App|_] = ?Query:exec(G, ?Fun:applications()),
    [Arg] = ?Query:exec(App,[{esub,{type,'==',arglist}}]),

    NewArg = ?Syn:construct({atom,x}),
    ?Syn:replace(Arg, {esub, 0, 0}, [NewArg]),
    ?ESG:finalize(),

    [NewG] = ?Query:exec(M1, ?Fun:find(g, 1)),
    [NewG] = ?Query:exec(App, ?Expr:function()),
    [App]  = ?Query:exec(NewG, ?Fun:applications()),
    ?Syn:replace(Arg, {node, NewArg}, []),
    ?ESG:finalize(),
    ok.

test_xmodify_application_modq() ->
    M1 = ?Query:exec1(?Mod:find(m1), mod_not_found),
    G  = ?Query:exec1(M1, ?Fun:find(g, 0), fun_not_found),
    [App|_] = ?Query:exec(G, ?Fun:applications()),

    ?Expr:add_modq(App, m2),
    ?ESG:finalize(),

    M2 = ?Query:exec1(?Mod:find(m2), mod_not_found),
    M2_G  = ?Query:exec1(M2, ?Fun:find(g, 0), fun_not_found),
    [M2_G] = ?Query:exec(App, ?Expr:function()),
    [App] = ?Query:exec(M2_G, ?Fun:applications()),

    ?Expr:upd_modq(App, m1),
    ?ESG:finalize(),

    [G] = ?Query:exec(App, ?Expr:function()),
    [App] = ?Query:exec(G, ?Fun:applications()),

    ?Expr:del_modq(App),
    ?ESG:finalize(),

    [G] = ?Query:exec(App, ?Expr:function()),
    [App] = ?Query:exec(G, ?Fun:applications()),
    ok.

test_modify_export_item_name() ->
    M1 = ?Query:exec1(?Mod:find(m1), mod_not_found),
    F  = ?Query:exec1(M1, ?Fun:find(f, 0), fun_not_found),
    [ExportItem|_] = ?Query:exec(F, ?Fun:exports()),
    [NameExpr,_ArityExpr] = ?Query:exec(ExportItem, ?Expr:children()),

    ?Syn:replace(NameExpr, {elex, 1}, ["newfunname"]),
    ?ESG:finalize(),

    [NewF] = ?Query:exec(M1, ?Fun:find(newfunname, 0)),
    [NewF] = ?Query:exec(ExportItem, ?Expr:function()),
    [NewF] = ?Query:exec(M1, ?Mod:exported(newfunname, 0)),

    ?Syn:replace(NameExpr, {elex, 1}, ["f"]),
    ?ESG:finalize(),
    ok.

test_modify_export_item_arity() ->
    M1 = ?Query:exec1(?Mod:find(m1), mod_not_found),
    F  = ?Query:exec1(M1, ?Fun:find(f, 0), fun_not_found),
    [ExportItem|_] = ?Query:exec(F, ?Fun:exports()),
    [_NameExpr,ArityExpr] = ?Query:exec(ExportItem, ?Expr:children()),

    ?Syn:replace(ArityExpr, {elex, 1}, ["2"]),
    ?ESG:finalize(),

    [NewF] = ?Query:exec(M1, ?Fun:find(f, 2)),
    [NewF] = ?Query:exec(ExportItem, ?Expr:function()),
    [NewF] = ?Query:exec(M1, ?Mod:exported(f, 2)),

    ?Syn:replace(ArityExpr, {elex, 1}, ["0"]),
    ?ESG:finalize(),
    ok.
