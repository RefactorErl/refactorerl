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

%%% @doc Unit test for {@link reflib_expression}.
%%% @author Melinda Tóth <toth_m@inf.elte.hu>

-module(reftest_reflib_expression).

-vsn("$Rev: 9568 $ ").

-compile([export_all]).

-include("test.hrl").

files() ->
    [{module, "expr.erl",
      "-module(expr).\n"
      "-export([f/1]).\n"
      "-record(rec, {first, second}).\n"
      "f(Rec) ->\n"
      "  S = Rec#rec.first,\n"
      "  H = case S of add -> g(Rec#rec.first); mul -> fun g/1 end,\n"
      "  g(S).\n"
      "g(X) ->\n"
      "  case X of add -> A = 1+1; mul -> A = 2*2 end, \n"
      "  A + A.\n"
      "h(X) ->\n"
      "  g(X), \n"
      "  m:g(X),\n"
      "  mm:s(X). \n"
      "var(A) ->\n"
      "  case A of\n"
      "    x -> ok;\n"
      "    B -> fun(A) -> ok end\n"
      "  end,\n"
      "  C=1, ok.\n"}].

test_visible_vars() ->
    {_, _, [Case, _, Ok], _} = data(var,1),
    [FunOk] = ?Query:exec(Case, ?Query:seq([?Expr:clause(3), ?Clause:body(1),
                                            ?Expr:clause(1), ?Clause:body(1)])),
    InVars = ?Query:exec(FunOk, ?Expr:visible_vars()),
    OutVars = ?Query:exec(Ok, ?Expr:visible_vars()),
    ["A", "B"] = lists:usort([?Var:name(V) || V <- InVars]),
    ["A", "C"] = lists:usort([?Var:name(V) || V <- OutVars]),
    OutVars = OutVars -- InVars,
    ok.

data(Name, Arity) ->
    Mod   = ?Query:exec1(?Mod:find(expr), test_module),
    Fun1  = ?Query:exec1(Mod, ?Mod:local(Name, Arity), test_fun),
    Cl    = ?Query:exec1(Fun1, 
                  ?Query:seq(?Fun:definition(), ?Form:clauses()), test_cl),
    Pat   = ?Query:exec1(Cl, ?Clause:patterns(), test_pattern),
    Expr  = ?Query:exec(Cl, ?Clause:body()),
    {Mod, Cl, Expr, Pat}.

test_clause() ->
    {_Mod, Cl, Exprs, _Pat} = data(g,1),
    [Cl] = ?Query:exec(hd(Exprs), ?Expr:clause()),
    ok.

test_attrib_form() ->
    RecF = ?Query:exec(?Query:exec1(?File:find("expr.erl"), file_not_found), 
                      ?File:form(2)),
    Attrs = ?Query:exec(RecF, ?Form:exprs()),
    RecF = lists:usort(?Query:exec(Attrs, ?Expr:attrib_form())),
    ok.

test_children() ->
    {_Mod, _Cl, Exprs, _Pat} = data(g,1),
    2 = length(?Query:exec(Exprs, ?Expr:children())),
    ok.
    
test_child() ->
    {_Mod, _Cl, Exprs, _Pat} = data(g,1),
    [_H|T] = ?Query:exec(lists:last(Exprs), ?Expr:children()),  
    T = ?Query:exec(lists:last(Exprs), ?Expr:child(2)),    
    ok.

test_parent() ->
    {_Mod, _Cl, [Case | _T], _Pat} = data(g,1),
    MatchE = ?Query:exec(Case, ?Query:seq(?Expr:clauses(), ?Clause:body(1))),
    [Case] = lists:usort(?Query:exec(MatchE, ?Expr:parent())),
    ok.

test_variables() ->
    {_Mod, _Cl, Exprs, _Pat} = data(g,1),
    2 = length(lists:usort(?Query:exec(Exprs, ?Expr:variables()))),
    ok.

test_varbinds() ->
    {_Mod, _Cl, Exprs, _Pat} = data(g,1),
    1 = length(lists:usort(?Query:exec(Exprs, ?Expr:varbinds()))),
    ok.

test_varrefs() ->
    {_Mod, _Cl, Exprs, Pat} = data(f,1),
    VarRec = ?Query:exec(hd(Exprs), ?Expr:varrefs()),
    VarRec = ?Query:exec(Pat, ?Expr:varbinds()),
    ok.

test_records() ->
    {_Mod, _Cl, Exprs, _Pat} = data(f,1),
    Rec = ?Query:exec(?Query:exec1(?File:find("expr.erl"), file_not_found), 
                      ?File:record(rec)),
    Rec = lists:usort(?Query:exec(Exprs, ?Expr:records())),
    ok.
    
test_function() ->
    {_Mod, _Cl, Exprs, _Pat} = data(f,1),
    Fun = ?Query:exec(lists:last(Exprs), ?Expr:function()),
    Fun = ?Query:exec(?Query:exec1(?Mod:find(expr), test_module),
                      ?Mod:local(g,1)),
    ok.

test_functions() ->
    {_Mod, _Cl, Exprs, _Pat} = data(f,1),
    3 = length(?Query:exec(Exprs, ?Expr:functions())),
    ok.

test_funapps() ->
    {_Mod, _Cl, Exprs, _Pat} = data(f,1),
    2 = length(?Query:exec(Exprs, ?Expr:funapps())),
    ok.

test_clauses() ->
    {_Mod, Cl, _Exprs, _Pat} = data(g,1),
    CaseE = ?Query:exec1(Cl, ?Clause:body(1), test_body_i),  
    3 = length(?Query:exec(CaseE, ?Expr:clauses())),
    ok.

test_nameof() ->
    {_Mod, Cl, _Exprs, _Pat} = data(g,1),
    Name = ?Query:exec(Cl, ?Clause:name()),
    [Cl] = ?Query:exec(Name, ?Expr:nameof()),
    ok.

test_modq() ->
    %% partly tested in test_add_modq
    {_Mod, Cl, _Exprs, _Pat} = data(f,1),    
    [App] = ?Query:exec(Cl, ?Clause:body(3)),   
    [] = ?Query:exec(App, ?Expr:modq()), 
    ok.

test_add_modq() ->
    {_Mod, Cl, _Exprs, _Pat} = data(h,1),    
    [App] = ?Query:exec(Cl, ?Clause:body(1)),
    ?Expr:add_modq(App, mod),
    ?ESG:finalize(),
    ModQ = ?Query:exec(App, ?Expr:modq()),
    [Name] = ?Query:exec(ModQ, ?Expr:child(1)),
    mod = (?ESG:data(Name))#expr.value,
    ok.
    
test_del_modq() ->
    {_Mod, Cl, _Exprs, _Pat} = data(h,1),    
    [App] = ?Query:exec(Cl, ?Clause:body(2)),
    ?Expr:del_modq(App),
    ?ESG:finalize(),
    [] = ?Query:exec(App, ?Expr:modq()),
    ok.

test_upd_modq() ->
    {_Mod, Cl, _Exprs, _Pat} = data(h,1),    
    [App] = ?Query:exec(Cl, ?Clause:body(3)),
    ?Expr:upd_modq(App, updmod),
    ?ESG:finalize(),
    ModQ = ?Query:exec(App, ?Expr:modq()),
    [Name] = ?Query:exec(ModQ, ?Expr:child(1)),
    updmod = (?ESG:data(Name))#expr.value,
    ok.

test_top_sub() ->
    {_Mod, _Cl, Exprs, _Pat} = data(g,1),
    Subs = ?Query:exec(Exprs, ?Expr:top_sub()),
    Subs = ?Query:exec(Exprs, ?Expr:sub()),
    4 = length(Subs),
    ok.

test_sub() ->
    {_Mod, Cl, _Exprs, _Pat} = data(f,1),   
    MatchE = ?Query:exec1(Cl, ?Clause:body(2), test_body_i),
    CaseE  = ?Query:exec(MatchE, ?Expr:child(2)),
    Subs = ?Query:exec(CaseE, ?Expr:sub()), 
    2 = length(?Query:exec(MatchE, ?Expr:top_sub())) - length(Subs),
    ok.

test_top_deep_sub() ->
    {_Mod, _Cl, Exprs, _Pat} = data(g,1),
    AllSub = ?Query:exec(Exprs, ?Expr:top_deep_sub()),
    AllSub = ?Query:exec(Exprs, ?Expr:deep_sub()),
    17 = length(AllSub),    
    ok.

test_deep_sub() ->
    {_Mod, Cl, _Exprs, _Pat} = data(f,1),   
    MatchE = ?Query:exec1(Cl, ?Clause:body(2), test_body_i),
    CaseE  = ?Query:exec(MatchE, ?Expr:child(2)),
    AllSub = ?Query:exec(CaseE, ?Expr:deep_sub()), 
    2 = length(?Query:exec(MatchE, ?Expr:top_deep_sub())) - length(AllSub),
    ok.

test_top() ->
    {_Mod, _Cl, Exprs, _Pat} = data(g,1),
    Subs = ?Query:exec(Exprs, ?Expr:sub()),
    [] = Exprs -- lists:usort(?Query:exec(Subs, ?Expr:top())),
    ok.
    
test_top_sup() ->
    {_Mod, _Cl, Exprs, _Pat} = data(g,1),
    true = ?Expr:is_top(hd(Exprs)),
    ok.
