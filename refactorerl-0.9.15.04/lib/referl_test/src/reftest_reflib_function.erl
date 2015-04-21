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

%%% @doc Unit test for {@link reflib_function}.
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(reftest_reflib_function).
-vsn("$Rev: 9568 $").

-compile([export_all]).

-include("test.hrl").

files() ->
    [{module, "test1.erl",
      "-module(test1).\n"
      "-export([first/0, third/2]).\n"
      "first() ->\n"
      "  second(1).\n"
      "second(A) ->\n"
      "  third(A, 2).\n"
       "third(B, C) ->\n"
      "  {B, C}.\n"},
     {module, "test2.erl",
      "-module(test2).\n"
      "-export([fourth/3]).\n"
      "-import(test1, [third/2]).\n"
      "fourth(X,Y,Z) ->\n"
      "  length([X, third(Y, Z)]).\n"
      "fifth() ->\n"
      "  test1:first().\n"
      "sixth(I) when integer(I) ->\n"
      "  integer_to_list(I).\n"
      "seventh() -> \n"
      "  fun sixth/1,\n"
      "  element(1, {fun sixth/1, 0}).\n"}].

test_find() ->
    ok.

test_module() ->
    Mod = ?Query:exec1(?Mod:find(test1), test_module),
    F1 = ?Query:exec1(Mod, ?Mod:local(first, 0), test_fun),
    Mod = ?Query:exec1(F1, ?Fun:module(), not_unique_mod),
    ok.

test_definition() ->
    F4 = ?Query:exec1(?Query:seq(?Mod:find(test2), ?Mod:local(fourth, 3)),
                      test_fun),
    Def = ?Query:exec1(F4, ?Fun:definition(), not_unique_fundef),
    Def = ?Query:exec1(
             ?Query:seq([?Mod:find(test2),
                         ?Mod:file(),
                         ?File:form(4)]),
             test_form),
    ok.

test_exported() ->
    Mod = ?Query:exec1(?Mod:find(test1), test_module),
    F1 = ?Query:exec1(Mod, ?Fun:find(first, 0), test_fun),
    F2 = ?Query:exec1(Mod, ?Fun:find(second, 1), test_fun),
    true = ?Fun:is_exported(F1),
    false = ?Fun:is_exported(F2),
    ok.

test_applications() ->
    F3 = ?Query:exec1(?Query:seq(?Mod:find(test1), ?Mod:local(third,2)),
                      test_fun),
    %% reflib_function:applications/0
    2 = length(?Query:exec(F3, ?Fun:applications())),
    [E|_] = ?Query:exec(?Query:seq([?Mod:find(test2),
                                    ?Fun:find(fourth, 3),
                                    ?Fun:definition(),
                                    ?Form:clauses(),
                                    ?Clause:body()])),
    %% reflib_function:applications/1
    1 = length(?Query:exec(F3, ?Fun:applications(E))),
    ok.

test_implicits() ->
    F6 = ?Query:exec1(?Query:seq(?Mod:find(test2), ?Mod:local(sixth,1)),
                      test_fun),
    %% reflib_function:implicits/0
    2 = length(?Query:exec(F6, ?Fun:implicits())),
    [_, E2|_] = ?Query:exec(?Query:seq([?Mod:find(test2),
                                        ?Fun:find(seventh, 0),
                                        ?Fun:definition(),
                                        ?Form:clauses(),
                                        ?Clause:body()])),
    %% reflib_function:implicits/1
    1 = length(?Query:exec(F6, ?Fun:implicits(E2))),
    ok.

test_exports() ->
    F1 = ?Query:exec1(?Query:seq(?Mod:find(test1),
                                 ?Fun:find(first, 0)), test_fun),
    Export = ?Query:exec1(F1, ?Fun:exports(), export),
    F1 = ?Query:exec1(Export, ?Expr:function(), function),
    ok.

test_imports() ->
    F3 = ?Query:exec1(?Query:seq(?Mod:find(test1), ?Mod:local(third,2)),
                      test_fun),
    Import = ?Query:exec1(F3, ?Fun:imports(), import),
    F3 = ?Query:exec1(Import, ?Expr:function(), function),
    ok.

test_imported() ->
    Mod = ?Query:exec1(?Mod:find(test2), test_module),
    F3 = ?Query:exec1(?Query:seq(?Mod:find(test1), ?Mod:local(third,2)),
                      test_fun),
    [Mod] = ?Query:exec(F3, ?Fun:imported()),
    ok.

test_0_add_export() ->
    Mod = ?Query:exec1(?Mod:find(test1), test_module),
    F2 = ?Query:exec1(Mod, ?Fun:find(second, 1), test_fun),
    ?Fun:add_export(F2),
    ?ESG:finalize(),
    true = ?Fun:is_exported(F2),
    ok.

test_1_del_export() ->
    Mod = ?Query:exec1(?Mod:find(test1), test_module),
    F2 = ?Query:exec1(Mod, ?Fun:find(second, 1), test_fun),
    ?Fun:del_export(F2),
    ?ESG:finalize(),
    false = ?Fun:is_exported(F2),
    ok.
