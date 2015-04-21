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

%%% @doc Unit test for {@link reflib_form}.
%%% @author István Bozó <bozo_i@inf.elte.hu>

-module(reftest_reflib_form).
-vsn("$Rev: 9568 $").

-compile([export_all]).

-include("test.hrl").

files() ->
    [{module, "test2.erl",
      "-module(test2).\n"
      "-export([fourth/3]).\n"
      "-define(Macro,macro).\n"
      "-import(test1, [third/2]).\n"
      "fourth(X,Y,Z) when true->\n"
      "  length([X, third(Y, Z)]);\n"
      "fourth(X,Y,Z) ->\n"
      "  ok.\n"
      "fifth() ->\n"
      "  ?Macro,\n"
      "  test1:first().\n"
      "sixth(I) when integer(I) ->\n"
      "  integer_to_list(I).\n"
      "seventh() -> \n"
      "  fun sixth/1.\n"}].

data()->
    Mod = ?Query:exec1(?Mod:find(test2), test_module),
    Forms = ?Query:exec(Mod, ?Query:seq(?Mod:file(), ?File:forms())),
    {Mod, Forms}.

test_type() ->
    {_, Forms} = data(),
    [module, export, macro, import, func, func, func, func] =
        [ ?Form:type(Form) || Form <- Forms],
    ok.

test_module() ->
    {Mod, Forms} = data(),
    [ Mod = ?Query:exec1(Form, ?Form:module(), not_unique_mod)||Form <- Forms],
    ok.

test_func() ->
    {_, Forms} = data(),
    [[],[],[],[] | Funs] =
        [ ?Query:exec(Form, ?Form:func()) || Form <- Forms],
    [{fourth,3}, {fifth, 0}, {sixth, 1}, {seventh, 0}] =
        [ {?Fun:name(Func), ?Fun:arity(Func)} || [Func] <- Funs],
    
    ok.

test_clauses() ->
    {_, Forms} = data(),
    F = lists:sublist(Forms, 4, 3),
    [0,2,1] =
        [ length(?Query:exec(Form, ?Form:clauses())) || Form <- F],
    ok.
    
test_clause() ->
    {_, Forms} = data(),
    F = lists:sublist(Forms, 4, 3),
    [[],[_C],[]] =
        [ ?Query:exec(Form, ?Form:clause(2)) || Form <- F],
    [[],[_C1],[_C2]] =
        [ ?Query:exec(Form, ?Form:clause(1)) || Form <- F],
    
    ok.
    
test_file() ->
    [File] = ?Query:exec(?File:find("test2.erl")),
    {_, Forms} = data(),
    [File = ?Query:exec1(Form, ?Form:file(), not_unique_file) || Form <- Forms],
    ok.

test_macros() ->
    {_Mod, Forms} = data(),
    [Macro] = lists:flatten(?Query:exec(Forms, ?Form:macros())),
    "Macro" = ?Macro:name(Macro),
    ok.

test_exprs() ->
    {_, Forms} = data(),
    [0,1,0,2,0,0,0,0] =
        [length(?Query:exec(Form, ?Form:exprs())) || Form <- Forms],
    ok.
