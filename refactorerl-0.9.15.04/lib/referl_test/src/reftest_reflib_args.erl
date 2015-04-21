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

%%% @doc Unit test for {@link reflib_args}.
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(reftest_reflib_args).
-vsn("$Rev: 9568 $ ").

-compile([export_all]).

-include("test.hrl").


files() ->
    [{module, "test.erl",
      "-module(test).\n"
      "f(X) -> X.\n"
      "g() -> fun f/1.\n"
      "h() -> erlang:error(test).\n"
      "i(A,B) ->\n"
      "  A, B, {A,B}.\n"
      "-define(M(X), X*X).\n"
      "j() -> ?M(3+4).\n"
      "-define(S, single).\n"
      "k() -> ?S.\n"
      "-record(rek, {egy, ketto}).\n"
      "l() -> #rek{egy=1}.\n"
     }].

%% Enhancement #180
test_subexp_range() ->
    Exprs = ?Query:exec(
               ?Query:seq([?Mod:find(test), ?Mod:local(i,2),
                           ?Fun:definition(), ?Form:clauses(),
                           ?Clause:body(3), ?Expr:children()])),
    Exprs = ?Args:expr_range([{file,"test.erl"}, {posrange, {89, 91}},
                              {ask_missing, false}]),
    ok.


%% Bug #162
test_macro_range() ->
    Expr1 = ?Query:exec1(
              ?Query:seq([?Mod:find(test), ?Mod:local(j,0),
                          ?Fun:definition(), ?Form:clauses(), ?Clause:body()]),
              test_expr),
    [Expr1] = ?Args:expr_range([{file,"test.erl"},{posrange,{122,128}},
                                {ask_missing, false}]),
    Expr2 = ?Query:exec1(
               ?Query:seq([?Mod:find(test), ?Mod:local(k,0),
                           ?Fun:definition(), ?Form:clauses(), ?Clause:body()]),
               test_expr),
    [Expr2] = ?Args:expr_range([{file,"test.erl"},{posrange,{158,159}},
                                {ask_missing, false}]),
    ok.

test_string() ->
    "alma" = ?Args:string([{text, "alma"}, {ask_missing, false}]),
    "alma" = ?Args:string([{text, alma}, {ask_missing, false}]),
    try
        ?Args:string([{text, 123}]),
        error
    catch
        throw:Err ->
            false = lists:prefix("Unknown",
                                 lists:flatten(?Error:error_text(Err))),
            ok
    end.

test_integer() ->
    123 = ?Args:integer([{number, 123}, {ask_missing, false}]),
    try
        ?Args:integer([{number, "alma"}, {ask_missing, false}]),
        error
    catch
        throw:Err ->
            false = lists:prefix("Unknown",
                                 lists:flatten(?Error:error_text(Err))),
            ok
    end.

test_atom() ->
    alma = ?Args:name([{name, alma}, {ask_missing, false}]),
    alma = ?Args:name([{name, "alma"}, {ask_missing, false}]),
    try
        ?Args:name([{name, 123}, {ask_missing, false}]),
        error
    catch
        throw:Err ->
            false = lists:prefix("Unknown",
                                 lists:flatten(?Error:error_text(Err))),
            ok
    end.

test_function() ->
    F = ?Query:exec1(?Query:seq(?Mod:find(test), ?Mod:local(f,1)), test_fun),
    F = ?Args:function([{module, test}, {function, f}, {arity, 1},
                        {ask_missing, false}]),
    F = ?Args:function([{file, "test.erl"}, {position, 38},
                        {ask_missing, false}]),
    E = ?Query:exec1(?Query:seq(?Mod:find(erlang), ?Mod:local(error,1)), test),
    E = ?Args:function([{file, "test.erl"}, {position, 52},
                        {ask_missing, false}]),
    try
        ?Args:function([{module, test}, {function, alma}, {arity, 0},
                        {ask_missing, false}]),
        error
    catch
        throw:Err ->
            false = lists:prefix("Unknown",
                                 lists:flatten(?Error:error_text(Err))),
            ok
    end.

test_file() ->
    F = ?Query:exec1(?File:find("test.erl"), test_mod),
    F = ?Args:file([{file, "test.erl"}, {ask_missing, false}]),
    try
        ?Args:file([{file, "not_existing_file.xyz"}, {ask_missing, false}]),
        error
    catch
        throw:Err ->
            false = lists:prefix("Unknown",
                                 lists:flatten(?Error:error_text(Err))),
            ok
    end.

test_module() ->
    M = ?Query:exec1(?Mod:find(test), test_mod),
    M = ?Args:module([{module, test}, {ask_missing, false}]),
    M = ?Args:module([{file, "test.erl"}, {ask_missing, false}]),
    try
        ?Args:module([{file, "not_existing_file.xyz"}, {ask_missing, false}]),
        error
    catch
        throw:Err ->
            false = lists:prefix("Unknown",
                                 lists:flatten(?Error:error_text(Err))),
            ok
    end.

test_variable() ->
    Var = ?Query:exec1(
             ?Query:seq([?Mod:find(test),
                         ?Mod:local(f, 1),
                         ?Fun:definition(),
                         ?Form:clause(1),
                         ?Clause:variable("X")]),
             test_var),
    Var = ?Args:variable([{file, "test.erl"}, {position, 24},
                          {ask_missing, false}]),
    ok.

test_order() ->
    [4,2,3,1] = ?Args:order([{order, [4,2,3,1]}, {ask_missing, false}]),
    try
        ?Args:order([{order, [4,1,3]}, {ask_missing, false}]),
        error
    catch
        throw:Err ->
            false = lists:prefix("Unknown",
                                 lists:flatten(?Error:error_text(Err))),
            ok
    end.

test_expr_range() ->
    Cls = ?Query:exec1(
             ?Query:seq([?Mod:find(test),
                         ?Mod:local(i,2),
                         ?Fun:definition(),
                         ?Form:clause(1)]),
             test_fun),
    B1 = ?Query:exec(Cls, ?Clause:body(1)),
    B1 = ?Args:expr_range([{file, "test.erl"}, {posrange, {82, 82}},
                           {ask_missing, false}]),
    B3 = ?Query:exec(Cls, ?Clause:body(3)),
    B3 = ?Args:expr_range([{file, "test.erl"}, {posrange, {88, 92}},
                           {ask_missing, false}]),
    B = ?Query:exec(Cls, ?Clause:body()),
    B = ?Args:expr_range([{file, "test.erl"}, {posrange, {82, 92}},
                          {ask_missing, false}]),
    ok.

test_expr_range_msg() ->
    try
        none = ?Args:expr_range([{file, "not existing"}, {posrange, {1,1}},
                                 {ask_missing, false}])
    catch
        throw:Err3 ->
            false = lists:prefix("Unknown",
                                 lists:flatten(?Error:error_text(Err3)))
    end,
    try
        none = ?Args:expr_range([{file, "test.erl"}, {posrange, {90, 92}},
                                 {ask_missing, false}])
    catch
        throw:Err1 ->
            false = lists:prefix("Unknown",
                                 lists:flatten(?Error:error_text(Err1)))
    end,
    %% Ticket #165
    try
        none = ?Args:expr_range([{file, "test.erl"}, {posrange, {1,10000}},
                                 {ask_missing, false}])
    catch
        throw:Err2 ->
            false = lists:prefix("Unknown",
                                 lists:flatten(?Error:error_text(Err2)))
    end,

    ok.

test_expression() ->
    E = ?Query:exec1(
           ?Query:seq([?Mod:find(test),
                       ?Mod:local(g,0),
                       ?Fun:definition(),
                       ?Form:clause(1),
                       ?Clause:body(1)]),
           test_fun),
    E = ?Args:expression([{file, "test.erl"}, {position, 35},
                          {ask_missing, false}]),
    try
        ?Args:expression([{file, "test.erl"}, {position, 31},
                          {ask_missing, false}]),
        error
    catch
        throw:Err ->
            false = lists:prefix("Unknown",
                                 lists:flatten(?Error:error_text(Err))),
            ok
    end.

test_functions() ->
    Fn = ?Args:functions([{file, "test.erl"}, {funlist, [{g,0}, {i,2}]},
                          {ask_missing, false}]),
    [{g,0},{i,2}] = lists:sort([{?Fun:name(F), ?Fun:arity(F)} || F <- Fn]),
    ok.

test_record() ->
    Record = ?Args:record([{file, "test.erl"}, {position, 170},
                           {ask_missing, false}]),
    Record = ?Args:record([{file, "test.erl"}, {position, 200},
                           {ask_missing, false}]),
    rek = ?Rec:name(Record),
    try
        ?Args:record([{file, "test.erl"}, {position, 1}, {ask_missing, false}]),
        error
    catch
        throw:Err ->
            false = lists:prefix("Unknown",
                                 lists:flatten(?Error:error_text(Err))),
            ok
    end.

test_record_field() ->
    Field = ?Args:record_field([{file, "test.erl"}, {position, 178},
                                {ask_missing, false}]),
    Field = ?Args:record_field([{file, "test.erl"}, {position, 203},
                                {ask_missing, false}]),
    egy = (?Graph:data(Field))#field.name,
    try
        ?Args:record_field([{file, "test.erl"}, {position, 1},
                            {ask_missing, false}]),
        error
    catch
        throw:Err ->
            false = lists:prefix("Unknown",
                                 lists:flatten(?Error:error_text(Err))),
            ok
    end.
