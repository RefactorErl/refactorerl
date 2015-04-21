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

%%% @doc Unit test for {@link reflib_syntax}.
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(reftest_reflib_syntax).
-vsn("$Rev: 9568 $ ").

-compile(export_all).

-include("test.hrl").

files() ->
    [{module, "test.erl",
      "-module(test).\n"
      "f() ->\n"
      "  ok.\n"
      "-define(M(X), X+X).\n"
      "g() -> ?M(1).\n"}].

test_root_path() ->
    [E1, E2] = ?Query:exec(
                  ?Query:seq([?Mod:find(test), ?Mod:local(g,0),
                              ?Fun:definition(), ?Form:clauses(),
                              ?Clause:body(), [esub]])),
    [M] = ?Graph:path(E1, [elex,llex]),
    [M] = ?Graph:path(E2, [elex,llex]),
    {_,Left} = lists:unzip(?Syn:root_path(M, left)),
    {_,Left} = lists:unzip(?Syn:root_path(M)),
    {_,Right} = lists:unzip(?Syn:root_path(M, right)),
    true  = lists:member(E1, Left),
    false = lists:member(E1, Right),
    true  = lists:member(E2, Right),
    false = lists:member(E2, Left),
    ok.

test_01_repl_list() ->
    ok = check_test_fun0("f()->ok."),
    [Clause] = ?Query:exec(
                ?Query:seq([get_test_fun0(),
                            ?Form:clauses()])),
    ?Syn:replace(
       Clause, {body, 1},
       fun ([Val]) ->
               Copy = ?Syn:copy(Val),
               {Val, CVal} = lists:keyfind(Val, 1, Copy),
               [?Syn:create(
                   #expr{type=cons},
                   [{esub, ?Syn:create(
                             #expr{type=list},
                             [{esub, CVal}])}]
                   )]
       end),
    ?ESG:finalize(),
    check_test_fun0("f()->[ok].").

test_02_repl_arg() ->
    [Clause] = ?Query:exec(
                  ?Query:seq([get_test_fun0(),
                              ?Form:clauses()])),
    ?Syn:replace(
       Clause, {pattern, 0, 0},
       [?Syn:create(
           #expr{type=variable},
           ["Tail"])]),
    ?ESG:finalize(),
    check_test_fun("f(Tail)->[ok].").

test_03_repl_tail() ->
    [Expr] = ?Query:exec(
                ?Query:seq([get_test_fun(),
                            ?Form:clauses(),
                            ?Clause:body()])),
    ?Syn:replace(
       Expr, {esub, 2, 0},
       [?Syn:create(
           #expr{type=variable},
           ["Tail"])]),
    ?ESG:finalize(),
    check_test_fun("f(Tail)->[ok|Tail].").

test_04_repl_head() ->
    [Head] = ?Query:exec(
                ?Query:seq([get_test_fun(),
                            ?Form:clauses(),
                            ?Clause:body(),
                            ?Expr:child(1),
                            ?Expr:child(1)])),
    ?Syn:replace(Head, elex, ["head"]),
    ?ESG:finalize(),
    #expr{value=head} = ?ESG:data(Head),
    check_test_fun("f(Tail)->[head|Tail].").

test_05_repl_addcall() ->
    [Clause] = ?Query:exec(
                  ?Query:seq([get_test_fun(),
                              ?Form:clauses()])),
    ?Syn:replace(
       Clause, {body, 1, 0},
       [?Syn:create(#expr{type=application},
                    [{esub, [?Syn:create(#expr{type=atom}, ["alma"]),
                             ?Syn:create(#expr{type=arglist}, 
                                         [{esub, 
                                           ?Syn:create(#expr{type=integer}, 
                                                       ["1"])}])]}])]),
    ?ESG:finalize(),
    2 = length(?Query:exec(Clause, ?Clause:body())),
    check_test_fun("f(Tail)->alma(1),[head|Tail].").

test_06_repl_addargs() ->
    [App] = ?Query:exec(
               ?Query:seq([get_test_fun(),
                           ?Form:clauses(),
                           ?Clause:body(1)])),
    [Arg] = ?Query:exec(App, ?Expr:child(2)),
    ?Syn:replace(
       App, {node, Arg},
       [?Syn:create(#expr{type=arglist}, [{esub, [ 
                     ?Syn:create(#expr{type=integer}, ["2"]),
                     ?Syn:create(#expr{type=integer}, ["3"]),
                     ?Syn:create(#expr{type=integer}, ["4"]),
                     ?Syn:create(#expr{type=integer}, ["5"]),
                     ?Syn:create(#expr{type=integer}, ["6"])]}])]),
    ?ESG:finalize(),
    check_test_fun("f(Tail)->alma(2,3,4,5,6),[head|Tail].").

test_07_repl_delargs() ->
    [ArgL] = ?Query:exec(
               ?Query:seq([get_test_fun(),
                           ?Form:clauses(),
                           ?Clause:body(1),
                           ?Expr:child(2)])),
    [Arg1] = ?Query:exec(ArgL, ?Expr:child(3)),
    [Arg2] = ?Query:exec(ArgL, ?Expr:child(5)),
    ?Syn:replace(
       ArgL, {range, Arg1, Arg2}, []),
    ?ESG:finalize(),
    check_test_fun("f(Tail)->alma(2,3),[head|Tail].").

test_08_repl_multi() ->
    Cl = ?Query:exec1(?Query:seq(get_test_fun(), ?Form:clause(1)), test_fun),
    ?Syn:replace(
       Cl, [{{guard, 1, 0},
             [?Syn:create(#expr{type=application},
                          [{esub,[?Syn:create(#expr{type=atom}, ["list"]),
                                  ?Syn:create(#expr{type=arglist}, [{esub, 
                                  ?Syn:create(#expr{type=variable},["Tail"])}])]
                           }])]},
            {{body, 1, 1}, []}]),
    ?ESG:finalize(),
    check_test_fun("f(Tail)whenlist(Tail)->[head|Tail].").


get_test_fun() -> get_fun(f,1).
get_test_fun0() -> get_fun(f,0).

get_fun(N,A) ->
    ?Query:seq([?Mod:find(test),
                ?Mod:local(N,A),
                ?Fun:definition()]).

check_test_fun(Content) -> check_fun(f,1,Content).
check_test_fun0(Content) -> check_fun(f,0,Content).

check_fun(N, A, Content) ->
    Fun = ?Query:exec1(get_fun(N, A), test_check),
    Text = lists:flatten(?Syn:tree_text(Fun)),
    Content = lists:filter(fun(C) -> C /= $\  andalso
                                         C /= $\t andalso
                                         C /= $\n end, Text),
    ok.

get_test_file() ->
    ?Query:exec1(?Query:seq(?Mod:find(test), ?Mod:file()), test_module).
