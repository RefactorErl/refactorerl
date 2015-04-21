
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

%%% @doc todo
%%%
%%% @todo author

-module(refcore_esg_tests).
-include_lib("eunit/include/eunit.hrl").

-include("test.hrl").

ins_rem_test() ->
    Name = "-testfile-.erl",
    F = ?ESG:create(#file{path=Name, type=module}),
    ?assertMatch(#file{path=Name}, ?ESG:data(F)),

    ?ESG:insert(?ESG:root(), file, F),
    ?assertMatch([F], ?ESG:path(?ESG:root(), [{file, {path, '==', Name}}])),

    ?ESG:remove(?ESG:root(), file, F),
    ?assertMatch([], ?ESG:path(?ESG:root(), [{file, {path, '==', Name}}])),

    ?ESG:finalize(),
    ?assertError(bad_node, ?ESG:data(F)),
    ok.

esg_test_() ->
    {setup, fun addfile/0, fun dropfile/1,
     fun(F) -> [{"basic", fun() -> basic_insrem(F) end},
                {"error", fun() -> ins_error(F) end},
                {"semq",  fun() -> semq(F) end}
               ] end}.

addfile() ->
    F = ?ESG:create(#file{path="test.erl", type=module}),
    ?ESG:insert(?ESG:root(), file, F),
    ?ESG:finalize(),
    F.

dropfile(F) ->
    ?ESG:remove(?ESG:root(), file, F),
    ?ESG:finalize().

basic_insrem(File) ->
    F  = ?Syn:create(#form{type=module},
                     ["-", "module", "(", "test", ")", ". "]),
    ?ESG:insert(File, form, F),
    ?ESG:finalize(),
    ?assertMatch(#form{type=module, tag=test}, ?ESG:data(F)),
    ?assertMatch([F], ?ESG:path(File, [form])),

    ?ESG:remove(File, form, F),
    ?ESG:finalize(),
    ?assertError(bad_node, ?ESG:data(F)),
    ?assertMatch(#file{}, ?ESG:data(File)).

ins_error(File) ->
    ?FileMan:add_text(File, last, "a 'syntax error'"),
    ?ESG:finalize(),
    [F] = ?ESG:path(File, [{form, last}]),
    ?assertMatch(#form{type=error}, ?ESG:data(F)),

    ?ESG:remove(File, form, F),
    ?ESG:finalize(),
    ?assertError(bad_node, ?ESG:data(F)),
    ok.

semq(File) ->
    F = ?ESG:create(#form{type=func}),
    ?ESG:path(?ESG:root(), [module]),
    ?ESG:insert(File, form, F),
    ?assertError(semantic_link, ?ESG:path(?ESG:root(), [module])),
    ?ESG:remove(File, form, F),
    ?ESG:finalize(),
    ?ESG:path(?ESG:root(), [module]),
    ok.

text(Node) ->
    lists:flatten(?Syn:tree_text(Node)).



off_test_() ->
    [{IT, fun() -> off_tester(Ins) end} ||
        {IT, Ins} <- [{"single", fun ins_form/1},
                      {"multi", fun ins_forms/1}]].

off_tester(Ins) ->
    F = ?ESG:create(#file{path="-test-file.erl-", type=module}),
    Ins(F),
    ?ESG:finalize(),
    ?assertError(bad_node, ?ESG:data(F)),
    ok.


anal_test_() ->
    [{IT++"-"++RT, fun() -> anal_tester(Ins, Rem) end} ||
        {IT, Ins} <- [{"single", fun ins_form/1},
                      {"multi", fun ins_forms/1}],
        {RT, Rem} <- [{"single", fun rem_form/1},
                      {"multi", fun rem_forms/1}]].

anal_tester(Ins, Rem) ->
    F = addfile(),
    Ins(F),
    ?ESG:finalize(),
    Rem(F),
    ?ESG:finalize(),
    dropfile(F),
    ok.

deleg_test_() ->
    [{T, fun() -> deleg_tester(Fin) end} ||
        {T, Fin} <- [{"deleg-ins", ins},
                     {"deleg-rmv", rmv},
                     {"deleg", none}]].

deleg_tester(Fin) ->
    F = addfile(),
    ins_forms(F),
    if Fin == ins -> ?ESG:finalize(); true -> ok end,
    rem_forms(F),
    if Fin == rmv -> ?ESG:finalize(); true -> ok end,
    dropfile(F),
    ok.

move_test() ->
    F1 = addfile(),
    ins_forms(F1),

    F2 = ?ESG:create(#file{path="test2.erl", type=module}),
    ?ESG:insert(?ESG:root(), file, F2),
    ?FileMan:add_text(F2, last, "-module(test2).\n"),
    ?ESG:finalize(),

    [Form] = ?ESG:path(F1, [{form, last}]),
    ?ESG:remove(F1, form, Form),
    ?ESG:insert(F2, form, Form),
    ?ESG:finalize(),
    ?assertMatch(none, ?ESG:index(F1, form, Form)),
    ?assert(is_integer(?ESG:index(F2, form, Form))),

    dropfile(F1),
    ?ESG:remove(?ESG:root(), file, F2),
    ?ESG:finalize().

ins_form(File) ->
    ?FileMan:add_text(File, last, "f(X)->X+1.\n"),
    [F] = ?ESG:path(File, [{form, last}]),
    ?assertMatch("f(X)->X+1.\n", text(F)).

ins_forms(File) ->
    ?FileMan:add_text(File, last, "-module(a).\nf()->1.\ng()->2.\n"),
    [F1,F2,F3] = ?ESG:path(File, [form]),
    ?assertMatch("-module(a).\n", text(F1)),
    ?assertMatch("f()->1.\n", text(F2)),
    ?assertMatch("g()->2.\n", text(F3)).



rem_form(File) ->
    Form =
        case ?ESG:path(File, [form]) of
            [F] -> F;
            [_, F | _] -> F
        end,
    ?ESG:remove(File, form, Form),
    ?assert(not lists:member(Form, ?ESG:path(File, [form]))).

rem_forms(File) ->
    [?ESG:remove(File, form, Form) || Form <- ?ESG:path(File, [form])],
    ?assertMatch([], ?ESG:path(File, [form])).

%%     E = ?Syn:create(#expr{type=infix_expr, value='-'},
%%                     [{esub, ?Syn:create(#expr{type=variable}, ["X"])},
%%                      {esub, ?Syn:create(#expr{type=integer},  ["1"])}]),
%%     [C] = ?ESG:path(F, [funcl]),
%%     ?Syn:replace(C, {body, 1, 0}, [E]),
%%     ?assertMatch("f(X)->X-1,X+1.\n", text(F)),

%%     ?ESG:remove(File, form, F),
%%     ?ESG:finalize(),
%%     ?assertError(bad_node, ?ESG:data(E)),
%%     ok.

%% two_rem(File) ->
%%     ?FileMan:add_text(File, last, "f(X)->X-1,X+1.\n"),
%%     ?ESG:finalize(),
%%     [F] = ?ESG:path(File, [{form, last}]),
%%     [C] = ?ESG:path(F, [funcl]),
%%     ?assertMatch("f(X)->X-1,X+1", text(C)),
%%     [E] = ?ESG:path(C, [{body,1}]),
%%     ?ESG:remove(File, form, F),
%%     ?ESG:remove(C, body, E),
%%     ?assertMatch("f(X)->X+1", text(C)),
%%     ?ESG:finalize(),
%%     ?assertError(bad_node, ?ESG:data(C)),
%%     ok.
