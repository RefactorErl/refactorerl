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

%%% @doc Unit test for {@link reflib_file}.
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>

-module(reftest_reflib_file).
-vsn("$Rev: 9568 $").

-compile([export_all]).

-include("test.hrl").

files() ->
    [{header, "h2.hrl",
      "-record(deep, {}).   \n"
     },
     {header, "h1.hrl",
      "-record(r1, {}).     \n"
      "-record(r2, {}).     \n"
      "-define(M1, alma).   \n"
      "-define(M2, korte).  \n"
      "-include(\"h2.hrl\").\n"
     },
     {module, "m2.erl",
      "-module(m2).         \n"
     },
     {module, "m3.erl",
      "-module(m3).         \n"
      "-record(deep, {}).   \n"
     },
     {module, "m1.erl",
      "-module(m1).         \n"
      "-include(\"h1.hrl\").\n"
      "f() ->               \n"
      "    ok.              \n"
     }
    ].

test_path() ->
    "m1.erl" = filename:basename(?File:path(?Query:exec1(?File:find("m1.erl"), file_not_found))),
    "h1.hrl" = filename:basename(?File:path(?Query:exec1(?File:find("h1.hrl"), file_not_found))),
    ok.

test_type() ->
    module = ?File:type(?Query:exec1(?File:find("m1.erl"), file_not_found)),
    header = ?File:type(?Query:exec1(?File:find("h1.hrl"), file_not_found)),
    ok.

test_length() ->
    (4*22 - 2) =
        ?File:length(?Query:exec1(?File:find("m1.erl"), file_not_found)),
    ok.

test_includable() ->
    M2 = filenode("m2.erl"),
    M3 = filenode("m3.erl"),
    H1 = filenode("h1.hrl"),
    false = ?File:includable(M3, H1),
    true = ?File:includable(M2, H1),
    ok.

%%% ============================================================================

test_token() ->
    M = filenode("m1.erl"),
    {24, 30} = ?Token:pos(?Query:exec1(M, ?File:token(24),
                                       token_not_found)),
    [] = ?Query:exec(M, ?File:token(14, none)),
    {2, 7} = ?Token:pos(?Query:exec1(M, ?File:token(7, none),
                                       token_not_found)),
    ok.

test_find() ->
    %% tested in other funs
    ok.

test_module() ->
    M1 = ?Query:exec(?Mod:find(m1)),
    M1 = ?Query:exec(?Query:seq(?File:find("m1.erl"), ?File:module())),
    ok.

test_forms() ->
    Forms = ?Query:exec(?Query:seq(?File:find("m1.erl"), ?File:forms())),
    3 = length(Forms),
    M = filenode("m1.erl"),
    [M = ?Query:exec1(Form, ?Form:file(), file_not_found) || Form <- Forms],
    ok.

test_form() ->
    M = filenode("m1.erl"),
    Form = ?Query:exec1(?Query:seq(?File:find("m1.erl"), ?File:form(3)),
                         form_not_found),
    func = ?Form:type(Form),
    M = ?Query:exec1(Form, ?Form:file(), file_not_found),
    ok.

test_include_form() ->
    M = filenode("m1.erl"),
    M1F = ?Query:exec1(?File:find("m1.erl"), file_not_found),
    H1F = ?Query:exec1(?File:find("h1.hrl"), file_not_found),
    Form = ?Query:exec1(M1F, ?File:include_form(H1F), form_not_found),
    lex = ?Form:type(Form),
    M = ?Query:exec1(Form, ?Form:file(), file_not_found),
    ok.

test_includes() ->
    H1 = filenode("h1.hrl"),
    H2 = filenode("h2.hrl"),
    [H1, H2] = ?Query:exec(filenode("m1.erl"), ?File:includes())
        -- [filenode("m1.erl")],
    ok.

test_included() ->
    M = filenode("m1.erl"),
    [M] = ?Query:exec(filenode("h1.hrl"), ?File:included())
        -- [filenode("h1.hrl")],
    ok.

test_records() ->
    H1 = filenode("h1.hrl"),
    H2 = filenode("h2.hrl"),
    Recs = ?Query:exec(?Query:seq([?File:find("h1.hrl"), ?File:includes(),
                                   ?File:records()])),
    Recs = ?Query:exec(?Query:seq([?File:find("m1.erl"), ?File:includes(),
                                   ?File:records()])),
    3 = length(Recs),
    [deep, r1, r2] = [?Rec:name(R) || R <- Recs],
    [H2, H1, H1] = [?Query:exec1(R, ?Rec:file(), file_not_found) || R <- Recs],
    ok.

test_record() ->
    H = filenode("h1.hrl"),
    Rec = ?Query:exec1(?Query:seq(?File:find("h1.hrl"),
                                  ?File:record(r1)), rec_not_found),
    Rec = ?Query:exec1(?Query:seq([?File:find("m1.erl"), ?File:includes(),
                                   ?File:record(r1)]), rec_not_found),
    r1 = ?Rec:name(Rec),
    H = ?Query:exec1(Rec, ?Rec:file(), file_not_found),
    ok.

test_macros() ->
    H = filenode("h1.hrl"),
    Macs = ?Query:exec(?Query:seq(?File:find("h1.hrl"), ?File:macros())),
    Macs = ?Query:exec(?Query:seq([?File:find("m1.erl"), ?File:includes(),
                                   ?File:macros()])),
    2 = length(Macs),
    ["M1", "M2"] = [?Macro:name(M) || M <- Macs],
    [H, H] = [?Query:exec1(M, ?Macro:file(), file_not_found) || M <- Macs],
    ok.

test_macro() ->
    H = filenode("h1.hrl"),
    Mac = ?Query:exec1(?Query:seq(?File:find("h1.hrl"),
                                  ?File:macro("M1")), mac_not_found),
    Mac = ?Query:exec1(?Query:seq([?File:find("m1.erl"), ?File:includes(),
                                   ?File:macro("M1")]), mac_not_found),
    "M1" = ?Macro:name(Mac),
    H = ?Query:exec1(Mac, ?Macro:file(), file_not_found),
    ok.

%%% ============================================================================

test_0_add_include() ->
    M2 = filenode("m2.erl"),
    H1 = filenode("h1.hrl"),
    H2 = filenode("h2.hrl"),
    ?File:add_include(M2, H1),
    ?ESG:finalize(),
    Headers = lists:usort([H1, H2]),
    Headers = lists:usort(?Query:exec(filenode("m2.erl"),
                                      ?File:includes())) -- [M2],
    ok.

test_1_del_include() ->
    M2 = filenode("m2.erl"),
    H1 = filenode("h1.hrl"),
    ?File:del_include(M2, H1),
    ?ESG:finalize(),
    Headers = lists:usort([]),
    Headers = lists:usort(?Query:exec(filenode("m2.erl"),
                                      ?File:includes())) -- [M2],
    ok.

test_add_form() ->
    M1 = filenode("m1.erl"),
    H1 = filenode("h1.hrl"),
    RecForm = ?Query:exec1(H1, ?File:form(1), form_not_found),
    {_, NewForm} = lists:keyfind(RecForm, 1, ?ESG:copy(RecForm)),
    ?File:add_form(M1, NewForm),
    ?ESG:finalize(),
    NewForm = ?Query:exec1(M1, ?File:form(2), form_not_found),
    record = ?Form:type(NewForm),
    ok.

test_del_form() ->
    M1 = filenode("m1.erl"),
    Form = ?Query:exec1(M1, ?File:form(2), form_not_found),
    ?File:del_form(M1, Form),
    ?ESG:finalize(),
    true = (Form =/= ?Query:exec1(M1, ?File:form(2), form_not_found)),
    ok.

test_upd_path() ->
    ok.


filenode(Name) ->
    ?Query:exec1(?File:find(Name), file_not_found).
