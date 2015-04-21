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

%%% @doc Unit test for {@link reflib_record}.
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>

%%% TODO
%%% Create test case for ?Rec:references()


-module(reftest_reflib_record).
-vsn("$Rev: 9568 $").

-compile([export_all]).

-include("test.hrl").

files() ->
[{module, "rec.erl",
	      "-module(recfield).    \n"
	      "-record(person, {name,age,phone}).\n"
	      "-record(person2, {name,sex,height}).\n"
	      "-export([run/0]).\n"
	      "run() ->\n"
	      "Hilo = #person{name=\"Hilo\", age=24, phone=\"999-9999\"}.\n"
	     }].

test_name() ->
    Rec = ?Query:exec1(?Query:seq(?File:find("rec.erl"),
                                  ?Rec:find(person)), rec_not_found),
    person = ?Rec:name(Rec),
    ok.

test_file() ->
    File = ?Query:exec1(?File:find("rec.erl"), file_not_found),
    Rec = ?Query:exec1(?Query:seq(?File:find("rec.erl"),
                                  ?File:record(person)), rec_not_found),
    File = ?Query:exec1(Rec, ?Rec:file(), file_not_found),
    ok.

test_form() ->
    File = ?Query:exec1(?File:find("rec.erl"), file_not_found),
    Rec = ?Query:exec1(?Query:seq(?File:find("rec.erl"),
                                  ?File:record(person)), rec_not_found),
    Form = ?Query:exec1(Rec, ?Rec:form(), form_not_found),
    File = ?Query:exec1(Form, ?Form:file(), file_not_found),
    ok.

