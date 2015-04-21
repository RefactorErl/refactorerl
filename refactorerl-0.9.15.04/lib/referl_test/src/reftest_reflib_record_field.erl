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

%%% @doc Unit test for {@link reflib_record_field}.
%%% @author Gabor Czini <gczini@gmail.com>

-module(reftest_reflib_record_field).
-vsn("$Rev: 4139 $").

-compile([export_all]).

-include("test.hrl").

files() ->
    [{module, "recfield.erl",
      "-module(recfield).    \n"
      "-record(person, {name,age,phone}).\n"
      "-record(person2, {name,sex,height}).\n"
      "-export([run/0]).\n"
      "run() ->\n"
      "Hilo = #person{name=\"Hilo\", age=24, phone=\"999-9999\"}.\n"
     }].

test_file() ->
    File  = ?Query:exec1(?File:find("recfield.erl"), file_not_found),
    Rec   = ?Query:exec1(?Query:seq(?File:find("recfield.erl"),
                                  ?File:record(person)), rec_not_found),
    Field = ?Query:exec(Rec, ?Rec:field(age)),
    File  = ?Query:exec1(Field, ?RecField:file(), file_not_found),
    ok.

test_form() ->
    File = ?Query:exec1(?File:find("recfield.erl"), file_not_found),
    Rec  = ?Query:exec1(?Query:seq(?File:find("recfield.erl"),
                                  ?File:record(person)), rec_not_found),
    Form = ?Query:exec1(Rec, ?Rec:form(), form_not_found),
    File = ?Query:exec1(Form, ?Form:file(), file_not_found),
    ok.   

test_names() ->
    Rec = ?Query:exec1(?Query:seq(?File:find("recfield.erl"),
                                  ?File:record(person)), rec_not_found),
    FieldNames = [?RecField:name(Name) || Name <- ?Query:exec(Rec, ?Rec:fields()) ],
    [name,age,phone] = FieldNames,
    ok.

test_def() ->
    Rec = ?Query:exec1(?Query:seq(?File:find("recfield.erl"),
                                  ?File:record(person)), rec_not_found),
    Field = ?Query:exec(Rec, ?Rec:field(name)),
    Rec = ?Query:exec1(Field, ?RecField:recorddef(),rec_not_found),
    ok.

test_ref() ->
    Field = ?Query:exec1(?Query:seq([?File:find("recfield.erl"),
                                  ?File:record(person), ?Rec:field(age)]), field_not_found),
    FieldRef = ?Query:exec1(Field, ?RecField:references(), no_field_expr),
    Field = ?Query:exec1(FieldRef, ?Expr:field(), no_field),
    ok.
