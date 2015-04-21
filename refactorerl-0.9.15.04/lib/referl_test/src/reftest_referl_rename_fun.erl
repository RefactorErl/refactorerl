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

%%% @doc This module tests the {@link referl_rename_fun} module.
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>
%%%
%%% @todo Does not work.
%%% referl_rename_fun:rename_function should be reftr_rename_fun:do.

-module(reftest_referl_rename_fun).
-vsn("$Rev: 9568 $").
-include("test.hrl").

-export([test_rename_fun/4]).

%%% @type data_checker_fun() = ((data(), data()) -> (true | {false, term()})).
%%%
%%% Represents a function that checks two nodes in two graphs and returns
%%% whether they are correct or not.

%% @spec test_rename_fun(atom(), atom(), integer(), atom()) ->
%%           true | {false, term()}
%%
%% @doc Tests whether the given "rename function" refactoring is executed
%% correctly.
%%
%% @todo Now there are two possible implementations that conforms this property:
%% the identity and the correct implementation. The former could be ruled out by
%% adding a property that says: the function name in the semantic function node
%% is changed from OldFunName to NewFunName.
test_rename_fun(ModName, OldFunName, Arity, NewFunName) ->

    %% creating graphs
    E1 = reflib_egraph:create_egraph(),
    referl_rename_fun:rename_function(ModName, OldFunName, Arity, NewFunName),
    E2 = reflib_egraph:create_egraph(),

    %% testing and deleting graphs
    TestResult = reftest_utils:test_graphs(
                   E1, E2, id_except(OldFunName, Arity, NewFunName)),
    ets:delete(E1),
    ets:delete(E2),
    TestResult.

%% @spec id_except(atom(), integer(), atom()) -> data_checker_fun()
%%
%% @doc Check whether the two given data are the same or the second one is the
%% renamed version of the first one.
id_except(OldFunName, Arity, NewFunName) ->
    fun (Data1, Data2) ->
        case {Data1, Data2} of
            {X, X} ->
                true;
            {#expr{role = expr, type = atom, value = OldFunName},
             #expr{role = expr, type = atom, value = NewFunName}} ->
                true;
            {#lex{type = token, data = T1 = #token{type = atom}},
             #lex{type = token, data = T2 = #token{type = atom}}} ->
                ?Token:get_value(T1) == OldFunName andalso
                ?Token:get_value(T2) == NewFunName;
            {#func{%%type = global,
                   name = OldFunName, arity = Arity},
             #func{%%type = global,
                   name = NewFunName, arity = Arity}} ->
                true;
            _ ->
                {false,
                 {id_except,"Data1 and Data2 are different",Data1,Data2}}
        end
    end.

