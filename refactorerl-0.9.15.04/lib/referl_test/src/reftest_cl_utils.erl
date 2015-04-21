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

%%% @doc This is a test module for module `cl_utils'.
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(reftest_cl_utils).
-vsn("$Rev: 9568 $").

-export([test/0]).

%% @spec test() -> ok
%% @doc It tests the modul and returns ok if the test passed.
test() ->
    ok = test_proplist_update().

test_proplist_update() ->
    [] = cl_utils:proplist_update([],[]),
    [{a,true}] = cl_utils:proplist_update([a],[]),
    [{a,true}] = cl_utils:proplist_update([],[a]),
    [{a,true}] = cl_utils:proplist_update([a],[a]),
    [{a,b}] = cl_utils:proplist_update([{a,b}],[]),
    [{a,b}] = cl_utils:proplist_update([],[{a,b}]),
    [{a,b}] = cl_utils:proplist_update([{a,b}],[{a,b}]),
    [{a,c}] = cl_utils:proplist_update([{a,b}],[{a,c}]),
    [{a,b}] = cl_utils:proplist_update([{a,c}],[{a,b}]),
    [{a,1},{c,true}] = cl_utils:proplist_update([{a,0},c],[{c,true},{a,1}]),
    [{a,0},{b,1},{c,2}] = cl_utils:proplist_update([{b,0},{a,0}],[{b,1},{c,2}]),
    [{a,0},{b,1},{c,2}] = cl_utils:proplist_update([{b,0},{a,0}],[{c,2},{b,1}]),
    ok.
