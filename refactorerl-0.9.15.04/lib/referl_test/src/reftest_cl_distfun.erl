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

%%% @doc This is a test module for module `cl_distfun'.
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(reftest_cl_distfun).
-vsn("$Rev: 9568 $").

-export([test/0]).

-include_lib("referl_cluster/include/cluster.hrl").

%% @spec test() -> ok
%% @doc It tests the modul and returns ok if the test passed.
test() ->
    ok = test_modcall_cnt(),
    ok = test_used_records().

test_modcall_cnt() ->

% TODO modcall_cnt function cannot be found
%
%    Mod1_f = #fun_attr{mod=mod_1,name=f,arity=0},
%    Mod1_g = #fun_attr{mod=mod_1,name=g,arity=1},
%    Mod2_g = #fun_attr{mod=mod_2,name=g,arity=2},
%    Mod2_h = #fun_attr{mod=mod_2,name=h,arity=2},
%
%    [] =
%        cl_distfun:modcall_cnt([{Mod1_f,0},{Mod1_g,0},{Mod2_g,0},{Mod2_h,0}],1),
%    [{mod_1,2}] =
%        cl_distfun:modcall_cnt([{Mod1_f,1},{Mod1_g,1},{Mod2_g,0},{Mod2_h,0}],1),
%    [{mod_2,2}] =
%        cl_distfun:modcall_cnt([{Mod1_f,0},{Mod1_g,0},{Mod2_g,2},{Mod2_h,3}],1),
%    [{mod_1,2},{mod_2,1}] =
%        cl_distfun:modcall_cnt([{Mod1_f,1},{Mod1_g,2},{Mod2_g,3},{Mod2_h,0}],1),
    ok.

test_used_records() ->
    Rec1 = #rec_attr{file=file_1, name=rec_1},
    Rec2 = #rec_attr{file=file_1, name=rec_2},
    Rec3 = #rec_attr{file=file_3, name=rec_2},
    Rec4 = #rec_attr{file=file_2, name=rec_4},
    Fun  = #fun_attr{mod=mod_1,name=f,arity=0},

    [Rec1,Rec2,Rec3] =
        cl_distfun:used_records([{Rec2,1},{Fun,3},{Rec3,3},{Rec4,0},{Rec1,2}]),
    ok.

