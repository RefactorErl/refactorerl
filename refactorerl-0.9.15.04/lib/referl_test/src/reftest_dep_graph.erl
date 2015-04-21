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

%%% ============================================================================
%%% Module information

%%% @doc Dependency examination.

%%% @author Barbara Oláh <olahb@caesar.elte.hu>

-module(reftest_dep_graph).

-include("test.hrl").

-export([test/0, test/1, test_referl/0]).

-define(Draw,refusr_dep_graph:draw).

-define(DMod,refusr_dep_graph).

-define(TestDir,filename:join(["..","test","refact","dep_graph"])).

-define(OutPut, 
    filename:join(["..","test","refact","dep_graph","test_results",postfix()])).

test()->
	try
        ri:reset(),
		ri:add(?TestDir),
		test_func_level(),
		test_mod_level(),
		test_mb_level()
	catch
		_ -> throw(?LocalError(error,
				"Error while running dependency graph tests"))
	after
		ri:reset()
	end.
    
test(Dir) when is_list(Dir)->
    try
        ri:reset(),
		ri:add(Dir),
		test_func_level(),
		test_mod_level(),
		test_mb_level()
	catch
		_ -> throw(?LocalError(error,
				"Error while running dependency graph tests"))
	after
		ri:reset()
	end;
test(_)->
    throw(?LocalError(error, "Bad parameter")).
    
test_referl()->
    try
        ri:reset(),
        ri:addenv(appbase, "/home/olahb/repos/branches/olahb/dep_graph/tool/lib"),
        ri:add(home, referl_user),
        test_func_level_referl(),
		test_mod_level_referl(),
		test_mb_level_referl()
    catch
		_:_ -> throw(?LocalError(error,
				"Error while running dependency graph tests"))
	after
		ri:reset()
	end.
    
test_func_level_referl()->
    write("TEST: Function level - starting nodes"),
	test_starting_nodes(func,[".*refusr.*:.*/.*"]),
    write("TEST: Function level - exclude"),
	test_exclude(func,[".*refusr.*:.*/.*"]),
    write("TEST: Function level - exclude_children"),
	test_exclude_children(func,[".*refusr.*:.*/.*"]),
    write("TEST: Function level - exclude library"),
	test_exclude_lib(func,[filename:join([home,olahb,repos,branches,olahb,dep_graph,tool,lib,referl_user,src])]),
    write("TEST: Function level - exclude otp"),
	test_exclude_otp(func),
    write("TEST: Function level - connection"),
%	test_connection(func,[".*refusr.*:.*/.*"]),
    write("TEST: Function level - type"),
	test_type(func),
%	test_output(),
	ok.
	
test_mod_level_referl()->
    write("TEST: Module level - starting nodes"),
	test_starting_nodes(mod,[".*refusr.*"]),
    write("TEST: Module level - exclude"),
	test_exclude(mod,[".*reftr.*"]),
    write("TEST: Module level - exclude children"),
	test_exclude_children(mod,[".*refusr.*"]),
    write("TEST: Module level - exclude library"),
	test_exclude_lib(mod,[filename:join([home,olahb,repos,branches,olahb,dep_graph,tool,lib,referl_user,src])]),
    write("TEST: Module level - exclude otp"),
	test_exclude_otp(mod),
    write("TEST: Module level - connection"),
	%test_connection(mod,[]),
    write("TEST: Module level - type"),
	test_type(mod),
%	test_output(),
	ok.

test_mb_level_referl()->
    write("TEST: Moduleblock level - groups"),
%	test_groups(),
    write("TEST: Moduleblock level - command"),
%	test_command(),
    write("TEST: Moduleblock level - type"),
	test_type(mb),
%	test_output(),
	ok.

test_func_level()->
    write("TEST: Function level - starting nodes"),
	test_starting_nodes(func,['m1:h/0','m6:k/0']),
    write("TEST: Function level - exclude"),
	test_exclude(func,['m4:d/0', 'm7:i/1']),
    write("TEST: Function level - exclude_children"),
	test_exclude_children(func,['m6:k/0']),
    write("TEST: Function level - exclude library"),
	test_exclude_lib(func,[filename:join([?TestDir,"2"])]),
    write("TEST: Function level - exclude otp"),
	test_exclude_otp(func),
    write("TEST: Function level - connection"),
	test_connection(func,['m4:d/0','m6:f/0']),
    write("TEST: Function level - type"),
	test_type(func),
%	test_output(),
	ok.
	
test_mod_level()->
    write("TEST: Module level - starting nodes"),
	test_starting_nodes(mod,[m1,m6]),
    write("TEST: Module level - exclude"),
	test_exclude(mod,[m4,m8]),
    write("TEST: Module level - exclude children"),
	test_exclude_children(mod,[m4,m1]),
    write("TEST: Module level - exclude library"),
	test_exclude_lib(mod,[filename:join([?TestDir,"2"])]),
    write("TEST: Module level - exclude otp"),
	test_exclude_otp(mod),
    write("TEST: Module level - connection"),
	test_connection(mod,[m4,m6]),
    write("TEST: Module level - type"),
	test_type(mod),
%	test_output(),
	ok.

test_mb_level()->
    write("TEST: Moduleblock level - groups"),
	test_groups(),
    write("TEST: Moduleblock level - command"),
%	test_command(),
    write("TEST: Moduleblock level - type"),
	test_type(mb),
%	test_output(),
	ok.

test_starting_nodes(Level,List)->
    delete_files(),
	write(?Draw([{level,Level},{starting_nodes,List},{output,name_terms}])),
	ok.

test_exclude(Level,List)->
	delete_files(),
    write(?Draw([{level,Level},{exclude,List},{output,name_terms}])),
	ok.

test_exclude_children(Level,List)->
	delete_files(),
    write(?Draw([{level,Level},{exclude_children,List},{output,name_terms}])),
	ok.

test_exclude_lib(Level,List)->
	delete_files(),
    write(?Draw([{level,Level},{exclude_lib,List},{output,name_terms}])),
	ok.

test_exclude_otp(Level)->
    delete_files(),
	write(?Draw([{level,Level},{exclude_otp,true},{output,name_terms}])),
	delete_files(),
    write(?Draw([{level,Level},{exclude_otp,false},{output,name_terms}])),
	ok.

test_connection(Level,List)->
	delete_files(),
    write(?Draw([{level,Level},{connection,List},{output,name_terms}])),
	ok.
	
test_type(Level)->
    delete_files(),
	write("Whole graph"),
    write(?Draw([{level,Level},{type,all},{output,name_terms}])),
    delete_files(),
    write("Cyclic subgraph"),
    write(?Draw([{level,Level},{type,cyclic},{output,name_terms}])),
	ok.

test_output(Level)->
	ok.
	
test_command(Groups)->
    delete_files(),
	write(?Draw([{level,mb},{command,is_direct_rel},{groups,Groups},{output,name_terms}])),
    delete_files(),   
	write(?Draw([{level,mb},{command,is_indirect_rel},{groups,Groups},{output,name_terms}])),
    delete_files(),
	write(?Draw([{level,mb},{command,get_direct_rel},{groups,Groups},{output,name_terms}])),
    delete_files(),
    write(?Draw([{level,mb},{command,get_indirect_rel},{groups,Groups},{output,name_terms}])),
	ok.

test_groups()->
    delete_files(),
	write(?Draw([{level,mb},{groups,["m.*"]},{output,name_terms}])),
    delete_files(),  
    write(?Draw([{level,mb},{groups,[[m4,m5],[m2,m3]]},{output,name_terms}])),
    ok.

delete_files()->
    refusr_dep_graph:delete_dep_files().

write(Data)->
    file:write_file(?OutPut,io_lib:fwrite("~p.\n",[Data]),[append]).

postfix()->
    {Y,M,D} = date(),
    io_lib:format("~p_~p_~p", [Y,M,D]).
