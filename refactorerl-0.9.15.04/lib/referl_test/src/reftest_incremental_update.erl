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

%%% @doc Unit test for incremental update
%%% @author Viktoria Fordos <f-viktoria@elte.hu>

-module(reftest_incremental_update).

-include("test.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TimeOut, 20000). %msec
-define(AppendText0, "reftest_incremental_update_test_macro").
-define(AppendText, "-define("++?AppendText0++", ok).").
-define(WorkDir, filename:join(referl_misc:data_dir(), "tmp")).
-define(RootDir, filename:join(["..", "test", "unit_tests", "incremental_update"])).

%Older Eunit versions, which comes with <ErlangR15, do not define this macro
-ifndef(assertNotEqual).
-define(assertNotEqual(Expect, Expr),
    ((fun (__X) ->
        case (Expr) of
        __X -> erlang:error({assertNotEqual_failed,
                      [{module, ?MODULE},
                       {line, ?LINE},
                       {expression, (??Expr)}]});
        __Y -> ok
        end
      end)(Expect))).
-endif.

%generate test for all test cases
incremental_update_new_content_test_()->
    {foreachx,
     fun (Case) -> prepare(Case, random) end,
     fun (Case, _R) -> cleanup(Case) end, 
     %after the update the appended text has to be found 
     [{Case, 
       fun(X, _R) ->
               fun()->
                       ?assertNotEqual({rich,[],[]},
                                       begin
                                           add( X ),
                                           ris:q("files.macros[name = \""++
                                                     ?AppendText0++"\"]")
                                       end)
                       end
       end} || Case <- supported_testcases()]
     }.

%after the update m1, h1, h2 need to be updated, 
%but m2 has to preserve its original context.  
incremental_update_is_updated_1_test()->
    {setup,
     begin
        prepare("1", "h2.hrl"),
        append_term_to_file(filename:join([?WorkDir, "1", "m2.erl"])),
        add(filename:join("1", "m1.erl"))
     end,
     fun(_)-> cleanup("1") end,
     [
      ?assertEqual({rich,[],[]},
                   ris:q("files[name = m2].macros[name = "
                        ++?AppendText0++"]")),
      ?assertNotEqual({rich,[],[]},
                      ris:q("files[name = h2].macros[name = "
                           ++?AppendText0++"]"))
      ]
     }.

%after update everything need to be updated (incl is transitive) 
incremental_update_is_updated_2_test()->
    {setup,
     begin
        prepare("2", "h1.hrl"),
        add(filename:join("2", "m1.erl"))
     end,
     fun(_)-> cleanup("2") end,
     [
      ?assertNotEqual({rich,[],[]},
                      ris:q("files[name = h1].macros[name = "
                           ++?AppendText0++"]"))
      ]
     }.

%after update everything need to be updated (incl is transitive)
incremental_update_is_updated_3_test()->
    {setup,
     begin
        prepare("3", "h2.hrl"),
        add(filename:join("3", "m1.erl"))
     end,
     fun(_)-> cleanup("3") end,
     [
      ?assertNotEqual({rich,[],[]},
                      ris:q("files[name = h2].macros[name = "
                           ++?AppendText0++"]"))
      ]
     }.

%after update everything need to be updated (incl is transitive)
incremental_update_is_updated_4_test()->
    {setup,
     begin
        prepare("4", "h2.hrl"),
        append_term_to_file(filename:join([?WorkDir, "4", "m2.erl"])),
        add(filename:join("4", "m1.erl"))
     end,
     fun(_)-> cleanup("4") end,
     [
      ?assertNotEqual({rich,[],[]},
                      ris:q("files[name = h2].macros[name = "
                           ++?AppendText0++"]")),
      ?assertNotEqual({rich,[],[]},
                      ris:q("files[name = h2].macros[name = "
                           ++?AppendText0++"]"))
      ]
     }.
%-------------------------------------------------------------------------------
%%Helper functions
prepare(Case, TargetToModify)->
    %create tmp dir
    create_work_dir(),
    %copy test case
    copy_work_dir(filename:join(?RootDir, Case), filename:join(?WorkDir, Case)),
    %reset db
    reset(),
    %add files
    add(Case), 
    ri:generate_all(filename:join(?WorkDir, Case)),
    %files are too small, 
    %so the modification timestamp won't change even if we modify the file
    timer:sleep(1000),
    %choose one
    Chosen = case TargetToModify of
        random -> choose_randomly(filename:join(?WorkDir, Case));
        _ -> TargetToModify
    end,
    %and modify it
    append_term_to_file(filename:join([?WorkDir, Case, Chosen])).

cleanup(_)->
    %reset db
    reset(),
    %remove work dir
    del_work_dir(?WorkDir).

supported_testcases()->
    {ok, FileNames} = file:list_dir(?RootDir),
    FileNames--[".svn"].

create_work_dir()->
    filelib:ensure_dir(?WorkDir).

del_work_dir(Path)->
    case filelib:is_dir(Path) of
        true -> del_dir(Path);
        false -> file:delete(Path)
    end.

del_dir(Path)->
    {ok, Files} = file:list_dir(Path),
    [del_work_dir(File) || File <- Files],
    file:del_dir(Path).
                           
copy_work_dir(From, To) ->
    {ok, Files} = file:list_dir(From),
    [ok = copy0(From, To, X) || X <- Files],
    ok.
                           
copy0(_From, _To, [$. | _T]) ->
    ok; 
copy0(From, To, File) ->
 
    NewFrom = filename:join(From, File),
    NewTo   = filename:join(To, File),
 
    case filelib:is_dir(NewFrom) of
 
        true  ->
            ok = filelib:ensure_dir(NewTo),
            copy_work_dir(NewFrom, NewTo);
        
        false ->
            case filelib:is_file(NewFrom) of                
                true  ->
                    ok = filelib:ensure_dir(NewTo),
                    {ok, _} = file:copy(NewFrom, NewTo),
                    ok;
                false ->
                    ok            
            end
    end.
                 
add(Folder)->
    FolderPath = filename:join([?WorkDir,
                                Folder]),
    Res = make_ui_request({add_dir, FolderPath}),
    Res.

reset()->
    make_ui_request({reset}).

choose_randomly(FolderPath)->
    {ok, FileNames} = file:list_dir(FolderPath),
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    lists:nth(random:uniform(length(FileNames)), FileNames).

append_term_to_file(File)->
    {ok, IoDevice} = file:open(File, [append]),
    ok = file:write(IoDevice, ?AppendText), 
    file:close(IoDevice).

make_ui_request(UIArgs)->
    ReqID = ?UI:getid(),
    case ?UI:request(ReqID,UIArgs) of
        ok -> receive
                        {ReqID, reply, _R} -> ok
              after ?TimeOut ->
                      timeout
              end;
        deny -> deny
    end.
