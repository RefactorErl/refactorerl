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

%%% @doc Unit test for the requests management of the ui router 
%%% @author Viktoria Fordos <f-viktoria@elte.hu>

-module(reftest_reflib_ui_router).
-compile(export_all).

-include("test.hrl").
-define(debug_test,io:format("~p~n",[?LINE])).

files()->
    [].

test_modifier()->
    %reset db - if the result is given then ok, otherwise error
    result_or_throw(reset_db(), ?LINE),?debug_test,
    %spawn  - add a huge module to db
    Worker=run_with_callback(fun()->
                              add_file(ri)
                      end),?debug_test,
    timer:sleep(10),
    %sq 'mods' - if the result is deny then ok, otherwise error
    deny_or_throw(sq("mods.funs.var.ref"), ?LINE),?debug_test,
    %add a huge module to db again - if the result is deny then ok, otherwise error
    deny_or_throw(add_file(ri), ?LINE),?debug_test,
    %waits for the first add - if it succeeded then ok, otherwise error
    ?debug_test,
    receive
        {result, R, Worker} -> result_or_throw(R, ?LINE)
    end,?debug_test,
    %reset db - if the result is is given then ok, otherwise error
    result_or_throw(reset_db(), ?LINE),?debug_test,
    ok.

test_nonmodifier()->
    %reset db - if the result is is given then ok, otherwise error
    result_or_throw(reset_db(), ?LINE),?debug_test,
    %add a huge module to db 
    result_or_throw(add_file(ri), ?LINE),?debug_test,
    result_or_throw(add_file(referl_ui_nitrogen_services), ?LINE),?debug_test,
    %spawn some sq - if any of them is denied error, otherwise ok
    master_multi_sq("mods.funs.vars.ref", 3),?debug_test,
    %spawn a huge sq, then add a module to db - if it is denied then ok, otherwise error
    Worker=run_with_callback(fun()->sq("mods.funs.(calls)+.vars.ref") end),?debug_test,
    deny_or_throw(add_file(ris), ?LINE),?debug_test,
    %waits for the huge sq - if it succeeded then ok, otherwise error
    receive
        {result, R, Worker} -> result_or_throw(R, ?LINE)
    end,?debug_test,
    %reset db - if the result is is given then ok, otherwise error
    result_or_throw(reset_db(), ?LINE),?debug_test,
    ok.


make_ui_request(UIArgs)->
    ReqID = ?UI:getid(),
    case ?UI:request(ReqID,UIArgs) of
        ok -> receive
                        {ReqID, reply, _R} -> ok
              end;
        deny -> deny
    end.

reset_db()->
    make_ui_request({reset}).

add_file(FileName)->
    Path0 = filename:split(filename:dirname(code:which(FileName))),
    Path =filename:join(lists:sublist(Path0,length(Path0)-1)),
    File = filename:join([Path, "src", atom_to_list(FileName)++".erl"]),
    make_ui_request({add_dir,File}).

sq(Query)->
    Args=[{display_opt,[{positions,scalar}, {output,other}]},
          {start_opt,[]},
          {querystr,Query}],
    make_ui_request({transform, semantic_query, Args}).

run_with_callback(Fun) when is_function(Fun)->
    Pid = self(),
    F = fun()->
                Result = Fun(),
                Pid ! {result, Result, self()}
        end,
    spawn(F).

result_or_throw(deny, Line)-> throw({error, deny, Line});
result_or_throw(_, _)->ok.

deny_or_throw(deny, _)-> ok;
deny_or_throw(_, Line)-> throw({error, result, Line}).

master_multi_sq(Query, CWorker) when is_integer(CWorker) andalso CWorker>0 ->
    ReceiverPid = produce_receiver(self(), CWorker),
    WorkerFun=fun()->
                      Result = sq(Query),
                      ReceiverPid ! {worker_result, Result}                   
              end,
    [spawn(F) || F <-lists:duplicate(CWorker, WorkerFun) ],
    Result = receive
        {receiver_finished, R} ->R
    end,
    case lists:any(fun(Elem)->Elem == deny end, Result) of
        true ->  throw({error, deny, ?LINE});
        false -> ok
    end.

produce_receiver(ParentPid, CWorker) when is_pid(ParentPid) ->
    spawn(fun()->
                  Result = receive_loop(CWorker, []),
                  ParentPid ! {receiver_finished, Result}
          end).

receive_loop(0, Acc)->
    Acc;
receive_loop(CWorker, AccIn) when is_integer(CWorker) andalso CWorker>0 ->
    Result = receive
        {worker_result, R} -> R
    end,
    receive_loop(CWorker-1, AccIn ++ [Result]).
