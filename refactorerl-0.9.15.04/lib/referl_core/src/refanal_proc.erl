%% -*- coding: latin-1 -*-

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

%%% @doc The module implements process analysis that extends the SPG
%%% with process semantic information. It creates new process nodes
%%% and communication links between them. Tha analysis also invokes
%%% `ets' analysis that adds ets manipulation information to the
%%% graphs.
%%%
%%% @author Melinda Toth <toth_m@inf.elte.hu>
%%% @author Istvan Bozo <bozo_i@inf.elte.hu>

-module(refanal_proc).
-vsn("$Rev: 11223 $ ").
-behaviour(refcore_anal).

%% callback functions
-export([schema/0, externs/1, insert/4, remove/4, update/2]).

-export([anal_proc/0, clean/0, create_dot/0]).

-include("core.hrl").

-define(CallAnal, refcore_callanal).
-define(ProcLib, refanal_proc_lib).
-define(PT, processes).

%%% @private
schema() ->
    [{pid, record_info(fields, pid), [{spawn, pid}, {spawn_link, pid}, 
                                      {link, pid}, {register, pid}, 
                                      {send, pid}, {create, ets_tab}, 
                                      {read, ets_tab}, {write, ets_tab}]},
     {expr, [{spawn_def, pid}, {reg_def, pid}, {eval_in, pid}]},
     {root, [{pid, pid}]}
    ].

%%% @private
externs(_) -> [].

%%% @private
insert(_Parent, _Pre, {_Tag, _Child}, _Post) -> ok.

%%% @private
remove(_Parent, _Pre, {_Tag, _Child}, _Post) -> ok.

%%% @private
update(_Node, _Data) -> ok.

-spec anal_proc() -> ok.
%% @doc The function analyses the process creation, communication
%% between them and creates a process communication graph from this
%% information. It creates an ets table `processes' where the process
%% nodes (executed function in the process) and the connections are
%% stored. The hidden communication through `ets' tables are also
%% considered, and included in the final graph.
anal_proc()->
    analyse(),
    create_additional_pids(),
    create_graph(),
    refanal_ets:add_ets_read_write().

-spec clean() -> ok.
%% @doc The function removes semantic information related to the
%% process analysis. Removes `pid' and `ets_tab' semantic nodes and
%% links from the SPG and deletes the `processes' table.
clean() ->
    %% cleaning up process information from the SPG
    refanal_ets:clean(),
    Pids = ?Graph:path(?Graph:root(), [pid]),
    [?Graph:rmlink(Pid, Link, To) || Pid <- Pids, 
                                     {Link, To} <- ?Graph:links(Pid)],
    [?Graph:rmlink(From, Link, Pid) || Pid <- Pids, 
                                       {Link, From} <- ?Graph:links(Pid)],
    lists:foreach(fun ?Graph:delete/1, Pids),

    %% cleaning up the communication information, removing the
    %% processes `ets' table
    case ets:info(?PT) of
        undefined -> ok;
        _         -> ets:delete(?PT)
    end,
    ok.

%% @doc Returns the applications of the spawn* functions.
find_spawns() ->
    ?Query:exec(?Query:seq([?Mod:find(erlang), 
                            ?Fun:find(spawn_link, 3),
                            ?Fun:applications()])).

%% @doc Returns the applications of the register/2 function.
find_registers() ->
    ?Query:exec(?Query:seq([?Mod:find(erlang), 
                            ?Fun:find(register,2),
                            ?Fun:applications()])).

%% @doc Extracts the pid informations from the SPG and creates the
%% process model in a named `ets' table (`processes'). 
create_graph()->
    case ets:info(?PT) of
        undefined -> ets:new(?PT, [named_table,bag]);
        _         ->
            ets:delete(?PT),
            ets:new(?PT, [named_table,bag])
    end,
%    DG = digraph:new(),
    Spawns = find_spawns(),
    SLinks = [{?ProcLib:get_proc(E), spawn_link, ?Graph:path(E, [spawn_def])} 
              || E <- Spawns],
    Regs   = find_registers(),
    RLinks = [{?ProcLib:get_proc(E), register, ?Graph:path(E, [reg_def])} 
              || E <- Regs],
%% Spawned/registered processes are linked to the spawning/registering process
    [[ets:insert(?PT, {{A, B},
                       ?ProcLib:label(?Graph:data(A)),
                       LinkType, 
                       ?ProcLib:label(?Graph:data(B))}) 
      || A <- List1,B <- List2] || {List1,LinkType,List2} <- SLinks ++ RLinks],
    %?d(SLinks++RLinks),
    AllPids = ?Graph:path(?Graph:root(), [{pid, {reg_names, '/=', [sp]}}]),
    SpawnedPids = lists:append(
                    ets:match(?PT, {{'_','$1'}, '_', spawn_link, '_'})),
%% Free pids are linked to SP
    [ets:insert(?PT, {{?ProcLib:ensure_sp(), P},
                      ?ProcLib:label(?Graph:data(?ProcLib:ensure_sp())),
                      spawn_sp, 
                      ?ProcLib:label(?Graph:data(P))})
     || P <- AllPids -- SpawnedPids],
    Sends = ?ProcLib:find_nodes_by_type(send_expr),
    [begin
         Message = ?Query:exec1(E, ?Expr:child(2), send),
         Flows   = ?ProcLib:run(fun()-> ?Dataflow:reach([Message],
                                                        [{back, false}]) end),
         Recs    = [ R || E1<-Flows, R <- ?Query:exec(E1, [top, 
                                                           {pattern, back}, 
                                                           {exprcl, back}]), 
                          (?Graph:data(R))#expr.type == receive_expr],
         [ets:insert(?PT, {{S, R}, ?ProcLib:label(?Graph:data(S)),
                           {send, ?Syn:flat_text(Message)}, 
                           ?ProcLib:label(?Graph:data(R))}) || 
             S <- ?ProcLib:get_proc(E),
             R <- lists:flatmap(fun ?ProcLib:get_proc/1, Recs)]      
     end || E <- Sends],
    
    ok.


-spec analyse() -> ok.
analyse() -> analyse([{strict, false}]).

%% @doc Possible options: `strict' (true by default).
-spec analyse([proplists:property()]) -> ok.
analyse(Options) -> %% {Mod, Name, Arity}
    Mode =
        case proplists:get_value(strict, Options, true) of
            true  -> strict;
            false -> heuristic
        end,
    AllFun = ?Query:exec(?Query:seq([?Mod:all(), ?Mod:locals()])),
    SFun =  [F || F <- AllFun,  
                  ?Query:exec(F, [{funcall, {{name, '==', spawn_link}, 'and', 
                                             {arity, '==', 3}}}]) =/= []],
%% todo: spawn*
    SpawnL = find_spawn_expr(SFun),
    SFuns  = lists:append(find_spawned_funs(SpawnL, Mode)),
    MatchList = match_send_and_rec(SFuns),
    Result = lists:foreach(fun({S,R}) -> 
                    [?ProcLib:ensure_link(From, flow, To) || From <- S, To <- R]
                  end, MatchList),
    % semantic objects have changed
    ?FileMan:inc_sem_db_hash(),
    Result.

match_send_and_rec([{SApp, FunList} | Tail]) ->
%%    Reach = ?Dataflow:reach([SApp], [{back, false}]),
    Reach = ?ProcLib:run(fun()-> ?Dataflow:reach([SApp], [{back, false}]) end),
    RegApps = find_reg_apps(Reach),
    {RegANames, RegNames} = find_reg_names(RegApps),
    Pids = ?Graph:path(SApp, [spawn_def]),
    [?ProcLib:ensure_link(App, reg_def, Pid) || App <- RegApps, Pid <- Pids],
%?d(RegNames),
%    RegANames = lists:map(fun ?ProcLib:atom_value/1, RegNames),
    [?Graph:update(Pid, 
                   (?Graph:data(Pid))#pid{reg_names = lists:usort(RegANames)}) 
     || Pid <- Pids],
    SentMessages = find_sent_message(Reach ++ RegNames),
    ExtFunList = ?Query:exec(FunList, [funcall]) ++ FunList,
%% todo: add the full CG
    ExprL = ?Query:exec(ExtFunList, ?Query:seq([?Fun:definition(), 
                                             ?Form:clauses(), 
                                             ?Clause:body(), 
                                             ?Expr:deep_sub()])),
    ReceivePats = lists:append([?Graph:path(E, [exprcl, pattern]) || 
                                    E <- ExprL,?Expr:type(E) =:= receive_expr]),
    [{SentMessages, ReceivePats} | match_send_and_rec(Tail)];
match_send_and_rec([]) ->
    [].

find_reg_names(Apps) ->
    Names = ?Query:exec(Apps, ?Query:seq(?Expr:child(2), ?Expr:child(1))),
    Reach = ?ProcLib:run(fun() -> ?Dataflow:reach(Names, [back]) end),
    AtomExprs = lists:filter(fun ?ProcLib:is_atom_expr/1, Reach),
    Atoms = lists:map(fun ?ProcLib:atom_value/1, AtomExprs),
    AtomNodes = ?ProcLib:atomnodes(Atoms),
    {Atoms, ?ProcLib:run(fun() -> ?Dataflow:reach(AtomNodes, 
                                                  [{back, false}]) 
                         end)}.
    

find_reg_apps([E | Tail]) ->
    lists:filter(fun(A) -> 
                     ?Expr:type(A) =:= application andalso 
                     ?Graph:path(A, [{funeref, {{name, '==', register}, 'and',
                                                {arity,'==',2}}},
                                     {{func,back},{name,'==',erlang}}])  =:= []
                 end, ?Query:exec(E, ?Query:seq(?Expr:parent(),
                                                ?Expr:parent()))) 
                      ++ find_reg_apps(Tail);
find_reg_apps([]) ->
    [].

find_sent_message([R | Reach]) ->
    case ?Query:exec(R, ?Expr:parent()) of
        [P] -> case ?Expr:type(P) of 
                   send_expr -> [?Query:exec1(P, ?Expr:child(2), bad_node) | 
                                 find_sent_message(Reach)];
                    _        -> find_sent_message(Reach)
               end;
        _   -> find_sent_message(Reach)
    end;
find_sent_message([]) ->
    [].

find_spawned_funs([{_Fun, Spawn} | Tail], Mode) ->
    case find_funs_in_sapp(Spawn, Mode) of
        [] -> find_spawned_funs(Tail, Mode);
        S  -> [S | find_spawned_funs(Tail, Mode)]
              %%[{Fun, S} | find_spawned_funs(Tail)]
    end;
find_spawned_funs([], _) ->
    [].

find_funs_in_sapp([{_SApp, todo_more_heuristic} | Tail], Mode) ->
    find_funs_in_sapp(Tail, Mode);
find_funs_in_sapp([{SApp, FunList} | Tail], Mode) ->
    [{SApp, functions(SApp, FunList, Mode)} | find_funs_in_sapp(Tail, Mode)];
find_funs_in_sapp([], _) ->
    [].

functions(SApp, [{M, F, undefined} | FunList], Mode = heuristic) ->
%% todo: MFA not loaded into the DB?
    Funs = ?Query:exec(?Query:seq(?Mod:find(M), [{func, {name, '==', F}}])),
    Pid = ?ProcLib:ensure_node(#pid{mod=M, func=F}),
    ?ProcLib:ensure_link(?Graph:root(), pid, Pid),
    ?ProcLib:ensure_link(Pid, spawn_def, SApp),
    Funs ++ functions(SApp, FunList, Mode);
functions(SApp, [{M, undefined, A} | FunList], Mode = heuristic) ->
%% todo: MFA not loaded into the DB?
    Funs = ?Query:exec(?Query:seq(?Mod:find(M), [{func, {arity, '==', A}}])),
    Pid = ?ProcLib:ensure_node(#pid{mod=M, ary=A}),
    ?ProcLib:ensure_link(?Graph:root(), pid, Pid),
    ?ProcLib:ensure_link(Pid, spawn_def, SApp),
    Funs ++ functions(SApp, FunList, Mode);
functions(SApp, [{undefined, F, A} | FunList], Mode = heuristic) ->
%% todo: MFA not loaded into the DB?
    Funs = ?Query:exec(?Query:seq(?Mod:all(), ?Fun:find(F,A))),
    Pid = ?ProcLib:ensure_node(#pid{func=F, ary=A}),
    ?ProcLib:ensure_link(?Graph:root(), pid, Pid),
    ?ProcLib:ensure_link(SApp, spawn_def, Pid),
    Funs ++ functions(SApp, FunList, Mode);
functions(SApp, [{M, F, A} | FunList], Mode) ->
    case ?Query:exec(?Query:seq(?Mod:find(M), ?Fun:find(F,A))) of
        [Fun] -> 
            Pid = ?ProcLib:ensure_node(#pid{mod=M, func = F, ary=A}),
            ?ProcLib:ensure_link(?Graph:root(), pid, Pid),
            ?ProcLib:ensure_link(SApp, spawn_def, Pid),
            [Fun | functions(SApp, FunList, Mode)];
        _     -> 
            Pid = ?ProcLib:ensure_node(#pid{mod=M, func = F, ary=A}),
            ?ProcLib:ensure_link(?Graph:root(), pid, Pid),
            ?ProcLib:ensure_link(SApp, spawn_def, Pid),
            functions(SApp, FunList, Mode)
    end;
functions(_, [], _) ->
    [].

find_spawn_expr([Fun | List]) ->
    [{Fun, find_spawn_expr(Fun)} | find_spawn_expr(List)];
find_spawn_expr([]) ->
    [];
find_spawn_expr(Fun) ->
    ExprL = ?Query:exec(Fun, ?Query:seq([?Fun:definition(), ?Form:clauses(), 
                                         ?Clause:body(),    ?Expr:deep_sub()])),
    SApps = [E || E <- ExprL, F <- ?Query:exec(E, ?Expr:function()), 
                  ?Expr:type(E) =:= application andalso 
                  ?Fun:name(F) =:= spawn_link andalso ?Fun:arity(F) =:= 3],
    [get_func_data(SApp) || SApp <- SApps].
    

get_func_data(SApp) ->
    [_, ArgList] = ?Query:exec(SApp, ?Expr:children()),
    [MN, FN, AN] = ?Query:exec(ArgList, ?Expr:children()),
    case  {wrap(?CallAnal:lookup_ID(MN, undefined)),
           wrap(?CallAnal:lookup_ID(FN, undefined)),
           ?CallAnal:listcons_length(AN)} of
        {List1, List2, ArityL} when is_list(ArityL) andalso is_list(List1)
                                    andalso is_list(List2) ->
            {SApp, [{MName, FName, Arity} || {_, MName} <- List1, 
                                             {_, FName} <- List2,
                                             Arity <- ArityL]};
        {List1, List2, incalculable} when is_list(List1) andalso 
                                          is_list(List2) ->
            {SApp, [{MName, FName, undefined} || {_, MName} <- List1, 
                                                 {_, FName} <- List2]};
        {List1, undefined, ArityL} when is_list(ArityL) andalso 
                                        is_list(List1) ->
            {SApp, [{MName, undefined, Arity} || {_, MName} <- List1, 
                                                 Arity <- ArityL]};
        {undefined, List2, ArityL} when is_list(ArityL) andalso 
                                        is_list(List2) ->
            {SApp, [{undefined, FName, Arity} || {_, FName} <- List2, 
                                                 Arity <- ArityL]};
%% todo: List1, List2 -> [{}|...], ??? ambflow...
        _ -> {SApp, todo_more_heuristic}
    end.
   
wrap(X = {_, _}) -> [X];
wrap(X) -> X.

create_additional_pids() ->
    ReceiveExprs = ?ProcLib:find_nodes_by_type(receive_expr),
    %% erlang:send/2,3 should also be considered 
    SendExprs    = ?ProcLib:find_nodes_by_type(send_expr),
    SpawnExprs   = find_spawns(),
    RegExprs     = find_registers(),
    EtsApps      = ?Query:exec(?Query:seq([?Mod:find(ets), ?Mod:locals(),
                                           ?Fun:applications()])),
                               
    Exprs        = SendExprs ++ ReceiveExprs ++ SpawnExprs
                               ++ RegExprs ++ EtsApps,
    ContFuns     = lists:append([?Graph:path(Expr, [top, {visib, back}, functx,
                                                    {funcl, back}, fundef])
                                 || Expr <- Exprs]),
    ContFunData  = lists:map(fun(F) -> function_data(F) end, ContFuns),

    Pids         = ?Graph:path(?Graph:root(), [pid]),
    FunPidData   = fun(P) -> {P#pid.mod, P#pid.func, P#pid.ary} end,
    PidData      = [ {Pid, FunPidData(?Graph:data(Pid))} || Pid <- Pids],
    PidCands = lists:flatten([find_pid_candidates(F, PidData, _Trace = [])
                              || F <- lists:zip3(Exprs,ContFuns,ContFunData)]),
    [ case PC of
          {newpid, Expr, Fun} ->
              {M, F, A} = function_data(Fun),
              PidMFA    =
                  ?Graph:path(?Graph:root(),
                              [{pid, 
                                {{{mod, '==', M}, 'and', {func, '==', F}},
                                 'and', {ary, '==', A}}}]),
              case PidMFA of
                  [] ->
                      Pid = ?ProcLib:ensure_node(#pid{mod=M, func = F, ary=A}),
                      ?ProcLib:ensure_link(?Graph:root(), pid, Pid),
                      ?ProcLib:ensure_link(Expr, eval_in, Pid);
                  [Pid]  ->
                      ?ProcLib:ensure_link(?Graph:root(), pid, Pid),
                      ?ProcLib:ensure_link(Expr, eval_in, Pid)
              end;
          {existingpid, Expr, Pid} ->
              ?ProcLib:ensure_link(Expr, eval_in, Pid)
      end || PC <- PidCands].
    
function_data(Fun) ->
    [Module] = ?Graph:path(Fun, [{func, back}]),
    ModName  = (?Graph:data(Module))#module.name,
    FData    = ?Graph:data(Fun),
    FunName  = FData#func.name,
    FunArity = FData#func.arity,
    {ModName, FunName, FunArity}.

find_pid_candidates({Expr, FunId, FunData}, PidData, Trace) ->
    case {lists:member(FunId, Trace), matches(FunData, PidData)} of 
        {true, _}       -> [];
        {_,  {true, P}} -> {existingpid, Expr, P};
        _ ->
            NewCallers = [C || C <- ?Graph:path(FunId, [{funcall,back}]),
                               not lists:member(C, Trace)],
            case NewCallers of
                [] -> {newpid, Expr, lists:last([FunId|Trace])};
                _  ->
                    PidCandL = [find_pid_candidates({Expr, C, function_data(C)},
                                                    PidData, [FunId | Trace])
                                || C <- NewCallers],
                    lists:flatten(PidCandL)
            end
    end.

matches({_FM, _FF, _FA}, []) -> false;
matches({FM, FF, FA}, [{P, {FM, FF, FA}} | _ ]) -> {true, P};
matches({FM, FF, _},  [{P, {FM, FF, undefined}} | _ ]) -> {true, P};
matches({FM, _, FA},  [{P, {FM, undefined, FA}} | _ ]) -> {true, P};
matches({_, FF, FA},  [{P, {undefined, FF, FA}} | _ ]) -> {true, P};
matches(F, [ _ | Tl]) -> matches(F, Tl).

-spec create_dot() -> ok | {error, Reason} when
      Reason :: file:posix() | badarg | system_limit.
%% @doc Creates a `dot' description of the process model. It uses the
%% results of the analysis.
create_dot() ->
    case ets:info(?PT) of
        undefined ->
            io:format("Process information is unavailable, " 
                      "please first run the analysis!");
        _ ->
            Data = [{[{Sn, SnL}, {En, EnL}], {Sn, En, L}}
                    || {{Sn, En}, SnL, L, EnL} <- qlc:eval(ets:table(?PT))],
            {Nodes, Edges} = lists:unzip(Data),
            UniqueNodes    = lists:usort(lists:concat(Nodes)),
            
            Path = filename:join([?MISC:data_dir(), "processes.dot"]),
            
            FileId =
                case file:open(Path, [write]) of
                    {ok, FI} -> FI;
                    {error, Reason1} ->
                        throw(Reason1)
                end,
            io:fwrite(FileId, "digraph{~n",[]),
            
            [io:fwrite(FileId, node_text(Node), []) || Node <- UniqueNodes],
            
            [io:fwrite(FileId, edge_text(Edge), []) || Edge <- Edges],
            io:fwrite(FileId, "}~n",[]),
            case file:close(FileId) of
                ok -> ok;
                {error, Reason2} ->
                    throw(Reason2)
            end
    end.

node_text({NodeId, Label}) ->
    io_lib:format("\"~p\" [label=\"~s\"];~n", [NodeId, Label]).

edge_text({SNodeId, ENodeId, Label}) when is_list(Label) ->
    io_lib:format("\"~p\" -> \"~p\" [label=\"~s\"];~n", [SNodeId, ENodeId,
                                                         Label]);
edge_text({SNodeId, ENodeId, Label}) when is_tuple(Label) ->
    {Tag, String} = Label,
    io_lib:format("\"~p\" -> \"~p\" [label=\"{~p, ~s}\"];~n", [SNodeId, ENodeId,
                                                               Tag, String]);
edge_text({SNodeId, ENodeId, Label}) when is_atom(Label) ->
    io_lib:format("\"~p\" -> \"~p\" [label=\"~p\"];~n", [SNodeId, ENodeId,
                                                         Label]).
    



