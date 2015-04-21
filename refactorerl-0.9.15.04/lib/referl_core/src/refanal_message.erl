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

%%% @doc Data flow based message passing analysis.
%%%
%%% @author Melinda Toth <toth_m@inf.elte.hu>

-module(refanal_message).
-vsn("$Rev: 9696 $ ").

-export([analyse/0, analyse/1]).

-export([run/1, is_atom_expr/1, atom_value/1, atomnodes/1]).

-include("core.hrl").

-define(CallAnal, refcore_callanal).

-spec analyse() -> ok.
analyse() -> analyse([]).

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
    SpawnL = find_spawn_expr(SFun),
    SFuns  = lists:append(find_spawned_funs(SpawnL, Mode)),
    MatchList = match_send_and_rec(SFuns),
    Result = lists:foreach(fun({S,R}) -> 
                    [?Graph:mklink(From, flow, To) || From <- S, To <- R]
                  end, MatchList),
    % semantic objects have changed
    ?FileMan:inc_sem_db_hash(),
    Result.

match_send_and_rec([{SApp, FunList} | Tail]) ->
%%    Reach = ?Dataflow:reach([SApp], [{back, false}]),
    Reach = run(fun()-> ?Dataflow:reach([SApp], [{back, false}]) end),
    RegApps = find_reg_apps(Reach),
    RegNames = find_reg_names(RegApps),
    SentMessages = find_sent_message(Reach ++ RegNames),
    ExprL = ?Query:exec(FunList, ?Query:seq([?Fun:definition(), 
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
    Reach = run(fun() -> ?Dataflow:reach(Names, [back]) end),
    AtomExprs = lists:filter(fun is_atom_expr/1, Reach),
    Atoms = lists:map(fun atom_value/1, AtomExprs),
    AtomNodes = atomnodes(Atoms),
    run(fun() -> ?Dataflow:reach(AtomNodes, [{back, false}]) end).
    

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
    [{SApp, functions(FunList, Mode)} | find_funs_in_sapp(Tail, Mode)];
find_funs_in_sapp([], _) ->
    [].

functions([{M, F, undefined} | FunList], Mode = heuristic) ->
    ?Query:exec(?Query:seq(?Mod:find(M), [{func, {name, '==', F}}])) ++ 
        functions(FunList, Mode);
functions([{M, undefined, A} | FunList], Mode = heuristic) ->
    ?Query:exec(?Query:seq(?Mod:find(M), [{func, {arity, '==', A}}])) ++
        functions(FunList, Mode);
functions([{undefined, F, A} | FunList], Mode = heuristic) ->
    ?Query:exec(?Query:seq(?Mod:all(), ?Fun:find(F,A))) ++ 
        functions(FunList, Mode);
functions([{M, F, A} | FunList], Mode) ->
    case ?Query:exec(?Query:seq(?Mod:find(M), ?Fun:find(F,A))) of
        [Fun] -> [Fun | functions(FunList, Mode)];
        _     -> functions(FunList, Mode)
    end;
functions([], _) ->
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
    case  {wrap(?CallAnal:lookup_ID(MN, {})),
           wrap(?CallAnal:lookup_ID(FN, {})),
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

is_atom_expr(Node) -> (?Graph:data(Node))#expr.type == atom.
atom_value(Node) ->  #expr{type = atom, value = Val} = ?Graph:data(Node), Val.

run(Fun) ->
    Self = self(),
    Pid = spawn(fun() -> R = Fun(), Self ! R end),
    receive
        Res -> Res
    after 
        10000 -> exit(Pid, kill), []
    end.

%% -define(mnesia).
-ifdef(mnesia).
atomnodes(Atoms) ->
    FindAtom = 
        fun(AList) ->
                lists:map(fun(A) -> 
                          mnesia:select(expr, [{{expr, '$1', 
                               #expr{type = atom, value = A, _ = '_'}, '_'}, 
                               [], ['$1']}])
                      end, AList)
        end,
    {atomic, Res} = mnesia:transaction(FindAtom, [Atoms]),
    lists:map(fun(N) -> {'$gn', expr, N} end, lists:append(Res)).
-else.
atomnodes(Atoms) ->
    Exprs = ?Query:exec(?Query:seq([?Mod:all(), ?Mod:locals(), 
                                    ?Fun:definition(), ?Form:clauses(),
                                    ?Clause:body(), ?Expr:deep_sub()])),
    lists:filter(fun(E) -> 
                         ?Expr:type(E) == atom andalso 
                         lists:any(fun(A) -> ?Expr:value(E) == A end, Atoms)
                 end, Exprs).
-endif.
