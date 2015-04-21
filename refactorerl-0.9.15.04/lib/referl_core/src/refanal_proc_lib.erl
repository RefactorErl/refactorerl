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

%%% @doc The module contains utitlites for process and `ets' analysis.
%%%
%%% @author Melinda Toth <toth_m@inf.elte.hu>
%%% @author Istvan Bozo <bozo_i@inf.elte.hu>

-module(refanal_proc_lib).
-vsn("$Rev: 11223 $ ").

-export([get_proc/1, label/1, run/1, is_atom_expr/1, atom_value/1,
         atomnodes/1, ensure_node/1, ensure_link/3, ensure_sp/0,
         find_nodes_by_type/1]).

-include("core.hrl").

-spec get_proc(Node) -> [Node] when Node :: tuple().
%% @doc Returns the process semantic node for a given node. If it is
%% not connected to any of the processes, it returns the identifier of
%% the super process.
get_proc(Node)->
    case ?Graph:path(Node, [eval_in]) of
        [] -> [ensure_sp()];
        A  -> A
    end.

-spec label(Node) -> string() when Node :: tuple().
%% @doc Returns text for `pid' and `ets' nodes.
label(#ets_tab{names = []})->
    "ets:new/2";
label(#ets_tab{names = [H|Tail]})->
    "ets:new/2 | " ++ atom_to_list(H) 
        ++ lists:append(["," ++ atom_to_list(N) || N <- Tail]);
label(#pid{reg_names = [sp]}) ->
    "SP";
label(#pid{reg_names = undefined, mod = M, func = F, ary = A}) ->
    label_mfa(M, F, A);
label(#pid{reg_names = [], mod = M, func = F, ary = A}) ->
    label_mfa(M, F, A);
label(#pid{reg_names = [R | Tail], mod = M, func = F, ary = A}) ->
    label_mfa(M,F,A) ++ " | " ++ atom_to_list(R) 
        ++ lists:append(["," ++ atom_to_list(N) || N <- Tail]);
label(ok) ->
    "ok".

label_mfa(undefined, F, A) ->
    "*:" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A);
label_mfa(M, undefined, A) ->
    atom_to_list(M) ++ ":*/" ++ integer_to_list(A);
label_mfa(M, F, undefined) ->
    atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "/*";
label_mfa(M, F, A)->
    atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A).

-spec run(Fun) -> term() when Fun :: fun(() -> term()).
%% @doc Executes the given funexpression in a separate process and
%% waits for the result. If the computation is not completed in a
%% certain time the process is killed and an empty list is returned.
run(Fun) ->
    Self = self(),
    Pid = spawn(fun() -> R = Fun(), Self ! R end),
    receive
        Res -> Res
    after 
        5000 -> exit(Pid, kill), []
    end.

-spec is_atom_expr(Node) -> boolean() when Node :: refcore_graph:gnode().
%% @doc The function gets an SPG node and decides whether it is an
%% atom expression or not. If it is an atom expression, it returns
%% `true' else `false'.
is_atom_expr(Node) -> (?Graph:data(Node))#expr.type == atom.

-spec atom_value(Node) -> atom() when Node :: refcore_graph:gnode().
%% @doc The function gets an SPG node type of `atom' and returns the
%% value of that espression.
atom_value(Node) ->  #expr{type = atom, value = Val} = ?Graph:data(Node), Val.

-spec ensure_link(From, Label, To) -> ok when 
      From :: refcore_graph:gnode(),
      To :: refcore_graph:gnode(),
      Label :: atom().
%% @doc The function creates a link between the given nodes `From' and
%% `To' with label `Label' if there is no such link between the nodes.
ensure_link(From, Label, To) ->
    Dest = ?Graph:path(From, [Label]),
    case lists:member(To, Dest) of
        true  -> ok;
        false ->
            ?Graph:mklink(From, Label, To)
    end.

-spec ensure_node(refcore_graph:enode()) -> refcore_graph:enode().
%% @doc The function creates a new SPG node if the given node does not
%% exist. The `pid' nodes are matched upon the module name, function
%% name and arity, other properties are disregarded.
ensure_node(Record)  when is_record(Record, pid) ->
    ensure_node(pid, Record);
ensure_node(Record)  when is_record(Record, ets_tab) ->
    ensure_node(ets_tab, Record).

-spec ensure_sp() -> refcore_graph:enode().
%% @doc The function returnes the node of the superprocess.
ensure_sp() ->
    case ?Graph:path(?Graph:root(), [{pid, {reg_names, '==', [sp]}}]) of
        [Pid] -> Pid;
        []    ->
            Pid = ensure_node(#pid{reg_names=[sp]}),
            ensure_link(?Graph:root(), pid, Pid),
            Pid
    end. 

-spec atomnodes([atom()]) -> [refcore_graph:gnode()].
%% @doc The function returns those SPG nodes that are type of `atom'
%% and their values are members of the given list.

%-define(mnesia, true).
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

find_nodes_by_type(Type) ->
    FindNodes = 
        fun() ->
                mnesia:select(expr,
                              [{{expr, '$1', #expr{type = Type, _ = '_'}, '_'}, 
                                [], ['$1']}])
        end,
    {atomic, Res} = mnesia:transaction(FindNodes),
    lists:map(fun(N) -> {'$gn', expr, N} end, Res).

ensure_node(Type, NodeData) ->
    FindNodes = 
        fun() ->
                mnesia:select(Type,[{{Type, '$1', matching_expr(NodeData), '_'},
                                     [],
                                     ['$1']}])
        end,
    {atomic, Res} = mnesia:transaction(FindNodes),
    case Res of
        []        -> ?Graph:create(NodeData);
        [PidNum] -> {'$gn', Type, PidNum}
    end.

matching_expr(#pid{mod = M, func = F, ary = A}) ->
    #pid{mod  = M, func = F, ary  = A, _ = '_'};
matching_expr(NodeData) ->
    NodeData.
    
-else.
atomnodes(Atoms) ->
    Exprs = ?Query:exec(?Query:seq([?Mod:all(), ?Mod:locals(), 
                                    ?Fun:definition(), ?Form:clauses(),
                                    ?Clause:body(), ?Expr:deep_sub()])),
    lists:filter(fun(E) -> 
                         ?Expr:type(E) == atom andalso 
                         lists:any(fun(A) -> ?Expr:value(E) == A end, Atoms)
                 end, Exprs).

find_nodes_by_type(Type) ->
    Exprs = ?Query:exec(?Query:seq([?Mod:all(), ?Mod:locals(), 
                                    ?Fun:definition(), ?Form:clauses(),
                                    ?Clause:body(), ?Expr:deep_sub()])),
    lists:filter(fun(E) -> 
                         ?Expr:type(E) == Type
                 end, Exprs).

ensure_node(Type, NodeData) ->
    PidNodes = ?Graph:path(?Graph:root(), [Type]),
    Res = [ PidNode || PidNode <- PidNodes, matching_fun(?Graph:data(PidNode),
                                                         NodeData)],
    case Res of
        []        -> ?Graph:create(NodeData);
        [PidNode] -> PidNode
    end.

matching_fun(#pid{mod = M1, func = F1, ary = A1},
              #pid{mod = M1, func = F1, ary = A1}) -> true;
matching_fun(#pid{}, #pid{}) -> false;
matching_fun(Node1, Node2) -> Node1 == Node2.


-endif.
