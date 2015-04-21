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

%%% @doc Function side effect analyser. Caches function side effect
%%% information for more efficient recalculation in case of change.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>

-module(refcore_funprop).
-vsn("$Rev: 9568 $ ").
-behaviour(gen_server).

-export([update/2, remove/1, reset/0]).

%% gen_server exports
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("core.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(FUNPROP_TIMEOUT, infinity).

%% @spec update(gnode(), undefined | {integer(), [gnode()]}) -> ok
%% @doc Updates side effect information. `Fun'  is a `#func{}' node, `Info'
%% may be `undefined', which means the body of the function is not known (e.g.
%% got removed from the graph), or a tuple that contains the number of
%% expressions with possible side effects in the body of the function, and the
%% list of functions (`#func{}' nodes) called in the body of the function.
update(Fun, Info) ->
    gen_server:call(?FUNPROP_SERVER, {update, Fun, Info}, ?FUNPROP_TIMEOUT).

%% @spec remove(gnode()) -> ok
%% @doc Removes a function node from the cache (must be called when a function
%% node is deleted).
remove(Fun) ->
    gen_server:call(?FUNPROP_SERVER, {remove, Fun}, ?FUNPROP_TIMEOUT).


%% @spec reset() -> ok
%% @doc This function has to be called before the graph is reset.
%%      The function empties the internal funprop representation.
reset() ->
    gen_server:call(?FUNPROP_SERVER, {reset}, ?FUNPROP_TIMEOUT).


%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @doc Starts the server.
start_link() ->
    gen_server:start_link({local, ?FUNPROP_SERVER}, ?MODULE, [], []).

%% @private
init(_) ->
    {ok, init_state()}.

%% @private
handle_call({update, Fun, Info}, _, State) ->
    {reply, ok, handle_update(Fun, Info, State)};
handle_call({remove, Fun}, _, State) ->
    {reply, ok, handle_remove(Fun, State)};
handle_call({reset}, _, _State) ->
    NewState = reset_call_graph(),
    {reply, ok, NewState}.

%% @private
handle_cast(_, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_Old, State, _Extra) ->
    {ok, State}.


%%% ============================================================================
%%% Server implementation

init_state() ->
    ets:new(call_graph, [named_table]).

handle_remove(Fun, CG) ->
    {Dirty, Refs, Deps} = read_info(Fun, CG),
    [del_ref(From, Fun, CG) || From <- Refs],
    [del_call(Fun, Dep, CG) || Dep <- Deps],
    Dirty =/= no orelse [turn_dirty(Ref, CG) || {_,Ref} <- Refs],
    ets:delete(CG, Fun),
    CG.

handle_update(Fun, undefined, CG) ->
    {Dirty, Refs, _} = read_info(Fun, CG),
    store_info(Fun, int, [], CG),
    Dirty =/= no orelse [turn_dirty(Ref, CG) || {_,Ref} <- Refs],
    CG;

handle_update(Fun, {Pure, Calls}, CG) ->
    {Dirty, Refs, _} = read_info(Fun, CG),
    {NewDirty, Update} =
        if
            Pure ->
                %% Looking for `int' in the closure instead of `int' or `ext'
                %% in direct calls ensures that mutually recursive functions
                %% are handled properly
                case lists:any(fun (F) -> dirty(F, CG) == int end,
                     closure(Calls, fwd, CG) -- [Fun]) of
                    true when Dirty == no  -> {ext, fun turn_dirty/2};
                    true                   -> {ext, no};
                    false when Dirty == no -> {no,  no};
                    false                  -> {no,  fun turn_pure/2}
                end;
            not Pure and (Dirty == no)       -> {int, fun turn_dirty/2};
            not Pure                       -> {int, no}
        end,
    %% todo Check whether this does the same thing.
    %% DirtyNeighbours = [F ||  F <- closure(Calls, fwd, CG),
    %%                          F =/= Fun, dirty(F, CG) == int],
    %% {NewDirty, Update} =
    %%     case {Pure, DirtyNeighbours, Dirty} of
    %%         {true,  [], no}  -> {no,  no};
    %%         {true,  [], _}   -> {no,  fun turn_pure/2};
    %%         {true,  _,  no}  -> {ext, fun turn_dirty/2};
    %%         {true,  _, _}    -> {ext, no};
    %%         {false, _, no}   -> {int, fun turn_dirty/2};
    %%         {false, _, _}    -> {int, no}
    %%     end,
%io:format("UPDATE: ~p/~p->~p (upd: ~p)~n  refs: ~p~n  deps: ~p~n",
%          [Fun, Dirty, NewDirty, Update, Refs, Calls]),
    store_info(Fun, NewDirty, Calls, CG),
    is_function(Update) andalso
        [Update(F, CG) || F <- closure(Refs, back, CG)],
    CG.


%% Empties the table `call_graph' and returns its name.
reset_call_graph() ->
    case lists:member(call_graph, ets:all()) of
        true  ->
            ets:delete_all_objects(call_graph),
            call_graph;
        false ->
            ets:new(call_graph, [named_table])
    end.

dirty(Fun, CG) ->
    [{Fun, Dirty, _, _}] = ets:lookup(CG, Fun),
    Dirty.

store_info(Fun, Dirty, Calls, CG) ->
    {_, Refs, Deps} = read_info(Fun, CG),
    D = ?Graph:data(Fun),
    ?Graph:update(Fun, D#func{dirty=Dirty}),
    [add_call(Fun, Dep, CG) || Dep <- Calls -- Deps],
    [del_call(Fun, Dep, CG) || Dep <- Deps -- Calls],
    ets:insert(CG, {Fun, Dirty, Refs, Calls}).

del_ref({Type, From}, Fun, CG) ->
    %% We must update the cache entry of `From' in order to be able to
    %% note the deletion of the call to `Fun'; if we didn't update and
    %% `From' was not present in the cache, we would simply skip
    %% noting the new state of the stored call graph (?Graph:delete/1
    %% on `Fun' deletes all call edges without notifying this cache,
    %% thus changing the stored call graph).
    {Dirty, Refs, Deps} = read_info(From, CG),
    ets:insert(CG, {From, Dirty, Refs, Deps -- [{Type, Fun}]}).

add_call(From, {Type, To}, CG) ->
    ?Graph:mklink(From, Type, To),
    case ets:lookup(CG, To) of
        [] -> ok;
        [{To, Dirty, Refs, Deps}] ->
            ets:insert(CG, {To, Dirty, [{Type, From} | Refs], Deps})
    end.

del_call(From, {Type, To}, CG) ->
    ?Graph:rmlink(From, Type, To),
    case ets:lookup(CG, To) of
        [] -> ok;
        [{To, Dirty, Refs, Deps}] ->
            ets:insert(CG, {To, Dirty, Refs -- [{Type, From}], Deps})
    end.

read_info(Fun, CG) ->
    case ets:lookup(CG, Fun) of
        [{Fun, Dirty, Refs, Deps}] -> {Dirty, Refs, Deps};
        [] ->
            #func{dirty=Dirty} = ?Graph:data(Fun),
            Refs = refs(Fun, funcall) ++ refs(Fun, dyncall) ++ refs(Fun, ambcall),
            Deps = deps(Fun, funcall) ++ deps(Fun, dyncall) ++ deps(Fun, ambcall),
            ets:insert(CG, {Fun, Dirty, Refs, Deps}),
            {Dirty, Refs, Deps}
    end.

refs(Fun, Label) ->
    List = ?Graph:path(Fun, [{Label, back}]),
    lists:zip(lists:duplicate(length(List), Label), List).
deps(Fun, Label) ->
    List = ?Graph:path(Fun, [Label]),
    lists:zip(lists:duplicate(length(List), Label), List).

-type calldesc() :: {funcall, node()}
                  | {dyncall, node()}
                  | {ambcall, node()}.

-spec closure([calldesc()], back | fwd, ets:tab()) -> [node()].
closure(Calls, Dir, CG) ->
    Funs = [Fun || {_Label,Fun} <- Calls],
    Tab = ets:new(closure, []),
    ets:insert(Tab, [{Fun} || Fun <- Funs]),
    closure(Funs, Tab, Dir, CG).

-spec closure([node()], ets:tab(), back | fwd, ets:tab()) -> [node()].
closure([], Tab, _, _) ->
    Result = ets:select(Tab, [{{'$1'},[],['$1']}]),
    ets:delete(Tab),
    Result;

closure([Fun | Rest], Tab, Dir, CG) ->
    Next =
        case {Dir, read_info(Fun, CG)} of
            {back, {_, Refs, _}} -> Refs;
            {fwd,  {_, _, Deps}} -> Deps
        end,
    New = [F || {_,F} <- Next, ets:insert_new(Tab, {F})],
    closure(New ++ Rest, Tab, Dir, CG).

turn_dirty(Fun, CG) ->
    {Dirty, _, Deps} = read_info(Fun, CG),
    if
        Dirty == no -> store_info(Fun, ext, Deps, CG);
        true -> ok
    end.

turn_pure(Fun, CG) ->
    {Dirty, _, Deps} = read_info(Fun, CG),
    case Dirty == int orelse
        lists:any(fun (F) -> dirty(F, CG) == int end,
                  closure(Deps, fwd, CG)) of
        true  -> ok;
        false -> store_info(Fun, no, Deps, CG)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Funprop test

update_test() ->
    M  = ?Graph:create(#func{name=m, arity=0}),
    ?FunProp:update(M, {true, []}),
    ?assertMatch(#func{dirty=no}, ?Graph:data(M)),

    S1 = ?Graph:create(#func{name=s, arity=1}),
    ?FunProp:update(S1, {true, [{funcall, M}]}),
    ?assertMatch(#func{dirty=no}, ?Graph:data(S1)),

    S2 = ?Graph:create(#func{name=s, arity=2}),
    ?FunProp:update(S2, {false, [{funcall, M}]}),
    ?assertMatch(#func{dirty=int}, ?Graph:data(S2)),

    T1 = ?Graph:create(#func{name=t, arity=1}),
    ?FunProp:update(T1, undefined),
    ?assertMatch(#func{dirty=int}, ?Graph:data(T1)),

    ?FunProp:update(M, {true, [{funcall, T1}]}),
    ?assertMatch(#func{dirty=ext}, ?Graph:data(M)),
    ?assertMatch(#func{dirty=ext}, ?Graph:data(S1)),

    ?FunProp:update(T1, {true, []}),
    ?assertMatch(#func{dirty=no}, ?Graph:data(T1)),
    ?assertMatch(#func{dirty=no}, ?Graph:data(M)),
    ?assertMatch(#func{dirty=no}, ?Graph:data(S1)),

    T2 = ?Graph:create(#func{name=t, arity=2}),
    ?FunProp:update(T2, {true, [{funcall, S2}]}),
    ?assertMatch(#func{dirty=ext}, ?Graph:data(T2)),

    ?FunProp:update(M, {true, [{funcall, T1}, {funcall, T2}]}),
    ?assertMatch(#func{dirty=ext}, ?Graph:data(M)),
    ?assertMatch(#func{dirty=ext}, ?Graph:data(S1)),
    ?assertMatch(#func{dirty=int}, ?Graph:data(S2)),
    ?assertMatch(#func{dirty=no}, ?Graph:data(T1)).

update_delete_test() ->
    A = ?Graph:create(#func{name=a, arity=0}),
    B = ?Graph:create(#func{name=b, arity=0}),
    C = ?Graph:create(#func{name=c, arity=0}),
    ?FunProp:update(C, {true, []}),
    ?FunProp:update(B, {true, [{funcall, C}]}),
    ?FunProp:update(A, {true, [{funcall, B}]}),
    ?assertMatch(#func{dirty=no}, ?Graph:data(A)),
    ?assertMatch(#func{dirty=no}, ?Graph:data(B)),
    ?assertMatch(#func{dirty=no}, ?Graph:data(C)),

    ?FunProp:remove(A),
    ?Graph:delete(A),

    ?FunProp:update(C, {false, []}),
    
    ?assertMatch(#func{dirty=int}, ?Graph:data(C)),
    ?assertMatch(#func{dirty=ext}, ?Graph:data(B)).

update_delete2_test() ->
    A = ?Graph:create(#func{name=a, arity=0}),
    B = ?Graph:create(#func{name=b, arity=0}),
    ?FunProp:update(B, {true, [{funcall, A}]}),
    ?FunProp:remove(A),
    ?Graph:delete(A),
    ?FunProp:remove(B),
    ?Graph:delete(B).

update_delete3_test() ->
    A = ?Graph:create(#func{name=a, arity=0}),
    B = ?Graph:create(#func{name=b, arity=0}),
    ?FunProp:update(A, {true, [{funcall, B}]}),
    ?FunProp:remove(A),
    ?Graph:delete(A),
    ?FunProp:remove(B),
    ?Graph:delete(B).
