%%% -*- coding: utf-8 -*-

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

%%% @doc ETS table based cache which flushes the content when database is
%%% changed. There is no other clearing mechanism.
%%%
%%% @author Tibor Pusztai <kondi@elte.hu>


-module(referl_ui_web2_cache).
-vsn("$Rev$").
-behaviour(gen_server).

%% Public interface
-export([start_link/0, stop/0, set/2, get/1, get/2, remove/1, flush/0]).

%% For gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

-define(SERVER, ?MODULE).

-include("ui.hrl").

-record(state, {}).

%%%% Public interface

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    try
        gen_server:cast(?MODULE, stop)
    catch
        error:noproc -> ok
    end.

get(Key) ->
    gen_server:call(?SERVER, {get, Key}).

get(Key, Fun) ->
    gen_server:call(?SERVER, {get, Key, Fun}).

set(Key, Value) ->
    gen_server:call(?SERVER, {set, Key, Value}).

remove(Key) ->
    gen_server:call(?SERVER, {remove, Key}).

flush() ->
    gen_server:call(?SERVER, {flush}).

%%% gen_server functions

init([]) ->
    ets:new(?SERVER, [set, named_table]),
    ?UI:add_msg_handler(self()),
    State = #state{},
    {ok, State}.

handle_call({set, Key, Value}, _From, State) ->
    {reply, set_local(Key, Value), State};

handle_call({get, Key}, _From, State) ->
    Result = case ets:lookup(?SERVER, Key) of
                 [{_, Value}] -> Value;
                 _            -> undefined
             end,
    {reply, Result, State};

handle_call({get, Key, Fun}, _From, State) ->
    Result = case get_local(Key) of
                 undefined -> Value = Fun(),
                              set_local(Key, Value),
                              Value;
                 Value     -> Value
             end,
    {reply, Result, State};

handle_call({remove, Key}, _From, State) ->
    {reply, remove_local(Key), State};

handle_call({flush}, _From, State) ->
    {reply, flush_local(), State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({_, jobinfo, {{_, modifier, _, _}, _}}, State) ->
    flush_local(),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?UI:del_msg_handler(self()),
    ets:delete(?SERVER),
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.

%%%% Private functions

set_local(Key, Value) ->
    ets:insert(?SERVER, {Key, Value}).

get_local(Key) ->
    case ets:lookup(?SERVER, Key) of
        [{_, Value}] -> Value;
        _            -> undefined
    end.

remove_local(Key) ->
    ets:delete(?SERVER, Key).

flush_local() ->
    ets:delete_all_objects(?SERVER).
