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

%%% @author Tibor Pusztai <kondi@elte.hu>

-module(referl_ui_web2_sessions).
-vsn("$Rev$").
-behaviour(gen_server).

%% Public interface
-export([start_link/0, stop/0, set_user_in_session/2, get_sessions_of_user/1,
         get_user_in_session/1, remove_user_from_session/1, remove_session/1,
         get_data_from_session/2, store_data_in_session/2,
         set_session_in_websocket/2, get_user_of_websocket/1,
         remove_websocket/1, get_websockets/0, get_websockets_of_user/1,
         dump/0]).

%% For gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {sessions_by_user, user_by_session, data_by_session,
                websockets_by_session, session_by_websocket}).

%%%% Public interface

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    try
        gen_server:cast(?MODULE, stop)
    catch
        error:noproc -> ok
    end.

get_sessions_of_user(User) ->
    gen_server:call(?SERVER, {get_sessions_of_user, User}).

get_user_in_session(SessionId) ->
    gen_server:call(?SERVER, {get_user_in_session, SessionId}).

set_user_in_session(SessionId, User) ->
    gen_server:call(?SERVER, {set_user_in_session, SessionId, User}).

remove_user_from_session(SessionId) ->
    gen_server:call(?SERVER, {remove_user_from_session, SessionId}).

get_data_from_session(SessionId, Key) ->
    gen_server:call(?SERVER, {get_data_from_session, SessionId, Key}).

store_data_in_session(SessionId, {Key, Data}) ->
    gen_server:call(?SERVER, {store_data_in_session, SessionId, {Key, Data}}).

remove_session(SessionId) ->
    gen_server:cast(?SERVER, {remove_session, SessionId}).

set_session_in_websocket(WebSocket, SessionId) ->
    gen_server:call(?SERVER, {set_session_in_websocket, WebSocket, SessionId}).

get_user_of_websocket(WebSocket) ->
    gen_server:call(?SERVER, {get_user_of_websocket, WebSocket}).

remove_websocket(WebSocket) ->
    gen_server:cast(?SERVER, {remove_websocket, WebSocket}).

get_websockets_of_user(User) ->
    gen_server:call(?SERVER, {get_websockets_of_user, User}).

get_websockets() ->
    gen_server:call(?SERVER, get_websockets).

dump() ->
    gen_server:call(?SERVER, dump).

%%% gen_server functions

init([]) ->
    State = #state{sessions_by_user = dict:new(),
                   user_by_session = dict:new(),
                   data_by_session = dict:new(),
                   websockets_by_session = dict:new(),
                   session_by_websocket = dict:new()},
    {ok, State}.

handle_call({set_user_in_session, SessionId, User}, _From, State0) ->
    %% Remove previous user
    State1 = remove_user_from_session(SessionId, State0),
    %% Handle user_by_session dict
    UserBySession0 = State1#state.user_by_session,
    UserBySession1 = dict:store(SessionId, User, UserBySession0),
    State2 = State1#state{user_by_session = UserBySession1},
    %% Handle sessions_by_user dict
    SessionsByUser0 = State2#state.sessions_by_user,
    SessionsByUser1 = safe_append(User, SessionId, SessionsByUser0),
    State3 = State2#state{sessions_by_user = SessionsByUser1},
    {reply, ok, State3};

handle_call({get_user_in_session, SessionId}, _From, State) ->
    {reply, get_user_by_session(SessionId, State), State};

handle_call({get_sessions_of_user, User}, _From, State) ->
    {reply, get_sessions_of_user(User, State), State};

handle_call({remove_user_from_session, SessionId}, _From, State0) ->
    State1 = remove_user_from_session(SessionId, State0),
    {reply, ok, State1};

handle_call({store_data_in_session, SessionId, {Key, Term}}, _From, State0) ->
    Data = find(SessionId, State0#state.data_by_session, []),
    NewData = lists:keystore(Key, 1, Data, {Key, Term}),
    DataBySession = dict:store(SessionId, NewData, State0#state.data_by_session),
    NewState = State0#state{data_by_session = DataBySession},
    {reply, ok, NewState};

handle_call({get_data_from_session, SessionId, Key}, _From, State) ->
    Data = find(SessionId, State#state.data_by_session, []),
    {reply, proplists:get_value(Key, Data, undefined), State};

handle_call({set_session_in_websocket, WebSocket, SessionId}, _From, State0) ->
    %% Handle session_by_websocket dict
    SessionByWebSocket0 = State0#state.session_by_websocket,
    SessionByWebSocket1 = dict:store(WebSocket, SessionId, SessionByWebSocket0),
    State1 = State0#state{session_by_websocket = SessionByWebSocket1},
    %% Handle websockets_by_session dict
    WebSocketsBySession0 = State1#state.websockets_by_session,
    WebSocketsBySession1 = safe_append(SessionId, WebSocket, WebSocketsBySession0),
    State2 = State1#state{websockets_by_session = WebSocketsBySession1},
    {reply, ok, State2};

handle_call({get_user_of_websocket, WebSocket}, _From, State) ->
    SessionId = get_session_by_websocket(WebSocket, State),
    User = get_user_by_session(SessionId, State),
    {reply, User, State};

handle_call({get_websockets_of_user, User}, _From, State) ->
    {reply, get_websockets_of_user(User, State), State};

handle_call(get_websockets, _From, State) ->
    {reply, get_websockets(State), State};

handle_call(dump, _From, State) ->
    io:format("~p.state:~n", [?MODULE]),
    io:format("  sessions_by_user:      ~p~n", [dict:to_list(State#state.sessions_by_user)]),
    io:format("  user_by_session:       ~p~n", [dict:to_list(State#state.user_by_session)]),
    io:format("  websockets_by_session: ~p~n", [dict:to_list(State#state.websockets_by_session)]),
    io:format("  session_by_websocket:  ~p~n", [dict:to_list(State#state.session_by_websocket)]),
    {reply, ok, State}.

handle_cast({remove_session, SessionId}, State0) ->
    State1 = remove_user_from_session(SessionId, State0),
    %% Handle session_by_websocket dict
    WebSockets = get_websockets_of_session(SessionId, State1),
    SessionByWebSocket0 = State1#state.session_by_websocket,
    SessionByWebSocket1 = erase_keys(WebSockets, SessionByWebSocket0),
    State2 = State1#state{session_by_websocket = SessionByWebSocket1},
    %% Handle websockets_by_session dict
    WebSocketsBySession0 = State2#state.websockets_by_session,
    WebSocketsBySession1 = dict:erase(SessionId, WebSocketsBySession0),
    State3 = State2#state{websockets_by_session = WebSocketsBySession1},
    {noreply, State3};

handle_cast({remove_websocket, WebSocket}, State0) ->
    %% Handle websockets_by_session dict
    SessionId = get_session_by_websocket(WebSocket, State0),
    State1 = case SessionId of
                 undefined ->
                     State0;
                 _ ->
                     WebSockets0 = get_websockets_of_session(SessionId, State0),
                     WebSockets1 = lists:delete(WebSocket, WebSockets0),
                     set_websockets_of_session(SessionId, WebSockets1, State0)
             end,
    %% Handle session_by_websocket dict
    SessionByWebSocket0 = State1#state.session_by_websocket,
    SessionByWebSocket1 = dict:erase(WebSocket, SessionByWebSocket0),
    State2 = State1#state{session_by_websocket = SessionByWebSocket1},
    {noreply, State2};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({yaws_session_end, _Reason, SessionId, _Session}, State) ->
    handle_cast({remove_session, SessionId}, State);

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.

%%%% Private functions

safe_append(Key, Value, Dict) ->
    case dict:is_key(Key, Dict) of
        true -> dict:append(Key, Value, Dict);
        _ ->    dict:store(Key, [Value], Dict)
    end.

erase_keys([Key | KeysTail], Dict0) ->
    Dict1 = dict:erase(Key, Dict0),
    erase_keys(KeysTail, Dict1);

erase_keys([], Dict) ->
    Dict.

find(Key, Dict, Default) ->
    case dict:find(Key, Dict) of
        {ok, Value} -> Value;
        error ->       Default
    end.

get_websockets_of_session(Session, State) ->
    find(Session, State#state.websockets_by_session, []).

get_sessions_of_user(User, State) ->
    find(User, State#state.sessions_by_user, []).

%% Handles only websockets_by_session dict and does not
%% deal with the previous value
set_websockets_of_session(Session, WebSockets, State0) ->
    WebSocketsBySession0 = State0#state.websockets_by_session,
    WebSocketsBySession1 = case WebSockets of
        [] -> dict:erase(Session, WebSocketsBySession0);
        _ ->  dict:store(Session, WebSockets, WebSocketsBySession0)
    end,
    State0#state{websockets_by_session = WebSocketsBySession1}.

%% Handles only sessions_by_user dict and does not
%% deal with the previous value
set_sessions_of_user(User, Sessions, State0) ->
    SessionsByUser0 = State0#state.sessions_by_user,
    SessionsByUser1 = dict:store(User, Sessions, SessionsByUser0),
    State1 = State0#state{sessions_by_user = SessionsByUser1},
    State1.

%% Handles the sessions_by_user and user_by_session dicts
remove_user_from_session(SessionId, State0) ->
    %% Remove the session from the sessions_by_user dict
    State1 = case get_user_by_session(SessionId, State0) of
                 "" ->
                     State0;
                 User ->
                     Sessions0 = get_sessions_of_user(User, State0),
                     Sessions1 = lists:delete(SessionId, Sessions0),
                     set_sessions_of_user(User, Sessions1, State0)
             end,
    %% Remove the related entry from the user_by_session dict
    UserBySession1 = dict:erase(SessionId, State1#state.user_by_session),
    State2 = State1#state{user_by_session = UserBySession1},
    State2.

get_user_by_session(SessionId, State) ->
    find(SessionId, State#state.user_by_session, "").

get_session_by_websocket(WebSocket, State) ->
    find(WebSocket, State#state.session_by_websocket, undefined).

get_websockets(State) ->
    dict:fetch_keys(State#state.session_by_websocket).

get_websockets_of_user(User, State) ->
    Sessions = get_sessions_of_user(User, State),
    GetWebSockets = fun(Session) -> get_websockets_of_session(Session, State) end,
    lists:append(lists:map(GetWebSockets, Sessions)).
