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

-module(referl_ui_web2_websocket).
-vsn("$Rev$").
-behaviour(gen_server).

%% Public interface
-export([notify/2]).
-export([notify/3]).

%% Export websocket callback for yaws
-export([handle_message/1]).

%% For gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {yaws_websocket_pid, session_id}).

-include("yaws_api.hrl").
-include("ui.hrl").
-include_lib("../../referl_core/include/core_export.hrl").

%%%% Public interface

notify(Name, Parameter) ->
   Send = fun(Pid) -> gen_server:cast(Pid, {notification, Name, Parameter}) end,
   lists:foreach(Send, ?WEB2_SESSIONS:get_websockets()).

notify(User, Name, Parameter) ->
   Send = fun(Pid) -> gen_server:cast(Pid, {notification, Name, Parameter}) end,
   lists:foreach(Send, ?WEB2_SESSIONS:get_websockets_of_user(User)).

%%%% websocket callback for yaws

handle_message({text, Binary}) ->
    handle_event(binary_to_event(Binary));

handle_message({close, Status, Reason}) ->
    handle_event({close, {Status, Reason}}),
    {close, normal}.

%%%% websocket callback for handle_message

%% "hello" event sent by front-end after connection
%% has been established. Starts the gen_server
%% passing the yaws websocket pid and hello
%% parameters.
handle_event({"hello", {struct, Proplist}}) ->
    {ok, Pid} = gen_server:start_link(?MODULE, {self(), Proplist}, []),
    put(?SERVER, Pid),
    noreply;

%% All other events should be sent after "hello",
%% so we can pass the event to the associated
%% gen_server instance.
handle_event(Event) ->
    gen_server:cast(get(?SERVER), {event, Event}),
    noreply.

%%%% gen_server functions

init({YWSPid, PropList}) ->
    process_flag(trap_exit, true),
    SessionId = proplists:get_value("sessionId", PropList),
    ?WEB2_SESSIONS:set_session_in_websocket(self(), SessionId),
    State = #state{yaws_websocket_pid = YWSPid,
                   session_id = SessionId},
    send_back(State, "ready", true),
    ?UI:add_msg_handler(self()),
    {ok, State};

init([#arg{pid=YawsWorkerPid, headers=#headers{cookie=[Cookies]}}|_])->
    [SessionId] = [Val || Cookie <- string:tokens(Cookies, "&"),
                    [Key, Val] <- [string:tokens(Cookie, "=")],
                     Key == "referl_web2_session"],
           init({YawsWorkerPid, [{"sessionId", SessionId}]});

init([#arg{pid=YawsWorkerPid}|_]) ->
    process_flag(trap_exit, true),
    State = #state{yaws_websocket_pid = YawsWorkerPid},
    send_back(State, "ready", true),
    ?UI:add_msg_handler(self()),
    {ok, State}.

handle_call(_Msg, _From, State) ->
    {reply, call_not_supported, State}.

handle_cast({event, {Name, Parameter}}, State) ->
    {noreply, handle_event(Name, Parameter, State)};

handle_cast({notification, Name, Parameter}, State) ->
    {noreply, handle_notification(Name, Parameter, State)}.

handle_info({global, jobinfo, {{_, modifier, Phase, _}, Msg}}, State) ->
    Json = {struct, [{global, true},
                     {message, Msg}]},
    case Phase of
        started  -> send_back(State, "db_start", Json);
        finished -> send_back(State, "db_done",  Json);
        _        -> ok
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?UI:del_msg_handler(self()),
    ?WEB2_SESSIONS:remove_websocket(self()),
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.

%%%% handle_event having state


handle_event("request_database_status", _, State) ->
    case reflib_ui_router:can_read_db() of
        false -> send_back(State, "db_start", "");
        true  -> State
    end;

handle_event(close, _, State) ->
    ?WEB2_SESSIONS:remove_websocket(self()),
    State;

handle_event(Name, Parameter, State) ->
    spawn(fun() -> handle_event_async(Name, Parameter, State) end),
    State.

handle_event_async("get_queue_ids", _, State) ->
    User = get_user(State),
    Ids = ?WEB2_SERVICES:get_queue_ids(User),
    RunningIds = proplists:get_value(running, Ids),
    UnseenIds = proplists:get_value(unseen, Ids),
    Data = {struct, [{"running", {array, RunningIds}},
                     {"unseen",  {array, UnseenIds}}]},
    send_back(State, "queue_ids", Data).

%%%% handle_notification

handle_notification(job_started, Id, State) ->
    send_back(State, "job_started", Id),
    handle_event("get_queue_ids", [], State);

handle_notification(job_finished, Id, State) ->
    send_back(State, "job_finished", Id),
    handle_event("get_queue_ids", [], State);

handle_notification(job_seen, Id, State) ->
    send_back(State, "job_seen", Id),
    handle_event("get_queue_ids", [], State);

handle_notification(job_removed, Id, State) ->
    send_back(State, "job_removed", Id),
    handle_event("get_queue_ids", [], State);

handle_notification(db_progress, {Type,File,Pcnt,FormCnt,FormMax,KBps}, S) ->
    Json = {struct, [{type, atom_to_list(Type)},
                     {file, File},
                     {percent, Pcnt},
                     {formCount, FormCnt},
                     {formMax, FormMax},
                     {speed, KBps}]},
    send_back(S, "db_progress", Json);

handle_notification(db_start, {Type, Path, IsFolder}, State) ->
    Json = {struct, [{path, Path},
                     {type, atom_to_list(Type)},
                     {isFolder, IsFolder},
                     {byWeb2, true}]},
    send_back(State, "db_start", Json);

handle_notification(db_done, {Type, Path, Result}, State) ->
    PropList = case Result of
        {ok, _} ->
            [{ok, true}];
        {error, {{reflib_ui, somebad, _}, Msg}} ->
            [{ok, true}, {somebad, true}, {message, Msg}];
        {error, _} ->
            [{ok, false}, {error, true}]
    end,
    Json = {struct, [{path, Path}, {type, atom_to_list(Type)} | PropList]},
    send_back(State, "db_done", Json);

handle_notification(_Name, _Parameter, State) ->
    State.

%%%% Private functions

send_back(State, Name, Parameter) ->
    send_back(State, {Name, Parameter}).

send_back(State, Event) ->
    #state{yaws_websocket_pid = YWSPid} = State,
    yaws_api:websocket_send(YWSPid, {text, event_to_binary(Event)}),
    State.

get_user(#state{session_id = SessionId}) ->
    ?WEB2_SESSIONS:get_user_in_session(SessionId).

%% @doc Convert [Name, Parameter] json binary to {Name, Parameter} erlang tuple
binary_to_event(Binary) ->
    {ok, {array, [Name, Parameter]}} = decode_binary_json(Binary),
    {Name, Parameter}.

%% @doc Convert {Name, Parameter} erlang tuple to [Name, Parameter] json binary
event_to_binary({Name, Parameter}) ->
    String = json2:encode({array, [Name, Parameter]}),
    list_to_binary(String).

decode_binary_json(Binary) ->
    JsonString = unicode:characters_to_list(Binary, utf8),
    json2:decode_string(JsonString).
