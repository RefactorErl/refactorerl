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

%%% @doc Asynchron transformation framework. Transformations are implemented
%%% in callback modules, and they are executed in a separate process. They
%%% can communicate with the user through this module. Currently only one
%%% normal transformation can be executed at a time, but it is possible
%%% to start nested transformations.
%%%
%%% == Callback modules ==
%%%
%%% Currently the only callback function in a transformation module is
%%%
%%% `prepare(Args::arglist()) -> Transformation'
%%%
%%% where `Transformation = fun() | [Transformation]'.
%%%
%%% This function gets the parameters of the transformation in an {@link
%%% reflib_args:arglist(). argument proplist}, and should return a fun (or a
%%% possibly deep list of funs) that makes the transformation. The `prepare'
%%% function must ensure that the transformation can be carried out, that is,
%%% it should check every side condition and collect every needed data. The
%%% returned fun(s) should not be aborted after making changes to the graph
%%% (which implies that it must not ask questions, as they may be cancelled).
%%%
%%% The first transformation function receives no parameters. The following
%%% functions in the list receive the result of the previous function. In case
%%% of embedded lists of functions, they get the same parameter, their return
%%% values are collected into a list, and that list is passed to the next
%%% function.
%%%
%%% Every function in the list must form a valid ESG batch (see {@link
%%% refcore_esg}). The batch is implicitly closed by this module, the
%%% transformation code should not call the ?ESG module directly.
%%%
%%% Error conditions are signalled by throwing exceptions from the
%%% function. Exception data should be constructed using the macros
%%% `?RefError' and `?LocalError'. See {@link reflib_error} for details.
%%%
%%% When the transformation modifies something, it should use the {@link
%%% touch/1} function to request the saving of the modified file after the
%%% transformation.
%%%
%%% The return value of the last transformation function can be queried using
%%% the {@link wait/0} function.
%%%
%%% == Interaction ==
%%%
%%% A transformation module can ask questions using the {@link ask/1}
%%% function. Questions are identified by an arbitrary term; clients can
%%% specify the ID and make use of a custom ID system, or may let the system
%%% choose a unique ID for a question. Asking a question is an asynchron
%%% operation, the answer is only waited for (and returned) by {@link
%%% answer/1}.
%%%
%%% === Question properties ===
%%%
%%% A question is specified by a list which may contain two-tuples and
%%% atoms. The following elements are supported:
%%%
%%% <dl>
%%%
%%% <dt>`{text, Text}'</dt>
%%%
%%%   <dd>The text of the question (a deep character list). Must be
%%%   specified.</dd>
%%%
%%% <dt>`{type, Type}'</dt>
%%%
%%%   <dd>The type of the question. `Type' may have the following values
%%%   (the default is `string'):
%%%
%%%   <ul>
%%%   <li>`string': the answer is an arbitrary string.</li>
%%%   <li>`yesno': the answer is `yes' or `no'.</li>
%%%   <li>`{select, [Values]}': the answer is one of `Values'.</li>
%%%   </ul>
%%%   </dd>
%%%
%%% <dt>`sync'</dt>
%%%
%%%   <dd>This property means that this question should be answered
%%%   immediately. User interfaces may choose to display this question
%%%   modally, while other questins can be accumulated into a form.</dd>
%%%
%%% <dt>`{ref, Node}'</dt>
%%%
%%%   <dd>This property is a reference to a syntax tree part. The user
%%%   interface will provide a way to show the code that belongs to the
%%%   subtree starting at `Node'.</dd>
%%%
%%% <dt>`{id, Id}'</dt>
%%%
%%%   <dd>The identifier of the question. Default: a unique identifier is
%%%   generated.</dd>
%%%
%%% </dl>
%%%
%%% @author Lovei Laszlo <lovei@inf.elte.hu>

-module(reflib_transform).
-vsn("$Rev: 12914 $"). %% "
-behaviour(gen_server).

%%% ============================================================================
%%% Exports

%% User interface exports
-export([do/3, reply/2, cancel/1, wait/0, status/0]).

%% Transformation interface exports
-export([question/1, touch/1, rename/2, validated_question/1]).

%-export([ask/1, answer/1]). % @depracated

%% Environment exports
-export([start_link/0]).

%% gen_server callback functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Callbacks
-export([error_text/2]).

% private
-export([test123/0]).
-export([get_running_queries/0, kill_query/1]).


-include("lib.hrl").
-include_lib("stdlib/include/ms_transform.hrl").


%%% ----------------------------------------------------------------------------
%% @todo extract

send_change(MCB,Change) ->
    (MCB#msg_cb.broadcast)(statusinfo,[{change,Change}]).

-define(Modified(File),
        {File, [{content,true}]}).
-define(Renamed(OldPath,NewPath),
        {OldPath, [{rename,NewPath}]}).


%%% ============================================================================
%%% Error texts

error_text(rename, [File,Err]) ->
    ["Error renaming ", File, ": ", Err];
error_text(save, [File]) ->
    ["File ", File, " could not be saved"];
error_text(save, []) ->
    ["error during saving results"];
error_text(exit, [Reason]) ->
    ["Transform exited: ", io_lib:print(Reason)];
error_text(error, [Error]) ->
    ["Transform internal error: ", io_lib:print(Error)];
error_text(tr_running, []) ->
    ["A transformation is already running.",
     " Close all interaction dialogs and try again later."];
error_text(no_tr_running, []) ->
    ["No transformation running, can't start embed transformation.",
     " Close all interaction dialogs and try again later."];
error_text(query_running, _Qs) ->
    ["A query is already running.",
     " Close all interaction dialogs and try again later."];
error_text(bad_client, []) ->
    ["Internal error: bad client request"];
error_text(bad_q, [Q]) ->
    ["Malformed question: ", io_lib:print(Q)].


%%% @type proplist() = [atom() | {atom(), term()}]. See the standard module
%%% `proplists'.

%%% @type node() = refcore_graph:node()

%%% ============================================================================
%%% Interface functions

%% @private
start_link() ->
    gen_server:start_link({local, ?TRANSFORM_SERVER}, ?MODULE, [], []).

get_running_queries() ->
    gen_server:call(?TRANSFORM_SERVER, {get_running_queries}, infinity).

kill_query(QId) ->
    gen_server:call(?TRANSFORM_SERVER, {kill_query, QId}, infinity).

%% @spec do(#msg_cb{}, atom(), reflib_args:arglist()) -> ok
%% @doc Start a transformation. Calls the transformation implemented by module
%% `Mod'. `Args' are passed as transformation parameters;
%% communication is done through `MCB'.
%% Query operations (function clustering and semantic queries) are run
%% in parallel, not embed transformations block until they are done.
do(MCB=#msg_cb{}, Mod, Args) when is_list(Args) ->
    Tab = ?RefChangeTab,
    dets:open_file(Tab, [{type, set}]),
    dets:delete_all_objects(Tab),
    dets:close(Tab),
    gen_server:call(?TRANSFORM_SERVER, {do, MCB, Mod, 
    {Args, continue_on_failure, {[],fun(A) -> A end},not_embed}}, infinity);
%    case gen_server:call(?TRANSFORM_SERVER, {do, MCB, Mod, Args}) of
%        ok -> ok;
%        {error, Error} -> erlang:error(Error, [Mod, Args])
%    end.

%% @spec do(embed, atom(), Data) -> ok
%% Data={reflib_args:arglist(), FailureStrategy,{Nodes, UpdateFuns}}
%% @doc Start an embed transformation. Calls the transformation implemented by module
%% `Mod'. `Args' are passed as transformation parameters;
%% communication is done through `MCB'.
%% Query operations (function clustering and semantic queries) are run
%% in parallel, not embed transformations block until they are done.
do(embed, Mod,{Args,_,_,_} =Data) when is_list(Args) ->
    gen_server:call(?TRANSFORM_SERVER, {do_embed, Mod, Data}, infinity).


%% @spec wait() -> none | {Return, term()}
%%       Return = result | abort | error | exit
%%
%% @doc Returns the result of the last transformation. If the transformation
%% is still running, waits for it to terminate. Only the first call will
%% return the result, subsequent calls will return `none'.
wait() ->
    gen_server:call(?TRANSFORM_SERVER, wait, infinity).

%% @spec question([proplist()]) -> term()
%% @doc Ask questions through the user interface and waits for the
%% answer. The answer to the question is returned.
question(PropLists) ->
    answer(ask(PropLists)).

%% @spec validated_question([proplist()]) -> term()
%% @doc Ask questions through the user interface and waits for the
%% answer. The answer to the question is returned. Every proplist
%% should contain a validator function, with the key `validator'.
validated_question(PropLists) ->
    Answers = answer(ask(PropLists)),
    case validate_answers(PropLists, Answers) of
        {[], ValidQaA}  ->
            {_Questions, VAnswers} = lists:unzip(ValidQaA),
            VAnswers;
        {ToAskAgain, All} ->
            InfoMsg = [{format, info},
                       {text, "You have not answered the following" ++
                            " questions correctly!"}],
            [_| ToReplace] = validated_question([InfoMsg|ToAskAgain]),
            Fun = fun({Q,A}, PropList) ->
                          lists:keyreplace(Q, 1, PropList, {Q, A})
                  end,
            [A || {_Q, A} <- lists:foldl(Fun, All, lists:zip(ToAskAgain,
                                                             ToReplace))]
    end.

%% @spec ask([proplist()]) -> term()
%% @doc Ask a question through the user interface. The question ID is returned.
ask(PropLists) ->
    error_handled(
      gen_server:call(?TRANSFORM_SERVER, {ask, self(), PropLists}, infinity),
      [PropLists]).

error_handled(Result,Args) ->
    case Result of
        {reply, Reply} ->
            Reply;
        {error, Error} ->
            erlang:error(Error, Args)
    end.

%% @spec answer(term()) -> term()
%%
%% @doc Wait for the answer to a question previously asked using `ask/1'.
%% The answer is returned.
answer(Id) ->
    case gen_server:call(?TRANSFORM_SERVER, {answer, self(), Id}, infinity) of
        cancel         -> throw(?RefErr0r(cancelled));
        {reply, Reply} -> Reply;
        {error, Error} -> erlang:error(Error, [Id])
    end.

%% @spec touch(node()) -> ok
%%
%% @doc Arranges for the file that contains `Node' to be saved after the
%% transformation.
touch(Node) ->
    gen_server:cast(?TRANSFORM_SERVER, {touch, Node}).

%% @spec rename(OldPath, NewPath) -> ok
%%
%% @doc Arranges for the file to be renamed after the transformation.
rename(OldPath, NewPath) ->
    gen_server:cast(?TRANSFORM_SERVER, {rename, {OldPath, NewPath}}).


%% @spec reply(integer(), term()) -> ok
%%
%% @doc Supply the answer to question number `Num'. This function is called
%% by the user interface bridge module.
reply(Num, Reply) ->
    error_handled(
      gen_server:call(?TRANSFORM_SERVER, {reply, Num, Reply}, infinity),
      [Num,Reply]).

%% @spec cancel(integer()) -> ok
%%
%% @doc Deny the answer to question number `Num', which results in the
%% abortion of the transformation. This function is called by the user
%% interface bridge module.
cancel(Num) ->
    error_handled(
      gen_server:call(?TRANSFORM_SERVER, {cancel, Num}, infinity),
      [Num]).

status() ->
    gen_server:call(?TRANSFORM_SERVER, {status}, infinity).


%%% ============================================================================
%%% Server callback functions

-type query_id() :: integer().

-record(qinfo, {
                pid :: pid(),
                from :: {pid(), reference()},
                mod :: atom(),
                text :: string(),
                % ETS table for question data
                question :: ets:tab(),
                % ETS table for answer data
                answer   :: ets:tab(),
                % ETS table for wait data
                wait     :: ets:tab(),
                query_id    :: query_id(),
                mcb         :: #msg_cb{}
               }).

-record(trinfo, {
                % The origin of the transformation request
                from :: {pid(), reference()},
                % The worker thread
                pid  :: pid(),
                % ETS table for question data
                question :: ets:tab(),
                % ETS table for answer data
                answer   :: ets:tab(),
                % ETS table for wait data
                wait     :: ets:tab(),
                % List of file nodes to be saved
                save     :: [any()],
                % List of file path pairs {OldPath, NewPath} to be renamed
                rename   :: [{string(), string()}],
                % #msg_cb{} message callbacks
                % mcb      :: [#msg_cb{}]
                mcb      :: #msg_cb{} %% ???
               }).

-record(state, {
                % Number of next question
                next     :: integer(),
                % PID of currently running trinfos or qinfos
                current = {qinfos, []} :: {qinfos, [#qinfo{}]} | {trinfos, [#trinfo{}]},
                % the id of the current transformation
                query_id = 1 :: query_id()
               }).


%% @private
init(_) ->
    process_flag(trap_exit, true),
    {ok, init_state()}.

%% @private
handle_call({get_running_queries}, _From, State = #state{current={qinfos, QInfos}}) ->
    Reply = [{QId, Pid, Mod, Args} || #qinfo{query_id=QId, pid=Pid, mod=Mod, text=Args} <- QInfos],
    {reply, Reply, State};
handle_call({get_running_queries}, _From, State) ->
    {reply, [], State};

handle_call({kill_query, QId}, _From, State = #state{current={qinfos, QInfos}}) ->
    case [Pid || #qinfo{query_id=QId2, pid=Pid} <- QInfos, QId2 == QId] of
        [QPid] ->
            exit(QPid, kill),
            KillResult = {abort, {user_abort, "Query aborted by user"}},
            gen_server:cast(?TRANSFORM_SERVER, {finish, QPid, KillResult, []}),
            {reply, ok, State};
        _ ->
            {reply, not_found, State}
    end;
handle_call({kill_query, _QId}, _From, State) ->
    {reply, not_found, State};

handle_call({do, MCB, Mod, Data}, From, State) ->
    handle_do(MCB, From, Mod, Data, State);


handle_call({do_embed, Mod, Data}, From, #state{current={trinfos, TrInfos}} = State) ->
    case TrInfos /= [] of
        true ->
            [CurrentTr|_]=TrInfos,
            CurrentTrPid=CurrentTr#trinfo.pid,
            {FromTrPid,_}=From,
            if
                FromTrPid == CurrentTrPid ->
                    MCB = CurrentTr#trinfo.mcb,
                    handle_do_embed(MCB, From, Mod, Data, State);
                true ->
                    {reply, {error, ?LocalError(tr_running,[])}, State}
            end;
        false ->
            {reply, {error, ?LocalError(no_tr_running,[])}, State}
    end;

handle_call({do_embed, _Mod, _Data}, _From, #state{current={qinfos, _QInfos}} = State) ->
    {reply, {error, ?LocalError(no_tr_running,[])}, State};


handle_call({ask, Pid, PropLists}, _From, #state{current={trinfos, TrInfos}} = State) ->
    case [Pid || #trinfo{pid=Pid2} <- TrInfos, Pid == Pid2] of
        [] ->
            {reply, {error, ?LocalError(bad_client, [])}, State};
        [Pid] ->
            {Rep, St} = handle_ask(Pid, PropLists, State),
            {reply, Rep, St}
    end;

handle_call({ask, Pid, PropLists}, _From, #state{current={qinfos, QInfos}} = State) ->
    case [Pid || #qinfo{pid=Pid2} <- QInfos, Pid == Pid2] of
        [] ->
            {reply, {error, ?LocalError(bad_client, [])}, State};
        [Pid] ->
            {Rep, St} = handle_ask(Pid, PropLists, State),
            {reply, Rep, St}
    end;

handle_call({answer, Pid, ID}, From, State) ->
    handle_answer(Pid, ID, From, State);

handle_call({pending, Pid}, _From, State) ->
    {reply, handle_pending(Pid, State), State};

handle_call({status}, _From, St) ->
    {reply, St#state.current, St};

handle_call({reply, Id, Reply}, _From, St) ->
    {reply, handle_reply(Id, Reply, St), St};

handle_call({cancel, Id}, _From, St) ->
    {reply, handle_cancel(Id, St), St}.

%% @private
handle_cast({touch, Node}, State) ->
    St = handle_touch(Node, State),
    {noreply, St};

handle_cast({rename, Data}, State) ->
    St = handle_rename(Data, State),
    {noreply, St};

handle_cast({finish, Pid, Result, _BadSmells}, #state{current={qinfos, QInfos}} = State) ->
    case lists:keyfind(Pid, #qinfo.pid, QInfos) of
        QInfo = #qinfo{from=From} ->
            del_qinfo(QInfo),
            QInfos2 = lists:keydelete(Pid, #qinfo.pid, QInfos),
            gen_server:reply(From, get_result_format(Result)),
            {noreply, State#state{current={qinfos, QInfos2}}};
        false ->
            {noreply, State}
    end;

handle_cast({finish, Pid, Res, BadSmells}, #state{current={trinfos, TrInfos}, next=Next} = State) ->
    case Res of
        {rollback_all,Result} -> 
            abort_all_tr(TrInfos, Result, BadSmells),
            {noreply, #state{next = Next}};
        Result ->
            case lists:keyfind(Pid, #trinfo.pid, TrInfos) of
                TrInfo = #trinfo{} ->
                    handle_tr_finished(Result, BadSmells, TrInfo),
                    TrInfos2 = lists:keydelete(Pid, #trinfo.pid, TrInfos),
                    case TrInfos2 of
                        [] -> 
                            {noreply, #state{next=Next}};
                        _ ->
                            {noreply, State#state{current={trinfos, TrInfos2}, next=Next}}
                    end;
                false ->
                    {noreply, State}
            end
    end.

abort_all_tr([TrInfo], Result, BadSmells) ->
    {_,Res}=Result,
    handle_tr_finished({error,Res}, BadSmells, TrInfo);

abort_all_tr(TrInfos, Result, BadSmells) ->
    [TrInfo|TrInfos2]=TrInfos,
    handle_tr_finished(Result, BadSmells, TrInfo),
    abort_all_tr(TrInfos2, Result, BadSmells).

%% @private
handle_info({'EXIT', Pid, Reason}, #state{current={trinfos, TrInfos}, next=Next} = State) ->
    case lists:keyfind(Pid, #trinfo.pid, TrInfos) of
        TrInfo = #trinfo{} ->
            handle_tr_finished({exit, Reason}, [], TrInfo),
            TrInfos2 = lists:keydelete(Pid, #trinfo.pid, TrInfos),
            case TrInfos2 of
                [] -> 
                    {noreply, #state{next=Next}};
                _ ->
                    {noreply, State#state{current={trinfos, TrInfos2}, next=Next}}
            end;
        false ->
            {noreply, State}
    end;
handle_info(_Msg, St) ->
    {noreply, St}.

%% @private
terminate(_R, #state{current={trinfos,TrInfos}}) when is_list(TrInfos) ->
    [exit(TrPid, kill) || #trinfo{pid=TrPid} <- TrInfos],
    ok;
terminate(_R, #state{current={qinfos, QInfos}}) when is_list(QInfos) ->
    [exit(QPid, kill) || #qinfo{pid=QPid} <- QInfos],
    ok;
terminate(_, _) ->
    ok.


%% @private
code_change(_Old, St, _New) -> St.

%%% ============================================================================
%%% Implementation
%%%
%%% There are three ETS tables that store question-related data:
%%%  question :: {Num::integer(), ID::term(), Owner::pid()}
%%%  answer   :: {{Owner::pid(), ID::term()}, Answer::term()}
%%%  wait     :: {{Owner::pid(), ID::term()}, Client::pid()}
%%%
%%% Questions are numbered by consecutive integers to make identification in
%%% UIs easier. Internally, the owner process and the specified or generated
%%% ID identifies a question.
%%%
%%% When an unavailable answer is requested, the request is stored in table
%%% `wait'. When the answer is received, this table is used to notify
%%% waiting clients. There is an extra `result' key for the client that
%%% waits for the result of the current transformation.

init_state() ->
    #state{next = 1}.

init_qinfo(From, TrPid, Mod, Args, CurrQId, MCB) ->
    #qinfo{
        from     = From,
        pid      = TrPid,
        mod      = Mod,
        text     = Args,
        question = ets:new(question, []),
        answer   = ets:new(answer, []),
        wait     = ets:new(wait, []),
        query_id = CurrQId,
        mcb      = MCB}.

init_trinfo(From, MCB, TrPid) ->
    #trinfo{
            from     = From,
            pid      = TrPid,
            question = ets:new(question, []),
            answer   = ets:new(answer, []),
            wait     = ets:new(wait, []),
            save     = [],
            rename   = [],
            mcb      = MCB}.

del_qinfo(#qinfo{question = QT, answer = AT, wait = WT}) ->
    ets:delete(QT),
    ets:delete(AT),
    ets:delete(WT).

del_trinfo(#trinfo{question = QT, answer = AT, wait = WT}) ->
    ets:delete(QT),
    ets:delete(AT),
    ets:delete(WT).

-define(QueryMod, refusr_sq).

%% Starts a new query (in parallel) if only queries are running.
%% Starts a new transformation if no queries are running, otherwise denies it.
handle_do(_MCB, _From, _Mod, _Data, #state{current={trinfos, TrInfos}} = St) 
    when is_list(TrInfos) ->
        {reply, {error, ?LocalError(tr_running,[])}, St};

handle_do(MCB, From, Mod, Data, #state{current={qinfos, QInfos}, query_id=QId} = St) ->
    IsQuery   = is_query_tr(Mod),
    BlockedTr = not IsQuery andalso QInfos /= [],
    {Args,_,_,_} = Data,
    case BlockedTr of
        true ->
            {reply, {error, ?LocalError(query_running,[])}, St};
        false ->
            CurrQId = QId + 1,
            TrPid = start_tr(MCB, Mod, Data, IsQuery),
            NewCurrent =
                case IsQuery of
                    false ->
                        {trinfos,[init_trinfo(From, MCB, TrPid)]};
                    true  ->
                        {qinfos, [init_qinfo(From, TrPid, Mod, Args, CurrQId, MCB) | QInfos]}
                end,
            case proplists:get_value(send_back_query_id,Args, false) of
                true -> (MCB#msg_cb.unicast)(query_id, CurrQId);
                false -> ok
            end,
    {noreply, St#state{current = NewCurrent, query_id=CurrQId}}
    end.

%% Starts a new embed transformation
handle_do_embed(MCB, From, Mod, Data, #state{current={trinfos, TrInfos}, query_id=QId} = St) when is_list(TrInfos) ->
    IsQuery = is_query_tr(Mod),
    CurrQId = QId + 1,
    TrPid = start_tr(MCB, Mod, Data, IsQuery),
    NewCurrent = {trinfos,[init_trinfo(From, MCB, TrPid) | TrInfos]},
    {noreply, St#state{current = NewCurrent, query_id=CurrQId}}.

%% Starts the transformation in a dedicated thread.
start_tr(MCB, Mod, Data, IsQuery) ->
    spawn_link(fun () -> do_transform(MCB, Mod, Data, IsQuery) end).

%% Returns whether the transformation is only querying the graph without modifications.
is_query_tr(Mod) ->
    QueryMods = [refusr_sq, reftr_apply_funcluster, refusr_metrics, refcl_main],
    lists:member(Mod, QueryMods).

%% Ask a question. Both queries and transformations are allowed.
handle_ask(Pid, PropLists, #state{current = Current, next=Next} = St) ->
    {MCB, QT} = get_mcb_qt(Current),
    P0 = [ begin
           [Format,TxtL] = ?MISC:pget([format,text], PropList),
           case {Format,TxtL} of
               {[F],[Txt=[_|_]]} when is_atom(F) ->
                   {true, [{text, lists:flatten(Txt)} |
                       proplists:delete(text, PropList)]};
               _ ->
                   {false, []}
           end
       end || PropList <- PropLists],
    OK = lists:all(fun({B,_})->B end, P0),
    case OK of
        true ->
            {_, P1} = lists:unzip(P0),
            ID = proplists:get_value(id, PropLists, make_ref()),
            ets:insert(QT, {Next, ID, Pid}),
            (MCB#msg_cb.unicast)(question, {Next, P1}),
            {{reply, ID}, St#state{next=Next+1}};
        false ->
            {{error, ?LocalError(bad_q, PropLists)}, St}
    end.

get_mcb_qt({trinfos, TrInfos}) ->
    [CurrentTr | _] = TrInfos,
    MCB = CurrentTr#trinfo.mcb,
    QT = CurrentTr#trinfo.question,
    {MCB, QT};
get_mcb_qt({qinfos, QInfos}) ->
    [{MCB, QT}] = [{MCB, QT} || #qinfo{mcb=MCB, question=QT} <- QInfos],
    {MCB, QT}.

%% Store a reply to a question.
handle_reply(Num, Reply, St) ->
    handle_reply2(Num, {reply, Reply}, St).

handle_cancel(Num, St) ->
    handle_reply2(Num, cancel, St).

%% Both queries and transformations are allowed.
handle_reply2(Num, Reply, #state{current=Current}) ->
    case lookup_question(Num, Current) of
        error ->
            {error,?LocalError(invalid_qid,[Num,Reply])};
        {ID, Owner, QT, AT, WT} ->
            ets:delete(QT, Num),
            ets:insert(AT, {{Owner, ID}, Reply}),
            [gen_server:reply(Client, Reply)
                ||  {_, Client} <- ets:lookup(WT, {Owner, ID})],
            ets:delete(WT, ID),
            {reply, noreply}
    end.

lookup_question(Num, {trinfos, TrInfos}) ->
    case [{QT, AT, WT, Lookup}
            || #trinfo{question=QT, answer=AT, wait=WT} <- TrInfos,
                Lookup <- [ets:lookup(QT, Num)],
                Lookup /= []] of
        [{QT, AT, WT, [{_, ID, Owner}]}] -> {ID, Owner, QT, AT, WT};
        _ -> error
    end;
lookup_question(Num, {qinfos, QInfos}) ->
    case [{QT, AT, WT, Lookup}
            || #qinfo{question=QT, answer=AT, wait=WT} <- QInfos,
                Lookup <- [ets:lookup(QT, Num)],
                Lookup /= []] of
        [{QT, AT, WT, [{_, ID, Owner}]}] -> {ID, Owner, QT, AT, WT};
        _ -> error
    end.


%% Return the answer for a question
handle_answer(Pid, ID, Client, St = #state{current=Current}) ->
    case lookup_answer(Pid, Current) of
        {AT, WT} ->
            case ets:lookup(AT, {Pid, ID}) of
                [{_, Reply}] ->
                    {reply, Reply, St};
                []           ->
                    ets:insert(WT, {{Pid, ID}, Client}),
                    {noreply, St}
            end;
        error ->
            {error, ?LocalError(bad_client, [])}
    end.


lookup_answer(Pid, {trinfos, TrInfos}) ->
    case [{AT, WT} || #trinfo{pid=Pid2, answer=AT, wait=WT} <- TrInfos, Pid == Pid2] of
        [{AT, WT}] -> {AT, WT};
        _ -> error
    end;
lookup_answer(Pid, {qinfos, QInfos}) ->
    case [{AT, WT} || #qinfo{pid=Pid2, answer=AT, wait=WT} <- QInfos, Pid == Pid2] of
        [{AT, WT}] -> {AT, WT};
        _ -> error
    end.



%% Return the list of question IDs waiting for an answer
handle_pending(Pid, #state{current={qinfos, QInfos}}) ->
    case lists:keyfind(Pid, #qinfo.pid, QInfos) of
        #qinfo{question=QT} ->
            pend(Pid, QT);
        false ->
            error
    end;

handle_pending(Pid, #state{current={trinfos, TrInfos}}) ->
        case lists:keyfind(Pid, #trinfo.pid, TrInfos) of
            false ->
                error; 
            #trinfo{question=QT} ->
                pend(Pid, QT)
        end.


pend(Pid, QT) ->
    ets:select(QT, ets:fun2ms(fun({_, Id, P}) when P =:= Pid -> Id end)).

%% Remembers the files that have been changed.
%% The list `save' is not unique until the point of saving.
%% Only transformations may attempt this operations, not queries.
handle_touch(Node, #state{current={trinfos, TrInfos}} = St) ->
    [TrInfo=#trinfo{save=Save}|TrInfos2]=TrInfos,
    St#state{current={trinfos,[TrInfo#trinfo{save=?Syn:get_file(Node)++Save}|TrInfos2]}}.

%% Remembers the files that have to be renamed.
%% The list `rename' is not unique until the point of saving.
%% Only transformations may attempt this operations, not queries.
handle_rename(Data,#state{current={trinfos, TrInfos}} = St) ->
    [TrInfo=#trinfo{rename=Rename}|TrInfos2]=TrInfos,
    St#state{current={trinfos, [TrInfo#trinfo{rename=[Data|Rename]}|TrInfos2]}}.


get_result_format({result, R}           ) -> {result, [{result,R}]};
get_result_format({exit, Reason}        ) -> {error, ?LocalError(exit, [Reason])};
get_result_format({abort, RefError}     ) -> {abort, RefError};
get_result_format({error, Error={_,_,_}}) -> {error, Error};
get_result_format({error, Error}        ) -> {error, ?LocalError(error, [Error])}.

%% Closes a transformation, replies to the waiting client.
%% todo Already renamed/saved files have to be restored as well.
handle_tr_finished(Res, BadSmells,
                   TrInfo = #trinfo{from=From, mcb=MCB,
                                    save=Saves, rename=Renames}) ->
    Result =
        case get_result_format(Res) of
            {result, Res4} ->
                [{result,{Res2,_Nodes}}]=Res4,
                Res3 = do_saves_renames(Renames, Saves, MCB),
		        if
                    erlang:is_list(Res2) == true ->
                        {result, [{result,Res2}] ++ Res3, BadSmells};
                    erlang:is_list(Res2) == false ->
                        {result, [{result,[Res2]}] ++ Res3, BadSmells}
                end;
            ResultFmt -> ResultFmt
        end,
    gen_server:reply(From, Result),
    del_trinfo(TrInfo).

do_saves_renames(Renames, Saves, MCB) ->
    try
        USaves = lists:usort(Saves),
        [rename_file(MCB, File) || File  <- lists:usort(Renames)],
        [save_file(MCB, Saved)  || Saved <- USaves],
        SavedPaths = [?File:path(Saved)      || Saved <- USaves],
        [{renamed,Renames},{saved,SavedPaths}]
    catch
        throw:Error -> {error, Error}
    end.


rename_file(MCB=#msg_cb{}, {OldPath, NewPath}) ->
    case file:rename(OldPath, NewPath) of
        ok ->
            send_change(MCB, [?Renamed(OldPath,NewPath)]);
        {error, Reason} ->
            throw(?LocalError(rename, [OldPath, file:format_error(Reason)]))
    end.

save_file(MCB=#msg_cb{}, File) ->
    try
        #file{path=Path} = ?Graph:data(File),
        try
            ok = ?FileMan:save_file(File),
            send_change(MCB, [?Modified(Path)])
        catch
            error:Reason ->
                error_logger:error_msg(
                  "Runtime error while saving file ~p~n"
                  "  Reason: ~p~n"
                  "  Stack: ~p~n",
                  [File, Reason, erlang:get_stacktrace()]),
                throw(?LocalError(save, [Path]))
        end
    catch
        _:_ ->
            error_logger:error_msg("Saving invalid file node ~p~n", [File]),
            throw(?LocalErr0r(save))
    end.

%%% ----------------------------------------------------------------------------
%%% Transformation process

do_transform(_MCB, Mod, {Args,_,{Nodes,UpdateFun},not_embed}, IsQuery) ->
    EndResult =
        try
            Transform = Mod:prepare(Args),
            PendIds   = gen_server:call(?TRANSFORM_SERVER, {pending, self()}, infinity),
            case PendIds of
                error ->
                    {error,{embed_tr_rollbacked,[]}};
                _ ->
                    [answer(Id) || Id <- PendIds],
                    Result    = runs(Transform, first),
                    case IsQuery of
                        true -> 
                            {result, Result};
                        false ->
                            {result, {Result, UpdateFun(Nodes)}}
                    end
            end
        catch
            throw:Error ->
                {abort,Error};
            error:Error ->
                Trace = erlang:get_stacktrace(),
                log_error(Mod, Args, Error, Trace),
                {error, {Error, Trace}}
        after
            ?ESG:finalize()
        end,
    BadSmells =
        case IsQuery of
            true -> [];
            false -> refanal_metrics_helper:bad_smells_after_changes(Mod, Args)
        end,
    gen_server:cast(?TRANSFORM_SERVER, {finish, self(), EndResult, BadSmells});

do_transform(_MCB, Mod, {Args,FailureStrategy,{Nodes,UpdateFun},embed}, IsQuery) ->
    EndResult =
        try
            Transform = Mod:prepare(Args),
            PendIds   = gen_server:call(?TRANSFORM_SERVER, {pending, self()}, infinity),
            case PendIds of
                error ->
                    {error,{embed_tr_rollbacked,[]}};
                _ ->
                    [answer(Id) || Id <- PendIds],
                    Result    = runs(Transform, first),
                    case IsQuery of
                        true -> 
                            {result, Result};
                        false ->
                            {result, {Result, UpdateFun(Nodes)}}
                    end
            end
        catch
            throw:Error when FailureStrategy == continue_on_failure ->
                {abort,Error};
            throw:Error when FailureStrategy == rollback_on_failure ->
                {rollback_all,{abort,Error}};
            error:Error when FailureStrategy == continue_on_failure ->
                Trace = erlang:get_stacktrace(),
                {error, {Error, Trace}};
            error:Error when FailureStrategy == rollback_on_failure ->
                Trace = erlang:get_stacktrace(),
                {rollback_all,{error, {Error, Trace}}}
        after
            ?ESG:finalize()
        end,
    BadSmells =
        case IsQuery of
            true -> [];
            false -> refanal_metrics_helper:bad_smells_after_changes(Mod, Args)
        end,
    gen_server:cast(?TRANSFORM_SERVER, {finish, self(), EndResult, BadSmells}).


runs([], first)          -> [];
runs([], {arg, Arg})     -> Arg;
runs([Head | Tail], Arg) -> runs(Tail, {arg, runp(Head, Arg)});
runs(Fun, _)             -> runt(Fun).

runp([], _)            -> [];
runp([Head|Tail], Arg) -> [runp(Head, Arg) | runp(Tail, Arg)];
runp(Fun, first)       -> runt(Fun);
runp(Fun, {arg, Arg})  -> runt(fun() -> Fun(Arg) end).

runt(Fun) ->
    R = try
            Fun()
        after
            ?ESG:finalize()
        end,
    case is_list(R) andalso lists:all(fun is_function/1, R) of
        true  -> runs(R, first);
        false -> R
    end.

log_error(Mod, Args, Error, Trace) ->
    error_logger:error_msg(
      "** A runtime error occurred in ~p~n"
      "** Arguments: ~p~n"
      "** Reason: ~p~n"
      "** Stack:~n     ~p~n",
      [Mod, Args, Error, Trace]).

%%% ----------------------------------------------------------------------------
%%% Validation
%%% @todo

%% @type ui_question() = proplist()
%% @type ui_answer() = proplist()
%% @type ui_questions() = [ui_question()]
%% @type ui_answers() = [ui_answer()]

test123() ->
    check_result([],[]).

check_result(Questions,Answers) ->
   Valid = validate(Questions,Answers),
   case lists:all(fun(X)->X end,Valid) of
       true -> true;
       false ->
           NewQ = insert_defaults(Questions,Answers,Valid),
           resend_questions(NewQ)
   end.

resend_questions(_) -> throw(todo).

%% currently unused function clause
%% insert_defaults([Q|Qs],[A|As],[V|Vs]) ->
%%     NQ = case V of
%%              true  -> Q ++ [{value,A}];
%%              false -> Q
%%          end,
%%     [NQ | insert_defaults(Qs,As,Vs)];
insert_defaults([],[],_) -> %% _ :: [boolean()] ???
    [].

%% @doc
%% Constraint: length(Questions)==length(Answers)
%% @spec (ui_questions(), ui_answers()) -> [boolean()]
validate(Questions, Answers) ->
   lists:zipwith(fun validate1_cancel/2, Questions, Answers).

validate1_cancel(Question, Answer) ->
   (Answer==cancel) orelse validate1(Question,Answer).

validate1(Question, Answer) ->
   [Type, Validator] = ?MISC:pgetu([type, {validator,none}], Question),
   Typed = case Type of
       yesno ->
           is_boolean(Answer);
       string ->
           is_list(Answer);
       _ ->
           throw(?RefError(valtype_unknown,[Type]))
   end,
   Typed andalso validator(Type,Validator,Answer).

validator(_,none,_) -> true;
validator(T,V,_) ->
           throw(?RefError(valsubtype_unknown,[T,V])).


validate_answers(Questions, Answers) ->
    Result = evaluate_answers(lists:zip(Questions, Answers)),
    Fun = fun({false, {Q, A}}, {ToAsk, All}) ->
                  {[Q | ToAsk], [{Q, A} | All]};
             ({true, {Q, A}}, {ToAsk, All}) ->
                  {ToAsk, [{Q, A} | All]}
          end,
    
    {TA, All} = lists:foldl(Fun, {[], []}, Result),
    {lists:reverse(TA), lists:reverse(All)}.
    

evaluate_answers([]) ->
    [];
evaluate_answers([{Q, A} | QAs]) ->
    case proplists:get_value(format, Q) of
        info ->
            [{true, {Q, A}} | evaluate_answers(QAs)];
        _ ->
            case ?MISC:pgetu([{validator, undefined}], Q) of
                [Fun] when is_function(Fun, 1) ->
                    case Fun(A) of
                        {true, Ans}  ->
                            [{true, {Q, Ans}} | evaluate_answers(QAs)];
                        {false, _} ->
                            [{false, {Q, A}} | evaluate_answers(QAs)]
                    end;
                _ ->
                    [{true, {Q, A}} | evaluate_answers(QAs)]
            end
    end.
