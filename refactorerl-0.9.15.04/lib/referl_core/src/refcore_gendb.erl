
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

%%% @doc Graph storage server. This is a mnesia based implementation of
%%% the semantical graph server interface.
%%%
%%% @author Andras Nemeth <andras.b.nemeth@ericsson.com>

-module(refcore_gendb).
-vsn("$Rev: 8043 $"). %% "

%%% ---------------------------------------------------------------------------
%%% Records

-record(refdb,{
          state = []            :: any(),
          dbmod                 :: atom(),
          has_schema = false    :: boolean(),
          schema                :: tuple(),
          schema_list           :: list(),
          name                  :: atom(),
          number_of_runners = 0 :: integer()
        }).

%%% ---------------------------------------------------------------------------
%%% Include files

-include("core.hrl").
-include("refcore_gendb.hrl").

%%% ---------------------------------------------------------------------------
%%% Exported Functions

-export([system_continue/3,
         system_terminate/4,
         system_code_change/4,
         handle_dbg/3]).

% we are cheating here. The user doesn't let to decide the name of refdb
-export([start_link/0,
         init_it/6,
         sync_graph_op/1,
         sync_graph_op/2,
         sync_graph_op/3,
         async_graph_op/1,
         async_graph_op/2,
         async_graph_op/3,
         result/2,
         get_state/0,
         get_dbmod/0
        ]).

% exported internal functions
-export([handle_single_req/4,
         save_envs/1,
         restore_envs/1,
         handle_path/2,
         handle_path_here/3]).

-type class()     :: atom().
-type tag()       :: atom().
-type id()        :: integer().
-type attribute() :: any().
-type gnode()     :: {?NODETAG, Class::class(), ID::id()}.
-type groot()     :: {?NODETAG, root, ID::id()}.
-type data()      :: tuple().

-type pathexpr() :: [pathelem()].
-type pathelem() :: step() | 
		    {step(), filter()}.
-type step()     :: tag() | 
		    {tag(), 'back'}.
-type filter()   :: 'last' |
		    id() |
		    {id(), 'last'} |
		    {id(), id()} | 
		    {'not', filter()} |
		    {filter(), 'and', filter()} |
		    {filter(), 'or', filter()} |
		    {attribute(), operator(), any()}.
-type operator() :: '>' | '>=' | '<' | '=<' | '==' | '/='.

% see 'http://www.erlang.org/download/otp_src_R15B.readme'
% OTP-9621  Add '-callback' attributes in stdlib\'s behaviours
-ifdef(leastR15B).
-callback init(Args::any()) ->
    {ok, State::any()} | 
    {stop, Reason::any()}.
%% -callback is_path_supported() ->
%%     boolean().
-callback is_parallel_exec_supported() ->
    boolean().
-callback create_class(Class::atom()) ->
    any().
-callback erase_class(Class::atom()) ->
    any().
-callback code_change(OldVsn::string(), State::any()) ->
    any().
-callback terminate(Reason::any(), State::any()) ->
    any().

-callback erase_nodes() ->
    any().
-callback root() ->
    {ok, Root::groot()}.
-callback create(data()) ->
    {ok, Node::gnode()}.
-callback create_prot(data()) ->
    {ok, Node::gnode()}.
-callback update(Node::gnode(), Data::data()) ->
    {ok, Node::gnode()} | {error, {bad_class, Class::class()}}.
-callback delete(Node::gnode()) ->
    ok.

-callback mklink(From::gnode(), Tag::tag(), To::gnode()) ->
    ok | 
    {error, {bad_node, From::gnode()}} | 
    {error, {bad_nodes, From::gnode(), To::gnode()}} | 
    {error, bad_link} |
    {error, {bad_link, From::gnode(), Tag::tag(), To::gnode()}}.
-callback mklink_prot(From::gnode(), Tag::tag(), To::gnode()) ->
    ok | 
    {error, {bad_node, From::gnode()}} | 
    {error, {bad_nodes, From::gnode(), To::gnode()}} | 
    {error, bad_link} |
    {error, {bad_link, From::gnode(), Tag::tag(), To::gnode()}}.
-callback rmlink(From::gnode(), Tag::tag(), To::gnode()) ->
    ok | 
    {error, not_exists} | 
    {error, bad_link} |
    {error, {bad_link, From::gnode(), Tag::tag(), To::gnode()}}.
-callback links(Node::gnode()) ->
    {ok, [{Tag::tag(), [Node::gnode()]}]}. % or use the list(...) ?
-callback index(From::gnode(), Tag::tag(), To::gnode()) ->
    {ok, none} |
    {ok, Id::id()} | 
    {error, {bad_node, From::gnode()}} | 
    {error, {bad_nodes, From::gnode(), To::gnode()}} | 
    {error, bad_link} |
    {error, {bad_link, From::gnode(), Tag::tag(), To::gnode()}}.
-callback path(Node::gnode(), Path::pathexpr()) ->
    {ok, [Node::gnode()]} |
    {ok, []} |
    {error, Reason::any()}.

%% FIXME: set_prop, get_prop, ...?

%% optional callbacks, but the stub is required
-callback remove_garbage() ->
    any().
-callback back_links(Node::gnode()) ->
    any().

%% database management facilities
-callback backup() ->
    {ok, {ok, BackupName::atom()}}.
-callback backup(CommitLog::any()) ->
    {ok, {ok, BackupIdName::atom()}}.
-callback save(FileName::before_transformation | string()) ->
    {ok, any()}.
-callback restore(BackupId::id()) ->
    ok |
    any().
-callback ls_backups() ->
    {ok, [BuName::atom()]}.
-callback backup_info(BackupId::id()) ->
    {ok, any()}.
-callback undo() ->
    {ok, ok | invalid_checkpoint_number}.
-callback redo() ->
    {ok, ok | invalid_checkpoint_number | not_implemented}.
-callback clean() ->
    any().

%% graph management
-callback create_graph(Name::atom()) ->
    {ok, ok} | 
    {ok, {graph_already_exists, Name::atom()}} |
    {ok, MnesiaError::string()}.
-callback rename_graph(OldName::atom(), NewName::atom()) ->
    ok | 
    {ok, {graph_rename_failed, [any()]}} |
    {ok, {graph_not_exist, OldName::atom()}} |
    {ok, {graph_not_exist, NewName::atom()}} | 
    {ok, MnesiaError::string()}.
-callback ls_graphs() ->
    {ok, [Name::atom()]} | 
    {ok, MnesiaError::string()}.
-callback delete_graph(Name::atom()) ->
    ok |
    {ok, {graph_not_exist, Name::atom()}} |
    {ok, {graph_is_in_use, Name::atom()}} |
    {ok, MnesiaError::string()}.
-callback delete_all_graphs() ->
    any().

-else.
-export([behaviour_info/1]).
-endif.
%%% ---------------------------------------------------------------------------
%%% Internal Macros
% for async queries. after this macro, the number of runners will be incremented
-define(SpawnRunner(Starter, Extra, DbMod, Query, State),
        spawn_link(fun() ->
                           ?MODULE:handle_single_req(Extra,
                                                     DbMod,
                                                     Query,
                                                     State),
                           Starter ! query_done
                   end)
       ).

% for synchronized queries. after this the number of runners will be 0.
% all queries which don't change the graph will run in parallel, others
% have to wait until noone uses it -> runners are finished.
% this is design base here.
-define(DoAfterRunners(Qs, Extra, DbMod, Query, State),
        begin
            [receive query_done -> ok end || _ <- lists:seq(1, Qs)],
            ?MODULE:handle_single_req(Extra, DbMod, Query, State)
        end
       ).

%%% ---------------------------------------------------------------------------
%%% API Functions
%%%

-ifdef(priorR15B).
 behaviour_info(callbacks) ->
     Spec = refcore_graph:module_info(exports),
     Impl = lists:filter(fun({F,_A}) ->
                                 SF = atom_to_list(F),
                                 string:str(SF, "schema") =:= 0 andalso
                                 string:str(SF, "class") =:= 0 andalso
                                 string:str(SF, "is_gnode") =:= 0 andalso
                                 string:str(SF, "save_envs") =:= 0 andalso
                                 string:str(SF, "get_dbmod") =:= 0
                         end, Spec),
     Impl ++
     [{is_path_supported, 0},
      {is_parallel_exec_supported, 0},
      {create_class, 1},
      {erase_class, 1},
      {init, 1},
      {code_change, 2},
      {terminate, 2}];

 behaviour_info(_Else) ->
     undefined.
-endif.

%%
%% function call flow:
%%
%% OP1 -> sync_op                                                                         -> DbMod:concrete_opretaion1
%% OP2 -> sync_op  -> sync_wrapper  -> loop                    -> execute_graph_op(sync)  -> DbMod:concrete_operation2
%% OP3 -> async_op -> async_wrapper -> loop -> process_message -> execute_graph_op(async) -> DbMod:concrete_operation3
%% OP4 -> async_op                                                                        -> DbMod:concrete_operation4
%% ...
%%
%% - The names of the concrete functions in the implemnetation module will be calculated
%%   dynamically by the generic db module, to provide flexibility.
%%
%% - State transitions will be forbidden, except the schema-related functions.
%%   In my opinion it is preferable for a query operation to not affect any
%%   other of it's mates.
%%   FIXME: Allow it for non-query operations?
%%   Since by default the query related operations will run in parallel,
%%   it is impossible for those to transform the state
%%
%% - Is it good to hide the schema system from the user modules? I think if a user
%%   wants to change the schema, then it should ask it directly and not through ESG
%%
%% TODO: check at startup, if the chosen db module exists, and if it does, also check the
%%       exports if everything is there

start_link() ->
    DbMod = get_init_arg(dbmod),
    DbArgs = get_init_arg(dbargs),
    start_link({local, ?RefDb}, DbMod, DbArgs).

%% rather die here if dbmod or dbargs comes in a bad format, then to
%% let 'undefined' values make a bigger problem
get_init_arg(Arg) ->
    {ok, AStr} = init:get_argument(Arg),
    {ok, Tokens, _} = erl_scan:string(lists:flatten(AStr) ++ "."),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.

start_link(Name, DbMod, Args) when is_list(Args) ->
    gen:start(?MODULE, link, Name, DbMod, Args, []).

init_it(Starter, Parent, Name, DbMod, Args, Options) ->
    process_flag(trap_exit, true),
    put(dbmod, DbMod),
    ets:new(?CLASS_SCHEMA, [bag, named_table, protected, {keypos, 2}]),
    ets:new(?CLASS_TARGETS, [bag, named_table, protected, {keypos, 2}]),
    State =
        case catch DbMod:init(Args) of
            {ok, S} ->
                S;
            {stop, {no_right, Message}} ->
                unregister_name(Name),
                io:format(Message),
                erlang:halt();

            {X, Reason} when X =:= stop;
                             X =:= 'EXIT'->
                unregister_name(Name),
                proc_lib:init_ack(Starter, {error, Reason}),
                exit(Reason);
            Other ->
                unregister_name(Name),
                Error = {unexpected_return_value, Other},
                proc_lib:init_ack(Starter, {error, Error}),
                exit(Error)
        end,
    RefDbState = #refdb{state = State,
                        dbmod = DbMod,
                        name = Name},
    Deb = debug_options(Options),
    %restore_envs(DbMod),
    proc_lib:init_ack(Starter, {ok, self()}),
    loop(Parent, Deb, RefDbState).

debug_options(Options) ->
    DebugOpts =
        case lists:keysearch(debug, 1, Options) of
            {value, {debug, DebOpts}} ->
                DebOpts;
            _ ->
                []
        end,
    sys:debug_options(DebugOpts).

% implementation #2
sync_graph_op(Q) ->
    sync_graph_op(?RefDb, Q, infinity).

sync_graph_op(Q, Timeout) ->
    sync_graph_op(?RefDb, Q, Timeout).

sync_graph_op(Destination, Q, Timeout)
  when is_pid(Destination) orelse
       is_atom(Destination) ->
    sync_wrapper(Destination, Q, Timeout);
sync_graph_op(_, _, _) ->
    throw (refcore_gendb_invalid_pid_or_name).

async_graph_op(Q) ->
    async_graph_op(?RefDb, Q, infinity).

async_graph_op(Q, Timeout) ->
    async_graph_op(?RefDb, Q, Timeout).

async_graph_op(Destination, Q, Timeout)
  when is_pid(Destination) orelse
       is_atom(Destination) ->
    async_wrapper(Destination, Q, Timeout);
async_graph_op(_, _, _) ->
    throw(refcore_gendb_invalid_pid_or_name).

result(Result, Extra) ->
    return_result(Result, Extra).

get_state() ->
    whereis(?RefDb) ! {'graph_op', self(), get_state}.

get_dbmod() ->
    whereis(?RefDb) ! {'graph_op', self(), get_dbmod},
    receive
        {dbmod, Result} ->
            Result
    end.

%%% ---------------------------------------------------------------------------
%%% Callbacks for system messages

system_event(Msg, From, Parent, Mod, Deb, Misc) ->
    sys:handle_system_msg(Msg, From, Parent, Mod, Deb, Misc).

system_continue(Parent, Deb, RefDbState) ->
    loop(Parent, Deb, RefDbState).

system_terminate(Reason, _Parent, _Deb, RefDbState) ->
    terminate(Reason, RefDbState).

system_code_change(#refdb{dbmod=DbMod, state = State} = RefDbState, _Module, OldVsn, Extra) ->
    case catch DbMod:code_change(OldVsn, State, Extra) of
        {ok, NewState} ->
            {ok, RefDbState#refdb{state = NewState}};
        Other ->
            Other
    end.

%%% ---------------------------------------------------------------------------
%%% Local Functions

% implementation #2
sync_wrapper(Dest, Q, Timeout) ->
    general_wrapper(Dest, ?SYNC_OP, Q, Timeout).

async_wrapper(Dest, Q, Timeout) ->
    general_wrapper(Dest, ?ASYNC_OP, Q, Timeout).

general_wrapper(Dest, Tag, Q, Timeout) ->
    %% this is where gen:call can die. If it dies, something is terribly wrong
    try
        {ok, Res} = gen:call(Dest, Tag, {request, Q}, Timeout),
        Res
    catch
        {'EXIT', Reason} ->
            exit({Reason, {?MODULE, general_wrapper, [Dest, Tag, Q, Timeout]}})
    end.

loop(Parent, Deb, RefDbState) ->
    Message =
        receive M -> M
        end,
    {NewDeb, NewRefDbState} = process_message(Message, Parent, Deb, RefDbState),
    loop(Parent, NewDeb, NewRefDbState).% RefDbState#refdb{state = NewState}).

%% the parent (supervisor) suddenly died. we have to follow him to the grave
process_message({'EXIT', Parent, Reason}, Parent, _Deb, _RefDbState) ->
    exit(Reason); % suicide O.o
%% This will be an 'EXIT' from a runner since there is no other linked process
%% here besides the supervisor
process_message({'EXIT', Pid, Reason}, _Parent, Deb, #refdb{number_of_runners = Qs
                              } = RefDbState) ->
    % exit from a linked runner
    % if it's unexpected, log it as a bug
    % FIXME: are you sure to continue here?
    case Reason of
        normal -> {Deb, RefDbState};
        Reason -> report_error(exit_from_parallel_query,
                               process_info(self(), current_function),
                               Reason,
                               process_info(Pid, current_function),
                               [{process_info, process_info(Pid)},
                                {state, RefDbState}]),
                  {?DEBUG(Deb, {exit_from_runner, Pid}),
                   RefDbState#refdb{number_of_runners = dec_qs(Qs)}}
    end;
process_message(query_done, _Parent, Deb, #refdb{number_of_runners = Qs
                                                 } = RefDbState) ->
    {?DEBUG(Deb, query_done), RefDbState#refdb{number_of_runners = dec_qs(Qs)}};
% now result messages doesn't come, since we don't use send it back to ourselves
% to dispatch back the results, those will go directly to the caller
process_message({Tag, From, Msg}, Parent, Deb, RefDbState) ->
    %?d(Msg),
    case Msg of
        get_state ->
            print_state(RefDbState),
            {?DEBUG(Deb, [get_state]), RefDbState};
        get_dbmod ->
            send_dbmod(From, RefDbState),
            {?DEBUG(Deb, [get_dbmod]), RefDbState};
        {request, [schema, Schema]} ->
            {?DEBUG(Deb, [schema, Schema]), handle_schema(Schema, From, RefDbState)};
        {request, [get_schema]} ->
            {?DEBUG(Deb, [get_schema]), handle_get_schema(From, RefDbState)};
        {request, [reset_schema]} ->
            {?DEBUG(Deb, [reset_schema]), handle_reset_schema(From, RefDbState)};
        {request, [save_envs]} ->
            {?DEBUG(Deb, [save_envs]), handle_save_envs(From, RefDbState)};
%        {request, Query} when hd(Query) =:= path ->
%            {?DEBUG(Deb, Msg), handle_path(Tag, Query, From, RefDbState)};
        {request, Query} ->
            {?DEBUG(Deb, Msg), execute_graph_op(Tag, Query, From, RefDbState)};
        {System, From, Msg} ->
            Deb1 = ?DEBUG(Deb, {system_msg, System, From, Msg}),
            system_event(Msg, From, Parent, ?MODULE, Deb, RefDbState),
            {Deb1, RefDbState}; %% FIXME: will this work??
        shutdown ->
            system_terminate(shutdown, Parent, Deb, RefDbState)
    end.

dec_qs(Qs) when Qs =:= 0 ->
    % oops, something went wrong
    throw(extra_query_done_received);
dec_qs(Qs) ->
    Qs - 1.

report_error(What, Where, Reason, MFA, UserData) ->
    ErrorReport = [{what, What},
                   {where, Where},
                   {reason, Reason},
                   {mfa, MFA},
                   {user_data, UserData}],
    error_logger:error_report(ErrorReport).

%% IMPLEMENTATION #1
%execute_graph_op('$graph_op', Query, Extra, #refdb{dbmod = DbMod,
%                                                   state = State,
%                                                   number_of_runners = Qs
%                                                  } = RefDbState) ->
%    case is_query_req(Query) of
%        true ->
%            case DbMod:is_parallel_exec_supported() of
%                true ->
%                    NewState = ?SpawnRunner(self(), Extra, DbMod, Query, State),
%                    RefDState#refdb{state = NewState, number_of_runners = Qs + 1};
%                false ->
%                    NewState = handle_single_req(Extra, DbMod, Query, State),
%                    RefDbState#refdb{state = NewState}
%            end;
%        false ->
%            case catch ?DoAfterRunners(Qs, Extra, DbMod, Query, State) of
                % DoAfterRunners will always return a result since it is synchronized
                %% {noresult, NewState} ->
                %%     RefDbState#refdb{state = NewState,
                %%                            number_of_runners = 0};

                % >.< ...
%                {ok, NewState} ->
%                    return_result(Result, Extra),
%                    RefDbState#refdb{state = NewState,
%                                     number_of_runners = 0};
%                {stop, Reason, State} ->
%                    (catch terminate(Reason, RefDbState)),
%                    return_result(Result, Extra),
%                    exit({stop, Reason});
%                Other ->
%                    Reason = {unexpected_return_value, Other},
%                    (catch terminate(Reason, RefDbState)),
%                    exit(Reason)
%            end
%    end.

execute_graph_op(_, Q, _, #refdb{has_schema = HasSchema} = State)
  when HasSchema =:= false orelse HasSchema =:= schema_error ->
    exit([{no_schema, HasSchema}, {state, State}, {q, Q}]);
execute_graph_op(?SYNC_OP, Query, Extra, #refdb{dbmod = DbMod,
                                                  state = State,
                                                  number_of_runners = Qs
                                                 } = RefDbState) ->
    %% State transitions are allowed
    %?d(Qs),
    NewState =
        case DbMod:is_parallel_exec_supported() of
            true ->
                ?DoAfterRunners(Qs, Extra, DbMod, Query, State);
            false ->
                handle_single_req(Extra, DbMod, Query, State)
        end,
    RefDbState#refdb{state = NewState, number_of_runners = 0};
execute_graph_op(?ASYNC_OP, Query, Extra, #refdb{dbmod = DbMod,
                                                   state = State,
                                                   number_of_runners = Qs
                                                  } = RefDbState) ->
    %% State transition is not allowed here
    NewQs =
        case DbMod:is_parallel_exec_supported() of
            true ->
                Self = self(),
                ?SpawnRunner(Self, Extra, DbMod, Query, State),
                Qs + 1;
            false ->
                handle_single_req(Extra, DbMod, Query, State),
                Qs
    end,
    RefDbState#refdb{number_of_runners = NewQs}.

handle_single_req(Extra, DbMod, Query, State) ->
    case handle_req(DbMod, Query, State) of
        {ok, _} = Answer ->
            refcore_gendb:result(Answer, Extra),
            ok;
        ok = Answer ->
            refcore_gendb:result(Answer, Extra),
            ok;
        %% {ok, Answer, State} ->
        %%     refcore_gendb:result(Answer, Extra),
        %%     {ok, State};
        {error, _} = Error ->
            %% report_error(maybe_graph_error,
            %%                  process_info(self(), current_function),
            %%                  Error,
            %%                  {?MODULE, handle_single_req, [Extra, DbMod, Query, State]},
            %%                  []),
            refcore_gendb:result(Error, Extra), % notify caller
            ok
    end.

handle_req(DbMod, [Fun | Args], _State) ->
    apply(DbMod, Fun, Args).

return_result(Result, {To, Tag}) ->
    catch To ! {Tag, Result}.

handle_dbg(Io, Event, Name) ->
    io:format(Io, "~p event: ~p~n", [Name, Event]).

terminate(Reason, #refdb{name = Name, dbmod = DbMod, state = State}) ->
    unregister_name(Name),
    DbMod:terminate(Reason, State),
    ok.
    %exit(Reason).

unregister_name({local, Name}) ->
    erlang:unregister(Name);
unregister_name({global, Name}) ->
    global:unregister_name(Name);
unregister_name(Name) when is_atom(Name)->
    try
        erlang:unregister(Name)
    catch
        error:badarg -> global:unregister_name(Name)
    end.

print_state(RefDbState) ->
    ?d(RefDbState).

send_dbmod(To, #refdb{dbmod = DBMod})->
    To ! {dbmod,DBMod}.

%%% ---------------------------------------------------------------------------
%%% Common functionality provided by refcore_gendb


%%% ===========================================================================
%%% SCHEMA

handle_reset_schema(From, St = #refdb{dbmod = DbMod,
                                      schema_list = Schema}) ->
    save_envs(DbMod),
    %Schema = get_schema_from_mnesia(),
    propagate_reset_to_servers(),
    ResetSt = reset_schema(Schema, From, St),
    %delete_all(DbMod, Schema),
    %{ok, InitSt} = DbMod:init(dummy_param),
    %ResetSt = handle_schema(Schema, From, St#refdb{has_schema = false,
    %                                         state = InitSt}),
    restore_envs(DbMod),
    ResetSt.

%% Notify all servers about the pending reset that require it.
propagate_reset_to_servers() ->
    refcore_funprop:reset().

%% Resets the schema to the one given in the parameter.
reset_schema(Schema, From, St = #refdb{dbmod = DbMod}) ->
    delete_all(DbMod, Schema),
    {ok, InitSt} = DbMod:init(get_init_arg(dbargs)),
    handle_schema(Schema, From, St#refdb{has_schema = false,
                                         state = InitSt}).

delete_all(DbMod, Schema) when is_list(Schema) ->
    [DbMod:erase_class(Class) || {Class, _, _} <- Schema],
    ets:delete_all_objects(?CLASS_SCHEMA),
    ets:delete_all_objects(?CLASS_TARGETS).

handle_get_schema(From, St = #refdb{schema_list = SchemaList}) ->
    return_result(SchemaList, From),
    St.
    %{atomic, SchemaClasses} = mnesia:transaction(fun() -> mnesia_get_schema() end),
    %Schema = [{C, As, Ls} || #class{name=C, attribs=As, links=Ls} <- SchemaClasses]
    %{result, {ok, Schema}, State}.

handle_save_envs(From, St = #refdb{dbmod = DbMod}) ->
    save_envs(DbMod),
    return_result(ok, From),
    St.


%% Saves the environment configuration to `EnvConfFile'.
save_envs(DbMod) ->
    %EnvConfFile = "refactorerl.emacs.configuration",
    {ok, Root} = DbMod:root(),
    {ok, Envs} =
        case DbMod:is_path_supported() of
            true -> DbMod:path(Root, [env]);
            false -> DbMod:path(Root, [env])
        end,
    EnvDatas = [Data || Env <- Envs, {ok, Data} <- [DbMod:data(Env)]],
    case EnvDatas of
        [] ->
            no_envs_saved;
        _ ->
            {ok, Dev} = file:open(?ENV_CONF_FILE, [write]),
            io:format(Dev, "~p.~n", [EnvDatas]),
            file:close(Dev)
    end.

%% Restores the environment nodes saved by `save_envs/0'.
restore_envs(DbMod) ->
    {ok, Root} = DbMod:root(),
    {ok, OldEnvs} =
        case DbMod:is_path_supported() of
            true -> DbMod:path(Root, [env]);
            false -> DbMod:path(Root, [env])
        end,
    case OldEnvs of
        [] ->
            %EnvConfFile = "refactorerl.emacs.configuration",
            Envs =
                case filelib:is_file(?ENV_CONF_FILE) of
                    true ->
                        {ok, [Envs2]} = file:consult(?ENV_CONF_FILE),
                        Envs2;
                    false ->
                        [#env{name=appbase, value=code:lib_dir()},
                         #env{name=output, value=original}]
                end,
            %{ok, Root} = handle_root(),
            EnvNodes = [DbMod:create(Env) || Env <- Envs],
            [DbMod:mklink(Root, env, Env) || {ok, Env} <- EnvNodes];
        _ ->
            envs_already_present
    end.

handle_schema(Schema, From, St = #refdb{has_schema=false,
                                        dbmod = DbMod}) ->
    init_schema(lists:usort(Schema), DbMod),
    ets:insert(?CLASS_SCHEMA, #class{name='$hash', attribs=schema_hash(Schema)}),

    restore_envs(DbMod),

    return_result({ok, init}, From),
    St#refdb{has_schema=true,
             schema_list = Schema};

handle_schema(Schema, From, St = #refdb{has_schema=true}) ->
    OldHash =
        case ets:lookup(?CLASS_SCHEMA, '$hash') of
            [#class{attribs=OH}] -> OH;
            _                    -> unknown
        end,
    NewHash = schema_hash(Schema),
    case {OldHash =:= NewHash, ?autoreset_schema} of
        {true, _} ->
            return_result({ok, match}, From);
        {false, true} ->
            error_logger:info_msg("Schema is changed, resetting database.\n"),
            reset_schema(Schema, From, St);
            %return_result({ok, init}, From);
        {false, false} ->
            error_logger:error_report(
              [{module, ?MODULE},
               {message,"Required and stored graph schemas are different.\n"}]),
            return_result({ok, init}, From)
    end,
    St.

schema_hash(Schema) ->
    erlang:phash2(lists:usort(Schema)).

init_schema(SchemaElems, DbMod) when is_list(SchemaElems) ->
    [init_schema(Elem, DbMod) || Elem <- SchemaElems];
    %io:format("~p~n", [ets:tab2list(?CLASS_TARGETS)]);
init_schema({Class, Attribs, Links}, DbMod) ->
    DbMod:create_class(Class), % this should create lnk tabs also
    ets:insert(?CLASS_SCHEMA, #class{name=Class, attribs=Attribs, links=Links}),
    [ets:insert(?CLASS_TARGETS, #target{from=Class, tag=Tag, to=To}) ||
               {Tag, To} <- Links].

%%% ===========================================================================
%%% PATH

%% handle_path(Tag, [path, Node, Path] = Q, From, #refdb{dbmod = DbMod,
%%                                                       number_of_runners = Qs}
%%                                                = St) ->
%%     case {DbMod:is_path_supported(), Tag} of
%%         {true, _}          -> execute_graph_op(Tag, Q, From, St);
%%         {false, ?ASYNC_OP} -> spawn_link(?MODULE,
%%                                          handle_path_here,
%%                                          [Node, Path, DbMod]),
%%                               St#refdb{number_of_runners = Qs + 1};
%%         {false, ?SYNC_OP}  -> handle_path_here(Node, Path, DbMod)
%%     end.

handle_path(Node, Path) ->
    DbMod = get(dbmod), % FIXME
    handle_path_here(Node, Path, DbMod).

handle_path_here(Node = {?NODETAG, Class, _Id}, Path, DbMod) ->
    %Return =
        try
            case do_path(Path, [Node], Class, DbMod) of
                {error, bad_node, _} ->
                    {ok, []};
                Result ->
                    %{ok, [{?NODETAG, int_class(ResultId, DbMod), ResultId}
                        %  || ResultId <- Result]}
                    {ok, Result}
            end
        catch
            error:badarg -> {ok, []};
            throw:{overreaching_path_idx,_,_} -> {ok, []};
            throw:Msg -> {error, {Msg, Node, Path}}
        end.%,
    %return_result(Return, Extra).

%% int_class(Node, DbMod) ->
%%     get_class(DbMod:data(Node)).

%% get_class(Data) ->
%%     element(1, Data).

do_path([], Nodes, _, _) ->
    Nodes;
do_path([Elems|Rest], Nodes, Class, DbMod) when is_list(Elems) ->
    % Elems is an union of tags, we walk them all
    NewNodeRes = lists:flatten([int_path(DbMod, Nodes, fwd, Elem) || Elem <- Elems]),
    NewNodes = unique_path_results(NewNodeRes),
    do_path(Rest, NewNodes, Class, DbMod);
do_path([{Tag, unique}|Rest], Nodes, Class, DbMod) ->
    NewNodes = lists:usort(unique_path_results(int_path(DbMod, Nodes, fwd, Tag))),
    do_path(Rest, NewNodes, Class, DbMod);
do_path([{intersect, {?NODETAG, _, _Id} = TestNode, TestPathElem}|Rest], Nodes, Class, DbMod) ->
    Dir =
        case TestPathElem of
            {Tag, back} -> fwd;
            Tag         -> back
        end,
    {class, TestClass} = ?GET_NEXT_CLASS(Dir, Class, Tag),
    Tests = do_path([TestPathElem], [TestNode], TestClass, DbMod),
    do_path(Rest, ?MISC:intersect(Tests, Nodes), Class, DbMod);
do_path([{Tag, {FromIdx, ToIdx}} | Rest], Nodes, Class, DbMod) when is_integer(FromIdx), is_integer(ToIdx) ->
    Nodes2 = do_path([Tag], Nodes, Class, DbMod),
    NewNodes = lists:sublist(Nodes2, FromIdx, ToIdx - FromIdx),
    {class, NextClass} = ?GET_NEXT_CLASS_FWD(Class, Tag),
    do_path(Rest, NewNodes, NextClass, DbMod);
do_path([Elem | Rest], Nodes, Class, St = DbMod) ->
    {Dir, Filter} =
        case Elem of
            {{Tag, back}, Filt} when is_atom(Tag) -> {back, Filt};
            {Tag,  back}        when is_atom(Tag) -> {back, {}};
            {Tag,         Filt} when is_atom(Tag) -> {fwd,  Filt};
            Tag                 when is_atom(Tag) -> {fwd,  {}};
            Tag                                   -> throw({bad_path, Elem})
        end,

    {class, NextClass} = ?GET_NEXT_CLASS(Dir, Class, Tag),

    NewNodes =
        case Filter of
            {} ->
                unique_path_results(int_path(DbMod, Nodes, Dir, Tag));
            _ ->
                NewNodess  = [int_path(DbMod, [Node], Dir, Tag) || Node <- Nodes],
                Res = lists:flatten([filter_ptrs(Filter, NNodes, NextClass, St)
				     || NNodes <- NewNodess]),
		unique_path_results(Res)
%		lists:foldl(fun(N,A) -> case lists:member(N,A) of
%					    true -> A;
%					    false -> [N | A] end end,
%			    [], Res)
        end,
    do_path(Rest, NewNodes, NextClass, DbMod).

int_path(DbMod, Nodes, Dir, Tag) ->
    DbMod:int_path(Nodes, Dir, Tag).

unique_path_results(Res) ->
    Uni = lists:reverse(lists:foldl(fun unique_path_results/2, [], Res)),
    unzip_path_results(Uni).

unique_path_results(Res = {{_From, _Tag}, _Ind, _To}, Acc) ->
    case lists:member(Res, Acc) of
	true ->
	    Acc;
	false ->
	    [Res | Acc]
    end.

unzip_path_results(Res) ->
    {_, _, Nodes} = lists:unzip3(Res),
    Nodes.

%% Filters a set of pointers.
filter_ptrs({}, _Nodes, _, _DbMod) ->
    throw(is_handled_outside);
filter_ptrs(_, [], _, _St) ->
    [];
filter_ptrs(Idx, Nodes, _, _DbMod) when is_integer(Idx) ->
    nth(Idx, Nodes);
filter_ptrs(last, Nodes, _, _DbMod) ->
    lists:last(Nodes);
filter_ptrs({Idx, last}, Nodes, _, _DbMod) when is_integer(Idx) ->
    %{_, Part2} = lists:split(Idx, Nodes),
    %Part2;
    lists:filter(fun({_, I, _}) when I >= Idx -> true; (_) -> false end, Nodes);
filter_ptrs({Idx1, Idx2}, Nodes, _, _DbMod) when is_integer(Idx1), is_integer(Idx2) ->
    %lists:sublist(Nodes, Idx1, Idx2 - Idx1);
    lists:filter(fun({_, I, _}) when I >= Idx1; I =< Idx2 -> true; (_) -> false end, Nodes);
%{_, Part2}  = lists:split(Idx1 - 1, Nodes),
    %{Part21, _} = lists:split(Idx2 - Idx1 - 1, Part2),
    %Part21;
filter_ptrs(Filter, Nodes, NextClass, DbMod) ->
    Attrs = ?GET_CLASS_ATTRS(NextClass),
    [Node || Node <- Nodes, data_filter(Filter, DbMod:data(element(3,Node)), Attrs)].

data_filter({'not', Filter}, Data, Attrs) ->
    not data_filter(Filter, Data, Attrs);
data_filter({Filt1, 'and', Filt2}, Data, Attrs) ->
    data_filter(Filt1, Data, Attrs) andalso data_filter(Filt2, Data, Attrs);
data_filter({Filt1, 'or', Filt2}, Data, Attrs) ->
    data_filter(Filt1, Data, Attrs) orelse data_filter(Filt2, Data, Attrs);
data_filter({Attr, Op, Value}, {ok, Data}, Attrs) when is_atom(Attr) ->
    OpF =
        case Op of
            '==' -> fun(A,B) -> A =:= B end;
            '/=' -> fun(A,B) -> A =/= B end;
            '<'  -> fun(A,B) -> A <   B end;
            '=<' -> fun(A,B) -> A =<  B end;
            '>'  -> fun(A,B) -> A >   B end;
            '>=' -> fun(A,B) -> A >=  B end
        end,
    Ind = indexof(Attr, Attrs) + 1,
    %% if
    %%     Ind =:= 0 -> throw({bad_attribute, Attr});
    %%     true      ->
    %%         OpF(element(Ind, Data), Value)
    %% end;
    OpF(element(Ind, Data), Value);
data_filter(Cond, Data, Attrs) ->
    throw({bad_condition, Cond, Data, Attrs}).

nth(Idx, Nodes) ->
    try
        lists:nth(Idx, Nodes)
    catch
        error:function_clause ->
            throw({overreaching_path_idx, Idx, Nodes})
    end.

%% todo Move to ?List
indexof(El, Lst) -> indexof(El, Lst, 1).

indexof(_,  [],        _)   -> 0;
indexof(El, [El | _],  Ind) -> Ind;
indexof(El, [_  | Tl], Ind) -> indexof(El, Tl, Ind+1).
