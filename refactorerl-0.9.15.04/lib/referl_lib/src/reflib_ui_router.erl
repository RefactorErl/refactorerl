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


%%% @author bkil.hu
%%% @author Viktoria Fordos <f-viktoria@elte.hu>

%%% @doc 
%%% <b>UI message router.</b>
%%%
%%% The strategy for managing requests:<br/>
%%% When a new request is arrived we examine it, whether is it a new request.
%%% A request, which is not a member of the requests list, is called new request.
%%% Otherwise, it is called  registered request. This classification
%%% is needed, because of some ui services (etc refactorings) exist, 
%%% which require more then one steps for its execution 
%%% and require user interactions.
%%%
%%% If a request is a registered request, its execution is always permitted
%%% and any administration is not needed, because of its parent request 
%%% had been administrated.
%%%
%%% If a request is a new request, another examinations would be performed.
%%% First, we have to determine whether the request is not in the 'ui router's
%%% blacklist'. If it is,it should be denied.
%%% The 'ui router's blacklist' is a list of denied operations. These operations
%%% can not be allowed to run in any circumstances. The blacklist is loaded from
%%% a file during the initialization of the server.
%%%
%%% Second, we have to determine the type of the request first. It can be a modifier or 
%%% a non-modifier. If a request modify the database, then it is called modifier, 
%%% else it is called non-modifier. After the type of the request has been 
%%% determined, the permission of the execution should be limited, as described 
%%% below.
%%% A modifier can only be executed when nothing else is running. 
%%% A non-modifier can only not be executed when a modifier is running. 
%%%
%%% If the request has the permission of the execution then the following sequence 
%%% should be executed.
%%%
%%% 1) Before the execution of the request, we add it to the requests list.<br/>
%%% 2) The request is executed.<br/>
%%% 3) After the execution of the request, we send back its result for 
%%% the client, and remove it from the requests list.
%%%
%%% If the request has not got the permission of the execution then it should be 
%%% denied and should be ignored.

-module(reflib_ui_router).
-vsn("$Rev: 8597 $ ").
-behaviour(gen_server).

%-compile(export_all).

%% interface
-export([getid/0, request/2, request/3]).
-export([add_msg_handler/1, del_msg_handler/1]).
-export([can_read_db/0, can_write_db/0]).
-export([is_restricted_mode/0]).

%% private
-export([broadcast/2]).

%%% Server exports
-export([start_link/0]).

%% gen_server callbacks
-export([handle_call/3, handle_cast/2]).
-export([init/1, handle_info/2, terminate/2, code_change/3]).

%% Callbacks
-export([error_text/2]).

-export([set_ref_node/1]).

%-define(DEBUG, true).
 
-ifdef(DEBUG).
-define(debug(Var), 
        error_logger:info_msg("DEBUG: ~p:~p~n~p~n  ~p~n", 
                              [?MODULE, ?LINE, ??Var, Var])).
-else.
-define(debug(__Arg), ok).
-endif.

-include("lib.hrl").

%% constants
-define(T_Broadcast, global).
-define(T_Reply, reply).

-define(NoReply,   noreply).
-define(ERR(R),    {error,{R, ?Error:error_text(R)}}).
-define(LErr(R),   ?LocalErr(R,[])).
-define(LocalErr(R,L), ?ERR(?LocalError(R,L))).
-define(RefErr(R,L),   ?ERR(?RefError(R,L))).

%Filename of the blacklist
-define(BLACKLIST_FILE, filename:join(mnesia:system_info(directory),
                                      "ui_router_blacklist")).

%% State of the ui_router
-record(state, {
                % Message handlers
                msg_handlers = []     :: list(),
                % Requests 
                requests = []  :: list(),
                % Blacklist
                blacklist = [] :: list()
               }).

%% Type of the available UI requests.
%% If a request modify the database, then it is called modifier, 
%% else it is called non-modifier.
%% Every UI request, which is identified by its name, needs to be classify.
%% The classes are exclusive to each other.
-record(request_type, {
                     %List of modifiers
                     modifiers=[%Database operations 
                                stop, reset, clean, add, drop, backup,
                                %Graph operations 
                                %not implemented in refdb_mnesia, but implemented in refdb_nif
                                create_graph, rename_graph, restore, undo, load_graph,
                                delete_graph, delete_all_graphs,
                                %Handling beam files
                                load_beam,
                                %Database operations (user level)
                                add_dir, drop_dir, add_by_emakefile,
                                load_configuration, unload_configuration,
                                %Enviromental nodes
                                add_env, del_env, del_env_val, set_env,
                                %Database synchronization
                                synchronize,
                                %Analysers
                                anal_dyn, clean_dyn, anal_dyn_spec, anal_message
                                ],


                     %List of non-modifiers
                     non_modifiers=[%Database operations
                                    ls_backups, backup_info,
                                   %Graph operations
                                    ls_graphs, actual_graph,
                                    %Status queries
                                    status, showconfig, filelist, status_info,
                                    system_info, data_dir,
                                    cat_file, cat_fun, cat_recmac,
                                    %Enviromental nodes
                                    get_envs, get_env,
                                    %Smart graph
                                    generate_smart_graph,
                                    %Directory sorting
                                    dir_sort, dir_sort_get_dirs, dir_sort_get_mods,
                                    %Function block analyses
                                    refusr_fb_regexp_re,
                                    %Interface layers
                                    if_layers_show_insults, if_layers_draw, 
                                    if_layers_draw, 
                                    if_layers_check_layered_architecture,
                                    %Duplicated code
                                    get_algorithms,
                                    get_algorithm_data,
                                    duplicated_code_const_var_diff,
                                    get_dupcode_group,
                                    get_all_dupcode_result,
                                    %Semantic queries utils
                                    draw, get_running_queries, kill_query,
                                    funlist, recordlist, macrolist,
                                    %Transformation utils
                                    reply, cancel,
                                    %Clustering
                                    run_cl, cl_refresh, cl_options, cl_prev_clustering,
                                    %Metric mode
                                    metric_mode,
                                    transform,
                                    %graph querying
                                    graph_query, graph_data,
                                    form_length,
                                    syn_leaves, syn_index, syn_class, syn_tree_text,

                                    syn_node_type, syn_first_leaf, syn_last_leaf,
                                    function_positions,

                                    %html generation
                                    generate_dir, html_generate_node, 
                                    html_generate,
                                    %Hashes
                                    database_hash, file_hash,
                                    %Positioning
                                    current_posmode,
                                    %Skeletons
                                    update_skeleton, update_prev_skeleton_comment, 
                                    delete_skeleton, skeleton_call_format, 
                                    save_skeleton, list_skeletons, evaluate_skeleton,
                                    evaluate_skeleton, try_parse_skeleton,
                                    determine_sq_request_type, 
                                    do_autocomplete_skeleton,
                                    get_file_from_server, do_depanal_and_transfer,
                                    %%Wx
                                    start_wx, fundef_pos, start_pos, node_pos, 
                                    get_container, definition, node_type, function_text,
                                    get_all_invtab_name_user_pairs, get_from_invtab,
                                    delete_from_invtab, insert_to_invtab, delete_from_qtab,
                                    find_in_qtab_by_pattern, find_in_qtab, insert_to_qtab,
                                    insert_to_qtab, update_qtab_if_needed, deptab_delete_all,
                                    deptab_delete, deptab_match_opts,
                                    deptab_dets_insert,

                                    act_graph,
                                    %Deadcode
                                    deadcode,deadcode_interface,
                                    %Dependency graph
                                    draw_dep_graph,
                                    %File Cache Server
                                    generate_all,
                                    %CloneIdentifierl
                                    clone_identifierl, clone_identifierl_by_pos]
                    }).


%% Request data
-record(request, {%Identify the request
                  reqid :: string(),
                  %The type of the request
                  req_type :: modifier | non_modifier
                  }).



%%% ============================================================================
%%% Error texts

error_text(bad_request, [X]) ->
    ["Bad UI request: ", ?MISC:any_to_string(X)];
error_text(exception, [Cl,Error,Stack]) ->
    ["Unhandled exception ", atom_to_list(Cl), ":", io_lib:write(Error),
    io_lib:print(Stack)];
error_text(bad_response, [X]) ->
    ["Bad UI functional backend response: ", ?MISC:any_to_string(X)].

%%% ============================================================================
%%% Error texts

% Note: you MUST NOT rely on the internal structure of a request ID
getid() ->
    gen_server:call({?MODULE, ?REFERL_NODE}, {getid}).

% Note: you MUST NOT rely on the internal structure of a request ID
request(ReqId, Request) ->
    request(self(), ReqId, Request).

% Note: you MUST NOT rely on the internal structure of a request ID
request(Pid, ReqId, Request) when is_pid(Pid) ->
    RPid = {Pid, node()},
    gen_server:call({?MODULE, ?REFERL_NODE}, {request, RPid, ReqId, Request});

% Note: you MUST NOT rely on the internal structure of a request ID
request(RPid, ReqId, Request) ->
    gen_server:call({?MODULE, ?REFERL_NODE}, {request, RPid, ReqId, Request}).

add_msg_handler(Pid) ->
    gen_server:call({?MODULE, ?REFERL_NODE}, {add_msg_handler, Pid}).

del_msg_handler(Pid) ->
    gen_server:call({?MODULE, ?REFERL_NODE}, {del_msg_handler, Pid}).

broadcast(Type, Details) ->
    gen_server:cast({?MODULE, ?REFERL_NODE}, {broadcast, Type, Details}).

% Returns true, if the database can be read
can_read_db()->
    gen_server:call({?MODULE, ?REFERL_NODE}, {can_access_db, non_modifier}).

% Returns true, if the database can be read or can be written
can_write_db()->
    gen_server:call({?MODULE, ?REFERL_NODE}, {can_access_db, modifier}).
%%

% Returns true, if RefactorErl is started in restricted mode
is_restricted_mode()->
    gen_server:call({?MODULE, ?REFERL_NODE}, {is_restricted_mode}).
%%

%% @doc Starts the server and links it with the calling process.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_Args) ->
    process_flag(trap_exit, true),
    Blacklist = init_blacklist(),
    setup_restricted_mode(),
    case check_exclusive_constraint() of
        ok -> {ok, #state{blacklist = Blacklist}};
        violator_found -> {stop, internal_error}
    end.

set_ref_node([server, _ ])->
    set_ref_node([node()]);

set_ref_node([Node]) ->
    application:set_env(refactorerl, referl_node, Node),
    ok.

%% ================
%% Implementation

handle_call({getid}, _From, State) ->
    Ref = make_ref(),
    Reply = {reqid, base64:encode_to_string(term_to_binary(Ref))}, %@todo
    {reply, Reply, State};

handle_call({is_restricted_mode}, _From, State=#state{}) ->
    {reply, get(restricted_mode), State};

handle_call({can_access_db, AccessType}, _From, State=#state{requests=Requests}) ->
    Reply = can_permit_execution(AccessType, Requests),
    {reply, Reply, State};

handle_call({request, {Pid, Node}, ReqId={reqid,S}, Request}, _From, State)
  when is_pid(Pid), is_atom(Node), is_list(S) ->
    {Reply, NewState}=case is_new_request(S, State#state.requests) of
        true -> start_new_request(Pid, S, Request, State);
        false -> R = handle_registered_request(Pid, ReqId, Request),
                 {R, State}
    end,
    ?debug(NewState),
    {reply, Reply, NewState};

handle_call({request, _Pid, _ReqId, _Request}, _From, State) ->
    {reply, error, State};

handle_call({add_msg_handler, HandlerPid}, _From, 
            State = #state{msg_handlers=Handlers} ) ->
    HandlerPid ! installed,
    NewHandlers = [HandlerPid | Handlers],
    {reply, ok, State#state{msg_handlers= NewHandlers}};

handle_call({del_msg_handler, Handler}, _From, 
            State = #state{msg_handlers=Handlers} ) ->
    NewHandlers = lists:delete(Handler, Handlers),
    {reply, ok, State#state{msg_handlers= NewHandlers}};

handle_call({remove_request, ReqId}, _From, State = #state{requests = Requests})
  when is_list(ReqId) ->
    NewRequests = lists:filter(fun(#request{reqid = Id}) -> 
                                       (Id /= ReqId) 
                               end,
                               Requests ),
    {reply, ok, State#state{requests=NewRequests}};


handle_call(_, _, State) ->
    {noreply, State}.

handle_cast({broadcast, Type, Details}, State = #state{msg_handlers=Handlers} ) ->
    lists:foreach(fun(Pid) ->
                          Pid ! {?T_Broadcast, Type, Details}
                  end, Handlers),
    {noreply, State};

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, _Reason}, State = #state{msg_handlers=Handlers} ) ->
    NewHandlers = lists:delete(Pid, Handlers),
    {noreply, State#state{msg_handlers= NewHandlers}};

%% @private
handle_info(_, S) ->
    {noreply, S}.

%% @private
terminate(_,_) ->
    ok.

%% @private
code_change(_,S,_) ->
    {ok, S}.

%% ========
%% Private implementation

create_callbacks(Pid, ReqId) ->
    UnicastFun =
        fun(Type, Details) -> 
            Pid ! {ReqId, Type, Details}
        end,
    BroadcastFun =
        fun(Type, Details) ->
            ?MODULE:broadcast(Type, Details)
        end,
    #msg_cb{unicast = UnicastFun, broadcast = BroadcastFun}.

run(Request, MCB=#msg_cb{},RunAfter)->
    spawn(fun() ->
                  R = exec_req(Request, MCB),
                  case R of
                      ?NoReply -> ?NoReply;
                      _ ->
                          Reply =
                              case R of
                                  {ok,{abort,E}} ->
                                      {ok,{abort,{E,?Error:error_text(E)}}};
                                  {ok,_}->
                                      R;
                                  {error,E} ->
                                      ?ERR(E);
                                  _ ->
                                      ?LocalErr(bad_response, [R])
                              end,
                          RunAfter(),
                          (MCB#msg_cb.unicast)(?T_Reply, Reply)
                  end
          end),
    ok.

exec_req(Request, MCB=#msg_cb{}) ->
    M = ?UIB,
    UI_funs = proplists:get_value(exports, M:module_info()),
    case catch tuple_to_list(Request) of
        [F | Args0] ->
            Args = [MCB | Args0],
            FunArity = {F,length(Args)},
            case lists:member(FunArity, UI_funs) of
                true ->
                    try
                        apply(M, F, Args)
                    catch
                        error:Error = {_,_,_} ->
                            {error, Error};
                        throw:Error = {_,_,_} ->
                            {error, Error};
                        Cl:Error ->
                            {error,?LocalError(exception, [Cl, Error,
                                                  erlang:get_stacktrace()])}
                    end;
                false ->
                    {error,?LocalError(bad_request,[Request])}
            end;
        _ ->
            {error,?LocalErr(bad_request,[Request])}
            %@todo: or perhaps throw an exception instead?!
    end.

add_request(Request, State = #state{requests = Requests}) 
  when is_record(Request, request) ->
    NewRequests = Requests ++ [Request],
    State#state{requests=NewRequests}.
    
remove_request(ReqId)->
    gen_server:call({?MODULE, ?REFERL_NODE}, {remove_request, ReqId}).

start_new_request(Pid, ReqId, Request, State)->
    #state{requests = Requests, blacklist = Blacklist} = State,
    RequestType = determine_request_type(Request),
    case not_in_blacklist(Request, Blacklist) 
        andalso can_permit_execution(RequestType, Requests) of
        false -> {deny, State};
        true -> Result = run_request(Pid, ReqId, Request, true),
                NewState = add_request(#request{reqid = ReqId,
                                                req_type = RequestType},
                                       State),
                inform_user({Request, started}),
                {Result, NewState}
    end.

handle_registered_request(Pid, ReqId, Request)->
    run_request(Pid, ReqId, Request, false),
    ok.

run_request(Pid, ReqId, Request, NeedRemove)->
    MCB = create_callbacks(Pid, {reqid,ReqId}),
    Fun = fun()->
                  case NeedRemove of
                      true -> remove_request(ReqId),
                              inform_user({Request, finished});
                      _ -> ok
                  end
          end,
    run(Request, MCB, Fun).

% It verifies whether the available UI requests are complying with the rule.
% The rule is descirbed above.
% Every UI request is assigned to exactly one class {modifier, non_modifier}.
check_exclusive_constraint()->
    RequestType=#request_type{},
    Modifiers = RequestType#request_type.modifiers,
    NonModifiers = RequestType#request_type.non_modifiers,
    case sets:is_disjoint(sets:from_list(Modifiers), 
                          sets:from_list(NonModifiers)) of
        true -> ok;
        false -> violator_found
    end.

% If the request Id of the request can not be be found in the requests list, 
% then the request is called new request.
is_new_request(ReqId, Requests) when is_list(ReqId) andalso is_list(Requests)->
    case lists:keyfind(ReqId, #request.reqid, Requests) of
        R when is_record(R, request) -> false;
        _ -> true
    end.

% We assume that a request is a modifier 
% till we can not proof that the inverse property is true. 
% If the request is an transformation we handle it as an exception.
determine_request_type({transform, Action, _}) ->
	case ?UIB:is_transformation(?UIB:action_module_name(Action)) of
		true -> modifier;
		false -> non_modifier
	end;
determine_request_type(Request) when is_tuple(Request) ->
    determine_request_type(element(1, Request));
determine_request_type(Request) when is_atom(Request) ->
    case lists:member(Request, (#request_type{})#request_type.non_modifiers) of
        true -> non_modifier;
        false -> modifier
    end.

% A modifier can only be executed when nothing else is running.
% A non-modifier can only not be executed when a modifier is running.
can_permit_execution(modifier, [])->
    true;
can_permit_execution(modifier, _) ->
    false;
can_permit_execution(non_modifier, [ #request{req_type = modifier} | _Tail ]) ->
    false;
can_permit_execution(non_modifier, _) ->
    true.

% If the blacklist contains the request,  then we say, the request is in blacklist.
not_in_blacklist(Request, Blacklist) when is_tuple(Request) ->
    not_in_blacklist(element(1, Request), Blacklist);
not_in_blacklist(Request, Blacklist) when is_list(Blacklist) ->
    not lists:member(Request, Blacklist).
   
% The initialization of the blacklist.
init_blacklist()->
    case file:consult(?BLACKLIST_FILE) of
        {ok, [BlackList]} when is_list(BlackList) ->
            lists:filter(fun(E)->
                                 is_atom(E) 
                         end, BlackList);
        _ -> []
    end.

inform_user({Request, JobStatus}) 
  when (JobStatus =:= started orelse JobStatus =:= finished)
  andalso is_tuple(Request)->
    RequestName = element(1, Request),
    [ _ | JobArgs ] = tuple_to_list(Request),
    Code = {RequestName, determine_request_type(Request), JobStatus, JobArgs},
    Msg = ?Info:info_text(Code),
    broadcast(jobinfo, {Code, Msg}),
    ok.

%% Set restricted mode internal flag if needed
setup_restricted_mode() ->
    case init:get_argument(restricted_mode) of
        error                 -> set_restricted(false);
        {ok, [["false"]]}     -> set_restricted(false);
        {ok, [["true"]]}      -> set_restricted(true)
    end.

set_restricted(Bool) ->
    put(restricted_mode, Bool).
