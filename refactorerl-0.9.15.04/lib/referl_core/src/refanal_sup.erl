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

%%% @doc The callback module of the refactorer application and the callback
%%% module for the supervisors of the refactorer application.
%%%
%%% @author Laszlo Lovei <lovei@elte.hu>

-module(refanal_sup).
-vsn("$Rev: 12913 $").
-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callback
-export([init/1]).

%% Supervisor tree building
-export([start_sup/1, start_sup/2]).

-include("core.hrl").

%%% ============================================================================
%%% Application functions

%% @private
start(normal, _StartArgs) ->
    start_sup(top).

%% @private
start_sup(Node) ->
    supervisor:start_link(?MODULE, Node).

%% @private
start_sup(Name, Node) ->
    supervisor:start_link({local, Name}, ?MODULE, Node).

%% @private
stop(_State) ->
    ok.

%% =============================================================================
%% Supervisor functions

-ifdef(development_mode).
    -define(ALLOWED_RESTARTS_COUNT, 1).
    -define(ALLOWED_RESTARTS_IN_SEC, 3).
    -define(SERVER_SHUTDOWN_TIMEOUT, 3000).
-else.
    -define(ALLOWED_RESTARTS_COUNT, 3).
    -define(ALLOWED_RESTARTS_IN_SEC, 3600).
    -define(SERVER_SHUTDOWN_TIMEOUT, 3000).
-endif.


%% @private
init(top) ->
    {ok,
     {{one_for_one, 3, 1},
      [%% Graph storage subtree
       {graphsup,
        {?MODULE, start_sup, [graph]},
        permanent,
        infinity,
        supervisor,
        [?MODULE]},
       %% File manager server
       {fileman,
        {?FileMan, start_link, []},
        permanent,
        ?SERVER_SHUTDOWN_TIMEOUT,
        supervisor,
        [?FileMan]},
       %% Semantic node server
       {semnode,
        {?NodeSync, start_link, []},
        permanent,
        ?SERVER_SHUTDOWN_TIMEOUT,
        worker,
        [?NodeSync]},
       %% Html manager server
       {htmlman,
        {referl_htmlserver, start_link, []},
        permanent,
        ?SERVER_SHUTDOWN_TIMEOUT,
        worker,
        [referl_htmlserver]}%,
        %% FileCache server
       % {filecacheman,
       %  {?FileCacheMan, start_link, []},
       %  permanent,
       %  3000,
       %  worker,
       %  []}
      ]}};

init(graph) ->
    {ok,
     {{one_for_all,   % restart everyone in case of runtime error
       ?ALLOWED_RESTARTS_COUNT,
       ?ALLOWED_RESTARTS_IN_SEC},
      % since graph_server will use it, this will be frist
      % one_for_all is still needed, because ESG has to re-inject schema if db crashes
       [{refdb,
        {?RefDb, start_link, []},
        permanent,
        ?SERVER_SHUTDOWN_TIMEOUT,
        worker,
        [?RefDb]},
      %% Low level graph storage
       %% Low level graph storage
       %% {graph,        % ID
       %%  {?Graph, start_link, []},   % start function
       %%  permanent,    % always restart
       %%  ?SERVER_SHUTDOWN_TIMEOUT,         % shutdown timeout: 3 second
       %%  worker,       % worker child (not a supervisor)
       %%  [?Graph]      % module list
       %% },
       %% Syntax tree storage and analyser
       {esg,
        {?ESG, start_link, []},
        permanent,
        ?SERVER_SHUTDOWN_TIMEOUT,
        worker,
        [?ESG]
       },
        {anal,
        {?MODULE, start_sup, [?ANAL_SERVER, anal]},
        permanent,
        ?SERVER_SHUTDOWN_TIMEOUT,
        supervisor,
        [?MODULE]},
      %% Function analyser server
       {funprop,
        {?FunProp, start_link, []},
        permanent,
        ?SERVER_SHUTDOWN_TIMEOUT,
        worker,
        [?FunProp]}
      ]}};

init(anal) ->
    {ok,
     {{simple_one_for_one, 1, 3},
      [{anal, {?Anal, start_link, []}, transient, brutal_kill, worker, [?Anal]}
      ]}}.

