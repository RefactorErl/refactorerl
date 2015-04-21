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

%%% @author Viktoria Fordos <f-viktoria@elte.hu>
%%% @doc Top supervisor of the web2 app.

-module(referl_ui_web2_sup).
-vsn("$Rev:$").
-behaviour(supervisor).

%% Supervisor callbacks
-export([init/1, start/0]).

-include("ui.hrl").

-define(SERVER, ?MODULE).

%%% ============================================================================
%%% Application functions

start() ->
    case whereis(?SERVER) of
        undefined -> supervisor:start_link({local,?SERVER}, ?MODULE, []);
        Pid when is_pid(Pid)->  {already_started, Pid}
    end.

%% =============================================================================
%% Supervisor functions

%% @private
init([]) ->
    {ok,
     {{one_for_one, 3, 1},
      [%% CACHE server
       {cache_server,
        {?WEB2_CACHE, start_link, []},
        permanent,
        4000,
        worker,
        [?WEB2_CACHE]},
       %% Session server
       {session_server,
        {?WEB2_SESSIONS, start_link, []},
        permanent,
        4000,
        worker,
        [?WEB2_SESSIONS]}
      ]}}.
