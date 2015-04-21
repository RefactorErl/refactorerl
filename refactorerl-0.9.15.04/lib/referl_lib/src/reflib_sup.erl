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

-module(reflib_sup).
-vsn("$Rev: 9848 $").
-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).
-export([start_sup/1]).

-include("lib.hrl").

%%% ============================================================================
%%% Application functions

%% @private
start(normal, _StartArgs) ->
    start_sup(top).

%% @private
start_sup(Node) ->
    supervisor:start_link(?MODULE, Node).

%% @private
stop(_State) ->
    ok.

%% =============================================================================
%% Supervisor functions

%% @private
init(top) ->
    {ok,
     {{one_for_one, 3, 1},
      [%% Transformation server
       {transform,
        {?Transform, start_link, []},
        permanent,
        3000,
        worker,
        [?Transform]},
       %% User interface bridge
       {ui,
        {?UI, start_link, []},
        permanent,
        1000,
        worker,
        [reflib_ui]}
      ]}}.
