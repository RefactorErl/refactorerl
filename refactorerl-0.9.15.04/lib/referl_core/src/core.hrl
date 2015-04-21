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

-include_lib("referl_gen/include/gen_export.hrl").
-include_lib("referl_core/include/core_global.hrl").
-include_lib("referl_core/include/core_export.hrl").
-include_lib("referl_lib/include/lib_export.hrl").
-include_lib("kernel/include/file.hrl").

%%% ===========================================================================
%%% Modules

-define(ErlScanner, refcore_erl_scanner).
-define(ErlParser,  refcore_erl_parser).
-define(ErlNodes,   refcore_erl_nodes).
-define(Anal,       refcore_anal).
-define(NodeSync,   refcore_nodesync).
-define(FunProp,    refcore_funprop).
-define(RefDb,      refcore_gendb).
-define(Db,         ?RefDb).

%%% ===========================================================================
%%% Processes

%% Graph storage server process name
-define(GRAPH_SERVER, graph_server).

%% Semantical analyser server process name
-define(ESG_SERVER, esg_server).

%% File manager server process name
-define(FILEMAN_SERVER, fileman_server).

%% Analyser process manager
-define(ANAL_SERVER, anal_sup_srv).

%% Analyser related processes
-define(NODESYNC_SERVER, nodesync_server).
-define(FUNPROP_SERVER, funprop_server).

%%% ===========================================================================
%%% Database

%% protected nodes and links table: nodes that should not be removed
-record(protected, {type, infos}).

%%% ===========================================================================
%%% Application
-define(APPLICATION_NAME, referl_core).

%%% ===========================================================================
%%% Default shutdown for supervisors
-define(SHUTDOWN, case erlang:system_info(otp_release) >= "R15B" of
                    true -> infinity;
                    false -> 1000
                  end).
