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

-include("cluster.hrl").
%-include_lib("referl_core/include/refactorerl.hrl").
-include("ucl_types.hrl").



%%% ============================================================================
%%% Processes

% Name of the generic server which executes the clustering algorihtms
-define(ClExec_server,  ucl_exec).



%%% ============================================================================
%%% Module name macros

% Generic server to execute the clustering algorihtms
-define(ClExec,         ucl_exec).

% Module to handle a clustering configuration
-define(ClConfig,       ucl_config).

% Libraries to handle clustering state
-define(ClState,        ucl_state).
%-define(ClAttr,         ucl_attr).
-define(ClConn,         ucl_connection).

% Library to handle an clustering (grouping)
-define(ClClustering,   ucl_clustering).

% Other libraries
-define(ClCommon,       ucl_common).        % Common functions
-define(ClOpts,         ucl_options).       % Options of clustering steps
%-define(ClOut,          cl_out).            % Output handling

% Clustering algorithms
-define(ClAlg,          ucl_alg).           % Base algorithm with base steps
-define(ClDm,           ucl_alg_dm).        % Data mining

% Data structures
%-define(Matrix,         cl_matrix).        % Matrix
-define(MHT,            multi_hashtree).    % Multiple level hash tree

% Miscellaneous macros
-define(UNDEF,          undefined).



%%% ============================================================================
%%% Guard sequences

% Create a refError() pattern.
-define(RefErrorPattern(ErrMod, ErrType, ErrArgs), {ErrMod, ErrType, ErrArgs}).

% Guard sequence to check a pattern is a refError().
-define(Is_RefError(Error), 
    (is_tuple(Error) andalso 3==tuple_size(Error) andalso 
     is_atom(element(1,Error)) andalso is_atom(element(2,Error))) ).



%%% ============================================================================
%%% Imports

% Proplists to handle options
-import(proplists, [get_value/2, get_value/3]).



