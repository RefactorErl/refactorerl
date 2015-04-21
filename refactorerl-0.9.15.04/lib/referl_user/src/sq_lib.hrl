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

-record(entity, {name, selectors, properties}).
-record(selector, {name, type, func, desc=" "}).
-record(property, {name, type, func, desc=" "}).

-record(initial_selector, {name, type, func, desc=" "}).
-record(statistics, {name, func, desc=" "}).

%% The record `state' stores the data of a single step in the semantic query.
%% `action' is the type of the current action. The action can be:
%%   selection
%%   closure
%%   iteration
%%   property_query
%%   composite_property_query
%%   statistics
%% `type', `res' and `groupby_type', `groupby_res' contains the data of the
%% current and the groupby results.
%% The `name' field contains the displayed name for the results,
%% `entitynumber' contains the number of the processed entities
%% `groupby_place' contains the number of the grouping entity (default value: (last entity's entitynumber) -1)
%% `variables' stores the variables used in semantic queries during
%% processing.
%% `new_variables' stores yet unbound variables.
%% `parallel_key' stores keys from rpc:async_call/4 if and only if the query
%% processed parallel, that is when variables bound.
%% `semquery' ++ `semquery_last' stores the remaining (unprocessed) part of 
%% the semantic query for the purpose of start processing parallel 
%% (see filter processing clause of refusr_sq:process/2)

-record(state, {action=selection,
		type,
		name = [],
		res,
		params = [],
		groupby_type = [],
		groupby_res = [],
		entitynumber = 1,
		groupby_place,
		checked_query = [],
		variables = [],
		parallel_key = [],
		semquery = [],
		semquery_last = [],
		closure_res = [],
		chain_keys = [],
		chain = [],
		chains_table}).
-record(chains, {complete = [], incomplete = [], recursive = []}).
