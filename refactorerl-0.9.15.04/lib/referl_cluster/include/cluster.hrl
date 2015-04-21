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
%%% The Initial Developer of the Original Code is E�tv�s Lor�nd University.
%%% Portions created  by E�tv�s Lor�nd University and ELTE-Soft Ltd.
%%% are Copyright 2007-2013 E�tv�s Lor�nd University, ELTE-Soft Ltd.
%%% and Ericsson Hungary. All Rights Reserved.

-include_lib("referl_gen/include/gen_export.hrl").
-include_lib("referl_core/include/core_global.hrl").
-include_lib("referl_lib/include/lib_export.hrl").

-include_lib("referl_core/include/core_export.hrl").


%% Records for the attribute matrix of clustering
-record(rec_attr,{file,name}).
-record(macro_attr,{file,name}).
-record(fun_attr,{mod,name,arity}).

%% Records for the fitness function
-record(which_entities, {funs=true, recs=true, macros=false}).


%%
-define(CMISC,      cl_misc).
-define(Matrix,     cl_matrix).
-define(ClPrint, 	cl_print).
-define(ClGenetic,	cl_genetic).
%-define(ClOut,		cl_out).
-define(ClUtils,	cl_utils).
-define(ClFitness,	cl_fitness).
-define(ClCore,		cl_core).
-define(ClInterface,cl_interface).
-define(ClUi,		cl_ui).
-define(ClMergefun, cl_mergefun).
-define(ClDistfun,  cl_distfun).
-define(ClAttr,		cl_attr).
-define(ClCutlib,   cl_cutlib).
-define(ClKmeans,   cl_kmeans).
-define(ClFuzzy,    cl_fuzzy_c_means).
