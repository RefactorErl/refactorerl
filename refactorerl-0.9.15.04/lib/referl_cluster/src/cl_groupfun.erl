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

%%% @doc This module contains functions that compute the distance of a newly
%%% created entity from other entities. The new entity is created by merging
%%% two originial entities. The functions have the following 6 arguments:
%%%
%%% <ol>
%%%     <li>Size of the first original entity</li>
%%%     <li>Size of the second original entity</li>
%%%     <li>Size of the other entity</li>
%%%     <li>Distance of the two original entities</li>
%%%     <li>Distance of the first original and the other entity</li>
%%%     <li>Distance of the second original and the other entity</li>
%%% </ol>
%%%
%%% The functions return the distance of the new, merged entity and the other
%%% entity.
%%%
%%% @author Aniko Nagyne Vig <viganiko@inf.elte.hu>
%%% @author Krisztian Toth <ccc@inf.elte.hu>

-module(cl_groupfun).
-vsn("$Rev: 9568 $").

-export([single/6, complete/6, uwaverage/6, waverage/6, centroid/6, median/6,
         ward/6]).

%% Argument usage note: functions calculate the value of d(R, P+Q) from |P|,
%% |Q|, |R|, d(P,Q), d(R,P), and d(R,Q), this is the origin of variable names.

%% @doc Single linkage distance.
single(_, _, _, _, DRP, DRQ) ->
    (DRP+DRQ-abs(DRP-DRQ))/2.

%% @doc Complete linkage distance.
complete(_, _, _, _, DRP, DRQ) ->
    (DRP+DRQ+abs(DRP-DRQ))/2.

%% @doc Unweighted average linkage distance.
uwaverage(_, _, _, _, DRP, DRQ) ->
    (DRP+DRQ)/2.

%% @doc Weighted average linkage distance.
waverage(NP, NQ, _, _, DRP, DRQ) ->
    NP/(NP+NQ)*DRP+ NQ/(NP+NQ)*DRQ.

%% @doc Centroid distance.
centroid(NP, NQ, _, DPQ, DRP, DRQ) ->
    NP/(NP+NQ)*DRP + NQ/(NP+NQ)*DRQ - NP*NQ/math:pow((NP+NQ),2)*DPQ.

%% @doc Median distance.
median(_, _, _, DPQ, DRP, DRQ) ->
    DRP/2 + DRQ/2 - DPQ/4.

%% @doc Ward distance.
ward(NP, NQ, NR, DPQ, DRP, DRQ) ->
    (NP+NR)/(NP+NQ+NR)*DRP + (NQ+NR)/(NP+NQ+NR)*DRQ - NR/(NP+NQ+NR)*DPQ.
