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

%%% ============================================================================
%%% Module Informations

%%% @doc Library of connection/similarity/distance functions between
%%% entities/attributes.
%%%
%%% == TODO ==
%%% Write/copy other connection functions.

%%% @todo Write/copy other connection functions.

%%% @author Kornel Horvath <kornel@inf.elte.hu>


-module(ucl_connection).
-vsn("$Rev:  $").

-include_lib("referl_cluster/include/ucluster.hrl").


%%% ============================================================================
%%% Imports/exports

% Similarty measures
-export([common_attr_cnt/3, common_ent_cnt/3]).



%%% ============================================================================
%%% Building connection matrix

%% @spec common_attr_cnt(Ent1::clItem(), Ent2::clItem(), AttrMatrix::matrix())
%%           -> ConnResult
%%       ConnResult = {def, any()} | {val, ConnVal::term()}
%% @doc Number of the common attributes of `Ent1' and `Ent2' entities.
%%      If there are no common attributes return a `{def, 0}' tuple otherwise
%%      a `{val, NumberOfCommonAtribuetes}' tuple.
common_attr_cnt(#clItem{id=Id1}, #clItem{id=Id2}, AttrMatrix) ->
    case length(?MISC:intersect(?Matrix:get_row_non_def_ids(Id1, AttrMatrix),
            ?Matrix:get_row_non_def_ids(Id2, AttrMatrix))) of
        0 -> {def,   0};
        N -> {value, N}
    end.


%% @spec common_ent_cnt(Attr1::clItem(), Attr2::clItem(), AttrMatrix::matrix())
%%           -> ConnResult
%%       ConnResult = {def, any()} | {val, ConnVal::term()}
%% @doc Number of the common entities of `Attr1' and `Attr2' attributes.
%%      If there are no common entities return a `{def, 0}' tuple otherwise
%%      a `{val, NumberOfCommonEntities}' tuple.
common_ent_cnt(#clItem{id=Id1}, #clItem{id=Id2}, AttrMatrix) ->
    case length(?MISC:intersect(?Matrix:get_col_non_def_ids(Id1, AttrMatrix),
            ?Matrix:get_col_non_def_ids(Id2, AttrMatrix))) of
        0 -> {def,   0};
        N -> {value, N}
    end.



