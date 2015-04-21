%% -*- coding: latin-1 -*-

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
%%%
%%% ============================================================================
%%% Module information
%%%
%%% @doc Slicing helper analyser module.
%%%
%%% @author Melinda Toth <tothmelinda@caesar.elte.hu>

-module(refanal_slice).
-vsn("$Rev $ ").
-behaviour(refcore_anal).

-export([schema/0, externs/1, insert/4, remove/4, update/2]).

-include("core.hrl").

%%% @private
schema() ->
    [].

%%% @private
externs(_) -> [].

%%% @private
insert(_Parent, _Pre, {_Tag, Child}, _Post) ->
    add_to_db(?RefChangeTab, expr_parent(Child, ?Anal:data(Child))).

%%% @private
remove(Parent, _Pre, {_Tag, _Child}, _Post) ->
    add_to_db(?RefChangeTab, expr_parent(Parent, ?Anal:data(Parent))).

%%% @private
update(Node, _) ->
    add_to_db(?RefChangeTab, expr_parent(Node, ?Anal:data(Node))).

add_to_db(Tab, Nodes) ->
    dets:open_file(Tab, [{type, set}]),
    [dets:insert(Tab, {E, changed}) || E <- Nodes],
    dets:close(Tab).

expr_parent(Node, #expr{}) ->
    [Node];
expr_parent(_Node, #form{}) ->
    [];
expr_parent(_Node, #file{}) ->
    [];
expr_parent(Node, _) ->
    [{_, Parent}] = ?Syn:parent(Node),
    expr_parent(Parent, ?Anal:data(Parent)).
