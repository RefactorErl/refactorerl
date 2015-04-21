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
%%% Module information

%%% @doc This module implements the eliminate unused clause refactoring.
%%%
%%% == Parameters ==
%%% <ul>
%%% <li>The node of the clause to be eliminated </li>
%%% </ul>
%%%
%%% == Conditions of applicability ==
%%% <ul>
%%% <li> The given clause should be unreachable because of 
%%%		 previous patterns.</li>
%%% </ul>
%%%
%%% == Transformation steps and compensations ==
%%% <ol>
%%%  <li>The module finds the required nodes, then eliminates the clause</li>
%%% </ol>
%%%
%%% == Implementation status ==
%%% <ul>
%%%   <li>The transformation can eliminate unused clauses.</li>
%%% </ul>

%%% @author Robert Molnar <elkor@caesar.elte.hu>

-module(reftr_elim_clause).
-vsn("$Rev$").

-export([prepare/1, error_text/2]).

-include("user.hrl").


%%% ============================================================================
%%% Errors

%% @private

error_text(badarg, [Error]) ->
    ["Cannot eliminate clause: ", ?MISC:format("~p",[Error])].

%%% ============================================================================
%%% Callbacks

prepare(Args) ->
    {nodes,ClauseNode} = lists:keyfind(nodes, 1, Args),
    [{_,Parent}]=?Syn:parent(ClauseNode),
    Clauses=?Query:exec(Parent,?Expr:clauses()),
    [FormNode]=?Query:exec(ClauseNode,?Clause:form()),
    File=?Query:exec(FormNode,?Form:file()),
    Eliminable= 
        case Clauses of
            [Clause] ->
                false;
            _ ->
                true
        end,
    if
        Eliminable == true ->
    		[fun() -> ?Syn:replace(Parent,{node,ClauseNode},[]) ,ok end] ++
            	[fun(ok) -> ?Transform:touch(File), [] end];
        Eliminable == false ->
            throw(?LocalError(badarg, ClauseNode))
    end.
