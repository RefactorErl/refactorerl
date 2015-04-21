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

%%% @doc This module implements the eliminate unused function refactoring.
%%%
%%% == Parameters ==
%%% <ul>
%%% <li>The function to be eliminated (see {@link reflib_args:function/1})</li>
%%
%%% </ul>
%%%
%%% == Conditions of applicability ==
%%% <ul>
%%% <li> The given function should not be exported
%%%      or referenced anywhere in the module.</li>
%%% </ul>
%%%
%%% == Transformation steps and compensations ==
%%% <ol>
%%%  <li>The module finds the required nodes, then eliminates the function</li>
%%% </ol>
%%%
%%% == Implementation status ==
%%% <ul>
%%%   <li>The transformation can eliminate unused function,
%%%       and gives message if the selected function is used.</li>
%%% </ul>
%%% @author Robert Molnar <elkor@caesar.elte.hu>

-module(reftr_elim_fun).
-vsn("$Rev$").

-export([prepare/1, error_text/2]).

-include("user.hrl").


%%% ============================================================================
%%% Errors

%% @private

error_text(badarg, [Error]) ->
    ["Used function: ", ?MISC:format("~p",[Error])].



%%% ============================================================================
%%% Callbacks

prepare(Args) ->
    {nodes,FormNode}=
        case lists:keyfind(nodes, 1, Args) of
            false ->
                Node  = ?Args:function(Args),
                [FNode]= ?Query:exec(Node,?Fun:definition()),
                {nodes,FNode};
            Found ->
                Found
        end,
    [FunNode]= ?Query:exec(FormNode, ?Form:func()),
    File=?Query:exec(FormNode,?Form:file()),
    Exported=?Fun:is_exported(FunNode),
    Refs=
        case lists:keyfind(deadcode, 1, Args) of
            false ->
                ?Query:exec(FunNode,?Query:all([?Fun:applications(),
                    ?Fun:implicits()]));
            _ ->
                []
        end,
    if 
        Refs==[] andalso not Exported ->
            [fun() -> ?File:del_form(FormNode) end] ++ 
            	[fun(ok) -> ?Transform:touch(File), [] end];
        true ->
            throw(?LocalError(badarg, FunNode))
    end.
