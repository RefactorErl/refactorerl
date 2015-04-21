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

%%% @doc Standalone static analyser for lists:map/2 constructs.
%%%
%%% @author Melinda Toth <tothmelinda@caesar.elte.hu>

-module(refanal_list).

-export([analyse/0]).

-include("core.hrl").

analyse()->
    AllFunc = ?Graph:path(?Graph:root(), [{module, {name, '==', lists}}, func]),
    AllFuncRefs = [{{reflib_function:name(Func),
                     reflib_function:arity(Func)},
                    ?Graph:path(Func, [{funeref, back}])} || Func <- AllFunc],
    case lists:keyfind({map,2}, 1, AllFuncRefs) of
        false -> ok;    
        {{map, 2}, MapRefs} -> analyse0(MapRefs)
    end.

analyse0(Refs)->
    Result = try
        analyse_maps(Refs)
    catch 
        _:_ -> ok
    end,
    % semantic objects have changed
    ?FileMan:inc_sem_db_hash(),
    Result.    

analyse_maps([M | Tail])->
    [Pars] = ?Graph:path(M, [{esub, 2}]),
    [FunExpr, List] = ?Graph:path(Pars, [esub]),
    Reach = refanal_message:run(fun()-> ?Dataflow:reach_1st([List], 
                                                        [{back, true}]) 
                                end),
    ListElements = reflib_query:exec(Reach, [{cons_e, back}]),
    case reflib_expression:type(FunExpr) of
        fun_expr -> 
            [Par] = ?Graph:path(FunExpr, [{exprcl, 1}, {pattern, 1}]),
            [?Graph:mklink(E, flow, Par) || E <- ListElements]
    end,
    analyse_maps(Tail);
analyse_maps([]) ->
    ok.
