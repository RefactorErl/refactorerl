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
%%% ============================================================================
%%% Module information

%%% @doc This module implements the create server process from function.
%%%
%%% == Parameters ==
%%% <ul>
%%% <li>... (see {@link referl_args:function/1})</li>
%%% <li>... (see {@link referl_args:name/1})</li>
%%% </ul>
%%%
%%% == Conditions of applicability ==
%%% <ul>
%%%   <li> </li>
%%%   <li> </li> 
%%% </ul>
%%%
%%% == Transformation steps and compensations ==
%%% <ol>
%%%  <li></li>
%%%  <li></li>
%%% </ol>
%%%
%%% == Implementation status ==
%%% <ul>
%%%   <li></li>
%%%   <li></li>
%%% </ul>
%%%
%%% @author Atilla Erdodi <erdodi@elte.hu>

-module(reftr_funapp_to_proc).

%% Callbacks
-export([prepare/1]).

-include("user.hrl").

%%% ============================================================================
%%% Callbacks
%% @private
prepare(Args) ->
    FunNode = ?Args:function(Args),
    [FunModule] = ?Query:exec(FunNode, ?Fun:module()),
    ModName = ?Mod:name(FunModule),
    FunName = ?Fun:name(FunNode),
    AppNodes = ?Query:exec(FunNode, ?Fun:applications()), 

    MatchExprFAs = [FunApp || FunApp <- AppNodes, is_match_expr(FunApp)],
    
    MEFAInfos = [{FunApp, {MatchExpr, Clause, VarName, Var, {Body, Index}}} 
                    || FunApp <- MatchExprFAs,
                       {MatchExpr, Clause, VarName, Var, InsertPos} <- [get_binding_info(FunApp)],
                       InsertPos =/= not_used,
                       {Body, Index} <- [InsertPos]],

%% use cases:
%% - return value used in a simple patternmatch 
%%      -> implemented
%% - return value used in a complex patternmatch 
%%      -> use varbind semantic edges instead?
%% - function app. is the last expression in a clause 
%%      -> return rpc ref. or deny transformation?
%% - function app. is not a top level expr
%%      -> extract function app.

%% TODO: check for ReqVarName collision
%%        -> user interaction
    [fun() ->
       [begin
	    ReqConstruct= construct_req(ModName, FunName, FunApp),
	    ReqApp = ?Syn:construct(ReqConstruct),

	    ReqVarName = VarName ++ "_req",
	    ReqVar = ?Syn:construct({var, ReqVarName}),

	    ResConstruct = construct_res(VarName, ReqVarName),
	    ResBind = ?Syn:construct(ResConstruct),
   
	    NewBody = lists_insert(Body, ResBind, Index),

	    ?Syn:replace(MatchExpr, {node, FunApp}, [ReqApp]),	    
	    ?Syn:replace(MatchExpr, {node, Var}, [ReqVar]),
        ?Syn:replace(Clause, body, NewBody), % TODO: move comments!
	    
	    ?Transform:touch(Clause)
		
	end || {FunApp, {MatchExpr, Clause, VarName, Var, {Body, Index}}} <- MEFAInfos]
    end].

%  TODO: modname ==> ?MODULE

%    fun(QModAtoms) ->
%	    [?Macro:atom_to_qmod(QModAtom) || QModAtom <- QModAtoms]
%    end].


get_binding_info(FunApp) ->
   Var = match_expr_var(FunApp),
   VarName = ?Expr:value(Var),
   [MatchExpr] = ?Query:exec(FunApp, ?Expr:parent()),
   [Clause] = ?Query:exec(MatchExpr, ?Expr:clause()),
   InsertPos = get_result_position(Clause, Var),
   {MatchExpr, Clause, VarName, Var, InsertPos}.

get_result_position(Clause, Var) ->
    [Variable] = ?Query:exec(Var, ?Expr:variables()),
    case ?Query:exec(Variable, ?Var:occurrences()) of
    	[_VarBind, VarRef1 | _] -> % patternmatch for first usage
	        [TopVarRef1] = ?Query:exec(VarRef1, ?Expr:top()),		
	        Index = ?Syn:index(Clause, body, TopVarRef1),
        	Body = [Child || {body, Child} <- ?Syn:children(Clause)],
	        {Body, Index - 1};
    	[_VarBind] -> % Variable not used
	        % emit warning?
	        not_used
    end.

construct_req(ModName, FunName, FunApp) ->
    AppArgs = ?Query:exec(FunApp, ?Query:seq(?Expr:child(2), ?Expr:children())),

    NewAppArgs = [proplists:get_value(Arg, ?Syn:copy(Arg)) || Arg <- AppArgs],

    ReqModQ = {{atom, rpc}, ':', {atom, async_call}},    
    ReqAppArgs = [{atom, ModName}, {atom, FunName}, {cons, {list, NewAppArgs}}],

    {app, ReqModQ, ReqAppArgs}.

construct_res(VarName, ReqVarName) ->
    ResModQ = {{atom, rpc}, ':', {atom, yield}},    
    ResApp = {app, ResModQ, [{var, ReqVarName}]},

    {match_expr, {var, VarName}, ResApp}.
    
is_match_expr(FunApp) ->
    case ?Query:exec(FunApp, ?Expr:parent()) of
        [Parent] ->
            case ?Graph:data(Parent) of
    	        {_, match_expr, _, _, _} ->
            	    true;
    	        _ ->
            	    false
            end;
        _ -> false
    end.

match_expr_var(FunApp) ->
    [MatchExpr] = ?Query:exec(FunApp, ?Expr:parent()),
    [Var, _FunApp] = ?Query:exec(MatchExpr, ?Expr:children()),
    Var.

lists_insert([], Element, _Pos) ->
    Element;
lists_insert(List, Element, 0) ->
    [Element | List];
lists_insert([Head | Rest], Element, Pos) when Pos > 0 ->
    [Head | lists_insert(Rest, Element, Pos - 1)].


