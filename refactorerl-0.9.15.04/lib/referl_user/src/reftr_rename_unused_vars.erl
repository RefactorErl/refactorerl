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

%%% @doc Rename unused variables implementation module. This    refactoring
%%% renames every  unused  variable  to  _VarName (where VarName is the old 
%%% name of the variable). A variable is unused, if it is not referenced in
%%% any expression.

%%% == Parameters ==
%%% <ul>
%%% <li>The name of the module, where unused variables should be 
%%%     renamed.</li>
%%% </ul>

%%% == Conditions of applicability ==
%%% <ul>
%%% <li></li>
%%% </ul>

%%% == Transformation steps and compensations ==
%%% <ol>
%%% <li></li>
%%% </ol>

%%% == Implementation status ==
%%% This refactoring is fully implemented.

%%% @author Gabor Hosszu <hogsabi@inf.elte.hu>

-module(reftr_rename_unused_vars).
-vsn("").

%% Callbacks
-export([prepare/1]).

-include("user.hrl").

%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args) ->
    Module     = ?Args:module(Args),
    Funs       = ?Query:exec(Module, ?Mod:locals()),
    Forms      = lists:concat([ ?Query:exec(Fun, 
                                            ?Fun:definition()) || Fun <- Funs ]),
    DeepExprs  = [ ?Query:exec(Form, ?Form:deep_exprs()) || Form <- Forms],
    DeepVars   = lists:usort(lists:append([ ?Query:exec(Expr, 
                             ?Expr:variables()) || Expr <- DeepExprs ])),
    
    
    VarsToRef  = [ Var || Var <- DeepVars, 
                          get_var_refnum(Var) == 0, 
                          string:left(?Var:name(Var), 1) =/= "_" ],

%%    VarsToRefNames   = [ ?Var:name(Var) || Var <- VarsToRef ], 

%%    ArgsInfo   = add_transformation_info(Args, VarsToRefNames),

    [ fun() -> [] end ] ++ 
    [ rename_var(Var) || Var <- VarsToRef ] ++
    [ fun(List) -> [ hd(?Query:exec(Item, 
                        ?Expr:variables())) || Item <- List]  end ].

%%  add_transformation_info(Args, VarsToRefNames) ->
%%    ?d(VarsToRefNames),
%%    VNames = string:join(VarsToRefNames, ", "),
%%    ?d(VNames),
%%    Info    = ?MISC:format("Renaming unused variables : "
%%                           ++ VNames),
%%    [{transformation_text, Info} | Args].

get_var_refnum(Var) ->
    References  = ?Query:exec(Var,?Var:references()),
    length(References)
. 

rename_var(Var) ->
   	Occs        = ?Query:exec(Var,  ?Var:occurrences()),
	VName = ?Var:name(Var), 	
    fun(List) ->
        ?Macro:inline_single_virtuals(Occs, elex),
        [?Macro:update_macro(VarOcc, {elex, 1}, ("_" ++ VName)) || VarOcc <- Occs],
	[ hd(Occs) | List ]
    end
.

%%% ============================================================================
%%% Checks

