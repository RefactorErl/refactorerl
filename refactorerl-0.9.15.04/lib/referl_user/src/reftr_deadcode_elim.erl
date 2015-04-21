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

%%% @doc This refactoring eliminates all deadcodes in the selected module, or the database.
%%% This module uses the refusr_deadcode module to find the deadcodes.


%%% == Transformation steps and compensations ==
%%% <ol>
%%% <li>First finds all deadcodes, and saves it in the Res valuable.
%%% Res is a list, which contains tuples, for example: {unused_var,Node}</li>
%%% <li>Then it calls different refactoring to eliminate the deadcodes</li>
%%% </ol>

%%% == Implementation status ==
%%% This refactoring is under development

%%% @author Robert Molnar <elkor@caesar.elte.hu>

-module(reftr_deadcode_elim).
-vsn("$Rev: 11643 $").

-include("user.hrl").

-export([prepare/1,error_text/2]).

%%% ============================================================================
%%% Errors

%% @private

error_text(internal_error, [Error]) ->
    ["Unhandled deadcode type: ", ?MISC:format("~p",[Error])];

error_text(badarg, [Error]) ->
    ["Unhandled deadcode type: ", ?MISC:format("~p",[Error])].

%%% ============================================================================
%%% Callbacks

prepare(Args) ->
    AllTypes=[func, guard, pattern, var, rec, recfield, mac, expr],
    {types,Types}=lists:keyfind(types, 1, Args),
    case Types -- AllTypes of
        [] ->
            ok;
        Error ->
            throw(?LocalError(badarg, Error))
    end,
    case lists:keyfind(file, 1, Args) of
        {file,ModPath} ->
            Res = refusr_deadcode:start(ModPath,nodes);
        false ->
            Res=refusr_deadcode:start(nodes)
    end,

    Res2=lists:filter(fun(I) -> is_intypes(I,Types) end,Res),
    FunRes=elimfun(Res2,Args,[]),
    GuardRes=elimguard(Res2,Args,[]),
    PatternRes=elimpattern(Res2,Args,[]),
    VarRes=elimvar(Res2,Args,[]),
    RenVarRes=renuvars(Res2,Args,[]),
    RecRes=elimrec(Res2,Args,[]),
    RecFieldRes=elimrecfield(Res2,Args,[]),
    MacRes=elimmac(Res2,Args,[]),
    ExprRes=elimexpr(Res2,Args,[]),
    SuccRes=FunRes++GuardRes++PatternRes++VarRes++RenVarRes++
        RecRes++RecFieldRes++MacRes++ExprRes,
    FailRes=Res2--SuccRes,
    io:format("~p / ~p refactoring finished. ~n", [length(Res2)-length(FailRes),length(Res2)]),
    fail_log(FailRes),
    [fun () -> [] end].

%% ============================================================================
%% Checks the validity of a node

is_valid(Node)->
    try
        ?ESG:data(Node),
        true
    catch
        _:_ -> false
    end.

%% ============================================================================
%% Checks if the selected transformation is requested

is_intypes({unused_func,_},Types) ->
    lists:member(func,Types);

is_intypes({unused_guard,_},Types) ->
    lists:member(guard,Types);

is_intypes({unused_pattern,_},Types) ->
    lists:member(pattern,Types);

is_intypes({unused_var,_},Types) ->
    lists:member(var,Types);

is_intypes({unused_rec,_},Types) ->
    lists:member(rec,Types);

is_intypes({unused_recfield,_},Types) ->
    lists:member(recfield,Types);

is_intypes({unused_macro,_},Types) ->
    lists:member(mac,Types);

is_intypes({unused_expr,_},Types) ->
    lists:member(expr,Types);

is_intypes(Error,_) ->
    throw(?LocalError(internal_error, Error)).

%% ============================================================================
%% Creates a log for the failed transformations

fail_log([{Type,Node}|Tl]) ->
    case is_valid(Node) of
        true ->
            FirstToken=hd(?Syn:leaves(Node)),
            File=filename:basename(
                ?File:path(hd(?Query:exec(FirstToken,?Token:file())))),
            {{Line,_},{_,_}}=?Token:linecol(FirstToken),
            io:format("The ~p, in: ~s at line: ~p failed to eliminate ~n",[Type,File,Line]),
            fail_log(Tl);
        false ->
            fail_log(Tl)
    end;

fail_log([]) ->
    ok.

%% ============================================================================
%% Eliminate functions
elimfun([{unused_func, Node}|Tl],Args,Res) ->
    case is_valid(Node) of
        true ->
            NewArgs=[{nodes,Node},{deadcode,true}|Args],
            Res2=?Transform:do(embed,reftr_elim_fun,
                {NewArgs,continue_on_failure,{[],fun(A) -> A end},embed}),
            case Res2 of
                {result,_,_} ->
                    elimfun(Tl, Args, [{unused_func, Node}]++Res);
                _ ->
                   elimfun(Tl, Args, Res)
            end; 
        false ->
            elimfun(Tl, Args, [{unused_func, Node}]++Res)
    end;

elimfun([_|Tl],Args,Res) ->
    elimfun(Tl,Args,Res);

elimfun([], _, Res) -> Res.

%% ============================================================================
%% Eliminate guards
elimguard([{unused_guard, Node}|Tl],Args,Res) ->
    case is_valid(Node) of
        true ->
            NewArgs=[{nodes,Node},{deadcode,true}|Args],
            Res2=?Transform:do(embed,reftr_elim_clause,
                {NewArgs,continue_on_failure,{[],fun(A) -> A end},embed}),
            case Res2 of
                {result,_,_} ->
                    elimguard(Tl, Args, [{unused_guard, Node}]++Res);
                _ ->
                    elimguard(Tl, Args, Res)
            end;
        false ->
            elimguard(Tl, Args, [{unused_guard, Node}]++Res)
    end;

elimguard([_|Tl],Args,Res) ->
    elimguard(Tl,Args,Res);

elimguard([], _, Res) -> Res.

%% ============================================================================
%% Eliminate patterns
elimpattern([{unused_pattern, Node}|Tl],Args,Res) ->
    case is_valid(Node) of
        true ->
            [ClauseNode]=?Query:exec(Node,?Expr:clause()),
            NewArgs=[{nodes,ClauseNode},{deadcode,true}|Args],
            Res2=?Transform:do(embed,reftr_elim_clause,
                {NewArgs,continue_on_failure,{[],fun(A) -> A end},embed}),
            case Res2 of
                {result,_,_} ->
                    elimpattern(Tl, Args, [{unused_pattern, Node}]++Res);
                _ ->
                    elimpattern(Tl, Args, Res)
            end;
        false ->
            elimpattern(Tl, Args, [{unused_pattern, Node}]++Res)
    end;

elimpattern([_|Tl],Args,Res) ->
    elimpattern(Tl,Args,Res);

elimpattern([], _, Res) -> Res.

%% ============================================================================
%% Eliminate variables

elimvar([{unused_var, Node}|Tl],Args,Res) ->
    case is_valid(Node) of
        true ->
            VarNode=?Query:exec(Node,?Expr:variables()),
            case lists:keyfind(file,1,Args) of
                false ->
                    [FileNode]=?Syn:get_file(Node),
                    Path=?File:path(FileNode),
                    [ModuleNode]=?Query:exec(FileNode,?File:module()),
                    Module=?Mod:name(ModuleNode),
                    NewArgs=[{nodes,VarNode},{file,Path},{module,Module}|Args];
                _ ->
                    NewArgs=[{nodes,VarNode}|Args]
            end,
            Res2=?Transform:do(embed,reftr_elim_var,
                {NewArgs,continue_on_failure,{[],fun(A) -> A end},embed}),
            case Res2 of
                {result,_,_} ->
                    elimvar(Tl, Args, [{unused_var, Node}]++Res);
                _ ->
                    elimvar(Tl, Args, Res)
            end;
        false ->
            elimvar(Tl, Args, [{unused_var, Node}]++Res)
    end;

elimvar([_|Tl],Args,Res) ->
    elimvar(Tl,Args,Res);

elimvar([], _, Res) -> Res.

%% ============================================================================
%% Eliminate records

elimrec([{unused_rec, Node}|Tl],Args,Res) ->
    case is_valid(Node) of
        true ->
            NewArgs=[{nodes,Node},{deadcode,true}|Args],
            Res2=?Transform:do(embed,reftr_elim_record,
                {NewArgs,continue_on_failure,{[],fun(A) -> A end},embed}),
            case Res2 of
                {result,_,_} ->
                    elimrec(Tl, Args, [{unused_rec, Node}]++Res);
                _ ->
                    elimrec(Tl, Args, Res)
            end;
        false ->
            elimrec(Tl, Args, [{unused_rec, Node}]++Res)
    end;

elimrec([_|Tl],Args,Res) ->
    elimrec(Tl,Args,Res);

elimrec([], _, Res) -> Res.

%% ============================================================================
%% Eliminate record fields

elimrecfield([{unused_recfield, Node}|Tl],Args,Res) ->
    case is_valid(Node) of
        true ->
            NewArgs=[{nodes,Node},{deadcode,true}|Args],
            Res2=?Transform:do(embed,reftr_elim_recordfield,
                {NewArgs,continue_on_failure,{[],fun(A) -> A end},embed}),
            case Res2 of
                {result,_,_} ->
                    elimrecfield(Tl, Args, [{unused_recfield, Node}]++Res);
                _ ->
                    elimrecfield(Tl, Args, Res)
            end;
        false ->
            elimrecfield(Tl, Args, [{unused_recfield, Node}]++Res)
    end;

elimrecfield([_|Tl],Args,Res) ->
    elimrecfield(Tl,Args,Res);

elimrecfield([], _, Res) -> Res.

%% ============================================================================
%% Eliminate macros

elimmac([{unused_macro, Node}|Tl],Args,Res) ->
    case is_valid(Node) of
        true ->
            NewArgs=[{nodes,Node},{deadcode,true}|Args],
            Res2=?Transform:do(embed,reftr_elim_macro,
                {NewArgs,continue_on_failure,{[],fun(A) -> A end},embed}),
            case Res2 of
                {result,_,_} ->
                    elimmac(Tl, Args, [{unused_macro, Node}]++Res);
                _ ->
                    elimmac(Tl, Args, Res)
            end;
        false ->
            elimmac(Tl, Args, [{unused_macro, Node}]++Res)
    end;

elimmac([_|Tl],Args,Res) ->
    elimmac(Tl,Args,Res);

elimmac([], _, Res) -> Res.

%% ============================================================================
%% Eliminate expressions

elimexpr([{unused_expr, Node}|Tl],Args,Res) ->
    case is_valid(Node) of
        true ->
            NewArgs=[{nodes,Node},{deadcode,true}|Args],
            Res2=?Transform:do(embed,reftr_elim_expr,
                {NewArgs,continue_on_failure,{[],fun(A) -> A end},embed}),
            case Res2 of
                {result,_,_} ->
                    elimexpr(Tl, Args, [{unused_expr, Node}]++Res);
                _ ->
                    elimexpr(Tl, Args, Res)
            end;
        false ->
            elimexpr(Tl, Args, [{unused_expr, Node}]++Res)
    end;

elimexpr([_|Tl],Args,Res) ->
    elimexpr(Tl,Args,Res);

elimexpr([], _, Res) -> Res.

%% ============================================================================
%% Rename unused vars

renuvars([{unused_var, Node}|Tl],Args,Res) ->
    case is_valid(Node) of
        true ->
            case lists:keyfind(file,1,Args) of
                false ->
                    [FileNode]=?Syn:get_file(Node),
                    Path=?File:path(FileNode),
                    [ModuleNode]=?Query:exec(FileNode,?File:module()),
                    Module=?Mod:name(ModuleNode),
                    NewArgs=[{file,Path},{module,Module}|Args];
                _ ->
                    NewArgs=Args
            end,
            Res2=?Transform:do(embed,reftr_rename_unused_vars,
                {NewArgs,continue_on_failure,{[],fun(A) -> A end},embed}),
            case Res2 of
                {result,Vars,_} ->
                    RenVar=lists:filter(fun(I) -> tuple_size(I) == 3 end,Vars),
                    renuvars(Tl, Args, [{unused_var,hd(?Query:exec(Var,?Var:bindings()))}||Var <- RenVar]++Res);
                _ ->
                    renuvars(Tl, Args, Res)
            end;
        false ->
            renuvars(Tl, Args, [{unused_var, Node}]++Res)
    end;

renuvars([_|Tl],Args,Res) ->
    renuvars(Tl,Args,Res);

renuvars([], _, Res) -> Res.
