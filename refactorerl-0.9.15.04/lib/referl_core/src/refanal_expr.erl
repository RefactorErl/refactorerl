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

%%% @doc Expression type and expression structure analyser.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(refanal_expr).
-vsn("$Rev$"). % for emacs"
-behaviour(refcore_anal).

-export([schema/0, externs/1, insert/4, remove/4, update/2]).

-include("core.hrl").

%%% @private
schema() ->
    [{expr, [{top, expr}, {clause, clause}]}].

%%% @private
externs(_) -> [].

%% TODO: pattern clauses in generators?

%%% @private
insert(Parent, Pre, {Tag,Child}, Post) ->
    case ?Anal:data(Parent) of
        #file{} when Tag == form ->
            walk(fun add/3, [{Cl,ok} || {funcl, Cl} <- ?Anal:children(Child)]),
            walk(fun add/3, [{Expr, {Expr, attr}} ||
                                {ET, Expr} <- ?Anal:children(Child),
                                ET == eattr orelse ET == tattr]);
        #form{} when Tag == funcl ->
            walk(fun add/3, [{Child, ok}]);
        #form{} when Tag == eattr; Tag == tattr ->
            walk(fun add/3, [{Child, {Child, attr}}]);
        #clause{} ->
            walk(fun add/3, [{Child, {Child, exprole(Tag)}}]);
        #typexp{} ->
            walk(fun add/3, [{Child, {Child, attr}}]);
        #expr{type=match_expr, role=expr} when Pre == [], Post == [] ->
            wait;
        #expr{type=match_expr, role=expr} when Pre == [] ->
            [Top] = ?Graph:path(Parent, [top]),
            walk(fun add/3, [{Child, {Top, pattern}}]);
        #expr{role=Role} when Tag == esub ->
            [Top] = ?Graph:path(Parent, [top]),
            walk(fun add/3, [{Child, {Top, Role}}]);
        #expr{} ->
            ?Graph:mklink(Parent, {clause, length(Pre)+1}, Child),
            walk(fun add/3, [{Child, ok}])
    end.

%%% @private
remove(Parent, _, {Tag, Child}, _) ->
    case ?Anal:data(Parent) of
        #file{} when Tag == form->
            walk(fun del/3, [{Cl, ok} || {funcl, Cl} <- ?Anal:children(Child)]),
            walk(fun del/3, [{Ex, Ex} ||
                                {ET, Ex} <- ?Anal:children(Child),
                                ET == eattr orelse ET == tattr]);
        #form{} when Tag == funcl ->
            walk(fun del/3, [{Child, ok}]);
        #form{} when Tag == eattr; Tag == tattr ->
            walk(fun del/3, [{Child, Child}]);
        #clause{} ->
            walk(fun del/3, [{Child, Child}]);
        #typexp{} ->
            walk(fun del/3, [{Child, Child}]);
        #expr{} when Tag == esub ->
            [Top] = ?Graph:path(Child, [top]),
            walk(fun del/3, [{Child, Top}]);
        #expr{} ->
            ?Graph:rmlink(Parent, clause, Child),
            walk(fun del/3, [{Child, ok}])
    end.

%%% @private
update(_,_) ->
    ok.

walk(Fun, [{Node, Ctx} | Tail]) ->
    walk(Fun, Fun(Node, ?Anal:data(Node), Ctx) ++ Tail);
walk(_, []) ->
    ok.

add(Expr, #expr{type=Type}=ED, {Top, Role}) ->
    ?Anal:update(Expr, ED#expr{role=Role}),
%io:format("~p: ~p +top+ ~p~n", [element(2, process_info(self(), registered_name)), Expr, Top]),
    ?Graph:mklink(Expr, top, Top),
    case ?Anal:children(Expr) of
        [{esub, P}, {esub, E}] when Type == match_expr, Role == expr ->
            [{P, {Top, pattern}}, {E, {Top, expr}}];
        [{esub, _} |_] = Chld ->
            [{C, {Top, Role}} || {esub, C} <- Chld];
        Chld ->
            [begin
                 ?Graph:mklink(Expr, clause, Cl),
                 {Cl, ok}
             end || {_, Cl} <- Chld]
    end;
add(Clause, #clause{}, _) ->
    [{Expr, {Expr, exprole(Tag)}} || {Tag, Expr} <- ?Anal:children(Clause)];
add(TE, #typexp{}, _) ->
    [{E, {E, attr}} || {_,E} <- ?Anal:children(TE)].


del(Expr, #expr{}, Top) ->
%io:format("~p: ~p -top- ~p~n", [element(2,process_info(self(), registered_name)), Expr, Top]),
%timer:sleep(100),
    ?Graph:rmlink(Expr, top, Top),
    case ?Anal:children(Expr) of
        [{esub, _} | _] = Chld ->
            [{C, Top} || {esub, C} <- Chld];
        Chld ->
            [begin
                 ?Graph:rmlink(Expr, clause, Cl),
                 {Cl, ok}
             end || {_, Cl} <- Chld]
    end;
del(Clause, #clause{}, _) ->
    [{Expr, Expr} || {_, Expr} <- ?Anal:children(Clause)];
del(TE, #typexp{}, _) ->
    [{E, E} || {_, E} <- ?Anal:children(TE)].


exprole(pattern) -> pattern;
exprole(guard) -> guard;
exprole(body) -> expr;
exprole(tmout) -> expr;
exprole(name) -> expr.
