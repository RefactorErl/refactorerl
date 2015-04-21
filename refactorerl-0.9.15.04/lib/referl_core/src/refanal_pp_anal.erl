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

%%% @doc Analyser for the pretty printer (source code formatting) functionality.
%%%
%%% @author Benjamin Somhegyi <soto20@inf.elte.hu>

-module(refanal_pp_anal).
-vsn("$Rev$"). % for emacs"
-behaviour(refcore_anal).

-export([schema/0, externs/1, insert/4, remove/4, update/2]).

-include("core.hrl").
-include("refcore_pp.hrl").

%%% @private
schema() -> [].

%%% @private
externs(_) -> [].

%%% @private
insert(_Parent, _, {_, Child}, _) ->
    format_subtree([Child]).

%TODO: Do we need reformatting when remove/4 is called??
%%% @private
remove(_, _, _, _) ->
    ok.

%%% @private
update(Node,Data) ->
    case Data of
        #form{pp=node} -> format_node(Node);
        _ -> ok
    end.

walk(Fun, [Node | Tail]) ->
    walk(Fun, Fun(Node, ?ESG:data(Node)) ++ Tail);
walk(_, []) ->
    ok.

clear(Expr, #expr{pp=PPAttr}=Data) ->
    case PPAttr of
        node  -> ?Anal:update(Expr, Data#expr{pp=none});
        _     -> ok
    end,
    [Child || {_, Child} <- ?Syn:children(Expr)];
clear(Form, #form{pp=PPAttr}=Data) ->
    case PPAttr of
        node  -> ?Anal:update(Form, Data#form{pp=none});
        _     -> ok
    end,
    [Child || {_, Child} <- ?Syn:children(Form)];
clear(Clause, #clause{pp=PPAttr}=Data) ->
    case PPAttr of
        node  -> ?Anal:update(Clause, Data#clause{pp=none});
        _     -> ok
    end,
    [Child || {_, Child} <- ?Syn:children(Clause)];
clear(_, _) -> [].

format_subtree([Node|Nodes]) ->
    case ?Graph:data(Node) of
        #form{pp=node} ->
            format_node(Node),
            format_subtree(Nodes);
        #clause{pp=node} ->
            format_node(Node),
            format_subtree(Nodes);
        #expr{pp=node} ->
            format_node(Node),
            format_subtree(Nodes);
        _ -> ok
            %%format_subtree([Child || {_,Child} <- ?Syn:children(Node)] ++ Nodes)
    end;
format_subtree([]) -> ok.

format_node(Node) ->
    ?PP:format(Node, Node, ?PP_OPTIONS, ?PPR:erlang()),
    walk(fun clear/2, [Node]).
