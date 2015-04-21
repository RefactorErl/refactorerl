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

%%% @doc Root analyser.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

-module(refanal_root).
-vsn("$Rev$"). % for emacs"
-behaviour(refcore_anal).

-export([schema/0, externs/1, insert/4, remove/4, update/2]).

-include("core.hrl").

%%% @private
schema() ->
    {ok, Mods} = application:get_env(form_anal),
    lists:flatmap(fun(M) -> M:schema() end, Mods).

%%% @private
externs(Node) ->
    {ok, Mods} = application:get_env(form_anal),
    case ?Graph:class(Node) of
        file -> [{Form, Mods} || {form, Form} <- ?Anal:children(Node)];
        form -> [{Node, Mods}];
        _    -> [{parent_form(Node), Mods}]
    end.


parent_form(Node) ->
    P = ?Anal:parent(Node),
    case ?Graph:class(P) of
        form -> P;
        _    -> parent_form(P)
    end.

%%% @private
insert(_Parent, _, {_Tag, _Child}, _) ->
    ok. %io:format("ROOT: ~p +++~s+++ ~p~n", [Parent, Tag, Child]).

%%% @private
remove(_Parent,_,{_Tag, _Child},_) ->
    ok. %io:format("ROOT: ~p ---~s--- ~p~n", [Parent, Tag, Child]).

%%% @private
update(_Node, _Data) ->
    ok. %io:format("ROOT: ~p = ~p~n", [Node, Data]).
