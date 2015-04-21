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

%%% @doc This module tests separate sq implementations by comparing their results.
%%% The test is organised as follows.
%%% <ol>
%%% <li>tries to connect the given nodes,</li>
%%% <li>initialise the databases of the given nodes by resetting and adding syntax_tools,</li>
%%% <li>runs all sq queries on the nodes and compares the results.</li>
%%% </ol>
%%%
%%% Be aware of using this test! No request can be run on nodes that executes sq.
%%%
%%% @author Viktória Fordos <f-viktoria@elte.hu>
%%%


-module(reftest_sq_comparator).

-export([test/3]).

-define(timeout, 15*60*1000).

%% @doc Starts testing
%% reftest_sq_comparator:test(rel_small_test@matlab, trunk_small_test@matlab,
%%                            ["mods[name = mnesia].funs.body.macro_value", "mods.funs.arity:sum",
%%                             "mods.funs", "mods", "mods.funs.{calls}2", "mods.funs.{calls}2.name",
%%                             "mods.funs.vars"]).
test(Node1, Node2, QueryList)->
    monitor_node(Node1, true),
    monitor_node(Node2, true),
    KnownNodes = nodes([this, visible]),
    case lists:member(Node1, KnownNodes) andalso
        lists:member(Node2, KnownNodes) of
        true ->
            init([Node1, Node2]),
            run_tests(Node1, Node2, QueryList);
        false -> error
    end.

init(Nodes)->
    [reset_and_load(Node) || Node <- Nodes].

reset_and_load(Node)->
    rpc:call(Node, ri, reset, [], ?timeout),
    %rpc:call(Node, ri, add, ["/home/v/work/erlang/working"], ?timeout).
    rpc:call(Node, ri, add, [usr, syntax_tools], ?timeout).

run_tests(Node1, Node2, QueryList)->
    [eval_test(Node1, Node2, QS) || QS <- QueryList].

eval_test(Node1, Node2, QueryStr)->
    Res1 = transform_query_result(run_sq(Node1, QueryStr)),
    error_logger:info_report([{the_following_query_finished, QueryStr}, {on, Node1}]),
    Res2 = transform_query_result(run_sq(Node2, QueryStr)),
    error_logger:info_report([{the_following_query_finished, QueryStr}, {on, Node2}]),
    case Res1 ==  Res2 of
        true ->
            error_logger:info_report([{query_str, QueryStr}, {result, success}]),
            ok;
        false -> error_logger:error_report([{query_str, QueryStr}, {result, fail},
                                           {result_on_node1, Res1}, {result_on_node2, Res2}]),
                 QueryStr
    end.

run_sq(Node, QueryStr)->
    rpc:call(Node, refusr_sq,run ,[[{output, other}],[], QueryStr], ?timeout).

transform_query_result([]) ->
    {struct, [{"empty", true}]};

transform_query_result([{list, []}]) ->
    {struct, [{"empty", true}]};

transform_query_result([{group_by, _By, eq, Name, _Value} | _Rest]=Input) ->
    transform_query_table(Input, Name);

transform_query_result([{group_by, _By, list, _Items} | _Rest]=Input) ->
    transform_query_grouped(Input);

transform_query_result([{list, _Items}]=Input) ->
    transform_query_list(Input);

transform_query_result([{chain, _, _} | _Rest]=Input) ->
    transform_query_chains(Input);

transform_query_result([{eq, Name, Value}]) ->
    transform_query_eq(Name, Value).

transform_query_table(Input, Name) ->
    {struct, [{"type", "table"},
              {"property", Name},
              {"rows", lists:sort(lists:map(fun transform_query_table_row/1, Input))}]}.

transform_query_table_row({group_by, Item, eq, _Name, Value}) ->
    {struct, [{"value", Value}, transform_query_item_partial(Item)]}.

transform_query_grouped(Input) ->
    {struct, [{"type", "grouped"},
              {"groups", lists:sort(lists:map(fun transform_group/1, Input))}]}.

transform_group({group_by, Header, list, Items}) ->
    {struct, [{"header", transform_query_item(Header)},
              {"rows", transform_query_items_ord(Items)}]}.

transform_query_list([{list, Items}]) ->
    {struct, [{"type", "list"},
              {"rows", transform_query_items_ord(Items)}]}.

transform_query_chains(Input) ->
    {struct, [{"type", "chains"},
              {"chains", lists:sort(lists:map(fun transform_query_chain/1, Input))}]}.

transform_query_chain({chain, Items, WS}) ->
    transform_query_items(Items).

transform_query_eq(Property, Value) ->
    {struct, [{"type", "scalar"},
              {"property", Property},
              {"value", Value}]}.

transform_query_items(Items)->
    lists:map(fun transform_query_item/1, Items).

transform_query_items_ord(Items) ->
    lists:sort(lists:map(fun transform_query_item/1, Items)).

transform_query_item(Item) ->
    transform_query_item_partial(Item).

transform_query_item_partial({Pos, Display}) ->
    {Display, transform_query_pos(Pos)}.

transform_query_pos(nopos) ->
    null;

transform_query_pos({File, 1, 1}) ->
    {struct, [{file, File}]};

transform_query_pos({File, {Start, {StartLine,_}}, {End, _}}) ->
    {struct, [{file, File},
              {start, Start},
              {start_line, StartLine},
              {'end', End}]}.
