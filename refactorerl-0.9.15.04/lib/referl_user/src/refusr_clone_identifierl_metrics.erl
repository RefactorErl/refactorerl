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

%%% @doc Definitions of different metrics, caching mechanism used by Clone IdentifiErl

%%% @author Viktoria Fordos <f-viktoria@elte.hu>

-module(refusr_clone_identifierl_metrics).
-vsn("$Rev: 9316 $").
-include("user.hrl").
-include("refusr_clone_identifierl.hrl").

% caching
-export([make_cache/2, cache_val/5, free_cache/1]).

% filters for ?TLEClones
-export([filter/2, filter_value/4]).

% metrics for ?FunClones
-export([available_metrics/2, available_sw_filters/1]).
%-compile(export_all).

% filtering constans
-define(simple_exprs, [application, atom, integer, float, char, variable,
                        cons, tuple, record_access, record_update, record_expr,
                        string, record_index, infix_expr]).
-define(simpler_exprs, [atom, integer, float, char, variable, string]).
-define(min_depth, 3).
-define(depth_limit, 10).
-define(complex_expr_limit, ?depth_limit).

%%% ============================================================================
%%% Caching
make_cache(CacheName, KeyPos)->
    ets:new(CacheName, [set, public,
                          {read_concurrency, true},
                          {write_concurrency, true},
                          {keypos, KeyPos}]).

cache_val(CacheName, CacheKey, ValPos, NewElemFun, ValFun)
    when is_integer(ValPos), is_function(NewElemFun, 0), is_function(ValFun,0)->
    case ets:lookup(CacheName, CacheKey) of
        [E] when element(ValPos, E) /= undef,
                 element(ValPos, E) /= undefined ->
            element(ValPos, E);
        R -> E = case R of
                     [EOld] -> EOld;
                     [] -> NewElemFun()
                 end,
            Val = ValFun(),
            NewE = setelement(ValPos,E,Val),
            ets:insert(CacheName, NewE),
            Val
    end.

free_cache(CacheName)->
    ets:delete(CacheName).

%%% ============================================================================
%%% Filters
filter(filters_for_one_element_clones,Storage)->
    filter(filters_for_one_element_clones(),Storage);
filter(filters_for_one_element_clones_light,Storage)->
    filter(filters_for_one_element_clones_light(),Storage);
filter(filters_for_multi_element_clones,Storage)->
    filter(filters_for_multi_element_clones(),Storage);
filter(Key,Storage) when is_number(Key) ->
    filter([Key],Storage);
filter(Keys,Storage) when is_list(Keys) ->
    [Filter || Filter=#filtering_metric{key=K}<-available_filters(Storage), lists:member(K, Keys)].


filter_value(#unit{id=Node}, Key, CalcFun, Storage)->
    filter_value(Node, Key, CalcFun, Storage);

filter_value([Node = {'$gn',_,_}], Key, CalcFun, Storage)->
    filter_value(Node, Key, CalcFun, Storage);

filter_value(Node = {'$gn',_,_}, Key, CalcFun, Storage)->
    cache_val(Storage, Node, Key,
                      fun()-> #filter_values{node=Node} end,
                      fun()->CalcFun(Node) end).


not_equals({other_expr, other_expr}) -> false;
not_equals({other_expr, _}) -> true;
not_equals({_, other_expr}) -> true;
not_equals({Val1=[H1|_], Val2=[H2|_]}) when is_list(H1), is_list(H2)->
    Val1/=Val2;
not_equals({Val1, Val2}) when is_list(Val1), is_list(Val2)->
    lists:usort(Val1)/=lists:usort(Val2);
not_equals({Val1, Val2}) -> Val1/=Val2.

is_simple(Val) -> lists:member(Val, ?simple_exprs).

is_simpler(Val) -> lists:member(Val, ?simpler_exprs).

other_expr_or_cond(Fun) when is_function(Fun,1) ->
    fun({other_expr, other_expr})->
           false;
       ({other_expr, _ }) ->
           true;
       ({_, other_expr }) ->
           true;
       (Exprs)->
           Fun(Exprs)
    end.

expr_or_subexpr_data(Node, Fun, Storage) when is_function(Fun,1) ->
    Expr = case expr_type(Node, Storage) of
               Type when Type == match_expr; Type == send_expr ->
                   [_, RC] = expr_children(Node, Storage),
                   RC;
               _ ->
                   Node
           end,
    Fun(Expr).

filters_for_one_element_clones()->
     [#filter_values.simple_expr,
      #filter_values.max_depth,
      #filter_values.simple_match,
      #filter_values.funapp_same,
      #filter_values.simple_send,
      #filter_values.branching_and_funexp_num_of_cls,
      #filter_values.branching_and_funexp_called_by_hd,
      #filter_values.branching_and_funexp_called_by_cls,
      #filter_values.list_comp_head
     ].

filters_for_multi_element_clones()->
    [#filter_values.funapp_same,
     #filter_values.branching_and_funexp_num_of_cls,
     #filter_values.branching_and_funexp_called_by_hd,
     #filter_values.branching_and_funexp_called_by_cls,
     #filter_values.rs_same,
     #filter_values.list_comp_head,
     #filter_values.recfield_same
    ].

filters_for_one_element_clones_light()->
     [
      #filter_values.recfield_same,
      #filter_values.ratio_funcalls_sets,
      #filter_values.same_binding
     ].


available_filters(Storage)->
    [
    %this filter always have to be first, filtered_suffix_tree uses this
    #filtering_metric{
        key = #filter_values.same_binding,
        calc_fun = fun(Node) ->
                       AllSub0 = ?Lib:deep_sub(Node),
                       AllSub  = [Sub || Sub<-AllSub0, ?Expr:is_leaf(Sub)],
                       ZipSub  = lists:zip(lists:seq(1,length(AllSub)),AllSub),
                       Binds   = [{?Query:exec(Node1,?Expr:varbinds()),N} || {N,Node1}<-ZipSub],
                       Vars    = [{?Query:exec(Node1,?Expr:variables()),N} || {N,Node1}<-ZipSub],
                       InfPatterns = [Sub || Sub<-AllSub0, ?Expr:type(Sub) == infix_expr, ?Expr:role(Sub) == pattern],
                       {Binds, Vars, InfPatterns}
                   end,
        arbitrate_fun = fun({{Bind1,Var1,Pattern1}, {Bind2,Var2,Pattern2}}) ->
                          Binds1 = [V || {V,_}<-Bind1, V /= []],
                          Binds2 = [V || {V,_}<-Bind2, V /= []],
                          case length(Binds1) == length(Binds2) of
                              true ->
                                   Bindings = check_same_binding(Bind1, Bind2, []),
                                   Bind = same_binded_vars(Var1, Var2, Bindings),
                                   Pat  = no_illegal_pattern(Pattern1, Pattern2),
                                   Bind orelse Pat;
                              _ -> true
                          end
                        end},

    #filtering_metric{
        key = #filter_values.ratio_funcalls_sets,
        calc_fun = fun(Node) ->
                       expr_fun_apps(Node, Storage)
                   end,
        arbitrate_fun = fun({FCL1, FCL2}) ->
                               FCCard1 = length(FCL1),
                               FCCard2 = length(FCL2),
                               FCCardCommon = length(FCL1 -- (FCL1 -- FCL2)),
                               case {FCCard1+FCCard2,FCCardCommon} of
                                    {0,0} -> false;
                                    {_,0} -> true;
                                    _ -> ((FCCard1+FCCard2 - 2 * FCCardCommon)/FCCardCommon) /= 0
                                end
                        end},
                        

     #filtering_metric{
        key = #filter_values.simple_expr,
        calc_fun = fun(Node) ->
                        case max_depth1(Node, Storage)>?complex_expr_limit of
                            true-> complex_expr;
                            _  -> expr_type(Node, Storage)
                        end
                   end,
        arbitrate_fun = fun({Val1, Val2}) ->
                               is_simple(Val1) orelse
                                   is_simple(Val2)
                        end},
      #filtering_metric{
        key = #filter_values.simple_match,
        calc_fun = fun(Node) ->
                          case expr_type(Node, Storage) of
                              match_expr ->
                                  [_, RC] = expr_children(Node, Storage),
                                  {match_expr, expr_type(RC, Storage), expr_fun_apps(RC, Storage)};
                              _ ->
                                  other_expr
                          end
                   end,
        arbitrate_fun = other_expr_or_cond(
                          fun({Val1,Val2}) ->
                                 is_simple(element(2, Val1)) orelse
                                     is_simple(element(2, Val2)) orelse
                                     element(3, Val1) /= element(3, Val2)
                          end)},
      #filtering_metric{
        key = #filter_values.simple_send,
        calc_fun = fun(Node) ->
                          case expr_type(Node, Storage) of
                              send_expr ->
                                  [_, RC] = expr_children(Node, Storage),
                                  {send_expr, expr_type(RC, Storage)};
                              _ ->
                                  other_expr
                          end
                   end,
        arbitrate_fun = other_expr_or_cond(
                          fun({Val1,Val2}) ->
                                 is_simple(element(2, Val1)) orelse
                                     is_simple(element(2, Val2))
                          end)},
      #filtering_metric{
        key = #filter_values.branching_and_funexp_num_of_cls,
        calc_fun = fun(Node) ->
                          Fun = fun(N)->
                            case expr_type(N, Storage) of
                                Type when Type == case_expr; Type == if_expr;
                                          Type == fun_expr; Type == receive_expr->
                                    length(clauses(N, Storage));
                                _ ->
                                    other_expr
                            end
                          end,
                          expr_or_subexpr_data(Node, Fun, Storage)
                   end,
        arbitrate_fun = fun not_equals/1},
      #filtering_metric{
        key = #filter_values.branching_and_funexp_called_by_hd,
        calc_fun = fun(Node) ->
                          Fun = fun(N)->
                            case expr_type(N, Storage) of
                                Type when Type == case_expr; Type == if_expr;
                                          Type == fun_expr; Type == receive_expr->
                                    called_by_cls([clauses(N, head, Storage)], Storage);
                                _ ->
                                    other_expr
                            end
                          end,
                          expr_or_subexpr_data(Node, Fun, Storage)
                   end,
        arbitrate_fun = fun not_equals/1},
      #filtering_metric{
        key = #filter_values.branching_and_funexp_called_by_cls,
        calc_fun = fun(Node) ->
                          Fun = fun(N)->
                            case expr_type(N, Storage) of
                                Type when Type == case_expr; Type == if_expr;
                                          Type == fun_expr; Type == receive_expr->
                                    called_by_cls([clauses(N, tail, Storage)], Storage);
                                _ ->
                                    other_expr
                            end
                          end,
                         expr_or_subexpr_data(Node, Fun, Storage)
                   end,
        arbitrate_fun = fun not_equals/1},
      #filtering_metric{
        key = #filter_values.rs_same,
        calc_fun = fun(Node) -> 
                          case expr_type(Node, Storage) of
                              Type when Type == match_expr; Type == send_expr ->
                                  [_, RC] = expr_children(Node, Storage),
                                  {expr_type(RC, Storage), expr_fun_apps(RC, Storage)};
                              _ ->
                                  other_expr
                          end
                   end,
        arbitrate_fun = other_expr_or_cond(
                          fun({{Type1, _}, {Type2, _}}) when Type1/=Type2 ->
                                 not(is_simpler(Type1) andalso is_simpler(Type2));
                             ({{_, FunApps1},{_, FunApps2}}) ->
                                 not_equals({FunApps1, FunApps2})
                          end)},
     #filtering_metric{
        key = #filter_values.funapp_same,
        calc_fun = fun(Node) ->
                          case expr_type(Node, Storage) of
                              application ->
                                  expr_fun_apps(Node, Storage);
                              _ ->
                                  other_expr
                          end
                   end,
        arbitrate_fun = fun not_equals/1},
     #filtering_metric{
        key = #filter_values.list_comp_head,
        calc_fun = fun(Node) ->
                          Fun = fun(N)->
                            case expr_type(N, Storage) of
                                list_comp ->
                                    HDExp = ?Query:exec(N, [{clause, {type, '==', hexpr}}, body]),
                                    NumOfChildren = length(expr_children(HDExp, Storage)),
                                    FunApps = expr_fun_apps(N, Storage),
                                    {NumOfChildren, FunApps};
                                _ ->
                                    other_expr
                            end
                          end,
                          expr_or_subexpr_data(Node, Fun, Storage)
                   end,
        arbitrate_fun = other_expr_or_cond(
                          fun({{NumOfChildren1, _}, {NumOfChildren2, _}})
                               when NumOfChildren1/=NumOfChildren2 ->
                                 true;
                             ({{_, FunApps1},{_, FunApps2}}) ->
                                 not_equals({FunApps1, FunApps2})
                          end)},
     #filtering_metric{
        key = #filter_values.recfield_same,
        calc_fun = fun(Node) ->
                          Fun = fun(N)->
                            case expr_type(N, Storage) of
                                Type when Type == record_index; Type == record_access ->
                                    {non_mod, ?Query:exec(N, ?Expr:fields())};
                                Type when Type == record_expr; Type == record_update ->
                                    {mod, ?Query:exec(N, ?Expr:fields())};
                                _ -> 
                                    {other, ?Query:exec(N, ?Expr:fields())}
                            end
                          end,
                          expr_or_subexpr_data(Node, Fun, Storage)
                   end,
        arbitrate_fun = other_expr_or_cond(
                          fun({{Type1, _}, {Type2, _}}) when Type1/=Type2 ->
                                 true;
                             ({{_, Fields1},{_, Fields2}}) ->
                                 not_equals({Fields1, Fields2})
                          end)},

%TODO: max_depth is very slow. Does a faster algorithm exist?
      #filtering_metric{
        key = #filter_values.max_depth,
        calc_fun = fun(Node) ->
                          max_depth1(Node, Storage)
                   end,
        arbitrate_fun = fun({Depth1,Depth2}) ->
                               Depth1<?min_depth orelse Depth2<?min_depth
                        end}].

available_sw_filters(lay)->
    [#filtering_metric{
        key = #sw_filter_values.num_of_token,
        calc_fun = fun(Node) ->
                        Exprs = ?Query:exec(Node, ?Query:seq([?Fun:definition(),
                                                              ?Form:clauses(),
                                                              ?Clause:body()])),
                         lists:flatlength([?Syn:leaves(E) || E<- Exprs])
                   end,
        arbitrate_fun = fun({Val1, Val2}) ->
                               Val1>=20 andalso Val2>=20
                        end}];

available_sw_filters(exp)->
    [#filtering_metric{
        key = #sw_filter_values.max_depth_of_funcls,
        calc_fun = fun max_depth_of_funcls/1,
        arbitrate_fun = fun({Val1, Val2}) ->
                               Val1>7 andalso Val2>7
                        end}];
available_sw_filters(cf)->
    [#filtering_metric{
        key = #sw_filter_values.ratio_funcalls_sets,
        calc_fun = fun(Node) ->
                        ?Query:exec(Node, ?Fun:funcalls())
                   end,
        arbitrate_fun = fun({FCL1, FCL2}) ->
                               FCCard1 = length(FCL1),
                               FCCard2 = length(FCL2),
                               FCCardCommon = length(FCL1 -- (FCL1 -- FCL2)),
                               case FCCardCommon of
                                    0 -> false;
                                    _ -> ((FCCard1+FCCard2 - 2 * FCCardCommon)/FCCardCommon) < 0.1
                                end
                        end}];

available_sw_filters(name_stricter)->
    [#filtering_metric{
        key = #sw_filter_values.mod_and_fun_pres_alphabet,
        calc_fun = fun(Node) ->
                         ?Alphabet:convert_graph([{subject, [Node]}, {unit, form},
                                                {type, to_mod_and_fun_preserving_alphabet}])
                   end,
        arbitrate_fun = fun({A, B}) ->
                               A == B
                        end},
     #filtering_metric{
        key = #sw_filter_values.softer_max_depth_of_funcls,
        calc_fun = fun max_depth_of_funcls/1,
        arbitrate_fun = fun({Val1, Val2}) ->
                               Val1>5 andalso Val2>5
                        end}];

available_sw_filters(name)->
    [#filtering_metric{
        key = #sw_filter_values.fun_pres_alphabet,
        calc_fun = fun(Node) ->
                         ?Alphabet:convert_graph([{subject, [Node]}, {unit, form},
                                                {type, to_fun_preserving_alphabet}])
                   end,
        arbitrate_fun = fun({A, B}) ->
                               A == B
                        end},
     #filtering_metric{
        key = #sw_filter_values.softer_max_depth_of_funcls,
        calc_fun = fun max_depth_of_funcls/1,
        arbitrate_fun = fun({Val1, Val2}) ->
                               Val1>4 andalso Val2>4
                        end}];


available_sw_filters(Storage)->
    Cats = ?categories,
    [{Cat,
      [{fun(Node) ->
            cache_val(Storage, Node, K,
                      fun()-> #sw_filter_values{node=Node} end,
                      fun()->C(Node) end)
        end, A}
        || #filtering_metric{key=K, calc_fun=C, arbitrate_fun=A} <- available_sw_filters(Cat)]}
    || Cat<-Cats].

%% filter_val(Node, Key)->
%%     Fun = hd([F || #filtering_metric{key=K, calc_fun=F} <- available_filters(), K == Key]),
%%     filter_value(Node, Key, Fun).

called_by_cls(ExprCls, Storage)-> 
    [expr_fun_apps(TLE, Storage) || ExprCl<-ExprCls, TLE<-?Query:exec(ExprCl,?Clause:body())].

max_depth_of_funcls(Func)->
    Cls = ?Query:exec(Func, ?Query:seq(?Fun:definition(),?Form:clauses())),
    lists:foldl(
                fun(Cl, Max) ->
                    case Max >= ?complex_expr_limit of
                        true -> Max;
                        false -> lists:max([Max, max_depth_without_cache(Cl)])
                    end
                end, 1, Cls).

max_depth_without_cache(Node)->
    lists:max(lists:flatten([1, max_depth(children(Node), 1)])).

max_depth1(Node, Storage)->
    filter_value(Node, #filter_values.max_depth,
        fun max_depth_without_cache/1, Storage).

max_depth([], Depth)->
    Depth;
max_depth(_, Depth) when Depth > ?depth_limit ->
    Depth;
max_depth(Nodes, Depth) ->
    [max_depth(children(Node), Depth+cond_inc_based_on_type(Node)) || Node <- Nodes].

% The current representation of mstring is structurally similar to bin. operators.
% However, it's fragments cannot be considered as new levels.
cond_inc_based_on_type(Node)->
    case ?Syn:node_type(Node) of
        expr ->
            case ?Expr:type(Node) of
                mstring -> 0;
                _ -> 1
            end;
        _ -> 1
    end.

children(Node)->
    ChildrenWithLinks = ?Syn:children(Node),
    {_, Children} = lists:unzip(ChildrenWithLinks),
    Children.


expr_type(Node, Storage)->
    filter_value(Node, #filter_values.expr_type,
        fun(N) -> ?Expr:type(N) end, Storage).

expr_fun_apps(Node, Storage)->
    filter_value(Node, #filter_values.expr_fun_apps,
        fun(N) -> ?Query:exec(N, ?Expr:funapps()) end, Storage).

expr_children(Node, Storage)->
    filter_value(Node, #filter_values.expr_children,
        fun(N) -> ?Query:exec(N, ?Expr:children()) end, Storage).

clauses(Node, Storage)->
    filter_value(Node, #filter_values.clauses,
        fun(N) -> ?Query:exec(N,?Expr:clauses()) end, Storage).

clauses(Node, head, Storage)->
    hd(clauses(Node, Storage));
clauses(Node, tail, Storage)->
    lists:nthtail(1,clauses(Node, Storage)).


available_metrics(cf, #storages{cf = Storage})->
    [{fun(Node, Unit) ->
             cache_val(Storage, Node, Key,
                      fun()-> #cf_metric_values{node=Node} end,
                      fun()->CalcFun(Node, Unit) end)
      end, Delta} || #metric{key=Key, calc_fun=CalcFun, delta=Delta}<-cf_metrics()];
available_metrics(exp, #storages{exp = Storage})->
    [{fun(Node, Unit) ->
             cache_val(Storage, Node, Key,
                      fun()-> #exp_metric_values{node=Node} end,
                      fun()->CalcFun(Node, Unit) end)
      end, Delta} || #metric{key=Key, calc_fun=CalcFun, delta=Delta}<-exp_metrics()];
available_metrics(lay, #storages{lay = Storage})->
    [{fun(Node, Unit) ->
             cache_val(Storage, Node, Key,
                      fun()-> #lay_metric_values{node=Node} end,
                      fun()->CalcFun(Node, Unit) end)
      end, Delta} || #metric{key=Key, calc_fun=CalcFun, delta=Delta}<-lay_metrics()];
available_metrics(name, #storages{name = Storage})->
    [{fun(Node, Unit) ->
             cache_val(Storage, Node, Key,
                      fun()-> #name_metric_values{node=Node} end,
                      fun()->CalcFun(Node, Unit) end)
      end, Delta} || #metric{key=Key, calc_fun=CalcFun, delta=Delta}<-name_metrics()].

cf_metrics()->
    [#metric{
        key=#cf_metric_values.num_of_arc,
        calc_fun =
            fun(Node, Type) -> num_of_arcs(Node, Type) end,
        delta=10},
    #metric{
        key=#cf_metric_values.num_of_dec,
        calc_fun=
            fun(Node,Type)-> num_of_decs(Node, Type) end,
        delta=2},
     #metric{
        key=#cf_metric_values.num_of_loops,
        calc_fun=
            fun(Node, Type)->
                   % recursion
                   case mq(is_tail_recursive, Node, Type) of
                        % non-rec.
                        -1 -> 0;
                        % tail-rec. or simply rec.
                        _ -> 1
                   end +
                   % lists module
                   length([F || F<-?Query:exec(tles(Node, Type),
                          ?Query:seq([?Expr:funapps(),[{{func, back}, {name, '==', 'lists'}}]]))]) +
                   % list comp.
                   length([ E || E <- ?Query:exec(tles(Node,Type), ?Expr:deep_sub()),
                  ?Expr:type(E) == list_comp])
            end,
        delta=2},
     #metric{
        key=#cf_metric_values.num_of_exits,
        calc_fun = fun(Node, Type) -> mq(fun_return_points, Node, Type) end,
        delta = 2},
     #metric{
        key=#cf_metric_values.num_of_nodes,
        calc_fun = fun(Node, Type) -> num_of_body_exprs(Node, Type) end,
        delta = 4},
     #metric{
        key=#cf_metric_values.avg_nesting_level,
        calc_fun = fun(Node,Type) ->
                          Maxs = depth_of_structs(Node, Type),
                          case Maxs of
                              []-> 0;
                              _ -> round(lists:sum(Maxs)/length(Maxs))
                          end
                   end,
        delta = 2},
     #metric{
        key=#cf_metric_values.num_of_indep_path,
        calc_fun = fun(Node, Type) -> num_of_indep_path(Node, Type) end,
        delta = 10},
    #metric{
       key=#cf_metric_values.num_of_messpass,
       calc_fun = fun(Node, Type) -> mq(number_of_messpass, Node, Type) end,
       delta = 2},
     #metric{
        key=#cf_metric_values.num_of_throw,
        calc_fun =
            fun(Node,Type) ->
                   Throw = ?Query:exec(?Query:seq(?Mod:find(erlang), ?Fun:find(throw,1))),
                   length([F || F<-?Query:exec(tles(Node, Type), ?Expr:funapps()), F == Throw])
            end,
        delta = 2},
      #metric{
        key=#cf_metric_values.num_of_spawn,
        calc_fun =
            fun(Node,Type) ->
                   Filter = [{func, {{name, '==', spawn}, 'or',
                                    {{name, '==', spawn_link}, 'or',
                                    {name, '==', spawn_monitor}}}}],
                   Spawns = ?Query:exec(?Query:seq(?Mod:find(erlang), Filter)),
                   length([F || F<-?Query:exec(tles(Node, Type), ?Expr:funapps()), lists:member(F, Spawns)])
            end,
        delta = 1}
    ].

exp_metrics()->
    [#metric{
        key=#exp_metric_values.num_of_funcalls,
        calc_fun=
            fun(Node, Type)->
                   Func = func(Node, Type),
                   length([F || F<-?Query:exec(tles(Node, Type), ?Expr:funapps()), F/= Func])
            end,
        delta=5},
     #metric{
        key=#exp_metric_values.num_of_unique_funcalls,
        calc_fun=
            fun(Node, Type)->
                   Func = func(Node, Type),
                    UniqueFuncalls = lists:usort(?Query:exec(tles(Node, Type),
                                                             ?Expr:funapps())),
                   length([F || F <- UniqueFuncalls, F/= Func])
            end,
        delta=2},
     #metric{
        key=#exp_metric_values.avg_complexity_of_decs,
        calc_fun=
            fun(Node, Type)->
                   avg_complexity_of_decs(Node, Type)
            end,
        delta=5}, %TODO: delta?
     #metric{
        key=#exp_metric_values.num_of_var_binds,
        calc_fun=
            fun(Node, clause)->
                   length(?Query:exec(Node, ?Query:seq([?Clause:variables(),
                                                        ?Var:bindings()])));
               (Node, form)->
                   length(?Query:exec(Node, ?Query:seq([?Fun:definition(),
                                                        ?Form:clauses(),
                                                        ?Clause:variables(),
                                                        ?Var:bindings()])))
            end,
        delta=4},
     #metric{
        key=#exp_metric_values.num_of_funexpr,
        calc_fun=
            fun(Node, Type)-> mq(number_of_funexpr, Node, Type) end,
        delta=2},
     #metric{
        key=#exp_metric_values.num_of_records,
        calc_fun =
            fun(Node, Type)->
                   length(lists:usort(?Query:exec(tles(Node, Type), ?Expr:records())))
            end,
        delta=3},
     #metric{
        key=#exp_metric_values.num_of_unused_matching,
        calc_fun =
            fun(Node, Type)->
                    DeepSubs = ?Query:exec(tles(Node, Type), ?Expr:deep_sub()),
                    Vars = ?Query:exec(DeepSubs, ?Query:unique(?Expr:variables())),
                    NumOfUnusedVars = length([R || Var <- Vars,
                        R <- [?Query:exec(Var, ?Var:references())], R == []]),
                    NumOfJokers = length([E || E <- DeepSubs, ?Expr:type(E) == joker]),
                    NumOfJokers + NumOfUnusedVars
            end,
        delta=5},
      #metric{
        key=#exp_metric_values.num_of_guarded_clauses,
        calc_fun =
            fun(Node, Type)->
                   length([E || E <- ?Query:exec(tles(Node,Type),
                        ?Query:seq([?Expr:deep_sub(), ?Expr:clauses(), ?Clause:guard()]))])
            end,
        delta=4},
      #metric{
        key=#exp_metric_values.num_of_dirty_expressions,
        calc_fun =
            fun(Node, Type)->
                   length([E || E <- ?Query:exec(tles(Node,Type),?Expr:deep_sub()),
                                ?Expr:has_side_effect(E)])
            end,
        delta=2},
      #metric{
        key=#exp_metric_values.num_of_infix_ops,
        calc_fun =
            fun(Node, Type)->
                   length([E || E <- ?Query:exec(tles(Node,Type),?Expr:deep_sub()),
                                ?Expr:type(E)==infix_expr])
            end,
        delta=5},
      #metric{
        key=#exp_metric_values.num_of_tles,
        calc_fun =
            fun(Node, Type)->
                   length(tles(Node,Type))
            end,
        delta=5}
    ].

lay_metrics()->
    [#metric{
        key=#lay_metric_values.avg_length_of_var_names,
        calc_fun=
            fun(Node, clause)->
                   VarNamesLength =
                       [length(?Var:name(Var))
                       || Var<-?Query:exec(Node, ?Clause:variables())],
                   case VarNamesLength of
                       [] -> 0;
                       _ -> lists:sum(VarNamesLength) / length(VarNamesLength)
                   end;
               (Node, form)->
                   VarNamesLength =
                       [length(?Var:name(Var))
                       || Var<-?Query:exec(Node, ?Query:seq([?Fun:definition(),
                                                             ?Form:clauses(),
                                                             ?Clause:variables()]))],
                   case VarNamesLength of
                       [] -> 0;
                       _ -> lists:sum(VarNamesLength) / length(VarNamesLength)
                   end
                 end,
        delta=3},
     #metric{
        key=#lay_metric_values.eloc,
        calc_fun=
            fun(Node, Type)-> mq(line_of_code, Node, Type) end,
        delta=5},
     #metric{
        key=#lay_metric_values.vol_of_comment,
        calc_fun=fun(Node,Type)->vol_of_comment(tles(Node, Type)) end,
        delta=10},
     #metric{
        key=#lay_metric_values.num_of_comment,
        calc_fun=fun(Node,Type)->
                        num_of_logical_comment(tles(Node, Type))
                 end,
        delta=5},
     #metric{
        key=#lay_metric_values.num_of_macs,
        calc_fun=
            fun(Node,Type)->
                   Macs = [?Macro:get_macros_under_expr(Exp)
                            ||Exp <- ?Query:exec(tles(Node, Type), ?Expr:deep_sub())],
                   length(lists:usort(lists:flatten(Macs)))
                 end,
        delta=2},
      #metric{
        key=#lay_metric_values.num_of_funcls,
        calc_fun=
            fun(_, clause)->
                        1;
                    (Node, form)->
                        length(?Query:exec(Node, ?Query:seq([?Fun:definition(),
                                                             ?Form:clauses()])))
                 end,
        delta=1},
      #metric{
        key=#lay_metric_values.num_of_funcls_guards,
        % @TODO: maybe the number of the forming exprs of the guard is better?
        calc_fun=
            fun(Node, clause)->
                        num_of_funcl_guards(Node);
                    (Node, form)->
                        lists:sum([num_of_funcl_guards(Cl)
                                    || Cl <- ?Query:exec(Node,
                                                         ?Query:seq([?Fun:definition(),
                                                                     ?Form:clauses()]))])
                 end,
        delta=4},
      #metric{
        key=#lay_metric_values.num_of_tokens,
        calc_fun=
            fun(Node, Type)->
                        lists:flatlength([?Syn:leaves(Expr)||Expr <- tles(Node,Type)])
                 end,
        delta=50}
].

name_metrics()->
    [#metric{
        key=#name_metric_values.name,
        calc_fun=fun(Node, clause)->
                        ?Expr:value(hd(?Query:exec(Node, ?Clause:name())));
                    (Node, form)->
                        ?Fun:name(Node)
                 end,
        delta=0}].

mq(Query, Node, form)->
    refusr_metrics:metric({Query, function, Node});
mq(Query, Node, clause)->
    refusr_metrics:metric({Query, clause, Node}).

num_of_comment(Expr) when not is_list(Expr)->
    num_of_comment([Expr]);
num_of_comment(Exprs)->
    CounterFun = fun({[],[]})-> 0;
                    ({[], _})-> 1;
                    ({_, []})-> 1;
                    ({_,_}) -> 2
                 end,
    lists:sum([CounterFun(Comment) || Comment<- ?Syn:read_comments(Exprs)]).

vol_of_comment(Expr) when not is_list(Expr)->
    vol_of_comment([Expr]);
vol_of_comment(Exprs)->
    AlNum=lists:seq($a, $z) ++ lists:seq($A, $Z) ++ lists:seq($0, $9),
    CounterFun =
        fun({L1,L2})->
               length(lists:filter(fun(E)->lists:member(E, AlNum) end, L1++L2))
        end,
    lists:sum([CounterFun(Comment) || Comment<- ?Syn:read_comments(Exprs)]).

num_of_logical_comment(Exprs)->
    num_of_comment(Exprs).

tles(Node, form)->
    ?Query:exec(Node, ?Query:seq([?Fun:definition(), ?Form:clauses(), ?Clause:body()]));
tles(Node, clause)->
    ?Query:exec(Node, ?Clause:body()).

func(Node, form)->
    [Node];
func(Node, clause)->
    ?Query:exec(Node, ?Query:seq(?Clause:form(),?Form:func())).

avg_complexity_of_decs(Node, Type)->
    DecTypes = [fun_expr, case_expr, receive_expr, block_expr, try_expr, if_expr],
    Complexitys =
        [length(?Query:exec(E,?Expr:clauses()) -- ?Query:exec(E,[headcl]))
            || E <- ?Query:exec(tles(Node,Type), ?Expr:deep_sub()),
               lists:member(?Expr:type(E), DecTypes)],
    case Complexitys of
        []-> 0;
        _ -> lists:sum(Complexitys)/length(Complexitys)
    end.

num_of_decs(Node, form)->
    Cls = ?Query:exec(Node, ?Query:seq([?Fun:definition(), ?Form:clauses()])),
    lists:sum([num_of_decs(Cl, clause) || Cl <- Cls]) + length(Cls)-1;
num_of_decs(Node, Type)->
    DecTypes = [case_expr, if_expr, receive_expr, try_expr],
    length([ E || E <- ?Query:exec(tles(Node,Type), ?Expr:deep_sub()),
                  lists:member(?Expr:type(E), DecTypes)]).

num_of_arcs(Node, form)->
    Cls = ?Query:exec(Node, ?Query:seq([?Fun:definition(), ?Form:clauses()])),
    lists:sum(lists:flatten([num_of_arcs(Cl, clause) || Cl <- Cls])) + length(Cls) + 2;
num_of_arcs(Node, clause)->
    TLEs = tles(Node, clause),
    lists:sum(lists:flatten([arc1([TLE]) || TLE <- TLEs])) + length(TLEs) -1.

arc1(Expr)->
    Cls = ?Query:exec(Expr, ?Expr:clauses()),
    case Cls of
        [] -> [0];
        _  -> [lists:sum(lists:flatten([ arc0(Cl) || Cl <- Cls ]))]
    end.
arc0(Clause)->
    Exprs  = ?Query:exec(Clause, ?Clause:exprs()),
    [lists:sum(lists:flatten([arc1([Expr]) || Expr <- Exprs])) + length(Exprs) -1].

num_of_indep_path(Node, form)->
    Cls = ?Query:exec(Node, ?Query:seq([?Fun:definition(), ?Form:clauses()])),
    lists:sum(lists:flatten([num_of_indep_path(Cl, clause) || Cl <- Cls]));
num_of_indep_path(Node, clause)->
    TLEs = tles(Node, clause),
    lists:foldl(fun(E, AccIn)->
                       E*AccIn
                end, 1 ,lists:flatten([path1([TLE]) || TLE <- TLEs])).

num_of_body_exprs(Node, clause) ->
    length(?Query:exec(Node, [{functx, back}, body]));
num_of_body_exprs(Node, form) ->
    Clauses = ?Query:exec(Node, ?Query:seq([?Fun:definition(),?Form:clauses()])),
    lists:sum([num_of_body_exprs(Cl, clause) || Cl <- Clauses]).


path1(Expr)->
    Cls = ?Query:exec(Expr, ?Expr:clauses()),
    HCls = ?Query:exec(Expr, [headcl]),
    case Cls of
        [] -> [1];
        _  -> [lists:sum(lists:flatten([path0(Cl) || Cl <- (Cls -- HCls)]))]
    end.

path0(Clause)->
    lists:foldl(fun(E, AccIn)->
                       E*AccIn
                end, 1,
                lists:flatten([path1([Expr])
                              || Expr <- ?Query:exec(Clause, ?Clause:exprs())])).

depth_of_structs(Node, Type) ->
    lists:flatten([max_s_depth(Expr, 0) || Expr <- tles(Node, Type)]).

% copied from refusr_metrics.erl
max_s_depth_({Expr, true}, Depth) ->
    Exprs = ?Query:exec(Expr, ?Query:seq(?Expr:clauses(), ?Clause:exprs())),
    [Depth|[max_s_depth(E, Depth + 1) || E <- Exprs, E =/= Expr]];
max_s_depth_({Expr, _}, Depth)->
    Exprs = ?Query:exec(Expr, ?Query:seq(?Expr:clauses(), ?Clause:exprs())),
    [Depth|[max_s_depth(E, Depth ) || E <- Exprs, E =/= Expr]].


max_s_depth(Expr, Depth) ->
    Types = [fun_expr, case_expr, receive_expr, block_expr, try_expr, if_expr],
    [max_s_depth_({E, lists:member(?Expr:type(E), Types)}, Depth)
         || E <- ?Query:exec(Expr, ?Expr:sub())].

num_of_funcl_guards(Clause)->
    length(?Query:exec(Clause,?Clause:guard())).


%% @doc Checks if the bindings does not collide with each other.
check_same_binding([],[],Bindings) -> Bindings;
check_same_binding([{[],_}|BindsA],[{[],_}|BindsB],Bindings) ->
    check_same_binding(BindsA,BindsB,Bindings);
check_same_binding([{[],_}|_],_,_) ->
    true;
check_same_binding(_,[{[],_}|_],_) ->
    true;
check_same_binding([{VarA,Id}|BindsA],[{VarB,Id}|BindsB],Bindings) ->
    case lists:keyfind(VarA,1,Bindings) of
        {VarA,VarB} -> check_same_binding(BindsA,BindsB,Bindings);
        {VarA,_} -> true;
        false -> check_same_binding(BindsA,BindsB,[{VarA,VarB}|Bindings])
    end;
check_same_binding(_,_,_) -> true.


%% @doc Checks if the binded variable's occurences are consistent.
same_binded_vars(_,_,true) ->
    false;
same_binded_vars(Vars1,Vars2,Bindings) ->
    same_binded_vars0(Vars1,Vars2,Bindings).

same_binded_vars0([],[],_) -> false;
same_binded_vars0([{[],_} | Vars1],[{[],_} | Vars2],Bindings) ->
    same_binded_vars0(Vars1,Vars2,Bindings);
same_binded_vars0([{Var1,_} | Vars1],[{[],_} | Vars2],Bindings) ->
    case lists:any(fun({A,_}) -> Var1 == A end, Bindings) of
        true -> true;
        _ -> same_binded_vars0(Vars1,Vars2,Bindings)
    end;
same_binded_vars0([{[],_} | Vars1],[{Var2,_} | Vars2],Bindings) ->
    case lists:any(fun({_,B}) -> Var2 == B end, Bindings) of
        true -> true;
        _ -> same_binded_vars0(Vars1,Vars2,Bindings)
    end;
same_binded_vars0([{Var1,Id} | Vars1],[{Var2,Id} | Vars2],Bindings) ->
    case lists:keyfind(Var1,1,Bindings) of
        {Var1,Var2} -> same_binded_vars0(Vars1,Vars2,Bindings);
        {Var1,_} -> true;
        false -> %have to check back and forth
            case lists:keyfind(Var2,2,Bindings) of
                {Var1,Var2} -> same_binded_vars0(Vars1,Vars2,Bindings);
                {_,Var2} -> true;
                false -> same_binded_vars0(Vars1,Vars2,Bindings)
            end
    end;
same_binded_vars0(_,_,_) -> true.

%% @doc Checks if there is a difference in an infix pattern.
no_illegal_pattern([], []) -> false;
no_illegal_pattern([Pat1 | Pattern1], [Pat2 | Pattern2]) ->
    PSub1 = [{?Expr:type(Sub),?Expr:value(Sub)} || Sub<-?Lib:deep_exprs(Pat1)],
    PSub2 = [{?Expr:type(Sub),?Expr:value(Sub)} || Sub<-?Lib:deep_exprs(Pat2)],

    case no_illegal_pattern0(PSub1, PSub2) of
        true -> true;
        _    -> no_illegal_pattern(Pattern1, Pattern2)
    end.


no_illegal_pattern0([], []) -> false;
no_illegal_pattern0([{Type1, Val1} | PSub1], [{Type2, Val2} | PSub2]) ->
    case Type1 == Type2 andalso Val1 == Val2 of
        true -> no_illegal_pattern0(PSub1, PSub2);
        _    -> true
    end.
