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

%%% @doc This module contains following helper functions for the metric
%%%      analyser:
%%%
%%%       - Node manipulation and metric based functions
%%%       - Ruleset of the `Bad-smell' detecting subsystem

%%% @author Roland Kiraly <kiralyroland@inf.elte.hu>

-module(refanal_metrics_helper).
-vsn("$Rev: 7182 $"). %% "

-include("core.hrl").
-export([bad_smells_after_changes/2, metricmode/0, metricmode/1, 
         metricmode/2, format/2, format/4, add_node/3, find_bad_code/2, 
         create_defs/0, find_bad_code/1, all_metric_errors/0, 
         all_metric_errors/1, metric_process/2, add_node/1]).

-export([error_text/2]).

%-export([addNode/2, lastTransform/0, transform/1, addVal/2]).
%%% ============================================================================
%%% ======= Table names of the DETS based metric subgraph ======================
%%% ============================================================================


-define(Tab, tabname2filename(metric_graph)).
-define(MNodes, tabname2filename(metric_nodelist)).
-define(MTransform, tabname2filename(metric_transform)).
-define(Mode, filename:join(?MISC:data_dir(), 
                            "metricmode"++?MISC:graph_based_postfix()++".dat")).
-define(Defs, filename:join(?MISC:data_dir(), 
                            "metricmode"++?MISC:graph_based_postfix()++".defs")).

%-record(?Tab, {node, data}).
%-record(?MNodes, {node, data}).
%-record(?MTransform, {node, data}).

%%% ============================================================================
%%% ======= Default metric values ==============================================
%%% ============================================================================

%%% ============================================================================
%%%  Protocoll :: {MtericName::atom(), DefaultValue::defval()}
%%%  defval    ::   Value::numeric()
%%%               | {Value::numeric(), Value::numeric()}
%%%               | {Val::numeric(), Rel::rel()}
%%%               | {Val1::numeric, Val2::numeric(), Rel::rel()}
%%%
%%%  rel       :: eq | ne | gt | ge | le | in
%%%  default   :: lt
%%%  in        :: intervall [Val1, Val2]
%%%
%%%  The `metricmode.defs' containes the primary default values (if exists).
%%%  The final default value list merged from the following (`DefaultMetricsMod'
%%%  and `DefaultMetricFuns' macros) and the primary values.
%%%
%%% ============================================================================
%%% ======= Handling relations =================================================
%%% ============================================================================
%%% 
%%%         (Val, eq), (Val, ne), (Val, gt), 
%%%         (Val, lt), (Val, ge), (Val, le), 
%%%         (Val1, Val2, in), 
%%%         ((Val) ::= (Val, lt))
%%% 
%%% ============================================================================ 

-define(DefaultMetricsMod, [{line_of_code, {3,500}},
                            {char_of_code, {20, 40000, in}},
                            {number_of_fun, {10, lt}},
                            {number_of_macros, 10},
                            {number_of_records, 10},
                            {included_files, 5},
                            {imported_modules, 5},
                            {number_of_funpath, 100},
                            {function_calls_in, 10},
                            {function_calls_out, 50},
                            {cohesion, {0, ge}},
                            {otp_used, 10},
                            {mcCabe, 10},
                            {number_of_funexpr, 30},
                            {number_of_messpass, {20, le}},
                            {average_size, {0, ge}},
                            {max_length_of_line, {80, le}},
                            {average_length_of_line, 30},
                            {no_space_after_comma, {1, le}}]).

-define(DefaultMetricsFun, [{line_of_code, 20},
                            {char_of_code, 1600},
                            {max_application_depth, 6},
                            {max_depth_of_cases, 4},
                            {max_depth_of_structs, 4},
                            {number_of_funclauses, 20},
                            {branches_of_recursion, 20},
                            {mcCabe, 10},
                            {calls_for_function, 100},
                            {calls_from_function, 10},
                            {number_of_funexpr, 20},
                            {number_of_messpass, 20},
                            {fun_return_points, 6},
                            {max_length_of_line, 80},
                            {average_length_of_line, 30},
                            {no_space_after_comma, {0, eq}},
                            {is_tail_recursive, {-1, ge}}]).

%%% ============================================================================
%%% ======= Module interface and important functions ===========================
%%% ============================================================================

error_text(dets_error, _) -> "Low level data operation failed".

%%% @private
create_defs(check)->
   case filelib:is_file(?Defs) of
        false -> create_defs();
         true -> ok
   end.

%%% @private
create_defs()->
    {ok, Device} = file:open(?Defs, [write]),
    Data = {{module_metrics, ?DefaultMetricsMod}, 
           {function_metrics, ?DefaultMetricsFun}},
    file:write_file(?Defs, io_lib:fwrite("~p.\n", [Data])),
    close_file(Device),
    metric_default_created.

%%% @private
all_metric_errors(ri) ->
    try metricmode() of
         true -> try lookup(?MTransform, hash) of
                      [{hash, H}] -> case referl_misc:database_hash() of
                                          H -> ok;
                                          _ -> mes(ri)
                                     end;
                      _ -> init(ri)
                 catch
                   _:_-> init(ri)
                 end,
                 ResList = find_bad_code(all, ri),
                 case ResList of
                    [] -> io:format("No bad smells found...");
                     _ ->
                       showtext("~nBadSmell list:~n", [], ri),
                       [begin 
                         io:format("~n~w~n",[{nodename(Node)}]),
                         [io:format("     ~w~n", [Attr0]) || Attr0 <- Attr]
                        end
                        || {Node, Attr} <- ResList]
                  end;
	    false -> []
    catch
       _:_ -> false
    end.

showtext(Text, Data, State) ->
    if 
      State == ri -> 
         io:format(Text, Data);
	true -> ok
    end.
showtext(Text, Data, State, Extra) ->
    case {State, Extra} of
         {unset, extra} ->
               case Data of 
                   [missing_node] -> ok;
                   _ ->
                   io:format(Text, Data)
               end;
	_ -> ok
    end.
         
%% TODO:need to change inactive the status of the refactorerl menus
%%           during the measuring operation
all_metric_errors()->
    try metricmode() of
         true -> try lookup(?MTransform, hash) of
                      [{hash, H}] -> case referl_misc:database_hash() of
                                          H -> ok;
                                          _ -> mes(unset)
                                     end;
                      _           -> init(unset)
                 catch
                   _:_ -> init(unset)
                 end,
                 showtext("please wait...~n",[], unset, extra),
                 format(find_bad_code(all, unset), unset);
	  false -> []
    catch
       _:_ -> false
    end.

%% Note: this transformation should not be run
%% if metrics mode is not turned on in the UI.
%% TODO:need to change inactive the status of the refactorerl menus
%%           during the measuring operation
%%% @private
bad_smells_after_changes(Mod, Args) ->
    try metricmode() of
         true -> init_start(unset),
                 store_transformation(Mod),
                 add_val(file,  proplists:get_value(file, Args)),
                 Show = calculate_metrics(unset),
                 case tab2list(?MNodes) of
                      [] -> [];
                       _ -> erase_data(?MNodes),
                            format(Show, unset)
                 end;
          _ -> []
    catch
      _:_ -> []
    end.

%%% @private
metricmode()->
    try filelib:is_file(?Mode) of
         false -> false;
         true  -> {ok, Device} = file:open(?Mode, [read]),
                  {ok, [Data]} = file:consult(?Mode),
                  close_file(Device),
                  case Data of
                       [{metricmode, on}]  -> true;
                       [{metricmode, off}] -> false
                  end
    catch
      _:_ -> false
    end.

%%% @private
metricmode(on, State) ->
    case metricmode() of
        true -> 
            create_defs(check),
            check_tables(),
            metric_mode_on;
        false -> 
            {ok, Device} = file:open(?Mode, [write]),
            Data = [{metricmode, on}],
            file:write_file(?Mode, io_lib:fwrite("~p.\n", [Data])),
            close_file(Device),
            create_defs(check),
            init(State),
            metric_mode_on
    end.

metricmode(on) ->
    metricmode(on, unset);
metricmode(off) ->
    {ok, Device} = file:open(?Mode, [write]),
    Data = [{metricmode, off}],
    file:write_file(?Mode, io_lib:fwrite("~p.\n", [Data])),
    close_file(Device),
    metric_mode_off.

%%% @private
open_defs()->
   try filelib:is_file(?Defs) of
         false -> {?DefaultMetricsMod, ?DefaultMetricsFun};
         true  -> {ok, Device} = file:open(?Defs, [read]),
                  {ok, [{{module_metrics, DefMods}, 
                        {function_metrics, DefFuns}}]} 
                        = file:consult(?Defs),
                  close_file(Device),
                  {merge_defs(?DefaultMetricsMod, DefMods, []),
                   merge_defs(?DefaultMetricsFun, DefFuns, [])}
   catch
       E1:E2 -> io:format("Wrong definition file (metricmode.defs), using defaults... ~w~n",
                [{E1, E2}]),
                {?DefaultMetricsMod, ?DefaultMetricsFun}
   end.

%%% @private
merge_defs([Def1|Defs], Def2, Acc) ->
    {Key, _Val} = Def1,
    case proplists:get_value(Key, Def2, nokey) of
         nokey -> merge_defs(Defs, Def2, Acc ++ [Def1]);
         FVal ->
               merge_defs(Defs, Def2, Acc ++ [{Key, FVal}])
    end;
merge_defs([], _, Acc) ->
    Acc.

%%% @private
init_start(State) ->
    try lookup(?MTransform, hash) of
         [{hash, H}] -> case referl_misc:database_hash() of
                             H -> ok;
                             _ -> merge_nodes(State)
                        end;
                   _ -> init(State)
    catch
       _:_ -> init(State)
    end.

%%% @private
calculate_metrics(State)->
    {ConnectedModules, ConnectedFunctions} 
                               = fnd_connected_nodes(State),
    Trs = fnd_transform(),
    ruleset(Trs, ConnectedModules, ConnectedFunctions),
    List = find_bad_code(active, State),
    DList = [Ds || {Ds, _} <- tab2list(?Tab)],
    change_status(DList, inactive),
    List.

%%% @private
find_bad_code(all) ->
    find_bad_code(all, unset).

find_bad_code(all, State) ->
    find_bad_code(inactive, State);
find_bad_code(Status, State)->
    try metricmode() of
         true -> {DefMods, DefFuns} = open_defs(),
                 merge_nodes(tab),
                 List = tab2list(?Tab),
                 find_bad_code(List, [], Status, DefMods, DefFuns, State);
         false -> metric_mode_inactive
    catch
       _:_ -> metric_mode_inactive
    end.

%%% @private
find_bad_code([Node|Nodes], BadValues, Stat, DefMods, DefFuns, State) ->
    {Nod, {Metrics, Type, _LastTR, Status}} = Node,
    WrongMetrValues = [check(Metr, Type, DefMods, DefFuns) || Metr
                          <- Metrics, Status == Stat],
    MVals = lists:flatten(WrongMetrValues),
    case MVals of
         [] -> find_bad_code(Nodes, BadValues, Stat, DefMods, DefFuns, State);
          _ -> find_bad_code(Nodes, BadValues ++ [{Nod, MVals}], Stat,
                                                   DefMods, DefFuns, State)
    end;
find_bad_code([], BadValues, _, _, _, _) ->
   case BadValues of
        [] -> [];
        _  -> BadValues
   end.

%%% @private
format(List, State) ->
    KRes = [rpc:async_call(node(), ?MODULE, format,
                          [?Graph:class(Node), Node, ValueList, State]) 
                          || {Node, ValueList} <- List],
    Results = [ _ = rpc:yield(K) || K <- KRes],
    showtext("done~n", [], unset, extra),
    lists:flatten(Results).

%%% @private
format(func, Node, ValueList, _State) ->
    showtext("format ~w ~n",[Node], unset, extra),
    [Module] = ?Query:exec(Node, ?Fun:module()),
    [File]   = ?Query:exec(Module, ?Mod:file()),
    TokenPos = tokenpos(Node, File),
    #file{path = Path} = ?Graph:data(File),
    Data = {Path, ?Fun:name(Node), 
                  ?Fun:arity(Node), TokenPos, ValueList},
    [frmat(outf, Data)];
format(module, Node, ValueList, _State) ->
   showtext("format ~w ~n",[Node], unset, extra),
    case ?Graph:path(Node, [{moddef, back}]) of
        [File] ->
           Data = {(?Graph:data(File))#file.path, 
                  ?Mod:name(Node), ValueList},
             [frmat(outm, Data)];
        _ -> []
    end;
format(_, _, _, _) ->
    [].

%%% @private
tokenpos(Node, File) ->
    case ?Graph:path(Node, [{fundef,back},{funcl, 1}, name, {elex, 1}]) of
        [Token] -> ?Token:pos(File, Token);
        []      -> {0, 0}
    end.

%%% @private
frmat(outf, {File, FunName, Arity, {P1, P2}, ValueList}) ->
    [{nopos, "\n\n Bad metric values of function"},
     {nopos, " "},
     {{File, P1, P2}, atom_to_list(FunName) ++ "/" 
                      ++ integer_to_list(Arity)},
     {nopos, "\n"},
     {nopos, frmat(value, ValueList, [])}];
frmat(outm, {File, Name, ValueList}) ->
    [{nopos, "\n\n Bad metric values of module "},
     {nopos, " "},
     {{File, 1, 1}, ?MISC:to_list(Name)},
     {nopos, "\n"},
     {nopos, frmat(value, ValueList, []), []}].

%%% @private
frmat(value, [Value| ValueList], Acc) ->
      EndData =
      case Value of
             {MName, V0, {{V1, V2}, in}}                  -> 
             ?MISC:format("\n   (~p : ~p) (default : value in [~p, ~p])]",
                                  [MName, V0, V1, V2]);
             {MName, V0, {{V1, V2}, Op}} when is_atom(Op) ->
             ?MISC:format("\n   (~p : ~p) Wrong default [~p, ~p, ~p])",
                                  [MName, V0, V1, V2, Op]);
             {MName, V1, {V2, in}}                        ->
             ?MISC:format("\n   (~p : ~p) wrong default ~p ~p)]",
                                  [MName, V1, V2]);
             {MName, V1, {V2, Op}} when is_atom(Op)       ->
             ?MISC:format("\n   (~p : ~p) (default : value ~p ~p)]",
                                  [MName, V1, Op, V2]);
             {MName, V1, V2}                              -> 
             ?MISC:format("\n    (~p : ~p) (default : ~p)",
                             [MName, V1, V2])
      end,
      frmat(value, ValueList, Acc ++ EndData);
frmat(value, [], Acc) ->
      Acc.

%%% ============================================================================
%%% ======= Init section of the error detecting subsystem ======================
%%% ============================================================================

%% @private
change_status([H|Tl], Stat) ->
    case lookup(?Tab, H) of
       [{Node, {Metrics, Type, Transform, _Status}}] ->
            insert_into_db(
             ?Tab, {Node, {Metrics, Type, Transform, Stat}});
        _ -> ok
    end,
    change_status(Tl, Stat);
change_status([], _) ->
    ok.

%%% @private
all_metrics_for_nodes(TNode, State) ->
    {Nd, {_, Type, LastTrsfm, Status}} = TNode,
    case exists_in_graph(Nd) of
         true -> NdName = nodename(Nd);
         _ ->    NdName = missing_node
    end,
    showtext("~n~w measured...~n",[NdName], State, extra),
    MValues = all_metrics_for_node([], Type, metric_funlist(Type), Nd, State),
    insert_into_db(?Tab, {Nd, {MValues, Type, LastTrsfm, Status}}).

%%% @private
all_metrics_for_node(MetricValues, Type, [Mtr| Mtrs], Node, State) ->
    case refusr_metrics:mtr({Mtr, Type, Node}) of
          {error, _Err}  ->
                  %{refusr_metric, Err},
                  all_metrics_for_node(MetricValues, Type, Mtrs, Node, State);
          {Metr, Value} -> showtext(">", [], State ),
                           all_metrics_for_node(MetricValues ++
                                 [{Metr, Value}], Type, Mtrs, Node, State)%;
% unused case clause detected by Dialyzer
%          _ErrIn         ->
%                  %{refanal_metrics_helper, ErrIn},
%                  all_metrics_for_node(MetricValues, Type, Mtrs, Node, State)
    end;
all_metrics_for_node(MetricValues, _, [], _, _) ->
    MetricValues.

%%% ============================================================================
%%% 
%%%         (Val, eq), (Val, ne), (Val, gt), 
%%%         (Val, lt), (Val, ge), (Val, le), 
%%%         (Val1, Val2, in), 
%%%         ((Val) ::= (Val, lt))
%%% 
%%% ============================================================================ 

%%% @private
check_relation(DefVal, Relation, Value) ->
    case Relation of
         lt -> Value  < DefVal;
         gt -> Value  > DefVal;
         ge -> Value >= DefVal;
         le -> Value =< DefVal;
         ne -> Value /= DefVal;
         eq -> Value == DefVal;
         in -> {V1, V2} = DefVal,
               (Value =< V2)
                   and (Value >= V1);
	  _ -> Value < DefVal 
    end.

%%% @private
check({Name, Value}, Type, DefMods, DefFuns) ->
    Gval = case Type of
             function ->
                proplists:get_value(Name, DefFuns);
             module ->
                proplists:get_value(Name, DefMods)
           end,
    {DefVal, Relation} =
           case Gval of
             {Val1, Val2} when
              is_number(Val1),
              is_number(Val2)   -> {{Val1, Val2}, in};  %{{1,2},in}
              {Val, Rel}        -> {Val, Rel};          %{1, gt}
              {Val1, Val2, Rel} -> {{Val1, Val2}, Rel}; %{{1,2}, lt}
                              _ -> {Gval, lt}           %{1, lt}
          end,
    case check_relation(DefVal, Relation, Value) of
         false -> {Name, Value, {DefVal, Relation}};
         _ -> []
    end.

%%% ============================================================================
%%% ======= Node manipulation and metric based functions =======================
%%% ============================================================================

%%% @private
exists_in_graph(Node) ->
        try ?Graph:data(Node) of
                 _  ->  true
        catch
                 _:_ ->  false
        end.

%%% @private
merge_nodes(tab)->
    [delete_from_db(?Tab, DNode) || {DNode, _DType}
              <- tab2list(?Tab), exists_in_graph(DNode) == false];
merge_nodes(State)->
    merge_nodes(tab),
    AllNodes = [{Node, Type} || {Node, {Type, _, _}}
                               <- tab2list(?MNodes)],
    %Temp
    _NewFuns = [begin
                 add_funs(for_mods, [FNode]),
                 [FFNode] = lookup(?Tab, FNode),
                 all_metrics_for_nodes(FFNode, State)
                end || {FNode, FTyp}
                            <- AllNodes, lookup(?Tab, FNode) == [],
                               FTyp == function],
    %Temp
    _NewMods = [begin
                 add_mods([MNode]),
                 [MMNode] = lookup(?Tab, MNode),
                 all_metrics_for_nodes(MMNode, State)
                end || {MNode, MTyp}
                            <- AllNodes, lookup(?Tab, MNode) == [],
                               MTyp == module],
    [begin
        delete_from_db(?Tab, DNode),
        delete_from_db(?MNodes, DNode)
     end || {DNode, _DType} 
            <- AllNodes, not exists_in_graph(DNode)],
    NewList = tab2list(?MNodes),
    CSNewList = [CSNode || {CSNode, _} <- NewList],
    change_status(CSNewList, active),
    NewList.

%%% @private
fnd_connected_nodes(State)->
   NewList = merge_nodes(State),
   Functions = [Fun || {Fun, {Type, _, _Action}}
                              <- NewList, Type == function],
   Modules   = [Mod || {Mod, {Type, _, _Action}}
                              <- NewList, Type == module],
   case {Modules, lookup(?MTransform, file)} of
         {[], [{file, FileName}] } ->
                  case ?Query:exec(?File:find(FileName)) of
                       [FileNode] -> Modules0 
                            = ?Query:exec(FileNode, ?File:module()),
                               change_status(Modules0, active);
   	                _ -> Modules0 = []
                  end;
         _ -> Modules0 = Modules
   end,
   {Modules0, Functions}.

%%% @private
fnd_transform()->
    case lookup(?MTransform, last) of
         [{_, Transform}] -> Transform;
         _ -> unknown
    end.

%%% @private
refresh_node_data([Mtr| Mtrs], Node, NodeType, MValues) ->
    try refusr_metrics:mtr({Mtr, NodeType, Node}) of
        {error, _Err}  -> %TODO:need some error handling here
                          refresh_node_data(Mtrs, Node, NodeType, MValues);
        {Metr, Value}  -> refresh_node_data(Mtrs, Node, NodeType,
                                              MValues ++ [{Metr, Value}])
    catch
          _Err0:_Err1  -> %TODO:need some error handling here
                          refresh_node_data(Mtrs, Node, NodeType, MValues)

    end;
refresh_node_data([], Node, _, MValues)->
    replace_node_data(Node, MValues).

%%% @private
replace_node_data(Node, MValues) ->
    case lookup(?Tab, Node) of
         [{Nd, {ActMetr, Type, Transform, Status}}] ->
           delete_from_db(?Tab, Node),
           insert_into_db(?Tab, {Nd, {replace(ActMetr, MValues),
                                      Type, Transform, Status}});
	_ -> ok
    end.

%%% @private
replace(OldList, NewList) ->
    MList = [Act || Act <- OldList, not_in(Act, NewList)],
    MList ++ NewList.

%%% @private
not_in(Act, List) ->
    proplists:get_value(element(1, Act), List, nokey) 
    == nokey.
 
%%% ============================================================================
%%% ======= Ruleset and the syntax system of the metrics========================
%%% ============================================================================

%%% @private
ruleset(reftr_rename_fun, CnM, CnF)->
    [refresh_node_data([line_of_code, char_of_code,
                      max_length_of_line, average_length_of_line,
                      no_space_after_comma, is_tail_recursive],
                      NdF, function, []) || NdF <- CnF],
    [refresh_node_data([line_of_code, char_of_code, average_size,
                      max_length_of_line, average_length_of_line,
                      no_space_after_comma],
                      NdM, module, []) || NdM <- CnM];

ruleset(reftr_rename_mod, CnM, _CnF)->
    [refresh_node_data([char_of_code, line_of_code,
                      max_length_of_line, no_space_after_comma],
                      NdM, module, [])|| NdM <- CnM];

ruleset(reftr_rename_macro, CnM, CnF)->
    [refresh_node_data([line_of_code, char_of_code,
                      max_length_of_line, average_length_of_line,
                      no_space_after_comma],
                      NdF, function, [])|| NdF <- CnF],
    [refresh_node_data([char_of_code, line_of_code,
                      max_length_of_line, no_space_after_comma],
                      NdM, module, [])|| NdM <- CnM];

ruleset(reftr_rename_record_field, CnM, CnF)->
     ruleset(reftr_rename_record, CnM, CnF);

ruleset(reftr_rename_record, CnM, CnF) ->
    [refresh_node_data([line_of_code, char_of_code,
                      max_length_of_line, average_length_of_line,
                      no_space_after_comma],
                      NdF, function, [])|| NdF <- CnF],
    [refresh_node_data([line_of_code, char_of_code,
                      max_length_of_line, average_length_of_line,
                      no_space_after_comma],
                      NdM, module, [])|| NdM <- CnM];

ruleset(reftr_rename_header, CnM, _CnF)->
    [refresh_node_data([char_of_code, line_of_code,
                      max_length_of_line, mcCabe,
                      no_space_after_comma],
                      NdM, function, [])|| NdM <- CnM];

ruleset(reftr_reorder_funpar, _CnM, _CnF) ->
     ok;

ruleset(reftr_move_fun, CnM, CnF) ->
    [refresh_node_data([line_of_code, char_of_code, mcCabe,
                      max_length_of_line, average_length_of_line,
                      no_space_after_comma],
                      NdF, function, [])|| NdF <- CnF],
    [refresh_node_data([line_of_code, char_of_code,
                      number_of_fun, number_of_macros,
                      number_of_records, included_files,
                      imported_modules, number_of_funpath,
                      function_calls_in, function_calls_out,
                      cohesion, otp_used, mcCabe, number_of_funexpr,
                      number_of_messpass, average_size,
                      max_length_of_line, average_length_of_line,
                      no_space_after_comma, max_application_depth,
                      max_depth_of_calling, min_depth_of_calling,
                      max_depth_of_cases, max_depth_of_structs,
                      number_of_funclauses],
                      NdM, module, [])|| NdM <- CnM];

ruleset(reftr_move_rec, CnM, CnF) ->
     ruleset(movemacro, CnM, CnF);

ruleset(reftr_move_mac, CnM, CnF) ->
    [refresh_node_data([line_of_code, char_of_code,
                      max_length_of_line, average_length_of_line,
                      no_space_after_comma],
                      NdF, function, [])|| NdF <- CnF],
    [refresh_node_data([line_of_code, char_of_code,
                      number_of_macros, number_of_records,
                      included_files, imported_modules, average_size,
                      max_length_of_line, average_length_of_line,
                      no_space_after_comma, max_depth_of_structs],
                      NdM, module, [])|| NdM <- CnM];

ruleset(reftr_extract_fun,  CnM, CnF) ->
    [refresh_node_data([char_of_code, line_of_code, max_length_of_line,
                      no_space_after_comma],
                      NdF, function, []) || NdF <- CnF],
    [refresh_node_data([line_of_code, char_of_code,
                      number_of_fun, number_of_funpath,
                      cohesion, average_size,
                      max_length_of_line, average_length_of_line,
                      no_space_after_comma, max_application_depth,
                      min_depth_of_calling, max_depth_of_cases,
                      max_depth_of_structs, number_of_funclauses],
                      NdM, module, []) || NdM <- CnM];

ruleset(reftr_introduce_rec, CnM, CnF) ->
    [refresh_node_data([line_of_code, char_of_code, mcCabe,
                     number_of_funexpr, max_length_of_line,
                     average_length_of_line, no_space_after_comma,
                     is_tail_recursive],
                     NdF, function, []) || NdF <- CnF],
    [refresh_node_data([line_of_code, char_of_code,
                     number_of_fun, number_of_macros,
                     number_of_records, included_files,
                     number_of_funpath, cohesion,
                     mcCabe, number_of_funexpr,
                     average_size, max_length_of_line,
                     average_length_of_line, no_space_after_comma],
                      NdM, module, []) || NdM <- CnM];

ruleset(reftr_introduce_import, CnM, CnF) ->
    [refresh_node_data(metric_funlist(function),
                      Nds, function, [])|| Nds <- CnF],
    [refresh_node_data(metric_funlist(module),
                      Nds, module, [])|| Nds <- CnM];

ruleset(reftr_funapp_to_proc, CnM, CnF) ->
    [refresh_node_data(metric_funlist(function),
                      Nds, function, [])|| Nds <- CnF],
    [refresh_node_data(metric_funlist(module),
                      Nds, module, [])|| Nds <- CnM];

ruleset(reftr_inline_fun, CnM, CnF) ->
    [refresh_node_data([line_of_code, char_of_code,
                     max_length_of_line,
                     average_length_of_line, no_space_after_comma],
                      Nds, function, [])|| Nds <- CnF],
    [refresh_node_data([line_of_code, char_of_code,
                     average_size, max_length_of_line,
                     average_length_of_line, no_space_after_comma],
                      Nds, module, [])|| Nds <- CnM];

ruleset(reftr_elim_funcall, CnM, CnF) -> %??
    [refresh_node_data(metric_funlist(function),
                      Nds, function, [])|| Nds <- CnF],
    [refresh_node_data(metric_funlist(module),
                      Nds, module, [])|| Nds <- CnM];

ruleset(reftr_elim_macro, CnM, CnF) -> %?
    [refresh_node_data([line_of_code, char_of_code,
                      max_length_of_line,
                      average_length_of_line, no_space_after_comma],
                      Nds, function, [])|| Nds <- CnF],
    [refresh_node_data([line_of_code, char_of_code,
                     cohesion, average_size, max_length_of_line,
                     average_length_of_line, no_space_after_comma],
                      Nds, module, [])|| Nds <- CnM];

ruleset(reftr_elim_var, CnM, CnF) ->
    [refresh_node_data([line_of_code, char_of_code,
                     max_length_of_line,
                     average_length_of_line, no_space_after_comma],
                      Nds, function, [])|| Nds <- CnF],
    [refresh_node_data([line_of_code, char_of_code,
                     max_length_of_line,
                     average_length_of_line, no_space_after_comma],
                      Nds, module, [])|| Nds <- CnM];

ruleset(reftr_expand_funexpr, CnM, CnF) ->
    [refresh_node_data(metric_funlist(function),
                      Nds, function, [])|| Nds <- CnF],
    [refresh_node_data(metric_funlist(module),
                      Nds, module, [])|| Nds <- CnM];

ruleset(reftr_list_comp, CnM, CnF) ->
    [refresh_node_data(metric_funlist(function),
                      Nds, function, [])|| Nds <- CnF],
    [refresh_node_data(metric_funlist(module),
                      Nds, module, [])|| Nds <- CnM];

ruleset(reftr_upgrade_regexp, CnM, CnF) ->
    [refresh_node_data([line_of_code, char_of_code],
                      Nds, function, [])|| Nds <- CnF],
    [refresh_node_data([line_of_code, char_of_code],
                      Nds, module, [])|| Nds <- CnM];

ruleset(reftr_rename_var, CnM, CnF) ->
    ruleset(reftr_rename, CnM, CnF);

ruleset(reftr_rename, CnM, CnF) ->
    [refresh_node_data(metric_funlist(function),
                      Nds, function, [])|| Nds <- CnF],
    [refresh_node_data(metric_funlist(module),
                      Nds, module, [])|| Nds <- CnM];

ruleset(_, CnM, CnF) ->
    [refresh_node_data(metric_funlist(function),
                      Nds, function, [])|| Nds <- CnF],
    [refresh_node_data(metric_funlist(module),
                      Nds, module, [])|| Nds <- CnM].

metric_funlist(module)->
      [line_of_code, char_of_code,
       number_of_fun, number_of_macros, number_of_records,
       included_files, imported_modules, number_of_funpath,
       function_calls_in, function_calls_out, cohesion,
       otp_used, mcCabe, number_of_funexpr, number_of_messpass,
       average_size, max_length_of_line, average_length_of_line,
       no_space_after_comma];

metric_funlist(function)->
      [line_of_code, char_of_code, 
       max_application_depth,
       max_depth_of_cases, max_depth_of_structs, number_of_funclauses,
       branches_of_recursion, mcCabe, calls_for_function,
       calls_from_function, number_of_funexpr, number_of_messpass,
       fun_return_points, max_length_of_line, average_length_of_line,
       no_space_after_comma, is_tail_recursive].

%%% ============================================================================
%%% ============= MNESIA based and other Node manipulation functions ===========
%%% ============================================================================

%%% @private
nodename(Node)->
    try
        case ?Graph:class(Node) of
    	   func -> #func{name = FName, arity=Arity} 
                       = ?Graph:data(Node),
                       [Mod]  = ?Graph:path(Node,   [{func, back}]),
                       #module{name = FMName} 
                       = ?Graph:data(Mod),
                       {FMName, FName, Arity};
    	 module -> #module{name = MName} 
                       = ?Graph:data(Node),
                       {module, MName}; 
    	      _ -> {node, noname}
        end
    catch
        _:_ -> {node, noname}
    end.

%%% @private
add_val(Key, Value) ->
    insert_into_db(?MTransform, {Key, Value}).

%%% @private
store_transformation(Transform) ->
    insert_into_db(?MTransform, {last, Transform}).

%%% @private
add_node(Table, {Key, Value}, State) ->
    try State of
        unset -> insert_into_db(Table, {Key, Value});
            _ -> ok
    catch
       _:_ -> ok
    end;
add_node(_Table, _, _State) ->
    ok.
add_node(_) ->
    ok.

%%% @private
tab2list(Tab) ->
    OpFun = fun() -> dets:match_object(Tab,{'_','_'}) end,
    run_on_dets(Tab, OpFun).

%%% @private
init(State) ->
    check_tables(),
    mes(State).

%%% @private
erase_data(Tab) ->
    OpFun = fun()-> dets:delete_all_objects(Tab) end,
    run_on_dets(Tab, OpFun).

check_tables()->
    case table_exists(?Tab) of
           false -> table_create(?Tab);
       _ -> ok
    end,
    case table_exists(?MNodes) of
           false -> table_create(?MNodes);
       _ -> ok
    end,
    case table_exists(?MTransform) of
           false ->  table_create(?MTransform);
       _ -> ok
    end.

%%% @private
mes(State) ->
    MS = ?Graph:path(?Graph:root(),
                        [{module, {name, '/=', []}}, 
                         {moddef, back}, moddef]),
    add_mods(MS),
    Keys = [rpc:async_call(node(), ?MODULE, metric_process,
                          [Module, State]) || Module <- MS],
    [ ok = rpc:yield(K) || K <- Keys],
    store_transformation(unknown), 
    insert_into_db(?MTransform, 
                      {hash, referl_misc:database_hash()}),
    ok.

metric_process(Mod, State)->
    ModDat = {Mod, {[], module, [unknown], inactive}},
    insert_into_db(?Tab, ModDat),
    FunsS = ?Graph:path(Mod, [func]),
    List = [{Fun, {[], function, [unknown], inactive}} || Fun <- FunsS],
    insert_into_db(?Tab, List),
    [all_metrics_for_nodes(TNode, State) || TNode <- [ModDat]++List], 
    ok.

%%% @private
add_mods(Modules) ->
    insert_into_db(?Tab, [{Mod, {[], module, [unknown], inactive}}
                         || Mod <- Modules]).

%%% @private
add_funs(for_mods, FunList) ->
    insert_into_db(?Tab, [{Fun, {[], function, [unknown], inactive}} 
                         || Fun <- FunList]).

%%% @private
insert_into_db(_, []) ->
    ok;
insert_into_db(Tab, {Key, Data}) ->
    insert_into_db(Tab, [{Key, Data}]);
insert_into_db(Tab, Objs = [{_, _}|_]) ->
    OpFun = fun() -> dets:insert(Tab, Objs) end,
    run_on_dets(Tab, OpFun).

%%% @private
delete_from_db(Tab, Key) ->
    OpFun = fun()-> dets:delete(Tab, Key) end,
    run_on_dets(Tab, OpFun).

%%% @private
lookup(Tab, Key) ->
    OpFun = fun() ->
                   case dets:lookup(Tab, Key) of 
                       R = [{_, _}] -> R;
                       _ -> []
                   end
            end,
    run_on_dets(Tab, OpFun).

close_file(File) ->
    case file:close(File) of
         ok -> ok;
	 {error, Reason} ->
              io:format("/~s was not closed. Reason :: ~w /",[File, Reason])
    end.

table_exists(Table) ->
    dets:info(Table) =/= undefined.

table_create(Table)->
    dets:open_file(Table, [{type,  set},
                           {file, Table},
                           {keypos, 1},
                           {auto_save, 60000}]),
    dets:close(Table).

tabname2filename(Tab) when is_atom(Tab)->
    filename:join(?MISC:data_dir(), atom_to_list(Tab)++?MISC:graph_based_postfix()).

run_on_dets(Table, OpFun) when is_function(OpFun,0)->
    case dets:open_file(Table,[{type, set},
                           {file, Table},
                           {keypos, 1},
                           {auto_save, 60000}]) of
        {ok, _} -> Result = OpFun(), dets:close(Table), Result;
        {error, _} -> throw(?LocalErr0r(dets_error))
    end.
    
