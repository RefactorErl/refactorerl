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

%%% @author Lilla Hajós <lya@elte.hu>

-module(refusr_sq).
-vsn("$Rev: 13085 $ ").

-export([run/3]).
-export([format_nodes/2]).
-export([prepare/1, error_text/2]).
-export([closure_worker/5, chains_worker/5, chains_worker_first/5]).
-export([pmap_helper/2]).
-export([process_query/3, process_parallel/1, process_parallel/3]).

-include("user.hrl").
-include("sq_lib.hrl").

-include_lib("stdlib/include/ms_transform.hrl").

-define(Lib, refusr_sq_lib).
-define(Format, refusr_sq_format).


%%% ============================================================================
%%% Errors

error_text(lexical_error, Error) ->
    refusr_sq_lexer:format_error(Error);
error_text(syntax_error, initial) ->
    "syntax error: query must start with initial selector\n"
        "use query '?' for help";
error_text(syntax_error, Error) ->
    refusr_ac_parser:format_error(Error);
error_text(illegal_property, Params) ->
    io_lib:format("illegal ~p property: ~p", Params);
error_text(illegal_selector, Params) ->
    io_lib:format("illegal ~p selector: ~p  ", Params);
error_text(illegal_operator, Params) ->
    io_lib:format("illegal ~p operator: ~p  ", Params);
error_text(bad_type, [Type, Expr]) ->
    io_lib:format("unexpected type (~p) for ~p", [Type, Expr]);
error_text(setop_type_mismatch, [TOp1, TOp2]) ->
    io_lib:format("type mismatch at the set operation: "
        "the type of the second operand is ~s (expected: ~s)", [TOp2, TOp1]);
error_text(type_mismatch, [At, Of, ExpType]) ->
    [ case At of
          {Pl,De} -> io_lib:format("type mismatch at ~p ~p :\n    ", [Pl,De]);
          Pl -> io_lib:format("type mismatch at ~p:\n    ", [Pl])
      end,
      case Of of
          {Ent, Type} -> io_lib:format("the type of ~p is ~s, not ~s",
                                       [Ent, Type, ExpType]);
          Ent -> io_lib:format("the type of ~p is not ~s", [Ent, ExpType])
      end ];
error_text(no_property_in_comparison, []) ->
    "no property in comparison";
error_text(statistics_error, []) ->
    "statistics are only available for properties with numeric values";
error_text(bad_regexp, [])->
    "illegal regexp";
error_text(bad_typing,[]) ->
    "bad grouping type, else the value is less than or equal to 0.";    
error_text(grouping_error, []) ->
    "grouping value run out of the query's length";
error_text(mult_error, []) ->
    "multiplicity must be greater than 0";
error_text(unbound_variable, Variable) ->
    io_lib:format("unbound variable: '~s' must be bound with a selector or a property first",
                  [Variable]);
error_text(already_bound, Variable)->
    io_lib:format("variable is already bound: '~s'", [Variable]);
error_text(unsupported_feature, set_mix) ->
    "set operations do not support the mixing selectors, properties, statistics and chains";
error_text(unsupported_feature, Feature) ->
    io_lib:format("unsupported feature: ~s", [Feature]);
error_text(bind_to_property, [Variable]) ->
    CorrectQuery = io_lib:format("[<property> = ~s]", [Variable]),
    io_lib:format("you can bind '~s' to a property with ~n  ~s",
                  [Variable, CorrectQuery]);
error_text(property_followed, [Property]) ->
    io_lib:format("Property '~s' must be the last element of the top level semantic query.",[query_to_string(Property)]);
error_text(ets_limit_reached, []) ->
    "Query cannot be executed, because system limit (related to the number of ETS tables) would be reached. You might try it again using smaller database.".
    

%%% ============================================================================
%%% Callbacks

%% @spec run(DisplayOpt::proplist(), Params::proplist(), Query::string()) ->
%%           QueryResult::term()
%% @doc Returns the result of `Query' starting from the initial state given by
%%      `Params'. The format of the result is determined by `DisplayOpt'.
%%
%%      `Params' contains either the
%%          - (optional) `file' and `position' keys or
%%          - the `node_list' and (optional) `node_type' key.
%%       The possible values for
%%          - `node_type': file|function|record|field|macro|variable|expression
%%
%%      `DisplayOpt' contains the keys `positions' and `output'.
%%      The possible values for
%%          - `positions': none|scalar|linecol|both
%%          - `output': stdio|{iodev,io_device()}|msg|other|nodes
%%
%%      The `QueryResult' depends on the `output' key in `DisplayOpt'.
%%          - stdio: a formatted text written to stdio
%%          - {iodev,Dev::io_device()}: a formatted text written to Dev
%%          - msg: a message containing a list with the following types of
%%                 elements: {eq, Name, Value} |
%%                           {list, [{Position, Text}]} |
%%                           {chain, [{Position, Text}], PostWS} |
%%                           {group_by, {Position, Text}, eq, Name, Value} |
%%                           {group_by, {Position, Text},
%%                            list, [{Position, Text}]}
%%          - other: the same list that is otherwise sent by a message
%%          - nodes: a proplist with the keys: `nodes' for a list of nodes and
%%                                             `text' for a formatted text
%%      The format of positions depends on the `positions' key in `DisplayOpt'.
%%          - none: nopos
%%          - scalar: {File::string(), PosFrom::integer(), PosTo::integer()}
%%          - linecol: {File::string(), PosFrom::{integer(), integer()},
%%                                      PosTo::{integer(), integer()}}

run(DisplayOpt, Params, Query) when is_list(Query) ->
    Tokens   = tokenize(?Lib:replace_special_chars(Query)),
    SynTree  = parse(Tokens),
    SynTree2 = make_internal_representation(SynTree), %?d(SynTree2),
    Result = process_semantic_query(Params, SynTree2, DisplayOpt),     %?d(Result),
    ?Format:result(Result, DisplayOpt).

make_internal_representation(SynTree) ->
    case refusr_ac:validate_query(SynTree) of
        {ok, Res} ->
            Res;
        {error, E} ->
            throw(?LocalError(syntax_error, E))
    end.

parse(Tokens) ->
    case refusr_ac_parser:parse(Tokens) of
        {ok, SynTree} ->
            SynTree;
        {error, {_, _, Err}} ->
            case lists:flatten(Err) of
                "{quoted," ++ A ->
                    A1 = lists:reverse(tl(lists:reverse(A))),
                    throw(?LocalError(syntax_error, A1));
                _ ->
                    throw(?LocalError(syntax_error, Err))
            end
    end.

tokenize(Query) ->
    case refusr_sq_lexer:string(Query) of
        {ok, Tokens, _} ->
            Tokens;
        {error, {_, _, Error}, _} ->
            throw(?LocalError(lexical_error, Error))
    end.
 
%% @private
prepare(Args) ->
    fun () ->
        [DisplayOpt, StartOpt, QueryStr] =
            ?MISC:pgetu([display_opt, start_opt, querystr], Args),
        run(DisplayOpt, StartOpt, QueryStr)
    end.

%%% ============================================================================
%%% Implementation

process_semantic_query(Params, SemanticQuery, DisplayOpt) ->
    SeqLength = entitynumber_increment(SemanticQuery),
    {GroupByParam, GroupBy} = case proplists:get_value(groupby, DisplayOpt) of
        undefined ->
            {default, SeqLength};
        GroupByOpt ->
            ?Check(is_integer(GroupByOpt) andalso (GroupByOpt > 0),
                   ?LocalError(bad_typing, [])),
            case SeqLength >= GroupByOpt of
                true ->
                    {GroupByOpt, GroupByOpt};
                _ ->
                    case SemanticQuery of
                        [{initial_selector, {set_op, _}} | _] -> 
                            {GroupByOpt, -42};
                        _ ->
                            throw(?LocalError(grouping_error, []))
                    end
            end
    end,
    {Seq, Last} = split_semantic_query(SemanticQuery),
    Skeleton = #state{groupby_place = GroupBy,
                      params = [{groupby, GroupByParam} | Params],
                      semquery = tl(Seq),
                      semquery_last = Last},
    Checked = check_query(SemanticQuery, Skeleton),
    {Query, LastPart} = split_semantic_query(lists:reverse(Checked#state.checked_query)),
    case find_help(Query ++ LastPart) of
        [] -> 
            Init = Skeleton#state{checked_query = [], 
                                  variables = Checked#state.variables},
            Processed = process_full_query(Init, Query, LastPart),
            Distinct = distinct_properties(Processed),
            simplify_prop_query_result(filter_empty_groups(Distinct));
        [Help | _] -> 
            Help
    end.

distinct_properties(State = #state{action = composite_property_query}) ->
    Distinct = lists:map(fun lists:usort/1, res(State)),
    State#state{res = Distinct};
distinct_properties(State) ->
    State.

split_semantic_query(Query = [_]) ->
    {Query, []};
split_semantic_query(Query) ->
    [Last | Seq] = lists:reverse(Query),
    {lists:reverse(Seq), [Last]}.

%% assumes statistics can occur only on top level
process_full_query(InitialState, QuerySeq, [{statistics,_} = Stat]) ->
    Merged = merge_states(process_query(InitialState, QuerySeq, []), false),
    process_last(Stat, Merged);
process_full_query(InitialState, QuerySeq, LastQuery) ->
    merge_states(process_query(InitialState, QuerySeq, LastQuery)).

process_query(InitialState, QuerySeq, QuerySeqLast) ->
    StartState = InitialState#state{semquery_last = QuerySeqLast},
    State = process_query_seq(StartState, QuerySeq),
    Processed = process_last_query(State, QuerySeqLast),
    EveryState = append_parallel_states(Processed),
    QueryBody = QuerySeq ++ QuerySeqLast,
    SeqLength = entitynumber_increment(QueryBody),
    GroupBy = InitialState#state.groupby_place,
    case is_atom(GroupBy) orelse 
        (SeqLength == 0) orelse (SeqLength == GroupBy) of
        true ->
            EveryState;
        _ ->
            lists:map(fun(EndState) ->    
                              grouped_prop_query_naming(EndState, GroupBy,
                                                        QueryBody)
                      end,
                      EveryState)
    end.

process_query_seq(State, QuerySeq) ->
    process_query_seq_worker(State, QuerySeq, QuerySeq).

process_closure_or_iteration_body(State, BodyQuery) ->
    FullQuery = BodyQuery ++ State#state.semquery,
    process_query_seq_worker(State, FullQuery, BodyQuery).

process_query_seq_worker(FirstState, RemainingQuery, QuerySeqToProcess) ->
    {LastState, _, ParallelKeys} =
        lists:foldl(
          fun (QueryElement, {ActualState, RemainingQueryPlusQueryElement,
                              ParallelKeys})->
                  ActualRemainingQuery = tl(RemainingQueryPlusQueryElement),
                  RemainingQueryForBinding =
                      remaining_query(RemainingQueryPlusQueryElement),
                  NewState =
                      process(QueryElement,
                              ActualState#state{
                                semquery = RemainingQueryForBinding,
                                parallel_key = []}),
                  NewKeys = NewState#state.parallel_key,
                  {NewState, ActualRemainingQuery, NewKeys ++ ParallelKeys}
          end,
          {FirstState, RemainingQuery, FirstState#state.parallel_key},
          QuerySeqToProcess),    
    LastState#state{semquery = [], parallel_key = ParallelKeys}.

remaining_query([{closure_seq, QSLst, infinite, Variables} | Query]) ->
    [{closure_seq_cont, QSLst, infinite, Variables} | Query];
remaining_query([{closure_seq, QSLst, Mult, VariablesToBind} | Query]) ->
    [{closure_seq_cont, QSLst, Mult - 1, VariablesToBind} | Query];
remaining_query([{iteration, QSLst, Mult} | Query]) ->
    [{iteration, QSLst, Mult - 1} | Query];
remaining_query(Query) ->
    tl(Query).

process_last_query(State, LastQuery) ->
    {LastState, _, ParallelKeys} =
        lists:foldl(
          fun (QueryElement, {ActualState, RemainingQueryPlusQueryElement,
                             ParallelKeys})->
                  ActualRemainingQuery = tl(RemainingQueryPlusQueryElement),
                  RemainingQueryForBinding =
                      remaining_query(RemainingQueryPlusQueryElement),
                  NewState =
                      process_last(QueryElement,
                                   ActualState#state{
                                     semquery_last = RemainingQueryForBinding,
                                     parallel_key = []}),
                  NewKeys = NewState#state.parallel_key,
                  {NewState, ActualRemainingQuery, NewKeys ++ ParallelKeys}
          end,
          {State, LastQuery, State#state.parallel_key},
          LastQuery),
    LastState#state{semquery_last = [], parallel_key = ParallelKeys}.

is_empty_state(#state{res = Chains}) when is_record(Chains, chains) ->
    #chains{complete = Completes, 
            incomplete = Incompletes, 
            recursive = Recursives} = Chains,
    (Completes == []) andalso (Incompletes == []) andalso (Recursives == []);
is_empty_state(#state{res = []}) ->
    true;
is_empty_state(#state{res = [H | _] = Result}) when is_list(H) ->
    lists:all(fun (Group) -> Group == [] end, Result);
is_empty_state(State) when is_record(State, state) ->
    false.

not_empty_states(States)->
    lists:filter(fun(State) -> not is_empty_state(State) end, States).

is_merged_state(#state{variables = []}) -> false;
is_merged_state(#state{variables = [H | _]}) -> is_list(H).

merge_states(States) ->
    merge_states(States, true).

merge_states([State], _) ->
    State;
merge_states(States = [#state{action = property_query,
                              entitynumber = EntNumber,
                              groupby_place = GbNumber} | _], _)
  when ((GbNumber + 1) == EntNumber) ->
    ?Check(length(ets:all())  < ?ets_limit,
           ?LocalError(ets_limit_reached, [])),
    Results = ets:new(results, [ordered_set]),
    Insert = fun(Entity, Property)->
                     ets:insert_new(Results, {Entity, Property})
            end,
    ProcessState = fun(State, VariableValues) -> 
                           foreach2(Insert,
                                    State#state.groupby_res,
                                    State#state.res),
                           store_variable_values(State, VariableValues)
                   end,
    First = hd(States),
    InitialVariableStore = initial_store(First#state.variables),
    CollectedVariableValues = lists:foldl(ProcessState, InitialVariableStore,
                                          States),
    {Entities, Properties} = lists:unzip(ets:tab2list(Results)),
    ets:delete(Results),
    First#state{
      res = Properties,
      groupby_res = Entities,
      variables = CollectedVariableValues};
merge_states(States = [#state{action = Action} | _], _) 
  when Action == iteration orelse Action == closure ->
    First = hd(States),
    InitialVariableStore = initial_store(First#state.variables),
    Empty = gb_sets:empty(),
    Add = fun (Chain, Results) ->
                  lists:foldl(fun gb_sets:add_element/2, Results, Chain) end,
    {Complete, Incomplete, Recursive, CollectedVariableValues} = 
        lists:foldl(fun(State, {Complete, Incomplete, Recursive, VariableValues})->
                            Chain = State#state.res,
                            {Add(Chain#chains.complete, Complete),
                             Add(Chain#chains.incomplete, Incomplete),
                             Add(Chain#chains.recursive, Recursive),
                             store_variable_values(State, VariableValues)}
                    end,
                    {Empty, Empty, Empty, InitialVariableStore},
                    States),
    First#state{res = #chains{complete = gb_sets:to_list(Complete),
                              incomplete = gb_sets:to_list(Incomplete),
                              recursive = gb_sets:to_list(Recursive)},
                variables = CollectedVariableValues};
merge_states(States = [#state{action = Action,
                              groupby_type = []} | _], Distinct) 
  when Action == selection orelse Action == property_query ->
    First = hd(States),
    VariablesInitialStore = initial_store(First#state.variables),
    InitStore = if Distinct -> gb_sets:empty();
                   true -> []
                end,
    Union = if Distinct -> 
                    fun (#state{res = Entities}, Set) ->
                            SetOfState = gb_sets:from_list(Entities),
                            gb_sets:union(Set, SetOfState) end;
               true -> 
                    fun (#state{res = Entities}, Set) ->
                            Entities ++ Set
                    end
            end,
    PostProc = if Distinct -> fun gb_sets:to_list/1;
                  true -> fun lists:reverse/1
               end,
    {Entities, CollectedVariableValues} = 
        lists:foldl(fun (State, {Results, VariableValues}) ->
                            {Union(State, Results),
                             store_variable_values(State, VariableValues)} end,
                    {InitStore, VariablesInitialStore},
                    States),
    First#state{res = PostProc(Entities),
                variables = CollectedVariableValues};
merge_states(States, Distinct) ->
    Union = if Distinct -> 
                    fun (Set, List) ->
                            ordsets:union(Set, ordsets:from_list(List)) end;
               true ->
                    fun (Merged, NewRes) ->
                            NewRes ++ Merged end
            end,
    Append =
        fun(_GroupingEntity, [], Results) ->
                Results;
           (GroupingEntity, Group, Results) ->
                case gb_trees:is_defined(GroupingEntity, Results) of
                    true ->
                        OldGroup = gb_trees:get(GroupingEntity, Results),
                        NewGroup = Union(OldGroup, Group),
                        gb_trees:update(GroupingEntity, NewGroup, Results);
                    false ->
                        OrderedGroup = ordsets:from_list(Group),
                        gb_trees:insert(GroupingEntity, OrderedGroup, Results)
                end
        end,
    First = hd(States),
    InitialVariableStore = initial_store(First#state.variables),
    {Results, CollectedVariableValues} =
        lists:foldl(
          fun(State, {Results, VariableValues}) ->
                  #state{res = Groups, groupby_res = GroupEntities} = State,
                  {foldl2(Append, Results, GroupEntities, Groups), 
                   store_variable_values(State, VariableValues)}
          end,
          {gb_trees:empty(), InitialVariableStore},
          States),
    {GroupingEntities, EntityGroups} =
        lists:unzip(gb_trees:to_list(Results)),
    to_composite_prop_query(
        First#state{
            res = EntityGroups,
            groupby_res = GroupingEntities,
            variables = CollectedVariableValues}).

to_composite_prop_query(#state{res = []} = State) -> State;
to_composite_prop_query(#state{action = property_query} = State) ->
    #state{res = Result, type = Type} = State,
    IsGroupedResult =
        fun ([Head | _]) ->
                case Type of
                    string -> is_list(Head);
                    _ -> true
                end;
            (_) -> false
        end,
    case lists:any(IsGroupedResult, Result) of
        true -> State#state{action = composite_property_query};
        false -> State
    end;
to_composite_prop_query(State) -> State.

initial_store([]) ->
    [];
initial_store([H | _] = Variables) ->
    if is_list(H) -> Variables;
       true -> [Variables]
    end.

store_variable_values(#state{res = []}, Store) ->
    Store;
store_variable_values(#state{variables = []}, Store) ->
    Store;
store_variable_values(#state{variables = Vars}, Store) when is_list(hd(Vars))->
    Vars ++ Store;
store_variable_values(#state{variables = Vars}, Store) ->
    [Vars | Store].

collect_variable_values([#state{variables = Variables} | _] = States) ->
    InitialStore = initial_store(Variables),
    lists:foldl(fun store_variable_values/2, InitialStore, States).    

foreach2(_Fun, [], []) ->
    ok;
foreach2(Fun, [ElemA | ListA], [ElemB | ListB]) when is_function(Fun, 2) ->
    Fun(ElemA, ElemB),
    foreach2(Fun, ListA, ListB).

foldl2(_Fun, Acc, [], [])->
    Acc;
foldl2(Fun, Acc, [ElemA | ListA], [ElemB | ListB]) when is_function(Fun, 3)->
    foldl2(Fun, Fun(ElemA, ElemB, Acc), ListA, ListB).

find_help(E) when is_list(E) ->
    lists:flatmap(fun find_help/1, E);
find_help({help, _} = H) ->
    [H];
find_help(_) ->
    [].

print(Query) ->
    lists:flatmap(fun query_to_string/1, Query).

grouped_prop_query_naming(State, Groupby, QuerySeq) ->
    case is_property_query(State) of
        true ->
            AfterGrouping = drop(Groupby, QuerySeq),
            QueryName = print(AfterGrouping),
            State#state{name = QueryName};
        _ ->
            State
    end.

drop(_, []) -> 
    [];
drop(N, Query) when 0 >= N ->
    Query;
drop(N, Query) ->
    IsFilter = fun({filter, _}) -> true;
                  ({filter_with_variable_to_bind, _, _}) -> true;
                  ({variable_match, _, _}) -> true;
                  ({variable_bind, _}) -> true;
                  (_) -> false
               end,
    DroppedFilters = lists:dropwhile(IsFilter, Query),
    drop(N - 1, lists:dropwhile(IsFilter, tl(DroppedFilters))).

query_to_string({selector, Sel}) ->
    "." ++ atom_to_list(Sel);
query_to_string({selector, Sel, _}) ->
    query_to_string({selector, Sel});
query_to_string({property, Property, _}) ->
    "." ++ atom_to_list(Property);
query_to_string({initial_selector, Sel}) ->
    "." ++ atom_to_list(Sel);
query_to_string({iteration, Sels, Mult}) ->
    Converted = lists:flatmap(fun query_to_string/1, Sels),
    lists:flatten(io_lib:format(".{~s}~B", [tl(Converted), Mult]));
query_to_string({closure, Sels, infinite}) ->
    Converted = lists:flatmap(fun query_to_string/1, Sels),
    lists:flatten(io_lib:format(".(~s)+", [tl(Converted)]));
query_to_string({closure, Sels, Mult}) ->
    Converted = lists:flatmap(fun query_to_string/1, Sels),
    lists:flatten(io_lib:format(".(~s)~B", [tl(Converted), Mult]));
query_to_string({closure_seq, Sels, Mult, _}) ->
    query_to_string({closure, Sels, Mult});
query_to_string({closure_seq_cont, Sels, Mult, _}) ->
    query_to_string({closure, Sels, Mult});
query_to_string({set_op, {_Setop, Op1, Op2}}) ->
    %% breaks tests in refusr_sq_variables_tests:
    %% setop_to_string(Operation);
    case Op1 of
        [{initial_selector, _} |_] ->
            case is_list(Op2) of
                true -> print(Op2);
                false -> query_to_string(Op2)
            end;
        _ when is_list(Op1) ->
            case io_lib:printable_list(Op1) of
                true -> Op1;
                false -> print(Op1)
            end;                    
        _ ->
            to_string(Op1)                
    end;
query_to_string({cons, Literals}) ->
    Strings = lists:map(fun to_string/1, Literals),
    Punctuated = string:join(Strings, ","),
    lists:flatten(io_lib:format("|~s|", [Punctuated]));
query_to_string({variable, Variable}) ->
    "." ++ Variable;
query_to_string({variable_match, CompOp, Variable}) ->
    lists:flatten(io_lib:format("~s~s", [CompOp, Variable]));
query_to_string({variable_bind, Variable}) ->
    lists:flatten(io_lib:format("->~s", [Variable]));
query_to_string({filter, _Filter}) ->
%%    Converted = query_to_string(Filter),
%%    lists:flatten(io_lib:format("[~s]", [Converted]));
    "";
query_to_string({filter_with_variable_to_bind, _, _}) ->
    "";
query_to_string({'and', Filter1, Filter2}) ->
    and_to_string(Filter1, Filter2);
query_to_string({'or', Filter1, Filter2}) ->
    or_to_string(Filter1, Filter2);
query_to_string({'not', Filter}) ->
    Converted = query_to_string(Filter),
    lists:flatten(io_lib:format("not (~s)", [Converted]));
query_to_string({seq, Seq}) ->
    print(Seq);
query_to_string({seq_with_variable, Seq}) ->
    print(Seq);
query_to_string({quoted, X}) ->
    to_string(X);
query_to_string({statistics, Stat}) ->
    lists:flatten(io_lib:format(":~p", [Stat]));
query_to_string({help, queries}) ->
    ".?";
query_to_string({help, filters}) ->
    "?";
query_to_string(Comparison) when is_tuple(Comparison) ->
    compop_to_string(Comparison);
query_to_string(X) ->
    to_string(X).

% higher value means higher priority
priority('or')  -> 1;
priority('and') -> 2;
priority(like) -> 3;
priority(in) -> 3;
priority(set_op) -> 3;
priority('not') -> 4;
priority(compop) -> 5;
priority(_) -> 5.

and_to_string(Filter1, Filter2) ->
    Format1 = formatstring('and', Filter1),
    Format2 = formatstring('and', Filter2),
    Converted1 = query_to_string(Filter1),
    Converted2 = query_to_string(Filter2),
    lists:flatten(io_lib:format(Format1 ++ ", " ++ Format2,
                                [Converted1, Converted2])).

or_to_string(Filter1, Filter2) ->
    Format1 = formatstring('or', Filter1),
    Format2 = formatstring('or', Filter2),
    Converted1 = query_to_string(Filter1),
    Converted2 = query_to_string(Filter2),
    lists:flatten(io_lib:format(Format1 ++ "; " ++ Format2,
                                [Converted1, Converted2])).

compop_to_string({CompOp, Op1, Op2}) ->
    PrettyOp1 = query_to_string(Op1),
    PrettyOp2 = query_to_string(Op2),
    Format = fun(Op) -> if is_tuple(Op) ->
                                "(~s)";
                           true ->
                                "~s"
                        end
             end,
    Format1 = Format(Op1),
    Format2 = Format(Op2),
    lists:flatten(io_lib:format(Format1 ++ " ~s " ++ Format2,
                                [PrettyOp1, CompOp, PrettyOp2])).

%% setop_to_string({Setop, Q1, Q2}) when Setop == all_in orelse
%%                                       Setop == any_in ->
%%        PrettyQ1 = if is_list(Q1) ->
%%                        lists:flatmap(fun query_to_string/1, Q1);
%%                   true ->
%%                        query_to_string(Q1)
%%                end,
%%     PrettyQ2 = if is_list(Q2) ->
%%                        lists:flatmap(fun query_to_string/1, Q2);
%%                   true ->
%%                        query_to_string(Q2)
%%                end,
%%     lists:flatten(io_lib:format("~s ~p ~s", [PrettyQ1, Setop, PrettyQ2]));
%% setop_to_string({Setop, Q1, Q2}) ->
%%     PrettyQ1 = if is_list(Q1) ->
%%                        print(Q1);
%%                   true ->
%%                        query_to_string(Q1)
%%                end,
%%     PrettyQ2 = if is_list(Q2) ->
%%                        print(Q2);
%%                   true ->
%%                        query_to_string(Q2)
%%                end,
%%     PrettySetop = case Setop of
%%                       union -> "U";
%%                       intersect -> "I";
%%                       minus -> "--"
%%                   end,
%%     lists:flatten(io_lib:format("(~s ~s ~s)", [PrettyQ1, PrettySetop, PrettyQ2])).

formatstring(Operator, Operand) ->
    case priority(element(1, Operand)) < priority(Operator) of
        true ->
            "(~s)";
        false ->
            "~s"
    end.

merged_type(Type1, Type2) ->
    case Type1 of
        Type2 -> Type2;
        _ ->
            case is_property_type(Type1) andalso is_property_type(Type2) of
                true -> string;
                _ -> 
                    case {Type1, Type2} of
                        {any, T} -> T;
                        {T, any} -> T;
                        {help, _} -> help;
                        {_, help} -> help;
                        _ -> null
                    end
            end
    end.

is_property_type(Type) ->
    lists:member(Type, [atom, bool, int, string, expression_value]).

%%% ============================================================================
%%% Preprocessing of queries

merge_results(Setop, Res1 = #state{type = file}, Res2 = #state{type = file}) ->
    IsFile = fun([]) -> depends;
                ([H | _]) -> ?Syn:node_type(H) == file
             end,
                     
    IsFile1 = IsFile(res(Res1)),
    IsFile2 = IsFile(res(Res2)),

    SetopFun = setop_merge_fun(Setop),

    ToFile = fun(Res) -> lists:flatmap(fun ?Lib:file/1, Res) end,

    if (IsFile1 == IsFile2) orelse (IsFile1 == depends) orelse (IsFile2 == depends) ->
            SetopFun(res(Res1), res(Res2));
       true ->
            Files1 = ToFile(res(Res1)),
            Files2 = ToFile(res(Res2)),
            SetopFun(Files1, Files2) 
    end;
merge_results(SetOp, Res1 = #state{action = A1}, Res2 = #state{action = A2}) 
  when A1 == property_query andalso A2 == property_query ->
    SetopFun = setop_merge_fun(SetOp),
    if Res1#state.type == Res2#state.type ->
            SetopFun(res(Res1), res(Res2));
       true ->
            {Conv1, Conv2} = property_to_string(Res1, Res2),
            SetopFun(res(Conv1), res(Conv2))
    end;
merge_results(SetOp, Res1, Res2) when is_record(Res1, state) andalso
                                      is_record(Res2, state) ->
    SetopFun = setop_merge_fun(SetOp),
    SetopFun(res(Res1), res(Res2));
merge_results(SetOp, Res1, Res2) ->
    case SetOp of
        union -> ordsets:union(
                    ordsets:from_list(Res1),
                    ordsets:from_list(Res2)
                 );
        intersect -> ordsets:intersection(
                    ordsets:from_list(Res1),
                    ordsets:from_list(Res2)
                 );
        minus -> ordsets:subtract(
                    ordsets:from_list(Res1),
                    ordsets:from_list(Res2)
                 )
    end.

setop_merge_fun(union) ->
    fun (Res1, Res2) ->
            ordsets:union(
              ordsets:from_list(Res1),
              ordsets:from_list(Res2))
    end;
setop_merge_fun(intersect) ->
    fun (Res1, Res2) ->
            ordsets:intersection(
              ordsets:from_list(Res1),
              ordsets:from_list(Res2))
    end;
setop_merge_fun(minus) ->
    fun (Res1, Res2) ->
                ordsets:subtract(
                  ordsets:from_list(Res1),
                  ordsets:from_list(Res2))
    end.

combine_same_groups(Group1, Group2) ->
    Same_added =
    [{ordsets:from_list(Ents ++
        case lists:keyfind(G, 2, Group2) of
            false -> [];
            {E, _} -> E
        end
        ), G} || {Ents, G} <- Group1],

    Only_in_G2 =
    lists:filter(
        fun({_, Key})->
            not(lists:keymember(Key, 2, Group1))
        end,
        Group2),

    lists:unzip(Same_added ++ Only_in_G2).

group_by_mod(State) ->
    St = 
    case State#state.type of
        file ->
            State#state{
                groupby_type = file,
                groupby_res = lists:append(State#state.res),
                res=[[E]||E <- lists:append(State#state.res)]
            };
        _ -> case State#state.groupby_type of
                [] -> State#state{groupby_res = State#state.res};
                _ -> State
             end
    end,

    Mods = [?Lib:node_file(N) || N <- St#state.groupby_res],

    Merge_same =
    fun(Groups) ->
        Gs = lists:keysort(1, Groups),
        lists:ukeysort(1,
            [ {K, ordsets:union([ordsets:from_list(E)|| {Key, E}<-Gs, Key==K])}
                || {K, _} <- Gs]
        )
    end,

    {PrevRes, Res} = 
        lists:unzip(
            Merge_same(lists:zip(Mods, St#state.res))
        ),
    St#state{groupby_type=file, groupby_res=PrevRes, res=Res}.

merge_states(SetOp, S1, S2) ->
%?d({is_group_state(S1), is_group_state(S2)}),
%?d({to_group_state(S1), to_group_state(S2)}),
    case {to_group_state(S1), to_group_state(S2)} of

        {#state{action=A1, res=R1} = A,
            #state{action=A2, res=R2}}
            when A1==iteration;A1==closure, A2==iteration;A2==closure ->
            Action = case A1 of
                        A2 -> A2;
                        _ -> closure
                     end,
            A#state{
                action=Action,
                res = {chains,
                    merge_results(SetOp, element(2, R1), element(2, R2)),
                    merge_results(SetOp, element(3, R1), element(3, R2)),
                    merge_results(SetOp, element(4, R1), element(4, R2))},
                groupby_res = [],
                groupby_type = []
            };

        {#state{groupby_type = GType} = A,
            #state{groupby_type = PrevType} = B}
            when GType==[] orelse PrevType==[] ->
            A1 = A#state{res = lists:append(res(A))},
            B1 = B#state{res = lists:append(res(B))},
            A#state{
                res = merge_results(SetOp, A1, B1),
                groupby_res = [],
                groupby_type = []
            };

        {State1, State2} when SetOp/=union ->
            State1#state{
                res = [merge_results(SetOp, E, lists:append(State2#state.res))
                        || E<-State1#state.res]
            };

        {#state{action=Action, type=Type, groupby_type=GType} = A,
         #state{action=Action, type=Type, groupby_type=GType} = B} ->
                {F_res, F_groupby_res} =
                    combine_same_groups(
                        lists:zip(A#state.res, A#state.groupby_res),
                        lists:zip(B#state.res, B#state.groupby_res)
                    ),
                A#state{
                  res =  F_res,
                  groupby_res = F_groupby_res};

        {#state{action = Action, groupby_type = GType} = A,
            #state{action=Action, groupby_type=PrevType} = B}
            when GType==[] orelse PrevType==[] orelse GType /= PrevType ->
                merge_states(SetOp, group_by_mod(A), group_by_mod(B));

        {A, B} ->
            case A#state.res of
                [] -> B;
                _  -> A
            end
    end.

property_to_string(State1 = #state{type = Type}, 
                   State2 = #state{type = Type}) ->
    {State1, State2};

property_to_string(State1, State2) ->
    {property_to_string(State1), property_to_string(State2)}.

property_to_string(#state{action=property_query}=State) ->
    State#state{
        type = string,
        name = value,
        res = [?MISC:any_to_string(E) || E<-State#state.res]
    };

property_to_string(#state{action=composite_property_query}=State) ->
    State#state{
        type = string,
        name = value,
        res = [[?MISC:any_to_string(E) || E<-G] || G<-State#state.res]
    };

property_to_string(State) ->
    State.

all_to_string(L) when is_list(L) ->
    [to_string(E) || E<-L].

to_string({quoted, Val})     -> to_string(Val);
to_string(X) when is_atom(X) -> atom_to_list(X);
to_string(X)                 -> ?MISC:any_to_string(X).

to_nongrouped_state(State = #state{groupby_place = undefined}) ->
    State#state{groupby_res = [],
                groupby_type = []};
to_nongrouped_state(State) ->
    State#state{groupby_res = [],
                groupby_type = [],
                entitynumber = State#state.groupby_place - 1}.

to_group_state(#state{action=property_query}=State) ->
    State#state{
        action = composite_property_query,
        res = [[E] || E <- State#state.res]
    };
to_group_state(State) ->
    case is_group_state(State) of
        true -> State;
        _ ->
            State#state{
                res = [[E] || E <- State#state.res]
            }
    end.

is_group_state(#state{action=closure}) -> true;
is_group_state(#state{action=iteration}) -> true;
is_group_state(#state{action=composite_property_query}) -> true;
is_group_state(#state{action=property_query}) -> false;
is_group_state(#state{action=statistics}) -> false;
is_group_state(#state{res=[H|_]}) -> is_list(H);
is_group_state(_) -> false.

is_property_query(#state{action=property_query}) -> true;
is_property_query(#state{action=composite_property_query}) -> true;
is_property_query(_) -> false.

filter_empty_results(#state{res = Res}=State)->
    case is_list(Res)of
        true -> 
            State#state{
                res = lists:filter(fun([]) -> false; (_) -> true end, Res)};
        _ -> State
    end.

filter_empty_groups(#state{res=Res, groupby_res=GRes}=State) when is_list(Res) ->
    Action = State#state.action,
    case is_group_state(State) orelse (Action == property_query andalso GRes /= []) of
        true ->
            Zip = lists:zip(Res, GRes),
            Filtered = lists:filter(fun({[],_})->false; (_)->true end, Zip),
            {NRes, NGRes} = lists:unzip(Filtered),
            State#state{
                res = NRes,
                groupby_res = NGRes};
        _ -> filter_empty_results(State)
    end;
filter_empty_groups(State) -> State.

simplify_prop_query_result(#state{action=composite_property_query, res=Res}=State)->
    OneElementGroups = lists:all(
                        fun ([_])   -> true;
                            (_)     -> false end, Res),
    case OneElementGroups of
        true ->
            State#state{
              action = property_query,
              res = lists:map(fun hd/1, Res)
             };
        _ -> State
    end;

simplify_prop_query_result(State) -> State.

check_query(Query, State) ->
    lists:foldl(fun ({statistics, _} = Element, St) ->
                        check(Element, St);
                    ({help, _} = Element, St) ->
                        check(Element, St);
                    (Element, St) ->
                        case is_property_type(St#state.type) of
                            true ->
                                Property = hd(St#state.checked_query),
                                throw(?LocalError(property_followed, [Property]));
                            false ->
                                check(Element, St)
                        end
                end,
                State,
                Query).

%todo:  node list <-> query format check
check({initial_selector, {set_op, {SetOp, Q1, Q2} = Q}}, State) ->
    ?Check(not (SetOp == any_in orelse SetOp == all_in),
           ?LocalError(illegal_operator, [SetOp, Q])),

    Followed = State#state.semquery /= [] orelse State#state.semquery_last /= [],

    CheckedQ1 = lists:foldl(fun check/2, State, Q1),
    CheckQ2 = State#state{variables = CheckedQ1#state.variables},
    CheckedQ2 = lists:foldl(fun check/2, CheckQ2, Q2),

    Type1 = CheckedQ1#state.type,
    Type2 = CheckedQ2#state.type,

    Action1 = CheckedQ1#state.action,
    Action2 = CheckedQ2#state.action,

    MergedType = merged_type(Type1, Type2),
    ?Check((is_property_type(Type1) xor is_property_type(Type2))
            orelse MergedType /= null,
            ?LocalError(setop_type_mismatch, [Type1, Type2])),

    ?Check((MergedType /= null) andalso (Action1 == Action2 orelse Followed)
           andalso Action1 /= statistics
           andalso Action2 /= statistics,
           ?LocalError(unsupported_feature, set_mix)),

    CheckedSetOp = {set_op, {SetOp,
                             lists:reverse(CheckedQ1#state.checked_query),
                             lists:reverse(CheckedQ2#state.checked_query)}},
    InitialSelector = {initial_selector, CheckedSetOp},

    case {Type1, Type2} of
        {help, _} ->
            CheckedQ1;
        {_, help} ->
            CheckedQ2;
        {_, _} ->
            State#state{action = Action1,
                        type = MergedType,
                        checked_query = [InitialSelector],
                        variables = CheckedQ2#state.variables}
    end;

check({initial_selector, {quoted, Selector}}, State) ->
    check({initial_selector, Selector}, State);

check({initial_selector, Selector} = InitialSelector,
      #state{params = Params} = State) ->
    NodeType =
        case proplists:get_value(node_list, Params) of
            undefined ->
                case ?Lib:init_sel_type(Selector) of
                    any ->
                        element(1, ?Lib:init_sel([{ask_missing,false} | Params],
                                                  Selector));
                    Other -> Other
                end;
            [] ->
                none;
            [HNode|TNodes] ->
                Type = proplists:get_value(node_type, Params,
                                           ?Lib:node_type(HNode)),
                NT = [{Node, ?Lib:node_type(Node)}|| Node <- TNodes],
                Diffs = lists:filter(fun({_N, NType}) -> NType/=Type end, NT),

                ?Check(Diffs == [],
                    ?LocalError(type_mismatch,
                        ["starting nodes", hd(Diffs), Type])),
                Type
        end,
    State#state{type = NodeType, checked_query = [InitialSelector]};

%todo: filterekben help?
check({help, HelpType}, #state{type=Type}=State) ->
    State#state{type=help, checked_query=[{help,
        case HelpType of
            initial_selectors -> {initial_selectors, []};
            queries           -> {selectors, Type};
            statistics        -> {statistics, []};
            filters           -> {properties, Type}
        end}]
    };

check(_, #state{type = undefined}) ->
    throw(?LocalError(syntax_error, initial));

check({set_op, {SetOp, Q1, Q2}}, State) ->
    StartState1 = State#state{checked_query = []},
    CheckedQ1 = lists:foldl(fun check/2, StartState1, Q1),
    
    StartState2 = State#state{checked_query = [],
                              variables = CheckedQ1#state.variables},
    CheckedQ2 = lists:foldl(fun check/2, StartState2, Q2),
    
    Type1 = CheckedQ1#state.type,
    Type2 = CheckedQ2#state.type,

    MergedType = merged_type(Type1, Type2),
    ?Check((is_property_type(Type1) xor is_property_type(Type2))
            orelse MergedType /= null,
            ?LocalError(setop_type_mismatch, [Type1, Type2])),
    ?Check((MergedType /= null),
           ?LocalError(unsupported_feature, set_mix)),

    State#state{
      type = MergedType,
      checked_query = [{set_op, 
                        {SetOp,
                         lists:reverse(CheckedQ1#state.checked_query),
                         lists:reverse(CheckedQ2#state.checked_query)
                        }} | State#state.checked_query],
      variables = CheckedQ2#state.variables};

check({selector, Sel}, #state{type=Type, checked_query=Lst}=State) ->
    Selector =
        case Sel of
            {quoted, Atom} -> Atom;
            Atom -> Atom
        end,
    case ?Lib:sel_type(Type, Selector) of
        [SelType] ->
            State#state{
              type = SelType,
              checked_query = [{selector, Selector, SelType}| Lst]};
        [] ->
            case ?Lib:prop_type(Type, Selector) of
                [PropType] ->
                    State#state{
                      action = selection,
                      type = PropType,
                      checked_query = [{property, Selector, PropType} | Lst]};
                [] ->
                    throw(?LocalError(illegal_selector, [Type, Selector]))
            end
    end;

check({variable_bind, Variable} = Elem, State) ->
    #state{checked_query = Lst, type = Type} = State,
    case is_property_type(Type) of
        true ->
            throw(?LocalError(bind_to_property, [Variable]));
        false ->
            ok
    end,
    case lookup_variable(Variable, State) of
	none ->
	    State#state{action = selection,
                        variables = add_variable(Variable, Type, State),
			checked_query = [Elem | Lst]};
        _ ->
            throw(?LocalError(already_bound, Variable))
    end;    

check({variable_match, _CompOp, Variable} = Elem, State) ->
    #state{checked_query = Lst, type = Type} = State,
    case lookup_variable(Variable, State) of
        none ->
            throw(?LocalError(unbound_variable, Variable));
	{Type, _} ->
	    State#state{action = selection,
                        checked_query = [Elem | Lst]};
	{OtherType, _} ->
	    throw(?LocalError(type_mismatch, [query_to_string(Elem),
					      {Variable, OtherType},
					      Type]))
    end;

check({variable, Variable}, #state{checked_query = Lst}=State) ->
    case lookup_variable(Variable, State) of
	{VariableType, _} ->
	    State#state{action = selection,
                        type = VariableType,
			checked_query = [{variable, Variable} | Lst]};
	none ->
	    throw(?LocalError(unbound_variable, Variable))
    end;

check({Action, {seq, Seq}, {mult, Mult}},
      #state{type=Type, checked_query=Lst, variables=Variables}=State) -> 
    ?Check(Mult > 0, ?LocalError(mult_error, [])),
    case lists:foldl(fun check/2, State#state{checked_query = []}, Seq) of
        #state{type = help} = St ->
            St;
        #state{type = Type, variables = Variables, checked_query = QSLst} ->
            Body = lists:reverse(QSLst),
            State#state{action = maybe_chain,
                        checked_query = [{Action, Body, Mult} | Lst]};
        #state{type = Type, variables = MoreVariables, checked_query = QSLst} = NewState
          when Action == closure ->
            NewlyBoundVariables = variables_got_bound(NewState, State),
            Body = lists:reverse(QSLst),
            Elem = {closure_seq, Body, Mult, NewlyBoundVariables},
            State#state{
              action = maybe_chain,
              checked_query = [Elem | Lst],
              variables = MoreVariables};                       
        #state{type = Type, variables = MoreVariables, checked_query = QSLst}->
            Body = lists:reverse(QSLst),
            State#state{action = maybe_chain,
                        checked_query = [{Action, Body, Mult} | Lst],
                        variables = MoreVariables};
        #state{type = BadType} ->
            PrettyAction = print([{Action, Seq, Mult}]),
            SeqNoDot = tl(print(Seq)),
            throw(?LocalError(type_mismatch, [PrettyAction, {SeqNoDot, BadType},
                                              Type]))
    end;

check({filter, Filter}, #state{checked_query = Lst} = State) ->
    {Checked_Query, NewState} = check_filter(State, Filter),
    Type = NewState#state.type,
    case {Type, has_variable_got_bound(NewState, State)} of
        {help, _} ->
            NewState;
        {_, true} ->
            FilterElement = {filter_with_variable_to_bind,
                             Checked_Query,
                             variables_got_bound(NewState, State)},
            State#state{action = selection,
                        checked_query = [FilterElement | Lst],
                        variables = NewState#state.variables};
        {_, false} ->
            State#state{action = selection, 
                        checked_query = [{filter, Checked_Query} | Lst]}
    end;

check({statistics, Stat}, #state{type = PropType, checked_query = Lst}=State) ->
    Statistics =
        case Stat of
            {quoted, S} -> S;
            S           -> S
        end,
    case PropType of
        int ->
            State#state{action = statistics,
                        type = int,
                        checked_query = [{statistics, Statistics} | Lst]};
        _ ->
            throw(?LocalError(statistics_error, []))
    end.

%% @private
%% @spec check_filter(Type::atom(), Filter::atom()|tuple()) -> atom()|tuple()
check_filter(State,'true') ->
    {'true', State};

check_filter(State, 'false') ->
    {'false', State};

check_filter(State, {'or', Filter1, Filter2}) ->
    {CheckedF1, NewState1} = check_filter(State, Filter1),
    {CheckedF2, NewState2} = check_filter(NewState1, Filter2),
    {{'or', CheckedF1, CheckedF2}, NewState2};

check_filter(State, {'and', Filter1, Filter2}) ->
    {CheckedF1, NewState1} = check_filter(State, Filter1),
    {CheckedF2, NewState2} = check_filter(NewState1, Filter2),
    {{'and', CheckedF1, CheckedF2}, NewState2};

check_filter(State, {'not', Filter}) ->
    {CheckedFilter, NewState} = check_filter(State, Filter),
    {{'not', CheckedFilter}, NewState};

check_filter(#state{type=Type} = State, {set_op, {SetOp, Q1, Q2}=Filter}) ->
    Check_filter_setop =
    fun(F, Variables) -> 
        case F of
            {seq, Q} ->
                StartState = State#state{checked_query = [],
                                         variables = Variables},
                Seq = lists:foldl(fun check/2, StartState, Q),
                Query = lists:reverse(Seq#state.checked_query),
                {Seq#state.type, {seq, Query}, Seq#state.variables};
            {cons, _} -> 
                {any, F, Variables};
            [{initial_selector, _} | _] ->
                StartState = State#state{checked_query = [],
                                         variables = Variables},
                Seq = lists:foldl(fun check/2, StartState, F),
                Query = lists:reverse(Seq#state.checked_query),
                {Seq#state.type, {'query', Query}, Seq#state.variables};
            Property when is_atom(Property) ->
                PropType = case ?Lib:prop_type(Type, Property) of
                               [T] -> T;
                               _ -> atom
                           end,
                {PropType, Property, Variables};
            {variable, Variable} ->
                case lookup_variable(Variable, State) of
                    none ->
                        throw(?LocalError(unbound_variable, Variable));
                    {VarType, _} ->
                        {VarType, F, Variables}
                end;
            String when is_list(String) ->
                {string, String, Variables};
            N when is_integer(N) ->
                {int, N, Variables};
            X ->
                {hd(type_of(X)), X, Variables}
        end
    end,
    Variables = State#state.variables,
    {Type1, Q1Q, MaybeNewVariables1} = Check_filter_setop(Q1, Variables),
    {Type2, Q2Q, MaybeNewVariables2} = Check_filter_setop(Q2, MaybeNewVariables1),

    ?Check(merged_type(Type1, Type2) /= null,
           ?LocalError(type_mismatch, [print([{set_op, Filter}]),
                                       {operand, Type1}, Type2])),

    {QueryElement, NewState} =
        case has_variable_got_bound(MaybeNewVariables2, Variables) of
            true ->
                {set_op_with_variable,
                 State#state{variables = MaybeNewVariables2}};
            false ->
                {set_op, State}
        end, 
    {{QueryElement, {SetOp, Q1Q, Q2Q}}, NewState};

check_filter(State, {'in', P1, P2})->
    check_filter(State, {set_op, {all_in, P1, P2}});

check_filter(State, {seq, Seq}) ->
    St = lists:foldl(fun check/2, 
                     State#state{checked_query = []},
                     Seq),
    CheckedQuery = lists:reverse(St#state.checked_query),
    NewType = St#state.type,
    case {NewType, has_variable_got_bound(St, State)} of
        {help, _} ->
            {{seq, Seq}, St};
        {_, true} ->
            New = State#state{variables = St#state.variables},
            {{seq_with_variable, CheckedQuery}, New};
        {_, false} ->
            {{seq, CheckedQuery}, State}
    end;

check_filter(State, {variable, Variable} = Expr) ->
    case lookup_variable(Variable, State) of
        {bool, _} ->
            {Expr, State};
        {Type, _} ->
            throw(?LocalError(type_mismatch, [print([Expr]),
                                              {Variable, Type}, bool]));
        none ->
            throw(?LocalError(unbound_variable, [Variable]))
    end;

check_filter(#state{type = Type} = State, {CompOp, Op1, Op2} = FullExpr) ->
    Analyze = fun(Op) ->
        case Op of
            {variable, VarName} ->
                case lookup_variable(VarName, State) of
                    {VarType, _} -> {variable, VarType, VarName};
                    _ -> {unbound_variable, undefined, VarName}
                end;
            {quoted, X} -> {const, hd(type_of(X)), Op};
            {seq, Seq} -> {const, State#state.type, {seq, Seq}};
            _ when is_tuple(Op) -> {compop, bool, Op};
            _ when is_atom(Op) ->
                case ?Lib:prop_type(Type, Op) of
                    [PropType] -> {property, PropType, Op};
                    _ -> {const, hd(type_of(Op)), Op}
                end;
            _ -> {const, hd(type_of(Op)), Op}
        end
    end,

    {Class1, Type1, Print1} = Analyze(Op1),
    {Class2, Type2, Print2} = Analyze(Op2),

    PrettyExpr = print([FullExpr]),
    PrettyOp1 = print([Print1]),
    PrettyOp2 = print([Print2]),

    ?Check( %Check for bad types like "any"
        is_property_type(Type1) orelse Class1 == unbound_variable,
        ?LocalError(bad_type, [Type1, PrettyOp1])),

    ?Check( %Check for bad types like "any"
        is_property_type(Type2) orelse Class2 == unbound_variable,
        ?LocalError(bad_type, [Type2, PrettyOp2])),

    ?Check( %...[3=2], ...[non_prop=non_prop], ...['atom' /= non_prop]
       Class1 /= const orelse Class2 /= const,
       ?LocalError(no_property_in_comparison, [])),

    ?Check( %mods[Var = non_prop]..., mods[Var <= prop]...
       Class1 /= unbound_variable orelse
                                    (Class2 == property andalso CompOp == '=='),
       ?LocalError(unbound_variable, PrettyOp1)),

    ?Check( %mods[non_prop = Var]..., mods[prop <= Var]...
       Class2 /= unbound_variable orelse
                                    (Class1 == property andalso CompOp == '=='),
       ?LocalError(unbound_variable, PrettyOp2)),

    ?Check( %mods[Var = name].funs[Var = arity], mods[Var = name].funs[Var = 3]
       Class1 /= variable orelse Type1 == Type2,
       ?LocalError(type_mismatch, [PrettyExpr, {PrettyOp1, Type1}, Type2])),

    ?Check( %mods[Var = name].funs[arity = Var], mods[Var = name].funs[3 = Var]
       Class2 /= variable orelse Type1 == Type2,
       ?LocalError(type_mismatch, [PrettyExpr, {Print2, Type2}, Type1])),

    ?Check( %[(name='name') = arity]
       Class1 /= compop orelse Type2 == bool,
       ?LocalError(type_mismatch, [PrettyExpr, {Print2, Type2}, bool])),

    ?Check( %[arity = (name='name')]
       Class2 /= compop orelse Type1 == bool,
       ?LocalError(type_mismatch, [PrettyExpr, {PrettyOp1, Type1}, bool])),

    ProcOf = fun(Op, Class, S, OtherType) ->
        case {Class, Op} of
            {unbound_variable, {variable, VarName}} ->
                {Op, S#state{variables = add_variable(VarName, OtherType, S)}};
            {compop, _} ->
                check_filter(S, Op);
            _ ->
                {Op, S}
        end
    end,

    {Expr1, State1} = ProcOf(Op1, Class1, State, Type2),
    {Expr2, State2} = ProcOf(Op2, Class2, State1, Type1),

    {{CompOp, Expr1, Expr2}, State2};

% check_filter(_State, {_CompOp, {quoted, _}, {quoted, _}}) ->
%     throw(?LocalError(no_property_in_comparison, []));

% check_filter(State, {CompOp, {quoted, _} = F1, F2}) ->
%     check_filter(State, {CompOp, F2, F1});

% check_filter(#state{type=Type}=State, {CompOp, F1, {quoted, _} = F2} = Expr) ->
%     case F1 of
%         {variable, Var} ->
%             case lookup_variable(Var, State) of
%                 {_,_} -> {Expr, State};
%                 %none when CompOp == '=='->
%                 _ -> throw(?LocalError(unbound_variable, Var))
%             end;
%         _ when is_tuple(F1) -> 
%         {CheckedFilter, NewState} = check_filter(State, F1),
%         {{CompOp, CheckedFilter, F2}, NewState};
%         _ when is_atom(F1)  ->
%             PropType1 = ?Lib:prop_type(Type, F1),
%             ?Check(ordsets:is_element(
%                 PropType1,
%                 [[atom], [bool], [int], [string]]
%             ),
%                    ?LocalError(no_property_in_comparison, [])),
%             {Expr, State};
%         _ ->
%             throw(?LocalError(no_property_in_comparison, []))
%     end;

% check_filter(State, {_CompOp, {variable, Variable1}, {variable, Variable2}}=Expr)->
%     VariableInfo1 = lookup_variable(Variable1, State),
%     VariableInfo2 = lookup_variable(Variable2, State),

%     case {VariableInfo1, VariableInfo2} of
%       {{Type, _}, {Type, _}} ->
%           {Expr, State};
%       {{Type1, _}, {Type2, _}} ->
%           throw(?LocalError(type_mismatch,
%                             [Expr, {Variable1, Type1}, Type2]));
%       {none, _} ->
%           throw(?LocalError(unbound_variable, Variable1));
%       {_, none} ->
%           throw(?LocalError(unbound_variable, Variable2))
%     end;

% check_filter(#state{type=Type} = State,
%            {CompOp, F1, {variable, Variable}} = Expression) ->
%     TypeOfProperty = ?Lib:prop_type(Type, F1),
%     [TypeOfLiteral] = type_of(F1),
%     NewState = 
%       case lookup_variable(Variable, State) of
%           {VariableType, _} when [VariableType] == TypeOfProperty ->
%               State;    
%           {VariableType, _} when TypeOfProperty /= [] ->
%               throw(?LocalError(type_mismatch, 
%                                 [Expression,
%                                  {Variable, VariableType},
%                                  hd(TypeOfProperty)]));
%           {VariableType, _} when VariableType == TypeOfLiteral ->
%               State;
%           {VariableType, _} ->
%               throw(?LocalError(type_mismatch,
%                                 [Expression,
%                                  {Variable, VariableType},
%                                  TypeOfLiteral]));
%           none when (CompOp == '==') andalso (TypeOfProperty /= []) ->
%               State#state{
%                 variables = add_variable(Variable, hd(TypeOfProperty), State)};
%           none when (CompOp == '==') ->
%               throw(?LocalError(no_property_in_comparison, []));
%           none when (CompOp /= '==') ->
%               throw(?LocalError(unbound_variable, Variable))
%       end,
%     {Expression, NewState};

% check_filter(State, {CompOp, F1 = {variable, _}, F2}) ->
%     check_filter(State, {CompOp, F2, F1});

% check_filter(State, {CompOp, F1, F2}) when is_tuple(F1) andalso is_tuple(F2) ->
%     {CheckedF1, NewState1} = check_filter(State, F1),
%     {CheckedF2, NewState2} = check_filter(NewState1, F2),
%     {{CompOp, CheckedF1, CheckedF2}, NewState2};

% check_filter(#state{type=Type} = State, {CompOp, F1, F2}) when is_tuple(F1) ->
%     ?Check(?Lib:prop_type(Type, F2) == [bool] orelse is_boolean(F2),
%            ?LocalError(type_mismatch, [{filter,{CompOp, F1, F2}}, F2, bool])),
%     {CheckedF1, NewState} = check_filter(State, F1),
%     {{CompOp, CheckedF1, F2}, NewState};

% check_filter(State, {CompOp, F1, F2}) when is_tuple(F2) ->
%     {{CompOp, CheckedF2, F1}, NewState} =
%     check_filter(State, {CompOp, F2, F1}),
%     {{CompOp, F1, CheckedF2}, NewState};

% check_filter(#state{type=Type}=State, {CompOp, F1, F2})
%   when
%       is_atom(F1) andalso is_atom(F2)->
    
%     PropType1 = ?Lib:prop_type(Type, F1),
%     PropType2 = ?Lib:prop_type(Type, F2),
%     ?Check(PropType1 /= [] orelse PropType2 /= [],
%            ?LocalError(no_property_in_comparison, [])),

%     Type1 = case PropType1 of [] -> atom; [H] -> H end,
%     Type2 = case PropType2 of [] -> atom; [Hd] -> Hd end,

%     ?Check(merged_type(Type1, Type2) /= null,
%             ?LocalError(type_mismatch, [{filter, {CompOp, F1, F2}},
%                                                   {F1, Type1}, Type2])),
%     {{CompOp, F1, F2}, State};

% check_filter(#state{type=Type} = State, {CompOp, F1, F2}) when is_atom(F1) ->
%     PropType = ?Lib:prop_type(Type, F1),
%     ?Check(PropType /= [],
%            ?LocalError(no_property_in_comparison, [])),

%     ?Check(merged_type(hd(PropType), hd(type_of(F2))) /= null,
%            ?LocalError(type_mismatch,
%                        [{filter, {CompOp, F1, F2}}, F2, hd(PropType)])),
%     {{CompOp, F1, F2}, State};

% check_filter(State, {CompOp, F1, F2}) when is_atom(F2) ->
%     {{CompOp, CheckedF2, CheckedF1}, State} = 
%     check_filter(State, {CompOp, F2, F1}),
%     {{CompOp, CheckedF1, CheckedF2}, State};

% check_filter(_Type, {_CompOp, _F1, _F2}) ->
%     throw(?LocalError(no_property_in_comparison, []));

check_filter(#state{type=Type} = State, Filter) ->
    PropType = ?Lib:prop_type(Type, Filter),
    case PropType of
        [bool] ->
            {Filter, State};
        [] ->
            throw(?LocalError(illegal_property, [Type, Filter]));
        _ ->
            throw(?LocalError(type_mismatch,
                              [{filter,Filter}, {Filter, hd(PropType)}, bool]))
    end.

type_of(E) when is_integer(E) -> [int];
type_of(true) -> [bool];
type_of(false) -> [bool];
type_of(E) when is_atom(E) -> [atom];
type_of(E) when is_list(E) -> case io_lib:printable_list(E) of
                                  true -> [string];
                                  false -> [list]
                              end;
type_of(E) when is_list(E) -> [list];
type_of(_) -> [any].
%%% ============================================================================
%%% Processing of queries

process_set_op_operand(State, Q) ->
    fun (Entities, SomeState) ->
            case is_merged_state(SomeState) of
                false -> 
                    process_query(
                      #state{
                         res = Entities,
                         type = State#state.type,
                         params = State#state.params,
                         variables = SomeState#state.variables},
                      Q,
                      []);
                true ->
                    lists:flatmap(
                      fun(Vars) -> 
                              process_query(
                                #state{
                                   res = Entities,
                                   type = State#state.type,
                                   params = State#state.params,
                                   variables = Vars},
                                Q,
                                []) end,
                      vars(SomeState))
            end
    end.

process_setop({Setop, Q1, Q2}, State = #state{entitynumber = EntNumber,
                                              groupby_place = GbPlace})
  when EntNumber < GbPlace ->
    Proc1 = process_set_op_operand(State, Q1),
    Proc2 = process_set_op_operand(State, Q2),
    Merge = fun (Q1State, Q2State) -> merge_results(Setop, Q1State, Q2State) end,

    Entities = State#state.res,
    ProcessedQ1 = Proc1(Entities, State),
    
    Res = case has_variable_got_bound(hd(ProcessedQ1), State) of
              false -> 
                  [Q1State] = ProcessedQ1,
                  Q2State = merge_states(Proc2(Entities, Q1State)),
                  Name = if Q1State#state.type /= Q2State#state.type -> 
                                 value;
                            true ->
                                 Q2State#state.name
                         end,
                  [Q2State#state{res = Merge(Q1State, Q2State),
                                 name = Name}];
              true -> 
                  Q2Res = [Proc2(Entities, Q1State) || Q1State <- ProcessedQ1],
                  AState = hd(ProcessedQ1),
                  AState2 = hd(hd(Q2Res)),
                  Name = if AState#state.type /= AState2#state.type -> 
                                 value;
                            true ->
                                 AState2#state.name
                         end,
                  F = fun (Q1State, Q2States) ->
                              lists:map(fun(Q2State) ->
                                                Ent = Merge(Q1State, Q2State),
                                                Q2State#state{res = Ent,
                                                              name = Name}
                                        end,
                                        Q2States)
                      end,
                  lists:append(lists:zipwith(F, ProcessedQ1, Q2Res))
          end,
    PostProc = fun (S) -> grouping(S, State) end,
    lists:map(PostProc, Res);

process_setop(Setop, State) ->
    NeedsGrouping = State#state.entitynumber == State#state.groupby_place,
    {Groups, GroupbyRes, GroupbyType} =
        if
           NeedsGrouping ->
                {[[Entity] || Entity <- State#state.res],
                 State#state.res,
                 State#state.type};
           true ->
                {State#state.res,
                 State#state.groupby_res,
                 State#state.groupby_type}
        end,
    
    lists:append(
      lists:zipwith(
        fun (Group, GroupingEntity) ->
                UnGrouped = to_nongrouped_state(State),
                State2 = UnGrouped#state{res = Group},
                Processed = process_setop(Setop, State2),
                Head = hd(Processed),
                #state{action = Action} = Head,
                NewAction = case Action of
                                property_query ->
                                    composite_property_query;
                                OtherAction ->
                                    OtherAction
                            end,
                GroupingState = State#state{
                                  res = [GroupingEntity],
                                  type = GroupbyType,
                                  groupby_res = [GroupingEntity],
                                  groupby_type = GroupbyType},
                lists:map(fun(S) ->
                                  Result = GroupingState#state{
                                             action = NewAction,
                                             name = S#state.name,
                                             type = S#state.type,
                                             res = [res(S)],
                                             variables = S#state.variables},
                                  grouping(Result, GroupingState)
                          end,
                          Processed)
        end,
        Groups,
        GroupbyRes)).

rpc_yield(Key) ->
    ?MISC:yield(Key).

append_parallel_states(State = #state{parallel_key = Keys}) ->
    ParallelStates = lists:flatmap(fun rpc_yield/1, Keys),
    [State#state{parallel_key = []} | ParallelStates].

process({initial_selector, {set_op, {SetOp, Q1, Q2}}}, State) ->
    ThereIsMore = State#state.semquery /= [] orelse
        State#state.semquery_last /= [],
    Params = State#state.params,
    {GroupbyPlace1, GroupbyPlace2} = case proplists:get_value(groupby, Params) of
                                         _ when ThereIsMore ->
                                             {dontgroup, dontgroup};
                                         default ->
                                             {entitynumber_increment(Q1),
                                              entitynumber_increment(Q2)};
                                         undefined ->
                                             {undefined, undefined};
                                         GroupbyPlace ->
                                             {GroupbyPlace, GroupbyPlace}
                                     end,

    StartState1 = State#state{groupby_place = GroupbyPlace1},
    Split = fun (Operand) ->
                    if ThereIsMore ->
                            {Operand, []};
                       true ->
                            split_semantic_query(Operand)
                    end
            end,
    {QuerySeq1, LastPart1} = Split(Q1),
    {QuerySeq2, LastPart2} = Split(Q2),

    Merge = fun (S1, S2) -> merge_states(SetOp, S1, S2) end,
    
    ProcessedQ1 = process_query(StartState1, QuerySeq1, LastPart1),
    VariablesBound = has_variable_got_bound(hd(ProcessedQ1), State),
    MergedState = is_merged_state(hd(ProcessedQ1)),

    States = 
        case {VariablesBound, MergedState} of
            %% No variables are bound or state consists of one entity:
            {false, _} ->
                [Processed] = ProcessedQ1,
                StartState2 = State#state{groupby_place = GroupbyPlace2,
                                          variables = vars(Processed)},
                ProcessedQ2 = process_full_query(StartState2, QuerySeq2, LastPart2),
                {Res1, Res2} = property_to_string(Processed, ProcessedQ2),
                [filter_empty_groups(Merge(Res1, Res2))];
            %% Left operand is a set operation with variable binding
            {true, true} ->
                [Processed] = ProcessedQ1,
                ProcessQ2 = 
                    fun(Vars) ->
                            StartState2 = State#state{
                                            groupby_place = GroupbyPlace2,
                                            variables = Vars},
                            ProcessedQ2 = process_full_query(StartState2, QuerySeq2, LastPart2),
                            {Res1, Res2} = property_to_string(Processed, ProcessedQ2),
                            filter_empty_groups(Merge(Res1, Res2)) end,
                lists:map(ProcessQ2, vars(Processed));
            %% One or more variables are bound:
            {true, false} ->
                ProcessQ2 = 
                    fun (Q1State) ->
                            StartState2 = State#state{
                                            groupby_place = GroupbyPlace2,
                                            variables = vars(Q1State)},
                            ProcessedQ2 = process_query(StartState2, QuerySeq2, LastPart2),
                            lists:map(fun (Q2State) ->
                                              {Res1, Res2} = property_to_string(Q1State,
                                                                                Q2State),
                                              Res = Merge(Res1, Res2),
                                              Filtered = filter_empty_groups(Res),
                                              Filtered#state{variables = vars(Q2State)}
                                      end,
                                      ProcessedQ2)
                    end,
                lists:flatmap(ProcessQ2, ProcessedQ1)
        end,
    RemainingQuery = State#state.semquery,
    RemainingQueryLastPart = State#state.semquery_last,
    case States of
        [Singleton] when ThereIsMore ->
            Singleton#state{entitynumber = 1,
                            groupby_place = State#state.groupby_place,
                            semquery = RemainingQuery,
                            semquery_last = RemainingQueryLastPart};
        [Singleton] ->
            Singleton;
        _ when ThereIsMore ->
            Original = State#state.groupby_place,
            Copy = fun (NewState) ->
                           NewState#state{entitynumber = 1,
                                          groupby_place = Original,
                                          semquery = RemainingQuery,
                                          semquery_last = RemainingQueryLastPart}
                   end,
            Copied = lists:map(Copy, States),
            [Head | Tail] = Copied,
            Key = ?MISC:async_call(node(), ?MODULE, process_parallel,
                                   [Tail, RemainingQuery, RemainingQueryLastPart]),
            Head#state{parallel_key = [Key]};
        [_ | _] ->
            merge_states(States)
    end;

process({initial_selector, InitialSelector}, #state{params = Params} = State) ->
    {NodeType, NodeList} =
        case proplists:get_value(node_list, Params) of
            undefined ->
                ?Lib:init_sel([{ask_missing,false} | Params], InitialSelector);
            [] ->
                {none, []};
            [HNode|_] = Nodes ->
                Type = proplists:get_value(node_type, Params,
                                           ?Lib:node_type(HNode)),

                {Type, Nodes}
        end,
    State#state{type=NodeType, res=ordsets:from_list(NodeList)};

process({selector, Selector, SelType}, St=#state{type = Type,
                                                 res = Entities, 
                                                 entitynumber=EntNum,
                                                 groupby_place=GbyPlace}) ->
    [Fun] = ?Lib:sel_fun(Type, Selector),
    Sort = fun lists:usort/1,
    PFun = case (Entities == []) orelse (not is_list(hd(Entities))) of
        true ->
            fun(E)->Sort(lists:flatten(Fun(E))) end;
        _ ->
            fun(E)->Sort(lists:flatten([Fun(F) || F <- E])) end
    end,
    Res = pmap({?MODULE, pmap_helper}, [PFun], Entities),
    Result = St#state{
               action = selection,
               type = SelType,
               res = case EntNum >= GbyPlace of
                         true -> Res;
                         false -> ordsets:union(Res)
                     end},
    grouping(Result, St);


process({property, Prop, PropType}, St=#state{type=Type}) ->
    [Fun] = ?Lib:prop_fun(Type, Prop),
    Sort = fun lists:sort/1,
    Res = case St#state.groupby_type of
              [] ->
                  St#state{
                    action = property_query,
                    type = PropType,
                    name = Prop,
                    res = pmap({?MODULE, pmap_helper}, [Fun], St#state.res)};
              _ ->
                  St#state{
                    action = composite_property_query,
                    type = PropType,
                    name = Prop,
                    res = pmap({?MODULE, pmap_helper},
                               [fun(Group)->Sort(lists:map(Fun, Group)) end],
                               St#state.res)}
          end,
    grouping(Res, St);

process({set_op, SetOp}, State) when is_record(State, state) ->
    case process_setop(SetOp, State) of
        [] ->
            State;
        [Singleton] ->
            Singleton;
        [Head | Tail] ->
            RemainingQuery = State#state.semquery,
            RemainingQueryLastPart = State#state.semquery_last,
            Key = ?MISC:async_call(node(), ?MODULE, process_parallel,
                                   [Tail, RemainingQuery, RemainingQueryLastPart]),
            Keys = [Key | State#state.parallel_key],
            Head#state{parallel_key = Keys}
    end;

process({iteration, _QSLst, 0}, State=#state{entitynumber=EntNum}) ->
    State#state{entitynumber = EntNum + 1};

process({iteration, QSLst, Mult}, State=#state{entitynumber=EntNum}) ->
    EntityNumber =
        if
            State#state.groupby_place > EntNum ->
                Increment = entitynumber_increment(QSLst),
                EntNum - Increment;
            true ->
                EntNum
        end,
    SubQueryState = State#state{entitynumber = EntityNumber},
    NewState = process_closure_or_iteration_body(SubQueryState, QSLst),
    Iteration = {iteration, QSLst, Mult - 1},
    process(Iteration, NewState);

process({closure, QSLst, Mult}, #state{res=Ents,
                                       entitynumber=EntNum,
                                       groupby_place=GbyPlace}=St)->
    ?Check(length(ets:all())  < ?ets_limit,
           ?LocalError(ets_limit_reached, [])),
    Tab = ets:new(store,
                  [public, bag, {write_concurrency, true}, {keypos, 2}]),
    ProcessParallel =
        fun(Entity, N)->
                ?MISC:async_call(node(), ?MODULE, closure_worker,
                               [St#state{res=Entity}, QSLst, Mult, Tab, N]) 
        end,
    case EntNum >= GbyPlace of 
        true->
            Entities = case EntNum of
                           GbyPlace -> [[E]||E<-Ents];
                           _ -> Ents
                       end,
            Length = lists:foldl(
                       fun(Group, Counter)->
                               [ets:insert(Tab, {Counter, Entity})
                                || Entity <- Group],
                               Counter + 1 end,
                       1,
                       Entities) - 1,        
            {_, KeyList} =
                lists:foldl(           
                  fun (Entity, {Counter, KeyList})->
                          {Counter + 1,
                           [ProcessParallel(Entity, Counter) | KeyList]} end,
                  {1, []},
                  Entities),
            lists:foreach(fun rpc_yield/1, KeyList),
            Res = [ets:select(Tab, [{{N, '$2'}, [], ['$2']}]) ||
                      N <- lists:seq(1, Length)],
            GroupbyRes = St#state.groupby_res;
        false ->
            [ets:insert(Tab, {0, E}) || E <- Ents],
            EntityLists = group_entities(Ents),
            KeyList = [ProcessParallel(E, 0) || E <- EntityLists],
            lists:foreach(fun rpc_yield/1, KeyList),
            Res = ets:select(Tab, ets:fun2ms(fun({0, X}) -> X end)),
            GroupbyRes = Res
    end,
    Result = St#state{action = selection, res = Res, groupby_res = GroupbyRes},
    ets:delete(Tab),
    grouping(Result, St);

process({closure_seq, QSLst, Mult, VariablesToBind},
        State = #state{entitynumber = EntNumber, groupby_place = GbPlace})
  when EntNumber < GbPlace ->

    %% entities, to which the new variables aren't bound
    BaseSet = gb_sets:from_list(State#state.res),

    %% entities, to which the new variables are bound
    SetofVariableBinding = gb_sets:empty(),

    ClosureRes = [BaseSet | State#state.closure_res],
    StartState = State#state{closure_res = ClosureRes, parallel_key = []},
    {ResultSet, ResultBaseSet, ResultState} = 
        closure_worker_seq(StartState, QSLst, Mult, SetofVariableBinding,
                           BaseSet),
    RemainingQuery = tl(State#state.semquery),
    RemainingQueryLastPart = State#state.semquery_last, 
    StateWithoutNewVariableBindings = 
        State#state{
          res = gb_sets:to_list(ResultBaseSet),
          entitynumber = EntNumber + 1,
          semquery = tl(State#state.semquery),
          parallel_key = []},
    MarkedState =
        mark_undefined_as_valueless(VariablesToBind,
                                    StateWithoutNewVariableBindings),
    Key = ?MISC:async_call(node(), ?MODULE, process_parallel,
                         [[MarkedState],
                          RemainingQuery,
                          RemainingQueryLastPart]),
    Keys = [Key | State#state.parallel_key ++ ResultState#state.parallel_key],
    EntitiesWithVariableBinding = gb_sets:to_list(ResultSet),
    ResultState#state{res = EntitiesWithVariableBinding,
                      closure_res = State#state.closure_res,
                      entitynumber = EntNumber + 1,
                      semquery = tl(State#state.semquery),
                      parallel_key = Keys};

process({closure_seq_cont, QSLst, Mult, _}, 
        State = #state{entitynumber = EntNumber, groupby_place = GbPlace})
  when EntNumber < GbPlace ->
    ClosureEntities = hd(State#state.closure_res),
    {NewEntities, SetofVariableBinding, BaseSet} = 
        filter_closure_result(State#state.res, gb_sets:empty(), ClosureEntities),
    StartState = State#state{res = NewEntities, 
                             closure_res = tl(State#state.closure_res),
                             parallel_key = []},
    {ResultSet, _ResultBaseSet, ResultState} = 
        closure_worker_seq(StartState, QSLst, Mult, SetofVariableBinding,
                           BaseSet),
    EntitiesWithVariableBinding = gb_sets:to_list(ResultSet),
    ResultState#state{res = EntitiesWithVariableBinding,
                      closure_res = tl(State#state.closure_res),
                      entitynumber = EntNumber + 1};

process({closure_seq, QSLst, Mult, VariablesToBind}, State) ->
    #state{res = Entities,
           groupby_place = GbPlace,
           entitynumber = EntNumber,
           type = Type,
           groupby_type = GbType} = State,
    {GroupingEntities, Groups, GroupbyType} = 
        if EntNumber == GbPlace ->
                {Entities, [[Entity] || Entity <- Entities], Type};
           true ->
                {State#state.groupby_res, Entities, GbType}
        end,
    Store = fun(GroupingEntity, Group, Dict)->
                    dict:store(GroupingEntity, 
                               {gb_sets:empty(), gb_sets:from_list(Group)},
                               Dict) end,
    GroupsWithoutVariableBinding = foldl2(Store,
                                          dict:new(),
                                          GroupingEntities,
                                          Groups),
    ClosureRes = [GroupsWithoutVariableBinding | State#state.closure_res],
    StartState = State#state{closure_res = ClosureRes,
                             parallel_key = []},
    {ResultSets, ResultState} = closure_worker_seq(StartState, QSLst, Mult,
                                                   GroupsWithoutVariableBinding),
    GroupWithoutBind =
        fun (GroupingEntity) -> 
                {_, GroupWithoutBind} = dict:fetch(GroupingEntity, ResultSets),
                gb_sets:to_list(GroupWithoutBind) end,
    RemainingQuery = tl(State#state.semquery),
    RemainingQueryLastPart = State#state.semquery_last,
    GroupByRes = dict:fetch_keys(ResultSets),
    StateWithoutNewVariableBindings = 
        State#state{
          res = lists:map(GroupWithoutBind, GroupByRes),
          groupby_res = GroupByRes,
          groupby_type = GroupbyType,
          entitynumber = EntNumber + 1,
          semquery = tl(State#state.semquery),
          parallel_key = []},
    MarkedState =
        mark_undefined_as_valueless(VariablesToBind,
                                    StateWithoutNewVariableBindings),
    Key = ?MISC:async_call(node(), ?MODULE, process_parallel,
                         [[MarkedState],
                          RemainingQuery,
                          RemainingQueryLastPart]),
    Keys = [Key | State#state.parallel_key ++ ResultState#state.parallel_key],
    GroupWithBoundVariable = 
        fun (GroupingEntity) ->
                {GroupWithBind, _} = dict:fetch(GroupingEntity, ResultSets),
                gb_sets:to_list(GroupWithBind) end,
    ActualGroupByRes = ResultState#state.groupby_res,
    State#state{
      res = lists:map(GroupWithBoundVariable, ActualGroupByRes),
      groupby_res = ActualGroupByRes,
      groupby_type = GroupbyType,
      entitynumber = EntNumber + 1,
      variables = ResultState#state.variables,
      semquery = tl(State#state.semquery),
      parallel_key = Keys};

process({closure_seq_cont, QSLst, Mult, _}, State) ->
    #state{res = Groups,
           groupby_res = GroupingEntities,
           closure_res = [Sets | _],
           entitynumber = EntNumber} = State,
    Updated = foldl2(fun read_update_store/3, {[], Sets}, Groups, GroupingEntities),
    {ReallyNewGroups, NewSets} = Updated,            
    Increment = entitynumber_increment(QSLst),
    OriginalEntityNumber = EntNumber-Increment,
    StartState  = State#state{
                    res = ReallyNewGroups,
                    entitynumber = OriginalEntityNumber + 1,
                    closure_res = tl(State#state.closure_res),
                    parallel_key = []},
    {ResultSets, ResultState} = 
        closure_worker_seq(StartState, QSLst, Mult, NewSets),
    GroupWithBoundVariable = 
        fun (SomeGroupingEntity) ->
                {GroupWithBind, _} = dict:fetch(SomeGroupingEntity, ResultSets),
                gb_sets:to_list(GroupWithBind) end,
    ActualGroupByRes = ResultState#state.groupby_res,
    IncrementedEntityNumber = OriginalEntityNumber + 1,
    State#state{
      res = lists:map(GroupWithBoundVariable, ActualGroupByRes),
      groupby_res = ActualGroupByRes,
      groupby_type = StartState#state.groupby_type,
      entitynumber = IncrementedEntityNumber,
      closure_res = tl(State#state.closure_res),
      variables = ResultState#state.variables};

process({variable, Variable}, State) ->
    {Type, Value} = case lookup_variable(Variable, State) of
                        {VariableType, {_, BoundValue}} ->
                            {VariableType, [BoundValue]};
                        {VariableType, _} ->
                            {VariableType, []}
                    end,
    IsProperty = is_property_type(Type),
    StateIsGrouped = is_group_state(State),
    NeedsGrouping = State#state.entitynumber == State#state.groupby_place,
    Action = if IsProperty andalso (StateIsGrouped orelse NeedsGrouping) ->
                     composite_property_query;
                IsProperty ->
                     property_query;
                true ->
                     selection
             end,
    Res = State#state{
            action = Action,
            type = Type,
            name = Variable,
            res = if StateIsGrouped ->
                          lists:duplicate(length(State#state.groupby_res), Value);
                     NeedsGrouping ->
                          lists:duplicate(length(State#state.res), Value);
                     true ->
                          Value
                  end},
    grouping(Res, State);

process({variable_bind, Variable}, State) ->
    case is_variable_bound(Variable, State) of
        true ->
            %% exception for closures (iterations) where
            %% the same loop body is executed in every iteration
            process({variable_match, '?', Variable}, State);
        false ->
            #state{res = Entities, parallel_key = Keys} = State,
            StateIsNotGrouped = not is_group_state(State),
            BoundStates = 
                if StateIsNotGrouped ->
                        Bind = fun (Entity) ->
                                       Singleton = State#state{res = [Entity]},
                                       bind_variable(Variable, Entity, Singleton) end,
                        lists:map(Bind, Entities);
                   true ->
                        Bind = fun (Entity, GroupingEntity) ->
                                       Singleton =
                                           State#state{res = [[Entity]],
                                                       groupby_res = [GroupingEntity]},
                                       bind_variable(Variable, Entity, Singleton) end,
                        BindGroup = fun (Group, GroupingEntity) ->
                                            [Bind(Entity, GroupingEntity) ||
                                                Entity <- Group] end,
                        GroupingEntities = State#state.groupby_res,
                        lists:append(
                          lists:zipwith(BindGroup, Entities, GroupingEntities))
                end,
            case BoundStates of
                %% case of binding when State is empty
                [] ->
                    mark_undefined_as_valueless([Variable], State);
                [Singleton] ->
                    Singleton;
                [Head | Tail] ->
                    Key = async_process_parallel(Tail),
                    Head#state{parallel_key = [Key | Keys]}
            end
    end;

process({variable_match, CompOp, Variable}, State) ->
    #state{res = Entities} = State,
    StateIsNotGrouped = not is_group_state(State),
    Relation = case CompOp of
                   '?'  -> '==';
                   '/?' -> '/='
               end,
    Type = fun ?Syn:node_type/1,
    Filter = fun (_BoundValue, []) ->
                     [];
                 (BoundValue, Group) ->
                     BoundT = Type(BoundValue),
                     GroupT = Type(hd(Group)),
                     NeedsConvert = (BoundT == file andalso GroupT == module) 
                         orelse (BoundT == module andalso GroupT == file),
                     Conv = if (NeedsConvert andalso BoundT == file) ->
                                    ?Lib:mod(BoundValue);
                               (NeedsConvert andalso BoundT == module) ->
                                    ?Lib:file(BoundValue);
                               true ->
                                    [BoundValue]
                            end,
                     [Entity || Var <- Conv,
                                Entity <- Group,
                                compare(Relation, Entity, Var)]
             end,
    case lookup_variable_value(Variable, State) of
        {_, BoundValue} when StateIsNotGrouped ->
            State#state{res = Filter(BoundValue, Entities)};
        {_, BoundValue} ->
            State#state{res = [Filter(BoundValue, Group) || Group <- Entities]};
        _ when StateIsNotGrouped ->
            State#state{res = []};
        _ ->
            GroupCount = length(State#state.groupby_res),
            State#state{res = lists:duplicate(GroupCount, [])}
    end;

process({filter, Filter}, State = #state{res = Groups}) ->
    case is_group_state(State) of
        true ->
            NonGrouped = to_nongrouped_state(State),
            State#state{
              res = pmap({?MODULE, pmap_helper},
                         [fun(Group) ->
                                  res(filter(Filter,
                                             NonGrouped#state{res = Group}))
                          end],
                         Groups)};
        false ->
            filter(Filter, State)
    end;

process({filter_with_variable_to_bind, _, _} = FilterElement, State) ->
    process_filter_with_variable_to_bind(FilterElement, State);

% todo: preproc
process({statistics, Stat}, #state{res = Entities} = State) ->
    Fun = ?Lib:stat_fun(Stat),
    case is_group_state(State) of
        false -> 
            #state{action = statistics, type = Stat, res = Fun(Entities)};
        true ->
            Stats = lists:map(Fun, Entities),
            State#state{action = statistics, type = Stat, res = Stats}
    end.
           
group_entities(Entities) ->
    NumberOfData = length(Entities),
    Divide =
        case erlang:system_info(logical_processors_available) of
            unknown ->
                (NumberOfData div 10) + 1;
            N ->
                (NumberOfData div (N*10)) + 1
        end,
    ?MISC:slice_list(Divide, Entities).

%% @private
closure_worker(_State, _QSLst, 0, _Tab, _Group) ->
    ok;
closure_worker(#state{res=[]}, _QSLst, _Mult, _Tab, _Group) ->
    ok;
closure_worker(State, QSLst, Mult, Tab, Group) ->
    StateToProc = State#state{
        entitynumber = -entitynumber_increment(QSLst),
        groupby_type = [],
        groupby_res = []},
    #state{res=NewEntities} = 
        process_query_seq(StateToProc, QSLst),
    ReallyNewEntities =
        lists:filter(fun(Entity) -> ets:match(Tab, {Group, Entity}) == [] end,
                     NewEntities),
    lists:foreach(fun (Entity) -> ets:insert(Tab, {Group, Entity}) end,
                  ReallyNewEntities),
    NewMult = case Mult of infinite = I -> I; M -> M-1 end,
    NewState = State#state{res=ReallyNewEntities},
    closure_worker(NewState, QSLst, NewMult, Tab, Group).


closure_worker_seq(State, _QSLst, 0, SetWithVariableBinding, BaseSet) ->
    {SetWithVariableBinding, BaseSet, State};

closure_worker_seq(State=#state{entitynumber=EntNumber, groupby_place=GbPlace},
                   QSLst, Mult, SetWithVariableBinding, BaseSet)
  when EntNumber < GbPlace ->
    case is_empty_state(State) of
        true ->
            {SetWithVariableBinding, BaseSet, State};
        false ->
            Increment = entitynumber_increment(QSLst),
            StartState = State#state{entitynumber = EntNumber - Increment},
            SubQueryState = process_closure_or_iteration_body(StartState, QSLst),
            #state{res = NewEntities, 
                   variables = Variables, 
                   parallel_key = Keys} = SubQueryState,
            {ReallyNewEntities, 
             EntitiesWithVariableBinding,
             EntitiesWithoutVariableBinding} = 
                filter_closure_result(NewEntities, SetWithVariableBinding, BaseSet),
            NewMult = case Mult of infinite = I -> I; M -> M - 1 end,
            NewState = State#state{res = ReallyNewEntities, 
                                   variables = Variables,
                                   parallel_key = Keys},
            closure_worker_seq(NewState, QSLst, NewMult, 
                               EntitiesWithVariableBinding,
                               EntitiesWithoutVariableBinding)
    end.

closure_worker_seq(State, _QSLst, 0, Sets) ->
    {Sets, State};

closure_worker_seq(State, QSLst, Mult, Sets) ->
    SubQueryState = process_closure_or_iteration_body(State, QSLst),
    case is_empty_state(SubQueryState) of
        true ->
            {Sets, State};
        false ->
            #state{res = Groups,
                   groupby_res = GroupingEntities} = SubQueryState,
            Updated = foldl2(fun read_update_store/3, {[], Sets}, Groups, GroupingEntities),
            {ReallyNewGroups, NewSets} = Updated,            
            NewMult = case Mult of infinite = I -> I; M -> M - 1 end,        
            NewState = SubQueryState#state{
                         res = ReallyNewGroups,
                         entitynumber = State#state.entitynumber},
            closure_worker_seq(NewState, QSLst, NewMult, NewSets)
    end.

read_update_store(Group, GroupingEntity, {NewEntities, Sets}) ->
    {SetofActualGroup, BaseSet} = dict:fetch(GroupingEntity, Sets),
    {ReallyNewEntities, 
     EntitiesWithVariableBinding,
     EntitiesWithoutVariableBinding} = 
        filter_closure_result(Group, SetofActualGroup, BaseSet),
    {[ReallyNewEntities | NewEntities],
     dict:store(GroupingEntity, 
                {EntitiesWithVariableBinding,
                 EntitiesWithoutVariableBinding},
                Sets)}.

grouping(NewState, #state{groupby_place=GroupBy, entitynumber=EntNum}=PrevState) ->
    Res =
        if
            EntNum == GroupBy ->
                NewState#state{
                  groupby_type = PrevState#state.type,
                  groupby_res = PrevState#state.res,
                  entitynumber = EntNum+1,
                  groupby_place = GroupBy,
                  params = PrevState#state.params,
                  semquery = PrevState#state.semquery,
                  semquery_last = PrevState#state.semquery_last};
            true ->
                NewState#state{
                  groupby_type = PrevState#state.groupby_type,
                  groupby_res = PrevState#state.groupby_res,
                  entitynumber = EntNum+1,
                  groupby_place = GroupBy,
                  params = PrevState#state.params,
                  semquery = PrevState#state.semquery,
                  semquery_last = PrevState#state.semquery_last}
        end,
    case Res#state.groupby_res of
        [] -> filter_empty_results(Res);
        _ -> filter_empty_groups(Res)
    end.

process_last({Action, QSLst, Mult, _}, State) when Action == closure_seq orelse
                                                   Action == closure_seq_cont ->
    process_last({closure, QSLst, Mult}, State);

process_last({selector, _, Type}, #state{res=[]}=State)->
    Res = State#state{
            action=selection,
            type=Type},
    grouping(Res, State);

process_last({property, Property, Type}, #state{res=[]}=State) ->
    Res = State#state{
            action=property_query,
            type=Type,
            name=Property},
    grouping(Res, State);

process_last(QueryElement, #state{res=[]}=State) 
  when (element(1, QueryElement) /= iteration) andalso
       (element(1, QueryElement) /= closure) andalso
       (element(1, QueryElement) /= set_op) andalso
       (element(1, QueryElement) /= variable) ->
    State#state{groupby_res=[]};

process_last({Action, QSLst, Mult}, #state{type=Type, res=Res}=State)
  when (Action == closure) orelse (Action == iteration) ->
    InitialChains = #chains{incomplete = [[Entity] || 
                                             Entity <- lists:flatten(Res)]},
    Result = 
        State#state{
          action = Action,
          type = Type, 
          res = chains(State#state{action=Action}, InitialChains, QSLst, Mult)},
    grouping(Result, State);

process_last(QueryElement, State) ->
    process(QueryElement, State).


%%% ===================o=========================================================
%%% Helper functions
pmap({Module, Func}, ExtraArgs, List) ->
    % rpc:pmap({Module, Func}, ExtraArgs, List). %slower than ?MISC:parallelise
     ?MISC:parallelise(List, Module, Func,
                       fun(Entities)-> [Entities|ExtraArgs] end, false).

pmap_helper(Entities, Fun) when is_list(Entities) ->
     [Fun(Entity) || Entity <- Entities];
pmap_helper(Entity, Fun)->
     pmap_helper([Entity], Fun).

chains_worker(#state{chain_keys=Keys}, [], _QSLst, _Mult, _Tab) ->
    rethrow_badrpc(lists:map(fun rpc_yield/1, Keys));
chains_worker(#state{chain_keys=Keys}, Chains, _QSLst, 0, Tab) ->
    ets:insert(Tab, [{incomplete, Chain} || Chain <- Chains]),
    rethrow_badrpc(lists:map(fun rpc_yield/1, Keys));
chains_worker(St, Chains, QSLst, Mult, Tab) when is_record(St, state)->
    StartState = St#state{action = selection, 
                          entitynumber = - entitynumber_increment(QSLst),
                          groupby_type = []},
    NewChains =
        lists:flatmap(
          fun([H|_] = Chain) ->
                  #state{res = Res} =
                      process_closure_or_iteration_body(
                        StartState#state{res = [H]},
                        QSLst),
                  next_chain(St#state{res = Res}, Chain)
          end,
          Chains),
    Incomplete = insert_chains(Tab, NewChains),
    NewMult = case Mult of infinite -> infinite; Mult -> Mult-1 end,
    chains_worker(St, Incomplete, QSLst, NewMult, Tab).

chains_worker_first(#state{action=Action}=St,
            Chains, QSLst, Mult, Tab)->
    StartState = St#state{action = selection, 
                          entitynumber = - entitynumber_increment(QSLst),
                          groupby_type = [],
                          parallel_key = []},
    {NewChains, States} =
        lists:unzip(
          lists:map(
            fun([H|_] = Chain) ->
                    NewState = #state{res=Res} =             
                        process_closure_or_iteration_body(
                          StartState#state{
                            res = [H],
                            chain = Chain}, 
                          QSLst),
                    {next_chain(St#state{res = Res}, Chain), NewState}
            end,
            Chains)),
    Incompletes =
        lists:map(fun(Chains_) -> insert_chains(Tab, Chains_) end, NewChains),
    NewMult = case Mult of infinite -> infinite; Mult -> Mult - 1 end,
    VariableBindingKeys =
        lists:flatmap(fun(State) -> State#state.parallel_key end, States),
    case has_variable_got_bound(States, St) of
        false ->            
            chains_worker(St, lists:append(Incompletes), QSLst, NewMult, Tab);
        true ->
            ProcessParallel =
                fun(State, Chain)->
                        ?MISC:async_call(node(), ?MODULE, chains_worker,
                                       [State#state{action = Action}, Chain,
                                        QSLst, NewMult, Tab])
                end,
            MoreKeys = lists:zipwith(ProcessParallel,
                                     tl(States),
                                     tl(Incompletes)),
            CurrentProcessState =
                (hd(States))#state{
                  action = Action,
                  chain_keys = VariableBindingKeys ++ MoreKeys,
                  parallel_key = St#state.parallel_key},
            CurrentProcessChains = hd(Incompletes),
            chains_worker(CurrentProcessState, CurrentProcessChains,
                          QSLst, NewMult, Tab)
    end.        

chains(_State, #chains{incomplete = []} = Chains, _QSLst, _Mult) ->
    Chains;
chains(State=#state{chains_table = undefined},
       #chains{incomplete=Chains} = ChainsRec, QSLst, Mult) ->
    ?Check(length(ets:all()) < ?ets_limit,
           ?LocalError(ets_limit_reached, [])),
    Table = ets:new(store,
                    [public, bag, {write_concurrency, true}]),
    StartState = State#state{chains_table = Table},
    ChainGroups = group_entities(Chains),
    Keys = [?MISC:async_call(node(), ?MODULE, chains_worker_first,
                           [StartState, Ch, QSLst, Mult, Table])
            || Ch <- ChainGroups],
    Results = lists:map(fun rpc_yield/1, Keys),
    try
        rethrow_badrpc(Results),
        Recursive =
            ets:select(Table, ets:fun2ms(fun({recursive, Chain}) -> Chain end)),
        Completed =
            ets:select(Table, ets:fun2ms(fun({complete, Chain}) -> Chain end)),
        Incomplete =
            ets:select(Table, ets:fun2ms(fun({incomplete, Chain}) -> Chain end)),
        ChainsRec#chains{complete = Completed ++ ChainsRec#chains.complete,
                         incomplete = Incomplete,
                         recursive = Recursive ++ ChainsRec#chains.recursive}
    after
        ets:delete(Table)
    end;

chains(State=#state{chains_table = Table, chain = Chain}, _, QSLst, Mult) ->
    Chains = next_chain(State, Chain),
    Incompletes = insert_chains(Table, Chains),
    ChainGroups = group_entities(Incompletes),
    Keys = [?MISC:async_call(node(), ?MODULE, chains_worker,
                           [State, Ch, QSLst, Mult, Table])
            || Ch <- ChainGroups],
    rethrow_badrpc(lists:map(fun rpc_yield/1, Keys)).

insert_chains(Table, NewChains)->
    SplitFun =
        fun({complete, _} = C, {Incomp, Comp, Rec}) ->
                {Incomp, [C | Comp], Rec};
           ({recursive, _} = R, {Incomp, Comp, Rec}) ->
                {Incomp, Comp, [R | Rec]};
           (List, {Incomp, Comp, Rec}) ->
                {[List | Incomp], Comp, Rec}
        end,
    {Incomplete, Completed, Recursive} =
        lists:foldl(SplitFun, {[], [], []}, NewChains),
    ets:insert(Table, Completed ++ Recursive),
    Incomplete.

next_chain(#state{action = iteration, res = Result}, Chain) ->
    lists:foldl(fun(Entity, Acc) -> [[Entity| Chain]|Acc] end, [], Result);
next_chain(#state{action = closure, res = []}, Chain) ->
    [{complete, Chain}];
next_chain(#state{action = closure, res = Result}, Chain) ->
    lists:foldl(
      fun(Entity, Acc) ->
              case lists:member(Entity, Chain) of
                  true -> [{recursive, [Entity|Chain]}|Acc];
                  _    -> [[Entity| Chain]|Acc]
              end
      end,
      [],
      Result).

%%% ============================================================================
%%% Filters

%% TODO: entity listakat rendezni filter elott!!!!
%% @private
%% @spec filter(Filter::term(), #state{}) -> ordset()
filter(_Filter, #state{res = []} = State) -> State;

filter('true', State) -> State;

filter('false', State) -> State#state{res = []};

filter({'not', Filter}, #state{res = Entities} = State) ->
    NewState = filter(Filter, State),
    NewState#state{res = ordsets:subtract(Entities, NewState#state.res)};

filter({'or', Filter1, Filter2}, State) ->
    NewStateF1 = filter(Filter1, State),
    NewVars = State#state{variables = NewStateF1#state.variables},
    StatesForFilter2 = separate_binds(NewVars),
    StatesOfFilter2 = [filter(Filter2, S2) || S2 <- StatesForFilter2],
    case not_empty_states([NewStateF1 | StatesOfFilter2]) of
        [] ->
            State#state{res = []};
        NotEmptyStates ->
            merge_states(NotEmptyStates)
    end;

filter({'and', Filter1, Filter2}, State) ->
    NewStateF1 = filter(Filter1, State),
    StatesForFilter2 = separate_binds(NewStateF1),
    StatesOfFilter2 = lists:map(fun (StateForFilter2) ->
                                        filter(Filter2, StateForFilter2) end,
                                StatesForFilter2),
    case not_empty_states(StatesOfFilter2) of
        [] ->
            State#state{res = []};
        NotEmptyStates ->
            merge_states(NotEmptyStates)
    end;

filter({set_op_with_variable, {SetOp, Q1, Q2}}, State) ->
    Get_val = get_setop_value(State),
    Op1 = Get_val(Q1),
    Op2 = Get_val(Q2),
    [Entity] = State#state.res,
    Filter =
        case SetOp of
            any_in ->
                fun (Op1State, Op2State) ->
                        not(ordsets:is_disjoint(res(Op1State), res(Op2State)))
                end;
            all_in ->
                fun (Op1State, Op2State) ->
                        ordsets:is_subset(res(Op1State), res(Op2State))
                end
        end,
    FilterStates = fun (Op1State, Op2States) ->
                           Op2State = merge_states(Op2States),
                           case Filter(Op1State, Op2State) of
                               true -> [Op2State];
                               false -> []
                           end
                   end,
    %for each state in ProcessedOp1 there is a list of states in ProcessedOp2 
    %so that length(ProcessedOp1) == length(ProcessedOp2)
    ProcessedOp1 = Op1(Entity, State),
    ProcessedOp2 = lists:map(fun (Op1State) ->
                                     Op2(Entity, Op1State)
                             end,
                             ProcessedOp1),
    case has_variable_got_bound(ProcessedOp1, State) of
        true -> 
            FilteredStates = lists:append(
                               lists:zipwith(FilterStates, ProcessedOp1, ProcessedOp2)),
            case FilteredStates of
                [] -> State#state{res = []};
                _  -> State#state{variables = collect_variable_values(FilteredStates)}
            end;
        false ->
            [Op1Result] = ProcessedOp1,
            [Op2Results] = ProcessedOp2,
            Op2Result = merge_states(Op2Results),
            case Filter(Op1Result, Op2Result) of
                false -> State#state{res = []};
                true  -> State#state{variables = vars(Op2Result)}
            end
    end;

filter({set_op, {SetOp, Q1, Q2}}, State) ->
    Get_val = get_setop_value(State),
    Op1 = Get_val(Q1),
    Op2 = Get_val(Q2),
    Filter =
        case SetOp of
            any_in ->
                fun (Op1State, Op2State) ->
                        not(ordsets:is_disjoint(res(Op1State), res(Op2State)))
                end;
            all_in ->
                fun (Op1State, Op2State) ->
                        ordsets:is_subset(res(Op1State), res(Op2State))
                end
        end,
    FilteredEntities =
        ordsets:filter(
          fun (Entity) ->
                  [ProcessedOp1] = Op1(Entity, State),
                  [ProcessedOp2] = Op2(Entity, State),
                  Filter(ProcessedOp1, ProcessedOp2)
          end,
          State#state.res),
    State#state{res = FilteredEntities};
    
filter({seq_with_variable, QuerySeq}, #state{res = Entity} = State) ->
    FilteredState = 
        process_full_query(State#state{res = Entity,
                                       entitynumber = -length(QuerySeq)},
                           QuerySeq,
                           []),
    case FilteredState#state.res of
        [] ->
            State#state{res = []};
        _ ->
            State#state{variables = FilteredState#state.variables}
    end;

filter({seq, QuerySeq}, #state{res = Entities} = State) ->
    FilteredEntities = 
        ordsets:filter(
          fun(Entity) ->
                  St = process_full_query(
                         State#state{
                           res = [Entity],
                           entitynumber = -length(QuerySeq)},
                         QuerySeq,
                         []),
                  St#state.res /= []
          end,
          Entities),
    State#state{res = FilteredEntities};

% filter({CompOp, {variable, Variable1}, {variable, Variable2}}, State) ->
%     {value, Value1} = lookup_variable_value(Variable1, State),
%     {value, Value2} = lookup_variable_value(Variable2, State),
%     filter(compare(CompOp, Value1, Value2), State);

% filter({CompOp, PropertyOrLiteral, {variable, Variable}},
%        #state{type = Type} = State) ->
%     VariableValue = lookup_variable_value(Variable, State),
%     MaybePropFun = ?Lib:prop_fun(Type, PropertyOrLiteral),
%     case {VariableValue, MaybePropFun} of
%       {undefined, [PropFun]} ->
%           case State#state.res of
%               [Entity] ->
%                   State#state{
%                     variables = bind_variable(Variable, PropFun(Entity), State)};
%               _ ->
%                   filter(false, State)
%           end;
%       {undefined, []} ->
%           filter(false, State);
%       {{value, BoundValue}, [_]} ->           
%           filter({CompOp, BoundValue, PropertyOrLiteral}, State);
%       {{value, BoundValue}, []} ->
%           filter(compare(CompOp, BoundValue, PropertyOrLiteral), State)
%     end;

filter({variable, Variable}, State) ->
    case lookup_variable_value(Variable, State) of
        {value, true} -> State;
        _ -> State#state{res = []}
    end;

%% Comparison works on atom, int and string.
%% todo: regexp match -> if a string doesn't match
filter({CompOp, Filt1, Filt2}, #state{type=EntityType, res=Entities} = State) ->
    ParamFunG = fun(Param, PrevParam) ->
        case Param of
            {quoted, B} -> %[name = 'Var']
                fun(_) -> B end;
            {variable, Var} ->
                case lookup_variable_value(Var, State) of
                    {value, Value} -> fun(_) -> Value end;
                    undefined -> {variable, Var};
                    valueless -> {valueless_variable}
                end;
            _ when is_tuple(Param) -> %[(name=run) = (arity=3)]
                fun(Entity) -> 
                        St = State#state{res = [Entity]},
                        res(filter(Param, St)) end;
            PrevParam -> %[name=name]
                fun(_) -> Param end;
            _ when is_atom(Param) ->
                case ?Lib:prop_fun(EntityType, Param) of
                    [Fun] -> Fun;
                    _ -> fun(_) -> Param end
                end;
            _ -> fun(_) -> Param end
        end
    end,

    LFun = ParamFunG(Filt1, []),
    RFun = ParamFunG(Filt2, Filt1),
    
    IsProperty = fun (Operand) ->
                         is_atom(Operand) andalso
                             ?Lib:prop_fun(EntityType, Operand) /= [] end,
    Filt1IsProperty = IsProperty(Filt1),
    Filt2IsProperty = IsProperty(Filt2),

    case {LFun, RFun, Entities, CompOp} of
        {{variable, Var}, _, [Entity], '=='} when Filt2IsProperty ->
            bind_variable(Var, RFun(Entity), State);
        {_, {variable, Var}, [Entity], '=='} when Filt1IsProperty ->
            bind_variable(Var, LFun(Entity), State);
        {_, _, _, _} when is_tuple(LFun) orelse is_tuple(RFun) ->
            filter(false, State);
        _ ->
            DoesTypeMatter = (CompOp == '=:=') orelse (CompOp == '=/='),
            FilteredEntities = 
                ordsets:filter(
                  fun(Entity) ->
                          CompL = LFun(Entity),
                          CompR = RFun(Entity),
                          HasSameType = type_of(CompL) == type_of(CompR),
                          if CompOp /= '~' andalso (HasSameType orelse DoesTypeMatter) ->
                                  compare(CompOp, CompL, CompR);
                             true ->
                                  S1 = to_string(CompL),
                                  S2 = to_string(CompR),
                                  compare(CompOp, S1, S2)
                          end
                  end,
                  Entities),
            State#state{res = FilteredEntities}
    end;

filter(Filter, #state{type = EntityType, res = Entities} = State) ->
    State#state{res = ordsets:filter(prop_fun(EntityType, Filter), Entities)}.

get_setop_value(State) ->
    ToOrdset = fun(Result = #state{res = Entities}) ->
                       Result#state{res = ordsets:from_list(Entities)} end,
    fun({cons, L}) ->
            fun(_, SomeState) -> 
                    [SomeState#state{res = ordsets:from_list(all_to_string(L))}]
            end;
       ({seq, S}) ->
            fun(E, SomeState) ->
                    Processed = process_query(
                                  #state{
                                     res = [E],
                                     type = State#state.type,
                                     params = State#state.params,
                                     variables = SomeState#state.variables
                                    },
                                  S,
                                  []),
                    lists:map(fun (Result) ->
                                      property_to_string(ToOrdset(Result)) end, 
                              Processed)
            end;
       ({'query', SemQuery}) ->
            fun(_, SomeState) -> 
                    StartState = #state{
                      params = State#state.params,
                      variables = SomeState#state.variables},
                    Processed = process_query(StartState, SemQuery, []),
                    lists:map(fun (Result) ->
                                      property_to_string(ToOrdset(Result)) end,
                              Processed)
            end;
       (Atom) when is_atom(Atom) ->
            case ?Lib:prop_fun(State#state.type, Atom) of
                [Fun] ->
                    fun(E, SomeState) ->
                            [SomeState#state{res = [to_string(Fun(E))]}]
                    end;
                []    ->
                    fun(_, SomeState) ->
                            [SomeState#state{res = [to_string(Atom)]}]
                    end
            end;
       ({variable, Var}) ->
            Val = case lookup_variable(Var, State) of
                      {Type, {value, Value}} -> 
                          case is_property_type(Type) of
                              true -> [to_string(Value)];
                              false -> [Value]
                          end;
                      _ -> []
                  end,
            fun(_, SomeState) -> 
                    [SomeState#state{res = Val}]
            end;
       (Literal) -> 
            fun(_, SomeState) ->
                    [SomeState#state{res = [to_string(Literal)]}]
            end
    end.

%%% ============================================================================
%%% Helper functions

compare(like, CompL, CompR) ->
    Distance =
        if
            is_list(CompL) andalso is_list(CompR) ->
                refusr_strm:getDistance(CompL, CompR);
            is_atom(CompL) andalso is_atom(CompR) ->
                refusr_strm:getDistance(atom_to_list(CompL),
                                        atom_to_list(CompR));
            true -> false
        end,

    case Distance of
         {lev, 0} -> true;
         {lev, 1} -> true;
         {lev, 2} ->
            Length = if is_atom(CompL) -> length(atom_to_list(CompL));
                        true           -> length(CompL)
                     end,
            case Length > 5 of true -> true; _ -> false end;
        _ -> false
    end;

compare('~', CompL, CompR) when is_atom(CompR)->
    compare('~', CompL, atom_to_list(CompR));
compare('~', CompL, CompR) when is_atom(CompL)->
    compare('~', atom_to_list(CompL), CompR);
compare('~', CompL, {quoted, A}) when is_atom(A)->
    compare('~', CompL, atom_to_list(A));
compare('~', {quoted, A}, CompR) when is_atom(A)->
    compare('~', atom_to_list(A), CompR);
compare('~', CompL, CompR) ->
    RegExp = case re:compile(CompR) of
        {ok, MP} -> MP;
        {error, _} -> throw(?LocalErr0r(bad_regexp))
    end,
    case re:run(CompL, RegExp) of
        {match, _} -> true;
        nomatch -> false
    end;

compare(CompOp, CompL, CompR) ->
    erlang:CompOp(CompL, CompR).

prop_fun(EntityType, Filter) ->
    case ?Lib:prop_fun(EntityType, Filter) of
        [] -> throw(?LocalError(illegal_property, [EntityType, Filter]));
        [Fun]   -> Fun
    end.

%% @spec format_nodes(Nodes::[entity()], Positions::atom()) -> string()
%%       Positions = none|scalar|linecol
%% @doc Returns a textual representation for a list of nodes.
format_nodes(Nodes, Position) -> ?Format:nodes(Nodes, Position).

async_process_parallel(States) ->
    ?MISC:async_call(node(), ?MODULE, process_parallel, [States]).

process_parallel([First | _] = States) ->
    RemainingQuery = First#state.semquery,
    RemainingQueryLastPart = First#state.semquery_last,
    process_parallel(States, RemainingQuery, RemainingQueryLastPart).

process_parallel(States, Query, LastQuery) ->
    Keys = [?MISC:async_call(node(), ?MODULE, process_query,
                             [State, Query, LastQuery]) || State <- States],
    lists:flatmap(fun rpc_yield/1, Keys).

entitynumber_increment(Query)->
    IncrementingElements =
        lists:filter(fun({initial_selector, _}) ->
                             false;
                        ({filter, _}) ->
                             false;
                        ({filter_with_variable_to_bind, _, _}) ->
                             false;
                        ({variable_match, _, _}) ->
                             false;
                        ({variable_bind, _}) ->
                             false;
                        (_) ->
                             true end,
                     Query),
    length(IncrementingElements).

lookup_variable(Variable, State) ->
    case orddict:find(Variable, State#state.variables) of
        {ok, TypeAndValue} ->
            TypeAndValue;
        error ->
            none
    end.

lookup_variable_value(Variable, State) ->
    {_, Value} = orddict:fetch(Variable, State#state.variables),
    Value.

add_variable(Variable, Type, State) ->
    orddict:store(Variable, {Type, undefined}, State#state.variables).

bind_variable(Variable, Value, State) ->
    Update = fun ({Type, undefined}) ->
                     {Type, {value, Value}} end,
    State#state{
      variables = orddict:update(Variable, Update, State#state.variables)}.

has_variable_got_bound(States, OldState) when is_list(States) andalso
                                              is_record(OldState, state) ->
    EarlierVariableList = OldState#state.variables,
    StatesWithNewVariables = 
        lists:dropwhile(fun (#state{variables = Variables}) ->
                                Variables == EarlierVariableList end,
                        States),
    case StatesWithNewVariables of
        [] ->
            false;
        _ ->
            true
    end;
has_variable_got_bound(NewState, OldState) 
  when is_record(NewState, state) andalso
       is_record(OldState, state)->
    NewState#state.variables /= OldState#state.variables;
has_variable_got_bound(Variables1, Variables2) ->
    Variables1 /= Variables2.

variables_got_bound(NewState, OldState) ->
    AlreadyExistingVariables = bound_variables(OldState),
    MoreVariables = bound_variables(NewState),
    ordsets:subtract(MoreVariables, AlreadyExistingVariables).

bound_variables(State) ->
    orddict:fetch_keys(State#state.variables).

is_variable_bound(Variable, State) ->
    case lookup_variable_value(Variable, State) of
        {value, _} ->
            true;
        valueless ->
            true;
        _ -> 
            false
    end.

are_variables_bound(Variables, State) ->
    lists:all(fun (Variable) -> is_variable_bound(Variable, State) end, Variables).

compound_bind(#state{variables = Vars}) ->
    case Vars of
        [] -> false;
        [H | _] -> is_list(H)
    end.             

separate_binds(State) ->
    case compound_bind(State) of
        false ->
            [State];
        true ->
            Binds = vars(State),
            [State#state{variables = Vars} || Vars <- Binds]
    end.

mark_undefined_as_valueless(Identifiers, States) when is_list(States) ->
    lists:map(fun (State) ->
                      mark_undefined_as_valueless(Identifiers, State) end,
              States);
mark_undefined_as_valueless(Identifiers, State) ->
    Variables = State#state.variables,
    Update = fun ({Type, undefined}) ->
                     {Type, valueless};
                 (StoredInfo) ->
                     StoredInfo end,
    NewVariables = 
        lists:foldl(fun (Identifier, SomeVariables) ->
                            orddict:update(Identifier, Update, SomeVariables)
                    end,
                    Variables,
                    Identifiers),
    State#state{variables = NewVariables}.

rethrow_badrpc({badrpc, _} = Exception) ->
    error(Exception);
rethrow_badrpc(Results) when is_list(Results)->
    lists:foreach(fun rethrow_badrpc/1, Results);
rethrow_badrpc(_) ->
    ok.

filter_closure_result(NewEntities, EntitiesWithVariableBinding,
                      EntitiesWithoutVariableBinding) ->
    SetofEntities = gb_sets:from_list(NewEntities),
    PossiblyNewEntities = 
        gb_sets:difference(SetofEntities, EntitiesWithoutVariableBinding),
    ReallyNewEntities = 
        gb_sets:difference(PossiblyNewEntities, EntitiesWithVariableBinding),
    NewEntitiesWithVariableBinding = 
        gb_sets:union(SetofEntities, EntitiesWithVariableBinding),
    NewEntitiesWithoutVariableBinding = 
        gb_sets:difference(EntitiesWithoutVariableBinding, ReallyNewEntities),
    {gb_sets:to_list(ReallyNewEntities), 
     NewEntitiesWithVariableBinding,
     NewEntitiesWithoutVariableBinding}.

process_filter_with_variable_to_bind(FilterElement, State) ->
    {_, Filter, VariablesToBind} = FilterElement,
    case are_variables_bound(VariablesToBind, State) of
        true ->
            process({filter, Filter}, State);
        false ->
            StateToRunFilterOn = State#state{parallel_key = []},
            StashedKeys = State#state.parallel_key,
            FilteredStates = not_empty_states(
                               filter_entities_separately(Filter,
                                                          StateToRunFilterOn)),
            Separated = lists:flatmap(fun separate_binds/1, FilteredStates),
            MarkedStates = mark_undefined_as_valueless(VariablesToBind,
                                                       Separated),
            case MarkedStates of
                [] ->
                    EmptyState = State#state{res = [], groupby_res = []},
                    mark_undefined_as_valueless(VariablesToBind, EmptyState);
                [NewState] ->
                    NewState#state{parallel_key = StashedKeys};
                [FirstState | States] ->
                    Key = ?MISC:async_call(node(), ?MODULE, process_parallel,
                                           [States]),
                    FirstState#state{parallel_key = [Key | StashedKeys]}
            end
    end.

filter_entities_separately(Filter, State = #state{res = [H | _]})
  when is_list(H) ->
    lists:append(
      lists:zipwith(fun (GroupingEntity, Group) ->
                            filter_group_separately(GroupingEntity, Group,
                                                    Filter, State)
                    end,
                    State#state.groupby_res,
                    State#state.res));
    
filter_entities_separately(Filter, State) ->
    pmap({?MODULE, pmap_helper},
         [fun(Entity) -> 
                  filter(Filter, State#state{res = [Entity]})
          end],
         State#state.res).

filter_group_separately(GroupingEntity, Group, Filter, State) ->
    ToGroupedState = fun (#state{res = Entity, variables = Variables}) ->
                             State#state{res = [Entity], 
                                         groupby_res = [GroupingEntity],
                                         variables = Variables} end,
    StateOfGroup = 
        to_nongrouped_state(State#state{res = Group}),
    FilteredStates = 
        filter_entities_separately(Filter, StateOfGroup),
    lists:map(ToGroupedState, FilteredStates).

res(State) ->
    State#state.res.

vars(State) ->
    State#state.variables.
