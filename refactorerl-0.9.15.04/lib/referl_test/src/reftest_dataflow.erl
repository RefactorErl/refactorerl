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

%%% @author Mate Tejfel <matej@inf.elte.hu>

-module(reftest_dataflow).
-vsn("$Rev: 5069 $ ").

%% Random module based testing callbacks
-export([prepare/1, perform_and_check/2]).

%% Interface
-export([prop_df/0, graph_dataflow_errors/0, test_dataflow_db/2]).

-include("test.hrl").



%% @doc Dataflow analyzer test using the test database.
%% The first parameter determines the library, the second the number of 
%% the analysed files from the library.
%%
%% @spec test_dataflow_db(int(), int()) -> boolean() 
test_dataflow_db(X,Y) ->
    io:format("The results are:~p~n",  [[db_aux(N,Y) || N <-lists:seq(1, X)]]).

%% @doc Dataflow analyzer test.
%%
%% @spec test_dataflow() -> boolean() 
test_dataflow() ->
    case graph_dataflow_errors() of
        []    -> true;
        Bads  -> 
            io:format("Errors:~p~n",[Bads]),
            false
    end.    

%% @doc Prepare function for random module based testing.
prepare(Mods) -> [Mods].

%% @doc "Perform and check" function for random module based testing.
perform_and_check(_ModsOrFiles, _Args) -> test_dataflow(). 


%% @doc Checks the dataflow consistency of the stored graph.
%%
%% @spec prop_df() -> boolean()
prop_df() ->
    graph_dataflow_errors() =:= [].

%% @spec graph_dataflow_errors() -> [tuple()]
%% @doc  Returns the dataflow consistency errors in the graph.
graph_dataflow_errors() ->
    TestFuns  = [fun  match_expr_test/1, fun  pattern_test/1, 
        fun expr_cons_test/1, fun expr_tuple_test/1, fun variable_test/1,
        fun fpar_test/1, fun fret_test/1, fun call_test/1, fun ret_test/1,
        fun case_test/1],
    Res = [[R || R <- lists:flatten(df_test(X)), R =/= ok] || X <- TestFuns],
    lists:flatten(Res).
    

df_test(Fun) ->
   Moduls = ?Query:exec(?Mod:all()),
   lists:map(Fun, Moduls).


%% @doc Match_expr test. It checks that in all match_expr there is a flow edge
%% from "esub/2" to "esub/1" and there is a flow edge from "esub/2" to match_expr.
%% It is also cheks that in all match_expr pattern there is a flow edge from
%% match_expr to esub/1 and esub/2.
%%
%% @spec match_expr_test(atom()) -> [tuple()]
match_expr_test(Module) ->
    QuerySeq = ?Query:seq([?Mod:locals(),
        ?Fun:definition(), ?Form:deep_exprs()]),
    ExprNodes = ?Query:exec(Module, QuerySeq),
    MatchExprNodes = [ExprNode || ExprNode <- ExprNodes,
        ?Expr:type(ExprNode) == match_expr],
    lists:map(fun propChilds/1, MatchExprNodes).

propChilds(MatchExpr) ->
    Child1 = ?Query:exec(MatchExpr, ?Expr:child(1)),
    case Child1 of
        [] -> {matchexpr_has_no_esub_1, MatchExpr};
        _  -> Ch1 = hd(Child1),
              Child2 = ?Query:exec(MatchExpr, ?Expr:child(2)),
              case Child2 of
                  [] -> {matchexpr_has_no_esub_2, MatchExpr};
                  _  -> Ch2 = hd(Child2),
                        case ?Expr:role(MatchExpr) of
                            pattern -> 
                                check_matchPattern(MatchExpr, Ch1, Ch2);
                            _       -> 
                                check_matchExpr(MatchExpr, Ch1, Ch2)                          
                        end         
              end
    end.

check_matchPattern(MatchExpr, Ch1, Ch2) ->
    MatchFlows = ?Query:exec(MatchExpr, [flow]),
    case lists:member(Ch1, MatchFlows) of
        true ->
            case lists:member(Ch2, MatchFlows) of
                true -> ok;
                false ->
                  {missing_flow_from_match_expr_pattern_to_esub2, 
                         MatchExpr, Ch2, MatchFlows}
            end;
        false -> {missing_flow_from_match_expr_pattern_to_esub1, 
                         MatchExpr, Ch1, MatchFlows}
    end.

check_matchExpr(MatchExpr, Ch1, Ch2) ->
    Ch2Flows = ?Query:exec(Ch2, [flow]),
    case lists:member(Ch1, Ch2Flows) of
        true  -> 
           case lists:member(MatchExpr, Ch2Flows) of
              true  -> ok;
              false -> 
                 {missing_flow_in_match_expr_from_esub2__to_expr,
                    MatchExpr, Ch2, Ch2Flows}
           end;
        false -> {missing_flow_in_match_expr_from_esub2_to_esub1, 
                         MatchExpr, Ch1, Ch2, Ch2Flows}
    end.



%% @doc Pattern test. It checks that by all cons pattern, which have esub child 
%%    there exist sel_e edges and by all tuple pattern there exist sel edges. 
%% 
%% @spec pattern_test(atom()) -> [tuple()]
pattern_test(Module) -> 
    QuerySeq = ?Query:seq([?Mod:locals(),
        ?Fun:definition(), ?Form:deep_exprs()]),
    ExprNodes = ?Query:exec(Module, QuerySeq),
    MatchExprNodes = [ExprNode || ExprNode <- ExprNodes,
        ?Expr:type(ExprNode) == match_expr],
    lists:map(fun propPatterns/1, MatchExprNodes).
    

propPatterns(MatchExpr) ->
    Child1 = ?Query:exec(MatchExpr, ?Expr:child(1)),
    case Child1 of
        [] -> {matchexpr_has_no_esub_1, MatchExpr};
        _  -> Ch1 = hd(Child1),
              case ?Expr:role(Ch1) of
                  pattern -> 
                      case ?Expr:type(Ch1) of
                          tuple -> selFind(Ch1);
                          cons  -> sel_eFind(Ch1);
                          _     -> ok
                      end; 
                  _      -> ok
              end
    end.
   

selFind(Node) ->
    Children = ?Query:exec(Node, ?Expr:children()),
    SelNodes = ?Query:exec(Node, [sel]),
    case [Bad || {Bad,Other} <- lists:zip(Children, SelNodes), 
                                              not (Bad == Other)] of
        [] -> ok;
        Bads  -> {tuple_has_child_without_sel, Node, SelNodes, Bads}
    end.
    
sel_eFind(Node) ->
    Children = ?Query:exec(Node, ?Query:seq([?Expr:child(1),?Expr:children()])), 
    SelNodes = ?Query:exec(Node, [sel_e]),
    case [Bad || Bad <- Children, not lists:member(Bad, SelNodes)] of
        [] -> ok;
        Bads  -> {cons_has_child_without_sel_e, Node, SelNodes, Bads}
    end.
    
    

%% @doc Cons expression test. It checks that by all cons expression, 
%%   which have esub child there exist cons_e edges.
%% 
%% @spec expr_cons_test(atom()) -> [tuple()]
expr_cons_test(Module) -> 
    QuerySeq = ?Query:seq([?Mod:locals(),
        ?Fun:definition(), ?Form:deep_exprs()]),
    ExprNodes = ?Query:exec(Module, QuerySeq),
    ConsExprNodes = [ExprNode || ExprNode <- ExprNodes,
        (?Expr:type(ExprNode) == cons andalso ?Expr:role(ExprNode) == expr)],
    lists:map(fun propExprCons/1, ConsExprNodes).

    
propExprCons(ConsExprNode) ->
    Child1 = ?Query:exec(ConsExprNode, ?Expr:child(1)),
    case Child1 of
        [] -> ok;
        _  -> Children = ?Query:exec(Child1, ?Expr:children()),
              case [Bad || Bad <- Children, 
                       not lists:member(ConsExprNode, 
                                 ?Query:exec(Bad, [cons_e]))] of
                 []    -> ok;
                 Bads  -> 
                     {consExprHasChildWithoutCons_e, ConsExprNode, Bads}
              end
    end.



%% @doc Tuple expression test. It checks that by all tuple expression, 
%%   there exist cons_back edges.
%% 
%% @spec expr_tuple_test(atom()) -> [tuple()]
expr_tuple_test(Module) -> 
    QuerySeq = ?Query:seq([?Mod:locals(),
        ?Fun:definition(), ?Form:deep_exprs()]),
    ExprNodes = ?Query:exec(Module, QuerySeq),
    TupleExprNodes = [ExprNode || ExprNode <- ExprNodes,
         (?Expr:type(ExprNode) == tuple andalso ?Expr:role(ExprNode) == expr)],
    lists:map(fun propExprTuple/1, TupleExprNodes).

propExprTuple(TupleExprNode) ->
    Children  = ?Query:exec(TupleExprNode, ?Expr:children()),
    ConsBacks = ?Query:exec(TupleExprNode, [cons_back]),
    case [Bad || {Bad,Other} <- lists:zip(Children, ConsBacks), 
                                              not (Bad == Other)] of
      []    -> ok;
      Bads  -> {tupleExprHasChildWithoutConsBack, TupleExprNode, Bads}
    end.
    


%% @doc Variable test. It checks that from nodes where a variable is 
%% binded there exist flow edge to the nodes where the variable is referred.
%% 
%% @spec variable_test(atom()) -> [tuple()]
variable_test(Module) -> 
    QuerySeq = ?Query:seq([?Mod:locals(),
        ?Fun:definition(), ?Form:deep_exprs(), ?Expr:variables()]),
    VarNodes = lists:usort(?Query:exec(Module, QuerySeq)),
    lists:map(fun propVar/1, VarNodes).

propVar(VarNode) ->
    Bindings = ?Query:exec(VarNode, ?Var:bindings()),
    BindFlows = ?Query:exec(Bindings, [flow]),
    References = ? Query:exec(VarNode, ?Var:references()),
    case [Bad || Bad <- References, not lists:member(Bad,BindFlows)] of
        [] -> ok; 
        Bads -> {varReferencesWithoutBindFlow, Bads, VarNode}
    end.    


%% @doc FPar test. It checks that from all fpar node, 
%%   there exist flow edge to the appropriate pattern.
%% 
%% @spec fpar_test(atom()) -> [tuple()]
fpar_test(Module) ->
    Functions = ?Query:exec(Module, ?Mod:locals()),
    lists:map(fun propFPar/1, Functions).

propFPar(Function) ->
    FPars = ?Query:exec(Function, [fpar]),
    Clauses = ?Query:exec(Function, 
        ?Query:seq([?Fun:definition(), ?Form:clauses()])),
    Res = [propFPar_(FPars,?Query:exec(Clause, ?Clause:patterns())) 
                   || Clause <- Clauses],
    case lists:member(false, lists:flatten(Res)) of
        true -> {missingFlowFromFparToPattern, Function, FPars};
        _    -> ok
    end.                       

propFPar_(FPars, Patterns) -> 
    lists:map(fun({X,Y}) -> lists:member(X,Y) end, 
               lists:zip(Patterns, [?Query:exec(Z,[flow]) || Z <- FPars])).      
        


%% @doc FRet test. It checks that to all fret node, 
%%   there exist flow edge from the appropriate last expression of 
%%   the function.
%% 
%% @spec fret_test(atom()) -> [tuple()]
fret_test(Module) ->
    Functions = ?Query:exec(Module, ?Mod:locals()),
    lists:map(fun propFRet/1, Functions).

propFRet(Function) ->
    FRets = ?Query:exec(Function, [fret]),
    case FRets of
        [] ->   {missingFRetsInFunction, Function, ?Fun:name(Function)};
        _  ->   Clauses = ?Query:exec(Function, 
                    ?Query:seq([?Fun:definition(), ?Form:clauses()])),
                case Clauses of
                    [] -> ok;
                    _  ->     
                        Res = [propFRet_(lists:zip(FRets,?Query:exec(Clause, 
                                                              [{body,last}]))) 
                                       || Clause <- Clauses],
                        case lists:member(false, lists:flatten(Res)) of
                            true -> {missingFlowFromLastExprToFRet, 
                                     Function, 
                                     FRets, 
                                     ?Query:exec(Clauses, [{body,last}]) };
                            _    -> ok
                        end
        end                
    end.                       

propFRet_(FRets_LastExprs) when is_list(FRets_LastExprs) -> 
    [LastExpr || {FRet, LastExpr} <- FRets_LastExprs,
              not (lists:member(FRet, ?Query:exec(LastExpr,[flow])))]
    == [].


%% @doc Call test. It checks that by all function application, 
%%   if the body of the called function exists there is a "call" edge 
%%   from every argumentum to the corresponding fpar node. 
%%
%% @spec call_test(atom()) -> [tuple()]
call_test(Module) ->
    QSeq = ?Query:seq([?Mod:locals(),
        ?Fun:definition(), ?Form:deep_exprs()]),
    ExprNodes = ?Query:exec(Module, QSeq),
    ApplExprs_CalledFuns = [{ExprNode,?Query:exec(ExprNode, ?Expr:function())} 
         || ExprNode <- ExprNodes,
            (?Expr:type(ExprNode) == application 
                 andalso ?Expr:role(ExprNode) == expr)],
    ApplExprs_CalledPars = [{ExprNode, ?Query:exec(Fun,[fpar])} 
         || {ExprNode,Fun} <- ApplExprs_CalledFuns, 
            not (?Query:exec(Fun,?Fun:definition()) == [])],
    lists:map(fun propCall/1, ApplExprs_CalledPars).

propCall({ExprNode,Pars}) ->
    QSeq = ?Query:seq([?Expr:child(2), ?Expr:children()]),
    Args = ?Query:exec(ExprNode, QSeq),
    BadList1 = [{Bad,?Query:exec(Bad,[call])}  || Bad <- Args,
                       not([X || X <- ?Query:exec(Bad,[call]), 
                                  not (lists:member(X, Pars))] == [])], 
    BadList2 = [{Bad, ?Query:exec(Args, [call])} || Bad <- Pars,
                       not (lists:member(Bad, ?Query:exec(Args, [call])))],
    case {BadList1, BadList2} of
        {[],[]} -> ok;
        _       -> {missingCallEdge, ExprNode, Args, Pars, BadList1, BadList2} 

    end.


%% @doc Ret test. It checks that by all function application, 
%%   if the body of the called function exists there is a "ret" edge 
%%   from the corresponding fret node. 
%%
%% @spec ret_test(atom()) -> [tuple()]
ret_test(Module) ->
    QSeq = ?Query:seq([?Mod:locals(),
        ?Fun:definition(), ?Form:deep_exprs()]),
    ExprNodes = ?Query:exec(Module, QSeq),
    ApplExprs_CalledFuns = [{ExprNode,?Query:exec(ExprNode, ?Expr:function())} 
         || ExprNode <- ExprNodes,
            (?Expr:type(ExprNode) == application 
                 andalso ?Expr:role(ExprNode) == expr)],
    ApplExprs_CalledRets = [{ExprNode, ?Query:exec(Fun,[fret])} 
         || {ExprNode,Fun} <- ApplExprs_CalledFuns, 
            not (?Query:exec(Fun,?Fun:definition()) == [])],
    lists:map(fun propRet/1, ApplExprs_CalledRets).

propRet({ExprNode,Rets}) ->
    BadList1 = [{Bad,?Query:exec(Bad,[ret])}  || Bad <- Rets,
                       not(lists:member(ExprNode,?Query:exec(Bad,[ret])))],
    RetNodes =  ?Query:exec(ExprNode, [{ret, back}]),                  
    BadList2 = [{Bad, RetNodes} || Bad <- Rets,
                       not (lists:member(Bad, RetNodes))],
    case {BadList1, BadList2} of
        {[],[]} -> ok;
        _       -> {missingRetEdge, ExprNode, Rets, BadList1, BadList2} 
    end.



%% @doc Case, If, Try test. It checks that in all case_expr, if_expr and try_expr 
%% there is a flow edge from last expressions to the case.
%% It also checks that in all case_expr there is a flow edge from the head expr
%% to the pattern expressions.
%%
%% @spec case_test(atom()) -> [tuple()]
case_test(Module) ->
    QuerySeq = ?Query:seq([?Mod:locals(),
        ?Fun:definition(), ?Form:deep_exprs()]),
    ExprNodes = ?Query:exec(Module, QuerySeq),
    CaseIfTryNodes = [ExprNode || ExprNode <- ExprNodes,
        (?Expr:type(ExprNode) == case_expr orelse 
         (?Expr:type(ExprNode) == if_expr orelse 
           ?Expr:type(ExprNode) == try_expr))],
    CaseNodes = [Node || Node <- CaseIfTryNodes, ?Expr:type(Node) == case_expr],
    lists:map(fun propCase/1, CaseIfTryNodes) ++
       lists:map(fun propCase2/1, CaseNodes).

propCase(Node) -> 
    QuerySeq = ?Query:seq([?Query:all([catchcl], [exprcl]), [{body,last}]]), 
    LastExprs = ?Query:exec(Node, QuerySeq), 
    BadList = [Bad || Bad <- LastExprs,
                        not (lists:member(Node, ?Query:exec(Bad, [flow])))],
    case BadList of
        [] -> ok;
        _  -> {missingFlowInCaseIfTryFromLastExpr, Node, BadList}
    end.

propCase2(CaseNode) -> 
    QuerySeq = ?Query:seq([[headcl], ?Clause:body()]),
    HeadB = hd(?Query:exec(CaseNode, QuerySeq)), 
    QuerySeq2 =  ?Query:seq([[exprcl], ?Clause:patterns()]),
    Patterns = ?Query:exec(CaseNode, QuerySeq2),
    BadList = [Bad || Bad <- Patterns,
                        not (lists:member(Bad, ?Query:exec(HeadB, [flow])))],
    case BadList of
        [] -> ok;
        _  -> {missingFlowInCaseFromHeadToPattern, CaseNode, BadList}
    end.                    
   



%%% ===========================================================================
%%% Auxulary functions 

%% @private
db_aux(N,Y) ->
    ri:reset(),
    [refcore_fileman:add_file("../db/" ++ integer_to_list(N) ++ "/t" 
        ++ integer_to_list(M) ++ ".erl") || M <- lists:seq(1, Y)],
    test_dataflow().

  



