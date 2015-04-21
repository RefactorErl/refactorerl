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

-module(reftest_cfg).
-vsn("$Rev: 5069 $ ").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Random module based testing callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([prepare/1, perform_and_check/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([prop_cfg/0, graph_cfg_errors/0, test_cfg_db/2]).

-include("test.hrl").

-define(CFGSRV,  refsc_cfg_server).


%% @doc Control flow analyzer test using the test database.
%% The first parameter determines the library, the second the number of 
%% the analysed files from the library.
%%
%% @spec test_cfg_db(int(), int()) -> boolean() 
test_cfg_db(X,Y) ->
    io:format("The results are:~p~n",  [[db_aux(N,Y) || N <-lists:seq(1, X)]]).


%% @doc Control flow analyzer test.
%%
%% @spec test_cfg() -> boolean() 
test_cfg() ->
    case graph_cfg_errors() of
        []    -> true;
        Bads  -> 
            io:format("Errors:~p~n",[Bads]),
            false
    end.   


%% @doc  Returns the CFG consistency errors.
%% @spec graph_cfg_errors() -> [tuple()]
graph_cfg_errors() ->
   QuerySeq = ?Query:seq([[file],
        ?File:forms()]),
   ?CFGSRV:start_link(),
   Forms = [X || X <- ?Query:exec(QuerySeq), ?Form:type(X) == func],
   ?CFGSRV:rebuild_cfgs(Forms),
   FormsCFGs = [?CFGSRV:get_cfg(X) || X <- Forms],
   %?d(FormsCFGs),
   TestFuns  = [fun is_form_node/1, fun expr_test/1],
   Res = [
          [R || R <- lists:flatten(lists:map(X,FormsCFGs)), R =/= ok] 
               || X <- TestFuns],
   lists:flatten(Res).
   
 %% @doc Prepare function for random module based testing.
prepare(Mods) -> [Mods].

%% @doc "Perform and check" function for random module based testing.
perform_and_check(_ModsOrFiles, _Args) -> test_cfg().   

%% @doc Checks the consistency of the CFG graph.
%%
%% @spec prop_cfg() -> boolean()
prop_cfg() ->
    graph_cfg_errors() =:= [].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% --- Testing functions ---
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% doc Start_ret test. Checks the existence of the start and ret edge of
%%      the form.
%%
%%start_test({Form,CFG}) ->
%%   L = lists:member({{start,Form}, Form, yes}, CFG) andalso
%%         lists:member({{start,Form}, {ret,Form}, no}, CFG),
%%   case L of
%%      true -> ok;
%%      _    -> {cfg_missingStartOrRet, Form, CFG}
%%   end.
 
%% @doc Basic test. Checks the basic edge from the form node.
%%
is_form_node({Form,CFG}) ->
   case suitB(Form, CFG) of
      true -> ok;
      _    -> {cfg_missingEdgeFromNode, Form}
   end.

%% @doc Expression node test.
expr_test({Form,CFG}) ->
   QuerySeq = ?Query:seq([?Form:clauses(),
        ?Clause:body()]),
   Exprs1 = ?Query:exec(Form, QuerySeq),
   Exprs = [Expr || 
             Expr 
              <- lists:flatten(
                        lists:map(
                            fun(X) -> ?Query:exec(X,?Expr:deep_sub()) end, 
                                  Exprs1))],
   Funs = [fun check_infix/1, fun check_par/1, fun check_tup/1, 
            fun check_cons/1, fun check_list_comp/1],
   lists:flatten(lists:append([lists:map(X, [{Exprs,CFG}]) || X <- Funs])).      
   %%lists:append[check_infix({Exprs,CFG}), check_par({Exprs, CFG}). 



%% @doc Infix operator test.
check_infix({Exprs,CFG}) ->
   InfExprs = [Expr || Expr <- Exprs,
                        (not (?Expr:role(Expr) == guard)) andalso
                          (?Expr:type(Expr) == match_expr orelse 
                            ?Expr:type(Expr) == infix_expr)],
   [check_infexpr1(Expr,CFG) || Expr <- InfExprs] 
      ++ [check_infexpr2(Expr,CFG) || Expr <- InfExprs].
         %% NOTE: the rules (Left assoc. op.) and (Right assoc. op.) are
         %% wrong in report 2010 ... 
   
   
%% @doc Infix operator test, first part. Checks the corresponding
%%      edge from the first operand to the next one.
%%   
check_infexpr1(Expr,CFG) ->
   Node1 = hd(?Query:exec(Expr,?Expr:child(1))),
   RetNode1 = ret(Node1),
   Ch2 = hd(?Query:exec(Expr,?Expr:child(2))),
   Node2 = getNextPat(Ch2),
   case ?Expr:type(Node1) of
      application ->     
           case lists:member({RetNode1, Node2, funcall}, CFG) of
              true -> ok;
              _    -> {cfg_missingEdgeInInfix1_app, RetNode1, Node2}
           end;
      _  ->     
           case lists:member({RetNode1, Node2, []}, CFG) of
              true -> ok;
              _    -> {cfg_missingEdgeInInfix1, RetNode1, Node2}
           end
   end.
 

%% @doc Infix operator test, second part. Checks the corresponding
%%      edge from the second operand to operator's node.
%%  
check_infexpr2(Expr,CFG) -> 
   Node = ret(hd(?Query:exec(Expr,?Expr:child(2)))),
   case suit(Node, Expr, CFG) of
      true -> ok;
      _    -> {cfg_missingEdgeInInfix2, Node, Expr}
   end.
 

%% @doc Parenthesis test.
check_par({Exprs,CFG}) ->
   ParExprs = [Expr || Expr <- Exprs,
                          (?Expr:type(Expr) == parenthesis)],
   [check_parexp(Expr,CFG) || Expr <- ParExprs].

%% @doc Parenthesis test. Checks the corresponding
%%      edge from the node of the parenthised item to the node of
%%      the parenthesis.
%%  
check_parexp(Expr,CFG) ->
   Node1 = hd(?Query:exec(Expr,?Expr:child(1))),
   RetNode1 = ret(Node1),
   case ?Expr:type(Node1) of
      application ->     
           case lists:member({RetNode1, Expr, funcall}, CFG) of
              true -> ok;
              _    -> {cfg_missingEdgeInParenthesis_app, RetNode1, Expr}
           end;
      _ ->     
           case lists:member({RetNode1, Expr, []}, CFG) of
              true -> ok;
              _    -> {cfg_missingEdgeInParenthesis, RetNode1, Expr}
           end
   end. 

%% @doc Tuple test.
check_tup({Exprs,CFG}) ->
   TupExprs = [Expr || Expr <- Exprs,
                          (?Expr:type(Expr) == tuple) andalso not(?Expr:role(Expr) == pattern)],
   lists:flatten([check_tupexp(Expr,?Query:exec(Expr,?Expr:children()), CFG) || Expr <- TupExprs]).

%% @doc Tuple test. Checks the corresponding
%%      edges between the nodes of the  item to the node of adjacent children
%%      and from the last child to the tuple.
%%  

check_tupexp(_Expr,[], _CFG) -> [];

check_tupexp(Expr,[Ch], CFG) -> 
   RetNode = ret(Ch),
   case ?Expr:type(Ch) of
      application ->     
           case lists:member({RetNode, Expr, funcall}, CFG) of
              true -> [];
              _    -> [{cfg_missingLastEdgeInTuple_app, RetNode, Expr}]
           end;
      _ ->     
           case lists:member({RetNode, Expr, []}, CFG) of
              true -> [];
              _    -> [{cfg_missingLastEdgeInTuple, RetNode, Expr}]
           end
   end;

check_tupexp(Expr,[Ch1, Ch2 | Chs], CFG) ->
   Errors = check_tupexp(Expr,[Ch2 | Chs], CFG),
   RetNode = ret(Ch1),
   Node2 = getNextPat(Ch2),
   case ?Expr:type(Ch1) of
      application ->     
           case lists:member({RetNode, Node2, funcall}, CFG) of
              true -> Errors;
              _    -> [{cfg_missingEdgeInTuple_app, RetNode, Node2} | Errors]
           end;
      _ ->     
           case lists:member({RetNode, Node2, []}, CFG) of
              true -> Errors;
              _    -> [{cfg_missingEdgeInTuple, RetNode, Node2} | Errors]
           end
   end. 


%% @doc Cons test.
check_cons({Exprs,CFG}) ->
   ConsExprs = [Expr || Expr <- Exprs,
                          (?Expr:type(Expr) == cons) andalso not(?Expr:role(Expr) == pattern)],
   lists:flatten([check_consexp(Expr,?Query:exec(Expr,?Query:seq([?Expr:child(1), ?Expr:children()])), CFG) || Expr <- ConsExprs]).

%% @doc Cons test. Checks the corresponding
%%      edges between the nodes of the  item to the node of adjacent children.
%%  

check_consexp(_Expr,[], _CFG) -> [];

check_consexp(Expr,[Ch], CFG) ->
   ListNode = hd(?Query:exec(Expr, ?Expr:child(1))), 
   Errors = check_consend(ListNode, Expr, CFG),
   RetNode = ret(Ch),
   case ?Expr:type(Ch) of
      application ->     
           case lists:member({RetNode, ListNode, funcall}, CFG) of
              true -> Errors;
              _    -> [{cfg_missingEdgeInCons_app, RetNode, ListNode} | Errors]
           end;
      _ ->     
           case lists:member({RetNode, ListNode, []}, CFG) of
              true -> Errors;
              _    -> [{cfg_missingEdgeInCons, RetNode, ListNode} | Errors]
           end
   end;

check_consexp(Expr,[Ch1, Ch2 | Chs], CFG) ->
   Errors = check_consexp(Expr,[Ch2 | Chs], CFG),
   RetNode = ret(Ch1),
   Node2 = getNextPat(Ch2),
   case ?Expr:type(Ch1) of
      application ->     
           case lists:member({RetNode, Node2, funcall}, CFG) of
              true -> Errors;
              _    -> [{cfg_missingEdgeInCons_app, RetNode, Node2} | Errors]
           end;
      _ ->     
           case lists:member({RetNode, Node2, []}, CFG) of
              true -> Errors;
              _    -> [{cfg_missingEdgeInCons, RetNode, Node2} | Errors]
           end
   end. 
   
check_consend(ListNode, Expr, CFG)  ->
   RetNode = ret(ListNode),
   Ch2 = ?Query:exec(Expr, ?Expr:child(2)),
   case Ch2 of
      [] ->
         case ?Expr:type(ListNode) of
              application ->     
                   case lists:member({RetNode, Expr, funcall}, CFG) of
                      true -> [];
                      _    -> [{cfg_missingEdgeInCons_app, RetNode, Expr} ]
                   end;
              _ ->     
                   case lists:member({RetNode, Expr, []}, CFG) of
                      true -> [];
                      _    -> [{cfg_missingEdgeInCons, RetNode, Expr} ]
                   end
         end;
      _  ->
         Node2 = getNextPat(hd(Ch2)),
         case ?Expr:type(ListNode) of
              application ->     
                   case lists:member({RetNode, Node2, funcall}, CFG) of
                      true -> [];
                      _    -> [{cfg_missingEdgeInCons_app, RetNode, Node2} ]
                   end;
              _ ->     
                   case lists:member({RetNode, Node2, []}, CFG) of
                      true -> [];
                      _    -> [{cfg_missingEdgeInCons, RetNode, Node2} ]
                   end
         end
   end.


%% @doc List_comp test.
check_list_comp({Exprs,CFG}) ->
   LCExprs = [Expr || Expr <- Exprs,
                          (?Expr:type(Expr) == list_comp)],
   lists:flatten([check_lcexp(Expr, CFG) || Expr <- LCExprs]).

%% @doc Checking "List_gen" rules.
check_lcexp(Expr,CFG) ->
   %?d(Expr),
   Seq1 = ?Query:seq([?Expr:clause(1),?Clause:body(1)]),
   Seq2 = ?Query:seq([?Expr:clause(2),?Clause:body(1),?Expr:clause(2),?Clause:body(1)]),
   Seq3 = ?Query:seq([?Expr:clause(2),?Clause:body(1),?Expr:clause(1),?Clause:pattern(1)]),
   E1 = hd(?Query:exec(Expr, Seq1)),
   RetNode = ret(E1), 
   %?d(E1),
   E2 = hd(?Query:exec(Expr, Seq2)), 
   %?d(E2),
   P = getNextPat(hd(?Query:exec(Expr, Seq3))),
   %?d(P),
   %% Mj  edges of application nodes are not impelemented correctly yet
   %% P -> E1 edges are not impelemented correctly yet
   case ?Expr:type(E1) of
          application ->     
               Err1 = case lists:member({RetNode, Expr, funcall}, CFG) of
                        %% edge e1 -> e0
                        true -> [];
                        _    -> [{cfg_missingEdgeInListComp_app_e1e0, RetNode, Expr}]
                      end,
               Err2 = case lists:member({RetNode, E2, funcall}, CFG) of
                        %% edge e1 -> e2
                        true -> [];
                        _    -> [{cfg_missingEdgeInListComp_app_e1e2, RetNode, E2}]
                      end;       
          _ ->     
               Err1 = case lists:member({RetNode, Expr, []}, CFG) of
                        %% edge e1 -> e0
                        true -> [];
                        _    -> [{cfg_missingEdgeInListComp_e1e0, RetNode, Expr}]
                      end,
               Err2 = case lists:member({RetNode,E2,[]},CFG) of
                        %% edge e1 -> e2
                        true -> [];
                        _    -> [{cfg_missingEdgeInListComp_e1e2, RetNode, E2}] 
                      end      
   end,
   case ?Expr:type(E2) of
          application ->
              Err3 = case  suit(E2,P,CFG) of
                        %% TODO [funcall,yes] lista kellene??
                        %% edge e2-> p 
                        true -> []; 
                        _    -> [{cfg_missingEdgeInListComp_app_e2p, E2, P}] 
                     end;
          _           ->
              Err3 = case lists:member({E2,P,yes},CFG) of
                        %% edge e2-> p
                        true -> [];
                        _    -> [{cfg_missingEdgeInListComp_e2p, E2, P}]
                     end 
   end,       
   Err1 ++ Err2 ++ Err3.
   %% TODO további élek...



%%
%% check_sendexpr ... TODO, sendexpr CFG is not implemented correctly yet...

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% --- Auxulary functions --- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% @private
getNextPat(Expr) ->
   case ?Expr:type(Expr) == list_comp of
          true ->
               Seq3 = ?Query:seq([?Expr:clause(2),?Clause:body(1),?Expr:clause(2),?Clause:body(1)]),
               getNextPat(hd(?Query:exec(Expr, Seq3)));
          _    ->
               NodeL  = ?Query:exec(Expr,?Expr:child(1)),
               case NodeL of
                  []  -> Expr;
                  _   -> getNextPat(hd(NodeL))
               end  
   end.
   
%% @private           
suitB(Form,CFG) ->
    case [Begin || {Begin,_End,_Edge} <- CFG, Begin == Form] of
       [] -> false;
       _  -> true
    end.   
    
%% @private
suitB(Form,SEdg,CFG) ->
    case [Begin || {Begin,_End,Edge} <- CFG, Begin == Form, Edge == SEdg] of
       [] -> false;
       _  -> true
    end.   
    
%% @private
suitE(Form,CFG) ->
    case [End || {_Begin,End,_Edge} <- CFG, End == Form] of
       [] -> false;
       _  -> true
    end.  
    
%% @private
suitE(Form,SEdg,CFG) ->
    case [End || {_Begin,End,Edge} <- CFG, End == Form, Edge == SEdg] of
       [] -> false;
       _  -> true
    end.  

%% @private
suit(Form1,Form2,CFG) ->
    case [Begin || {Begin,End,_Edge} <- CFG, Begin == Form1, End == Form2] of
       [] -> false;
       _  -> true
    end.  


%% @private
ret(Node) ->
   Typ = ?Expr:type(Node), 
   case (Typ == case_expr) 
           orelse (Typ == receive_expr) 
             orelse (Typ == if_expr) of
      true   -> {ret,Node};
      _      -> Node
   end.

%% @private
db_aux(N,Y) ->
    ri:reset(),
    [refcore_fileman:add_file("../db/" ++ integer_to_list(N) ++ "/t" 
        ++ integer_to_list(M) ++ ".erl") || M <- lists:seq(1, Y)],
    test_cfg().

