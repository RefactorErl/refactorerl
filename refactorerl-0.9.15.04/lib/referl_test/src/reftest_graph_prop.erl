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
%%% @author Judit Koszegi <kojqaai@inf.elte.hu>

-module(reftest_graph_prop).
-vsn("$Rev: 5069 $ ").

-export([prop/0, graph_prop_errors/0]).
% todo These are only exported for testing purposes, right?
% -export([propVar/2, fun_test/0, semvariables_test/0]).

-compile(export_all).

-export([dyn_prop/0, dyn_graph_prop_errors/0]).

-include("test.hrl").

%% @doc Checks the consistency of the stored graph.
%%
%% @spec prop() -> boolean()
prop() ->
    graph_prop_errors() =:= [].

%% @spec graph_prop_errors() -> [tuple()]
%% @doc  Returns the consistency errors in the graph.
graph_prop_errors() ->
    TestResults  = [fun_test(), variables_test(), semvariables_test()],
    [R || R <- lists:flatten(TestResults), R =/= ok].

%% @spec dyn_prop() -> boolean()
%% @doc Checks the consistency of the dynamic function call graph.
dyn_prop() ->
    dyn_graph_prop_errors() =:= [].

%% @spec dyn_graph_prop_errors() -> [tuple()]
%% @doc Returns the consistency errors in the dynamic function call
%% graph.
dyn_graph_prop_errors() ->
    [R || R <- lists:flatten(dynfun_test()), R =/= ok].

semvariables_test() ->
    Moduls = [File || File <- ?Query:exec([file]), ?File:type(File) == module],
    Funs = [Form || Form <- ?Query:exec(Moduls, ?File:forms()),
        ?Form:type(Form) == func],
    lists:map(fun semvar_test/1, Funs).

semvar_test(Fun) ->
    Seq = ?Query:seq([?Form:clauses(), ?Clause:body(), ?Expr:deep_sub()]),
    Exprs = ?Query:exec(Fun, Seq),
    Vars = [Expr || Expr <- Exprs, ?Expr:type(Expr) == variable],
    lists:map(fun hasSemVarTest/1, Vars).

hasSemVarTest(Var) ->
    SemVars   = ?Query:exec(Var, ?Expr:variables()),
    %io:format("Var:~p, SemVars:~p~n",[Var,SemVars]),
    case SemVars of
        [] -> {var_has_no_sem_var_node, Var}; % , ?Var:name(Var)};
        _  -> ok
    end.


variables_test() ->
    %Moduls = [File || File <- ?Query:exec([file]), ?File:type(File) == module],
    Moduls = ?Query:exec(?Mod:all()),
    lists:map(fun  var_test/1, Moduls).

%% @doc Variable analyzer test. It checks the graph consistency for a module,
%% checking the property prop_anal_var for one times (without QuickCheck).
%%
%% @spec var_test(atom()) -> [tuple()]
var_test(Module) ->
    QuerySeq = ?Query:seq([?Mod:locals(),
        ?Fun:definition(), ?Form:clauses(), ?Clause:body()]),
    ExprNodes = ?Query:exec(Module, QuerySeq),
    %QuerySeq = ?Query:seq([?Mod:find(Module), ?Mod:locals(),
        %?Fun:definition(), ?Form:clauses(), ?Clause:body()]),
    %ExprNodes = ?Query:exec(QuerySeq),
    MatchExprNodes = [ExprNode || ExprNode <- ExprNodes,
        ?Expr:type(ExprNode) == match_expr],
    MatchVarNodes = lists:filter(fun(X)-> ?Expr:type(X)==variable end,[hd(?Query:exec(ExprNode, ?Expr:child(1)))
        || ExprNode <- MatchExprNodes]),
    propVarTest(MatchVarNodes, Module).


propVarTest(MatchVarNodes, Module) ->
    propVarExTest(MatchVarNodes) ++ propMatchExTest(Module).


%% There exists semantic variable node for all match expression
propVarExTest(MatchExprs) when is_list(MatchExprs) ->
    [propVarExTest(N) || N <- MatchExprs];
propVarExTest(MatchExpr) ->
    Vars = ?Query:exec(MatchExpr, ?Expr:varbinds())
        ++ ?Query:exec(MatchExpr, ?Expr:varrefs()),
    Name = ?Expr:value(MatchExpr),
    GoodVars = [Var || Var <- Vars, ?Var:name(Var) == Name],
    %% io:format("Match:~p , ~p, ~p~n", [Name, Vars, GoodVars]),
    case GoodVars of
        [] -> {matchexpr_has_no_vars, MatchExpr, Name};
        _  -> ok
    end.

%% There exists binding for all semantic variable node
propMatchExTest(Module) ->
    QuerySeq = ?Query:seq([?Mod:locals(),
        ?Fun:definition(), ?Form:clauses(), ?Clause:variables()]),
    Variables = ?Query:exec(Module, QuerySeq),
    existsBind(Variables).

existsBind(Vars) when is_list(Vars) ->
    [existsBind(V) || V <- Vars];
existsBind(Var) ->
    Binds = ?Query:exec(Var, ?Var:bindings()),
    case Binds of
        [] -> {no_var_bind, Var}; % , ?Var:name(Var)};
        _  -> ok
    end.


fun_test() ->
    propFun1() ++ propFun2() ++ propFun3() ++ propFun4().

%% @doc Function analyzer test. It checks the function name and
%% arity information stored in the semantic function nodes corresponds to the
%% information stored in the syntactic graph of the function clause.
%%
%% @spec propFun1() -> [tuple()]
propFun1() ->
    Moduls = [File || File <- ?Query:exec([file]), ?File:type(File) == module],
    Funs = [Form || Form <- ?Query:exec(Moduls, ?File:forms()),
                           ?Form:type(Form) == func],
    lists:map(fun testFun/1, Funs).

testFun(Fun) ->
    FunNodes  = ?Query:exec(Fun, ?Form:func()),
    case FunNodes of
        [] ->
           {no_func_node};
        [FunNode]  ->
            Arity = ?Fun:arity(FunNode),
            Name  = ?Fun:name(FunNode),
            FirstClause = ?Query:exec(Fun, ?Form:clause(1)),
            NameCl = ?Expr:value(
                          hd(?Query:exec(FirstClause, ?Clause:name()))),
            ArityCl = length(?Query:exec(FirstClause, ?Clause:patterns())),
            BadArity =
                case Arity == ArityCl of
                    true  -> [];
                    false -> [{bad_arity_in_sem_node, FunNode, ?Fun:name(FunNode), Arity, ArityCl}]
                end,
            BadName =
                case Name == NameCl of
                    true  -> [];
                    false -> [{bad_name_in_sem_node, FunNode, ?Fun:name(FunNode), Name, NameCl}]
                end,
            BadArity ++ BadName;
        _ ->
            {too_many_func_nodes, FunNodes, Fun}
    end.

%% @doc Module and function analyzer test. It checks for every function form
%% there exists a corresponding semantic function node reachable in a
%% "moddef + func" route and for every semantic module node there exists
%% a corresponding "modctx" edge.
%%
%% @spec propFun2() -> [tuple()]
propFun2() ->
    Moduls = [File || File <- ?Query:exec([file]), ?File:type(File) == module],
    ModFuns = getFuns(Moduls),
    lists:map(fun testModFuns/1, ModFuns).

getFuns(Moduls) ->
    case Moduls of
        [] ->
            [];
        [Mod | MoreMod] ->
            Funs = [Form || Form <- ?Query:exec(Mod, ?File:forms()),
                ?Form:type(Form) == func],
            case Funs of
                [] -> getFuns(MoreMod);
                _  -> [{Mod, Funs} | getFuns(MoreMod)]
            end
    end.

testModFuns({Mod, Funs}) ->
    [testModF(Mod, F) || F <- Funs].

testModF(Mod, Fun) ->
    ModNodes = ?Query:exec(Mod, ?File:module()),
    case ModNodes of
        [] ->
            {missing_mod_node, Mod};
        [ModNode]  ->
            testModF_fundef(ModNode, Fun) ++ testModF_modctx(ModNode, Fun);
        _ ->
            {too_many_mod_nodes, ModNodes, Mod, ?Mod:name(Mod)}
    end.

testModF_fundef(ModNode, Fun) ->
    FunNodes  = ?Query:exec(Fun, ?Form:func()),
    case FunNodes of
        [] ->
            {missing_fun_node, Fun};
        [FunNode] ->
            case ?Query:exec(FunNode, ?Fun:definition()) of
                []   -> [ok];
                [_X] ->
                    LocFuns = ?Query:exec(ModNode, ?Mod:locals()),
                    case lists:member(FunNode, LocFuns) of
                        true  -> [ok];
                        false -> [{missing_local_fun, FunNode, ModNode, Fun}, ?Fun:name(FunNode)]
                    end;
                _   -> {too_many_form_for_fun_nodes, FunNode, ModNode, Fun}
            end;
        _ ->
            {too_many_fun_nodes, FunNodes, ModNode, Fun}
    end.

testModF_modctx(ModNode, Fun) ->
    FirstClause = ?Query:exec(Fun, ?Form:clause(1)),
    ModCtx = ?Query:exec(FirstClause, [modctx]),
    %io:format("ModNode:~p, ModCtx:~p\n", [ModNode, ModCtx]),
    case [ModNode] == ModCtx of
        true  -> [ok];
        false -> [{modnode_is_not_modctx, ModNode, ModCtx, Fun}]
    end.

%% @doc Function analyzer test. It checks if there exists a "funcall" edge
%% from a semantic function node to another, there exists a "funlref" edge
%% from the corresponding function body to the same semantic function node
%% and vice-versa.
%%
%% @spec propFun3() -> [tuple()]
propFun3() ->
    Moduls = [File || File <- ?Query:exec([file]), ?File:type(File) == module],
    Funs = [Form || Form <- ?Query:exec(Moduls, ?File:forms()),
        ?Form:type(Form) == func],
    lists:map(fun testFunCall/1, Funs) ++ lists:map(fun testFunCallBack/1, Funs).

testFunCall(Fun) ->
    Seq = ?Query:seq([?Form:clauses(), ?Clause:body(), ?Expr:deep_sub()]),
    Exprs = ?Query:exec(Fun, Seq),
    Applications = [App || App <- Exprs, ?Expr:type(App) == application],
    case Applications of
        [] -> [];
        _  -> lists:map(fun(X) ->  testApp(Fun,X) end, Applications)
    end.

testApp(Fun, App) ->
    CalledFuns = ?Query:exec(App, ?Expr:dynfunction()),
    FunNodes  = ?Query:exec(Fun, ?Form:func()),
    case FunNodes of
        [] ->
            {missing_fun_node, Fun, App};
        [FunNode]  ->
            case CalledFuns of
                [] ->
                    {missing_funlref, Fun, App, ?Fun:name(FunNode), ?Syn:flat_text(App)};
                [CalledFun] ->
                    FunCalls = ?Query:exec(FunNode, [funcall]),
                    case lists:member(CalledFun, FunCalls) of
                        true  -> ok;
                        false -> {fun_is_not_called, Fun, CalledFun, FunCalls}
                    end;
                _ ->
                    {too_many_funlrefs, Fun, App, CalledFuns}
            end;
        _ ->
            {too_many_fun_nodes, FunNodes, App, Fun}
    end.

testFunCallBack(Fun) ->
    FunCalls =  ?Query:exec(Fun, ?Query:seq([?Form:func(), [funcall]])),
    case FunCalls of
        [] -> ok;
        _  -> lists:map(fun(X) -> testCalls(Fun,X) end, FunCalls)
    end.

testCalls(Fun, FunCall) ->
    Seq = ?Query:seq([?Form:clauses(), ?Clause:body(), ?Expr:deep_sub()]),
    Exprs = ?Query:exec(Fun, Seq),
    Applications = [App || App <- Exprs, ?Expr:type(App) == application],
    CalledFuns = ?Query:exec(Applications, ?Expr:ambdynfunction()),
    case lists:member(FunCall, CalledFuns) of
        true  -> ok;
        false -> {testcalls_fails, FunCall, CalledFuns}
    end.

%% @doc Module analyzer test. It checks if there exists a syntatic module
%% node (file node, with type module), than there exists a semantic module
%% node reachable via edge "module".
%%
%% @spec propFun4() -> [tuple()]
propFun4() ->
    Moduls = [File || File <- ?Query:exec([file]), ?File:type(File) == module],
    lists:map(fun testMod/1, Moduls).

testMod(Mod) ->
    ModNodes  = ?Query:exec(Mod, ?File:module()),
    case ModNodes of
        [] ->
            {missing_modnode, Mod};
        [ModNode]  ->
            Moduls = ?Query:exec(?Mod:all()),
            %io:format("ModNode:~p, Moduls:~p\n", [ModNode, Moduls]),
            case lists:member(ModNode, Moduls) of
                true  -> ok;
                false -> {mod_is_not_in_all_mods, Mod, ModNode, Moduls}
            end;
        _ ->
            {too_many_modnodes, Mod, ModNodes}
    end.

%%% ============================================================================
%%% Testing stored graph extended with dynamic function calls

dynfun_test() ->
    propDynFun1() ++ propDynFun2() ++ propDynFun3() ++ 
        propDynFun4() ++ propDynFun5().

%%% ----------------------------------------------------------------------------
%%% Helper functions for collecting different types of nodes

get_all_funs() ->
    ?Query:exec(?Query:seq([(?Mod:all()),?Mod:locals()])).

get_opaque_funs() ->
    [Fun || Fun <- get_all_funs(), ?Fun:is_opaque(Fun)].

get_all_apps() ->
    Seq = ?Query:seq([ ?Fun:definition(), ?Form:clauses(),
                       ?Clause:body(), ?Expr:deep_sub()]),
    Exprs = ?Query:exec(get_all_funs(), Seq),
    [App || App <- Exprs, ?Expr:type(App) == application].

%%% ----------------------------------------------------------------------------
%%% Propeties

%% @doc Dynamic function call analyzer test. It checks whether all the
%% semantic function nodes with `opaque =\= false' value are reachable
%% via at least one `ambfuneref' edge, but are not reachable via other
%% function call reference edges.
%%
%% @spec propDynFun1() -> [tuple()]
propDynFun1() ->
    OpaqueFuns = get_opaque_funs(),
    lists:map(fun testDynApps/1, OpaqueFuns) ++
        lists:map(fun testNoOtherApps/1, OpaqueFuns).

testDynApps(Fun) ->
    AmbRefs = ?Query:exec(Fun, ?Dynfun:ambdyn_call()),
    case AmbRefs of
        [] -> {opaque_fun_has_no_amb_appl,Fun};
        _ -> ok
    end.

testNoOtherApps(Fun) ->
    DynRefs = ?Query:exec(Fun, ?Query:all([?Dynfun:dynfun_call(),
                                           ?Fun:applications()])),
    case DynRefs of
        [] -> ok;
        _ -> {opaque_fun_has_unproper_appl, Fun}
    end.

%% @doc Dynamic function call analyzer test. It checks functions which
%% are reachable via `may_be' edge from a function with `opaque =\=
%% false' node. If `opaque == arity', module and the function names of
%% these functions have to be the same. Similar, if `opaque == name',
%% the module names and function arities, if `opaque == module', the
%% function names and arities have to be equal.
%%
%% @spec propDynFun2() -> [tuple()]
propDynFun2() ->
    lists:map(fun testMaybes/1, get_opaque_funs()).

testMaybes(Fun) ->
    Maybes = ?Query:exec(Fun, [may_be]),
    ModName = ?Mod:name(hd(?Query:exec(Fun, ?Fun:module()))),
    FunName = ?Fun:name(Fun),
    Arity = ?Fun:arity(Fun),
    Props =
        [{module,
          [ModName | [?Mod:name(M) || M <-?Query:exec(Maybes, ?Fun:module())]]},
      {name,  [FunName | [?Fun:name(F)  || F <- Maybes]]},
      {arity, [Arity   | [?Fun:arity(F) || F <- Maybes]]}],
    UnambiguousProps =
        [{Key,List} || {Key, List} <- Props, Key =/= ?Fun:opaque(Fun)],
    testAllSame(UnambiguousProps, Fun).

testAllSame([], _Fun) ->
    ok;
testAllSame([{Key, List} | Ls], Fun) ->
    case List of
        [] -> testAllSame(Ls, Fun);
        [X | Xs] ->
            case lists:all(fun (A) -> A == X end, Xs) of
                true  -> testAllSame(Ls, Fun);
                false -> {list_to_atom("opaque_fun_maybes_have_not_the_same_" ++
                                       atom_to_list(Key)), Fun}

            end
    end.

%% @doc Dynamic function call analyzer test. In case of every
%% functions with `arity' `opaque' value and given lower bound of
%% arities, it checks whether the functions connected with `may_be'
%% edge have greater or equal arity to this lower bound.
%%
%% @spec propDynFun3() -> [tuple()]
propDynFun3() ->
    lists:flatten(
      lists:map(fun testMaybesArity/1,
                [Fun || Fun <- get_opaque_funs(), ?Fun:opaque(Fun) == arity])).

testMaybesArity(Fun) ->
    Arity = ?Fun:arity(Fun),
    case Arity of
        -1 -> ok;
        _  -> [testMaybeArity(Fun, MaybeFun, (-1)*(Arity+1)) ||
                  MaybeFun <- ?Query:exec(Fun, [may_be])]
    end.

testMaybeArity(Fun, MaybeFun, Arity) ->
    case ?Fun:arity(MaybeFun) < Arity of
        true  -> {maybe_of_opaque_fun_has_too_little_arity, Fun, MaybeFun};
        false -> ok
    end.

%% @doc Dynamic function call analyzer test. Tests whether all the
%% applications owning both static and dynamic function call edge are
%% apply-calls.
%%
%% @spec propDynFun4() -> [tuple()]
propDynFun4() ->
    lists:map(fun testApply/1, get_all_apps()).

testApply(App) ->
    Funs = ?Query:exec(App, ?Expr:function()),
    DynFuns = ?Query:exec(App, ?Query:all([?Dynfun:ambdyn(),?Dynfun:dynfun()])),
    case Funs =/= [] andalso DynFuns =/= [] of
        false ->
            ok;
        true ->
            SubExprs =  ?Query:exec(App, ?Expr:children()),
            case lists:member(apply, [?Expr:value(Expr) || Expr <- SubExprs]) of
                true ->
                    ok;
                false ->
                    {appl_with_funref_and_dynfunref_has_no_apply_child, App}
            end
    end.

%% @doc Dynamic function call analyzer test. When an `ambfuneref' edge
%% points to a function, tests whether this function has an opaque
%% value.
%%
%% @spec propDynFun5() -> [tuple()]
propDynFun5() ->
    Funs = [Fun || Fun <- get_all_funs(),
                   ?Query:exec(Fun, [{ambfuneref, back}]) =/= []],
                                    %needs to change to an interface function
    lists:map(fun(Fun) -> case ?Fun:is_opaque(Fun) of
                              true -> ok;
                              false -> {fun_referred_by_ambfunref_is_not_opaque,
                                        Fun}
                          end
              end, Funs).
