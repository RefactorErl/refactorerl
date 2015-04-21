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

%%% @doc
%%% This module implements the generalize function definition refactoring.
%%% The refactoring is generalize a function definition by selecting an
%%% expression or a continious sequence of expressions and making this
%%% the value of a new argument added to the definition of the function,
%%% and the actual parameter at all call site becomes the selected part
%%% with the corresponing compensation.
%%%
%%% == Parameters ==
%%% <ul>
%%%   <li> A non-empty, continuous sequence of expressions
%%%        (see {@link reflib_args:expr_range/1}). </li>
%%%   <li> The name of the variable to add to the parameters
%%%        (see {@link reflib_args:varname/1}). </li>
%%% </ul>
%%%
%%% == Conditions of applicability ==
%%% <ul>
%%%   <li>The name of the function with its arity increased by one should not
%%%   conflict with another function, either defined in the same module,
%%%   imported from another module, or being an auto-imported built-in function.
%%%   </li>
%%%   <li> The new variable name does not exist in the scope of the selected
%%%   expression(s) and must be a legal variable name .</li>
%%%   <li> The starting and ending positions should delimit an expression
%%%   or a sequence of expressions.</li>
%%%   <li> The selected expressions do not bind variables that are used outside
%%%   the selection.</li>
%%%   <li> Variable names bound by the expressions do not exist in the scopes of
%%%   the generalized function calls.</li>
%%%   <li> The expressions to generalize are not patterns and
%%%   they do not call their containing function.</li>
%%%   <li> If the selection is part of a list comprehension, it must be a single
%%%   expression and must not be the generator of the comprehension.</li>
%%%   <li> The extracted sequence of expressions are not part of a macro
%%%   definition, and are not part of macro application parameters.</li>
%%% </ul>
%%%
%%% == Rules of the transformation ==
%%% <ol>
%%%   <li>If the selected expression does not contain any variables:
%%%     <ul>
%%%       <li> Give an extra argument (a simple variable) to the function.
%%%       The name of this argument is the name given as a parameter
%%%       of the transformation. </li>
%%%       <li> Replace the expression with a variable expression.</li>
%%%       <li> Add the selected expression to the argument list of
%%%      every call of the function.</li>
%%%     </ul>
%%%   </li>
%%%   <li> If the selected expression contains variable(s) or contains more
%%%   than one expression or has a side-effect:
%%%     <ul>
%%%       <li> Add a new argument (a simple variable) to the function
%%%       definition with the given variable name.</li>
%%%       <li> Replace the expression with an application. The name of the
%%%       application is the given variable name, the arguments of the
%%%       application are the contained variables.</li>
%%%       <li> Add a fun expression to the argument list of every call of the
%%%       function. The parameters of the fun expression are the contained
%%%       variables and the body is the selected expression.</li>
%%%     </ul>
%%%   </li>
%%%   <li> If the selected expression is part of a guard expression:
%%%     <ul>
%%%       <li> Give an extra argument (a simple variable) to the function.
%%%       The name of this argument is the name given as a parameter
%%%       of the transformation.</li>
%%%       <li> Replace the guard expression with a variable expression
%%%       with that new name. </li>
%%%       <li> Add the selected expression to the argument list of the
%%%       function calls.</li>
%%%       <li> If the selected expression contains a formal parameter, replace
%%%       this with the actual parameter.</li>
%%%       <li> If the selected guard is a conjunction or a disjunction,
%%%       create an andalso or an orelse expression and add this to the
%%%       argument list of the function call. </li>
%%%     </ul>
%%%   </li>
%%%   <li> If the generalized function is recursive, than  instead of adding
%%%   the selection to the argument list of the function calls in the body,
%%%   add the new parameter to the argument list.</li>
%%%   <li> If the generalized function is exported from the module,
%%%   add a new function definition to the current module with the same
%%%   parameters and guard. The body of this function is a call of
%%%   the generalized function.</li>
%%%   <li> If the selection contains macro application/record expression
%%%   move the definition of the macro/record before the first
%%%   call of function.</li>
%%% </ol>
%%%
%%% == Implementation status ==
%%% The transformation is fully implemented.
%%%
%%% @author Melinda Tóth <toth_m@inf.elte.hu>

-module(reftr_gen).
-vsn("$Rev: 12913 $").

%% Callbacks
-export([prepare/1, error_text/2]).

-include("user.hrl").

%%% ============================================================================
%%% Errors

%% @private
%% @todo Print (a relevant portion of) the code with the side effect as well.
error_text(side_eff, []) ->
    "Generalizing would duplicate code with side effects";
error_text(guard_var, []) ->
    "It is impossible to match against the formal and actual parameters" ++
    " (compound data structures)".

%%% ============================================================================
%%% Callbacks

%%% @private
prepare(Args) ->
    [Expr1|_] = Exprs = ?Args:expr_range(Args),

    {Link, Parent} = check_expr_link(Exprs),
    lists:foreach(fun check_expr/1, Exprs),
    [Clause] = ?Query:exec(Expr1, ?Query:seq([?Expr:clause(),
                                              ?Clause:funcl()])),
    Form     = ?Query:exec1(Clause, ?Clause:form(),
                            ?RefErr0r(parent_not_form)),

    Fun      = ?Query:exec1(Form, ?Form:func(), ?RefErr0r(parent_not_form)),
    FunName  = ?Fun:name(Fun),
    Arity    = ?Fun:arity(Fun),
    check_recursive_calls(Exprs, Fun),
    [File]   = ?Query:exec(Form, ?Form:file()),
    FilePath = ?File:path(File),
    [Module] = ?Query:exec(File, ?File:module()),
    check_fun(Module, FunName, Arity + 1),

    SideE  = [Expr || Expr <- Exprs, ?Expr:has_side_effect(Expr)],
    GuardE = ?Expr:role(Expr1) == guard,
    LHS    = is_generalizable_lhs(Exprs),
    if LHS ->
           RhsExprs = rhs_exps(Exprs);
       true ->
           RhsExprs = []
    end,
    {TApps, TImps, RecApps, RecImps, RecCls} = get_fun_calls(Exprs, Fun, File),
    Apps   = TApps ++ RecApps,
    not GuardE orelse [check_side_effect(App) || App <- Apps],
    %?Check(not is_from_mac_subst(Apps), ?RefErr0r(mac_error)),

    {Bound, NotBound} = vars(Exprs),
    check_var(Bound,Exprs),
    check_bound_var_name(TApps, Bound),
    AppForms = ?Query:exec(TApps, ?Query:seq([?Expr:clause(),
                                              ?Clause:funcl(),
                                              ?Clause:form()])),
    ClIndex = ?Syn:index(Form, funcl, Clause),
    Index  = ?Syn:index(File, form, Form),
    {Moves, AppInd} = check_record_macro(AppForms, Index, Exprs),
    UsedVarNames = lists:usort([?Var:name(Var) || Var <- NotBound]),
    Export = ?Fun:is_exported(Fun) orelse (TImps /= []) orelse (RecImps /= []),
%% TODO: Should we trasform the implicit funs???
    FunCl  = ?Query:exec(Expr1,
                         ?Query:seq(?Expr:clause(), ?Clause:funcl())),
    Pairs = get_pairs_for_guard_tr(Exprs, FunCl, TApps, GuardE),
    FunCls = ?Query:exec(Form, ?Form:clauses()),
    ClData = [{?Query:exec(Cl, ?Clause:name()),
               ?Query:exec(Cl, ?Clause:patterns()),
               ?Query:exec(Cl, ?Clause:guard()),
               Cl} || Cl <- FunCls],
    {Type, NewExprs} =
        case {Bound, NotBound, SideE =/= [], length(Exprs)>1, GuardE, LHS} of
            { _, _,    _,    _,    _, true} ->
                                            {lhs,    RhsExprs};
            { _, _,    _,    _, true,    _} ->
                                            {guard, separate_guard(hd(Exprs))};
            { _, _,    _, true,    _,    _} ->
                                            {funexp, Exprs};
            { _, _, true,    _,    _,    _} ->
                                            {funexp, Exprs};
            {[], [],   _,    _,    _,    _} ->
                                            {var,    Exprs};
            { _, _,    _,    _,    _,    _} ->
                                            {funexp, Exprs}
        end,
    FunAppDatas  = [get_app_data(App, NewExprs) || App <- TApps],
    RecAppDatas  = [get_app_data(App, NewExprs) || App <- RecApps],

    IsAppComesFromMacro = fun({_,_,_,{}}) -> false;
                             ({_,_,_,_}) -> true
                          end,
    {MacroFunDatas,NonMacroFunDatas} = lists:partition(IsAppComesFromMacro,
                                                      FunAppDatas),
    {MacroRecDatas,NonMacroRecDatas} = lists:partition(IsAppComesFromMacro,
                                                      RecAppDatas),
    
    FunAppClosingToks = get_app_closepar_tok(MacroFunDatas),
    RecAppClosingToks = get_app_closepar_tok(MacroRecDatas),
    ?Check(ordsets:is_disjoint(
            ordsets:from_list(FunAppClosingToks),
            ordsets:from_list(RecAppClosingToks)), ?RefErr0r(mac_error)),
    MacroAppForms = get_funapp_forms(MacroFunDatas ++ MacroRecDatas),
    
    
    Vars     = ?Query:exec(Form, ?Query:seq(?Form:clauses(), ?Clause:variables())),
    VarNames = [?Var:name(Var) || Var <- Vars],
    % @todo Add transformation info
    ?Transform:touch(File),
    if
        LHS ->
            [{_, MatchExpParent}] = ?Syn:parent(Parent),
            [{_, CommentsAfter}] = get_existing_comment(get_next_sibling(
                                       Parent, ?Syn:children(MatchExpParent))),
            {CommentsBefore, _} = ?Syn:get_comments(Parent),
            Comments = [{ CommentsBefore ++ CommentsAfter, []}],
            VarName = "";
        true ->
            Comments = ?Syn:get_comments(Exprs),
            VarName = ?Args:ask(Args, varname, fun cc_varname/2,
                                fun cc_error/3, VarNames)
    end,
    [fun() ->
        transform_app_macros(Type, FunAppClosingToks, NewExprs, UsedVarNames),
        if
            LHS ->
                NewLeft = copy_left(Parent),
                R = replace_expressions_lhs(Parent, NewLeft),
                insert_rec_app_arg_lhs(NonMacroRecDatas, NewLeft),
                insert_fun_pattern_lhs(NewLeft, ClData);
            true ->
                transform_recapp_macros(RecAppClosingToks, VarName),
                R = replace_expressions(Parent,VarName,Exprs, Type, 
                                        UsedVarNames, Link),
                insert_rec_app_arg(NonMacroRecDatas, VarName),
                insert_fun_pattern(VarName, FunCl, RecCls, ClData)
        end,
        Ret = insert_old_fun(File, ClData, NewExprs,Type,
                             UsedVarNames, Index, Export),
        NewApps = insert_app_arg(NonMacroFunDatas, Type, UsedVarNames),
        move_macros_records(Moves, AppInd),
        {R, NewApps, Ret}
     end,
     fun(Tuple) ->
        Transform = prepare_guard(Pairs),
        transform_guard(Transform),
        Tuple
     end,
     fun({R, NewApps, Ret}) ->
        get_comments(Ret),
        case Exprs of
            [_] -> ?Syn:put_comments([R], Comments);
            _   -> [?Syn:put_comments(NewExprs2, Comments) || 
                       NewExprs2 <- NewApps]
        end
     end,
     fun(_) ->
        [?File:reload_form(File, AppForm) || AppForm <- MacroAppForms]
     end,
     fun(_)->
        ?Query:exec(?Query:seq([?File:find(FilePath),
                                ?File:module(),
                                ?Fun:find(FunName,Arity+1),
                                ?Fun:definition(),
                                ?Form:clause(ClIndex),
                                ?Clause:pattern(Arity+1)]))
     end].

get_comments({Fun, Apps}) ->
    ?Syn:get_comments(Fun),
    [?Syn:get_comments(App) || App <- Apps];
get_comments(_) -> ok.

%%% ===========================================================================
%%% Checks

check_expr_link(Exprs)->
    case ?Syn:parent(hd(Exprs)) of
        [{body, Par}] ->
            ?Check(length(Exprs)=:=1 orelse ?Expr:type(hd(Exprs)) =/= 'filter',
                   ?RefError(bad_kind, filter)),
            {body, Par};
        [{esub, Par}]  ->
            ParKind = ?Expr:type(Par),
            ParVal = ?Expr:value(Par),
            Index = ?Syn:index(Par, esub, hd(Exprs)),
            ?Check(ParKind =/= application orelse
                   length(Exprs) =/= 1 orelse Index =/= 1,
                   ?RefError(bad_kind, 'application name')),
            ?Check((ParKind =/= infix_expr orelse ParVal =/= ':') orelse
                   length(Exprs) =/= 1,
                   ?RefError(bad_kind, 'module qualifier')),
            ?Check(length(Exprs) =:= 1, ?RefErr0r(bad_range)),
            {esub, Par};
        [{pattern, _Par}] -> throw(?RefError(bad_kind, pattern));
        [{guard, Par}] -> {guard, Par};
        _ ->  throw(?RefErr0r(bad_kind))
    end.


check_expr(Expr) ->
    Type = ?Expr:role(Expr),
    Kind = ?Expr:type(Expr),
    ?Check(Type =:= expr orelse Type =:= guard
                         orelse is_generalizable_lhs([Expr]),
           ?RefError(bad_kind, Type)),
    ?Check(Kind =/= compr andalso
           Kind =/= list_gen,
           ?RefError(bad_kind, list_comp)),
    ?Check(Kind =/= binary_gen andalso
           Kind =/= binary_field andalso
           Kind =/= prefix_bit_expr andalso
           Kind =/= bit_size_expr andalso
           Kind =/= size_qualifier,
           ?RefError(bad_kind, binary)),
    ?Check(Kind =/= record_field,
           ?RefError(bad_kind, record_field)),
    ?Check(Kind =/= list orelse
           length(?Query:exec(Expr, ?Expr:children())) ==1,
           ?RefError(bad_kind, list_elements)),
    case ?Query:exec(Expr, ?Expr:parent()) of
        []        -> ok;
        [Parent]  ->
            ParKind = ?Expr:type(Parent),
            ?Check(ParKind =/= binary_field andalso
                   ParKind =/= prefix_bit_expr andalso
                   ParKind =/= bit_size_expr andalso
                   ParKind =/= size_qualifier,
                   ?RefError(bad_kind, binary)),
            ?Check(ParKind =/= implicit_fun,
                   ?RefError(bad_kind, implicit_fun_part)),
            ?Check(ParKind =/= record_access,
                   ?RefError(bad_kind, record_access_part)),
            ?Check(ParKind =/= record_update,
                   ?RefError(bad_kind, record_update_part)),
            ?Check(ParKind =/= record_expr orelse
                   ?Syn:index(Parent, esub, Expr) =/= 1,
                   ?RefError(bad_kind, record_name)),
            ?Check(ParKind =/= record_expr orelse
                   ?Syn:index(Parent, esub, Expr) =/= 2,
                   ?RefError(bad_kind, record_expr_part))
    end.

check_fun(Module, NewName, Arity) ->
    ModName = ?Mod:name(Module),
    ?Check(?Query:exec(Module, ?Mod:local(NewName, Arity)) =:= [],
           ?RefError(fun_exists, [ModName, NewName, Arity])),
    ?Check(?Query:exec(Module, ?Mod:imported(NewName, Arity)) =:= [],
           ?RefError(imported_fun_exists, [ModName, [NewName, Arity]])),
    ?Check(not ?Fun:is_autoimported(NewName, Arity),
           ?RefError(autoimported_fun_exists, [NewName, Arity])).

check_var(BoundVars, Exprs)->
    Types = [ ?Expr:type(hd(?Query:exec(Expr, ?Expr:top()))) || Expr <- Exprs],
    case Types of
        [match_expr] -> ok;
        _ ->
            Occurrences  = 
                lists:append([?Query:exec(BoundVars, ?Var:occurrences(Expr)) ||
                                 Expr <- Exprs]),
            AllVarOccurs = ?Query:exec(BoundVars, ?Var:occurrences()),
            Clash = lists:usort([?Expr:value(Var) ||
                                    Var <- (AllVarOccurs -- Occurrences)]),
            ?Check( Clash =:= [],
                    ?RefError(outside_used_vars, Clash))
    end.


check_recursive_calls(Exprs, Fun)->
    RecApps = [?Query:exec(Fun, ?Fun:applications(Expr)) || Expr <- Exprs],
    RecImps = [?Query:exec(Fun, ?Fun:implicits(Expr)) || Expr <- Exprs],
    ?Check(lists:append(RecApps ++ RecImps) =:= [], 
           ?RefErr0r(recursive_subexpr)).

check_bound_var_name([],  _) -> ok;
check_bound_var_name( _, []) -> ok;
check_bound_var_name(Apps, Bound) ->
    Names  = [?Var:name(Var) || Var <- Bound],
    FunCls = ?Query:exec(Apps, ?Query:seq(?Expr:clause(), ?Clause:funcl())),
    VisibVars = ?Query:exec(FunCls, ?Clause:variables()),
    VisibNames = [?Var:name(Var) || Var <- VisibVars],
    Clash = lists:usort(?MISC:intersect(Names, VisibNames)),
    ?Check(Clash =:= [],
           ?RefError(var_exists_app, Clash)),
    Forms  = lists:usort(?Query:exec(FunCls, ?Clause:form())),
    lists:min(get_form_index(Forms)).


check_side_effect(App) ->
    [_|Params] = ?Query:exec(App, ?Expr:children()),
    SideEffectExprs = [Expr || Expr <- Params, ?Expr:has_side_effect(Expr)],
    ?Check([] =:= SideEffectExprs, ?LocalErr0r(side_eff)).

get_form_index(Forms) when is_list(Forms)->
    [begin
        File = ?Query:exec1(Form, ?Form:file(), bad_file),
        ?Syn:index(File, form, Form)
    end || Form <- Forms];
get_form_index(Form) ->
    File = ?Query:exec1(Form, ?Form:file(), bad_fil),
    {File, ?Syn:index(File, form, Form)}.

check_record_macro([], FunInd, _Exprs)  -> {no_move, FunInd};
check_record_macro(AppForms, FunInd, Exprs)->
    AppInd = lists:min(get_form_index(AppForms)),
    case AppInd >= FunInd of
        true -> {no_move, FunInd};
        false ->
            Records = lists:usort(?Query:exec(Exprs, ?Query:seq(?Expr:records(),
                                                                ?Rec:form()))),
            Macros = lists:usort(?Query:exec(Exprs, ?Expr:macros())),
            Pairs = [{Elem, get_form_index(Elem)} || Elem <- Records ++ Macros],
            {lists:usort([{Ind,Elem,File} || {Elem,{File, Ind}} <- Pairs,
                                            Ind > AppInd]), AppInd}
    end.


get_pairs_for_guard_tr(_, _, _, false) ->
    no_pair;
get_pairs_for_guard_tr(Exprs, Cl, Apps, true) ->
    Vars = [Var || Var <- ?Query:exec(Exprs, ?Expr:deep_sub()),
                   ?Expr:type(Var) == 'variable'],
    Pats = ?Query:exec(Cl, ?Clause:patterns()),
    Clash = [Pat || Pat <- Pats, ?Expr:type(Pat) =/= 'variable'],
    ?Check(length(Clash) =:= 0, ?LocalError(guard_var, [])),
    VarPairs = [{?Var:name(hd(?Query:exec(Var, ?Expr:variables()))),
                 ?Syn:index(hd(Cl), pattern, Pat)} || Pat <- Pats, Var <- Vars,
                 ?MISC:intersect(?Query:exec(Pat, ?Expr:varbinds()),
                 ?Query:exec(Var, ?Expr:varrefs())) /= []],
    [begin
        [AppListNode] = ?Query:exec(App, ?Expr:child(2)),
        {App, lists:usort([{Var, ?Query:exec1(AppListNode, ?Expr:child(I),
                                     bad_app)} ||{Var, I} <- VarPairs])}
     end || App <- Apps].


vars(Exprs) ->
    VarBinds = ?Query:exec(Exprs, ?Expr:varbinds()),
    VarRefs  = ?Query:exec(Exprs, ?Expr:varrefs()),
    {VarBinds, VarRefs -- VarBinds}.

get_fun_calls(Exprs, Fun, File)->
    Apps    = ?Query:exec(Fun, ?Fun:applications()),
    Imps    = ?Query:exec(Fun, ?Fun:implicits()),
    Body    = ?Query:exec(Fun, ?Query:seq([?Fun:definition(),
                                           ?Form:clauses(),
                                           ?Clause:body()])),
    RecApps = lists:flatten([?Query:exec(Fun, ?Fun:applications(Expr))
                              || Expr <- Body]),
    RecImps = lists:flatten([?Query:exec(Fun, ?Fun:implicits(Expr))
                              || Expr <- Exprs]),
    ModApps = [App || App <- (Apps -- RecApps),
                      [File] == ?Query:exec(App, ?Query:seq([?Expr:clause(),
                                                             ?Clause:form(),
                                                             ?Form:file()]))],
    ModImps = [App || App <- (Imps -- RecImps),
                      [File] == ?Query:exec(App,?Query:seq([?Expr:clause(),
                                                            ?Clause:form(),
                                                            ?Form:file()]))],
    RecCls      = [hd(?Query:exec(App,
                                  ?Query:seq(?Expr:clause(), ?Clause:funcl())))
                   || App <- RecApps],
    {ModApps, ModImps, RecApps, RecImps, RecCls}.


get_app_data(AppNode, Exprs)->
    Args = ?Query:exec(AppNode, ?Query:seq(?Expr:child(2), ?Expr:children())),
    [CloseParTok] = ?Query:exec(AppNode,
                                ?Query:seq(?Expr:child(2), [{elex,last}])),
    CloseParOrigTok =
        case (?Graph:data(CloseParTok))#lex.data of
            virtual ->
                [OrigTok] = ?Query:exec(CloseParTok, [orig]),
                ArgL = ?Query:exec(OrigTok,[{orig,back},{elex,back}]),
                ?Check(lists:all(fun
                          (Expr)-> ?Expr:type(Expr)==arglist
                                 end,ArgL), ?RefErr0r(mac_error)),
                Appls = ?Query:exec(ArgL,[{esub,back}]),
                Funs = ?Query:exec(Appls,?Expr:function()),
                ?Check(length(lists:usort(Funs)) == 1, ?RefErr0r(mac_error)),
                OrigTok;
            _ -> {}
        end,
    
    {Args, Exprs, AppNode, CloseParOrigTok}.

get_app_closepar_tok(AppDatas) ->
    ClosingParTokens = [Token || {_,_,_,Token} <- AppDatas],
    sets:to_list(sets:from_list(ClosingParTokens)).

get_funapp_forms(AppDatas) ->
    AppNodes = [Node || {_,_,Node,_} <- AppDatas],
    case AppNodes of
        [] -> [];
        _  -> Forms = ?Query:exec(AppNodes, ?Query:seq(?Expr:clause(), ?Clause:form())),
              lists:usort(Forms)
    end.
    
separate_guard(Expr)->
    case ?Expr:value(Expr) of
        ',' ->
            [Left, Right] = ?Query:exec(Expr, ?Expr:children()),
            [separate_guard(Left), conjunction, separate_guard(Right)];
        ';' ->
            [Left, Right] = ?Query:exec(Expr, ?Expr:children()),
            [separate_guard(Left), disjunction, separate_guard(Right)];
        _   -> Expr
    end.

%% Returns whether there is an application which comes (partly or wholly)
%% from a macro substitution.
%% is_from_mac_subst(Apps) when is_list(Apps) ->
%%     lists:any(fun(App) -> is_from_mac_subst(App) end, Apps);
%% is_from_mac_subst(App) ->
%%     ?Expr:virtuals(App) =/= [].

%%% ----------------------------------------------------------------------------
%%% Checks

cc_varname(VarName, VarNames) ->
    ?Check(not lists:member(VarName, VarNames), ?RefError(var_exists, VarName)),
    VarName.

cc_error(?RefError(var_exists, VarName), VarName, _VarNames) ->
    ?MISC:format("Variable ~p already exists.", [VarName]).


%%% ===========================================================================
%%% Transformation part

transform_app_macros(Type, FunAppClosingToks, Exprs, ParNames) ->
    lists:foreach(
      fun(CloseParToken) ->
          [Body] = ?Query:exec(CloseParToken, [{llex, back}]),
          Index = ?Graph:index(Body, llex, CloseParToken),
          InsertToken =
                  fun(Tok, ok) ->
                      Data = ?ESG:data(Tok),
                      NewTok = ?ESG:create(?Token:trim(Data)),
                      ?ESG:insert(Body, {llex, Index}, NewTok),
                      ok
                  end,
          SubTree = case Type of
              var ->
                  CExprsList = [{?Syn:copy(Expr), Expr}||Expr<-Exprs],
                  [CExpr]     = find_copy_expr(CExprsList),
                  CExpr;
              funexp ->
                  Pars = [?Syn:create(#expr{type = variable}, [Par])
                    || Par <- ParNames],
                  CExprsList = [{?Syn:copy(Expr), Expr}||Expr<-Exprs],
                  CExprs     = find_copy_expr(CExprsList),
                  Cl = ?Syn:create(#clause{type=funexpr},
                                        [{pattern, Pars},{body,CExprs}]),
                  FunExpr = ?Syn:create(#expr{type=fun_expr},[{exprcl,Cl}]),
                  FunExpr;
              guard ->
                  NewArg = create_condition(Exprs),
                  NewArg
          end,
          Tokens = ?Syn:leaves(SubTree),
          lists:foldr(InsertToken,ok,Tokens),
          Comma = ?Syn:create_lex(',', ","),
          ?ESG:insert(Body, {llex, Index}, Comma),
          [MacroForm] = ?Query:exec(Body, [{flex, back}]),
          MacroData = ?ESG:data(MacroForm),
          ?ESG:update(MacroForm, MacroData#form{pp=node})
      end, FunAppClosingToks).

transform_recapp_macros(FunAppClosingToks, VarName) ->
    lists:foreach(
      fun(CloseParToken) ->
          [Body] = ?Query:exec(CloseParToken, [{llex, back}]),
          Index = ?Graph:index(Body, llex, CloseParToken),
          VarTok = ?Syn:create_lex(variable, VarName),
          CommaTok = ?Syn:create_lex(',', ","),
          ?ESG:insert(Body, {llex, Index}, VarTok),
          ?ESG:insert(Body, {llex, Index}, CommaTok),
          [MacroForm] = ?Query:exec(Body, [{flex, back}]),
          MacroData = ?ESG:data(MacroForm),
          ?ESG:update(MacroForm, MacroData#form{pp=node})
      end, FunAppClosingToks).

insert_app_arg(FunCallData, Type, ParNames) ->
    lists:map(
        fun({AppArgs, Exprs, App, _})->
            Pars = [?Syn:create(#expr{type = variable}, [Par])
                    || Par <- ParNames],
            case Type of
                lhs    ->
                    CExprsList = [{?Syn:copy(Expr), Expr}||Expr<-Exprs],
                    CExprs     = find_copy_expr(CExprsList),
                    [NewArg] = CExprs;
                var    ->
                    CExprsList = [{?Syn:copy(Expr), Expr}||Expr<-Exprs],
                    CExprs     = find_copy_expr(CExprsList),
                    [NewArg] = CExprs;
                funexp ->
                    CExprsList = [{?Syn:copy(Expr), Expr}||Expr<-Exprs],
                    CExprs     = find_copy_expr(CExprsList),
                    Cl     = ?Syn:create(#clause{type=funexpr},
                                        [{pattern, Pars},{body,CExprs}]),
                    NewArg = ?Syn:create(#expr{type=fun_expr},[{exprcl,Cl}]);
                guard  ->
                    NewArg = create_condition(Exprs),
                    CExprs = [NewArg]
            end,
            [ArgListNode] = ?Query:exec(App, ?Expr:child(2)),
            ?Syn:replace(ArgListNode, {esub, length(AppArgs) + 1, 0}, [NewArg]),
            CExprs
        end, FunCallData).

create_condition([Left, Type, Right])->
    case Type of
        conjunction -> Value = 'andalso';
        disjunction -> Value = 'orelse'
    end,
    CondL = create_condition(Left),
    CondR = create_condition(Right),
    ?Syn:create(#expr{type = infix_expr, value = Value},
         [{exprcl, [?Syn:create(#clause{type = expr}, [{body, [CondL]}])]
           ++ [?Syn:create(#clause{type = expr}, [{body, [CondR]}])]}]);
create_condition(Node) ->
    CNode = ?Syn:copy(Node),
    hd(find_copy_expr([{CNode, Node}])).

insert_rec_app_arg(RecCallData, VarName)->
    lists:foreach(
        fun({AppArgs, _Exprs, App, _})->
            VarNode = ?Syn:create(#expr{type = variable}, [VarName]),
             [ArgListNode] = ?Query:exec(App, ?Expr:child(2)),
            ?Syn:replace(ArgListNode, {esub, length(AppArgs) + 1, 0}, [VarNode])
        end, RecCallData).

insert_rec_app_arg_lhs(RecCallData, VarNode)->
    lists:foreach(
        fun({AppArgs, _Exprs, App, _})->
             [ArgListNode] = ?Query:exec(App, ?Expr:child(2)),
            ?Syn:replace(ArgListNode, {esub, length(AppArgs) + 1, 0}, [VarNode])
        end, RecCallData).

insert_fun_pattern(VarName, OrCl, RecFunCl, ClData)->
    lists:foreach(
        fun({_, Pats, _, Cl})->
            case lists:member(Cl, OrCl ++ RecFunCl) of
                true ->
                    New = ?Syn:create(#expr{type=variable},[VarName]);
                false ->
                    New = ?Syn:create(#expr{type=variable},["_" ++ VarName])
            end,
            ?Syn:replace(Cl, {pattern, length(Pats) + 1, 0}, [New])
        end, ClData).

insert_fun_pattern_lhs(Node, ClData)->
    lists:foreach(
        fun({_, Pats, _, Cl})->
            Copy = ?Syn:copy(Node),
            {Node, NewNode} = lists:keyfind(Node, 1, Copy),
            ?Syn:replace(Cl, {pattern, length(Pats) + 1, 0}, [NewNode])
        end, ClData).

replace_expressions(Parent, VarName, DelExprs, Type, ArgNames, Link)->
    VarNode = ?Syn:create(#expr{type = variable}, [VarName]),
    case Type of
        funexp ->
            Args = [?Syn:create(#expr{type = variable}, [Arg])
                    || Arg <- ArgNames],
            ArgListNode = ?Syn:create(#expr{type=arglist}, [{esub, Args}]),
            Node        = ?Syn:create(#expr{type=application},
                              [{esub,VarNode}, {esub, ArgListNode}]);
        _      ->  Node = VarNode
    end,
    case Link of
        body -> ?Syn:replace(Parent,
                             {range, hd(DelExprs), lists:last(DelExprs)},
                             [Node]);
        _   -> ?Syn:replace(Parent,{node, hd(DelExprs)},[Node])
    end,
    Node.

replace_expressions_lhs(MatchExp, NewLeft)->
    [{_, MatchExpParent}] = ?Syn:parent(MatchExp),
    NodeSiblings = [ X || {body, X} <-?Graph:links(MatchExpParent)],
    NextNode = following_node(MatchExp, NodeSiblings),
    if
        NextNode =:= MatchExp ->
            ?Syn:replace(MatchExpParent, {node, MatchExp}, [NewLeft]),
            Node = NewLeft;
        true               ->
            ?Syn:replace(MatchExpParent, {node, MatchExp}, []),
            Node = NextNode
    end,
    Node.

insert_old_fun(_, _, _, _, _, _, false)->
    ok;
insert_old_fun(File, ClData, Exprs, Type, ArgNames, Index, true)->
    Data =
        lists:map(
            fun({[Name], Pats, Guard, _})->
                CGuardList = [{?Syn:copy(Expr), Expr}||Expr<-Guard],
                CGuard     = find_copy_expr(CGuardList),
                CPatsList1 = [{?Syn:copy(Expr), Expr}||Expr<-Pats],
                CPats1     = find_copy_expr(CPatsList1),
                CNameList1 = ?Syn:copy(Name),
                CName1     = find_copy_expr([{CNameList1, Name}]),
                CPatsList2 = [{?Syn:copy(Expr), Expr}||Expr<-Pats],
                CPats2     = find_copy_expr(CPatsList2),
                AppArgs    = tr_joker_patterns(CPats2),
                CNameList2 = ?Syn:copy(Name),
                CName2     = find_copy_expr([{CNameList2, Name}]),
                Args = [?Syn:create(#expr{type = variable}, [Arg])
                        || Arg <- ArgNames],
                case Type of
                    lhs    ->
                        CExprsList = [{?Syn:copy(Expr), Expr}||Expr<-Exprs],
                        CExprs     = find_copy_expr(CExprsList),
                        NewArg = CExprs;
                    var    ->
                        CExprsList = [{?Syn:copy(Expr), Expr}||Expr<-Exprs],
                        CExprs     = find_copy_expr(CExprsList),
                        NewArg = CExprs;
                    funexp ->
                        CExprsList = [{?Syn:copy(Expr), Expr}||Expr<-Exprs],
                        CExprs     = find_copy_expr(CExprsList),
                        Clause = ?Syn:create(#clause{type=funexpr},
                                      [{pattern, Args},{body,CExprs}]),
                        NewArg = [?Syn:create(#expr{type=fun_expr},
                                                       [{exprcl,Clause}])];
                    guard  -> NewArg = [create_condition(Exprs)]
                end,
                ArgListNode = ?Syn:create(#expr{type=arglist},
                                  [{esub, AppArgs++NewArg}]),
                NewApp = ?Syn:create(#expr{type = application},
                                     [{esub, CName2}, {esub, ArgListNode}]),
                case Type of
                    guard ->
                        C = ?Syn:create(#clause{type = fundef}, [{name, CName1},
                                        {pattern, CPats1}, {body,  [NewApp]}]);
                    _ ->
                        C = ?Syn:create(#clause{type = fundef},
                                        [{name, CName1}, {pattern, CPats1},
                                         {guard, CGuard}, {body,  [NewApp]}])
                end,
                {C, NewApp}
            end, ClData),
    FunCls = [Cl || {Cl, _} <- Data],
    Apps = [App || {_, App} <- Data],
    OldFun = ?Syn:create(#form{type = func}, [{funcl, FunCls}]),
    ?File:add_form(File, Index, OldFun),
    {OldFun, Apps}.

find_copy_expr(CExprsList)->
    {_Exprs, CExprs} = lists:unzip([lists:keyfind(Expr, 1, List) ||
                                         {List, Expr} <- CExprsList]),
    CExprs.

tr_joker_patterns(ExprList) ->
    lists:map(
        fun(Pattern) ->
            case ?Expr:type(Pattern) of
                joker -> ?Syn:create(
                            #expr{type=atom},["undef"]);
                _     -> Pattern
            end
        end, ExprList).

move_macros_records(no_move, _) -> ok;
move_macros_records([], _) -> ok;
move_macros_records([{_Ind, Form, File} | Moves], AppInd) ->
    ?File:del_form(File, Form),
    ?File:add_form(File, AppInd, Form),
    move_macros_records(Moves, AppInd+1).

prepare_guard(no_pair) ->
    [];
prepare_guard([{App, Pair} | Pairs]) ->
    [AppListNode] = ?Query:exec(App, ?Expr:child(2)),
    TrGuard = lists:last(?Query:exec(AppListNode, ?Expr:children())),
    TrVars  = [{?Expr:value(Var), Var, ?Syn:parent(Var)} ||
                Var <- ?Query:exec(TrGuard, ?Expr:deep_sub()),
                ?Expr:type(Var) == 'variable'],
    Transform = [{Var, Parent, NewVar} || {Name1, Var, [{_,Parent}]} <- TrVars,
                                          {Name2, NewVar} <- Pair,
                                           Name1 == Name2],
    Transform ++ prepare_guard(Pairs);
prepare_guard([]) ->
    [].

transform_guard([]) ->
    ok;
transform_guard([{Var, Parent, NewVar} | Tail]) ->
    CNode = ?Syn:copy(NewVar),
    New = find_copy_expr([{CNode, NewVar}]),
    ?Syn:replace(Parent, {node, Var}, New),
    transform_guard(Tail).

get_next_sibling(N, [{body, N}]) -> [];
get_next_sibling(N, [{body, N} | T]) -> possible_comment_container(hd(T));
get_next_sibling(N, [_ | T]) -> get_next_sibling(N, T);
get_next_sibling(_, _) -> [].

possible_comment_container({clex, N}) -> [N];
possible_comment_container(_) -> [].

get_existing_comment([N]) -> ?Syn:get_comments([N]);
get_existing_comment(_) -> [{[],[]}].

following_node(P, [P]) -> P;
following_node(P, [P | T])   -> hd(T);
following_node(P, [_ | [P]]) -> P;
following_node(P, [_ | T])   -> following_node(P, T).

rhs_exps([Expr]) ->
    [ExprParent] = ?Query:exec(Expr, ?Expr:top()),
    ?ESG:path(ExprParent, [{esub,2}]).

is_generalizable_lhs([Expr]) ->
    [ExprParent] = ?Query:exec(Expr, ?Expr:top()),
    ExprParType = ?Expr:type(ExprParent),
    LeftSide = ?ESG:path(ExprParent, [{esub,1}]),
    if
        ExprParType =/= match_expr orelse hd(LeftSide) =/= Expr ->
            false;
        true ->
            [LHS] = ?ESG:path(ExprParent, [{esub,1}]),
            [RHS] = ?ESG:path(ExprParent, [{esub,2}]),
            compare_sides(LHS,RHS)
    end;

is_generalizable_lhs(_) ->
    false.

compare_sides(LHS,RHS) ->
    Lesubs = ?Graph:path(LHS, [esub]),
    Resubs = ?Graph:path(RHS, [esub]),
    Llen = length(Lesubs),
    Rlen = length(Resubs),
    Ltype = ?Expr:type(LHS),
    Rtype = ?Expr:type(RHS),
    Rconstant = lists:member(Rtype, constant_literal()),
    if
        Llen > 0 andalso (Llen =/= Rlen orelse Ltype =/= Rtype) ->
            false;
        Llen =:= 0 andalso (Ltype =/= variable orelse not Rconstant) ->
            false;
        Llen > 0 ->
            MatchingSides = lists:zipwith(fun compare_sides/2, Lesubs, Resubs),
            is_all_true(MatchingSides);
        true ->
            true
    end.

is_all_true([]) ->
    true;
is_all_true([H|T]) ->
    if
        H =/= true ->
            false;
        true ->
            is_all_true(T)
    end.

constant_literal() ->
    [integer, float, atom, string, cons, list, tuple].

copy_left(MatchExp)->
    LeftExpr = hd(?ESG:path(MatchExp, [{esub, 1}])),
    Copy = ?Syn:copy(LeftExpr),
    {LeftExpr, NewLeftNode} = lists:keyfind(LeftExpr, 1, Copy),
    NewLeftNode.
