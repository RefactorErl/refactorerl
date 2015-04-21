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
%%% In this transformation, consecutive arguments of a function are
%%% contracted into a tuple. This transformation addresses the formal parameter
%%% list of all the clauses in the function definition as well as the
%%% actual parameter list in each perceptible (viz. by static analysis)
%%% call of the function. The transformation affects more than one module
%%% if the function is exported.
%%%
%%% == Conditions of applicability ==
%%%
%%% <ul>
%%%   <li>The function must be declared at the top level of a module, not a
%%%     function expression.</li>
%%%   <li>If the number of parameters that should be contracted into tuple is
%%%     greater than one, the arity of function will be changed.
%%%     In this case, the function with new arity should not conflict with
%%%     other functions.
%%%     <ul>
%%%       <li>If the function is not exported, it should not conflict with
%%%         other functions defined in the same module or imported from
%%%         other modules.</li>
%%%       <li>If the function is exported, then besides the
%%%         requirement above, for all modules where it is imported,
%%%         it should not conflict with functions defined in those
%%%         modules or imported by those modules.</li>
%%%       <li> If the function is imported to an other module and the
%%%         transformation would introduce a conflict with an other
%%%         function, the function is removed from the import list and
%%%         the applications are qualified, thus the conflict is
%%%         resolved.</li>
%%%     </ul></li>
%%% </ul>
%%%
%%% == Transformation steps and compensations ==
%%%
%%% <ol>
%%%   <li>Change the formal parameter list in every clause of the function:
%%%     contract the formal arguments into a tuple pattern from the first
%%%     to the last argument that should be contracted.</li>
%%%   <li>If the function is exported from the module, then the export list
%%%     has to be modified: the arity of the function is updated with new
%%%     arity.</li>
%%%   <li>If the function is exported and another module imports it, then the
%%%     arity must be adjusted in the corresponding import list of that
%%%     module.</li>
%%%   <li>Implicit function references are turned into fun expressions that
%%%     call the function, and the result is handled as any other function
%%%     call.</li>
%%%   <li>For every application of the function, modify the actual parameter
%%%     list by contracting the actual arguments into a tuple from the first
%%%     to the last argument that should be contracted.</li>
%%%   <li>The transformed function is removed from the import list (or
%%%     the entire import list is removed if the removed function was
%%%     the only element), where the transformation could introduce a
%%%     conflict with an other function and the applications are
%%%     qualified.</li></ol>
%%%
%%% == Implementation status ==
%%%
%%% Dynamic function calls currently are not recognized therefore not handled.
%%% When the tool is able to detect these dynamic calls and determine the the
%%% parameters, then this transformation will able to handle these calls
%%% correctly.
%%%
%%% Currently, if the selection does not unambiguously determine the arguments
%%% of function that should be contracted into a tuple, the transformation is
%%% refused. When the tool is able to communicate with the user, this scenario
%%% can be resolved. (If the selection is an arity qualifier or an implicit
%%% function, the arguments aren't selected. So, currently these kinds of
%%% input are not permitted.)
%%%
%%% @author Kornél Horváth <kornel@inf.elte.hu>

-module(reftr_tuple_funpar).
-vsn("$Rev: 12913 $"). % for emacs"

%% Callbacks
-export([prepare/1, error_text/2]).

-include("user.hrl").

%%% ============================================================================
%%% Callbacks


%% @private
%% @spec error_text(Code::atom(), Args::[term()]) -> string()
%% @doc
%% Give back the error message text for the transformation specific errors.
%% The error is specified by the `Code' and the `ArgsList'.
error_text(fun_not_found, []) ->
    ?MISC:format("Internal error: function representation not found", []);
error_text(fun_expr, []) ->
    ?MISC:format("Fun expressions are not supported", []);
error_text(no_fundef, {ModName,FunName,OrigArity}) ->
    ?MISC:format("The definition of ~p:~p/~b is not available",
                 [ModName,FunName,OrigArity]);
error_text(zero_arity, [ModName,FunName]) ->
    ?MISC:format("Function ~p:~p should not have zero arity",
                 [ModName,FunName]);
error_text(conflict, [ModName,FunName,NewArity]) ->
    ?MISC:format("Function ~p:~p/~b is already defined",
                 [ModName,FunName,NewArity]);
error_text(conflictAuto, [ModName,FunName,NewArity]) ->
    ?MISC:format("Function ~p:~p/~b conflicts with an auto-imported function",
                 [ModName,FunName,NewArity]).

%% @private
%% @spec prepare(Args::proplist()) -> TransformFunction::(() -> ok)
%% @throws {ModuleName::atom, ErrorCode::atom(), ErrorDetails::[term()]}
%% @doc  Prepares the transformation and returns the transformation function.
prepare(Args) ->
    Exprs = get_selected_expr_range_tuple(Args),
    ?Check([] =/= Exprs, ?RefErr0r(bad_range)),
    [Expr1|_] = Exprs,
    
    [{_, Parent}] = ?Syn:parent(Expr1),
    ParentType    = get_node_type(Parent),
    ?Check(ParentType =/= funexpr, ?LocalErr0r(fun_expr)),
    ?Check(lists:member(ParentType, [fundef, arglist]), ?RefErr0r(bad_range)),
    
    Fun = get_fun_node(ParentType, Parent),
    [File] = ?Query:exec(Fun, ?Query:seq(?Fun:module(), ?Mod:file())),
    {Mod, ModFunArity} = ?Fun:mod_fun_arity(Fun),
    {ModName, FunName, OrigArity} = ModFunArity,
    ?Check([] =/= ?Query:exec(Fun, ?Fun:definition()),
           ?LocalError(no_fundef, ModFunArity)),
    ?Check(0 < OrigArity, ?LocalError(zero_arity, [ModName,FunName])),
    
    IdxLen     = length(Exprs),
    Idx1       = first_expr_idx(ParentType, Parent, Expr1),
    NewArity   = OrigArity - IdxLen + 1,
    
    ExpMods    = ?Query:exec(Fun, ?Fun:exports()),
    ImpMods    = ?Query:exec(Fun, ?Fun:imported()),
    
    IsExported = ExpMods =/= [],
    IsAutoImp  = ?Fun:is_autoimported(FunName, NewArity),
    
    case OrigArity =:= NewArity of
        true  -> ok;
        false ->
            ModNewFunArity = [ModName,FunName,NewArity],
            % VisibleFuns    = visible_funs([Mod|ImpMods], FunName, NewArity),
            IsVisibleInDefMod =
                ?Query:exec(Mod, ?Mod:visible(FunName, NewArity)),
            ?Check([] =:= IsVisibleInDefMod, 
                   ?LocalError(conflict, ModNewFunArity)),
            ?Check(not IsAutoImp orelse IsExported,
                   ?LocalError(conflictAuto, ModNewFunArity))
    end,

    ConflImpMods = visible_in_mods(ImpMods, FunName, NewArity),
    
    FunDefs = ?Query:exec(Fun, ?Query:seq([?Fun:definition(),?Form:clauses()])),
    ?Check(not is_from_mac_subst(FunDefs), ?RefErr0r(mac_error)),
    
    Impls   = ?Query:exec(Fun, ?Fun:implicits()),
    {_Links, ImplParents} = lists:unzip(lists:append([?Syn:parent(Impl) ||
                                                         Impl <- Impls])),
    FunImpExpsWArityNode  = fun_impexps(OrigArity, NewArity, Fun),
    {FunImpExps, ArityNodes} = lists:unzip(FunImpExpsWArityNode),
    ?Macro:check_single_usage([{ArityNodes, [elex]}]),
    
    DynFunCalls = ?Dynfun:collect(tuple, Fun, {Idx1, IdxLen}),
    Apps = ?Query:exec(Fun, ?Fun:applications()),
    FunAppDatas = get_funapp_datas(Apps, Idx1, IdxLen),
    IsMacroData = fun({_, ModType, _}) -> ModType/=none end,
    {MacroFunDatas,NonMacroFunDatas} = lists:partition(IsMacroData,FunAppDatas),
    FilteredMacroDatas = lists:ukeysort(3,MacroFunDatas),
    %lists:foreach(fun check_macro_unambiguousity/1, FilteredMacroDatas),
    MacroAppForms = get_expr_forms(FilteredMacroDatas),
    {DelFormInfo, UpdFormInfo} =
        get_imp_exp_attr_form_info(FunImpExps, ConflImpMods, IsAutoImp),
    
    [fun() ->
             [make_tuple_in_macro_parts(MacroData) || 
                 MacroData <- FilteredMacroDatas]
     end,
     fun(_) ->
        ?Expr:expand_funexpr(Impls)
     end,
     % todo Use ?Syn:get_comment and put_comment to preserve comments
     fun(ExpandedImplicits) ->
        % Expanded implicits must be modified too.
        AppsFromImpls =
                 ?Query:exec(ExpandedImplicits, ?Query:seq([?Expr:clauses(),
                                                            ?Clause:body()])),

        FuncAppls = [ A || {A,_,_} <- NonMacroFunDatas] ++ AppsFromImpls,

        AppsToQualify =
                 select_apps_for_qual(FuncAppls, ConflImpMods, IsAutoImp),
        % Modifying the function definition
        [make_tuple_in_fundef(FunDef, Idx1, IdxLen) || FunDef <- FunDefs],

        % Modifying the parameter list of the function applications
        [begin
             make_tuple_in_application(FunApp, Idx1, IdxLen)
         end || FunApp <- FuncAppls],

        % Adding module qualifiers to applications where it is necceasary
        [?Expr:add_modq(App_, ModName) || App_ <- AppsToQualify],

        % Removing functions from import forms where it should
        % introduce clash with other function (autoimported or other
        % function) and changing the export list

        % Updating
        ToTouchUpds =
            [begin
                 [OldArityNode] = ?Query:exec(Expr, ?Expr:child(2)),
                 change_impexp_arity(Expr, OldArityNode, NewArity),
                 Expr
             end || Expr <- UpdFormInfo],

         % Deleting
         DelFun =
             fun({form, delete, AttrForm}) ->
                     [F] = ?Query:exec(AttrForm, ?Form:file()),
                     ?File:del_form(AttrForm),
                     % the form will be deleted, return the filenode instead
                     F;
                ({expr, delete, Expr}) ->
                     [{_, AttrParent}] = 
                         ?Query:exec(Expr, ?Expr:parent()),
                     ?Syn:replace(AttrParent, {node, Expr}, []),
                     % the expr will be deleted return the parent instead
                     AttrParent
             end,
        ToTouchDels = lists:map(DelFun, DelFormInfo),

        [?Transform:touch(Node) || 
            Node <- FunDefs ++ ImplParents ++ FuncAppls ++ ToTouchUpds ++ 
                ToTouchDels]
     end,
     fun(_) ->
	     ?Dynfun:transform(DynFunCalls)
     end,
     fun(_) ->
             [?File:reload_form(File, Form) || Form <- MacroAppForms]
     end].

%%% ============================================================================
%%% Implementation

%% Returns whether there is a function definition clause which comes
%% (partly or wholly) from a macro substitution. More exactly, this
%% function checks for tokens of the clause
is_from_mac_subst(Clauses) when is_list(Clauses) ->
    lists:any(fun(Clause) -> is_from_mac_subst(Clause) end, Clauses);
is_from_mac_subst(Clause) ->
    [] =/= [Token || Token <- ?ESG:path(Clause, [clex]), 
                     ?Syn:is_virtual(Token)].


make_tuple_in_fundef(FunDef, Idx1, IdxLen) ->
    ?Syn:replace(FunDef, {pattern, Idx1, IdxLen}, tuple_replacer()).

change_impexp_arity(ImpExp, OldArityNode, NewArity) ->
    [Token] = ?Query:exec(OldArityNode, [elex]),
    case ?Token:is_virtual(Token) of
        true ->
            ?Macro:update_virtual_token(Token, integer_to_list(NewArity));
        _    ->
            ?Syn:replace(ImpExp, {node, OldArityNode}, 
                         [new_arity_node(NewArity)])
    end.

make_tuple_in_macro_parts({_,macdef,{Parent, FirstIndex,LastIndex,_}}) ->
    LeftBrace = ?Syn:create_lex('{',"{"),
    RightBrace = ?Syn:create_lex('}',"}"),
    ?ESG:insert(Parent, {llex,FirstIndex}, LeftBrace),
    case LastIndex of
        last ->
            ?ESG:insert(Parent, llex, RightBrace);
        _ ->
            ?ESG:insert(Parent, {llex,LastIndex}, RightBrace)
    end;
make_tuple_in_macro_parts({_,arglist,{Parent,FirstIndex,LastIndex}}) ->
    [FirstArg] = ?Query:exec(Parent,[{esub,FirstIndex}]),
    SubExpr1 = get_first_subexpr(FirstArg),
    [Lex1] = ?Query:exec(SubExpr1,[elex]),
    Lex1Data = ?Graph:data(Lex1),
    Lex1Tok = Lex1Data#lex.data,
    {Lex1ToUpd, Data1, Token1} =
        case Lex1Tok of
            virtual ->
                [QuestionMarkLex] = ?Query:exec(Lex1, [llex, {llex, 1}]),
                QMLexData = ?Graph:data(QuestionMarkLex),
                {QuestionMarkLex,QMLexData,QMLexData#lex.data};
            _ ->
                {Lex1, Lex1Data, Lex1Tok}
        end,
    ?Graph:update(Lex1ToUpd,
                  Data1#lex{data=Token1#token{prews=Token1#token.prews ++
                                                  "{"}}),
    [LastArg] = ?Query:exec(Parent,[{esub,LastIndex}]),
    SubExpr2 = get_last_subexpr(LastArg),
    [Lex2] = ?Query:exec(SubExpr2,[elex]),
    Lex2Data = ?Graph:data(Lex2),
    Lex2Tok = Lex2Data#lex.data,
    {Lex2ToUpd, Data2, Token2} =
        case Lex2Tok of
            virtual ->
                [SubstLastLex] = ?Query:exec(Lex2, [llex, {llex, last}]),
                SNLexData = ?Graph:data(SubstLastLex),
                {SubstLastLex,SNLexData,SNLexData#lex.data};
            _ ->
                {Lex2, Lex2Data, Lex2Tok}
        end,
    ?Graph:update(Lex2ToUpd, 
                  Data2#lex{data=Token2#token{postws="}" ++
                                                  Token2#token.postws}}).

make_tuple_in_application(App, Idx1, IdxLen) ->
    [ArgList] = ?Query:exec(App, ?Expr:child(2)),
    ?Syn:replace(ArgList, {esub, Idx1, IdxLen}, tuple_replacer()).

tuple_replacer() ->
    fun(Lst) ->
            CLst =
                [begin
                     CNodes = ?Syn:copy(Node),
                     {Node, CNode} = lists:keyfind(Node, 1, CNodes),
                     CNode
                 end|| Node <- Lst],
            [?Syn:create(#expr{type=tuple}, [{esub, CLst}])]
    end.

get_fun_node(fundef, Parent) ->
    ?Query:exec1(Parent, ?Query:seq(?Clause:form(), ?Form:func()),
                 ?LocalErr0r(fun_not_found));
get_fun_node(arglist, Parent) ->
    ?Query:exec1(Parent, ?Query:seq(?Expr:parent(), ?Expr:function()),
                 ?LocalErr0r(fun_expr)).

get_funapp_datas([], _, _) ->
    [];
get_funapp_datas([App|Apps],Idx1,IdxLen) ->
    [ArglistNode] = ?Query:exec(App, ?Expr:child(2)),
    [LexBeforeFirst] = ?Query:exec(ArglistNode, [{elex, Idx1}]),
    [LexAfterLast] = ?Query:exec(ArglistNode, [{elex, Idx1+IdxLen}]),
    case {(?Graph:data(LexBeforeFirst))#lex.data,
          (?Graph:data(LexAfterLast))#lex.data} of
        {virtual,virtual} ->
            [FirstMac] = ?Query:exec(LexBeforeFirst, [llex, mref]),
            [LastMac] = ?Query:exec(LexAfterLast, [llex, mref]),
            case FirstMac == LastMac of
                true ->
                    [FirstOrig] = ?Query:exec(LexBeforeFirst, [orig]),
                    [LastOrig] = ?Query:exec(LexAfterLast, [orig]),
                    [Body] = ?Query:exec(FirstOrig, [{llex,back}]),
                    FirstIndex = ?Graph:index(Body,llex,FirstOrig),
                    LastIndex = ?Graph:index(Body,llex,LastOrig),
                    [{App, macdef, {Body,FirstIndex+1,LastIndex+1,FirstOrig}} |
                     get_funapp_datas(Apps,Idx1,IdxLen)];
                _ -> throw(?RefErr0r(mac_error))
            end;
        {_, virtual} ->
            [FirstArgExpr] = ?Query:exec(ArglistNode, ?Expr:child(Idx1)),
            [ExprStartTok] = apply(?Syn:first_leaf(), [FirstArgExpr]),
            [{_, StartTokParent}] = ?Syn:parent(ExprStartTok),
            ParentData = ?Graph:data(StartTokParent),
            case ParentData of
                #lex{type=subst} ->
                    StartTokMacName = ParentData#lex.data,
                    [EndTokMacSubst] = ?Query:exec(LexAfterLast,[llex]),
                    case (?Graph:data(EndTokMacSubst))#lex.data of
                        StartTokMacName ->
                            [LastOrig] = ?Query:exec(LexAfterLast, [orig]),
                            [MacBody] = ?Query:exec(LastOrig, [{llex,back}]),
                            LastIndex = ?Graph:index(MacBody,llex,LastOrig),
                            [{App, macdef, {MacBody, 1, LastIndex+1,LastOrig}} |
                             get_funapp_datas(Apps,Idx1,IdxLen)];
                        _ -> throw(?RefErr0r(mac_error))
                    end;
                _ ->
                    throw(?RefErr0r(mac_error))
            end;
        {virtual,_} ->
            [LastArgExpr] = ?Query:exec(ArglistNode, 
                                        ?Expr:child(Idx1+IdxLen-1)),
            [ExprEndTok] = apply(?Syn:last_leaf(), [LastArgExpr]),
            [{_, EndTokParent}] = ?Syn:parent(ExprEndTok),
            ParentData = ?Graph:data(EndTokParent),
            case ParentData of
                #lex{type=subst} ->
                    EndTokMacName = ParentData#lex.data,
                    [StartTokMacSubst] = ?Query:exec(LexBeforeFirst,[llex]),
                    case (?Graph:data(StartTokMacSubst))#lex.data of
                        EndTokMacName ->
                            [FirstOrig] = ?Query:exec(LexBeforeFirst, [orig]),
                            [MacBody] = ?Query:exec(FirstOrig, [{llex,back}]),
                            FirstIndex = ?Graph:index(MacBody,llex,FirstOrig),
                            [{App, macdef, 
                              {MacBody, FirstIndex+1, last,FirstOrig}} |
                             get_funapp_datas(Apps,Idx1,IdxLen)];
                        _ -> throw(?RefErr0r(mac_error))
                    end;
                _ -> throw(?RefErr0r(mac_error))
            end;
        _ ->
            ArgMacs = ?Macro:get_macros_under_expr(ArglistNode),
            case length(ArgMacs) of
                0 -> [{App, none, {}}|get_funapp_datas(Apps,Idx1,IdxLen)];
                _ -> [{App, arglist, {ArglistNode, Idx1, Idx1+IdxLen-1}} |
                      get_funapp_datas(Apps,Idx1,IdxLen)]
            end
    end.

%todo: deny transformation if the same macro (body) is used in
%different ranges of different funapp arguments. (Eg. 1-2 and 2-3).
%% check_macro_unambiguousity({_,macdef,{_Body,_,_,OrigNode}}) ->
%%     Exprs = ?Query:exec(OrigNode, [{orig,back}, {elex,back}]),
%%     ?Check(lists:all(fun(Expr)->
%%                              ?Expr:type(Expr)==arglist
%%                      end,Exprs), ?RefErr0r(mac_error)),
%%                 Appls = ?Query:exec(Exprs,[{esub,back}]),
%%                 Funs = ?Query:exec(Appls,?Expr:function()),
%%                 ?Check(length(lists:usort(Funs)) == 1, ?RefErr0r(mac_error)),
%%     ok;
%% check_macro_unambiguousity({_,arglist,_}) ->
    
%%     ok.

get_expr_forms(MacroDatas) ->
    Exprs = [Expr || {Expr,_,_} <- MacroDatas],
    case Exprs of
        [] -> [];
        _  -> Forms = ?Query:exec(Exprs, ?Query:seq(?Expr:clause(),
                                                    ?Clause:form())),
              lists:usort(Forms)
    end.

fun_impexps(OrigArity, NewArity, _) when OrigArity =:= NewArity ->
    [];
fun_impexps(_, _, Fun) ->
    ImpExps  = ?Query:exec(Fun, ?Fun:impexps()),
    
    ArityIdx = 2,
    [{ImpExp, ?Query:exec1(ImpExp, ?Expr:child(ArityIdx), ?RefErr0r(internal))}
     || ImpExp <- ImpExps].


%% Returns the type of the node.
%% todo Eliminate ?ESG call.
get_node_type(Parent) ->
    case ?ESG:data(Parent) of
        #clause{type = Type} -> Type;
        #expr{type = Type}   -> Type
    end.


visible_in_mods(Mods, FunName, NewArity) ->
    lists:usort([Mod || Mod <- Mods, 
                        ?Query:exec(Mod, 
                                    ?Mod:visible(FunName, NewArity)) =/= []]).

new_arity_node(Arity) ->
    ?Syn:construct({integer, Arity}).

%% Returns the range of expressions that describes what is to be tupled.
%% If a function application is selected, its argument nodes are returned,
%% since the number of these nodes gives the arity of the new tuple.
get_selected_expr_range_tuple(Args) ->
    case ?Args:expr_range(Args) of
        [Expr] ->
            case ?Expr:type(Expr) of
                application -> ?Query:exec(Expr, ?Expr:children());
                _           -> [Expr]
            end;
        ExprRange ->
            ExprRange
    end.


%% @doc Returns the index of the first expression and the length of
%% the selection.
first_expr_idx(Type, Parent, Expr1) ->
    All =
        case Type of
            fundef      -> ?Query:exec(Parent, ?Clause:patterns());
            arglist ->
                ?Query:exec(Parent, ?Expr:children())
        end,
    Before1  = lists:takewhile(fun(Node) -> Node =/= Expr1 end, All),
    length(Before1) + 1.


get_first_subexpr(Expr) ->
    deep_subexpr(Expr, 1).

get_last_subexpr(Expr) ->
    deep_subexpr(Expr, last).

deep_subexpr(Parent, Index) ->
    Child = ?Query:exec(Parent, ?Expr:child(Index)),
    case Child of
        [] ->
            Parent;
        [Expr] ->
            deep_subexpr(Expr, Index)
    end.

get_imp_exp_attr_form_info(FunImpExps, _ConflImpMods, true) ->
    get_imp_exp_attr_clash_with_autoimp(FunImpExps, [], []);
get_imp_exp_attr_form_info(FunImpExps, ConflImpMods, false) ->
    get_imp_exp_attr_clash_with_otherfun(FunImpExps, ConflImpMods, [], []).

get_imp_exp_attr_clash_with_autoimp([], DAcc, UAcc) ->
    {DAcc, UAcc};
get_imp_exp_attr_clash_with_autoimp([Expr|FunImpExps], DAcc, UAcc) ->
    {NewDAcc, NewUAcc} = get_imp_exp_attr_common(Expr, DAcc, UAcc),
    get_imp_exp_attr_clash_with_autoimp(FunImpExps, NewDAcc, NewUAcc).

get_imp_exp_attr_clash_with_otherfun([], _CMods, DAcc, UAcc) ->
    {DAcc, UAcc};
get_imp_exp_attr_clash_with_otherfun([Expr|FunImpExps], CMods, DAcc, UAcc) ->
    [Mod] = ?Query:exec(Expr, ?Query:seq(?Expr:attrib_form(), ?Form:module())),
    {NewDAcc, NewUAcc} = 
        case lists:member(Mod, CMods) of
            true ->
                get_imp_exp_attr_common(Expr, DAcc, UAcc);
            false ->
                {DAcc, [Expr | UAcc]}
        end,
    get_imp_exp_attr_clash_with_otherfun(FunImpExps, CMods, NewDAcc, NewUAcc).

get_imp_exp_attr_common(Expr, DAcc, UAcc) ->
    [AttrForm] = ?Query:exec(Expr, ?Expr:attrib_form()),
    AttrList = 
        ?Query:exec(AttrForm, ?Query:seq(?Form:expr(2),
                                         ?Expr:children())),
    case {?Form:type(AttrForm), AttrList} of
        {import, [_]} ->
            {[{form, delete, AttrForm} | DAcc], UAcc};
        {import,  _ } ->
            {[{expr, delete, Expr}| DAcc], UAcc};
        {export,  _ } -> {DAcc, [Expr | UAcc]}
    end.

select_apps_for_qual(AppExprs, _ConflImpMods, true) ->
    AppExprs;
select_apps_for_qual(AppExprs, ConflImpMods, false) ->
    Fun =
        fun(AppExpr, Acc) ->
                Q = [?Expr:clause(), ?Clause:funcl(),
                     ?Clause:form(), ?Form:module()],
                [AppMod] = ?Query:exec(AppExpr, ?Query:seq(Q)),
                case lists:member(AppMod, ConflImpMods) of
                    true  ->
                        [AppExpr | Acc];
                    false ->
                        Acc
                end
        end,
    lists:foldl(Fun, [], AppExprs).
