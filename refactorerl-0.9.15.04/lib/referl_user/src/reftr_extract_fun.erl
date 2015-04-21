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
%%% The Initial Developer of the Original Code is E�tv�s Lor�nd University.
%%% Portions created  by E�tv�s Lor�nd University and ELTE-Soft Ltd.
%%% are Copyright 2007-2013 E�tv�s Lor�nd University, ELTE-Soft Ltd.
%%% and Ericsson Hungary. All Rights Reserved.

%%% ============================================================================
%%% Module information

%%% @doc This module implements the extract function refactoring.
%%% An alternative of a function definition might contain an expression
%%% (or a sequence of expressions) which can be considered as a logical unit,
%%% hence a function definition can be created from it. The extracted function
%%% is lifted to the module level, and it is parametrized with the `free'
%%% variables of the original expression(s): those variables which are bound
%%% outside of the expression(s), but the value of which is used by the
%%% expression(s). The extracted function will not be exported from the module.
%%%
%%% == Parameters ==
%%% <ul>
%%%   <li> A non-empty, continuous sequence of expressions
%%%        (see {@link reflib_args:expr_range/1}). </li>
%%%   <li> The name of the function to introduce
%%%        (see {@link reflib_args:name/1}). </li>
%%% </ul>
%%%
%%% == Conditions of applicability ==
%%% <ul>
%%%   <li> The name of the function to be introduced should not conflict with
%%%   another function, either defined in the same module, imported from
%%%   another module, or being an autoimported built-in function (overloading).
%%%    Furthermore, the name should be a legal function name.</li>
%%%   <li> The starting and ending positions should delimit a sequence of
%%%   expressions.</li>
%%%   <li> Variables with possible binding occurrences in the selected
%%%   sequence of expressions should not appear outside of the
%%%   sequence of expressions, except if the selected sequence is a body
%%%   of a function.</li>
%%%   <li> The extracted sequence of expressions cannot be part of a guard
%%%   sequence.</li>
%%%   <li> The extracted sequence of expressions cannot be part of a
%%%   pattern.</li>
%%%   <li> The extracted sequence of expressions cannot be part of macro
%%%   definition, and are not part of macro application parameters.</li>
%%%   <li> If the selection is a part of a list comprehension, it must be
%%%   a single expression and must not be the generator of the comprehension.
%%%   </li>
%%% </ul>
%%%
%%% == Rules of the transformation ==
%%% <ol>
%%%   <li> Collect all variables that the selected sequence of expressions
%%%   depends on.</li>
%%%   <li> Collect variables from the selected variables in step 1, which has
%%%   binding occurrence out of the selected part of the module. </li>
%%%   <li> Collect variables from the selected variables in step 1, which is
%%%   used outside after the selection. </li>
%%%   <li> Add a new function definition to the current module with a single
%%%   alternative. The name of the function is an argument to the
%%%   refactoring. The formal parameter list consists of the variables
%%%   selected in step 2.</li>
%%%   <li> Replace the selected sequence of expressions with a function call
%%%   expression, where the name of the function is given as an argument
%%%   to the refactoring, and the actual parameter list consists of the
%%%   variables selected in step 2.</li>
%%%   <li> The order of the variables must be the same in steps 4 and 5.</li>
%%%   <li> If the selected expression is a block-expression, eliminate the
%%%   begin-end keywords from the expression in the body of the created new
%%%   function.</li>
%%% </ol>
%%%
%%% == Implementation status ==
%%% The transformation is fully implemented.
%%%
%%% @author Melinda T�th <toth_m@inf.elte.hu>

-module(reftr_extract_fun).
-vsn("$Rev: 12978 $"). % for emacs"

%% Callbacks
-export([prepare/1]).
-export([vars/2]).

-include("user.hrl").

%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args) ->
    {Expr1,Exprs,FullExprs,Var,ElimModule} = 
        case proplists:get_value(elim_clone,Args,[]) of
            [] -> [Expr0|_] = RangeExprs = ?Args:expr_range(Args),
                % need to put in this format to become compatibilite
                % with the modified extract_fun
                RVar = [],
                ROuts = null,
                ElimDatas = get_elim_data([RangeExprs], [], []),
                {Expr0, RangeExprs, [RangeExprs], [], {no_export, []}};
            {Data, Replaces,MainPattern, Mod} ->
                {RVar, Replace} = Replaces,
                ROuts = MainPattern,
                {V, DelE, Outs} = lists:unzip3(Data),
                ElimDatas = get_elim_data(DelE, V, Outs),
                {hd(Replace), Replace, DelE, V, Mod}          
        end,
     
    AllFiles = all_files(FullExprs, []),
    {Link, _Parent} = check_expr_link(Exprs),
    
    lists:foreach(fun check_expr/1, Exprs),
    Form = ?Query:exec1(Expr1, ?Query:seq([?Expr:clause(),
                                           ?Clause:funcl(),
                                           ?Clause:form()]),
                        ?RefErr0r(parent_not_form)),
    {Export, EModule} = ElimModule,
    [File] = get_file(Form, EModule),
    MName =  module_name(EModule),
    Filepath = ?File:path(File),
    [Module] = ?Query:exec(File, ?File:module()),
    {Bound, NotBound} = vars(Exprs,Var),
    OutVars = case ROuts of
                null -> check_var(Bound,Exprs);
                _ -> ROuts
            end,
    ?Check(OutVars =:= [] orelse Link =:= body,
           ?RefError(outside_used_vars, OutVars)),
    NewVars = [{variable,Name} || {Name,{_,_,_}}<-RVar],
    PatNames = [{variable,?Var:name(V)} || V <- NotBound]++NewVars,
    Arity    = length(PatNames),
    ModName  = ?Mod:name(Module),
    % @todo Add transformation info
    NewName  = ?Args:ask(Args, name,
                       fun cc_fun/2, fun cc_error/3, {Module, ModName, Arity}),

    FormIndex = form_index(File,Form),
    [?Transform:touch(F) || F<-AllFiles],
    {NewExprs, CommentExprs} = eliminate_begin_end(Exprs),
    Comments = ?Syn:get_comments(CommentExprs),
    [fun() ->
        
             add_export_form({Export, File}, NewName, PatNames),
             lists:foreach(fun({EParent, EPatNames,EDelExprs, ELink, EOutVars})->
                replace_with_app({Export, MName}, EParent, NewName, EPatNames,
                               EDelExprs, ELink, EOutVars)
             end, ElimDatas),
             add_fun_form(File, NewName, PatNames,
                          NewExprs, FormIndex, OutVars)
     end,
     fun(_) ->
            ?Syn:put_comments(NewExprs, Comments)
     end,
     fun(_)->
             ?Query:exec(?Query:seq([?File:find(Filepath),
                                     ?File:module(),
                                     ?Fun:find(NewName,Arity)]))
     end].


eliminate_begin_end(Exprs) ->
    case {?Expr:type(hd(Exprs)), length(Exprs)} of
        {block_expr, 1} ->
            NewExprs = ?Query:exec(hd(Exprs),
                                 ?Query:seq(?Expr:clauses(), ?Clause:body())),
            {[proplists:get_value(Node, ?Syn:copy(Node)) || Node <- NewExprs],
             NewExprs};
        _ -> {Exprs, Exprs}
    end.

%%% ----------------------------------------------------------------------------
%%% Checks

%% Note: the expression list is never empty, guaranteed by `?Args:expr_range/1'.
check_expr_link(E = [Expr1|Rest])->
    case ?Syn:parent(Expr1) of
        [{body, Par}] ->
            ?Check(Rest =:= [] orelse ?Expr:type(Expr1) =/= filter,
                   ?RefError(bad_kind, filter)),
            {body, Par};
        [{esub, Par}]  ->
            ParKind = ?Expr:type(Par),
            ParVal = ?Expr:value(Par),
            Index = ?Syn:index(Par, esub, Expr1),
            ?Check(ParKind =/= application orelse
                   length(E) =/= 1 orelse Index =/= 1,
                   ?RefError(bad_kind, 'application name')),
            ?Check((ParKind =/= infix_expr orelse ParVal =/= ':') orelse
                   length(E) =/= 1,
                   ?RefError(bad_kind, 'module qualifier')),
            ?Check(Rest =:= [], ?RefErr0r(bad_range)),
            {esub, Par};
        [{pattern, _Par}] -> throw(?RefError(bad_kind, pattern));
        [{guard, _Par}] -> throw(?RefError(bad_kind, guard));
        _ ->  throw(?RefErr0r(bad_kind))
    end.

check_expr(Expr) ->
    Type = ?Expr:role(Expr),
    Kind = ?Expr:type(Expr),
    ?Check(Type =:= expr,
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


cc_fun(NewName, {Module, ModName, Arity}) ->
    ?Check(?Query:exec(Module, ?Mod:local(NewName, Arity)) =:= [],
        ?RefError(fun_exists, [ModName, NewName, Arity])),
    ?Check(?Query:exec(Module, ?Mod:imported(NewName, Arity)) =:= [],
        ?RefError(imported_fun_exists, [ModName, [NewName, Arity]])),
    ?Check(not ?Fun:is_autoimported(NewName, Arity),
        ?RefError(autoimported_fun_exists, [NewName, Arity])),
    NewName.

cc_error(?RefError(fun_exists, _), NewName, {_, ModName, Arity}) ->
    error_txt("already exists", ModName, NewName, Arity);
cc_error(?RefError(imported_fun_exists, _), NewName, {_, ModName, Arity}) ->
    error_txt("is imported", ModName, NewName, Arity);
cc_error(?RefError(autoimported_fun_exists, _), NewName, {_, ModName, Arity}) ->
    error_txt("is autoimported", ModName, NewName, Arity);
cc_error(_, _, _) ->
    ?RefErr0r(unknown_exception).

error_txt(Txt, ModName, NewName, Arity) ->
    ?MISC:format("Function ~p:~p/~p ~s.",
                 [ModName, NewName, Arity, Txt]).

check_var(BoundVars, Exprs)->
    Occurrences  = lists:flatten([?Query:exec(BoundVars,
                                              ?Var:occurrences(Expr)) ||
                                  Expr <- Exprs]),
    AllVarOccurs = ?Query:exec(BoundVars, ?Var:occurrences()),
    OutsideVars = AllVarOccurs -- Occurrences,
    remove_dups([?Expr:value(Var) || Var <- OutsideVars]).

remove_dups([])    -> [];
remove_dups([H|T]) -> [H | [X || X <- remove_dups(T), X /= H]].
%%% ===========================================================================
%%% Syntactic transformations.

replace_with_app(ElimModule, Parent, AppName, AppArgNames, DelExprs, Type, OutVars)->
    Name = create_app_name(ElimModule, AppName),
    Args     = [?Syn:create(#expr{type = AType}, [?MISC:any_to_string(AName)])
                                || {AType,AName} <- AppArgNames],
    ArgList  = ?Syn:create(#expr{type = arglist}, [{esub, Args}]),
    App      = ?Syn:create(#expr{type = application}, [{esub, [Name]},
                                                       {esub, ArgList}]),
    Pattern = case OutVars of
                  [] -> App;
                  _List -> VarPattern = make_pattern(_Role = pattern, OutVars),
                           ?Syn:create(#expr{type=match_expr},
                                       [{esub, VarPattern}, {esub, App}])
              end,
    case Type of
        esub  -> ?Syn:replace(Parent, {node, hd(DelExprs)}, [Pattern]);
        body -> Dels = {range, hd(DelExprs), lists:last(DelExprs)},
                ?Syn:replace(Parent, Dels, [Pattern])
    end.


add_fun_form(File, FunName, ClPatternNames, Body, FormIndex, OutVars) ->
    ClPatternNodes = [?Syn:create(#expr{type = Type},[Name])
                     || {Type,Name} <- ClPatternNames ],
    FName   = ?Syn:create(#expr{type = atom}, [io_lib:write(FunName)]),
    FinalBodies = case OutVars of
                     [] -> Body;
                     _List -> Body ++ make_pattern(_Role = expr, OutVars)
                 end,
    ClNode  = ?Syn:create(#clause{type = fundef},[{name, FName},
                             {pattern, ClPatternNodes},{body, FinalBodies}]),
    FunForm = ?Syn:create(#form{type = func}, [{funcl,ClNode}]),
    ?File:add_form(File, FormIndex + 1, FunForm).

make_pattern(Role, Vars) ->
    case Vars of
        [Single] ->
            VarNode = ?Syn:create(#expr{role=Role, type=variable,
                                        value=Single}, [Single]),
            [VarNode];
        Vars ->
            VarNodes = [begin case Var of 
                                joker -> ?Syn:create(#expr{type=joker}, []);
                                _     -> ?Syn:create(#expr{role=Role, type=variable,
                                           value=Var}, [Var])
                            end end ||
                           Var <- Vars],
            Tuple = ?Syn:create(#expr{type=tuple},
                                [ {esub, Var} || Var <- VarNodes ]),
            [Tuple]
    end.

%%% ===========================================================================
%%% Functions added in order to handle duplicate code elimination

get_file(Form, []) ->
    ?Query:exec(Form, ?Form:file());
get_file(_Form, Module) ->
    ?Query:exec(Module, ?Mod:file()).

all_files([], Res) -> lists:usort(Res);
all_files([Exprs | FullExprs], Res) ->
    [File] = ?Query:exec(hd(Exprs), ?Query:seq([?Expr:clause(),
                                                ?Clause:form(),
                                                ?Form:file()])),    
    all_files(FullExprs, [File|Res]).

module_name([]) -> [];
module_name(Module) ->
    ?Mod:name(Module).

form_index(File, Form) ->
    FormFile = ?Query:exec(Form, ?Form:file()),
    case File of
        FormFile -> ?Syn:index(File, form, Form);
        _        -> length(?Query:exec(File, ?File:forms()))
                    %last index helyett
    end.


%% @doc Modified vars function in order to be able to handle duplicate code
%%      elimination.
vars(Exprs, []) ->
    VarBinds = lists:usort(?Query:exec(Exprs, ?Expr:varbinds())),
    VarRefs  = lists:usort(?Query:exec(Exprs, ?Expr:varrefs())),
    {VarBinds, VarRefs -- VarBinds};
vars(Exprs, ElimVars) ->
    VarBinds = lists:usort(?Query:exec(Exprs, ?Expr:varbinds())),
    Refs0 = [{Sub,?Query:exec(Sub,?Expr:varrefs())} || Sub<-deep_exprs(Exprs)],
    Refs  = [{E,V} || {E,[V]}<-Refs0, V /= []],
    GVarRefs = group_refs(Refs,[]),
    FVarRefs = filter_vars(GVarRefs, ElimVars),
    {VarBinds, lists:usort(FVarRefs -- VarBinds)}.


%% @doc Collects variable occurences in exporessions.
group_refs([], Res) -> Res;
group_refs([{RE, RV} | Refs], Res) ->
    {Group, Rem} = lists:partition(fun({_,V}) -> V == RV end, Refs),
    Mapped = [E || {E, _}<-Group]++[RE],
    group_refs(Rem, [{RV, Mapped} | Res]).

%% @doc Drops the variables if the only occurence is in ElimVars.
filter_vars(Refs, ElimVars) ->
    [V || Ref = {V,_E}<-Refs, filter_vars0(Ref, ElimVars)].
filter_vars0({_Var, Exprs}, Vars) ->
    VarExprs = lists:flatten([ Expr || {_, {Expr ,_ ,_}}<-Vars]),
    case lists:all(fun(E)-> lists:member(E, VarExprs)  end, Exprs) of
        true -> false;
        _ -> true
    end.


%% @doc Different name needed if the function is from other module 
%%      (only duplicate code elimination)
create_app_name({no_export, _}, AppName) ->
    ?Syn:create(#expr{type = atom}, [io_lib:write(AppName)]);
create_app_name({export, Module}, AppName) ->
    Name = ?Syn:create(#expr{type = atom}, [io_lib:write(AppName)]),
    ModName = ?Syn:create(#expr{type = atom}, [io_lib:write(Module)]),
    ?Syn:create(#expr{type = infix_expr, value = ':'}, [{esub, [ModName,Name]}]).


%% @doc Inserts an export form if needed (only duplicate code elimination)
add_export_form({no_export, _}, _, _) -> ok;
add_export_form({export, File} , Name, Attribs) ->
    NumOfParams = length(Attribs),
    FunName = ?Syn:create(#expr{type = atom}, [io_lib:write(Name)]),
    FunArg = ?Syn:create(#expr{type = integer}, [io_lib:write(NumOfParams)]),
    FunRef = ?Syn:create(#expr{type = funref}, [{esub, [FunName, FunArg]}]),
    FunList = ?Syn:create(#expr{type = funlist}, [{esub, FunRef}]),
    ExportForm = ?Syn:create(#form{type = export}, [{eattr, FunList}]),
    ?File:add_form(File, ExportForm).


%% @doc Collects every data to eliminate the clone.
get_elim_data([], _, _) -> [];
get_elim_data(Exprs, [], []) ->
    List = lists:duplicate(length(Exprs),[]),
    get_elim_data(Exprs, List, List);
get_elim_data([Exprs | T], [Vars | Vars0], [Out | Outs]) ->
    {Link, Parent} = check_expr_link(Exprs),
    lists:foreach(fun check_expr/1, Exprs),
    {NotBound, OutVars} =
        case {Vars,Out} of
            {[],[]} ->
                {Bound0,NotBound0} = vars(Exprs, []),
                NotBound1 = [?Var:name(V) || V<-NotBound0],
                Out0 = check_var(Bound0, Exprs),
                {NotBound1, Out0};
            _  -> Out
        end,
    ?Check(OutVars =:= [] orelse Link =:= body,
           ?RefError(outside_used_vars, OutVars)),

    Pats = [ {Type, Name} || {_, {_, Name, Type}}<-Vars],
    PatNames = [{variable,Var} || Var <- NotBound]++Pats,
    [{Parent, PatNames, Exprs, Link, OutVars}]++get_elim_data(T, Vars0, Outs).


%% @doc Returns every leaf expression of Expr.
deep_exprs(Expr) ->
    [E || E<-deep_sub(Expr), ?Expr:is_leaf(E)].

%% @doc Determenistic verions of ?Expr:deep_sub/1.
deep_sub(Sub) ->
    [Sub | ?Query:exec(Sub,
                       ?Query:any(
                          ?Query:seq(?Expr:children(), fun deep_sub/1),
                          ?Query:seq([?Expr:clauses(),
                                      ?Clause:exprs(),
                                      fun deep_sub/1])))].
