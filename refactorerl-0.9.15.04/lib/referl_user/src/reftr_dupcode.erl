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

%%% @doc This module implements the duplicated code elimination refactoring.
%%% A clone contains two or more members, that need to be eliminated.
%%% A selected clone's specific variables will be renamed, every other member
%%% will be extracted using the modified extract funciton refactoring.
%%% If a clone group is present in more than one module, then a module is
%%% chosen, and the extracted funciton will be exported.

%%% == Parameters ==
%%% <ul>
%%%   <li> A previously saved duplicated code search name </li>
%%%   <li> A groupnumber </li>
%%% </ul>
%%%
%%% == Conditions of applicability ==
%%% <ul>
%%%   <li> The clone name should be valid.</li>
%%%   <li> The group number must be present in the given clone.</li>
%%%   <li> The class of the clone should not be class 3.</li>
%%%   <li> The duplicated search algorithm must be filtered_suffix_tree
%%%   and the max_invalid_seq_length paramteres set to 0.</li>
%%% </ul>
%%%
%%% == Rules of the transformation ==
%%% <ol>
%%%   <li> Asks for clone name and group number.</li>
%%%   <li> Collects variable data to call the extract function refactoring.</li>
%%%   <li> Checks if the clone group is a class 3 clone. </li>
%%%   <li> If needed asks the user to give a module where the function will be
%%%   extracted.</li>
%%%   <li> In one of the clone member the variables will be renamed.</li>
%%%   <li> Calls the extract funtion refactoring.</li>
%%% </ol>
%%%
%%% == Implementation status ==
%%% The transformation is not fully implemented.
%%%
%%% @author Szabó Bence János <szbtadi@caesar.elte.hu>

-module(reftr_dupcode).
-vsn("$Rev: 12391 $"). % for emacs"

%% Callbacks
-export([prepare/1, error_text/2]).

-include("user.hrl").
-include("refusr_clone_identifierl.hrl").

-define(TABNAME, reftr_dupcode_ets).

%%% ============================================================================
%%% Callbacks

prepare(Args) ->
    CloneName = ask_clone_name(Args),
    Group     = ask_clone_group(Args),
    Clone = 
        case ?Lib:match_name_dets(CloneName, original, undef) of 
            {false, _} -> throw(?LocalError(invalid_name, CloneName));
            {true, Result}  ->
                CloneNum  = proplists:get_value(detected_clones_num, Result),
                Options   = proplists:get_value(options, Result),
                Algorithm = proplists:get_value(algorithm, Options),
                InvSeqLen = proplists:get_value(max_invalid_seq_length, Options),
                Clones    = proplists:get_value(detected_clones, Result),
                ?Check(CloneNum >= Group orelse Group == -1, ?LocalError(invalid_group, Group)),
                ?Check(Algorithm == filtered_suffix_tree, ?LocalError(invalid_algorithm, [Algorithm])),
                ?Check(InvSeqLen == 0, ?LocalError(max_invalid_seq_length, InvSeqLen)),
                lists:usort(lists:nth(Group, Clones))
        end,
    {MainPattern,Datas} = collect_data(Clone),
    R = {Replace, _RepExprs} = choose_replace_expr(Datas),
    ElimModule = choose_elim_module(Datas, Args),
    [fun () ->
        % replaces the renamed variables in the to be extracted clone.
        Nodes = 
            [[{?Syn:create(#expr{type = variable}, [?MISC:any_to_string(Name)]),  Node}
                    || Node<-Nodes]
                        || {Name,{Nodes, _Value, _Type}}<-Replace],
        lists:map(fun(NodeAndOld)->
            lists:map(fun({Node, Old})->
                [{_,Parent}|_] = ?Syn:parent(Old),
                ?Syn:replace(Parent,{node, Old}, [Node])
            end, NodeAndOld)
        end, Nodes)
     end,
     fun(_)->
        Res = ?Transform:do(embed, reftr_extract_fun,
            {Args++[{elim_clone,{Datas, R, MainPattern, ElimModule}}],continue_on_failure,{[],fun(A)->A end},embed}),
        {result,[{result, Node}, _, _], _} = Res,
        Node
     end].


ask_clone_name(Args) ->
    Question = [{format,textbox},
                {text, "Please enter a saved duplicated code result name: "},
                {default, dupgroup_name}],
    case proplists:get_value(clone_name, Args) of
        undefined -> [CloneName] = ?Transform:question([Question]),
                 list_to_atom(CloneName);
        Name -> Name
    end.

ask_clone_group(Args) ->
    Question = [{format,textbox},
                {text, "Please specify a clone group number: "},
                {default, 1}],
   case proplists:get_value(group, Args) of
    undefined ->
        [Group] = ?Transform:question([Question]),
        try
            list_to_integer(Group)
        catch
        _:_ -> throw(?LocalError(invalid_group, Group))
        end;
    Group -> Group
    end.

ask_module(Modules, Mods) ->
    Qu = [{format,info},{text,"Please specify a module:"}],
    Question = [Qu] ++
        [add_to_proplist(io_lib:format("~p",[Mod]))
         || Mod <- Mods],
    Ans = ?Transform:question(Question),
    ModName = get_mod_from_ans(Ans,Mods),
    {_,ModNode,_} = lists:keyfind(ModName, 3, Modules),
    {export,ModNode}.


% Error handling
error_text(invalid_name, Name)->
    io_lib:format("No duplicated code result found with name: ~p.~n",[Name]);
error_text(invalid_group, Group) ->
    io_lib:format("Invalid groupnumber: ~p.~n",[Group]);
error_text(max_invalid_seq_length, _)->
    io_lib:format("Invalid sequence length must be set to 0.");
error_text(invalid_algorithm, _)->
    io_lib:format("Only the filtered_suffix_tree algorithm is supported.~n",[]);
error_text(unsupported_type, _)->
    io_lib:format("This type of clone elimination is currenty unsupported.~n",[]).

%% @doc Collects information for the elimination process.
collect_data(Clone) ->
    DelExprs = [ [ Unit#unit.id || Unit<-Units] || Units<-?Lib:unzip_clones(Clone)],
    check_type(DelExprs),
    VarNames = variable_names(lists:flatten(DelExprs)),
    ets:new(?TABNAME,[set,named_table]),
    ets:insert(?TABNAME,{id,0}),
    ets:insert(?TABNAME,{vars,VarNames}),
    ets:insert(?TABNAME,{bindings,[]}),
    Vars = get_vars(Clone,[]),
    ets:delete(?TABNAME),
    VarBinds = [scope_var_binds(Exprs) || Exprs<-DelExprs],
    OrdVars  = ord_vars(Vars,1,length(DelExprs),[]),
    NotBoundsAndOutVars = notbound_and_out_vars(DelExprs, OrdVars, []),
    {MainPattern,ExtendedBoundOuts}   = extend_outvars(NotBoundsAndOutVars, VarBinds),
    {MainPattern,lists:zip3(OrdVars,DelExprs,ExtendedBoundOuts)}.


choose_replace_expr(Datas) ->
    ExprWithLen = [choose_replace_expr0(Data) || Data<-Datas],
    {_, Vars, Exprs} = lists:max(ExprWithLen),
    {Vars, Exprs}.
choose_replace_expr0(Data) ->
    {Vars, Exprs, {_, Outs}} = Data,
    NoJokerOuts = [Out || Out<-Outs, Out /= joker],
    {length(NoJokerOuts), Vars, Exprs}.

%% @doc Checks if the clone is level3 type clone.
check_type(ExprsList) ->
    DeepExprsList = 
        [ lists:flatten([ begin E = ?Lib:deep_sub(Expr),
                  [ ?Expr:value(Exp) || Exp<-E, ?Expr:type(Exp) == infix_expr] end || Expr<-Exprs]) || Exprs<-ExprsList],
    Res = check_same(DeepExprsList, 1, length(hd(DeepExprsList))),
    ?Check(Res, ?LocalError(unsupported_type, [])).

check_same(_Exprs, Act, Max) when Act > Max -> true;
check_same(Exprs, Act, Max) ->
    Nths = get_nth(Exprs, Act),
    V = hd(Nths),
    case lists:all(fun(Val)-> Val == V end, Nths) of
        true -> check_same(Exprs, Act+1, Max);
        _ -> false
    end.


variable_names(Exprs) ->
    Vars = 
        lists:usort(lists:flatten(
            [?Query:exec(Expr,?Expr:variables()) || Expr<-Exprs])),
    lists:usort([?Var:name(Var) || Var<-Vars, Var /= []]).


ord_vars([], _, Max, _) -> lists:duplicate(Max, []);
ord_vars(_, Act, Max, Res) when Act > Max -> Res;
ord_vars(Vars, Act, Max, Res) ->
    NewVar = [{Name,lists:nth(Act,Var)} || {Name, Var}<-Vars],
    ord_vars(Vars, Act+1, Max, [NewVar | Res]).


%% @doc Collects variables to the elimiation process.
get_vars([], Res) -> lists:reverse(Res);
get_vars([#clone_item{items = Items} | Clone], Res) ->
    NewRes = get_vars0(Items, Res),
    get_vars(Clone, NewRes).

get_vars0([], Res) -> Res;
get_vars0(Units, Res) ->
    AllExprs = [?Lib:deep_exprs(Unit#unit.id) || Unit<-Units],
    pair_vars(AllExprs,1,length(hd(AllExprs)), Res).

%% @doc Creates a new variable if needed, and stored the
%%      neccessary informations.
pair_vars(_, Act, Max, Res) when Act > Max -> Res;
pair_vars(Exprs, Act, Max, Res) ->
    Vars = [{[Expr], ?Expr:value(Expr), ?Expr:type(Expr)}
                || Expr<-get_nth(Exprs, Act), is_binded(Expr)],
    Result = check_vars(Vars, Res),
    case Result of
        Res -> pair_vars(Exprs, Act+1, Max, Res);
        new_var ->
            VarNames = ets:lookup_element(?TABNAME, vars, 2),
            NewName = generate_new_name(VarNames),
            pair_vars(Exprs, Act+1, Max, [{NewName, Vars} | Res]); 
        {old, NewRes} -> pair_vars(Exprs, Act+1, Max, NewRes);
        no_var -> pair_vars(Exprs, Act+1, Max, Res)
    end.

generate_new_name(VarNames) ->
    Id = ets:update_counter(?TABNAME, id, {2, 1}),
    NewName = "NewVar"++integer_to_list(Id),
    case lists:member(NewName, VarNames) of
        true -> generate_new_name(VarNames);
        _ -> NewName
    end.

is_binded(Expr) ->
    VarB = ?Query:exec(Expr,?Expr:varbinds()),
    Vars = ?Query:exec(Expr,?Expr:variables()),
    case {VarB,Vars} of
        {[],[]}  -> true;
        {[],[Var]} -> 
            Bindings = ets:lookup_element(?TABNAME,bindings,2),
            not lists:member(Var, Bindings);
        {VarB,_}  ->
            Bindings = ets:lookup_element(?TABNAME,bindings,2),
            ets:update_element(?TABNAME,bindings,{2,Bindings++VarB}),
            false
    end.
              

%foldl?
get_nth([], _) -> [];
get_nth([List | Lists], N) ->
    get_nth(Lists, N)++[lists:nth(N, List)].


%% @doc Determines if the connected variables needed to be extracted.
check_vars([], Res) -> Res;
check_vars(Vars, Res) ->
    {Exprs, Names, _} = lists:unzip3(Vars),
    case length(lists:usort(Names)) of
        1 -> no_var;
        _ -> check_vars0(Names, Exprs, Res, [])
    end.


check_vars0(_, _, [], _) -> new_var;
check_vars0(_, _, old, Res) -> {old, Res};
check_vars0(Names, Exprs, [Var | Vars], Res) ->
    {Name,VarList} = Var,
    {VExprs,VarNames, Types} = lists:unzip3(VarList),
    case VarNames of
        Names ->
            New = create_new_var(Name, Names, Exprs, VExprs, Types, []),
            check_vars0(Names, Exprs, old, lists:reverse([New | Res])++Vars);
        _ -> check_vars0(Names, Exprs, Vars, [Var | Res])
    end.

create_new_var(VarName, [], [], [], [], Res) -> {VarName, lists:reverse(Res)};
create_new_var(VarName, [Name | Names], [Expr | Exprs], [VExpr | VExprs],
                    [Type | Types], Res) ->
    New = {VExpr++Expr, Name, Type},
    create_new_var(VarName, Names, Exprs, VExprs, Types, [New | Res]).


scope_var_binds(Exprs) ->
    lists:usort(?Query:exec(lists:last(Exprs), ?Expr:scope_varbinds())).
    %usort??? nem kéne nagyon ezzel játszani

%% @doc Returns the bound vars of the expressions, and the outside used vars.
notbound_and_out_vars([], [], Res) -> lists:reverse(Res);
notbound_and_out_vars([Exprs | Exprs0], [Vars | Vars0], Res) ->
    {Bound, NotBound} = reftr_extract_fun:vars(Exprs,Vars),
    OutVars = check_var(Bound,Exprs),
    notbound_and_out_vars(Exprs0, Vars0, [{NotBound, OutVars} | Res]).


%% @doc Extends the outside used variables to the maximum union of the group.
extend_outvars(NotBoundsAndOutVars, VarBinds) ->
    {_, Fullest} =
        lists:max([{length(Outs),Outs} || {_, Outs}<-NotBoundsAndOutVars]),
    ExtFullest = extend_fullest(Fullest, NotBoundsAndOutVars, VarBinds),
    FullestBinds = 
        [Binds || Binds<-VarBinds,
                      length(Binds -- Fullest) < length(Binds)],
    Res = case FullestBinds of
        [] -> 
            [ begin
                {NotBound, Outs} = NBAOV,
                Names = [?Var:name(V) || V<-NotBound],
                Names1 = [?Var:name(V) || V<-Outs],
                {Names,Names1}
            end || NBAOV<-NotBoundsAndOutVars];
        [_] -> 
            Param = {hd(FullestBinds), ExtFullest},
            extend_outvars0(NotBoundsAndOutVars, VarBinds, Param, [])
    end,
    {[?Var:name(V) || V<-ExtFullest],Res}.

%% @doc Calculates the maximum union outside used variables.
extend_fullest([], _, _) -> [];
extend_fullest(_, [], _) -> [];
extend_fullest(Fullest, NotBoundsAndOutVars, VarBinds) ->
    BoundAndOuts = [F || {_, F}<-NotBoundsAndOutVars, Fullest /= F],
    Pos = find_pos_in_list(Fullest, VarBinds, 1),
    extend_fullest0(Fullest, BoundAndOuts, VarBinds, Pos).

extend_fullest0(Fullest, [], _, _) -> Fullest;
extend_fullest0(Fullest, [H|T], VarBinds, Pos) ->
    TransBinds = group_by_nth(VarBinds, 1, []),
    FilterH = [ext_not_in(V, Fullest, TransBinds, Pos) || V<-H],
    FFilterH = [V || V<-FilterH, V /= no],
    extend_fullest0(Fullest ++ FFilterH, T, VarBinds, Pos).

find_pos_in_list(List, [H|T], N) ->
    case ?MISC:intersect(List, H) of
        [] -> find_pos_in_list(List, T, N+1);
        _  -> N
    end.

group_by_nth([], _, _) -> [];
group_by_nth(List, N, Res) when length(hd(List)) < N -> lists:reverse(Res);
group_by_nth(List, N, Res) ->
    group_by_nth(List, N+1, [lists:reverse(get_nth(List, N)) | Res]).

ext_not_in(E, List, Assocs, Pos) ->
    [EPairs] = [Assoc || Assoc<-Assocs, lists:member(E, Assoc)],
    PairOfE = lists:nth(Pos, EPairs),
    case lists:member(PairOfE, List) of
        true -> no;
        _    -> PairOfE
    end.


%% @doc Extends a member's outside used vars to the maximum union.
extend_outvars0(NotBoOuts, _, {_, []}, _) -> NotBoOuts;
extend_outvars0([], [], _, Res) -> lists:reverse(Res);
extend_outvars0([{B, Outs} | NotBoOuts], [_| VarBinds], {Fb, F} , Res)
        when length(Outs) == length(F) ->
    Names = [?Var:name(V) || V<-Outs],
    Names1 = [?Var:name(V) || V<-B],
    extend_outvars0(NotBoOuts, VarBinds, {Fb, F}, [{Names1,Names} | Res]);
extend_outvars0([{N, Outs} | NotBoOuts], [VarBind | VarBinds], {Fb, F}, Res) ->
    Extended = apply_extend(VarBind, Fb, F, []),
    Jokerd   = apply_jokers(Extended, Outs, []),
    Names1 = [?Var:name(V) || V<-N],
    extend_outvars0(NotBoOuts, VarBinds, {Fb, F}, [{Names1,Jokerd} | Res]).

apply_extend(_, _, [], Res) -> lists:reverse(Res);
apply_extend(Binds, Fb, [H|T], Res) ->
    DropLen = length(lists:takewhile(fun(K) -> K /= H end, Fb)),
    apply_extend(Binds, Fb, T, [lists:nth(DropLen+1, Binds) | Res]).

apply_jokers([], _, Res) -> lists:reverse(Res);
apply_jokers([Var | Vars], Outs, Res) ->
    case lists:member(Var, Outs) of
        true -> apply_jokers(Vars, Outs, [?Var:name(Var) | Res]);
        _    -> apply_jokers(Vars, Outs, [joker | Res])
    end.

check_var(BoundVars, Exprs)->
    Occurrences  = lists:flatten([?Query:exec(BoundVars,
                                              ?Var:occurrences(Expr)) ||
                                  Expr <- Exprs]),
    AllVarOccurs = ?Query:exec(BoundVars, ?Var:occurrences()),
    OutsideVars = AllVarOccurs -- Occurrences,
    lists:usort(
        lists:flatmap(fun(Var) ->
            ?Query:exec(Var, ?Expr:variables()) end, OutsideVars)).


%% @doc Asks the user in which module the user wants the function to be extracted.
choose_elim_module(Exprs, Args) ->
    Modules =
        lists:map(fun({_, [Expr | _], _}) ->
            [Module] =
                ?Query:exec(Expr,
                    ?Query:seq([?Expr:clause(), ?Clause:form(), ?Form:module()])),
            Name = ?Mod:name(Module),
            {Expr, Module, Name}
        end, Exprs),
    Mods = lists:usort([Name || {_, _, Name}<-Modules]),
    {ToExport, Module} = 
        case length(Mods) of
            1 -> {_,ModNode,_} = hd(Modules),
                 {no_export, ModNode};
            _ ->
                MaxMod = max_mod_occ(Modules),
                case proplists:get_value(ask_missing, Args) of
                    true -> ask_module(Modules, Mods);
                    _    -> {_,ModNode,_} = lists:keyfind(MaxMod, 3, Modules),
                            {export,ModNode}
                end
        end,
    {ToExport, Module}.


add_to_proplist(Name) ->
    [{format,radio},
     {text,lists:flatten(Name)},
     {default,false}].

get_mod_from_ans([info | Ans], Mods) ->
    get_mod_from_ans(Ans, Mods);
get_mod_from_ans([no | Ans], [_ | Mods]) ->
    get_mod_from_ans(Ans, Mods);
get_mod_from_ans([yes | _], [Mod | _]) ->
    Mod.

%% @doc Returns the module with the max occurences in clone.
max_mod_occ(Modules) ->
    [Mod | Mods] = [Name || {_,_,Name}<-Modules],
    max_mod_occ(Mods, Mod, 1, Mod, 0).

max_mod_occ([], _, _, OMod, _OAct) -> OMod;
max_mod_occ([M | Mods], Mod, Max, OMod, OAct) when M == Mod ->
    max_mod_occ(Mods, Mod, Max+1, OMod, OAct);
max_mod_occ([M | Mods], Mod, Max, _OMod, OAct) when Max > OAct ->
    max_mod_occ(Mods, M, 1, Mod, Max);
max_mod_occ([M | Mods], _Mod, _Max, OMod, OAct) ->
    max_mod_occ(Mods, M, 1, OMod, OAct).
