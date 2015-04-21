%%% -*- coding: latin-1 -*-

%%% The  contents of this  file are  subject to  the Erlang  Public License,
%%% Version  1.1, (the  "License");  you may  not  use this  file except  in
%%% compliance  with the License.  You should  have received  a copy  of the
%%% Erlang  Public License  along  with this  software.  If not,  it can  be
%%% retrieved at http://plc.inf.elte.hu/erlang/
%%%
%%% Software  distributed under  the License  is distributed  on an  "AS IS"
%%% basis, WITHOUT  WARRANTY OF ANY  KIND, either expressed or  implied. See
%%% the License  for the specific language governing  rights and limitations
%%% under the License.
%%%
%%% The Original Code is RefactorErl.
%%%
%%% The Initial Developer of the  Original Code is Eötvös Loránd University.
%%% Portions created  by Eötvös  Loránd University are  Copyright 2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @author Tamas Hoffman <hoffmantamas@caesar.elte.hu>

-module(refusr_deadcode).
-vsn("$Rev: 12625 $").

-export([start/0,start/1,start/2,check_interface/3]).

-include_lib("referl_user/src/user.hrl").

%% @spec start() -> ok
%% @doc Start deadcode detection on all files.
start() ->
    start(print).

%% @spec start(atom()) -> list()
%% @doc Start deadcode detection on all files.
start(Mode) when (Mode==print) or (Mode==term) or (Mode==nodes) ->
    ets:new(deadcodes,[bag,named_table]),
    ets:insert(deadcodes,{mode,Mode}),
    leaves(?Graph:root(),[]),
    DCList=ets:foldl(fun({mode,_},Acc) -> Acc; (E, Acc) -> [E|Acc] end,[],deadcodes),
    ets:delete(deadcodes),
    if (Mode==term) or (Mode==nodes) -> DCList; Mode==print -> ok end;
% Start with default mode
start(X) when is_list(X) -> start(X,print).

%% @spec start(list(),atom()) -> list()
%% @doc Start deadcode detection on the specified files.
start([],_) -> [];
start([F=[_|_]|FS],print)->
    start(F,print),start(FS,print);
start([F=[_|_]|FS],nodes)->
    start(F,nodes),start(FS,nodes);
start([F=[_|_]|FS],term)->
    start(F,term)++start(FS,term);
start([_|_]=Filename,Mode)  when (Mode==print) or (Mode==term) or (Mode==nodes)->
    Node=?Query:exec(?File:find(Filename)),
    if
        length(Node)/=1 ->
            io:format("File not found: ~p~n",[Filename]),
            [];
        true ->
            ets:new(deadcodes,[bag,named_table]),
            ets:insert(deadcodes,{mode,Mode}),
            leaves(hd(Node),[]),
            DCList=ets:foldl(fun({mode,_},Acc) -> Acc; (E, Acc) -> [E|Acc] end,[],deadcodes),
            ets:delete(deadcodes),
            if (Mode==term) or (Mode==nodes) -> DCList; Mode==print -> ok end
    end.

%% @spec check_interface(list(), list(), atom()) -> ok
%% @doc Start deadcode detection on all files with a given interface.
check_interface(Include,Exclude,Mode) ->
    AllFuns=?Query:exec(?Query:seq(?Mod:all(),?Mod:locals_all())),
    ExportedFuns=lists:filter(fun(F) -> ?Fun:is_exported(F) end, AllFuns),
    SpecialFuns=try
    IncludeList=parse_interface(Include),
    ExcludeList=parse_interface(Exclude),
    InterfaceFuns=lists:filter(fun(F) ->
                check_match(F,IncludeList) and not check_match(F,ExcludeList)
                               end, ExportedFuns),
    ExportedFuns--InterfaceFuns
    catch
        {bad_function_format,E} ->
            io:format("Bad function format: ~p~n",[E]),error;
        {parse_error,S} ->
            io:format("Error parsing: ~p~n",[S]),error;
        {bad_regexp,R} -> io:format("Bad Regexp: ~p~n",[R]),error
    end,
    if
        SpecialFuns==error -> error;
        true ->
            ets:new(deadcodes,[bag,named_table]),
            ets:insert(deadcodes,{mode,Mode}),
            leaves(?Graph:root(),SpecialFuns),
            DCList=ets:foldl(fun({mode,_},Acc) -> Acc; (E, Acc) -> [E|Acc] end,[],deadcodes),
            ets:delete(deadcodes),
            if Mode==term -> DCList; Mode==print -> ok end
    end.

leaves(Top,SFuns) ->
    Nodes = ets:new(visited_nodes, [set]),
    try
        leaves(Top, Nodes,SFuns)
    after
        ets:delete(Nodes)
    end.

leaves(Top, Nodes,SFuns) ->
    case ets:lookup(Nodes, Top) of
        [] ->
            ets:insert(Nodes, {Top}),
            case ?Syn:children(Top) of
                [] ->
                    ok;
                Children ->
                    [check_children(C,Nodes,SFuns) || {_,C} <- Children]
            end;
        _  ->
            ok
    end.

check_children(Node,Nodes,SFuns) ->
    NodeData=?ESG:data(Node),
    Check=case NodeData of
        #expr{type=if_expr} -> check_if(Node),true;
        #expr{type=case_expr} -> check_case(Node),true;
        #expr{type=receive_expr} -> check_receive(Node),true;
        #expr{type=try_expr} -> check_catch(Node),true;
        #form{type=func} -> check_func(Node,SFuns);
        #expr{type=variable} -> check_var(Node),true;
        #form{type=record} -> check_rec(Node),true;
        #form{type=macro} -> check_macro(Node),true;
        _ -> ok,true
    end,
    Check andalso leaves(Node, Nodes,SFuns).

%%% ============================================================================
%%% Checks

check_var(Node) ->
    [{mode,Mode}]=ets:lookup(deadcodes,mode),
    SVar=?Query:exec(Node,[{varbind,1}]),
    if
        SVar/=[] ->
            Unused=hd(?Var:name(hd(SVar)))==$_,
            if
                Unused -> ok;
                true ->
                    Refs=?Query:exec(hd(SVar),?Var:references()),
                    if
                        Refs==[] ->
                            case Mode of
                                nodes ->
                                    ets:insert(deadcodes,{unused_var,Node});
                                _ ->
                                    dc_message(unused_var,Node,?Var:name(hd(SVar)))
                            end;
                        true -> ok
                    end
            end;
        true -> ok
    end.

check_rec(Node) ->
    Rec=?Query:exec1(Node,?Form:record(),err),
    RecRefs=?Query:exec(Rec,?Rec:references()),
    [{mode,Mode}]=ets:lookup(deadcodes,mode),
    if
        RecRefs==[] -> 
            case Mode of
                nodes ->
                    ets:insert(deadcodes,{unused_rec,Node});
                _ ->
                    dc_message(unused_rec,Node,?Rec:name(Rec))
            end;
        true -> ok
    end,
    RecFields=?Query:exec(Rec,?Rec:fields()),
    lists:map(fun(Field) ->
                  RecFieldRefs=?Query:exec(Field,?RecField:references()),
                  if
                      RecFieldRefs==[] ->
                        case Mode of
                            nodes ->
                                ets:insert(deadcodes,{unused_recfield,Field});
                            _ ->
                                dc_message(unused_recfield,Node,?RecField:name(Field))
                        end;
                      true -> ok
                  end
              end, RecFields).

check_macro(Node) ->
    Refs=?Query:exec(Node,[{mref,back}]),
    [{mode,Mode}]=ets:lookup(deadcodes,mode),
    if
        Refs==[] ->
            case Mode of
                nodes ->
                    ets:insert(deadcodes,{unused_macro,Node});
                _ ->
                    ?Query:exec(Node,[{mref,back}]),
                    dc_message(unused_macro,Node,(?ESG:data(Node))#form.tag)
            end;
        true -> ok
    end.

%TODO: include function params in origin
check_if(Node) ->
    Guards=?Query:exec(Node,?Expr:clauses()),
    Exprs=?Query:exec(Guards,?Query:seq([?Clause:guard(),?Expr:deep_sub()])),
    VarsExprs=lists:filter(fun(X) -> ?Expr:type(X)==variable end,Exprs),
    VarNames=lists:map(fun(X)-> {?Expr:value(X),X} end,VarsExprs),
    Vars=lists:ukeysort(1, VarNames),
    VarValues=[{N,S} || {N,V}<-Vars, (S=lists:usort([parse_structure(X,[]) ||
        X<-?Dataflow:reach([V], [back], true)]))/=[]],
    check_guards(Guards,{VarValues,{value,false}}).

check_case(Node) ->
    Patterns=?Query:exec(Node,?Expr:clauses()),
    check_patterns(tl(Patterns),
        ?Query:exec1(hd(Patterns),?Clause:body(),err),[]).

%TODO: explcl/catchcl, after
check_catch(Node) ->
    Patterns=?Query:exec(Node,?Expr:clauses()),
    check_patterns(tl(Patterns),[],[]).

check_receive(Node) ->
    Patterns=?Query:exec(Node,?Expr:clauses()),
    Type=?Clause:type(lists:last(Patterns)),
    if
        Type==timeout ->
            check_patterns(lists:delete(lists:last(Patterns),Patterns),[],[]);
        true -> check_patterns(Patterns,[],[])
    end.

% TODO: check cyclical function references
% check dynamic functions if special
% unused parameters
check_func(Node,SFuns) ->
    [{mode,Mode}]=ets:lookup(deadcodes,mode),
    Funnode=hd(?Query:exec(Node,?Form:func())),
    Special=lists:member(Funnode,SFuns),
    Exported=?Fun:is_exported(Funnode) and not Special,
    if
        Exported -> check_pointless_code(Node),true;
        not Exported ->
            Refs=?Query:exec(Funnode,?Query:all([?Fun:applications(),
                                                        ?Fun:implicits()])),
            if
                Refs==[] -> 
                    case Mode of
                        nodes ->
                            ets:insert(deadcodes,{unused_func,Node});
                        _ ->
                            dc_message(unused_func,Node,?Fun:name(Funnode))
                    end,
                    false;
                true -> check_pointless_code(Node),true
            end
    end.

% TODO: blocks in list comprehensors
% Macros get detected usually
% _ -> ok
% non-dirty functions?
check_pointless_code(Fun) ->
    [{mode,Mode}]=ets:lookup(deadcodes,mode),
    Rets=?Query:exec(?Fun:return_points(?Query:exec(Fun,?Form:func()))),
    Exprs=?Query:exec(Fun,?Query:seq([?Form:clauses(),?Clause:body()])),
    AllExprs=lists:flatten(lists:map(fun(E) ->
        IsBranch=is_branch_expr(E),
        if IsBranch ->
            ?Query:exec(E,?Query:seq([?Expr:clauses(),?Clause:body()]));
            true -> E
        end
        end,Exprs)),
    Dirties=lists:filter(fun(E) ->?Expr:has_side_effect(E) end,AllExprs),
    Importants=get_important_exprs(lists:usort(Rets++Dirties),[]),
    Unused=AllExprs--Importants,
    case Mode of
        nodes ->
            [ets:insert(deadcodes,{unused_expr,U}) || U <- Unused];
        _ ->
    	    [dc_message(unused_expr,U,lists:flatten(?Syn:tree_text(U))) || U<-Unused]
    end.

get_important_exprs([],Sum) -> Sum;
get_important_exprs([E|ES],Sum) ->
    Vars=?Query:exec(E,?Expr:variables()),
    NotSameVars=lists:filter(fun(V) ->
        (hd(?Query:exec(V,?Query:seq([?Var:bindings(),?Expr:top()])))/=E) and
        (not lists:member(
            hd(?Query:exec(V,?Query:seq([?Var:bindings(),?Expr:top()]))),Sum))
                             end,Vars),
    get_important_exprs(ES,Sum++[E]++
        get_important_exprs(
            [hd(?Query:exec(V,?Query:seq([?Var:bindings(),?Expr:top()]))) ||
                V<-NotSameVars],[])++
        get_important_exprs(used_branch(E,Sum),[])).

% TODO: try,receive
used_branch(Expr,S) ->
    Top=hd(?Query:exec(Expr,?Expr:clause())),
    Type=?Clause:type(Top),
    Result=if
        Type==guard -> ?Query:exec(Top,?Clause:guard());
        Type==pattern -> ?Query:exec(Top,?Clause:patterns())++
            ?Query:exec(Top,?Query:seq([[{exprcl,back}],[headcl],[body]]));
        true -> []
    end,
    lists:filter(fun(E) -> (E/=Expr) and (not lists:member(E,S)) end,Result).

is_branch_expr(Expr) ->
    Type=?Expr:type(Expr),
    case Type of
        if_expr -> true;
        case_expr -> true;
        _ -> false
    end.


%%% ============================================================================
%%% Guards

check_guard_allvalues(Guard,V) ->
    Perms=get_perms(V),
    Fun=fun(X) -> case try eval_guard(Guard,X) catch badarg -> error end of
                      {value,Y} when Y/=true -> false;
                      error -> false;
                      {value,true} -> true;
                      _ -> unknown
                  end end,
    AllFalse=lists:all(fun(X)->Fun(X)==false end,Perms),
    if
        AllFalse==true -> {value,false};
        true ->
            AllTrue=lists:all(fun(X)->Fun(X)==true end,Perms),
            if
                AllTrue==true -> {value,true};
                true -> eval_guard(Guard,[])
            end
    end.

%TODO: types, intervals, bitstrings
check_guards([],_) -> ok;
check_guards([Guard|GS],{V,GuardUnion}) ->
    [{mode,Mode}]=ets:lookup(deadcodes,mode),
    AbsGuard=check_guard_allvalues(?Query:exec1(Guard,?Clause:guard(),err),V),
    SimpGuard=simplify_guard(AbsGuard),
    case SimpGuard of
        {value,X} when X/=true ->
            case Mode of
                nodes ->
                    ets:insert(deadcodes,{unused_guard,Guard});
                _ ->
                    dc_message(false_guard,Guard,lists:flatten(?Syn:tree_text(
                        ?Query:exec1(Guard,?Clause:guard(),err))))
            end,
            check_guards(GS,{V,GuardUnion});
        error ->
            case Mode of
                nodes ->
                    ets:insert(deadcodes,{unused_guard,Guard});
                _ ->
                    dc_message(error_guard,Guard,lists:flatten(?Syn:tree_text(
                        ?Query:exec1(Guard,?Clause:guard(),err))))
                end,
            check_guards(GS,{V,GuardUnion});
        _ ->
            SimpGuardUnion=simplify_guard({'or',GuardUnion,SimpGuard}),
            if
                SimpGuardUnion=={value,true} ->
                    case Mode of
                        nodes ->
                            [ets:insert(deadcodes,{unused_guard,G}) || G <- GS];
                        _ ->
                            [dc_message(true_guard,G,lists:flatten(?Syn:tree_text(
                                ?Query:exec1(G,?Clause:guard(),err)))) || G <- GS]
                    end;
                true -> check_guards(GS,{V,SimpGuardUnion})
            end
    end.

%TODO: xor bnot div rem band bor bxor bsl bsr
eval_guard([],_) -> {value,true};
eval_guard([Node],V) -> eval_guard(Node,?ESG:data(Node),V);
eval_guard(Node,V) -> eval_guard(Node,?ESG:data(Node),V).
eval_guard(_,#expr{value=true},_) -> {value,true};
eval_guard(_,#expr{value=false},_) -> {value,false};
eval_guard(_,#expr{type=integer}=Expr,_) -> {value,Expr#expr.value};
eval_guard(_,#expr{type=float}=Expr,_) -> {value,Expr#expr.value};
eval_guard(_,#expr{type=atom}=Expr,_) -> {value,Expr#expr.value};
eval_guard(_,#expr{type=char}=Expr,_) -> {value,Expr#expr.value};
eval_guard(_,#expr{type=string}=Expr,_) -> {value,Expr#expr.value};
eval_guard(_,#expr{type=variable}=Expr,V) ->
    case lists:keyfind(Expr#expr.value,1,V) of
        false -> {var,Expr#expr.value};
        {_,{var,_}} -> {var,Expr#expr.value};
        {_,Val} -> {value,Val}
    end;
eval_guard(Node,#expr{type=cons},V) -> {value,parse_structure(Node,V)};
eval_guard(Node,#expr{type=tuple},V) -> {value,parse_structure(Node,V)};
eval_guard(Node,#expr{value='=='},V) -> op_helper(Node,'==',fun(V1,V2) -> V1==V2 end,V);
eval_guard(Node,#expr{value='/='},V) -> op_helper(Node,'/=',fun(V1,V2) -> V1/=V2 end,V);
eval_guard(Node,#expr{value='<'},V) -> op_helper(Node,'<',fun(V1,V2) -> V1<V2 end,V);
eval_guard(Node,#expr{value='>'},V) -> op_helper(Node,'>',fun(V1,V2) -> V1>V2 end,V);
eval_guard(Node,#expr{value='>='},V) -> op_helper(Node,'>=',fun(V1,V2) -> V1>=V2 end,V);
eval_guard(Node,#expr{value='=<'},V) -> op_helper(Node,'=<',fun(V1,V2) -> V1=<V2 end,V);
eval_guard(Node,#expr{value='=:='},V) -> op_helper(Node,'=:=',fun(V1,V2) -> V1=:=V2 end,V);
eval_guard(Node,#expr{value='=/='},V) -> op_helper(Node,'=/=',fun(V1,V2) -> V1=/=V2 end,V);
eval_guard(Node,#expr{value='and'},V) -> op_helper(Node,'and',fun(V1,V2) -> V1 and V2 end,V);
eval_guard(Node,#expr{value='or'},V) -> op_helper(Node,'or',fun(V1,V2) -> V1 or V2 end,V);
eval_guard(Node,#expr{value='not'},V) -> op_helper(Node,'not',fun(V1) -> not V1 end,V);
eval_guard(Node,#expr{value='orelse'},V) -> op_helper(Node,'orelse',fun(V1,V2) -> V1 orelse V2 end,V);
eval_guard(Node,#expr{value='andalso'},V) -> op_helper(Node,'andalso',fun(V1,V2) -> V1 andalso V2 end,V);
eval_guard(Node,#expr{value=';'},V) -> op_helper(Node,'orelse',fun(V1,V2) -> V1 orelse V2 end,V);
eval_guard(Node,#expr{value=','},V) -> op_helper(Node,'andalso',fun(V1,V2) -> V1 andalso V2 end,V);
eval_guard(Node,#expr{type=prefix_expr,value='+'},V) -> op_helper(Node,'+',fun(V1) -> V1 end,V);
eval_guard(Node,#expr{type=prefix_expr,value='-'},V) -> op_helper(Node,'-',fun(V1) -> -V1 end,V);
eval_guard(Node,#expr{type=infix_expr,value='+'},V) -> op_helper(Node,'+',fun(V1,V2) -> V1+V2 end,V);
eval_guard(Node,#expr{type=infix_expr,value='-'},V) -> op_helper(Node,'-',fun(V1,V2) -> V1-V2 end,V);
eval_guard(Node,#expr{value='*'},V) -> op_helper(Node,'*',fun(V1,V2) -> V1*V2 end,V);
eval_guard(Node,#expr{value='/'},V) -> op_helper(Node,'*',fun(V1,V2) -> V1/V2 end,V);
eval_guard(Node,#expr{type=parenthesis},V) -> eval_guard(?Query:exec1(Node,?Expr:child(1),err),V);
eval_guard(Node,#expr{type=application},V) -> appl_helper(Node,V);
eval_guard(Node,#expr{type=record_access},V) ->
    case eval_guard(?Query:exec1(Node,?Expr:child(1),err),V) of
        {var,Var} -> {var,"rec"++Var};
        {value,Val} ->
            FieldName=?Expr:value(?Query:exec1(Node,?Expr:child(2),err)),
            RecName=?Expr:value(Node),
            FieldNames=lists:map(fun(F) -> ?RecField:name(F) end,
                ?Query:exec(Node,?Query:seq([?Expr:record(),?Rec:fields()]))),
            if
                RecName/=element(1,Val); length(Val)/=length(FieldNames)+1 ->
                    throw(badarg);
                true ->
                    {_,Num}=lists:keyfind(FieldName,1,
                        lists:zip(FieldNames,lists:seq(1,length(FieldNames)))),
                    {value,element(Num+1,Val)}
            end
    end;
eval_guard(_,Notimpl,_) -> ?d({"NOT IMPLEMENTED",Notimpl}).

%TODO: only use explicit structures when necessary
parse_structure(undefined,_) -> undefined;
parse_structure(Node,V) ->
    NodeData=?ESG:data(Node),
    case NodeData of
        #expr{type=cons} ->
            List=?Query:exec(Node,
                ?Query:seq([?Expr:child(1),?Expr:children()])),
            [parse_structure(L,V) || L<-List];
        #expr{type=tuple} ->
            List=?Query:exec(Node,?Expr:children()),
            list_to_tuple([parse_structure(L,V) || L<-List]);
        #expr{type=record_expr} ->
            Rec=convert_record(Node,undefined),
            list_to_tuple([(hd(Rec))#expr.value|[parse_structure(L,V) ||
                L<- tl(Rec)]]);
        #expr{type=variable} ->
            case lists:keyfind(NodeData#expr.value,1,V) of
                false -> {var,NodeData#expr.value};
                {_,{var,Var}} -> {var,Var};
                {_,Val} -> Val
            end;
        #expr{type=joker} -> {var,"_"};
        #expr{type=integer} -> NodeData#expr.value;
        #expr{type=float} -> NodeData#expr.value;
        #expr{type=atom} -> NodeData#expr.value;
        #expr{type=char} -> NodeData#expr.value;
        #expr{type=string} -> NodeData#expr.value;
        _ -> {var,"0"}
    end.

op_helper(Node,Atom,Fun,V) when is_function(Fun,1)->
    O=?Query:exec1(Node,?Expr:child(1),err),
    case eval_guard(O,V) of
        {value,Val} -> {value,Fun(Val)};
        Val -> {Atom,Val}
    end;
op_helper(Node,Atom,Fun,V) when is_function(Fun,2) ->
    O1=?Query:exec1(Node,?Query:any([?Expr:child(1),
        ?Query:seq([?Expr:clause(1),?Clause:body()])]),err),
    O2=?Query:exec1(Node,?Query:any([?Expr:child(2),
        ?Query:seq([?Expr:clause(2),?Clause:body()])]),err),
    if
        Atom=='orelse' ->
            case simplify_guard(eval_guard(O1,V)) of
                {value,true} -> {value,true};
                _ -> op_helper2(O1,O2,Atom,Fun,V)
            end;
        Atom=='andalso' ->
            case simplify_guard(eval_guard(O1,V)) of
                {value,X} when X/=true -> {value,false};
                _ -> op_helper2(O1,O2,Atom,Fun,V)
            end;
        true -> op_helper2(O1,O2,Atom,Fun,V)
    end.

op_helper2(O1,O2,Atom,Fun,V) ->
    case {eval_guard(O1,V),eval_guard(O2,V)} of
        {{value,V1},{value,V2}} -> {value,Fun(V1,V2)};
        {{var,V1},{var,V1}} -> {value,Fun(V1,V1)};
        {V1,V2} -> {Atom,V1,V2}
    end.

appl_helper(Node,V) ->
    Fun=(?ESG:data(?Query:exec1(Node,?Expr:child(1),err)))#expr.value,
    Args=?Query:exec(Node,?Query:seq([?Expr:child(2),?Expr:children()])),
    Res=case length(Args) of
        0 -> {value, erlang:Fun()};
        1 ->
            [A1]=Args,
            case eval_guard(A1,V) of
                {value,V1} ->
                    {value,try erlang:Fun(V1) catch _:_->throw(badarg) end};
                V1 -> {appl,Fun,V1}
            end;
        2 ->
            [A1,A2]=Args,
            case {eval_guard(A1,V),eval_guard(A2,V)} of
                {{value,V1},{value,V2}} ->
                    {value,try erlang:Fun(V1,V2) catch _:_->throw(badarg) end};
                {V1,V2} -> {appl,Fun,[V1,V2]}
            end;
        3 ->
            [A1,A2,A3]=Args,
            case {eval_guard(A1,V),eval_guard(A2,V),eval_guard(A3,V)} of
                {{value,V1},{value,V2},{value,V3}} ->
                    {value,try erlang:Fun(V1,V2,V3)catch _:_->throw(badarg)end};
                {V1,V2,V3} -> {appl,Fun,[V1,V2,V3]}
            end
    end,
    case Res of
        {value,{var,Var}} -> {var,Var};
        _ -> Res
    end.

simplify_guard({'not',{value,true}}) -> {value,false};
simplify_guard({'not',{value,false}}) -> {value,true};
simplify_guard({'not',Op1}=Orig) ->
    NewOp1=simplify_guard(Op1),
    if
        NewOp1==Op1 -> Orig;
        true -> simplify_guard({'not',NewOp1})
    end;
simplify_guard({'or',{value,true},_}) -> {value,true};
simplify_guard({'or',_,{value,true}}) -> {value,true};
simplify_guard({'or',{value,false},X}) -> X;
simplify_guard({'or',X,{value,false}}) -> X;
simplify_guard({'or',X,{'not',X}}) -> {value,true};
simplify_guard({'or',{'not',X},X}) -> {value,true};
simplify_guard({'or',X,X}) -> X;
simplify_guard({'or',{appl,F1,X},{appl,F2,X}}=G) when F1/=F2 ->
    case {F1,F2} of %TODO: is_record/is_tuple
        {is_number,is_integer} -> {appl,F1,X};
        {is_number,is_float} -> {appl,F1,X};
        {is_float,is_number} -> {appl,F2,X};
        {is_integer,is_number} -> {appl,F2,X};
        {is_atom,is_boolean} -> {appl,F1,X};
        {is_boolean,is_atom} -> {appl,F2,X};
        {is_bitstring,is_binary} -> {appl,F1,X};
        {is_binary,is_bitstring} -> {appl,F2,X};
        _ -> G
    end;
simplify_guard({'or',Op1,Op2}=Orig) ->
    NewOp1=simplify_guard(Op1),
    NewOp2=simplify_guard(Op2),
    if
        (NewOp1==Op1) and (NewOp2==Op2) -> Orig;
        true -> simplify_guard({'or',NewOp1,NewOp2})
    end;
simplify_guard({'and',{value,false},_}) -> {value,false};
simplify_guard({'and',_,{value,false}}) -> {value,false};
simplify_guard({'and',{value,true},X}) -> X;
simplify_guard({'and',X,{value,true}}) -> X;
simplify_guard({'and',X,{'not',X}}) -> {value,false};
simplify_guard({'and',{'not',X},X}) -> {value,false};
simplify_guard({'and',X,X}) -> X;
simplify_guard({'and',{appl,F1,X},{appl,F2,X}}) when F1/=F2 ->
    case {F1,F2} of
        {is_number,is_integer} -> {appl,F2,X};
        {is_number,is_float} -> {appl,F2,X};
        {is_float,is_number} -> {appl,F1,X};
        {is_integer,is_number} -> {appl,F1,X};
        {is_atom,is_boolean} -> {appl,F2,X};
        {is_boolean,is_atom} -> {appl,F1,X};
        {is_bitstring,is_binary} -> {appl,F2,X};
        {is_binary,is_bitstring} -> {appl,F1,X};
        _ -> {value,false}
    end;
simplify_guard({'and',Op1,Op2}=Orig) ->
    NewOp1=simplify_guard(Op1),
    NewOp2=simplify_guard(Op2),
    if
        (NewOp1==Op1) and (NewOp2==Op2) -> Orig;
        true -> simplify_guard({'and',NewOp1,NewOp2})
    end;
simplify_guard(G) -> G.

%%% ============================================================================
%%% Pattern matching

%TODO: dataflow, bitstrings
check_patterns([],_,_) -> ok;
check_patterns([Pattern|Patterns],M,Prevs) ->
    [{mode,Mode}]=ets:lookup(deadcodes,mode),
    P=?Query:exec(Pattern,?Clause:patterns()),
    G=case ?Query:exec(Pattern,?Clause:guard()) of
        [] -> [];
        [A] -> A
    end,
    SimpGuard=simplify_guard(eval_guard(G,[])),
    case SimpGuard of
        {value,X} when X/=true ->
            case Mode of
                nodes ->
                    ets:insert(deadcodes,{unused_pattern,hd(P)});
                _ ->
                    dc_message(false_pattern_guard,hd(P),lists:flatten(?Syn:tree_text(hd(P))))
            end,
            check_patterns(Patterns,M,[{P,SimpGuard}|Prevs]);
        _ ->
            % @todo: it seems to be an error?
            % hook_for_latter_comment
            % HeadMatch=if M==[] -> true; true -> true end,%match(P,M) end,
            % if
            %     HeadMatch==false ->
            %         dc_message(head_mismatch,hd(P),
            %             lists:flatten(?Syn:tree_text(hd(P)))),
            %         check_patterns(Patterns,M,Prevs);
            %     true ->
                    Guards=lists:map(fun({Pi,Grd}) -> case match(P,Pi) of
                        {true,Eq} -> subst_vars(Grd,Eq);
                        false -> false
                    end end, Prevs),
                    GuardUnion=simplify_guard(
                        lists:foldl(fun(X,Acc) -> {'or',Acc,X} end,
                            {value,false},Guards)),
                    if
                        GuardUnion=={value,true} ->
                            case Mode of
                                nodes ->
                                    ets:insert(deadcodes,{unused_pattern,hd(P)});
                                _ ->
                                    dc_message(pattern_nevermatch,hd(P),
                                    tl(lists:foldl(fun(X,Acc) -> Acc++","++
                                    lists:flatten(?Syn:tree_text(X)) end,[],P))++
                                    (if G==[] -> ""; true -> " when"++
                                    lists:flatten(?Syn:tree_text(G)) end))
                             end;
                        true -> ok
                    end,
                    check_patterns(Patterns,M,[{P,SimpGuard}|Prevs])
            % end
    end.

%TODO: anything in head
match(P1,P2) -> match(P1,P2,[]).
match([],[],Eq) -> {true,Eq};
match([P1|P1S],[P2|P2S],Eq) ->
    case match(P1,P2,Eq) of
        false -> false;
        {true,ResEq} -> match(P1S,P2S,ResEq)
    end;
match(P1={'$gn',_,_},P2={'$gn',_,_},Eq) ->
    match(P1,P2,?ESG:data(P1),?ESG:data(P2),Eq);
match(P1,P2,Eq) -> match(P1,P2,P1,P2,Eq).
match(P1,P2,_,#expr{type=match_expr},Eq) ->
    Elems=?Query:exec(P2,?Expr:children()),
    case match(P1,hd(Elems),Eq) of
        false -> false;
        {true,ResEq} -> match(P1,hd(tl(Elems)),ResEq)
    end;
match(P1,P2,#expr{type=infix_expr},#expr{type=infix_expr},Eq) ->
    Elems1=?Query:exec(P1,?Expr:children()),
    Elems2=?Query:exec(P2,?Expr:children()),
    case match(hd(Elems1),hd(Elems2),Eq) of
        false -> false;
        {true,ResEq} -> match(hd(tl(Elems1)),hd(tl(Elems2)),ResEq)
    end;
match(_,_,#expr{type=infix_expr},_,_) -> false;
match(P1,P2,#expr{type=match_expr},_,Eq) ->
    Elems=?Query:exec(P1,?Expr:children()),
    case match(hd(Elems),hd(tl(Elems)),[]) of
        false -> match(hd(tl(Elems)),P2,Eq);
        {true,_} -> match(hd(Elems),P2,Eq) %TODO: keep equivalence
    end;
match(P1,P2,_,#expr{type=variable},Eq) ->
    Binded=(?Query:exec(P2,?Expr:varbinds()))==[],
    if
        Binded==true -> false;
        true ->
            S1=parse_structure(P1,[]),
            S2=parse_structure(P2,[]),
            if
                S2=={var,"_"} -> {true,Eq};
                true ->
                    case lists:keyfind(S2,1,Eq) of
                        %TODO: match structure elements
                        false -> {true,[{S2,S1}|Eq]};
                        {_,{var,"_"}} -> false;
                        {S2,S1} -> {true,Eq};
                        _ -> false
                    end
            end
        end;
match(_,_,_,#expr{type=joker},Eq) -> {true,Eq};
match(P1,P2,#expr{type=tuple},#expr{type=tuple},Eq) ->
    Elems1=?Query:exec(P1,?Expr:children()),
    Elems2=?Query:exec(P2,?Expr:children()),
    if
        length(Elems1)/=length(Elems2) -> false;
        true -> match_tuple(Elems1,Elems2,Eq)
    end;
match(P1,P2,#expr{type=cons},#expr{type=cons},Eq) ->
    Elems1=?Query:exec(P1,?Expr:children()),
    Elems2=?Query:exec(P2,?Expr:children()),
    {List1,Tail1}=if
        length(Elems1)==0 -> {[],[]};
        length(Elems1)==1 -> {?Query:exec(hd(Elems1),?Expr:children()),[]};
        length(Elems1)==2 ->
            {?Query:exec(hd(Elems1),?Expr:children()),hd(tl(Elems1))}
    end,
    {List2,Tail2}=if
        length(Elems2)==0 -> {[],[]};
        length(Elems2)==1 -> {?Query:exec(hd(Elems2),?Expr:children()),[]};
        length(Elems2)==2 ->
            {?Query:exec(hd(Elems2),?Expr:children()),hd(tl(Elems2))}
    end,
    if
        (Tail1==[]) and (Tail2==[]) and (length(List1)/=length(List2)) -> false;
        true -> match_list(List1,List2,Tail1,Tail2,Eq)
    end;
%TODO: record+tuple
match(P1,P2,#expr{type=record_expr},#expr{type=record_expr},Eq) ->
    match_tuple(convert_record(P1,#expr{type=joker}),
        convert_record(P2,#expr{type=joker}),Eq);
match(_,_,#expr{type=joker},_,_) -> false;
match(_,_,#expr{value=V1},#expr{value=V2},Eq) when (V1==V2) and
    (V1/=undefined) and (V2/=undefined) -> {true,Eq};
match(_,_,_,_,_) -> false.

match_tuple([],[],Eq) -> {true,Eq};
match_tuple([E1|E1S],[E2|E2S],Eq) ->
    case match(E1,E2,Eq) of
        false -> false;
        {true,ResEq} -> match_tuple(E1S,E2S,ResEq)
    end.

%TODO: match tails better
match_list([],[],[],[],Eq) -> {true,Eq};
match_list([],[],_,[],_) -> false;
match_list([],[],[],_,Eq) -> {true,Eq};
match_list([],[],T1,T2,Eq) -> match(T1,T2,Eq);
match_list([],[_|_],[],_,_) -> false;
match_list([],[_|_],_,[],_) -> false;
match_list([],[_|_],_,_,Eq) -> {true,Eq};
match_list([_|_],[],[],_,Eq) -> {true,Eq};
match_list([_|_],[],_,[],_) -> false;
match_list([_|_],[],_,_,Eq) -> {true,Eq};
match_list([E1|E1S],[E2|E2S],T1,T2,Eq) ->
    case match(E1,E2,Eq) of
        false -> false;
        {true,ResEq} -> match_list(E1S,E2S,T1,T2,ResEq)
    end.

subst_vars({var,V},Eq) ->
    case lists:keyfind({var,V},1,Eq) of
        {_,X} -> X;
        false -> {var,V}
    end;
subst_vars(T,Eq) when is_tuple(T) ->
    list_to_tuple(subst_vars(tuple_to_list(T),Eq));
subst_vars([L|LS],Eq) -> [subst_vars(L,Eq)|subst_vars(LS,Eq)];
subst_vars(X,_) -> X.

convert_record(Node,Undefineds) ->
    Record=?Query:exec1(Node,?Expr:record(),err),
    RecordName=?Expr:value(Node),
    Fields=?Query:exec(Record,?Rec:fields()),
    FieldNames=lists:map(fun(X)-> ?RecField:name(X) end,Fields),
    FieldNodes=?Query:exec(Node,
        ?Query:seq([?Expr:children(),?Expr:children()])),
    FieldExprs=lists:map(fun(X)->
        {?Expr:value(X),?Query:exec1(X,?Expr:children(),err)} end,FieldNodes),
        [#expr{type=atom,value=RecordName}|[
            case lists:keyfind(Name,1,FieldExprs) of
                false -> Undefineds;
                {_,E} -> E
            end || Name<-FieldNames]].

%%% ============================================================================
%%% Interface checker functions

parse_interface([]) -> [];
parse_interface([E|ES]) ->
    Result=re:split(E,"([:/])",[{return,list},group]),
    {Modules,Functions,Arities}=case Result of
        [[Module,":"],[Function,"/"],[Arity]] when length(Module)>0,
                length(Function)>0, length(Arity)>0 ->
            {parse_string(Module),parse_string(Function),parse_num(Arity,E)};
        [[Module,":"],[Function]] when length(Module)>0, length(Function)>0 ->
            {parse_string(Module),parse_string(Function),'.*'};
        [[Module]] when length(Module)>0 ->
            {parse_string(Module),'.*','.*'};
        _ ->
            throw({bad_function_format,E})
    end,
    [{Modules,Functions,Arities}|parse_interface(ES)].

parse_num(N,E) ->
    Parsed=parse_string(N),
    if
        is_list(Parsed) ->
            AllInt=lists:all(fun(I) -> is_integer(I) end,Parsed),
            if
                AllInt -> Parsed;
                true -> throw({bad_function_format,E})
            end;
        not is_integer(Parsed) -> throw({bad_function_format,E});
        true -> Parsed
    end.

parse_string("*") -> '.*';
parse_string(S) ->
    try
        {ok,Scanned,_} = erl_scan:string(S++"."),
        {ok,Parsed} = erl_parse:parse_term(Scanned),
        Parsed
    catch
        _:_ -> throw({parse_error,S})
    end.

check_match(_,[]) -> false;
check_match(Fun,[{M,F,A}|ES]) ->
    {_,{Module,Function,Arity}}=?Fun:mod_fun_arity(Fun),
    ModuleMatch=if
        M=='.*' -> true;
        is_list(M) -> lists:any(fun(E) -> fullmatch(Module,E) end,M);
        is_atom(M) -> fullmatch(Module,M)
    end,
    FuncMatch=if
        F=='.*' -> true;
        is_list(F) -> lists:any(fun(E) -> fullmatch(Function,E) end,F);
        is_atom(F) -> fullmatch(Function,F)
    end,
    ArityMatch=if
        A=='.*' -> true;
        is_list(A) -> lists:any(fun(E) -> Arity==E end,A);
        is_integer(A) -> Arity==A
    end,
    if
        ModuleMatch and FuncMatch and ArityMatch -> true;
        true -> check_match(Fun,ES)
    end.

fullmatch(S,Regexp) ->
    RString=atom_to_list(Regexp),
    MString=atom_to_list(S),
    {X,R}=re:compile(RString),
    if
        X==error -> throw({bad_regexp,RString});
        true -> ok
    end,
    M=re:run(MString,R),
    L=length(MString),
    case M of
        {match,[{0,L}]} -> true;
        _ -> false
    end.

%%% ============================================================================
%%% Messages

dc_message(Type,Node,Text) ->
    [{mode,Mode}]=ets:lookup(deadcodes,mode),
    FirstToken=hd(?Syn:leaves(Node)),
    File=filename:basename(
        ?File:path(hd(?Query:exec(FirstToken,?Token:file())))),
    {{Line,_},{_,_}}=?Token:linecol(FirstToken),
    case Mode of
        nodes ->
            ok;
        _ ->
            ets:insert(deadcodes,{Type,trim(Text),File,Line})
    end,
    Mode==print andalso dc_message(Type,trim(Text),File,Line).
dc_message(error_guard,Text,File,Line) ->
    io:format("~s:~p: The guard \"~s\" will never match because it always throws a badarg exception.~n",[File,Line,Text]);
dc_message(false_guard,Text,File,Line) ->
    io:format("~s:~p: The guard \"~s\" will never match.~n",[File,Line,Text]);
dc_message(true_guard,Text,File,Line) ->
    io:format("~s:~p: The guard \"~s\" will never match because the union of previous guards always matches.~n",[File,Line,Text]);
dc_message(false_pattern_guard,Text,File,Line) ->
    io:format("~s:~p: The pattern \"~s\" will never match because it's guard is always false.~n",[File,Line,Text]);
% @todo: after the event has been investigated (search for hook_for_latter_comment) this clause becomes necessary
% dc_message(head_mismatch,Text,File,Line) ->
%     io:format("~s:~p: The pattern \"~s\" will never match because the case head expression doesn't match it.~n",[File,Line,Text]);
dc_message(pattern_nevermatch,Text,File,Line) ->
    io:format("~s:~p: The pattern \"~s\" will never match because previous patterns always do.~n",[File,Line,Text]);
dc_message(unused_expr,Text,File,Line) ->
    io:format("~s:~p: The expression \"~s\" is probably unnecessary.~n",[File,Line,Text]);
dc_message(unused_func,Text,File,Line) ->
    io:format("~s:~p: The function \"~s\" is unused.~n",[File,Line,Text]);
dc_message(unused_var,Text,File,Line) ->
    io:format("~s:~p: The variable \"~s\" is unused.~n",[File,Line,Text]);
dc_message(unused_rec,Text,File,Line) ->
    io:format("~s:~p: The record \"~s\" is unused.~n",[File,Line,Text]);
dc_message(unused_recfield,Text,File,Line) ->
    io:format("~s:~p: The record field \"~s\" is unused.~n",[File,Line,Text]);
dc_message(unused_macro,Text,File,Line) ->
    io:format("~s:~p: The macro \"~s\" is unused.~n",[File,Line,Text]).

trim(Text) -> trim(Text,false).
trim([],_) -> [];
trim([$ |CS],Comm) -> trim(CS,Comm);
trim([$\t|CS],Comm) -> trim(CS,Comm);
trim([$\n|CS],Comm) when Comm -> trim(CS,false);
trim([$\n|CS],_) -> trim(CS,false);
trim([$%|CS],_) -> trim(CS,true);
trim([_|CS],Comm) when Comm -> trim(CS,true);
trim(Text,_) -> Text.

get_perms(List) -> get_perms(List,lists:duplicate(length(List),1)).
get_perms(List,Indexes) ->
    NextIndexes=inc_indexes(List,Indexes),
    if
        length(NextIndexes)/=length(List) -> [perm_elem(List,Indexes,1)];
        true -> [perm_elem(List,Indexes,1)|get_perms(List,NextIndexes)]
    end.

perm_elem(_,[],_) -> [];
perm_elem(List,[I|Indexes],Cur) ->
    {V,CurList}=lists:nth(Cur,List),
    [{V,lists:nth(I,CurList)}|perm_elem(List,Indexes,Cur+1)].

inc_indexes([],_) -> [done];
inc_indexes([{_,L}|LS],[_|IS]=Indexes) when hd(Indexes)==length(L) ->
    [1|inc_indexes(LS,IS)];
inc_indexes(_,[I|IS]) ->
    [I+1|IS].
