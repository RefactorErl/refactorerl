%% -*- coding: latin-1 -*-

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

%%% @doc Data flow analyser.
%%%
%%% @author Melinda Toth <toth_m@inf.elte.hu>

-module(refanal_dataflow).
-vsn("$Rev $ ").
-behaviour(refcore_anal).

-export([schema/0, externs/1, insert/4, remove/4, update/2]).

-export([reach/2, reach/3, reach_1st/2, reach_1st/3]).

-export([back_nodes/1, forw_tags/0, back_tags/0]).

-include("core.hrl").

%%% @private
schema() ->
    [{expr,   [{flow, expr},  {dep, expr},
               {call, expr},  {ret, expr},
               {sel, expr},   {cons_back, expr},
               {sel_e, expr}, {cons_e, expr}
              ]},
     {func,   [{fpar, expr}, {fret, expr}]}].
    %% Note: s_i, c_i <-> sel/i, cons_back/i

%%% @private
externs(_) -> [].


remove_app_deps(App, Name, ArgList) ->
    Pars = [Par || {_, Par} <- ?Anal:children(ArgList)],
    Dels =
         case ?Anal:children(Name) of
             []                   -> [Name];
             [{_, Mod},{_, Fun}]  -> [Mod, Fun]
         end,
    safe_link(rmlink, Dels ++ Pars, dep, App),
    {App, ?Anal:parent(App)}.


%%% @private
insert(Parent, Pre, {Tag, Child}, Post) ->
    case ?Anal:data(Parent) of
        #file{} when Tag == form ->
            Apps = get_app(Child),
            AppList = [remove_app_deps(App, Name, ArgList)
                            || App <- Apps,
                               [{_,Name}, {_,ArgList}] <- [?Anal:children(App)],
                               ?Graph:path(Name, [dep]) =/= []],
            walk(fun add_del/4, [{Child, Parent} | AppList], mklink);
        #form{type=func} when Tag == funcl ->
            Apps = get_app(Parent),
            Children = ?Anal:children(Child),
            {_, Last} = lists:last(Children),
            Pats = [Pat || {pattern, Pat} <- Children],
            [begin
                [_, {_, ArgList}] = ?Anal:children(App),
                Pars = [Par || {_, Par} <- ?Anal:children(ArgList)],
                [link(mklink, Par, call, Pat) ||
                    {Pat, Par} <- lists:zip(Pats, Pars)],
                [FRet] = ?Graph:path(Parent, [fundef, fret]),
                link(mklink, Last, flow, FRet),
                link(mklink, FRet,  ret,  App)
             end || App <- Apps],
            walk(fun add_del/4, [{Child, Parent}], mklink);
        #form{} -> ok;
        #clause{type=fundef} when Tag == body ->
            case Post of
                [] ->
                    Form = ?Anal:parent(Parent),
                    Apps = get_app(Form),      
                    [FRet] = ?Graph:path(Parent, [{funcl,back}, fundef, fret]),
                    link(mklink, Child, flow, FRet),
                    link(mklink, FRet,  ret,  Apps),
                    case Pre of
                        [] -> ok;
                        _  -> {_, Last} = lists:last(Pre),
                              safe_link(rmlink, Last, ret, Apps)
                    end;
                _ -> ok
            end,
            walk(fun add_del/4, [{Child, Parent}], mklink);
        #clause{type=fundef} when Tag == pattern ->
            [SemFun] = ?Graph:path(Parent, [{funcl, back}, fundef]),
            #func{arity = Arity} = ?Graph:data(SemFun),
            add_del_param_nodes(mklink, SemFun, [Parent], Arity),
            add_del_return_node(mklink, SemFun, [Parent]),
            walk(fun add_del/4, [{Child, Parent}], mklink);
        #clause{type=fundef} when Tag == name ->
            [SemFun] = ?Graph:path(Parent, [{funcl, back}, fundef]),
            #func{arity = Arity} = ?Graph:data(SemFun),
            add_del_param_nodes(mklink, SemFun, [Parent], Arity),
            add_del_return_node(mklink, SemFun, [Parent]),
            walk(fun add_del/4, [{Child, Parent}], mklink);
        #clause{type=fundef} -> ok;
        #clause{type=pattern} when Tag == pattern -> %% case pattern
            Expr = ?Anal:parent(Parent),
            [{_, HeadCl}| _] = ?Anal:children(Expr),
            [{_, HeadE} | _] = ?Anal:children(HeadCl),
            link(mklink, HeadE, flow, Child),
            walk(fun add_del/4, [{Child, Parent}], mklink);

        #clause{type=Type} when Type == pattern orelse Type == guard orelse
                                Type == timeout orelse Type == block orelse
                                Type == funexpr ->
            case Post of
                [] ->
                    Expr = ?Anal:parent(Parent),
                    link(mklink, Child, flow, Expr),
                    case Pre of
                        [] -> ok;
                        _  -> {_, Last} = lists:last(Pre),
                              safe_link(rmlink, Last, flow, Expr) %%ins vs. repl
                    end;
                _ -> ok
            end,
            walk(fun add_del/4, [{Child, Parent}], mklink);
        #clause{type=expr} -> %% catch
            Expr = ?Anal:parent(Parent),
            case ?Anal:data(Expr) of
                #expr{type=catch_expr} -> link(mklink, Child, flow, Expr);
                #expr{type=case_expr} ->
                    [_| Clauses] = ?Anal:children(Expr),
                    [begin
                      [{_, PatI} | _] = ?Anal:children(ClI),
                      link(mklink, Child, flow, PatI)
                     end || {_, ClI} <- Clauses];
                %% todo andalso, orelse
                _ -> ok
            end,
            walk(fun add_del/4, [{Child, Parent}], mklink);
        #clause{type=hexpr} ->
            ListComp = ?Anal:parent(Parent),
            link(mklink, Child, cons_e, ListComp),
            walk(fun add_del/4, [{Child, Parent}], mklink);
        #clause{type=T} when T==compr orelse T==list_gen orelse T==filter->
            walk(fun add_del/4, [{Child, Parent}], mklink);
        #expr{type=T, value=V} when (T == infix_expr andalso V == ':') orelse
                                     T ==  arglist ->
            %TODO: check infix and ':'
            App = ?Anal:parent(Parent),
            %add_del(rmlink, ?Anal:data(App), App, AppPar),
            app_add(Parent, App),
            walk(fun add_del/4, [{Child, Parent}], mklink);
        #expr{type = application} ->
            App = Parent,
            [_, {_, ArgList}] = ?Anal:children(App),
            %add_del(rmlink, ?Anal:data(App), App, AppPar),
            app_add(ArgList, App),
            walk(fun add_del/4, [{Child, Parent}], mklink);
        #expr{} ->
            %% TODO: this branch could be refined
            Parent2 = ?Anal:parent(Parent),
            add_del(rmlink, ?Anal:data(Parent), Parent, Parent2),
            add_del(mklink, ?Anal:data(Parent), Parent, Parent2),
            walk(fun add_del/4, [{Child, Parent}], mklink)
    end.

app_add(ArgList, App)->
    Pars = [ Ch || {_, Ch} <- ?Anal:children(ArgList)],
    [[?Graph:rmlink(P, call, OldFPar) || 
        OldFPar <- ?Graph:path(P, [{call, {type, '==', fpar}}])] || P <- Pars],
    [?Graph:rmlink(OldFRet, ret, App) || 
        OldFRet <- ?Graph:path(App,[{{ret,back},{type,'==',fret}}])],
    [?Graph:rmlink(Dep, dep, App) || 
        Dep <- ?Graph:path(App,[{dep,back}])],
    add_del(mklink, ?Anal:data(App), App, ?Anal:parent(App)).

add_del(Dir, #expr{type=variable}, Expr, _P) ->
    VarB = ?Graph:path(Expr, [varbind]),
    case VarB of
        [Var] ->
            VarRefs = ?Graph:path(Var, [{varref, back}]),
            safe_link(Dir, Expr, flow, VarRefs);
        [] ->
            case ?Graph:path(Expr, [varref]) of
                [Var] ->
                    VarBs = ?Graph:path(Var, [{varbind, back}]),
                    safe_link(Dir, VarBs, flow, Expr);
                [] -> ok %%io:format("~nError in variable analyzer!~n~n", [])
                %% Todo: Eliminate this branch...
            end
    end,
    [];
add_del(Dir, #expr{type=match_expr, role=pattern}, Expr, _P) ->
    [{_, P1}, {_, P2}] = ?Anal:children(Expr),
    safe_link(Dir, Expr, flow, [P1,P2]),
    [{P1, Expr}, {P2, Expr}];
add_del(Dir, #expr{type=match_expr}, Expr, _P) ->
    [{_, P}, {_, E}] = ?Anal:children(Expr),
    safe_link(Dir, E, flow, [P, Expr]),
    [{P, Expr}, {E, Expr}];
add_del(Dir, #expr{type=infix_expr, value = V}, Expr, _P) when (V == 'andalso')
                                                 orelse  (V == 'orelse') ->
    [{_, C1}, {_, C2}] = ?Anal:children(Expr),
    [{_, E1}] = ?Anal:children(C1),
    [{_, E2}] = ?Anal:children(C2),
    safe_link(Dir, [E1, E2], dep, Expr),
    [{E1, Expr}, {E2, Expr}];
add_del(Dir, #expr{type=infix_expr,value='++'}, Expr, _P) ->
    [{_, E1}, {_, E2}] = ?Anal:children(Expr),
    safe_link(Dir, [E1, E2], flow, Expr),
    [{E1, Expr}, {E2, Expr}];
add_del(Dir, #expr{type=infix_expr}, Expr, _P) ->
    [{_, E1}, {_, E2}] = ?Anal:children(Expr),
    safe_link(Dir, [E1, E2], dep, Expr),
    [{E1, Expr}, {E2, Expr}];
add_del(Dir, #expr{type=prefix_expr}, Expr, _P) ->
    [{_, E1}] = ?Anal:children(Expr),
    safe_link(Dir, E1, dep, Expr),
    [{E1, Expr}];
add_del(Dir, #expr{type=parenthesis, role=pattern}, Expr, _P) ->
    [{_, P}] = ?Anal:children(Expr),
    safe_link(Dir, Expr, flow, P),
    [{P, Expr}];
add_del(Dir, #expr{type=parenthesis}, Expr, _P) ->
    [{_, E}] = ?Anal:children(Expr),
    safe_link(Dir, E, flow, Expr),
    [{E, Expr}];
add_del(Dir, #expr{type=tuple, role=pattern}, Expr, _P) ->
    Children = ?Anal:children(Expr),
    [safe_link(Dir, Expr, sel, P) || {_, P} <- Children],

    [{P, Expr} || {_, P} <- Children];
add_del(Dir, #expr{type=tuple}, Expr, _P) ->
    Children = ?Anal:children(Expr),
    [safe_link(Dir, Expr, cons_back, E) || {_, E} <- Children],
    [{E, Expr} || {_, E} <- Children];
add_del(Dir, #expr{type=cons, role=pattern}, Expr, _P) ->
    Children = ?Anal:children(Expr),
    case Children of
        [] -> [];
        [{_, List} | Cons] ->
            Elements = ?Anal:children(List),
            [safe_link(Dir, Expr, sel_e, P) || {_, P} <- Elements],
            [safe_link(Dir, Expr, flow, P) || {_, P} <- Cons],
            [{P, Expr} || {_, P} <- (Elements ++ Cons)]
    end;
add_del(Dir, #expr{type=cons}, Expr, _P) ->
    Children = ?Anal:children(Expr),
    case Children of
        [] -> [];
        [{_, List} | Cons] ->
            Elements = ?Anal:children(List),
            [safe_link(Dir, E, cons_e, Expr) || {_, E} <- Elements],
            [safe_link(Dir, E, flow, Expr) || {_, E} <- Cons],
            [{E, Expr} || {_, E} <- (Elements ++ Cons)]
    end;
add_del(Dir, #expr{type=case_expr}, Expr, _P) ->
    [{_, HeadCl} | Clauses] = ?Anal:children(Expr),
    [{_, HeadE}] = ?Anal:children(HeadCl),
    List =
        [begin
            List = [{_, PatI} | Rest ] = ?Anal:children(ClI),
            safe_link(Dir, HeadE, flow, PatI),
            safe_link(Dir, element(2, lists:last(Rest)), flow, Expr),
            List
         end || {_, ClI} <- Clauses],
    [{HeadE, Expr} | [{E, Expr} || {_, E} <- lists:flatten(List)]];
add_del(Dir, #expr{type=application}, Expr, _P) ->
    [{_, Name}, {_, ArgList}] = ?Anal:children(Expr),
    Pars = [Par || {_, Par} <- ?Anal:children(ArgList)],
    case {?Graph:path(Expr, [funlref]) ++ ?Graph:path(Expr, [funeref]),
          Dir} of
        {[Fun], mklink} ->
            FPars = ?NodeSync:get_node(fpar, Fun),
            %% case ?Graph:path(Fun, [fpar]) of
            %%     [] -> 
            %%         #func{arity = Arity} = ?Graph:data(Fun),
            %%         FPars = [?Graph:create(#expr{type=fpar}) || 
            %%                      _ <- lists:seq(1, Arity)],
            %%         [?Graph:mklink(Fun, fpar, FPar) || FPar <- FPars];
            %%     FPars -> ok
            %% end,
            FRet = ?NodeSync:get_node(fret, Fun),
            %% case ?Graph:path(Fun, [fret]) of
            %%     [] -> FRets = [?Graph:create(#expr{type=fret})],
            %%           [?Graph:mklink(Fun, fret, FRet) || FRet <- FRets];
            %%     FRets -> ok
            %% end,
            [safe_link(Dir, From, call, To) ||
                {From, To} <- safe_zip(Pars, FPars)],
            safe_link(Dir, FRet, ret, Expr),
            [{E, Expr} || E <- Pars];
        {[Fun], rmlink} ->
            FPars = ?Graph:path(Fun, [fpar]),
            FRet  = ?Graph:path(Fun, [fret]),
            [safe_link(Dir, From, call, To) ||
                {From, To} <- safe_zip(Pars, FPars)],
            safe_link(Dir, FRet, ret, Expr),
            [{E, Expr} || E <- Pars];

        {[], _} ->
            case ?Anal:children(Name) of
                [{esub, Mod},{esub, Fun}]  ->
                    safe_link(Dir, [Mod, Fun | Pars], dep, Expr),
                    [{E, Expr} || E <- [Mod, Fun | Pars]];
                [] -> %% Atom
                    safe_link(Dir, [Name | Pars], dep, Expr),
                    [{E, Expr} || E <- [Name | Pars]];
                _List -> %% fun(...) -> ... end (Args)
                        %% (fun(...) -> ... end) (Args)
                    safe_link(Dir, [Name | Pars], dep, Expr),
                    [{E, Expr} || E <- [Name | Pars]]
            end
    end;

add_del(Dir, #expr{type=block_expr}, Expr, _P) ->
    [{_, BlockCl}] = ?Anal:children(Expr),
    Body = ?Anal:children(BlockCl),
    safe_link(Dir, element(2, lists:last(Body)), flow, Expr),
    [{E, Expr} || {_, E} <- Body];
add_del(Dir, #expr{type=catch_expr}, Expr, _P) ->
    [{_, CatchCl}] = ?Anal:children(Expr),
    [{_, Body}] = ?Anal:children(CatchCl),
    safe_link(Dir, Body, flow, Expr),
    [{Body, Expr}];
add_del(Dir, #expr{type=if_expr}, Expr, _P) ->
    Clauses = ?Anal:children(Expr),
    List =
        [begin
            Children = ?Anal:children(ClI),
            safe_link(Dir, element(2, lists:last(Children)), flow, Expr),
            Children
         end || {_, ClI} <- Clauses],
    [{E, Expr} || {_, E} <- lists:flatten(List)];
add_del(Dir, #expr{type=try_expr}, Expr, _P) ->
    [{_, HeadCl} | Clauses] = ?Anal:children(Expr),
    HeadChildren = [ E || {_, E} <- ?Anal:children(HeadCl)],
    Last = lists:last(HeadChildren),
    List1 =
        [begin
            List = [{_, PatI} | Rest ] = ?Anal:children(ClI),
            safe_link(Dir, Last, flow, PatI),
            safe_link(Dir, element(2, lists:last(Rest)), flow, Expr),
            List
         end || {exprcl, ClI} <- Clauses],
    List2 =
        [begin
            List = ?Anal:children(ClI),
            safe_link(Dir, element(2, lists:last(List)), flow, Expr),
            List
         end || {catchcl, ClI} <- Clauses],
     [{H, Expr} || H <- HeadChildren] ++
      [{E, Expr} || {_, E} <- lists:flatten(List1 ++ List2)];

add_del(Dir, #expr{type=receive_expr}, Expr, _P) ->
    %% TODO: iteration needed through message passing analysis
    Clauses = ?Anal:children(Expr),
    List =
        [begin
            List = ?Anal:children(ClI),
            safe_link(Dir, element(2, lists:last(List)), flow, Expr),
            List
         end || {_, ClI} <- Clauses],
    [{E, Expr} || {_, E} <- lists:flatten(List)];

add_del(Dir, #expr{type=send_expr}, Expr, _P) ->
    %% TODO: iteration needed through message passing analysis
    [{_, E1}, {_, E2}] = ?Anal:children(Expr),
    safe_link(Dir, E2, flow, Expr),
    [{E1, Expr}, {E2, Expr}];

add_del(Dir, #expr{type=list_comp}, Expr, _P) ->
    [{_, HeadCl}, {_, ComprCl}] = ?Anal:children(Expr),
    [{_, HeadE}] = ?Anal:children(HeadCl),
    safe_link(Dir, HeadE, cons_e, Expr),
    [{HeadE, Expr} | [ {E, Expr} || {_, E} <- ?Anal:children(ComprCl)]];
add_del(Dir, #expr{type=list_gen}, Expr, _P) ->
    [{_, PCl}, {_, ECl}] = ?Anal:children(Expr),
    [{_, P}] = ?Anal:children(PCl),
    [{_, E}] = ?Anal:children(ECl),
    safe_link(Dir, E, sel_e, P),
    [{P, Expr}, {E, Expr}];
add_del(_Dir, #expr{type=filter}, Expr, _P) ->
    [{_, ECl}] = ?Anal:children(Expr),
    [{_, E}] = ?Anal:children(ECl),
    [{E, Expr}];

add_del(_Dir, #expr{type=binary_gen},  _Expr, _P) -> [];
%% P <= E
add_del(_Dir, #expr{type=binary},  _Expr, _P) -> [];
%% << ... >>
add_del(_Dir, #expr{type=bin_comp},  _Expr, _P) -> [];
%% << E || ... >>

%% size_qualifier, prfix_bit_expr
%% bit_size_expr
%% field_list, arglist

add_del(Dir, #expr{type=record_update}, Expr, _P) ->
    [{_, RecVar}, {_, FList}] = ?Anal:children(Expr),
    Fields = ?Anal:children(FList),
    safe_link(Dir, RecVar, cons_e, Expr), %% todo: cons_e ?????
    [safe_link(Dir, F, cons_e, Expr) || {_, F} <- Fields],
    [{RecVar, Expr} | [{F, Expr} || {_, F} <- Fields]];

add_del(Dir, #expr{type=record_expr, role = pattern}, Expr, _P) ->
    [{_, FList}] = ?Anal:children(Expr),
    Fields = ?Anal:children(FList),
    [safe_link(Dir, Expr, sel_e, F) || {_, F} <- Fields],
    [{F, Expr} || {_, F} <- Fields];

add_del(Dir, #expr{type=record_expr}, Expr, _P) ->
    [{_, FList}] = ?Anal:children(Expr),
    Fields = ?Anal:children(FList),
    [safe_link(Dir, F, cons_e, Expr) || {_, F} <- Fields],
    [{F, Expr} || {_, F} <- Fields];

add_del(Dir, #expr{type=T, role = pattern}, Expr, _P) when (T == record_field)
                                           orelse (T == record_joker_field) ->
    [{_, Pat}] = ?Anal:children(Expr),
    safe_link(Dir, Expr, flow, Pat),
    [{Pat, Expr}];

add_del(Dir, #expr{type= T}, Expr, _P) when (T == record_field) orelse
                                   (T == record_joker_field) ->
    [{_, E}] = ?Anal:children(Expr),
    safe_link(Dir, E, flow, Expr),
    [{E, Expr}];

add_del(Dir, #expr{type=record_index, role = pattern}, Expr, _P) ->
    [{_, P}] = ?Anal:children(Expr),
    safe_link(Dir, P, dep, Expr),
    [];

add_del(Dir, #expr{type= T}, Expr, _P) when (T == record_access) orelse
                                   (T == record_index) ->
    Children = ?Anal:children(Expr),
    [safe_link(Dir, E, dep, Expr) || {_, E} <- Children],
    [{E, Expr} || {_, E} <- Children];

add_del(Dir, #expr{type=implicit_fun}, Expr, _P) ->
    [{_, Name}, {_, Arity}] = ?Anal:children(Expr),
    safe_link(Dir, [Name, Arity], dep, Expr),
    [{Name, Expr}];

add_del(_Dir, #expr{type=fun_expr}, Expr, _P) ->
    Clauses = ?Anal:children(Expr),
    Exprs = lists:flatmap(fun ({_, Cl}) ->
                              ?Anal:children(Cl)
                          end, Clauses),
    [{E, Expr} || {_, E} <- Exprs];
add_del(_Dir, #expr{}, _Expr, _P) -> [];

add_del(Dir, #form{type=func}, Func, _File) ->
    case Dir of
        mklink -> ok;
        _ -> Apps = get_app(Func),
             [clear_clause(Cl, Apps) || {funcl, Cl} <- ?Anal:children(Func)]
    end,
    {_, Clauses} = lists:unzip(?Anal:children(Func)),
    [SemFun] = ?Graph:path(Func, [fundef]),
    #func{arity = Arity} = ?Graph:data(SemFun),
    add_del_return_node(Dir, SemFun, Clauses),
    add_del_param_nodes(Dir, SemFun, Clauses, Arity),
    Exprs = lists:flatmap(fun (Cl) ->
                              ?Anal:children(Cl)
                          end, Clauses),
    [{Expr, Func} || {_, Expr} <- Exprs];

add_del(_Dir, #clause{}, Clause, P) ->
    Exprs = ?Anal:children(Clause),
    [{Expr, P} || {_, Expr} <- Exprs];

add_del(_Dir, _Type, _Expr, _P) -> [].

safe_zip([A | As], [B | Bs]) ->
    [{A, B} | safe_zip(As, Bs)];
safe_zip(_,_) ->
    [].

add_del_param_nodes(mklink, Node, Clauses, Arity) when Arity > 0->
    FPars = ?NodeSync:get_node(fpar, Node),
    %% case ?Graph:path(Node, [fpar]) of
    %%     [] -> FPars = [?Graph:create(#expr{type=fpar}) || 
    %%                        _ <- lists:seq(1, Arity)],
    %%           [?Graph:mklink(Node, fpar, FPar) || FPar <- FPars];
    %%     FPars -> ok
    %% end,
    PatsCl = [?Graph:path(Cl, [pattern]) || Cl <- Clauses],
%% TODO: remove old fpars and fret
    %% [[?Graph:rmlink(OldFPar, flow, P) || 
    %%       OldFPar <- ?Graph:path(P, [{{flow,back}, {type, '==', fpar}}])] || 
    %%             P <- lists:append(PatsCl)],
    [[safe_link(mklink, FPar, flow, Pat) || 
          {Pat,FPar} <- lists:zip(Pats,FPars)] || Pats <- PatsCl];
add_del_param_nodes(rmlink, Node, Clauses, Arity) when Arity > 0->
    FPars = ?NodeSync:get_node(fpar, Node), %%FPars = ?Graph:path(Node, [fpar]),
    [safe_link(rmlink, Node, fpar, FPar) || FPar <- FPars],
    PatsCl = [?Graph:path(Cl, [pattern]) || Cl <- Clauses],
    [[safe_link(rmlink, FPar, flow, Pat) || 
          {Pat,FPar} <- lists:zip(Pats,FPars)] || Pats <- PatsCl],
    [?Graph:delete(FPar) || FPar <- FPars];
add_del_param_nodes(_, _, _, _) -> ok.

add_del_return_node(mklink, Node, Clauses) ->
    FRet = ?NodeSync:get_node(fret, Node),
    %% case ?Graph:path(Node, [fret]) of
    %%     [] -> FRet_ = ?Graph:create(#expr{type=fret}),
    %%           ?Graph:mklink(Node, fret, FRet);
    %%     [FRet_] -> ok;
    %%     [FRet_, _] -> ?d(?Graph:data(Node))
    %% end,
    Lasts  = lists:append([?Graph:path(Cl, [{body, last}]) || Cl <- Clauses]),
    safe_link(mklink, Lasts, flow, FRet);
add_del_return_node(rmlink, Node, Clauses) ->
    Lasts  = lists:append([?Graph:path(Cl, [{body, last}]) || Cl <- Clauses]),
    FRet = ?NodeSync:get_node(fret, Node), %[FRet] = ?Graph:path(Node, [fret]),
    safe_link(rmlink, Lasts, flow, FRet),
    ?Graph:rmlink(Node, fret, FRet),
    ?Graph:delete(FRet).

%%% @private
remove(Parent, Pre, {Tag, Child}, Post) ->
    case ?Anal:data(Parent) of
        #file{} when Tag == form ->
             Apps = get_app(Child),
             [clear_clause(Cl, Apps) || {funcl, Cl} <- ?Anal:children(Child)],
             walk(fun add_del/4, [{Child, Parent}], rmlink);
        #form{type=func} when Tag == funcl ->
             Apps = get_app(Parent),
             clear_clause(Child, Apps),
             walk(fun add_del/4, [{Child, Parent}], rmlink);
        #form{} -> ok;

%%% TODO: handle fpar-fret!!!!!

        #clause{type=fundef} when Tag == body ->
            case Post of
                [] ->
                    Form = ?Anal:parent(Parent),
                    Apps = get_app(Form),
                    [FRet] = ?Graph:path(Parent, [{funcl,back}, fundef, fret]),
                    safe_link(rmlink, Child, flow, FRet),
                    safe_link(rmlink, FRet,  ret,  Apps),
                    case lists:reverse(Pre) of
                        [{body, Last} | _] -> link(mklink, Last, ret, Apps);
                        _ -> ok
                    end;
                _ -> ok
            end,
            walk(fun add_del/4, [{Child, Parent}], rmlink);
        #clause{type=fundef} when Tag == pattern ->
            FPars = [N || N <-?Graph:path(Child, [{flow, back}]), 
                           (?Graph:data(N))#expr.type == fpar ],
            [safe_link(rmlink, FPar, flow, Child) || FPar <- FPars],
            %?Graph:delete(FPar),
            walk(fun add_del/4, [{Child, Parent}], rmlink);
        #clause{type=fundef} -> ok;
        #clause{type=Type} when Type == pattern orelse Type == guard orelse
                                Type == timeout orelse Type == block orelse
                                Type == funexpr ->
            case Post of
                [] ->
                    Expr = ?Anal:parent(Parent),
                    safe_link(rmlink, Child, flow, Expr),
                    case lists:reverse(Pre) of
                        [{body, Last} | _] -> link(mklink, Last, flow, Expr);
                        _ -> ok
                    end;
                _ -> ok
            end,
            walk(fun add_del/4, [{Child, Parent}], rmlink);
        #clause{type=expr} ->
            Expr = ?Anal:parent(Parent),
            case ?Anal:data(Expr) of
                #expr{type=catch_expr} -> safe_link(rmlink, Child, flow, Expr);
                #expr{type=case_expr} ->
                    [_| Clauses] = ?Anal:children(Expr),
                    [begin
                      [{_, PatI} | _] = ?Anal:children(ClI),
                      safe_link(rmlink, Child, flow, PatI)
                     end || {_, ClI} <- Clauses];
                %% todo andalso, orelse
                _ -> ok
            end,
            walk(fun add_del/4, [{Child, Parent}], rmlink);
        #clause{type=hexpr} ->
            ListComp = ?Anal:parent(Parent),
            safe_link(rmlink, Child, cons_e, ListComp),
            walk(fun add_del/4, [{Child, Parent}], rmlink);
        #clause{type=T} when T==compr orelse T==list_gen orelse T==filter->
            walk(fun add_del/4, [{Child, Parent}], rmlink);
        #expr{type=T, value=V} when (T == infix_expr andalso V == ':') orelse
                                     T == arglist ->
            App = ?Anal:parent(Parent),
            AppPar = ?Anal:parent(App),
            add_del(rmlink, App, ?Anal:data(App), AppPar),
            walk(fun add_del/4, [{Child, Parent}], rmlink);
        #expr{} ->
             walk(fun add_del/4, [{Child, Parent}], rmlink),
             case ?Anal:data(Child) of
                 #expr{} ->
                     [link(rmlink, Child, Link, ?Graph:path(Child, [Link]))
                        || Link <- forw_tags() ++ back_tags()];
                 _ -> ok
             end
    end.

%%% @private
update(Node, #expr{type=atom, value = _Name}) ->
    case {?Graph:path(Node, [{esub,back}]), ?Graph:path(Node, [{name,back}])} of
        {[Parent], _} ->
            case ?Graph:data(Parent) of
                #expr{type=application}->
                    todo;
                _ ->
                    ok
            end;        
        {_, [Clause]} -> 
            case ?Graph:data(Clause) of
                #clause{type=fundef} ->
                    insert(Clause, [], {name, Node}, []);
                _ ->
                    ok
            end;
        _->
            ok
    end;
%% TODO: check macro substitution handling
update(_,_) -> ok.

walk(Fun, [{Node, Parent} | Tail], Dir) ->
    walk(Fun, Fun(Dir, ?Anal:data(Node), Node, Parent) ++ Tail, Dir);
walk(_, [], _) ->
    ok.

%% -----------------------------------------------------------------------------
%%

clear_clause(Cl, Apps) ->
    FPars = ?Graph:path(Cl, [{funcl, back}, fundef, fpar]),
    FRet = ?Graph:path(Cl, [{funcl, back}, fundef, fret]),
    [begin
        [{_, Name}, {_, ArgList}] = ?Anal:children(App),
        Pars = [Par || {_, Par} <- ?Anal:children(ArgList)],
        [safe_link(rmlink, Par, flow, FPar) ||
                {FPar, Par} <- lists:zip(FPars, Pars)],
        safe_link(rmlink, FRet, flow, App),
        case ?Anal:children(Name) of
            [{_, Mod},{_, Fun}]  ->
                link(mklink, [Mod, Fun | Pars], dep, App);
            [] ->
                link(mklink, [Name | Pars], dep, App)
        end
      end || App <- Apps].

forw_tags() ->
    [flow, call, ret, dep, sel, {cons_back, back}, sel_e, cons_e].

back_tags() ->
    [{flow, back}, {call, back}, {ret, back}, {dep, back}, {sel, back}, 
     cons_back, {sel_e, back}, {cons_e, back}].

back_nodes(Node) ->
    case ?Graph:data(Node) of
        #expr{} ->
            lists:flatten([?Graph:path(Node, [Tag]) || Tag <- back_tags()]);
        _ ->
            []
    end.

get_app(Node) ->
    ?Graph:path(Node,[fundef, {{funeref, back}, {type,'==', application }}]) ++
    ?Graph:path(Node,[fundef, {{funlref, back}, {type,'==', application }}]).

link(Dir, List1, {Link, back}, List2) ->
    link(Dir, List2, Link, List1);
link(Dir, List1, Link, Elem2) when is_list(List1) ->
    [?Graph:Dir(Elem1, Link, Elem2) || Elem1 <- List1];
link(Dir, Elem1, Link, List2) when is_list(List2) ->
    [?Graph:Dir(Elem1, Link, Elem2) || Elem2 <- List2];
link(Dir, Elem1, Link, Elem2) ->
    ?Graph:Dir(Elem1, Link, Elem2).

%% safe_link(Dir, List1, {Link, back}, List2) ->
%%     safe_link(Dir, List2, Link, List1);
safe_link(mklink, List1, Link, List2) ->
    link(mklink, List1, Link, List2);
safe_link(Dir, List1, Link, Elem2) when is_list(List1) ->
    [?Graph:Dir(Elem1, Link, Elem2)
      || Elem1 <- List1, lists:member(Elem2, ?Graph:path(Elem1, [Link]))];
safe_link(Dir, Elem1, Link, List2) when is_list(List2) ->
    [?Graph:Dir(Elem1, Link, Elem2)
      || Elem2 <- List2, lists:member(Elem2, ?Graph:path(Elem1, [Link]))];
safe_link(Dir, Elem1, Link, Elem2) ->
    case lists:member(Elem2, ?Graph:path(Elem1, [Link])) of
        false -> ok;
        true  -> ?Graph:Dir(Elem1, Link, Elem2)
    end.
%% -----------------------------------------------------------------------------
%% Reaching

%% @spec reach([node()], [Opt]) -> [node()]
%%       Opt = back | {back, bool()} |
%%             safe | {safe, bool()}
%%
%% @doc Given a set of source expression nodes, returns the set of reachable
%% expressions. Supported options are:
%% <dl>
%%  <dt>{@type {back, false@}}</dt>
%%  <dd>This is the default behaviour: the result set contains expressions
%%      that may return the result of one of the source expressions.</dd>
%%
%%  <dt>{@type back | {back, true@}}</dt>
%%  <dd>The result set contains expressions with results that may be returned
%%      by one of the source expressions.</dd>
%%
%%  <dt>{@type {safe, false@}}</dt>
%%  <dd>This is the default behaviour: expressions in the result set may
%%      return a value that is independent of the source set.</dd>
%%
%%  <dt>{@type safe | {safe, true@}}</dt>
%%  <dd>Expressions in the result set cannot return values independent of the
%%      source set.</dd>
%% </dl>
reach(From, Opts) ->
    reach(From, Opts, false).

%% @spec reach([node()], [Opt], bool()) -> [node()]
%%       Opt = back | {back, bool()} |
%%             safe | {safe, bool()}
reach(From, Opts, Compact) ->
%?d(From),
    Dir =
        case proplists:get_value(back, Opts, false) of
            true  -> back;
            false -> forw
        end,
    Init = set_addl(From, set_new()),
    {Reach, EReach} = walk3(fun(Node, Set) -> flow_step(Node, Dir, Set) end, 
                             Init, Init, set_new()),
    Result = 
        case proplists:get_value(safe, Opts, false) of
            false ->
                set_lst(Reach);
            true ->
                Strict = set_filt(Reach,fun(N) -> is_strict(N, Reach, Dir) end),
                Safe = set_filt(Strict, fun(N) -> is_safe(N, Strict, Dir) end),
                walk1(fun(N, _) -> safe_step(N, Safe, Dir) end, Init, Init)
        end,
    Fun = fun(Node) -> 
              ?Syn:class(Node) == expr andalso 
              reflib_expression:role(Node) =/= undefined 
          end,
    case Compact of
        true  -> lists:filter(Fun, EReach);
        false -> lists:filter(Fun, Result)
    end.

is_strict(Node, Reach, Dir) ->
    lists:all(fun not_dep/1, edges(Node, rev(Dir)))
    andalso
    lists:all(fun(N) -> set_has(N, Reach) end, flow_step(Node, rev(Dir), [])).

rev(forw) -> back;
rev(back) -> forw.

not_dep({_, dep, _}) -> false;
not_dep(_)      -> true.

is_safe(Node, Strict, Dir) ->
    lists:all(fun not_depcomp/1, edges(Node, Dir))
    andalso
    lists:all(fun(N) -> set_has(N, Strict) end,
              [N || {_, flow, N} <- edges(Node, Dir)]).

not_depcomp({_, dep, _})      -> false;
not_depcomp({_, {cons, _}, _}) -> false;
not_depcomp(_)           -> true.

safe_step(Node, Safe, Dir) ->
    case set_has(Node, Safe) of
        true  -> [N || {_, flow, N} <- edges(Node, Dir)];
        false -> []
    end.

walk1(NextF, Work, Set) ->
    case set_select(Work) of
        empty ->
            Set;
        {Sel, Rest} ->
            New = [N || N <- NextF(Sel, Set), not set_has(N, Set)],
            walk1(NextF, set_addl(New, Rest), set_addl(New, Set))
    end.

walk2(NextF, Work, Set, Orig) ->
    case set_select(Work) of
        empty ->
            Set;
        {Sel, Rest} ->
            New = [N || N <- NextF(Sel, Set), not set_has(N, Set), 
                                              not set_has(N, Orig)],
            walk2(NextF, set_addl(New, Rest), set_addl(New, Set), 
                  set_addl(New, Orig))
    end.

walk3(NextF, Work, Set, EndSet) ->
    case set_select(Work) of
        empty ->
            {Set, EndSet};
        {Sel, Rest} ->
            Next = NextF(Sel, Set),
            New  = [N || N <- Next, not set_has(N, Set)],
            case Next of
                [] -> walk3(NextF, set_addl(New, Rest), 
                            set_addl(New, Set), set_addl([Sel], EndSet));
                [Fret] ->
                    case {reflib_expression:role(Fret), NextF(Fret, Set)} of
                        {undefined, []} ->
                            walk3(NextF, set_addl(New, Rest), 
                                  set_addl(New, Set), set_addl([Sel], EndSet));
                        _ -> walk3(NextF, set_addl(New, Rest), 
                                   set_addl(New, Set), EndSet)
                    end;
                _  -> walk3(NextF, set_addl(New, Rest), 
                            set_addl(New, Set), EndSet)
            end
    end.

flow_step(Node, Dir, Set) ->
    lists:flatmap(fun (N) -> flow_step1(N, Dir, Set) end, edges(Node, Dir)).

flow_step1({_, flow, Next}, _, _) ->
    [Next];
flow_step1({_Orig, {cons, Ind}, Comp}, Dir=forw, Set) ->
    [Next || Reach <- walk2(fun(Node, NewSet) -> 
                                flow_step(Node, forw, set_addl(NewSet, Set)) 
                            end, [Comp], [Comp], Set),
             {_, {sel, I}, Next} <- edges(Reach, Dir),
             I =:= Ind];
flow_step1({_Orig, {sel, Ind}, Comp}, Dir=back, Set) ->
    [Next || Reach <- walk2(fun(Node, NewSet) -> 
                                flow_step(Node, back, set_addl(NewSet, Set))
                            end, [Comp], [Comp], Set),
             {_, {cons, I}, Next} <- edges(Reach, Dir),
             I =:= Ind];
flow_step1(_A, _B, _C) ->
    [].

edges(Node, forw) ->
    [{Node, flow, To} || To <- ?Graph:path(Node, [flow])] ++
    [{Node, flow, To} || To <- ?Graph:path(Node, [call])] ++
    [{Node, flow, To} || To <- ?Graph:path(Node, [ret])] ++
    [{Node, dep, To} || To <- ?Graph:path(Node, [dep])] ++
    [{Node, {sel, ?Graph:index(Node, sel, To)}, To}
        || To <- ?Graph:path(Node, [sel])] ++
    [{Node, {cons, ?Graph:index(To, cons_back, Node)}, To}
        || To <- ?Graph:path(Node, [{cons_back, back}])] ++
    [{Node, {sel, e}, To} || To <- ?Graph:path(Node, [sel_e])] ++
    [{Node, {cons, e}, To} || To <- ?Graph:path(Node, [cons_e])];

edges(Node, back) ->
    [{Node, flow, To} || To <- ?Graph:path(Node, [{flow, back}])] ++
    [{Node, flow, To} || To <- ?Graph:path(Node, [{call, back}])] ++
    [{Node, flow, To} || To <- ?Graph:path(Node, [{ret, back}])] ++
    [{Node, {sel, ?Graph:index(To, sel, Node)}, To}
        || To <- ?Graph:path(Node, [{sel, back}])] ++
    [{Node, {cons, ?Graph:index(Node, cons_back, To)}, To}
        || To <- ?Graph:path(Node, [cons_back])] ++
    [{Node, {sel, e}, To} || To <- ?Graph:path(Node, [{sel_e, back}])] ++
    [{Node, {cons, e}, To} || To <- ?Graph:path(Node, [{cons_e, back}])] ++
    [{Node, dep, To} || To <- ?Graph:path(Node, [{dep, back}])].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% First order data-flow

reach_1st(From, Opts) ->
    reach_1st(From, Opts, false).

%% Dir = forw, back
%% From = [node()]
reach_1st(From, Opts, Comp) -> 
    Dir =
        case proplists:get_value(back, Opts, false) of
            true  -> back;
            false -> forw
        end,
    Init = set_addl(From, set_new()),
    Fun = fun(Node, Set, Chains) -> 
              flow_step(Node, Dir, Comp, Set, fun walk_1st/8, Chains) 
          end,
    [{_Chains, Reach, Compact}] = 
        walk_1st(Fun, Init, Init, [], [], set_new(), Dir, Comp),
    Filter = fun(Node) -> 
                 ?Syn:class(Node) == expr andalso 
                 reflib_expression:role(Node) =/= undefined 
             end,
    case Comp of
        true  -> lists:filter(Filter, Compact);
        false -> lists:filter(Filter, Reach)
    end.

%% Calls = [[node()|atom()]]
%% e.g. [[n1, call, n2, call, n3], [n5, call, n6]]
walk_1st(_NextF, [], ResultS, Compact, CallL, _Orig, _Dir, _Comp) ->    
    [{CallL, ResultS, Compact}];
walk_1st(NextF, [Node | WorkS], ResultS, Compact, CallL, Orig, Dir, Comp) ->
    PartCallL = lists:partition(fun([N| _]) when N == Node-> true;
                                    (_) -> false 
                                 end, CallL),
    Filter = fun(N,S1,S2) -> not set_has(N, S1) andalso not set_has(N, S2) end,
    case Dir of
        forw -> Fun = fun fpar/1, Label = call;
        back -> Fun = fun fret/1, Label = ret
    end,
    case {Dir, ?Graph:data(Node), PartCallL} of
        {forw, #expr{type=fret},  {[], Chains2}} ->
            Next = ?Graph:path(Node, [ret]),
            New = [N || N <- Next, Filter(N,ResultS,Orig)],
            NewCallL = Chains2; %%[ [N, ret, Node] || N <- New] ++ Chains2;
        {back, #expr{type=fpar},  {[], Chains2}} ->
            Next = ?Graph:path(Node, [{call, back}]),
            New = [N || N <- Next, Filter(N, ResultS, Orig)],
            NewCallL = Chains2;%%[ [N, call, Node] || N <- New] ++ Chains2;
        {forw, #expr{type=fret}, {Chains1, Chains2}} -> 
            {Next, NewCallL1} = reduce_ret(Node, Chains1, {[],[]}),
            New = [N || N <- Next, Filter(N, ResultS, Orig)],
            NewCallL = NewCallL1 ++ Chains2;
        {back, #expr{type=fpar}, {Chains1, Chains2}} -> 
            {Next, NewCallL1} = reduce_call(Node, Chains1, {[],[]}),
            New = [N || N <- Next, Filter(N, ResultS, Orig)],
            NewCallL = NewCallL1 ++ Chains2;
        {_, _,  {[], _}} ->
            NextList = NextF(Node, ResultS, []),
            {_, C} = lists:unzip(NextList),
            Next = lists:filter(fun([])-> false; (_) -> true end, C),
            {A, B} = lists:unzip([{NewChain, N} || 
                                    {NewChain, N} <- NextList, 
                                    Filter(N, ResultS, Orig)]),
            NewChains = lists:append(A),
            New = lists:filter(fun([])-> false; (_) -> true end, B),
            FPars = [N || N  <- New, Fun(N)],
            NewCallL = CallL ++ NewChains ++ [[N, Label, Node] || N <- FPars];
        {_, _, {Chains1, Chains2}} -> 
            NextList = NextF(Node, ResultS, Chains1),
            {_, C} = lists:unzip(NextList),
            Next = lists:filter(fun([])-> false; (_) -> true end, C),
            {A, B} = lists:unzip([{NewCh, N} || 
                                    {NewCh, N} <- NextList,  
                                    Filter(N, ResultS, Orig)]),
            NewChains = lists:append(A),
            New = lists:filter(fun([])-> false; (_) -> true end, B),
            FPars = [N || N <-New, Fun(N)],
            case FPars of
                [] -> NewCallL = Chains2 ++ NewChains ++
                                 [[N|tl(L)] || N<-New, L<-Chains1]; %% dupl??
                _  -> NewCallL = Chains2 ++ NewChains ++
                                 [[N | tl(L)] || N<-New--FPars, L<-Chains1] ++ 
                                 [[N, Label | L] || N <- FPars, L <- Chains1]
            end
    end,
    case Comp of 
        true -> compute_compact_set(Comp, Compact, Dir, New, NewCallL, Next, 
                                    NextF, Node, Orig, ResultS, WorkS);
        false -> walk_1st(NextF, set_addl(New, WorkS), set_addl(New, ResultS), 
                          Compact, NewCallL, set_addl(New, Orig), Dir, Comp)
    end.

compute_compact_set(Comp, Compact, Dir, New, NewCallL, [], NextF, Node, Orig,
                    ResultS, WorkS) ->
    walk_1st(NextF, set_addl(New, WorkS), set_addl(New, ResultS), 
             set_addl([Node],Compact), NewCallL, set_addl(New,Orig), Dir, Comp);
compute_compact_set(Comp, Compact, Dir, New, NewCallL, [Fret], NextF, Node, 
                    Orig, ResultS, WorkS) ->
    case (reflib_expression:role(Fret) == undefined) andalso
         begin {_,B} = lists:unzip(NextF(Fret,ResultS,NewCallL)), B==[] end of
        true ->
            walk_1st(NextF, set_addl(New, WorkS), set_addl(New, ResultS),
                     set_addl([Node], Compact), NewCallL, set_addl(New, Orig), 
                     Dir, Comp);
        _ ->
            walk_1st(NextF, set_addl(New, WorkS), set_addl(New, ResultS), 
                     Compact, NewCallL, set_addl(New, Orig), Dir, Comp)
    end;
compute_compact_set(Comp, Compact, Dir, New, NewCallL, _, NextF, _Node, Orig,
                    ResultS, WorkS) ->
    walk_1st(NextF, set_addl(New, WorkS), set_addl(New, ResultS), Compact, 
             NewCallL, set_addl(New, Orig), Dir, Comp).

%% reduce_call(Node, [L = [_,call,_ | _] | Chains], _Res = {New, CallL}) ->
%%     Pars = ?Graph:path(Node, [{call, back}]),
%%     reduce_call(Node, Chains,
%%                 {Pars ++ New, [[Par, call | L] || Par <- Pars] ++ CallL});
reduce_call(Node, [[_,ret,App | Tail] | Chains], Res = {New, CallL}) ->
    Pars = ?Graph:path(Node, [{call, back}]),
    Merge = lists:flatten([[Par || 
                     A <- reflib_query:exec(Par, [{esub,back},{esub,back}]), 
                     A == App] || Par <- Pars]),
    case {Merge, Tail} of
        {[], _}  -> reduce_call(Node, Chains, Res);
        {_,  []} -> reduce_call(Node, Chains, {Merge ++ New, CallL});
        {_,  _}  -> reduce_call(Node, Chains, {Merge ++ New,
                                [[N|Tail] || N <- Merge] ++ CallL})
    end;
reduce_call(_Node, [], Res) ->
    Res.    

%% reduce_ret(Node, [L = [_,ret,_ | _] | Chains], _Res = {New, CallL}) ->
%%     Apps = ?Graph:path(Node, [ret]),
%%     reduce_call(Node, Chains, 
%%                 {Apps ++ New, [[App, ret | L] || App <- Apps] ++ CallL});
reduce_ret(Node, [[_,call,Param | Tail] | Chains], Res = {New, CallL}) ->
    Apps = ?Graph:path(Node, [ret]),
    [{_, ArgList}] = ?Syn:parent(Param),
    [{_, App}]     = ?Syn:parent(ArgList),
    case {lists:member(App, Apps), Tail} of
        {true, []} -> reduce_ret(Node, Chains, {[App|New], CallL});
        {true,  _} -> reduce_ret(Node, Chains, {[App|New], [[App|Tail]|CallL]});
        {false, _} -> reduce_ret(Node, Chains, Res)
    end; 
reduce_ret(_Node, [], Res) ->
    Res.    

fret(Node) -> 
     ?Syn:class(Node) == expr andalso reflib_expression:type(Node) == fret.

fpar(Node) -> 
    ?Syn:class(Node) == expr andalso reflib_expression:type(Node) == fpar.


    
%% First order flow_step
flow_step(Node, Dir, Compact, Set, Fun, Chains) ->
    lists:flatmap(fun (N) -> flow_step1(N, Dir, Compact, Set, Fun, Chains) end,
                  edges(Node, Dir)).

flow_step1({_, flow, Next}, _, _, _, _, _Chains) ->
    [{[], Next}];
flow_step1({_, {cons, Ind}, Comp}, Dir=forw, Compact, Set, Fun, Chains) ->
     [{lists:usort(reduce2(Ch, Next, N, [])), Next} || 
     {Ch, Reach, _} <- Fun(fun(Node, NewSet, Ch2) -> %% walk_1st/6
                               flow_step(Node,forw, Compact, 
                                         set_addl(NewSet, Set),Fun,Ch2) 
                           end, [Comp], [Comp], [], transform(Comp,Chains,[]),
                           Set,Dir, Compact),
     {N, {sel, I}, Next} <- lists:flatmap(fun(E) -> edges(E, Dir) end, Reach),
     I =:= Ind];
flow_step1({_Orig, {sel, Ind}, Comp}, Dir=back, Compact, Set, Fun, Chains) ->
     [{lists:usort(reduce2(Ch, Next, N, [])), Next} || 
     {Ch, Reach, _} <- Fun(fun(Node,NewSet,Ch2) -> %% walk_1st/6
                               flow_step(Node, back, Compact, 
                                        set_addl(NewSet, Set),Fun,Ch2)
                           end, [Comp], [Comp], [], transform(Comp,Chains,[]),
                           Set, Dir, Compact),
     {N, {cons, I}, Next} <- lists:flatmap(fun(E) -> edges(E, Dir) end, Reach),
     I =:= Ind];
flow_step1(_A, _B, _C, _D, _F, Chains) ->
    [{Chains,[]}].

transform(N, [[_|Ch] | Chains], Res) ->
    transform(N, Chains, [[N|Ch]|Res]);
transform(_, [], Res) ->
    Res.

reduce2([], _, _, Res) ->
    Res;
reduce2([[Comp | NewCh] | NewChs], Next, Comp, Res)->
    reduce2(NewChs, Next, Comp, [[ Next | NewCh] | Res]);
reduce2([_| NewChs], Next, Comp, Res ) ->
    reduce2(NewChs, Next, Comp, Res).

%% replace(Next, NewCh, [OrigCh | OrigChs], Res) ->
%%     replace(Next, NewCh, OrigChs, [[ Next | tl(Head) ++ tl(Chain)] | Res]);
%% replace(_, _, [], _, Res) ->
%%     Res.

%%[ [ Next | tl(Head) ++ tl(Chain)] | Res]

%% merge(Next, NewCh, OrigCh, Fun) ->
%%     case merge1(NewCh, OrigCh, Fun, []) of
%%         [] -> [];
%%         [[_|Tail]] -> [Next|Tail]
%%     end.

%% merge1([], _, _, Res) ->
%%     Res;
%% merge1([_], _, _, Res) ->
%%     Res;
%% merge1([Node, _ | Tail], OrigCh, Fun, _Res) ->
%%     {_New, NewChain} = Fun(Node, [OrigCh], {[], []}),
%%     case NewChain of 
%%         [] -> [];
%%         _  -> merge1(Tail, OrigCh, Fun, NewChain)
%%     end.



%% ---------------------------------------------------------------------------
%% Manupulating the set
set_new() -> [].
set_add(El, Set) -> ordsets:add_element(El, Set).
set_has(El, Set) -> ordsets:is_element(El, Set).
set_filt(Set, Pred) -> ordsets:filter(Pred, Set).
set_lst(Set) -> Set.

set_select([]) -> empty;
set_select([H|T]) -> {H,T}.


set_addl(Lst, Set) ->
    lists:foldl(
      fun(E, S) -> set_add(E, S) end,
      Set, Lst).
