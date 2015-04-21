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

%%% @doc Function analyser.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Melinda Toth <toth_m@inf.elte.hu>

-module(refanal_fun).
-vsn("$Rev$"). % for emacs"
-behaviour(refcore_anal).

-export([schema/0, externs/1, insert/4, remove/4, update/2]).

-export([update_funprop/1]).

-include("core.hrl").

%%% @private
schema() ->
    [{func, record_info(fields, func), [{funcall, func}, {dyncall, func},
                                        {ambcall, func}, {may_be, func}]},
     {form,   [{fundef, func}]},
     {clause, [{functx, clause}]},
     {expr,   [{modref, module}, {funeref, func}, {funlref, func},
               {dynfuneref, func}, {ambfuneref, func},
               {dynfunlref, func}, {ambfunlref, func},
               {localfundef, func}]},
     {module, [{func, func}, {funexp, func}, {funimp, func}]}
    ].

%%% @private
externs(_) -> [].

%%% @private
insert(Parent, _, {Tag, Child}, _) ->
    case ?Anal:data(Parent) of
        #file{type=module} when Tag == form ->
            walk(fun add/3, [{Child, ?NodeSync:get_node(module, Parent)}]);
%%         #file{type=header} ->
%%             [File] = ?Graph:path(Parent, [{iref, back}, {form, back}]),
%%             walk(fun add/3, [{Child, ?NodeSync:get_node(module, File)}]);
        #file{} -> ok;
        #form{type=export} ->
            File = ?Anal:parent(Parent),
            walk(fun add/3,
                 [{Child, {?NodeSync:get_node(module, File), {lref, exp}}}]);
        #form{type=import} -> %% todo atom vagy funlist megy ala...
            File = ?Anal:parent(Parent),
            walk(fun add/3,
                 [{Child, {?NodeSync:get_node(module, File), {eref, imp}}}]);
        #form{type=func} when Tag == funcl ->
            File = ?Anal:parent(Parent),
            walk(fun add/3,[{Child,{?NodeSync:get_node(module,File),Child}}]);
        #form{type=record} ->
            File = ?Anal:parent(Parent),
            walk(fun add/3,[{Child,{File,none}}]);
        #form{} -> ok;

        #clause{type=fundef} when Tag == name ->
            [Fun] = ?Graph:path(Parent, [{funcl, back}, fundef]),
            [Mod] = ?Graph:path(Fun, [{func, back}]),
            #func{arity=Arity} = ?Graph:data(Fun),
            #expr{value=Name} = ?Anal:data(Child),
            ?NodeSync:move_refs(func, [{def, ?Anal:parent(Parent)}],
                                Fun, {Mod, {Name, Arity}});
            %% ?NodeSync:move_refs(func,[{def,?Anal:parent(Parent)},fpar,fret],
            %%                     Fun, {Mod, {Name, Arity}});
        #clause{type=fundef} when Tag == pattern ->
            [Fun] = ?Graph:path(Parent, [{funcl, back}, fundef]),
            [Mod] = ?Graph:path(Fun, [{func, back}]),
            #func{name = Name} = ?Graph:data(Fun),
            Arity = length(?Graph:path(Parent, [pattern])),
            ?NodeSync:move_refs(func, [{def, ?Anal:parent(Parent)}],
                                Fun, {Mod, {Name, Arity}});
            %% ?NodeSync:move_refs(func,[{def,?Anal:parent(Parent)},fpar,fret],
            %%                     Fun, {Mod, {Name, Arity+1}});
        #clause{type=fundef} ->
            File = ?Anal:parent(?Anal:parent(Parent)),
            walk(fun add/3,[{Child,{?NodeSync:get_node(module,File), Parent}}]);
        #clause{} ->
            [FunCl] = ?Graph:path(Parent, [functx]),
            File = ?Anal:parent(?Anal:parent(FunCl)),
            walk(fun add/3,[{Child,{?NodeSync:get_node(module, File), FunCl}}]);

        #expr{type=funref} ->
            FunList = ?Anal:parent(Parent),
            Form = ?Anal:parent(FunList),
            File = ?Anal:parent(Form),
            case ?Anal:data(Form) of
                #form{type=export} -> {FRef, MRef} = {lref, exp}, DefMod = File;
                #form{type=import} ->
                    [{eattr, ModNameN}, _] = ?Anal:children(Form),
                    #expr{value = DefMod} = ?Anal:data(ModNameN),
                    {FRef, MRef} = {eref, imp}
            end,
            del(Parent, ?Anal:data(Parent), none),
            add(Parent, ?Anal:data(Parent),
                {?NodeSync:get_node(module, File), {{FRef, DefMod}, MRef}});
        #expr{type=funlist} ->
            Form = ?Anal:parent(Parent),
            File = ?Anal:parent(Form),
            case ?Anal:data(Form) of
                #form{type=export} -> Ctx = {lref, exp};
                #form{type=import} -> Ctx = {eref, imp}
            end,
            walk(fun add/3, [{Child,{?NodeSync:get_node(module,File),Ctx}}]);
        #expr{role=attr} ->
            [File] = ?Graph:path(Parent, [top, {texpr, back}, {tattr, back},
                                          {form, back}, {incl, back}]),
            walk(fun add/3,[{Child,{?NodeSync:get_node(module,File),none}}]);
        #expr{type=T} when T == application orelse T == implicit_fun ->
            Ctx = ctx_info(Parent),
            del_ref(Parent, Child),
            add(Parent, ?Anal:data(Parent), Ctx),
            walk(fun add/3, [{Child, Ctx}]);
        #expr{type=T, value=V} when (T == infix_expr andalso V == ':') orelse
                                     T ==  arglist ->
            Ctx = ctx_info(Parent),
            App = ?Anal:parent(Parent),
            del_ref(App, Child),
            add(App, ?Anal:data(App), Ctx),
            walk(fun add/3, [{Child, Ctx}]);
        #expr{} ->
            Ctx = ctx_info(Parent),
            walk(fun add/3, [{Child, Ctx}]);

        #typexp{} ->
            [File] = ?Graph:path(Parent,[{tattr,back},{form,back},{incl,back}]),
            walk(fun add/3,[{Child,{?NodeSync:get_node(module,File),none}}])
    end,
    update_funprop(Child).


add(Form, #form{type=func}, Mod) ->
    Cls = [Cl || {funcl, Cl} <- ?Anal:children(Form)],
    Defs = [fundef(Cl) || Cl <- Cls],
    case lists:usort(Defs) of
        [Def] ->
            ?NodeSync:add_ref(func, {def, Form}, {Mod, Def});
        _ ->
            ok
    end,
    [{Cl, {Mod, Cl}} || Cl <- Cls];
add(Form, #form{type=export}, Mod) ->
    [{eattr, FunList}] = ?Anal:children(Form),
    FunRefs = ?Anal:children(FunList),
    [{FunRef, {Mod, {{lref, Mod}, exp}}} || {_, FunRef} <- FunRefs];
add(Form, #form{type=import}, Mod) ->
    [{eattr, ModNameN}, {eattr, FunList}] = ?Anal:children(Form),
    #expr{type = atom, value = DefModName} = ?Anal:data(ModNameN),
    ?NodeSync:add_ref(module, {ref, ModNameN}, DefModName),
    FunRefs = ?Anal:children(FunList),
    [{FunRef, {Mod, {{eref, DefModName}, imp}}} || {_, FunRef} <- FunRefs];
add(Form, #form{type=record}, Mod) ->
    [{T, Mod} || {_, T} <- ?Anal:children(Form)];
add(Form, #form{tag=include}, Mod) ->
    [Header] = ?Graph:path(Form, [iref]),
    [{T, Mod} || {_, T} <- ?Anal:children(Header)];
add(_, #form{}, _) ->
    [];

add(Clause, #clause{}, {_, none} = Ctx) -> %% in record definition
    [{Chld, Ctx} || {_, Chld} <- ?Anal:children(Clause)];
add(Clause, #clause{}, {_, Top} = Ctx) ->
    ?Graph:mklink(Clause, functx, Top),
    [{Chld, Ctx} || {_, Chld} <- ?Anal:children(Clause)];


add(Expr, #expr{type=application}, {Mod, _} = Ctx) ->
    [{esub, FunName}, {esub, ArgLst}] = ?Anal:children(Expr),
    Args = [A || {esub, A} <- ?Anal:children(ArgLst)],
    add_funref(Expr, FunName, length(Args), Mod),
    [{FunName, Ctx} | [{A, Ctx} || A <- Args]];
add(Expr, #expr{type=implicit_fun}, {Mod, _}) ->
    [{esub, FunName}, {esub, ArExp}] = ?Anal:children(Expr),
    case ?Anal:data(ArExp) of
        #expr{type=integer, value=Arity} ->
            add_funref(Expr, FunName, Arity, Mod);
        #expr{type=variable} ->
            % TODO: handle the case _:_/Variable
            todo
    end,
    [];

add(Expr, #expr{type=funref}, {Mod, {{_FRef, DefMod}, MRef}}) ->
    [{esub, FunNameN}, {esub, ArityN}] = ?Anal:children(Expr),
    Arity = reflib_expression:value(ArityN),
    FunName = reflib_expression:value(FunNameN),
    ?NodeSync:add_ref(func, {MRef, {Expr, Mod}}, {DefMod, {FunName, Arity}}),
    [];

add(Expr, #expr{}, Ctx) ->
    [{C, Ctx} || {_, C} <- ?Anal:children(Expr)];

add(TExpr, #typexp{}, Mod) ->
    [{C, {Mod, none}} || {_, C} <- ?Anal:children(TExpr)].


add_funref(Ref, FunNd, Arity, LM) ->
%% io:format("~nAdd: ~p~n", [[Ref, FunNd, Arity, LM]]),
    case name(FunNd, ?Anal:data(FunNd)) of
        {ModRef, ModName, _FunRef, FunName} ->
            ?NodeSync:add_ref(module, {ref, ModRef}, ModName),
            ?NodeSync:add_ref(func, {eref, Ref}, {ModName, {FunName, Arity}});
        {_FunRef, FunName} ->
            ?NodeSync:add_ref(func, {lref, Ref}, {LM, {FunName, Arity}});
        {} ->
            ok
    end.

%%% @private
remove(Parent, _, {Tag, Child}, _) ->
    case ?Anal:data(Parent) of
        #file{} when Tag == form ->
            walk(fun del/3, [{Child, Parent}]);
        #form{type=func} when Tag == funcl ->
            walk(fun del/3, [{Child, Child}]);
        #form{type=T} when T == export orelse T == import orelse T == record ->
            walk(fun del/3, [{Child, ?Anal:parent(Parent)}]);
        #form{} -> ok;

        %% #clause{type=fundef} when Tag == name ->
        %%     [Fun] = ?Graph:path(Parent, [{funcl, back}, fundef]),
        %%     [Mod] = ?Graph:path(Fun, [{func, back}]),
        %%     #func{arity=Arity} = ?Graph:data(Fun),
        %%     #expr{value=Name} = ?Anal:data(Child),
        %%     ?NodeSync:move_refs(func,[{def,?Anal:parent(Parent)},fpar,fret],
        %%                         Fun, {Mod, {Name, Arity}});
        #clause{type=fundef} when Tag == pattern ->
            [Fun] = ?Graph:path(Parent, [{funcl, back}, fundef]),
            [Mod] = ?Graph:path(Fun, [{func, back}]),
            #func{name=Name} = ?Graph:data(Fun),
            Arity = length(?Graph:path(Parent, [pattern])),
            ?NodeSync:move_refs(func, [{def, ?Anal:parent(Parent)}],
                                Fun, {Mod, {Name, Arity}});
            %% ?NodeSync:move_refs(func,[{def,?Anal:parent(Parent)},fpar,fret],
            %%                     Fun, {Mod, {Name, Arity-1}});
        #clause{type=fundef} ->
            walk(fun del/3, [{Child, Parent}]);
        #clause{} ->
            [FunCl] = ?Graph:path(Parent, [functx]),
            walk(fun del/3, [{Child, FunCl}]);

        #expr{type=attr} ->
            walk(fun del/3, [{Child, none}]);
        #expr{type=funref} ->
            File = ?Anal:parent(?Anal:parent(?Anal:parent(Parent))),
            del(Parent, ?Anal:data(Parent), File);
        #expr{type=funlist} ->
            File = ?Anal:parent(?Anal:parent(Parent)),
            walk(fun del/3, [{Child, File}]);
        #expr{type=T} when T == application orelse T == implicit_fun ->
            FunCl = get_funcl(Parent),
            del_ref(Parent, Child),
            walk(fun del/3, [{Child, FunCl}]);
        #expr{type=infix_expr, value=':'} ->
            FunCl = get_funcl(Parent),
            App = ?Anal:parent(Parent),
            del_ref(App, Child),
            walk(fun del/3, [{Child, FunCl}]);
        #expr{type=arglist} ->
            FunCl = get_funcl(Parent),
            App = ?Anal:parent(Parent),
            del(App, ?Anal:data(App), FunCl),
            [{esub, FunName}, {esub, ArgLst}] = ?Anal:children(App),
            [Mod] = ?Graph:path(FunCl, [{funcl, back}, {form, back}]),
            add_funref(App, FunName, length(?Anal:children(ArgLst)), Mod),
            update_funprop(App),
            walk(fun del/3, [{Child, FunCl}]);
        #expr{} ->
            FunCl = get_funcl(Parent),
            walk(fun del/3, [{Child, FunCl}]);

        #typexp{} ->
            walk(fun del/3,[{Child, none}])
    end.


del(Form, #form{type=func}, _Mod) ->
    ?NodeSync:del_ref(func, {def, Form}, Form),
    [{Cl, Cl} || {funcl, Cl} <- ?Anal:children(Form)];
del(Form, #form{type=export}, _Mod) ->
    [{eattr, FunList}] = ?Anal:children(Form),
    FunRefs = ?Anal:children(FunList),
    [{FunRef, _Mod} || {_, FunRef} <- FunRefs];
del(Form, #form{type=import}, Mod) ->
    [{eattr, _ModNameN}, {eattr, FunList}] = ?Anal:children(Form),
    FunRefs = ?Anal:children(FunList),
    [{FunRef, Mod} || {_, FunRef} <- FunRefs];
del(Form, #form{type=record}, _Mod) ->
    [{T, none} || {_, T} <- ?Anal:children(Form)];
del(_Form, #form{}, _Mod) ->
    [];

del(Clause, #clause{}, none) ->
    [{Chld, none} || {_, Chld} <- ?Anal:children(Clause)];
del(Clause, #clause{}, Top) ->
    ?Graph:rmlink(Clause, functx, Top),
    [{Chld, Top} || {_, Chld} <- ?Anal:children(Clause)];

del(Expr, #expr{type=Type}, Top) when Type == implicit_fun;
                                      Type == application ->
    case ?Graph:path(Expr, [funlref]) of
        [Fun] -> ?NodeSync:del_ref(func, {lref, Expr}, Fun);
        [] ->
            case ?Graph:path(Expr, [funeref]) of
                [Fun] ->
                    ?NodeSync:del_ref(func, {eref, Expr}, Fun),
                    [{esub, FunName}| _] = ?Anal:children(Expr),
                    [{esub, ModName}, _] = ?Anal:children(FunName),
                    [Mod] = ?Graph:path(ModName, [modref]),
                    ?NodeSync:del_ref(module, {ref, ModName}, Mod);
                [] ->
                    ok
            end
    end,
    [{Chld, Top} || {_, Chld} <- ?Anal:children(Expr)];
del(Expr, #expr{type=funref}, ImpMod) ->
    case ?Graph:path(Expr, [funlref]) of
        [Fun] ->
            ?NodeSync:del_ref(func, {lref, Expr}, Fun),
            [Mod] = ?Graph:path(Fun, [{funexp, back}]),
            ?NodeSync:del_ref(func, {exp, Mod}, Fun);
        [] ->
            case ?Graph:path(Expr, [funeref]) of
                [Fun] ->
                    ?NodeSync:del_ref(func, {eref, Expr}, Fun),
%%                    [Mod] = ?Graph:path(Fun, [{funimp, back}]),
                    ?NodeSync:del_ref(func, {imp, ImpMod}, Fun);
                [] ->
                    ok
            end
    end,
    [];
del(Expr, #expr{}, Top) ->
    [{Chld, Top} || {_, Chld} <- ?Anal:children(Expr)];
del(TExpr, #typexp{}, Ctx) ->
    [{C, Ctx} || {_, C} <- ?Anal:children(TExpr)].

%%% @private
update(App, #expr{type=application}) ->
    [NameNode, ArgsNode] = ?Graph:path(App, [esub]),
    #expr{value=Name} = ?Graph:data(NameNode),
    Arity = length(?Graph:path(ArgsNode, [esub])),

    [FunSynNode] = ?Graph:path(App, [funlref]),
%   todo Using ?NodeSync would be the right way to do it...
%    FunSynNode = ?NodeSync:get_node(func, {Name, Arity}),
    FunData = ?Graph:data(FunSynNode),
    ?Graph:update(FunSynNode, FunData#func{name=Name, arity=Arity}),
    ok;
update(Node, #expr{type=atom, value = Name}) ->
    case {?Graph:path(Node, [{esub,back}]), ?Graph:path(Node, [{name,back}])} of
        {[Parent], _} ->
            case ?Graph:data(Parent) of
                #expr{type=application} = Data ->
                    update(Parent, Data);
                _ ->
                    ok
            end;
        {_, [Clause]} ->
            case ?Graph:data(Clause) of
                #clause{type=fundef} ->
                    update_name(Clause, Name);
                _ ->
                    ok
            end;
        _->
            ok
    end;
update(_,_) -> ok.

update_name(Clause, Name) ->
    [Form] = ?Graph:path(Clause, [{funcl, back}]),
    [Fun]  = ?Graph:path(Form,   [fundef]),
    [Mod]  = ?Graph:path(Fun,    [{func, back}]),
    #func{arity=Arity} = ?Graph:data(Fun),
    ?NodeSync:move_refs(func, [{def, Form}], Fun, {Mod, {Name, Arity}}),
    update_funprop(Form).


name(N, #expr{type=atom, value=Name}) -> {N, Name};
name(N, #expr{type=implicit_fun}) ->
    [{esub, FId}, {esub, _Arity}] = ?Anal:children(N),
    name(FId, ?Anal:data(FId));
name(C, #expr{type=infix_expr, value=':'}) ->
    [{esub, Mod}, {esub, Fun}] = ?Anal:children(C),
    case {name(Mod, ?Anal:data(Mod)), name(Fun, ?Anal:data(Fun))} of
        {{MNode, MName}, {FNode, FName}} -> {MNode, MName, FNode, FName};
        _ -> {}
    end;
name(_, _) -> {}.

fundef(Cl) ->
    Chld = ?Anal:children(Cl),
    [FN] = [N || {name, N} <- Chld],
    Args = [A || {pattern, A} <- Chld],
    #expr{type=atom, value=Name} = ?Anal:data(FN),
    {Name, length(Args)}.

walk(Fun, [{Node, Ctx} | Tail]) ->
    walk(Fun, Fun(Node, ?Anal:data(Node), Ctx) ++ Tail);
walk(_, []) ->
    ok.


update_funprop(Node) ->
    case ?Anal:data(Node) of
        #form{type=func} -> update_form_funprop(Node);
        #form{} -> ok;
        _D -> %%io:format("~p:~p~n", [Node,_D]),
             update_funprop(?Anal:parent(Node))
    end.

update_form_funprop(Form) ->
    %% TODO: maybe ?NodeSync:get_node support
    case ?Graph:path(Form, [fundef]) of
        [Fun] ->
            ?FunProp:update(Fun, funprops(Form));
        [] ->
            ok
    end.

funprops(Form) -> funprops(Form, {true, []}).

funprops(Node, Props) ->
    lists:foldl(fun funprops/2, node_funprop(Node, ?Anal:data(Node), Props),
                [Child || {_, Child} <- ?Anal:children(Node)]).

node_funprop(Node, #expr{type=Type}, {Pure, Calls}) ->
    {Pure andalso Type /= send_expr andalso Type /= receive_expr,
     if
         Type == application -> add_calls(Node, Calls);
         true -> Calls
     end};

node_funprop(_, _, Props) -> Props.

add_calls(Node, Calls) ->
    FunCalls = [{funcall, F} || {T, F} <- ?Graph:links(Node), lists:member(T, [funeref, funlref])],
    DynCalls = [{dyncall, F} || {T, F} <- ?Graph:links(Node), lists:member(T, [dynfuneref, dynfunlref])],
    AmbCalls = [{ambcall, F} || {T, F1} <- ?Graph:links(Node), lists:member(T, [ambfuneref, ambfunlref]),
                                F <- ?Graph:path(F1, [may_be])],
    ordsets:union(Calls, lists:usort(FunCalls++DynCalls++AmbCalls)).

ctx_info(Expr)->
    FunCl = get_funcl(Expr),
    File = ?Anal:parent(?Anal:parent(FunCl)),
    {?NodeSync:get_node(module, File), FunCl}.

get_funcl(Expr) ->
    [FunCl] = ?Graph:path(Expr, [top, {body, back}, functx]) ++
              ?Graph:path(Expr, [top, {guard, back}, functx]) ++
              ?Graph:path(Expr, [top, {pattern, back}, functx]), %% ++
%% %%    ?Graph:path(Expr, [top, {name, back}, functx]),
    FunCl.

del_ref(App, Child)->
    case ?Graph:path(App, [funlref]) of
        [Fun] -> ?NodeSync:del_ref(func, {lref, App}, Fun);
        [] ->
            case {?Graph:path(App, [funeref]),
                  ?Graph:path(Child, [modref])} of
                {[Fun], [Mod] } ->
                    ?NodeSync:del_ref(func, {eref, App}, Fun),
                    ?NodeSync:del_ref(module, {ref, Child}, Mod);
                {[Fun], []} ->
                    ?NodeSync:del_ref(func, {eref, App}, Fun);
                {[], _} ->
                    ok
            end
    end.
