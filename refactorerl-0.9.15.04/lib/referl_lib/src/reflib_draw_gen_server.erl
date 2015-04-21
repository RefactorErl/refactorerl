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

%%% @doc This module draws a gen_server module and its clients
%%%      with flow of communication between them.
%%%
%%% @author Laszlo Lang <llk@inf.elte.hu>

-module(reflib_draw_gen_server).

-export([draw/2]).

-include("lib.hrl").

draw(ModName, ToFile) ->
    Mod = ?Query:exec(?Mod:find(ModName)),

    GenServerMod = ?Query:exec(?Mod:find(gen_server)),

    ServerNames = server_starts(ModName, GenServerMod),
    Exporteds = ?Query:exec(Mod, ?Mod:exports()),
    ClientFuns = [{[C || C <- ?Query:exec(E, ?Fun:called()),
                            fun_mod_name(C) =/= ModName], E} || E <- Exporteds],

    Starts = gen_server_fun_clients(ServerNames, {start, 4}, GenServerMod),

    StartLinks =
       gen_server_fun_clients(ServerNames, {start_link, 4}, GenServerMod),

    Calls = %[{_CallClients, _CallFun}] =
            gen_server_fun_clients(ServerNames, {call, 2}, GenServerMod),

    Casts = %[{_CastClients, _CastFun}] =
            gen_server_fun_clients(ServerNames, {cast, 2}, GenServerMod),
    {ok, Dev} = file:open(ToFile, [write]),
    io:format(Dev, "digraph model {~n", []),
    GenServerClients =
        draw_gen_server_funs(Dev, Casts ++ Calls ++ StartLinks ++ Starts,
                                                                     ModName),
    ExportedClients =
        draw_callback_exporteds(Dev, ClientFuns, ModName, GenServerClients),
    draw_client_functions(Dev, GenServerClients, ExportedClients, ModName),
    io:format(Dev, "}~n", []),
    file:close(Dev),
    ToRoot = filename:rootname(ToFile),
    os:cmd("dot -Tsvg " ++ ToRoot ++ ".dot -o" ++ ToRoot ++ ".svg"),
    os:cmd("dot -Tpdf " ++ ToRoot ++ ".dot -o" ++ ToRoot ++ ".pdf").


%% @spec draw_gen_server_funs(device(), Functions, atom()) -> Result
%% where
%%       Functions = [{Clients :: [function()], function()}],
%%       Result    = [{Client :: function(), [{function(), Category :: atom()}]}]
draw_gen_server_funs(Dev, Functions, CallbackModName) ->
    GenServerFuns = [{Clients, {F, []}} || {Clients, F} <- Functions],
    FunCatList = draw_module(Dev, GenServerFuns, gen_server, CallbackModName),
    FunClientList = lists:flatten([[{Client, Function} || Client <- Clients]
                                        || {Clients, Function} <- FunCatList]),
    Clients = proplists:get_keys(FunClientList),
    [{C, proplists:get_all_values(C, FunClientList)} || C <- Clients].

%% @spec draw_callback_exporteds(device(), Functions, atom(), GenServerClients) -> Result
%% where
%%       Functions = [{Clients :: [function()], function()}],
%%       GenServerClients = [{Client :: function(), [{function(), Category :: atom()}]}]
%%       Result = [{Client :: function(), [{function(), Category :: atom()}]}]
draw_callback_exporteds(Dev, Functions, CallbackMod, GenServerClients) ->
%    ExportedFuns = [F || {_, F} <- Functions],
    FunList = merge_fun_lists(Functions, GenServerClients, []),
    ExportedCats = draw_module(Dev, FunList, CallbackMod, CallbackMod),
    ExportedClientList = lists:flatten([[{Client, Function} || Client <- Clients]
                                        || {Clients, Function} <- ExportedCats]),
    Clients = proplists:get_keys(ExportedClientList),
    [{C, proplists:get_all_values(C, ExportedClientList)} || C <- Clients].

%% @spec draw_client_functions(device(), GenServerClients, ExportedClients, atom()) -> ok
%% where
%%   GenServerClients=[{Client::function(),[{function(),Category::atom()}]}]
%%   ExportedClients =[{Client::function(),[{function(),Category::atom()}]}]
draw_client_functions(Dev, GenServerClients, ExportedClients, CallbackModName) ->
    Clients = GenServerClients ++ ExportedClients,
    ClientFuns = proplists:get_keys(Clients),
    List = [{C, lists:flatten(proplists:get_all_values(C, Clients))}
                                                          || C <- ClientFuns],
    ClientsByMods = [{M, C} || C <- ClientFuns,
                                (M = fun_mod_name(C)) =/= CallbackModName],
    Mods = proplists:get_keys(ClientsByMods),
    [begin
        FunList = [{[], {C, proplists:get_value(C, List)}}
                          || C <- proplists:get_all_values(M, ClientsByMods)],
        draw_module(Dev, FunList, M, CallbackModName) end || M <- Mods],
    ok.

%% @spec draw_module(device(), FunList, atom(), atom()) -> Result
%% where
%%       FunList = [{Clients :: [function()], {function(), CalledFuns}}]
%%       CalledFuns = [{function(), Category :: atom()}]
%%       Result = [{Clients :: [function()], {function(), Category :: atom()}}]
draw_module(Dev, FunList, ModName, CallbackModName) ->
    SubgraphStyleColor = case ModName of
        gen_server      ->
            "style=filled;color=lightsteelblue1;";
        CallbackModName ->
            "style=filled;color=lightgrey;";
        _               ->
            ""
    end,
    io:format(Dev, "subgraph cluster_~p {~s", [ModName, SubgraphStyleColor]),
    io:format(Dev, "label=\"~p.erl\";~n", [ModName]),
    FunCatList = draw_functions(Dev, FunList, ModName, []),
    io:format(Dev, "}~n", []),
    FunCatList.

%% @spec draw_functions(device(), FunList, atom(), Result) -> Result
%% where
%%       FunList = [{Clients :: [function()], {function(), CalledFuns}}],
%%                  CalledFuns = [{function(), Category :: atom()}]
%%       Result = [{Clients :: [function()], {function(), Category :: atom()}}]
draw_functions(_Dev, [], _ModName, FunCatList) ->
    FunCatList;
draw_functions(Dev, [{Clients, {Fun, CalledFuns}} | FunList], ModName, FunCatList) ->
    FunCat = case CalledFuns of
        [] ->
            FunName = fun_name_string(Fun),
            Cat = case ModName of
                gen_server ->
                    io:format(Dev, "~p [color=white,style=filled];~n",
                                                             [FunName]),
                    ?Fun:name(Fun);
                _ ->
                    io:format(Dev, "~p [color=white,style=filled];~n",
			      [FunName]),
                    undefined
            end,
            {Fun, Cat};
        _ ->
            FunName = fun_name_string(Fun),
            Cat = category(CalledFuns),
            NodeStyleColor = case Cat of
                cast  ->
                    "[shape=\"box\",color=black];";
                call  ->
                    "[shape=\"oval\",color=black];";
                start_link ->
                    "[shape=\"Mrecord\",color=black];";
                start ->
                    "[shape=\"Mrecord\",color=black];";
                mixed ->
                    "[shape=\"hexagon\",color=black];";
                _     ->
                    "[color=white,style=filled];"
            end,
            io:format(Dev, "~p ~s~n", [FunName, NodeStyleColor]),
            draw_communication(Dev, FunName, CalledFuns),
            {Fun, Cat}
    end,
    draw_functions(Dev, FunList, ModName, [{Clients, FunCat} | FunCatList]).

draw_communication(_, _, []) -> ok;
draw_communication(Dev, FunName, [{Fun, Cat} | CalledFunctions]) ->
    CalledFunName = fun_name_string(Fun),
    io:format(Dev, "~p -> ~p;~n", [FunName, CalledFunName]),
    case Cat of
        call ->
            io:format(Dev, "~p -> ~p [style=dashed];~n",
                                                 [CalledFunName, FunName]);
        _ -> ok
    end,
    draw_communication(Dev, FunName, CalledFunctions).

fun_name_string(Fun) ->
    atom_to_list(fun_mod_name(Fun)) ++ ":" ++ atom_to_list(?Fun:name(Fun)) ++
                                       "/" ++ integer_to_list(?Fun:arity(Fun)).

%% @spec merge_fun_lists(Functions, ClientsWFuns, Result) -> Result
%% where
%%    Functions = [{Clients :: [function()], function()}]
%%    ClientsWFuns = CalledFuns::[{function(), Category :: atom()}]
%%    Result = [{Clients :: [function()], Function}]
%%    Function = {function(), [{function(), Category :: atom()}]}
merge_fun_lists([], _, Result) ->
    Result;
merge_fun_lists([{Clients, F} | Funs], ClientsWFuns, Result) ->
    NewVal = case proplists:lookup(F, ClientsWFuns) of
        none -> {F, []};
        FwC -> FwC
    end,
    merge_fun_lists(Funs, ClientsWFuns, [{Clients, NewVal} | Result]).

%% @spec category(CalledFuns) -> Result
%% where
%%       CalledFuns = [{function(), Category :: atom()}]
%%       Result = Category :: atom()
category(CalledFuns) ->
    case length(lists:usort([C || {_, C} <- CalledFuns])) of
        0 -> undefined;
        1 -> {_, Cat} = hd(CalledFuns), Cat;
        _ -> mixed
    end.

fun_mod_name(Function) ->
    [Mod] = ?Query:exec(Function, ?Fun:module()),
    ?Mod:name(Mod).

server_starts(CallBackModName, GenServerMod) ->
    StartLink = ?Query:exec(GenServerMod, ?Fun:find(start_link, 4)),
    Start = ?Query:exec(?Query:seq(?Mod:find(gen_server),
                                                 ?Fun:find(start, 4))),
    StartApps = ?Query:exec(StartLink ++ Start, ?Fun:applications()),
    AppsWServerNames = [callback_mod_name(StartApp) || StartApp <- StartApps],
    Starts = [{SNames, App} || {SNames, MNames, App} <- AppsWServerNames, 
                                       lists:member(CallBackModName, MNames)],
    lists:usort(lists:flatten([SNames || {SNames, _} <- Starts])).

callback_mod_name(StartApp) ->
    [SName, ModNameExpr | _ArgList] = 
        ?Query:exec(?Expr:fun_app_args(StartApp), ?Expr:children()),
    ModNames = [?Expr:value(Expr) ||
                 Expr <- ?Dataflow:reach([ModNameExpr], [back]),
                                              ?Expr:type(Expr) =:= atom],
    ServerNameExpr = case ?Expr:type(SName) of
        tuple-> 
            [Child] = ?Query:exec(SName, ?Expr:child(2)),
            Child;
        _ ->
            SName
    end,
    ServerNames = [?Expr:value(Expr) || 
                   Expr <- ?Dataflow:reach([ServerNameExpr], [back]),
                                               ?Expr:type(Expr) =:= atom],
    {ServerNames, ModNames, StartApp}.

gen_server_fun_clients(ServerNames, {FunName, Arity}, GenServerMod) ->
    case F = ?Query:exec(GenServerMod, ?Fun:find(FunName, Arity)) of
        [] -> [];
        _  -> [Fun] = F,
            FunApps = ?Query:exec(Fun, ?Fun:applications()),
            SNameArgs = [get_server_name_args(App) || App <- FunApps],
            Apps = [App || {Names, App} <- SNameArgs,
                                    ServerNames -- Names =/= ServerNames],
            [{[calling_fun(A) || A <- Apps], Fun}]
    end.

%% returns the server name from a gen_server:... application
get_server_name_args(App) ->
    [NameArg | _] =
        ?Query:exec(?Expr:fun_app_args(App), ?Expr:children()),
    NameExpr = case ?Expr:type(NameArg) of
        tuple-> 
            [Child] = ?Query:exec(NameArg, ?Expr:child(2)),
            Child;
        _ ->
            NameArg
    end,
    Names = case ?Expr:type(NameExpr) of
        atom ->
            [?Expr:value(NameExpr)];
        variable ->
            Reached = ?Dataflow:reach([NameExpr], [back]),
            [?Expr:value(R) || R <- Reached, ?Expr:type(R) =:= atom];
        _ -> []
    end,
    {Names, App}.

calling_fun(App) ->
    [Fun] = ?Query:exec(App,
             ?Query:seq([?Expr:clause(), ?Clause:form(), ?Form:func()])),
    Fun.
