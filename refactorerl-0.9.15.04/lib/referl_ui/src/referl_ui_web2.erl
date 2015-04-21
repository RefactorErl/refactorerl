%%% -*- coding: utf-8 -*-

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

%%% @author Tibor Pusztai <kondi@elte.hu>
%%% @author Viktoria Fordos <f-viktoria@elte.hu>


-module(referl_ui_web2).
-behaviour(application).

-export([start/2, stop/1]).

%% Public API
-export([start/0, start/1, stop/0, set_web2_pass/2,
         show_dynfuns/0, show_dynfuns/1, delete_dynfuns/0]).

%% Required for web_helper:start_yaws
-export([get_index/0]).

-define(WEB2ROOT, "web2").
-define(DEF_BROWSER_ROOT, "no_path").

-vsn("$Rev$").

-include("ui.hrl").
-include_lib("referl_core/include/core_export.hrl").

%%% ============================================================================
%%% Start and stop Web2 framework

show_dynfuns() ->
    show_dynfuns([]).

show_dynfuns(Options) ->
    referl_ui_web2_services:show_dynfuns(Options).

delete_dynfuns() ->
    referl_ui_web2_services:delete_dynfuns().

start() ->
    start([]).

start(["from_script", YPath, YPort, YListen, RestrictedMode | BrowserRoot]) ->
    start(YPath, YPort, YListen, BrowserRoot, ?MISC:to_atom(RestrictedMode));

start(PropList) ->
    YPort = proplists:get_value(yaws_port, PropList, "8001"),
    YListen = proplists:get_value(yaws_listen, PropList, "127.0.0.1"),
    YPath = proplists:get_value(yaws_path, PropList, "no_path"),
    BRoot = proplists:get_value(browser_root, PropList, [?DEF_BROWSER_ROOT]),
    Restricted = proplists:get_value(restricted_mode, PropList, false),
    start(YPath, YPort, YListen, BRoot, Restricted).

start(YPath, YPort, YListen, BrowserRoot0, RestrictedMode) ->
    case catch start_web2_app(handle_multi_browser_roots(BrowserRoot0),
                              RestrictedMode,
                              make_proplist(YPath, YPort, YListen)) of
        ok -> ok;
        {error, Reason} ->
            io:format("~p~n", [Reason]),
            exit("Cannot start web2.");
        RefErr ->
            io:format(?Error:error_text(RefErr) ++ "~n"),
            io:format("~p~n", [RefErr]),
            exit("Cannot start web2.")
    end.

set_web2_pass(User, Password) ->
    referl_ui_web2_services:set_web2_pass(User, Password).

stop()->
    application:stop(?WEB2),
    application:unload(?WEB2).

make_proplist(YPath, YPort, YListen) ->
    YawsPathProp = {yaws_path, YPath},
    YawsNameProp = {yaws_name, default}, %% see https://github.com/klacke/yaws/issues/66
    YawsPortProp = {yaws_port, web_helper:convert_list_to_integer(YPort)},
    YawsListenProp = {yaws_listen, web_helper:string_to_ip(YListen)},
    YawsIndexFunModuleProp = {yaws_index_fun_module, ?MODULE},
    YawsIndexFunNameProp = {yaws_index_fun_name, get_index},
    YawsPostSize = {yaws_partial_post_size, 1024*1024},
    YawsAppMod = {yaws_app_mod, [{"/api", ?WEB2_APPMOD}]},
    AllProp = [YawsNameProp, YawsPortProp, YawsListenProp, YawsIndexFunModuleProp,
               YawsIndexFunNameProp, YawsAppMod, YawsPostSize],
    case YPath of
        "no_path" -> AllProp;
        _ -> [YawsPathProp | AllProp]
    end.

get_base() ->
    Path0 = filename:split(filename:dirname(code:which(?MODULE))),
    Path = filename:join(lists:sublist(Path0, length(Path0) - 1)),
    Path.

get_index() ->
    filename:join([get_base(), ?WEB2ROOT, "app"]).

handle_multi_browser_roots(BrowserRoot0)->
    BrowserRoots = case BrowserRoot0 of
        [?DEF_BROWSER_ROOT] -> [filename:join([get_base(), "..", ".."])];
        [H|_] when not is_list(H) -> [BrowserRoot0];
        _ -> BrowserRoot0
    end,
    lists:filter(fun(AbsPath) ->
        case filelib:is_file(AbsPath) andalso
             filelib:is_dir(AbsPath) of
            true ->
                true;
            false ->
                Report = io_lib:format(
                    "The given path, namely ~p, is not a directory, "
                    "thus it is removed from the list of browser roots.~n",
                    [AbsPath]),
                error_logger:info_report(Report),
                false
        end
    end, [ filename:absname(BR) || BR <- BrowserRoots, BR /= ?DEF_BROWSER_ROOT]).

%%% ============================================================================
%%% Start and stop Web2 application

%% @private
start(normal, _)->
    {ok, BRoot} = application:get_env(?WEB2_APP, browser_root),
    {ok, RestrictedMode} = application:get_env(?WEB2_APP, restricted_mode),
    {ok, YawsProps} = application:get_env(?WEB2_APP, yaws_props),
    case catch web_helper:start_yaws(YawsProps++[{only_config, true}]) of
        %% configure and embed Yaws
        {ok, SCList, GC, ChildSpecs} ->
            %% start the top supervisor of web2
            TopSup = ?WEB2_SUP:start(),
            %% start Yaws's supervisors
            [supervisor:start_child(?WEB2_SUP, Ch) || Ch <- ChildSpecs],
            %% now configure Yaws
            yaws_api:setconf(GC, SCList),
            %% init services
            ?WEB2_SERVICES:start(BRoot, RestrictedMode),
            TopSup;
        E={Mod, _, _} when Mod == web_helper; Mod == ?Error ->
            {error, ?Error:error_text(E)};
        Err ->
            {error, io_lib:format("Unexcepted error occurred: ~p", [Err])}
    end.

%% @private
stop(_)->
    ?WEB2_SERVICES:stop().

web2_appspec()->
    Vsn = case lists:dropwhile(fun({referl_ui,_,_})-> false;
                                  (_) -> true
                               end, application:which_applications()) of
        [] -> "0.9.14.01";
        [{referl_ui, _ , V}|_] -> V
    end,
    {application,
    ?WEB2_APP,
    [{description, "RefactorErl WEB2 Application"},
     {vsn, Vsn},
     {modules,[?WEB2,
               ?WEB2_APPMOD,?WEB2_CACHE,
               ?WEB2_SERVICES,?WEB2_SESSIONS,
               ?WEB2_SUP,?WEB2_WEBSOCKET,
               web_helper]},
     {mod,{?WEB2,[]}},
     {applications,[kernel,stdlib]},
     {registered,[]}]}.

start_web2_app(Broots, RestrictedMode, YawsProps)->
    application:load(web2_appspec()),
    application:set_env(?WEB2_APP, browser_root, Broots),
    application:set_env(?WEB2_APP, restricted_mode, RestrictedMode),
    application:set_env(?WEB2_APP, yaws_props, YawsProps),
    application:start(?WEB2_APP).
