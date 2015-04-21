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

%%% @author Judit Koszegi <kojqaai@inf.elte.hu>

-module(web_helper).
-vsn("$Rev: 10780 $ ").

-export([start_yaws/1]).
-export([get_index_yaws_folder/0]).
-export([convert_list_to_integer/1, string_to_ip/1]).

-export([error_text/2]).

-include("ui.hrl").
-include_lib("referl_core/include/core_export.hrl").

error_text(bound_port, Port)->
    io_lib:format("The given port (~p) has been already bound by another application.", [Port]);
error_text(no_right_associated_to_port, Port) ->
    io_lib:format("you don't have appropriate rights to use the given port (~p).", [Port]).
%%% ============================================================================
%%% Start, configure and stop yaws web server

%% @doc Configure and start yaws web server.
start_yaws(["from_script", YPath, YName, YPort, YListen]) ->
    try
        YawsPathProp = {yaws_path,YPath},
        YawsNameProp = {yaws_name,YName},
        YawsPortProp = {yaws_port, convert_list_to_integer(YPort)},
        YawsListenProp = {yaws_listen, string_to_ip(YListen)},
        YawsIndexFunModuleProp = {yaws_index_fun_module, ?MODULE},
        YawsIndexFunNameProp = {yaws_index_fun_name, get_index_yaws_folder},
        AllProp = [YawsNameProp,YawsPortProp,YawsListenProp,
                   YawsIndexFunModuleProp,YawsIndexFunNameProp],
        case YPath of
            "no_path" -> start_yaws(AllProp);
            _ -> start_yaws([YawsPathProp|AllProp])
        end
    catch
        RefErr ->
            io:format(?Error:error_text(RefErr) ++ "~n"),
            exit("Cannot start yaws.")
    end;

start_yaws(Proplist) ->
    case is_yaws_started() of
        true -> {error, yaws_already_started};
        false -> start_yaws0(Proplist)
    end.

start_yaws0(Proplist) ->
    case (proplists:get_value(yaws_path,Proplist)) of
        undefined -> ok;
        YawsDir ->
            case code:add_path(YawsDir) of
                true -> ok;
                {error,_} ->
                    throw(?RefError(file_notdir,[YawsDir]))
            end
    end,
    case code:ensure_loaded(yaws) of
        {error,_} -> throw(?RefErr0r(yaws_not_loaded));
            _ -> ok
    end,
    Port = proplists:get_value(yaws_port,Proplist,8001),
    validate_port_number(Port),
    Listen = proplists:get_value(yaws_listen,Proplist,{0,0,0,0}),
    validate_ip_tuple(Listen),
    Name0 = proplists:get_value(yaws_name,Proplist,"refactorErl"),
    Name = case is_atom(Name0) of
               true -> atom_to_list(Name0);
               false -> Name0
           end,
    validate_name(Name),
    YawsIndexFunModule = proplists:get_value(yaws_index_fun_module,
                                             Proplist,web_helper),
    YawsIndexFunName = proplists:get_value(yaws_index_fun_name,Proplist,
                                           get_index_yaws_folder),
    YawsAppMod = proplists:get_value(yaws_app_mod,Proplist,no_appmod),
    Nitrogen = proplists:get_value(nitrogen_prop,Proplist,no_nitrogen),
    YawsPostSize = proplists:get_value(yaws_partial_post_size,Proplist,1024),

    GconfList = case Nitrogen of
                    no_nitrogen -> [{id, Name},
                                    {servername, Name},
                                    {logdir, ?MISC:data_dir()}];
                    with_nitrogen ->[
                                     {ebin_dir,
                [?NITRO_CORE:get_nitrogen_site_dir()++"/ebin"]},
                                     {include_dir,
                [?NITRO_CORE:get_nitrogen_site_dir()++"/include"]},
                                     {id, Name},
                                     {servername,Name},
                                     {logdir, ?MISC:data_dir()}]
                end,

    SconfList =
    case YawsAppMod of
        no_appmod ->[{docroot, apply(YawsIndexFunModule,YawsIndexFunName,[])},
                     {port, Port},
                     {listen, Listen}];
        _ ->        [{docroot, apply(YawsIndexFunModule,YawsIndexFunName,[])},
                     {port, Port},
                     {listen, Listen},
                     {appmods, YawsAppMod},
                    {partial_post_size,YawsPostSize}]
    end,
    DocRoot = apply(YawsIndexFunModule,YawsIndexFunName,[]),
    case proplists:get_value(only_config, Proplist, false) of
        false ->
            yaws:start_embedded(DocRoot,SconfList,GconfList);
        true ->
            yaws_api:embedded_start_conf(DocRoot, SconfList, GconfList, Name)
    end.


validate_port_number(Port) ->
    case is_number(Port) of
        true -> is_unbound_port(Port);
        false ->throw(?RefError(port_format_error,[io_lib:format("~p",[Port])]))
    end.

validate_ip_tuple(Listen) ->
    Str = io_lib:format("~p",[Listen]),
    case Listen of
        {A,B,C,D} ->
            case is_number(A) andalso is_number(B) andalso
                is_number(C) andalso is_number(D) of
                true -> ok;
                false -> throw(?RefError(ip_format_error,[Str]))
            end;
        _ -> throw(?RefError(ip_format_error,[Str]))
    end.

validate_name(Name) ->
    case is_string(Name) of
        true -> ok;
        false ->throw(?RefError(name_format_error,[io_lib:format("~p",[Name])]))
    end.

is_unbound_port(Port)->
    % business logic and also some fragments were copied from yaws_ctl.erl
    case gen_tcp:connect({127,0,0,1}, Port,
                         [{active, false},
                          {reuseaddr, true},
                          binary,
                          {packet, 2}], 2000) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            throw(?LocalError(bound_port, Port));
        {error,eacces} ->
            throw(?LocalError(no_right_associated_to_port, Port));
        {error, _} ->
            true
    end.

is_char(Ch) ->
    if Ch < 0 -> false;
       Ch > 255 -> false;
       true -> true
    end.

is_string(Str) ->
    case is_list(Str) of
        false -> false;
        true -> lists:all(fun is_char/1, Str)
    end.

get_index_yaws_folder() ->
    Path0 = filename:split(filename:dirname(code:which(web_helper))),
    Path =
        filename:join([
                       filename:join(
                         lists:sublist(Path0,length(Path0)-1)),
                      "web"]),
    Path.

convert_list_to_integer(String) ->
    try
        list_to_integer(String)
    catch _A:_B ->
            throw(?RefError(list_to_integer_error,[String]))
    end.

take_and_drop(L) ->
    {convert_list_to_integer(lists:takewhile(fun(X) -> not(X == $.) end,L)),
     (lists:dropwhile(fun(X) -> not(X == $.) end,L))}.


string_to_ip(YPort) ->
    L1 = YPort,
    {Number1,L2} = take_and_drop(L1),
    case L2 of
        [] ->  throw(?RefError(ip_format_error,[YPort]));
        _ ->
            {Number2,L3} = take_and_drop(tl(L2)),
            case L3 of
                [] -> throw(?RefError(ip_format_error,[YPort]));
                _ ->  {Number3,L4} = take_and_drop(tl(L3)),
                      case L4 of
                          [] -> throw(?RefError(ip_format_error,[YPort]));
                          _ ->  {Number4,_L5} = take_and_drop(tl(L4)),
                                {Number1,Number2,Number3,Number4}
                      end
            end
    end.

is_yaws_started()->
    lists:any(fun({yaws,_,_})->
                true;
                (_)->false
            end, application:which_applications()).