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

%%% @author Viktoria Fordos <v@fviktoria.hu>

%% -*- mode: nitrogen -*-

-module (nitrogen_lib).
-include_lib("referl_ui/include/nitrogen.hrl").
-include_lib("nitrogen/include/wf.hrl").
-include("records.hrl").
-vsn("$Rev: 9568 $ ").

-export([logout/1, logout_link/1, main/1]).

logout(User)->
    reflib_ui_router:del_msg_handler(wf:session(jobinfo)),
    wf:session(jobinfo,undefined),
    ?NITRO_SERVICES:delete_dependency_graphs_files(User),
    ?NITRO_SERVICES:stop_running_queries(User),
    wf:logout(),
    wf:redirect("/index").

main(TemplateName=[C|_]) when is_integer(C)->
    if
        (TemplateName=="main.html") orelse (TemplateName=="files.html")-> 
             OldHash=wf:session(db_hash),
             NewHash=?NITRO_HELPER:get_database_hash(),
             if
                OldHash/=NewHash -> ?NITRO_SERVICES:need_to_update();
                true -> ok
             end;
        true -> ok
    end,
    case reflib_ui_router:can_read_db() of
        true -> wf:wire(#toggle_access_to_db{enable=true});
        false -> 
            wf:wire(#toggle_access_to_db{enable=false}),
            wf:wire(#notification{type=notification,
                text="Database in use, most features are disabled."})
    end,
    Jobinfo=wf:session(jobinfo),
    if
        Jobinfo/=undefined ->
            reflib_ui_router:del_msg_handler(Jobinfo);
        true -> ok
    end,
    {ok,Pid}=wf:comet(fun() -> jobinfo_loop(TemplateName) end),
    reflib_ui_router:add_msg_handler(Pid),
    wf:session(jobinfo,Pid),
    case wf:role(auth_users) of
        true ->wf:session(flashes, undefined),
               #template { file=?NITRO_CORE:get_nitrogen_site_dir()
                         ++"/templates/"++TemplateName };
        false ->wf:redirect_to_login("/index")
    end.
    

jobinfo_loop(TemplateName) ->
    receive 
        {global,jobinfo,{{_,modifier,State,_},Msg}} 
        when (State==started) andalso (Msg/=[])->
            wf:wire(#toggle_access_to_db{enable=false}),
            wf:wire(#notification{type=notification,text=Msg}),
            wf:flush(),
            jobinfo_loop(TemplateName);
        {global,jobinfo,{{_,modifier,State,_},Msg}} 
        when (State==finished) andalso (Msg/=[]) ->
            ?NITRO_SERVICES:need_to_update(),
            if
                TemplateName=="main.html" -> main:update_source();
                true -> ok
            end,
            wf:wire(#toggle_access_to_db{enable=true}),
            wf:wire(#notification{type=notification,text=Msg}),
            wf:flush(),
            jobinfo_loop(TemplateName);
         _ -> jobinfo_loop(TemplateName)
    end.

logout_link(User)->
        #link {text="Log out "++User, 
           title = "Log out "++User,
           postback=logout }.
