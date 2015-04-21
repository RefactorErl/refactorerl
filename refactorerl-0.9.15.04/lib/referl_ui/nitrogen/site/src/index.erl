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
-module (index).
-compile(export_all).
-include_lib("referl_ui/include/nitrogen.hrl").
-include_lib("nitrogen/include/wf.hrl").
-vsn("$Rev: 9568 $ ").

-define(PW_TAB, filename:join(mnesia:system_info(directory),"adminpass")).

%%% ============================================================================
%%% HTML output

main() ->
    #template { file=?NITRO_CORE:get_nitrogen_site_dir()++
                               "/templates/loginpage.html" }.

title() -> "RefactorErl Queries".

body() ->
    #panel{ body=[
       form()
    ]}.

form() ->
    Form=[% Create a label
          #label { text="Username" },
          % Create a textbox
          #textbox { id=usernameTextBox, next=loginButton },
          % Create a button with a postback of 'login'
          #button { id=loginButton, 
                    text="Ok", 
                    postback=login, 
                    class=referl_button },

          #label { id=passwordlabel, text="" },
          #panel{id=password},
          #panel{id=wrongpw}],
    % Validate the text in the textbox
    wf:wire(loginButton, 
            usernameTextBox, 
            #validate { validators=[#is_required { text="Required." }]}),
    Form.

%%% ============================================================================
%%% Handlers for postback events    

event(login) ->
    User=wf:q(usernameTextBox),
    Pass=wf:q(password),
    RestrictedMode= ?NITRO_CORE:get_restricted_mode(),
    PasswordIsCorrect=if
        not RestrictedMode -> true;
        Pass==undefined -> checkPassword("");
        true -> checkPassword(Pass)
    end,
    if
        (Pass==undefined) and (not PasswordIsCorrect) and (User=="admin") -> 
            askForPassword();
        (not PasswordIsCorrect) and (User=="admin") -> 
            ok;
        true ->
            wf:replace(wrongpw,#panel{id=wrongpw,body=""}),
            wf:session(username, User),
            wf:role(auth_users, true),
            if
                (User=="admin") or (not RestrictedMode) -> wf:role(admin, true);
                true -> ok
            end,
            wf:user(User),
            wf:redirect_from_login("/main")
    end.

askForPassword() ->
    wf:replace(passwordlabel,#label { id=passwordlabel, text="Password" }),
    wf:replace(password, #password { id=password, next=loginButton }),
    wf:replace(wrongpw,#panel{id=wrongpw,body=""}).

checkPassword(Pass) ->
    dets:open_file(?PW_TAB,[]),
    Result=dets:match_object(?PW_TAB,{'_'}),
    dets:close(?PW_TAB),
    if
        Result==[] -> 
            wf:wire(#script{script="alert('No admin password set!');"}),false;
        true -> 
            {Md5}=hd(Result),
            WrittenPass=erlang:md5(Pass),
            if
                WrittenPass==Md5 -> true;
                true -> 
                    wf:replace(wrongpw,#panel{id=wrongpw,
                                              body="Wrong password!"}),
                    false
            end
    end.

makeadminpassword(Pass) ->
    dets:open_file(?PW_TAB,[]),
    dets:delete_all_objects(?PW_TAB),
    dets:insert(?PW_TAB,{erlang:md5(Pass)}),
    dets:close(?PW_TAB).
