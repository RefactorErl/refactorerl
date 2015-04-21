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

%%% @author Viktoria Fordos <f-viktoria@elte.hu>

%%% @doc This action is designed to limit the access to the database.
%%%
%%% In the case of any job should not be requested, then the following command 
%%% should be called: <pre>wf:wire(#toggle_access_to_db{enable = false}).</pre>
%%% 
%%% In the case of any job can be requested, then the following command 
%%% should be called: <pre>wf:wire(#toggle_access_to_db{enable = true}).</pre>

-module(action_toggle_access_to_db).
-compile (export_all).
-include_lib ("nitrogen/include/wf.hrl").
-include("records.hrl").

-vsn("$Rev: 9568 $ ").

render_action(#toggle_access_to_db{enable = true}) ->
    wf:session(disablethings,undefined),
    "$('input.need_db, button.need_db, select.need_db').removeAttr('disabled');
     $('input.need_db, button.need_db, select.need_db').removeClass('disabled');
     $('div.need_db').css('display','none');
     $('a.need_db, img.need_db').css('display','');";
render_action(#toggle_access_to_db{enable = false}) ->
    wf:session(disablethings,true),
    "$('input.need_db, button.need_db, select.need_db').attr('disabled', 'disabled');
     $('input.need_db, button.need_db, select.need_db').addClass('disabled');
     $('div.need_db').css('display','block');
     $('a.need_db, img.need_db').css('display','none');".
