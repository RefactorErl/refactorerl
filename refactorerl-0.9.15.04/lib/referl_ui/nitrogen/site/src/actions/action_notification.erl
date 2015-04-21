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
-module (action_notification).
-include_lib("nitrogen/include/wf.hrl").
-include("records.hrl").
-compile(export_all).

render_action(Record) ->

    %Color=case Record#notification.type of
        %notification -> "#33FF33";
        %warning -> "#FFFF33";
        %_ -> "#FF3333"
    %end,
    Text=dequot(Record#notification.text),
    Type=case Record#notification.type of
        notification -> "notice";
        warning -> "warning";
        _ -> "error"
    end,
    %Id=wf:temp_id(),
    %wf:remove(wf:session(last_notification)),
    %wf:session(last_notification,Id),
    %InnerPanel = #panel {class=notification, style="background-color: "++Color, body=[
            %#link { class=close_button, text="Close", actions=#event { type=click, target=Id, actions=#fade { speed=200 } } },
            %#panel { class=textarea, body=Text}
        %]},
    %Notif=#panel { id=Id, style="display: none;", body=InnerPanel},
    %wf:insert_top(pagetop,Notif),
    %wf:wire(Id, #appear { speed=200 }),
    
    "
    if (typeof lasttoast != 'undefined') $().toastmessage('removeToast', lasttoast);
    lasttoast=$().toastmessage('showToast', {
    text     : '"++Text++"',
    sticky   : true,
    position:  'bottom-right',
    type     : '"++Type++"'
    });".
    
dequot([])  -> [];
dequot([C|CS]) when C==$' -> [$\\,$'|dequot(CS)];
dequot([C|CS]) when C==$\n -> [dequot(CS)];
dequot([C|CS]) -> [C|dequot(CS)].
