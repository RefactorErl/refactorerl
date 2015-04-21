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

-module (static).
-compile(export_all).
-include_lib("referl_ui/include/nitrogen.hrl").
-include_lib("nitrogen/include/wf.hrl").
-include("records.hrl").
-vsn("$Rev: 6466 $ ").


main() -> 
    case wf:q(file) of
        undefined-> <<>>;
        Name->
            File=filename:join([?NITRO_CORE:get_nitrogen_index(),
                    Name]),
               case filename:extension(File) of
                   ".js"->wf:content_type("application/javascript");
                   ".css"->wf:content_type("text/css");
                   _ -> ok
               end,
               wf_context:header("Expires", expire()),
               binary_to_list(content(File))
    end.

event(_) -> ok.

content(File) -> 
    case file:read_file(File) of
            {ok, Binary} -> Binary;
            {error,_}-> <<>>
    end.

expire()->
%% Calculate expire date far into future...
    Seconds = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    TenYears = 10 * 365 * 24 * 60 * 60,
    Seconds1 = calendar:gregorian_seconds_to_datetime(Seconds + TenYears),
    httpd_util:rfc1123_date(Seconds1).
