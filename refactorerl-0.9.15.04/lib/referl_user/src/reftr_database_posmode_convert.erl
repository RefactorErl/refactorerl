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

%%% ============================================================================
%%% Module information

%%% @doc This module implements the preparation for converting positioning
%%% modes of the database. Before converting the database the module checks
%%% if the conversion is possible and if so how long does it take.
%%%
%%% @author Viktoria Fordos <f-viktoria@elte.hu>
%%% @author Marton Wolosz <wolosz@inf.elte.hu>
%% Created: 2013.04.09.

-module(reftr_database_posmode_convert).
-export([prepare/1]).
-export([error_text/2]).
-include("user.hrl").

prepare(ToPos) ->
    ConvMode = get_conv_mode(?Syn:get_env(db_posmode), ToPos),
    ConvTime = refcore_convert_pos:estimated_conversion_time(ConvMode),
    TimeWarning = if ConvTime >= 5 -> io_lib:format(
                                     "Converting takes about ~p second! ",
                                     [ConvTime]);
                     true         -> ""
                  end,
    Question = [{format, yesno}, 
               {type, yesno},
               {text, TimeWarning ++
                      "Are you sure to preform database conversion?"},
               {default, no}],
    case ?Transform:question([Question]) of
        [yes] ->
            [fun()->call_convert(ToPos) end];
        _ ->
            throw(?RefErr0r(cancelled))
    end.

call_convert([abs]) ->
    refcore_convert_pos:to_abs();
call_convert([rel]) ->
    refcore_convert_pos:to_rel();
call_convert(To)    ->
    throw(?LocalError(invalid_posmode, To)).

get_conv_mode([abs], [rel]) ->
    abs_to_rel;
get_conv_mode([rel], [abs]) ->
    rel_to_abs;
get_conv_mode([abs], [abs]) ->
    throw(?LocalError(posmode_already_in_use, ["abs"]));
get_conv_mode([rel], [rel]) ->
    throw(?LocalError(posmode_already_in_use, ["rel"]));
get_conv_mode(_, To)        ->
    throw(?LocalError(invalid_posmode, To)).

error_text(posmode_already_in_use, [PosM]) ->
    ["The given positioning mode (", PosM, ") is already in use."];
error_text(invalid_posmode, [To])      ->
    ["The given positioning mode (", io_lib:print(To), ") is invalid. ",
     "The available modes are: ",
     string:join(lists:map(fun format_pos_modes/1, ?availablePosModes), ", "),
     "."].

format_pos_modes(PMod) ->
    lists:flatten(io_lib:format("~p", [PMod])).
