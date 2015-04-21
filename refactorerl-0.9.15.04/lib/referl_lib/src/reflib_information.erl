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
%%% @doc <b>Standard textual informations.</b>
%%%
%%% The module is designed to inform the user of the current running processes.
-module(reflib_information).

-vsn("$Rev: 9568 $ ").

-export([info_text/1]).

%% @type info_code() = {JobName, JobType, JobStatus, JobArgs}
%%       JobName = atom()
%%       JobType = modifier | non_modifier
%%       JobStatus = started | finished
%%       JobArgs = any()
%% @spec info_text( Code ) -> Text
%%       Code = info_code()
%%       Text = string()
%% @doc Returns a textual description of the info_code term.
info_text({synchronize, _, started, _})->
    "Database synchronization has been started.";
info_text({synchronize, _, finished, _})->
    "Database synchronization has been finished.";
info_text({add_env, _, started, _})->
    "Adding an environmental node has been started.";
info_text({add_env, _, finished, _})->
    "Adding an environmental node has been finished.";
info_text({set_env, _, started, _})->
    "The setting of the environmental node has been started.";
info_text({set_env, _, finished, _})->
    "The setting of the environmental node has been finished.";
info_text({del_env, _, started, _})->
    "The removal of the environmental node has been started.";
info_text({del_env, _, finished, _})->
    "The removal of the environmental node has been finished.";
info_text({del_env_val, _, started, _})->
    "The removal of the value of the environmental node has been started.";
info_text({del_env_val, _, finished, _})->
    "The removal of the value of the environmental node has been finished.";
info_text({add_dir, _, started, [FileName]}) when 
  is_list(FileName) andalso is_integer(hd(FileName))->
    "Adding "++FileName++" to database has been started.";
info_text({add_dir, _, finished, [FileName]}) when
  is_list(FileName) andalso is_integer(hd(FileName))->
    "Adding "++FileName++" to database has been finished.";
info_text({drop_dir, _, started, [FileName]}) when
  is_list(FileName) andalso is_integer(hd(FileName))->
    "The removal of the "++FileName++" from database has been started.";
info_text({drop_dir, _, finished, [FileName]}) when
  is_list(FileName) andalso is_integer(hd(FileName))->
    "The removal of the "++FileName++" from database has been finished.";
info_text({load_beam, _, started, _})->
    "Adding a BEAM file to database has been started.";
info_text({load_beam, _, finished, _})->
    "Adding a BEAM file to database has been finished.";
info_text({undo, _, started, _})->
    "The undo procedure has been started.";
info_text({undo, _, finished, _})->
    "The undo procedure has been finished.";
info_text({restore, _, started, _})->
    "The restore procedure has been started.";
info_text({restore, _, finished, _})->
    "The restore procedure has been finished.";
info_text({backup, _, started, _})->
    "The backup procedure has been started.";
info_text({backup, _, finished, _})->
    "The backup procedure has been finished.";
info_text({reset, _, started, _})->
    "The initialization of the tool has been started.";
info_text({reset, _, finished, _})->
    "The initialization of the tool has been finished.";
info_text({transform, non_modifier, started, [semantic_query, Args]})->
    QueryStr = proplists:get_value(querystr, Args),
    "A semantic query ("++QueryStr++") has been started.";
info_text({transform, non_modifier, finished, [semantic_query, Args]})->
    QueryStr = proplists:get_value(querystr, Args),
    "A semantic query ("++QueryStr++") has been finished.";
info_text(_)->
    "".