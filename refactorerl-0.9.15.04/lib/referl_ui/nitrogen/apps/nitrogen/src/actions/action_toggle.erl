% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_toggle).
-include_lib ("wf.hrl").
-compile(export_all).

render_action(Record) ->
    #jquery_effect {
        type=toggle,
        effect = Record#toggle.effect,
        options = Record#toggle.options,
        speed = Record#toggle.speed,
        actions = Record#toggle.actions
    }.
