% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_animate).
-include_lib ("wf.hrl").
-compile(export_all).

render_action(Record) ->
    #jquery_effect {
        type=animate,
        options = Record#animate.options,
        speed = Record#animate.speed,
        easing = Record#animate.easing,
        actions = Record#animate.actions
    }.
