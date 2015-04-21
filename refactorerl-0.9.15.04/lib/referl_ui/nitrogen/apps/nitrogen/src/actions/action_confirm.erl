% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_confirm).
-include_lib ("wf.hrl").
-compile(export_all).

render_action(Record) -> 
    TriggerPath = Record#confirm.trigger,
    TargetPath = Record#confirm.target,
    Delegate = Record#confirm.delegate,
    [
        wf:f("if (confirm(\"~s\")) {", [wf:js_escape(Record#confirm.text)]),
        #event { postback=Record#confirm.postback, trigger=TriggerPath, target=TargetPath, delegate=Delegate },
        Record#confirm.actions,
        "}"
    ].

