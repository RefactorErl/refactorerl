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
-module (element_textarea_autocomplete).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include("records.hrl").
-vsn("$Rev: 9568 $ ").

reflect() -> record_info(fields, textarea_autocomplete).

render_element(Record) ->
	% Get properties...
	Delegate = Record#textarea_autocomplete.delegate,
	Tag = Record#textarea_autocomplete.tag,
	Anchor = Record#textarea_autocomplete.anchor,
	AutoCompleteMinLength = Record#textarea_autocomplete.minLength,
	AutoCompleteDelay = Record#textarea_autocomplete.delay,
	
	% Write out the script to make this element autocompletable...
	AutoCompleteEnterPostbackInfo = wf_event:serialize_event_context(
									  {autocomplete_enter_event, Delegate, Tag}, 
									  Anchor, 
									  undefined, 
									  ?MODULE),
	AutoCompleteSelectPostbackInfo = wf_event:serialize_event_context(
									   {autocomplete_select_event,Delegate,Tag},
									   Anchor, 
									   undefined, 
									   ?MODULE ),
	
	AutoCompleteOptions = {struct, [
		{dataType, <<"json">>},
		{minLength, AutoCompleteMinLength},
		{delay, AutoCompleteDelay}]},
	
	AutoCompleteScript = #script {
		script = wf:f("Nitrogen.$autocomplete('~s', ~s, '~s', '~s');", [
		  Anchor,
		  mochijson2:encode(AutoCompleteOptions),
		  AutoCompleteEnterPostbackInfo, 
		  AutoCompleteSelectPostbackInfo])},
	
	wf:wire(AutoCompleteScript),
	
	% Render as a textarea.
	Value = wf:html_encode(Record#textarea_autocomplete.text, 
						   Record#textarea_autocomplete.html_encode),
	wf_tags:emit_tag(textarea, 
					 Value, [{class, 
							  [textarea_autocomplete, 
							   Record#textarea_autocomplete.class]},
							 {style, Record#textarea_autocomplete.style}]).

event({autocomplete_select_event, Delegate, SelectTag})->
	SelectItem = mochijson2:decode(wf:q(select_item)),
	Module = wf:coalesce([Delegate, wf:page_module()]),
	Module:autocomplete_select_event(SelectItem, SelectTag);

event({autocomplete_enter_event, Delegate, EnterTag})->
	SearchTerm = wf:q(search_term),
	wf_context:type(first_request),
	wf:content_type("application/json"),
	Module = wf:coalesce([Delegate, wf:page_module()]),
	wf_context:data([Module:autocomplete_enter_event(SearchTerm, EnterTag)]).

