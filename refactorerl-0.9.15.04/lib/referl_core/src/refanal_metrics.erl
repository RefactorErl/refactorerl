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

%%% @doc Metric analyser.
%%%
%%% @author Roland Kiraly <kiralyroland@inf.elte.hu>

-module(refanal_metrics).
-vsn("$Rev$"). % for emacs "
-behaviour(refcore_anal).

-export([schema/0, externs/1, insert/4, remove/4, update/2]).

-include("core.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(Helper, refanal_metrics_helper).
-define(MNodes, metric_nodelist).

%%% @private
schema() -> [].

%%% @private
externs(_) -> [].

%%% @private
insert(Parent, Previous, {Tag, Child}, Next) ->
    try ?Helper:metricmode() of
         true -> Data = node_typer({?Anal:data(Parent),
                 Parent, Previous, {Tag, Child}, Next, [insert]}),
                 [?Helper:add_node(?MNodes, D, unset) || D <- Data];
	 false -> ok
    catch
       _:_ -> ok
    end.

%%% @private
remove(Parent, Prev, {Tag, Child}, Next) ->
    try ?Helper:metricmode() of
         true ->
            Data = node_typer({?Anal:data(Parent),
                 Parent, Prev, {Tag, Child}, Next, [remove]}),
                 [?Helper:add_node(?MNodes, D, unset) || D <- Data];
	    false -> ok
    catch
      _:_ -> ok
    end.

%%% @private
update(Node, #form{type = record}) ->
   try ?Helper:metricmode() of
       true ->
            try
             File = ?Anal:parent(Node),
             [Mod] = ?Graph:path(File, [moddef]),
             ?Helper:add_node(?MNodes, {Mod, {module, data, [update]}}, unset)
           catch
             _:_-> ok
           end;
       false -> ok
   catch
      _:_ -> ok
   end;
update(Node, #form{}) ->
   try ?Helper:metricmode() of
             true ->
                  [Mod] = ?Graph:path(Node, [{form,back}, moddef]),
                  ?Helper:add_node(?MNodes,
                              {Mod, {module, data, [update]}}, unset);
            false -> ok
   catch
      _:_ -> ok
   end;
update(_, #expr{role = attr}) ->
   ok;
update(Node, #expr{})->
   try ?Helper:metricmode() of
            true ->
             try
              [Func] = ?Graph:path(?Anal:parent(Node),
                        [top, {visib, back}, functx,
                        {funcl, back}, fundef]),
              [Mod]  = ?Graph:path(Func,   [{func, back}]),
              ?Helper:add_node(?MNodes, {Func, {function, data, [update]}}, unset),
              ?Helper:add_node(?MNodes, {Mod,  {module,   data, [update]}}, unset)
             catch
              _:_ -> ok
             end;
            false -> ok
   catch
     _:_-> ok
   end;
update(_, _) ->
    ok.

%%% @private
node_typer({Node, Parent, _Previous, {Tag, _Child}, _Next, Transform})->
   case Node of
         #file{type=module} when Tag == form ->
          try
            [Mod] = ?Graph:path(Parent, [moddef]),
            [{Mod, {module, data, Transform}}]
          catch
             _:_-> []
          end;

	#file{type=header} ->
          try
            Mods = ?Graph:path(Parent, [{incl, back}, moddef]),
            [{Mod, {module, data, Transform}} || Mod <- Mods]
          catch
             _:_-> []
          end;

        #file{} ->
          try
            [Mod] = ?Graph:path(Parent, [moddef]),
	    [{Mod, {module, data, Transform}}]
          catch
             _:_-> []
          end;

        #form{type = module} ->
           try
            [Mod] = ?Graph:path(Parent, [{form,back}, moddef]),
            [{Mod, {module, data, Transform}}]
          catch
             _:_-> []
          end;

        #form{type=export} ->
           try
            File = ?Anal:parent(Parent),
            [Mod] = ?Graph:path(File, [moddef]),
            [{Mod, {module, data, Transform}}]
          catch
             _:_-> []
          end;

        #form{type=import} ->
            try
             File = ?Anal:parent(Parent),
             [Mod] = ?Graph:path(File, [moddef]),
             [{Mod, {module, data, Transform}}]
           catch
             _:_-> []
           end;

        #form{type=func} when Tag == funcl ->
           try
            [Func] = ?Graph:path(Parent, [fundef]),
            [Mod]  = ?Graph:path(Func,   [{func, back}]),
            [{Func, {function, data, Transform}},
             {Mod,  {module,   data, Transform}}]
           catch
             _:_-> []
           end;

        #form{type = record} ->
           try
            File = ?Anal:parent(Parent),
            [Mod] = ?Graph:path(File, [moddef]),
            [{Mod, {module, data, Transform}}]
           catch
             _:_-> []
           end;

        #form{} ->
           try
            File = ?Anal:parent(Parent),
            [Mod] = ?Graph:path(File, [moddef]),
            [{Mod, {module, data, Transform}}]
           catch
             _:_-> []
           end;

       #clause{} ->
           try
            [Func] = ?Graph:path(Parent, [functx, {funcl, back}, fundef]),
            [Mod]  = ?Graph:path(Func,   [{func, back}]),
            [{Func, {function, data, Transform}},
             {Mod,  {module,   data, Transform}}]
           catch
             _:_-> []
          end;

      #expr{role = attr} ->
           try
            [Mod] = ?Graph:path(Parent, [top, {eattr,back}, {form,back},
                                         moddef]),
            [{Mod, {module, data, Transform}}]
           catch
             _:_-> []
           end;

       #expr{} ->
           try
            [Func] = ?Graph:path(Parent, [top, {visib, back}, functx,
                                          {funcl, back}, fundef]),
            [Mod]  = ?Graph:path(Func,   [{func, back}]),
            [{Func, {function, data, Transform}},
             {Mod,  {module,   data, Transform}}]
           catch
             _:_-> []
           end;

      #typexp{} ->
           try
            [File] = ?Graph:path(Parent,[{tattr,back},{form,back},{incl,back}]),
            [Mod] = ?Graph:path(File, [moddef]),
            [{Mod, {module, data, Transform}}]
           catch
             _:_-> []
           end;

      _Error ->  []
   end.%;
% unused fun. clause detected by Dialyzer
% node_typer(_) -> [].

