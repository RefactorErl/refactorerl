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
%%% The Initial Developer of the Original Code is E�tv�s Lor�nd University.
%%% Portions created  by E�tv�s Lor�nd University and ELTE-Soft Ltd.
%%% are Copyright 2007-2013 E�tv�s Lor�nd University, ELTE-Soft Ltd.
%%% and Ericsson Hungary. All Rights Reserved.

%%% @author Melinda T�th <toth_m@inf.elte.hu>
%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>

-module(reftr_wrangler).
-vsn("$Rev: 9568 $").

-export([rename_function/1, rename_variable/1, rename_module/1,
         move_function/1, extract_function/1, tuple_funargs/1,
         introduce_macro/1, generalize_function/1, fun_to_process/1,
         rename_process/1, register_pid/1, add_a_tag/1, pos2tlc/2]).

-include("test.hrl").
-define(CallArg(A,S,L),?MISC:call_arg(A,S,L)).

%%% ============================================================================
%%% Args helpers

%% @type proplist(K) = [K | {K,any()}]
%% @type proplista() = proplist(atom())
%% @type rargatoms() = 'module' | 'file' | 'name' | 'varname' | 'position' |
%%                     'posrange'

arg_file(Args) ->
    ?Args:string(file, "File", Args).

arg_name(Args) ->
    ?Args:string(name, "Target name", Args).

arg_varname(Args) ->
    ?Args:string(varname, "Variable name", Args).

arg_num(Args) ->
    ?Args:integer(Args).

arg_side_effect(Args) ->
    ?Args:bool(has_side_effect, "Has side effect", Args).

arg_create_new_file(Args) ->
    ?Args:bool(create_new_file, "Create new file", Args).

arg_pos(Args) ->
    ?CallArg(Args, "Position",
             [{fun arg_pos2lc/2, [file,position]},
              {fun arg_lc2lc/2,  [line,col]}]).

arg_pos2lc(File,Pos) ->
    {_L,_C} = pos2tlc(File,Pos).

arg_lc2lc(L,C) ->
    {L,C}.

arg_posrange(Args) ->
    ?CallArg(Args, "Position range",
             [{fun arg_posr2lc/2, [file,posrange]},
              {fun arg_lcr2lcr/4, [startline,startcol,endline,endcol]}]).

arg_lcr2lcr(L1,C1,L2,C2) ->
    {{L1,C1}, {L2,C2}}.

arg_posr2lc(File,{Pos1,Pos2}) ->
    {pos2tlc(File,Pos1),
     pos2tlc(File,Pos2)}.

%% @type filename() = string()

%% @doc compute the line and column number of a linear position
%% @throws {error, string()}
%% @spec (filename(),natural()) -> {natural(),natural()}
%% @deprecated use ?MISC:pos2tlc instead of it
pos2tlc(File,Pos) when is_list(File), is_integer(Pos) ->
    hd(?MISC:pos2tlc(File, Pos)).

%%% ----------------------------------------------------------------------------
%%% Other helpers

get_dirs()->
    Dirs = [filename:dirname(?File:path(FileNode))
              || FileNode <- ?Graph:path(?Graph:root(), [file])],
    lists:usort(Dirs).

get_dirs(FilePath) ->
    lists:usort(get_dirs() ++ [filename:dirname(FilePath)]).

%%% ============================================================================
%%% Interface implementation

%% @spec rename_function(Args::proplist()) -> {ok, Files} | {error, Reason}
%% @doc Args examples:
%%     [{file, string()}, {line, integer()}, {col, integer()}, {name, string()}]
%%     [{file, string()}, {position, integer()}, {name, atom()|string()}]
rename_function(Args)->
   File       = arg_file(Args),
   {Line,Col} = arg_pos(Args),
   NewName    = arg_name(Args),
   wrangler:rename_fun(File, Line, Col, NewName, get_dirs(File), 8).

%% @spec rename_variable(Args::proplist()) -> {ok, Files} | {error, Reason}
%% @doc Args examples:
%%  [{file, string()}, {line, integer()}, {col, integer()}, {varname, string()}]
%%  [{file, string()}, {position, integer()}, {varname, string()}]
rename_variable(Args)->
   File       = arg_file(Args),
   {Line,Col} = arg_pos(Args),
   NewName    = arg_varname(Args),
   case wrangler:rename_var(File, Line, Col, NewName, get_dirs(), 8) of
       {ok, _} -> {ok, [File]};
       Result -> Result
   end.


%% @spec rename_module(Args::proplist()) -> {ok, Files} | {error, Reason}
%% @doc Args example:
%%     [{file, string()}, {name, atom()|string()}]
rename_module(Args)->
   File    = arg_file(Args),
   NewName = arg_name(Args),
   NewPath = filename:join(filename:dirname(File), NewName ++ ".erl"),
   {NewPath, wrangler:rename_mod(File, NewName, get_dirs(), 8)}.


%% @spec move_function(Args::proplist()) -> {ok, Files} | {error, Reason}
%% @doc Args examples:
%%     [{file, string()}, {line, integer()}, {col, integer()}, {name, string()},
%%      {create_new_file, bool()}]
%%     [{file, string()}, {position, integer()}, {name, atom()|string()},
%%      {create_new_file, bool()}]
move_function(Args)->
   File       = arg_file(Args),
   {Line,Col} = arg_pos(Args),
   Name       = arg_name(Args),
   Create     = arg_create_new_file(Args),
   try
       wrangler:move_fun_1(File, Line, Col, Name, get_dirs(), 8)
   catch
       error:undef ->
           wrangler:move_fun(File, Line, Col, Name, Create, get_dirs(), 8)
   end.
 %%  @todo: handle create


%% @spec extract_function(Args::proplist()) -> {ok, Files} | {error, Reason}
%% @doc Args examples:
%%     [{file, string()}, {startline, integer()}, {startcol, integer()}, 
%%      {endline, integer()}, {endcol, integer()}, {name, string()}]
%%     [{file, string()}, {posrange, {integer(),integer()},
%%      {name, atom()|string()}]
extract_function(Args)->
   File        = arg_file(Args),
   {Start,End} = arg_posrange(Args),
   Name        = arg_name(Args),
   case wrangler:fun_extraction(File, Start, End, Name, 8) of
       {ok, _} -> {ok, [File]};
       Res -> Res
   end.


%% @spec tuple_funargs(Args::proplist()) -> {ok, Files} | {error, Reason}
%% @doc Args examples:
%%   [{file, string()}, {line, integer()}, {col, integer()},
%%    {number, integer()}]
%%   [{file, string()}, {position, integer()}, {number, integer()}]
tuple_funargs(Args)->
   File = arg_file(Args),
   New  =
       fun()->
           {Line,Col} = arg_pos(Args),
           Num        = integer_to_list(arg_num(Args)),
           wrangler:tuple_funpar(File, Line, Col, Num, get_dirs(), 8)
       end,
   try
       {Start,End} = arg_posrange(Args),
       wrangler:tuple_funpar_1(File, Start, End, get_dirs(), 8)
   catch
       {?Error,_,_} -> New();
       error:undef  -> New()
   end.


%% @spec introduce_macro(Args::proplist()) -> {ok, Files} | {error, Reason}
%% @doc Args examples:
%%     [{file, string()}, {startline, integer()}, {startcol, integer()}, 
%%      {endline, integer()}, {endcol, integer()}, {name, string()}]
%%     [{file, string()}, {posrange, {integer(),integer()}},
%%      {name, atom()|string()}]
introduce_macro(Args)->
   File        = arg_file(Args),
   {Start,End} = arg_posrange(Args),
   Name        = arg_name(Args),
   case wrangler:new_macro(File, Start, End, Name, get_dirs(), 8) of
       {ok, _} -> {ok, [File]};
       Res -> Res
   end.


%% @spec generalize_function(Args::proplist()) -> {ok, Files} | {error, Reason}
%% @doc Args examples:
%%     [{file, string()}, {startline, integer()}, {startcol, integer()}, 
%%      {endline, integer()}, {endcol, integer()}, {name, string()}, 
%%      {has_side_effect, bool()}]
%%     [{file, string()}, {posrange, {integer(),integer()}},
%%      {name, atom()|string()}, {has_side_effect, bool()}]
%%  If the selection has a side-effect then has_side_effect must be true,
%%  and false otherwise.
generalize_function(Args)->
   File        = arg_file(Args),
   {Start,End} = arg_posrange(Args),
   Name        = arg_varname(Args),
   SE          = arg_side_effect(Args),
   case wrangler:generalise(File, Start, End, Name, get_dirs(), 8) of
       {ok, _} -> {ok, [File]};
       {unknown_side_effect, {PName, FName, FArity, FDefPos, Exp}} ->
           case refac_gen:gen_fun_1(SE, File, PName, FName, FArity,
                                    FDefPos, Exp, 8) of
               {ok, _} -> {ok, [File]};
               Res1 -> Res1
           end;
       Res -> Res
   end.


%% @spec fun_to_process(Args::proplist()) -> {ok, Files} | {error, Reason}
%% @doc Args examples:
%%     [{file, string()}, {line, integer()}, {col, integer()}, {name, string()}]
%%     [{file, string()}, {position, integer()}, {name, atom()|string()}]
fun_to_process(Args)->
   File       = arg_file(Args),
   {Line,Col} = arg_pos(Args),
   Name       = arg_name(Args),
   case wrangler:fun_to_process(File, Line, Col, Name, get_dirs(), 8) of
       {undecidables, _} ->
           refac_fun_to_process:fun_to_process_1(File, Line, Col, Name, 
                                                get_dirs(), 8);
       Res -> Res
   end.


%%%%%% BETA refactorings from Wrangler


%% @spec rename_process(Args::proplist()) -> {ok, Files} | {error, Reason}
%% @doc Args examples:
%%     [{file, string()}, {line, integer()}, {col, integer()}, {name, string()}]
%%     [{file, string()}, {position, integer()}, {name, atom()|string()}]
rename_process(Args)->
   File       = arg_file(Args),
   {Line,Col} = arg_pos(Args),
   Name    = arg_name(Args),
   case wrangler:rename_process(File, Line, Col, Name, get_dirs(File), 8) of
       {undecidables, Old} ->
           refac_rename_process:rename_process1(File, Old, Name, get_dirs(), 8);
       Res -> Res
   end.

%% @spec register_pid(Args::proplist()) -> {ok, Files} | {error, Reason}
%% @doc Args examples:
%%     [{file, string()}, {startline, integer()}, {startcol, integer()}, 
%%      {endline, integer()}, {endcol, integer()}, {name, string()}]
%%     [{file, string()}, {posrange, {integer(),integer()}},
%%      {name, atom()|string()}]
register_pid(Args)->
   File        = arg_file(Args),
   {{S1, C1},{S2, C2}} = {Start,End} = arg_posrange(Args),
   Name        = arg_name(Args),
   case wrangler:register_pid(File, Start, End, Name, get_dirs(), 8) of
       {unknown_pnames, PN} -> 
           case refac_register_pid:register_pid_1(File,
                   S1, C1, S2, C2, Name, PN, get_dirs(), 8) of
               {unknown_pids, _} -> 
                   refac_register_pid:register_pid_2(File,S1,C1,S2,C2,
                                                     Name,get_dirs(),8);
                   Res -> Res
           end;
       {unknown_pids, _} -> 
           refac_register_pid:register_pid_2(File, S1, C1, S2, C2,
                                             Name, get_dirs(), 8);
       Res -> Res
   end.


%% @spec add_a_tag(Args::proplist()) -> {ok, Files} | {error, Reason}
%% @doc Args examples:
%%     [{file, string()}, {line, integer()}, {col, integer()}, {name, string()}]
%%     [{file, string()}, {position, integer()}, {name, atom()|string()}]
add_a_tag(Args)->
   File       = arg_file(Args),
   {Line,Col} = arg_pos(Args),
   Name    = arg_name(Args),
   wrangler:add_a_tag(File, Line, Col, Name, get_dirs(File), 8).
