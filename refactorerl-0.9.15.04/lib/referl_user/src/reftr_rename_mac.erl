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

%%% @doc This module implements the rename macro refactoring. This
%%% refactoring can rename macros in either modules or header files.
%%% The transformation results in the macro name at both the definition
%%% and at references to the given macro to be changed. The condition
%%% of the renaming is that there is no name conflict with another record
%%% in the file containing the macro, in any of its includes or anywhere
%%% it has been included at.
%%%
%%% == Parameters ==
%%% <ul>
%%%   <li> The macro to be renamed
%%%        (see {@link reflib_args:macro/1}).</li>
%%%   <li> The name which is to be given to the macro
%%%        (see {@link reflib_args:name/1}).</li>
%%% </ul>
%%%
%%% == Conditions of applicability ==
%%% <ul>
%%%   <li> No macro already exist with the same new name in either
%%%     <ul>
%%%       <li>in a file hosting definition or usage of the macro,</li>
%%%       <li>in files included by the said,</li>
%%%       <li>in files that include the said</li>
%%%     </ul>
%%%   </li>
%%% </ul>
%%% == Transformation steps and compensations ==
%%% <ol>
%%%   <li>The macro name is replaced with the new name at both definition
%%%       and all usage sites</li>
%%% </ol>
%%%
%%% == Implementation status ==
%%% The transformation is fully implemented.
%%%
%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>

-module(reftr_rename_mac).
-vsn("$Rev: 9568 $").

%% Callbacks
-export([prepare/1]).
-export([error_text/2]).

-include("user.hrl").

%%% ============================================================================
%%% Callbacks

%% @private
error_text(int_graph,[]) ->
    ["Internal error caused by unexpected graph structure"];
error_text(expected_node,[Atom,Node])->
    ["Expected node class "++io_lib:print(Atom)++
     " at node "++io_lib:print(Node)];
error_text(_,_) -> unknown.

%% @private
prepare(Args) ->
    Macro        = ?Args:macro(Args),
    OldMacName   = ?Macro:name(Macro),
    [File]       = ?Query:exec(Macro, ?Macro:file()),
    Files        = lists:usort(?Query:exec(File, {all, [incl], [{incl,back}]})),
    Macros       = ?Query:exec(Files,?File:macros()),
    MacNames     = [?Macro:name(M) || M <- Macros],
    ArgsInfo     = add_transformation_info(Args, Macro, File),
    NewName      = ?Args:ask(ArgsInfo, macname, 
                             fun cc_newmacname/2, fun cc_error/3, 
                             {OldMacName,MacNames}),

    Refs     	 = query_mac_replacers(Macro,NewName),

    MacD     	 = ?ESG:data(Macro),
    NewMacD  	 = MacD#form{tag=NewName},

    ?Transform:touch(Macro),
    [?Transform:touch(Parent) || {Parent, _, _} <- Refs],
    [fun() ->
        ?ESG:update(Macro, NewMacD),
        [?ESG:update(NameN, NewNameND) || {_, NameN, NewNameND} <- Refs]
    end,
    fun(_)->
        [Macro]
    end].

add_transformation_info(Args, Macro, File) ->
    MacName = ?Macro:name(Macro),
    Path    = ?File:path(File),
    Info    = ?MISC:format("Renaming macro: ~p (Location: ~p)",
                           [MacName, Path]),
    [{transformation_text, Info} | Args].


%%% ============================================================================
%%% Implementation

%% @doc Gathers the lexical nodes corresponding to both the definition and
%% the usages of the macro.
query_mac_replacers(Macro,NewName) ->
    Refs = ?Query:exec(Macro, [{mref,back}]),
    [  replacer_of_macname(Macro, {flex,4}, NewName) |
     [ replacer_of_macname(Subst, {llex,2}, NewName) ||
       Subst <- Refs ] ].

%% @doc It follows `Path' from `Parent' to handle both macro definition and uses.
%% It also handles virtual nodes caused by included macros.
replacer_of_macname(Parent, LexWIdx, New) ->
    NameNode = ?Query:exec1([Parent], [LexWIdx], ?LocalErr0r(int_graph)),
    ?Check(?Graph:class(NameNode)==lex,
           ?LocalError(expected_node,[lex,NameNode])),
    NameNodeD=#lex{data=NND} = ?ESG:data(NameNode),
    case NND of
        virtual ->
            replacer_of_macname(NameNode,{orig,1},New);
        _ ->
            NewNameNodeD = NameNodeD#lex{data=NND#token{text=New}},
            case ?Query:exec(NameNode, [{flex,back}]) of
                 []           -> RealParent = Parent;
                 [RealParent] -> ok
            end,
            {RealParent,NameNode,NewNameNodeD}
    end.

%%% ============================================================================
%%% Checks

cc_newmacname(NewName, {OldMacName,MacNames}) ->
    ?Check(OldMacName =/= NewName, ?RefError(new_macname_identical,[NewName])),
    ?Check(not lists:member(NewName, MacNames), ?RefError(mac_exists,[NewName])),
    NewName.

cc_error(?RefError(Type,[NewName]), NewName, {_OldMacName, _Macro}) ->
    case Type of
       mac_exists -> 
             ?MISC:format("A macro of name ~p is already present.", [NewName]);
       new_macname_identical -> 
             ?MISC:format("The name of the macro is already ~p.", [NewName])
    end.
