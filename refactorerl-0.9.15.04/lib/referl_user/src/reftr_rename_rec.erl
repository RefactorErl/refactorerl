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

%%% @doc This refactoring renames records in modules or header
%%% files. After the transformation, the old name will be replaced by
%%% the new name in the record definition and in every reference to
%%% the given record (e.g.\ record field access or field update
%%% expressions). The condition of the renaming is that there is no
%%% name conflict with another record in the file (which contains the
%%% given record), in its includes, or where it is included (the
%%% latter is only possible when we are renaming in a header file).
%%%
%%% == Parameters ==
%%% <ul>
%%%   <li>The record to be renamed
%%%       (see {@link reflib_args:record/1}).</li>
%%%   <li>The new name of the record
%%%       (see {@link reflib_args:name/1}).</li>
%%% </ul>
%%%
%%% == Conditions of applicability ==
%%% <ul>
%%%   <li>There must be no record with the new name
%%%     <ul>
%%%       <li>in the file that contains the record,</li>
%%%       <li>in files, which are included by this file,</li>
%%%       <li>in files, which include this file.</li>
%%%     </ul>
%%%   </li>
%%% </ul>
%%%
%%% == Transformation steps and compensations ==
%%% <ol>
%%%   <li>The record name is changed to the new name in the definition of
%%%   the record and in every record expression that refers the record.</li>
%%% </ol>
%%%
%%% == Implementation status ==
%%% The transformation is fully implemented.
%%%
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>
%%% @author Lilla Hajos <lya@elte.hu>


-module(reftr_rename_rec).
-vsn("$Rev: 12913 $"). % for emacs"

%% Callbacks
-export([prepare/1, error_text/2]).

-include("user.hrl").

%%% ============================================================================
%%% Callbacks
error_text(new_rec_name_identical, NewName) ->
   ["New record name (",io_lib:write_atom(NewName),
     ") is identical to the old one."];
error_text(rec_exists, NewName) ->
   ["Record ", io_lib:write_atom(NewName), " already exists"].

%% @private
prepare(Args) ->
    Record  = ?Args:record(Args),
    OldRecName = ?Rec:name(Record),
    [File] = ?Query:exec(Record, ?Rec:file()),
    FilePath = ?File:path(File),

    Files = lists:usort(?Query:exec(Record,
                ?Query:seq(?Rec:file(),
                           ?Query:all(?File:includes(), ?File:included())))),
    Names = [?Rec:name(Rec) || Rec <- ?Query:exec(Files,?File:records())],

    ArgsInfo    = ask_transformation_info(Args, Record),
    NewName  = ?Args:ask(ArgsInfo, name, fun cc_newname/2, fun cc_error/3, 
                         {OldRecName, Names}),

    NameStr = io_lib:write_atom(NewName),
    {Def, Refs} = query_rec_refs(Record),

    [ ?Transform:touch(Expr) || Expr <- [Def | Refs]],
    [fun() ->
        ?ESG:update(Def, (?ESG:data(Def))#form{tag=NewName}),
        ?Macro:inline_single_virtuals(Refs, elex),
        [?Macro:update_macro(Expr, {elex, 2}, NameStr) || Expr <- Refs]
    end,
    fun(_)->
            ?Query:exec(?Query:seq([?File:find(FilePath),
                                    ?Rec:find(NewName)]))
    end].

ask_transformation_info(Args, Record) ->
    RecName = ?Rec:name(Record),
    Info    = ?MISC:format("Renaming record: ~p", [RecName]),
    [{transformation_text, Info} | Args].

%%% ============================================================================
%%% Implementation

cc_newname(NewName, {OldRecName, Names}) ->
    ?Check( NewName =/= OldRecName,
            ?LocalError(new_rec_name_identical, NewName)),
    ?Check( not lists:member(NewName, Names),
            ?LocalError(rec_exists, NewName)),
    NewName.

cc_error(?LocalError(new_rec_name_identical, NewName), NewName, _Names) ->
    ?MISC:format("The name of the record is already ~p.",
                 [NewName]);
cc_error(?LocalError(rec_exists, NewName), NewName, _Names) ->
    ?MISC:format("The given record name (~p) is already used!",
                 [NewName]).

query_rec_refs(Record) ->
    [InDefs] = ?Query:exec(Record, ?Rec:form()),
    InRefs   = ?Query:exec(Record, ?Rec:references()),
    {InDefs, InRefs}.

