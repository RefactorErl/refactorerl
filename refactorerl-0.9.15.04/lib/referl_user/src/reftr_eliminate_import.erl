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

%%% @doc The refactoring extends the imported functions with module
%%% qualifiers and eliminates the import list(s) for the selected file.

%%% == Parameters ==
%%% <ul>
%%%     <li>The file where to eliminate imports (see {@link
%%%     reflib_args:file/1}).</li>
%%%     <li>The import form that indicates the module that should be
%%%     unimported (see {@link reflib_args:form/1}).</li>
%%% </ul>
%%%
%%% == Conditions of applicability ==
%%% <ul>
%%%     <li>An import form has to be selected to perform the
%%%     transformation.</li>
%%% </ul>
%%%
%%% == Transformation steps and compensations ==
%%% <ol>
%%%     <li>In case there is only one import list of the module in the
%%%     file the functions listed in the attribute list will be
%%%     qualified and the import list will be eliminated.</li>
%%%     <li>In case there is more then one import list of the module
%%%     every function will be qualified and every import list related
%%%     to the specific module will be eliminated.</li>
%%% </ol>
%%%
%%% == Implementation status ==
%%% The transformation is fully implemented.
%%%
%%% @author Istvan Bozo <bozo_i@inf.elte.hu>

-module(reftr_eliminate_import).
-vsn("$Rev: 9568 $ ").

%% Callbacks
-export([prepare/1, error_text/2]).

-include("user.hrl").

%%% ============================================================================
%%% Errors

%%% @private
error_text(not_an_import_form, [])->
    ["The position has to indicate an import form."].

%%% ============================================================================
%%% Callbacks

%%% @private
prepare(Args) ->
    File = ?Args:file(Args),
    Form = ?Args:import_form(Args),
    ?Check(?Form:type(Form) =:= import, ?LocalError(not_an_import_form, [])),
    [Module] = ?Query:exec(Form, ?Query:seq(?Form:expr(1), ?Expr:module())),
    Refs = references(File, Module),
    ImportForms = import_forms(File, Module),
    ModuleName = ?Mod:name(Module),
    fun() ->
            [?File:del_form(File, Form_) || Form_ <- ImportForms ],
            lists:foreach(fun(Ref) -> ?Expr:add_modq(Ref,ModuleName) end, Refs),
            ?Transform:touch(File)
    end.

%%% ============================================================================
%%% Implementation

%% @doc The function returns these import forms from the file `File'
%% that imports functions from module `Module'
import_forms(File, Module) ->
    [Form || Form <- ?Query:exec(File, ?File:forms()),
             ?Form:type(Form) == import,
             ?Query:exec(Form, ?Query:seq(?Form:expr(1), ?Expr:module())) 
                 == [Module]].

%% @doc References to functions imported from the module `Mod' in the
%% current file `File' that are not qualified.
references(File, Mod) ->
    lists:flatmap(
      fun(Fun) ->
              [FunRef ||
                  FunRef <- ?Query:exec(Fun, ?Fun:applications()),
                  ?Query:exec(FunRef, ?Expr:modq()) == [],
                  ?Query:exec(FunRef, ?Query:seq([?Expr:clause(),
                                                  ?Clause:form(),
                                                  ?Form:file()])) =:= [File]]
      end,
      ?Query:exec(Mod, ?Mod:locals())).
