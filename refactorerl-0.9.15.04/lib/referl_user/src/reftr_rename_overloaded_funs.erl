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

%%% @doc This module implements the rename function refactoring.
%%%
%%% == Parameters ==
%%% <ul>
%%% <li>The function to be renamed (see {@link reflib_args:function/1})</li>
%%% <li>The new name of the function (see {@link reflib_args:name/1})</li>
%%% </ul>
%%%
%%% == Conditions of applicability ==
%%% <ul>
%%% <li> There must be no function with the given name and the same arity as the
%%% function to be renamed among the functions in the module,
%%% the functions imported in the module, and the auto-imported BIFs.</li>
%%% <li>There must be no function with the given name and the same arity as the
%%% function to be renamed among the local and imported
%%% functions in the modules that import the function to be renamed.</li>
%%% <li>When there are multiple overloaded versions of the function, there must
%%% be no call to the function that creates the list of arguments dynamically.
%%% </li>
%%% <li>When the name of the function comes from a macro definition
%%% the macro must has single role. It must be used only as the name
%%% for the selected function.</li>
%%% </ul>
%%%
%%% == Transformation steps and compensations ==
%%% <ol>
%%%  <li>The name label of the function is changed at every branch of the
%%%  definition to the new one.</li>
%%%  <li>In every static call to the function, the old function name is changed
%%%  to the new one.</li>
%%%  <li>Every implicit function expression is modified to contain the new
%%%  function name instead of the old one.</li>
%%%  <li>If the function is exported from the module, the old name is removed
%%%  from the export list and the new name is put in it.</li>
%%%  <li>If the function is imported in an other module, the import list is
%%%  changed in that module to contain the new name instead of the old one.</li>
%%%  <li>If the name of the function comes from a macro definition and
%%%  the macro is used only as the name for the function, the macro
%%%  definition is updated, otherwise the transformation is
%%%  rejected.</li>
%%% </ol>
%%%
%%% == Implementation status ==
%%% <ul>
%%%   <li>All the transformation steps are implemented.</li>
%%%   <li >The transformation does not takes into account
%%%   the dynamic function calls. </li>
%%% </ul>
%%%
%%% @author Gabor Hosszu <gabor.hosszu@t-online.hu>
%%% @author Csaba Hoch <hoch@inf.elte.hu>
%%% @author Atilla Erdodi <erdodi@elte.hu>

-module(reftr_rename_overloaded_funs).
-vsn("$Rev: 7323 $ ").

%% Callbacks
-export([prepare/1]).

-include("user.hrl").

%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args) ->
	 %Module = ?Args:module(Args),
	AFunWName = ?Args:funname(Args),
	Oldname = ?Fun:name(AFunWName),
	Module = ?Query:exec(AFunWName,?Fun:module()),
	%% Oldname = ?Args:name(Args),
	% ?d(Oldname),
	ModulesFuns = ?Query:exec(Module, ?Mod:locals()),
	FunsWithOldname = [ X || X <- ModulesFuns, ?Fun:name(X) == Oldname ],
%%	Checks, if it is an overloaded function
	?Check(length(FunsWithOldname) > 1,?RefError(not_overloaded_fun, [Oldname])),
	ArgsInfo = [ {transformation_text, "Renaming functions named " ++ atom_to_list(Oldname)} | Args ], % TODO proper info
	%ArsInfo = [ {ask_missing, true} | ArgsInfo ],
	NewName = ?Args:ask(ArgsInfo, name, fun cc_funname/2, fun cc_error/3, FunsWithOldname),

	 [fun() -> ok end] ++
	 [rename_fun(FunNode,NewName) || FunNode <- FunsWithOldname] ++
	 [fun(_) -> NewModulesFuns = ?Query:exec(Module, ?Mod:locals()),
	 [ X || X <- NewModulesFuns, ?Fun:name(X) == NewName ] end	]
 .

%% Adds a `transformation_info' field to `Args'
%% that describes the current transformation.
%add_transformation_info(Args, Fun) ->
%    [Mod]   = ?Query:exec(Fun, ?Fun:module()),
%    ModName = ?Mod:name(Mod),
%    FunName = ?Fun:name(Fun),
%    FunAr   = ?Fun:arity(Fun),
%    Info    = ?MISC:format("Renaming function ~p:~p/~p",
%                           [ModName, FunName, FunAr]),
%    [{transformation_text, Info} | Args].

%%% ============================================================================
%%% Implementation

rename_fun(FunNode,Newname) ->
	NewnameStr = io_lib:write_atom(Newname),
    FunNameNodes      = get_fun_name_nodes(FunNode),
    ImpExpNameNodes   = get_imp_exp_name_nodes(FunNode),
    AppNameNodes      = get_app_name_nodes(FunNode),
    ImplicitNameNodes = get_imp_name_nodes(FunNode),
    Updates =
        FunNameNodes ++ ImpExpNameNodes ++ AppNameNodes ++ ImplicitNameNodes,

    ?Macro:check_single_usage(Updates, [{elex, 1}]),

    [FunDefNode] =
        ?Query:exec(FunNode, ?Query:seq(?Fun:definition(), ?Form:func())),
    %[File] = ?Query:exec(FunDefNode, ?Query:seq([?Fun:module(),?Mod:file()])),
    %FilePath = ?File:path(File),
    DynUpdates   = reflib_dynfun:collect({rename, func}, FunDefNode, Newname),
    
    fun(_) ->
        ?Macro:inline_single_virtuals(Updates, elex),
	% Comment from rename fun:
        % updating virtuals and non-virtuals in arbitrary order caused problems
        % todo check whether such a condition is still necessary
        %        if yes, document it in test cases
        {Virts, NonVirts} =
                 lists:partition(fun(Node) ->
                                         ?Macro:is_virtual(Node, {elex, 1})
                                 end, Updates),
        [?Macro:update_macro(Node, {elex, 1}, NewnameStr) || Node <- Virts],
        [?Macro:update_macro(Node, {elex, 1}, NewnameStr) || Node <- NonVirts],

        reflib_dynfun:transform(DynUpdates)
    end
    .

get_imp_name_nodes(FunNode) ->
    ImplicitNodes = ?Query:exec(FunNode, ?Fun:implicits()),
    ImplicitNameNodes =
        [case ( ?Query:exec(Node, ?Expr:modq())==[]) of
             true ->
                 hd( ?Query:exec(Node, ?Expr:child(1)));
             false ->
                 [DelimiterNode] = ?Query:exec(Node, ?Expr:child(1)),
                 hd( ?Query:exec(DelimiterNode, ?Expr:child(2)))
         end || Node<-ImplicitNodes],
    ImplicitNameNodes.

get_app_name_nodes(FunNode) ->
    AppNodes     = ?Query:exec(FunNode, ?Fun:applications()),
    AppNameNodes =
        [case ?Query:exec(Node, ?Expr:modq()) of
             [] ->
                 hd( ?Query:exec(Node, ?Expr:child(1)));
             _ -> % module qualifier present
                 [DelimiterNode] = ?Query:exec(Node, ?Expr:child(1)),
                 hd( ?Query:exec(DelimiterNode, ?Expr:child(2)))
         end || Node <- AppNodes],
    AppNameNodes.

get_imp_exp_name_nodes(FunNode) ->
    ImpExpNodes = ?Query:exec(FunNode, ?Fun:impexps()),
    [hd(?Query:exec(Node, ?Expr:child(1))) || Node<-ImpExpNodes].

get_fun_name_nodes(FunNode) ->
    [FunDefNode] = ?Query:exec(FunNode, ?Fun:definition()),
    FunClauseNodes = ?Query:exec(FunDefNode, ?Form:clauses()),
    FunNameNodes =
        lists:append([ ?Query:exec(FunClauseNode, ?Clause:name())
                       || FunClauseNode<-FunClauseNodes]),
    FunNameNodes.


%%% ============================================================================
%%% Checks
cc_funname(NewName, NodeList) ->
	[ cc_onefun(NewName, FunNode) || FunNode <- NodeList ],
	NewName.

cc_onefun(NewName, FunNode) ->
    Arity = ?Fun:arity(FunNode),
    ?Check(?Query:exec(FunNode, ?Fun:definition()) =/= [],
           ?RefError(fun_def_not_found, [?Fun:name(FunNode), Arity])),
    OldName = ?Fun:name(FunNode),
    ?Check(NewName =/= OldName,
                     ?RefError(new_funname_identical, OldName)),
    Modules =
        ?Query:exec(FunNode, ?Fun:module()) ++
        ?Query:exec(FunNode, ?Fun:imported()),

    lists:foreach(
      fun(Module) ->
              ModName = ?Mod:name(Module),
              ?Check(?Query:exec(Module, ?Mod:local(NewName, Arity)) =:= [],
                     ?RefError(fun_exists, [ModName, NewName, Arity])),
              ?Check(?Query:exec(Module, ?Mod:imported(NewName, Arity)) =:= [],
                     ?RefError(imported_fun_exists,[ModName, [NewName, Arity]]))
      end, Modules),

    ?Check(not ?Fun:is_autoimported(NewName, Arity),
           ?RefError(autoimported_fun_exists, [NewName, Arity]))
    .

cc_error(?RefError(Type, _), Name, _FunNode) ->
    case Type of
      new_funname_identical -> ?MISC:format("The function name is already ~p", [Name]);
      fun_def_not_found -> ?MISC:format("Function definition not found for ~p.", [Name]);
      fun_exists -> ?MISC:format("The function name ~p is already used.", [Name]);
      imported_fun_exists -> ?MISC:format("The function name ~p is already used as a name of an imported function.", [Name]);
      autoimported_fun_exists -> ?MISC:format("The function name ~p is already used as a name of an autoimported function.", [Name])
    end.
