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

%%% ============================================================================
%%% Module information

%%% @doc This module implements the `move functions between modules'
%%% refactoring.
%%%
%%% == Parameters ==
%%% <ul>
%%%   <li>The module from the functions are moved
%%%       (see {@link reflib_args:module/1}).</li>
%%%   <li>The module's name to the functions are moved
%%%       (see {@link reflib_args:name/1}).</li>
%%%   <li>The functions to be moved
%%%       (see {@link reflib_args:functions/1}).</li>
%%% </ul>
%%%
%%% == Conditions of applicability ==
%%% <ul>
%%%   <li>The names of the selected functions should not conflict with
%%%   other functions in the target module, neither with those
%%%   imported from another module (overloading). Furthermore, the
%%%   name should be a legal function name in all modules.</li>
%%%   <li>Macro name conflicts must not occur in the target module,
%%%   that is, macro names used in the functions must refer to the
%%%   same macro definition in the source and in the target
%%%   module. This applies to macros used in these macros too.</li>
%%%   <li>Record name conflicts must not occur in the target module,
%%%   that is, record names used in the functions must refer to the
%%%   same record definition in the source and in the target module.</li>
%%% </ul>
%%%
%%% == Transformation steps and compensations ==
%%% <ol>
%%%   <li>In the refactoring step the functions to be moved have to be
%%%   marked either at the definition or in the export list. A list
%%%   has to be created from the function name and arity
%%%   pairs. Duplicity should be avoided and only real function names
%%%   and arities should occur in the list.</li>
%%%   <li>The new place of the functions, or the target module, has to
%%%   be asked from the user. If there is no such module in the tool
%%%   database, it has to be loaded.</li>
%%%   <li>If the transformation does not disobey the rules, the
%%%   functions have to be deleted from their original places together
%%%   with all their clauses.</li>
%%%   <li>The moved functions have to be placed to the end of the new
%%%   module.</li>
%%%   <li>Functions have to be deleted if they appear in the export
%%%   list of the original module. (If they were exported, they have
%%%   to be exported in their new place, too.)</li>
%%%   <li>The functions, which are called in the moved function but
%%%   remain in the original module, have to be put in an export list
%%%   in the original module.</li>
%%%   <li>If the functions to be moved are called in other functions
%%%   from the original module, they have to be exported in the new
%%%   module and the calls in the original module have to be changed
%%%   to qualified calls.</li>
%%%   <li>If the moved functions are referred to by qualified names,
%%%   the module names have to be changed to the new module name.</li>
%%%   <li>After the transformation the module names in the import
%%%   lists have to be changed to the name of the target module.</li>
%%%   <li>The moved function in the target module has to be deleted
%%%   from the import list.</li>
%%%   <li>Records and macros used in the moved function have to be
%%%   made visible in the target module, either by moving them into
%%%   header files (or including the header file if the definition is
%%%   already in one), or copying their definition.</li>
%%% </ol>
%%%
%%%
%%% @todo handle all error_text atoms either here or in ?Error
%%%
%%% @author Daniel Horpacsi <daniel_h@inf.elte.hu>


-module(reftr_move_fun).
-vsn("$Rev: 12913 $ ").

%%% ============================================================================
%%% Exports

%% Callbacks
-export([prepare/1]).
-export([error_text/2]).

%% For inline_fun
-export([prepare_recmac/3, correct_recmac/4]).


-include("user.hrl").

-import(lists, [filter/2, usort/1, member/2, flatten/1]).
-import(?MISC, [intersect/2, flatsort/1]).
-import(?Query, [seq/1, seq/2, all/1, all/2, any/1, any/2,
                 exec/1, exec/2, exec1/2, exec1/3]).


%%% ============================================================================
%%% Main types

-record(info, {fromfile, frommod, tofile, tomod, fundefs, funobjs}).

-record(rec_info, {loc_incl, node, form, removable}).
-record(mac_info, {loc_incl, form, removable}).

-record(list_del, {type, list, item}).
-record(list_add, {type, where, location, name, arity, funobj}).
-record(list_ren, {form, name}).

-record(modq_add, {expr, module}).
-record(modq_add_macro, {expr, module}).
-record(modq_upd, {expr, module}).
-record(modq_upd_macro, {expr, module}).
-record(modq_del, {expr}).
-record(modq_del_macro, {expr}).

-record(funname_tokinfo, {orig, colon, modq}).

-define(TableName, modifs).
-define(NOTE(D), ets:insert(?TableName, D)).

%%% ============================================================================
%%% Errors

%% @private

error_text(fundef_not_found, Fun) ->
    ["Definition of function ", ?MISC:fun_text(Fun)," not found"];

error_text(name_conflict, [ToModName, {Name, Arity}]) ->
    ["Name conflict with ", ?MISC:fun_text([ToModName, Name, Arity])];

error_text(recmac_local_conflict, [Clash]) ->
    ["Records and macros defined in the source file conflict in the target: ",
     ?MISC:join(Clash)];

error_text(unincludables, [Conflicts]) ->
    ["Cannot include headers in the target module ",
     ?MISC:format("(~p)", [Conflicts])];

error_text(include_mismatch, [Clash]) ->
    ["Different definitions come from includes in the source and target files",
     " for some records and macros: ",
     ?MISC:format("(~p)", [Clash])];

error_text(funobj, [Loc, Name, Arity]) ->
    ["No such function object found in the given module ",
     ?MISC:format("~p:~p/~p", [Loc, Name, Arity])].

%% Returns the name of a record or a macro from its node.
recmac_name(Node) ->
    case ?Syn:class(Node) of
        form   -> [$? | ?Macro:name(Node)];
        record -> [$# | atom_to_list(?Rec:name(Node))]
    end.

%%% ============================================================================
%%% Callbacks

%% @private
prepare(Args) ->

    %% ETS table to buffer the required modifications
    ets:new(?TableName, [named_table, bag]),
    ets:new(token_info, [named_table, set, {keypos, 2}]),

    %% Finding the source and the target module
    FunObjs = ?Args:functions(Args),
    FunNameA = [ {FunName, Arity}
               || FunObj  <- FunObjs,
                  FunName <- [?Fun:name(FunObj)],
                  Arity   <- [?Fun:arity(FunObj)]],
    [FromMod] = ?Query:exec(hd(FunObjs), ?Fun:module()),
    ToMod   = exec1(?Mod:find(?Args:name(Args)),
                    ?RefErr0r(target_not_found)),
    ToModName = ?Mod:name(ToMod),
    %% Checking if the source and target modules are equals

    ?Check(ToMod =/= FromMod, ?RefErr0r(source_and_target_equals)),

    %% Finding the files that belong to the modules

    FromFile = exec1(FromMod, ?Mod:file(), ?RefErr0r(source_not_found)),
    ToFile   = exec1(ToMod, ?Mod:file(), ?RefErr0r(target_not_found)),
    ToModPath = ?File:path(ToFile),

    %% Finding function objects and definitions

    FunDefs = [exec1(FunObj, ?Fun:definition(),
                     ?LocalError(fundef_not_found, [FunName,Arity]))
               || FunObj  <- FunObjs,
                  FunName <- [?Fun:name(FunObj)],
                  Arity   <- [?Fun:arity(FunObj)]],


    %% Checking function name collisions

    [?Check(exec(ToMod, ?Mod:visible(Name, Arity)) -- FunObjs == [],
            ?LocalError(name_conflict, [ToModName, {Name, Arity}])) ||
        FunObj <- FunObjs,
        Name  <- [?Fun:name(FunObj)],
        Arity <- [?Fun:arity(FunObj)]],

    %% Checking macro multirole usage
    lists:foreach(
        fun(FunObj) ->
                ?Macro:check_single_usage(exec(FunObj,
                         all([?Fun:applications(),
                         ?Fun:implicits()])),
                                    [{esub, 1}, {elex, 1}])
        end,
        FunObjs),

    %% Checking ?MODULE macros

    Exprs = ?Query:exec(?Query:seq([[file],?File:forms(),?Form:clauses(),?Clause:exprs(),?Expr:deep_sub()])),
    ModMacExprs = lists:filter(fun(E) -> ?Expr:value(E) == ?Mod:name(FromMod) end, Exprs),
    ModMacTokens = ?Query:exec(ModMacExprs, [elex,llex,llex]),
    ?Check(not(lists:any(fun(E) -> ?Token:text(E) == "MODULE" end, ModMacTokens)), ?RefErr0r(module_macro_found)),

    %% Collecting record and macro infos, checking collisions

    [?NOTE(I) || I <- prepare_recmac(FunDefs, FromFile, ToFile)],

    %% Returning transformation function

    Info = #info{fromfile = FromFile, frommod = FromMod,
                 tofile = ToFile, tomod = ToMod,
                 fundefs = FunDefs, funobjs = FunObjs},

    DynFunCalls = [?Dynfun:collect(move, Fun, ToMod) || Fun <- FunObjs],

    [fun()   -> transform_1(Info)          end,
     fun(ok) -> flatten(transform_2(Info)) end,
     fun(_)  -> [?Dynfun:transform(DCall) || DCall <- DynFunCalls] end,
     fun(_) ->
        [Target] = ?Query:exec(?Query:seq(?File:find(ToModPath), ?File:module())),
        [ ?Query:exec1(Target, ?Fun:find(F,A),
                       ?LocalError(funobj, [ToModName, F, A]))
         || {F,A} <- FunNameA]
     end].

%%% @private
prepare_recmac(FunDefs, FromFile, ToFile) ->
    Used_Rec = usort(exec(FunDefs, ?Form:records())),
    Used_Mac = usort(exec(FunDefs, ?Form:macros())),
    Mac_By_Mac = usort(Used_Mac ++ exec(Used_Mac, ?Macro:macros()) ++
                       exec(Used_Mac, ?Macro:references())),
    Rec_By_Mac = usort(Used_Rec ++ exec(Used_Mac, ?Macro:records())),
    RecInfos = rec_mac_infos(record, FromFile, ToFile,
                             usort(Used_Rec ++ Rec_By_Mac), FunDefs),
    MacInfos = rec_mac_infos(macro,  FromFile, ToFile,
                             usort(Used_Mac ++ Mac_By_Mac), FunDefs),
    RecInfos ++ MacInfos.

rec_mac_infos(Tag, FromFile, ToFile, Used_, FunDefs) ->
    %% Querying defined and used records and macros
    %%     (F = From,  T = Target)
    %%     (L = Local, I = Included, A = All)

    {FromFileQuery, GetFileQuery, GetName} =
        case Tag of
            record -> {?File:records(),
                       ?Rec:file(), fun ?Rec:name/1 };
            macro  -> {?File:macros() , ?Macro:file(), fun ?Macro:name/1}
        end,

    WithName = fun(List) -> [{Node, GetName(Node)} || Node <- List] end,
    GetClash = fun(L1, L2) ->
                       usort([Node || {Node, Name} <- WithName(L1),
                                      {Node2, Name2} <- WithName(L2),
                                      Node /= Node2, Name == Name2])
               end,

    F_L_ = exec(FromFile, FromFileQuery),
    F_I  = exec(FromFile, ?Query:seq(?File:includes(),
                                     FromFileQuery)) -- F_L_,
    T_A_ = exec(ToFile,   ?Query:seq(?File:includes(),
                                     FromFileQuery)),

    %% Filtering ?MODULE macro

    Filter = fun(Macro) -> GetName(Macro) =/= "MODULE" end,
    {Used, F_L, T_A} = {filter(Filter, Used_),
                        filter(Filter, F_L_),
                        filter(Filter, T_A_)},

    Clash         = GetClash(Used, T_A),
    Local_Clash   = intersect(Clash, F_L),
    Used_Included = intersect(F_I, Used),

    ClashNames = lists:map(fun recmac_name/1, Clash),
    case {Clash, Local_Clash} of
        {[], _} ->
            % There is no direct name collision, but the inclusion
            % should not induct any conflicts.

            Conflicts = [Hrl || Entity <- Used_Included,
                                Hrl <- exec(Entity, GetFileQuery),
                                not ?File:includable(ToFile, Hrl)],
            ConflictNames = lists:map(fun ?File:path/1, Conflicts),
            ConflictNameL = string:join(ConflictNames, ", "),
            ?Check(Conflicts == [],
                   ?LocalError(unincludables, [ConflictNameL]));
        {_, []} ->
            % The clashes only come from includes,
            % hopefully it can be resolved by matching the include entries.
            % Note: in this case, Clash === Incl_Clash.

            InclClashFiles = usort(exec(Clash, GetFileQuery)),
            ToIncludes = exec(ToFile, ?File:includes()),
            ?Check(InclClashFiles -- ToIncludes == [],
                   ?LocalError(include_mismatch, [ClashNames]));
        {_, _}  ->
            throw(?LocalError(recmac_local_conflict, [ClashNames]))
    end,
    [recmac_info(Node, FunDefs, Used_Included, Tag) || Node <- Used].

recmac_info(Node, FunDefs, UsedIncluded, Tag) ->
    Removable = usort(exec(Node, recmac_referer_funforms(Tag))) -- FunDefs == [],
    Loc_Incl = case member(Node, UsedIncluded) of
                   true  -> include;
                   false -> localdefine
               end,
    case Tag of
        record ->
            Form = exec1(Node, ?Rec:form(), form_not_found),
            #rec_info{loc_incl = Loc_Incl, node = Node,
                      form = Form, removable = Removable};
        macro  ->
            #mac_info{loc_incl = Loc_Incl, form = Node,
                      removable = Removable}
    end.

recmac_referer_funforms(record) ->
    seq([?Rec:references(), ?Expr:clause(), ?Clause:form()]);

recmac_referer_funforms(macro) ->
    seq([{mref, back}],
        all(seq(?Token:clause(), ?Clause:form()),
            seq([?Token:expr(), ?Expr:clause(), ?Clause:form()]))).


%%% ============================================================================
%%% Transformation

transform_1(I = #info{frommod = FromMod, fromfile = FromFile, tofile = ToFile,
                      fundefs = FunDefs, funobjs = FunObjs}) ->

    [correct_body(FunDef, exec(FromMod, ?Mod:imports()), I)
     || FunDef <- FunDefs],
    [correct_refs(FunObj, Expr, I)
     || FunObj <- FunObjs, Expr <- funrefs(FunObj)],

    ?Transform:touch(FromFile),
    ?Transform:touch(ToFile),

    [fun() -> init end] ++
        [fun(_) ->
                ?File:del_form(FunDef),
                 ?File:add_form(ToFile, FunDef)
         end || FunDef <- FunDefs].

transform_2(#info{frommod = FromMod, fundefs = FunDefs,
                  fromfile = FromFile, tofile = ToFile}) ->
    ListAdds =
        [Add#list_add{funobj = exec1(Loc, ?Fun:find(Name, Arity),
                                     ?LocalError(funobj, [?Mod:name(Loc), Name, Arity]))}
         || Add <- ets:lookup(?TableName, list_add),
            #list_add{location=Loc, name=Name, arity=Arity} <- [Add]],

    R = [fun() -> init end]
        ++
        [fun(_) -> ?Expr:add_modq(Expr, ?Mod:name(Module)) end
         || #modq_add{expr=Expr, module=Module}
                <- ets:lookup(?TableName, modq_add)]
        ++
        [fun(_) -> handle_modq_macro(add, Expr, ?Mod:name(Module)) end
         || #modq_add_macro{expr=Expr, module=Module}
                <- ets:lookup(?TableName, modq_add_macro)]
        ++
        [fun(_) -> ?Expr:upd_modq(Expr, ?Mod:name(Module)) end
         || #modq_upd{expr=Expr, module=Module}
                <- ets:lookup(?TableName, modq_upd)]
        ++
        [fun(_) -> handle_modq_macro(upd, Expr, ?Mod:name(Module)) end
         || #modq_upd_macro{expr=Expr, module=Module}
                <- ets:lookup(?TableName, modq_upd_macro)]
        ++
        [fun(_) -> ?Expr:del_modq(Expr) end
         || #modq_del{expr=Expr} <- ets:lookup(?TableName, modq_del)]
        ++
        [fun(_) -> handle_modq_macro(del, Expr) end
         || #modq_del_macro{expr=Expr} <- ets:lookup(?TableName, modq_del_macro)]
        ++
        [fun(_) -> imp_list_rename(Form, Name) end
         || #list_ren{form=Form, name=Name} <- ets:lookup(?TableName, list_ren)]
        ++
        [fun(_) -> expimp_list_adds(A) end || A <- ListAdds]
        ++
        [fun(_) -> expimp_list_dels(List, Exprs, FromMod, FunDefs) end
         || {List, Exprs} <- group_list_dels(ets:lookup(?TableName, list_del))]
        ++
        [fun(_) ->
                 correct_recmac(record, {Loc_Incl, Node, Form, Removable},
                                  FromFile, ToFile) end
         || #rec_info{loc_incl=Loc_Incl, node=Node, form=Form, removable=Removable}
                <- ets:lookup(?TableName, rec_info)]
        ++
        [fun(_) -> correct_recmac(macro,  {Loc_Incl, Form, Form, Removable},
                                  FromFile, ToFile) end
         || #mac_info{loc_incl=Loc_Incl, form=Form, removable=Removable}
                <- ets:lookup(?TableName, mac_info)],
    ets:delete(?TableName),
    R.


%%% ============================================================================
%%% Compensations

%%% ----------------------------------------------------------------------------
%%% Macro/record
%%% @private
correct_recmac(_, {localdefine, _Node, Form, false}, _, ToFile) ->
    {_, NewForm} = lists:keyfind(Form, 1, ?ESG:copy(Form)),
    ?File:add_form(ToFile, NewForm);

correct_recmac(_, {localdefine, _Node, Form, true}, FromFile, ToFile) ->
    ?File:del_form(FromFile, Form),
    ?File:add_form(ToFile, Form);

correct_recmac(Tag, {include, Node, _Form, Removable}, FromFile, ToFile) ->
    GetFileQuery = case Tag of
                       record -> ?Rec:file();
                       macro ->  ?Macro:file()
                   end,
    IncludeFile = exec1(Node, GetFileQuery, file_not_present),
    case Removable andalso
        exec(FromFile, ?File:include_form(IncludeFile)) =/= [] of
        true ->
            ?File:del_include(FromFile, IncludeFile);
        false ->
            ok
    end,
    ?File:add_include(ToFile, IncludeFile).

%%% ----------------------------------------------------------------------------
%%% Moved function references

correct_refs(Fun, Expr, I = #info{tomod = ToMod}) ->
    case [?Form:type(A) || A <- exec(Expr, ?Expr:attrib_form())] of
        [export] ->
            correct_export_refs(Expr, Fun, ToMod);
        [import] ->
            correct_import_refs(Expr, Fun, ToMod);
        [] ->
            correct_module_qualifier(Expr, I)
    end.

correct_export_refs(Expr, Fun, ToMod) ->
    ExportForm = exec1(Expr, ?Expr:attrib_form(), form_not_found),
    ExportList = exec1(ExportForm, ?Form:exprs(), list_not_found),
    ?NOTE(#list_del{type=export, list=ExportList, item=Expr}),
    ?NOTE(#list_add{type=export, location = ToMod,
                    name = ?Fun:name(Fun), arity=?Fun:arity(Fun)}).

correct_import_refs(Expr, Fun, TargetMod) ->
    ImportForm = exec1(Expr, ?Expr:attrib_form(), form_not_found),
    Mod = exec1(ImportForm, seq(?Form:file(), ?File:module()), mod_not_found),
    Attrs = exec(ImportForm, ?Form:exprs()),
    case {Attrs, Mod}  of
        {[_, ImportList], TargetMod} ->
            ?NOTE(#list_del{type=import, list=ImportList, item=Expr});
        {[_, ImportList], _} ->
            ?NOTE(#list_del{type=import, list=ImportList, item=Expr}),
            ?NOTE(#list_add{type=import, where=Mod, location = TargetMod,
                            name = ?Fun:name(Fun), arity = ?Fun:arity(Fun)});
        {_,TargetMod} ->
            ?File:del_form(ImportForm),
            ?Transform:touch(exec1(Mod, ?Mod:file(),
                                   ?RefError(no_file,[module,Mod])));
        {_, _} ->
            ?NOTE(#list_ren{form=ImportForm, name=?Mod:name(TargetMod)}),
            ?Transform:touch(ImportForm)
    end.


%%% ----------------------------------------------------------------------------
%%% Module qualifiers

correct_module_qualifier(Expr, #info{frommod = FromMod, tomod = ToMod,
                                     funobjs = FunObjs}) ->
    [File]           = exec(Expr,
                            seq([?Expr:clause(), ?Clause:form(), ?Form:file()])),
    [ModOrigin]      = exec(File, ?File:module()),
    [Referred]       = exec(Expr, ?Expr:function()),
    [FunIsReferring] = exec(Expr,
                            seq([?Expr:clause(), ?Clause:form(), ?Form:func()])),
    MaybeMod         = exec(Expr, seq([?Expr:modq(), ?Expr:child(1), ?Expr:module()])),
    case {ModOrigin, MaybeMod} of
        {ToMod, [FromMod]} ->
            ?Transform:touch(File),
            {Token, Orig} = get_funname_token(Expr),
            case Token /= Orig of
                true -> ?NOTE(#modq_del_macro{expr=Expr});
                _    -> ?NOTE(#modq_del{expr = Expr})
            end;
        {FromMod, []} ->
            ReferredMoved = lists:member(Referred, FunObjs),
            RefererMoved  = lists:member(FunIsReferring, FunObjs),
            case {ReferredMoved, RefererMoved} of
                {false, true} ->
                    ?NOTE(#list_add{type=export, location=ToMod,
                                    name = ?Fun:name(Referred),
                                    arity = ?Fun:arity(Referred)});
                {true, false} ->
                    ?Transform:touch(File),
                    {Token, Orig} = get_funname_token(Expr),
                    case Token /= Orig of
                        true -> ?NOTE(#modq_add_macro{expr = Expr, module = ToMod});
                        _    -> ?NOTE(#modq_add{expr = Expr, module = ToMod})
                    end,
                    ?NOTE(#list_add{type=export, location=ToMod,
                                    name = ?Fun:name(Referred),
                                    arity = ?Fun:arity(Referred)});
                %%{false, true} ->
                %%    insert_module_qualifier(Expr, FromMod),
                %%    [#list_add{type=export, file=FFile, name=Name, arity=Arity}];
                _ ->
                    ok
            end;
        {_, [FromMod]} ->
            ?Transform:touch(File),
            ModSubstT = ?Query:exec(Expr, [{esub, 1}, elex, llex]),
            FNameSubstT = ?Query:exec(Expr, [{esub, 2}, elex, llex]),
            case ModSubstT /= [] andalso FNameSubstT /= [] andalso
                hd(ModSubstT) == hd(FNameSubstT) of
                true -> ?NOTE(#modq_upd_macro{expr = Expr, module = ToMod});
                _    -> ?NOTE(#modq_upd{expr = Expr, module = ToMod})
            end;
        {_, _} ->
            ok
    end.


%%% ----------------------------------------------------------------------------
%%% Applications in moved bodies

correct_body(FunForm, ImportedFuns, I) ->
    Expressions = exec(FunForm, path_form_expressions()),
    flatten([correct_application(Expr, ImportedFuns, I) ||
                Expr <- Expressions]).


correct_application(Expr, ImportedFuns,
                    I = #info{funobjs = FunObjs, tomod=ToMod}) ->
    case exec(Expr, ?Expr:function()) of
        [ReferredFun] ->
            ReferredMoved = lists:member(ReferredFun, FunObjs),
            case {ReferredMoved, exec(Expr, ?Query:seq([?Expr:modq(),
                                                        ?Expr:child(1),
                                                        ?Expr:module()]))} of
                {false, [ToMod]} ->
                    ?Transform:touch(Expr),
                    ?NOTE(#modq_del{expr = Expr});
                {false, []} ->
                    correct_app_noqual(Expr, ReferredFun, ImportedFuns, I);
                {_, _} ->
                    ok
            end;
        _ ->
            ok
            %% Applied function is unknown
            %% e.g. `Fun(...)', fun name comes from a variable
    end.

correct_app_noqual(Application, ReferredFun, ImportedFuns,
                   #info{fromfile=FromFile, frommod=FromMod, tomod=ToMod}) ->
    Imported = lists:member(ReferredFun, ImportedFuns),
    case Imported of
        true ->
            ImportRefsInFrom =
                filter(
                  fun(Expr) -> exec(Expr, seq(?Expr:attrib_form(),
                                              ?Form:file())) == [FromFile]
                  end,
                  exec(ReferredFun, ?Fun:imports())),
            case ImportRefsInFrom of
                [] -> [];
                Exprs ->
                    flatsort(
                      [begin
                           Form = exec1(Expr, ?Expr:attrib_form(), form_not_found),
                           [ModNameExpr, List] = exec(Form, ?Form:exprs()),
                           Module = exec1(ModNameExpr, ?Expr:module(), no_modref),
                           ?NOTE(#list_del{type=import, list=List, item=Expr}),
                           case Module =:= ToMod of
                               true -> ok;
                               _ ->
                                   ?NOTE(#list_add{type=import, where=ToMod,
                                                   location=Module,
                                                   name = ?Fun:name(ReferredFun),
                                                   arity = ?Fun:arity(ReferredFun)})
                           end
                       end || Expr <- Exprs])
            end;
        false ->
            case exec(ReferredFun, ?Fun:definition()) of
                [] ->
                    %% BIF
                    ok;
                _ ->
                    %% local function in `FromMod'
                    ?Transform:touch(Application),
                    ?NOTE(#modq_add{expr = Application, module = FromMod}),
                    ?NOTE(#list_add{type=export, location=FromMod,
                                    name = ?Fun:name(ReferredFun),
                                    arity = ?Fun:arity(ReferredFun)})
            end
    end.


%%% ----------------------------------------------------------------------------
%%% Export/import lists

imp_list_rename(ImportForm, NewName) ->
    [NameExpr|_] = exec(ImportForm, ?Form:exprs()),
    NewNameExpr  = ?Syn:create(#expr{type = atom},
                               NewName),
    ?Syn:replace(ImportForm, {node, NameExpr}, [NewNameExpr]).

expimp_list_adds(#list_add{type = Type, where = Module, funobj = FunObj}) ->
    case Type of
        export ->
            FunDef = exec1(FunObj, ?Fun:definition(), fundef_not_found),
            ?Transform:touch(FunDef),
            ?Fun:add_export(FunObj);
        import ->
            File = exec1(Module, ?Mod:file(),
                         ?RefError(no_file,[module,Module])),
            ?Transform:touch(File),
            ?Mod:add_import(Module, FunObj)
    end.

expimp_list_dels(ListCons, Exprs, FromMod, FunDefs) ->
    %% All applications and implicit funs in the moved bodies
    %% todo: ezt szebben is lehet?
    AllApps = [App || Form <- FunDefs,
                      App  <- exec(Form, path_form_expressions()),
                      Type <- [?Expr:type(App)],
                      Type == application orelse Type == implicit_fun],

    %% File that defines the list
    File = exec1(ListCons, seq(?Expr:attrib_form(), ?Form:file()),
                 ?RefError(no_file,[attrib_form])),

    %% The expr is removable, when only the moved applications refer to it
    Removable = filter_removable_exprs(Exprs, File, FromMod, AllApps),

    %% Removable expressions in the list
    RemExprs = [E || {_, E} <- Removable],
    case exec(ListCons, ?Expr:children()) -- RemExprs of
        [] ->
            [Form] = exec(ListCons, ?Expr:attrib_form()),
            ?Transform:touch(?Syn:get_file(Form)),
            ?File:del_form(Form);
        _ ->
            [fun() -> init end]
                ++
                [fun(_) -> del_expimp(ListCons, Expr) end
                 || {_, Expr} <- Removable]
    end.

del_expimp(ListCons, Expr) ->
    Exprs  = exec(ListCons, ?Expr:children()),
    case Exprs -- [Expr] of
        [] ->
            ?File:del_form(exec1(ListCons, ?Expr:attrib_form(), form_not_found));
        _ ->
            ?Syn:replace(ListCons, {node, Expr}, [])
    end.

handle_modq_macro(add, Expr, Module) ->
    {Token, Orig} = get_funname_token(Expr),
    [Subst] = ?Query:exec(Token, [llex]),
    {Colon, Modq} = case ets:lookup(token_info, Orig) of
        [] ->
            [Body]  = ?Query:exec(Orig, [{llex, back}]),
            Index   = ?Syn:index(Body, llex, Orig),
            Colon_   = ?Syn:create_lex(':', ":"),
            Modq_    = ?Syn:create_lex('atom', io_lib:write_atom(Module)),
            ?ESG:insert(Body, {llex, Index}, Colon_),
            ?ESG:insert(Body, {llex, Index}, Modq_),
            ets:insert(token_info, #funname_tokinfo{
                       orig=Orig, colon=Colon_, modq=Modq_}),
            {Colon_, Modq_};
        [#funname_tokinfo{orig=Orig,
                      colon=Colon_,
                      modq =Modq_}] ->
            {Colon_, Modq_}
    end,
    [FunName] = ?Graph:path(Expr, ?Expr:child(1)),

    InfixExpr = ?ESG:create(
                           #expr{type=infix_expr, role=expr,
                                 value=':', pp=none}),
    InfixVirtTok = ?ESG:create(#lex{type=token, data=virtual}),
    ModqExpr = ?ESG:create(
                  #expr{type=atom, role=expr,value=Module, pp=none}),
    ModqVirtTok  = ?ESG:create(#lex{type=token, data=virtual}),
    Copy     = ?Syn:copy(FunName),
    {FunName, CFunName} = lists:keyfind(FunName, 1, Copy),
    [CFunVirtTok] = ?Query:exec(CFunName, [{elex, 1}]),
    [CSubstTok]   = ?Query:exec(CFunVirtTok, [{llex, 1}]),
    ?Graph:delete(CSubstTok),

    ?Graph:mklink(CFunVirtTok, orig, Orig),
    ?Graph:mklink(CFunVirtTok, llex, Subst),

    ?Graph:mklink(ModqVirtTok, llex, Subst),
    ?Graph:mklink(InfixVirtTok, llex, Subst),
    ?Graph:mklink(InfixVirtTok, orig, Colon),
    ?Graph:mklink(ModqVirtTok, orig, Modq),

    ?ESG:insert(InfixExpr, elex, InfixVirtTok),
    ?ESG:insert(ModqExpr, elex, ModqVirtTok),
    ?ESG:insert(InfixExpr, {esub, 1}, ModqExpr),
    ?ESG:insert(InfixExpr, {esub, 2}, CFunName),
    ?ESG:remove(Expr, esub, FunName),
    ?ESG:insert(Expr, {esub, 1}, InfixExpr);
handle_modq_macro(upd, Expr, Module) ->
    [Orig] = ?Query:exec(Expr, ?Query:seq([?Expr:child(1), ?Expr:child(1), [elex, orig]])),
    case ets:lookup(token_info, Orig) of
        [] ->
            ets:insert(token_info, #funname_tokinfo{orig=Orig, colon={}, modq={}}),
            ND=#lex{data=NND} = ?ESG:data(Orig),
            NewNode           = ND#lex{data=NND#token{text=atom_to_list(Module)}},
            ?ESG:update(Orig, NewNode);
        _ -> ok
    end.

handle_modq_macro(del, Expr) ->
    {Token, Orig} = get_funname_token(Expr),
    [Subst] = ?Query:exec(Token, [llex]),
    [Modq, _ArgList] = ?Query:exec(Expr, ?Expr:children()),
    [_ModName, FunName] = ?Query:exec(Modq, ?Expr:children()),
    Copy     = ?Syn:copy(FunName),
    {FunName, CFunName} = lists:keyfind(FunName, 1, Copy),
    [CFunVirtTok] = ?Query:exec(CFunName, [{elex, 1}]),
    [CSubstTok]   = ?Query:exec(CFunVirtTok, [{llex, 1}]),
    ?Graph:delete(CSubstTok),

    ?Graph:mklink(CFunVirtTok, orig, Orig),
    ?Graph:mklink(CFunVirtTok, llex, Subst),

    % if module qualifier is in the same macro body
    % and there are no more references to them
    % we shall delete the qualifier from the macro
    Substs   = ?Query:exec(Modq, [esub, elex, llex]),
    {Delete, ModqT, ColonT} = case length(Substs) of
        1 ->
            [Body]   = ?Query:exec(Orig, [{llex, back}]),
            Index    = ?Syn:index(Body, llex, Orig),
            [ModqT_] = ?Query:exec(Body, [{llex, Index-2}]),
            [ColonT_]= ?Query:exec(Body, [{llex, Index-1}]),
            MRefs    = ?Query:exec(ModqT_, [{orig, back}]),
            {length(MRefs) == 1, ModqT_, ColonT_};
        _ -> {false, undef, undef}
    end,

    ?ESG:remove(Expr, esub, Modq),
    ?ESG:insert(Expr, {esub,1}, CFunName),
    case Delete of
       true ->
            ?Graph:delete(ModqT),
            ?Graph:delete(ColonT);
       _    -> ok
    end.

get_funname_token(Expr) ->
    [FirstChild] = ?Query:exec(Expr, ?Expr:child(1)),
    Query = case ?Expr:type(FirstChild) of
        infix_expr -> [{esub, 2}, {elex, 1}];
        _ -> [{elex, 1}] end,
    [Token] = ?Query:exec(FirstChild, Query),
    [Orig]    = ?Query:exec(Token, ?Token:original()),
    {Token, Orig}.

%% @doc We remove that import/export lists, which are removable: have
%% no other referers, just the moved applications.
filter_removable_exprs(Exprs, File, FromMod, AllApps) ->
    lists:filter(
      fun({export, _})    -> true;
         ({import, Expr}) ->
              exec(Expr, seq([?Expr:attrib_form(),
                              ?Form:file(),
                              ?File:module()])) =/= [FromMod] orelse
                  referers(Expr, File) -- AllApps == []
      end,
      Exprs).

%% @doc Which expressions are applying the imported function in the
%% source module?
referers(ImportExpr, File) ->
    [FunRef ||
        FunRef <- exec(ImportExpr, seq(?Expr:function(), any(?Fun:applications(),
                                                             ?Fun:implicits()))),
        is_same_module_ref(FunRef, File)].

is_same_module_ref(FunRef, File) ->
    SameModule = fun() -> exec(FunRef, seq([?Expr:clause(),
                                            ?Clause:form(),
                                            ?Form:file()])) =:= [File] end,
    NoQualifier = fun() -> exec(FunRef, ?Expr:modq()) == [] end,
    case ?Expr:type(FunRef) of
        application  -> SameModule() andalso NoQualifier();
        implicit_fun -> SameModule();
        _                        -> false
    end.


%%% ----------------------------------------------------------------------------
%%% Short functions

path_form_expressions() ->
    seq([?Form:clauses(), [{functx,back}, {scope,back}], ?Clause:exprs(),
         [{{top, back}, {{type, '==', application}, 'or',
                         {type, '==', implicit_fun}}}]]).

group_list_dels(ListDels) ->
    [{List, [{Type, Expr}
             || #list_del{type=Type, item=Expr, list=L} <- ListDels, List == L]}
     || List <- lists:usort([List || #list_del{list=List} <- ListDels])].

funrefs(Fun) ->
    exec(Fun, all([?Fun:applications(), ?Fun:implicits(), ?Fun:impexps()])).
