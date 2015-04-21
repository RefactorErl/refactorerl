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

%%% @author Lilla Hajós <lya@elte.hu>

-module(refusr_sq_lib).
-vsn("$Rev: 13057 $ ").

-include("user.hrl").
-include("sq_lib.hrl").

-define(Metrics, refusr_metrics).
-define(hide_opaque, true).


-export([replace_special_chars/1]).
-export([sel_fun/2, prop_fun/2, stat_fun/1,
         sel_type/2, prop_type/2, init_sel/2, init_sel_type/1]).
-export([help_data/2]).
-export([node_type/1, file/1, mod/1, node_file/1]).
-export([error_text/2]).
-export([entity/1, entities/0, statistics/0, initial_selectors/0]).

%% @spec replace_special_chars(String::string()) -> string()
%% @doc Replaces some special characters in a query-string.
replace_special_chars(String) ->
    lists:map(
        fun (710) -> 94;    %%Converts ˆ To ^
            (733) -> 34;    %%Converts ˝ To "
            (8220) -> 34;   %%Converts “ To "
            (8221) -> 34;   %%Converts ” To "
            (C) -> C
        end, String).

%%% ============================================================================
%%% Errors

error_text(illegal_entity, Params) ->
    io_lib:format("The proper entity for the given type can't be found: ~p",
                  Params);
error_text(illegal_initial_selector, Param) ->
    io_lib:format("Illegal initial selector: ~p  ", Param);
error_text(illegal_property, Params) ->
    io_lib:format("Illegal ~p property: ~p", Params);
error_text(illegal_selector, Params) ->
    io_lib:format("Illegal ~p selector: ~p  ", Params);
error_text(illegal_statistics, Params) ->
    io_lib:format("Illegal statistics: ~p", Params).



%%% ============================================================================
%%% Callbacks

%% @type entity().

%% @spec init_sel(Params::proplist(), SelectorName::atom()) ->
%%               {Type::atom(), [entity()]}
%% @doc Returns a list of entities and their type.
init_sel(Params, InitSelName) ->

    FunL = [InitSel#initial_selector.func ||
               InitSel <- initial_selectors(),
               lists:member(InitSelName, InitSel#initial_selector.name)],
    case FunL of
        [] -> throw(?LocalError(illegal_initial_selector, [InitSelName]));
        [Fun] -> Fun(Params)
    end.

%% @spec init_sel_type(SelectorName::atom()) -> Type::atom()
%% @doc Returns the type of the initial-selector.
init_sel_type(InitSelName) ->
    Res = [InitSel#initial_selector.type ||
           InitSel <- initial_selectors(),
           lists:member(InitSelName, InitSel#initial_selector.name)],
    case Res of
        [] -> throw(?LocalError(illegal_initial_selector, [InitSelName]));
        [R|_] -> R
    end.

%% @type node() = refcore_graph:node().

%% @spec sel_fun(EntityType::atom(), SelectorName::atom()) -> [Fun]
%%       Fun = (node()) -> [node()]
%% @doc Returns a function that computes the nodes resulting by the use of the
%%      given selector.
sel_fun(EntityType, SelectorName) ->
    Entity = entity(EntityType),
    [Selector#selector.func ||
        Selector <- Entity#entity.selectors,
        lists:member(SelectorName, Selector#selector.name)].

%% @spec prop_fun(EntityType::atom(), PropertyName::atom()) -> [Fun]
%%       Fun = (node()) -> atom()
%% @doc Returns a function that computes the value of the given property.
prop_fun(EntityType, PropertyName) ->
    Entity = entity(EntityType),
    [Property#property.func ||
        Property <- Entity#entity.properties,
        lists:member(PropertyName, Property#property.name)].

%% @spec sel_type(EntityType::atom(), SelectorName::atom()) -> [atom()]
%% @doc The result is the type of the entities you'll get after the use of the
%%      given selector.
sel_type(EntityType, SelectorName) ->
    Entity = entity(EntityType),
    [Selector#selector.type ||
        Selector <- Entity#entity.selectors,
        lists:member(SelectorName, Selector#selector.name)].

%% @spec prop_type(EntityType::atom(), PropertyName::atom()) -> [atom()]
%% @doc The result is the type of the property.
prop_type(EntityType, PropertyName) ->
    Entity = entity(EntityType),
    [Property#property.type ||
        Property <- Entity#entity.properties,
        lists:member(PropertyName, Property#property.name)].


%% @spec stat_fun(StatisticsName::atom()) -> Fun
%%       Fun = ([int()]) -> term()
%% @doc Returns a function that computes  a statistics.
stat_fun(StatName) ->
    Fun = [Statistics#statistics.func ||
          Statistics <- statistics(),
          lists:member(StatName, Statistics#statistics.name)],
    case Fun of
    [] ->throw(?LocalError(illegal_statistics, [StatName]));
    _ -> hd(Fun)
    end.


%%% ============================================================================
%%% Helper functions

%% @spec node_type(Node::node()) -> atom()
%% @doc Returns the type of the node used by the semantic query.
node_type(Node) ->
    case ?Syn:node_type(Node) of
        form           -> macro;
        variable       -> variable;
        field          -> field;
        record         -> record;
        module         -> file;
        file           -> file;
        func           -> function;
        expr           -> expression;%todo: check, ?todo:param
        clause         -> clause;
        spec           -> spec;
        specparam      -> specparam;
        typexp         -> type;
        namedtype      -> type
    end.

%% @spec node_file(Node::node()) -> Node::node()
%% @doc Returns the file the node is defined in.
node_file(Nodes) -> %%TODO: most of this can be done in ?Syn
    Node = case Nodes of
            [A|_] -> A;
            _ -> Nodes
           end,
    Res = case ?Syn:node_type(Node) of
        form     -> ?Query:exec(Node, ?Form:file());
        variable -> [node_file(?Query:exec(Node, ?Var:clause()))];
        field    -> [node_file(?Query:exec(Node, ?RecField:form()))];
        record   -> [node_file(?Query:exec(Node, ?Rec:form()))];
        module   -> case ?Query:exec(Node, ?Mod:file()) of
                        [] -> [Node];
                        File -> File
                    end;
        file     -> [Node];
        func     -> [node_file(?Query:exec(Node, ?Fun:module()))];
        expr     -> [node_file(?Query:exec(Node, ?Expr:clause()))];
        clause   -> [node_file(?Query:exec(Node, ?Clause:form()))];
        spec     -> case ?Query:exec(Node, ?Spec:module()) of
                        [] -> ?Query:exec(Node, ?Spec:def_file());%wouldn't this be enough
                        Mod -> [node_file(Mod)]
                    end;
        specparam-> [node_file(?Query:exec(Node, ?SpecParam:definition()))];
        typexp   -> [?Typexp:file(Node)];
        namedtype-> [node_file(?Query:exec(Node, ?Type:module()))]
    end,
    case Res of
        [N|_] -> N;
        _ -> throw(?LocalError(illegal_node, [Node]))
    end.

%%% ============================================================================
%%% Initial selectors

initial_selectors() ->
    [#initial_selector{
        name = ['@function', '@fun'],
        type = function,
        desc = "The function indicated by the given position.",
        func = fun(Params) -> {function, [?Args:function(Params)]} end},
     #initial_selector{
        name = ['@variable', '@var'],
        type = variable,
        desc = "The variable indicated by the given position.",
        func = fun(Params) -> {variable, [?Args:variable(Params)]} end},
     #initial_selector{
        name = ['@macro'],
        type = macro,
        desc = "The macro indicated by the given position.",
        func = fun(Params) -> {macro, [?Args:macro(Params)]} end},
     #initial_selector{
        name = ['@record', '@rec'],
        type = record,
        desc = "The record indicated by the given position.",
        func = fun(Params) -> {record, [?Args:record(Params)]} end},
     #initial_selector{
        name = ['@recfield', '@field'],
        type = field,
        desc = "The record field indicated by the given position.",
        func = fun(Params) -> {field, [?Args:record_field(Params)]} end},
     #initial_selector{
        name = ['@file'],
        type = file,
        desc = "The current file.",
        func = fun(Params) -> {file, [?Args:file(Params)]} end},
     #initial_selector{
        name = [files],
        type = file,
        desc = "The files loaded into the database (including headers).",
        func = fun(_Params) -> {file, ?Query:exec([file])} end},
     #initial_selector{
        name = ['@module','@mod'],
        type = file,
        desc = "The module indicated by a given position (from a module qualifier).",
        func = fun(Params) -> {file, [?Args:module(Params)]} end},
     #initial_selector{
        name = [modules, mods],
        type = file,
        desc = "The modules loaded into the database and the modules used in them.",
        func = fun(_Params) -> {file, ?Query:exec(?Mod:all())} end},
     #initial_selector{
        name = ['@expression', '@expr'],
        type = expression,
        desc = "The expression indicated by the given position.",
        func = fun(Params) ->{expression, [expression(Params)]} end},
     #initial_selector{
        name = ['@clause'],
        type = clause,
        desc = "The clause indicated by the given position.",
        func = fun(Params) ->{clause, [?Args:clause(Params)]} end},
     #initial_selector{
        name = ['@spec'],
        type = spec,
        desc = "The spec indicated by the given position.",
        func = fun(Params) ->{spec, spec(Params)} end},
     #initial_selector{
        name = ['@type'],
        type = spec,
        desc = "The type expression indicated by the given position.",
        func = fun(Params) ->{type, typexp(Params)} end},
     #initial_selector{
        name = ['@definition', '@def'],
        type = any,
        desc = "The entity indicated by the given position.",
        func = fun(Params) -> definition(Params) end}].

spec(Params) ->
    FilePath = proplists:get_value(file, Params),
    Pos = proplists:get_value(position, Params),
    ?Query:exec(?Query:seq([?File:find(FilePath),
                            ?File:token(Pos),
                            ?Token:form(),
                            ?Form:spec()])).

typexp(Params) ->
    FilePath = proplists:get_value(file, Params),
    Pos = proplists:get_value(position, Params),
    Token = ?Query:exec(?Query:seq([?File:find(FilePath),
                            ?File:token(Pos)])),
    Type = ?Query:exec(Token, ?Query:seq(?Token:form(), ?Form:type())),
    case Type of
        [] ->
            Typexp = ?Query:exec(Token, ?Token:typexp()),
            
            case Typexp of
                [_] ->
                    main_type(Typexp);
                _ ->
                    Typexp
            end;
        T ->
            T
    end.

main_type(Typexp) ->
    case namedtype(Typexp) of
        [] ->
            case ?Typexp:parent(hd(Typexp)) of
                [P] -> main_type([P]);
                []  -> Typexp
            end;
        A -> A
    end.

expression(Params)->
    try
        ?Args:expression(Params)
    catch
        Ex = ?RefError(token_parent, [expr]) ->
            expression0(Params, Ex)
    end.

expression0(Params, ToThrow)->
    FilePath = proplists:get_value(file, Params),
    Pos = proplists:get_value(position, Params),
    MayBeSubst =
        ?Query:exec(?Query:seq([?File:find(FilePath),
                                ?File:token(Pos),
                                [{llex, back}]])),
    ?Check(length(MayBeSubst) == 1, ToThrow),
    Subst = get_subst(MayBeSubst, ToThrow),
    expression_above_subst(Subst).

get_subst([MayBeSubst], ToThrow)->
    case ?Graph:data(MayBeSubst) of
        #lex{type=subst} ->
            MayBeSubst;
        #lex{type=param} ->
            get_subst(?Query:exec(MayBeSubst,[{llex,back}]), ToThrow);
        _ -> throw(ToThrow)
    end;
get_subst(_, ToThrow)->
    throw(ToThrow).

expression_above_subst(Subst)->
    Exprs = get_expr_from_mac_ref(Subst),
    last_common_node(?Syn:root_path(hd(Exprs)),
                     ?Syn:root_path(lists:last(Exprs))).

definition(Params) ->
    FilePath = proplists:get_value(file, Params),
    Pos = proplists:get_value(position, Params),
    Entity0 = case ?Query:exec(?Query:seq(?File:find(FilePath), ?File:token(Pos))) of
        [Token] ->
            ?Query:exec(
               Token,
               ?Query:any( %todo: vegiggondolni + casenel mi tortenik?
                  [ [{llex,back}, mref], %macro ref
                    ?Query:seq(?Token:expr(), ?Expr:variables()), %var
                    ?Query:seq(?Token:expr(), ?Expr:field()), %field ref
                    ?Query:seq(?Token:expr(), ?Expr:record()), %recref
                    ?Query:seq(?Token:typexp(), ?Expr:fielddef()), %field def
                    ?Query:seq(?Token:expr(), ?Expr:module()), %modq
                    ?Query:seq([?Token:expr(), ?Expr:parent(), ?Expr:function()]),
                    ?Query:seq([?Token:expr(), ?Expr:parent(),
                                ?Expr:parent(), ?Expr:function()]), %funref infix
                    ?Query:seq([?Token:expr(), ?Expr:nameof(),
                                ?Clause:form(), ?Form:func()]), %fundef
                    [{{flex,back}, {type, '==', macro}}], %macro form
                    ?Query:seq(?Token:form(), ?Form:record()) ])); %rec form
        _ -> []
    end,
    Entity = case Entity0 of
        [] ->
            case spec(Params) of
                [] -> typexp(Params);
                S -> S
            end;
        E -> E
    end,
    Type = case Entity of
               []     -> none;
               %todo: can the elements of the list be of different types?
               [Node|_] -> node_type(Node)
           end,
    {Type, Entity}.


%%% ============================================================================
%%% Entities

%% @spec entity(Type::atom()) -> entity()
%% @doc Returns the entity record determined by `Type'.
entity(Type) ->
    case lists:keyfind(Type, 2, entities()) of
        false -> throw(?LocalError(illegal_entity, [Type]));
        Rec   -> Rec
    end.

fun_param_types(Param) ->
    Index = expr_index(Param),
    Clauses = ?Query:exec(Param, ?Query:seq([?Expr:clause(),
                                             ?Clause:form(),
                                             ?Form:func(),
                                             ?Fun:spec(),
                                             ?Spec:clauses()])),
    SpecParams = lists:append(lists:map(
        fun(Clause) ->
            ?SpecClause:specpar(Clause, Index) end, Clauses)),
    Typexp = ?Query:exec(SpecParams, ?SpecParam:definition()),
    prefer_namedtypes(filter_atom_typexps(Typexp)).

entities() ->
    Expr = lists:keyfind(expression, 2, entities0()),
    entities0() ++
    [#entity{
        name = param,
        selectors = Expr#entity.selectors ++
        [#selector{
            name = [type, types],
            type = type,
            desc = "Possible types of the argument according to the spec.",
            func = fun(Param) ->
                        lists:map(fun fun_param_types/1, lists:flatten([Param]))
                     end}],
        properties = Expr#entity.properties}].

entities0() ->

%%% ============================================================================
%%% File entity

    [#entity{
        name = file,
        selectors =
        [#selector{
            name = [functions, function, funs, 'fun'],
            type = function,
            desc = "The functions defined in the file.",
            func = fun(File) -> ?Query:exec(mod(File), ?Mod:locals()) end},

         #selector{
            name = [typerefs],
            type = type,
            desc = "The types used in the file.",
            func = fun(File) -> 
                        Mod = mod(File),
                        Specs = ?Query:exec(Mod, ?Mod:specs()),
                        SpecTypes = lists:append(
                            [?Spec:named_typerefs(Spec) || Spec <- Specs]),
                        Types = ?Query:exec(Mod, ?Mod:types()),
                        TypeTypes = lists:append(
                            [?Type:named_subtypes(Type) || Type <- Types]),
                        Records = ?Query:exec(file(File), ?File:records()),
                        RecTypes = lists:append(
                            [?Rec:named_typerefs(Rec) || Rec <- Records]),
                        RecTypes ++ TypeTypes ++ SpecTypes
                    end},

         #selector{
            name = [typedef, typedefs, types, type],
            type = type,
            desc = "The types defined in the file.",
            func = fun(File) -> ?Query:exec(mod(File), ?Mod:typedefs()) end},

         #selector{
            name = [spec, specs],
            type = spec,
            desc = "Specs defined in the file.",
            func = fun(File) -> ?Query:exec(file(File), ?Query:seq([?File:forms(),
                                                                    ?Form:spec()]))
                   end},

         #selector{
            name = [records, record, recs, rec],
            type = record,
            desc = "The records defined in the file.",
            func = fun(File) -> ?Query:exec(file(File), ?File:records()) end},

         #selector{
            name = [macros, macro],
            type = macro,
            desc = "The macros defined in the file.",
            func = fun(File) -> ?Query:exec(file(File), ?File:macros()) end},

         #selector{
            name = [includes],
            type = file,
            desc = "The files included by the given file.",
            func = fun(File) ->
                           FileNode = file(File),
                           ?Query:exec(FileNode, ?File:includes()) -- [FileNode]
                   end},

         #selector{
            name = [included_by],
            type = file,
            desc = "The files including the given file.",
            func = fun(File) ->
                           FileNode = file(File),
                           ?Query:exec(FileNode, ?File:included()) -- [FileNode]
                   end},

         #selector{
            name = [imports],
            type = function,
            desc = "The imported functions of the file.",
            func = fun(File) -> ?Query:exec(mod(File), ?Mod:imports()) end},

         #selector{
            name = [exports],
            type = function ,
            desc = "The exported functions of the file.",
            func = fun(File) -> ?Query:exec(mod(File), ?Mod:exports()) end}
        ],

%%% ----------------------------------------------------------------------------
%%% File properties

        properties =
        [#property{
            name = [is_module, is_mod, module, mod],
            type = bool,
            desc = "Determines whether the file is module or not.",
            func = fun file_is_module/1},

         #property{
            name = [is_header, header],
            type = bool,
            desc = "Determines whether the file is header or not.",
            func = fun(File) -> not file_is_module(File) end},

         #property{
            name = [name],
            type = atom,
            desc = "The name of a file(without extension).",
            func = fun(File) -> file_prop(name, File) end},

         #property{
            name = [filename],
            type = string,
            desc = "The filename(with extension).",
            func = fun(File) -> file_prop(filename, File) end},

         #property{
            name = [directory, dir],
            type = string,
            desc = "The directory of the file.",
            func = fun(File) -> file_prop(dir, File) end},

         #property{
            name = [path],
            type = string,
            desc = "The path to the file.",
            func = fun(File) -> file_prop(path, File)  end},

%%%.............................................................................
%%% Metrics as properties
         #property{
            name = [module_sum, mod_sum],
            type = int,
            func = fun(File) -> file_metrics(module_sum, File, true) end},

         #property{
            name = [line_of_code, loc],
            type = int,
            func = fun(File) -> file_metrics(line_of_code, File, true) end},

         #property{
            name = [char_of_code, choc],
            type = int,
            func = fun(File) -> file_metrics(char_of_code, File, true) end},

         #property{
            name = [number_of_fun, num_of_fun, num_of_funs,
                    num_of_functions, number_of_functions, funnum],
            type = int,
            func = fun(File) -> file_metrics(number_of_fun, File, false) end},

         #property{
            name = [number_of_macros, num_of_macros, num_of_mac, macnum] ,
            type = int,
            func = fun(File) -> file_metrics(number_of_macros, File, true) end},

         #property{
            name = [number_of_records, num_of_records, num_of_rec, recnum],
            type = int,
            func = fun(File) ->
                           file_metrics(number_of_records, File, true)
                   end},

         #property{
            name = [included_files, inc_files],
            type = int,
            func = fun(File) -> file_metrics(included_files, File, true) end},

         #property{
            name = [imported_modules, imp_modules, imported_mod, imp_mod,
            impmods],
            type = int,
            func = fun(File) -> file_metrics(imported_modules, File, true) end},

         #property{
            name = [number_of_funpath, number_of_funpaths,
                    num_of_funpath, num_of_funpaths,funpathnum],
            type = int,
            func = fun(File) ->
                           file_metrics(number_of_funpath, File, true)
                   end},

         #property{
            name = [function_calls_in, fun_calls_in,callsin],
            type = int,
            func = fun(File) ->
                           file_metrics(function_calls_in, File, true)
                   end},

         #property{
            name = [function_calls_out, fun_calls_out,callsout],
            type = int,
            func = fun(File) ->
                           file_metrics(function_calls_out, File, true)
                   end},

         #property{
            name = [cohesion, coh],
            type = int,
            func = fun(File) -> file_metrics(cohesion, File, true) end},

         #property{
            name = [otp_used, otp],
            type = int,
            func = fun(File) -> file_metrics(otp_used, File, true) end},

         #property{
            name = [max_application_depth, max_app_depth, maxappdepth],
            type = int,
            func = fun(File) ->
                           file_metrics(max_application_depth, File, true)
                   end},

          #property{
            name = [max_depth_of_calling, max_depth_calling,
                    max_depth_of_call, max_depth_call, maxcalldepth],
            type = int,
            func = fun(File) ->
                           file_metrics(max_depth_of_calling, File, true)
                   end},

          #property{
            name = [min_depth_of_calling, min_depth_calling,
                    min_depth_of_call, min_depth_call, mincalldepth],
            type = int,
            func = fun(File) ->
                           file_metrics(min_depth_of_calling, File, true)
                   end},

         #property{
            name = [max_depth_of_cases, max_depth_cases, maxcasedepth],
            type = int,
            func = fun(File) ->
                           file_metrics(max_depth_of_cases, File, true)
                   end},
         #property{
            name = [min_depth_of_cases, min_depth_cases, mincasedepth],
            type = int,
            func = fun(File) ->
                           file_metrics(min_depth_of_cases, File, true)
                   end},
        #property{
            name = [max_depth_of_structs, max_depth_structs,
            max_depth_of_structures, max_depth_structures, maxstrdepth],
            type = int,
            func = fun(File) ->
                           file_metrics(max_depth_of_structs, File, true)
                   end},

         #property{
            name = [number_of_funclauses, num_of_funclauses,
                    number_of_funclaus, num_of_funclaus, funclnum],
            type = int,
            func = fun(File) ->
                           file_metrics(number_of_funclauses, File, true)
                   end},

         #property{
            name = [branches_of_recursion, branches_of_rec,
                    branch_of_recursion, branch_of_rec, recbranches],
            type = int,
            func = fun(File) ->
                           file_metrics(branches_of_recursion, File, true)
                   end},

         #property{
            name = [mcCabe, mccabe],
            type = int,
            func = fun(File) -> file_metrics(mcCabe, File, true) end},

         #property{
            name = [number_of_funexpr, num_of_funexpr, funexprnum],
            type = int,
            func = fun(File) ->
                           file_metrics(number_of_funexpr, File, true)
                   end},

         #property{
            name = [number_of_messpass, messpassnum],
            type = int,
            func = fun(File) ->
                           file_metrics(number_of_messpass, File, true)
                   end},

         #property{
            name = [fun_return_points, fun_return_point, funretpoints,
                    function_return_points, function_return_point],
            type = int,
            func = fun(File) ->
                           file_metrics(fun_return_points, File, true)
                   end},

         #property{
            name = [max_length_of_line, maxlinelength],
            type = int,
            func = fun(File) ->
                           file_metrics(max_length_of_line, File, true)
                   end},

         #property{
            name = [average_length_of_line, avg_length_of_line, avglinelength],
            type = int,
            func = fun(File) ->
                           file_metrics(average_length_of_line, File, true)
                   end},

         #property{
            name = [no_space_after_comma],
            type = int,
            func = fun(File) ->
                           file_metrics(no_space_after_comma, File, true)
                   end}

        ]},

%%% ============================================================================
%%% Function entity

     #entity{
        name = function,
        selectors =
        [#selector{
            name = [references, reference, refs, ref],
            type = expression,
            desc = "Function references (including unambiguous dynamic).",
            func = fun(Fun) ->
                           ?Query:exec(Fun, ?Query:all([?Fun:applications(),
                                                        ?Fun:implicits(),
                                                        ?Fun:impexps(),
                                                        ?Dynfun:dynfun_call()]))
                   end},

         #selector{
            name = [dynamic_references, dynrefs, dynref],
            type = expression,
            desc = "The dynamic references of the given function.",
            func = fun(Fun) ->
                           ?Query:exec(Fun, ?Query:all([?Dynfun:dynfun_call(),
                                                        ?Dynfun:ambdyn_call()]))
                   end},

         #selector{
            name = [calls],
            type = function,
            desc = "The functions called by the given function"
                    " (including the unambiguous dynamic calls).",
            func =  fun(Fun) ->
                        ?Query:exec(Fun, ?Query:all([   ?Fun:funcalls(),
                                                        ?Dynfun:dyncalls()]))
                    end},

         #selector{
            name = [dynamic_calls, dyncalls, dyncall],
            type = function,
            desc = "The dynamic calls from the given function.",
            func = fun(Fun) ->
                           filter_opaques(
                               ?Query:exec(Fun, ?Query:all([dyncall], [ambcall])))
                   end},

         #selector{
            name = [called_by],
            type = function,
            desc = "The functions calling the given function"
                    " (including the unambiguous dynamic calls).",
            func =  fun(Fun) -> 
                        filter_opaques( %%Is it possible for opaque funs to call anything?
                            ?Query:exec(Fun, ?Query:all([?Fun:called(),
                                                        [{dyncall, back}]])))
                    end},

         #selector{
            name = [dynamic_called_by, dyncalled_by],
            type = function,
            desc = "The functions calling the given function (dynamic calls).",
            func = fun(Fun) ->
                           Query = ?Query:all([{dyncall,back}], [{ambcall,back}]),
                           filter_opaques(?Query:exec(Fun, Query))
                   end},

         #selector{
            name = [arguments, argument, args, arg, parameters, parameter,
                    params, param],
            type = param,
            desc = "The arguments of the given function.",
            func = fun(Fun) ->
                           ?Query:exec(Fun, ?Query:seq([?Fun:definition(),
                                                        ?Form:clauses(),
                                                        ?Clause:patterns()]))
                   end},

         #selector{
            name = [body],
            type = expression,
            desc = "The body of the given function.",
            func = fun(Fun) ->
                           ?Query:exec(Fun, ?Query:seq([?Fun:definition(),
                                                        ?Form:clauses(),
                                                        ?Clause:body()]))
                   end},

         #selector{
            name = [expressions, expression, exprs, expr],
            type = expression,
            desc = "The expressions of the given function.",
            func = fun(Fun) ->
                           ?Query:exec(Fun, ?Query:seq([?Fun:definition(),
                                                        ?Form:clauses(),
                                                        ?Clause:exprs()]))
                   end},

         #selector{
            name = [variables, variable, vars, var],
            type = variable,
            desc = "The variables of the given function.",
            func = fun(Fun) ->
                           ?Query:exec(Fun, ?Query:seq([?Fun:definition(),
                                                        ?Form:clauses(),
                                                        ?Clause:variables()]))
                   end},

         #selector{
            name = [file],
            type = file,
            desc = "The file defining the given function.",
            func = fun(Fun) -> ?Query:exec(Fun, ?Fun:module()) end},

         #selector{
            name = [clause],
            type = clause,
            desc = "The clauses of the given function.",
            func = fun(Fun) -> ?Query:exec(Fun,
                                           ?Query:seq([?Fun:definition(),
                                                       ?Form:clauses()]))
                   end},

         #selector{
            name = [spec],
            type = spec,
            desc = "Specification of the given function.",
            func = fun(Fun) -> ?Query:exec(Fun, ?Fun:spec()) end},

         #selector{
            name = [returntype, returntypes, rettype],
            type = type,
            desc = "Possible return types of the function according to the spec.",
            func = fun(Fun) ->
                        Typexp = ?Query:exec(Fun, ?Query:seq(
                                                    ?Fun:spec(),
                                                    ?Spec:returntypes())),
                        prefer_namedtypes(filter_atom_typexps(Typexp)) end}
        ],

%%% ----------------------------------------------------------------------------
%%% Function properties

        properties =
        [#property{
            name = [exported],
            type = bool,
            desc = "Returns whether the function is exported.",
            func = fun(Fun) ->
                           case ?Query:exec(Fun, ?Fun:definition()) of
                               [] -> true;
                               _  ->  ?Fun:is_exported(Fun)
                           end
                   end},

         #property{
            name = [name],
            type = atom,
            desc = "The name of the function.",
            func = fun ?Fun:name/1},

         #property{
            name = [arity],
            type = int,
            desc = "The arity of the function.",
            func = fun ?Fun:arity/1},

         #property{
            name = [bif],
            type = bool,
            desc = "Returns whether the function is a BIF.",
            func = fun(Fun) ->
                           ?Fun:is_autoimported(?Fun:name(Fun), ?Fun:arity(Fun))
                   end},

         #property{
            name = [pure],
            type = bool,
            desc = "Returns whether the function is pure.",
            func = fun(Fun) -> not ?Fun:is_dirty(Fun) end},

         #property{
            name = [defined],
            type = bool,
            desc = "Returns whether the code of function is present in the database.",
            func = fun(Fun) -> ?Query:exec(Fun, ?Fun:definition()) /= [] end},

         #property{
            name = [is_dirty, dirty],
            type = bool,
            desc = "Returns whether the function is dirty.",
            func = fun ?Fun:is_dirty/1},

         #property{
            name = [module, mod],
            type = atom,
            desc = "Returns the module where the function is defined.",
            func = fun(Fun) ->
                           ?Mod:name(hd(?Query:exec(Fun, ?Fun:module())))
                   end},

%%%.............................................................................
%%% Metrics as properties

         #property{
            name = [otp_used, otp],
            type = int,
            func = fun(Fun) -> fun_metrics(otp_used, Fun) end},

         #property{
            name = [line_of_code, loc],
            type = int,
            func = fun(Fun) -> fun_metrics(line_of_code, Fun) end},

         #property{
            name = [char_of_code, choc],
            type = int,
            func = fun(Fun) -> fun_metrics(char_of_code, Fun) end},

         #property{
            name = [function_sum, fun_sum],
            type = int,
            func = fun(Fun) -> fun_metrics(function_sum, Fun) end},

         #property{
            name = [max_application_depth, max_app_depth],
            type = int,
            func = fun(Fun) -> fun_metrics(max_application_depth, Fun) end},

         #property{
            name = [max_depth_of_calling, max_depth_calling,
                    max_depth_of_call, max_depth_call],
            type = int,
            func = fun(Fun) -> fun_metrics(max_depth_of_calling, Fun) end},

         #property{
            name = [min_depth_of_calling, min_depth_calling,
                    min_depth_of_call, min_depth_call],
            type = int,
            func = fun(Fun) -> fun_metrics(min_depth_of_calling, Fun) end},

         #property{
            name = [max_depth_of_cases, max_depth_cases],
            type = int,
            func = fun(Fun) -> fun_metrics(max_depth_of_cases, Fun) end},

         #property{
            name = [min_depth_of_cases, min_depth_cases],
            type = int,
            func = fun(Fun) -> fun_metrics(min_depth_of_cases, Fun) end},

         #property{
            name = [max_depth_of_structs, max_depth_structs, max_depth_of_structures, max_depth_structures],
            type = int,
            func = fun(Fun) -> fun_metrics(max_depth_of_structs, Fun) end},

         #property{
            name = [number_of_funclauses, num_of_funclauses,
                    number_of_funclaus, num_of_funclaus],
            type = int,
            func = fun(Fun) -> fun_metrics(number_of_funclauses, Fun) end},

         #property{
            name = [branches_of_recursion, branches_of_rec,
                    branch_of_recursion, branch_of_rec],
            type = int,
            func = fun(Fun) -> fun_metrics(branches_of_recursion, Fun) end},

         #property{
            name = [mcCabe, mccabe],
            type = int,
            func = fun(Fun) -> fun_metrics(mcCabe, Fun) end},

         #property{
            name = [calls_for_function, calls_for_fun,
                    call_for_function, call_for_fun],
            type = int,
            func = fun(Fun) -> fun_metrics(calls_for_function, Fun) end},

         #property{
            name = [calls_from_function, calls_from_fun,
                   call_from_function, call_from_fun],
            type = int,
            func = fun(Fun) -> fun_metrics(calls_from_function, Fun) end},

         #property{
            name = [number_of_funexpr, num_of_funexpr],
            type = int,
            func = fun(Fun) -> fun_metrics(number_of_funexpr, Fun) end},

         #property{
            name = [number_of_messpass],
            type = int,
            func = fun(Fun) -> fun_metrics(number_of_messpass, Fun) end},

         #property{
            name = [fun_return_points, fun_return_point,
                    function_return_points, function_return_point],
            type = int,
            func = fun(Fun) -> fun_metrics(fun_return_points, Fun) end},

         #property{
            name = [max_length_of_line],
            type = int,
            func = fun(Fun) -> fun_metrics(max_length_of_line, Fun) end},

         #property{
            name = [average_length_of_line, avg_length_of_line],
            type = int,
            func = fun(Fun) -> fun_metrics(average_length_of_line, Fun) end},

         #property{
            name = [no_space_after_comma],
            type = int,
            func = fun(Fun) -> fun_metrics(no_space_after_comma, Fun) end},

         #property{
            name = [is_tail_recursive, is_tail_rec],
            type = atom,
            func = fun(Fun) -> fun_metrics(is_tail_recursive, Fun) end}

        ]},

%%% ============================================================================
%%% Function clause entity

     #entity{
        name = clause,
        selectors =
        [#selector{
            name = [calls],
            type = function,
            desc = "The functions called by the given function clause"
                   " (including the unambiguous dynamic calls).",
            func = fun(Cl) ->
                           Exps = ?Query:exec(Cl, ?Query:seq(?Clause:exprs(),
                                                             ?Expr:deep_sub())),
                           filter_opaques( 
                               ?Query:exec(Exps, ?Query:all(?Expr:function(), 
                                                            ?Expr:dynfunction())))
                   end},

         #selector{
            name = [dynamic_calls, dyncalls, dyncall],
            type = function,
            desc = "The dynamic calls from the given function clause.",
            func = fun(Cl) ->
                          Query = ?Query:seq([?Clause:exprs(),
                                              ?Expr:deep_sub(),
                                              ?Expr:ambdynfunction()]),
                          filter_opaques(?Query:exec(Cl, Query))
                   end},

         #selector{
            name = [arguments, argument, args, arg, parameters, parameter,
                    params, param],
            type = param,
            desc = "The arguments of the given function clause.",
            func = fun(Cl) ->
                           ?Query:exec(Cl, ?Clause:patterns())
                   end},

         #selector{
            name = [body],
            type = expression,
            desc = "The body of the given function clause.",
            func = fun(Cl) ->
                           ?Query:exec(Cl, ?Clause:body())
                   end},

         #selector{
            name = [expressions, expression, exprs, expr],
            type = expression,
            desc = "The expressions of the given function clause.",
            func = fun(Cl) ->
                           ?Query:exec(Cl, ?Clause:exprs())
                   end},

         #selector{
            name = [variables, variable, vars, var],
            type = variable,
            desc = "The variables of the given function clause.",
            func = fun(Cl) ->
                           ?Query:exec(Cl, ?Clause:variables())
                   end},

         #selector{
            name = [file, module],
            type = file,
            desc = "The file defining the function of the given clause.",
            func = fun(Cl) ->
                           ?Query:exec(Cl, ?Query:seq([?Clause:form(),
                                                       ?Form:func(),
                                                       ?Fun:module()]))
                   end},

         #selector{
            name = [function, func],
            type = function,
            desc = "The function of the given clause.",
            func = fun(Cl) ->
                           ?Query:exec(Cl, ?Query:seq(?Clause:form(),
                                                      ?Form:func()))
                   end},

         #selector{
            name = [spec],
            type = spec,
            desc = "The specification of the function defining"
                   " the given function clause.",
            func = fun(Fun) -> ?Query:exec(Fun, ?Query:seq([
                                                    ?Clause:form(),
                                                    ?Form:func(),
                                                    ?Fun:spec()])) end},

         #selector{
            name = [returntypes, returntype],
            type = type,
            desc = "Possible return types of the clause according to the spec.",
            func = fun(Fun) -> 
                        Typexp = ?Query:exec(Fun, ?Query:seq([
                                                    ?Clause:form(),
                                                    ?Form:func(),
                                                    ?Fun:spec(),
                                                    ?Spec:returntypes()])),
                        prefer_namedtypes(filter_atom_typexps(Typexp)) end}
        ],

%%% ----------------------------------------------------------------------------
%%% Function clause properties

        properties =
        [#property{
            name = [macro_value, macro_val, macrovalue, macval],
            type = string,
            desc = "The value of the macro contained by the function clause.",
            func = fun ?Expr:macro_value/1},
         #property{
            name = [name],
            type = atom,
            desc = "The name of the function defining the "
                   "given function clause.",
            func = fun(Cl) ->
                           [Fun] = ?Query:exec(Cl, ?Query:seq(?Clause:form(),
                                                              ?Form:func())),
                           ?Fun:name(Fun)
                   end},

         #property{
            name = [arity],
            type = int,
            desc = "The arity of the function defining the "
                   "given function clause.",
            func = fun(Cl) ->
                           [Fun] = ?Query:exec(Cl, ?Query:seq(?Clause:form(),
                                                              ?Form:func())),
                           ?Fun:arity(Fun)
                   end},

         %% TODO: Implement it. Maybe, this can be usefull
         %% #property{
         %%    name = [pure],
         %%    type = bool,
         %%    desc = "Returns whether the function clause is pure.",
         %%    func = fun(Fun) -> not ?Fun:is_dirty(Fun) end},

         %% #property{
         %%    name = [is_dirty, dirty],
         %%    type = bool,
         %%    desc = "Returns whether the function clause is dirty.",
         %%    func = fun ?Fun:is_dirty/1},

         #property{
            name = [module, mod],
            type = atom,
            desc = "Returns the module where the function"
                    " of the function clause is defined.",
            func = fun(Cl) ->
                           [Mod] = ?Query:exec(Cl, ?Query:seq([?Clause:form(),
                                                               ?Form:func(),
                                                               ?Fun:module()])),
                           ?Mod:name(Mod)
                   end},

        #property{
            name = [index],
            type = int,
            desc = "The index of the given clause",
            func = fun(Cl) ->
                           [Form] = ?Query:exec(Cl, ?Clause:form()),
                           refcore_syntax:index(Form, funcl, Cl)
                   end},

%%%.............................................................................
%%% Metrics as properties for function clauses

         #property{
            name = [line_of_code, loc],
            type = int,
            func = fun(Cl) -> fun_metrics(line_of_code, {clause, Cl}) end},

         #property{
            name = [char_of_code, choc],
            type = int,
            func = fun(Cl) -> fun_metrics(char_of_code, {clause, Cl}) end},

         #property{
            name = [function_sum, fun_sum],
            type = int,
            func = fun(Cl) -> fun_metrics(function_sum, {clause, Cl}) end},

         #property{
            name = [max_application_depth, max_app_depth],
            type = int,
            func = fun(Cl) ->
                           fun_metrics(max_application_depth, {clause, Cl})
                   end},

         #property{
            name = [max_depth_of_calling, max_depth_calling,
                    max_depth_of_call, max_depth_call],
            type = int,
            func = fun(Cl) ->
                           fun_metrics(max_depth_of_calling, {clause, Cl})
                   end},

         #property{
            name = [min_depth_of_calling, min_depth_calling,
                    min_depth_of_call, min_depth_call],
            type = int,
            func = fun(Cl) ->
                           fun_metrics(min_depth_of_calling, {clause, Cl})
                   end},

         #property{
            name = [max_depth_of_cases, max_depth_cases],
            type = int,
            func = fun(Cl) ->
                           fun_metrics(max_depth_of_cases, {clause, Cl})
                   end},

          #property{
            name = [min_depth_of_cases, min_depth_cases],
            type = int,
            func = fun(Cl) ->
                           fun_metrics(min_depth_of_cases, {clause, Cl})
                   end},

         #property{
            name = [max_depth_of_structs, max_depth_structs,
                    max_depth_of_structures, max_depth_structures],
            type = int,
            func = fun(Cl) ->
                           fun_metrics(max_depth_of_structs, {clause, Cl})
                   end},

         #property{
            name = [number_of_funclauses, num_of_funclauses,
                    number_of_funclaus, num_of_funclaus],
            type = int,
            func = fun(Cl) ->
                           fun_metrics(number_of_funclauses, {clause, Cl})
                   end},

         #property{
            name = [branches_of_recursion, branches_of_rec,
                    branch_of_recursion, branch_of_rec],
            type = int,
            func = fun(Cl) ->
                           fun_metrics(branches_of_recursion, {clause, Cl})
                   end},

         #property{
            name = [mcCabe, mccabe],
            type = int,
            func = fun(Cl) -> fun_metrics(mcCabe, {clause, Cl}) end},

         #property{
            name = [calls_for_function, calls_for_fun,
                    call_for_function, call_for_fun],
            type = int,
            func = fun(Cl) ->
                           fun_metrics(calls_for_function, {clause, Cl})
                   end},

         #property{
            name = [calls_from_function, calls_from_fun,
                   call_from_function, call_from_fun],
            type = int,
            func = fun(Cl) ->
                           fun_metrics(calls_from_function, {clause, Cl})
                   end},

         #property{
            name = [number_of_funexpr, num_of_funexpr],
            type = int,
            func = fun(Cl) -> fun_metrics(number_of_funexpr, {clause, Cl}) end},

         #property{
            name = [number_of_messpass, num_of_messpass],
            type = int,
            func = fun(Cl) ->
                           fun_metrics(number_of_messpass, {clause, Cl})
                   end},

         #property{
            name = [fun_return_points, fun_return_point,
                    function_return_points, function_return_point],
            type = int,
            func = fun(Cl) -> fun_metrics(fun_return_points, {clause, Cl}) end},

         #property{
            name = [max_length_of_line],
            type = int,
            func = fun(Cl) ->
                           fun_metrics(max_length_of_line, {clause, Cl})
                   end},

         #property{
            name = [average_length_of_line, avg_length_of_line],
            type = int,
            func = fun(Cl) ->
                           fun_metrics(average_length_of_line, {clause, Cl})
                   end},

         #property{
            name = [no_space_after_comma],
            type = int,
            func = fun(Cl) ->
                           fun_metrics(no_space_after_comma, {clause, Cl})
                   end},

         #property{
            name = [is_tail_recursive, is_tail_rec],
            type = atom,
            func = fun(Cl) -> fun_metrics(is_tail_recursive, {clause, Cl}) end}

        ]},

%%% ============================================================================
%%% Variable entity

     #entity{
        name = variable,
        selectors =
        [#selector{
            name = [references, reference, refs, ref],
            type = expression,
            desc = "The references of the variable.",
            func = fun(Var) -> ?Query:exec(Var, ?Var:references()) end},

         #selector{
            name = [bindings],
            type = expression,
            func = fun(Var) -> ?Query:exec(Var, ?Var:bindings()) end},

         #selector{
            name = [function_definition, function_def, fun_def, fundef],
            type = function,
            desc = "The function where the variable is used.",
            func = fun(Var) -> ?Query:exec(Var, ?Query:seq([?Var:clause(),
                                                            ?Clause:form(),
                                                            ?Form:func()]))
                   end}%,
% TODO
%         #selector{
%            name = visib,
%            type = expression,
%            func = undef}
%         #selector{name = file, type = file, func = undef}
        ],

%%% ----------------------------------------------------------------------------
%%% Variable properties

        properties =
        [#property{
            name = [name],
            type = string,
            desc = "The name of the variable.",
            func = fun ?Var:name/1}
        ]},

%%% ============================================================================
%%% Spec entity

#entity{
        name = spec,
        selectors =
        [#selector{
            name = ['fun', func, function],
            type = function,
            desc = "The function that has this spec.",
            func = fun(Spec) -> ?Query:exec(Spec, ?Spec:references()) end},
         #selector{
            name = [file, deffile],
            type = file,
            desc = "The file in which the spec was defined.",
            func = fun(Spec) -> ?Query:exec(Spec, ?Spec:def_file()) end},
         #selector{
            name = [modref, refmod],
            type = file,
            desc = "The module to which the spec pertains.",
            func = fun(Spec) -> ?Query:exec(Spec, ?Spec:module()) end},
         #selector{
            name = [arguments, args, params, parameters, param, arg, argument],
            type = specparam,
            desc = "Arguments of the spec.",
            func = fun(Spec) ->
                        ?Query:exec(Spec, ?Query:seq([?Spec:clauses(),
                                                      ?SpecClause:specpars()]))
                    end},
         #selector{
            name = [guardtype, guardtypes],
            type = type,
            desc = "Types referenced in the guards of the spec.",
            func = fun(Spec) ->
                        Typexp = ?Query:exec(Spec, ?Query:seq([
                                                    ?Spec:clauses(),
                                                    ?SpecClause:specguard(),
                                                    ?SpecGuard:definition()])),
                        prefer_namedtypes(filter_atom_typexps(Typexp))
                    end},
         #selector{
            name = [returntype, returntypes],
            type = type,
            desc = "Possible return types of the spec.",
            func = fun(Spec) ->
                        Typexp = ?Query:exec(Spec, ?Spec:returntypes()),
                        prefer_namedtypes(filter_atom_typexps(Typexp))
                    end}],

%%% ----------------------------------------------------------------------------
%%% Spec properties

        properties = 
        [#property{
            name = [funname, name],
            type = atom,
            desc = "Name of the function spec.",
            func = fun ?Spec:name/1},
         #property{
            name = [arity],
            type = int,
            desc = "Arity of the spec.",
            func = fun ?Spec:arity/1},
         #property{
            name = [guardtext],
            type = string,
            desc = "String representation of the specs guards.",
            func = fun(Spec) ->
                        Guards = ?Query:exec(Spec,
                            ?Query:seq([?Spec:clauses(),
                                        ?SpecClause:specguard()])),
                        case Guards of
                            [] -> undefined;
                            _ ->
                                GuardTexts = lists:map(fun ?SpecGuard:value/1,
                                                        Guards),
                                lists:append(?MISC:join(GuardTexts, "|"))
                        end
                    end},
         #property{
            name = [returntext],
            type = string,
            desc = "String representation of the specs return values.",
            func = fun(Spec) ->
                        Rets = ?Query:exec(Spec,
                            ?Query:seq([?Spec:clauses(),
                                        ?SpecClause:specret()])),
                        RetTexts = lists:map(fun ?SpecParam:value/1, Rets),
                        lists:append(?MISC:join(RetTexts, "|"))
                    end},
         #property{
            name = [text],
            type = string,
            desc = "String representation of the spec.",
            func = fun(Spec) ->
                        [Form] = ?Query:exec(Spec, ?Spec:definition()),
                        ?Syn:flat_text2(Form)
                    end}
        ]},

%%% ============================================================================
%%% SpecParam entity

#entity{
        name = specparam,
        selectors = %% TODO: reference to function expression (fpar)
        [#selector{
            name = [type],
            type = type,
            desc = "The type referenced by the spec-parameter.",
            func = fun(Par) ->
                        Typexp = ?Query:exec(Par, ?SpecParam:definition()),
                        prefer_namedtypes(filter_atom_typexps(Typexp)) end}
         ],

%%% ----------------------------------------------------------------------------
%%% SpecParam properties

        properties = 
        [#property{
            name = [text],
            type = string,
            desc = "String representation of the spec-parameter.",
            func = fun ?SpecParam:value/1},
         #property{
            name = [name],
            type = string,
            desc = "Name of the spec-parameter.",
            func = fun(Param) -> ?MISC:any_to_string(?SpecParam:name(Param)) end},
         #property{
            name = [index],
            type = int,
            desc = "Index of the spec-parameter.",
            func = fun ?SpecParam:index/1}
        ]},


%%% ============================================================================
%%% Type entity

#entity{
        name = type,
        selectors =
        [#selector{
            name = [file],
            type = file,
            desc = "The defining file of the type.",
            func = mult_type_fun([
                        {namedtype, fun(Type) ->
                            ?Query:exec(Type, ?Type:module()) end},
                        {typexp, fun(Type) ->
                            case namedtype(Type) of
                                [NamedType] ->
                                    ?Query:exec(NamedType, ?Type:module());
                                _ ->
                                    [?Typexp:file(Type)]
                            end end}
                        ])},

         #selector{
            name = [params, param, typeparams, typeparam,
                    namedtypeparams, namedtypeparam, par, pars],
            type = type,
            desc = "The parameters of the type.",
            func = mult_type_fun([
                        {namedtype, fun(Type) ->
                            Typexp = ?Query:exec(Type,
                                        ?Query:seq([?Type:typeparams(),
                                                    ?TypeParam:definition()])),
                            prefer_namedtypes(filter_atom_typexps(Typexp)) end},
                        {typexp, fun(Type) ->
                            Typexp = ?Typexp:call_args(Type),
                            prefer_namedtypes(filter_atom_typexps(Typexp)) end}
                        ])},
%[Typexp] = ?Query:exec(Type,
%                                ?Query:seq([?Type:typebody(),
%                                            ?TypeBody:definition()])),
%                            ?Typexp:subtypes(Typexp)

         #selector{
            name = [subtype, subtypes, sub],
            type = type,
            desc = "Subtypes of the given type.",
            func = mult_type_fun([
                        {namedtype, fun(Type) ->
                            Typexp = case ?Query:exec(Type, 
                                        ?Query:seq(?Type:typebody(),
                                                    ?TypeBody:definition())) of
                                [Body] ->
                                    case ?Typexp:type(Body) of
                                        union -> ?Typexp:subtypes(Body);
                                        _ -> [Body]
                                    end;
                                _ -> []
                            end,
                            filter_atom_typexps(Typexp) end},
                        {typexp, fun(Type) ->
                            filter_atom_typexps(?Typexp:subtypes(Type)) end}
                        ])},

         #selector{
            name = [refs, ref, references],
            type = type,
            desc = "The references of the type.",
            func = fun(Type) ->
                        ?Query:exec(namedtype(Type), ?Type:references()) end},

         #selector{
            name = [paramsrefs, paramsref],
            type = type,
            desc = "The references of the types arguments.",
            func = fun(Type) -> ?Query:exec(namedtype(Type),
                        ?Query:seq([?Type:typeparams(),
                                    ?TypeParam:references()])) end},

         #selector{
            name = [specref, specrefs, refspec, refspecs],
            type = spec,
            desc = "Specs using this type.",
            func = mult_type_fun([
                        {namedtype, fun(Type) ->
                            ?Type:refspecs(Type) end},
                        {typexp, fun(_) ->
                            [] end}
                        ])}
        ],

%%% ----------------------------------------------------------------------------
%%% Type properties

        properties =
        [#property{
            name = [name],
            type = atom,
            desc = "The name of the type.",
            func = mult_type_fun([
                        {namedtype, fun ?Type:name/1},
                        {typexp, fun ?Typexp:name/1}
                        ])},

         #property{
            name = [arity, number_of_parameters],
            type = int,
            desc = "The arity of the type.",
            func = mult_type_fun([
                        {namedtype, fun ?Type:arity/1},
                        {typexp, fun ?Typexp:arity/1}
                        ])},

         #property{
            name = [isopaque, opaque, is_opaque],
            type = bool,
            desc = "Returns whether the type is opaque.",
            func = fun(Type) ->
                        case namedtype(Type) of
                            [NamedType] ->
                                ?Type:isopaque(NamedType);
                            _ ->
                                false
                        end end},

         #property{
            name = [isbuiltin, is_builtin, is_built_in, builtin],
            type = bool,
            desc = "Returns whether the type is builtin.",
            func = fun(Type) ->
                        case namedtype(Type) of
                            [NamedType] ->
                                ?Type:isbuiltin(NamedType);
                            _ ->
                                false
                        end end},

         #property{
            name = [exported, remote_type],
            type = bool,
            desc = "Returns whether the type is exported.",
            func = fun(Type) ->
                        case namedtype(Type) of
                            [NamedType] ->
                                ?Type:exported(NamedType);
                            _ ->
                                false
                        end
                   end}
        ]},

%%% ============================================================================
%%% Record entity

     #entity{
        name = record,
        selectors =
        [#selector{
            name = [references, reference, refs, ref],
            type = expression,
            desc = "The references of the record.",
            func = fun(Rec) -> ?Query:exec(Rec, ?Rec:references()) end},

         #selector{
            name = [fields, field],
            type = field,
            desc = "The fields of the record.",
            func = fun(Rec) -> ?Query:exec(Rec, ?Rec:fields()) end},

         #selector{
            name = [file],
            type = file,
            desc = "The defining file of the record.",
            func = fun(Rec) -> ?Query:exec(Rec, ?Rec:file()) end}
        ],

%%% ----------------------------------------------------------------------------
%%% Record properties

        properties =
        [#property{
            name = [name],
            type = atom,
            desc = "The name of the record.",
            func = fun ?Rec:name/1}
        ]},

%%% ============================================================================
%%% Record field entity
     #entity{
        name = field,
        selectors =
        [#selector{
            name = [references, reference, refs, ref],
            type = expression,
            desc = "The references of a record field.",
            func = fun(Field) ->
                           ?Query:exec(Field, ?RecField:references())
                   end},

         #selector{
            name = [record, rec],
            type = record,
            desc = "The record to which the record field belongs to.",
            func = fun(Field) -> ?Query:exec(Field, ?RecField:recorddef()) end},

         #selector{
            name = [type, types],
            type = type,
            desc = "The type or type expression of the given record field.",
            func = fun(Field) ->
                        Typexp = ?Query:exec(Field, [{fielddef, back}, tsub]),
                        prefer_namedtypes(filter_atom_typexps(Typexp)) end},

         #selector{
            name = [file],
            type = file,
            desc = "The file defining the record of the field.",
            func = fun(Field) -> ?Query:exec(Field, ?RecField:file()) end}
        ],

%%% ----------------------------------------------------------------------------
%%% Record field properties

        properties =
        [#property{
            name = [name],
            type = atom,
            desc = "The name of the record field.",
            func = fun ?RecField:name/1}
        ]},

%%% ============================================================================
%%% Macro entity

     #entity{
        name = macro,
        selectors =
        [#selector{
            name = [references, reference, refs, ref],
            type = expression,
            desc = "The references that are expanded to expressions of a macro.",
            func = fun(Macro) ->
                           Subts = ?Query:exec(Macro, [{mref, back}]),
                           Fun = fun expression_above_subst/1,
                           lists:filter(fun ?Expr:is_expr/1,
                                         lists:map(Fun, Subts))
                   end},
         #selector{
            name = [clause_references, clause_reference, clause_refs, clause_ref],
            type = clause,
            desc = "The references that are expanded to clauses of a macro.",
            func = fun(Macro) ->
                            Subts = ?Query:exec(Macro, [{mref, back}]),
                            Fun = fun expression_above_subst/1,
                            ToFuncl = fun(Cl) ->
                                ?Query:exec(Cl, ?Clause:funcl())
                            end,
                            lists:map(ToFuncl, 
                                lists:filter(fun ?Clause:is_clause/1,
                                    lists:map(Fun, Subts)))
                   end},
         #selector{
            name = [all_references, all_reference, all_refs, all_ref],
            type = expression, % sq doesn't handle ambigous return types currently
            desc = "The references (either expressions or clauses) of a macro.",
            func = fun(Macro) ->
                            Subts = ?Query:exec(Macro, [{mref, back}]),
                            Fun = fun(Subst) ->
                                Cnt = expression_above_subst(Subst),
                                case ?Clause:is_clause(Cnt) of
                                    true -> hd(?Query:exec(Cnt, ?Clause:funcl()));
                                    _ -> Cnt
                                end
                            end,
                            lists:map(Fun, Subts)
                   end},
         #selector{
            name = [file],
            type = file,
            func = fun(Macro) -> ?Query:exec(Macro, ?Macro:file()) end}
        ],

%%% ----------------------------------------------------------------------------
%%% Macro properties

        properties =
        [#property{
            name = [name],
            type = string,
            desc = "The name of the macro.",
            func = fun ?Macro:name/1},

         #property{
            name = [arity],
            type = int,
            desc = "The arity of a macro.",
            func = fun(Macro) ->
                           HasVar =
                               fun(X) ->
                                       Token = ((?Graph:data(X))#lex.data),
                                       Token#token.type == variable
                               end,
                           case ?Query:exec(Macro,
                                            [{flex, {type, '==', arg}}]) of
                               [] ->
                                   0; %%Change this to undefined
                               Args ->
                                   length(
                                     lists:filter(
                                       HasVar,
                                       ?Query:exec(Args, [llex])))
                           end
                   end},

         #property{
            name = [const],
            type = bool,
            func = fun(Macro) ->
                        case ?Query:exec(Macro, [{flex, {type, '==', arg}}]) of
                            [] ->
                                case ?Query:exec(Macro, [{mref, back}]) of
                                    [H|_] ->
                                        ?Query:exec(expression_above_subst(H),
                                            ?Expr:functions()) == [];
                                    _ ->
                                        true
                                end;
                            _ -> false
                        end
                   end},
         #property{
            name = [body],
            type = string,
            func = fun(Macro) ->
                        ?Macro:macro_body(Macro)
                   end}
        ]},

%%% ============================================================================
%%% Expression entity

     #entity{
        name = expression,
        selectors =
        [#selector{
            name = [fundef],
            type = function,
            desc = "The function where the expression is present.",
            func = fun(Expr) ->
                           ?Query:exec(Expr, ?Query:seq([?Expr:clause(),
                                                         ?Clause:form(),
                                                         ?Form:func()]))
                   end},

         #selector{
            name = [functions, function, funs, 'fun'],
            type = function,
            desc = "Functions used in the expression.",
            func = fun(Expr) ->
                            Query = ?Query:seq(?Expr:deep_sub(),
                                               ?Expr:dynfunction()),
                            filter_opaques(?Query:exec(Expr, Query)) 
                   end},

         #selector{
            name = [dynamic_functions, dynamic_function, dynfuns, dynfun],
            type = function,
            desc = "The dynamic functions used in the expression.",
            func = fun(Expr) ->
                            Query = ?Query:seq([?Expr:deep_sub(),
                                                ?Query:all([?Dynfun:ambdyn(),
                                                            ?Dynfun:dynfun()])]),
                            filter_opaques(?Query:exec(Expr, Query))
                   end},

         #selector{
            name = [variables, variable, vars, var],
            type = variable,
            desc = "The variables in the expression.",
            func = fun(Expr) -> ?Query:exec(Expr, ?Expr:variables()) end},

         #selector{
            name = [records, record, recs, rec],
            type = record,
            desc = "The records in the expression.",
            func = fun(Expr) -> ?Query:exec(Expr, ?Expr:records()) end},

         #selector{
            name = [macros, macro],
            type = macro,
            desc = "The macros in the expression.",
            func = fun(Expr) -> ?Query:exec(Expr, ?Expr:macros()) end},

         #selector{
            name = [subexpression, subexpr, esub, sub],
            type = expression,
            desc = "The subexpressions of the expression.",
            func = fun(Expr) -> ?Query:exec(Expr, ?Expr:deep_sub()) end},

         #selector{
            name = [parameter, param],
            type = expression,
            func = fun(Expr) ->
                           case ?Expr:type(Expr) of
                               application ->
                                   ?Query:exec(Expr,
                                               ?Query:seq(?Expr:child(2),
                                                          ?Expr:children()));
                               match_expr ->
                                   ?Query:exec(Expr, ?Expr:child(2));
                               cons ->
                                   ?Query:exec(Expr,
                                               ?Query:all(
                                                  ?Query:seq(?Expr:child(1),
                                                             ?Expr:children()),
                                                  ?Expr:child(2)));
                               infix_expr ->
                                   ?Query:exec(Expr, ?Query:any(
                                                        ?Expr:children(),
                                                        [exprcl, body]));
                               _ ->
                                   ?Query:exec(Expr, ?Expr:children())
                           end
                   end},

         #selector{
            name = [top_expression, top_expr, top],
            type = expression,
            func = fun(Expr) -> ?Query:exec(Expr, ?Expr:top()) end},

         #selector{
            name = [reach],
            type = expression,
            func = dataflow_reach([])},

         #selector{
            name = [origin],
            type = expression,
            func = dataflow_reach([back])},

         #selector{
            name = [file],
            type = file,
            desc = "The file of the expression.",
            func = fun(Expr) -> ?Query:exec(Expr, ?Query:seq([?Expr:clause(),
                                                              ?Clause:form(),
                                                              ?Form:file()]))
                   end}
        ],

%%% ----------------------------------------------------------------------------
%%% Expression properties

        properties =
        [#property{
            name = [type],
            type = atom,
            desc = "The type of the expression.",
            func = fun(Expr) ->
                           case ?Expr:type(Expr) of
                               prefix_expr -> ?Expr:value(Expr);
                               infix_expr  -> ?Expr:value(Expr);
                               Kind        -> Kind
                           end
                   end},

         #property{
            name = [value, val],
            type = expression_value,
            desc = "The value of the expression.",
            func = fun ?Expr:value/1},

         #property{
            name = [macro_value, macro_val, macrovalue, macval],
            type = string,
            desc = "The value of the macro contained by the expression.",
            func = fun ?Expr:macro_value/1},

         #property{
            name = [class],
            type = atom,
            desc = "The class of the expression.",
            func = fun ?Expr:role/1},

         #property{
            name = [is_last, last],
            type = bool,
            desc = "Returns whether the expression is the last in the clause.",
            func = fun is_last_expr/1},

         #property{
            name = [index],
            type = int,
            func = fun expr_index/1},

         #property{
            name = [has_side_effect],
            type = bool,
            desc = "Returns whether the expression has side effects.",
            func = fun ?Expr:has_side_effect/1},

         #property{
            name = [is_tailcall, tailcall],
            type = bool,
            func = fun(Expr) ->
                           is_last_expr(Expr) andalso
                               ?Expr:type(Expr) == application
                   end},

          #property{
            name = [tuple_repr_of_record, is_tuple_repr_of_record,
                   record_tuple, is_record_tuple],
            type = bool,
            func = fun(Expr) ->
                           ?Expr:type(Expr) == tuple andalso
                               has_record_definition(Expr)
                   end}
        ]}
    ].

filter_opaques(Funs) when ?hide_opaque =:= true -> 
    [ Fun || Fun <- Funs, ?IS_NODE(Fun), not ?Fun:is_opaque(Fun) ]; 
filter_opaques(Funs) -> 
    Funs.

expr_index(Expr) ->
    lists:foldl(
     fun(Tag, none) ->
             case ?ESG:path(Expr, [{Tag, back}]) of
                 [Parent] ->
                     ?ESG:index(Parent, Tag, Expr);
                 [] ->
                     none
             end;
        (_, Index) -> Index
     end,
     none,
     [pattern, body, esub]).

mult_type_fun(List) ->
    fun(Ents) when is_list(Ents) ->
            lists:append(lists:map(
                fun(Ent) ->
                    Fun = proplists:get_value(?Syn:node_type(Ent), List),
                    Fun(Ent)
                end, Ents));
        (Ent) ->
            Fun = proplists:get_value(?Syn:node_type(Ent), List),
            Fun(Ent)
    end.

filter_atom_typexps(Typexprs) ->
    lists:filter(fun(Typexp) -> ?Typexp:type(Typexp) /= atom end, Typexprs).

namedtype(List) when is_list(List) ->
    lists:flatmap(fun namedtype/1, List);
namedtype(Type) ->
    case ?Syn:node_type(Type) of
        namedtype ->
            [Type];
        typexp ->
            ?Query:exec(Type, ?Typexp:typeref())
    end.

prefer_namedtypes(List) when is_list(List) ->
    lists:map(fun prefer_namedtypes/1, List);
prefer_namedtypes(Typexp) ->
    case namedtype(Typexp) of
        [] -> Typexp;
        [N] -> N
    end.

get_expr_from_mac_ref(Mac)->
    get_expr_from_mac_ref(Mac, 0).

% probably need an upper limit of the recursion..  (Level =< 100)
get_expr_from_mac_ref(Mac, Level) when Level >= 0 ->
    Edges = lists:duplicate(Level * 2 + 1, {llex,back}) ++ [{elex, back}],
    case ?Query:exec(Mac, Edges) of
        [] -> get_expr_from_mac_ref(Mac, Level +1 );
        Exprs -> Exprs
    end;
get_expr_from_mac_ref(_, _) ->
    [].

dataflow_reach(Opt) ->
    fun (Expr) ->
            ?Dataflow:reach([Expr], Opt)
    end.

file_is_module(File) ->
    case ?Syn:node_type(File) of
        module -> true;
        file -> ?File:type(File) == module
    end.

file_prop(filename, File) ->
    case file(File) of
        [] -> %todo: lehet mas mint module node?
            %io_lib:write_atom(?Mod:name(File));
            "";
        [FileNode] ->
            Path = ?File:path(FileNode),
            filename:basename(Path)
    end;

file_prop(name, File) ->
    case mod(File) of
        [ModuleNode] ->
            ?Mod:name(ModuleNode);
        [] ->
            Path = ?File:path(File),
            Name = filename:rootname(filename:basename(Path)),
            list_to_atom(Name)
    end;

file_prop(Prop, File) ->
    case file(File) of
        [] -> io_lib:write_atom(?Mod:name(File));
        [FileNode] ->
            Path = ?File:path(FileNode),
            case Prop of
                path -> Path;
                dir -> filename:dirname(Path)
            end
    end.


mod(Node) ->
    case ?Syn:node_type(Node) of
        file -> ?Query:exec(Node, ?File:module());
        module -> [Node]
    end.

file(Node) ->
    case ?Syn:node_type(Node) of
        file -> [Node];
        module -> ?Query:exec(Node, ?Mod:file())
    end.

is_last_expr(Expr) ->
    Last = ?Query:exec(Expr, ?Query:seq([?Expr:clause(), [{visib, last}]])),
    Top = ?Query:exec(Expr, ?Expr:top()),
%    ?Expr:is_same_expr({Last, Top}).
    Last == Top.

last_common_node([{_L, N} = H|T1], [H|T2]) -> last_common_node(T1, T2, N);
last_common_node(_,_) -> ?Graph:root().

last_common_node([{_L, N} = H|T1], [H|T2], _) -> last_common_node(T1, T2, N);
last_common_node(_, _, Node) -> Node.

has_record_definition(Tuple) ->
    case lists:filter (fun(Node) -> ?Expr:type(Node) == atom end,
                       ?Dataflow:reach(
                          ?Query:exec(Tuple, ?Expr:child(1)), [back]))
    of
        [AtomExpr] ->
            case find_rec_with_name(Tuple, ?Expr:value(AtomExpr)) of
                [Rec] ->
                    length(?Query:exec(Rec, ?Rec:fields())) + 1 ==
                        length(?Query:exec(Tuple, ?Expr:children()));
                [] ->
                    false
            end;
        _ ->
             false
    end.

find_rec_with_name(Tuple, Atom) ->
    ?Query:exec(Tuple, ?Query:seq([?Expr:clause(),
                                   ?Clause:form(),
                                   ?Form:file(),
                                   ?Rec:find(Atom)])).

%% @private
%% @spec file_metrics(Name::atom(), File::entity(), Check::atom()) -> int()
%% @doc Metrics needs a module node, and a file entity can be either a module
%%      node or a file node, so it needs to be converted.
%%      Module nodes exist without files in the database, and most of the
%%      metrics needs the files too. The `Check' parameter indicates whether
%%      the file is needed.
%%      The default value is 0 for all the metrics.
file_metrics(Name, File, false) ->
    case mod(File) of
        []       -> 0;
        [Module] -> ?Metrics:metric({Name, module, Module})
    end;
file_metrics(Name, File, true) ->
    case file(File) of
        [] -> 0;
        _  -> file_metrics(Name, File, false)
    end.

fun_metrics(Name, {clause, Cl}) ->
    Result = ?Metrics:metric({Name, clause, Cl}),
    case Name of
        is_tail_recursive ->
            int_to_atom(Result);
        _ ->
            Result
    end;
fun_metrics(Name, Fun) ->
    case ?Query:exec(Fun, ?Fun:definition()) of
        [] ->
            case Name of
                is_tail_recursive ->
                    unknown;
                _ ->
                    0
            end;
        [_FunDef] ->
            Result = ?Metrics:metric({Name, function, Fun}),
            case Name of
                is_tail_recursive ->
                    int_to_atom(Result);
                _ ->
                    Result
            end
    end.

int_to_atom(Int) ->
    case Int of
        -1 -> non_rec;
        0 -> non_tail_rec;
        1 -> tail_rec
    end.

%%% ============================================================================
%%% Statistics

statistics() ->
    [#statistics{
        name = [minimum, min],
        desc = "The minimum of the values.",
        func = fun(Values) -> lists:min(Values) end},
     #statistics{
        name = [maximum, max],
        desc = "The maximum of the values.",
        func = fun(Values) -> lists:max(Values) end},
     #statistics{
        name = [sum],
        desc = "The sum of the values.",
        func = fun(Values) -> lists:sum(Values) end},
     #statistics{
        name = [mean, average, avg],
        desc = "The mean of the values.",
        func = fun([]) -> 'NaN';
                  (Values) -> lists:sum(Values)/length(Values) end},
     #statistics{
        name = [median, med],
        desc = "The median of the values.",
        func = fun median/1},
     #statistics{
        name = [variance, var],
        desc = "The variance of the values.",
        func = fun variance/1},
     #statistics{
        name = [standard_deviaton, sd],
        desc = "The standard deviation of the values.",
        func = fun(Values) -> math:sqrt(variance(Values)) end}].
%% todo: megjelenitesnel kezelni
     %% #statistics{
     %%    name = [all],
     %%    desc = "The values for all available statistics.",
     %%    func = fun all_stat/1}]

variance([_]) ->
    0.0;
variance(Values) ->
    Mean = lists:sum(Values)/length(Values),
    Sum1 = lists:foldl(fun(Elem, Sum) -> Sum + (Elem-Mean)*(Elem-Mean) end,
               0,
               Values),
    Sum1/(length(Values)-1).

median(Values) ->
    SVals = lists:sort(Values),
    Length2 = length(SVals)/2,
    X = trunc(Length2),
    Y = round(Length2),
    if
    X == Y -> (lists:nth(X, SVals) + lists:nth(X+1, SVals))/2;
    true -> lists:nth(Y, SVals)
    end.

%% all_stat(Values) ->
%%     [{"minimum",            lists:min(Values)},
%%      {"maximum",            lists:max(Values)},
%%      {"sum",                lists:sum(Values)},
%%      {"mean",               lists:sum(Values)/length(Values)},
%%      {"median",             median(Values)},
%%      {"variance",           variance(Values)},
%%      {"standard deviation", math:sqrt(variance(Values))}].

%%% ============================================================================
%%% Help

help_data(selectors, EntityType) ->
    Entity = entity(EntityType),
    Data =
        [ begin
              [Name| Synonyms] = Selector#selector.name,
              {atom_to_list(Name), [atom_to_list(Syn)|| Syn <- Synonyms],
               atom_to_list(Selector#selector.type), Selector#selector.desc}
          end || Selector <- Entity#entity.selectors ],
    Title = lists:flatten(io_lib:format("Selectors for ~ps" , [EntityType])),
    {Title, Data};

help_data(properties, EntityType) ->
    Entity = entity(EntityType),
    Data =
        [ begin
              [Name| Synonyms] = Property#property.name,
              {atom_to_list(Name), [atom_to_list(Syn)|| Syn <- Synonyms],
               atom_to_list(Property#property.type), Property#property.desc}
          end || Property <- Entity#entity.properties ],
    Title = lists:flatten(io_lib:format("Properties for ~ps" , [EntityType])),
    {Title, Data};

help_data(statistics, []) ->
    Data =
        [ begin
              [Name| Synonyms] = Statistics#statistics.name,
              {atom_to_list(Name), [atom_to_list(Syn)|| Syn <- Synonyms],
               "int",
               Statistics#statistics.desc}
          end || Statistics <- statistics() ],
    {"Statistics", Data};

help_data(initial_selectors, []) ->
    Data =
        [ begin
              [Name| Synonyms] = InitSel#initial_selector.name,
              {atom_to_list(Name), [atom_to_list(Syn)|| Syn <- Synonyms],
               atom_to_list(InitSel#initial_selector.type),
               InitSel#initial_selector.desc}
          end || InitSel <- initial_selectors() ],
    {"Initial Selectors", Data}.
