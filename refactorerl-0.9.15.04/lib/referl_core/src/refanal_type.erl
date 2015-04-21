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

%%% @doc Type (-type) analyser
%%%
%%% There are two types of types: named types and anonym types.<br/>
%%% <b>Named types</b> have names; they can be Erlang built-in types (like
%%% `any()', `atom()', `tuple()', etc.) or user-defined types (defined with
%%% the `-type' or `-opaque' compiler attributes). Named types are present in
%%% the graph with the `namedtype' semantic node.<br/>
%%% <b>Anonym types</b> don't have names. They describe sets of Erlang terms.
%%% They consist and are built from a set of predefined types. They are named
%%% simply "types" in the
%%% <a href="http://www.erlang.org/doc/reference_manual/typespec.html#id75333">
%%% Erlang documentation about types and specifications</a> (the 6.2 section is
%%% about them).<br/>
%%% Example: `-spec foo(atom() | myt() | {ok}) -> any().'<br/>
%%% The (anonym) type of the first parameter of `foo' is a `union'. It means,
%%% that it can be a (built-in named type) atom, a (user-defined named type)
%%% myt or a tuple with the `ok' atom in it. The `atom()' `call', `myt()'
%%% `call' and the `{ok}' `tuple' are subtypes of the `union'. The `typexp'
%%% syntactic nodes represent anonym types in the graph.
%%%
%%% Nodes added by the type analyser:
%%% <ul>
%%%     <li><b>`namedtype'</b>: Gets created, when we insert a type form,
%%%         <b>`typedef'</b> edge points to the `namedtype' node from the form.
%%%         It's under a `module' (with a <b>`type'</b> edge). <b>`typeref'</b>
%%%         points from the referencing `call' `typexp' nodes (`namedtype' gets
%%%         created, when something's referencing it too, then there may not be
%%%         a `typedef' edge). Built-in types are under the `erlang'
%%%         `module'.</li>
%%%     <li><b>`namedtypebody'</b>: They are under `namedtype'
%%%         (<b>`typebody'</b> edge), if there's a definition of the type.
%%%         <b>`typebodydef'</b> points to the anonym type (`typexp'), the
%%%         `namedtype' represents.</li>
%%%     <li><b>`namedtypeparam'</b>: Polymorphic types have parameters.<br/>
%%%         Example: `-type queue(Type) :: {fifo, list(Type), list(Type)}.'<br/>
%%%         The `namedtypeparam' semantic node is under `namedtype'
%%%         (<b>`typeparam'</b> edge), <b>`typeparamdef'</b> points from the
%%%         `variable' `typexp' definition, <b>`typeparamref'</b> points from
%%%         the `typexp' nodes under the `arglist' of a `call'.</li>
%%% </ul>
%%%
%%% Other edges:
%%% <ul>
%%%     <li><b>`typexpmodref'</b>: `call' type-expressions can reference types
%%%         in other modules; `typexpmodref' points from a `atom' under the
%%%         `module_qualifier' to the referenced `module'. If we're referencing
%%%         a built-in type, the `typexpmodref' points from the name of the
%%%         named type to the `erlang' `module'.</li>
%%%     <li><b>`typetop'</b>: `typetop' edges point to the definitions of
%%%         `namedtypeparam', `namedtypebody', `field', `specguard' and
%%%         `specparam' edges. They start from the `typexp' nodes under the
%%%         pointed `typexp', which are referenced in the
%%%         {@link topedges_filter/1} function (they wouldn't make much sense
%%%         from nodes like `arglist' and `bin_base'). They're used, when we
%%%         determine what named typed does a `specparam' use for example.</li>
%%% </ul>
%%%
%%% @author Denes Peteri <petden@caesar.elte.hu>

-module(refanal_type).
-vsn("$Rev$").
-behaviour(refcore_anal).

-export([schema/0, externs/1, insert/4, remove/4, update/2]).
-export([error_text/2]).

-export([get_module_from_file/1]). % for refanal_spec

-include("core.hrl").

%%% ============================================================================
%%% Schema, externs, error_text

%%% @private
error_text(redefine_builtin_type, [Name, Arity]) ->
    io_lib:format("Type ~p/~p is a builtin type; it cannot be redefined.~n",
              [Name, Arity]).
%%% @private
schema() ->
    [{namedtype, record_info(fields, namedtype), []},
     {module, [{type, namedtype}]},
     {form, [{typedef, namedtype}]},
     {typexp, [{typeref, namedtype}]},
     {namedtypeparam, record_info(fields, namedtypeparam), []},
     {namedtype, [{typeparam, namedtypeparam}, {typebody, namedtypebody}]},
     {typexp, [{typeparamdef, namedtypeparam}, {typeparamref, namedtypeparam},
               {typetop, typexp}, {typebodydef, namedtypebody},
               {typexpmodref, module}]},
     {namedtypebody, record_info(fields, namedtypebody), []}].

%%% @private
externs(_) -> [].

%%% ============================================================================
%%% Insert

%%% @private
insert(Parent, _Pre, {_Tag, Child}, _Post) ->
    case ?Anal:data(Child) of
        #form{type='type', tag=Tag} ->
            {File, Form} = {Parent, Child},
            Module = get_module_from_file(File),
            add_form_typerefs(Form, Module),
            Type = add_typedefs(Form, Module, Tag),
            add_del_typetop(type, add, {Type});
        #form{type='spec'} ->
            Module = get_module_from_file(Parent),
            add_form_typerefs(Child, Module),
            add_del_typetop(spec, add, {Child});
        #form{type='record'} ->
            Module = get_module_from_file(Parent),
            add_form_typerefs(Child, Module),
            add_del_typetop('record', add, {Child});
        #form{} -> ok;
        _ ->
            Form = refanal_spec:findover(Parent, 'form'),
            case Form of
                not_found -> ok;
                _ -> insert(?Anal:parent(Form), undefined, {undefined, Form},
                            undefined)
            end
    end,
    ok.

%%% ============================================================================
%%% Functions used in insert

%% @doc Adds the def edges, returns Type
add_typedefs(Form, Module, Tag) ->
    Typexps = [Typexp || {tattr, Typexp} <- ?Anal:children(Form)],
    [Namexp, Varlist, Body] = Typexps,
    {Name, Arity, IsOpaque} =
        type_data(Namexp, Varlist, Tag),
    builtin_check(Name, Arity),
    ?NodeSync:add_ref(type, {def, Form}, {Module, IsOpaque, false,
                                          {Name, Arity}}),
    Type = ?NodeSync:get_node(type, {true, Module, IsOpaque, false,
                                     {Name, Arity}}),
    ?NodeSync:add_ref(typebody, {def, Body},
                      {Type, ?Syn:flat_text(Body)}),
    add_typeparams(Type, Varlist),
    Type.

type_data(Namexp, Varlist, Tag) ->
    #typexp{type='atom', tag=Name} = ?Anal:data(Namexp),
    Arity = length(?Anal:children(Varlist)), 
    IsOpaque = case Tag of
        'opaque' -> true;
        _ -> false
    end,
    {Name, Arity, IsOpaque}.

add_typeparams(Type, Varlist) ->
    Vars1 = [V || {tsub, V} <- ?Anal:children(Varlist)],
    Vars = lists:zip(Vars1, lists:seq(1, length(Vars1))),
    lists:foreach(fun({V, Index}) ->
        #typexp{tag=Tag, type=TypexpType} = ?Anal:data(V),
        ?NodeSync:add_ref(typeparam, {def, V}, {Index, Type, Tag, TypexpType})
    end, Vars).

%% @doc Draws the TYPEREFs stating from this Form (type, spec or record form).
add_form_typerefs(Form, FormModule) ->
    FirstLevel = [F || {_, F} <- ?Anal:children(Form)],
    Calls = findallbelow(FirstLevel, 'call'),
    lists:foreach(fun(C) ->
        {CName, CArity, CModule, EdgeArgPairs, ModuleQualifierExp, IsBuiltin} =
            call_data(C, FormModule),
        case ModuleQualifierExp of
            undefined -> ok;
            _ ->
                ?NodeSync:add_ref(module, {typexpref, ModuleQualifierExp},
                    CModule)
        end,
        Type = ?NodeSync:get_node(type, {true, CModule, false, IsBuiltin,
                                  {CName, CArity}}),
        ?NodeSync:add_ref(type, {ref, C}, Type),
        Vars = [V || {tsub, V} <- EdgeArgPairs],
        VarIndexParirs = lists:zip(Vars, lists:seq(1, length(Vars))),
        lists:foreach(fun({V, Index}) ->
            ParamNode = ?NodeSync:get_node(typeparam, {Index, Type,
                undefined, undefined}),
            ?NodeSync:add_ref(typeparam, {ref, V}, ParamNode)
        end, VarIndexParirs)
    end, Calls).

%% used by add_form_typerefs/2
call_data(C, FormModule) ->
    [{tsub, NameModule}, {tsub, Arglist}] = ?Anal:children(C),
    NameExps = refanal_spec:findbelow([NameModule], 'atom', ['module_qualifier']),
    case NameExps of
        [ModuleQualifierExp, Namexp] ->
            #typexp{tag=CModuleName} = ?Anal:data(ModuleQualifierExp),
            CModule = ?NodeSync:get_node(module, CModuleName);
        [Namexp] ->
            CModule = FormModule,
            ModuleQualifierExp = undefined
    end,
    #typexp{type='atom', tag=CName} = ?Anal:data(Namexp),
    EdgeArgPairs = ?Anal:children(Arglist),
    CArity = length(EdgeArgPairs),
    case is_builtin_type(CName, CArity) of
        true ->
            ErlangModule = ?NodeSync:get_node(module, 'erlang'),
            {CName, CArity, ErlangModule, EdgeArgPairs, Namexp, true};
        false -> {CName, CArity, CModule, EdgeArgPairs, ModuleQualifierExp, false}
    end.

builtin_check(Name, Arity) ->
    case is_builtin_type(Name, Arity) of
        true -> throw(?LocalError(redefine_builtin_type, [Name, Arity]));
        _ -> ok
    end.

%% todo: This is not the full list. Should use `erl_types:t_from_form/1'
%% instead. Like this:
%% ```
%% {_, BuiltInTokens,_}=erl_scan:string("-spec dummy()->atom().").
%% {ok,{attribute,1,spec, {_, [BuiltInTypeForm]}}}=
%%     erl_parse:parse_form(BuiltInTokens).
%% erl_types:t_from_form(BuiltInTypeForm).
%% '''
%% This returns:
%% ```
%% {c,function,
%%    [{c,product,[],unknown},{c,atom,any,unknown}],
%%    unknown}
%% '''
%% If it gets a user-defined type, it throws an exception.
builtin_types() ->
    [{any, 0}, {none, 0}, {pid, 0}, {port, 0}, {reference, 0}, {float, 0},
     {atom, 0}, {'fun', 0}, {'fun', 2}, {integer, 0}, {list, 0}, {list, 1},
     {improper_list, 0}, {improper_list, 2}, {nonempty_list, 0},
     {nonempty_list, 1}, {tuple, 0}, {term, 0}, {binary, 0}, {bitstring, 0},
     {boolean, 0}, {byte, 0}, {char, 0}, {number, 0},
     {maybe_improper_list, 0}, {string, 0}, {nonempty_string, 0},
     {iodata, 0}, {iolist, 0}, {module, 0}, {mfa, 0}, {arity, 0}, {node, 0},
     {timeout, 0}, {no_return, 0}, {non_neg_integer, 0}, {pos_integer, 0},
     {neg_integer, 0}, {nonempty_maybe_improper_list, 0},
     {nonempty_improper_list, 2}, {nonempty_maybe_improper_list, 2}].

is_builtin_type(Name, Arity) ->
    lists:any(fun({N, A}) ->
        {N, A} =:= {Name, Arity}
    end, builtin_types()).

%%% ============================================================================
%%% Remove

%%% @private
remove(Parent, _Pre, {_Tag, Child}, _Post) ->
    case ?Anal:data(Child) of
        #form{type='type'} ->
            {_File, Form} = {Parent, Child},
            case ?NodeSync:get_node(type, Form) of
                not_found -> ok;
                TypeNode ->
                    del_typedefs(TypeNode, Form),
                    add_del_typetop(type, del, {TypeNode})
            end,
            del_typerefs(Form);
        #form{type='spec'} ->
            del_typerefs(Child),
            add_del_typetop(spec, del, {Child});
        #form{type='record'} ->
            del_typerefs(Child),
            add_del_typetop('record', del, {Child});
        #form{} -> ok;
        _ ->
            Form = refanal_spec:findover(Parent, 'form'),
            case Form of
                not_found -> ok;
                _ ->
                    remove(?Anal:parent(Form), undefined, {undefined, Form},
                           undefined)
            end
    end,
    ok.

%%% ============================================================================
%%% Functions used in remove

%% @doc Removes the def edges.
del_typedefs(TypeNode, Form) ->
    TypeParams = ?NodeSync:get_node(typeparam, TypeNode),
    lists:foreach(fun(ParamNode) ->
        PDefTypexps = ?Graph:path(ParamNode, ?TypeParam:definition()),
        [?NodeSync:del_ref(typeparam, {def, PDefTypexp}, {ParamNode, TypeNode}) ||
            PDefTypexp <- PDefTypexps]
    end, TypeParams),
    case ?NodeSync:get_node(typebody, TypeNode) of
        not_found -> ok;
        BodyNode ->
            BDefTypexps = ?Graph:path(BodyNode, [{typebodydef, back}]),
            [?NodeSync:del_ref(typebody, {def, BDefTypexp},
                {BodyNode, TypeNode}) || BDefTypexp <- BDefTypexps]
    end,
    TDefModules = ?Graph:path(TypeNode, ?Type:module()),
    [?NodeSync:del_ref(type, {def, Form}, {TypeNode, TDefModule}) ||
        TDefModule <- TDefModules].

%% @doc Removes TYPEREFs (and typexpmodrefs) starting from Form.
del_typerefs(Form) ->
    FirstLevel = [F || {_, F} <- ?Anal:children(Form)],
    del_typexpmodrefs_firstlevel(FirstLevel),
    Calls = findallbelow(FirstLevel, 'call'),
    lists:foreach(fun(CallNode) ->
        case ?NodeSync:get_node(type, CallNode) of
            not_found -> ok;
            TypeNode ->
                del_typeparamrefs(CallNode),
                ?NodeSync:del_ref(type, {ref, CallNode}, TypeNode)
        end
    end, Calls).

del_typexpmodrefs_firstlevel(FirstLevel) ->
    Typexps = findallbelowfun(FirstLevel, fun({_Node, _T}) -> true end),
    lists:foreach(fun(Typexp) ->
        RefModules = ?Graph:path(Typexp, ?Typexp:typexpmodref()),
        lists:foreach(fun(RefModule) ->
            ?NodeSync:del_ref(module, {typexpref, Typexp}, RefModule)
        end, RefModules)
    end, Typexps).

%% used by del_typerefs/1
del_typeparamrefs(CallNode) ->
    Vars = call_vars(CallNode),
    lists:foreach(fun(V) ->
        case ?NodeSync:get_node(typeparam, {ref, V}) of
            [ParamNode] ->
                ?NodeSync:del_ref(typeparam, {ref, V}, ParamNode);
            _ -> ok
        end
    end, Vars).

%% used by del_typerefs/1
call_vars(CallNode) ->
    [{tsub, _NameModule}, {tsub, Arglist}] = ?Anal:children(CallNode),
    EdgeArgPairs = ?Anal:children(Arglist),
    [V || {tsub, V} <- EdgeArgPairs].

%%% ============================================================================
%%% Functions used in insert and remove

%% @doc Inserts/removes TYPETOPs for the definitions of typebodies, typeparams,
%% specguards, specparams and fields.
add_del_typetop(type, Action, {TypeNode}) ->
    case ?NodeSync:get_node(typebody, TypeNode) of
        not_found -> ok;
        TypeBody -> add_del_topedgessem(Action, TypeBody, typebodydef)
    end,
    TypeParams = ?NodeSync:get_node(typeparam, TypeNode),
    add_del_topedgessems(Action, TypeParams, typeparamdef);
add_del_typetop(spec, Action, {Form}) ->
    case ?NodeSync:get_node(spec, Form) of
        not_found -> ok;
        Spec ->
            Clauses = ?NodeSync:get_node(specclause, Spec),
            lists:foreach(fun(Clause) ->
                case ?NodeSync:get_node(specguard, {false, Clause, undefined}) of
                    not_found -> ok;
                    SpecGuard ->
                        add_del_topedgessem(Action, SpecGuard, specguarddef)
                end,
                case ?NodeSync:get_node(specparam, {specret, Clause}) of
                    not_found -> ok;
                    Ret -> add_del_topedgessem(Action, Ret, specparamdef)
                end,
                Pars = ?NodeSync:get_node(specparam, {specpar, Clause}),
                add_del_topedgessems(Action, Pars, specparamdef)
            end, Clauses)
    end;
add_del_typetop('record', Action, {Form}) ->
    case ?Graph:path(Form, [recdef]) of
        [Rec] ->
            Fields = ?Graph:path(Rec, [field]),
            add_del_topedgessems(Action, Fields, fielddef);
        _ -> ok
    end.

add_del_topedgessem(Action, Node, Def) ->
    Typexps = ?Graph:path(Node, [{Def, back}]),
    [add_del_topedges(Action, Typexp) || Typexp <- Typexps].

add_del_topedgessems(Action, Nodes, Def) ->
    [add_del_topedgessem(Action, Node, Def) || Node <- Nodes].

%% @doc Inserts/removes TYPETOPs in the subtree under Node.
add_del_topedges(add, Node) ->
    #typexp{type=NodeType} = ?Anal:data(Node),
    Children = [{C, NodeType} || {_, C} <- ?Anal:children(Node)],
    Froms = [Node | findallbelowfunparent(Children, (fun topedges_filter/1))],
    lists:foreach(fun(F) ->
        ?Graph:mklink(F, typetop, Node)
    end, Froms);
add_del_topedges(del, Node) ->
    Froms = ?Graph:path(Node, [{typetop, back}]),
    lists:foreach(fun(F) ->
        ?Graph:rmlink(F, typetop, Node)
    end, Froms).

%% used by add_del_topedges(add, Node)
topedges_filter({_Node, ParentType, Type}) ->
    % #typexp{} node's type can be: union, vardef, tuple, record, field, list,
    % binary, bin_base, bin_unit, integer, interval, negate, variable, joker,
    % atom, func, fun_sig, poly_sig, arglist, spec_guard?, paren, call,
    % module_qualifier, ...
    case Type of
        'bin_base' -> false;
        'bin_unit' -> false;
        'integer' ->
            case ParentType of
                'bin_base' -> false;
                'bin_unit' -> false;
                'arity_qualifier' -> false;
                _ -> true
            end;
        'variable' ->
            case ParentType of
                'bin_base' -> false;
                'bin_unit' -> false;
                _ -> true
            end;
        'joker' ->
            case ParentType of
                'bin_base' -> false;
                'bin_unit' -> false;
                _ -> true
            end;
        'atom' ->
            case ParentType of
                'call' -> false;
                'field' -> false;
                'module_qualifier' -> false;
                'arity_qualifier' -> false;
                'spec' -> false;
                'type' -> false;
                _ -> true
            end;
        'poly_sig' -> false;
        'fun_sig' -> false;
        'arglist' -> false;
        'spec_guard' -> false;
        'paren' -> false;
        'module_qualifier' -> false;
        _ -> true 
    end.

%%% ============================================================================
%%% Update

%%% @private
update(_Form, #form{type=module, tag=Name}) ->
    Module = ?NodeSync:get_node(module, Name),
    Typemodrefs = ?Graph:path(Module, [{typexpmodref, back}]),
    lists:foreach(fun(Typexp) ->
        Parent = ?Anal:parent(Typexp),
        case ?Anal:data(Parent) of
            #typexp{type='module_qualifier'} ->
                [ModLex] = ?Graph:path(Typexp, [{tlex, 1}]),
                ?Syn:update_lex_with_text(ModLex, Name),
                ?Graph:update(Typexp, (?Graph:data(Typexp))#typexp{tag=Name}),
                Call = ?Anal:parent(Parent),
                lists:foreach(fun(Typebody) ->
                    update_value(Typebody, typebodydef)
                end, ?Graph:path(Call, ?Typexp:typetop() ++ ?Typexp:typebody())),
                lists:foreach(fun(Specguard) ->
                    update_value(Specguard, specguarddef)
                end, ?Graph:path(Call, ?Typexp:typetop() ++ ?Typexp:specguard())),
                lists:foreach(fun(Specparam) ->
                    update_value(Specparam, specparamdef)
                end, ?Graph:path(Call, ?Typexp:typetop() ++ ?Typexp:specparam()));
            _ -> ok
        end
    end, Typemodrefs),
    ok;
%% todo: Right now, update/2 only handles the ri:renmod refactoring. Should
%% handle all of them.
update(Node, _Data) ->
    Parent = ?Anal:parent(Node),
    remove(Parent, undefined, {undefined, Node}, undefined),
    insert(Parent, undefined, {undefined, Node}, undefined),
    ok.

%%% ============================================================================
%%% Functions used in update

update_value(Typebody, typebodydef) ->
    case ?Graph:path(Typebody, ?TypeBody:definition()) of
        [SRoot | _] ->
            ?Graph:update(Typebody, (?Graph:data(Typebody))#namedtypebody{
                value=?Syn:flat_text(SRoot)});
        _ -> ok
    end;
update_value(Specguard, specguarddef) ->
    case ?Graph:path(Specguard, ?SpecGuard:definition()) of
        [SRoot | _] ->
            ?Graph:update(Specguard, (?Graph:data(Specguard))#specguard{
                value=?Syn:flat_text(SRoot)});
        _ -> ok
    end;
update_value(Specparam, specparamdef) ->
    case ?Graph:path(Specparam, ?SpecParam:definition()) of
        [SRoot | _] ->
            ?Graph:update(Specparam, (?Graph:data(Specparam))#specparam{
                value=?Syn:flat_text(SRoot)});
        _ -> ok
    end.

%%% ============================================================================
%%% Other functions

%%% @private
get_module_from_file(File) ->
    case ?Graph:path(File, [moddef]) of
        [Mod] -> Mod;
        _ ->
            Includers = lists:filter(
                fun(F) -> F /= File end,
                ?Graph:path(File, [{incl, back}])),
            case Includers of
                [] -> ?NodeSync:get_node(module, File);
                [Inc | _] -> get_module_from_file(Inc)
            end
    end.

%%% private
%get_module_from_file(File) ->
%    Files = incl_files(File),
%    case lists:flatmap(fun(FF) -> ?Graph:path(FF, [moddef]) end, Files) of
%        [Module | _] -> Module;
%        _ -> ?NodeSync:get_node(module, File)
%    end.

%%% private
%incl_files(File) ->
%    incl_files([File], 1).

%%% private
%incl_files(Files, N) ->
%    NewFiles = lists:flatmap(fun(FF) ->
%        ?Graph:path(FF, [{incl, back}, incl])
%    end, Files),
%    NewFilesSet = sets:from_list(NewFiles),
%    NewFilesUnique = sets:to_list(NewFilesSet),
%    NewN = length(NewFilesUnique),
%    if
%        NewN > N -> incl_files(NewFilesUnique, NewN);
%        true -> NewFilesUnique
%    end.


%%% @private
%% @doc Typexp nodes below 'Nodes' with 'What' type.
findallbelow(Nodes, What) ->
    findallbelowfun(Nodes, (fun({_Node, T}) -> T == What end)).

%%% @private
%% @doc Searches every typexp node below Nodes for nodes, what meet the
%% Fun confition.
findallbelowfun([], _) -> [];
findallbelowfun(Nodes, Fun) ->
    NodeDataPairs = [{Node, ?Anal:data(Node)} || Node <- Nodes],
    NodeTypePairs = [{Node, T} || {Node, #typexp{type=T}} <- NodeDataPairs],
    Hits = [H || {H, _} <- lists:filter(Fun, NodeTypePairs)],
    Children = lists:append([[C || {_, C} <- ?Anal:children(R)] || R <- Nodes]),
    Hits ++ findallbelowfun(Children, Fun).

%%% @private
%% @doc Searches every typexp node below NodesAndParents for nodes, what meet
%% the Fun confition. ParentType is the type of the parent of Node.
findallbelowfunparent([], _) -> [];
findallbelowfunparent(NodesAndParents, Fun) ->
    NodeDataPairs = [{Node, ParentType, ?Anal:data(Node)} ||
        {Node, ParentType} <- NodesAndParents],
    NodeTypePairs = [{Node, ParentType, T} ||
        {Node, ParentType, #typexp{type=T}} <- NodeDataPairs],
    Hits = [H || {H, _, _} <- lists:filter(Fun, NodeTypePairs)],
    Children = lists:append([[{C, T} || {_, C} <- ?Anal:children(R)] ||
        {R, _, T} <- NodeTypePairs]),
    Hits ++ findallbelowfunparent(Children, Fun).

