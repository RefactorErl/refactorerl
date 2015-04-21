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

%%% @doc Function specification (-spec) analyser
%%%
%%% Nodes added by the spec analyser:
%%% <ul>
%%%     <li><b>`spec'</b>: Gets created, when we insert a spec form,
%%%         <b>`specdef'</b> edge points to the `spec' node from the form.
%%%         It's under a `module' (with a <b>`spec'</b> edge). If there's a
%%%         `module_qualifier', the `spec' node gets inserted under the
%%%         `module' referred by the qualifier. If there's `function' with the
%%%         same name and arity in the module, a <b>`specref'</b> edge links
%%%         the `function' to the `spec'.</li>
%%%     <li><b>`specclause'</b>: A clause of the function specification.<br/>
%%%         Example: `-spec f() -> ok; () -> nok.'<br/>
%%%         There's at least one `specclause' under `spec' (with a
%%%         <b>`specclause'</b> edge), can be more. The sequence of the clauses
%%%         are the same in the code and in the `[specclause]' graph path.
%%%         <b>`specclausedef'</b> edge points from the definition of the
%%%         clause (a `fun_sig' type expression) to the `specclause'.</li>
%%%     <li><b>`specguard'</b>: There's a `specguard' node under `specclause'
%%%         (<b>`specguard'</b> edge), if there are constraints in the
%%%         clause.<br/>
%%%         Example: `-spec f(A) -> ok when A :: tuple().'<br/>
%%%         There can only be at most 1 `specguard' connected to a clause.
%%%         <b>`specguarddef'</b> goes from the definition of the guard-group
%%%         (which can contain more guard-like constrains) to the `specguard'
%%%         node.</li>
%%%     <li><b>`specparam'</b>: There's exactly one `specparam' with `specret'
%%%         `type' under every `specclause' (with a <b>`specret'</b> edge).
%%%         <b>`specparamdef'</b> edge comes from the definition of the
%%%         specification's body; <b>`specparamref'</b> edge comes from the
%%%         `fret' expression of the function, if there's a function
%%%         referencing the spec.<br/>
%%%         There could be any number of `specparam' nodes with `specpar'
%%%         `type' and <b>`specpar'</b> edge under the clause; it depends on
%%%         the arity of the function. <b>`specparamdef'</b> edge comes from
%%%         the definition of the parameter; <b>`specparamref'</b> edge comes
%%%         from the `fpar' expression of the function.<br/>
%%%         In the graph, `specpar' always means the input arguments of the
%%%         specification; `specret' is the output parameter of the spec (type
%%%         of the return value of the function); and `specparam' means both
%%%         of them together.</li>
%%% </ul>
%%%
%%% Other edges:
%%% <ul>
%%%     <li><b>`typexpmodref'</b>: If there's a `module_qualifier' in the
%%%         definition of the specification, a `typexpmodref' edge points from
%%%         a `atom' `typexp' under the `module_qualifier' to the `module' it
%%%         references. There could be other `typexpmodref' edges in the form
%%%         in `call' `typexp' nodes (see `refanal_type' documentation).</li>
%%% </ul>
%%%
%%% @author Denes Peteri <petden@caesar.elte.hu>

-module(refanal_spec).
-vsn("$Rev$").
-behaviour(refcore_anal).

-export([schema/0, externs/1, insert/4, remove/4, update/2]).
-export([error_text/2]).

-export([findbelow/3, findover/2]). % for refanal_type

-include("core.hrl").

%%% ============================================================================
%%% Schema, externs, error_text

%%% @private
error_text(spec_arity_mismatch, [Arity, Arity2]) ->
    io_lib:format("The arity_qualifier (~p) doesn't match the number of
        parameters (~p).~n", [Arity2, Arity]).

%%% @private
schema() ->
    [{spec, record_info(fields, spec), []},
     {module, [{spec, spec}]},
     {form, [{specdef, spec}]},
     {func, [{specref, spec}]},
     {specparam, record_info(fields, specparam), []},
     {typexp, [{specparamdef, specparam}]},
     {expr, [{specparamref, specparam}]},
     {specclause, record_info(fields, specclause), []},
     {specclause, [{specpar, specparam}, {specret, specparam},
                   {specguard, specguard}]},
     {typexp, [{specclausedef, specclause}, {typexpmodref, module},
               {specguarddef, specguard}]},
     {spec, [{specclause, specclause}]},
     {specguard, record_info(fields, specguard), []}].

%%% @private
externs(_) -> [].

%%% ============================================================================
%%% Insert

%%% @private
insert(Parent, _Pre, {_Tag, Child}, _Post) ->
    case ?Anal:data(Child) of
        #form{type=spec} ->
            {File, Form} = {Parent, Child},
            [Left, Right] = [Typexp || {tattr, Typexp} <- ?Anal:children(Form)],
            Arity = spec_arity(Right),
            check_arity(Left, Arity),
            {Name, Module} = spec_name(Left, File),
            ?NodeSync:add_ref(spec, {def, Form}, {Module, {Name, Arity}}),
            Spec = ?NodeSync:get_node(spec, {true, Module, {Name, Arity}}),
            add_body(Right, Spec),
            Func = ?NodeSync:get_node(func, {Module, {Name, Arity}}),
            add_fun(Spec, Func);
        #form{type=func} ->
            {_File, Form} = {Parent, Child},
            Func = ?NodeSync:get_node(func, Form),
            {Name, Arity} = {?Fun:name(Func), ?Fun:arity(Func)},
            case ?Graph:path(Func, ?Fun:module()) of
                [Module] ->
                    Spec = ?NodeSync:get_node(spec, {false, Module, {Name, Arity}}),
                    case Spec of
                        not_found -> ok;
                        _ -> add_fun(Spec, Func)
                    end;
                _ -> ok
            end;
        #form{} -> ok;
        _ ->
            Form = findover(Parent, 'form'),
            case Form of
                not_found -> ok;
                _ ->
                    insert(?Anal:parent(Form), undefined, {undefined, Form},
                           undefined)
                    % would do almost the same thing as insert, that's why I call it
            end
    end,
    ok.

%%% ============================================================================
%%% Functions used in insert

spec_arity(Right) ->
    FunSigs = findbelow([Right], 'fun_sig', ['spec_union', 'spec_guard']),
    [OneFunSig | _] = FunSigs,
    [{tsub, OneArgList}, {tsub, _OneReturn}] = ?Anal:children(OneFunSig),
    OnePars = [P || {tsub, P}<-?Anal:children(OneArgList)],
    length(OnePars).

check_arity(Left, Arity) ->
    IntExps =
        findbelow([Left], 'integer', ['arity_qualifier', 'module_qualifier']),
    case IntExps of
        [] -> ok;
        [IntExp | _] ->
            #typexp{tag=Arity2} = ?Anal:data(IntExp),
            case Arity2 of
                Arity -> ok;
                _ -> throw(?LocalError(spec_arity_mismatch, [Arity, Arity2]))
            end
    end.

spec_name(Left, File) ->
    NameExps = findbelow([Left], 'atom', ['arity_qualifier', 'module_qualifier']),
    case NameExps of
        [ModuleQualifierExp, NameExp] ->
            #typexp{tag=ModuleQualifierName} = ?Anal:data(ModuleQualifierExp),
            ?NodeSync:add_ref(module, {typexpref, ModuleQualifierExp},
                              ModuleQualifierName),
            Module = ?NodeSync:get_node(module, ModuleQualifierName);
        [NameExp] ->
            Module = refanal_type:get_module_from_file(File)
    end,
    #typexp{tag=Name} = ?Anal:data(NameExp),
    {Name, Module}.

add_body(Right, Spec) ->
    case ?Anal:data(Right) of
        #typexp{type='spec_union'} ->
            ClauseAndGuards = [C || {tsub, C} <- ?Anal:children(Right)];
        _ -> ClauseAndGuards = [Right]
    end,
    lists:foreach(fun(CG) ->
        {F, Guards} = get_funsig_guards(CG),
        add_cluase_param_guard(F, Guards, Spec)
    end, ClauseAndGuards).

%% used by add_body/2
get_funsig_guards(CG) ->
    case ?Anal:data(CG) of
        #typexp{type='spec_guard'} ->
            [{tsub, F}, {tsub, Guards}] = ?Anal:children(CG);
        #typexp{type='fun_sig'} ->
            F = CG,
            Guards = not_found
    end,
    {F, Guards}.

%% used by add_body/2
add_cluase_param_guard(F, Guards, Spec) ->
    ?NodeSync:add_ref(specclause, {def, F}, {-1, Spec}),
        Clause = ?NodeSync:get_node(specclause, F),
        [{tsub, ArgList}, {tsub, Return}] = ?Anal:children(F),
        Pars = [P || {tsub, P}<-?Anal:children(ArgList)],
        ?NodeSync:add_ref(specparam, {def, Return}, {-1, varname(Return),
            Clause, specret, value(Return)}),
        [?NodeSync:add_ref(specparam, {def, P}, {-1, varname(P), Clause,
            specpar, value(P)}) || P <- Pars],
        % todo: function clause information is lost
        case Guards of
            not_found -> ok;
            _ -> ?NodeSync:add_ref(specguard, {def, Guards},
                                   {Clause, value(Guards)})
        end.

%% for #form{type=spec} and #form{type=func}
add_fun(Spec, Func) ->
    ?NodeSync:add_ref(spec, {ref, Func}, Spec),
    Clauses = ?NodeSync:get_node(specclause, Spec),
    Fret = ?NodeSync:get_node(fret, Func),
    lists:foreach(fun(Clause) ->
        case ?NodeSync:get_node(specparam, {specret, Clause}) of
            not_found -> ok;
            Sret -> ?NodeSync:add_ref(specparam, {ref, Fret}, Sret)
        end
    end, Clauses),
    Fpars = ?NodeSync:get_node(fpar, Func),
    FparIndexPairs = lists:zip(Fpars, lists:seq(1, length(Fpars))),
    lists:foreach(fun(Clause) ->
        lists:foreach(fun({Fpar, Index}) ->
            case ?NodeSync:get_node(specparam, {Index, undefined, Clause,
                                                specpar, undefined}) of
                not_found -> ok;
                Spar -> ?NodeSync:add_ref(specparam, {ref, Fpar}, Spar)
            end
        end, FparIndexPairs)
    end, Clauses).

%% used by add_cluase_param_guard/3
value(Typexp) -> ?Syn:flat_text(Typexp).

%% used by add_cluase_param_guard/3
varname(Typexp) ->
    case ?Anal:data(Typexp) of
        #typexp{type=vardef} ->
            MaybePairs = [{MaybeVariable, ?Anal:data(MaybeVariable)} ||
                {tsub, MaybeVariable} <- ?Anal:children(Typexp)],
            [N | _] = [Name || {_MaybeVariable, #typexp{type=variable,
                tag=Name}} <- MaybePairs],
            N;
        _ -> undefined
    end.

%%% ============================================================================
%%% Remove

%%% @private
remove(Parent, _Pre, {_Tag, Child}, _Post) ->
    case ?Anal:data(Child) of
        #form{type=spec} ->
            {_File, Form} = {Parent, Child},
            case ?NodeSync:get_node(spec, Form) of
                not_found -> ok;
                Spec -> del_spec(Spec, Form)
            end;
        #form{type=func} ->
            {_File, Form} = {Parent, Child},
            Func = ?NodeSync:get_node(func, Form),
            case ?NodeSync:get_node(spec, Func) of
                not_found -> ok;
                Spec ->
                    Clauses = ?NodeSync:get_node(specclause, Spec),
                    lists:foreach(fun(Clause) ->
                        SpecParams = ?NodeSync:get_node(specparam, Clause),
                        SFPairs = [{S, F} || S <- SpecParams, F <-
                            ?Graph:path(S, [{specparamref,back}])],
                        [?NodeSync:del_ref(specparam, {ref, F}, S) || {S, F} <-
                            SFPairs]
                    end, Clauses),
                    ?NodeSync:del_ref(spec, {ref, Func}, Spec)
            end;
        #form{} -> ok;
        _ ->
            Form = findover(Parent, 'form'),
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

del_spec(Spec, Form) ->
    del_body(Spec),
    Modules = ?Graph:path(Spec, ?Spec:module()),
    lists:foreach(fun(Module) ->
        ?NodeSync:del_ref(spec, {def, Form}, {Module, Spec}),
        del_typexpmodref(Form, Module)
    end, Modules).

del_body(Spec) ->
    Clauses = ?NodeSync:get_node(specclause, Spec),
    lists:foreach(fun(Clause) ->
        STTTriplets = get_param_typexp_type(Clause),
        [?NodeSync:del_ref(specparam, {def, T}, {Clause, S, SType}) ||
            {S, T, SType} <- STTTriplets],
        case ?NodeSync:get_node(specguard, {false, Clause, undefined}) of
            not_found -> ok;
            Guard ->
                case ?Graph:path(Guard, [{specguarddef, back}]) of
                    [GuardTypexp] ->
                        ?NodeSync:del_ref(specguard, {def, GuardTypexp},
                                          {Guard, Clause});
                    _ -> ok
                end
        end,
        CDefTypexps = ?Graph:path(Clause, [{specclausedef, back}]),
        [?NodeSync:del_ref(specclause, {def, Typexp}, {Clause, Spec}) ||
            Typexp <- CDefTypexps]
    end, Clauses).

%% used by del_body/1
get_param_typexp_type(Clause) ->
    STTTriplets1 = [{S, T, specpar} || S <- ?NodeSync:get_node(specparam,
        {specpar, Clause}), T <- ?Graph:path(S, [{specparamdef,back}])],
    case ?NodeSync:get_node(specparam, {specret, Clause}) of
        not_found -> STTTriplets2 = [];
        S -> STTTriplets2 = [{S, T, specret} || T <-
            ?Graph:path(S, [{specparamdef,back}])]
    end,
    STTTriplets1 ++ STTTriplets2.

del_typexpmodref(Form, Module) ->
    Typexps = [Typexp || {tattr, Typexp} <- ?Anal:children(Form)],
    NameExps = findbelow(Typexps, 'atom', ['arity_qualifier', 'module_qualifier']),
    case NameExps of
        [ModuleQualifierExp, _NameExp] ->
            % checking if refanal_type already deleted the typexpmodref
            case ?Graph:path(ModuleQualifierExp, ?Typexp:typexpmodref()) of
                [Module] ->
                    ?NodeSync:del_ref(module, {typexpref, ModuleQualifierExp},
                                      Module);
                _ -> ok
            end;
        [_NameExp] -> ok
    end.

%%% ============================================================================
%%% Update

%%% @private
%% todo: Handle refactorings.
update(Node, _Data) ->
    Parent = ?Anal:parent(Node),
    remove(Parent, undefined, {undefined, Node}, undefined),
    insert(Parent, undefined, {undefined, Node}, undefined),
    ok.

%%% ============================================================================
%%% Other functions

%%% @private
%% @doc Typexp nodes below `Nodes' with `What' type with a route with nodes
%% only with a type from Through.
findbelow([], _, _) -> [];
findbelow(Nodes, What, Through) ->
    NodeDataPairs = [{Node, ?Anal:data(Node)} || Node <-Nodes],
    NodeTypePairs = [{Node, T} || {Node, #typexp{type=T}} <- NodeDataPairs],
    Hits = [H || {H, _} <- lists:filter((fun({_Node, T}) -> T == What end),
        NodeTypePairs)],
    Routes = [R || {R, _} <- lists:append([lists:filter((fun({_Node, T}) ->
        T == Thr end),  NodeTypePairs) || Thr <- Through])],
    Children = lists:append([[C || {tsub, C} <- ?Anal:children(R)] || R <-
        Routes]),
    Hits ++ findbelow(Children, What, Through).

%%% @private
%% @doc The form node over `Node'.
findover(Node, What) ->
    case ?Syn:node_type(Node) of
        What -> Node;
        'root' -> not_found;
        _ -> findover(?Anal:parent(Node), What)
    end.

