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

%%% @doc High level typexp related operations. This module contains
%%% functions that expect a typexp node as their parameter
%%% (or return a query that expects a typexp node as starting point).
%%%
%%% @author Denes Peteri <petden@caesar.elte.hu>

-module(reflib_typexp).
-vsn("$Rev$").

%% =============================================================================
%% Exports

-export([type/1, tag/1, form/1, flat_text/1, name/1, arity/1, file/1, subtypes/1]).
-export([all_typerefs/1, all_typerefs/2, named_typerefs/1, named_typerefs/2]).
-export([call_name/1, call_args/1, call_module/1, parent/1]).
-export([typetop/0, record_field/0, typebody/0, typeparam/0, specclause/0,
         specguard/0, specparam/0, typeref/0, typeparamref/0, typexpmodref/0,
         children/0]).

-include("lib.hrl").

%% =============================================================================
%% Properties

%% @spec flat_text(node(#typexp{})) -> string()
%% @doc The text representation of the type expression.
flat_text(Typexp) ->
    ?Syn:flat_text(Typexp).

%% @spec file(node(#typexp{})) -> node(#file{})
%% @doc The defining file of the type expression.
file(Typexp) ->
    [File] = ?ESG:path(form(Typexp), ?Form:file()),
    File.

%% @spec form(node(#typexp{})) -> node(#form{})
%% @doc The form over the type expression.
form(Typexp) ->
    case parent(Typexp) of
        [] ->
            [Form] = ?ESG:path(Typexp, [{tattr, back}]),
            Form;
        [Parent] ->
            form(Parent)
    end.

subtypes(Typexp) ->
    Children = ?ESG:path(Typexp, children()),
    lists:foldl(
        fun(Elem, Acc) ->
            case type(Elem) of
                arglist ->
                    Acc ++ subtypes(Elem);
                'atom' ->
                    Acc;
                _ ->
                    Acc ++ [Elem]
            end
        end, [], Children).

name(Typexp) ->
    case type(Typexp) of
        'atom' ->
            case tag(Typexp) of
                undefined ->
                    'atom';
                A ->
                    A
            end;
        call ->
            call_name(Typexp);
        variable ->
            tag(Typexp);
        vardef ->
            [Var] = ?Query:exec(Typexp, [{tsub, {type, '==', variable}}]),
            tag(Var);
        _ ->
            undefined
    end.

arity(Typexp) ->
    case type(Typexp) of
        call ->
            [NamedType] = ?ESG:path(Typexp, typeref()),
            ?Type:arity(NamedType);
        vardef ->
            [Var] = ?Query:exec(Typexp, [{tsub, {type, '/=', variable}}]),
            arity(Var);
        arglist ->
            length( ?ESG:path(Typexp, children()) );
        _ ->
            0
    end.

%% @spec parent(node(#typexp{})) -> node(#typexp{})
%% @doc The parent typexp of the type expression.
parent(Typexp) ->
    ?ESG:path(Typexp, parent_typexp()).

%% @spec type(node(#typexp{})) -> atom()
%% @doc The type (structure) of the type expression.
type(Typexp) ->
    (?ESG:data(Typexp))#typexp.type.

%% @spec tag(node(#typexp{})) -> any()
%% @doc The tag field of the typexp.
tag(Typexp) ->
    (?ESG:data(Typexp))#typexp.tag.

%% @spec all_typerefs(node(#typexp{})) -> [node(#typexp{})]
%% @doc Returns the (typexp) types used in this node.
all_typerefs(Typexp) ->
    all_typerefs(Typexp, infinite).

%% @spec all_typerefs(node(#typexp{}), integer() | infinite) ->
%%     [node(#typexp{})]
%% @doc Returns the (typexp) types referenced by this node within a given Depth.
%% 0 depth means direct references. `infinite' is the same as all_subtypes/1.
all_typerefs(Typexp, Depth) ->
    SubtreeRoots = [Typexp],
    Typexps = ?Type:findbelowdepth(SubtreeRoots, Depth),
    lists:filter(fun(CTypexp) ->
        case ?ESG:path(CTypexp, [typetop]) of
            [] -> false;
            _ -> true
        end
    end, Typexps).

%% @spec named_typerefs(node(#typexp{})) -> [node(#namedtype{})]
%% @doc Returns the namedtypes used by this node.
named_typerefs(Typexp) ->
    named_typerefs(Typexp, infinite).

%% @spec named_typerefs(node(#typexp{}), integer() | infinite) ->
%%     [node(#namedtype{})]
%% @doc Returns the namedtypes referenced by this node within a given Depth.
%% 0 depth means direct references. `infinite' is the same as named_typerefs/1.
named_typerefs(Typexp, Depth) ->
    SubtreeRoots = [Typexp],
    Typexps = ?Type:findbelowdepth(SubtreeRoots, Depth),
    lists:append([?ESG:path(CTypexp, [typeref]) || CTypexp <- Typexps]).

%% @spec call_name(node(#typexp{})) -> atom() | undefined
%% @doc If the node's type is `call', returns the name of the referenced
%% named type.
call_name(Typexp) ->
    case type(Typexp) of
        call ->
            case ?ESG:path(Typexp, children()) of
                [ModuleQualifier, _Arglist] ->
                    call_name_module_qualifier(ModuleQualifier);
                _ -> undefined
            end;
        _ -> undefined
    end.

%%% @private
call_name_module_qualifier(ModuleQualifier) ->
    case type(ModuleQualifier) of
        'atom' -> tag(ModuleQualifier);
        'module_qualifier' ->
            case ?ESG:path(ModuleQualifier, children()) of
                [_ModuleAtom, NameAtom] ->
                    case type(NameAtom) of
                        'atom' -> tag(NameAtom);
                        _ -> undefined
                    end;
                 _ -> undefined
            end;
        _ -> undefined
    end.

%% @spec call_module(node(#typexp{})) -> atom() | undefined
%% @doc If the node's type is `call', and there's a `quality_modifier' under
%% it, returns the referenced module's name. Otherwise, it returns `undefined'.
call_module(Typexp) ->
    case type(Typexp) of
        call ->
            case ?ESG:path(Typexp, children()) of
                [ModuleQualifier, _Arglist] ->
                    call_module_module_qualifier(ModuleQualifier);
                _ -> undefined
            end;
        _ -> undefined
    end.

%%% @private
call_module_module_qualifier(ModuleQualifier) ->
    case type(ModuleQualifier) of
        'module_qualifier' ->
            case ?ESG:path(ModuleQualifier, children()) of
                [ModuleAtom, _NameAtom] ->
                    case type(ModuleAtom) of
                        'atom' -> tag(ModuleAtom);
                        _ -> undefined
                    end;
                 _ -> undefined
            end;
        _ -> undefined
    end.

%% @spec call_args(node(#typexp{})) -> [node(#typexp{})]
%% @doc If the node's type is `call', returns the parameters of the call.
call_args(Typexp) ->
    case type(Typexp) of
        call ->
            case ?ESG:path(Typexp, children()) of
                [_ModuleQualifier, Arglist] ->
                    case type(Arglist) of
                        'arglist' -> ?ESG:path(Arglist, children());
                        _ -> []
                    end;
                _ -> []
            end;
        _ -> []
    end.

%% =============================================================================
%% Queries

%% @spec children() -> query(#typexp{}, #typexp{})
%% @doc The subtypes (type expressions) of the typexp. The components of it.
children() ->
    [tsub].

%% @spec parent_typexp() -> query(#typexp{}, #typexp{})
%% @doc The parent (type expressions) of the typexp.
parent_typexp() ->
    [{tsub, back}].

%% @spec typetop() -> query(#typexp{}, #typexp{})
%% @doc The top of the type expression subtree the node is in.
%% Doesn't work for these types: arglist, bin_base, bin_unit, poly_sig,
%% fun_sig, spec_guard, paren, module_qualifier
%% Doesn't always work for: integer, variable, joker, atom
typetop() ->
    [typetop].

%% @spec record_field() -> query(#typexp{}, #field{})
%% @doc If the typexp defines a field of a record, returns that.
record_field() ->
    [fielddef].

%% @spec typebody() -> query(#typexp{}, #namedtypebody{})
%% @doc If the typexp defines the body of a named type, returns that.
typebody() ->
    [typebodydef].

%% @spec typeparam() -> query(#typexp{}, #namedtypeparam{})
%% @doc If the typexp defines a parameter of a named type, returns that.
typeparam() ->
    [typeparamdef].

%% @spec specclause() -> query(#typexp{}, #specclause{})
%% @doc If the typexp defines a clause of a spec, returns that.
specclause() ->
    [specclausedef].

%% @spec specguard() -> query(#typexp{}, #specguard{})
%% @doc If the typexp defines a guard of a specclause, returns that.
specguard() ->
    [specguarddef].

%% @spec specparam() -> query(#typexp{}, #specparam{})
%% @doc If the typexp defines a parameter of a specclause, returns that.
specparam() ->
    [specparamdef].

%% @spec typeref() -> query(#typexp{}, #namedtype{})
%% @doc If the typexp is a call, returns the referenced named type
typeref() ->
    [typeref].

%% @spec typeparamref() -> query(#typexp{}, #namedtypeparam{})
%% @doc If the typexp is in a arglist under a call, returns the referenced
%% type parameters.
typeparamref() ->
    [typeparamref].

%% @spec typexpmodref() -> query(#typexp{}, #module{})
%% @doc If the typexp references a module, returns it.
%% The type of the typexp is `atom'. It's either under a `module_qualifier'; or
%% a call referencing an Erlang built-in type.
typexpmodref() ->
    [typexpmodref].

