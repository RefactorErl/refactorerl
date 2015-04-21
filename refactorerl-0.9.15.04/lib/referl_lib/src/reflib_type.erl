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

%%% @doc High level type-related operations. This module contains
%%% functions that expect a namedtype semantical node as their parameter (or
%%% return a query that expects a namedtype semantical node as starting point).
%%%
%%% @author Denes Peteri <petden@caesar.elte.hu>

-module(reflib_type).
-vsn("$Rev$"). % for emacs "

%% =============================================================================
%% Exports

-export([name/1, arity/1, isopaque/1, isbuiltin/1, exported/1]).
-export([all_references/1, refspecs/1]).
-export([named_subtypes/1, named_subtypes/2, all_subtypes/1, all_subtypes/2]).
-export([find/2]).
-export([definition/0, module/0, typebody/0, typeparams/0, typeparam/1]).
-export([references/0, record_field_references/0, specguard_references/0,
         specparam_references/0, typebody_references/0]).

%%% @private
-export([findbelowdepth/2, named_typerefs_def/2,
         all_typerefs_def/2]). % for other reflib modules

-include("lib.hrl").

%%% testing: reflib_type:name({'$gn',namedtype,1}).
%%% ri:q("files.?"). % data of selectors
%%% refusr_ac:run("files.types.", detailed). % data of selectors and properties

%% =============================================================================
%% Type related properties

%% @spec name(node(#namedtype{})) -> atom()
%% @doc Returns the name of the type.
name(Type) -> (?ESG:data(Type))#namedtype.name.

%% @spec arity(node(#namedtype{})) -> integer()
%% @doc Returns the arity of the type.
arity(Type) -> (?ESG:data(Type))#namedtype.arity.

%% @spec isopaque(node(#namedtype{})) -> boolean()
%% @doc Is the type opaque?
isopaque(Type) -> (?ESG:data(Type))#namedtype.isopaque.

%% @spec exported(node(#namedtype{})) -> boolean()
%% @doc Returns whether the type is exported.
exported(Type) ->
    ?Query:exec(Type, ?Query:seq([
        module(),
        ?Mod:file(),
        [{form, {tag, '==', export_type}},
         eattr, esub, esub,
         {esub, {value, '==', name(Type)}}],
        [{esub, back},
         {esub, {value, '==', arity(Type)}}]]))
    /= [].

%% @spec isbuiltin(node(#namedtype{})) -> boolean()
%% @doc Is the type built-in Erlang type?
%% If not, then it's a user-defined type.
isbuiltin(Type) -> (?ESG:data(Type))#namedtype.isbuiltin.

%% @spec refspecs(node(#namedtype{})) -> [node(#spec{})]
%% @doc Specs using this type.
refspecs(Type) ->
    ?Query:exec(Type,
        ?Query:all([
            ?Query:seq([?Type:specguard_references(),
                        ?SpecGuard:specclause(),
                        ?SpecClause:spec()]),
            ?Query:seq([?Type:specparam_references(),
                        ?Query:any([[{specpar, back}],
                                    [{specret, back}]]),
                        ?SpecClause:spec()])])).

%% @spec all_references(node(#namedtype{})) -> [node()]
%% @doc Returns the fields, specguards, specparams, typebodies referencing
%% this Type.
all_references(Type) ->
    ?ESG:path(Type, record_field_references()) ++
        ?ESG:path(Type, specguard_references()) ++
        ?ESG:path(Type, specparam_references()) ++
        ?ESG:path(Type, typebody_references()).

%% @spec named_subtypes(node(#namedtype{})) -> [node(#namedtype{})]
%% @doc Returns the namedtypes used by this node.
named_subtypes(Type) ->
    %% don't need to go through typeparams, typeparams don't have named_subtypes
    Typebodies = ?ESG:path(Type, typebody()),
    lists:append([?TypeBody:named_subtypes(Typebody) || Typebody <- Typebodies]).

%% @spec named_subtypes(node(#namedtype{}), integer() | infinite) ->
%%     [node(#namedtype{})]
%% @doc Returns the namedtypes referenced by this node within a given Depth.
%% 0 depth means direct references. `infinite' is the same as named_subtypes/1.
named_subtypes(Type, Depth)->
    Typebodies = ?ESG:path(Type, typebody()),
    lists:append([?TypeBody:named_subtypes(Typebody, Depth) ||
                  Typebody <- Typebodies]).

%% @spec all_subtypes(node(#namedtype{})) -> [node(#typexp{})]
%% @doc Returns the (typexp) types used in this node.
all_subtypes(Type) ->
    Typebodies = ?ESG:path(Type, typebody()),
    Params = ?ESG:path(Type, typeparams()),
    lists:append([?TypeBody:all_subtypes(Typebody) || Typebody <- Typebodies]) ++
        lists:append([?TypeParam:all_subtypes(Param) || Param <- Params]).

%% @spec all_subtypes(node(#namedtype{}), integer() | infinite) ->
%%     [node(#typexp{})]
%% @doc Returns the (typexp) types referenced by this node within a given Depth.
%% 0 depth means direct references. `infinite' is the same as all_subtypes/1.
all_subtypes(Type, Depth)->
    Typebodies = ?ESG:path(Type, typebody()),
    Params = ?ESG:path(Type, typeparams()),
    lists:append([?TypeBody:all_subtypes(Typebody, Depth) ||
                  Typebody <- Typebodies]) ++
        lists:append([?TypeParam:all_subtypes(Param, Depth) ||
                      Param <- Params]).

%% =============================================================================
%% Queries starting from types

%% @spec find(atom(), integer()) -> query(#module{}, #namedtype{})
%% @doc The result query returns the type with name `Name' and arity `Arity'.
find(Name, Arity) ->
    [{type, {{name, '==', Name}, 'and', {arity, '==', Arity}}}].

%% @spec definition() -> query(#namedtype{}, #form{})
%% @doc The result query returns the defining form of the type.
definition() ->
    [{typedef, back}].

%% @spec module() -> query(#namedtype{}, #module{})
%% @doc The defining module of the type.
module() ->
    [{type, back}].

%% @spec references() -> query(#namedtype{}, #typexp{})
%% @doc Returns the call typexp nodes referencing this type.
references() ->
    [{typeref, back}].

%% @spec record_field_references() -> query(#namedtype{}, #field{})
%% @doc Returns the record fields referencing this Type.
record_field_references() ->
    references() ++ [typetop, fielddef].

%% @spec specguard_references() -> query(#namedtype{}, #specguard{})
%% @doc Returns the specguards referencing this Type.
specguard_references() ->
    references() ++ [typetop, specguarddef].

%% @spec specparam_references() -> query(#namedtype{}, #specparam{})
%% @doc Returns the specparams referencing this Type.
specparam_references() ->
    references() ++ [typetop, specparamdef].

%% @spec typebody_references() -> query(#namedtype{}, #namedtypebody{})
%% @doc Returns the typebodies referencing this Type.
typebody_references() ->
    references() ++ [typetop, typebodydef].

%% don't need typeparam_references/0, because typeparams can only be variables

%% @spec typebody() -> query(#namedtype{}, #namedtypebody{})
%% @doc The namedtypebody node belonging to the type.
typebody() ->
    [typebody].

%% @spec typeparams() -> query(#namedtype{}, #namedtypeparam{})
%% @doc The parameters of the named type.
typeparams() ->
    [typeparam].

%% @spec typeparam(integer()) -> query(#namedtype{}, #namedtypeparam{})
%% @doc The Index-th parameter.
typeparam(Index) ->
    [{typeparam, Index}].

%% =============================================================================
%% Functions used by other reflib modules

%%% @private
findbelowdepth([], _) -> [];
findbelowdepth(Nodes, infinite) ->
    Children = lists:append([?ESG:path(Node, [tsub]) || Node <- Nodes]),
    Nodes ++ findbelowdepth(Children, infinite);
findbelowdepth(Nodes, Depth) when Depth =< 0 ->
    Nodes;
findbelowdepth(Nodes, Depth) when is_integer(Depth) ->
    Children = lists:append([?ESG:path(Node, [tsub]) || Node <- Nodes]),
    Nodes ++ findbelowdepth(Children, Depth - 1).

%%% @private
named_typerefs_def(Node, Def) ->
    ?ESG:path(Node, Def ++ [{typetop, back}, typeref]).

%%% @private
all_typerefs_def(Node, Def) ->
    ?ESG:path(Node, Def ++ [{typetop, back}]).

