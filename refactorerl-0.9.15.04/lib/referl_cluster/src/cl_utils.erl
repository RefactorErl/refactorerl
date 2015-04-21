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

%%% @doc Contains utilities for the clustering modules.
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(cl_utils).
-vsn("$Rev: 9609 $").

-export([ignore/1, leave/1, transform_to_01/3, 
         get_defined_value/2, proplist_update/2,
         concat_atoms/2]).
         
-include("../include/cluster.hrl").

%% @spec ignore(term()) -> (term(),[{Attr,Out::integer()}]) -> bool()
%%
%% @doc Entity filter that drops the modules given as arguments.
ignore(Modules) ->
    fun (Module, _Calls) ->
            lists:member(Module, Modules)
    end.

%% @spec leave(term()) -> (term(),[{Attr,Out::integer()}]) -> bool()
%%
%% @doc Entity filter that keeps the modules given as arguments.
leave(Modules) ->
    fun (Module, _Calls) ->
            not lists:member(Module, Modules)
    end.

%% @private
transform_to_01(_,size,N) -> N;
transform_to_01(_,entities,L) -> L;
transform_to_01(default,default,_N) -> ?Matrix:get_default();
transform_to_01(_,_,Val) -> 
    case ?Matrix:get_from_value(Val, func) of
	    0 -> Val;
	    _ -> ?Matrix:change_value(Val, func, 1)
	end.

%%%%% proplists

%% @spec get_defined_value(atom(),proplist()) -> term()
%%
%% @doc Gets the value belonging to `Item' in the `Options' property list.
%% If `Item' is not defined in `Options', an exception will be thrown.
get_defined_value(Item,Options) ->
    case proplists:get_value(Item,Options) of
        undefined ->
            throw(
              "The following option is undefined, which should be defined: '"++
              atom_to_list(Item)++"'");
        Value ->
            Value
    end.

%% @spec proplist_update(proplist(),proplist()) -> proplist()
%%
%% @doc Updates `List1' by `List2'.
%% The new properties will be the properties of `List2', except for the
%% properties that are only present in `List1'.
proplist_update(List1, List2) ->
    UList1 = proplists:unfold(List1),
    UList2 = proplists:unfold(List2),
    Keys   = lists:usort(proplists:get_keys(UList1) ++ 
                             proplists:get_keys(UList2)),
    proplist_update(Keys, UList1, UList2).

%% @spec proplist_update(list(), proplist(), proplist()) -> proplist()
%%
%% @doc Creates a new proplist, according to the given `Key' list.
%% The new properties will be the properties of `List2', except for the
%% properties that are only present in `List1'.
%% `List1' and `List2' must be unfolded.
proplist_update([], _List1, _List2) ->
    [];
proplist_update([Key | Keys], List1, List2) ->
    case proplists:lookup(Key, List2) of
        none ->
            [proplists:lookup(Key, List1)
             | proplist_update(Keys, List1, List2)];
        Entry ->
            [Entry | proplist_update(Keys, List1, List2)]
    end.
    
%% spec proplist_update_sorted(proplist(),proplist()) -> proplist()
%%
%% doc Updates `List1' by `List2'.
%% The new properties will be the properties of `List2', except for the
%% properties that are only present in `List1'.
%% `List1' and `List2' must be unfolded and sorted.
%% proplist_update_sorted([],[]) ->
%%     [];
%% proplist_update_sorted([],L2) ->
%%     L2;
%% proplist_update_sorted(L1,[]) ->
%%     L1;
%% proplist_update_sorted([{Key,_Value1}|T1],[{Key,Value2}|T2]) ->
%%     [{Key,Value2}|proplist_update_sorted(T1,T2)];
%% proplist_update_sorted([{Key1,_}=H1|T1],[{Key2,_}=H2|T2]) ->
%%     case Key1 < Key2 of
%%         true ->
%%             [H1|proplist_update_sorted(T1,[H2|T2])];
%%         false ->
%%             [H2|proplist_update_sorted([H1|T1],T2)]
%%     end.

%%%%% etc

%% @spec concat_atoms(atom(),atom()) -> atom()
%%
%% @doc Concatenates two atoms as strings.
concat_atoms(Atom1,Atom2) ->
    list_to_atom(atom_to_list(Atom1) ++ atom_to_list(Atom2)).

