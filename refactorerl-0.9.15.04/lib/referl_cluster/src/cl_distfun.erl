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

%%% @doc This module contains functions to calculate the distance of two
%%% entities based on their attributes. The functions assume that attribute
%%% values are numbers.
%%%
%%% @author Aniko Nagyne Vig <viganiko@inf.elte.hu>
%%% @author Krisztian Toth <ccc@inf.elte.hu>
%%% @author Csaba Hoch <hoch@inf.elte.hu>
%%% @author Laszlo Budai <budail@caesar.elte.hu>

-module(cl_distfun).
-vsn("$Rev: 9609 $").

-include_lib("referl_cluster/include/cluster.hrl").

%% =============================================================================
%% Exports

-compile({no_auto_import, [min/2, max/2]}).

-export([euclidian/4, jaccard/4, sorensen_dice/4, sorensen_dice2/4,
         ochiai/4, correlation/4, call_sum/4, call_cnt/4,
         weight_gen/1,weight_distvec/4,
         call_fun_cnt/4,call_fun_wgt/1,lexgr_dist/1]).
-export([pow_size_fun_gen/1]).
% Distance function with antigravity generator
-export([generate_antigravity/2]).
% Function distances
-export([generate_fun_common_refs/1, fun_common_refs/4]).


%% =============================================================================
%%

-import(proplists,[get_value/2, get_value/3]).


%% =============================================================================
%% Types

%% @type entity() = term().
%% Entity of the clustering.

%% @type attr() = term().
%% Attribute of the clustering.

%% @type attrLst() = [{attr(), Value::term()}].
%% Tha attribute value list of an entity.

%%% @type dist_fun() = (Entity1::entity(), AttrLst1::attrLst(),
%%%                     Entity2::entity(), AttrLst2::attrLst()) -> number().
%%% Calculate distance of two entity in the same clusterings.

%%% @type size_fun() = (Size1::number(), Size2::number()) -> number().
%%%
%%% It is a function that can be used by the {@link weight_gen/1} function.



%% =============================================================================
%%

fold2(_,   Acc, [],      _)       -> Acc;
fold2(_,   Acc, _,       [])      -> Acc;
fold2(Fun, Acc, [H1|T1], [H2|T2]) -> fold2(Fun, Fun(H1, H2, Acc), T1, T2).

%% @spec euclidian(Mod, [{Attr, Weight}], Mod, [{Attr, Weight}]) -> Distance
%%       Attr = #fun_attr{} | #rec_attr{}
%%
%% @doc Euclidian distance function.
euclidian(_E1, Attr1, _E2, Attr2) ->
    math:sqrt(fold2(
                fun({_,V1}, {_,V2}, Sum) -> Sum + (V1-V2) * (V1-V2) end,
                0, Attr1, Attr2)).


-record(presence, {both=0, first=0, second=0, none=0}).

presence(Attr1, Attr2) ->
    fold2(
      fun
      ({_,0}, {_,0}, P=#presence{none=N})   -> P#presence{none=N+1};
      ({_,_}, {_,0}, P=#presence{first=N})  -> P#presence{first=N+1};
      ({_,0}, {_,_}, P=#presence{second=N}) -> P#presence{second=N+1};
      ({_,_}, {_,_}, P=#presence{both=N})   -> P#presence{both=N+1}
         end,
      #presence{},
      Attr1, Attr2).

%% @spec jaccard(Mod, [{Attr, Weight}], Mod, [{Attr, Weight}]) -> Distance
%%       Attr = #fun_attr{} | #rec_attr{}
%%
%% @doc Jaccard distance function.
jaccard(_E1, Attr1, _E2, Attr2) ->
    #presence{both=A, first=B, second=C} = presence(Attr1, Attr2),
    if
        A+B+C == 0 -> 1;
        true       -> 1 - A/(A+B+C)  %% the paper contains an sqrt too
    end.

%% @spec sorensen_dice(Mod, [{Attr, Weight}], Mod, [{Attr, Weight}]) -> Distance
%%       Attr = #fun_attr{} | #rec_attr{}
%%
%% @doc Sorensen-Dice distance function.
sorensen_dice(_E1, Attr1, _E2, Attr2) ->
    #presence{both=A, first=B, second=C} = presence(Attr1, Attr2),
    if
        A+B+C == 0 -> 1;
    true       -> 1 - 2*A / (2*A + B + C)
    end.

%% @spec sorensen_dice2(Mod,[{Attr, Weight}],Mod,[{Attr, Weight}]) -> Distance
%%       Attr = #fun_attr{} | #rec_attr{}
%%
%% @doc Sorensen-Dice distance function variant.
sorensen_dice2(_E1, Attr1, _E2, Attr2) ->
    #presence{both=A, first=B, second=C, none=D} = presence(Attr1, Attr2),
    if
        A+B+C+D == 0 -> 1;
    true         -> 1 - 2*(A+D) / (2*A + B + C + D)
                        %%orig nincs +D alul, de akkor negativ ertekek
    end.

%% @spec ochiai(Mod, [{Attr, Weight}], Mod, [{Attr, Weight}]) -> Distance
%%       Attr = #fun_attr{} | #rec_attr{}
%%
%% @doc Ochiai distance function.
ochiai(_E1, Attr1, _E2, Attr2) ->
    #presence{both=A, first=B, second=C} = presence(Attr1, Attr2),
    if
        (A+B)*(A+C) == 0 -> 1;
        true             -> 1 - A/math:sqrt((A+B)*(A+C))
    end.

%% @spec correlation(Mod, [{Attr, Weight}], Mod, [{Attr, Weight}]) -> Distance
%%       Attr = #fun_attr{} | #rec_attr{}
%%
%% @doc Distance function based on correlation of attributes.
correlation(_E1, Attr1, _E2, Attr2) ->
    {SumV1, SumV2, SumV1xV1, SumV2xV2, SumV1xV2} =
        fold2(
          fun({_,V1}, {_,V2},
              {SV1, SV2, SV11, SV22, SV12}) ->
                  {SV1+V1, SV2+V2, SV11 + V1*V1, SV22 + V2*V2, SV12 + V1*V2}
          end, {0, 0, 0, 0, 0}, Attr1, Attr2),
    Corr = (SumV1xV1 - SumV1*SumV1) * (SumV2xV2 - SumV2*SumV2),
    R = if
            Corr =< 0 -> 1;
            true      -> (SumV1xV2 - SumV1*SumV2/length(Attr1)) / Corr
        end,
    if
        1-R < 0 -> 1;
        true    -> math:sqrt((1-R)/2)
    end.


%% @spec call_sum(Mod, [{Attr, Weight}], Mod, [{Attr, Weight}]) -> Distance
%%       Attr = #fun_attr{} | #rec_attr{}
%%
%% @doc Distance function based on function call structure, sums call weights.
call_sum(Ent1, Attr1, Ent2, Attr2) ->
    Val = calls(Ent1, modcall_sum(Attr1), Ent2, modcall_sum(Attr2)),
    %?d({Ent1, Attr1, Ent2, Attr2, Val}),
    Val.

modcall_sum(Attr) ->
    Mods = lists:usort([M || {M,W} <- Attr, 
                                 ?Matrix:get_from_value(W,func)>0]),
    [{Mod, lists:sum([?Matrix:get_from_value(W,func) || {M,W} <- Attr,
                               M == Mod])} ||
        Mod <- Mods].

%% @spec call_cnt(Mod, [{Attr, Weight}], Mod, [{Attr, Weight}]) -> Distance
%%       Attr = #fun_attr{} | #rec_attr{}
%%
%% @doc Distance function based on function call structure, ignores call
%% weights, only the count of different functions is used.
call_cnt(Ent1, Attr1, Ent2, Attr2) ->
    calls(Ent1, modcall_cnt(Attr1,1), Ent2, modcall_cnt(Attr2,1)).

%% @spec modcall_cnt(attrLst(), Add::(w | 1)) -> [{mod_name(), value()}]
%%
%% @doc Calculates that which module is called how many times by the module
%% which has the given attributes.
%% The result contains `{Mod, N}' pairs, where `Mod' is a module name.
%% Each module is contained only once.
%% If `Add' is `w', then `{Mod, N}' means that `Mod' is called `N' times. If
%% `Add' is `1', `{Mod, N}' means that `Mod' is called from `N' different
%% function.
%%
%% Examples:
%% ```
%% > cl_distfun:modcall_cnt([{#fun_attr{mod=a,name=f,arity=0}, 3},
%%                           {#fun_attr{mod=a,name=g,arity=0}, 4},
%%                           {#fun_attr{mod=b,name=f,arity=0}, 5}], w).
%% [{a,7}, {b,5}]
%%
%% > cl_distfun:modcall_cnt([{#fun_attr{mod=a,name=f,arity=0}, 3},
%%                           {#fun_attr{mod=a,name=g,arity=0}, 4},
%%                           {#fun_attr{mod=b,name=f,arity=0}, 5}], 1).
%% [{a,2}, {b,1}]
%% '''

%% Note, that we use element(1,W) to recover function call value!
%% It is neccesary, because ?Matrix:get_from_value is not a valid guard.
modcall_cnt(Attr,Add) ->
    D = lists:foldl(
          fun ({M,W},D2) ->
              case ?Matrix:get_from_value(W, func) of
                  0 -> D2;
                  _ -> 
                  Plus = case Add of 
                             w -> ?Matrix:get_from_value(W, func); 
                             1 -> 1 
                         end,
                  case dict:is_key(M,D2) of
                      true ->
                          dict:store(M,dict:fetch(M,D2)+Plus,D2);
                      false ->
                          dict:store(M,Plus,D2)
                  end
              end
          end,
          dict:new(),
          Attr),
    dict:to_list(D).

%% Generic distance function for function calls
calls(Ent1, ModCall1, Ent2, ModCall2) ->
    Call12 = proplists:get_value(Ent1, ModCall2, 0),
    Call21 = proplists:get_value(Ent2, ModCall1, 0),
    Similar = lists:sum([min(W, proplists:get_value(M, ModCall2, 0)) ||
                            {M, W} <- ModCall1]),
    lexgr_dist([min(Call12, Call21), Call12 + Call21, Similar]).

min(A, B) when A < B -> A;
min(_, B) -> B.
%max(A, B) when A > B -> A;
%max(_, B) -> B.

%% @private
lexgr_dist([]) -> 0;
lexgr_dist([Head | Tail]) ->
    1 / (2 + Head - lexgr_dist(Tail)).

%% @spec pow_size_fun_gen(number()) -> size_fun()
%%
%% @doc Returns a function that can calculate some kind of antigravity between
%% two clusters. `S1' and `S2' are the sizes of the clusters.
%% `PowWeight' is the strength of the antigravity. If it is 0, there is no
%% antigravity. If it is 1, it is a strong antigravity. (A negative `PowWeight'
%% is gravity, a `PowWeight' greater than 1 is a very strong one.)
pow_size_fun_gen(PowWeight) ->
    fun(S1,S2) ->
            math:pow(S1+S2,PowWeight)
    end.

%% @spec weight_gen(size_fun()) -> dist_fun()
%%
%% @doc Generates a distance function.
%% The distance function is based on function call structure and record usage,
%% and it is weighted by the number calculated by `SizeFun'.
%% It ignores call weights, only the count of different functions is used.
weight_gen(SizeFun) ->
    fun (Mod1, Attr1, Mod2, Attr2) ->
            DistVector = weight_distvec(Mod1, Attr1, Mod2, Attr2),
            Size1 = length(
                [M || {M,W} <- Attr1, ?Matrix:get_from_value(W,func) > 0 
                                      orelse 
                                      length(?Matrix:get_from_value(W,rec))>0]),
            Size2 = length(
                [M || {M,W} <- Attr2, ?Matrix:get_from_value(W,func) > 0 
                                      orelse 
                                      length(?Matrix:get_from_value(W,rec))>0]),
            %?d({Mod1, Mod2, lexgr_dist(DistVector), SizeFun(Size1, Size2)}),
            lexgr_dist(DistVector) * SizeFun(Size1,Size2)
    end.

%% @private
weight_distvec(_Mod1, Attr1, _Mod2, Attr2) ->
    ModCall1 = modcall_cnt(Attr1,w),
    ModCall2 = modcall_cnt(Attr2,w),
    Call12 = modcall2_cnt(
        [M || {M,W} <- Attr2, ?Matrix:get_from_value(W,func) > 0], ModCall1),
    Call21 = modcall2_cnt(
        [M || {M,W} <- Attr1, ?Matrix:get_from_value(W,func) > 0], ModCall2),
    SameRec = length(?Matrix:get_from_value(proplists:get_value(_Mod2, 
                                                         Attr1, 
                                                         ?Matrix:get_default()),
                                                         rec)),
    Similar = lists:sum(
        [min(W, proplists:get_value(M, ModCall2, 0)) ||
                            {M, W} <- ModCall1]),
    %?d({min(Call12, Call21), SameRec, Call12 + Call21, Similar}),
    [min(Call12, Call21), SameRec, Call12 + Call21, Similar].

%% @doc Returns how heavily are the modules in `Modules' called by the module
%% from which ModCall was created.
modcall2_cnt(Modules,ModCall) ->
    ModuleSet = sets:from_list(Modules),
    lists:foldl(
      fun ({M,W},N) -> % my module calls module M
              case sets:is_element(M,ModuleSet) of
                  true -> N+W;
                  false -> N
              end;
          (_,N) ->
              N
      end,
      0,
      ModCall).

%% @spec call_fun_cnt(#fun_attr{}, [{Attr, Weight}], #fun_attr{},
%%           [{Attr, Weight}]) -> Distance
%%
%%           Attr = #fun_attr{} | #rec_attr{}
%%
%% @doc Distance function based on function call structure and record usage
%% to be used with function clustering.
call_fun_cnt(Fun1, Attr1, Fun2, Attr2) ->
    Call12 = proplists:get_value(Fun1, Attr2, 0), % Bad: Fun1 column is missing except
    Call21 = proplists:get_value(Fun2, Attr1, 0), % if Fun1 is function not group/cluster
    Rec1 = used_records(Attr1),
    Rec2 = used_records(Attr2),
    SameRec = length(ordsets:intersection(Rec1, Rec2)),
    %Similar = lists:sum([min(W, proplists:get_value(F, Attr2, 0)) ||
    %                        {F=#fun_attr{}, W} <- Attr1]),
    lexgr_dist([min(Call12, Call21), SameRec, Call12 + Call21]). %, Similar]).

%% @spec call_fun_wgt(float()) -> dist_fun()
%% @doc Generate a distance function from the given Weight with the
%%      result of `pow_size_fun_gen(Weight)'
call_fun_wgt(Weight) ->
    SizeFun = pow_size_fun_gen(Weight),
    fun (F1, Attr1, F2, Attr2) ->
            Size1 = proplists:get_value(size, Attr1, 1),
            Size2 = proplists:get_value(size, Attr2, 1),
            call_fun_cnt(F1, Attr1, F2, Attr2) * SizeFun(Size1,Size2)
    end.


%% @spec generate_antigravity(DistFun::dist_fun(), AntigravityFun::size_fun())
%%           -> NewDistFun::dist_fun()
%% @doc  Generate a distance function from `DistFun' with the `AntigravityFun'
%%       as antigravity factor.
generate_antigravity(DistFun, SizeFun) ->
    fun(Fun1, Attr1, Fun2, Attr2) ->
        Size1 = proplists:get_value(size, Attr1, 1),
        Size2 = proplists:get_value(size, Attr2, 1),
        DistFun(Fun1, Attr1, Fun2, Attr2) * SizeFun(Size1,Size2)
    end.


%% @spec generate_fun_common_refs(Weight::float()) -> DistanceFun::dist_fun()
%% @doc  Generate a distance function from {@link fun_common_refs} with the
%%       result of `pow_size_fun_gen(Weight)' as antigravity factor.
%% @see  pow_size_fun_gen/1
generate_fun_common_refs(Weight) ->
    generate_antigravity(fun fun_common_refs/4, pow_size_fun_gen(Weight)).


%% @spec fun_common_refs(_Fun1::entity(), Attr1::attrLst(),
%%                       _Fun2::entity(), Attr2::attrLst()) -> Distance::float()
%% @doc  Distance of two group/cluster from a clasterings. `Fun1' and `Fun2' are
%%       identifiers of clusters. `Attr1' and `Attr2' are rows from attribute
%%       matrix of clustering that belong to the identifiers.
fun_common_refs(_Fun1, Attr1, _Fun2, Attr2) ->
    % Entities of groups
    Entities1 = get_value(entities, Attr1, []),
    Entities2 = get_value(entities, Attr2, []),
    % Number of calls from the other group
    Call1from2 = count_objects_refs(Entities1, Attr2),
    Call2from1 = count_objects_refs(Entities2, Attr1),
    % Number of common used records
    Recs1 = used_records(Attr1),
    Recs2 = used_records(Attr2),
    CommonRecCnt = length(Recs1--(Recs1--Recs2)),
    % Number of common used functions
    CommonFunCnt = count_objects_refs1([F||{F=#fun_attr{},W}<-Attr1,W>0],Attr2),
    %CommonFunCnt = [min(W,get_value(F,Attr2,0))||{F=#fun_attr{},W}<-Attr1,W>0],
    % Number of common used macros
    % ---
    % Calc distance with the following priority:
    lexgr_dist([min(Call1from2,Call2from1), CommonRecCnt, Call1from2+Call2from1,
                CommonFunCnt]).


%% @spec count_objects_refs(Objects::[Object],
%%               RefList::[{Object, UsageCount::integer()}]) ->
%%           TotalUsageCount::integer()
%%       Object = term()
%% @doc  Count usages of the `Objects' in `RefList'. Wrapper function to
%%       {@link count_objects_refs/3} with `fun(A,B) -> A+B end' as `AddFun'.
%% @see  count_objects_refs/3
count_objects_refs(Objects, RefList) ->
    count_objects_refs(Objects, RefList, fun(A, B) -> A+B end).

%% @spec count_objects_refs1(Objects::[Object],
%%               RefList::[{Object, UsageCount::integer()}]) ->
%%           DiffObjUsageCount::integer()
%%       Object = term()
%% @doc  Count how many different objects from `Objects' are used in
%%       `RefList'. Wrapper function to {@link count_objects_refs/3} with
%%       `fun(A,0)->A; (A,B) when B<0 -> A-1; (A,B)->A+1 end' as `AddFun'.
%%       The number of usage of an object is irrelevant. Every object usage
%%       counted as one usage.
%% @see  count_objects_refs/3
count_objects_refs1(Objects, RefList) ->
    count_objects_refs(Objects, RefList,
                       fun(A,0)->A; (A,B) when B<0 -> A-1; (A,_B)->A+1 end).

%% @spec count_objects_refs(Objects::[Object],
%%               RefList::[{Object, UsageCount::integer()}], AddFun) ->
%%           TotalUsageCount::integer()
%%       Object = term()
%%       AddFun = ((OpLeft::integer(), OpRight::integer()) -> integer())
%% @doc  Count usages of the `Objects' in `RefList'. `Object' contain several
%%       object. The elements in `RefList' describe an how many times an object
%%       is used.
%%       The `AddFun' summation two integer. If the number of usage is
%%       irrelevant than you can use the `fun(A,_) -> A+1 end' function to count
%%       how many object are used from the `Object'.
count_objects_refs(Objects, RefList, AddFun) ->
    lists:foldl(
        fun(Object, Count) -> AddFun(Count, get_value(Object, RefList, 0)) end,
        0, Objects).

%% @private
used_records(Attr) ->
    lists:usort([R || {R, W} <- Attr, W>0]).

