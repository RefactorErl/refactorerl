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

%%% @doc

%%% == Implementation status ==
%%% This feature is _not_ fully implemented.

%%% @author Gabor Olah <olikas.g@gmail.com>

-module(refusr_spec_typer).

-vsn("$Rev: 9568 $"). %for emacs"

-include("user.hrl").

-export([solve/2, genspec/3]).
%-export([t_union_test/0]).

%TODO only to avoid warnings
-export([get_type/2, t_none/0, t_int/0, t_int/1, t_float/0, t_float/1,
         t_number/0, t_atom/0, t_atom/1, t_tuple/0, t_tuple/1, t_nil/0,
         t_list/0, t_list/1, t_funsig/2, t_union/1, is_comparable_types/2,
         partition/2]).

-record(type, {kind, 
               value = 'none'}).

-record(funsigvalue, {arity,
                      args,
                      retval}).

-record(state, {env,
                cons}).

-define(UNION_MAX_LENGTH, 10).


%%% ===========================================================================
%%% Constraint generation
%%% ===========================================================================

genspec(Module, Name, Arity) ->
    [Func] = ?Query:exec(?Query:exec(?Mod:find(Module)),
                         ?Mod:local(Name, Arity)),
    consgen(new_env(), Func).

consgen(Env, Func) ->
    Clauses = ?Query:exec(Func, ?Query:seq([?Fun:definition(),
                                            ?Form:clauses()])),
    % TODO Is the function recursive?
    case Clauses of
        [] ->
            % Function has no definition
            % TODO Determine the arity of the function
            % TODO Generate "blank" funsig type
            no_body;
        _ ->
            [cons_gen(Env, Func, C, clause) || C <- Clauses]
    end.

cons_gen(Env, Func, Clause, clause) ->
    Patterns = ?Query:exec(Clause, ?Clause:patterns()),
    _Guards   = ?Query:exec(Clause, ?Clause:guard()),
    _Body     = ?Query:exec(Clause, ?Clause:body()),
    [ cons_gen(Env, Func, P, ?Expr:type(P))  || P <- Patterns];

cons_gen(Env, _Func, Expr, variable) ->
    [_Var]  = ?Query:exec(Expr, ?Expr:varbinds()),
    NewEnv = add_env(Env, Expr, t_any()),
    #state{env = NewEnv, cons = new_conset()}.













%% ----------------------------------------------------------------------------
%% Constraint set helper

new_conset() ->
    ordsets:new().


%% ----------------------------------------------------------------------------
%% Semantic environment helpers

new_env() ->
    dict:new().

add_env(Env, Node, Type) ->
    dict:store(Node, Type, Env).

get_type(Env, Node) ->
    try
        dict:fetch(Node, Env)
    catch
        Error:Reason -> {Error, Reason}
    end.




%%% ===========================================================================
%%% Constraint set solver
%%% ===========================================================================



% %spec solve(Solution(), ConstraintSet()) -> Solution()
% ConstraintSet() = {subt, {Cons(), Cons()} } | 
%                   {conj, [ConstraintSet()] } |
%                   {disj, [ConstraintSet()] } 
%
% Solution()      = set({TypeVariable(), Type()})
%
% TypeVariable() = {Node(), isRecursive()}
%
% Cons() = TypeVariable() | Type()


solve(bot, _) ->
    bot;
solve(Sol, {subt, {Alpha, Beta}}) ->
    SA = sol(Alpha),
    SB = sol(Beta),
    case is_subtype(SA, SB) of
        true ->
            Sol;
        false ->
            case match(SA, SB) of
                #type{kind='none',value='none'} ->
                    bot;
                T        ->
                    substitute(Sol, Alpha, T)
            end
    end;    
solve(Sol, {conj, Conj}) ->
    SolCResult = solve_conj(Sol, Conj),
    case SolCResult == Sol of
        true -> Sol;
        false -> solve(SolCResult,{conj, Conj})
    end;
solve(Sol, {disj, Disj}) ->
    case [S || S<-[solve(Sol,C) || C<-Disj], S =/= bot] of
        []   -> bot;
        Sol2 -> upper_bound(Sol2)
    end.


solve_conj(bot, _) -> bot;
solve_conj(Sol, [C]) -> solve(Sol,C);
solve_conj(Sol, [C1|Conj]) -> solve_conj(solve(Sol,C1),Conj).

% determines whether a concrete type is subtype of an other.
% true if T1 is subtype of T2 
is_subtype(_T1, _T2) ->
    not_implemented.

% maps T to concrete type
sol(_T) ->
    not_implemented.

% intersects T1 with T2 resulting a type that is "the biggest common devisor"
match(_T1, _T2) ->
    not_implemented.

% change TV with T in Sol
substitute(_Sol, _TV, _T) ->
    not_implemented.

% pointwise leas upper bound, i.e. the union of all types in Sol.
upper_bound(_Sol) ->
    not_implemented.




%%% ======================================================================
%%% Type constructor functions
%%% ======================================================================

t_none() ->
    #type{ kind   = 'none', 
           value  = 'none'
         }.

t_any() ->
    #type{ kind  = 'any',
           value = 'any'
         }.

t_int() ->
    #type{ kind   = 'int',
           value  = 'any'
         }.

t_int(N) when is_integer(N) ->
    #type{ kind   = 'int',
           value  = N
         }.

t_float() ->
    #type{ kind   = 'float',
           value  = 'any'
         }.

t_float(F) when is_float(F) ->
    #type{ kind   = 'float',
           value  = 'any'
         }.

t_number() ->
    #type{ kind  = 'number',
           value = 'any'
         }.

t_atom() ->
    #type{ kind   = 'atom',
           value  = 'any'
         }.

t_atom(A) when is_atom(A) ->
    #type{ kind   = 'atom',
           value  = A
         }.

t_tuple() ->
    #type{ kind   = 'tuple',
           value  = 'any'
         }.

t_tuple(TypeList) when is_list(TypeList) ->
    #type{ kind   = 'tuple',
           value  = TypeList
         }.

t_nil() ->
    #type{ kind  = 'list',
           value = 'nil'
         }.

t_list() ->
    #type{ kind  = 'list',
           value = 'any'
         }.

t_list(T) when is_record(T, type) ->
    #type{ kind  = 'list',
           value = T
         }.

t_funsig(Args, RetValue) when is_list(Args), is_record(RetValue, type) ->
    #type{ kind  = 'funsig',
           value = #funsigvalue{ arity = length(RetValue),
                                 args  = RetValue,
                                 retval = RetValue
                               }
         }.

%% Union types can only be created by union type constructor, hence
%% elements of Types can be one level deep union.

t_union(Types) when is_list(Types) ->
    UnionTypes = lists:flatten([ T#type.value || T <- Types, T#type.kind == 'union']),
    NonUnionTypes = [T || T <- Types, T#type.kind =/= 'union'],
    UniqTypes = lists:usort(UnionTypes ++ NonUnionTypes),
    
    EquivTypes = partition(fun is_comparable_types/2, UniqTypes),
    MaxTypes = [ hd(lists:sort(fun(A,B) -> is_subtype(B,A) end,T)) || T <- EquivTypes ],
    Res = case length(MaxTypes) > ?UNION_MAX_LENGTH of
                true -> 
                    t_any();
                false ->
                    MaxTypes
          end,

        #type{ kind  = 'union',
           value = Res
         }.


%TODO numbers
is_comparable_types(#type{kind='any'}, _) ->
    true;
is_comparable_types(_,                 #type{kind='any'}) ->
    true;
is_comparable_types(#type{kind=K},     #type{kind=K}) ->
    true;
is_comparable_types(_T1,               _T2) ->
    false.
            



%#type{ kind  =
%           value =
%         }.


%%% ==========================================================================
%%% Utility functions
%%% ==========================================================================

%% Makes classifiction on the list according to Pred
%% @spec partition(Pred, List) -> ListList
%%
%%      Pred = fun((Item, Item) -> boolean())
%%      List = [term()]
%%      ListList = [[term()]]
%% @end
partition(_Pred, []) ->
    [];
partition(Pred, [H|_T] = L) ->
    {L1, L2} = lists:partition(fun(E) -> Pred(H,E) end, L),
    [L1| partition(Pred, L2)].




