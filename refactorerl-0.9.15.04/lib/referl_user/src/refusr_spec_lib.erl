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
-module(refusr_spec_lib).

-vsn("$Rev: 9568 $"). %for emacs"

-include("user.hrl").

-include("spec.hrl").




-export([specs/0, typexp_type/1, typexp_tag/1, all_children/1]).

-export([t_none/0, t_int/0, t_int/1, t_float/0, t_float/1,
         t_number/0, t_atom/0, t_atom/1, t_tuple/0, t_tuple/1, t_nil/0,
         t_list/0, t_list/1, t_funsig/2, t_union/1, t_record/0, t_record/1,
         t_record/2, t_any/0, t_bool/0, t_char/1, t_char/0, t_byte/0, t_byte/1,
         t_bitstring/0, t_binary/0, t_ref/0, t_pid/0, t_port/0, t_identifier/0,
         t_function/0, t_node/0, t_node/1]).

-export([is_subtype/2, is_comparable_types/2]).

-export([init_ets/0, delete_ets/0, tv/0, get_new_env/0, get_new_env/1,
         lookup_var/2, get_func_type/1, update_func_type/2,
         add_var_to_table/2, list_elements/1]).

 %%-export([finalize/2]).

-export([type_of_infix/1, type_of_prefix/1]).

-export([partition/2, zipN/1]).

%% @todo: move these functions to their proper place

%% to reflib_file:
specs() ->
    ?Query:seq([file],[{form, {type, '==', spec}}]).
%% to reflib_typexp:
typexp_type(Node) ->
    (?Graph:data(Node))#typexp.type.
typexp_tag(Node) ->
    (?Graph:data(Node))#typexp.tag.

%% @doc This function is needed, because andalso and orelse declares a
%% new clause due to short-cut semantics.
all_children(Expr) ->
    Expr_value = ?Expr:value(Expr),
    if
        (Expr_value == 'andalso') or (Expr_value == 'orelse') ->
            ?Query:exec(Expr, ?Query:seq([exprcl], ?Clause:body()));
        true ->
            ?Query:exec(Expr, ?Expr:children())
    end.


%% =========================================================================
%% =========================================================================
%% Handling type environments

init_ets() ->
    dets:open_file(specs, [{auto_save, 1000}, {type, set}]).
    %% case ets:info(specs) of
    %%     undefined ->
    %%         ets:new(specs, [named_table, public]);
    %%     _ ->
    %%         ok
    %% end.

delete_ets() ->
    dets:close(specs).

tv() ->
    tv:start().

get_new_env() ->
    ets:new(vars, []).

get_new_env(OldEnv) ->
    NewEnv = get_new_env(),
    ets:insert(NewEnv, ets:tab2list(OldEnv)),
    NewEnv.

lookup_var(Env, Var) ->
    case ets:lookup(Env, Var) of
        [] ->
            t_any();
        [{Var, Result}] ->
            Result
    end.

get_func_type(Func) ->
    case dets:lookup(specs, Func) of
        [] ->
            update_func_type(Func, generate_dummy_type_for_func(Func, t_none())),
            refusr_spec:gen_spec(Func);
        [{Func, Result}] ->
            Result
    end.
%fixme
generate_dummy_type_for_func(Func, Type) ->
    FuncArity = ?Fun:arity(Func),
    FuncArgs = [ Type || _S <- lists:seq(1, FuncArity) ],
    FuncRet = Type,
    ?SLIB:t_funsig(FuncArgs, FuncRet).

update_func_type(Func, NewFunSig) ->
    case dets:lookup(specs, Func) of
        [] ->
            dets:insert(specs, {Func, NewFunSig}),
            NewFunSig;
        [{Func, _FunSig}] ->
            dets:insert(specs, {Func, NewFunSig}),
            NewFunSig
    end.


add_var_to_table(Env, V) ->
    case ets:lookup(Env,V) of
        [] ->
            ets:insert(Env, {V, t_any()});
        _ ->
            ok
    end.


list_elements([Child|ChildrenList]) ->
    case ?Expr:type(Child) of
        list ->
            ?Query:exec(Child, ?Expr:children()) ++ list_elements(ChildrenList);
        cons ->
            list_elements(?Query:exec(Child, ?Expr:children()) ++ ChildrenList);
        _ ->
            [Child]
    end;
list_elements([]) ->
    [].








%%% ===========================================================================
%%%  Subtyping
%%% ===========================================================================


is_subtype(#type{kind='any'}, #type{kind='any'}) ->
    #st_state{success='true', type=t_any()};
is_subtype(A                , #type{kind='any'}) ->
    #st_state{success='true', type=A};
is_subtype(#type{kind='any'}, B) ->
    #st_state{success='true', type=B};
is_subtype(#type{kind=AK, value='any'}, #type{kind=AK, value=_BV} = B) ->
    #st_state{success='true', type=B};
is_subtype(#type{kind='none'}, _B) ->
    #st_state{success='true', type=t_none()};
is_subtype(A, A) ->
    #st_state{success='true', type=A};
% ---- variable
% It is needed in union creation
% Should be reviewed
is_subtype(#type{kind='variable', value=_AV} = A, _B) ->
    #st_state{success='false', type=A};
% ---- tuple
is_subtype(#type{kind='tuple', value=_AV} = A, #type{kind='tuple', value='any'}) ->
    #st_state{success='true', type=A};
is_subtype(#type{kind='tuple', value=AV} = A, #type{kind='tuple', value=BV}) ->
    case length(AV) == length(BV) of
        true ->
            L = lists:zip(AV, BV),
            R = lists:all(fun({X, Y}) ->
                                  (is_subtype(X, Y))#st_state.success
                          end, L),
            case R of
                true ->
                    #st_state{success='true', type=A};
                false ->
                    #st_state{success='false', type=t_none()}
            end;
        false ->
            #st_state{success='false', type=t_none()}
    end;
% ---- list
is_subtype(#type{kind='list', value='nil'}, #type{kind='list', value='any'}) ->
    #st_state{success='true', type=t_list()};
is_subtype(#type{kind='list', value=_AV} = A, #type{kind='list', value='any'}) ->
    #st_state{success='true', type=A};
is_subtype(#type{kind='list', value=AV}, #type{kind='list', value=BV}) ->
    case is_subtype(AV, BV) of
        #st_state{success='true', type=V} ->
            #st_state{success='true', type=t_list(V)};
        _ ->
            #st_state{success='false', type=t_none()}
    end;
% ---- funsig
is_subtype(#type{kind='funsig', value=_AV} = A, #type{kind='funsig', value='any'}) ->
    #st_state{success='true', type=A};
is_subtype(#type{kind='funsig', value=AV}, #type{kind='funsig', value=BV}) ->
    case AV#funsigvalue.arity =/= BV#funsigvalue.arity of
        true ->
            #st_state{success='false', type=t_none()};
        false ->
            ArgPairs = lists:zip(AV#funsigvalue.args, BV#funsigvalue.args),
            ArgSTList = [ is_subtype(X,Y) || {X,Y} <- ArgPairs ],
            ArgsSuccess = lists:all(fun(T) ->
                                            T#st_state.success
                                    end, ArgSTList),
            % contra-variance
            RetST = is_subtype(BV#funsigvalue.retval,
                               AV#funsigvalue.retval),
            case ArgsSuccess and RetST#st_state.success of
                true ->
                    NewArgs = [ T#st_state.type || T <- ArgSTList ],
                    #st_state{success='true',
                              type=t_funsig(NewArgs, RetST#st_state.type)};
                false ->
                    #st_state{success='false', type=t_none()}
            end
    end;
% ---- union
is_subtype(#type{kind='union', value=AV}, #type{kind='union', value=_BV} = B) ->
    case [ (is_subtype(T, B))#st_state.type || T <- AV ] of
        [] ->
            #st_state{success='false', type=t_none()};
        [R] ->
            #st_state{success='true', type=R};
        L ->
            #st_state{success='true', type=t_union(L)}
    end;
is_subtype(A, #type{kind='union', value=BV}) when A#type.kind =/= 'union' ->
    L = lists:filter(fun(T) ->
                             (is_subtype(A, T))#st_state.success
                     end, BV),
    case L of
        [] ->
            #st_state{success='false', type=t_none()};
        [R] ->
            #st_state{success='true', type=R};
        L ->
            #st_state{success='true', type=t_union(L)}
    end;
is_subtype(#type{kind='union', value=_AV} = A, B) ->
    is_subtype(B, A);
% ---- other cases
is_subtype(#type{kind=_T, value=_V} = A, #type{kind=_T, value='any'}) ->
    #st_state{success='true', type=A};
is_subtype(_A, _B) ->
    % ?d({A,B}),
    #st_state{success='false', type=t_none()}.



is_comparable_types(A,B) ->
    (is_subtype(A,B))#st_state.success
        or
          (is_subtype(B,A))#st_state.success.


%%% ======================================================================
%%% Type constructor functions
%%% ======================================================================

t_none() ->
    #type{ kind  = 'none',
           value = 'none'
         }.

t_any() ->
    #type{ kind  = 'any',
           value = 'any'
         }.

t_int() ->
    #type{ kind  = 'int',
           value = 'any'
         }.

t_int(N) when is_integer(N) ->
    #type{ kind  = 'int',
           value = N
         }.

t_char() ->
    t_int().

t_char(N) ->
    t_int(N).

t_byte() ->
    t_int().

t_byte(N) ->
    t_int(N).

t_bitstring() ->
    #type{ kind  = 'bitstring',
           value = 'any'
         }.

t_binary() ->
    #type{ kind  = 'binary',
           value = 'any'
         }.

t_ref() ->
    #type{ kind  = 'ref',
           value = 'any'
         }.

t_pid() ->
    #type{ kind  = 'pid',
           value = 'any'
         }.

t_port() ->
    #type{ kind  = 'port',
           value = 'any'
         }.

t_node() ->
    #type{ kind  = 'atom',
           value = 'any' }.

t_node(Name) ->
    #type{ kind  = 'atom',
           value = Name }.

t_identifier() ->
    t_union([t_pid(), t_port(), t_ref()]).



t_float() ->
    #type{ kind  = 'float',
           value = 'any'
         }.

t_float(F) when is_float(F) ->
    #type{ kind  = 'float',
           value = 'any'
         }.

t_number() ->
    t_union([t_int(), t_float()]).

t_atom() ->
    #type{ kind  = 'atom',
           value = 'any'
         }.

t_atom(A) when is_atom(A) ->
    #type{ kind  = 'atom',
           value = A
         }.

t_bool() ->
    t_union([t_atom('true'), t_atom('false')]).

t_tuple() ->
    #type{ kind  = 'tuple',
           value = 'any'
         }.

t_tuple(TypeList) when is_list(TypeList) ->
    #type{ kind  = 'tuple',
           value = TypeList
         }.

t_nil() ->
    #type{ kind  = 'list',
           value = 'nil'
         }.

t_list() ->
    #type{ kind  = 'list',
           value = t_any() %'any'
         }.

t_list(nil) ->
    t_nil();
t_list(T) ->
    #type{ kind  = 'list',
           value = T
         }.

t_function() ->
    #type{ kind  = 'funsig',
           value = 'any'
         }.

t_funsig(Args, RetValue) when is_list(Args)  ->
    #type{ kind  = 'funsig',
           value = #funsigvalue{ arity  = length(Args),
                                 args   = Args,
                                 retval = RetValue
                               }
         }.

t_record() ->
    #type{ kind  = 'record',
           value = 'any'
         }.

t_record(RecordName) when is_atom(RecordName) ->
    #type{ kind  = 'record',
           value = #recvalue{name   = RecordName,
                             fields = 'any'
            }
         }.

t_record(RecordName, [] = _Fields) when is_atom(RecordName) ->
    t_record(RecordName);
t_record(RecordName, Fields) when is_atom(RecordName), is_list(Fields) ->
    #type{ kind  = 'record',
           value = #recvalue{name   = RecordName,
                             fields = Fields
            }
         }.


%% Union types can only be created by union type constructor, hence
%% elements of Types can be one level deep union.
t_union([]) ->
    t_none();
t_union([Type]) ->
    Type;
t_union(Types) when is_list(Types) ->
    UnionTypes = lists:flatten([ T#type.value
                                 || T <- Types, T#type.kind == 'union']),
    NonUnionTypes = [T || T <- Types, T#type.kind =/= 'union'],
    UniqTypes = lists:usort(UnionTypes ++ NonUnionTypes),
    EquivTypes = partition(fun is_comparable_types/2, UniqTypes),
                                                %?d(EquivTypes),
    MaxTypes = [ lists:last(lists:sort(fun(A,B) ->
                                               (is_subtype(A,B))#st_state.success
                                       end,T))
		 || T <- EquivTypes ],
                                                %?d(MaxTypes),
    case length(MaxTypes)  of
        N when N > ?UNION_MAX_LENGTH ->
            t_any();
        0 ->
            t_any(); %fixme
        1 ->
            [T] = MaxTypes,
            T;
        _ ->
            #type{ kind  = 'union',
                   value = MaxTypes
                 }
    end.









%%% =========================================================================
%%%  Finalize types by SOL
%%% =========================================================================

%%TODO incomplete

%% finalize(#type{kind='funsig',
%%                value=#funsigvalue{args=Args,
%%                                   retval=Retval}}, Sol) ->
%%     FinArgs = [finalize(A, Sol) || A <- Args],
%%     FinRetVal = finalize(Retval, Sol),
%%     t_funsig(FinArgs, FinRetVal);
%% finalize(Type, _Sol) when is_record(Type, type) ->
%%     Type;
%% finalize(?TV(Node), Sol) ->
%%     case ?SOL:get_type(Sol, Node) of
%%         {ok, Type} ->
%%             FinType = finalize(Type, Sol);
%%         {error, _} ->
%%             throw(bad_solution)
%%     end.








%% =========================================================================
%% =========================================================================
%% Built-in type information

%% INFIX OPERATORS

type_of_infix('+') ->
    {t_number(), t_number(), t_number()};
type_of_infix('-') ->
    {t_number(), t_number(), t_number()};
type_of_infix('*') ->
    {t_number(), t_number(), t_number()};
type_of_infix('/') ->
    {t_number(), t_number(), t_float()};
type_of_infix('div') ->
    {t_int(), t_int(), t_int()};
type_of_infix('rem') ->
    {t_int(), t_int(), t_int()};
type_of_infix('band') ->
    {t_int(), t_int(), t_int()};
type_of_infix('bor') ->
    {t_int(), t_int(), t_int()};
type_of_infix('bxor') ->
    {t_int(), t_int(), t_int()};
type_of_infix('bsl') ->
    {t_int(), t_int(), t_int()};
type_of_infix('bsr') ->
    {t_int(), t_int(), t_int()};
type_of_infix('and') ->
    {t_bool(), t_bool(), t_bool()};
type_of_infix(',') ->
    {t_bool(), t_bool(), t_bool()};
type_of_infix(';') ->
    {t_bool(), t_bool(), t_bool()};
type_of_infix('or') ->
    {t_bool(), t_bool(), t_bool()};
type_of_infix('xor') ->
    {t_bool(), t_bool(), t_bool()};
type_of_infix('orelse') ->
    {t_bool(), t_any(), t_any()};
type_of_infix('andalso') ->
    {t_bool(), t_any(), t_any()};
type_of_infix('++') ->
    {t_list(), t_list(), t_list()};
type_of_infix('!') ->
    {t_identifier(), t_any(), t_any()};
type_of_infix(_) ->
    {t_any(), t_any(), t_any()}.

%% =========================================================================
%% PREFIX OPERATORS
%% =========================================================================


type_of_prefix('+') ->
    {t_number(), t_number()};
type_of_prefix('-') ->
    {t_number(), t_number()};
type_of_prefix('bnot') ->
    {t_int(), t_int()};
type_of_prefix('not') ->
    {t_bool(), t_bool()};
type_of_prefix(_) ->
    {t_any(), t_any()}.





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

%% =========================================================================
%% like lists:zip/2, but it takes arbitrary number of list with the same
%% length and returns ordered n-th.

zipN(List) ->
    F = fun(N) ->
                lists:map(takeNth(N), List)
        end,
    lists:map(F, lists:seq(1,length(hd(List)))).

takeNth(N) ->
    fun(X) when is_list(X) ->
	    lists:nth(N, X)
    end.





