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
-module(refusr_spec).
-vsn("$Rev: 9568 $"). %for emacs"

-include_lib("stdlib/include/ms_transform.hrl").

-include("user.hrl").

-include("spec.hrl").

-export([run/1, run/3, run_tr/1]).

-export([init/0,
         recursion_level/0, recursion_level/1,
         analyse_ambigous_types/0, analyse_ambigous_types/1,
         analyse_types/0, analyse_types/1,
         analyse_specs/0, analyse_specs/1,
         use_stored_otp_specs/0, use_stored_otp_specs/1,
         use_existing_specs/0, use_existing_specs/1,
         verbose/0, verbose/1]).

-export([delete_spec/3, delete_spec/1, delete_all_specs/0]).

%% for debig purposoes only.
-export([gen_spec/3, gen_spec/1]).
-export([solve/2]).



%%% ===========================================================================
%%%  INTERFACE
%%% ===========================================================================

%%% ---------------------------------------------------------------------------
%%%  Configuration
%%% ---------------------------------------------------------------------------

init() ->
    init_option_table(),
    ?SLIB:init_ets().



recursion_level() ->
    get_opt_value(recursion_level).

recursion_level(N) when is_integer(N) andalso (N > 0) ->
    ets:insert(?OPTTABLE, {recursion_level, N}).


analyse_ambigous_types() ->
    get_opt_value(analyse_ambigous_types).

analyse_ambigous_types(Value) when is_boolean(Value) ->
    ets:insert(?OPTTABLE, {analyse_ambigous_types, Value}).


analyse_specs() ->
    get_opt_value(analyse_specs).

analyse_specs(Value) when is_boolean(Value) ->
    ets:insert(?OPTTABLE, {analyse_specs, Value}).


analyse_types() ->
    get_opt_value(analyse_types).

analyse_types(Value) when is_boolean(Value) ->
    ets:insert(?OPTTABLE, {analyse_types, Value}).


use_stored_otp_specs() ->
    get_opt_value(use_stored_otp_specs).

use_stored_otp_specs(Value) when is_boolean(Value) ->
    ets:insert(?OPTTABLE, {use_stored_otp_specs, Value}).


use_existing_specs() ->
    get_opt_value(use_existing_specs).

use_existing_specs(Value) when is_boolean(Value) ->
    ets:insert(?OPTTABLE, {use_existing_specs, Value}).


verbose() ->
    get_opt_value(verbose).

verbose(Value) when is_boolean(Value) ->
    ets:insert(?OPTTABLE, {verbose, Value}).



%%% ---------------------------------------------------------------------------
%%%  Configuration internals
%%% ---------------------------------------------------------------------------

init_option_table() ->
    case ets:info(?OPTTABLE) of
        undefined ->
            ets:new(?OPTTABLE, [named_table, public, set]),
            {ok, Options} = file:consult("lib/referl_user/src/refusr_spec.conf"),
            ets:insert(?OPTTABLE, Options);
        _ ->
            ok
    end.


get_opt_value(Value) ->
    case ets:lookup(?OPTTABLE, Value) of
        [{_Name, V}] ->
            V;
        Error ->
            Error
    end.


%%% ---------------------------------------------------------------------------
%%%  Specs handling
%%% ---------------------------------------------------------------------------

delete_spec(Module, Name, Arity) ->
    [Func] = ?Query:exec(?Query:exec(?Mod:find(Module)),
                         ?Mod:local(Name, Arity)),
    delete_spec(Func).

delete_spec(Func) ->
    dets:delete(specs, Func).


delete_all_specs() ->
    dets:delete_all_objects(specs).




run(Mod, Name, Arity) ->
    [Func] = ?Query:exec(?Query:exec(?Mod:find(Mod)),
                         ?Mod:local(Name, Arity)),
    run(Func).

run(Func) ->
    init(),
    Res = case {use_existing_specs(), dets:lookup(specs, Func)} of
              {true, [{_, Spec}]} ->
                  Spec;
              _ ->

                  SCCs = generate_sccs(Func),
                  case analyse_specs of
                      true ->
                          refusr_spec_analspec:analyzeSpecs();
                      _ ->
                          ok
                  end,
                  Specs = [gen_spec(F) || F <- SCCs],
                  lists:last(Specs)
          end,
    %% ?SLIB:delete_ets(),
    %?d(Res),
    {refusr_spec_pp:print({Func,Res}),nothing,nothig}.

run_tr(Func) ->
    init(),
    gen_spec(Func).




generate_sccs(Func) ->
    fcg([Func], []).


%% Fun Call Graph in a flatten list.
fcg([Func | Others], FCG) ->
    case lists:member(Func, FCG) of
        true ->
            fcg(Others, FCG);
        false ->
            Callees = ?Query:exec(Func, ?Fun:funcalls()),
            case Callees of
                [] ->
                    fcg(Others, [Func | FCG]);
                L ->
                    fcg(Others ++ L, [Func | FCG])
            end
    end;
fcg([], FCG) ->
    FCG.






%% =========================================================================
%% =========================================================================
%% generates specs for a fun.


gen_spec(Module, Name, Arity) ->
    [Func] = ?Query:exec(?Query:exec(?Mod:find(Module)),
                         ?Mod:local(Name, Arity)),
    gen_spec(Func).

gen_spec(Func) ->
    {_, {M,F,A}}= ?Fun:mod_fun_arity(Func),
    case verbose() of
        true ->
            io:format("Generating type information for ~p:~p/~p...~n",[M,F,A]);
        _ ->
            ok
    end,

    case {use_existing_specs(), dets:lookup(specs, Func)} of
        {true, [{_Func, Result}]} ->
            case verbose() of
                true ->
                    io:format("  Read from database.~n");
                _ ->
                    ok
            end,
            Result;
        _ ->
                                                %?d(Func),
            Clauses = ?Query:exec(Func, ?Query:seq([?Fun:definition(),
                                                    ?Form:clauses()])),
            Res = case Clauses of
                      [] ->
                          %% When there is no body for the function
                          generate_dummy_type_for_func(Func, ?SLIB:t_any());
                      [Cl] ->
                          Cs = generate_cons_cl(Cl),
                                                %?d(Cs),
                          Sol = ?SOL:update(?SOL:new(), Func, 'intersect',
                                            generate_dummy_type_for_func(Func, ?SLIB:t_none())),
                          iterate_single(Cs, Func, Cl, Sol, recursion_level());
                      Cls when is_list(Cls) ->
                          Css = [ generate_cons_cl(Cl) || Cl <- Cls ],
                          NewCs = ?CS:add(?CS:new(), disj, Css),
                                                %?d(NewCs),
                          Sol = ?SOL:update(?SOL:new(), Func, 'intersect',
                                            generate_dummy_type_for_func(Func, ?SLIB:t_none())),
                          iterate_mult(NewCs, Func, Cls, Sol, recursion_level())
                  end,
                                                % ?d(Func),
                                                % ?d(Res),
            ?SLIB:update_func_type(Func, Res)
    end.

iterate_single(Cs, Func, Cl, Sol, RecNum) ->
    OldSpec = ?SOL:get_type(Sol, Func),

    Sol2 = solve(Sol, Cs),

    NewSpec = case ?SOL:get_type(Sol2, Cl) of
                  {ok, Spec} ->
                      Spec;
                  {error, _X} ->
                      generate_dummy_type_for_func(Func, ?SLIB:t_none())
              end,
    Sol3 = ?SOL:update(Sol2, Func, 'intersect', NewSpec),
    case (OldSpec =:= NewSpec) orelse RecNum < 1 of
        true ->
            NewSpec;
        false ->
            iterate_single(Cs, Func, Cl, Sol3, RecNum - 1)
    end.

iterate_mult(Cs, Func, Cls, Sol, RecNum) ->
    OldSpec = ?SOL:get_type(Sol, Func),

    Sol2 = solve(Sol, Cs),
    %?d(Sol2),
    Types = [ case ?SOL:get_type(Sol2, Cl) of
                  {ok, Spec} -> Spec;
                  {error, _} ->  generate_dummy_type_for_func(Func, ?SLIB:t_none())
              end || Cl <- Cls ],
    Args = [ (T#type.value)#funsigvalue.args  || T <- Types ],
    GroupedArgs = ?SLIB:zipN(Args),
    %?d(GroupedArgs),
    NewArgs = [ ?SLIB:t_union(A) || A <- GroupedArgs ],
    RetTypes = [ (T#type.value)#funsigvalue.retval || T <- Types ],
    NewRetType = ?SLIB:t_union(RetTypes),

    NewSpec = ?SLIB:t_funsig(NewArgs, NewRetType),
    Sol3 = ?SOL:update(Sol2, Func, 'intersect', NewSpec),
    case (OldSpec =:= NewSpec) orelse RecNum < 1 of
        true ->
            NewSpec;
        false ->
            iterate_mult(Cs, Func, Cls, Sol3, RecNum - 1)
    end.


get_func_type(Func) ->
    ?SLIB:get_func_type(Func).

generate_dummy_type_for_func(Func, Type) ->
    FuncArity = ?Fun:arity(Func),
    FuncArgs = [ Type || _S <- lists:seq(1, FuncArity) ],
    FuncRet = Type,
    ?SLIB:t_funsig(FuncArgs, FuncRet).

generate_cons_cl(Cl) ->
    Patterns = ?Query:exec(Cl, ?Clause:patterns()),
    Guards   = ?Query:exec(Cl, ?Clause:guard()),
    Bodies   = ?Query:exec(Cl, ?Clause:body()),

    PatternRes = [ gen_cons_expr(P) || P <- Patterns ],
    GuardRes = [ gen_cons_guards(G) || G <- Guards ],
    BodyRes = [ gen_cons_expr(B) || B <- Bodies ],

    PTypes = [ P#cg_state.ret_type || P <- PatternRes ],
    %?d(BodyRes),
    RetType = (lists:last(BodyRes))#cg_state.ret_type,
    %?d(RetType),
    Fun = fun(C, Acc) -> ?CS:add(Acc, conj, C#cg_state.conset) end,
    PCons = lists:foldl(Fun, ?CS:new(), PatternRes),
    GCons = lists:foldl(Fun, ?CS:new(), GuardRes),
    BCons = lists:foldl(Fun, ?CS:new(), BodyRes),

    Cs1 = ?CS:add(?CS:new(), conj,
                 [{subt, ?TV(Cl), ?SLIB:t_funsig(PTypes, RetType)}]),
    Cs2 = ?CS:add(Cs1, conj, PCons),
    Cs3 = ?CS:add(Cs2, conj, GCons),
    _Cs4 = ?CS:add(Cs3, conj, BCons).


%% @doc Generate constraints for guard expressions
gen_cons_guards(Node) ->
    gcg(?Expr:type(Node), Node).


gcg([]) ->
    #cg_state{ret_type = ?SLIB:t_atom('true'),
              conset = ?CS:new()};
gcg([Node]) ->
    gcg(?Expr:type(Node), Node);
gcg(Node) ->
    gcg(?Expr:type(Node), Node).


gcg(infix_expr, Node) ->
    [Lhs, Rhs] = ?SLIB:all_children(Node),
    case ?Expr:value(Node) of
        Disj when (Disj =:= ';') or (Disj =:= 'or') ->
            LhsRes = gcg(Lhs),
            RhsRes = gcg(Rhs),
            Cs = ?CS:add(?CS:new(), disj, [LhsRes#cg_state.conset,
                                           RhsRes#cg_state.conset]),
            #cg_state{ret_type = ?SLIB:t_bool(),
                      conset = Cs};
        Conj when (Conj =:= ',') or (Conj =:= 'and') ->
            LhsRes = gcg(Lhs),
            RhsRes = gcg(Rhs),
            Cs = ?CS:add(?CS:new(), conj, [LhsRes#cg_state.conset,
                                           RhsRes#cg_state.conset]),
            #cg_state{ret_type = ?SLIB:t_bool(),
                      conset = Cs};
        _V ->
            ResLhs = gcg(Lhs),
            ResRhs = gcg(Rhs),
            {LT, RT, RetT} = ?SLIB:type_of_infix(?Expr:value(Node)),
            Cs1 = ?CS:add(?CS:new(), conj,
                          [{subt, ResLhs#cg_state.ret_type, LT},
                           {subt, ResRhs#cg_state.ret_type, RT}]),
            Cs2 = ?CS:add(Cs1, conj, ResLhs#cg_state.conset),
            FinalCons = ?CS:add(Cs2, conj, ResRhs#cg_state.conset),
            #cg_state{ret_type = RetT,
                      conset = FinalCons}
    end;
gcg(application, Node) ->
    [NameNode | ArgList] = ?Query:exec(Node, ?Expr:children()),
    Name = ?Expr:value(NameNode),
    gcgappl(Name, ?Query:exec(ArgList, ?Expr:children()));
gcg(prefix_expr, Node) ->
    [Ch] = ?Query:exec(Node, ?Expr:children()),
    Res = gcg(Ch),
    {T, RetT} = ?SLIB:type_of_prefix(?Expr:value(Node)),
    Cs1 = ?CS:add(?CS:new(), conj,
                  [{subt, Res#cg_state.ret_type, T}]),
    FinalCons = ?CS:add(Cs1, conj, Res#cg_state.conset),
    #cg_state{ret_type = RetT,
              conset = FinalCons};
gcg(parenthesis, Node) ->
    [N] = ?Query:exec(Node, ?Expr:children()),
    gcg(N);
gcg(variable, Node) ->
    VarNode = ?Query:exec(Node,
                          ?Query:any([varref], [varbind])),
    case VarNode of
        [] ->
            io:format("Error with variable ~p in guard! Maybe the source"
                      " has syntactic errors. ~n", [Node]),
            #cg_state{ret_type = ?SLIB:t_none(),
                      conset = ?CS:new()};
        [V] ->
            #cg_state{ret_type = ?TV(V),
                      conset = ?CS:new()}
    end;
gcg(_, _Node) ->
    %?d(Node),
    #cg_state{ret_type = ?SLIB:t_any(),
              conset = ?CS:new()}.


%%% ============================================================================
%%% Applications in Gouards
%%% ============================================================================

gcgappl('is_atom', [Node]) ->
    Res = gcg(Node),
    Cons = ?CS:add(?CS:new(), conj,
                   [{subt, Res#cg_state.ret_type, ?SLIB:t_atom()},
                    Res#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_bool(),
              conset = Cons};
gcgappl('is_binary', [Node]) ->
    Res = gcg(Node),
    Cons = ?CS:add(?CS:new(), conj,
                   [{subt, Res#cg_state.ret_type, ?SLIB:t_binary()},
                    Res#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_bool(),
              conset = Cons};
gcgappl('is_boolean', [Node]) ->
    Res = gcg(Node),
    Cons = ?CS:add(?CS:new(), conj,
                   [{subt, Res#cg_state.ret_type, ?SLIB:t_bool()},
                    Res#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_bool(),
              conset = Cons};
gcgappl('is_float', [Node]) ->
    Res = gcg(Node),
    Cons = ?CS:add(?CS:new(), conj,
                   [{subt, Res#cg_state.ret_type, ?SLIB:t_float()},
                    Res#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_bool(),
              conset = Cons};
gcgappl('is_function', [Node]) ->
    Res = gcg(Node),
    Cons = ?CS:add(?CS:new(), conj,
                   [{subt, Res#cg_state.ret_type, ?SLIB:t_function()},
                    Res#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_bool(),
              conset = Cons};
gcgappl('is_integer', [Node]) ->
    Res = gcg(Node),
    Cons = ?CS:add(?CS:new(), conj,
                   [{subt, Res#cg_state.ret_type, ?SLIB:t_int()},
                    Res#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_bool(),
              conset = Cons};
gcgappl('is_list', [Node]) ->
    Res = gcg(Node),
    Cons = ?CS:add(?CS:new(), conj,
                   [{subt, Res#cg_state.ret_type, ?SLIB:t_list()},
                    Res#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_bool(),
              conset = Cons};
gcgappl('is_number', [Node]) ->
    Res = gcg(Node),
    Cons = ?CS:add(?CS:new(), conj,
                   [{subt, Res#cg_state.ret_type, ?SLIB:t_number()},
                    Res#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_bool(),
              conset = Cons};
gcgappl('is_pid', [Node]) ->
    Res = gcg(Node),
    Cons = ?CS:add(?CS:new(), conj,
                   [{subt, Res#cg_state.ret_type, ?SLIB:t_pid()},
                    Res#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_bool(),
              conset = Cons};
gcgappl('is_port', [Node]) ->
    Res = gcg(Node),
    Cons = ?CS:add(?CS:new(), conj,
                   [{subt, Res#cg_state.ret_type, ?SLIB:t_port()},
                    Res#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_bool(),
              conset = Cons};
gcgappl('is_refernce', [Node]) ->
    Res = gcg(Node),
    Cons = ?CS:add(?CS:new(), conj,
                   [{subt, Res#cg_state.ret_type, ?SLIB:t_ref()},
                    Res#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_bool(),
              conset = Cons};
gcgappl('is_tuple', [Node]) ->
    Res = gcg(Node),
    Cons = ?CS:add(?CS:new(), conj,
                   [{subt, Res#cg_state.ret_type, ?SLIB:t_tuple()},
                    Res#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_bool(),
              conset = Cons};
gcgappl('is_bitsring', [Node]) ->
    Res = gcg(Node),
    Cons = ?CS:add(?CS:new(), conj,
                   [{subt, Res#cg_state.ret_type, ?SLIB:t_bitstring()},
                    Res#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_bool(),
              conset = Cons};
gcgappl('is_function', [TermNode, ArityNode]) ->
    ResTerm = gcg(TermNode),
    ResArity = gcg(ArityNode),
    Cons = ?CS:add(?CS:new(), conj,
                                                % TODO The arity of the function can be inferred
                                                % somehow so this case should be handled, too
                   [{subt, ResTerm#cg_state.ret_type, ?SLIB:t_function()},
                    {subt, ResArity#cg_state.ret_type, ?SLIB:t_int()},
                    ResTerm#cg_state.conset,
                    ResArity#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_bool(),
              conset = Cons};
%% TODO records should be handled
gcgappl('abs', [Node]) ->
    Res = gcg(Node),
    Cons = ?CS:add(?CS:new(), conj,
                   [{subt, Res#cg_state.ret_type, ?SLIB:t_number()},
                    Res#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_number(),
              conset = Cons};
gcgappl('bit_size', [Node]) ->
    Res = gcg(Node),
    Cons = ?CS:add(?CS:new(), conj,
                   [{subt, Res#cg_state.ret_type, ?SLIB:t_bitstring()},
                    Res#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_int(),
              conset = Cons};
gcgappl('byte_size', [Node]) ->
    Res = gcg(Node),
    Cons = ?CS:add(?CS:new(), conj,
                   [{subt, Res#cg_state.ret_type, ?SLIB:t_bitstring()},
                    Res#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_int(),
              conset = Cons};
gcgappl('element', [NNode, TupleNode]) ->
    ResN = gcg(NNode),
    ResTuple = gcg(TupleNode),
    Cons = ?CS:add(?CS:new(), conj,
                   [{subt, ResN#cg_state.ret_type, ?SLIB:t_int()},
                    {subt, ResTuple#cg_state.ret_type, ?SLIB:t_tuple()},
                    ResN#cg_state.conset,
                    ResTuple#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_any(),
              conset = Cons};

%% TODO Note that if used on the top-level in a guard, it will test
%% whether the argument is a floating point number; for clarity, use
%% is_float/1 instead.
%% When float/1 is used in an expression in a
%% guard, such as 'float(A) == 4.0', it converts a number as described
%% above.
gcgappl('float', [Node]) ->
    Res = gcg(Node),
    Cons = ?CS:add(?CS:new(), conj,
                   [{subt, Res#cg_state.ret_type, ?SLIB:t_number()},
                    Res#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_float(),
              conset = Cons};
gcgappl('hd', [Node]) ->
    Res = gcg(Node),
    Cons = ?CS:add(?CS:new(), conj,
                   [{subt, Res#cg_state.ret_type, ?SLIB:t_list()},
                    Res#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_any(),
              conset = Cons};
gcgappl('length', [Node]) ->
    Res = gcg(Node),
    Cons = ?CS:add(?CS:new(), conj,
                   [{subt, Res#cg_state.ret_type, ?SLIB:t_list()},
                    Res#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_int(),
              conset = Cons};
gcgappl('node', []) ->
    #cg_state{ret_type = ?SLIB:t_node(),
              conset = ?CS:new()};
gcgappl('node', [Node]) ->
    Res = gcg(Node),
    Cons = ?CS:add(?CS:new(), conj,
                   [{subt, Res#cg_state.ret_type, ?SLIB:t_identifier()},
                    Res#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_node(),
              conset = Cons};
gcgappl('round', [Node]) ->
    Res = gcg(Node),
    Cons = ?CS:add(?CS:new(), conj,
                   [{subt, Res#cg_state.ret_type, ?SLIB:t_number()},
                    Res#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_int(),
              conset = Cons};
gcgappl('self', []) ->
    #cg_state{ret_type = ?SLIB:t_pid(),
              conset = ?CS:new()};
gcgappl('size', [Node]) ->
    Res = gcg(Node),
    Cons = ?CS:add(?CS:new(), conj,
                   [{subt, Res#cg_state.ret_type,
                     ?SLIB:t_union([?SLIB:t_tuple(), ?SLIB:t_bitstring()])},
                    Res#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_int(),
              conset = Cons};
gcgappl('tl', [Node]) ->
    Res = gcg(Node),
    Cons = ?CS:add(?CS:new(), conj,
                   [{subt, Res#cg_state.ret_type, ?SLIB:t_list()},
                    Res#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_list(),
              conset = Cons};
gcgappl('trunc', [Node]) ->
    Res = gcg(Node),
    Cons = ?CS:add(?CS:new(), conj,
                   [{subt, Res#cg_state.ret_type, ?SLIB:t_number()},
                    Res#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_int(),
              conset = Cons};
gcgappl('tuple_size', [Node]) ->
    Res = gcg(Node),
    Cons = ?CS:add(?CS:new(), conj,
                   [{subt, Res#cg_state.ret_type, ?SLIB:t_tuple()},
                    Res#cg_state.conset]),
    #cg_state{ret_type = ?SLIB:t_int(),
              conset = Cons};
%%TODO error: this application is not allowed in guards
gcgappl(_Funname, _Arglist) ->
    % ?d(_Funname),
    % ?d(_Arglist),
    #cg_state{ret_type = ?SLIB:t_bool(),
              conset = ?CS:new()}.




%%% ========================================================================
%%%  SOLVE
%%% ========================================================================

%% @doc Solves the constraint set. The algorithm is based on
%% structural decomposition of the constraint set. Although the most
%% precise result could be reached by letting the algorithm reach its
%% fix point, there is a safety mechanism to ensure there are no
%% infinit recurion. The maximum recurion limit can be set by the
%% user, but the default is 15 iteration.
solve(Sol, CS) ->
    solve(Sol, CS, recursion_level()).

solve(bottom, _, _) ->
    ?SOL:bottom();
solve(Sol, _ , 0) ->
    Sol;
solve(Sol, {conj, Set}, RecNum) ->
    SolCResult = solve_conj(Sol, Set),
    case SolCResult =:= Sol of
        true ->
            Sol;
        false ->
            solve(SolCResult, {conj, Set}, RecNum - 1)  % !!! Recursion
    end;
solve(Sol, {disj, Set}, _RecNum) ->
    NewSols = [S || S <- [ solve(Sol, C) || C <- Set], S =/= bottom ],
    case NewSols of
        [] ->
            ?SOL:bottom();
        _ ->
            ?SOL:merge(NewSols)
    end;
%% ---- A <: B, where A is a type variable, B can be arbitrary
solve(Sol, {subt, ?TV(Alpha), Beta}, _RecNum) ->
    %?d(Alpha),
    %?d(Beta),
    {Sol1, SA} = map_to_concrete(Sol, ?TV(Alpha)),
    {Sol2, SB} = map_to_concrete(Sol1, Beta),
    %?d(SA), ?d(SB),
    StRet = ?SLIB:is_subtype(SA, SB),
    %?d(StRet),
    case StRet#st_state.success of
        true ->
            %?d(Sol2),
            Sol3 = ?SOL:update(Sol2, Alpha, 'intersect', StRet#st_state.type),
            %?d(Sol3),
            Sol3;
        false ->
            ?SOL:bottom()
    end;
%% ---- A <: B, where A is a concrete type, B can be arbitrary
solve(Sol, {subt, TAlpha, Beta}, _RecNum) ->
    %?d(TAlpha),
    %?d(Beta),
    {Sol1, SA} = map_to_concrete(Sol, TAlpha),
    {Sol2, SB} = map_to_concrete(Sol1, Beta),
    %?d(SA), ?d(SB),
    StRet = ?SLIB:is_subtype(SA, SB),
    %?d(StRet),
    case StRet#st_state.success of
        true ->
            %?d(Sol2),
            ?SOL:update(Sol2, TAlpha, intersect, SB);
        false ->
            ?SOL:bottom()
    end.

solve_conj(bottom, _) ->
    ?SOL:bottom();
solve_conj(Sol, []) ->
    Sol;
solve_conj(Sol, [C | Conj]) ->
    solve_conj( solve(Sol, C), Conj).       % !!! Recursion



%% TODO map_to_concrete should be rewritten and moved to ?SOL

map_to_concrete(Sol, ?TV(Node)) ->
    case ?SOL:get_type(Sol, Node) of
        {ok, T} ->
            map_to_concrete(Sol, T);
        {error, node_missing} ->
            case Node of
                {'$gn', func, _} ->
                    {Sol, get_func_type(Node)};
                _ ->
                    {?SOL:update(Sol, Node, 'intersect', ?SLIB:t_any()),
                     ?SLIB:t_any()}
            end
    end;
map_to_concrete(Sol, #type{kind='funsig',
                           value=#funsigvalue{args=Args,
                                              retval=RetVal}}) when is_list(Args) ->

    Fun = fun(T, AccSolIn) ->
                  {AccSolOut, NewT} = map_to_concrete(AccSolIn, T),
                  {NewT, AccSolOut}
          end,
    {FinArgs, Sol1} = lists:mapfoldl(Fun, Sol, Args),
    {Sol2, FinRetVal} = map_to_concrete(Sol1, RetVal),
    {Sol2, ?SLIB:t_funsig(FinArgs, FinRetVal)};
map_to_concrete(Sol, #type{kind='list', value=Type}) ->
    {NewSol, NewType} = map_to_concrete(Sol, Type),
    {NewSol, ?SLIB:t_list(NewType)};
map_to_concrete(Sol, #type{kind='tuple', value=Types}) when is_list(Types) ->
    Fun = fun(T, AccSolIn) ->
                  {AccSolOut, NewT} = map_to_concrete(AccSolIn, T),
                  {NewT, AccSolOut}
          end,
    {NewTypes, NewSol} = lists:mapfoldl(Fun, Sol, Types),
    {NewSol, ?SLIB:t_tuple(NewTypes)};
map_to_concrete(Sol, #type{kind='union', value=Types}) when is_list(Types)->
    Fun = fun(T, AccSolIn) ->
                  {AccSolOut, NewT} = map_to_concrete(AccSolIn, T),
                  {NewT, AccSolOut}
          end,
    {NewTypes, NewSol} = lists:mapfoldl(Fun, Sol, Types),
    {NewSol, ?SLIB:t_union(NewTypes)};
map_to_concrete(Sol, Type) ->
    {Sol, Type}.













%%% ============================================================================
%%% EXPRESSIONS
%%% ----------------------------------------------------------------------------

%% TODO Document gce

gen_cons_expr(Node) ->
    gce(?Expr:type(Node), Node).

gce(Node) ->
    gce(?Expr:type(Node), Node).

gce(integer, Node) ->
    #cg_state{ret_type = ?SLIB:t_int(?Expr:value(Node)),
              conset = ?CS:new()};
gce(float, Node) ->
    #cg_state{ret_type = ?SLIB:t_float(?Expr:value(Node)),
              conset = ?CS:new()};
gce(atom, Node) ->
    #cg_state{ret_type = ?SLIB:t_atom(?Expr:value(Node)),
              conset = ?CS:new()};
gce(string, _Node) ->
    #cg_state{ret_type = ?SLIB:t_list(?SLIB:t_int()),
              conset = ?CS:new()};
gce(joker, _Node) ->
    #cg_state{ret_type = ?SLIB:t_any(),
              conset = ?CS:new()};
gce(tuple, Node) ->
    Elements = ?Query:exec(Node, ?Expr:children()),
    Res = [ gce(E) || E <- Elements ],
    RetTypes = [ E#cg_state.ret_type || E <- Res ],
    RetCons = [ E#cg_state.conset || E <- Res ],
    FinalCons = ?CS:add(?CS:new(), conj, RetCons),
    #cg_state{ret_type = ?SLIB:t_tuple(RetTypes),
              conset = FinalCons};
%% TODO Different list types should be handled differently
gce(cons, Node) ->
    Elements = ?Query:exec(Node, ?Expr:children()),
    case Elements of
        [] ->
            #cg_state{ret_type = ?SLIB:t_nil(),
                      conset = ?CS:new()};
        _ ->
            FlatList = listElements(Elements),
            Res = [ gce(E) || E <- FlatList ],
            RetTypes = [ E#cg_state.ret_type || E <- Res ],
            RetUnion = ?SLIB:t_union(RetTypes),
            %% TODO cons union
            RetCons = [ E#cg_state.conset || E <- Res ],
            FinalCons = ?CS:add(?CS:new(), conj, RetCons),
            #cg_state{ret_type = ?SLIB:t_list(RetUnion),
                      conset = FinalCons}
    end;
gce(variable, Node) ->
    VarNode = ?Query:exec(Node,
                          ?Query:any([varref], [varbind])),
    case VarNode of
        [] ->
            io:format("Error with variable ~p! Maybe the source has syntactic errors. ~n", [Node]),
            #cg_state{ret_type = ?SLIB:t_none(),
                      conset = ?CS:new()};
        [V] ->
            #cg_state{ret_type = ?TV(V),
                      conset = ?CS:new()}
    end;
gce(match_expr, Node) ->
    [Lhs, Rhs] = ?Query:exec(Node, ?Expr:children()),
    ResLhs = gce(Lhs),
    ResRhs = gce(Rhs),
    Cs1 = ?CS:add(?CS:new(), conj,
                  [{subt, ResLhs#cg_state.ret_type, ResRhs#cg_state.ret_type},
                   {subt, ResRhs#cg_state.ret_type, ResLhs#cg_state.ret_type}]),
    Cs2 = ?CS:add(Cs1, conj, ResLhs#cg_state.conset),
    FinalCons = ?CS:add(Cs2, conj, ResRhs#cg_state.conset),
    #cg_state{ret_type = ResRhs#cg_state.ret_type,
              conset = FinalCons};
%% TODO Constraints for application head are missing
gce(application, Node) ->
    Funcs = ?Query:exec(Node, ?Expr:function()),
    case Funcs of
        [] ->
            #cg_state{ret_type = ?SLIB:t_any(),
                      conset = ?CS:new()};
        [Func] ->
            Arguments = ?Query:exec(
                           ?Query:exec(Node, ?Expr:child(2)),
                           ?Expr:children()),
            Res = [ gce(A) || A <- Arguments ],
            ArgTypes = [ E#cg_state.ret_type || E <- Res],
            ArgCons = [ E#cg_state.conset || E <- Res],

            FuncArity = ?Fun:arity(Func),
            FuncArgs = [ ?TV({Func, a, S}) || S <- lists:seq(1, FuncArity) ],
            FuncRetType = ?TV({Func, ret}),
            FunSpec = ?SLIB:t_funsig(FuncArgs, FuncRetType),

            ZippedArgs = lists:zip(ArgTypes, FuncArgs),

            Cs1 = ?CS:add(?CS:new(), conj,
                          [{subt, ?TV(Func), FunSpec},
                           {subt, FunSpec,   ?TV(Func)},
                           {subt, ?TV(Node), FuncRetType}]
                          ++ [ {subt, A, F} || {A, F} <- ZippedArgs ]),

            FinalCons = ?CS:add(Cs1, conj, ArgCons),
            #cg_state{ret_type = FuncRetType,
                      conset = FinalCons}
    end;
gce(parenthesis, Node) ->
    [N] = ?Query:exec(Node, ?Expr:children()),
    gce(N);
gce(case_expr, Node) ->
    [Head] = ?Query:exec(Node, ?Query:seq([headcl], ?Clause:body())),
    HeadRes = gce(Head),
    Clauses = ?Query:exec(Node, ?Expr:clauses()),
    PartsByClauses = [ { ?Query:exec(Cl, ?Clause:patterns()),
                         ?Query:exec(Cl, ?Clause:guard()),
                         ?Query:exec(Cl, ?Clause:body()) }
                       || Cl <- Clauses ],
    Res = [ { gce(P), gcg(G), gcb(B) }
            || {[P], G, B} <- PartsByClauses ],
    %?d(Res),
    BodyRetTypes = [ R#cg_state.ret_type || {_, _, R} <- Res ],
    %?d(BodyRetTypes),
    Fun = fun(C, Acc) -> ?CS:add(Acc, conj, C#cg_state.conset) end,
    ConsByClause = [ lists:foldl(Fun, ?CS:new(),
                             [ P, G, B ])
                     || { P, G, B } <- Res ],
    %?d(ConsByClause),
    Cons = ?CS:add(?CS:new(), disj, ConsByClause),
    %?d(Cons),
    RetTypeOfPatterns = ?SLIB:t_union([ P#cg_state.ret_type
                                        || {P, _, _} <- Res ]),
    FinalCons = ?CS:add(Cons, conj,
                        [HeadRes#cg_state.conset,
                         {subt, HeadRes#cg_state.ret_type,
                          RetTypeOfPatterns}]),
    RetType = ?SLIB:t_union(BodyRetTypes),
    %?d(RetType),
    #cg_state{ret_type = RetType,
              conset = FinalCons};
%% TODO message send operator has different semantics
gce(infix_expr, Node) ->
    [Lhs, Rhs] = ?SLIB:all_children(Node),
    ResLhs = gce(Lhs),
    ResRhs = gce(Rhs),
    {LT, RT, RetT} = ?SLIB:type_of_infix(?Expr:value(Node)),
    Cs1 = ?CS:add(?CS:new(), conj,
                  [{subt, ResLhs#cg_state.ret_type, LT},
                   {subt, ResRhs#cg_state.ret_type, RT}]),
    Cs2 = ?CS:add(Cs1, conj, ResLhs#cg_state.conset),
    FinalCons = ?CS:add(Cs2, conj, ResRhs#cg_state.conset),
    #cg_state{ret_type = RetT,
              conset = FinalCons};
gce(prefix_expr, Node) ->
    [Ch] = ?Query:exec(Node, ?Expr:children()),
    Res = gce(Ch),
    {T, RetT} = ?SLIB:type_of_prefix(?Expr:value(Node)),
    Cs1 = ?CS:add(?CS:new(), conj,
                  [{subt, Res#cg_state.ret_type, T}]),
    FinalCons = ?CS:add(Cs1, conj, Res#cg_state.conset),
    #cg_state{ret_type = RetT,
              conset = FinalCons};
gce(if_expr, Node) ->
    Clauses = ?Query:exec(Node, ?Expr:clauses()),
    PartsByClauses = [{?Query:exec(Cl, ?Clause:guard()),
                       ?Query:exec(Cl, ?Clause:body())} || Cl <- Clauses],
    %?d(PartsByClauses),
    Res = [ {gcg(G), gcb(B)} || {G,B} <- PartsByClauses],
    %?d(Res),
    ResType = ?SLIB:t_union([ B#cg_state.ret_type || {_G, B} <- Res]),
    ResCons = ?CS:concat([ ?CS:concat([G#cg_state.conset, B#cg_state.conset], conj) ||
                             {G, B} <- Res],
                         disj),
    #cg_state{ret_type = ResType,
              conset = ResCons};
gce(fun_expr, Node) ->
    Clauses = ?Query:exec(Node, ?Expr:clauses()),
    %?d(Clauses),
    PartsByClauses = [ { ?Query:exec(Cl, ?Clause:patterns()),
                         ?Query:exec(Cl, ?Clause:guard()),
                         ?Query:exec(Cl, ?Clause:body()) }
                       || Cl <- Clauses ],
    Res = [ { [gce(P) || P <- Patterns ],
              gcg(G),
              gcb(B) }
            || {Patterns, G, B} <- PartsByClauses ],
    %?d(Res),
    ConsByClause = [ ?CS:concat([ P#cg_state.conset || P <- Patterns] ++
                                    [G#cg_state.conset, B#cg_state.conset],
                                conj) || {Patterns, G, B} <- Res],

    Cons = ?CS:concat(ConsByClause, disj),

    Patterns = [ P || {P, _, _} <- Res],
    GroupedArgs = ?SLIB:zipN(Patterns),
    ?d(GroupedArgs),
    ArgTypes = [ ?SLIB:t_union([ Arg#cg_state.ret_type || Arg <- ArgStates])
                 || ArgStates <- GroupedArgs],

    BodyRetType = ?SLIB:t_union([ B#cg_state.ret_type || {_, _, B} <- Res ]),

    ?d(ArgTypes),
    ?d(BodyRetType),
    #cg_state{ret_type = ?SLIB:t_funsig(ArgTypes, BodyRetType),
              conset = Cons};


%% TODO other expression types !!!

gce(_, _Node) ->
    #cg_state{ret_type = ?SLIB:t_any(),
              conset = ?CS:new()}.



gcb([BodyExpr]) ->
    gce(BodyExpr);
gcb(BodyExprs) ->
    BodyRes = [ gce(B) || B <- BodyExprs ],
    BodyRet = (lists:last(BodyRes))#cg_state.ret_type,
    FinalCons = ?CS:concat([ R#cg_state.conset || R <- BodyRes ], conj),
    #cg_state{ret_type = BodyRet,
              conset = FinalCons}.



listElements([Child|ChildrenList]) ->
    case ?Expr:type(Child) of
        list ->
            ?Query:exec(Child, ?Expr:children()) ++ listElements(ChildrenList);
        cons ->
            listElements(?Query:exec(Child, ?Expr:children()) ++ ChildrenList);
        _ ->
            [Child]
    end;
listElements([]) ->
    [].


