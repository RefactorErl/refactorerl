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

%%% @doc This module contains filters which can recognize Wrangler and
%%% refactorerl differences.
%%%
%%% Status: not finished
%%%
%%% @todo code known differences
%%% @todo:wishlist Introduce pre, in-situ and post condition filters.
%%%
%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>

-module(reftest_diff_wr).
-vsn("$Rev: 3378 $").

-export([known_difference/0]).
-export([recognize/2]).

%-vsn("$Rev: 3370 $")

-include("test.hrl").
%-include("difftest.hrl").

%%% ============================================================================
%%% Interface

%% @spec () -> ignore_diffs()
known_difference() ->
    [{move_fun,     diffs_move_fun()},
     {extract_fun,  diffs_extract_fun()},
     {rename_fun,   diffs_rename_fun()},
     {rename_var,   diffs_rename_var()},
     {rename_mod,   diffs_rename_mod()},
     {tuple_funpar, diffs_tuple_funpar()},
     {gen,          diffs_gen()}].

recognize(Diffs,Args) when is_list(Diffs)->
    [{K,V} || {K,D} <- Diffs, V <- [recognize_(D,Args)], V /= [] ].

%%% ============================================================================
%%% Implementation

%% @todo ternary logic true/false/maybe
%% @private
recognize_(Diff,Args) when is_list(Args), is_function(Diff,1)->
    try
        Diff(Args)
    catch
        error:{E,_} when E==badmatch ->%; E==case_clause, E==function_clause ->
            [];
        {?Error,_,_}->
            []
    end.

args(ToBind,ToCopy,Fun)->
    fun(Args)->
            Bind = ?MISC:pgetu(ToBind,Args),
            R = case ToCopy of
                    [] ->
                        apply(Fun,Bind);
                    _ ->
                        Copy = ?MISC:pcopy(ToCopy,Args),
                        N = length(Copy),
                        N = length(ToCopy),
                        apply(Fun,Bind++[Copy])
                end,
            case R of
                false ->
                    [];
                true  ->
                    [{args_used,ToBind++ToCopy}]
            end
    end.

args(ToBind,Fun)->
    args(ToBind,[],Fun).

%%% ----------------------------------------------------------------------------
%%% Move function

diffs_move_fun() ->
    [eq_modfile()].

eq_modfile()->
    {eq_modfile,
     args([file,name],
          fun(F,M)->
                  [MFN] = ?Query:exec(?Query:seq(?Mod:find(M),
                                                 ?Mod:file())),
                  F == ?File:path(MFN)
          end)}.

%%% ----------------------------------------------------------------------------
%%% Rename function

diffs_rename_fun() ->
    [eq_fun_name(), newfun_bif()].

eq_fun_name()->
    {eq_fun_name,
     args([name],[file,position],
          fun(N,Args)->
                  Fun = ?Args:function(Args),
                  N == ?Fun:name(Fun)
          end)}.

newfun_bif()->
    {newfun_bif,
     args([name],[file,position],
          fun(Name,Args)->
                  Fun = ?Args:function(Args),
                  Arity = ?Fun:arity(Fun),
                  erl_internal:bif(Name,Arity)
          end)}.

%%% ----------------------------------------------------------------------------
%%% Rename variable

diffs_rename_var() ->
    [eq_var_name(), newvar_()].

eq_var_name()->
    {eq_var_name,
     args([varname],[file,position],
          fun(N,Args)->
                  Var = ?Args:variable(Args),
                  N == ?Var:name(Var)
          end)}.

%%% ----------------------------------------------------------------------------
%%% Extract function

diffs_extract_fun() ->
    [invalid_exprange()].
%% BIF: maybe, depending on argument count

%%% ----------------------------------------------------------------------------
%%% Generalize function

diffs_gen() ->
    [invalid_exprange(), newvar_(), funpp_bif()].

funpp_bif()->
    {funpp_bif,
     args([],[file,posrange],
          fun(Args)->
                  Exp = ?Args:expr_range(Args),
                  [Fun] = ?Query:exec(Exp,?Query:seq([?Expr:clause(),
                                                      ?Clause:form(),
                                                      ?Form:func()])),
                  {Name,Arity} = {?Fun:name(Fun),?Fun:arity(Fun)},
                  erl_internal:bif(Name,Arity+1)
          end)}.

%exprkind_app(X)->
%    .
% @todo:        {?Error,bad_kind,'application name'} ->

%%% ----------------------------------------------------------------------------
%%% Rename module

diffs_rename_mod() ->
    [newmod_exist()].

newmod_exist()->
    {newmod_exist,
     args([name],
          fun(New)->
                  [] /= ?Query:exec(?Mod:find(New))
          end)}.


%%% ----------------------------------------------------------------------------
%%% Tuple function arguments

diffs_tuple_funpar() ->
    [tup1(),implicit_usage()].

tup1()->
    {tup1,
     args([number],
          fun(N) ->
                  N == 1
          end)}.

implicit_usage()->
    {implicit_usage,
     args([],
            fun()->
%    [[_],[_],[_]] = ?MISC:pget([file,position,number],Args),
%    ?Args:expr_range(Args),
%    Fun = ?Expr:top(),
%    [] =/= ?Query:exec(Fun,?Fun:implicits()).
                    false
            end)}.


%%% ============================================================================
%%% crosscutting checks

%% @todo: make it more fine grain (non-single level, funapp arg, ...)
invalid_exprange() ->
    {invalid_exprange,
     args([],[file,posrange],
          fun(_Args)->
                  try
%                      _ = ?Args:expr_range(Args),
                      false
                  catch
                      {?Error,A,_} when A==bad_range; A==illegal_pos ->
                          true
                  end
          end)}.

newvar_()->
    {newvar_,
     args([varname],
          fun(N)->
                  N == "_"
          end)}.
