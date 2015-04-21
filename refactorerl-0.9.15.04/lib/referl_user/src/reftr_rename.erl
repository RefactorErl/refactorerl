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
%%%
%%% ============================================================================
%%% Module information

%%% @doc This module implements a common renamer refactoring.
%%%
%%% @author Robert Kitlei <kitlei@inf.elte.hu>

-module(reftr_rename).
-vsn("$Rev$ ").

%% Callbacks
-export([prepare/1]).
-export([error_text/2]).

%% Includes and definitions
-include("user.hrl").

%%% ============================================================================
%%% Callbacks

%% todo Move this to another module.
convert_data(Node) ->
    case ?ESG:data(Node) of
        #lex{data=#token{type=T}}   -> T;
        L=#lex{}                    -> L;
        #expr{type=T}               -> T;
        #clause{type=T}             -> T;
        #form{type=Type, tag=Tag}   -> {Type, Tag};
        #typexp{type=T}             -> T;
        #file{}                     -> file;
        _                           -> unknown
    end.

%% @private
%% todo ?LocalErrors thrown in the chosen module won't reach their destination,
%%      as their ?MODULE is different from this one.
%%      Perhaps ?LocalErrors should be eliminated altogether.
prepare(Args) ->
    File        = ?Args:file(Args),
    ?Check(proplists:is_defined(position, Args), ?RefErr0r(bad_kind)),
    Pos         = proplists:get_value(position, Args),
    Tokens      = ?Query:exec(File, ?File:token(Pos)),
    ?Check(length(Tokens) == 1, ?RefErr0r(bad_kind)),
    [Token]     = Tokens,
    RootPath    = ?Syn:root_path(Token),
    Datas       = [convert_data(Node) || {_PLink, Node} <- RootPath],
    RevDatas    = lists:reverse(Datas),
    HasVirtual  = [] =/= [true || #lex{data=virtual} <- Datas],

    RenameMod =
        case {HasVirtual, RevDatas, Datas} of
            {_, _, [_File, {_, include}|_]} ->
                throw(?RefErr0r(bad_kind));

            {true, _, _} ->
                reftr_rename_mac;
            {false, [_Lex, {macro, _}|_], _} ->
                reftr_rename_mac;

            {false, [_Lex, variable|_], _} ->
                reftr_rename_var;

            {false, [atom, spec_field|_], _} ->
                reftr_rename_recfield;
            {false, [atom, record_field|_], _} ->
                reftr_rename_recfield;
            {false, [atom, atom, record_access|_], _} ->
                reftr_rename_recfield;
            {false, [atom, atom, record_index|_], _} ->
                reftr_rename_recfield;

            {false, [atom, {record, _}|_], _} ->
                reftr_rename_rec;
            {false, [atom, record_expr|_], _} ->
                reftr_rename_rec;
            {false, [atom, record_update|_], _} ->
                reftr_rename_rec;
            {false, [atom, record_access|_], _} ->
                reftr_rename_rec;
            {false, [atom, record_index|_], _} ->
                reftr_rename_rec;

            {false, [atom, atom, infix_expr, implicit_fun|_], _} ->
                [_, {_, Atom}, {Lex, Infix}|_] = lists:reverse(RootPath),
                case ?ESG:path(Infix, [Lex]) of
                    [Atom, _] -> reftr_rename_mod;
                    [_, Atom] -> reftr_rename_fun
                end;

            {false, [_Lex, atom, fundef|_], _} ->
                reftr_rename_fun;
            {false, [_Lex, atom, application|_], _} ->
                reftr_rename_fun;
            {false, [atom, atom, implicit_fun|_], _} ->
                reftr_rename_fun;
            {false, [atom, atom, funref|_], _} ->
                reftr_rename_fun;

            {false, [atom, {module, _}|_], _} ->
                reftr_rename_mod;

            _ -> throw(?RefErr0r(bad_kind))
        end,
    case proplists:get_value(ask_missing, Args) of
        false ->
            NewArgs = transform_args(?Args:string(Args), RenameMod, Args),
            RenameMod:prepare(NewArgs);
        _ ->
            RenameMod:prepare(Args)
    end.

%% Transforms the non-interactive string representation to appropriate properties.
transform_args(String, reftr_rename_var, Args) ->
    Name = ?MISC:to_list(String),
    VarName = case ?Var:valid_name(Name) of
                  true  -> Name;
                  false -> throw(?RefErr0r(bad_var_name))
              end,
    [{varname, VarName} | Args];
transform_args(String, reftr_rename_mac, Args) ->
    [{macname, ?MISC:to_list(String)} | Args];
transform_args(String, reftr_rename_mod, Args) ->
    [{name, String} | proplists:delete(position, Args)];
transform_args(String, _RenameMod, Args) ->
    [{name, String} | Args].


error_text(_, _) ->
    "The universal renamer currently does not support " ++
    "relaying the error messages of the specific renamings.".
