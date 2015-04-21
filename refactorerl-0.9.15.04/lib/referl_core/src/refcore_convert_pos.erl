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
%%% @todo
%%% == Implementation status ==
%%% This feature is _not_ fully implemented.
%%%
%%% @author Marton Wolosz <wolosz@inf.elte.hu>

-module(refcore_convert_pos).
-vsn("$Rev$").

-export([to_rel/0, to_abs/0, estimated_conversion_time/1]).

-include("core.hrl").

%% To relative from absolute

to_rel() ->
    [PosMode] = ?Syn:get_env(db_posmode),
    to_rel_from(PosMode).

to_rel_from(abs) ->
    Files = [File || {file, File} <- ?Syn:children(?Graph:root())],
    Forms = lists:map(fun(File) ->
        [Form || {form, Form} <- ?Syn:children(File)] end, Files),
    calculate_rel_files(Forms),
    ?FileMan:restart(rel);
to_rel_from(_)   ->
    ok. %already rel

calculate_rel_files([])    -> [];
calculate_rel_files([H|T]) ->
    calculate_rel_forms(H),
    calculate_rel_files(T).

calculate_rel_forms([])       -> [];
calculate_rel_forms([Form|T]) ->
    Lexes = ?Syn:leaves(Form),
    {lex, token, FirstToken}           = (?Graph:data(hd(Lexes)))#lex{},
    {FirstTokenScalar, _} = FirstToken#token.scalar,
    {{FirstTokenLine, _}, _} = FirstToken#token.linecol,
    PreWs = FirstToken#token.prews,
    {PreWsScalar, PreWsLine} = step_prews({0,0}, PreWs),
    StartScalar = FirstTokenScalar - PreWsScalar,
    StartLine = FirstTokenLine - PreWsLine,
    ?Graph:update(Form, (?Graph:data(Form))#form{
                                               form_length=undefined,
                                               start_scalar=StartScalar,
                                               start_line=StartLine}),
    calculate_rel_tokens(Lexes, StartScalar, StartLine),
    calculate_rel_forms(T).

step_prews(ScalarAndLine, [])       ->
    ScalarAndLine;
step_prews({Scalar, Line}, [$\n|T]) ->
    step_prews({(Scalar + 1), (Line + 1)}, T);
step_prews({Scalar, Line}, [_|T])   ->
    step_prews({(Scalar + 1), Line}, T).

calculate_rel_tokens([], _, _)                                      -> [];
calculate_rel_tokens([Lex|T], RelFormStartScalar, RelFormStartLine) ->
    {lex, token, Data} = (?Graph:data(Lex))#lex{},
    {AbsTokenScalarBegin, AbsTokenScalarEnd} = Data#token.scalar,
    Scalar = {(AbsTokenScalarBegin - RelFormStartScalar),
              (AbsTokenScalarEnd   - RelFormStartScalar)},
    {{BeginLine, BeginColumn}, {EndLine, EndColumn}} = Data#token.linecol,
    Linecol = {{(BeginLine - RelFormStartLine), BeginColumn},
               {(EndLine - RelFormStartLine), EndColumn}},
    ?Graph:update(Lex,{lex, token, Data#token{scalar=Scalar, linecol=Linecol}}),
    calculate_rel_tokens(T, RelFormStartScalar, RelFormStartLine).

%% To absolute from relative

to_abs() ->
    [PosMode] = ?Syn:get_env(db_posmode),
    to_abs_from(PosMode).

to_abs_from(rel) ->
    Files = [File || {file, File} <- ?Syn:children(?Graph:root())],
    Forms = lists:map(fun(File) ->
        [Form || {form, Form} <- ?Syn:children(File)] end, Files),
    calculate_abs_files(Forms),
    ?FileMan:restart(abs);
to_abs_from(_)   ->
    ok. %already abs

calculate_abs_files([])    -> [];
calculate_abs_files([H|T]) ->
    calculate_abs_forms(H),
    calculate_abs_files(T).

calculate_abs_forms([])       -> [];
calculate_abs_forms([Form|T]) ->
    FormData = (?Graph:data(Form)),
    RelFormStartScalar = (FormData)#form.start_scalar,
    RelFormStartLine = (FormData)#form.start_line,
    Lexes = ?Syn:leaves(Form),
    {lex, token, LastToken} = (?Graph:data(lists:last(Lexes)))#lex{},
    {_, LastTokenEndScalar} = LastToken#token.scalar,
    LastTokenPostWsLength = length(LastToken#token.postws),
    FormLength = LastTokenEndScalar + LastTokenPostWsLength + 1,
    ?Graph:update(Form, (FormData)#form{form_length=FormLength,
                                        start_scalar=undefined,
                                        start_line=undefined}),
    calculate_abs_tokens(Lexes, RelFormStartScalar, RelFormStartLine),
    calculate_abs_forms(T).

calculate_abs_tokens([], _, _)                                      -> [];
calculate_abs_tokens([Lex|T], RelFormStartScalar, RelFormStartLine) ->
    {lex, token, Data} = (?Graph:data(Lex))#lex{},
    {RelTokenScalarBegin, RelTokenScalarEnd} = Data#token.scalar,
    Scalar = {RelTokenScalarBegin + RelFormStartScalar,
              RelTokenScalarEnd   + RelFormStartScalar},
    {{BeginLine, BeginColumn}, {EndLine, EndColumn}} = Data#token.linecol,
    Linecol = {{(BeginLine + RelFormStartLine), BeginColumn},
               {(EndLine + RelFormStartLine), EndColumn}},
    ?Graph:update(Lex,{lex, token, Data#token{scalar=Scalar, linecol=Linecol}}),
    calculate_abs_tokens(T, RelFormStartScalar, RelFormStartLine).

%% Conversion time estimation

estimated_conversion_time(ConvMode) ->
    DbMod    = ?Graph:get_dbmod(),
    TokenNum = length(?Syn:leaves(?Graph:root())),
    round(expected_time(DbMod, ConvMode, TokenNum)).

expected_time(refdb_mnesia,    abs_to_rel, TokenNum) -> TokenNum * 0.0007873692;
expected_time(refdb_mnesia,    rel_to_abs, TokenNum) -> TokenNum * 0.0007756384;
expected_time(refdb_nif,       abs_to_rel, TokenNum) -> TokenNum * 0.0001053677;
expected_time(refdb_nif,       rel_to_abs, TokenNum) -> TokenNum * 0.0001049473;
expected_time(refdb_kyotomini, abs_to_rel, TokenNum) -> TokenNum * 0.0001504854;
expected_time(refdb_kyotomini, rel_to_abs, TokenNum) -> TokenNum * 0.0001772428.
