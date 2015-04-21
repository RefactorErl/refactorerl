
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

-module(reftest_stress_test).

-include("test.hrl").

-export([test/1, test/2]).
-export([pos_before/2]).

%%% ============================================================================
%%% Utils


%% Returns a random value from a range.
%% It is supposed that `Start` =< `End`.
rnd_pos_from_range({Start, End}) ->
    Start + random:uniform(End - Start + 1) - 1.

%% Drops elements from a list with 0.5 probability.
rnd_list_subset(List) ->
    lists:filter( fun(_) -> random:uniform(2) == 2 end, List).

%% Returns a random element of the list with uniform probability.
rnd_listelem(List) ->
    Length = length(List),
    Idx    = random:uniform(Length),
    lists:nth(Idx, List).

%% Returns a random nonempty subrange of the given list.
rnd_elem_range([]) ->
    [];
rnd_elem_range(List) ->
    rnd_listelem(list_ranges(List)).

%% Returns the nonempty subranges of the list.
%% E.g. [1,2,3] ==> [[1,2,3], [1,2], [1], [2,3], [2], [3]]
list_ranges([]) -> [];
list_ranges([X|Xs]) ->
    [[X|H] || H <- heads(Xs)] ++ list_ranges(Xs).

%% Returns the heads of a list (including the empty list).
heads([])     -> [[]];
heads([X|Xs]) -> [[X|Xs], heads(Xs)].

%Returns a random permutation of a list.
rnd_perm([]) -> [];
rnd_perm(List) ->
    Length = length(List),
    RndElem = lists:nth(random:uniform(Length),List),
    [ RndElem | rnd_perm(lists:delete(RndElem,List)) ].

%%% ============================================================================
%%% Stress test

%% This module applies a number of transformations in sequence
%% to the loaded modules.
%% The next transformation and its parameters are selected
%% in a random manner.
%% The selected position/range is not guaranteed to fit the
%% parameter of the refactoring, except
%% when using the parameter `rnd_pos`.


%% Does `Iterations' iteration steps using the default transformations
%% and parameters.
test(Iterations) ->
    test(Iterations, []).

%% Does `Iterations' iteration steps with the given parameters.
%% Parameters:
%%      {rnd_seed, Seed}   : initialises the random seed for reused stress tests
%%                              Seed is a triplet of integers
%%      {tr, Trs}          : uses only the given transformations
%%      {tr, {except, Trs}}: uses all transformations except the ones given
%%      reset_db           : clears the database before the test
%%      {files, Files}     : loads a set of files before the test
%%      append_info        : append the info about the actual transformation
%%                           to all saved files
%% Planned parameters:
%%      {coverage, Mods}   : prints coverage of the given modules
%%      {coverage, all}    : prints coverage of all modules
%%      coverage           : prints coverage of the modules actually used
test(Iterations, Opts) ->
    [RandomSeed, Trs, ResetDB, Files, AppendInfo, _Cover] =
        [proplists:get_value(Opt, Opts)
            || Opt <- [rnd_seed, tr, reset_db, files, append_info, cover]],

    case RandomSeed of
        {Seed1, Seed2, Seed3} -> ok;
        _                     -> {Seed1, Seed2, Seed3} = now()
    end,
    random:seed(Seed1, Seed2, Seed3),

    init_database(ResetDB),

    case Files of
        undefined -> ok;
        _         ->
            [referl_fileman:add_file(File) || File <- Files],
            [io:format("Adding file ~s~n", [File]) || File <- Files]
    end,

    iterate(Iterations, 1, Trs, AppendInfo).

%% Does the given number of iterations.
%% In case of `infty`, it is an endless cycle of iterations.
%% todo Rework the module so that the iterations are asynchronous and can be stopped.
iterate(Limit, StepCount, Trs, AppendInfo) ->
    {Success, Transformation, Params} = step(Trs),
    % todo save only new/changed files
    Files     = ?Query:exec(?ESG:root(), [file]),
    NewFiles  = [ {File, filename(Path, StepCount)}
                    ||  File <- Files,
                        #file{path=Path} <- [?ESG:data(File)] ],
    [ referl_fileman:save_file(File) || {File, _NPath} <- NewFiles ],
    [ io:format("File written: ~p~n", [NPath])  || {_File, NPath} <- NewFiles ],
    io:format("----------------------------------------~n", []),

    case AppendInfo of
        true ->
            {_OKs, IODevs} = lists:unzip([file:open(NPath, [append])
                                            || {_, NPath} <- NewFiles]),
            case Success of
                success -> ok;
                failure ->
                    [io:put_chars(Dev, "% Transformation failed~n") || Dev <- IODevs]
            end,
            [io:format(Dev, "% ~p~n", [Transformation]) || Dev <- IODevs],
            [io:format(Dev, "% ~p~n", [Params]) || Dev <- IODevs],
            [file:close(Dev) || Dev <- IODevs];
        _ -> ok
    end,

    if
        Limit == StepCount  -> ok;
        true                -> iterate(Limit, StepCount + 1, Trs, AppendInfo)
    end.


%% Appends an index to a file name.
filename(Path, Index) ->
    lists:flatten(Path ++ io_lib:format("~15.15.0w", [Index])).


%% Performs a random refactoring.
step(AllTrs) ->
    DefTrs = default_transformations(),
    case AllTrs of
        {except, List} -> Trs = lists:filter(fun({Tr, _, _, _}) ->
                                                not lists:member(Tr, List)
                                             end, DefTrs);
        undefined      -> Trs = DefTrs;
        _              -> Trs = lists:filter(fun({Tr, _, _, _}) ->
                                                lists:member(Tr, AllTrs)
                                             end, DefTrs)
    end,
    % todo

    {Transformation, ParType, PosGen, OtherPars} = rnd_listelem(Trs),

    io:format("Transformation: ~p~n", [Transformation]),

    case generate_position(PosGen, 10) of
        no_pos ->
            io:format("Reached number of retries, "
                      "could not generate transformation parameter~n"),
            {failure, Transformation, OtherPars};
        {Pos, Additional} ->
            Params = transform_params(ParType, Pos) ++ Additional ++ OtherPars,

            io:format("Parameters: ~p~n", [Params]),

            ?Transform:do(Transformation, Params),
            Result = ?Transform:wait(),

            io:format("Result of ~p: ~p~n", [Transformation, Result]),

            {success, Transformation, Params}
    end.


generate_position(_, 0) ->
    no_pos;
generate_position(PosGen, Retries) ->
    case PosGen() of
        retry               -> generate_position(PosGen, Retries - 1);
        {Pos, Additional}   -> {Pos, Additional}
    end.

%% Transforms the parameters to conform to `?Transform:do` call convention.
transform_params(none, File) ->
    #file{path = FilePath} = ?Graph:data(File),
    [{file, FilePath}];
transform_params(range, {File, Node}) ->
    transform_params(range, {File, Node, Node});
transform_params(pos, {File, Node}) ->
    #file{path = FilePath} = ?Graph:data(File),
    {StartRange, EndRange} = pos_in_node(File, Node),
io:format("ranges ~p ~p~n", [StartRange, EndRange]),
    if
        StartRange /= EndRange ->
            throw({error, "Single element expected"});
        true                   ->
            Pos = rnd_pos_from_range(StartRange),
            [{file, FilePath}, {position, Pos}]
    end;
transform_params(range, {File, Node1, Node2}) ->
    #file{path = FilePath} = ?Graph:data(File),
    {Range1, _} = pos_in_node(File, Node1),
    {_, Range2} = pos_in_node(File, Node2),
    case {rnd_pos_from_range(Range1), rnd_pos_from_range(Range2)} of
        {Pos1, Pos2} when Pos1 =< Pos2 -> ok;
        {Pos2, Pos1}                   -> ok_reversed
    end,
    [{file, FilePath}, {posrange, {Pos1, Pos2}}].

%% Returns the position range of the first and last token below the node.
%% Since Emacs starts calculating positions from 0, an adjustment of -1 is done.
pos_in_node(File, Node) ->
    First       = ?Query:exec1(Node, ?Syn:first_leaf(), []),
    Last        = ?Query:exec1(Node, ?Syn:last_leaf(), []),
    AllLeaves   = ?Syn:leaves(File),
    {BeforeLen, NodeLen, After} = pos_before(AllLeaves, First),
    Pos1Start = BeforeLen,
    Pos1End   = BeforeLen + NodeLen - 1,
    if
        First == Last ->
            Pos2Start = Pos1Start,
            Pos2End   = Pos1End;
        true ->
            {BeforeLen2, NodeLen2, _After} = pos_before(After, Last),
            Pos2Start = Pos1Start + NodeLen + BeforeLen2,
            Pos2End   = Pos1Start + NodeLen + BeforeLen2 + NodeLen2 - 1
    end,

    Range = {{Pos1Start,  Pos1End}, {Pos2Start,  Pos2End}},
    emacs_range(Range).

%% Converts ranges to such as supplied by Emacs, since those start at 1, not 0.
emacs_range({{P11, P12}, {P21, P22}}) ->
    {{P11 + 1, P12 + 1}, {P21 + 1, P22 + 1}}.


%% Returns the number of characters that the tokens before the given token node
%% contain, the length of the node, and the token nodes after the token.
%% It discards whitespace attached before the token.
%% todo Position calculation fails for macros.
pos_before(AllLeaves, Node) ->
    case lists:splitwith(fun(N) -> N /= Node end, AllLeaves) of
        {Before, [Node|After]} ->
            BeforeLen           = length(lists:flatten(lists:map( fun ?Syn:tree_text/1,
                                                        Before))),
            PreWSLen            = token_prews_len(Node),
            NodeLen             = token_len(Node),
            {BeforeLen + PreWSLen, NodeLen, After};
        {_, []} ->
            throw({error, "The designated node is not found"})
    end.

%% Returns length of the whitespace before the token.
token_prews_len(Node) ->
    #lex{data=#token{prews=Pre}} = ?ESG:data(Node),
    length(Pre).


%% Returns length of the actual contents of the token.
token_len(Node) ->
    #lex{data=#token{text=Text}} = ?ESG:data(Node),
    length(Text).


%% Clears the database wheh the arguments specify so.
init_database(undefined)    -> ok;
init_database(false)        -> ok;
init_database(true) ->
    io:format("----------------------------------------~n"),
    io:format("Resetting database...~n"),
    ?UI:reset(),
    wait_until_server_is_active_again(),
    io:format("Database reset, continuing...~n"),
    io:format("----------------------------------------~n").

%% Returns when the graph server is running again.
%% This is not really elegant, but works.
wait_until_server_is_active_again() ->
    try
        timer:sleep(timer:seconds(1)),
        ?Graph:root()
    catch _ ->
        wait_until_server_is_active_again()
    end.


%% The common list of all transformations that are allowed to be used.
default_transformations() ->
    [
        {reftr_merge,
            range,  fun rnd_expr/0,           [{varname, rnd_varname()}] },
        {reftr_rename_var,
            pos,    fun rnd_var/0,            [{varname, rnd_varname()}] },
        {reftr_elim_var,
            pos,    fun rnd_var/0,            []},
        {reftr_rename_mod,
            none,   fun rnd_mod/0,            [{name, rnd_atom()}]},
        {reftr_extract_fun,
            range,  fun rnd_expr_body/0,      [{name, rnd_atom()}]},
        {reftr_gen,
            range,  fun rnd_expr_body/0,      [{varname, rnd_varname()}] },
        {reftr_inline_fun,
            pos,    fun rnd_fun_app/0,        []},
        {reftr_move_rec,
            none,   fun rnd_recs/0,           []},
        {reftr_expand_funexpr,
            pos,    fun rnd_funexpr/0,        []},
        {reftr_reorder_funpar,
            pos,    fun rnd_fun_with_order/0, []},
        {reftr_move_fun,
            none,   fun rnd_funs/0,           []},
        {reftr_tuple_funpar,
            range,  fun rnd_fun_args/0,       []},
        {reftr_rename_recfield,
            pos,    fun rnd_recfield/0,       [{name, rnd_atom()}]},
        {reftr_rename_fun,
            pos,    fun rnd_fun/0,            [{name, rnd_atom()}]},
        {reftr_rename_rec,
            pos,    fun rnd_rec/0,            [{name,  rnd_atom()}] } ].

%%% ============================================================================
%%% Random node selector functions
%%%
%%% These functions return a node from the syntax tree (or two when applicable),
%%% selected with equal probability from all loaded modules.
%%% They also return the file from which the choice was made.
%%% In case no random position is available, they return `retry'.

%% Returns all expressions in a file having one of the given kinds.
%% In case `no_restriction' is given, all expressions are returned.
exprs(File, no_restriction) ->
    ?Query:exec(File, ?Query:seq([?File:forms(), ?Form:deep_exprs()]));
exprs(File, Kinds) when is_list(Kinds) ->
    [Expr   ||  Expr <- exprs(File, no_restriction),
                #expr{type = Kind} <- [?ESG:data(Expr)],
                lists:member(Kind, Kinds)].

%% Chooses a random file and a random expression in it.
%% The kind of the expression can be limited to a list or `no_restriction'.
rnd_expr_in_rnd_file(Kinds, Msg) ->
    File = rnd_file(),
    AllExprs = exprs(File, Kinds),

    case AllExprs of
        [] -> retry;
        _  ->
            RndVar = rnd_listelem(AllExprs),
            io:format(Msg ++ " ~p~n", [lists:flatten(?Syn:tree_text(RndVar))]),
            {{File, RndVar}, []}
    end.

rnd_clause() ->
    File = rnd_file(),
    Form = rnd_form(File),
    case
        ?Query:exec([Form], ?Query:all(
                                ?Form:clauses(),
                                ?Query:seq(
                                    ?Form:deep_exprs(), ?Expr:clauses()
                                    )))
    of
        []      ->  rnd_clause();
        Clauses ->  {{File, rnd_listelem(Clauses)}, []}
    end.

rnd_mod() ->
    File = rnd_file(),
    case ?Query:exec(File,?File:module()) of
        [] -> retry;
        _ -> {File,[]}
    end.

rnd_var()       -> rnd_expr_in_rnd_file([variable], "var").
rnd_expr()      -> rnd_expr_in_rnd_file(no_restriction, "expr").

%% todo It does not handle module qualified applications yet.
rnd_fun_app() ->
    case rnd_expr_in_rnd_file([application], "app") of
        retry                     -> retry;
        {{File, App}, Additional} ->
            AppName     = ?Query:exec1([App], ?Expr:child(1), []),
            {{File, AppName}, Additional}
    end.


rnd_expr_body() ->
    case random:uniform(2) of
        1 ->
            {{File, Expr}, Additional} = rnd_expr(),
            {{File, Expr, Expr}, Additional};
        2 -> rnd_body()
    end.

rnd_body() ->
    {{File, Clause}, Additional}  = rnd_clause(),
    case ?Query:exec(Clause, ?Clause:body()) of
        [] ->
            io:format("no body: ~p~n", [Clause]),
            retry;
        Body ->
            Range = lists:flatten(rnd_elem_range(Body)),
io:format("trng: ~p~n", [Range]),
io:format("rng: ~p ~p~n", [hd(Range), lists:last(Range)]),
            {{File, hd(Range), lists:last(Range)}, Additional}
    end.

rnd_funexpr() ->
    File = rnd_file(),
    ImpFunExprs =
        lists:filter(
          fun(Expr) ->
                  FunRef = hd(?Query:exec(Expr,?Expr:child(1))),
		  Kind = ?Expr:type(FunRef),
		  Val = ?Expr:value(FunRef),
                  if
                      Kind == atom -> true;
                      Kind == infix_expr andalso Val == ':' -> true;
		      true -> false
                  end
          end,
          exprs(File,[implicit_fun])),
    case ImpFunExprs of
        [] -> retry;
%	_ -> {{File, rnd_listelem(ImpFunExprs)},[]}
	_ -> {{File,?Query:exec(rnd_listelem(ImpFunExprs),?Expr:child(1))},[]}
    end.

rnd_fun() ->
    File = rnd_file(),
    FunDefs = ?Query:exec( File,
                           ?Query:seq([ ?File:module(),
                                        ?Mod:locals(),
                                        ?Fun:definition(),
                                        ?Form:clauses(),
                                        ?Clause:name() ])),
    FunRefs =
        lists:map(
          fun(Expr) ->
                  case ?Query:exec(Expr,?Expr:modq()) of
                      [] -> ?Query:exec(Expr, ?Expr:child(1));
                      _ -> ?Query:exec(Expr, ?Query:seq(?Expr:child(1),
                                                        ?Expr:child(2)))
                  end
          end,
          exprs(File,[application,implicit_fun])),
    AllFuns = FunDefs ++ FunRefs,
    case AllFuns of
        [] -> retry;
        _ -> {{File, rnd_listelem(AllFuns)},[]}
    end.

rnd_fun_with_order() ->
    File = rnd_file(),
    LocalFuns =
        [ {LocalFun,?Fun:arity(LocalFun)} ||
            LocalFun <- ?Query:exec(
                           File,
                           ?Query:seq(?File:module(), ?Mod:locals()))],
    FunDefs =
        [ {hd(?Query:exec(
                 LocalFun,
                 ?Query:seq([ ?Fun:definition(),
                             ?Form:clauses(),
                             ?Clause:name() ]))),
           Arity} || {LocalFun,Arity} <- LocalFuns ],
    FunRefs =
        lists:map(
          fun(Expr) ->
                  Arity =
                      ?Fun:arity(hd(?Query:exec(Expr,[funref]))),
                  case ?Query:exec(Expr,?Expr:modq()) of
                      [] ->
                          { hd(?Query:exec(Expr, ?Expr:child(1))),
                            Arity };
                      _ ->
                          { hd(?Query:exec(Expr, ?Query:seq(?Expr:child(1),
                                                            ?Expr:child(2)))),
                            Arity }
                  end
          end,
          exprs(File,[application,implicit_fun])),
    AllFuns = FunDefs ++ FunRefs,
    FunsWithParams =
        [ {Fun,Arity} || {Fun,Arity} <- AllFuns, Arity > 0 ],
    case FunsWithParams of
        [] -> retry;
        _ ->
            {Fun,Arity} = rnd_listelem(FunsWithParams),
            {{File,Fun},[{order,rnd_perm(lists:seq(1,Arity))}]}
    end.

rnd_funs() ->
    File = rnd_file(),
    Mods = ?Query:exec([module]),
    case length(Mods) of
        1 -> retry;
        _ ->
            Mod = ?Query:exec(File, ?File:module()),
            Funs = ?Query:exec(Mod, ?Mod:locals()),
            RndFunList = [ {?Fun:name(Fun),?Fun:arity(Fun)} ||
                          Fun <- rnd_list_subset(Funs) ],
            case RndFunList of
                [] -> retry;
                _ ->
                    NewMod = ?Mod:name( rnd_listelem(Mods--Mod) ),
                    {File, [ {funlist, RndFunList}, {name, NewMod} ]}
            end
    end.

rnd_fun_args() ->
    File = rnd_file(),
    LocalFunsWithParams =
        [ Fun ||
            Fun <- ?Query:exec( File,
                                ?Query:seq([ ?File:module(), ?Mod:locals()])),
            ?Fun:arity(Fun) > 0 ],
    ParamListsInFunDefs =
        [ ?Query:exec( Fun, ?Query:seq([ ?Fun:definition(),
                                         ?Form:clauses(),
                                         ?Clause:patterns() ])) ||
            Fun <-  LocalFunsWithParams ],
    FunAppsWithParams =
        [ FunApp ||
            FunApp <- exprs(File,[application]),
            ?Query:exec(FunApp, [funref,{func,back},{moddef,back}]) == File,
            ?Fun:arity(hd(?Query:exec(FunApp, [funref]))) > 0 ],
    ParamListsInFunApps =
        [ ?Query:exec(Fun,?Expr:children()) --
          ?Query:exec(Fun,?Expr:child(1)) ||
            Fun <- FunAppsWithParams ],
    FunParamLists = ParamListsInFunDefs ++ ParamListsInFunApps,
    case FunParamLists of
        [] -> retry;
        _ ->
            RndParamList = rnd_listelem(FunParamLists),
            { {File,rnd_listelem(RndParamList),rnd_listelem(RndParamList)},
              [] }
    end.

rnd_recfield() ->
    File = rnd_file(),
    FieldDefs = ?Query:exec(File,
                            ?Query:seq(?File:records(),
                                       [field,{fielddef,back}])),
    FieldRefs = [ hd(?Query:exec(Expr,?Expr:child(1))) ||
                    Expr <- exprs(File,[record_field]) ],
    AllFields = FieldDefs ++ FieldRefs,
    case AllFields of
        [] -> retry;
        _ -> {{File, rnd_listelem(AllFields)},[]}
    end.

rnd_rec() ->
    File = rnd_file(),
    RecDefs = ?Query:exec(File,
                          ?Query:seq(?File:records(),
                                     [{recdef,back}, {attr,1}])),
    RecRefs =
        [ hd(?Query:exec(Expr, ?Expr:child(1))) ||
            Expr <- exprs(File, [record_index, record_expr]) ] ++
        [ hd(?Query:exec(Expr, ?Expr:child(2))) ||
            Expr <- exprs(File, [record_update, record_access]) ],
    AllRecs = RecDefs ++ RecRefs,
    case AllRecs of
        [] -> retry;
        _  -> {{File, rnd_listelem(AllRecs)},[]}
    end.

rnd_recs() ->
    File = rnd_file(),
    case length(?Query:exec([file])) of
        1 -> retry;
        _ ->
            RecNames = [ ?Rec:name(Rec) ||
                           Rec <- ?Query:exec(File, ?File:records()) ],
            RndRecNames = rnd_list_subset(RecNames),
            case RndRecNames of
                [] -> retry;
                _ ->
                    NewFile =
                        ?File:path(rnd_listelem(?Query:exec([file])--[File])),
                    {File, [ {reclist, RndRecNames}, {filename, NewFile} ]}
            end
    end.

%%% ----------------------------------------------------------------------------
%%% Various helper random selection functions

rnd_file() ->
    AllFiles = ?Query:exec([file]),
    rnd_listelem(AllFiles).

rnd_form(File) ->
    AllForms = ?Query:exec([File], ?File:forms()),
    rnd_listelem(AllForms).


%% Returns a random position in the given file.
%% todo Check: are limits reached/exceeded?
%% rnd_pos_in_file(File) ->
%%     FileLength = length(lists:flatten(?Syn:tree_text(File))),
%%     random:uniform(FileLength).

%% Returns a random range in the file.
%% rnd_range() ->
%%     File = rnd_file(),
%%     Pos1 = rnd_pos_in_file(File),
%%     Pos2 = rnd_pos_in_file(File),
%%     case Pos1 < Pos2 of
%%         true  -> {File, Pos1, Pos2};
%%         false -> {File, Pos1, Pos2}
%%     end.


%% Returns a random file and a random position in it.
%% To be used for testing with errors allowed.
%% rnd_pos() ->
%%     File = rnd_file(),
%%     {File, rnd_pos_in_file(File)}.


%% Creates a random variable name.
rnd_varname() ->
    Uppers = lists:seq($A, $Z),
    [rnd_listelem(Uppers) | rnd_name_inner()].

%% Creates a random (unquoted) atom.
rnd_atom() ->
    Lowers = lists:seq($a, $z),
    [rnd_listelem(Lowers) | rnd_name_inner()].

%% Creates a random name, except for the first character.
%% Is useful for both variable names and atoms.
rnd_name_inner() ->
    MaxLength = 10,
    case random:uniform(MaxLength) of
        1       -> "";
        Length  -> [rnd_var_inner() || _ <- lists:seq(1, Length) ]
    end.

%% A random character inside a variable name.
rnd_var_inner() ->
    Possible = [$@, $_] ++ lists:seq($0, $9) ++ lists:seq($a, $z) ++ lists:seq($A, $Z),
    rnd_listelem(Possible).
