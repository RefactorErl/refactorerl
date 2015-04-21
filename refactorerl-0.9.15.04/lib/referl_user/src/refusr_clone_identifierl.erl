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

%%% @doc Bridge module of different duplicated code detector algorithms

%%% @author Viktoria Fordos <f-viktoria@elte.hu>

-module(refusr_clone_identifierl).
-vsn("$Rev: 9316 $").
-include("user.hrl").
-include("refusr_clone_identifierl.hrl").

-export([get_clones/0, get_clones/1, default_options/0, error_text/2]).



get_clones()->
    get_clones([]).

get_clones(Options) when is_list(Options) ->
    try
        ?Lib:delete_diff_db_hash_clones(),
        AlgExpandedOptions =  expand_alg_options(Options),
        Results = check_exist_search_or_search(AlgExpandedOptions),        
        FormattedResults = ?Lib:format_results(Results, AlgExpandedOptions, undef),
        ?Lib:write_to_file(FormattedResults, Results, AlgExpandedOptions)
    catch
        _Cl:E when not is_tuple(E) orelse (tuple_size(E) =/= 3)->
            throw(?LocalErr0r(unexpected_error))
    end.

default_options()->
    [{algorithm, sw_metrics},
     {cache, false},
     {format, prettyprint},
     {postype, linecol},
     {enforce, false},
     {name, ?Lib:generate_name_by_time()}].

error_text(empty_db, _)->
    io_lib:format("Database is empty.~n",[]);
error_text(name_already_used, _) ->
    io_lib:format("Name already used. Use the enforce option.~n",[]);
error_text(unexpected_error, _)->
    io_lib:format("Unexpected error occurred.~n",[]);
error_text(invalid_position, _)->
    io_lib:format("Invalid position.~n",[]).


%% @doc Expands and checks the validity of Options.
expand_alg_options(Options) ->
    ExpandedOptions = ?Lib:validate_and_expand_options(?MODULE, Options),
    Alg = proplists:get_value(algorithm, ExpandedOptions),
    Cache = proplists:get_value(cache, ExpandedOptions), 
    ExpOpts = ?Lib:validate_and_expand_options(select_alg(Alg,Cache), ExpandedOptions),
    transform_to_subjects(Alg, ExpOpts).

%% @doc Does the dup. code search, either runs new algoirthm 
%%      or returns the stored result from the DETs.
check_exist_search_or_search(Options) ->
    Name = proplists:get_value(name, Options),
    Enforce = proplists:get_value(enforce, Options),
    ConvOptions =
        case proplists:get_value(max_rank, Options) of
            Value when is_list(Value) ->
                DelOpts = proplists:delete(max_rank, Options),
                {MaxRank,_} = string:to_integer(Value),
                DelOpts ++ [{max_rank, MaxRank}];
            _ -> Options
        end,
    Key = ?Lib:create_key(ConvOptions),
    case ?Lib:used_name_dets(Name, Key, Enforce) of
        false -> ok;
        _ -> throw(?LocalErr0r(name_already_used))
    end,
    Result = 
        case ?Lib:match_key_dets(Key) of
            {false, []} ->
                Res = search(ConvOptions),
                case ?Lib:match_key_dets(Key) of
                    {false, []} ->
                        ?Lib:insert_dets(#ci_results
                                    {key = Key,
                                    name = Name,
                                    opts = lists:usort(ConvOptions),
                                    results = Res,
                                    db_hash = ?MISC:database_hash()}),
                        Res;
                    {true, NRes} -> 
                        ?Lib:add_alias_and_return(NRes, Name)
                end;
            {true, Res} -> ?Lib:add_alias_and_return(Res, Name)
        end,
    Result++[{clone_name, Name}].

search(Options)->
    Opt0 = proplists:delete(algorithm, Options), 
    OptForAlgs = proplists:delete(cache, Opt0),
    run_selected_alg(proplists:get_value(algorithm, Options),
                     proplists:get_value(cache, Options),
                     OptForAlgs).

run_selected_alg(Algorithm, UseCache, Options)->
    check_common_preconds(),
    run_selected_alg0(Algorithm, select_alg(Algorithm, UseCache),
                      Options).


%% why are there clauses?
run_selected_alg0(suffix_tree, AlgModule, Options)-> 
    AlgModule:get_clones(Options);
run_selected_alg0(filtered_suffix_tree, AlgModule, Options)->
    AlgModule:get_clones(Options);
run_selected_alg0(matrixfilter, AlgModule, Options) ->
    AlgModule:get_clones(Options);
run_selected_alg0(matrix, AlgModule, Options)->
    AlgModule:get_clones(Options);
run_selected_alg0(sw_metrics, AlgModule, Options)->
    AlgModule:get_clones(Options).

select_alg(matrix, false)->
    ?TLEClones;
select_alg(matrix, true)->
    ?CachedTLEClones;
select_alg(sw_metrics, _)->
    ?FunClones;
select_alg(suffix_tree, _)->
    ?STree;
select_alg(filtered_suffix_tree, _) ->
    ?FSTree;
select_alg(matrixfilter, _) ->
    ?MatrixFilter.


transform_to_subjects(suffix_tree, Options) ->
    Subjects0 = proplists:get_value(subject,Options),
    NodeSubjects = 
        case Subjects0 of
            all -> [];
            _ -> Subjects0
        end,
    Subjects = 
        transform_to_subjects0(suffix_tree,
            ?STree:get_subjects(Options)++NodeSubjects),
    Positions = proplists:get_value(positions, Options),
    PropListSubject = 
        case {Subjects,Positions} of
            {[],undefined} -> all;
            _ -> lists:usort(Subjects)
        end,
    Opt0 = proplists:delete(subject, Options),
    Opt1 = proplists:delete(positions, Opt0),
    Opt1 ++ [{subject, PropListSubject}];
transform_to_subjects(filtered_suffix_tree, Options) ->
    transform_to_subjects(suffix_tree, Options);
transform_to_subjects(Algorithm, Options)->
    Files = case proplists:get_value(files, Options, []) of
        [] -> [];
        FileList-> ?Lib:get_files_from_options(FileList)
    end,
    FileFuns = ?Query:exec(Files,?Query:seq([?File:forms(),?Form:func()])),
    FileSubjects = [begin {_,Fun0} = ?Fun:mod_fun_arity(Fun),
                            Fun0 end || Fun <- FileFuns],
    Subjects = proplists:get_value(subject, Options),
    PosSubjects =
        case proplists:get_value(positions, Options) of
            undefined -> 
                [];
            Poss ->
                lists:flatmap(fun(Pos) ->
                    ?Lib:get_clones_by_pos(Algorithm, Pos)
                end, Poss)
        end,
    FuncSubjects =
        case proplists:get_value(func_list, Options, FileSubjects) of
            FileSubjects -> 
                lists:flatten(transform_to_subjects0(Algorithm, FileSubjects));
            List -> 
                lists:flatten(transform_to_subjects0(Algorithm, List++FileSubjects))
        end,
    Opt0 = proplists:delete(subject, Options),
    Opt1 = proplists:delete(positions, Opt0),
    Opt = proplists:delete(func_list, Opt1),
    case {Subjects,PosSubjects++FuncSubjects} of 
        {undefined,_} -> Opt;
        {all,[]} -> Opt ++ [{subject, all}];
        {all,Sub} -> Opt ++ [{subject, lists:usort(Sub)}];
        {Sub1,Sub2} -> Opt ++ [{subject, lists:usort(Sub1++Sub2)}]
    end.

transform_to_subjects0(matrixfilter, FunList) ->
    transform_to_subjects0(sw_metrics, FunList);
transform_to_subjects0(sw_metrics, FunList)->
    convert_funs_to_gnodes(func, FunList);
transform_to_subjects0(matrix, FunList)-> 
    convert_funs_to_gnodes(tle, FunList);
transform_to_subjects0(suffix_tree, SubList)->
    convert_forms_to_clauses(SubList,[]).


convert_funs_to_gnodes(func, Funs)->
    [?Query:exec(?Query:seq(?Mod:find(Mod), ?Fun:find(Fun,Arity)))
            || {Mod, Fun, Arity} <- Funs];
convert_funs_to_gnodes(tle, Funs)->
    [?Query:exec(?Query:seq([?Mod:find(Mod), ?Fun:find(Fun,Arity),
                                 ?Fun:definition(), ?Form:clauses(),
                                 ?Clause:body()]))
        || {Mod, Fun, Arity} <- Funs].

%file és funckok is?
convert_forms_to_clauses([],Res)-> [R || R<-Res, R /= [] ];
convert_forms_to_clauses([H|T],Res)->
    {Forms,Others} = lists:partition(fun(K)-> ?Graph:class(K) == form end, H),
    Clauses = [ [Clause] || Clause <- ?Query:exec(Forms,?Form:clauses())],
    convert_forms_to_clauses(T,Res++Clauses++[Others]).

check_common_preconds()->
    ?TrueOrThrow(db_not_empty(), ?LocalErr0r(empty_db)).

db_not_empty()->
    length(?Query:exec([file]))>0.

