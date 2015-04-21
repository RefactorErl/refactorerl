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

%%% @doc Library functions of Clone IdentifiErl

%%% @author Viktoria Fordos <f-viktoria@elte.hu>

-module(refusr_clone_identifierl_lib).
-vsn("$Rev: 9316 $").
-include("user.hrl").
-include("refusr_clone_identifierl.hrl").

% interface
-export([error_text/2, validate_and_expand_options/2,
         generate_name_by_time/0, write_to_file/3,get_files_from_options/1]).
% caching
-export([make_unit_storage/2, free_storage/1, make_match_spec/1]).
% internal
-export([format_worker/2]).
% dets
-export([insert_dets/1, match_key_dets/1, match_name_dets/3, get_group_dets/3,
         used_name_dets/3, delete_diff_db_hash_clones/0, add_alias_and_return/2,
         get_results_dets/0, create_key/1]).
% ui algorithm data calls
-export([get_algorithms/0, get_algorithm_data/1]).
%grouping
-export([group_clones/1]).
%position
-export([get_clones_by_pos/2, get_incl_form/1, get_file/1]).
% formating
-export([format_results/3, to_tlexpr_clones/2, to_uni_format/1, format/3]).

-define(TABNAME,
        filename:join([?MISC:data_dir(), "clone_identifierl"++ "v1"++
                                         ?MISC:graph_based_postfix()])).
-define(DUPFORMTYPES, [macro,record,module,export,include,import,vsn,undef,lex]).
-define(COUNTER, igraph_counter).
-define(DATA, igraph_data).

-export([extend_file_and_loc/2,get_real_form/1,unzip_clones/1,format_to_tlexpr/2,
         pretty_print/1,unitize_clone/2,get_name_from_dets/1,current_time_str/0]).

-export([deep_exprs/1, deep_sub/1]).
%%% ============================================================================
%%% Result formatting

make_unit_storage(Name, Options)->
    AlphabetOpts = proplists:get_value(alphabet_opts, Options,[]),
    StorageOpts = proplists:get_value(storage_opts, Options,[]),
    AlphaBet = ?Alphabet:convert_graph(AlphabetOpts),
    UnitsWithAlphabet = ?MISC:index_list(AlphaBet),
    Storage = ets:new(Name,
                      [set, {read_concurrency, true}, {keypos, ?unit_key_pos}]
                        ++StorageOpts),
    ets:insert(Storage, UnitsWithAlphabet),
    {Storage, length(UnitsWithAlphabet)}.

free_storage(Storage)->
    ets:delete(Storage).

%% 8> ets:fun2ms(fun({S,N}) when S == 1 orelse S == 2 orelse S == 3-> {N, 2} end).
%% [{{'$1','$2'},
%%   [{'orelse',{'==','$1',1},
%%              {'orelse',{'==','$1',2},{'==','$1',3}}}],
%%   [{{'$2',2}}]}]
%% if record is used
%% ets:fun2ms(fun({#unit{id=Subject},Index}) -> {Subject, Index} end).
%% [{{{unit,'$1','_','_'},'$2'},[],[{{'$1','$2'}}]}]

make_match_spec(Subjects) when length(Subjects) >0 ->
    [{{{unit,'$1','_','_'},'$2'},
      [make_match_spec_cond_rec(Subjects)],
     [{{'$2','$1'}}]}].

make_match_spec_cond_rec([Subject]) ->
    match_spec_eq_cond(Subject);
make_match_spec_cond_rec([Subject1, Subject2]) ->
    {'orelse', match_spec_eq_cond(Subject1), match_spec_eq_cond(Subject2)};
make_match_spec_cond_rec([Subject| Subjects]) ->
    {'orelse', match_spec_eq_cond(Subject), make_match_spec_cond_rec(Subjects)}.

match_spec_eq_cond(Subject) ->
    {'==','$1',{const, Subject}}.
%%% ============================================================================
%%% Result formatting

%% @doc Fromats a clone result to the UserFormat if given.
format_results(Results, Options, UserFormat) ->
    Format = case UserFormat of
                undef -> proplists:get_value(format, Options);
                _ -> UserFormat
            end,
    PosType = proplists:get_value(postype, Options),
    Clones = proplists:get_value(detected_clones, Results),
    Formatted = format(Clones, Format, PosType),
    Temp = proplists:delete(detected_clones,Results),
    Temp++[{detected_clones,Formatted}].

format(ClonesList, Style, PosType) ->  
    format(ClonesList, [{format, Style},{postype, PosType}]).

format(ClonesList, Options) when is_list(Options) ->           
    MakeProperArgs = fun(Arg)-> [Arg, Options] end,
    ZipClonesList = lists:zip(ClonesList,lists:seq(1,length(ClonesList))),
    Result = ?MISC:parallelise(ZipClonesList, ?MODULE, format_worker, MakeProperArgs),
    {_, FormattedClones} = lists:unzip(Result),
    FormattedClones.

format_worker(ClonesList, Options)-> 
    UnzippedClones = [{unzip_clones(Clone),Num} || {Clone,Num}<-ClonesList],
    case proplists:get_value(format, Options) of
        prettyprint ->
            [try 
                Formatted = [pretty_print(C) || C<-Clone],
                io_lib:format("~p. Clone group:~n", [Num])++
                io_lib:format("~s", [Formatted])++
                delimiter2()
            catch
                _:_ -> ""
            end || {Clone, Num}<-UnzippedClones, Clone /=[] ];
        file_and_loc ->
            ExtClones = extend_file_and_loc_map(UnzippedClones, Options),
            remove_nodes(ExtClones);
        ui_format ->
            ExtClones = extend_file_and_loc_map(UnzippedClones, Options),
            NoNodeClones = remove_nodes(ExtClones),
            {_,C} = lists:unzip(NoNodeClones),
            C;
        nodes -> 
            {Clone, _} = lists:unzip(UnzippedClones),
            lists:map(fun(K) ->
                lists:map(fun(L) ->
                    lists:map(fun(J) -> J#unit.id end, L) 
                end, K)
            end, Clone);
        original -> {Clones, _} = lists:unzip(ClonesList),
                    Clones;
        _ -> throw(?LocalError(unexpected_format,[]))
    end.

%% @doc Writes the formatted result to a file.
write_to_file(Formatted, Result, Options) ->
    Path =
        case proplists:get_value(output, Options, '') of
            '' -> "";
            FileName ->
                FormattedResult = format_results(Result, Options, prettyprint),
                FormattedClones = proplists:get_value(detected_clones, FormattedResult),
                FilePath = filename:join([?MISC:data_dir(), FileName]),
                {ok,IODev} = file:open(FilePath, [write]),
                io:put_chars(IODev, ?MISC:any_to_string(FormattedClones)),
                ok = file:close(IODev),
                FilePath
       end,
    Formatted ++ [{output_file_path, Path}].


flat_text(Node = {_,Type, _})
    when Type =:= expr; Type =:= clause; Type =:= form; Type =:= lex->
    flat_text0(Node);
flat_text(Node={_,Type,_}) when Type=:=func ->
    [Form] = ?Query:exec(Node, ?Fun:definition()),
    flat_text(Form).


%% @doc Unzip a clone to prepare formatting.
unzip_clones(Clone) when is_list(Clone) ->
    Head = hd(Clone),
    Length = length(Head#clone_item.items),
    unzip_nth_unit(Clone, 1, Length, []).

unzip_nth_unit(_, Act, Max, Res) when Act > Max ->
    lists:reverse(Res);
unzip_nth_unit(Clone, Act, Max, Res) ->
    Units = unzip_nth_unit(Clone, Act, []),
    unzip_nth_unit(Clone, Act+1, Max, [Units|Res]).

unzip_nth_unit([], _, Res) ->
    lists:reverse(Res);
unzip_nth_unit([Head | Tail], Act, Res) ->
    ActClone = lists:nth(Act, Head#clone_item.items),
    unzip_nth_unit(Tail, Act, [ActClone|Res]). 


%% @doc Prettyprints a clone element.
pretty_print(Clone) -> 
     [File] = get_file(hd(Clone)),
     FileName = filename:basename(?File:path(File)),
    delimiter()++
    io_lib:format("Clone element: (found in ~s):~n",[FileName])++
    format_ent(Clone)++
    io_lib:format("~n",[])++
    delimiter().


%% @doc Returns a func's form.
get_node({_,Type,_} = Node) ->
    case Type of
        func -> [Form] = ?Query:exec(Node, ?Fun:definition()),
                Form;
        _ -> Node
    end.

%% @doc Formats all clones to file_and_loc format.
extend_file_and_loc_map(Clones, Options) ->
    PosType = proplists:get_value(postype, Options),
    [extend_file_and_loc(Clone, PosType) || Clone<-Clones].

%% @doc Formats a clone to file_and_loc format.
extend_file_and_loc({Clone, Num}, PosType) ->
    {Num,extend_file_and_loc(Clone, PosType)};

extend_file_and_loc([], _)-> [];
extend_file_and_loc([ICPE|Tail], PosType)->
    A = hd(ICPE),
    Hd = get_node(A#unit.id),
    [File] = get_file(A),
    Path = {filepath, ?File:path(File)},
    StartLex = get_orig(?Query:exec(Hd,?Syn:first_leaf())),
    {StartPos,_} = ?Token:pos(File, StartLex, PosType),
    Start = {startpos, StartPos},
    EndLex =
        case length(ICPE) of
            1 ->
                get_orig(?Query:exec(Hd,?Syn:last_leaf()));
            _ ->
                B = lists:last(ICPE),
                Last = B#unit.id,
                LastLexList = ?Syn:leaves2(Last), % use last leaf?
                lists:last(LastLexList)
        end,
    {_,EndPos} = ?Token:pos(File, EndLex, PosType),
    End = {endpos, EndPos},
    Node = lists:flatten(lists:map(fun(K) -> K#unit.id end, ICPE)),
    Nodes = {nodes, Node},
    [[Path,Start,End,Nodes]]++extend_file_and_loc(Tail, PosType).

get_orig([Node]) ->
    case ?Query:exec(Node,[orig]) of
        [] -> Node;
        [A] -> A
    end.

%% @doc Returns the file node of a Node
get_file(#unit{id=Lex}) when ?IS_NODE(Lex), element(2,Lex) == lex->
    [Form] = ?Query:exec(Lex,?Token:form()),
    get_file(Form);
get_file(#unit{id=Expr}) when ?IS_NODE(Expr), element(2,Expr) == expr->
    [Form] = ?Query:exec(Expr,?Query:seq([?Expr:clause(),?Clause:form()])),
    get_file(Form);
get_file(#unit{id=Clause}) when ?IS_NODE(Clause), element(2,Clause) == clause->
    [Form] = ?Query:exec(Clause,?Clause:form()),
    get_file(Form);
get_file(#unit{id=Form}) when ?IS_NODE(Form), element(2,Form) == form->
    get_file(Form);
get_file(#unit{id=Module}) when ?IS_NODE(Module), element(2,Module) == module->
    ?Query:exec(Module,?Mod:file());
get_file(#unit{id=File}) when ?IS_NODE(File), element(2,File) == file->
    File;
get_file(#unit{id=Func}) when ?IS_NODE(Func), element(2,Func) == func->
    case ?Query:exec(Func,?Query:seq([?Fun:definition(),[fdep],[iref]])) of
        [] -> ?Query:exec(Func,?Query:seq([?Fun:definition(),?Form:file()]));
        File -> File
    end;
get_file(Form) when ?IS_NODE(Form), element(2,Form) == form->
    NForm = get_incl_form(Form),
    ?Query:exec(NForm,?Form:file()).


get_incl_form(Form) ->
    case ?Query:exec(Form,[forig]) of
        [] -> Form;
        [Forig|_] -> Forig
    end.

format_ent(List)->
    S=string:join([flat_text(Node) || #unit{id = Node} <- List], ",\n"),
    string:sub_string(S,string:span(S, " \t\n")+1).

delimiter()->
    string:concat(string:copies("-",80),"\n").
delimiter2()->
    string:concat(string:copies("*",80),"\n").


current_time_str()->
    {{Y,M,D},{H,N,S}} = erlang:localtime(),
    String =
        integer_to_list(Y)++"_"++
        integer_to_list(M)++"_"++
        integer_to_list(D)++"_"++
        integer_to_list(H)++"_"++
        integer_to_list(N)++"_"++
        integer_to_list(S),
    String.


%%% Error handling

error_text(badarg, [Key, Val])->
    io_lib:format("Bad value (~p) was given for an argument (~p)~n.",[Val, Key]);
error_text(badarg, [Key])->
    io_lib:format("Bad value was given for the following argument: ~p~n.",[Key]);
error_text(badarg, [])->
    io_lib:format("Bad argument was given. ~n",[]);
error_text(bad_format, [Format]) ->
    io_lib:format("Bad value was given as format: ~p.~n",[Format]);
error_text(bad_group, [Group]) ->
    io_lib:format("Bad value was given as groupnumber: ~p.~n",[Group]);
error_text(dets, [Reason]) ->
    io_lib:format("Problem with DETS: ~p. ~n",[Reason]);
error_text(unexpected_format, []) ->
    io_lib:format("Problem with format. ~n",[]).

%%% ============================================================================
%%% Input validation
validate_and_expand_options(Module, Options)->
    Defaults = Module:default_options(),
    Expanded = expand_options(Options, Defaults),
    validate_options(Module, Expanded),
    Expanded.

expand_options(Options, Defaults)->
    lists:foldl(fun({Key, Val}, OptsIn) ->
        case proplists:get_value(Key, OptsIn) of
            undefined -> [{Key, Val} | OptsIn];
            _ -> OptsIn
        end
    end, Options, Defaults).

badarg(Key)->
    ?LocalError(badarg, [Key]).
%badarg(Key, Val)->
%    ?LocalError(badarg, [Key, Val]).

validate_options(?CloneIdentifiErl, Options) when is_list(Options)->
    MandatoryOptKeys = [algorithm, cache, format, name, postype],
    OptionalOptKeys = [func_list, output],
    lists:all(fun(Key)->
        ?TrueOrThrow(validate(Key, proplists:get_value(Key, Options, undef)),
            badarg(Key))
              end, MandatoryOptKeys)  andalso
        lists:all(fun(Key)->
            ?TrueOrThrow(((not proplists:is_defined(Key, Options)) orelse
                validate(Key, proplists:get_value(Key, Options, undef))),
                badarg(Key))
                  end, OptionalOptKeys) orelse throw(?LocalErr0r(badarg));

validate_options(?CachedTLEClones, Options) ->
    validate_options(?TLEClones, Options);
validate_options(?TLEClones, Options) when is_list(Options)->
    MandatoryOptKeys = [subject, metric, method, diff_limit, max_invalid_seq_length],
    OptionalOptKeys = [alphabet_opts, positions, files],
    lists:all(fun(Key)->
        ?TrueOrThrow(validate(Key, proplists:get_value(Key, Options, undef)),
            badarg(Key))
              end, MandatoryOptKeys) andalso
        lists:all(fun(Key)->
            ?TrueOrThrow(((not proplists:is_defined(Key, Options)) orelse
                             validate(Key, proplists:get_value(Key, Options, undef))),
                badarg(Key))
                  end, OptionalOptKeys) orelse
        throw(?LocalErr0r(badarg));

validate_options(?FunClones, Options) when is_list(Options)->
    OptionalOptKeys = [subject, unit, max_rank, positions, files],
        lists:all(fun(Key)->
            ?TrueOrThrow(((not proplists:is_defined(Key, Options)) orelse
                             validate(Key, proplists:get_value(Key, Options, undef))),
                badarg(Key))
                  end, OptionalOptKeys) orelse
        throw(?LocalErr0r(badarg));

validate_options(?Alphabet, Opts) when is_list(Opts)->
    NullArityFuns = [file_filter, form_filter, clause_filter,
                     expr_filter, token_filter, alphabet],
    OneArityFuns = [tokenizer],
    ValidUnits = [file, form, clause, expr, token],
    ValidTypes = [to_alphabet, to_unit, to_mod_and_fun_preserving_alphabet,
                  to_fun_preserving_alphabet],
    (?TrueOrThrow(lists:member(proplists:get_value(unit, Opts, undef), ValidUnits),
        badarg(unit)) andalso
         ?TrueOrThrow(lists:member(proplists:get_value(type, Opts, undef), ValidTypes),
            badarg(type)) andalso
         lists:all(fun(Key)->
            ?TrueOrThrow(is_function(proplists:get_value(Key, Opts, fun(_)->ok end), 1),
                badarg(Key))
                   end, OneArityFuns) andalso
         lists:all(fun(Key)->
            ?TrueOrThrow(is_function(proplists:get_value(Key, Opts, fun()->ok end),0),
                badarg(Key))
                   end, NullArityFuns))  orelse
        throw(?LocalErr0r(badarg));
validate_options(?STree, Options) when is_list(Options) ->
    MandatoryOptKeys = [minlen, minnum, files, name],
    OptionalOptKeys = [],
    lists:all(fun(Key)->
            ?TrueOrThrow(validate(Key, proplists:get_value(Key, Options, undef)),
                badarg(Key))
                  end, MandatoryOptKeys) andalso
        lists:all(fun(Key)->
            ?TrueOrThrow(((not proplists:is_defined(Key, Options)) orelse
                             validate(Key, proplists:get_value(Key, Options, undef))),
                badarg(Key))
                  end, OptionalOptKeys) orelse
        throw(?LocalErr0r(badarg));
validate_options(?FSTree, Options) when is_list(Options) ->
    MandatoryOptKeys = [minlen, minnum, files, name, max_invalid_seq_length],
    OptionalOptKeys = [],
    lists:all(fun(Key)->
            ?TrueOrThrow(validate(Key, proplists:get_value(Key, Options, undef)),
                badarg(Key))
                  end, MandatoryOptKeys) andalso
        lists:all(fun(Key)->
            ?TrueOrThrow(((not proplists:is_defined(Key, Options)) orelse
                             validate(Key, proplists:get_value(Key, Options, undef))),
                badarg(Key))
                  end, OptionalOptKeys) orelse
        throw(?LocalErr0r(badarg));
validate_options(?MatrixFilter, Options) when is_list(Options)->
    MandatoryOptKeys = [subject, metric, method, diff_limit, max_invalid_seq_length],
    OptionalOptKeys = [subject, unit, max_rank, positions, files],
    lists:all(fun(Key)->
        ?TrueOrThrow(validate(Key, proplists:get_value(Key, Options, undef)),
            badarg(Key))
              end, MandatoryOptKeys) andalso
        lists:all(fun(Key)->
            ?TrueOrThrow(((not proplists:is_defined(Key, Options)) orelse
                             validate(Key, proplists:get_value(Key, Options, undef))),
                badarg(Key))
                  end, OptionalOptKeys) orelse
        throw(?LocalErr0r(badarg));

validate_options(_,_)->
    throw(?LocalErr0r(badarg)).

validate(func_list, [])->
  false;
validate(func_list, Funs) when is_list(Funs)->
  lists:all(fun(E) when is_tuple(E)->
                    Items = tuple_to_list(E),
                    [Mod, Fun, Arity |_] = Items,
                    (length(Items) == 3) andalso
                        lists:all(fun erlang:is_atom/1, [Mod, Fun]) andalso
                        is_integer(Arity) andalso (Arity >= 0);
                (_)->
                     false
        end, Funs);
validate(func_list, _)->
  false;
validate(cache, Val) ->
  lists:member(Val, [false, true]);
validate(algorithm, Algorithm)->
    lists:member(Algorithm, [sw_metrics, matrix, suffix_tree,suffix_tree2, filtered_suffix_tree, matrixfilter]);
validate(format, Format) ->
    lists:member(Format, [prettyprint, file_and_loc, ui_format, nodes, original]);
validate(subject, [{'$gn', _ , _ }|_])->
    true;
validate(subject, Subject) ->
    Subject =:= all;
validate(metric, Metric) ->
    (Metric =:= leveinstein) orelse (Metric =:= dice_sorensen)
        orelse is_function(Metric, 2);
validate(method, Method) ->
    (Method =:= quick);
validate(diff_limit, Limit) when is_float(Limit) -> 
    (Limit >= 0) andalso (Limit =< 1);
validate(diff_limit, _Limit) ->
    false;
validate(max_invalid_seq_length, Length) ->
    is_integer(Length) andalso (Length >= 0);
validate(alphabet_opts, AOpts) ->
    is_list(AOpts);
validate(unit, Unit) ->
    Unit =:= form;
    %lists:member(Unit, [form, clause]);
validate(max_rank, MaxRank) when is_integer(MaxRank)->
    (MaxRank >= 1) andalso (MaxRank =< 5);
validate(max_rank, MaxRank) when is_list(MaxRank)->
    {Num, _} = string:to_integer(MaxRank),
    case Num of
        error -> false;
        _ -> validate(max_rank,Num)
    end;
validate(minlen, Minlen) ->
    is_integer(Minlen) andalso (Minlen > 0);
validate(minnum, MinNum) ->
    is_integer(MinNum) andalso (MinNum > 1);
validate(files, Files) when is_list(Files) -> %maybe not enough maybe too much
    lists:all(fun(File) ->
        is_atom(File) orelse is_list(File) end, Files);
validate(output, Output) ->
   is_atom(Output);
validate(name, Name) ->
    is_atom(Name);
validate(postype,PosType) ->
    lists:member(PosType,[linecol, scalar]);
validate(positions,Positions) when is_list(Positions) ->
    lists:all(fun({Path,Start,End}) -> 
                is_list(Path) andalso 
                    ((is_integer(Start) andalso is_integer(End)) orelse
                     (is_tuple(Start) andalso is_tuple(End)))
                end, Positions);
validate(enforce, Enforce) ->
    is_boolean(Enforce);
validate(_,_)->
    false.



%%% ============================================================================
%%% Formatting to unified format

to_uni_format([[]]) -> [];
to_uni_format(EFTICs) ->
    [uni_format_clone(K, 1, length(hd(K)) ,[]) || K <- EFTICs].


%% @doc Creates a clone_item record from a clone group.
uni_format_clone(_, Act, Max, Result) when Act > Max ->
    lists:reverse(Result);
uni_format_clone(Clone, Act, Max, Result) ->
    Units = unitize_clone(Clone, Act),
    Ci = #clone_item{items = Units, score = 1.0},
    uni_format_clone(Clone, Act+1, Max, [Ci|Result]).


%% @doc Makes an unit record from a clone group's nth elements.
unitize_clone([], _) -> [];
unitize_clone([[]|Tail], Act) -> unitize_clone(Tail, Act);
unitize_clone([Clone | Tail], Act) when Clone /= [] ->
    ActClone = lists:nth(Act, Clone),
    Alphabet = case ?Graph:class(ActClone) of
        func -> [];
        _ -> ?Alphabet:to_alphabet0(?Alphabet:default_alphabet(), ?Syn:leaves2(ActClone), [])
    end,
    Parent = get_parent(ActClone), 
    Unit = #unit{id = ActClone, parent = Parent, alphabet = Alphabet},
    [Unit] ++ unitize_clone(Tail, Act).
    

%% @doc Returns the parent of the Node.
get_parent(Node) ->
    {_, Type, _} = Node,
    case Type of
        func -> [Parent] = ?Query:exec(Node, ?Fun:module()),
                Parent;
        _ -> [{_, Parent}] = ?Syn:parent(Node),
            Parent
    end.


%% @doc Removes the nodes from the results.
remove_nodes([]) -> [];
remove_nodes([{Num,Clone} | Clones]) ->
        NoNodeClone = [proplists:delete(nodes, Cl) || Cl <- Clone],
        [{Num,NoNodeClone}] ++ remove_nodes(Clones).

%%% ============================================================================
%%% Trim clones to top level expressions
to_tlexpr_clones(Clones, Algorithm) ->
    lists:flatmap(fun(K) -> to_tlexpr_clone(K, Algorithm) end, Clones).

to_tlexpr_clone(Clone, Algorithm)->
    case need_to_tlexpr(hd(Clone)) of
        true -> format_to_tlexpr(Clone, Algorithm);
        _ -> [Clone]
    end.

%% @doc Determines if the clone item needs trim.
need_to_tlexpr(#clone_item{items = Items}) ->
     Bool = lists:any(fun(#unit{id = Id}) ->
                         is_node_type([Id], [form,clause,func])
                     end, Items),
     Bool.

%% @doc Trims a clone to top level expressions.
format_to_tlexpr([#clone_item{items = Items}], Algorithm) ->
    ClauseList = lists:map(fun(#unit{id = Id}) ->
                           get_clauses(Id) end, Items),
    Exprs =
        case Algorithm of
            fstree ->
                group_clauses(ClauseList);
            mfilter ->
                get_exprs(ClauseList)
        end,
    to_uni_format(Exprs).

%% @doc Returns every clause of a form or func.
get_clauses({_, Type, _} = Id) ->
    case Type of
        form ->
            NewId = hd(get_real_form(Id)),
            ?Query:exec(NewId, ?Form:clauses());
        func -> ?Query:exec(Id, ?Query:seq(
                    ?Fun:definition(), ?Form:clauses()));
        _ ->  [Id]
    end.

%% @doc Trims every clauses to bodies. 
group_clauses(ClauseList) ->
    Len = length(hd(ClauseList)),
    group_clauses_by_n(ClauseList, 1, Len).

group_clauses_by_n(_, N, Max) when N > Max -> [];
group_clauses_by_n(ClauseList, N, Max) -> 
    [get_n_group(ClauseList, N, Max)] ++ group_clauses_by_n(ClauseList, N+1, Max).

get_n_group([], _, _) -> [];
get_n_group([H|T], N, Max) ->
    [get_bodies(lists:nth(N, H))]++get_n_group(T, N, Max).


%% @doc Returns every body of the clause.
get_bodies(Clause) -> 
    ?Query:exec(Clause, ?Clause:body()).


%% @doc Returns groups of expressions form the group of clauses.
get_exprs(GroupClause) ->
    case is_diff_length(GroupClause) of
        true -> DiffLenExprs = group_diff_len_clauses(GroupClause, []),
                do_window(DiffLenExprs);
        _ -> Exprs = [[?Query:exec(Group,?Clause:body()) || Group <- GroupClause]],
             case need_window(Exprs) of
                true -> do_window([lists:flatten(K) || K <- Exprs]);
                _ ->
                    case length(Exprs) == 1 andalso is_diff_length(hd(Exprs)) of
                        true -> do_window(hd(Exprs));
                        _ -> Exprs
                    end
             end
    end.



%% @doc Different length clauses handled differently, 
%%      using windows technique.
group_diff_len_clauses([], Res) -> lists:reverse(Res);
group_diff_len_clauses([Clause | Clauses], Res) ->
    Exprs = [lists:flatmap(fun(K) -> get_bodies(K) end, Clause)],
    group_diff_len_clauses(Clauses, Exprs++Res).


%% @doc Determines if a group of expressions needs to be windowed.
need_window(Exprs) ->
    not lists:all(fun(K) -> same_structure(K, hd(Exprs), length(K), true) end, Exprs).


%% @doc Determines if Expr has the same tlexpr structure as Head.
same_structure(_, _, _, false) -> false;
same_structure(_, _, 0, Result) -> Result;
same_structure(Expr, Head, Length, _) ->
   Bool = length(lists:nth(Length, Expr)) == length(lists:nth(Length, Head)),
   same_structure(Expr, Head, Length-1, Bool).


%% @doc Given a clone pair, generated all same length expression clonegroups,
%%      using windows technique.
do_window([Clone1 , Clone2]) ->
    case length(Clone1) < length(Clone2) of
        true -> 
            do_window(Clone1, Clone2, length(Clone2)-length(Clone1)+1, []);
        _ -> do_window(Clone2, Clone1, length(Clone1)-length(Clone2)+1, [])
    end.

do_window(_, _, 0, Res) -> lists:reverse(Res);
do_window(MinClone, Clone, DiffLen, Res) ->
    Exprs = [[MinClone] ++ [lists:sublist(Clone, DiffLen, length(MinClone))]],
    do_window(MinClone, Clone, DiffLen-1, Exprs++Res).

%% @doc Determines if the node's type in the GoodTypes list.
is_node_type([{_, Type, _}|_], GoodTypes) ->
    lists:member(Type, GoodTypes).


%% @doc Determines if a list contains different length lists.
is_diff_length(List) ->
        HeadLengths = lists:filter(fun(K) -> length(K) == length(hd(List)) end, List),
    length(HeadLengths) /= length(List).


%%% ============================================================================
%%% Dets Utilities

%% @doc Opens DETS table.
open_dets()->
    case dets:open_file(?TABNAME, [{keypos, 1}]) of
        {ok, _} -> ok;
        _ -> throw(?LocalError(dets,[open]))
    end.


%% @doc Closes DETS table.
close_dets()->
    case dets:close(?TABNAME) of
        ok -> ok;
        _ -> throw(?LocalError(dets,[close]))
    end.

%% @doc Inserts a clone into the dets table.
insert_dets(#ci_results{key=Key, name = Name} = Clone)->
    open_dets(),   
    case dets:insert(?TABNAME, {Key, Clone, [Name]}) of
        ok -> ok;
        _ -> throw(?LocalError(dets,[insert]))
    end,
    close_dets().

%% @doc If a key is already used returns the clone search result.
match_key_dets(Key)->
    open_dets(),
    Result =
        case dets:lookup(?TABNAME, Key) of
            [] -> {false, []};
            [Res] -> {true, Res};
            _ -> throw(?LocalError(dets,[match]))
        end,
    close_dets(),
    Result.

%% @doc Searchs for the name in dets and returns the formatted result.
match_name_dets(Name, Format, PosType)->
    delete_diff_db_hash_clones(),
    case validate(format, Format) orelse Format == undef of
        true -> ok;
        _ -> throw(?LocalError(bad_format,[Format]))
    end,
    case validate(postype, PosType) orelse PosType == undef of
        true -> ok;
        _ -> throw(?LocalError(bad_postype,[PosType]))
    end,
    case get_name_from_dets(Name) of
        {false, Reason} ->
            {false, Reason};
        {true, Result, Options} ->
            NewOpt =
                case PosType of
                    undef -> Options;
                    _ -> proplists:delete(postype, Options)++[{postype,PosType}]
                end,
            {true, format_results(Result, NewOpt, Format)++[{options,NewOpt}]}
    end.

%% @doc Gets result Name from DETS.
get_name_from_dets(Name) ->
    open_dets(),
    AllData = dets:match(?TABNAME, {'$1', '$2', '$3'}),
    close_dets(),
    MatchedClone =
        lists:filter(fun([_, _, Names]) ->
            lists:member(Name, Names)
        end, AllData),
    Result =
        case MatchedClone of
            [] -> {false, undef_name};
            [[_, Res, _]] -> #ci_results{opts = Opts, results = Results} = Res,
                         {true, Results++[{clone_name, Name}], Opts};
             _ -> throw(?LocalError(dets,[get_name]))
         end,
    Result.

%% @doc Searchs for the name and then returns the nth group formatted.
get_group_dets(Name, GroupNum, UserFormat) ->
    delete_diff_db_hash_clones(),
     Result = 
        case validate(format, UserFormat) orelse UserFormat == undef of
            true -> get_name_from_dets(Name);
            _ -> throw(?LocalError(bad_format,[UserFormat]))
        end,

    case Result of
        {true, Res, Opts} ->
            Length = proplists:get_value(detected_clones_num, Res),
            case GroupNum =< 0 orelse GroupNum > Length of
                 true -> throw(?LocalError(bad_group,[GroupNum]));
                 false-> Clones = proplists:get_value(detected_clones, Res),
                         CloneGroup = lists:nth(GroupNum, Clones),
                         Format = get_format(UserFormat, Opts),
                         PosType = proplists:get_value(postype, Opts),
                         [FormCloneGroup] = format([CloneGroup], Format, PosType),
                         {true, FormCloneGroup}
            end
    end.

%% @doc Returns the format from Options or Format.
get_format(Format, Options) ->
    case Format of 
        undef -> proplists:get_value(format, Options);
        _ -> Format
    end.

%% @doc Decides if a name is used already.
used_name_dets(Name, Key, Enforce) ->
    open_dets(),
    Results = dets:match(?TABNAME, {'$1', '$2', '$3'}),
    close_dets(),
    Bool = lists:any(fun([RKey, _, RName] = Clone) ->
                            case lists:member(Name,RName) of
                                true when Key /= RKey ->
                                   delete_name_dets(Clone,Enforce,Name);
                                true when Key == RKey -> false;
                                _ -> false
                            end
                    end, Results),
    Bool.

%% @doc Deletes an alias and if neccessary removes the clone.
delete_name_dets([RKey, Res, RName], Enforce, Name)-> 
    NoNames = lists:delete(Name, RName),
    open_dets(),
    Bool = case {NoNames,Enforce} of
        {[], true} -> dets:delete_object(?TABNAME, {RKey, Res, RName}),
                      false;
        {_, true} -> dets:delete_object(?TABNAME, {RKey, Res, RName}),
                     dets:insert(?TABNAME, {RKey, Res, NoNames}),
                     false;
        {_, false}-> true
    end,
    close_dets(),
    Bool.

%% @doc Returns every clone results name, options, clone number.
get_results_dets() ->
    delete_diff_db_hash_clones(),
    open_dets(),
    Results = dets:match(?TABNAME, {'$1', '$2', '$3'}),
    close_dets(),

    lists:map(fun([_Key, CloneResult, NameList]) -> 
                   #ci_results{opts = Opts, results = Res} = CloneResult,
                    GroupNum = proplists:get_value(detected_clones_num, Res),
                    [NameList, Opts, GroupNum] end, Results).

%% @doc Adds an alias to the same key search and returns with the result.
add_alias_and_return({Key, CiRes, Names} = Res, Name) -> 
    NewNames = lists:usort(Names ++ [Name]),
    open_dets(),
    dets:delete_object(?TABNAME, Res),
    dets:insert(?TABNAME, {Key, CiRes, NewNames}),
    close_dets(),
    CiRes#ci_results.results.

%% @doc Removes results with different DB-hash.
delete_diff_db_hash_clones() -> 
    CurrDbHash = ?MISC:database_hash(),
    open_dets(),
    Clones = dets:match(?TABNAME, {'$1', '$2', '$3'}), 
    close_dets(),
    lists:foreach(
        fun(Clone)->
            delete_diff_db_hash_clone(Clone, CurrDbHash)
        end, Clones).

%% @doc Removes results with given key and different DB-hash.
delete_diff_db_hash_clone([Key, CiResult, _], CurrDbHash) ->
    #ci_results{db_hash = DbHash} = CiResult,
    case CurrDbHash of
        DbHash -> ok;
        _ -> open_dets(),
             dets:delete(?TABNAME, Key),
             close_dets()
    end.

%% @doc Generates a string using the current time.
time_extend_right(Str)->
    string:right(Str,2,$0).
generate_name_by_time()->
    {{Y,M,D},{H,N,S}} = erlang:localtime(),
    NameString = "temp"
        ++integer_to_list(Y)
        ++time_extend_right(integer_to_list(M))
        ++time_extend_right(integer_to_list(D))
            ++time_extend_right(integer_to_list(H))
            ++time_extend_right(integer_to_list(N))
            ++time_extend_right(integer_to_list(S)),
    list_to_atom(NameString).

%% @doc Creates a key from the options, and returns the key
%%      and the modified optios
create_key(Options) ->
    UnneccessaryOpts = [format, name, output, cache, enforce, positions, postype],
    NewOptions = remove_options(Options, UnneccessaryOpts),
    _Key = erlang:md5(term_to_binary({NewOptions})).

%% @doc Removes Opt from Options, then sorts it.
remove_options(Options, []) -> lists:usort(Options);
remove_options(Options, [Opt | Opts]) ->
    RemovedOpts = proplists:delete(Opt, Options),
    remove_options(RemovedOpts, Opts).


%%% ============================================================================
%%% Grouping clones using Igraph.

%% @doc Groups clones based on clique search.
group_clones(Clones) -> 
    CounterEts = ets:new(?COUNTER, [set, public]),
    DataEts = ets:new(?DATA, [set, public]),
    ets:insert(CounterEts, {id, -1}),
    ClonesAsEdges =
        lists:flatmap(
            fun(K) -> convert_clone_to_edge(K,CounterEts,DataEts) end, 
        Clones),
    MaximalCliques = ?Igraph:maximal_cliques_to_file(ClonesAsEdges),
    GroupedClones = lists:map(fun(K)-> convert_clique(K,DataEts) end, MaximalCliques),
    FormattedGroups = to_uni_format(GroupedClones),
    ets:delete(CounterEts),
    ets:delete(DataEts),
    FormattedGroups.

%% @doc Returns an unique id for the NodeList.
get_id(NodeList,CounterEts,DataEts) ->
    case ets:lookup(DataEts, NodeList) of
        [] ->  [Id] = ets:update_counter(CounterEts, id, [{2, 1}]),
                ets:insert(DataEts, {NodeList, Id}),
                Id;
        [{_, Id}] -> Id
    end.

get_clones_n([], _) -> [];
get_clones_n([#clone_item{items = Items} | Tail], N) ->
    Unit = lists:nth(N, Items),
    Id = Unit#unit.id,
    [Id]++get_clones_n(Tail, N).

%% @doc Converts clones to edges using unique id-s.
convert_clone_to_edge(CloneItem,CounterEts,DataEts) ->
    Unit1s = get_clones_n(CloneItem, 1),
    Unit2s = get_clones_n(CloneItem, 2),
    NewId1 = get_id(Unit1s,CounterEts,DataEts),
    NewId2 = get_id(Unit2s,CounterEts,DataEts),
    [NewId1, NewId2].

%% @doc Converts cliques to clonegroups.
convert_clique(Clique,DataEts) ->
   lists:map(fun(K) -> get_element(K,DataEts) end, Clique).

%% @doc Returns the nodes corresponding to Id.
get_element(Id,DataEts) ->
  [{Node, _}] = ets:match_object(DataEts, {'$1', Id}),
  Node.


%%% ============================================================================
%%% Functions for interfaces

%% @doc Returns the algorithm's key and label.
get_algorithms() ->
    [{sw_metrics, [{key, sw_metrics}, {label, "Software metrics (providing only relevant results)"}]},
     {matrix, [{key, matrix}, {label, "Matrix (providing only relevant results)"}]},
     {suffix_tree, [{key, suffix_tree}, {label, "Suffix tree"}]},
     {filtered_suffix_tree, [{key, filtered_suffix_tree}, {label, "Suffix tree (providing only revelant results)"}]},
     {matrixfilter, [{key, matrixfilter}, {label, "Software metrics with syntactic filters (providing only relevant results)"}]}
     ].

%% @doc Common options of the algoritms.
get_algorithm_common_options() ->
    [{name, [{key, name}, {label, "Name"}, {default, ""}, {type, atom}]},
     {enforce, [{key, enforce}, {label, "Enforce name"}, {default, false}, {type, boolean}]},
     {output, [{key, output}, {label, "Output"}, {default, ""}, {type, atom}]}
    ].

get_algorithm_data(matrix) ->
    DefaultOptions = ?TLEClones:default_options(),
    DefMetric = proplists:get_value(metric, DefaultOptions),
    %DefMethod = proplists:get_value(method, DefaultOptions),
    DefDiffLimit = proplists:get_value(diff_limit, DefaultOptions),
    DefInvSeqLen = proplists:get_value(max_invalid_seq_length, DefaultOptions),

    [{metric, [{key, metric}, {label, "Metric"}, {default, DefMetric}, {type, enum},
         {options, [dice_sorensen, leveinstein]}, {enumtype, atom}]},
     %{method, [{key, method}, {label, "Method"}, {default, DefMethod}, {type, enum},
        %{options, [quick]}, {enumtype, atom}]},
     {diff_limit, [{key, diff_limit}, {label, "Diff. limit"}, {default, DefDiffLimit}, {type, float}]},
     {max_invalid_seq_length, [{key, max_invalid_seq_length}, {label, "Maximum invalid sequence length"},
        {default, DefInvSeqLen}, {type, integer}]},
     {cache, [{key, cache}, {label, "Cache"}, {default, false}, {type, boolean}]},
     {files, [{key, files}, {label, "Files"}, {default, []}, {type, atoms}]}
    ] ++ get_algorithm_common_options();

get_algorithm_data(sw_metrics) ->
    DefaultOptions = ?FunClones:default_options(),
    %DefUnit = proplists:get_value(unit, DefaultOptions),
    DefMaxrank = proplists:get_value(max_rank, DefaultOptions),

    [%{unit, [{key, unit}, {label, "Unit"}, {default, DefUnit}, {type, enum},
         %{options, [form]}, {enumtype, atom}]}, %[clause, form]
     {max_rank, [{key, max_rank}, {label, "Max Rank"}, {default, integer_to_list(DefMaxrank)}, {type, enum},
        {options, ["1","2","3","4","5"]}, {enumtype, string}]},
     {files, [{key, files}, {label, "Files"}, {default, []}, {type, atoms}]}
    ] ++ get_algorithm_common_options();

get_algorithm_data(matrixfilter) ->
    DefaultOptions = ?MatrixFilter:default_options(),
    DefUnit = proplists:get_value(unit, DefaultOptions),
    DefMaxrank = proplists:get_value(max_rank, DefaultOptions),
    DefInvSeqLen = proplists:get_value(max_invalid_seq_length, DefaultOptions),

    [{unit, [{key, unit}, {label, "Unit"}, {default, DefUnit}, {type, enum},
        {options, [clause, form]}, {enumtype, atom}]},
     {max_rank, [{key, max_rank}, {label, "Max Rank"}, {default, integer_to_list(DefMaxrank)},
     {type, enum}, {options, ["1","2","3","4","5"]}, {enumtype, string}]},
     {max_invalid_seq_length, [{key, max_invalid_seq_length}, {label, "Maximum invalid sequence length"},
        {default, DefInvSeqLen}, {type, integer}]},
     {files, [{key, files}, {label, "Files"}, {default, []}, {type, atoms}]}
    ] ++ get_algorithm_common_options();

get_algorithm_data(suffix_tree) ->
    DefaultOptions = ?STree:default_options(),
    DefMinlen = proplists:get_value(minlen, DefaultOptions),
    DefMinnum = proplists:get_value(minnum, DefaultOptions),
    DefFiles = proplists:get_value(files, DefaultOptions),

    [{minlen, [{key, minlen}, {label, "Minlen"}, {default, DefMinlen}, {type, integer}]},
     {minnum, [{key, minnum}, {label, "Minnum"}, {default, DefMinnum}, {type, integer}]},
     {files, [{key, files}, {label, "Files"}, {default, DefFiles}, {type, atoms}]}
    ] ++ get_algorithm_common_options();

get_algorithm_data(filtered_suffix_tree) ->
    DefaultOptions = ?FSTree:default_options(),
    DefMinlen = proplists:get_value(minlen, DefaultOptions),
    DefMinnum = proplists:get_value(minnum, DefaultOptions),
    DefFiles = proplists:get_value(files, DefaultOptions),
    DefInvSeqLen = proplists:get_value(max_invalid_seq_length, DefaultOptions),

    [{minlen, [{key, minlen}, {label, "Minlen"}, {default, DefMinlen}, {type, integer}]},
     {minnum, [{key, minnum}, {label, "Minnum"}, {default, DefMinnum}, {type, integer}]},
     {max_invalid_seq_length, [{key, max_invalid_seq_length}, {label, "Maximum invalid sequence length"},
        {default, DefInvSeqLen}, {type, integer}]},
     {files, [{key, files}, {label, "Files"}, {default, DefFiles}, {type, atoms}]}
    ] ++ get_algorithm_common_options().


%%% ============================================================================
%%% Getting nodes from position selection

get_clones_by_pos(Alg,{FilePath,Start,End}) ->
    [FileNode] = ?Query:exec(?File:find(FilePath)),
    [SStart,SEnd] =
        case is_tuple(Start) of
            true ->
                {EndLine,EndCol} = End,
                case ?Token:pos(FileNode,hd(?Query:exec(FileNode,?Syn:last_leaf())),linecol) of
                    {_,{L,_C}} when L < element(1,Start) ->
                        throw(?LocalErr0r(invalid_position));
                    {_,{L,C}} when L < EndLine ->
                        ?Token:file_lc2sc(FileNode, [Start,{L,C}]);
                    _ -> ?Token:file_lc2sc(FileNode, [Start,{EndLine,EndCol-1}])
                end;
            _ -> [Start,End-1]
        end,
    StartLex = get_lex(FileNode,SStart,left),
    EndLex =  get_lex(FileNode,SEnd,right),
    {StartExpr,EndExpr} =
        case ?File:type(FileNode) of
            header ->
                [StartExpr0] = lex_to_node_include(StartLex,left),
                [EndExpr0] = lex_to_node_include(EndLex,right),
                {StartExpr0,EndExpr0};
            _ ->
                [StartExpr0] = lex_to_node(StartLex,left),
                [EndExpr0] = lex_to_node(EndLex,right),
                {StartExpr0,EndExpr0}
        end,
    Res = case Alg of
        sw_metrics ->
           trim_sw_metrics(StartExpr,EndExpr);
        matrixfilter ->
           trim_sw_metrics(StartExpr,EndExpr);
        matrix ->
           trim_matrix(StartExpr,EndExpr)
    end,
    case Res of 
        [] -> throw(?LocalErr0r(invalid_position));
        _ -> Res
    end.

%%% ----------------------------------------------------------------------------
%%% Trimming functions for sw_metrics algorithm
trim_sw_metrics(Start,End) ->
    {Ancestor,{{_,LeftChild},{_,RightChild}}} =
        minimal_common_ancestor_with_children([Start,End]),
    Res = 
        case Ancestor of
            {file,Node} ->
                trim_file(Node,{LeftChild,Start},{RightChild,End});
            {form,Node} ->
                trim_form(Node,{LeftChild,Start},{RightChild,End},none);
            {funcl,Node} ->
                trim_clause(Node,Start,End); 
            {body,Node} ->
                Clause = ?Query:exec(Node,?Expr:clause()),
                trim_clause(Clause,Start,End);
            _ -> []
        end,
    lists:flatten([?Query:exec(Form,?Form:func()) || Form<-Res]).


transform_to_tlexpr([], Res) -> Res;
transform_to_tlexpr([Node | Nodes], Res) ->
    case Node of
        {_,expr,_} ->
            transform_to_tlexpr(Nodes,Res++[Node]);
        {_,clause,_} ->
            Exprs = ?Query:exec(Node, ?Clause:body()),
            transform_to_tlexpr(Nodes,Res++Exprs);
        {_,form,_} ->
            Exprs = ?Query:exec(Node, ?Query:seq([?Form:clauses(),?Clause:body()])),
            transform_to_tlexpr(Nodes,Res++Exprs)
    end.


trim_file(File,{LeftChild,LeftExpr},{RightChild,RightExpr}) ->
    Forms = ?Query:exec(File,?File:forms()),
    InForms = ?MISC:separate_interval(Forms,LeftChild,RightChild),
    case InForms of
        [] -> throw(?LocalErr0r(invalid_position));
        _ -> ok
    end,
    %check elsőre és utolsóra
    case {trim_form(hd(InForms),{nil,LeftExpr},{nil,RightExpr},left),
          trim_form(lists:last(InForms),{nil,LeftExpr},{nil,RightExpr},right)} of
        {[],[]} -> lists:delete(lists:last(InForms), lists:delete(hd(InForms),InForms));
        {[],_} -> lists:delete(hd(InForms),InForms);
        {_,[]} -> lists:delete(lists:last(InForms),InForms);
        _ -> InForms
    end.

trim_form(Form,{LeftChild,LeftExpr},{RightChild,RightExpr},Direction) ->
    Clauses = ?Query:exec(Form,?Form:clauses()),
    case Direction of
        none -> InClauses = ?MISC:separate_interval(Clauses,LeftChild,RightChild),
                case length(InClauses) == length(Clauses) of
                    true -> [Form];
                    _ -> []
                end;
        left -> [LeftClause] = ?Query:exec(LeftExpr, ?Expr:clause()),
                case hd(Clauses) == LeftClause of
                    true -> [Form];
                    _ -> []
                end;
        right ->[RightClause] = ?Query:exec(RightExpr, ?Expr:clause()),
                case lists:last(Clauses) == RightClause of
                    true -> [Form];
                    _ -> []
                end
    end.

trim_clause(Clause,_LeftExpr,_RightExpr) ->
    clause_to_form(Clause). % formmá kell alakítani ha egy klózos a form

 
clause_to_form(Clause) ->
    Form = ?Query:exec(Clause,?Clause:form()),
    Clauses = ?Query:exec(Form,?Form:clauses()),
    case length(Clauses) of
        1 -> Form;
        _ -> []
    end.

minimal_common_ancestor_with_children(TokenList)->
    Left = ?Syn:root_path(hd(TokenList),left),
    Right = ?Syn:root_path(lists:last(TokenList),right),
    case ?MISC:common_prefix(Left,Right) of
        [] -> {{root, ?Graph:root()},{hd(Left),hd(Right)}};
        Prefix ->
            %%TODO
            PLen =
                lists:min([length(Prefix),
                           length(Left)-1,
                           length(Right)-1]),
            {lists:last(Prefix),
             {lists:nth(PLen+1,Left),lists:nth(PLen+1,Right)}}
    end.


%%% ----------------------------------------------------------------------------
%%% Trimming functions for matrix algorithm
trim_matrix(Start,End) ->
    {Ancestor,{{_,LeftChild},{_,RightChild}}} =
        minimal_common_ancestor_with_children([Start,End]),
    Nodes =
        case Ancestor of
            {file,Node} ->
                trim_file_m(Node,{LeftChild,Start},{RightChild,End});
            {form,Node} ->
                trim_form_m(Node,{LeftChild,Start},{RightChild,End},none);
            {funcl,Node} ->
                trim_clause_m(Node,Start,End,none);
            {body,Node} -> [Node]
        end,
    transform_to_tlexpr(Nodes,[]).
    
trim_file_m(File,{LeftChild,LeftExpr},{RightChild,RightExpr}) ->
    Forms = ?Query:exec(File,?File:forms()),
    InForms0 = ?MISC:separate_interval(Forms,LeftChild,RightChild),
    case InForms0 of
        [] -> throw(?LocalErr0r(invalid_position));
        _ -> ok
    end,
    InForms = lists:sublist(InForms0,2,length(InForms0)-2),
    %check elsőre és utolsóra
    case {trim_form_m(hd(InForms0),{nil,LeftExpr},{nil,RightExpr},left),
          trim_form_m(lists:last(InForms0),{nil,LeftExpr},{nil,RightExpr},right)} of
        {[],[]} -> InForms;
        {[],A} -> InForms ++ A;
        {A,[]} -> A ++ InForms;
        {A,B} -> A++InForms++B
    end.

trim_form_m(Form,{LeftChild,LeftExpr},{RightChild,RightExpr},Direction) ->
    Clauses = ?Query:exec(Form,?Form:clauses()),
    InClauses = ?MISC:separate_interval(Clauses,LeftChild,RightChild),
    case {InClauses,Direction} of
        {[],none} -> throw(?LocalErr0r(invalid_position));
        _ -> ok
    end,
    case Direction of
        none ->
            case {trim_clause_m(hd(InClauses),LeftExpr,RightExpr,left),
                  trim_clause_m(lists:last(InClauses),LeftExpr,RightExpr,right)} of
                {[],[]} -> lists:delete(lists:last(InClauses), lists:delete(hd(InClauses),InClauses));
                {[],_} -> lists:delete(hd(InClauses),InClauses);
                {_,[]} -> lists:delete(lists:last(InClauses),InClauses);
                _ -> InClauses
            end;
        left ->
            [LeftClause] = ?Query:exec(LeftExpr,?Expr:clause()),
            case trim_clause_m(LeftClause,LeftExpr,nil,Direction) of
                [] -> {_,Next} = lists:splitwith(fun(K) -> K /= LeftClause end, Clauses),
                    case Next of 
                        [] -> [];
                        _ -> lists:sublist(Next,2,length(Next)-1)
                    end;
                Exprs ->
                    {_,Next} = lists:splitwith(fun(K) -> K /= LeftClause end, Clauses),
                    case Next of 
                        [] -> Exprs;
                        _ -> Exprs ++ lists:sublist(Next,2,length(Next)-1)
                    end
            end;
        right ->
            [RightClause] = ?Query:exec(RightExpr,?Expr:clause()),
            case trim_clause_m(RightClause,nil,RightExpr,Direction) of
                [] -> [];
                Exprs ->
                    {Prev,_} = lists:splitwith(fun(K) -> K /= RightClause end, Clauses),
                    Prev ++ Exprs
            end
    end.


trim_clause_m(Clause,LeftExpr,RightExpr,Direction) ->
    Exprs = ?Query:exec(Clause,?Clause:body()),
    case Direction of
        none ->  ?MISC:separate_interval(Exprs,LeftExpr,RightExpr);
        left ->
            case hd(Exprs) == LeftExpr of
                true -> Exprs;
                _ -> []
            end;
        right ->    
            case lists:last(Exprs) == RightExpr of
                true -> Exprs;
                _ -> []
            end
    end.


%%% ----------------------------------------------------------------------------
%%% Functions to get the closest top level expression from the position
get_node_from_include(NLex) ->
    NewLex = hd(?Query:exec(NLex,
                    ?Query:any([[{elex,back}],[{clex,back}],[{flex,back}],[{llex,back}]]))
               ),
    case ?Graph:class(NewLex) of
        lex -> get_node_from_include(NewLex);
        _ -> [NewLex]
    end.

lex_to_node_include(Lex, Direction) ->
    NLex =
        case ?Query:exec(Lex,[{orig,back}]) of
            [] -> Lex;
            Orig -> Orig
        end,
    [Node] = get_node_from_include(NLex),
    case ?Graph:class(Node) of
        expr -> 
            NExpr = to_top([Node],[Node]), 
            % if the expr is a name pattern of guard then we 
            % need to get the next expr based on direction
            case NExpr of
                [] -> get_node_from_file(hd(?Query:exec(Lex,?Token:form())),Direction);
                _ -> 
                case ?Query:exec(NExpr,?Query:any([[{name,back}],[{pattern,back}],[{guard,back}]])) of
                   [] -> NExpr;
                   Clause ->
                      get_node_from_form(get_real_form(?Query:exec(Lex,?Token:form())), Clause,Direction)
                end
            end;
        clause -> get_node_from_clause([Node],NLex,Direction);
        form -> get_node_from_form([Node],NLex,Direction)
    end.


%% @doc Sometimes a lex node does not connected to a position.
get_lex(File,Pos,Dir) ->
    Lex = ?Query:exec(File,?File:token(Pos)),
    %todo: check if need a stop
    % case Pos of
    %     1000 -> throw(error);
    %     _ ->ok
    % end,
    case {Lex,Dir} of
        {[],left} -> get_lex(File,Pos+1,Dir);
        {[],right} -> get_lex(File,Pos-1,Dir);
        _ -> Lex
    end.


llex_to_token(Lex) ->
    case ?Query:exec(Lex,[{llex,back}]) of
        [] -> hd(Lex);
        LLex -> llex_to_token(LLex)
    end.

%% @doc Searches for the nearest top level expression based on Lex and Direction.
lex_to_node(Lex0,Direction) ->
    Lex = [llex_to_token(Lex0)],
    Res = 
        case ?Query:exec(Lex,[{elex,back}]) of
            [] ->
                case ?Query:exec(Lex,?Token:clause()) of % if Lex == ; ( ) or -> 
                    [] -> get_node_from_form(?Query:exec(Lex,?Token:form()),Lex,Direction); % if .
                    Clause -> get_node_from_clause(Clause,Lex,Direction)
                end;
            Expr -> 
                NExpr = to_top(Expr,Expr), 
                % if the expr is a name pattern of guard then we 
                % need to get the next expr based on direction
                case NExpr of
                    [] -> get_node_from_file(hd(?Query:exec(Lex,?Token:form())),Direction);
                    _ -> 
                    case ?Query:exec(NExpr,?Query:any([[{name,back}],[{pattern,back}],[{guard,back}]])) of
                       [] -> NExpr;
                       Clause -> get_node_from_form(?Query:exec(Lex,?Token:form()), Clause,Direction)
                    end
                end
        end,
    case Res of
        [] -> throw(?LocalErr0r(invalid_position));
        _  -> Res
    end.

get_node_from_form(Form,[Lex],Direction) ->
    FChilds = ?Syn:children(hd(Form)),
    {_,ClauseLex} = lists:unzip(FChilds),
    FGoods = 
        case Direction of
            left -> lists:dropwhile(fun(K) -> K /= Lex end,ClauseLex);
            right -> lists:takewhile(fun(K) -> K /= Lex end,ClauseLex)
        end,
    FRealGoods = lists:filter(fun({_,Class,_}) -> Class == clause end, FGoods),
    case {FRealGoods,Direction} of
        {[],_} -> get_node_from_file(hd(Form),Direction);
        {_,left} -> ?Query:exec(hd(FRealGoods),?Clause:body(1));
        {_,right} -> Exprs = ?Query:exec(lists:last(FRealGoods), ?Clause:body()),
                     [lists:last(Exprs)]
    end.

get_node_from_file(Form,Direction) ->
    File = ?Query:exec(Form, ?Form:file()),
    Forms = ?Query:exec(File,?File:forms()),
    GoodForms =
        case Direction of
            left -> [Head|Res] = lists:dropwhile(fun(K) -> K /= Form end, Forms),
                     case Res of
                        [] -> [Head];
                        _ -> Res
                    end;
            right -> Res = lists:takewhile(fun(K) -> K /= Form end, Forms),
                     case Res of 
                        [] -> [hd(Forms)];
                        _ -> Res
                    end
        end,
    RealForms = lists:map(fun(K) -> hd(get_real_form(K)) end, GoodForms),
    GForms = lists:filter(fun(K) ->  not lists:member(?Form:type(K),?DUPFORMTYPES) end, RealForms),
    case {GForms,Direction} of
        {[],_} -> throw(?LocalErr0r(invalid_position)); %% hiba
        {_,left} -> ?Query:exec(hd(GForms),?Query:seq([?Form:clause(1),?Clause:body(1)]));
        {_,right}-> Clauses = ?Query:exec(lists:last(GForms),?Form:clauses()),
                    Expr = ?Query:exec(lists:last(Clauses),?Clause:body()),
                    [lists:last(Expr)]
    end.

get_real_form([Form]) ->
    get_real_form(Form);
get_real_form(Form) ->
    Orig =
        case ?Form:type(Form) of
            lex -> ?Query:exec(Form,[{forig,back}]);
            _ -> [Form]
        end,
    case Orig of
        [] -> %throw(?LocalErr0r(invalid_position));
            [Form];
        _ -> Orig
    end.



get_node_from_clause(Clause,[Lex],Direction) ->
    Top = to_top(Clause,Clause),
    case Top of
        Clause -> 
            Childs = ?Syn:children2(hd(Clause)),
            {_,BodyLex} =
                lists:unzip(
                    lists:filter(
                        fun({Type,_}) ->
                            Type /= name andalso Type /= pattern andalso Type/=guard
                        end,
                    Childs)),      
            case Direction of
                left -> Goods = lists:dropwhile(fun(K) -> K /= Lex end,BodyLex),
                        RealGoods = lists:filter(fun({_,Class,_}) -> Class == expr end, Goods),
                        case RealGoods of
                            [] -> Form = ?Query:exec(Clause,?Clause:form()),
                                  get_node_from_form(Form,Clause,Direction);
                            _ -> [hd(RealGoods)]
                        end;
                right ->Goods = lists:takewhile(fun(K) -> K /= Lex end,BodyLex),
                        RealGoods = lists:filter(fun({_,Class,_}) -> Class == expr end, Goods),
                        case RealGoods of
                            [] -> Form = ?Query:exec(Clause,?Clause:form()),
                                  get_node_from_form(Form,Clause,Direction);
                            _ -> [lists:last(RealGoods)]
                        end
            end;
        _ -> Top
    end.


%% @doc Brings an expression or clause to top level expression
to_top(Node = [{_,expr,_}],_) ->
    to_top(?Query:exec(Node,?Query:any([?Expr:clause(),[{name,back}]])),Node);
to_top(Node = [{_,clause,_}],Prev) ->
    case ?Clause:type(hd(Node)) of
        fundef when Prev == Node->  Prev;
        fundef -> ?Query:exec(Prev,?Expr:top());
        _ -> to_top(?Query:exec(Node,[{clause,back}]),Node)
    end;
to_top([],_) ->
    [];
to_top(_,_) ->
    throw(?LocalErr0r(invalid_position)).


get_files_from_options(none) -> [];
get_files_from_options([]) ->
    lists:filter(fun(File) -> ?File:type(File) == module end, ?Query:exec(?File:all()));
get_files_from_options(Files) ->
    FileNodes =
        lists:usort(
            lists:flatten([get_file_from(File) || File <- Files])),
    FileNodes.

get_file_from(File) when is_atom(File) ->
    FileNode =
        ?Query:exec(?Graph:root(),
                    ?Query:seq([?Mod:find(File),
                                ?Mod:file()])),
    % more than one filenode can have the same module name
    case length(FileNode) of
        0 -> io:format("Warning: ~p ignored (not present in the database).~n",
                        [File]),
              [];
        1 -> FileNode;
        2 -> io:format("Warning: ~p appears more than once in the database.~n",
                        [File]),
             FileNode
    end;
%nem igazán jó a regexp
get_file_from(File) when is_list(File) ->
    DBFilePaths = [?File:path(DBFile) || DBFile <- ?Query:exec(?File:all())],
    case lists:member(File, DBFilePaths) of
        true -> 
            ?Query:exec(?Graph:root(),?File:find(File));
        _ ->
            Mods =
                case catch referl_misc:regexp([File]) of
                    [{_,Result}] when is_list(Result) ->
                        lists:usort(Result);
                    _ -> []
                end,
            [?Query:exec(Mod, ?Mod:file()) || Mod<-Mods] 
    end.



%% @spec tree_text(node()) -> Chars
%%       Chars = [char() | Chars]
%% @doc Returns the textual representation of the syntactical subtree that
%% starts at `Top' as a deep list.
tree_text(Top) ->
    [[T#token.prews, T#token.text, T#token.postws]
        || Token <- ?Syn:leaves2(Top),
           #lex{data=T=#token{}} <- [?ESG:data(Token)]].


%% @spec flat_text0(node()) -> [char()]
%% @doc Returns the textual representation of the syntactical subtree that
%% starts at `Top'.
flat_text0(Top) ->
    lists:flatten(tree_text(Top)).



%% @doc Returns every leaf expression of Expr.
deep_exprs(Expr) ->
    [E || E<-deep_sub(Expr), ?Expr:is_leaf(E)].

%% @doc Determenistic verions of ?Expr:deep_sub/1.
deep_sub(Sub) ->
    [Sub | ?Query:exec(Sub,
                       ?Query:any(
                          ?Query:seq(?Expr:children(), fun deep_sub/1),
                          ?Query:seq([?Expr:clauses(),
                                      ?Clause:exprs(),
                                      fun deep_sub/1])))].