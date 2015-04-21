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
%%% The Initial Developer of the Original Code is E�tv�s Lor�nd University.
%%% Portions created  by E�tv�s Lor�nd University and ELTE-Soft Ltd.
%%% are Copyright 2007-2013 E�tv�s Lor�nd University, ELTE-Soft Ltd.
%%% and Ericsson Hungary. All Rights Reserved.

%%% @doc This module collects miscellaneous functions,
%%%      mostly with functionality missing from standard modules, e.g. lists.
%%%
%%% @author Robert Kitlei <kitlei@inf.elte.hu>
%%% @author Kornel Horvath <kornel@inf.elte.hu>
%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>

-module(referl_misc).
-vsn("$Rev: 13087 $ ").

%%% ============================================================================
%%% Exports

%% refgen_typeconvert ?Conv
%% Conversions between data structures.
-export([to_atom/1, to_list/1]).
-export([any_to_string/1, integer_to_list/2,
         separated_text/1, table_to_list/1,
         record_to_proplist/2, record_to_proplist/3]).
-export([funlist_text/1, fun_text/1, recfld_text/1]).
-export([string_to_term/1]).
-export([normalize/1]).
-export(['or'/2]).

%% refgen_list ?List
%% General list transformations
-export([intersect/2, merge/2, list_substract/3,
         substract_ranges/2,
         slice_list/2,
         index_list/1, index_list/2, text_index_list/1,
         join/1, join/2, flatjoin/2, flatsort/1,
         fast_usort/1, unique_sorted/1, is_ordered/1,
         list_cnt/1,
         list_swap/3, list_move/4,
         transpose/1,
         group_list/1, group/1, groupby/2, groupbywith/3,
         map_not/1, mask/2, mask/3, zipwith/3,
         uniq/1]).

%% General list queries
-export([min_max/1, seq/2, seq2/2]).
-export([partfold/3, partfold/4,
         common_prefix/2,
         list_find/2, list_member/2, list_contains/2,
         list_compare/2, list_compare/3, list_compareL/1, list_compareL/2,
         separate_interval/3]).

%% refgen_proplist ?Prop
%% Property lists
-export([proplist_merge_def/2, proplist_validate/2,
         call_arg/3]).
-export([pget/2, pgetu/2, pcopy/2, pdel/2]).
-export([lookup_symbols/3, is_proplist/1]).

%% refgen_text ?Text
%% Text transformations
-export([add_article/1, plural/2,
         string_replace/4,
         string_trim/1, string_trim/2, string_trim/3, strip/1,
         string_lines/1, string_lines/2, string_split/5, lines/1,
         dos2unix/1, canonical_filename/1]).

%% Text queries
-export([string_char_type/1, string_is_letter/1, escape/1,
         string_length/1, string_length/2,
         string_strs/2, string_EOLs/0,
         string_linecol/2, string_linecol/3]).
-export([is_erl/1, is_beam/1]).

%% @todo
%% refgen_misc ?MISC
%% ETS
-export([ets_exist/1, ets_keys/1, ets_list2tab/2, ets_clone/1]).

%% Math
-export([math_floor/1, math_ceil/1]).

%% OS
-export([os_cmd/2]).
-export([find_files/2]).

%% Records
-export([get_rec_type/1, get_rec_value/2, set_rec_value/3, check_record/2]).

%% Error signalling.
-export([format/1, format/2]).
-export([error_on_difference/3]).

%% Exports for testing framework
-export([drop_files/1]).

%% Database hash
-export([database_hash/0, file_hash/1]).

%% 'Data' directory full path
-export([data_dir/0]).

%% Graph-based postfix for identification purpose
-export([graph_based_postfix/0]).

%% is_client
-export([is_client/0]).

%% Environment variable utilities.
-export([is_sideeffect_check_on/0]).

% Parallel util.s
-export([parallelise/4, parallelise/5, parallelise/6, processor_count/0,
         async_call/3, async_call/4, yield/1]).

% @private
-export([async_call_helper/1]).

-export([general_closure/2]).

-export([general_closure0/3]).

-export([regexp/1]).
-export([error_text/2]).

%%% ============================================================================
%%% Includes

-include_lib("referl_gen/include/gen_export.hrl").

%% TODO: move functions that depends on these headers into their proper
%% applications (or generalize them)
-include_lib("referl_core/include/core_export.hrl").
-include_lib("referl_lib/include/lib_export.hrl").
-include_lib("referl_core/include/core_global.hrl").

-define(EXEC_TIMEOUT,100000).
-define(BASIC_REGEXP, ["^(/)[0-9a-zA-Z_./]+/common/[0-9a-zA-Z_.]+/(lib/)?(ebin)$"]).

error_text(regexp_bad_regexp, [Value]) -> ["The regexp is incorrect: ", Value];
error_text(regexp_file_failed_to_open, [File, Reason]) -> 
    ["Could not open the regexp file: ~s ~n Reason: ~s. ", File, Reason].
%% -type(natural() :: non_neg_integer()).

%% @type funinfo2() = [atom() | natural()]

%% @type funinfo3() = [atom() | atom() | natural()]



%% @spec data_dir()-> Path::string()
%% @doc Returns the exact path of the 'data' directory.
data_dir()->
	mnesia:system_info(directory).

%% @spec graph_based_postfix()-> Postfix::string()
%% @doc Returns a postfix which can be used for graph-based identification purpose.
graph_based_postfix()->
    "_"++case ?Graph:actual_graph() of
        %The mnesia-based implementation returns this constant value
        "In mnesia there can be only one graph" -> "mnesia";
        Graph when is_atom(Graph) -> atom_to_list(Graph);
        Graph when is_list(Graph) andalso is_integer(hd(Graph)) -> Graph;
        _ -> "default"
    end.

%%% ============================================================================
%%% List/atom/etc. conversions

%% @doc parse a string into an Erlang term
%% @spec (string()) -> {ok, term()} |
%%           {error, ErrorInfo, EndLocation} | {error, ErrorInfo}
string_to_term(String) ->
    case erl_scan:string(String) of
        {ok,Tokenized,_EndLine} ->
            erl_parse:parse_term(Tokenized);
        Error ->
            Error
    end.

%% @spec to_atom(atom() | integer()) -> atom()
%% @doc Converts a list or an atom to an atom.
to_atom(Data) when is_atom(Data) -> Data;
to_atom(Data) when is_list(Data) -> list_to_atom(Data).

%% @spec to_list(atom() | integer() | float() | list()) -> list()
%% @doc Converts an atom/integer/list to a list.
to_list(Text) when is_atom(Text)    -> atom_to_list(Text);
to_list(Text) when is_integer(Text) -> integer_to_list(Text);
to_list(Text) when is_float(Text)   -> float_to_list(Text);
to_list(Text) when is_list(Text)    -> Text.

%% @spec (string()) -> string()
%% @doc Normalize a filename. Makes the drive letter lower case and skips
%% directory self-references.
normalize(Filename) ->
    filename:join([Filename]).

%%% ============================================================================
%%% Additions to the lists: module


%% @doc remove dups from an ordered list (hand-fusion of the below)
%% @equiv [hd(A)||A<-referl_misc:group(L)]
%% @spec ([A]) -> [A]
uniq([]) -> [];
uniq([A,B|T]) when A =:= B -> uniq([B|T]);
uniq([H|T]) -> [H|uniq(T)].

%% @doc The function slices the list into sublists with length `N'. If
%% the length of `List' is not divisible by `Len' the last list will
%% be shorter.
%% @spec slice_list(Len :: integer(), List :: [A]) -> [[A]]
slice_list(Len, List) ->
    slice_list(List, Len, Len, [], []).

slice_list([], _, _, CAcc, Acc) ->
    lists:reverse([lists:reverse(CAcc) | Acc]);
slice_list(List, 0, OLen, CAcc, Acc) ->
    slice_list(List, OLen, OLen, [], [lists:reverse(CAcc) | Acc]);
slice_list([H|T], Len, OLen, CAcc, Acc) ->
    slice_list(T, Len-1, OLen, [H | CAcc], Acc).


%% @spec separated_text([atom()]) -> [string()]
%% @doc Converts the list of atoms into a printable format, the elements are
%% separated by a comma.
separated_text(Atoms) ->
    Texts = lists:map(fun to_list/1, Atoms),
    join(Texts).


%% @spec index_list([Elem]) -> [{Elem, integer()}]
%% @doc Adds indices to the elements of the list.
index_list(List) -> index_list(List, 1).


%% -spec(index_list/2::([Elem], integer()) -> [{Elem, integer()}]).
%% @spec index_list([Elem], integer()) -> [{Elem, integer()}]
%% @doc Adds indices to the elements of the list, starting with `Start'.
index_list(List, Start) ->
    {Result, _} = lists:mapfoldl(fun(E, N) -> {{E, N}, N + 1} end,
                                 Start,
                                 List),
    Result.


%% -spec(text_index_list/1 :: ([Elem]) -> [{Elem, string()}]).
%% @spec text_index_list([Elem]) -> [{Elem, string()}]
%% @doc Adds textual indices to the elements of the list.
text_index_list(List) ->
    [ {Elem, integer_to_list(Index)} || {Elem, Index} <- index_list(List) ].


%% @spec seq(last|integer(), integer()) -> {[last|integer()], last|integer()}
%% @doc Similar to `lists:seq/2'.
%%      Returns the list starting from `Start' and running `N' long,
%%      and the value next in line.
%%      Also, this function can take the extremal element `last'.
seq(last, N)  -> {[last || _ <- lists:seq(1, N)], last};
seq(Start, N) -> {lists:seq(Start, Start + N - 1), Start + N}.


%% @spec (integer(), integer()) -> [integer()]
%% @doc Works almost exactly as `lists:seq/2', but returns [] for empty ranges
%%
%%      (like `Prelude.enumFromTo'::Int->Int->[Int] in Haskell)
seq2(Start,End) when Start>End -> [];
seq2(Start,End)                -> lists:seq(Start,End).


%% @spec intersect([Elem], [Elem]) -> [Elem]
%% @doc Returns the intersection of the lists. Keep oiginal order of elements
%%      in the first list. If there are more same element in the first list
%%      than in the second list the elements which are closer to the tail will
%%      be kept in the intersection.
%%
%% Example:
%% ```
%% > intersect([1,2,3,4,2,5], [4,2]).
%% [4,2]
%% '''
intersect(L1, L2) -> L1 -- (L1 -- L2).


%% @spec merge([Elem], [Elem]) -> [Elem]
%% @doc Merges two lists (having the same length) by elements
merge([], [])         -> [];
merge([X|Xs], [Y|Ys]) -> [X,Y|merge(Xs,Ys)].


%% @spec separate_interval([Elem], integer(), integer()) -> [Elem]
%% @doc Returns the elements of the list between the two given elements.
separate_interval(List, First, Last) ->
    list_reverse_drop(list_reverse_drop(List, First), Last).

%% @spec common_prefix(list(), list()) -> list()
%% @doc  Returns the longest prefix of two lists.
common_prefix([A|Xs], [A|Ys]) -> [A|common_prefix(Xs, Ys)];
common_prefix(_, _) -> [].


%% @spec ([[any()]]) -> ([[any()]])
%% @doc Transposition. Only works for complete rectangular matrices.
%%
%%      (similar to `Data.List.transpose' in Haskell)
transpose([H|T]) when is_list(H) ->
    Append  = fun(V, M) -> lists:zipwith(fun(E,L) -> [E|L] end, V, M) end,
    Vector  = fun(L) -> lists:map(fun(E) -> [E] end, L) end,
    [RH|RT] = lists:reverse([H|T]),
    lists:foldl(Append, Vector(RH), RT).

%% @doc convert a matrix into a string
%% @spec ([any()])->string()
table_to_list(Table) ->
    Cols = fun(R)-> [ any_to_string(C) || C <- R] end,
    Rows = [ string:join(Cols(R), "; ") || R <- Table],
    string:join(Rows, "\n").


%% @doc Splits a string by its end of line markers, resulting in a string for
%%      each input line. If a final newline is present then an empty list
%%      is appended to the output.
%%
%%      (resembling `Prelude.lines' in Haskell)
%% @spec (string()) -> [string()]
lines(S) ->
    {L,_} = string_split(S,string_EOLs(),0,true,false),
    L.

%% @doc Breaks a sequence into groups based on equivalence classes of
%%      consecutive elements.
%%
%%      (like `Data.List.group'::Eq a=>[a]->[[a]] in Haskell)
%% @spec ([A])->[[A]]
group(L) ->
    groupby(fun(X,Y)->X==Y end,L).

%% @doc Like `group/1', but the equivalence relation can be given as argument.
%%
%%      (like `Data.List.groupBy' in Haskell)
%% @see group/1
%% @spec (fun((A,A)->bool()),[A]) -> [[A]]
%% @todo Research if using `groupbywith/3' would degrade performance
groupby(_,[]) ->
    [];
groupby(C,[H|T]) when is_function(C,2) ->
    {G,NG} = lists:splitwith(fun(X)->C(H,X) end,T),
    [[H|G]|groupby(C,NG)].

%% @equiv lists:map(fun(G)->lists:map(F,G)end,groupby(C,L))
%% @spec (fun((A)->A),fun((A,A)->bool()),[A]) -> [[A]]
groupbywith(_,_,[]) ->
    [];
groupbywith(F,C,[H|T]) when is_function(F,1), is_function(C,2) ->
    {G,NG} = lists:splitwith(fun(X)->C(H,X) end,T),
    [[F(H)|lists:map(F,G)]|groupbywith(F,C,NG)].

%% @doc negates each element
%% @spec ([bool()]) -> [bool()]
map_not(L) ->
    [not B || B<-L].


%% @doc Masks an integer list by a bool bitvector
%% @spec ([bool()],[integer()]) -> [integer()]
mask(M,L) ->
    mask(M,L,0).

%% @private
%% @spec ([bool()],[E],E) -> [E]
mask(M,L,X) ->
    zipwith(fun (true,E) -> E;
                (_,_)    -> X
            end, M, L).

%% @doc a zipwith variant that handles differing list lengths by truncation
%%
%%      (like `Prelude.zipWith' in Haskell)
%% @see lists:zipwith/3
%% @spec (fun((P,Q)->R), [P], [Q]) -> [R]
zipwith(F,[H1|T1],[H2|T2]) ->
    [F(H1,H2) | zipwith(F,T1,T2)];
zipwith(_,_,_) ->
    [].


%% @spec list_reverse_drop([Elem], Elem) -> [Elem]
%% @doc Reverses the list and drops elements up to the one given.
%%      Use it twice to separate an interval.
list_reverse_drop(List, Elem) ->
    Drop = lists:dropwhile(fun(Elem1) -> Elem1 /= Elem end, List),
    lists:reverse(Drop).


%% @spec join([string()]) -> [string()]
%% @doc Separate a list with commas.
join(List) -> join(List, ", ").


%% @spec join([Elem], Elem) -> [Elem]
%% @doc Inserts a separator between elements of a list.
%%
%%      Also called `intercalate' in other languages (which ones??).
%%      (similar to `Data.List.intersperse'::a->[a]->[a] in Haskell)
join([E1, E2| Es], S) -> [E1, S| join([E2| Es], S)];
join([E], _)          -> [E];
join([], _)           -> [].

%% @spec flatjoin([Elem], Elem) -> [Elem]
%% @doc Inserts a separator between elements of a list, then flattens the
%%      result. Useful when handling texts.
flatjoin(List, Separator) -> lists:flatten(join(List, Separator)).

%% @spec flatsort([Elem]) -> [Elem]
%% @doc Flattens a list, then sorts it.
flatsort(List) ->
    lists:usort(lists:flatten(List)).

%% Sorts and then uniques the given list.
%% Runs in linear time if the list is already ordered,
%% otherwise in N*log N, as `lists:usort/1'.
fast_usort(Xs) ->
    try
        unique_sorted(Xs)
    catch
        not_ordered ->
            lists:usort(Xs)
    end.

%% Uniques a sorted list. If the argument list is not sorted,
%% throws `not_ordered'.
unique_sorted([X1, X2|_]) when X1 > X2        -> throw(not_ordered);
unique_sorted([X |Xs2 = [X |_]])              -> unique_sorted(Xs2);
unique_sorted([X1|Xs2 = [X2|_]]) when X1 < X2 -> [X1|unique_sorted(Xs2)];
unique_sorted(Xs)                             -> Xs.

%% Determines in linear time whether a list is sorted in non-descending order.
is_ordered([X1|Xs2 = [X2|_]]) -> X1 =< X2 andalso is_ordered(Xs2);
is_ordered(_)                 -> true.


%% @doc  Groups a list by its first elements. The list has to be ordered.
%% @equiv groupbywith(fun({X,_},{Y,_})->X==Y end,fun({_,Y})->Y end,L)
%% @spec group_list([{Keyword, Elem}]) -> [{Keyword, [Elem]}]
group_list([])                      -> [];
group_list([{Keyword, Value} | Ms]) ->
    {Vs, Rest} = split_by_keyword(Keyword, Ms),
    [{Keyword, [Value] ++ Vs}] ++ group_list(Rest).


%% @private
%% @doc Splits the first elements of an [{A,B}] list
%%      whose first component equals the given value.
%% @spec (A,[{A,B}]) -> {[B],[{A,B}]}
split_by_keyword(Keyword, Pairs)     -> split_by_keyword(Keyword, Pairs, []).

%% @private
%% @spec (A,[{A,B}],[B]) -> {[B],[{A,B}]}
split_by_keyword(K, [{K,X}|Ks], Acc) -> split_by_keyword(K, Ks, Acc ++ [X]);
split_by_keyword(_, Ks, Acc)         -> {Acc, Ks}.

%% @spec partfold(Fun, Acc::term(), [term()]) -> term()
%%   Fun = (Elem::term(), AccIn::term()) ->
%%             {stop, AccOut} | {next, AccOut}
%%
%% @doc Similar to `lists:foldl/3', except that this can stop processing.
partfold(_Fun, Acc, []) ->
    Acc;
partfold(Fun, Acc0, [Head|Tail]) ->
    case Fun(Head, Acc0) of
        {stop, Result} -> Result;
        {next, Acc1} -> partfold(Fun, Acc1, Tail)
    end.

%% @spec partfold(Cond, Fun, Acc::term(), [term()]) -> term()
%%   Cond = (Elem::term(), AccIn::term()) -> bool()
%%   Fun = (Elem::term(), AccIn::term()) -> AccOut
%%
%% @doc Similar to `lists:foldl/3', except that this can stop processing.
partfold(_Cond, _Fun, Acc, []) ->
    Acc;
partfold(Cond, Fun, Acc, [Head|Tail]) ->
    case Cond(Head, Acc) of
        false -> Acc;
        true -> partfold(Cond, Fun, Fun(Head, Acc), Tail)
    end.


%% @@spec funlist_text([{atom(), natural()}]) -> [deep_string()]
%% @spec ([funinfo2() | funinfo3()]) -> [deep_string()]
%% @doc  A list that enumerates a nonempty list of functions when flattened.
funlist_text([First|Rest]) ->
    [   fun_text(First),
        [", " ++ fun_text(Funinfo) || Funinfo <- Rest] ].


%% -spec(fun_text/1::(funinfo2()|funinfo3()) -> [deep_string()]).
%% @spec fun_text(funinfo2()|funinfo3()) -> [deep_string()]
%% @doc Converts the function signature into a printable format.
%%      The signature is accepted either as a list or a tuple.
fun_text({Name, Arity}) ->
    fun_text([Name, Arity]);
fun_text({Mod, Name, Arity}) ->
    fun_text([Mod, Name, Arity]);
fun_text([Name, Arity]) ->
    [atom_to_list(Name), "/", integer_to_list(Arity)];
fun_text([Mod, Name, Arity]) ->
    [atom_to_list(Mod), ":", fun_text([Name, Arity])].


%% @spec recfld_text(recfld()) -> [string()]
%% @doc Converts the function signature into a printable format.
recfld_text([Record, Field]) ->
    [atom_to_list(Record), ".", atom_to_list(Field)].


%% @doc Adds the appropriate article to a string.
%%      Special cases (e.g. "a European") are not accounted for.
add_article(Text = [Ch1|_])
    when Ch1 =:= $a; Ch1 =:= $e; Ch1 =:= $i; Ch1 =:= $o; Ch1 =:= $u;
         Ch1 =:= $A; Ch1 =:= $E; Ch1 =:= $I; Ch1 =:= $O; Ch1 =:= $U ->
    ["an ", Text];
add_article(Text) ->
    ["a ", Text].

%% @doc Returns the first argument in plural or singular depending on the
%%      length of the second argument.
plural(Text, [])         -> Text;
plural(Text, [_OneElem]) -> Text;
plural(Text, [_|_])      -> Text ++ "s".


%% @type range() = {integer(),integer()}

%% @doc substract lists of range data types
%%      substract(A,B) removes all ranges from A that are subranges of any B.
%%      Both A and B must be ordered.
%% @todo verify correctness
%% @spec ([range()],[range()]) -> [range()]
substract_ranges([], _) ->
    [];

substract_ranges(V, []) ->
    V;

substract_ranges([{{B1,L1},_S1}|T1], [{{B2,L2},S2}|T2])
    when (B2=<B1) and (B2+L2>=B1+L1)->
        substract_ranges(T1, [{{B2,L2},S2}|T2]);

substract_ranges([{{B1,L1},S1}|T1], [{{B2,L2},_S2}|T2])
    when (B2+L2<B1+L1)->
        substract_ranges([{{B1,L1},S1}|T1], T2);

substract_ranges([{{B1,L1},S1}|T1], [{{B2,L2},S2}|T2])
    when (B1<B2)->
        [{{B1,L1},S1} | substract_ranges(T1, [{{B2,L2},S2}|T2])].



%%% ============================================================================
%%% Property lists


%% @spec proplist_merge_def(List::proplist(), DefList::proplist()) ->
%%           proplist()
%% @doc  Add missing key value pair from `DefList' to `List'.
proplist_merge_def(List, DefList) when is_list(List), is_list(DefList) ->
    List ++ list_substract(DefList, List,
        fun({Type,_},{Type,_}) -> true; ({_,_},{_,_}) -> false end).


%% @spec proplist_validate(List::proplist(),
%%           ValidatorList::ValidatorProplist) ->
%%           {MissedPairs::propllist(), WrongPairs::proplist()}
%%       ValidatorProplist = [{Key::atom(),
%%                             ValidatorFun::((Value::term()) -> boolean())}]
%% @doc  Validate the values of keys in the `List' by the validator functions
%%       given in `ValidatorList'. If there is a key in the `List' without
%%       validator function in `ValidatorProplist' the pair will be copied into
%%       the `MissedPairs' list. If a the validator function of a key return
%%       with false the pair will be copied into the `WrongPairs' list.
proplist_validate(List, ValidatorList) when is_list(List),
        is_list(ValidatorList) ->
    {RevMisses, RevWrongs} =
        lists:foldl(
        fun({Key,Value}, {AccMisses,AccWrongs}) ->
            case proplists:get_value(Key, ValidatorList) of
                Fun when is_function(Fun) ->
                    case Fun(Value) of
                        true  -> {AccMisses,              AccWrongs };
                        false -> {AccMisses, [{Key,Value}|AccWrongs]}
                    end;
                undefined -> {[{Key,Value}|AccMisses], AccWrongs}
            end
        end,
        {[],[]},
        List),
    {lists:reverse(RevMisses), lists:reverse(RevWrongs)}.


%% @doc Looks up a list of keys in a proplist at once.
%% @spec ([Key|{Key,Default}],[{Key,Value}])->[[Value]]
pget(Keys,Prop) when is_list(Keys), is_list(Prop) ->
    [begin
         case Key of
             {K,D} when is_atom(K) ->
                 case proplists:get_all_values(K,Prop) of
                     [] -> [D];
                     R  -> R
                 end;
             K when is_atom(K) ->
                 proplists:get_all_values(K,Prop)
         end
     end || Key <- Keys].

%% @doc Looks up a list of keys in a proplist at once and unpacks the results.
%% @throws {badmatch,[]}
%% @spec ([K],[{K,V}])->[V]
pgetu(Keys,Prop) when is_list(Keys), is_list(Prop) ->
    [begin
         [[X]] = pget([K],Prop),
         X
     end ||
        K <- Keys].

%% @doc Copies all tuples with a key matching any of that listed.
%% @spec ([Key|{OldKey,NewKey}],[{Key,Value}]) -> [{Key,Value}]
pcopy(Keys,Prop)->
    Copy = fun(K) when is_atom(K) ->
                   proplists:lookup_all(K,Prop);
              ({O,N}) when is_atom(O), is_atom(N)->
                   [{N,V} || {_K,V} <- proplists:lookup_all(O,Prop)]
           end,
    lists:flatmap(Copy, Keys).

%% @doc Deletes keys from a proplist
%% @spec ([Key],[{Key,Value}]) -> [{Key,Value}]
%% @equiv [KV || KV={K,_V}<-Prop, not lists:member(K,Delete)]
pdel(Delete,Prop) ->
    proplist_cut_(sets:from_list(Delete),Prop,[]).

proplist_cut_(_, [], Res) ->
    Res;
proplist_cut_(ToDel, [KV={K,_V}|Tail], Res0) ->
    Res =
        case sets:is_element(K,ToDel) of
            true ->
                Res0;
            false ->
                [KV | Res0]
        end,
    proplist_cut_(ToDel, Tail, Res).

%% @doc Determines if the type of a value is proplist.
is_proplist([]) ->
    true;
is_proplist([{K,_V}|T]) when is_atom(K) ->
    is_proplist(T);
is_proplist([K|T]) when is_atom(K) ->
    is_proplist(T);
is_proplist(_) ->
    false.

%%% ----------------------------------------------------------------------------
%%% Generic argument finder (for reflib_args and reftr_wrangler)

%% @spec call_arg(arglist(), string(), ArgSpec) -> term()
%%       ArgSpec = [{fun(), [atom()]}]
%% @doc Applies the first argument specification that matches `Args'. Matching
%% means that every property key that is specified in the argument
%% specification list exists in `Args'; in this case, the function provided in
%% the specification is called with the property values passed as individual
%% arguments to the function. When no math is found, an exception is thrown
%% using the argument description in `Desc'.
call_arg(_Args, Desc, []) ->
    throw(?RefError(missing_arg, [Desc]));
call_arg(Args, Desc, [{Fun, Params} | Tail]) ->
    try
        apply(Fun, [arg(Args, Name) || Name <- Params])
    catch
        throw:?LocalError(not_found, _) ->
            call_arg(Args, Desc, Tail)
    end.

%% @private
arg(Args, Name) ->
    case proplists:lookup(Name, Args) of
        {Name, Value} -> Value;
        none -> throw(?LocalError(not_found, [Name]))
    end.



%%% ============================================================================
%%% Error signalling

format(Fmt) ->
    format(Fmt, []).

%% @spec format(string(), [Params]) -> string()
%% @doc Creates a formatted string similar to io_lib:format/2.
%%      This is an ordinary string, not a deep list.
%%      If the text is too long, it will be broken to lines.
%%      In order to avoid breaking into lines, use for only the smallest
%%      portion to be formatted, and concatenate the rest of the message.
format(Format, Params) ->
    lists:flatten(io_lib:format(Format, Params)).

%% @spec error_on_difference(Type, Type, string()) -> ok
%% @doc Signals an error if the first two parameters differ.
error_on_difference(Val, ExpectedVal, ErrorText) ->
    if
        Val == ExpectedVal -> ok;
        true               -> throw(ErrorText)
    end.


%%% ============================================================================
%%% Map handling

%% @doc N nodes of symbol type S are removed from GrpSyms.
%% @spec (S,integer(),[{S,[V]}]) -> {[V],[{S,[V]}]}
lookup_symbols(S, N, GrpSyms) ->
    {S, SNodes} = lists:keyfind(S, 1, GrpSyms),
    {SNodes1, SNodes2} = lists:split(N, SNodes),
    GrpSyms2 =
      case SNodes2 of
        [] -> lists:keydelete(S, 1, GrpSyms);
        _  -> lists:keydelete(S, 1, GrpSyms) ++ [{S, SNodes2}]
      end,
    {SNodes1, GrpSyms2}.

%%% ============================================================================
%%% Text handling

%% @spec strip(string()) -> string()
%% @doc Removes whitespace from the beginning and end of text. Also removes
%%      Erlang comments.
strip(Text) ->
    IsWS     = fun(C) -> lists:member(C, " \t\n") end,
    RevStrip = fun(X) -> lists:dropwhile(IsWS, lists:reverse(X)) end,
    NoCommentText = lists:takewhile(fun(Char) -> Char /= $% end, Text),
    RevStrip(RevStrip(NoCommentText)).


%% @spec escape(string()) -> string()
%% @doc Escapes characters with backspace in a string.
escape([Char | Tail])
  when Char =:= $*; Char =:= $+; Char =:= $\\;
       Char =:= $.; Char =:= $$; Char =:= $^;
       Char =:= $(; Char =:= $); Char =:= $?;
       Char =:= $[; Char =:= $]; Char =:= $|;
       Char =:= $"     -> [$\\, Char | escape(Tail)];
escape([Char | Tail])
  when Char =:= $\ ; Char =:= $\t; Char =:= $\n  -> escape(Tail);
escape([Char | Tail])                            -> [Char | escape(Tail)];
escape([])                                       -> [].

%%% ============================================================================
%%% Check makers

%%% ----------------------------------------------------------------------------
%%% Record functions

%% @type record() = tuple(). The Built-in erlang record type.


%% @spec get_rec_type(Record::record()) -> atom()
%% @doc  Give back the type of `Record'.
get_rec_type(Record) when is_tuple(Record) -> element(1,Record).


%% @spec get_rec_value(Record::record(), FieldIndex::natural()) -> term()
%% @throws badarg
%% @doc  Return the value of the `Field' field of the `Record'. The
%%       `FieldIndex' is the position of the specified field in the tuple
%%        representation of the record.
%%
%% See <A HREF="http://www.erlang.org/doc/reference_manual/records.html#8.3"
%%      TARGET="_blank"><TT>Erlang Reference Manual - 8.3 Acessing Record Fields
%%      </TT></A>
%%
%% Example:
%% ```
%% get_rec_value(#people{name="Brian"}, #people.name).
%% '''
get_rec_value(Record, Key) when is_tuple(Record), is_integer(Key), 1<Key ->
    element(Key, Record).


%% @spec set_rec_value(Record::record(), FieldIndex::recordField(),
%%           Value::term()) -> record()
%% @throws badarg
%% @doc  Set the `Field' field of the `Record' to `Value'. The
%%       `FieldIndex' is the position of the specified field in the tuple
%%        representation of the record.
%%
%% See <A HREF="http://www.erlang.org/doc/reference_manual/records.html#8.3"
%%      TARGET="_blank"><TT>Erlang Reference Manual - 8.3 Acessing Record Fields
%%      </TT></A>
%%
%% Example:
%% ```
%% set_rec_value(#people{name="Brian", age=32}, #people.age, 33).
%% '''
set_rec_value(Record, Key, Value)
        when is_tuple(Record), is_integer(Key), 1<Key ->
    setelement(Key, Record, Value).


%% @type recordFilter() = {FieldIndex::natural(), Op, Value::term()}
%%       Op = '==' | '/='.
%% Condition for a field of a record.

%% @spec check_record(Record::record(), Filters::[recordFilter()]) -> bool()
%% @throws badarg | {badmatch, Op}
%% @doc  Check fields of `Record' by the given filter list.
%%       Get the field of the `Record' which specified with `Field' and compare
%%       that with `Value' using the `Op' as operator. If `Filters' is empty
%%       `Record' will be accepted.
%%
%% Example:
%% ```
%% check_record(#people{name="Brian", age=33}, [{#people.age,'==',33}]).
%% '''
%% @see  get_rec_value/2
check_record(Record, RecordFilters) when is_tuple(Record),
                                         is_list(RecordFilters) ->
    check_record_(Record, RecordFilters).

check_record_(_Record, []) -> true;
check_record_(Record, [{Key, Op, Value}|Filters]) ->
    RecordValue = get_rec_value(Record, Key),
    Match = case Op of
        '==' -> RecordValue == Value;
        '/=' -> RecordValue /= Value;
        _    -> erlang:error({badmatch,Op})
    end,
    case Match of
        true -> check_record_(Record, Filters);
        _    -> false
    end.


%% @spec record_to_proplist(Record::record(), RecordFields::[recordField()]) ->
%%           [{recordField(), term()}]
%% @doc  Put the record field name and value pairs into a tuple list.
%%       `RecordField' must contain the all field names of the record type in
%%       rigth order.
record_to_proplist(Record, RecFields)
        when is_tuple(Record), is_list(RecFields) ->
    lists:zip(RecFields,tl(tuple_to_list(Record))).

%% @spec record_to_proplist(Record::record(), Fields::[recordField()],
%%               RecordFields::[recordField()]) ->
%%           [{recordField(), term()}]
%% @doc  Put the record field name and value pairs into a tuple list.
%%       `Fields' may be partial and permuted list.
%%       `RecordField' must contain the all field names of the record type in
%%       rigth order.
record_to_proplist(Record, Fields, RecFields) when is_tuple(Record),
        is_list(Fields), is_list(RecFields) ->
    lists:foldl(fun(F, List) ->
                    [{F, get_rec_value(Record, F)}|List] end,
                [], lists:reverse(Fields)).



%%% ----------------------------------------------------------------------------
%%% List functions

%% @spec list_find(Pred::Pred, List::[term()]) ->
%%                {Index::integer(), Value::term()}
%%       Pred    = Value | PredFun
%%       Value   = term()
%%       PredFun = ((term()) -> bool())
%% @doc  Find the first element int the `List' that is accepted by `Pred'.
%%       `Pred' also may be a value and a function. If `Pred' is a value the
%%       `PredFun' is generated automatically to "`fun(Value) ->
%%       Value==ListElement end'" function.
%%       Return the value and the one based index of finded element. If there
%%       is no element found the `Index' is 0.
%%
%% Example:
%% ```
%% > list_find(fun(Ch) -> $X==Ch end, "Hello Mr. X!").
%% {11, 88}
%% '''
list_find(PredFun, List) when is_function(PredFun), is_list(List) ->
    list_find_(List, PredFun, 1);
list_find(Elem, List) when (not is_function(Elem)), is_list(List) ->
    list_find(fun(E) -> E==Elem end, List).

list_find_([],     _,       _  ) -> {0,no};
list_find_([X|Xs], PredFun, Idx) ->
    case PredFun(X) of
        true -> {Idx, X};
        _    -> list_find_(Xs, PredFun, Idx+1)
    end.


%% @spec list_member(Pred::Pred, List::[term()]) -> boolean()
%%       Pred    = Value | PredFun
%%       Value   = term()
%%       PredFun = ((term()) -> bool())
%% @doc  Find the first element int the `List' that is accepted by `Pred'.
%%       `Pred' also may be a value and a function. If `Pred' is a value the
%%       `PredFun' is generated automatically to "`fun(Value) ->
%%       Value==ListElement end'" function.
%%       If there is an element in the list that is accepted it returns
%%       with true otherwise false.
%%       Wrapper function to {@link list_find/2}.
%%
%% Example:
%% ```
%% > list_member(fun(Ch) -> $X==Ch end, "Hello Mr. X!").
%% true
%% '''
%% @see list_find/2
list_member(Pred, List) ->
    0/=element(1,list_find(Pred,List)).


%% @spec ([Elem]) -> {Elem,Elem}
%% @todo Optimize to a hand-crafted recursion
%% @equiv {lists:min(L), lists:max(L)}
min_max(L) ->
    {lists:min(L), lists:max(L)}.


%% @spec list_compareL(ListOfLists::[[term()]]) ->
%%           {CommonPrefix::[term()], ListOfTails::[[term()]]}
%% @doc  Compare lists elements with `==' operator and return the same prefix
%%       and the different tails.
%%
%% Example:
%% ```
%% > list_compareL(["Hello World!", "Hello Mars!", Hello Jupiter!"]).
%% {"Hello ", ["World!", "Mars!", "Jupiter!"]}
%% '''
%% @see  list_compare/2
%% @see  list_compare/3
list_compareL([]) -> {[], []};
list_compareL([List|Lists]) when is_list(List) ->
    list_compareL(fun(X,Y) -> X==Y end, [List|Lists]).


%% @spec list_compareL(
%%               EquFun::((ListElem1::term(),ListOfLists::term())->bool()),
%%               ListOfLists::[[term()]]) ->
%%           {CommonPrefix::[term()], ListOfTails::[[term()]]}
%% @doc  Compare the lists in the `ListOfLists' and give back the common prefix
%%       and the remain tail for all lists. `EquFun' function will be used to
%%       compare the elements in the lists of `ListOfLists'.
%%
%% Example:
%% ```
%% > list_compareL(fun(Ch1, Ch2) -> Ch1==Ch2 end,
%%                ["Hello World!", "Hello Mars!", Hello Jupiter!"]).
%% {"Hello ", ["World!", "Mars!", "Jupiter!"]}
%% '''
%% @see  list_compare/3
list_compareL(EquFun, [List|Lists]) when is_function(EquFun) ->
    Common = lists:foldl(
        fun(L, AccList) -> element(1, list_compare(EquFun, L, AccList)) end,
        List, Lists),
    RevTails = lists:foldl(
        fun(L, AccTails) ->
            [element(2, list_compare(EquFun, L, Common)) |AccTails]
        end,
        [],
        [List|Lists]),
    {Common, lists:reverse(RevTails)}.


%% @spec list_compare(List1::[term()], List2::[trem()]) ->
%%           {CommonPrefix::[term()], Tail1::[term()], Tail2::[term()]}
%% @doc  Compare lists elements with `==' operator and return the same prefix
%%       and the different tails.
%% Example:
%% ```
%% > list_compare("Hello World!", "Hello Mars!").
%% {"Hello ", "World!", "Mars!"}
%% '''
%% @see  list_compare/3
list_compare(List1, List2) when is_list(List1), is_list(List2) ->
    list_compare(fun(X,Y) -> X==Y end, List1, List2).


%% @spec list_compare(EquFun::((ListElem1::term(),ListElem2::term()) -> bool()),
%%                List1::[term()], List2::[trem()]) ->
%%            {CommonPrefix::[term()], Tail1::[term()], Tail2::[term()]}
%% @doc  Compare lists elements with `EquFun' function and return the same
%%       prefix and the different tails.
%%
%% Example:
%% ```
%% > list_compare(fun(Ch1, Ch2) -> Ch1==Ch2 end, "Hello World!", "Hello Mars!").
%% {"Hello ", "World!", "Mars!"}
%% '''
list_compare(EquFun, List1, List2) when is_function(EquFun),
                                        is_list(List1), is_list(List2) ->
    list_compare_(EquFun, List1, List2, []).

list_compare_(_EquFun, [], Tail2, Common) -> {lists:reverse(Common), [], Tail2};
list_compare_(_EquFun, Tail1, [], Common) -> {lists:reverse(Common), Tail1, []};
list_compare_(EquFun, Tail1=[X|Xs], Tail2=[Y|Ys], Common) ->
    case EquFun(X,Y) of
        true -> list_compare_(EquFun, Xs, Ys, [X|Common]);
        _    -> {lists:reverse(Common), Tail1, Tail2}
    end.


%% @spec list_contains(List::list(), PossibleSubLists::[list()]) ->
%%           RealSubLists::[list()]
%% @doc  Find the real sublist of `List' from `PossibleSubLists'.
list_contains(List, SubLists) when is_list(List), is_list(SubLists) ->
    lists:filter(fun(SubList) -> []==(SubList--List) end, SubLists).


%% @spec list_substract(List1::[term()], List2::[term()], EquFun) -> [term()]
%%       EquFun = ((Elem1::term(), Elem2::term()) -> boolean())
%% @doc  Generalization of `--' list operator. Value of `EquFun' true when
%%       two element from the two list is equivalent.
list_substract([],  _List2, _EquFun) -> [];
list_substract(List1, [],   _EquFun) when is_list(List1) -> List1;
list_substract(List1, List2, EquFun) when is_list(List1), is_list(List2),
                                           is_function(EquFun) ->
    list_substract_(lists:reverse(List1), List2, EquFun, []).

list_substract_([],    _Ys, _EquFun, Zs) -> Zs;
list_substract_([X|Xs], Ys,  EquFun, Zs) ->
    case list_find(fun(Y) -> EquFun(X,Y) end, Ys) of
        {0,_} ->
            list_substract_(Xs, Ys, EquFun, [X|Zs]);
        {N,_} ->
            NewYs = lists:sublist(Ys,N-1) ++ lists:nthtail(N,Ys),
            list_substract_(Xs, NewYs, EquFun, Zs)
    end.


%% @spec list_cnt(List::[term()]) -> [{term(), Count::integer()}]
%% @doc  Return a sorted list of elments of `List' where same elements are
%%       present only once. The elements are zipped into a tuple with their
%%       frequency.
list_cnt(List) when is_list(List) ->
    dict:to_list(lists:foldl(fun(Elem,D) -> dict:update_counter(Elem,1,D) end,
                             dict:new(), List)).


%% @spec list_swap(Elem1::term(), Elem2::term(), List::[term()]) ->
%%           NewList::[term()]
%% @doc Swap `Elem1' and `Elem2' at all occurrences in `List'.
list_swap(Elem1, Elem2, List) when Elem1=/=Elem2, is_list(List) ->
    lists:map(
        fun(E) when E==Elem1 -> Elem2;
           (E) when E==Elem2 -> Elem1;
           (E) -> E
        end,
        List).


%% @spec list_move(SrcElem::term(), DstElem::term(), Mode, List::[term()]) ->
%%           NewList::[term()]
%%      Mode = before | after
%% @doc Move the first occurrence of `SrcElem' before/after the first occurrence
%%      of `DstElem'. Technically it works the following way: erase the first
%%      occurrence of `SrcElem' from `List' and replace the first occurrence
%%      of `DstElem' to `SrcElem, DstElem' or `DstElem, SrcElem' sequence.
list_move(SrcElem,DstElem,before, List) when SrcElem=/=DstElem, is_list(List) ->
    list_move_(SrcElem, DstElem, fun(Id) -> Id end, List);
list_move(SrcElem,DstElem,'after',List) when SrcElem=/=DstElem, is_list(List) ->
    list_move_(SrcElem, DstElem, fun lists:reverse/1, List).

list_move_(SrcElem, DstElem, DirFun, List) ->
    % Reverse list until find source and destination also
    {RevList1, _, Cnt1} = partfold(
        fun(Elem, {RevList, Ctrls, Cnt}) when Elem==SrcElem ->
            {hd(Ctrls), {RevList, tl(Ctrls), Cnt+1}};
        (Elem, {RevList, Ctrls, Cnt}) when Elem==DstElem ->
            {hd(Ctrls), {DirFun([DstElem,SrcElem])++RevList, tl(Ctrls), Cnt+1}};
        (Elem, {RevList, Ctrls, Cnt}) ->
            {next, {[Elem|RevList], Ctrls, Cnt+1}}
        end,
        {[], [next,stop], 0},
        List),
    % Concat modified reverse head with the tail
    lists:reverse(RevList1) ++ lists:nthtail(Cnt1, List).



%%% ----------------------------------------------------------------------------
%%% String functions

%% @spec string_char_type(CharCode::integer()) -> CharType
%%       CharType = control | punctuation | digit | uppercase | lowercase |
%%                  unknown
%% @doc  Give back the type of the character specified by `CharCode'. If
%%       the type is unknown the result is `unknown'. Based on the erlang
%%       syntax.
%%
%% See
%% <A HREF="http://www.erlang.org/doc/reference_manual/introduction.html#1.6"
%%      TARGET="_blank"><TT>Erlang Reference Manual - 1.6 Character Set</TT></A>
string_char_type(CharCode) when is_integer(CharCode) ->
    ChTypes = [
        { 32,  47, punctuation},    % SP !"#$%&'()*+,-./
        { 48,  57, digit},          % 0..9
        { 58,  64, punctuation},    % :;>=<?@
        { 65,  90, uppercase},      % A..Z
        { 91,  96, punctuation},    % [\]^_`
        { 97, 122, lowercase},      % a..z
        {123, 126, punctuation},    % {|}~
        {128, 159, control},        %
        {160, 191, punctuation},    %
        {192, 214, uppercase},      %
        {215, 215, punctuation},    %
        {216, 222, uppercase},      %
        {223, 246, lowercase},      %
        {247, 247, punctuation},    %
        {248, 255, lowercase}],     %
    CheckFun = fun({B, E, _T}) -> (CharCode<B) orelse (E<CharCode) end,
    case lists:dropwhile(CheckFun, ChTypes) of
        [] -> unknown;
        [{_B, _E, Type}|_] -> Type
    end.


%% @spec string_is_letter(CharCode::integer()) -> bool()
%% @doc  Check the `CharCode' represent a letter.
%% @see string_char_type/1
string_is_letter(CharCode) when is_integer(CharCode) ->
    Type = string_char_type(CharCode),
    (Type==uppercase) orelse (Type==lowercase).


%% @spec string_strs(String::string(), SubStrings::[string()]) ->
%%           {FirstPosition::integer(), MatchSubString::string()}
%% @doc  Find first substring in the `String'. Same as `string:str/2' but it
%%       find more than one substring. The returned pair is contained the lowest
%%       matching position of substrings, and the first from macthed substings
%%       at that position. If theres is no matched substring the position is 0.
%%
%% Example:
%% ```
%% > string_strs("Hello World!", ["Hello","World"]).
%% {1, "Hello"}
%% '''
string_strs(String, SubStrs) when is_list(String), is_list(SubStrs)->
    % Find first occurence of delimiters
    FirstFun = fun(Sub, {MinPos, MinSubStr}) ->
        Pos = string:str(String, Sub),
        if
            0<Pos andalso Pos<MinPos -> {Pos, Sub};
            true -> {MinPos, MinSubStr}
        end
    end,
    {MinPos, MinSubStr} =
        lists:foldl(FirstFun, {string:len(String)+1,""}, SubStrs),
    % Return founded position
    if
        MinSubStr/="" -> {MinPos, MinSubStr};
        true  -> {0, MinSubStr}
    end.


%% @spec string_length(String::string()) -> Length::integer()
%% @doc  Calculate the length of `String'. Windows new line characters are
%%       counted as one character.
%% @see string_length/2
string_length(String) when is_list(String) ->
    string_length(String, ["\r\n"]).

%% @spec string_length(String::string(), MultiByteChars::[string()]) ->
%%           Length::integer()
%% @doc  Calculate the length of `String'. All multibyte character given in
%%       `MultiByteChars' are counted as one charcter.
string_length(String, MultiByteChars) when is_list(String),
                                           is_list(MultiByteChars) ->
    string_length_(String, MultiByteChars, 0).

string_length_(String, MultiByteChars, Length) ->
    case string_strs(String, MultiByteChars) of
        {0,_} ->
            Length + string:len(String);
        {Pos,ChrStr} ->
            string_length_(lists:nthtail(Pos+string:len(ChrStr)-1,String),
                MultiByteChars, Length+Pos)
    end.


%% @spec string_EOLs() -> [string()]
%% @doc  Supported "End of Line" charcter sequences. Currently support Windows
%%       (\r\n), Macintosh (\r) and Unix (\n) end of line character sequences.
string_EOLs() ->
    ["\r\n","\r","\n"].  % [Windows, Macintosh, Unix]


%% @spec string_lines(String::string()) ->
%%           {Lines::[string()], FoundedEOLs::integer()}
%% @doc  Broke `String' into lines. Lines are also conatin the "End of Line"
%%       character sequences.
%% @see string_lines/2
string_lines(String) when is_list(String) ->
    string_lines(String, string_EOLs()).

%% @spec string_lines(String::string(), EOLs::[string()]) ->
%%           {Lines::[string()], FoundedEOLs::integer()}
%% @doc  Broke `String' into lines. Lines are also conatin the End of Line
%%       character sequences.
%% @see string_split/5
string_lines(String, EOLs) when is_list(String), is_list(EOLs) ->
    string_split(String, EOLs, 0, true, true).


%% @spec string_split(String::string(), DelimStrs::[string()], Count::integer(),
%%               Empty::bool(), WithDelim::bool()) ->
%%           {Parts::[string()], FoundedDelims::integer()}
%% @doc  Split `String' into `Parts' by strings from `DelimStrs'. Find
%%       only first `Count' part and the all remain string will be the
%%       `Count'+1-th part. If `Count' is lower then 1 all part will be
%%       returned.
%%       If `Empty' is true the parts with zero length are also given back in
%%       `Parts'. If `WithDelim' is true the string parts and the delimiter
%%       strings will be concatented.
%%
%% Example:
%% ```
%% > string_split("123.::456.789::", [".","::"], 2, true, true).
%% {["123.","::","456.789::"], 2}
%% '''
%%
%% @see string_strs/2
string_split(String, DelimStrs, Count, Empty, WithDelim) when is_list(String),
        is_list(DelimStrs), is_integer(Count), is_boolean(Empty),
        is_boolean(WithDelim) ->
    % String2 = lists:flatten(String),
    % DelimStrs2 = lists:map(fun lists:flatten/1, DelimStrs),
    string_split_(String, DelimStrs, Count, Empty, WithDelim, {[],0,0}).

%% @private
string_split_(String, DelimStrs, Count, Empty, WithDelim,
        {Parts,PartsLen,Founds}) ->
    % Find first occurence of delimiters
    {MinPos0, MinDelim} = string_strs(String, DelimStrs),
    % If enough parts are founded
    MinPos = if
        0<Count andalso Count=<PartsLen -> 0;
        true -> MinPos0
    end,
    % Set splitting parameters
    {Founds2, Pos, Len} = if
        0<MinPos -> {Founds+1, MinPos, length(MinDelim)};
        true     -> {Founds,   length(String)+1, 0}
    end,
    % Split with or without delimiter string
    {Part1, TailStr} = if
        WithDelim ->
            lists:split(Pos+Len-1, String);
        true ->
            {H,  T0} = lists:split(Pos-1, String),
            {_T1,T2} = lists:split(Len,   T0),
            {H, T2}
    end,
    % Add splitted part to parts if necessarry
    {Parts2, PartsLen2} = if
        ""==Part1 andalso (not Empty) -> {Parts, PartsLen};
        true -> {[Part1|Parts], PartsLen+1}
    end,
    % Process remains string if necessary
    if
        0==MinPos ->
            {lists:reverse(Parts2), Founds2};
        true ->
            string_split_(TailStr, DelimStrs, Count, Empty, WithDelim,
                {Parts2, PartsLen2, Founds2})
    end.


%% @spec string_replace(String::string(), SrcStrs::[string()],
%%               DstStr::string(), Count::integer()) -> string()
%% @doc  Find substrings from `SubStrs' in `String' and replace them to
%%       `DstStr'. If `Count' is positive than replace only the first `Count'
%%       occurence otherwise replace all.
string_replace(String, SrcStrs, DstStr, Count) when is_list(String),
        is_list(SrcStrs), is_list(DstStr), is_integer(Count) ->
    % Split string by SrcStrs
    {Parts, Found} = string_split(String, SrcStrs, Count, true, false),
    if
        0<Found -> % Rejoin parts with DstStr
            RevParts = lists:reverse(Parts),
            lists:foldl(
                fun(Str, AccStr) -> Str++DstStr++AccStr end,
                hd(RevParts),
                tl(RevParts));
        true -> % Not found subsrings
            String
    end.


%% @equiv string_trim(String,both)
%% @spec (string()) -> string()
string_trim(String) ->
    string_trim(String, both).

%% @type trim_mode() = 'left'|'right'|'both'

%% @equiv string_trim(String,Mode," \t")
%% @spec (string(),trim_mode()) -> string()
string_trim(String, Mode) ->
    string_trim(String, Mode, " \t").

%% @doc Trims off a set of characters (e.g., whitespace) from leading (left),
%%      trailing (right) or both ends of a string.
%% @spec (string(),trim_mode(),[char()]) -> string()
string_trim(String, Mode, TrimChrs) ->
    String1 =
        case lists:member(Mode, [left, both]) of
            true ->
                lists:dropwhile(fun(Ch) -> lists:member(Ch, TrimChrs) end,
                                String);
            _ ->
                String
        end,
    case lists:member(Mode, [right, both]) of
        true -> lists:reverse(lists:dropwhile(
                                    fun(Ch) -> lists:member(Ch, TrimChrs) end,
                                    lists:reverse(String1)));
        _ -> String1
    end.



%% @doc Gives the line and column of the given position in the string.
%%      Note that if the given position is on a newline then
%%      the coordinates of the first character of the line following
%%      the one closed by the said newline is returned.
%%      If the position is out of range then an extreme is returned.
%% @spec (string(), natural()|'inf') -> {natural(), natural()}
string_linecol(S,P) when is_list(S), (is_integer(P) or (P==inf)) ->
    string_linecol(S,P,{1,1,1}).

%% @doc Similar to string_linecol/2, but can be given starting row, column and
%%      linear accumulator values.
%% @see string_linecol/2
%% @spec (string(), natural()|'inf',
%%        {Acc::integer(),StartRow::integer(),StartCol::integer()}) ->
%%            {natural(), natural()}
string_linecol([$\n|T],P,{A,R,_})          -> string_linecol(T,P,{A+1,R+1,1});
string_linecol([_  |T],P,{A,R,C}) when A<P -> string_linecol(T,P,{A+1,R,C+1});
string_linecol(_,      _,{_,R,C})          -> {R,C}.


%% @spec (string()) -> string()
%% @doc Converts a string from any newline format (CR,LF,CR+LF) to UNIX.
dos2unix(Str) ->
    W   = [$\n], %wanted newline sequence in reverse
    FSM = fun($\r, {L,_N}) -> {W++L ,[]} ;
             ($\n, {L, N}) -> {N++L ,W } ;
             (C  , {L,_N}) -> {[C|L],W } end,
    {L,_N} = lists:foldl(FSM, {[]   ,W }, Str),
    lists:reverse(L).


%% @doc Returns the canonical name of the file, which does not contain ".."
%% in the path. Should work for both Unix and Windows (and mixed) style paths.
canonical_filename(Filename) ->
    canonical_filename2(filename:absname(Filename)).

canonical_filename2(Filename) ->
    RmBackSlash = elim_up_dir(Filename, [$\\, $\\]),
    RmFwdSlash  = elim_up_dir(RmBackSlash, "/"),
    case RmFwdSlash of
        Filename -> Filename;
        Shorter  -> canonical_filename2(Shorter)
    end.

%% @doc Eliminates a "/../dir/" section from the path.
%% `Sep' is the path separator, which has to expand to one regexp character.
elim_up_dir(Filename, Sep) ->
    re:replace(Filename,
               "[^" ++ Sep ++ "]+" ++ Sep ++ "[.][.]" ++ Sep ++ "?", "",
               [{return, list}]).

%% @doc Returns a string in flattened form, and converts any other term into
%%      printable form.
%% @spec (any()) -> string()
any_to_string(X) ->
    lists:flatten(case io_lib:deep_char_list(X) of
        true  -> X;
        false -> io_lib:print(X)
    end).

%% @spec integer_to_list(integer(),integer()) -> string()
%%
%% @doc Converts `N' to string with `P' leading zeros.
integer_to_list(N,P) ->
    case lists:flatten(io_lib:format("~"++integer_to_list(P)++"..0b", [N])) of
        %% if "***" was returned, it means that N does not fit in P characters.
        [$*|_] -> integer_to_list(N);
        Result -> Result
    end.

is_erl(F)->
    nomatch /= re:run(F, "\\.[eE][rR][lL]$", [{capture,first}]).
%    lists:suffix(".erl",File).

is_beam(F)->
    nomatch /= re:run(F, "\\.[bB][eE][aA][mM]$", [{capture,first}]).
%    lists:suffix(".beam",File).

'or'(F,G) when is_function(F,1), is_function(G,1)->
    fun(X)->
            F(X) orelse G(X)
    end.


%%% ----------------------------------------------------------------------------
%%% ETS functions

%% @spec ets_exist(EtsTableID::tid()) -> bool()
%% @doc Return `true' if the ETS table identified by `EtsTableID' is exists
%%      otherwise `false'.
ets_exist(EtsTableID) ->
    undefined =/= ets:info(EtsTableID, type).


%% @spec ets_keys(EtsTableID::tid()) -> [term()]
%% @doc  Returns the keys of the ETS table identified by `EtsTableID'.
%%       If the type ot the ETS table is `ordered_set' than list is also
%%       ordered. In other cases the order is undefined it depending on the
%%       storage order.
ets_keys(EtsTableID) ->
    ets:select(EtsTableID,
        [{'_', [], [{element,ets:info(EtsTableID,keypos),'$_'}]}]).


%% @spec ets_list2tab(Objects::[Object], TableETS::tid()) -> ok
%%       Object = tuple()
%% @doc  Load the object from `Objects' into the `TableETS' table.
ets_list2tab(Objects, TableETS) ->
    lists:foreach(fun(Object) -> ets:insert(TableETS, Object) end, Objects).


%% @spec ets_clone(EtsTableID::tid()) -> CloneEtsTableID::tid()
%% @doc  Create an new ETS table with same options as `EtsTableID' and
%%       copy the content.
%%       If the original table is named the new table will not be named table.
ets_clone(EtsTableID) ->
    % Get informations about original table
    Name        = ets:info(EtsTableID, name),
    Type        = ets:info(EtsTableID, type),
    Protection  = ets:info(EtsTableID, protection),
    Keypos      = ets:info(EtsTableID, keypos),
    % Create same new table
    EtsTableID2 = ets:new(Name, [Type, Protection, {keypos,Keypos}]),
    % Copy content
    ets:foldl(
        fun(Object,_) -> ets:insert(EtsTableID2,Object) end,
        undefined,
        EtsTableID),
    % Return the identifier of clone table
    EtsTableID2.



%%% ----------------------------------------------------------------------------
%%% Math functions

%% @spec math_floor(Number::number()) -> Floor::integer()
%% @doc  Return the biggest integer which is less or equal than `Number'.
math_floor(Number) when is_number(Number) ->
    Trunc = trunc(Number),
    if
        Trunc=<Number -> Trunc;
        true          -> Trunc-1
    end.


%% @spec math_ceil(Number::number()) -> Ceil::integer()
%% @doc  Return the smallest integer which is greater or equal than `Number'.
math_ceil(Number) when is_number(Number) ->
    Trunc = trunc(Number),
    if
        Number=<Trunc -> Trunc;
        true          -> Trunc+1
    end.



%%% ----------------------------------------------------------------------------
%%% OS functions

%% @todo os_cmd/1 with command line parsing
%% @todo binary version

%% @doc a platform independent replacement for os:cmd/1
%% @spec (string(),[string()]) -> {integer() | 'timeout',string()}
os_cmd(Cmd,Args) ->
    NeedQ = fun(S) ->
                    lists:any(fun(C)->(C<33)or(C>126)or(C==$\\)end,S)
            end,
    Q = fun(S) ->
                Esc = lists:flatmap(fun($")->"\\\""; %$"
                                       (C) ->[C]
                                    end,S),
                case NeedQ(Esc) of
                    true ->
                        "\"" ++ Esc ++ "\"";
                    false ->
                        Esc
                end
        end,
    case os:find_executable(Cmd) of
        false ->
            {127,""};
        Exec=[_|_] ->
            os_cmd_(string:join([Q(Exec)|lists:map(Q,Args)]," "))
    end.

%% @doc a platform independent replacement for os:cmd/1
%% @spec (string()) -> {integer() | 'timeout',string()}
os_cmd_(CmdLine) ->
    Opts = [stderr_to_stdout,exit_status,in,hide],
    P = open_port({spawn,CmdLine},Opts),
    get_data(P,"").

get_data(P,Old) ->
    receive
        {P,{data,New}} ->
            get_data(P,[New|Old]);
        {P,{exit_status,St}} ->
            {St,lists:flatten(lists:reverse(Old))}
    after
        ?EXEC_TIMEOUT ->
            {timeout,Old}
    end.

find_files(File, Mask) when is_list(File), is_function(Mask,1) ->
    case filelib:is_dir(File) of
        true ->
            Files = filelib:wildcard(filename:join(File,"*")),
            lists:append([find_files(F, Mask) || F<-Files]);
        false ->
            case filelib:is_regular(File) of
                true  ->
                    case Mask(File) of
                        true ->
                            [File];
                        false ->
                            []
                    end;
                false ->
%                    message("Warning: Not a file: " ++ File),
                    []
            end
    end.

%% Dropping the loaded files from the graph.
%% Dropping a file will drop its includers as well;
%% these will not be dropped again.
drop_files([]) ->
    ok;
drop_files([File|Files]) ->
    AllFiles  = ?Query:exec([file]),
    case lists:member(File, AllFiles) of
        true ->
            Includers = ?Query:exec(File, ?File:included()) -- [File],
            ?FileMan:drop_file(File),
            Rest = Files -- Includers;
        false ->
            Rest = Files
    end,
    drop_files(Rest).

%% ---------------------------------------------------------------------------
%% @spec database_hash() -> {tuple(),int()}
%% @doc Gives back the hash value of the current database.
%% Function for checking whether the database was changed or not.
database_hash() ->
    SynHash = case ?Syn:get_env(?syn_database_hash) of
        [Hash] ->
            Hash;
        [] ->
            Hash = ?FileMan:db_hash(),
	        ?FileMan:set_db_hash(Hash),
	        Hash
     end,
     SemHash = case ?Syn:get_env(?sem_database_hash) of
        [Version] ->
            Version;
        [] ->
            ?FileMan:inc_sem_db_hash()
    end,
    {SemHash, SynHash}.


%% @spec (file()) -> int()
%% @doc Gives back the hash value the given file node.
%% Function for checking whether a file node was changed or not.
file_hash(FileNode) ->
    ?File:hash(FileNode).


%% Side effects are not to be checked if the environment variable
%% `no_check_sideeffect' is set.
is_sideeffect_check_on() ->
    ?Syn:get_env(no_check_sideeffect) == [].

parallelise(List, Module, Function, MakeProperArgs)->
    parallelise(List, Module, Function, MakeProperArgs, true, normal).

parallelise(List, Module, Function, MakeProperArgs, true)->
    parallelise(List, Module, Function, MakeProperArgs, true, normal);

parallelise(List, Module, Function, MakeProperArgs, false)->
    parallelise(List, Module, Function, MakeProperArgs, false, normal);
    
parallelise(List, Module, Function, MakeProperArgs, DBUsage)->
    parallelise(List, Module, Function, MakeProperArgs, true, DBUsage).

parallelise(List, Module, Function, MakeProperArgs, ReturnZipped, DBUsage) when is_list(List)
    andalso is_atom(Module) andalso is_atom(Function) andalso is_atom(DBUsage)
    andalso is_function(MakeProperArgs, 1)->
    TKeys =
    [async_call(node(), Module, Function,
                    MakeProperArgs(NodeGroup))
         || NodeGroup <- partition_list_according_cpus(List, DBUsage)],
    case ReturnZipped of
        true -> lists:zip(List, lists:append([yield(K) || K <- TKeys]));
        false -> lists:append([yield(K) || K <- TKeys])
    end;

parallelise(List, Module, Function, MakeProperArgs, ReturnZipped, DBUsage) when is_list(List)
    andalso is_atom(Module) andalso is_atom(Function)
    andalso is_function(MakeProperArgs, 2)->
    TKeys =
    [async_call(node(), Module, Function,
                    MakeProperArgs(NodeGroup, Index))
         || {NodeGroup, Index} <- ?MISC:index_list(partition_list_according_cpus(List, DBUsage))],
    case ReturnZipped of
        true -> lists:zip(List, lists:append([yield(K) || K <- TKeys]));
        false -> lists:append([yield(K) || K <- TKeys])
    end.

partition_list_according_cpus(List) ->
    partition_list_according_cpus(List, normal).

partition_list_according_cpus(List, DBUsage) ->
    Modifier =
        case DBUsage of
            heavy -> 0.5;
            low -> 4;
            _ -> 1
        end,
    Length = length(List),
    GroupNum = (Length div trunc(processor_count() * Modifier)) + 1, % todo: check which one is better!
    ?MISC:slice_list(GroupNum, List).

processor_count()->
    LogProcA = case erlang:system_info(logical_processors_available) of
            unknown ->
                2;
            Na ->
                Na
        end,
    LogProcO = case erlang:system_info(logical_processors_online) of
            unknown ->
                2;
            No ->
                No
        end,
    lists:max([LogProcO, LogProcA]).


is_client() ->
    ?REFERL_NODE /= node().

general_closure(WorkerFun, InitialItems)
  when is_function(WorkerFun), is_list(InitialItems)->
    Tab = ets:new(store, [public, set, {write_concurrency, true}]),
    general_closure0(WorkerFun, InitialItems, Tab),
    Result = lists:flatten(ets:match(Tab,{'$1'})),
    ets:delete(Tab),
    Result.

general_closure0(WorkerFun, InitialItems, Tab) ->
    Items = WorkerFun(InitialItems),
    ReallyNewItems =
        [begin
             ets:insert(Tab, {Item}),
             Item
         end || Item <- Items, not  ets:member(Tab, Item)],
    Keys = [ rpc:async_call(node(), ?MODULE, general_closure0,
                            [WorkerFun, PackedItems, Tab])
             || PackedItems <- partition_list_according_cpus(ReallyNewItems),
                PackedItems/=[]],
    [ ok = rpc:yield(K) || K <- Keys],
    ok.

async_call(Module, Function, Args)->
    async_call(node(), Module, Function, Args).

async_call(Node, Module, Function, Args)->
    rpc:async_call(Node, ?MODULE, async_call_helper,
                            [[Module, Function | Args]]).

async_call_helper([Module, Function | Args])->
    try
        {result, apply(Module, Function, Args)}
    catch
        error:E ->
            {error, {error, E}, erlang:get_stacktrace()};
        Cl:E ->
            {error, {Cl, E}}
    end.

yield(Key)->
    case rpc:yield(Key) of
        {result, Result} -> Result;
        {error, {throw,T}} -> throw(T);
        {error, {exit,Exit}} -> exit(Exit);
        {error, {error,Error}, Stack} -> error(Error, Stack)
    end.
%%
%% @doc Prints out every function block which matches the basic regular expression.
%%
regexp(Value) ->
    case Value of
          % the Value contains list of regexps 
          [Hd | _] when is_list(Hd) ->  Modules = mods_from_fbs(Value, []); 
          % Value is a filename that contains the regexps
          [Hd| _] when not is_list(Hd) -> Modules = mods_from_fbs(read(Value), []); 
           % atoms and numbers are not allowed in the regexp value
          _ -> Modules = {}, throw(?LocalError(regexp_bad_regexp, [Value]))
    end,
    Modules.

%% ============================================================================
%% Modules from  file
mods_from_fbs(undefined, _)->
    mods_from_fbs(?BASIC_REGEXP, []);
mods_from_fbs([], Mods)->
    lists:reverse(Mods);
mods_from_fbs([[Element | _] = Hd | Rest], Mods) when is_atom(Element)->
    mods_from_fbs(Rest, [{Hd, get_modules(Hd)} | Mods]);
mods_from_fbs([Hd | Rest],Mods)->
    ModuleFiles = ls_files(),
    mods_from_fbs(Rest,[{Hd, 
            dir_files2(match(ModuleFiles,[Hd]), [])} | Mods]).
%% ============================================================================
get_modules(ModuleList)->
    [Mod || {_, Mod} <- ls_path(),
        lists:member(?Mod:name(Mod), ModuleList) == true].
%% ============================================================================
dir_files2([], Result)->
    Result;
dir_files2([Hd | Rest], Result)->
    Element = ?Query:exec(
               ?Mod:find(
                 list_to_atom(
                filename:rootname(
                    filename:basename((Hd)))))), %% .erl and .beam files could exist
    case {lists:member(Element, Result), Element} of
        {_, []} ->
            dir_files2(Rest, Result);
        {true, _}->
            dir_files2(Rest, Result);
        {false, _} ->
            dir_files2(Rest, [hd(Element) | Result])            
    end.
%% ============================================================================
%% Listing every files
ls_files()->
    lists:usort([{?File:path(Path), Mod} || Mod <- ?Query:exec(?Mod:all()), 
                         Path <- ?Query:exec(Mod, ?Mod:file())]).
%% ============================================================================
%% Reading file
read(File)->
    case file:open(File, [read]) of
   {ok, Dev}->
      try
         read_line(file:read_line(Dev), Dev, [])
      after
          file:close(Dev)
      end;
   {error, Reason} ->
       throw(?LocalError(regexp_file_failed_to_open, [File, Reason]))
    end.
%% ============================================================================
read_line({ok, "\n"}, File, List)->
    read_line(file:read_line(File), File, List);
read_line({ok, Data}, File,  List)->
    NewData = leave(Data),
    read_line(file:read_line(File), File, check(re:compile(NewData), NewData,  List));
read_line(eof, _, List) ->
    List;
read_line({error, Reason}, _, _) ->
    io:format(file:format_error(Reason)).
%% ============================================================================
check({ok, _}, Data, List)->
    [Data | List];
check({error, {ErrString, Position}}, Data, List)->
    io:format("In regexp: ~s~nError: ~s at position ~p~n ", [Data, ErrString, Position]),
    List.
%% ============================================================================
leave(Data)->
    string:substr(Data, 1, string:len(Data)-1).

%%==========================================================================
%% Matching with regular expressions
match([{_, _} | _] = PathList, RegExpList)->
    [Path|| {Path, _} <- PathList,
        RegExp <- RegExpList,
        match_single(Path, RegExp) == true];
match(PathList, RegExpList)->
    [Path|| Path <- PathList,
        RegExp <- RegExpList,
        match_single(Path, RegExp) == true].

match_single(Path, RegExp)->
     match_regexp(re:run(Path, RegExp, [])).


match_regexp(nomatch)-> false;
match_regexp({match, [{0, _} | _]})-> true;
match_regexp({match, [{_, _} | _]})-> true.

%%======================================================================
%% Listing every path
ls_path()->
    lists:usort([{filename:dirname(?File:path(Path)), Mod} || Mod <- ?Query:exec(?Mod:all()), 
                         Path <- ?Query:exec(Mod, ?Mod:file())]).
