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

%%% @doc A duplicated code search algorithm based on suffix tree.

%%% @author Szabo Bence <szbtadi@caesar.elte.hu>

-module(refusr_clone_identifierl_suffix_tree).
-vsn("$Rev: 11297 $").
-include("user.hrl").
-include("refusr_clone_identifierl.hrl").

%%nif functions
-export([search_dupcode/3, search_dupcode_by_pos/3]).
%%search functions
-export([default_options/0, get_clones/0, get_clones/1,
         trim_to_clone0/2, get_subjects/1]).

%@private
-export([trim_clone_map/2]).
%%------------------------------------------------------------------------------
-define(LEXTYPES, [variable,atom,string,integer,float,param,arg,subst]).
-define(DUPFORMTYPES, [macro,record,module,export,include,import]).
-define(DUPCODE, dupcode).
-define(HELPER, helper).

-on_load(load_nif/0).
-record(trim_unit, {lex_list  = [] :: list(),
                    nodes     = [] :: list(),
                    length    = -1 :: integer(),
                    left_rem  = [] :: list(),
                    right_rem = [] :: list()}).

-record(data, {ets    = -1,
               minlen = -1 :: integer()}).

%%% ============================================================================
%%% Interface

default_options() ->
    [{minlen, 32},
     {minnum, 2},
     {files,[]},
     {subject, all}].

get_clones()->
    get_clones(default_options()).


get_clones(Options) when is_list(Options) ->
    case proplists:get_value(subject, Options, all) of
        all -> find_all_clones(Options);
        _ -> find_specific_clones(Options)
    end.

%% @doc Finds all clones speficied in options or the whole database
find_all_clones(Options) ->
    FormsWClauses = forms_with_clauses(Options),
    Minlen = proplists:get_value(minlen, Options),
    Minnum = proplists:get_value(minnum, Options),

    LongClauses = get_possible_clauses(FormsWClauses,Minlen),
    ToFile = create_suffix_string(LongClauses),
    ICs = initial_clones(fun ?MODULE:search_dupcode/3,
                         ToFile,LongClauses,Minlen,Minnum),

    Ets = ets:new(suffix_tree,[public,bag,{write_concurrency,true}]),
    Data = #data{ets = Ets, minlen = Minlen},
    trim_clones(ICs,Data),
    find_forms(FormsWClauses,Minlen,Minnum,Ets),
    Groups = get_groups_from_ets(Ets),
    ets:delete(Ets),

    Filtered = filter_clones(Groups,Minlen,Minnum),
    DroppedGroups = drop_groups(Filtered),
    UniGroups = to_uni_format(DroppedGroups,[]),
    [{analysed_candidates_num, no_data},
     {detected_clones_num, length(UniGroups)},
     {detected_clones, UniGroups}].


%% @doc Finds specific clones using subjects parameter.
find_specific_clones(Options) ->
    case proplists:get_value(subject, Options, []) of
        [] ->     
            [{analysed_candidates_num, no_data},
             {detected_clones_num, 0},
             {detected_clones, []}];
        _ ->
            find_specific_clones0(Options)
    end.
find_specific_clones0(Options) ->
    FormsWClauses = forms_with_clauses(Options),
    Minlen = proplists:get_value(minlen, Options),
    Minnum = proplists:get_value(minnum, Options),
    Subjects = proplists:get_value(subject, Options),

    {_,Separator} = lists:keyfind(',', 1, ?Alphabet:default_alphabet()),
    SubjectsWLeaves =
        [ [{Subject,?Syn:leaves2(Subject)} || Subject<-Subject0]
            || Subject0<-Subjects],
    Patterns =
        [merge_tokens(Subject,Separator,[]) || Subject<-SubjectsWLeaves],

    PatternsToFile =
        integer_to_list(length(Patterns))++"\n"++lists:flatten(Patterns),
    LongClauses = get_possible_clauses(FormsWClauses,1),
    ToFile = create_suffix_string(LongClauses)++PatternsToFile,
    ICs = initial_clones(fun ?MODULE:search_dupcode_by_pos/3,
                         ToFile,LongClauses,Minlen,Minnum),

    Ets = ets:new(suffix_tree,[public,bag,{write_concurrency,true}]),
    Data = #data{ets = Ets, minlen = Minlen},
    trim_clones(ICs,Data),
    clause_to_form(FormsWClauses,1,Minnum,Ets),
    Groups = get_groups_from_ets(Ets),
    ets:delete(Ets),

    Filtered = filter_clones(Groups,Minlen,Minnum),
    DroppedGroups = drop_groups(Filtered),
    UniGroups = to_uni_format(DroppedGroups,[]),
    [{analysed_candidates_num, no_data},
     {detected_clones_num, length(UniGroups)},
     {detected_clones, UniGroups}].


%%% ============================================================================
%%% Main Search functions

%% @doc Returns the database elements like [{form,[{clause,lexlist,tokens}]}].
forms_with_clauses(Options) ->
    Files = get_files_from_options(proplists:get_value(files, Options)),
    Funcs =
        [Form || Form <- ?Query:exec(Files,?File:forms()),
            ?Form:type(Form) == func],
    _FormsWClauses = get_clauses_with_leaves(Funcs,[]).

%% @doc Filters the good clauses that are longer or equal than minlen.
get_possible_clauses(FormsWClauses,Minlen) ->
    [{Clause,LexList,Tokens} ||
        {_,Clauses} <- FormsWClauses, {Clause,LexList,Tokens} <- Clauses,
            length(Tokens)>=Minlen].


%% @doc Creates a string that the C suffix tree algorithm uses to search.
create_suffix_string(LongClauses) ->
    List = get_list(LongClauses),
    integer_to_list(length(List))++"\n"++lists:flatten(List).

%% @doc Calculates the initial clones using the C suffix tree algorithm.
initial_clones(SearchFun,ToFile,LongClauses,Minlen,Minnum) ->
    TempFile = ?TEMPFILE,
    case file:write_file(TempFile, ToFile) of
        {error, _} -> throw(?LocalErr0r(file_write_error));
        ok -> ok
    end,
    Res = SearchFun(TempFile,Minlen,Minnum),
    Cs = case Res of
        0 -> [];
        _ -> {ok,Cs0} = file:consult(TempFile),
             Cs0
        end,
    file:delete(TempFile),
    Css = case Cs of
        [] -> [];
        _  -> lists:usort(hd(Cs))
    end,
    _ICs = get_tokens(LongClauses,Css).

%% @doc Trims the initial clones.
trim_clones(ICs, Data) ->
    ParArgs = fun(Arg)-> [Arg,Data] end,
    ?MISC:parallelise(ICs, ?MODULE, trim_clone_map, ParArgs, false).


%% @doc Merges the clauses to forms and finds the short forms
%%      that has not long enough clause
find_forms(FormsWClauses,Minlen,Minnum,Ets) ->
    ShortForms =
        [{to_form(Form,ClausesWLeaves),ClausesWLeaves} ||
             {Form,ClausesWLeaves}<-FormsWClauses,
                  is_short(ClausesWLeaves,Minlen)],
    Matched = match_forms(ShortForms,Minnum,[]),
    Forms = [ Form || {Form,_} <- Matched],
    [ets:insert(Ets,{Form#unit.alphabet,form,[Form]}) || Form<-Forms],
    clause_to_form(FormsWClauses,Minlen,Minnum,Ets).


%%% ----------------------------------------------------------------------------
%%% Functions for preparations

%% @doc Returns the files from the options.
get_files_from_options([]) ->
   [File || File <- get_all_files(), ?File:type(File) == module];
get_files_from_options(Files) ->
    lists:usort(
        lists:flatten([get_file_from(File) || File <- Files])).

%% @doc Returns the files given by regexp or full path or module names.
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
        _ -> io:format("Warning: ~p appears more than once in the database.~n",
                        [File]),
             FileNode
    end;
get_file_from(File) when is_list(File) ->
    DBFilePaths = [?File:path(DBFile) || DBFile <- get_all_files()],
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

%% @doc Returns all files in the database.
get_all_files() ->
    ?Query:exec(?File:all()).

%% @doc Calculates the forms clauses and its leaves and lexlists.
get_clauses_with_leaves([], FormsWClauses) -> FormsWClauses;
get_clauses_with_leaves([H|T], FormsWClauses) ->
    Clauses0 = ?Query:exec(H,?Form:clauses()),
    ClausesWithLeaves =
        [begin L = ?Syn:leaves2(Clause), {Clause,L,process_tokens(L)} end
             || Clause<-Clauses0],
    get_clauses_with_leaves(T,[{H,ClausesWithLeaves}|FormsWClauses]).


%%% ----------------------------------------------------------------------------
%%% Functions for initial clone detection

%% @doc Merges the leaves for C suffix tree algorithm.
get_list(ClausesWithLeaves) ->
   [ [[Token,","] || Token <- Tokens ]++"$\n"
        || {_,_,Tokens} <- ClausesWithLeaves].
        
%% @doc Creates #trim_unit records using the C suffix tree result.
get_tokens(Clauses, Cs) ->
    [ get_tokens_form_funcs(Clauses,PosList,Len) || {PosList,Len,_} <- Cs].

%% @doc Maps the PosList using Clauses.
get_tokens_form_funcs(Clauses,PosList,Length) ->
    [ get_tokens_from_func(lists:nth(NthClause,Clauses),StartPos,Length) ||
         {NthClause,StartPos} <- PosList].

%% @doc Returns the #trim_unit record correspoding the Start and Length
get_tokens_from_func({Func,Leaves,_Tokens},_Start,Length)
        when length(Leaves) == Length ->
    #trim_unit{nodes=[Func]}; % if the whole clause found as clone
get_tokens_from_func({_Func,Leaves,_Tokens},Start,Length) ->
    LexList = lists:sublist(Leaves,Start,Length),
    #trim_unit{lex_list = LexList}.


%%% ----------------------------------------------------------------------------
%%% Functions for trimming

%% @doc Trims all #trim_unit.
trim_clone_map(ListOfTrimUnits,Data) ->
    [trim_clone(TrimUnits,Data) || TrimUnits<-ListOfTrimUnits].

%% @doc Main loop of clone trimming.
trim_clone(ListOfTrimUnits, Data) ->
    Res = [common_cut(TrimUnit, Data) || TrimUnit<-ListOfTrimUnits],
    Ets = Data#data.ets,

    insert_to_ets(Res,Ets),
    case is_diff_length(Res) orelse is_diff_type(Res) of
        true -> insert_to_trim(Res,Ets);
        _ -> ok
    end,
    LeftTrimUnits  =
        [#trim_unit{lex_list = K#trim_unit.left_rem,
         length = K#trim_unit.length,
         nodes = K#trim_unit.nodes} || K<-Res],

    RightTrimUnits =
        [#trim_unit{lex_list = K#trim_unit.right_rem,
         length = K#trim_unit.length,
         nodes = K#trim_unit.nodes} || K<-Res],

    case lists:all(fun(K) -> K#trim_unit.lex_list == [] end, LeftTrimUnits) of
        false -> trim_clone(LeftTrimUnits, Data);
        _ -> ok
    end,
    case lists:all(fun(K) -> K#trim_unit.lex_list == [] end, RightTrimUnits) of
        false -> trim_clone(RightTrimUnits, Data);
        _ -> ok
    end.


%% @doc Trims a trim_unit using max common ancestor and its children.
common_cut(#trim_unit{lex_list = []} = TrimUnit, _Data) -> TrimUnit; 
common_cut(#trim_unit{length = Length, lex_list = LexList} = TrimUnit, #data{minlen = Minlen})
     when Length /= -1 andalso Length =< length(LexList); Minlen > length(LexList) ->
    TrimUnit#trim_unit{lex_list = []}; 
common_cut(#trim_unit{lex_list = LexList} = TrimUnit, _Data) ->
    {{_,Anc},{{_,Left},{_,Right}}} = common_ancestor(LexList),
    [FirstLeaf] = ?Query:exec(Anc, ?Syn:first_leaf()),
    [LastLeaf] = ?Query:exec(Anc, last_leaf()),

    case FirstLeaf == hd(LexList) andalso LastLeaf == lists:last(LexList) of
        true -> #trim_unit{nodes = [Anc], length = length(LexList)}; 
        _ -> common_cut0(TrimUnit, Anc,Left,Right)
    end.

%% @doc Returns a new #trim_unit after trimming.
common_cut0(TrimUnit,Anc,Left,Right) ->
    Children = get_children(Anc),
    GoodChildren = get_between(Children,Left,Right),
    {LLexList,LeftRes} = check(Left,TrimUnit#trim_unit.lex_list,left),
    {RLexList,RightRes} = check(Right,TrimUnit#trim_unit.lex_list,right),
    NewNodes = filter_nodes(Anc,LeftRes++GoodChildren++RightRes),
    #trim_unit{nodes = NewNodes, length = length(TrimUnit#trim_unit.lex_list),
                left_rem = LLexList, right_rem = RLexList}.


%% @doc Determines if a [#trim_unit] has different type nodes.
is_diff_type(ListOfTrimUnits) ->
    NoEmpty =
        [K || K <- ListOfTrimUnits, 
             K#trim_unit.nodes /= [] andalso 
             ?Graph:class(hd(K#trim_unit.nodes)) /= lex],
    case NoEmpty of
        [] -> false;
        _  -> Head = hd(NoEmpty),
              Type = ?Graph:class(hd(Head#trim_unit.nodes)),
              lists:any(fun(TrimUnit) ->
                ?Graph:class(hd(TrimUnit#trim_unit.nodes))/=Type end, NoEmpty)
    end.

%% @doc Determines if a [#trim_unit] has different length of nodes.
is_diff_length(ListOfTrimUnits) ->
    NoEmpty =
        [K || K <- ListOfTrimUnits,
            K#trim_unit.nodes /= [] andalso
            ?Graph:class(hd(K#trim_unit.nodes)) /= lex],
    case NoEmpty of 
        [] -> false;
        _  -> Head = hd(NoEmpty),
              is_diff_length(NoEmpty,length(Head#trim_unit.nodes))
    end.    
is_diff_length(ListOfTrimUnits, Length) -> 
    lists:any(fun(TrimUnit) ->
        length(TrimUnit#trim_unit.nodes) /= Length end, ListOfTrimUnits).


%% @doc Filters nodes that are uninteresing for clone finding.
filter_nodes(Anc,Nodes) ->
    case ?Graph:class(Anc) of
        clause -> filter_expr(Nodes);
        lex -> filter_lex(Nodes);
        _ -> [Node || Node <- Nodes, ?Graph:class(Node) /= lex]
    end.

filter_expr(Nodes) ->
    [Expr ||
        Expr <- Nodes, ?Graph:class(Expr) /= lex andalso
        ?Query:exec(Expr,?Query:any([[{name,back}],[{guard,back}],[{pattern,back}]])) == []].

filter_lex(Nodes) ->
    lists:filter(fun(LexNode) ->  Data = ?Graph:data(LexNode),
                    lists:member(Data#lex.type,?LEXTYPES) orelse
                    lists:member(Data#lex.data#token.type,?LEXTYPES) end, Nodes).

%% @doc Trims lexList based on direction and parent Node.
check({_,lex,_} = Node, LexList, Direction) ->
    NewNode = case ?Query:exec(Node,[orig]) of
               [] -> Node;
               [A] -> A
            end,
    check0(NewNode,LexList,Direction);
check(Node,LexList,Direction) ->
    check0(Node,LexList,Direction).
check0(Node, LexList, Direction) ->
    {Fun1,Fun2} = get_check_fun(Direction),
    [FirstLeaf] = Fun1(Node),
    case lists:member(FirstLeaf,LexList) of
        true -> {[],[Node]};
        false -> {trim_lex_list(Fun2(Node),LexList,Direction),[]}
    end.

%% @doc Returns functions for checking, based on Direction
get_check_fun(Direction) ->
    Funs = {fun(Node) -> orig(?Query:exec(Node,first_leaf())) end,
            fun(Node) -> orig(?Query:exec(Node,last_leaf())) end},
    case Direction of
        left -> Funs;
        right -> {element(2,Funs),element(1,Funs)}
    end.


%% @doc Trims a LexList based on Lex and Direction.
trim_lex_list([Lex], LexList, Direction) ->
    case Direction of
        left -> lists:takewhile(fun(K) -> Lex /= K end, LexList) ++ [Lex];
        right -> lists:dropwhile(fun(K) -> Lex /= K end, LexList)
    end.


%% @doc Returns: get_between([1,2,3,4],1,3) == [2]
get_between([], _, _) -> [];
get_between([H|T], First, Last) when Last == H ->
    get_between0(T, First, []);
get_between([H|T], First, Last) ->
    case H == First of
        true -> get_between0(T ,Last, []);
        false -> get_between(T, First, Last)
    end.
get_between0([], _, Res) -> Res;
get_between0([H|T], Last, Res) ->
    case H == Last of
        true -> lists:reverse(Res);
        false -> get_between0(T, Last, [H|Res])
    end.


%% @doc Returns the children of a Node.
get_children(Node) ->
    case ?Graph:class(Node) of
        %root -> all_children(Node); %shoud never been called here
        file -> all_children(Node); %only called when searching by pos
        form -> all_children(Node);
        clause -> all_children(Node); 
        expr -> all_children(Node);
        lex -> lex_children(Node);
        _ -> throw(get_children_error)
    end.

%% @doc Returns every children of a Node.
all_children(Node)->
    Children = ?Syn:children2(Node),
    {_, Nodes} = lists:unzip(Children),
    Nodes.

%% Specially returns a LexNode's children.
lex_children(LexNode) ->
    case ?Query:exec(LexNode,[llex]) of
        [] -> [];
        _ -> Children = ?Syn:children2(LexNode),
         {_, Nodes} = lists:unzip(Children),
         Nodes
    end.
    
%% @doc Returns the common ancestor of a LexList and its closest children.
common_ancestor(LexList) ->
    Hd = hd(LexList),
    Ld = lists:last(LexList),
    Head = case ?Query:exec(Hd,[{orig,back}]) of
               [] -> Hd;
               A -> hd(A)
            end,
    Last = case ?Query:exec(Ld,[{orig,back}]) of
               [] -> Ld;
               B -> hd(B)
            end,
    Left = get_root_path(Head, left),
    Right = get_root_path(Last, right),
    Anc = get_last_common(Left,Right,[]),
    case Anc of
        [] -> {{root,?Graph:root()},{get_real_form(lists:nth(2,Left)),
                 get_real_form(lists:nth(2,Right))}};
        {file,File} -> {{file,File},{get_real_form(lists:nth(2,Left)),
                 get_real_form(lists:nth(2,Right))}};
        _ -> {get_real_form(Anc),{get_next(Left,Anc),get_next(Right,Anc)}}
end.


%% @doc Returns a Node's root path.
get_root_path(Node,Direction) ->
    [File,Form|Path]  = ?Syn:root_path(Node,Direction),
    [File,get_real_form(Form)|Path].

get_next([H|T],Elem) ->
    Node = case H == Elem of
            true -> get_next0(T,Elem);
            _ ->    get_next(T,Elem)
        end,
    case Node of
        {_,{_,lex,_} = K} -> case ?Query:exec(K,[{orig,back}]) of
                    [] -> Node;
                    [A] ->
                        [NewNode|_] = ?Syn:parent(A),
                        NewNode
                end;
        _ -> Node
    end.
get_next0([],Elem) -> Elem;
get_next0([H|_],_Elem) -> H.


%% @doc Returns the last common element of two list.
get_last_common([],_,Result) -> Result;
get_last_common([Left|Lefts],Right,Result) ->
    case lists:member(Left,Right) of
        true -> get_last_common(Lefts,Right,Left);
        _ -> get_last_common(Lefts,Right,Result)
    end.


%% @doc Gets the original form of a form.
get_real_form({_,form,_} = Form) ->
      case ?Query:exec(Form,[{forig,back}]) of
        [] -> Form;
        [Orig|_] -> Orig
    end;
get_real_form({form,Form}) ->
    case ?Query:exec(Form,[{forig,back}]) of
        [] -> {form,Form};
        [Orig|_] -> {form,Orig}
    end;
get_real_form(Node) -> Node.


%%% ----------------------------------------------------------------------------
%%% Functions for after trimming

%% @doc Function that trims units if needed.
trim_further(Units,_) when length(Units) > 1 -> Units;
trim_further([Unit],Ets) ->
    Res = case ?Graph:class(Unit#unit.id) of
            clause -> trim_further_clause(Unit,Ets);
            expr -> trim_further_expr(Unit);
            _ -> [Unit]
        end,
    [Unit0 || Unit0<-Res, Unit0 /= []].

%% @doc Trims a case or if clause to its expressions
trim_further_clause(Unit,Ets) ->
    case ?Graph:class(Unit#unit.parent) /= form of
        true -> Exprs = ?Query:exec(Unit#unit.id, ?Clause:body()),
                Pattern = ?Query:exec(Unit#unit.id, ?Clause:patterns()),
                insert_unitlist_to_ets([make_units(Pattern)],Ets),
                make_units(Exprs);
        _ -> [Unit]
    end.

%% @doc Trims uninteresting expressions further
trim_further_expr(Unit) ->
    case ?Expr:type(Unit#unit.id) of
            list -> make_units(?Query:exec(Unit#unit.id, ?Expr:children()));
            arglist -> make_units(?Query:exec(Unit#unit.id, ?Expr:children()));
            _ -> [Unit]
    end.

%% @doc If a group contains a clause and expression the
%%      clause will be trimmed to its expressions.
trim_to_clone0(Nodes,Ets) ->
    NoLex = [Node || Node <- Nodes, ?Graph:class(hd(Node)) /= lex],
    HeadNodes =
        lists:usort(
            [Type || [{_,Type,_}|_]<- NoLex]),
    Trimmed = case HeadNodes of
        [expr] -> trim_expr(NoLex,Ets);
        [clause,expr] -> trim_clause(NoLex);
        _ -> [] 
    end,
    NoEmpty = [List || List <-Trimmed, List /= []],
    insert_to_ets0(NoEmpty, Ets).

%% @doc Trims expression groups to match each other.
trim_expr(Nodes,Ets) ->
    Minlen = lists:min([length(Node) || Node <- Nodes]),
    MinExprs = [make_units(K) || K <- Nodes, length(K) == Minlen],
    Exprs = [make_units(K) || K <- Nodes, length(K) /= Minlen],
    Trimmed = case Exprs of
                [] -> trim_to_expr(MinExprs);
                _ -> insert_to_ets_grouped(MinExprs, Ets),
                     [trim_to_minexpr(Expr,hd(MinExprs),[]) || Expr <- Exprs]
            end,
    [Trim || Trim <- Trimmed, Trim /= []].


%% @doc Calculates the minimal common expression seq.
trim_to_minexpr([],_,Res) -> Res;
trim_to_minexpr(_,[],Res) -> Res;
trim_to_minexpr([Expr|Exprs],[MinExpr|MinExprs] = MinE,Res) ->
    case MinExpr#unit.alphabet == Expr#unit.alphabet of
        true -> trim_to_minexpr(Exprs,MinExprs,Res++[Expr]);
        _ -> trim_to_minexpr(Exprs,MinE,Res)
    end.

%% @doc Special case when expression groups differ.
trim_to_expr(Exprs) when length(Exprs) > 1, length(hd(Exprs)) > 1 ->
    StringWithExprs =
        lists:map(fun(ExprList) ->
                    TotalStr = [Expr#unit.alphabet || Expr <- ExprList],
                    {TotalStr, ExprList}
                  end, Exprs),
    {TotalStrings,_} = lists:unzip(StringWithExprs),
    UniqTotalStrings = lists:usort(TotalStrings),
    case length(UniqTotalStrings) =< 2 of
        true ->
            {FirstGroup, SecondGroup} =
                lists:partition(fun({String,_}) ->
                    String == hd(UniqTotalStrings)
                end, StringWithExprs),
            {FirstStr,_} = hd(FirstGroup),
            {SecondStr,_} = hd(SecondGroup),
            {FirstFun,SecondFun} = determine_cut(FirstStr,SecondStr),
            FirstCut = [ FirstFun(Units) || {_,Units} <-FirstGroup],
            SecondCut = [ SecondFun(Units) || {_,Units} <-SecondGroup],
            FirstCut++SecondCut;
        _ -> []
    end;
trim_to_expr(_) -> [].


determine_cut(FirstStr,SecondStr) ->
    {Fun1, Fun2} =
        {fun([_|Tail]) -> Tail end,
         fun(List)     -> lists:reverse(tl(lists:reverse(List))) end},
    case Fun1(FirstStr) == Fun2(SecondStr) of
        true -> {Fun1, Fun2};
        _ -> {Fun2, Fun1}
    end.


%% @doc Trims the clauses to its expressions.
trim_clause(Nodes) ->  
    {Exprs, Clauses} = 
        lists:partition(fun([{_,Type,_}|_]) ->
            Type == expr 
        end, Nodes),
    Length = length(hd(Exprs)),
    [{_,Parent}|_] = ?Syn:parent(hd(hd(Clauses))),
    Fun = case ?Graph:class(Parent) of
                expr -> fun(List) -> hd(List) end;
                _ -> fun(List) -> lists:last(List) end
            end,
    Trimmed =
        [lists:sublist(?Query:exec(Fun(ClauseList), ?Clause:body()),1,Length) ||
             ClauseList<-Clauses],
    [make_units(Trim) || Trim <- Trimmed].


%%% ----------------------------------------------------------------------------
%%% Inserting clones to Ets functions

%% @doc Inserts clones to ets table.
insert_to_ets(Res, Ets) ->
    Exprs =
        [make_units(TrimUnit#trim_unit.nodes) ||
            TrimUnit <- Res, TrimUnit#trim_unit.nodes /= [] andalso
            ?Graph:class(hd(TrimUnit#trim_unit.nodes)) == expr],
    Others =
        [make_units(TrimUnit#trim_unit.nodes) ||
            TrimUnit <- Res, TrimUnit#trim_unit.nodes /= [] andalso
            ?Graph:class(hd(TrimUnit#trim_unit.nodes)) /= expr],
    insert_to_ets0(Exprs,Ets),
    insert_to_ets0(Others,Ets).

insert_to_ets0([], _) -> ok;
insert_to_ets0(Units, Ets) ->
    Head = hd(hd(Units)),
    case ?Graph:class(Head#unit.id) == expr of
        true -> insert_to_ets_grouped(Units,Ets);
        _ -> insert_to_ets_separate(Units, Ets)
    end.

%% @doc Case when an unitlist need to be inserted into ets.
insert_unitlist_to_ets([], _Ets) -> ok;
insert_unitlist_to_ets([H|T], Ets) when H == [] ->
    insert_unitlist_to_ets(T, Ets);
insert_unitlist_to_ets(Units, Ets) ->
    Head = hd(hd(Units)),
    case {?Graph:class(Head#unit.id), ?Graph:class(Head#unit.parent)} of
        {expr,clause} ->
            insert_to_ets_grouped(Units,Ets);
        {clause,form} -> 
            [insert_to_ets_separate1(Unit, Ets, clause) || Unit<-Units];
        _ ->[insert_to_ets_separate1(Unit, Ets, expr) || Unit<-Units]
    end.

%% @doc Clauses need to be inserted separately, plus trims.
insert_to_ets_separate(Units, Ets) ->
     lists:foreach(fun(K) ->
        lists:foreach(fun(Unit) -> 
            Trimmed = [trim_further([Unit],Ets)],
            insert_unitlist_to_ets(Trimmed, Ets)
        end, K)
    end, Units).

insert_to_ets_separate1(Units, Ets, Type) ->
    lists:foreach(fun(Unit) -> 
            ets:insert(Ets,{[Unit#unit.alphabet],Type,[Unit]}) 
    end, Units).

%% @doc Expressions need to be inserted grouped, does some trimming.
insert_to_ets_grouped(Units, Ets) ->
    TrimmedUnits = [trim_further(Unit,Ets) || Unit <- Units],
    UnitsWAlphabet = 
        [begin TotalAlphabet = [K#unit.alphabet || K <- Uni],
                {TotalAlphabet,Uni} end || Uni <- TrimmedUnits, Uni /= []],
    {InsertLikeExprs, InsertLikeOthers} =
        lists:partition(fun({_,[Unit|_]}) ->
            ?Graph:class(Unit#unit.parent) == clause end, UnitsWAlphabet),
    {Alphabets,_} = lists:unzip(InsertLikeExprs),
    insert_to_ets_separate([Unit || {_,Unit} <- InsertLikeOthers],Ets),
     case length(lists:usort(Alphabets)) of
        1 -> lists:foreach(
                fun({TotalAlphabet,Units0}) ->
                    ets:insert(Ets,{TotalAlphabet,expr,Units0})
                end, InsertLikeExprs);
        _ -> ToTrim =
             [ [ Unit#unit.id || Unit <- UnitList] ||
                     {_,UnitList} <- InsertLikeExprs],
             trim_to_clone0(ToTrim,Ets) 
    end.

%% @doc Trims a Clonegroup.
insert_to_trim(Res, Ets) ->
    CloneGroup =
        [TrimUnit#trim_unit.nodes || 
            TrimUnit <- Res, TrimUnit#trim_unit.nodes /= []],
    trim_to_clone0(CloneGroup, Ets).


%%% ----------------------------------------------------------------------------
%%% Form finding functions

%% @doc Function that converts clauses to forms.
clause_to_form(FuncsWClauses,Minlen,Minnum,Ets) ->
    Clauses = [Clause || [Clause] <- ets:match(Ets,{'_',clause,['$2']})],
    ets:match_delete(Ets,{'_',clause,['$2']}),

    Forms = get_clause(Clauses,FuncsWClauses,Minlen,[]),
    GoodForms = match_forms(Forms,Minnum,[]),
    {Forms0,FormClauses} = lists:unzip(GoodForms),
    FlatFormClauses = lists:flatten(FormClauses),

    FClauses = lists:filter(fun(K) -> not lists:member(K,FlatFormClauses) end, Clauses),
    BackClauses = get_back_clauses(FlatFormClauses,FClauses, []),
    [ets:insert(Ets,{Unit#unit.alphabet,clause,[Unit]}) ||
        Unit<-Forms0++BackClauses++FClauses].


%% @doc Returs forms converted from clauses.
get_clause([],_, _Minlen, Res) -> Res;
get_clause([H|_] = Clauses,FuncsWClauses, Minlen, Res) ->
    {Filter,Filter0} =
        lists:partition(fun(#unit{parent = P}) -> P == H#unit.parent end, Clauses),
    {value, {Form,Similars}} = lists:keysearch(H#unit.parent,1,FuncsWClauses),
    GoodLen =
        length([{Clause,LexList,Tokens} ||
            {Clause,LexList,Tokens}<-Similars, length(LexList)>=Minlen]),
    Form0 =
        case length(Filter) == GoodLen of
            true -> [{to_form(Form,Similars),Filter}];
            _ -> []
        end,
    get_clause(Filter0,FuncsWClauses,Minlen,Form0++Res).


%% @doc Returns matching forms.
match_forms([],_,Forms) -> Forms;
match_forms(MaybeForms,Minnum,Forms) ->
    {Form,_} = hd(MaybeForms),
    {Pair,Res} =
        lists:partition(fun({K,_}) ->
            Form#unit.alphabet == K#unit.alphabet
        end, MaybeForms),
    case length(Pair) >= Minnum of
        true -> match_forms(Res,Minnum,Pair++Forms);
        _ ->    match_forms(Res,Minnum,Forms)
    end.

%% @doc If a form does not have a match its clauses must go back.
get_back_clauses([],FClauses,Res) -> Res ++ FClauses;
get_back_clauses(_,[],Res) -> Res;
get_back_clauses(FormClauses, [H|T], Res) ->
    {Back,Back0} =
        lists:partition(fun(K) ->
            H#unit.alphabet == K#unit.alphabet
        end, FormClauses),
    get_back_clauses(Back0, T, Back++Res).

%% @doc Makes a #unit form of the clauses.
to_form(Form,Clauses) ->
    Tokens = get_form_tokens(Clauses,[]),
    [Parent|_] = ?Syn:parent(Form),
    #unit{id = ?Lib:get_incl_form(Form), alphabet = Tokens, parent = Parent}.

%% @doc Returns the form's tokens.
get_form_tokens([],Res) -> Res;
get_form_tokens([{_,_,Tokens}|[]],Res) -> 
    Res++Tokens++"o";
get_form_tokens([{_,_,Tokens}|T],Res) ->
    get_form_tokens(T,Res++Tokens++";").

%% @doc Drops groups that are subgroups.
drop_groups(Groups) ->
    FileLoc =
        [{Group,?Lib:extend_file_and_loc(Group,scalar)} || Group<-Groups],
    filter_that(FileLoc,FileLoc,[]).

filter_that([], _, Res) -> Res;    
filter_that([{Group,PosList}|T],A,Res) ->
    case lists:any(fun({_,PosListt}) -> is_sub_pos(PosList,PosListt) end, A) of
        true -> filter_that(T,A,Res);
        _ -> filter_that(T,A,[Group|Res])
    end.

%% @doc Determines that PosList is subclone of Other.
is_sub_pos(PosList, Other) when PosList /= Other ->
   lists:all(fun(K) -> 
        lists:any(fun(J) ->
            proplists:get_value(filepath,J) == proplists:get_value(filepath,K)
                andalso proplists:get_value(startpos,K)>= proplists:get_value(startpos,J)
                andalso proplists:get_value(endpos,K)=<proplists:get_value(endpos,J)
        end, Other)
    end, PosList);
is_sub_pos(_,_) -> false.

%% @doc Returns true if a Clauselist length is higher than Minlen.
is_short(ClausesWLeaves,Minlen) ->
    Short =
        lists:any(fun({_,LexList,_}) ->
            length(LexList)>=Minlen
        end, ClausesWLeaves),
    case Short of
        true -> false;
        _ -> Len =
                lists:sum([length(LexList) ||
                    {_,LexList,_}<-ClausesWLeaves])+length(ClausesWLeaves),
            case Len >= Minlen of
                true -> true;
                _ -> false
            end
    end.


%%% ----------------------------------------------------------------------------
%%% Filtering functions

%% @doc Filter clones by minlen and minnum.
filter_clones(Clones,Minlen,Minnum) ->
    [Clone || Clone <- Clones, 
        filter_by_length(Clone,Minlen), length(Clone) >= Minnum].

%% + hozzáadni az elválasztó cuccokat is ',' length listofunits-1
filter_by_length(ListOfUnits,Minlen) ->
    TokenStringLenght = length(lists:flatmap(fun(#unit{alphabet = Tokens}) -> Tokens end, hd(ListOfUnits))),
    Minlen =< TokenStringLenght.


%%% ----------------------------------------------------------------------------
%%% Grouping functions

%% @doc Converts the result to the unific format.
to_uni_format([],Res) -> Res;
to_uni_format([H|T],Res) ->
    Length = length(hd(H)),
    UniGroup = lists:map(fun(K) ->
                   Items = [lists:nth(K,K2) || K2 <- H],
                   #clone_item{items = Items, score = 1.0}
               end, lists:seq(1,Length)),
    to_uni_format(T,[UniGroup|Res]).
    
%% @doc Groups the clones from Ets by the tokenstring as key.
get_groups_from_ets(Ets) ->
    Key = ets:first(Ets),
    get_groups_from_ets0(Ets, Key, []).

get_groups_from_ets0(_,'$end_of_table',Res) -> Res;
get_groups_from_ets0(Ets,Key,Res) ->
    Group = ets:lookup(Ets,Key),
    Units = [element(3,K) || K<- Group],
    NextKey = ets:next(Ets,Key),
    get_groups_from_ets0(Ets,NextKey,[Units|Res]).


%%% ----------------------------------------------------------------------------
%%% Functions for subject search 

%% @doc Converts positions to subjects.
get_subjects(Options) ->
    case proplists:get_value(positions, Options, []) of
        [] -> [];
        Poses -> get_subjects0(Options, Poses)
    end.

get_subjects0(Options, Positions) ->
    Minlen = proplists:get_value(minlen, Options),
    TrimUnits = [get_subject_trim_units(Pos) || Pos<-Positions],

    Ets = ets:new(suffix_tree,[public,bag,{write_concurrency,true}]),
    Data = #data{ets = Ets, minlen = Minlen},
    trim_clone_map(TrimUnits,Data),
    clause_to_form2(1,1,Ets),
    Groups = get_groups_from_ets(Ets),
    ets:delete(Ets),

    Filtered = filter_clones(Groups,Minlen,1),
    [ [Unit#unit.id || Unit<-hd(Group)] || Group<-Filtered].

%% @doc Trims the position to nodes using suffix_tree alg.
get_subject_trim_units({FilePath,Start,End}) ->
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
    [StartLex] = get_lex(FileNode,SStart,left),
    [EndLex] =  get_lex(FileNode,SEnd,right),
    {{_,Anc},_} = common_ancestor([StartLex,EndLex]),
    LexList = ?MISC:separate_interval(?Syn:leaves2(Anc),StartLex,EndLex),
    [#trim_unit{lex_list = LexList}].

%% @doc Modified version of clause_to_form
clause_to_form2(Minlen,Minnum,Ets) ->
    Clauses = [Clause || [Clause] <- ets:match(Ets,{'_',clause,['$2']})],
    ets:match_delete(Ets,{'_',clause,['$2']}),

    Forms = get_clause2(Clauses,Minlen,[]),
    GoodForms = match_forms(Forms,Minnum,[]),
    {Forms0,FormClauses} = lists:unzip(GoodForms),

    FlatFormClauses = lists:flatten(FormClauses),
    FClauses = lists:filter(fun(K) -> not lists:member(K,FlatFormClauses) end, Clauses),
    BackClauses = get_back_clauses(FlatFormClauses,FClauses, []),
    [ets:insert(Ets,{Unit#unit.alphabet,clause,[Unit]}) || Unit<-Forms0++BackClauses++FClauses].

%% @doc Modified version of get_clause
get_clause2([], _Minlen, Res) -> Res;
get_clause2([H|_] = Clauses, Minlen, Res) ->
    {Filter,Filter0} =
        lists:partition(fun(#unit{parent = P}) -> P == H#unit.parent end, Clauses),
    Similars = ?Query:exec(H#unit.parent,?Form:clauses()),
    GoodLen =
        length([{Clause,LexList,Tokens} || {Clause,LexList,Tokens}<-Similars]),
    Form0 =  case length(Filter) == GoodLen of
        true -> [{to_form(H#unit.parent,Similars),Filter}];
        _ -> []
    end,
    get_clause2(Filter0,Minlen,Form0++Res).

merge_tokens([{_,Leaves} | []], _, Res) ->
    Tokens = process_tokens(Leaves),
    Res++add_separators(Tokens,[])++"\n";
merge_tokens([{_,Leaves} | T], Separator, Res) ->
    Tokens = process_tokens(Leaves),
    merge_tokens(T, Separator, Res++add_separators(Tokens,[])++","++Separator++",").


%%depending on the c implementation Separator should be ,
add_separators([H|[]],Res) -> Res++[H];
add_separators([H|T],Res) ->
    add_separators(T,Res++[H]++",").



%%% ----------------------------------------------------------------------------
%%% Helper functions

process_tokens(Tokens)->
    ?Alphabet:to_alphabet0(?Alphabet:default_alphabet(), Tokens, []).


make_units(Nodes) ->
    lists:map(fun(Node) ->
        [{_,Parent}|_] = ?Syn:parent(Node),
        Tokens = process_tokens(?Syn:leaves2(Node)),
        #unit{id = Node, parent = Parent, alphabet = Tokens} 
    end, Nodes).


load_nif() ->
    Nif = filename:join(code:lib_dir(referl_user, priv), "suffix_tree"),
    try 
        case erlang:load_nif(Nif, 0) of
            {error, Rnle} -> throw(Rnle);
            ok -> ok
        end
    catch
        _:_ ->
            io:format("Error during compiling C suffix_tree source.~n"),
            io:format("The suffix_tree and filtered_suffix_tree algorithm won't work.~n")
    end.


first_leaf() ->
    fun(Node) ->
        lists:flatten(leaf_down_(Node, fun erlang:hd/1))
    end.

last_leaf() ->
    fun(Node) ->
        lists:flatten(leaf_down_(Node, fun lists:last/1))
    end.

%% Implementation function for first_leaf/1 and last_leaf/1 functions.
leaf_down_(Node, DownFun) ->
    case ?Syn:children(Node) of
        [] -> [Node];
        Childs -> leaf_down_(orig(element(2,DownFun(Childs))), DownFun)
    end.


orig({_,lex,_} = Node) ->
     case ?Query:exec(Node,[orig]) of
         [] -> Node;
         [A] -> A
    end;
orig(Node) -> Node.



get_lex(File,Pos,Dir) ->
    Lex = ?Query:exec(File,?File:token(Pos)),
    case {Lex,Dir} of
        {[],left} -> get_lex(File,Pos+1,Dir);
        {[],right} -> get_lex(File,Pos-1,Dir);
        _ -> Lex
    end.

search_dupcode(_Files, _Minlen, _Minclones) ->
    "Not loaded!".

search_dupcode_by_pos(_Files, _Minlen, _Minclones) ->
    "Not loaded!".
