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

%%% @doc Standalone static analyser for dynamic call constructs.
%%%
%%% @author Dániel Horpácsi <daniel_h@inf.elte.hu>

-module(refanal_dynfun).
-vsn("$Rev: 9696 $").

-export([analyse/0, analyse/1, clean/0]).
-export([analyse_local_funexprs/0]).

-include("core.hrl").

-define(CallAnal, refcore_callanal).

-define(anal_timeout, 10000).
-define(store_timeout, 5000).

%-define(mnesia, true).

-record(match_spec, {function :: {atom(), atom()},
                     pattern  :: list(),
                     mapping  :: list(tuple() | tuple(tuple()) |tuple(tuple(), tuple()))
                    }).

-type gnode(A) :: {atom(), A, integer()}.

%%  3: exactly 3
%% -1: unknown
%% -4: at least 3
-type dyn_arity() :: integer().

-record(funref, {expr  :: gnode(expr),
                 id    :: undefined
                        | {?CallAnal:id(), ?CallAnal:id()}
                        | {local, {atom(), atom()}}
                        | list(),
                 arity :: dyn_arity() | [dyn_arity()],
                 allow_amb = true :: boolean()
                }).

-define(links, [{dynfuneref,back}, {dynfunlref,back},
                {ambfuneref,back}, {ambfunlref,back}]).
-define(NSTags, [{dynfuneref, dyneref},
                 {dynfunlref, dynlref},
                 {ambfuneref, amberef},
                 {ambfunlref, amblref}]).

-define(out(S),    ?out(S, [])).
-define(out(S, A), io:format("~n" ++ S, A)).

%% =============================================================================
%% Cleaning

%% TODO how to delete modref links?
%% TODO how to delete funs created by dynamic analysis?
clean() ->
    ?out("Cleaning dynamic references..."),
    Funs = ?Query:exec(?Query:seq([?Mod:all(), ?Mod:locals_all()])),
    Clean = [clean(Fun) || Fun <- Funs],
    ?NodeSync:clean(),
    ?FunProp:reset(),
    Funs2 = ?Query:exec(?Query:seq([?Mod:all(), ?Mod:locals_all()])),
    Result = ?out("~p references removed and ~p functions deleted.~n",
         [length(lists:flatten(Clean)), length(Funs) - length(Funs2)]),
    % semantic objects have changed
    ?FileMan:inc_sem_db_hash(),
    Result.

clean(Fun) ->
    [Mod] = ?Query:exec(Fun, ?Fun:module()),
    Formatted = io_lib:format("~p:~p/~p...",
                              [?Mod:name(Mod), ?Fun:name(Fun), ?Fun:arity(Fun)]),
    io:format("\rCleaning ~-60s", [Formatted]),
    case ?Graph:path(Fun, [{localfundef, back}]) of
        []     -> ok;
        [Expr] -> ?NodeSync:del_ref(func, {localdef, Expr}, Fun)
    end,
    %% TODO: modules of local funs still remain in the graph...
    [clean(Fun, Link) || Link <- ?links].

clean(Fun, {Tag,back}) ->
    [begin
         NSTag = element(2, lists:keyfind(Tag, 1, ?NSTags)),
         ?NodeSync:del_ref(func, {NSTag, Expr}, Fun),
         update_funprop(Expr),
         ok
     end || Expr <- ?Graph:path(Fun, [{Tag,back}])].

%% =============================================================================
%% Analysis process

read_spec_refs(Filepath) ->
    {ok, Terms} = file:consult(Filepath),
    [#match_spec{function = MF, pattern = PS, mapping = MS} ||
        {MF, PS, MS} <- Terms, legal_patterns(PS)].

legal_patterns(PS) when is_list(PS) ->
    Res = lists:all(fun legal_pattern/1, PS),
    Res orelse io:format("Illegal function arguments pattern: ~p~n", [PS]),
    Res.
legal_pattern(P) when is_atom(P) -> legal_pattern_elem(atom_to_list(P));
legal_pattern(T) when is_tuple(T) -> legal_pattern(tuple_to_list(T));
legal_pattern(L) when is_list(L) -> lists:all(fun legal_pattern/1, L);
legal_pattern(_) -> false. 
legal_pattern_elem("_") -> true;
legal_pattern_elem([$$, N]) -> N >= 48 andalso N =< 57;
legal_pattern_elem(_) -> false.

analyse(SpecRefs) ->
    try
        anal_dynrefs(inspect_dynrefs(get_specrefs(all_call_exprs(), SpecRefs)))
    catch
        no_calls ->
            ?out("No function calls found in the database.");
        no_dynamic ->
            ?out("No references match the pattern.")
    end,
    ?out("~nAnalysis completed.~n").

analyse() ->
    DynRefFile = "dynfunref.conf",
    SpecRefs =
        try read_spec_refs(DynRefFile) of
            L ->
                io:format("Function reference patterns loaded (~p, ~s)~n", [length(L), DynRefFile]),
                L
        catch
            _:_ -> []
        end,
    %% Disabled until RefactorErl has prepared for it
    %% analyse_local_funexprs(),
    try
        anal_dynrefs(inspect_dynrefs(get_dynrefs(all_call_exprs(), SpecRefs)))
    catch
        no_calls ->
            ?out("No function calls found in the database.");
        no_dynamic ->
            ?out("No dynamic references found in the database.")
    end,
    Result = ?out("~nAnalysis completed.~n"),
    % semantic objects have changed
    ?FileMan:inc_sem_db_hash(),
    Result.    

%% Note that the former version returns all the application
%% expressions, while the latter one only gives those inside modules.
-ifdef(mnesia). %................................................. ifdef(mnesia)

all_call_exprs() -> all_exprs(fun mnesia_get_applications/0).

all_exprs(Selector) ->
    {atomic, Res} = mnesia:transaction(Selector),
    lists:map(fun(N) -> {'$gn', expr, N} end, Res).

mnesia_get_applications() ->
    Match = {expr, '$1', #expr{type=application, _='_'}, '_'},
    mnesia:select(expr, [{Match, [], ['$1']}]).

-else. %................................................................... else

all_call_exprs() ->
    ?out("Collecting function calls..."),
    all_exprs(fun(Expr) -> ?Expr:type(Expr) == application end).

all_exprs(Filter) ->
    AllExprs = ?Query:exec(?Query:seq([?Mod:all(),
                                       ?Mod:locals(),
                                       ?Fun:definition(),
                                       ?Form:clauses(),
                                       ?Clause:body(),
                                       ?Expr:deep_sub()])),
    lists:filter(Filter, AllExprs).

-endif. %................................................................. endif

get_dynrefs([], _) -> throw(no_calls);
get_dynrefs(AllCalls, SpecRefs) ->
    ?out("~p function calls found in the database.", [length(AllCalls)]),
    ?out("Looking for dynamic references..."),
    [{Ref, Expr} || Expr <- AllCalls,
                    Ref <- funrefs(Expr, SpecRefs),
                    Ref /= static].

get_specrefs([], _) -> throw(no_calls);
get_specrefs(AllCalls, SpecRefs) ->
    ?out("~p function calls found in the database.", [length(AllCalls)]),
    ?out("Looking for special references..."),
    [{Ref, Expr} || Expr <- AllCalls, Ref <- specfunrefs(Expr, SpecRefs)].

numlength(0) -> 1;
numlength(I) -> integer_to_list(trunc(math:log(I)/math:log(10)) + 1).

inspect_dynrefs([]) -> throw(no_dynamic);
inspect_dynrefs(DynRefs) ->
    NumL = numlength(length(DynRefs)),
    ?out("~p dynamic function references found.", [length(DynRefs)]),
    ?out("Inspecting dynamic references...", []),
    S1 = "\rInspecting dynamic references... (~"++NumL++"w/~"++NumL++"w)",
    S2 = "~nIdentification of ~p dynamic references timed out.~n",
    Info1 = parallel(fun inspect_funref/1, DynRefs,
                     fun(X, Y) -> io:format(S1, [X, Y]) end,
                     fun(X) -> io:format(S2, [X]) end,
                     ?anal_timeout),
    Info = lists:flatten(Info1),
    ?out("~p potential callee successfully spotted.", [length(Info)]),
    Info.

anal_dynrefs(Info) ->
    StoreWrapper =
        fun(X) ->
                Res = try store_funref(X) of
                          _ -> []
                      catch 
                          throw:too_complex_funref -> [nok]
                      end,
                {Res, func_form(X#funref.expr)}
        end,
    NumL = numlength(length(Info)),
    ?out("Storing dynamic references..."),
    S1 = "\rStoring dynamic references... (~"++NumL++"w/~"++NumL++"w)",
    S2 = "~n of ~p dynamic references timed out.~n",
    Res = parallel(StoreWrapper, Info,
                   fun(X, Y) -> io:format(S1, [X, Y]) end,
                   fun(X) -> io:format(S2, [X]) end,
                   ?store_timeout),
    {NOk, Forms} = lists:unzip(Res),
    TooComplex = length(lists:append(NOk)),
    TooComplex > 0 andalso
        ?out("~w function references ignored.", [TooComplex]),
    ?out("Updating the call graph..."),
    UForms = lists:usort(lists:flatten(Forms)),
    lists:mapfoldl(
      fun(X, I) ->
              update_form_funprop(X),
              io:format("\rUpdating the call graph... (~"++NumL++"w/~"++NumL++"w)", [I, length(UForms)]),
              {ok, I+1}
      end, 1, UForms).

parallel(Fun, Xs, O1, O2, Time) ->
    Self = self(),
    Pids = [spawn(fun() -> Self ! {self(), Fun(X)} end) || X <- Xs],
    receive_results(Pids, [], length(Pids), O1, O2, Time).
receive_results([], Results, _C, _, _, _) ->
    Results;
receive_results(Pids, Results, C, O1, O2, Time) ->
    receive {Pid, Rep} ->
            exit(Pid, normal),
            O1(C-length(Pids)+1, C),
            receive_results(Pids -- [Pid], Results ++ [Rep], C, O1, O2, Time)
    after Time ->
            [exit(Pid, kill) || Pid <- Pids],
            O2(length(Pids)),
            receive_results([], Results, C, O1, O2, Time)
    end.

%% =============================================================================
%% Storing dyamic references into the database

%% TODO: eliminating the synchronisation?


%% The complexity of a function reference is M*F*A where
%%   M = number of possible module names,
%%   F = number of possible function names,
%%   A = number of possible function arities.
complexity(#funref{id = undefined}) ->
    0;
complexity(#funref{id = {undefined, undefined}}) ->
    0;
complexity(#funref{id = ID, arity = Arities})
  when is_list(Arities) ->
    complexity(#funref{id = ID, arity = 1}) * length(Arities);
complexity(#funref{id = IDs})
  when is_list(IDs) ->
    lists:sum([complexity(#funref{id = I}) || I <- IDs]);
complexity(#funref{id = {ModIDs, FunIDs}})
  when is_list(ModIDs); is_list(FunIDs) ->
    lists:sum([complexity(#funref{id = {M, F}}) ||
                  M <- lists:flatten([ModIDs]),
                  F <- lists:flatten([FunIDs])]);
complexity(_) ->
    1.

store_funref(X = #funref{}) ->
    complexity(X) > ?dynfun_complexity_limit andalso throw(too_complex_funref),
    store(X).

store(#funref{expr = Expr, id = ID, arity = Arities, allow_amb = AA}) when is_list(Arities) ->
    [store(#funref{expr = Expr, id = ID, arity = Arity, allow_amb = AA}) || Arity <- Arities];

store(#funref{expr = Expr, id = ID, arity = Arity, allow_amb = AllowAmb}) ->
    case ID of

        {local, {ModName, FunName}} ->
            ?NodeSync:add_ref(func, {dynlref, Expr}, {ModName, {FunName, Arity}});

        %% Unambiguous identifiers / possibly opaque arity

        %% GOODTOKNOW: at present simplify_id/1 ignores the ModRef
        %% part of the id in order to reduce the number of dyneref
        %% links to be created or checked; altought this way we are
        %% not able to create module reference links.

        {{ModName}, {FunName}} ->
            %%?NodeSync:add_ref(module, {ref, ModRef}, ModName),
            (Arity >= 0 orelse AllowAmb) andalso 
                ?NodeSync:add_ref(func, {dyneref, Expr}, {ModName, {FunName, Arity}});

        %% Dynamic reference with opaque module name

        {undefined, {FunName}} ->
            AllowAmb andalso 
                ?NodeSync:add_ref(func, {dyneref, Expr}, {-1, {FunName, Arity}});

        %% Dynamic reference with opaque function name

        {{ModName}, undefined} ->
            %%?NodeSync:add_ref(module, {ref, ModRef}, ModName),
            AllowAmb andalso
                ?NodeSync:add_ref(func, {dyneref, Expr}, {ModName, {-1, Arity}});

        %% Fully ambiguous references --- skipped!

        undefined              -> ok;
        {undefined, undefined} -> ok;

        %% Multiple references handled one by one (recursion!)

        {ModIDs, FunIDs} when is_list(ModIDs); is_list(FunIDs) ->
            [store(#funref{expr = Expr, id = {M, F}, arity = Arity, allow_amb = AllowAmb}) ||
                M <- lists:flatten([ModIDs]), F <- lists:flatten([FunIDs])];
        IDs when is_list(IDs) ->
            [store(#funref{expr = Expr, id = I, arity = Arity, allow_amb = AllowAmb}) || I <- IDs]
    end.

%% =============================================================================

specfunrefs(CallExpr, SpecRefs) ->
    case ?Graph:path(CallExpr, [funlref]) ++ ?Graph:path(CallExpr, [funeref]) of
        [Fun] ->
            [Mod] = ?Graph:path(Fun, ?Fun:module()),
            #module{name = M} = ?Graph:data(Mod),
            #func{name = F, arity = A} = ?Graph:data(Fun),
            [{spec_ref, Spec} ||
                Spec = #match_spec{function = {MS, FS}, pattern = PS} <- SpecRefs,
                MS == M, FS == F, length(PS) == A];
        _ -> []
    end.

funrefs(CallExpr, SpecRefs) ->
    case ?Graph:path(CallExpr, [funlref]) ++ ?Graph:path(CallExpr, [funeref]) of
        [] ->
            [FunId, _] = ?Query:exec(CallExpr, ?Expr:children()),
            case ?Graph:data(FunId) of
                %% TODO should we support deprecated tuple-syntax funs?
                #expr{type=infix_expr, value=':'} ->
                    [dynamic_mfa];
                #expr{} ->
                    [funexpr]
            end;
        [Fun] ->
            [Mod] = ?Graph:path(Fun, ?Fun:module()),
            case {?Graph:data(Mod), ?Graph:data(Fun)} of
                {#module{name=erlang}, #func{name=apply, arity=2}} ->
                    [static, apply_2];
                {#module{name=erlang}, #func{name=F, arity=3}} when F == apply; F == spawn ->
                    [static, apply_3];
                {#module{name=M}, #func{name=F, arity=A}} ->
                    [static] ++
                        [{spec_ref, Spec} || Spec = #match_spec{function = {MS, FS}, pattern = PS} <- SpecRefs,
                                             MS == M, FS == F, length(PS) == A];
                _ -> [static]
            end;
        _ -> [static]
    end.

lookup_funexpr_via_dataflow(N, ArgCnt) ->
    Ns = ?Dataflow:reach_1st([N], [back]),

    L = [N2 || N2 <- Ns, N2 /= N, ?Graph:class(N2) == expr,
               ?Expr:type(N2) == fun_expr orelse ?Expr:type(N2) == implicit_fun],

    LRef = lists:usort(lists:flatten([?Graph:path(N2, [funlref])         || N2 <- L])),
    ERef = lists:usort(lists:flatten([?Graph:path(N2, [funeref])         || N2 <- L])),
    LDef = try lists:usort(lists:flatten([?Graph:path(N2, [localfundef]) || N2 <- L]))
           catch error:{bad_path,expr,localfundef} -> [] end,

    funexpr_refs(ERef, external, ArgCnt) ++ funexpr_refs(LRef++LDef, local, ArgCnt).

funexpr_refs(Refs, Type, ArgCnt) ->
    case Refs of
        [] ->
            [undefined];
        Nodes when is_list(Nodes) ->
            [funexpr_ref(Node, Type, ArgCnt) || Node <- Nodes]
    end.

funexpr_ref(Node, Type, ArgCnt) ->
    %% ArgCnt may be a list in case of inspecting apply/2 calls
    case ArgCnt == ?Fun:arity(Node) orelse
        (is_list(ArgCnt) andalso lists:member(?Fun:arity(Node), ArgCnt)) of
        true ->
            [Mod] = ?Graph:path(Node, [{func, back}]),
            case Type of
                external ->
                    {{?Mod:name(Mod)}, {?Fun:name(Node)}};
                local ->
                    {local, {?Mod:name(Mod), ?Fun:name(Node)}}
            end;
        false ->
            %% Wrong (or unknown) number of arguments
            %% TODO: ambiguous funexpr references?
            undefined
    end.

inspect_funref({funexpr, CallExpr}) ->
    [FunId, ArgLst] = ?Query:exec(CallExpr, ?Expr:children()),
    Args = [A || {esub, A} <- ?Syn:children(ArgLst)],
    Id = lookup_funexpr_via_dataflow(FunId, length(Args)),
    [#funref{expr = CallExpr, arity = length(Args), id = Id}];

inspect_funref({dynamic_mfa, CallExpr}) ->
    [CalleeId, ArgLst] = ?Query:exec(CallExpr, ?Expr:children()),
    Args = [A || {esub, A} <- ?Syn:children(ArgLst)],
    [Mod, Fun] = ?Query:exec(CalleeId, ?Expr:children()),
    {ModId, FunId}  = ?CallAnal:lookup_IDs(Mod, Fun, undefined),
    [#funref{expr = CallExpr, arity = length(Args), id = {simplify_id(ModId), simplify_id(FunId)}}];

inspect_funref({{spec_ref, #match_spec{pattern = Pat, mapping = Map}}, CallExpr}) ->
    [_, Args] = ?Query:exec(CallExpr, ?Expr:children()),
    try
        Match = lists:flatten(match_args(Args, Pat)),
        MFAs = [specialize_mapping(Match, Mapping, CallExpr) || Mapping <- Map],
        [mfa_ref(Expr, M, F, A, false) || {Expr, {Ms, Fs, As}} <- MFAs, M <- Ms, F <- Fs, A <- As]
        %% this will likely crash if one tries to specify references
        %% to link from a non-compound expression, since Expr will be
        %% not an expression node but a list of expression
        %% nodes... todo
    catch
        error:function_clause ->
            %% zipwith f [] [a,b,c] crashes
            []
    end;

inspect_funref({apply_2, CallExpr}) ->
    [_, ApplyArgs] = ?Query:exec(CallExpr, ?Expr:children()),
    [FunId, ListOfArgs] = ?Query:exec(ApplyArgs, ?Expr:children()),
    Arity = lookup_arity(ListOfArgs),
    Id = lookup_funexpr_via_dataflow(FunId, Arity),
    [#funref{expr = CallExpr, arity = Arity, id = Id}];

inspect_funref({apply_3, CallExpr}) ->
    [_, ApplyArgs] = ?Query:exec(CallExpr, ?Expr:children()),
    [ApplyModRef, ApplyFunRef, ListOfArgs] = ?Query:exec(ApplyArgs, ?Expr:children()),
    mfa_ref(CallExpr, ApplyModRef, ApplyFunRef, ListOfArgs).

simplify_id({_Node, Name}) -> {Name};
simplify_id(undefined) -> undefined;
simplify_id(Xs) when is_list(Xs) -> lists:usort(lists:map(fun simplify_id/1, Xs)).

match_args(Exprs, Spec) when is_list(Exprs), is_list(Spec) ->
    lists:zipwith(fun match_args/2, Exprs, Spec);
match_args(_Exp, '_') ->
    [];
match_args(Expr, Tag)  when is_atom(Tag)   ->
    Ns = [N || N <- ?Dataflow:reach_1st([Expr], [back], true), ?Graph:class(N) == expr],
    {Tag, Ns};
match_args(Expr, Spec) when is_list(Spec) ->
    [{Spec, Expr} | [match_args(?Query:exec(N, ?Expr:children()), Spec)
                     || N <- ?Dataflow:reach_1st([Expr], [back], true)]];
match_args(Expr, Spec) when is_tuple(Spec) ->
    [{Spec, Expr} | [match_args(?Query:exec(N, ?Expr:children()), tuple_to_list(Spec))
                     || N <- ?Dataflow:reach_1st([Expr], [back], true)]].

specialize_mapping(Match, Mapping, CallExpr) ->
    Replace = fun(Key) -> case lists:flatten(proplists:get_all_values(Key, Match)) of [] -> [Key]; R -> R end end,
    case Mapping of
        {M, F, A} ->
            {CallExpr, {Replace(M), Replace(F), Replace(A)}};
        {{M, F, A}} ->
            {CallExpr, {Replace(M), Replace(F), Replace(A)}};
        {T, {M, F, A}} ->
            {proplists:get_value(T, Match), {Replace(M), Replace(F), Replace(A)}}
    end.

mfa_ref(Expr, ModRef, FunRef, ListOfArgs) -> mfa_ref(Expr, ModRef, FunRef, ListOfArgs, true).

mfa_ref(Expr, ModRef, FunRef, ListOfArgs, AllowAmb) ->
    ID = {lookup_ref(ModRef), lookup_ref(FunRef)},
    Arity = lookup_arity(ListOfArgs),
    [#funref{expr = Expr, arity = Arity, id = ID, allow_amb = AllowAmb}].

lookup_ref(Name) when is_atom(Name) -> {Name};
lookup_ref(Expr) -> simplify_id(?CallAnal:lookup_ID(Expr, undefined)).
lookup_arity(Arity) when is_integer(Arity) -> [Arity];
lookup_arity(Expr) ->
    case ?CallAnal:listcons_length(Expr) of
        incalculable -> -1;
        Is -> lists:usort([-1*I-1 || {I} <- Is] ++ [I || I <- Is, is_integer(I)])
    end.

%% =============================================================================
%% Copied from refanal_fun and slightly modified

update_funprop(Node) ->
    case func_form(Node) of
        []     -> ok;
        [Form] -> update_form_funprop(Form)
    end.

func_form(Node) ->
    case ?Graph:data(Node) of
        #form{type=func} -> [Node];
        #form{} -> [];
        _D ->
            case ?Syn:parent(Node) of
                [{_,P}] -> func_form(P);
                _ -> []
            end
    end.

update_form_funprop(Form) ->
    %% TODO: maybe ?NodeSync:get_node support
    case ?Graph:path(Form, [fundef]) of
        [Fun] -> ?FunProp:update(Fun, funprops(Form));
        []    -> ok
    end.

funprops(Form) ->
    Exprs = ?Query:exec(Form, ?Query:seq([?Form:clauses(), ?Clause:body(), ?Expr:deep_sub()])),
    lists:foldl(fun node_funprop/2, node_funprop(Form, {true, []}), Exprs).

node_funprop(Node, {Pure, Calls}) ->
    node_funprop(Node, ?Graph:data(Node), {Pure, Calls}).

node_funprop(Node, #expr{type=Type}, {Pure, Calls}) ->
    {Pure andalso Type /= send_expr andalso Type /= receive_expr,
     if
         Type == application -> add_calls(Node, Calls);
         true -> Calls
     end};

node_funprop(_, _, Props) -> Props.

add_calls(Node, Calls) ->
    FunCalls = [{funcall, F} || {T, F} <- ?Graph:links(Node), lists:member(T, [funeref, funlref])],
    DynCalls = [{dyncall, F} || {T, F} <- ?Graph:links(Node), lists:member(T, [dynfuneref, dynfunlref])],
    AmbCalls = [{ambcall, F} || {T, F1} <- ?Graph:links(Node), lists:member(T, [ambfuneref, ambfunlref]),
                                F <- ?Graph:path(F1, [may_be])],
    ordsets:union(Calls, lists:usort(FunCalls++DynCalls++AmbCalls)).

%% =============================================================================
%% Analysis of local fun definitions (fun (...) -> ...; (...) -> ... end)

-compile({nowarn_unused_function, [analyse_local_funexprs/0, fun_exprs_of_fun/2,
                                   fun_expr_name_arity/4, fun_exprs/1]}).
analyse_local_funexprs() ->
    ?out("Analysing local fun definitions..."),
    X = [fun_exprs_of_fun(Mod, Fun) || Mod <- ?Query:exec(?Mod:all()),
                                       Fun <- ?Query:exec(Mod, ?Mod:locals())],
    io:format("\rAnalysing local funs... ~-55s", [""]), 
    io:format("\rAnalysing local funs... (~p done).", [lists:flatlength(X)]).

fun_exprs_of_fun(Mod, Fun) ->
    {ParentName, ParentArity} = {?Fun:name(Fun), ?Fun:arity(Fun)},

    Exprs = ?Query:exec(Fun, ?Query:seq([?Fun:definition(), ?Form:clauses(), ?Clause:body()])),
    FunExprs = lists:flatten([fun_exprs(Expr) || Expr <- Exprs]),

    [begin
         {Name, Arity} = fun_expr_name_arity(Expr, ParentName, ParentArity, N),
         Formatted = io_lib:format("~p:~p/~p", [?Mod:name(Mod), Name, Arity]),
         io:format("\rAnalysing local funs... ~-55s", [Formatted]), 
         ?NodeSync:add_ref(func, {localdef, Expr}, {Mod, {Name, Arity}})
     end || {N, Expr} <- lists:zip(lists:seq(0,length(FunExprs)-1), FunExprs)].

fun_expr_name_arity(Expr, ParentName, ParentArity, N) ->
    S = io_lib:format("-~p/~p-fun-~p-", [ParentName, ParentArity, N]),
    Name = list_to_atom(lists:flatten(S)),
    [Clause | _] = ?Query:exec(Expr, [exprcl]),
    Arity = length(?Query:exec(Clause, [pattern])),
    {Name, Arity}.

fun_exprs(Node = {'$gn', Class, _}) when Class == expr; Class == clause ->
    Children = [N || {_, N} <- ?Syn:children(Node)],
    [fun_exprs(Child) || Child <- Children]
        ++ [Node || ?Graph:class(Node) == expr, ?Expr:type(Node) == fun_expr];
fun_exprs(_) -> [].
