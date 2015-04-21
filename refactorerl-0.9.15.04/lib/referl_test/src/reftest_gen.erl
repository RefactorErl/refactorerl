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

%%% @doc This module generates refactoring descriptions.
%%%
%%% Status: generation for rename_function, rename_var, rename_mod,
%%%         move_fun, extract function, tuple_funpar
%%%         and generalize function seems to be working.
%%%         Operation is very reliable and stable.
%%%         Simple optimization and simple additional features could still be
%%%         implemented.
%%%         Unit testing would be helpful in the future.
%%%
%%% @todo CPS input instead of list return for interleaved mode
%%% @todo report which recognizers were fired
%%% @todo parametrize which refactorings to generate (and other such)
%%% @todo :wishlist support various test case pruning post-checks
%%% @todo :wishlist think about reorganizing a bit from test_difftest
%%% @todo :wishlist integrate with regression tester if possible
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>
%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>

-module(reftest_gen).
-vsn("$Rev: 9568 $").

-export([gen_refac_list/1]).
-export([all_refacs/0]).

%-vsn("$Rev: 9568 $").

-include("test.hrl").

-define(D(S,P),io:format("DEBUG: ~s ~p~n",[S,P])).
%-define(D(X,Y),_=X,_=Y,ok).

-define(MaxRetries,30).
-define(ArgsCache,args_cache).

%%% @type gr() = #gr{files = [file_node()],
%%%                  existing_symbols = symbol_set(),
%%%                  atom_server = pid()}.
%%%
%%% This record describes the input for the refactoring generator functions.
%%%
%%% Fields:
%%% <ul>
%%%     <li>`files': the list of files in which the refactoring can take
%%%         place.</li>
%%%     <li>`existing_symbols': the set of symbols that exists in the code
%%%         base.</li>
%%%     <li>`atom_server': the process that generates new atoms.</li>
%%% </ul>
-record(gr, {files, existing_symbols, atom_server}).

%%% @type ref_desc() = {refac(),refargs()}.
%%%
%%% A ref_desc() (refactoring description) describes a possible refactoring that
%%% can be performed by the tool.

%% @type refac() = rename_fun | rename_var | rename_mod |
%%                 move_fun | extract_fun | tuple_funpar |
%%                 gen.
%%
%% Atoms associated with RefactorErl transformations.

%% @type wrrefac() = rename_function | rename_variable | rename_module |
%%                   move_function | extract_function | tuple_funargs |
%%                   generalize_function.
%%
%% Atoms associated with Wrangler transformations.

%% @type refopts() = atom().
%% @type refargs() = [{refopts(),any()}].
%%
%% The parameter format to call refactorings with.

%%% @type symbols() = {{natural(), symbol_ets()}, {natural(), symbol_ets()}}.
%%%
%%% It describes a set of symbols in an indexed data structure, so it is easy to
%%% generate a random value. It stored a set of atoms and a set of variables.
%%% It should be created by {@link collect_symbols/0} and destroyed by
%%% {@link delete_symbols/1}.

%% @type symtype() = atom() | var()

%% @type dectree() = decleaf() | [dectree()] | {eor, [dectree()]}.
%%
%% Random decision tree type.
%% This is traversed to arrive at a parametrization.

%% @type decleaf() = fun((proplist(),dec_cont()) -> proplist()).
%%
%% Continuation-passing random decision tree leaf.

%% @type dec_cont() = fun((proplist())->proplist()).
%%
%% A decision tree based continuation for CPS.


%% @type expr_filter() = fun(([#expr{}]) -> [#expr{}]).
%%
%% Filters expressions.

%% @type relation() = fun((any()) -> bool()).
%%
%% Any relation.

%% @type ignore_diffs() = [{refac(),[fun((refargs())->{atom(),bool()})]}].
%%
%% These functions can flag differences that had already been isolated.

%% @type range() = {Start::integer(),End::integer()}.
%%
%% Position range.

%% @type posr_tr() = fun((range())->range()).
%%
%% Position range transformer.

%%% ----------------------------------------------------------------------------
%%% ...

%% @spec gen_refac_list(proplist()) -> [ref_desc()]
%%
%% @doc Generates `test_count' refactoring descriptions.
%% `file_list' is the list of the Erlang source files (relative to `suite').
gen_refac_list(Args) ->
    [Dir, FileList, N] = ?MISC:pgetu([suite,file_list,test_count],Args),
    catch ets:new(?ArgsCache,[named_table]),
    ?d(ets:info(?ArgsCache)),
    io:format("Adding all files..."),
    %% initialization
    reftest_utils:add_files(Dir, FileList),
    io:format("done.~n"),
    ExistingSymbols = collect_symbols(),

    %% calculating the result
    Result =
        case ?ESG:path(?ESG:root(), [file]) of
            [] ->
                {error, no_files};
            Files ->
                Gr = #gr{files = Files,
                         existing_symbols = ExistingSymbols},
                gen_refac_loop(Args, Gr, N)
        end,

    %% cleanup
    delete_symbols(ExistingSymbols),
    reftest_utils:clear_files(),

    %% returning
    case Result of
        {error, _} -> throw(Result);
        _ -> Result
    end.

%% @spec gen_refac_loop(proplist(), Gr::#gr{}, N::natural()) -> ref_desc()
%%
%% @doc Generates `N' refactoring descriptions.
%% `Gr' will be used when creating the refactoring descriptions.
gen_refac_loop(Args, Gr, N) when N >= 0 ->
    gen_refac_loop(Args, Gr, N, []).

%% @spec gen_refac_loop(proplist(), Gr::#gr{}, N::natural(),
%%     L::[ref_desc()]) -> ref_desc()
%%
%% @doc Generates and inserts `N' refactoring descriptions to the beginning of
%% `L'. `Gr' will be used when creating the refactoring descriptions.
gen_refac_loop(_Args, _Gr, 0, L) ->
    io:format("~n"),
    L;
gen_refac_loop(Args, Gr, N, L) when N > 0 ->
    try
        io:format("."),
        gen_refac_loop(Args, Gr, N-1, [gen_refac(Args, Gr)|L])
    catch
        throw:backtrack ->
            io:format(" exhausted all major branches of every refactoring,"
                      " aborted.~n"),
            L
    end.


%% @spec gen_refac(proplist(),#gr{}) -> ref_desc()
%%
%% @doc Tries to generate a refactoring description.
%%
%% @throws backtrack
gen_refac(Args,Gr=#gr{}) ->
    Enabled =
        case ?MISC:pgetu([refac],Args) of
            [all] ->
                all_refacs();
            [L] when is_list(L) ->
                L;
            [A] when is_atom(A) ->
                [A]
        end,
    Known   = reftest_diff_wr:known_difference(), %% TODO: parameter
    RefArgA = {Ref,ArgA} =
        backtracking_choice2(fun(R)-> gen_refac_(Args,Gr,Known,R) end,
                             Enabled),
    RefArg = {Ref,proplists:delete(t_argidx,ArgA)},
    {RefArg,describe(RefArgA)}.

%%% ----------------------------------------------------------------------------
%%% ...

%% @type ref_text_desc() = {string(),string()}

%% @spec ({refac(),refargs()}) -> ref_text_desc()
describe({Ref,Args})->
    Short  = "?",
    [Desc] = ?MISC:pgetu([Ref],refdesc()),
    Long   = lists:flatmap(fun(X)->describe(X,Args)end, Desc),
    R={Short,lists:flatten([Long,"."])},
%    ?d(R),
    R.

%% @type textref() = string() | refopts() | fun((refargs())->string())

%% @spec (textref(),refargs()) -> string()
describe(S,_) when is_list(S)->
    S;

describe(Key,Args) when is_atom(Key)->
    case ?MISC:pget([Key],Args) of
        [[]] ->
            ?MISC:any_to_string(Key);
        [[Value]] ->
            [$",?MISC:any_to_string(Value),$"]
    end;

describe(Fun,Args) when is_function(Fun,1)->
    Fun(Args).

%% @spec () -> [{refac(),[textref()]}]
refdesc()->
    [{rename_fun,
      ["Rename ", fun srcfun/1, " to ", name]},
     {rename_var,
      ["Rename ", fun srcvar/1, " to ", varname]},
     {rename_mod,
      ["Rename module ", fun srcmod/1, " to ", name]},
     {move_fun,
      ["Move ", fun srcfun/1, " to ", name]},
     {extract_fun,
      ["Extract ", fun range/1, " into function ", name]},
     {tuple_funpar,
      ["Tuple argument ", fun argrange/1, " of ", fun contfun/1]},
     {gen,
      ["Generalize ", fun contfun/1,
       " with new variable ", varname,
       " from ", fun range/1]}].

safe_args(Fun,Args)->
    try
        [apply(?Args,Fun,[Args])]
    catch
        {?Error, _, _} ->
            []
    end.

%% @spec (refargs()) -> string()
srcmod(Args)->
    Name =
        case ?MISC:pget([module],Args) of
            [Mod] ->
                [$ ,$",?MISC:any_to_string(Mod),$"];
            _ -> ""
        end,
    ["module", Name].

%% @spec (refargs()) -> string()
srcvar(Args)->
    Name =
        case safe_args(variable,Args) of
            [Var] ->
                [$ , $", ?Var:name(Var), $"];
            _ -> ""
        end,
    ["variable", Name].

%% @spec (refargs()) -> string()
srcfun(Args)->
    Name =
        case safe_args(function,Args) of
            [Fun] ->
                [$ , fun_text(Fun)];
            [] -> ""
        end,
    ["function", Name].

%% @spec (refargs()) -> string()
range(Args)->
    [FName,{Begin,End}] = ?MISC:pgetu([file,posrange],Args),
    {ok,Bin} = file:read_file(FName),
    Text = lists:sublist(binary_to_list(Bin),Begin,End-Begin+1),
    Type =
        case safe_args(expr_range,Args) of
            [[H]] ->
                [atom_to_list(?Expr:type(H)), $/,
                 atom_to_list(?Expr:role(H))];
            _ -> "selection"
        end,
    ["the ", Type, $ , $", Text, $"].
%    expr_text(?Args:expr_range(Args)).

%% @spec (refargs()) -> string()
contfun(Args)->
    case safe_args(expr_range,Args) of
        [[Exp|_]] ->
%  ?d(Args),
 %                   Q=graph_tree(Exp),
  %                  io:format(" ~P~n",[Q,60]),
            Funs = ?Query:exec(Exp,?Query:seq([?Expr:clause(),?Clause:form(),
                                             ?Form:func()])),
            case Funs of
                [Fun] ->
                    fun_text(Fun);
                _ -> "function"
            end;
        _ ->
            "function"
    end.

%% @@spec (#expr{}) -> string()
%expr_text([H|T])->
%    First = lists:dropwhile(fun(C)->C==$ end,
%                            lists:flatten(?Syn:tree_text(H))),
%    [$", First, [?Syn:tree_text(Expr) || Expr <- T], $"].

%% @spec (#func{}) -> string()
fun_text(Fun)->
    {Name,Arity} = {?Fun:name(Fun),?Fun:arity(Fun)},
    [$",?MISC:fun_text([Name,Arity]),$"].

%% @spec (refargs()) -> string()
argrange(Args)->
    [First,N] = ?MISC:pgetu([t_argidx,number],Args),
%    First = %?MISC:expr_range(), @todo
    case N of
        1 -> [integer_to_list(First)];
        _ -> Last = First+N-1,
             [integer_to_list(First),"..",integer_to_list(Last)]
    end.

%%% ----------------------------------------------------------------------------
%%% ...

%% @throws backtrack
%% @spec (proplist(),#gr{},ignore_diffs(),refac()) -> ref_desc()
gen_refac_(Opts,Gr=#gr{},Known,Ref) when is_list(Known),is_atom(Ref) ->
% ?D("Ref:",Ref),
    T =
        case ?MISC:pget([Ref],gen_list()) of
            [[]]  -> throw("invalid refactoring "++?MISC:any_to_string(Ref));
            [[L]] -> L
        end,
    Q     = proplists:get_value(Ref,Known,[]),
    Args0 = [{t_gr,Gr},{t_funrec,Q},{t_ref,Ref}],
    Args  = tree_descent(Args0,T,terminator(Opts)),
    {Ref,Args}.

terminator(Opts)->
    fun(Args1)->
            [RunCorrect,RunDefect] =
                ?MISC:pgetu([correct,defect],Opts),
            [Q,Ref] = ?MISC:pgetu([t_funrec,t_ref],Args1),
            Patch = [{create_new_file,false},{has_side_effect,false}],
            Args2 = ?MISC:pcopy([t_argidx|ref_atoms()],Args1), %%@todo remove
            Args  = lists:sort(Args2 ++ Patch),
            Key = {Ref,Args},
            Rec =
                case ets:lookup(?ArgsCache,Key) of
                    [] ->
                        R       = reftest_diff_wr:recognize(Q,Args),
                        ArgsRow = [{recognize,R}],
                        true    = ets:insert(?ArgsCache,{Key,ArgsRow}),
                        R;
                    [{Key,ArgsRow}] ->
%                ?D("cache hit:",{Key,ArgsRow}),
                        throw(backtrack)
                end,
            Run =
                case Rec of
                    [] ->
                        RunCorrect;
                    _ ->
%                ?D("recognized by:",Rec),
                        RunDefect
                end,
            case Run of
                true ->
                    Args++ArgsRow;
                false ->
                    throw(backtrack)
            end
    end.

ref_atoms() ->
    [file,position,posrange,module,name,varname,number].

%%% ----------------------------------------------------------------------------
%%% ...


%% @spec (proplist(),dectree(),dec_cont()) -> proplist()
tree_descent(A,_P={eor, L},Cont) when is_list(L) ->
%    ?D("XP=",{P,A}),
    backtracking_choice2(fun(X)-> tree_descent(A,X,Cont) end, L);

tree_descent(A,[],Cont)->
%    ?D("Cont=",{A}),
    Cont(A);

tree_descent(A,_L=[H|T],Cont) ->
%    ?D("L=",{L,A}),
    tree_descent(A,H,fun(A2)-> tree_descent(A2,T,Cont) end);

tree_descent(A,F,Cont) when is_function(F,2) ->
%    ?D("F=",{F,A}),
    F(A,Cont).

%%% ----------------------------------------------------------------------------
%%% ...

%% @spec () -> [{refac(),dectree()}]
gen_list() ->
    [{rename_fun,   [gen_funpos(), getname(name,atom)]},
     {rename_var,   [gen_varpos(), getname(varname,var)]},
     {rename_mod,   [gen_srcmod(), getname(name,atom)]},
     {move_fun,     [gen_funpos(), fun gen_destmod/2]},
     {extract_fun,  [gen_exprng(), getname(name,atom)]},
     {tuple_funpar, [gen_fapos()]},
     {gen,          [gen_exprng(), getname(varname,var)]}].

%% @spec () -> dectree()
gen_fapos() ->
    [fun gen_file/2,
     fun genfile_funcl/2,
     fun genfuncl_argpos/2].

%% @spec () -> dectree()
gen_exprng() ->
    [fun gen_file/2,
     fun genfile_form/2,
     {eor, [[{eor,[genform_exp(leaves()),
                   genform_exp(non_leaves()),
                   [fun genform_cl/2,
                    fun gencl_expr/2]]},
             genexp_bounds(fun(X)->X end)]%,
%            [genform_exp(non_leaves()),
%             genexp_bounds(fun random_range/1)]
]}].

%% @spec () -> dectree()
gen_funpos() ->
    [fun gen_file/2,
     fun genfile_funcl/2,
     fun genfuncl_name/2,
     fun genname_pos/2].

%% @spec () -> dectree()
gen_varpos() ->
    [fun gen_file/2,
     fun genfile_varname/2,
     fun genname_pos/2].

%% @spec () -> dectree()
gen_srcmod() ->
    [fun gen_file/2].

%% @spec (atom(),symtype()) -> dectree()
getname(A,T) ->
    {eor, [new_symbol(A,T), random_existing(A,T)]}.

%%% ----------------------------------------------------------------------------
%%% ...

%% @spec () -> [refac()]
all_refacs() ->
    [R || {R,_A} <- gen_list()].

%% @spec () -> expr_filter()
leaves() ->
    fun(L) ->
        lists:filter(fun ?Expr:is_leaf/1, L)
    end.

%% @spec () -> expr_filter()
non_leaves() ->
    fun(L) ->
        lists:filter(non(fun ?Expr:is_leaf/1), L)
    end.

%% @spec (relation()) -> relation()
non(F) when is_function(F,1) ->
    fun(X) ->
        not F(X)
    end.


%%% ----------------------------------------------------------------------------
%%% Random functions
%%% Missing input keys throw a `badmatch'.
%%% Failure of any other precondition throws either `backtrack',
%%% or the recognizer origin `{backtrack,RecogList}'.

%% @spec (proplist(),dec_cont()) -> proplist()
gen_destmod(A,Cont) ->
    [#gr{files=Files}] = ?MISC:pgetu([t_gr],A),
    Fun =
        fun(File)->
                Mod = ?Query:exec1(File, ?File:module(), backtrack),
                [{name,?Mod:name(Mod)}]
        end,
    btseq(A, Files, Fun, Cont).

%% @spec (proplist(),dec_cont()) -> proplist()
gen_file(A,Cont) ->
    [#gr{files=Files}] = ?MISC:pgetu([t_gr],A),
    Fun =
        fun(File) ->
                ModL = case ?Query:exec(File, ?File:module()) of
                           []  -> [];
                           [M] -> [{module,?Mod:name(M)}]
                       end,
                [{file,?File:path(File)}, {t_file,File} | ModL]
        end,
    btseq(A, Files, Fun, Cont).

%% @spec (atom(),symtype()) -> decleaf()
new_symbol(K,Type) ->
    fun(A,Cont) ->
            CntKey = list_to_atom("t_"++atom_to_list(Type)++"cnt"),
            Cnt = case ?MISC:pget([CntKey],A) of
                      [[]] ->
                          0;
                      [[Cnt0]] ->
                          Cnt0+1
                  end,
            A2 = proplists:delete(CntKey,A),
            New = case Type of
                      atom ->
                          list_to_atom("random_atom_"++integer_to_list(Cnt));
                      var ->
                          "A_random_atom_"++integer_to_list(Cnt)
                  end,
            Cont([{K,New},{CntKey,Cnt}|A2])
    end.

%% @doc Returns a random symbol with the given type stored in
%% `#gr.existing_symbols'.
%% The typo of the value used in the proplist is `atom' if `Type' is atom,
%% `string' otherwise.
%%
%% @spec (atom(),symtype()) -> decleaf()
random_existing(K,Type) ->
    fun(A,Cont)->
            [#gr{existing_symbols={Atoms, Vars}}] =
                ?MISC:pgetu([t_gr],A),
            {Count, Ets} =
                case Type of
                    atom -> Atoms;
                    var  -> Vars
                end,
            btseq(A,
                  ?MISC:seq2(1,Count),
                  fun(Idx)->
                          [{_Index, Item}] = ets:lookup(Ets, Idx),
                          [{K,Item}]
                  end,
                  Cont)
    end.



%% @spec (proplist(),dec_cont()) -> proplist()
%% @doc Chooses a random form `t_form' from the file `t_file'.
genfile_form(A,Cont) ->
    [File] = ?MISC:pgetu([t_file],A),
    Forms = ?Query:exec(File, ?File:forms()),
    btseq(A, Forms, t_form, Cont).

%% @spec (proplist(),dec_cont()) -> proplist()
%% @doc Chooses a random clause `t_cl' of the form `t_form'.
genform_cl(A,Cont) ->
    [Form] = ?MISC:pgetu([t_form],A),
    Clauses =
        ?Query:exec(Form,
                    ?Query:all(?Form:clauses(),
                               ?Query:seq(
                                  ?Form:deep_exprs(),
                                  ?Expr:clauses()))),
    btseq(A, Clauses, t_cl, Cont).


%% @spec (proplist(),dec_cont()) -> proplist()
%%
%% @doc Chooses a random expression `t_exp' from the clause `t_cl'.
%% Note that this does not support backtracking because of the possibly high
%% branching factor.
gencl_expr(A,Cont) ->
    [Clause] = ?MISC:pgetu([t_cl],A),
    Body  = ?Query:exec(Clause, ?Clause:body()),
    Exprs = random_subseq(Body),
    Cont([{t_exp,Exprs}|A]).

%% @spec (expr_filter()) -> dec_leaf()
%% @doc Chooses a random expression `t_exp' from the form `t_form'.
genform_exp(Filter) ->
    fun(A,Cont) ->
            [Form] = ?MISC:pgetu([t_form],A),
            Exprs = ?Query:exec(Form,
                                ?Query:seq([?Form:clauses(),
                                            ?Clause:body(),
                                            ?Expr:deep_sub()])),
            FExprs = Filter(Exprs),
            btseq(A, FExprs, t_exp, Cont)
    end.

%% @spec (posr_tr()) -> dec_leaf()
%%
%% @doc Chooses the possibly post-processed bounds of the expression
%% `t_exp' located in file `t_file'.
genexp_bounds(Post)->
    fun(A,Cont)->
            [File,Expr] = ?MISC:pgetu([t_file,t_exp],A),
            try
                New = {posrange,Post(expr_bounds(File, Expr))},
                Cont([New|A])
            catch
                error:function_clause ->
% @todo macro arguments are currently not supported
%                    io:format("~n function clause exception:~n ~p~n",
%                              [erlang:get_stacktrace()]),
%                    Q=graph_tree(Expr),
%                    io:format(" ~P~n",[Q,40]),
                    throw(backtrack)
            end
    end.

% todo If this function is not used anymore (for debugging genexp_bounds),
%      it should be removed.
%      Otherwise, move to another module (?MISC).
% graph_tree(Root)->
%     F=fun(F,L) ->
%               [begin
%                    D=referl_graph:data(N),
%                    {E,
%                     case referl_syntax:children(N)of
%                         []->D;
%                         C ->{D,F(F,C)}
%                     end}
%                end|| {E,N}<-L]
%       end,
%     F(F,[{0,Root}]).

%% @spec (proplist(),dec_cont()) -> proplist()
%% @doc Chooses the name token `t_entity' of function clause `t_funcl'.
genfuncl_name(A,Cont) ->
    [Fun] = ?MISC:pgetu([t_funcl],A),
    [Name] = ?Query:exec(Fun, ?Clause:name()),
    Cont([{t_entity,Name}|A]).

%% @spec (proplist(),dec_cont()) -> proplist()
%% @doc Chooses an occurrence `t_entity' of a variable from file `t_file'.
genfile_varname(A,Cont) ->
    [File] = ?MISC:pgetu([t_file],A),
    Vars = ?Query:exec(File,
                       ?Query:seq([?File:forms(), ?Form:clauses(),
                                   ?Clause:body(), ?Expr:variables(),
                                   ?Var:occurrences()])),
    btseq(A, Vars, t_entity, Cont).

%% @spec (proplist(),dec_cont()) -> proplist()
%%
%% @doc Chooses a `position' in name token `t_entity' from file `t_file'.
%% Note that this does not support backtracking because of the possibly high
%% branching factor.
genname_pos(A,Cont) ->
    [File,Name] = ?MISC:pgetu([t_file,t_entity],A),
    [Token] = ?Query:exec(Name,[elex]),
    New = {position, random_tokpos(File,Token)},
    Cont([New|A]).

%% @spec (proplist(),dec_cont()) -> proplist()
%%
%% @doc Chooses a function argument range from the function clause `t_funcl'
%% in `t_file'.
%% The range is interpreted as selecting `number' arguments starting from
%% `position'.
genfuncl_argpos(A,Cont)->
    [File,FCl] = ?MISC:pgetu([t_file,t_funcl],A),
    [FunDef] = ?Query:exec(FCl, ?Query:seq([?Clause:form(),
                                            ?Form:func()])),
    Arity  = ?Fun:arity(FunDef),
    MinLen = 1,
    ArgPos =
        fun(Number)->
            fun(Idx)->
                    [Arg1] = ?Query:exec(FCl, ?Clause:pattern(Idx)),
                    {Begin,_} = expr_bounds(File,Arg1),
                    [Argk] = ?Query:exec(FCl, ?Clause:pattern(Idx+Number-1)),
                    {_,End} = expr_bounds(File,Argk),
                    [{position,Begin},{posrange,{Begin,End}},
                     {t_argidx,Idx}] %% @todo remove
            end
        end,
    btseq(
      A,
      ?MISC:seq2(MinLen,Arity),
      number,
      fun(A2)->
              [Len] = ?MISC:pgetu([number],A2),
              btseq(A2,?MISC:seq2(1,Arity-Len+1),ArgPos(Len),Cont)
      end).

%% @spec (proplist(),dec_cont()) -> proplist()
%% @doc Chooses a top level function clause `t_funcl' from file `t_file'.
genfile_funcl(A,Cont) ->
    [File] = ?MISC:pgetu([t_file],A),
    Funs = ?Query:exec(File,?Query:seq([?File:module(),    ?Mod:locals(),
                                        ?Fun:definition(), ?Form:clauses()])),
    btseq(A, Funs, t_funcl, Cont).


%%% ----------------------------------------------------------------------------
%%% ...

%% @todo Args setting
%% @throws backtrack
random_tokpos(File,Token) ->
    {First,_Last} = ?Token:pos(File,Token),
    First.
%    uniform(First,Last).

%% @spec (#file{},#expr{}) -> {integer(),integer()}
expr_bounds(File,Expr) ->
    Tokens  = ?Query:exec(Expr, ?Query:seq([?Expr:deep_sub(),[elex]])),
    NBounds = ?Token:map_pos(File, Tokens),
    Bounds  = [B || {_N,B} <- NBounds],
    case Bounds of
        [] ->
            throw(backtrack); %% @todo debug
        _  ->
            {{Start, _}, {_, End}} = ?MISC:min_max(Bounds),
            {Start, End}
    end.


%% @spec ([{A,B}],[C],fun((C)->[{A,B}]),dec_cont()) -> [{A,B}]
%% @throws backtrack
btseq(A, List, F, Cont) when is_list(A),is_function(F,1)->
    Fun =
        fun(I)->
                New = F(I),
                try
                    Cont(New++A)
                catch
                    E={backtrack,Rec}->
                        Gen = proplists:get_keys(New),
                        UsedP = lists:flatmap(fun({_,V})->V end,Rec),
                        Used = lists:flatten(
                                 proplists:get_all_values(args_used,UsedP)),
                        case ?MISC:intersect(Used,Gen) of
                            [] ->
% ?D("qq A List I New E",[A,List,I,New,E]),
% ?D(" Used Gen",[Used,Gen]),
                                throw(E);
                            _ ->
% ?D("re A List I New E",[A,List,I,New,E]),
% ?D(" Used Gen",[Used,Gen]),
                                throw(backtrack)
                        end
                end
        end,
    backtracking_choice(Fun,List);

%% @spec ([{A,B}],[B],A,dec_cont()) -> [{A,B}]
%% @throws backtrack
btseq(A, List, Key, Cont) when is_list(A),is_atom(Key)->
    btseq(A,
          List,
          fun(Value)->
                  [{Key,Value}]
          end,
          Cont).

%% @throws backtrack
%% @spec (fun((A)->B),[A]) -> B
backtracking_choice(F,L) when is_function(F,1), is_list(L) ->
    I = random_element(L),
    try
        F(I)
    catch
        backtrack ->
%            ?d(E),
            backtracking_choice(F,L--[I])
    end.

%% @throws backtrack
%% @spec (fun((A)->B),[A]) -> B
backtracking_choice2(F,L) ->
    backtracking_choice(
      fun(I)->
              try
                  F(I)
              catch
                  {backtrack,_}->
                      throw(backtrack)
              end
      end,L).

%% @spec random_element(List::[Element]) -> Element
%% @doc Returns a random element of `List'.
%% @throws backtrack
random_element(List) ->
    Index = uniform(1, length(List)),
    lists:nth(Index,List).

%% @spec ([A]) -> [A]
%%
%% @doc Returns a random sublist of continuous elements with at least two
%% elements from the original list.
%%
%% @throws backtrack
random_subseq(L) ->
    K     = length(L),
    Start = uniform(1,K-1),
    Len   = uniform(2,K-Start+1),
    lists:sublist(L,Start,Len).

% todo If these functions are not used anymore (see gen_exprng),
%      remove them.
% %% @spec (integer(),integer()) -> range()
% random_range(Start,End) ->
%     random_range({Start,End}).
%
% %% @spec (range()) -> range()
% random_range({Start,End}) ->
%     {Start1,Len} = random_range_sl(Start,End),
%     {Start1,Start1+Len-1}.
%
% %% @spec (integer(),integer()) -> {Start::integer(),Len::integer()}
% random_range_sl(Start,End) ->
%     random_range_sl(Start,End,1).
%
% %% @spec (integer(),integer(),integer()) -> {Start::integer(),Len::integer()}
% random_range_sl(Start,End,Min) ->
%     Len    = uniform(Min,End-Start+1),
%     Start1 = uniform(Start,End-Len+1),
%     {Start1,Len}.


%% @spec uniform(A::integer(), B::integer()) -> integer()
%%
%% @doc Returns an integer between `A' and `B'.
%% Both `A' and `B' can be returned.
%% The distribution is uniform.
%%
%% @throws backtrack
uniform(A, B) ->
    uniform(B - A + 1) + A - 1.

%% @throws backtrack
%% @doc Returns an integer between 1 and `X'.
%% @spec (integer())->integer()
uniform(X) when X =< 0 ->
    throw(backtrack);
uniform(X) ->
    random:uniform(X).

%%% ----------------------------------------------------------------------------
%%% Generating existing symbols

%% @spec collect_symbols() -> symbols()
%%
%% @doc Collects the symbols from the database of the tool.
collect_symbols() ->

    %% First we collect the atoms and vars into the Atoms1 and Vars1 ets tables,
    %% which contain {Value} rows, so they behave as sets.
    Atoms1 = ets:new(atoms, []),
    Vars1  = ets:new(vars, []),
    true = ets:insert(Vars1,[{"_"},{"_TestGenNoName"}]),
    Files = ?ESG:path(?ESG:root(), [file]),
    [   [ case ?Graph:data(Token) of
              #lex{type=token, data=T=#token{type=atom}} ->
                  true = ets:insert(Atoms1, {?Token:get_value(T)});
              #lex{type=token, data=T=#token{type=variable}} ->
                  true = ets:insert(Vars1,  {?Token:get_value(T)});
              _ -> none
          end
        || Token <- ?Syn:leaves(File) ]
    || File <- Files ],

    %% Then we copy Atom1 and Vars1 into indexed tables Atoms and Vars, which
    %% contain {Index, Value} rows. The rows will be easy to access by
    %% their indices.
    Atoms = ets:new(atoms, []),
    Vars  = ets:new(vars, []),
    AtomsCount = add_indices(Atoms1, Atoms),
    VarsCount  = add_indices(Vars1, Vars),
    true = ets:delete(Atoms1),
    true = ets:delete(Vars1),
    {{AtomsCount, Atoms}, {VarsCount, Vars}}.

%% @spec add_indices(ets(), ets()) -> natural()
%%
%% @doc Copies and wraps the rows of `EtsSource' to `EtsTarget'.
%% If `X' was a row of `EtsSource', `{N, X}' will be a row of `EtsTarget', where
%% `N' is the index of the row. The indices vary between 1 and the number of
%% rows. This function returns the number of rows.
add_indices(EtsSource, EtsTarget) ->
    ets:foldl(
      fun({Item}, NextIndex) ->
              ets:insert(EtsTarget, {NextIndex, Item}),
              NextIndex + 1
      end, 1, EtsSource) - 1.

%% @spec delete_symbols(symbols()) -> ok
%%
%% @doc Deletes the given data structure.
%% Deletes the `ets' tables that belong to it.
delete_symbols({{_, Atoms}, {_, Vars}}) ->
    ets:delete(Atoms),
    ets:delete(Vars),
    ok.
