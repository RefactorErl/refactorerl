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

%%% @doc Candidate production of Clone IdentifiErl

%%% @author Viktoria Fordos <f-viktoria@elte.hu>

-module(refusr_clone_identifierl_candidates).
-vsn("$Rev: 9316 $").
-include("user.hrl").
-include("refusr_clone_identifierl.hrl").


% interface
-export([convert_graph/0, convert_graph/1, default_options/0, default_alphabet/0, to_alphabet0/3]).

% internal
-export([sentence_worker/2]).

%-export([convert_unit/1]).

-record(proc_desc, {type, key, default}).

-define(proc_sequence,
        [#proc_desc{type = file, key = file_filter, default=fun default_file_filter/0},
         #proc_desc{type = form, key = form_filter, default=fun default_form_filter/0},
         #proc_desc{type = clause, key = clause_filter, default=fun default_clause_filter/0},
         #proc_desc{type = expr, key = expr_filter, default=fun default_expr_filter/0},
         #proc_desc{type = token, key = token_filter, default=fun default_token_filter/0}]).

-define(ext_symbol(S), begin ?d(refcore_graph:data(S)), "0" end).
%%% ============================================================================
%%% Interface
convert_graph()->
    convert_graph(default_options()).

default_options()->
    [{unit, expr},
     {type, to_alphabet},
     {tokenizer, fun tokenizer/1},
     {alphabet, fun default_alphabet/0},
     {subject, []}].

convert_graph(Options)->
     Expanded = ?Lib:validate_and_expand_options(?MODULE, Options),
     case proplists:get_value(type, Expanded) of
         to_alphabet ->
             make_sentences(produce_units(Expanded), Expanded);
         to_unit ->
             produce_units(Expanded);
         to_mod_and_fun_preserving_alphabet ->
             convert_unit(Expanded);
         to_fun_preserving_alphabet ->
             convert_unit(Expanded)
     end.

% @todo generalise this function
convert_unit(Options)->
    form = proplists:get_value(unit, Options),
    Func = proplists:get_value(subject, Options),
    lists:flatten([convert_unit0(Expr, Options)
                    || Expr<- ?Query:exec(Func, ?Query:seq([?Fun:definition(),
                                                            default_clause_filter(),
                                                            default_expr_filter()]))]).

convert_unit0(Ent, Options)->
    case ?Syn:class(Ent) of
        expr -> convert_expr_unit(Ent, Options);
        lex -> convert_lex_unit(Ent);
        _ -> [convert_unit0(Child, Options) || {_,Child} <- ?Syn:children(Ent)]
    end.

convert_expr_unit(Expr, Options)->
    case ?Expr:type(Expr) of
        application -> convert_app_expr(Expr, Options);
        _ -> [convert_unit0(Child, Options) || {_,Child} <- ?Syn:children(Expr)]
    end.

convert_lex_unit(Lex)->
    to_alphabet([Lex], [{alphabet, fun default_alphabet/0}]).

convert_app_expr(Expr, Options)->
    {ModQ, FunQ} = case ?Query:exec(Expr, ?Expr:dynfunction()) of
        [Fun] ->
             {_,{Mod, Func,_}} = reflib_function:mod_fun_arity(Fun),
             {Mod, Func};
        _ -> {undefmod, undeffun}
    end,
    [funapp_text({ModQ,FunQ}, proplists:get_value(type, Options)),
     [convert_unit0(Child, Options) || Child <- ?Query:exec(Expr, ?Expr:child(2))]].

funapp_text({ModQ, FunQ}, to_mod_and_fun_preserving_alphabet)->
    [atom_to_list(ModQ),":",atom_to_list(FunQ)];
funapp_text({_, FunQ}, to_fun_preserving_alphabet)->
    [atom_to_list(FunQ)].

%%% ============================================================================
%%% Controller functions

produce_units(Options)->
    Target = proplists:get_value(unit, Options),
    produce_units(none, ?proc_sequence, Target, Options).

produce_units(StartingNodes, [ProcDesc=#proc_desc{type=Type}|_], Target, Options)
  when Type == Target ->
    to_smaller_units_with_parent(StartingNodes, ProcDesc, Options);
produce_units(StartingNodes, [ProcDesc|ProcSeq], Target, Options) ->
    produce_units(to_smaller_units(StartingNodes, ProcDesc, Options),
                  ProcSeq,
                  Target,
                  Options).

to_smaller_units(StartingNodes, #proc_desc{key = Key, default=DefaultFilter}, Options)->
    Filter = proplists:get_value(Key, Options, DefaultFilter),
    case StartingNodes of
        none -> ?Query:exec(Filter());
        _ -> ?Query:exec(StartingNodes, Filter())
    end.

to_smaller_units_with_parent(StartingNodes,
                             #proc_desc{key = Key, default=DefaultFilter}, Options) ->
    Filter = proplists:get_value(Key, Options, DefaultFilter),
    Parents = case StartingNodes of
                  none -> ?Query:exec([]); %?Query:exec(Filter());
                  _ -> StartingNodes
              end,
    [#unit{id=Unit, parent=StartingNode}
        || StartingNode<- Parents, Unit <- ?Query:exec(StartingNode, Filter())].

make_sentences(Units, Options)->
    MakeProperArgs = fun(Arg)-> [Arg, Options] end,
    Result = ?MISC:parallelise(Units, ?MODULE, sentence_worker, MakeProperArgs),
    {_, Sentences} = lists:unzip(Result),
    Sentences.

to_alphabet(Tokens, Options) ->
    AlphabetFun = proplists:get_value(alphabet, Options),
    to_alphabet0(AlphabetFun(), Tokens, []).

to_alphabet0(_, [], Res)->
    lists:flatten(lists:reverse(Res));
to_alphabet0(Alphabet, [Token|Tokens], Res)->
    Symbol = case lists:keyfind(token_type(Token), 1, Alphabet) of
        false->
            ?ext_symbol(Token);
        {_, Valid} ->
            Valid
    end,
    to_alphabet0(Alphabet, Tokens, [Symbol|Res]).

%%% ============================================================================
%%% Defaults
% the ',' character should not be assigned here because of the C suffix_tree alg
default_alphabet()->
    % The same symbol is used for substituting atoms, variables, and numbers.
     [{atom, "A"},{'and', "L"},{char, "C"},{'orelse', "L"},{export, "E"},
     {'andalso', "L"},{import, "G"},{integer, "A"},{module, "M"},{'not', "N"},
     {'or', "L"},{'div', "P"}, {'rem', "P"}, {record, "R"},{string, "S"},{spec, "T"},
     {variable, "A"},{'_', "A"}, {float, "A"}, {'type', "Y"}, {file, "F"},
     {'after', "a"},{'begin', "b"},{'case', "c"},{'catch', "d"},{'end', "e"},
     {'==', "K"},{'>=', "K"},{'=<', "K"},{'<=', "K"},{'if', "i"}, {'xor', "V"},
     {'||', "l"},{'=/=', "K"},{'=:=', "K"},{stop, "o"},{'of', "p"},{'/=', "K"},
     {'receive', "r"},{'>>', "s"},{'try', "t"},{'<<', "u"},{'<-', "v"},
     {'when', "w"},{'fun', "x"},{'::',"y"},{'->', "z"}, {'opaque', "q"},
     {'(', "("},{')', ")"},{'{', "{"},{'}', "}"},{'[', "["},{']', "]"},
     {'.', "."},{',', "m"},{':', ":"},{';', ";"},{'!', "!"},{'?', "?"},
     {'#', "#"},{'|', "|"},{'++', "j"},{'--', "k"},
     {'+', "O"},{'-', "O"},{'*', "O"},{'/', "O"},{'<', "K"},{'>', "K"},{'=', "K"},
     {'band', "1"}, {'bor', "2"}, {'bxor', "3"}, {'bnot', "4"}, {'bsl', "5"},
     {'bsr', "6"}].

default_file_filter()->
    % every module
    ?Mod:all().

default_form_filter()->
    % every function definition
    ?Query:seq(?Mod:locals(), ?Fun:definition()).

default_clause_filter()->
    % every clause of a function
    ?Form:clauses().

default_expr_filter()->
    % every top level expression of a clause
    % fun. name, params, guards are ignored
    ?Clause:body().

default_token_filter()->
    % every token
    ?Expr:tokens().

tokenizer(Node)->
    ?Syn:leaves(Node).
%%% ============================================================================
%%% Internal functions

sentence_worker(Units, Options)->
    Tokenizer = proplists:get_value(tokenizer, Options),
    [Unit#unit{alphabet = to_alphabet(Tokens, Options) }
        || Unit <- Units, Tokens <- [Tokenizer(Unit#unit.id)]].

token_type(Token)->
    try
        ?Token:type(Token)
    catch
        _:_ -> ext_item
    end.
