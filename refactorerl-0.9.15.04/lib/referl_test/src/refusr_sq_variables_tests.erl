-module(refusr_sq_variables_tests).

%%-define(NODEBUG, 1).

-include_lib("eunit/include/eunit.hrl").

-include_lib("referl_gen/include/gen_export.hrl").
-include_lib("referl_core/include/core_global.hrl").
-include_lib("referl_core/include/core_export.hrl").
-include_lib("referl_lib/include/lib_export.hrl").

-compile(export_all).

anys_to_strings(Terms)->
    [lists:flatten(io_lib:format("~p", [Term])) || Term <- Terms].

substitute([], Query) ->
    Query;
substitute([{_, _, []} = Head | MoreValues], Query) ->
    {Placeholder, Identifier, _} = Head,
    Substituted = re:replace(Query, Placeholder, Identifier,
                             [global, {return, list}]),
    substitute(MoreValues, Substituted);
substitute([Head | MoreValues], Query) ->
    {Placeholder, _, Values} = Head,
    Substituted = [re:replace(Query, Placeholder, "'" ++ Value ++ "'",
                              [global, {return, list}]) || Value <- Values],
    Unioned = string:join(Substituted, " U "),
    substitute(MoreValues, Unioned).

modulenodes()->
    ?Graph:path(?Graph:root(), ?Mod:all()).

modulenames(ModuleNodes)->
    Atoms = lists:map(fun ?Mod:name/1, ModuleNodes),
    anys_to_strings(Atoms).

functionnames(ModuleNodes)->
    FunPath = ?Mod:locals(),
    FunctionNodes = lists:flatmap(
                      fun(Module)-> ?Graph:path(Module, FunPath) end,
                      ModuleNodes),
    Atoms = lists:map(fun ?Fun:name/1, FunctionNodes),
    anys_to_strings(ordsets:from_list(Atoms)).

functions_to_chains([{list, []}])->
    [];
functions_to_chains(RawOutput)->
    RawFunctions = lists:flatmap(fun({_, _, _, Functions})-> Functions end,
                                 RawOutput),
    [{chain, [Function],"\n"} || Function <- RawFunctions].

run(OutputParam, Query) ->
    %% if length(Query) < 200 ->
    %%         ?debugMsg(Query);
    %%    true -> ok
    %% end,
    %% ?debugMsg(Query),
    %% ?d({OutputParam, Query}),
    lists:sort(refusr_sq:run(OutputParam, [], Query)).

run_property_query(OutputParam, Query)->
    %%?debugMsg(Query),
    grouped_result_to_list(OutputParam, run(OutputParam, Query)).

run_chain_query(OutputParam, Query, ExtraQuery) ->
    ExtraResults = run(OutputParam, ExtraQuery),
    ExtraChains = functions_to_chains(ExtraResults),
    QueryResults = run(OutputParam, Query),
    NewChains = ExtraChains -- QueryResults,
    lists:sort(NewChains ++ QueryResults).

grouped_result_to_list(_, [{list, []}] = Empty) ->
    Empty;
grouped_result_to_list(OutputParam, RawResults) ->
    case proplists:is_defined(groupby, OutputParam) of 
        true ->
            ToList = fun (Result) when is_list(Result)->
                             Result;
                         (Result) ->
                             [Result]
                     end,
            lists:map(fun({A,B,C,D,Result})->
                              {A,B,C,D, ToList(Result)} end,
                      RawResults);
        false ->
            RawResults
    end.

variables_test_()->
    ModuleNodes = modulenodes(),
    if (ModuleNodes == []) ->
            ?debugMsg("Warning: your database is empty. "
                      "All test will pass trivially.");
       true ->
            ok
    end,
    ModuleNames = modulenames(ModuleNodes),
    FunctionNames = functionnames(ModuleNodes),
    [set_timeout(variables__tests(ModuleNames, FunctionNames, [])),
     set_timeout(variables__tests(ModuleNames, FunctionNames, [{groupby, 1}]))
    ].

set_timeout(Tests)->
    Timeout = 600,  % 10 minutes
    [{timeout, Timeout, Test} || Test <- Tests].

variables__tests(ModuleNames, FunctionNames, GroupByParam)->
    OutputParam = [{output, other} | GroupByParam],
    Run = fun(Query) -> run(OutputParam, Query) end,
    RunProperty = fun(Query) -> run_property_query(OutputParam, Query) end,
    RunChain = fun(Query, ExtraQuery) ->
                       run_chain_query(OutputParam, Query, ExtraQuery) end,
    Empty = [{list, []}],
    EmptyProperty = [],
    [
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     %% simple filters
     ?_assertEqual(
        Run("mods[A=name].funs[name==A]"),
        Run(substitute([{"&0", "A", ModuleNames}],
                       "mods[name==&0].funs[name==&0]"))),

     ?_assertEqual(
        RunProperty("mods[A=name].funs[name==A].name"),
        RunProperty(substitute([{"&0", "A", ModuleNames}],
                               "mods[name==&0].funs[name==&0].name"))),

     ?_assertEqual(
        Run("mods[A=name].funs[B=name]"),
        Run("mods.funs")),

     ?_assertEqual(
        RunProperty("mods[A=name].funs[B=name].name"),
        RunProperty("mods.funs.name")),

     ?_assertEqual(
        Run("mods.funs[name=A]"),
        Run("mods.funs")),

     ?_assertEqual(
        RunProperty("mods.funs[A=name].name"),
        RunProperty("mods.funs.name")),

     ?_assertEqual(
        Run("mods[A=name].funs[name==A, name/=A]"),
        Run(substitute([{"&0", "A", ModuleNames}],
                                    "mods[name==&0].funs[name==&0, name/=&0]"))),

     ?_assertEqual(
        Run("mods[A=name].funs[B=name, A==B]"),
        Run(substitute([{"&0", "A", ModuleNames}],
                       "mods[name==&0].funs[name==&0]"))),

     ?_assertEqual(
        Run("mods[A=name].funs[B=name, A/=B]"),
        Run(substitute([{"&0", "A", ModuleNames}],
                       "mods[name==&0].funs[name/=&0]"))),

     ?_assertEqual(
        Run("mods[A=name].funs[B=name, B==f]"),
        Run("mods.funs[name==f]")),

     ?_assertEqual(
        Run("mods.funs[A=name, A==f].name"),
        Run("mods.funs[name==f].name")),

     ?_assertEqual(
        RunProperty("mods[A=name].funs[B=name, A==B].name"),
        RunProperty(substitute([{"&0", "A", ModuleNames}],
                               "mods[name==&0].funs[name==&0].name"))),

     ?_assertEqual(
        RunProperty("mods[A=name].funs[name==A, name/=A].arity"),
        RunProperty(substitute([{"&0", "A", ModuleNames}],
                               "mods[name==&0].funs[name==&0, name/=&0].arity"))),


     ?_assertEqual(
        RunProperty("mods[A=name].funs[name==A, name/=A].name"),
        RunProperty(substitute([{"&0", "A", ModuleNames}],
                               "mods[name==&0].funs[name==&0, name/=&0].name"))),

     ?_assertEqual(
        Run("mods.funs[A=arity, A<2]"),
        Run("mods.funs[arity<2]")),

     ?_assertEqual(
        Run("mods.funs[(A=name,B=name,false);A==B]"),
        Empty),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     %% closures

     ?_assertEqual(
        RunProperty("mods[A=name].funs[name==A].(calls[B=name])+.name"),
        RunProperty(substitute(
                      [{"&0", "A", ModuleNames}],
                      lists:append(
                        ["mods[name==&0].funs[name==&0](",
                         substitute([{"&1", "B", FunctionNames}],
                                    ".(calls[name==&1])+"),
                         ").name"])))),

     ?_assertEqual(
        RunChain("mods[A=name].funs[name==A].(calls[B=name])+",
                 "mods[A=name].funs[name==A]"),
        Run("mods[A=name].funs[name==A].(calls[false])+ U " ++
                substitute([{"&0", "A", ModuleNames},
                            {"&1", "B", FunctionNames}],
                           "mods[name==&0].funs[name==&0].(calls[name==&1])+"))),

     ?_assertEqual(
        RunChain("mods[A=name].funs[B=name].(calls[C=name])+",
                 "mods.funs"),
        Run("mods.funs.(calls[false])+ U " ++  
                substitute([{"&0", "C", FunctionNames}],
                           "mods.funs.(calls[name==&0])+"))),

     ?_assertEqual(
        RunProperty("mods[A=name].funs[B=name].(calls[C=name])+.name"),
        RunProperty(lists:append(["mods.funs(",
                                  substitute([{"&0", "C", FunctionNames}],
                                             ".(calls[name==&0])+"),
                                  ").name"]))),

     ?_assertEqual(
        RunProperty("mods.funs.(calls[A=name])+.name"),
        RunProperty(lists:append(["mods.funs(",
                                  substitute([{"&0", "A", FunctionNames}],
                                             ".(calls[name==&0])+"),
                                  ").name"]))),

     ?_assertEqual(
        RunChain("mods.funs.(calls[A=name])+",
                 "mods.funs"),
        Run("mods.funs.(calls[false])+ U " ++
                substitute([{"&0", "A", FunctionNames}],
                           "mods.funs.(calls[name==&0])+"))),

     ?_assertEqual(
        RunChain("mods.funs[A=name].(calls[B=name])+",
                 "mods.funs"),
        Run("mods.funs.(calls[false])+ U " ++  
                substitute([{"&0", "B", FunctionNames}],
                           "mods.funs.(calls[name==&0])+"))),

     ?_assertEqual(
        RunProperty("mods.funs[A=name].(calls[name==A])+.name"),
        RunProperty(substitute([{"&0", "A", FunctionNames}],
                               "mods.funs[name==&0].(calls[name==&0])+.name"))),

     ?_assertEqual(
        Run("mods.funs[A=name].(calls[name==A])+"),
        Run(substitute([{"&0", "A", FunctionNames}],
                       "mods.funs[name==&0].(calls[name==&0])+"))),

     ?_assertEqual(
        RunChain("mods.funs[A=name].(calls[A==f])+",
                 "mods.funs"),

        RunChain("mods.funs[name==f].(calls)+",
                 "mods.funs")),

     ?_assertEqual(
        RunChain("mods.funs[A=arity].(calls[A==1])+",
                 "mods.funs"),

        RunChain("mods.funs[arity==1].(calls)+",
                 "mods.funs")),

     ?_assertEqual(
        Run("mods.funs[A=name].(calls[B=name, A==B])+"),
        Run(substitute([{"&0", "A", FunctionNames}],
                       "mods.funs[name==&0].(calls[name==&0])+"))),

     ?_assertEqual(
        RunProperty("mods.funs[A=name].(calls[B=name, A==B])+.name"),
        RunProperty(substitute([{"&0", "A", FunctionNames}],
                               "mods.funs[name==&0].(calls[name==&0])+.name"))),

     ?_assertEqual(
        RunProperty("mods.funs[A=arity, A==1].(calls[A==1])+.name"),
        RunProperty("mods.funs[arity==1].(calls)+.name")),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     %% iterations

     ?_assertEqual(
        RunProperty("mods[A=name].funs[B=name].{calls[C=name]}1.name"),
        RunProperty(substitute([{"&0", "C", FunctionNames}],
                               "mods.funs.{calls[name==&0]}1.name"))),

     ?_assertEqual(
        RunProperty("mods.funs.{calls[A=name]}1.name"),
        RunProperty(substitute([{"&0", "A", FunctionNames}],
                               "mods.funs.{calls[name==&0]}1.name"))),

     ?_assertEqual(
        Run("mods.funs.{calls[A=name]}1"),
        Run(substitute([{"&0", "A", FunctionNames}],
                       "mods.funs.{calls[name==&0]}1"))),

     ?_assertEqual(
        Run("mods.funs[A=name].{calls[name==A]}1"),
        Run(substitute([{"&0", "A", FunctionNames}],
                       "mods.funs[name==&0].{calls[name==&0]}1"))),

     ?_assertEqual(
        Run("mods.funs[A=name].{calls[B=name]}1"),
        Run("mods.funs.{calls}1")),

     ?_assertEqual(
        RunProperty("mods.funs[A=name].{calls[B=name]}1.name"),
        RunProperty(substitute([{"&0", "B", FunctionNames}],
                               "mods.funs.{calls[name==&0]}1.name"))),

     ?_assertEqual(
        RunProperty("mods.funs[A=name].{calls[B=name]}1.arity"),
        RunProperty(substitute([{"&0", "B", FunctionNames}],
                               "mods.funs.{calls[name==&0]}1.arity"))),

     ?_assertEqual(
        Run("mods.funs[A=arity].{calls[A==2]}1"),
        Run("mods.funs[arity==2].{calls}1")),

     ?_assertEqual(
        Run("mods.funs[A=name].{calls[B=name, A==B]}1"),
        Run(substitute([{"&0", "A", FunctionNames}],
                       "mods.funs[name==&0].{calls[name==&0]}1"))),

     ?_assertEqual(
        RunProperty("mods.funs[A=name].{calls[name==A]}1.name"),
        RunProperty(substitute([{"&0", "A", FunctionNames}],
                               "mods.funs[name==&0].{calls[name==&0]}1.name"))),


     ?_assertEqual(
        RunProperty("mods.funs[A=arity].{calls[A==2]}1.name"),
        RunProperty("mods.funs[arity==2].{calls}1.name")),

     ?_assertEqual(
        RunProperty("mods.funs[A=name].{calls[B=name, A==B]}1.arity"),
        RunProperty(substitute([{"&0", "A", FunctionNames}],
                               "mods.funs[name==&0].{calls[name==&0]}1.arity"))),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     %% embedded queries
     ?_assertEqual(
        RunProperty("mods.funs[A=arity, .calls[B=arity], A<B, A==B].name"),
        EmptyProperty),

     ?_assertEqual(
        Run("mods.funs[A=arity, .calls[B=arity], A<B, A==B]"),
        Empty),

     ?_assertEqual(
        Run("mods.funs[A=name, .calls[B=name], A==B]"),
        Run(substitute([{"&0", "A", FunctionNames}],
                       "mods.funs[name==&0, .calls[name==&0]]"))),

     ?_assertEqual(
        RunProperty("mods[A=name].funs[.calls[name==A]].name"),
        RunProperty(substitute([{"&0", "A", FunctionNames}],
                               "mods[name==&0].funs[.calls[name==&0]].name"))),

     ?_assertEqual(
        Run("mods[A=name].funs[.calls[name==A]]"),
        Run(substitute([{"&0", "A", FunctionNames}],
                       "mods[name==&0].funs[.calls[name==&0]]"))),

     ?_assertEqual(%x
        Run("mods.funs[.calls[A=name].calls[B=name]].calls[A==B, name==A, .calls[name==B]]"),
        Run(substitute([{"&0", "A", FunctionNames}],
                       "mods.funs.calls[name==&0, .calls[name==&0]]"))),

     ?_assertEqual(%x
        Run("mods[M=name].funs[.calls[A=name].calls[B=name]].calls[A==B, name==A, .calls[name==B]]"),
        Run(substitute([{"&0", "A", FunctionNames}],
                       "mods.funs.calls[name==&0, .calls[name==&0]]"))),

     ?_assertEqual(
        Run("mods.funs[(.calls[A=arity][false], A<2); A=arity]"),
        Run("mods.funs")),

     ?_assertEqual(
        RunProperty("mods.funs[(.calls[A=arity][false], A<2); A=arity].name"),
        RunProperty("mods.funs.name")),

     ?_assertEqual(
        RunProperty("mods[M=name].funs[.calls[A=name][false]; A==M].name"),
        EmptyProperty),

     ?_assertEqual(
        Run("mods.funs[.calls[A=name][false];true].calls[name==A]"),
        Empty),

     ?_assertEqual(
        Run("mods.funs[.calls[A=name,false];true].calls[name==A]"),
        Empty),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     %% set operations
     ?_assertEqual(
        Run("mods.funs[A=name] I mods.funs[name/=A]"),
        Empty),

     ?_assertEqual(
        RunProperty("mods[M1=name].funs[A=name].name I mods[M2=name].funs[name/=A].name"),
        EmptyProperty),

     ?_assertEqual(
        Run("mods(.funs[A=name] I .funs[name/=A])"),
        Empty),

     ?_assertEqual(
        RunProperty("mods[M=name](.funs[A=name] I .funs[name/=A]).name"),
        EmptyProperty),

     ?_assertEqual(
        Run("mods.funs[A=name] U mods.funs[B=name] U mods.funs[C=name]"),
        Run("mods.funs")),

     ?_assertEqual(
        RunProperty("mods(.funs[A=name] I .funs[B=name] I .funs[C=name]).name"),
        RunProperty("mods.funs.name")),

     ?_assertEqual(
        RunProperty("mods[M=name](.funs[A=name] I .funs[B=name] I .funs[C=name]).name"),
        RunProperty("mods.funs.name")),

     ?_assertEqual(
        Run("mods(.funs[A=name] U mods.funs[name==A] I mods.funs[name/=A])"),
        Run("mods.funs")),

     ?_assertEqual(
        Run("(mods[M=name] U (mods[name==M] I mods[name/=M])).funs.calls[mod/=M]"),
        Run(substitute([{"&0", "M", ModuleNames}],
                       "mods[name==&0].funs.calls[mod/=&0]"))),

     ?_assertEqual(
        RunProperty("(mods[M=name].funs I mods[name==M].funs[A=name].calls).file[name/=M].name"),
        EmptyProperty),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     %% all_in, any_in
     
     ?_assertEqual(
        Run("mods[M=name].funs[name in mods[name==M].name]"),
        Run(substitute([{"&0", "M", ModuleNames}],
                       "mods[name==&0].funs[name in mods[name==&0].name]"))),

     ?_assertEqual(
        Run("mods[.funs[A=name] all_in mods[name==A].funs[name==A]].funs[mod==A, name==A]"),
        Run(substitute([{"&0", "A", ModuleNames}],
                       "mods[name==&0].funs[name==&0]"))),

     ?_assertEqual(
        Run("mods[mods[M=name].funs[A=name] any_in .funs[mod==M, name==A]].funs[name==M]"),
        Run(substitute([{"&0", "M", ModuleNames}],
                       "mods[name==&0].funs[name==&0]"))),

     ?_assertEqual(
        Run("mods[M=name, .funs any_in mods[name/=M].funs].funs"),
        Empty),

     ?_assertEqual(
        Run("mods[.funs[M=mod] all_in mods[name/=M].funs[A=name]].funs"),
        Empty),

     ?_assertEqual(
        Run("mods.funs[.calls[M=mod].name any_in mods[name/=M].funs.name].calls[mod/=M]"),
        Run(substitute([{"&0", "M", ModuleNames}],
                       "mods.funs[.calls[mod==&0].name any_in mods[name/=&0].funs.name].calls[mod/=&0]"))),

     ?_assertEqual(
        Run("mods.loc:avg"),
        Run("mods->M.loc:avg")),

     ?_assertEqual(
        Run("mods.funs.loc:avg"),
        Run("mods.funs[loc=A].A:avg"))        
     
    ].
