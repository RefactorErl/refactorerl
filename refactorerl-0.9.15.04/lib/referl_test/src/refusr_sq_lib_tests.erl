-module(refusr_sq_lib_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("referl_gen/include/gen_export.hrl").
-include_lib("referl_core/include/core_global.hrl").
-include_lib("referl_lib/include/lib_export.hrl").

-include_lib("referl_core/include/core_export.hrl").

-export([run_nodes/1,group_setup/0,variables_setup/0,sort/1,conv_setup/0,setop_setup/0]).

-define(Lib, refusr_sq_lib).

empty() ->
    [{list, []}].

empty_property() ->
    [].

sort([{_, _, list, _} | _] = Results) ->
    SortedGroups = lists:sort(Results),
    [{A, B, C, lists:sort(Group)} || {A, B, C, Group} <- SortedGroups];
sort(Results) ->
    lists:sort(Results).

variables_testfile1() ->
    {module, ?MISC:canonical_filename("variable_m1.erl"),
     "-module(variable_m1).\n"
     "-export([f/0]).\n"
     "variable_m1()-> ok.\n"
     "variable_m1(X)-> variable_m1(X).\n"
     "f()-> variable_m2:h(5).\n"
     "g()-> g(5),
            variable_m2:g().\n"
     "g(X)-> variable_m2:g().\n"
     "m()-> f().\n"
     "variable_m2() -> ok.\n"}.

variables_testfile2() ->
    {module, ?MISC:canonical_filename("variable_m2.erl"),
     "-module(variable_m2).\n"
     "-export([h/1]).\n"
     "h()-> ok.\n"
     "h(A)-> variable_m1:f(), A + 2.\n"
     "variable_m2()-> variable_m2().\n"
     "g() -> h().\n"
     "g(A) -> g().\n"}.

variables_setup() ->
    [add_file(variables_testfile1()), add_file(variables_testfile2())].

cleanup(Files) ->
    lists:foreach(fun ?FileMan:drop_file/1, Files).

variables_test_() ->
    {setup,
     fun variables_setup/0,
     fun cleanup/1,
     variables_tests()}.

raw(Query) ->
    raw(Query, []).

raw(Query, Options) ->
    sort(refusr_sq:run([{output, other} | Options], [], Query)).

variables_tests() ->
    FunsWithNameOfMod = [{group_by,
                          {nopos,"variable_m1.erl"},
                          list,
                          [{nopos,"variable_m1:variable_m1/0"},
                           {nopos,"variable_m1:variable_m1/1"}]},
                         {group_by,
                          {nopos,"variable_m2.erl"},
                          list,
                          [{nopos,"variable_m2:variable_m2/0"}]}],
    [
     ?_assertEqual(
        FunsWithNameOfMod,
        raw("mods[A=name].funs[name==A]")),
     
     ?_assertEqual(
        [{group_by,{nopos,"variable_m1:variable_m1/0"},eq,name,variable_m1},
         {group_by,{nopos,"variable_m1:variable_m1/1"},eq,name,variable_m1},
         {group_by,
          {nopos,"variable_m2:variable_m2/0"},
          eq,name,variable_m2}],
        raw("mods[A=name].funs[name==A].name")),

     ?_assertEqual(
        raw("mods.funs"),
        raw("mods[A=name].funs[B=name]")),

     ?_assertEqual(
        raw("mods.funs.name"),
        raw("mods[A=name].funs[B=name].name")),

     ?_assertEqual(
        empty(),
        raw("mods[A=name].funs[name==A, name/=A]")),

     ?_assertEqual(
        [{group_by,{nopos,"variable_m1:f/0"},
          list,
          [{nopos,"variable_m2:h/1"}]},
         {group_by,{nopos,"variable_m1:g/0"},
          list,
          [{nopos,"variable_m1:g/1"},{nopos,"variable_m2:g/0"}]},
         {group_by,{nopos,"variable_m1:m/0"},
          list,
          [{nopos,"variable_m1:f/0"}]},
         {group_by,{nopos,"variable_m1:variable_m1/1"},
          list,
          [{nopos,"variable_m1:variable_m1/1"}]},
         {group_by,{nopos,"variable_m2:g/0"},
          list,
          [{nopos,"variable_m2:h/0"}]},
         {group_by,{nopos,"variable_m2:variable_m2/0"},
          list,
          [{nopos,"variable_m2:variable_m2/0"}]}],
        raw("mods.funs[A=arity].calls[A=<arity]")),
     
     ?_assertEqual(
        FunsWithNameOfMod,
        raw("mods[M=name].funs[M==name]")),

     ?_assertEqual(
        [{group_by,
          {nopos,"variable_m1.erl"},
          list,
          [{nopos,"variable_m1:variable_m1/0"},
           {nopos,"variable_m1:variable_m1/1"}]},
         {group_by,
          {nopos,"variable_m2.erl"},
          list,
          [{nopos,"variable_m2:variable_m2/0"}]}],
        raw("mods[name=A].funs[name=B, A==B]")),

     ?_assertEqual(
        [{group_by,{nopos,"variable_m1.erl"},
          list,
          [{nopos,"variable_m1:f/0"},
           {nopos,"variable_m1:g/0"},
           {nopos,"variable_m1:g/1"},
           {nopos,"variable_m1:m/0"},
           {nopos,"variable_m1:variable_m2/0"}]},
         {group_by,{nopos,"variable_m2.erl"},
          list,
          [{nopos,"variable_m2:g/0"},
           {nopos,"variable_m2:g/1"},
           {nopos,"variable_m2:h/0"},
           {nopos,"variable_m2:h/1"}]}],
        raw("mods[A=name].funs[B=name, A/=B]")),

     ?_assertEqual(
        [{group_by,{nopos,"variable_m1.erl"},
          list,
          [{nopos,"variable_m1:f/0"}]}],
        raw("mods[A=name].funs[B=name, B==f]")),
     
     ?_assertEqual(
        [{group_by,{nopos,"variable_m1:g/0"},eq,name,g},
         {group_by,{nopos,"variable_m1:g/1"},eq,name,g},
         {group_by,{nopos,"variable_m2:g/0"},eq,name,g},
         {group_by,{nopos,"variable_m2:g/1"},eq,name,g}],
        raw("mods.funs[A=name, A==g].name")),

     ?_assertEqual(
        [{group_by,{nopos,"variable_m1:variable_m1/0"},
          eq,name,variable_m1},
         {group_by,{nopos,"variable_m1:variable_m1/1"},
          eq,name,variable_m1},
         {group_by,{nopos,"variable_m2:variable_m2/0"},
          eq,name,variable_m2}],
        raw("mods[A=name].funs[B=name, A==B].name")),

     ?_assertEqual(
        empty_property(),
        raw("mods[A=name].funs[name==A, name/=A].arity")),

     ?_assertEqual(
        [{group_by,{nopos,"variable_m1:variable_m1/0"},
          eq,name,variable_m1},
         {group_by,{nopos,"variable_m1:variable_m1/1"},
          eq,name,variable_m1},
         {group_by,{nopos,"variable_m2:variable_m2/0"},
          eq,name,variable_m2}],        
        raw("mods[A=name].funs[name==A].(calls[B=name])+.name")),

     ?_assertEqual(empty(),
                   raw("mods -- mods->A")),

     ?_assertEqual(empty(),
                   raw("mods(.funs -- .funs->A)")),

     ?_assertEqual(raw("mods.funs.name U mods.funs.calls.name"),
                   raw("mods.funs.(calls[A=name])1.name")),

     ?_assertEqual(
        [{chain,[{nopos,"variable_m1:variable_m1/0"}],"\n"},
         {chain,[{nopos,"variable_m1:variable_m1/1"},
                 {nopos,"variable_m1:variable_m1/1"}],
          "*\n"},
         {chain,[{nopos,"variable_m2:variable_m2/0"},
                 {nopos,"variable_m2:variable_m2/0"}],
          "*\n"}],
        raw("mods[A=name].funs[name==A].(calls[B=name])+")),

     ?_assertEqual(
        [{chain,[{nopos,"variable_m1:f/0"}],"\n"},
         {chain,[{nopos,"variable_m1:g/0"}],"\n"},
         {chain,[{nopos,"variable_m1:g/1"}],"\n"},
         {chain,[{nopos,"variable_m1:g/1"},{nopos,"variable_m2:g/0"}],
          "\n"},
         {chain,[{nopos,"variable_m1:m/0"}],"\n"},
         {chain,[{nopos,"variable_m1:variable_m1/0"}],"\n"},
         {chain,[{nopos,"variable_m1:variable_m1/1"}],"\n"},
         {chain,[{nopos,"variable_m1:variable_m1/1"},
                 {nopos,"variable_m1:variable_m1/1"}],
          "*\n"},
         {chain,[{nopos,"variable_m1:variable_m2/0"}],"\n"},
         {chain,[{nopos,"variable_m2:g/0"}],"\n"},
         {chain,[{nopos,"variable_m2:g/1"}],"\n"},
         {chain,[{nopos,"variable_m2:h/0"}],"\n"},
         {chain,[{nopos,"variable_m2:h/1"}],"\n"},
         {chain,[{nopos,"variable_m2:variable_m2/0"}],"\n"},
         {chain,[{nopos,"variable_m2:variable_m2/0"},
                 {nopos,"variable_m2:variable_m2/0"}],
          "*\n"}],
        raw("mods.funs.(calls[name=A])+.(calls[name=A])+")),

     ?_assertEqual(
        [{chain,[{nopos,"variable_m1:f/0"},
                 {nopos,"variable_m2:h/1"}],
          "\n"},
         {chain,[{nopos,"variable_m1:g/0"},
                 {nopos,"variable_m1:g/1"},
                 {nopos,"variable_m2:g/0"}],
          "\n"},
         {chain,[{nopos,"variable_m1:g/0"},
                 {nopos,"variable_m2:g/0"}],
          "\n"},
         {chain,[{nopos,"variable_m1:g/1"},{nopos,"variable_m2:g/0"}],
          "\n"},
         {chain,[{nopos,"variable_m1:m/0"},{nopos,"variable_m1:f/0"}],
          "\n"},
         {chain,[{nopos,"variable_m1:variable_m1/0"}],"\n"},
         {chain,[{nopos,"variable_m1:variable_m1/1"},
                 {nopos,"variable_m1:variable_m1/1"}],
          "*\n"},
         {chain,[{nopos,"variable_m1:variable_m2/0"}],"\n"},
         {chain,[{nopos,"variable_m2:g/0"},{nopos,"variable_m2:h/0"}],
          "\n"},
         {chain,[{nopos,"variable_m2:g/1"},{nopos,"variable_m2:g/0"}],
          "\n"},
         {chain,[{nopos,"variable_m2:h/0"}],"\n"},
         {chain,[{nopos,"variable_m2:h/1"},{nopos,"variable_m1:f/0"}],
          "\n"},
         {chain,[{nopos,"variable_m2:variable_m2/0"},
                 {nopos,"variable_m2:variable_m2/0"}],
          "*\n"}],
        raw("mods[A=name].funs[B=name].(calls[C=name])+")),
     
     ?_assertEqual(
        [{group_by,{nopos,"variable_m1:f/0"},eq,name,f},
         {group_by,{nopos,"variable_m1:g/0"},eq,name,g},
         {group_by,{nopos,"variable_m1:g/1"},eq,name,g},
         {group_by,{nopos,"variable_m1:m/0"},eq,name,m},
         {group_by,{nopos,"variable_m1:variable_m1/0"},
          eq,name,variable_m1},
         {group_by,{nopos,"variable_m1:variable_m1/1"},
          eq,name,variable_m1},
         {group_by,{nopos,"variable_m1:variable_m2/0"},
          eq,name,variable_m2},
         {group_by,{nopos,"variable_m2:g/0"},eq,name,g},
         {group_by,{nopos,"variable_m2:g/1"},eq,name,g},
         {group_by,{nopos,"variable_m2:h/0"},eq,name,h},
         {group_by,{nopos,"variable_m2:h/1"},eq,name,h},
         {group_by,{nopos,"variable_m2:variable_m2/0"},
          eq,name,variable_m2}],
        raw("mods[A=name].funs[B=name].(calls[C=name])+.name")),

     ?_assertEqual(
        [{group_by,{nopos,"variable_m1:f/0"},eq,name,f},
         {group_by,{nopos,"variable_m1:g/0"},eq,name,g},
         {group_by,{nopos,"variable_m1:g/1"},eq,name,g},
         {group_by,{nopos,"variable_m1:m/0"},eq,name,m},
         {group_by,{nopos,"variable_m1:variable_m1/0"},
          eq,name,variable_m1},
         {group_by,{nopos,"variable_m1:variable_m1/1"},
          eq,name,variable_m1},
         {group_by,{nopos,"variable_m1:variable_m2/0"},
          eq,name,variable_m2},
         {group_by,{nopos,"variable_m2:g/0"},eq,name,g},
         {group_by,{nopos,"variable_m2:g/1"},eq,name,g},
         {group_by,{nopos,"variable_m2:h/0"},eq,name,h},
         {group_by,{nopos,"variable_m2:h/1"},eq,name,h},
         {group_by,{nopos,"variable_m2:variable_m2/0"},
          eq,name,variable_m2}],
        raw("mods.funs[A=name].(calls[name==A])+.name")),

     ?_assertEqual(
        [{chain,[{nopos,"variable_m1:f/0"}],"\n"},
         {chain,[{nopos,"variable_m1:g/0"},
                 {nopos,"variable_m1:g/1"},
                 {nopos,"variable_m2:g/0"}],
          "\n"},
         {chain,[{nopos,"variable_m1:g/0"},
                 {nopos,"variable_m2:g/0"}],
          "\n"},
         {chain,[{nopos,"variable_m1:g/1"},{nopos,"variable_m2:g/0"}],
          "\n"},
         {chain,[{nopos,"variable_m1:m/0"}],"\n"},
         {chain,[{nopos,"variable_m1:variable_m1/0"}],"\n"},
         {chain,[{nopos,"variable_m1:variable_m1/1"},
                 {nopos,"variable_m1:variable_m1/1"}],
          "*\n"},
         {chain,[{nopos,"variable_m1:variable_m2/0"}],"\n"},
         {chain,[{nopos,"variable_m2:g/0"}],"\n"},
         {chain,[{nopos,"variable_m2:g/1"},{nopos,"variable_m2:g/0"}],
          "\n"},
         {chain,[{nopos,"variable_m2:h/0"}],"\n"},
         {chain,[{nopos,"variable_m2:h/1"}],"\n"},
         {chain,[{nopos,"variable_m2:variable_m2/0"},
                 {nopos,"variable_m2:variable_m2/0"}],
          "*\n"}],
        raw("mods.funs[A=name].(calls[name==A])+")),

     ?_assertEqual(
        [{chain,[{nopos,"variable_m1:f/0"}],"\n"},
         {chain,[{nopos,"variable_m1:g/0"}],"\n"},
         {chain,[{nopos,"variable_m1:g/1"},
                 {nopos,"variable_m2:g/0"},
                 {nopos,"variable_m2:h/0"}],
          "\n"},
         {chain,[{nopos,"variable_m1:m/0"}],"\n"},
         {chain,[{nopos,"variable_m1:variable_m1/0"}],"\n"},
         {chain,[{nopos,"variable_m1:variable_m1/1"},
                 {nopos,"variable_m1:variable_m1/1"}],
          "*\n"},
         {chain,[{nopos,"variable_m1:variable_m2/0"}],"\n"},
         {chain,[{nopos,"variable_m2:g/0"}],"\n"},
         {chain,[{nopos,"variable_m2:g/1"},
                 {nopos,"variable_m2:g/0"},
                 {nopos,"variable_m2:h/0"}],
          "\n"},
         {chain,[{nopos,"variable_m2:h/0"}],"\n"},
         {chain,[{nopos,"variable_m2:h/1"},
                 {nopos,"variable_m1:f/0"},
                 {nopos,"variable_m2:h/1"}],
          "*\n"},
         {chain,[{nopos,"variable_m2:variable_m2/0"}],"\n"}],
        raw("mods.funs[A=arity].(calls[A==1])+")),

     ?_assertEqual(
        empty(),
        raw("mods.funs[name==h].(calls[A=name])+[name==A,A==h]")),

     ?_assertEqual(
        empty(),
        raw("mods.funs.(calls[A=name])+[name/=A]")),

     ?_assertEqual(
        raw("mods.funs.calls.name"),
        raw("mods.funs.(calls[A=name])1[name==A].name")),

     ?_assertEqual(
        [{group_by,{nopos,"variable_m1:f/0"},eq,name,f},
         {group_by,{nopos,"variable_m1:g/1"},eq,name,g},
         {group_by,{nopos,"variable_m1:variable_m1/1"},
          eq,name,variable_m1},
         {group_by,{nopos,"variable_m2:g/0"},eq,name,g},
         {group_by,{nopos,"variable_m2:h/0"},eq,name,h},
         {group_by,{nopos,"variable_m2:h/1"},eq,name,h},
         {group_by,{nopos,"variable_m2:variable_m2/0"},
          eq,name,variable_m2}],
        raw("mods[A=name].funs[B=name].{calls[C=name]}1.name")),

     ?_assertEqual(
        [{chain,[{nopos,"variable_m1:g/0"},
                 {nopos,"variable_m1:g/1"}],
          "\n"},
         {chain,[{nopos,"variable_m1:g/0"},
                 {nopos,"variable_m2:g/0"}],
          "\n"},
         {chain,[{nopos,"variable_m1:g/1"},{nopos,"variable_m2:g/0"}],
          "\n"},
         {chain,[{nopos,"variable_m1:variable_m1/1"},
                 {nopos,"variable_m1:variable_m1/1"}],
          "\n"},
         {chain,[{nopos,"variable_m2:g/1"},{nopos,"variable_m2:g/0"}],
          "\n"},
         {chain,[{nopos,"variable_m2:variable_m2/0"},
                 {nopos,"variable_m2:variable_m2/0"}],
          "\n"}],
        raw("mods.funs[A=name].{calls[name==A]}1")),
     
     ?_assertEqual(
        [{chain,[{nopos,"variable_m1:f/0"},
                 {nopos,"variable_m2:h/1"}],
          "\n"},
         {chain,[{nopos,"variable_m1:g/0"},{nopos,"variable_m1:g/1"}],
          "\n"},
         {chain,[{nopos,"variable_m1:g/0"},{nopos,"variable_m2:g/0"}],
          "\n"},
         {chain,[{nopos,"variable_m1:g/1"},{nopos,"variable_m2:g/0"}],
          "\n"},
         {chain,[{nopos,"variable_m1:m/0"},{nopos,"variable_m1:f/0"}],
          "\n"},
         {chain,[{nopos,"variable_m1:variable_m1/1"},
                 {nopos,"variable_m1:variable_m1/1"}],
          "\n"},
         {chain,[{nopos,"variable_m2:g/0"},{nopos,"variable_m2:h/0"}],
          "\n"},
         {chain,[{nopos,"variable_m2:g/1"},{nopos,"variable_m2:g/0"}],
          "\n"},
         {chain,[{nopos,"variable_m2:h/1"},{nopos,"variable_m1:f/0"}],
          "\n"},
         {chain,[{nopos,"variable_m2:variable_m2/0"},
                 {nopos,"variable_m2:variable_m2/0"}],
          "\n"}],
        raw("mods.funs[A=name].{calls[B=name]}1")),

     ?_assertEqual(
        [{chain,[{nopos,"variable_m1:f/0"},
                 {nopos,"variable_m2:h/1"}],
          "\n"},
         {chain,[{nopos,"variable_m1:m/0"},{nopos,"variable_m1:f/0"}],
          "\n"},
         {chain,[{nopos,"variable_m2:g/0"},{nopos,"variable_m2:h/0"}],
          "\n"},
         {chain,[{nopos,"variable_m2:h/1"},{nopos,"variable_m1:f/0"}],
          "\n"}],
        raw("mods.funs[A=name].{calls[B=name, A/=B]}1")),
     
     ?_assertEqual(
        FunsWithNameOfMod,
        raw("mods[M=name].funs[M in .name]")),

     ?_assertEqual(
        raw("files"),
        raw("files[.funs in .funs->F.calls.F]")),

     ?_assertEqual(
        raw("files"),
        raw("files[.funs->G.calls in .funs.calls->F.F]")),

     ?_assertEqual(
        empty_property(),
        raw("mods.funs[A=arity, .calls[B=arity], A<B, A==B].name")),

     ?_assertEqual(
        [{group_by,{nopos,"variable_m1.erl"},
          list,
          [{nopos,"variable_m1:g/0"},
           {nopos,"variable_m1:g/1"},
           {nopos,"variable_m1:variable_m1/1"}]},
         {group_by,{nopos,"variable_m2.erl"},
          list,
          [{nopos,"variable_m2:g/1"},
           {nopos,"variable_m2:variable_m2/0"}]}],
        raw("mods.funs[A=name, .calls[B=name], A==B]")),

     ?_assertEqual(
        raw("mods.funs[.calls]"),
        raw("mods.funs[.calls[A=name].A]")),

     ?_assertEqual(
        [{group_by,{nopos,"variable_m1:variable_m1/1"},eq,name,variable_m1},
         {group_by,
          {nopos,"variable_m2:variable_m2/0"},
          eq,name,variable_m2}],
        raw("mods[A=name].funs[.calls[name==A]].name")),

     ?_assertEqual(
        [{group_by,{nopos,"variable_m1:g/0"},
          list,
          [{nopos,"variable_m1:g/1"},{nopos,"variable_m2:g/0"}]},
         {group_by,{nopos,"variable_m1:variable_m1/1"},
          list,
          [{nopos,"variable_m1:variable_m1/1"}]},
         {group_by,{nopos,"variable_m2:variable_m2/0"},
          list,
          [{nopos,"variable_m2:variable_m2/0"}]}],
        raw("mods.funs[.calls[A=name].calls[B=name]].calls[A==B]")),

     ?_assertEqual(
        empty(),
        raw("mods.funs[A=name] I mods.funs[name/=A]")),
     
     ?_assertEqual(
        empty_property(),
        raw("mods[M1=name].funs[A=name].name I mods[M2=name].funs[name/=A].name")),

     ?_assertEqual(
        raw("mods[name=variable_m1]"),
        raw("(mods->M[name=variable_m1]->FM I mods->Mods) I mods?Mods")),
     
     ?_assertEqual(
        empty(),
        raw("mods(.funs[A=name] I .funs[name/=A])")),
     
     ?_assertEqual(
        empty_property(),
        raw("mods[M=name](.funs[A=name] I .funs[name/=A]).name")),
     
     ?_assertEqual(
        [{group_by,{nopos,"variable_m1.erl"},
          list,
          [{nopos,"variable_m1:f/0"},
           {nopos,"variable_m1:g/0"},
           {nopos,"variable_m1:g/1"},
           {nopos,"variable_m1:m/0"},
           {nopos,"variable_m1:variable_m1/0"},
           {nopos,"variable_m1:variable_m1/1"},
           {nopos,"variable_m1:variable_m2/0"}]},
         {group_by,{nopos,"variable_m2.erl"},
          list,
          [{nopos,"variable_m2:g/0"},
           {nopos,"variable_m2:g/1"},
           {nopos,"variable_m2:h/0"},
           {nopos,"variable_m2:h/1"},
           {nopos,"variable_m2:variable_m2/0"}]}],
        raw("mods.funs[A=name] U mods.funs[B=name] U mods.funs[C=name]")),

     ?_assertEqual(
        [{group_by,{nopos,"variable_m1:f/0"},eq,name,f},
         {group_by,{nopos,"variable_m1:g/0"},eq,name,g},
         {group_by,{nopos,"variable_m1:g/1"},eq,name,g},
         {group_by,{nopos,"variable_m1:m/0"},eq,name,m},
         {group_by,{nopos,"variable_m1:variable_m1/0"},
          eq,name,variable_m1},
         {group_by,{nopos,"variable_m1:variable_m1/1"},
          eq,name,variable_m1},
         {group_by,{nopos,"variable_m1:variable_m2/0"},
          eq,name,variable_m2},
         {group_by,{nopos,"variable_m2:g/0"},eq,name,g},
         {group_by,{nopos,"variable_m2:g/1"},eq,name,g},
         {group_by,{nopos,"variable_m2:h/0"},eq,name,h},
         {group_by,{nopos,"variable_m2:h/1"},eq,name,h},
         {group_by,{nopos,"variable_m2:variable_m2/0"},
          eq,name,variable_m2}],
        raw("mods[M=name](.funs[A=name] I .funs[B=name] I .funs[C=name]).name")),

     ?_assertEqual(
        [{group_by,{nopos,"variable_m1:f/0"},
          list,
          [{nopos,"variable_m2:h/1"}]},
         {group_by,{nopos,"variable_m1:g/0"},
          list,
          [{nopos,"variable_m2:g/0"}]},
         {group_by,{nopos,"variable_m1:g/1"},
          list,
          [{nopos,"variable_m2:g/0"}]},
         {group_by,{nopos,"variable_m2:h/1"},
          list,
          [{nopos,"variable_m1:f/0"}]}],
        raw("(mods[M=name] U (mods[name==M] I mods[name/=M])).funs.calls[mod/=M]")),

     ?_assertEqual(
        empty_property(),
        raw("(mods[M=name].funs I mods[name==M].funs[A=name].calls).file[name/=M].name")),

     ?_assertEqual(
        raw("mods.funs->F[.calls?F]"),
        raw("mods->M.funs->C[.calls, .calls any_in C]")),

     ?_assertEqual(
        empty(),
        raw("mods[M=name, .funs any_in mods[name/=M].funs].funs")),

     ?_assertEqual(
        empty(),
        raw("mods[.funs[M=mod] all_in mods[name/=M].funs[A=name]].funs")),

     ?_assertEqual(
        FunsWithNameOfMod,
        raw("mods[M=name].funs[name in mods[name==M].name]")),

     ?_assertEqual(
        FunsWithNameOfMod,
        raw("mods[.funs[A=name] all_in mods[name==A].funs[name==A]].funs[mod==A, name==A]")),

     ?_assertEqual(
        FunsWithNameOfMod,
        raw("mods[mods[M=name].funs[A=name] any_in .funs[mod==M, name==A]].funs[name==M]")),

     ?_assertEqual(
        raw("mods.funs.name"),
        raw("mods->M.funs.name")),
     
     ?_assertEqual(
        [{group_by,{nopos,"variable_m1:variable_m1/1"},
          eq,".called_by.name",variable_m1},
         {group_by,{nopos,"variable_m2:variable_m2/0"},
          eq,".called_by.name",variable_m2}],
        raw("mods.funs->F.calls?F.called_by.name",[{groupby,3}])),

     ?_assertEqual(
        [{group_by,{nopos,"variable_m1:variable_m1/1"},
          list,
          [{nopos,"variable_m1:variable_m1/1"}]},
         {group_by,{nopos,"variable_m2:variable_m2/0"},
          list,
          [{nopos,"variable_m2:variable_m2/0"}]}],
        raw("mods.funs->F.calls?F")),

     ?_assertEqual(
        [{group_by,{nopos,"variable_m1:f/0"},
          list,
          [{nopos,"variable_m2:h/1"}]},
         {group_by,{nopos,"variable_m1:g/0"},
          list,
          [{nopos,"variable_m1:g/1"},{nopos,"variable_m2:g/0"}]},
         {group_by,{nopos,"variable_m1:g/1"},
          list,
          [{nopos,"variable_m2:g/0"}]},
         {group_by,{nopos,"variable_m1:m/0"},
          list,
          [{nopos,"variable_m1:f/0"}]},
         {group_by,{nopos,"variable_m1:variable_m1/1"},
          list,
          [{nopos,"variable_m1:variable_m1/1"}]},
         {group_by,{nopos,"variable_m2:g/0"},
          list,
          [{nopos,"variable_m2:h/0"}]},
         {group_by,{nopos,"variable_m2:g/1"},
          list,
          [{nopos,"variable_m2:g/0"}]},
         {group_by,{nopos,"variable_m2:h/1"},
          list,
          [{nopos,"variable_m1:f/0"}]},
         {group_by,{nopos,"variable_m2:variable_m2/0"},
          list,
          [{nopos,"variable_m2:variable_m2/0"}]}],
        raw("mods.funs[.calls->A].A")),
     
     ?_assertEqual(
        [{group_by,{nopos,"variable_m1:m/0"},
          list,
          [{nopos,"variable_m1:f/0"}]},
         {group_by,{nopos,"variable_m2:h/1"},
          list,
          [{nopos,"variable_m1:f/0"}]}],
        raw("mods.fun[.calls->A[name==f]].A")),

     ?_assertEqual(
        [{group_by,{nopos,"variable_m1:f/0"},eq,name,f},
         {group_by,{nopos,"variable_m1:g/0"},eq,name,g},
         {group_by,{nopos,"variable_m1:g/1"},eq,name,g},
         {group_by,{nopos,"variable_m2:h/1"},eq,name,h}],
        raw("mods->M.funs[.calls.file/?M].name")),

     ?_assertEqual(
        [{group_by,{nopos,"variable_m1:variable_m1/0"},
          eq,"M",variable_m1},
         {group_by,{nopos,"variable_m1:variable_m1/1"},
          eq,"M",variable_m1},
         {group_by,{nopos,"variable_m2:variable_m2/0"},
          eq,"M",variable_m2}],
        raw("mods[M=name].funs[name==M].M")),

     ?_assertEqual(
        [{chain,[{nopos,"variable_m1:f/0"},
                 {nopos,"variable_m2:h/1"}], "\n"},
         {chain,[{nopos,"variable_m1:g/0"},{nopos,"variable_m1:g/1"}], "\n"},
         {chain,[{nopos,"variable_m1:g/0"},{nopos,"variable_m2:g/0"}], "\n"},
         {chain,[{nopos,"variable_m1:g/1"},{nopos,"variable_m2:g/0"}], "\n"},
         {chain,[{nopos,"variable_m1:m/0"},{nopos,"variable_m1:f/0"}], "\n"},
         {chain,[{nopos,"variable_m1:variable_m1/0"}],"\n"},
         {chain,[{nopos,"variable_m1:variable_m1/1"},
                 {nopos,"variable_m1:variable_m1/1"}], "*\n"},
         {chain,[{nopos,"variable_m1:variable_m2/0"}],"\n"},
         {chain,[{nopos,"variable_m2:g/0"},{nopos,"variable_m2:h/0"}], "\n"},
         {chain,[{nopos,"variable_m2:g/1"},{nopos,"variable_m2:g/0"}], "\n"},
         {chain,[{nopos,"variable_m2:h/0"}],"\n"},
         {chain,[{nopos,"variable_m2:h/1"},{nopos,"variable_m1:f/0"}], "\n"},
         {chain,[{nopos,"variable_m2:variable_m2/0"},
                 {nopos,"variable_m2:variable_m2/0"}],
          "*\n"}],
        raw("mods->M.funs->F.(calls->C)+")),

     ?_assertEqual(
        [{chain,[{nopos,"variable_m1:f/0"},
                 {nopos,"variable_m1:f/0"}],
          "*\n"},
         {chain,[{nopos,"variable_m1:g/0"},{nopos,"variable_m1:g/0"}],
          "*\n"},
         {chain,[{nopos,"variable_m1:g/1"},{nopos,"variable_m1:g/1"}],
          "*\n"},
         {chain,[{nopos,"variable_m1:m/0"},{nopos,"variable_m1:m/0"}],
          "*\n"},
         {chain,[{nopos,"variable_m1:variable_m1/0"},
                 {nopos,"variable_m1:variable_m1/0"}],
          "*\n"},
         {chain,[{nopos,"variable_m1:variable_m1/1"},
                 {nopos,"variable_m1:variable_m1/1"}],
          "*\n"},
         {chain,[{nopos,"variable_m1:variable_m2/0"},
                 {nopos,"variable_m1:variable_m2/0"}],
          "*\n"},
         {chain,[{nopos,"variable_m2:g/0"},{nopos,"variable_m2:g/0"}],
          "*\n"},
         {chain,[{nopos,"variable_m2:g/1"},{nopos,"variable_m2:g/1"}],
          "*\n"},
         {chain,[{nopos,"variable_m2:h/0"},{nopos,"variable_m2:h/0"}],
          "*\n"},
         {chain,[{nopos,"variable_m2:h/1"},{nopos,"variable_m2:h/1"}],
          "*\n"},
         {chain,[{nopos,"variable_m2:variable_m2/0"},
                 {nopos,"variable_m2:variable_m2/0"}],
          "*\n"}],
        raw("mods.funs->F.(calls.F)+")),

     ?_assertEqual(
        empty_property(),
        raw("mods.funs[name=f].(calls->F)1.calls?F.name")),

     ?_assertEqual(
        [{group_by,
          {nopos,"variable_m1:variable_m1/1"},
          list,
          [{nopos,"variable_m1:variable_m1/1"}]},
         {group_by,
          {nopos,"variable_m2:variable_m2/0"},
          list,
          [{nopos,"variable_m2:variable_m2/0"}]}],
        raw("mods.funs.(calls->F)1.calls?F")),

     %% grouped tests
     
     ?_assertEqual(
        [{group_by,{nopos,"variable_m1.erl"},
          propertylist,".funs->F.(calls.F)+.name",
          [f,g,m,variable_m1,variable_m2]},
         {group_by,{nopos,"variable_m2.erl"},
          propertylist,".funs->F.(calls.F)+.name",
          [g,h,variable_m2]}],           
        raw("mods.funs->F.(calls.F)+.name", [{groupby,1}])),

     ?_assertEqual(
        [{group_by,{nopos,"variable_m1.erl"},
          list,
          [{nopos,"variable_m1.erl"}]},
         {group_by,{nopos,"variable_m2.erl"},
          list,
          [{nopos,"variable_m2.erl"}]}],
        raw("mods->M.funs.M",[{groupby,1}])),
     
     ?_assertEqual(
        [{group_by,{nopos,"variable_m1.erl"},
          list,
          [{nopos,"variable_m1:f/0"},
           {nopos,"variable_m1:g/0"},
           {nopos,"variable_m1:g/1"},
           {nopos,"variable_m1:m/0"},
           {nopos,"variable_m1:variable_m1/0"},
           {nopos,"variable_m1:variable_m1/1"},
           {nopos,"variable_m1:variable_m2/0"}]},
         {group_by,{nopos,"variable_m2.erl"},
          list,
          [{nopos,"variable_m2:g/0"},
           {nopos,"variable_m2:g/1"},
           {nopos,"variable_m2:h/0"},
           {nopos,"variable_m2:h/1"},
           {nopos,"variable_m2:variable_m2/0"}]}],
        raw("mods.funs->F.F",[{groupby,1}])),

     ?_assertEqual(
        raw("mods.funs.calls", [{groupby,1}]),
        raw("mods.funs[.calls->A].A", [{groupby,1}])),

     ?_assertEqual(
         raw("mods"),
         raw("mods->M U (mods?M I mods/?M)", [{groupby,1}])),

     ?_assertEqual(
        [{group_by,{nopos,"variable_m2.erl"},
           propertylist,".funs.(calls)2.name",
           [f,g,h,variable_m2]}],
        raw("mods[name=variable_m2].funs.(calls[A=name])2.name", [{groupby, 1}])),

     ?_assertEqual([{eq,avg,8.5}], raw("mods->M.loc:avg")),

     ?_assertEqual(
        raw("mods.funs.loc:avg", [{groupby,1}]),
        raw("mods.funs[loc=A].A:avg", [{groupby,1}]))

    ].

setop_testfile1() ->
    {module, ?MISC:canonical_filename("m1.erl"),
    "-module(m1).\n"
    "-export([f/1]).\n"
    "f(Var)-> [Var, m2:h()].\n"}.
    
setop_testfile2() ->
    {module, ?MISC:canonical_filename("m2.erl"),
    "-module(m2).\n"
    "-export([h/0]).\n"
    "h()-> ok.\n"
    "g(stop)-> ok;\n"
    "g(X)-> g(X-1).\n"}.

setop_setup() ->
    [add_file(setop_testfile1()),add_file(setop_testfile2())].

setop_test_() ->
    {setup,
     fun setop_setup/0,
     fun cleanup/1,
     setop_tests()}.
    
setop_tests() ->
    [?_assertEqual(
	[{group_by,{nopos,"m1:f/1"},eq,name,f},
	 {group_by,{nopos,"m2:h/0"},eq,name,h},
	 {group_by,{nopos,"m2:g/1"},eq,name,g}],

	refusr_sq:run( [{output, other}],[],
		       "mods[.name all_in |m1,m2|](.funs union .funs.calls).name")
       ),

     ?_assertEqual(empty_property(),
                   raw("mods[false](.funs U mods.funs).name", [{groupby,1}])),

     ?_assertEqual(raw("mods.funs[exported].name"),
                   raw("mods.funs[name in mods.funs[exported].name].name")),

     ?_assertEqual(raw("mods.funs.name"),
                   raw("mods[false](.funs U mods.funs).name")),

     ?_assertEqual(
	refusr_sq:run( [{output, other}],[],
		       "mods.funs"),

	refusr_sq:run( [{output, other}],[],
		       "mods(.funs union .funs)")
       ),

     ?_assertEqual(empty(), raw("mods -- files")),                   

     ?_assertEqual(
	refusr_sq:run( [{output, other}],[],
		       "mods.funs"),

	refusr_sq:run( [{output, other}],[],
		       "mods(.funs union .funs intersect .funs)")
       ),

     ?_assertEqual(
	refusr_sq:run( [{output, other}],[],
		       "mods.funs"),

	refusr_sq:run( [{output, other}],[],
		       "mods((.funs intersection .funs) union ((.funs minus .funs) union .funs))")
       ),

     ?_assertEqual(
	[{list,[]}],

	refusr_sq:run( [{output, other}],[],
		       "mods(.funs minus .funs).vars")
       ),

     ?_assertEqual(
	[{list, []}],

	refusr_sq:run( [{output, other}],[],
		       "mods(.funs minus .funs)")
       ),

     ?_assertEqual(
	refusr_sq:run( [{output, other}],[],
		       "mods(.funs union .funs)"),

	refusr_sq:run( [{output, other}],[],
		       "mods(.funs intersection .funs)")
       ),

     ?_assertEqual(
	element(2,hd(refusr_sq:run( [{output, nodes}],[],
				    "mods(.funs.called_by union .funs.calls).vars"))),

	element(2,hd(refusr_sq:run( [{output, nodes}],[],
				    "(mods.funs.called_by union mods.funs.calls).vars")))
       ),

     ?_assertEqual(
	element(2,hd(refusr_sq:run( [{output, nodes}],[],
				    "mods(.funs.called_by union .funs.calls).vars"))),

	element(2,hd(refusr_sq:run( [{output, nodes}],[],
				    "mods.funs.called_by.vars union mods.funs.calls.vars")))
       ),

     ?_assertEqual(
	refusr_sq:run( [{output, other}],[],
		       "mods.funs"),

	refusr_sq:run( [{output, other}],[],
		       "(mods.funs intersection mods.funs)")
       ),

     ?_assertEqual(
	refusr_sq:run( [{output, other}],[],
		       "mods.funs[.name any_in |g,h|]"),

	refusr_sq:run( [{output, other}],[],
		       "mods.funs[name==g or name==h]")
       ),

     ?_assertEqual(
	refusr_sq:run( [{output, other}],[],
		       "mods.funs[.name any_in mods.funs.name]"),

	refusr_sq:run( [{output, other}],[],
		       "mods.funs")
       ),

     ?_assertEqual(
	refusr_sq:run( [{output, other}],[],
		       "mods[.funs.name all_in mods.funs.name]"),

	refusr_sq:run( [{output, other}],[],
		       "mods")
       ),

     ?_assertEqual(
	lists:sort([{chain,[{nopos,"m2:g/1"},{nopos,"m2:g/1"}],"*\n"},
		    {chain,[{nopos,"m1:f/1"},{nopos,"m2:h/0"},{nopos,"m1:f/1"}],"*\n"},
		    {chain,[{nopos,"m2:h/0"},{nopos,"m1:f/1"},{nopos,"m2:h/0"}],"*\n"}]),

	lists:sort(refusr_sq:run( [{output, other}],[],
				  "mods[.name any_in |m1,m2|].funs.(.calls union .called_by)+"))
       ),

     ?_assertEqual(
	lists:sort(refusr_sq:run( [{output, other}],[],
					 "mods.funs.(.calls union (.called_by minus .called_by))+")),

	lists:sort(refusr_sq:run( [{output, other}],[],
					 "mods.funs.(calls)+"))
       ),

     ?_assertEqual(
	lists:sort(refusr_sq:run( [{output, other}],[],
					 "mods[name==m2].funs[name==g]")),

	lists:sort(refusr_sq:run( [{output, other}],[],
					 "mods[name==m2].funs[[true] in .calls U .called_by]"))
       ),

     ?_assertEqual(
	lists:sort(refusr_sq:run( [{output, other}],[],
					 "mods[name==m2].funs[name==h]")),

	lists:sort(refusr_sq:run( [{output, other}],[],
					 "mods[name==m1 or name==m2].funs[.exprs.(sub)+.val all_in (mods[name in |m1,m2|].funs--[true]).exprs.(sub)+.val]"))
       ),

     ?_assertEqual(
	run_nodes("mods.funs(.calls.calls U .called_by)"),
	run_nodes("mods.funs I mods(.funs(.calls U .calls).calls U .funs.called_by I mods.funs)")
       )].

run_nodes(Query) ->
    lists:sort(
        lists:flatten(
            element(2, 
                hd(refusr_sq:run([{output, nodes}], [], Query))))).
    
conv_testfile1() ->
    {module, ?MISC:canonical_filename("m3.erl"),
    "-module(m3).\n"
    "-export([f/0]).\n"
    "f()-> m4:h(ok).\n"
    "stop()-> m4:h(ok).\n"}.
    
conv_testfile2() ->
    {module, ?MISC:canonical_filename("m4.erl"),
    "-module(m4).\n"
    "-export([h/1]).\n"
    "-define(F, stop).\n"
    "h(X)-> ?F.\n"
    "g(stop)-> h;\n"
    "g(st)-> 1;\n"
    "g(X)-> g(X-1).\n"}.

conv_setup() ->
    [add_file(conv_testfile1()),add_file(conv_testfile2())].

conv_test_() ->
    {setup,
     fun conv_setup/0,
     fun cleanup/1,
     conv_tests()}.

conv_tests()->
    [?_assertEqual(
	[{group_by,{nopos,"m4:g/1"},eq,name,g}],

	refusr_sq:run( [{output, other}],[],
		       "mods[.name all_in |m4|](.funs union .funs.calls)[name in |\"g\"|][.body.val any_in .file.funs.name].name")
       ),

     ?_assertEqual(
	refusr_sq:run( [{output, other}],[],
		       "mods[name==\"m4\"].funs[name=='h']"),

	refusr_sq:run( [{output, other}],[],
		       "mods[name==m4].funs[.expr.macro_value any_in .file.funs.args.val]")
       ),

     ?_assertEqual(
	refusr_sq:run( [{output, other}],[],
		       "mods[name=='m4'].funs[name==\"g\"]"),

	refusr_sq:run( [{output, other}],[],
		       "mods[name==m4].funs[.args.val any_in .file.funs.expr.macro_value]")
       ),

     ?_assertEqual(
	refusr_sq:run( [{output, other}],[],
		       "mods[name==m4 or name==m3].funs"),

	refusr_sq:run( [{output, other}],[],
		       "mods[.name any_in |'m3', m3, \"m4\"|].funs[arity in |\"0\", 1|]")
       ),

     ?_assertEqual(
	refusr_sq:run( [{output, other}],[],
		       "mods[name==m4].funs[name==g]"),

	refusr_sq:run( [{output, other}],[],
		       "mods[name==m4].funs[.body.val any_in .arity]")
       ),

     ?_assertEqual(
	refusr_sq:run( [{output, other}],[],
		       "mods[name==m4].funs[name in |g, h|]"),

	refusr_sq:run( [{output, other}],[],
		       "mods[name==m4].funs[arity in mods.funs.body.val]")
       ),

     ?_assertEqual(
	refusr_sq:run( [{output, other}],[],
		       "mods[name==m3].funs[name in |'stop'|]"),

	refusr_sq:run( [{output, other}],[],
		       "mods[name==m3].funs[.name any_in mods.funs.args.val]")
       ),

     ?_assertEqual(
	[{group_by,{nopos,"m3.erl"},propertylist,value,["m3"]},
	 {group_by,{nopos,"m4.erl"},propertylist,value,["X","m4"]}],

	refusr_sq:run( [{output, other}],[],
		       "mods[name in |m3, m4|](.name U .funs.vars.name)")
       ),

     ?_assertEqual(
	run_nodes("mods[name in |m3, m4|](.name U .funs.vars.name)"),
	run_nodes("mods[name in |m3, m4|].name U mods[name in |m3, m4|].funs.vars.name")
       ),

     ?_assertEqual(
	run_nodes("mods[name in |m3, m4|](.name U .funs.vars.name)"),
	run_nodes("mods[name in |m3, m4|](.name U mods[name in |m3, m4|].funs.vars.name)"))].

filter_test_() ->
    {setup,
     fun conv_setup/0,
     fun cleanup/1,
     filter_tests()}.

filter_tests()->
    [?_assertEqual(
        raw("mods[A=name]"),
        raw("mods[(A=name)]")),

     ?_assertEqual(
        raw("mods[A=name]"),
        raw("mods[(A=name, name==A)]")),

     ?_assertEqual(
        [{group_by,{nopos,"m3.erl"},list,[{nopos,"m3:stop/0"}]},
         {group_by,{nopos,"m4.erl"},list,[{nopos,"m4:g/1"}]}],
        raw("mods.funs[not exported]")),

     ?_assertEqual(
        [{group_by,{nopos,"m3.erl"},list,[{nopos,"m3:stop/0"}]},
         {group_by,{nopos,"m4.erl"},list,[{nopos,"m4:g/1"}]}],
        raw("mods.funs[not (exported)]")),

     ?_assertEqual(
        [{group_by,{nopos,"m3.erl"},list,[{nopos,"m3:stop/0"}]},
         {group_by,{nopos,"m4.erl"},list,[{nopos,"m4:h/1"}]}],
        raw("mods.funs[(exported == true) == (arity == 1)]")),

     ?_assertEqual(
        [{list,[{nopos,"m3.erl"}]}],
        raw("mods[name='m3']")),

     ?_assertEqual(
        [{group_by,{nopos,"m3.erl"},
          list,
          [{nopos,"m3:f/0"},{nopos,"m3:stop/0"}]}],
        raw("mods[name~3].funs")),

     ?_assertEqual(
        [{group_by,{nopos,"m3.erl"},list,[{nopos,"m3:stop/0"}]}],
        raw("mods.funs[name like \"sto\"]"))
    ].

name_testfile() ->
    {module, ?MISC:canonical_filename("name.erl"),
    "-module(name).\n"
    "-export([f/0]).\n"
    "f()-> it2:h(3).\n"
    "name()-> ok.\n"}.

name_setup() ->
    [add_file(name_testfile())].

name_test_() ->
    {setup,
     fun name_setup/0,
     fun cleanup/1,
     name_tests()}.

name_tests()->
    [
     ?_assertEqual(
        [{group_by,{nopos,"name.erl"},list,[{nopos,"name:name/0"}]}],
        raw("mods.funs[name=name]"))
    ].

iteration_testfile1() ->
    {module, ?MISC:canonical_filename("it1.erl"),
    "-module(it1).\n"
    "-export([f/0]).\n"
    "f()-> it2:h(3).\n"
    "g()-> it2:f().\n"}.
    
iteration_testfile2() ->
    {module, ?MISC:canonical_filename("it2.erl"),
    "-module(it2).\n"
    "-export([f/0, h/1]).\n"
    "-define(F, g).\n"
    "f()-> ?F().\n"
    "g()-> g().\n"
    "h(0)-> 1;\n"
    "h(X)-> h(X-1).\n"}.

iteration_setup() ->
    [add_file(iteration_testfile1()),add_file(iteration_testfile2())].

iteration_tests() ->
    [
        ?_assertEqual(
            lists:sort(
                [{chain,
                    [{nopos,"it1:f/0"},{nopos,"it2:h/1"},{nopos,"it2:h/1"}],
                    "*\n"},
                 {chain,
                    [{nopos,"it2:f/0"},{nopos,"it2:g/0"},{nopos,"it2:g/0"}],
                    "*\n"}]),

            lists:sort( refusr_sq:run( [{output, other}],[],
                "mods[name in |it1, it2|].funs[name==f].(calls)+") )
            ),

        ?_assertEqual(
            run_nodes("mods.funs.{calls}1"),

            run_nodes("mods.funs.calls")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.{calls}2"),

            run_nodes("mods.funs.calls.calls")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.{calls}3"),

            run_nodes("mods.funs.calls.calls.calls")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.{calls}4"),

            run_nodes("mods.funs.calls.calls.calls.calls")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.{calls.calls}2"),

            run_nodes("mods.funs.calls.calls.calls.calls")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.exprs.(sub)1"),

            run_nodes("mods.funs(.exprs U .exprs.sub)")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.exprs.(sub)2"),

            run_nodes("mods.funs(.exprs U .exprs(.sub U .sub.sub))")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.exprs.(sub)3"),

            run_nodes("mods.funs(.exprs U .exprs(.sub U .sub(.sub U .sub.sub)))")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.exprs.(sub)4"),

            run_nodes("mods.funs(.exprs U .exprs(.sub U .sub(.sub U .sub(.sub U .sub.sub))))")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.exprs.(sub.sub)2"),

            run_nodes("mods.funs(.exprs U .exprs(.sub.sub U .sub.sub.sub.sub))")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.exprs.(sub)+"),

            run_nodes("mods.funs(.exprs U .exprs.(sub)+)")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.exprs.(sub)+"),

            run_nodes("mods.funs(.exprs U .exprs.sub.(sub)+)")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.exprs.(sub.sub)+"),

            run_nodes("mods.funs(.exprs U .exprs.sub.sub.(sub.sub)+)")
            ),

        ?_assert(
            ordsets:is_subset(
                run_nodes("mods.funs.(calls)+.calls"),
                run_nodes("mods.funs.(calls)+"))
            ),

        ?_assertEqual(
            run_nodes("mods.funs.(calls)+"),

            run_nodes("mods.funs.(calls)+.(calls)+")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.(calls)+"),

            run_nodes("mods.funs.(calls)+.(calls)+.(calls)+")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.(calls)+"),

            run_nodes("mods.funs.(exprs.(sub)+.funs)+")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.(calls)+.body"),

            run_nodes("mods.funs.(exprs.(sub)+.funs)+.body")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.(calls)2"),

            run_nodes("mods.funs.(exprs.(sub)+.funs)2")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.{calls}2"),

            run_nodes("mods.funs.{exprs.(sub)+.funs}2")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.(calls[name==f])+"),

            run_nodes("mods.funs.(exprs.(sub)+.funs[name==f])+")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.(calls[name==f])+"),

            run_nodes("mods(.funs U .funs.calls[name==f].(calls[name==f])+)")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.{calls[name==f]}2"),

            run_nodes("mods.funs.calls[name==f].calls[name==f]")
            ),

        ?_assertEqual(
            run_nodes("mods.funs"),

            run_nodes("mods.funs.(calls[false])+")
            ),

        ?_assertEqual(
            [],

            run_nodes("mods.funs[false].(calls)+")
            ),

        ?_assertEqual(
            [],

            run_nodes("mods.funs.{calls[false]}1")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.{calls}4"),

            run_nodes("mods.funs.{calls}2.{calls}2")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.(calls)4"),

            run_nodes("mods.funs.(calls)2.(calls)2")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.(calls)+"),

            run_nodes("mods.funs.(calls)+.(calls)2")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.vars"),

            run_nodes("mods.funs.exprs.(sub)+.vars")
            ),

        ?_assertEqual(
            run_nodes("mods.funs[name==f]"),

            run_nodes("mods.funs.(calls)+[name==f]")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.calls.calls[name==f]"),

            run_nodes("mods.funs.{calls}2[name==f]")
            ),

        ?_assertEqual(
            run_nodes("mods.funs[name==f].exprs"),

            run_nodes("mods.funs.(calls)+[name==f].exprs")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.calls.calls[name==f].exprs"),

            run_nodes("mods.funs.{calls}2[name==f].exprs")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.exprs"),

            run_nodes("mods.funs.exprs.(sub)+.top")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.name"),

            run_nodes("mods.funs.(calls)+.name")
            ),

        ?_assertEqual(
            [],

            run_nodes("mods.funs.{[false]}1")
            ),

        ?_assertEqual(
            run_nodes("mods.funs"),

            run_nodes("mods.funs.([false])+")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.vars"),

            run_nodes("mods.funs.([false])+.vars")
            ),

        ?_assertEqual(
            [],

            run_nodes("mods.funs.{[false]}1.vars")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.exprs.(sub.sub)+"),

            run_nodes("mods.funs.exprs.(.{sub}2)+")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.exprs.sub.sub.sub.sub"),

            run_nodes("mods.funs.exprs.{.{sub}2}2")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.exprs.(sub.sub)+.reach"),

            run_nodes("mods.funs.exprs.(.{sub}2)+.reach")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.exprs.sub.sub.sub.sub.reach"),

            run_nodes("mods.funs.exprs.{.{sub}2}2.reach")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.exprs.(sub)+"),

            run_nodes("mods.funs.exprs.{.(sub)+}2")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.exprs.(sub)+.reach"),

            run_nodes("mods.funs.exprs.{.(sub)+}2.reach")
            ),

        ?_assertEqual(
            run_nodes("mods.funs.exprs(.(sub)+.reach U .(sub)+.origin)"),

            run_nodes("mods.funs.exprs.(sub)+(.reach U .origin)")
            ),

        ?_assertEqual(
            run_nodes("mods.funs(.{calls}2.calls U .{calls}2.called_by)"),

            run_nodes("mods.funs.{calls}2(.calls U .called_by)")
            ),

        ?_assertEqual(
            run_nodes("mods.funs(.{calls}2.calls U .{calls}2.called_by).vars"),

            run_nodes("mods.funs.{calls}2(.calls U .called_by).vars")
            ),

        ?_assertEqual(
            run_nodes("mods.funs[name==f].(exprs.(sub)+.funs)2"),

            run_nodes("mods.funs[name==f].{exprs.(sub)+(.funs U .fundef)}2")
            )
        ].

iteration_test_() ->
    {setup,
        fun iteration_setup/0,
        fun cleanup/1,
        {timeout, 30, iteration_tests()}
    }.


group_testfile1() ->
    {module, ?MISC:canonical_filename("m5.erl"),
    "-module(m5).\n"
    "-export([f/1]).\n"
    "f(Var)-> [Var, m6:h()].\n"}.
    
group_testfile2() ->
    {module, ?MISC:canonical_filename("m6.erl"),
    "-module(m6).\n"
    "-export([h/0]).\n"
    "h()-> ok.\n"
    "g(stop)-> ok;\n"
    "g(X)-> g(X-1).\n"}.

group_setup() ->
    [add_file(group_testfile1()),add_file(group_testfile2())].

group_test_() ->
    {setup,
     fun group_setup/0,
     fun cleanup/1,
     group_tests()}.

group_tests()->
    [
     ?_assertEqual(
	[{group_by,{nopos,"m5.erl"},list,[{nopos,"m5:f/1"}]},
	 {group_by,{nopos,"m6.erl"},list,[{nopos,"m6:h/0"},{nopos,"m6:g/1"}]}],

	refusr_sq:run( [{output, other}],[],
		       "mods[name in |m5,m6|].funs")
       ),
     ?_assertEqual(
	refusr_sq:run( [{output, other}],[],
		       "mods.funs"),

	refusr_sq:run( [{output, other}, {groupby, 1}],[],
		       "mods.funs")
       ),
     ?_assertEqual(
	refusr_sq:run( [{output, other}, {groupby, 1}],[],
		       "mods.funs"),

	refusr_sq:run( [{output, other}, {groupby, 1}],[],
		       "mods(.funs U .funs)")
       ),
     ?_assertEqual(
	[{group_by,{nopos,"m5:f/1"},eq,name,f},
	 {group_by,{nopos,"m6:h/0"},eq,name,h},
	 {group_by,{nopos,"m6:g/1"},eq,name,g}],

	refusr_sq:run( [{output, other}],[],
		       "mods[name in |m5,m6|].funs.name")
       ),
     ?_assertEqual(
	refusr_sq:run( [{output, other}],[],
		       "mods.funs.name"),

	refusr_sq:run( [{output, other}, {groupby, 2}],[],
		       "mods.funs.name")
       ),
     ?_assertEqual(
	[{group_by,{nopos,"m5:f/1"},list,[{nopos,"Var"}]},
	 {group_by,{nopos,"m6:g/1"},list,[{nopos,"X"}]}],

	refusr_sq:run( [{output, other}],[],
		       "mods[name in |m5,m6|].funs(.vars U .vars)")
       ),
     ?_assertEqual(
	refusr_sq:run( [{output, other}, {groupby, 2}],[],
		       "mods.funs(.vars U .vars)"),

	refusr_sq:run( [{output, other}],[],
		       "mods.funs(.vars U .vars)")
       ),
     ?_assertEqual(
	[{group_by,{nopos,"m5.erl"},list,[{nopos,"Var"}]},
	 {group_by,{nopos,"m6.erl"},list,[{nopos,"X"}]}],

	refusr_sq:run( [{output, other}, {groupby, 1}],[],
		       "mods[name in |m5,m6|].funs(.vars U .vars)")
       ),
     ?_assertEqual(
	[{group_by,{nopos,"m5:f/1"},eq,".vars.name","Var"},
	 {group_by,{nopos,"m6:g/1"},eq,".vars.name","X"}],

	refusr_sq:run( [{output, other}, {groupby, 2}],[],
		       "mods[name in |m5,m6|].funs.vars(.name U .name)")
       ),
     ?_assertEqual(
	[{group_by,{nopos,"m5.erl"},list,[{nopos,"m5:f/1"}]}],

	refusr_sq:run( [{output, other}],[],
		       "mods[name in |m5,m6|].funs[name==f]")
       ),
     ?_assertEqual(
	[{group_by,{nopos,"m5:f/1"},list,[{nopos,"Var"}]},
	 {group_by,{nopos,"m6:g/1"},list,[{nopos,"X"}]}],

	refusr_sq:run( [{output, other}],[],
		       "mods[name in |m5,m6|].funs.vars")
       ),
     ?_assertEqual(
	refusr_sq:run( [{output, other}],[],
		       "mods.funs.vars"),

	refusr_sq:run( [{output, other}, {groupby, 2}],[],
		       "mods.funs.vars")
       ),
     ?_assertEqual(
	[{group_by,{nopos,"m5.erl"},list,[{nopos,"Var"}]},
	 {group_by,{nopos,"m6.erl"},list,[{nopos,"X"}]}],

	refusr_sq:run( [{output, other},{groupby, 1}],[],
		       "mods[name in |m5,m6|].funs.vars")
       ),
     ?_assertEqual(
	[{group_by,{nopos,"m5.erl"},eq,name,m5},
	 {group_by,{nopos,"m6.erl"},eq,name,m6}],

	refusr_sq:run( [{output, other},{groupby, 1}],[],
		       "mods[name in |m5,m6|].name")
       ),
     ?_assertEqual(
	[{group_by,{nopos,"m5.erl"},propertylist,".funs.name",[f]},
	 {group_by,{nopos,"m6.erl"},propertylist,".funs.name",[g,h]}],

	refusr_sq:run( [{output, other},{groupby, 1}],[],
		       "mods[name in |m5,m6|].funs.name")
       ),
     ?_assertEqual(
	[{chain,[{nopos,"m6:h/0"}],"\n"},
	 {chain,[{nopos,"m5:f/1"},{nopos,"m6:h/0"}],"\n"},
	 {chain,[{nopos,"m6:g/1"},{nopos,"m6:g/1"}],"*\n"}],

	refusr_sq:run( [{output, other},{groupby, 2}],[],
		       "mods[name in |m5,m6|].funs.(calls)+")
       ),
     ?_assertEqual(
	[{list,[]}],

	refusr_sq:run( [{output, other},{groupby, 2}],[],
		       "mods[name == 0].funs.vars")
       ),
     ?_assertEqual(
	[{list,[]}],

	refusr_sq:run( [{output, other},{groupby, 1}],[],
		       "mods[name == 0].funs.vars")
       ),
     ?_assertEqual(
        [{group_by,{nopos,"m6:g/1"},list,[{nopos,"m6:g/1"}]}],

	refusr_sq:run( [{output, other}],[],
		       "mods[name in |m,1,m6|].funs.{calls}2[name==g]")
       ),
     ?_assertEqual(
        [{group_by,{nopos,"m6:g/1"},list,[{nopos,"m6:g/1"}]}],

	refusr_sq:run( [{output, other},{groupby, 2}],[],
		       "mods[name in |m,1,m6|].funs.{calls}2[name==g]")
       ),
     ?_assertEqual(
	[{group_by,{nopos,"m6:g/1"},list,[{nopos,"m6:g/1"}]}],

	refusr_sq:run( [{output, other}],[],
		       "mods[name in |m5,m6|].funs.(calls)+[name==g]")
       ),
     ?_assertEqual(
	[{group_by,{nopos,"m6:g/1"},list,[{nopos,"m6:g/1"}]}],

	refusr_sq:run( [{output, other},{groupby, 2}],[],
		       "mods[name in |m5,m6|].funs.(calls)+[name==g]")
       ),
     ?_assertEqual(
	[{group_by,{nopos,"m5.erl"},eq,".funs.vars.name","Var"},
	 {group_by,{nopos,"m6.erl"},eq,".funs.vars.name","X"}],

	refusr_sq:run( [{output, other},{groupby, 1}],[],
		       "mods[name in |m5,m6|].funs.vars.name")
       ),
     ?_assertEqual(
	[{group_by,{nopos,"m5:f/1"},eq,".vars.name","Var"},
	 {group_by,{nopos,"m6:g/1"},eq,".vars.name","X"}],

	refusr_sq:run( [{output, other},{groupby, 2}],[],
		       "mods[name in |m5,m6|].funs.vars.name")
       ),
     ?_assertEqual(
	[{group_by,{nopos,"m5.erl"},eq,".funs.vars.name","Var"},
	 {group_by,{nopos,"m6.erl"},eq,".funs.vars.name","X"}],

	refusr_sq:run( [{output, other},{groupby, 1}],[],
		       "mods[name in |m5,m6|](.funs U .funs).vars.name")
       ),
     ?_assertEqual(
	[{group_by,{nopos,"m5:f/1"},eq,".vars.name","Var"},
	 {group_by,{nopos,"m6:g/1"},eq,".vars.name","X"}],

	refusr_sq:run( [{output, other},{groupby, 2}],[],
		       "mods[name in |m5,m6|](.funs U .funs).vars.name")
       ),
     ?_assertEqual(
	[{group_by,{nopos,"Var"},eq,name,"Var"},
	 {group_by,{nopos,"X"},eq,name,"X"}],

	refusr_sq:run( [{output, other},{groupby, 3}],[],
		       "mods[name in |m5,m6|](.funs U .funs).vars.name")
       ),
     ?_assertEqual(
	[{group_by,{nopos,"m6.erl"},eq,".funs.{calls}2.vars.name","X"}],

	refusr_sq:run( [{output, other},{groupby, 1}],[],
		       "mods[name in |m5,m6|].funs.{calls}2.vars.name")
       ),
     ?_assertEqual(
	[{group_by,{nopos,"m6:g/1"},eq,".{calls}2.vars.name","X"}],

	refusr_sq:run( [{output, other},{groupby, 2}],[],
		       "mods[name in |m5,m6|].funs.{calls}2.vars.name")
       ),
     ?_assertEqual(
	[{group_by,{nopos,"m6:g/1"},eq,".vars.name","X"}],

	refusr_sq:run( [{output, other},{groupby, 3}],[],
		       "mods[name in |m5,m6|].funs.{calls}2.vars.name")
       ),
     ?_assertEqual(
	[{group_by,{nopos,"X"},eq,name,"X"}],

	refusr_sq:run( [{output, other},{groupby, 4}],[],
		       "mods[name in |m5,m6|].funs.{calls}2.vars.name")
       ),
     ?_assertEqual(
	[{group_by,{nopos,"Var"},eq,name,"Var"},
	 {group_by,{nopos,"X"},eq,name,"X"}],

	refusr_sq:run( [{output, other},{groupby, 4}],[],
		       "mods[name in |m5,m6|].funs.(calls)+.vars.name")
       ),
     ?_assertEqual(
	lists:sort(
	  [{group_by,{nopos,"m5:f/1"},eq,".vars.name","Var"},
	   {group_by,{nopos,"m6:g/1"},eq,".vars.name","X"}]),
	
	lists:sort(
	  refusr_sq:run( [{output, other},{groupby, 3}],[],
			 "mods[name in |m5,m6|].funs.(calls)+.vars.name"))
       ),
     ?_assertEqual(
	[{group_by,{nopos,"m5:f/1"},eq,".(calls)+.vars.name","Var"},
	 {group_by,{nopos,"m6:g/1"},eq,".(calls)+.vars.name","X"}],

	refusr_sq:run( [{output, other},{groupby, 2}],[],
		       "mods[name in |m5,m6|].funs.(calls)+.vars.name")
       ),
     ?_assertEqual(
	[{group_by,{nopos,"m5.erl"},eq,".funs.(calls)+.vars.name","Var"},
	 {group_by,{nopos,"m6.erl"},eq,".funs.(calls)+.vars.name","X"}],

	refusr_sq:run( [{output, other},{groupby, 1}],[],
		       "mods[name in |m5,m6|].funs.(calls)+.vars.name")
       ),
     ?_assertEqual(
	[{group_by,{nopos,"m5.erl"},list,[{nopos,"Var"}]}],

	refusr_sq:run( [{output, other},{groupby, 1}],[],
		       "mods[name in |m5,m6|].funs[name==f].vars")
       )].

add_file({Type, Name, Text}) ->
    File = ?ESG:create(#file{path=Name, type=Type, eol={lf,eol},lastmod=now()}),
    ?ESG:insert(?ESG:root(), file, File),
    ?Graph:mklink(File, incl, File),
    ?FileMan:add_text(File, last, Text),
    ?ESG:finalize(),
    File.

%% Initial selectors
init_sel_testfile() ->
    {module, ?MISC:canonical_filename("init_sel_test.erl"),
     "-module(init_sel_test).\n"
     "-record(rec, {f1=1, f2}).\n"
     "-define(F,\n f).\n"
     "f(X) -> io:format(\"alma\"),\n"
     "        X.\n"
     "g(Rec) -> ?F(h(Rec)).\n"
     "h(Rec) -> Rec#rec.f2,\n"
     "          #rec.f1.\n"}.

init_sel_test_()->
    {setup,
     fun() -> add_file(init_sel_testfile()) end,
     fun ?FileMan:drop_file/1,
     init_sel_tests()}.


init_sel_tests() ->
    Filename = ?MISC:canonical_filename("init_sel_test.erl"),
    [?_assertEqual([{list,[{nopos,"init_sel_test.erl"},{nopos,"io"}]}],
                   raw("'mods'")),
     ?_assertEqual(refusr_sq_lib:init_sel(
		     [{file, Filename}, {position, 34}], '@def'),
		   refusr_sq_lib:init_sel(
		     [{file, Filename}, {position, 34},
		      {ask_missing, false}], '@rec')),
     ?_assertEqual(refusr_sq_lib:init_sel(
		     [{file, Filename}, {position, 40}], '@def'),
		   refusr_sq_lib:init_sel(
		     [{file, Filename}, {position, 40},
		      {ask_missing, false}], '@field')),
     ?_assertEqual(refusr_sq_lib:init_sel(
		     [{file, Filename}, {position, 59}], '@def'),
		   refusr_sq_lib:init_sel(
		     [{file, Filename}, {position, 59},
		      {ask_missing, false}], '@macro')),
     ?_assertEqual(refusr_sq_lib:init_sel(
		     [{file, Filename}, {position, 79}], '@def'),
		   refusr_sq_lib:init_sel(
		     [{file, Filename}, {position, 79},
		      {ask_missing, false}], '@fun')),
     ?_assertEqual(refusr_sq_lib:init_sel(
		     [{file, Filename}, {position, 24}], '@def'),
		   {none, []}),
     ?_assertEqual(refusr_sq_lib:init_sel(
		     [{file, Filename}, {position, 161}], '@def'),
		   refusr_sq_lib:init_sel(
		     [{file, Filename}, {position, 161},
		      {ask_missing, false}], '@rec')),
     ?_assertEqual(refusr_sq_lib:init_sel(
		     [{file, Filename}, {position, 114}], '@def'),
		   refusr_sq_lib:init_sel(
		     [{file, Filename}, {position, 114},
		      {ask_missing, false}], '@macro')),
     ?_assertEqual(refusr_sq_lib:init_sel(
		     [{file, Filename}, {position, 139}], '@def'),
		   refusr_sq_lib:init_sel(
		     [{file, Filename}, {position, 139},
		      {ask_missing, false}], '@var')),
     ?_assertEqual(refusr_sq_lib:init_sel(
		     [{file, Filename}, {position, 74}], '@def'),
		   refusr_sq_lib:init_sel(
		     [{file, Filename}, {position, 74},
		      {ask_missing, false}], '@mod'))]. %,
%    io:format("init_sel_tests vege").
%      ?assertEqual(refusr_sq_lib:init_sel(
%                     [{file, Filename}, {position, 116}], '@def'),
%                   refusr_sq_lib:init_sel(
%                     [{file, Filename}, {position, 116},
%                      {ask_missing, false}], '@fun')),
%      ?assertEqual(refusr_sq_lib:init_sel(
%                     [{file, Filename}, {position, 144}], '@def'),
%                   refusr_sq_lib:init_sel(
%                     [{file, Filename}, {position, 144},
%                      {ask_missing, false}], '@field')),
%      ?assertEqual(refusr_sq_lib:init_sel(
%                     [{file, Filename}, {position, 65}], '@def'),
%                   refusr_sq_lib:init_sel(
%                     [{file, Filename}, {position, 65},
%                      {ask_missing, false}], '@fun')).


file_testfile() ->
    {module, ?MISC:canonical_filename("file_test.erl"),
     "-module(file_test).\n"
     "-record(rec, {f1=1, f2}).\n"
     "-define(F,f).\n"
     "f(X) -> io:format(\"alma\"),\n"
     "        X.\n"
     "g(Rec) -> ?F(h(Rec)).\n"
     "h(Rec) -> Rec#rec.f2,\n"
     "          #rec.f1.\n"}.


file_test_() ->
    {setup,
     fun() -> add_file(file_testfile()) end,
     fun(File) -> ?FileMan:drop_file(File) end,
     file_tests()}.

file_tests() ->
    Filename = ?MISC:canonical_filename("file_test.erl"),
    [?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file"),
		  [{list,
		    [{{Filename,1,1},"file_test.erl"}]}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.funs"),
		  [{group_by,{nopos,"file_test.erl"},
		    list,
		    [{{Filename,61,97},"file_test:f/1"},
		     {{Filename,99,119},"file_test:g/1"},
		     {{Filename,121,160},"file_test:h/1"}]}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.recs"),
		  [{group_by,{nopos,"file_test.erl"},
		    list,[{{Filename,21,45},"rec"}]}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.macros"),
		  [{group_by,{nopos,"file_test.erl"},
		    list,[{{Filename,47,59},"F"}]}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.includes"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    list,
		    [{{Filename,1,1},"file_test.erl"}]}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.included_by"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    list,
		    [{{Filename,1,1},"file_test.erl"}]}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.imports"),
		  [{list, []}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.exports"),
		  [{list, []}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.module"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,module,true}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.header"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,header,false}]),
						%atom vs string???
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.name"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,name,file_test}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.dir"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,dir,filename:dirname(Filename)}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.path"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,path,Filename}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.mod_sum"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,mod_sum,239}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.loc"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,loc,8}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.choc"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,choc,161}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.num_of_fun"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,num_of_fun,3}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.num_of_macros"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,num_of_macros,1}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.num_of_records"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,num_of_records,1}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.included_files"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,included_files,0}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.imported_modules"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,imported_modules,0}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.number_of_funpath"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,number_of_funpath,3}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.fun_calls_in"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,fun_calls_in,0}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.fun_calls_out"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,fun_calls_out,1}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.cohesion"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,cohesion,2}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.otp"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,otp,0}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.max_app_depth"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,max_app_depth,2}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.max_depth_of_calling"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,max_depth_of_calling,2}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.min_depth_of_calling"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,min_depth_of_calling,1}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.max_depth_of_cases"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,max_depth_of_cases,0}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.num_of_funclauses"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,num_of_funclauses,3}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.branches_of_recursion"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,branches_of_recursion,0}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.mccabe"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,mccabe,3}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.num_of_funexpr"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,num_of_funexpr,0}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.number_of_messpass"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,number_of_messpass,0}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.fun_return_points"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,fun_return_points,3}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.max_length_of_line"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,max_length_of_line,26}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.avg_length_of_line"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,avg_length_of_line,19}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.no_space_after_comma"),
		  [{group_by,
		    {{Filename,1,1},"file_test.erl"},
		    eq,no_space_after_comma,3}])].


fun_testfile() ->
    {module, ?MISC:canonical_filename("fun_test.erl"),
     "-module(fun_test).\n"
     "-record(rec, {f1=1, f2}).\n"
     "-define(F,f).\n"
     "f(X) -> io:format(\"alma\"),\n"
     "        X.\n"
     "g(Rec) -> ?F(h(Rec)).\n"
     "h(Rec) -> Rec#rec.f2,\n"
     "          #rec.f1.\n"}.

fun_test_()->
    {setup,
     fun() -> add_file(fun_testfile()) end,
     fun(File) -> ?FileMan:drop_file(File) end,
     fun_tests()}.

fun_tests() ->
    Filename = ?MISC:canonical_filename("fun_test.erl"),
    [?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.funs.refs"),
		  [{group_by,{nopos,"fun_test:f/1"},
		    list,[{{Filename,108,117},"?F(h(Rec))"}]},
		   {group_by,{nopos,"fun_test:h/1"},
		    list,[{{Filename,111,116},"h(Rec)"}]}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.funs.calls"),
		  [{group_by,{{Filename,60,96},"fun_test:f/1"},
		    list,[{nopos,"io:format/1"}]},
		   {group_by,{nopos,"fun_test:g/1"},
		    list,
		    [{{Filename,60,96},"fun_test:f/1"},
		     {{Filename,120,159},"fun_test:h/1"}]}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.funs.called_by"),
		  [{group_by,{nopos,"fun_test:f/1"},
		    list,[{{Filename,98,118},"fun_test:g/1"}]},
		   {group_by,{nopos,"fun_test:h/1"},
		    list,[{{Filename,98,118},"fun_test:g/1"}]}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.funs.args"),
		  [{group_by,{nopos,"fun_test:f/1"},
		    list,[{{Filename,62,62},"X"}]},
		   {group_by,{nopos,"fun_test:g/1"},
		    list,[{{Filename,100,102},"Rec"}]},
		   {group_by,{nopos,"fun_test:h/1"},
		    list,[{{Filename,122,124},"Rec"}]}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.funs[name==f].body"),
		  [{group_by,{nopos,"fun_test:f/1"},
		    list,
		    [{{Filename,68,84},"io:format(\"alma\")"},
		     {{Filename,95,95},"X"}]}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.funs[name==f].exprs"),
		  [{group_by,{nopos,"fun_test:f/1"},
		    list,
		    [{{Filename,62,62},"X"},
		     {{Filename,68,84},"io:format(\"alma\")"},
		     {{Filename,95,95},"X"}]}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.funs[name==f].vars"),
		  [{group_by,{nopos,"fun_test:f/1"},
		    list,
		    [{{Filename,62,62},"X"}]}]),
     ?_assertEqual(lists:usort(
		    refusr_sq:run(
		      [{positions, scalar},{output, other}],
		      [{file, Filename}, {position, 34}],
		      "@file.funs.calls.exported")),
		  lists:usort([{group_by,{nopos,"io:format/1"},
				eq,exported,true},
			       {group_by,{{Filename,60,96},"fun_test:f/1"},
				eq,exported,false},
			       {group_by,{{Filename,120,159},"fun_test:h/1"},
				eq,exported,false}])),
     ?_assertEqual(lists:usort(
		    refusr_sq:run(
		      [{positions, scalar},{output, other}],
		      [{file, Filename}, {position, 34}],
		      "@file.funs.calls.name")),
		  lists:usort([{group_by,{nopos,"io:format/1"},
				eq,name,format},
			       {group_by,{{Filename,60,96},"fun_test:f/1"},
				eq,name,f},
			       {group_by,{{Filename,120,159},"fun_test:h/1"},
				eq,name,h}])),
     ?_assertEqual(lists:usort(
		    refusr_sq:run(
		      [{positions, scalar},{output, other}],
		      [{file, Filename}, {position, 34}],
		      "@file.funs.calls.arity")),
		  lists:usort([{group_by,{nopos,"io:format/1"},
				eq,arity,1},
			       {group_by,{{Filename,60,96},"fun_test:f/1"},
				eq,arity,1},
			       {group_by,{{Filename,120,159},"fun_test:h/1"},
				eq,arity,1}])),
     ?_assertEqual(lists:usort(
		    refusr_sq:run(
		      [{positions, scalar},{output, other}],
		      [{file, Filename}, {position, 34}],
		      "@file.funs.calls.bif")),
		  lists:usort([{group_by,{nopos,"io:format/1"},
				eq,bif,false},
			       {group_by,{{Filename,60,96},"fun_test:f/1"},
				eq,bif,false},
			       {group_by,{{Filename,120,159},"fun_test:h/1"},
				eq,bif,false}])),
     ?_assertEqual(lists:usort(
		    refusr_sq:run(
		      [{positions, scalar},{output, other}],
		      [{file, Filename}, {position, 34}],
		      "@file.funs.calls.pure")),
		  lists:usort([{group_by,{nopos,"io:format/1"},
				eq,pure,false},
			       {group_by,{{Filename,60,96},"fun_test:f/1"},
				eq,pure,false},
			       {group_by,{{Filename,120,159},"fun_test:h/1"},
				eq,pure,true}])),
     ?_assertEqual(lists:usort(
		    refusr_sq:run(
		      [{positions, scalar},{output, other}],
		      [{file, Filename}, {position, 34}],
		      "@file.funs.calls.defined")),
		  lists:usort([{group_by,{nopos,"io:format/1"},
				eq,defined,false},
			       {group_by,{{Filename,60,96},"fun_test:f/1"},
				eq,defined,true},
			       {group_by,{{Filename,120,159},"fun_test:h/1"},
				eq,defined,true}])),
     ?_assertEqual(lists:usort(
		    refusr_sq:run(
		      [{positions, scalar},{output, other}],
		      [{file, Filename}, {position, 34}],
		      "@file.funs.calls.dirty")),
		  lists:usort(
		    [{group_by,{nopos,"io:format/1"},
		      eq,dirty,true},
		     {group_by,{{Filename,60,96},"fun_test:f/1"},
		      eq,dirty,true},
		     {group_by,{{Filename,120,159},"fun_test:h/1"},
		      eq,dirty,false}])),
     ?_assertEqual(lists:usort(
		    refusr_sq:run(
		      [{positions, scalar},{output, other}],
		      [{file, Filename}, {position, 34}],
		      "@file.funs.calls.mod")),
		  lists:usort([{group_by,{nopos,"io:format/1"},
				eq,mod,io},
			       {group_by,{{Filename,60,96},"fun_test:f/1"},
				eq,mod,fun_test},
			       {group_by,{{Filename,120,159},"fun_test:h/1"},
				eq,mod,fun_test}])),
     ?_assertEqual(lists:usort(
		    refusr_sq:run(
		      [{positions, scalar},{output, other}],
		      [{file, Filename}, {position, 34}],
		      "@file.funs.calls.exported")),
		  lists:usort([{group_by,{nopos,"io:format/1"},
				eq,exported,true},
			       {group_by,{{Filename,60,96},"fun_test:f/1"},
				eq,exported,false},
			       {group_by,{{Filename,120,159},"fun_test:h/1"},
				eq,exported,false}])),
     ?_assertEqual(lists:usort(
		    refusr_sq:run(
		      [{positions, scalar},{output, other}],
		      [{file, Filename}, {position, 34}],
		      "@file.funs.calls.loc")),
		  lists:usort([{group_by,{nopos,"io:format/1"},
				eq,loc,0},
			       {group_by,{{Filename,60,96},"fun_test:f/1"},
				eq,loc,2},
			       {group_by,{{Filename,120,159},"fun_test:h/1"},
				eq,loc,2}])),
     ?_assertEqual(lists:usort(
		    refusr_sq:run(
		      [{positions, scalar},{output, other}],
		      [{file, Filename}, {position, 34}],
		      "@file.funs.calls.choc")),
		  lists:usort([{group_by,{nopos,"io:format/1"},
				eq,choc,0},
			       {group_by,{{Filename,60,96},"fun_test:f/1"},
				eq,choc,36},
			       {group_by,{{Filename,120,159},"fun_test:h/1"},
				eq,choc,39}])),
     ?_assertEqual(lists:usort(
		    refusr_sq:run(
		      [{positions, scalar},{output, other}],
		      [{file, Filename}, {position, 34}],
		      "@file.funs.calls.max_application_depth")),
		  lists:usort([{group_by,{nopos,"io:format/1"},
				eq,max_application_depth,0},
			       {group_by,{{Filename,60,96},"fun_test:f/1"},
				eq,max_application_depth,1},
			       {group_by,{{Filename,120,159},"fun_test:h/1"},
				eq,max_application_depth,0}])),
     ?_assertEqual(lists:usort(
		    refusr_sq:run(
		      [{positions, scalar},{output, other}],
		      [{file, Filename}, {position, 34}],
		      "@file.funs.calls.max_depth_of_calling")),
		  lists:usort([{group_by,{nopos,"io:format/1"},
				eq,max_depth_of_calling,0},
			       {group_by,{{Filename,60,96},"fun_test:f/1"},
				eq,max_depth_of_calling,1},
			       {group_by,{{Filename,120,159},"fun_test:h/1"},
				eq,max_depth_of_calling,0}])),
     ?_assertEqual(lists:usort(
		    refusr_sq:run(
		      [{positions, scalar},{output, other}],
		      [{file, Filename}, {position, 34}],
		      "@file.funs.calls.max_depth_of_cases")),
		  lists:usort([{group_by,{nopos,"io:format/1"},
				eq,max_depth_of_cases,0},
			       {group_by,{{Filename,60,96},"fun_test:f/1"},
				eq,max_depth_of_cases,0},
			       {group_by,{{Filename,120,159},"fun_test:h/1"},
				eq,max_depth_of_cases,0}])),
     ?_assertEqual(lists:usort(
		    refusr_sq:run(
		      [{positions, scalar},{output, other}],
		      [{file, Filename}, {position, 34}],
		      "@file.funs.calls.number_of_funclauses")),
		  lists:usort([{group_by,{nopos,"io:format/1"},
				eq,number_of_funclauses,0},
			       {group_by,{{Filename,60,96},"fun_test:f/1"},
				eq,number_of_funclauses,1},
			       {group_by,{{Filename,120,159},"fun_test:h/1"},
				eq,number_of_funclauses,1}])),
     ?_assertEqual(lists:usort(
		    refusr_sq:run(
		      [{positions, scalar},{output, other}],
		      [{file, Filename}, {position, 34}],
		      "@file.funs.calls.branches_of_recursion")),
		  lists:usort([{group_by,{nopos,"io:format/1"},
				eq,branches_of_recursion,0},
			       {group_by,{{Filename,60,96},"fun_test:f/1"},
				eq,branches_of_recursion,0},
			       {group_by,{{Filename,120,159},"fun_test:h/1"},
				eq,branches_of_recursion,0}])),
     ?_assertEqual(lists:usort(
		    refusr_sq:run(
		      [{positions, scalar},{output, other}],
		      [{file, Filename}, {position, 34}],
		      "@file.funs.calls.mccabe")),
		  lists:usort([{group_by,{nopos,"io:format/1"},
				eq,mccabe,0},
			       {group_by,{{Filename,60,96},"fun_test:f/1"},
				eq,mccabe,1},
			       {group_by,{{Filename,120,159},"fun_test:h/1"},
				eq,mccabe,1}])),
     ?_assertEqual(lists:usort(
		    refusr_sq:run(
		      [{positions, scalar},{output, other}],
		      [{file, Filename}, {position, 34}],
		      "@file.funs.calls.calls_for_function")),
		  lists:usort([{group_by,{nopos,"io:format/1"},
				eq,calls_for_function,0},
			       {group_by,{{Filename,60,96},"fun_test:f/1"},
				eq,calls_for_function,1},
			       {group_by,{{Filename,120,159},"fun_test:h/1"},
				eq,calls_for_function,1}])),
     ?_assertEqual(lists:usort(
		    refusr_sq:run(
		      [{positions, scalar},{output, other}],
		      [{file, Filename}, {position, 34}],
		      "@file.funs.calls.calls_from_function")),
		  lists:usort([{group_by,{nopos,"io:format/1"},
				eq,calls_from_function,0},
			       {group_by,{{Filename,60,96},"fun_test:f/1"},
				eq,calls_from_function,1},
			       {group_by,{{Filename,120,159},"fun_test:h/1"},
				eq,calls_from_function,0}])),
     ?_assertEqual(lists:usort(
		    refusr_sq:run(
		      [{positions, scalar},{output, other}],
		      [{file, Filename}, {position, 34}],
		      "@file.funs.calls.number_of_funexpr")),
		  lists:usort([{group_by,{nopos,"io:format/1"},
				eq,number_of_funexpr,0},
			       {group_by,{{Filename,60,96},"fun_test:f/1"},
				eq,number_of_funexpr,0},
			       {group_by,{{Filename,120,159},"fun_test:h/1"},
				eq,number_of_funexpr,0}])),
     ?_assertEqual(lists:usort(
		    refusr_sq:run(
		      [{positions, scalar},{output, other}],
		      [{file, Filename}, {position, 34}],
		      "@file.funs.calls.number_of_messpass")),
		  lists:usort([{group_by,{nopos,"io:format/1"},
				eq,number_of_messpass,0},
			       {group_by,{{Filename,60,96},"fun_test:f/1"},
				eq,number_of_messpass,0},
			       {group_by,{{Filename,120,159},"fun_test:h/1"},
				eq,number_of_messpass,0}])),
     ?_assertEqual(lists:usort(
		    refusr_sq:run(
		      [{positions, scalar},{output, other}],
		      [{file, Filename}, {position, 34}],
		      "@file.funs.calls.fun_return_points")),
		  lists:usort([{group_by,{nopos,"io:format/1"},
				eq,fun_return_points,0},
			       {group_by,{{Filename,60,96},"fun_test:f/1"},
				eq,fun_return_points,1},
			       {group_by,{{Filename,120,159},"fun_test:h/1"},
				eq,fun_return_points,1}])),
     ?_assertEqual(lists:usort(
		    refusr_sq:run(
		      [{positions, scalar},{output, other}],
		      [{file, Filename}, {position, 34}],
		      "@file.funs.calls.max_length_of_line")),
		  lists:usort([{group_by,{nopos,"io:format/1"},
				eq,max_length_of_line,0},
			       {group_by,{{Filename,60,96},"fun_test:f/1"},
				eq,max_length_of_line,26},
			       {group_by,{{Filename,120,159},"fun_test:h/1"},
				eq,max_length_of_line,21}])),
     ?_assertEqual(lists:usort(
		    refusr_sq:run(
		      [{positions, scalar},{output, other}],
		      [{file, Filename}, {position, 34}],
		      "@file.funs.calls.average_length_of_line")),
		  lists:usort([{group_by,{nopos,"io:format/1"},
				eq,average_length_of_line,0},
			       {group_by,{{Filename,60,96},"fun_test:f/1"},
				eq,average_length_of_line,18},
			       {group_by,{{Filename,120,159},"fun_test:h/1"},
				eq,average_length_of_line,19}])),
     ?_assertEqual(lists:usort(
		    refusr_sq:run(
		      [{positions, scalar},{output, other}],
		      [{file, Filename}, {position, 34}],
		      "@file.funs.calls.no_space_after_comma")),
		  lists:usort([{group_by,{nopos,"io:format/1"},
				eq,no_space_after_comma,0},
			       {group_by,{{Filename,60,96},"fun_test:f/1"},
				eq,no_space_after_comma,0},
			       {group_by,{{Filename,120,159},"fun_test:h/1"},
				eq,no_space_after_comma,0}])),
     ?_assertEqual(lists:usort(
		    refusr_sq:run(
		      [{positions, scalar},{output, other}],
		      [{file, Filename}, {position, 34}],
		      "@file.funs.calls.is_tail_recursive")),
		  lists:usort([{group_by,{nopos,"io:format/1"},
				eq,is_tail_recursive,unknown},
			       {group_by,{{Filename,60,96},"fun_test:f/1"},
				eq,is_tail_recursive,non_rec},
			       {group_by,{{Filename,120,159},"fun_test:h/1"},
				eq,is_tail_recursive,non_rec}]))]. %,
%      ?assertEqual(lists:usort(
%                     refusr_sq:run(
%                       [{positions, scalar},{output, other}],
%                       [{file, Filename}, {position, 34}],
%                       "@file.funs.calls.fun_sum")),
%                   lists:usort([{group_by,{nopos,"io:format/1"},
%                                 eq,fun_sum,0},
%                                {group_by,{{Filename,60,96},"fun_test:f/1"},
%                                 eq,fun_sum,9},
%                                {group_by,{{Filename,120,159},"fun_test:h/1"},
%                                 eq,fun_sum,9}])).


var_testfile() ->
    {module, ?MISC:canonical_filename("var_test.erl"),
     "-module(var_test).\n"
     "-record(rec, {f1=1, f2}).\n"
     "-define(F,f).\n"
     "f(X) -> io:format(\"alma\"),\n"
     "        X.\n"
     "g(Rec) -> ?F(h(Rec)).\n"
     "h(Rec) -> Rec#rec.f2,\n"
     "          #rec.f1.\n"}.

var_test_()->
    {setup,
     fun() -> add_file(var_testfile()) end,
     fun(File) -> ?FileMan:drop_file(File) end,
     var_tests()}.

var_tests() ->
    Filename = ?MISC:canonical_filename("var_test.erl"),
    [?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.funs[name==h].vars"),
		  [{group_by,{nopos,"var_test:h/1"},
		    list,[{{Filename,122,124},"Rec"}]}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.funs[name==h].vars.refs"),
		  [{group_by,{nopos,"Rec"},
		    list,[{{Filename,130,132},"Rec"}]}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.funs[name==h].vars.bindings"),
		  [{group_by,{nopos,"Rec"},
		    list,[{{Filename,122,124},"Rec"}]}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.funs[name==h].vars.fundef"),
		  [{group_by,{nopos,"Rec"},
		    list,[{{Filename,120,159},"var_test:h/1"}]}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.funs[name==h].vars.name"),
		  [{group_by,{{Filename,122,124},"Rec"},
		    eq,name,"Rec"}])].

rec_testfile() ->
    {module, ?MISC:canonical_filename("rec_test.erl"),
     "-module(rec_test).\n"
     "-record(rec, {f1=1, f2}).\n"
     "-define(F,f).\n"
     "f(X) -> io:format(\"alma\"),\n"
     "        X.\n"
     "g(Rec) -> ?F(h(Rec)).\n"
     "h(Rec) -> Rec#rec.f2,\n"
     "          #rec.f1.\n"}.

rec_test_()->
    {setup,
     fun() -> add_file(rec_testfile()) end,
     fun(File) -> ?FileMan:drop_file(File) end,
     rec_tests()}.

rec_tests() ->
    Filename = ?MISC:canonical_filename("rec_test.erl"),
    [?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.recs"),
		  [{group_by,{nopos,"rec_test.erl"},
		    list,[{{Filename,20,44},"rec"}]}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.recs.refs"),
		  [{group_by,{nopos,"rec"},
		    list,
		    [{{Filename,130,139},"Rec#rec.f2"},
		     {{Filename,152,158},"#rec.f1"}]}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.recs.fields"),
		  [{group_by,{nopos,"rec"},
		    list,
		    [{{Filename,20,44},"f1"},
		     {{Filename,20,44},"f2"}]}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.recs.file"),
		  [{group_by,{nopos,"rec"},
		    list,[{{Filename,1,1},"rec_test.erl"}]}]),
     ?_assertEqual(refusr_sq:run(
		    [{positions, scalar},{output, other}],
		    [{file, Filename}, {position, 34}],
		    "@file.recs.name"),
		  [{group_by,{{Filename,20,44},"rec"},
		    eq,name,rec}])].

recfield_testfile() ->
    {module, ?MISC:canonical_filename("recfield_test.erl"),
     "-module(recfield_test).\n"
     "-record(rec, {f1=1, f2}).\n"
     "-define(F,f).\n"
     "f(X) -> io:format(\"alma\"),\n"
     "        X.\n"
     "g(Rec) -> ?F(h(Rec)).\n"
     "h(Rec) -> Rec#rec.f2,\n"
     "          #rec.f1.\n"}.

recfield_test_() ->
    {setup,
     fun() -> add_file(recfield_testfile()) end,
     fun(File) -> ?FileMan:drop_file(File) end,
     recfield_tests()}.

recfield_tests() ->
    Filename = ?MISC:canonical_filename("recfield_test.erl"),
    [?_assertEqual(refusr_sq:run(
		     [{positions, scalar},{output, other}],
		     [{file, Filename}, {position, 34}],
		     "@file.recs.fields.refs"),
		   [{group_by,{nopos,"f1"},
		     list,[{{Filename,162,163},"f1"}]},
		    {group_by,{nopos,"f2"},
		     list,[{{Filename,143,144},"f2"}]}]),
     ?_assertEqual(refusr_sq:run(
		     [{positions, scalar},{output, other}],
		     [{file, Filename}, {position, 34}],
		     "@file.recs.fields.rec"),
		   [{group_by,{nopos,"f1"},
		     list,[{{Filename,25,49},"rec"}]},
		    {group_by,{nopos,"f2"},
		     list,[{{Filename,25,49},"rec"}]}]),
     ?_assertEqual(refusr_sq:run(
		     [{positions, scalar},{output, other}],
		     [{file, Filename}, {position, 34}],
		     "@file.recs.fields.file"),
		   [{group_by,{nopos,"f1"},
		     list,[{{Filename,1,1},"recfield_test.erl"}]},
		    {group_by,{nopos,"f2"},
		     list,[{{Filename,1,1},"recfield_test.erl"}]}]),
     ?_assertEqual(refusr_sq:run(
		     [{positions, scalar},{output, other}],
		     [{file, Filename}, {position, 34}],
		     "@file.recs.fields.name"),
		   [{group_by,{{Filename,25,49},"f1"},
		     eq,name,f1},
		    {group_by,{{Filename,25,49},"f2"},
		     eq,name,f2}])].

macro_testfile() ->
    {module, ?MISC:canonical_filename("macro_test.erl"),
     "-module(macro_test).\n"
     "-record(rec, {f1=1, f2}).\n"
     "-define(F,f).\n"
     "f(X) -> io:format(\"alma\"),\n"
     "        X.\n"
     "g(Rec) -> ?F(h(Rec)).\n"
     "h(Rec) -> Rec#rec.f2,\n"
     "          #rec.f1.\n"}.

macro_test_() ->
    {setup,
     fun() -> add_file(macro_testfile()) end,
     fun(File) -> ?FileMan:drop_file(File) end,
     macro_tests()}.

macro_tests() ->
    Filename = ?MISC:canonical_filename("macro_test.erl"),
    [?_assertEqual(refusr_sq:run(
		     [{positions, scalar},{output, other}],
		     [{file, Filename}, {position, 34}],
		     "@file.macros"),
		   [{group_by,{nopos,"macro_test.erl"},
		     list,[{{Filename,48,60},"F"}]}]),
     ?_assertEqual(refusr_sq:run(
		     [{positions, scalar},{output, other}],
		     [{file, Filename}, {position, 34}],
		     "@file.macros.refs"),
		   [{group_by,{nopos,"F"},
		     list,[{{Filename,110,111},"?F"}]}]),
     ?_assertEqual(refusr_sq:run(
		     [{positions, scalar},{output, other}],
		     [{file, Filename}, {position, 34}],
		     "@file.macros.file"),
		   [{group_by,{nopos,"F"},
		     list,[{{Filename,1,1},"macro_test.erl"}]}]),
     ?_assertEqual(refusr_sq:run(
		     [{positions, scalar},{output, other}],
		     [{file, Filename}, {position, 34}],
		     "@file.macros.name"),
		   [{group_by,{{Filename,48,60},"F"},
		     eq,name,"F"}]),
     ?_assertEqual(refusr_sq:run(
		     [{positions, scalar},{output, other}],
		     [{file, Filename}, {position, 34}],
		     "@file.macros.arity"),
		   [{group_by,{{Filename,48,60},"F"},
		     eq,arity,0}]),
     ?_assertEqual(refusr_sq:run(
		     [{positions, scalar},{output, other}],
		     [{file, Filename}, {position, 34}],
		     "@file.macros.const"),
		   [{group_by,{{Filename,48,60},"F"},
		     eq,const,true}])].

expr_testfile() ->
    {module, ?MISC:canonical_filename("expr_test.erl"),
     "-module(expr_test).\n"
     "-record(rec, {f1=1, f2}).\n"
     "-define(F,f).\n"
     "f(X) -> io:format(\"alma\"),\n"
     "        X.\n"
     "g(Rec) -> ?F(h(Rec)).\n"
     "h(Rec) -> Rec#rec.f2,\n"
     "          #rec.f1.\n"}.

expr_test_() ->
    {setup,
     fun() -> add_file(expr_testfile()) end,
     fun(File) -> ?FileMan:drop_file(File) end,
     expr_tests()}.

expr_tests() ->
    Filename = ?MISC:canonical_filename("expr_test.erl"),
    [?_assertEqual(refusr_sq:run(
		     [{positions, scalar},{output, other}],
		     [{file, Filename}, {position, 34}],
		     "@file.funs[name==h].exprs"),
		   [{group_by,{nopos,"expr_test:h/1"},
		     list,
		     [{{Filename,123,125},"Rec"},
		      {{Filename,131,140},"Rec#rec.f2"},
		      {{Filename,153,159},"#rec.f1"}]}]),
     ?_assertEqual(refusr_sq:run(
		     [{positions, scalar},{output, other}],
		     [{file, Filename}, {position, 34}],
		     "@file.funs[name==h].exprs.fundef"),
		   [{group_by,{nopos,"Rec"},
		     list,[{{Filename,121,160},"expr_test:h/1"}]},
		    {group_by,{nopos,"Rec#rec.f2"},
		     list,[{{Filename,121,160},"expr_test:h/1"}]},
		    {group_by,{nopos,"#rec.f1"},
		     list,[{{Filename,121,160},"expr_test:h/1"}]}]),
     ?_assertEqual(refusr_sq:run(
		     [{positions, scalar},{output, other}],
		     [{file, Filename}, {position, 34}],
		     "@file.funs.exprs.funs"),
		   [{group_by,{nopos,"io:format(\"alma\")"},
		     list,[{nopos,"io:format/1"}]},
		    {group_by,{nopos,"?F(h(Rec))"},
		     list,
		     [{{Filename,61,97},"expr_test:f/1"},
                      {{Filename,121,160},"expr_test:h/1"}]}]),
     ?_assertEqual(refusr_sq:run(
		     [{positions, scalar},{output, other}],
		     [{file, Filename}, {position, 34}],
		     "@file.funs[name==h].exprs.vars"),
		   [{group_by,{nopos,"Rec"},
		     list,[{{Filename,123,125},"Rec"}]},
		    {group_by,{nopos,"Rec#rec.f2"},
		     list,[{{Filename,123,125},"Rec"}]}]),
     ?_assertEqual(refusr_sq:run(
		     [{positions, scalar},{output, other}],
		     [{file, Filename}, {position, 34}],
		     "@file.funs[name==h].exprs.records"),
		   [{group_by,{nopos,"Rec#rec.f2"},
		     list,[{{Filename,21,45},"rec"}]},
		    {group_by,{nopos,"#rec.f1"},
		     list,[{{Filename,21,45},"rec"}]}]),
     ?_assertEqual(refusr_sq:run(
		     [{positions, scalar},{output, other}],
		     [{file, Filename}, {position, 34}],
		     "@file.funs.exprs.macros"),
		   [{group_by,{nopos,"?F(h(Rec))"},
		     list,[{{Filename,47,59},"F"}]}]),
     ?_assertEqual(refusr_sq:run(
		     [{positions, scalar},{output, other}],
		     [{file, Filename}, {position, 34}],
		     "@file.funs[name==g].exprs.sub"),
		   [{group_by,{{Filename,101,103},"Rec"},
		     list,[{{Filename,101,103},"Rec"}]},
		    {group_by,{{Filename,109,118},"?F(h(Rec))"},
		     list,
		     [{{Filename,109,110},"?F"},
		      {{Filename,112,112},"h"},
		      {{Filename,114,116},"Rec"},
		      {{Filename,113,117},"(Rec)"},
		      {{Filename,112,117},"h(Rec)"},
		      {{Filename,111,118},"(h(Rec))"},
		      {{Filename,109,118},"?F(h(Rec))"}]}]),
     ?_assertEqual(refusr_sq:run(
		     [{positions, scalar},{output, other}],
		     [{file, Filename}, {position, 34}],
		     "@file.funs[name==g].exprs.param"),
		   [{group_by,{nopos,"?F(h(Rec))"},
		     list,[{{Filename,112,117},"h(Rec)"}]}]),
     ?_assertEqual(refusr_sq:run(
		     [{positions, scalar},{output, other}],
		     [{file, Filename}, {position, 34}],
		     "@file.funs[name==g].exprs.top"),
		   [{group_by,{{Filename,101,103},"Rec"},
		     list,[{{Filename,101,103},"Rec"}]},
		    {group_by,{{Filename,109,118},"?F(h(Rec))"},
		     list,[{{Filename,109,118},"?F(h(Rec))"}]}]),
     %%       ?_assertEqual(refusr_sq:run(
     %%                   [{positions, scalar},{output, other}],
     %%                   [{file, Filename}, {position, 34}],
     %%                   "@file.funs[name==g].exprs.reach"),
     %%                 []),
     %%       ?_assertEqual(refusr_sq:run(
     %%                   [{positions, scalar},{output, other}],
     %%                   [{file, Filename}, {position, 34}],
     %%                   "@file.funs[name==g].exprs.origin"),
     %%                 []),
     ?_assertEqual(refusr_sq:run(
		     [{positions, scalar},{output, other}],
		     [{file, Filename}, {position, 34}],
		     "@file.funs[name==g].exprs.file"),
		   [{group_by,{nopos,"Rec"},
		     list,[{{Filename,1,1},"expr_test.erl"}]},
		    {group_by,{nopos,"?F(h(Rec))"},
		     list,[{{Filename,1,1},"expr_test.erl"}]}])].
    
clause_testfile() ->
    {module, ?MISC:canonical_filename("clause_test.erl"),
    "-module(clause_test).\n"
    "fun_with_one_clause() -> 42.\n"
    "fun_with_multi_clause(1) -> 1;\n"
    "fun_with_multi_clause([A]) -> A;\n"
    "fun_with_multi_clause({A,B}) -> A+B;\n"
    "fun_with_multi_clause(_) -> alma,\n"
    "korte,\n"
    "ok.\n"}.

clause_test_()->
    {setup,
     fun() -> add_file(clause_testfile()) end,
     fun(File) -> ?FileMan:drop_file(File) end,
     clause_tests()}.

clause_tests() ->
    Filename = ?MISC:canonical_filename("clause_test.erl"),
    [?_assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 1}],
                     "@file.funs[name == fun_with_one_clause].clause"),
                   [{group_by,{nopos,"clause_test:fun_with_one_clause/0"},
		     list,
		     [{{Filename,23,49},
		       "clause_test:fun_with_one_clause/0_1"}]}]),
     ?_assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 1}],
                     "@file.funs[name == fun_with_one_clause].clause[index == 2]"),
                   [{list, []}]),
     ?_assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 1}],
                     "@file.funs[name == fun_with_multi_clause].clause"),
                   [{group_by,{nopos,"clause_test:fun_with_multi_clause/1"},
		     list,
		     [{{Filename,52,80},
		       "clause_test:fun_with_multi_clause/1_1"},
		      {{Filename,83,113},
		       "clause_test:fun_with_multi_clause/1_2"},
		      {{Filename,116,150},
		       "clause_test:fun_with_multi_clause/1_3"},
		      {{Filename,153,195},
		       "clause_test:fun_with_multi_clause/1_4"}]}]),
     ?_assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 1}],
                     "@file.funs[name == fun_with_multi_clause].clause[index == 1]"),
                   [{group_by,{nopos,"clause_test:fun_with_multi_clause/1"},
		     list,
		     [{{Filename,52,80},
		       "clause_test:fun_with_multi_clause/1_1"}]}]),
     ?_assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 1}],
                     "@file.funs[name == fun_with_multi_clause].clause[index == 3]"),
                   [{group_by,{nopos,"clause_test:fun_with_multi_clause/1"},
		     list,
		     [{{Filename,116,150},
		       "clause_test:fun_with_multi_clause/1_3"}]}]),
     ?_assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 1}],
                     "@file.funs[name == fun_with_multi_clause].clause[index == 4]"),
                   [{group_by,{nopos,"clause_test:fun_with_multi_clause/1"},
		     list,
		     [{{Filename,153,195},
		       "clause_test:fun_with_multi_clause/1_4"}]}]),
     ?_assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 1}],
                     "@file.funs[name == fun_with_multi_clause].clause.module"),
                   [{group_by,{nopos,"clause_test:fun_with_multi_clause/1_1"},
		     list,
		     [{{Filename,1,1},
		       "clause_test.erl"}]},
		    {group_by,{nopos,"clause_test:fun_with_multi_clause/1_2"},
		     list,
		     [{{Filename,1,1},
		       "clause_test.erl"}]},
		    {group_by,{nopos,"clause_test:fun_with_multi_clause/1_3"},
		     list,
		     [{{Filename,1,1},
		       "clause_test.erl"}]},
		    {group_by,{nopos,"clause_test:fun_with_multi_clause/1_4"},
		     list,
		     [{{Filename,1,1},
		       "clause_test.erl"}]}]),
     ?_assertEqual(refusr_sq:run(
                     [{positions, scalar},{output, other}],
                     [{file, Filename}, {position, 1}],
                     "@file.funs[name == fun_with_multi_clause].choc:avg"),
                   [{eq,avg,140.0}])].    

help_test_() ->
    [
     ?_assertEqual({"Statistics",
                    [{"minimum",["min"],"int","The minimum of the values."},
                     {"maximum",["max"],"int","The maximum of the values."},
                     {"sum",[],"int","The sum of the values."},
                     {"mean",["average","avg"],"int","The mean of the values."},
                     {"median",["med"],"int","The median of the values."},
                     {"variance",["var"],"int","The variance of the values."},
                     {"standard_deviaton",
                      ["sd"],
                      "int","The standard deviation of the values."}]},
                   refusr_sq:run([{output,other}],[],"mods.loc:?")),

     ?_assertEqual({"Selectors for files",
                    [{"functions",
                      ["function","funs","fun"],
                      "function","The functions defined in the file."},
                     {"typerefs",[],"type","The types used in the file."},
                     {"typedef",
                      ["typedefs","types","type"],
                      "type","The types defined in the file."},
                     {"spec",["specs"],"spec","Specs defined in the file."},
                     {"records",
                      ["record","recs","rec"],
                      "record","The records defined in the file."},
                     {"macros",
                      ["macro"],
                      "macro","The macros defined in the file."},
                     {"includes",[],"file",
                      "The files included by the given file."},
                     {"included_by",[],"file",
                      "The files including the given file."},
                     {"imports",[],"function",
                      "The imported functions of the file."},
                     {"exports",[],"function",
                      "The exported functions of the file."}]},
                   refusr_sq:run([{output,other}],[],"mods.?")),

     ?_assertEqual(
        {"Initial Selectors",
         [{"@function",
           ["@fun"],
           "function","The function indicated by the given position."},
          {"@variable",
           ["@var"],
           "variable","The variable indicated by the given position."},
          {"@macro",[],"macro",
           "The macro indicated by the given position."},
          {"@record",
           ["@rec"],
           "record","The record indicated by the given position."},
          {"@recfield",
           ["@field"],
           "field","The record field indicated by the given position."},
          {"@file",[],"file","The current file."},
          {"files",[],"file",
           "The files loaded into the database (including headers)."},
          {"@module",
           ["@mod"],
           "file",
           "The module indicated by a given position (from a module qualifier)."},
          {"modules",
           ["mods"],
           "file",
           "The modules loaded into the database and the modules used in them."},
          {"@expression",
           ["@expr"],
           "expression",
           "The expression indicated by the given position."},
          {"@clause",[],"clause",
           "The clause indicated by the given position."},
          {"@spec",[],"spec",
           "The spec indicated by the given position."},
          {"@type",[],"spec",
           "The type expression indicated by the given position."},
          {"@definition",
           ["@def"],
           "any","The entity indicated by the given position."}]},
        refusr_sq:run([{output,other}],[],"?")),

     ?_assertEqual(
        {"Properties for files",
         [{"is_module",
           ["is_mod","module","mod"],
           "bool","Determines whether the file is module or not."},
          {"is_header",
           ["header"],
           "bool","Determines whether the file is header or not."},
          {"name",[],"atom","The name of a file(without extension)."},
          {"filename",[],"string","The filename(with extension)."},
          {"directory",["dir"],"string","The directory of the file."},
          {"path",[],"string","The path to the file."},
          {"module_sum",["mod_sum"],"int"," "},
          {"line_of_code",["loc"],"int"," "},
          {"char_of_code",["choc"],"int"," "},
          {"number_of_fun",
           ["num_of_fun","num_of_funs","num_of_functions",
            "number_of_functions","funnum"],
           "int"," "},
          {"number_of_macros",
           ["num_of_macros","num_of_mac","macnum"],
           "int"," "},
          {"number_of_records",
           ["num_of_records","num_of_rec","recnum"],
           "int"," "},
          {"included_files",["inc_files"],"int"," "},
          {"imported_modules",
           ["imp_modules","imported_mod","imp_mod","impmods"],
           "int"," "},
          {"number_of_funpath",
           ["number_of_funpaths","num_of_funpath","num_of_funpaths",
            "funpathnum"],
           "int"," "},
          {"function_calls_in",["fun_calls_in","callsin"],"int"," "},
          {"function_calls_out",
           ["fun_calls_out","callsout"],
           "int"," "},
          {"cohesion",["coh"],"int"," "},
          {"otp_used",["otp"],"int"," "},
          {"max_application_depth",
           ["max_app_depth","maxappdepth"],
           "int"," "},
          {"max_depth_of_calling",
           ["max_depth_calling","max_depth_of_call","max_depth_call",
            "maxcalldepth"],
           "int"," "},
          {"min_depth_of_calling",
           ["min_depth_calling","min_depth_of_call","min_depth_call",
            "mincalldepth"],
           "int"," "},
          {"max_depth_of_cases",
           ["max_depth_cases","maxcasedepth"],
           "int"," "},
          {"min_depth_of_cases",
           ["min_depth_cases","mincasedepth"],
           "int"," "},
          {"max_depth_of_structs",
           ["max_depth_structs","max_depth_of_structures",
            "max_depth_structures","maxstrdepth"],
           "int"," "},
          {"number_of_funclauses",
           ["num_of_funclauses","number_of_funclaus","num_of_funclaus",
            "funclnum"],
           "int"," "},
          {"branches_of_recursion",
           ["branches_of_rec","branch_of_recursion","branch_of_rec",
            "recbranches"],
           "int"," "},
          {"mcCabe",["mccabe"],"int"," "},
          {"number_of_funexpr",
           ["num_of_funexpr","funexprnum"],
           "int"," "},
          {"number_of_messpass",["messpassnum"],"int"," "},
          {"fun_return_points",
           ["fun_return_point","funretpoints","function_return_points",
            "function_return_point"],
           "int"," "},
          {"max_length_of_line",["maxlinelength"],"int"," "},
          {"average_length_of_line",
           ["avg_length_of_line","avglinelength"],
           "int"," "},
          {"no_space_after_comma",[],"int"," "}]},
        refusr_sq:run([{output,other}], [], "mods[?]"))
    ].


