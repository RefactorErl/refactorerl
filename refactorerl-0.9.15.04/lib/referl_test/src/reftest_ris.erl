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

%%% @doc Unit test for {@link ris}.
%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>

-module(reftest_ris).
-vsn("$Rev: 3893 $ ").

-compile([export_all]).

-include("test.hrl").

files() ->
    [
%     {header, "u.hrl",
%     " "
     %%tester bug: includes don't work
     %%core bug: empty files don't work
%     },
%     {header, "v.hrl",
%     " "
     %%tester bug: includes don't work
     %%core bug: empty files don't work
%     },
     {module, "a.erl",
      "-module(a).\n"
      "-export([f/0,g/0,g/1,g/2]).\n"

      "-define(M(X), {X, X}).\n"
      "-define(N, 0).\n"
      "-record(r, {x, y}).\n"
      "-record(s, {}).\n"

%      "-include(\"a.hrl\").\n"
      "f() -> Y=2+2, #r{x=2+2, y=Y*Y}.\n"
      "g() -> ?M(3).\n"
      "g({X}) -> {fun f/0, f(), X}.\n"
      "g(X,Y) -> X#r{x=1, y=Y}.\n"
      "h() -> regexp:match(\"u\",\"v\").\n"
     },
     {module, "b.erl",
      "-module(b).\n"
     }
     ].

%%% ============================================================================
%%% macros

-define(AssertMatch(Pattern, X),
    case X of
        Pattern->
            ok;
        _->
            erlang:error(
                {assertMatch_failed,
                 [{module, ?MODULE},
                  {expected,??Pattern},
                  {value,X}]})
    end).

%%% ============================================================================
%%% helpers

qd(Q) ->
    ris:desugar(ris:q(Q)).

qu(Q) ->
    ris:unpack(ris:q(Q)).

assertEqualList(X,Y) when is_list(X), is_list(Y)->
    assertEqual(lists:sort(X), lists:sort(Y)).

assertEqual(X,Y)->
    case X=:=Y of
        true->
            ok;
        false->
            erlang:error(
                {assertEqual_failed,
                 [{module, ?MODULE},
                  {expected,X}, {value,Y}]})
    end.

assertAbort(Mod, Args)->
    assertEx(abort, Mod, Args).

assertErr(Mod, Args)->
    assertEx(error, Mod, Args).

assertError(Mod, Args)->
    assertErr(Mod, Args).

assertEx(Class, Fun, Args)->
    Mod = ris,
    B_Ex =
        try
            {error, [{got, {noex, apply(Mod, Fun, Args)}}]}
        catch
            Err={Cl, {{M,F,_A}, Txt=[C|_]}} when Cl==Class, is_integer(C),
                    is_atom(M), is_atom(F) ->
                    % (is_list(A) orelse is_tuple(A)) ->
                case io_lib:deep_char_list(Txt) of
                    true->
                        ok;
                    false->
                        {error, [{got, {throw, Err}},
                                 {trace, erlang:get_stacktrace()}]}
                end;
            Cl:Err->
                {error, [{got, {Cl, Err}},
                         {trace, erlang:get_stacktrace()}]}
        end,
    case B_Ex of
        ok->
            ok;
        {error, Ex}->
            erlang:error(assertEx_unexpected,
                 [{module, Mod},
                  {function, Fun},
                  {arguments, Args},
                  {expected,Class}] ++ Ex)
    end.

withundo(Fun) when is_function(Fun,0)->
    try
        Fun()
    after
        try ris:undo() catch _ -> nothing_done end
    end.

%%% ============================================================================
%%% tests

test_atomq()->
    ?AssertMatch(_, ris:q(mods)).

test_strq()->
    ?AssertMatch(_, ris:q("mods")).

test_uatomq()->
    ?AssertMatch(L when is_list(L), ris:desugar(ris:q(mods))).

test_ustrq()->
    ?AssertMatch(L when is_list(L), ris:desugar(ris:q("mods"))).

test_badsyntax1()->
    Invalid =
        [%bkil.hu, mods.bkil, mods.bkil.hu, 'mods[bkil].bkil',
         '.', '..', 'mods[name!=bkil]', 'mods[name==bkil#path=="bkil.hu"]', '@mod',
         'mods:sum', 'mods.fun:sum', 'mods.fun.arity:bkil',
         'mods.fun.(calls)', 'mods.fun.{calls}',
%         'mods.fun.mod.name',
     ['mods.fun.mod', '.name'],
         ['mods', '.fun', '.bkil']
        ],
    [assertAbort(q, [Q]) || Q <- Invalid],
    ok.

test_missing()->
    assertEqual([], qd('files[path=="bkil.hu"]')).

my_mods()->
    'files[name==a or name==b or filename=="u.hrl" or filename=="v.hrl"]'.

test_showrichstr()->
    assertEqual(true, io_lib:char_list(ris:show(ris:q(my_mods())))).

test_showustr()->
    assertEqual(true, io_lib:char_list(ris:show(qd(my_mods())))).

test_linecol()->
    assertEqual(true, io_lib:char_list(ris:show(ris:q(my_mods()), [linecol]))).

% TODO show file output

% TODO add
% TODO drop
% TODO add_byname

test_a_print()->
    assertEqual(ok, ris:print(ris:q(my_mods()))).

redirect_output(Fun) when is_function(Fun,1)->
    Sink = spawn(fun()-> sink(self(), []) end),
    Source = spawn_link(fun()-> receive start-> Fun(), Sink ! eof end end),
    group_leader(Sink, Source),
    Source ! start,
    receive
        {result, Sink, Result}->
            Result
    end.

sink(Parent, Sum)->
    receive
        {io_request, _X, _Y, {get_geometry, columns}}->
            80;
        eof->
            Parent ! {result, self(), Sum};
        IO->
            sink(Parent, [IO | Sum])
    end.

test_q1()->
    ?AssertMatch(_, ris:q1([my_mods(), '.name'])).

test_qstr()->
    assertEqual(true, io_lib:char_list(ris:qstr([my_mods(), '.name']))).

test_mods()->
    Mods = ris:q(my_mods()),
%    assertEqual(4, length(ris:desugar(Mods))),
%    ExpMods = ["a.erl", "b.erl", "u.hrl", "v.hrl"],
    assertEqual(2, length(ris:desugar(Mods))), %4
    ExpMods = ["a.erl", "b.erl"],
    assertEqualList(ExpMods, qu([Mods, ".filename"])),
    assertEqualList(ExpMods, qu([my_mods(), ".filename"])).

test_aritysum()->
    Mods = ris:q(my_mods()),
    Fun = ris:q([Mods, ".fun"]),
    AritySum = ris:q1([Fun, ".arity:sum"]),
    ExpSum = 3,
    assertEqual(ExpSum, AritySum),
    assertEqual(ExpSum, ris:q1([my_mods(), '.fun', ".arity:sum"])).

test_closure()->
    assertEqual(4, length(qd([my_mods(), ".fun[name==g].(calls)+"]))).

test_iteration0()->
    assertEqual(3, length(qd([my_mods(), ".fun[name==g].{calls}0"]))).

test_iteration1()->
    assertEqual(1, length(qd([my_mods(), ".fun[name==g].{calls}1"]))).

test_iteration2()->
    assertEqual(0, length(qd([my_mods(), ".fun[name==g].{calls}2"]))).

test_union()->
    R = qd({[my_mods(), ".fun[arity=<1]"],
            union,
            [my_mods(), ".fun[arity==0]"]}),
    assertEqual(4, length(R)).

test_intersection()->
    R = qd({[my_mods(), ".fun[arity=<1]"],
            intersect,
            [my_mods(), ".fun[arity==0]"]}),
    assertEqual(3, length(R)).

test_minus()->
    R = qd({[my_mods(), ".fun[arity=<1]"],
            minus,
            [my_mods(), ".fun[arity==0]"]}),
    assertEqual(1, length(R)).

% TODO: range

% SQ TODO: ris:q(['mods', '.fun', '.arity', ':sum'])
% SQ TODO: ris:q(['mods', '[name==a]'])
% SQ TODO: ris:q(['mods', '@mod.fun'])

%%% ----------------------------------------------------------------------------

xtest_ref_movefun_ok()->
    withundo(
      fun()->
              Moved = ris:move("mods[name=a].fun[name=g,arity=0]", b),
              assertEqual(b, ris:q1([Moved,'.file.name']))
      end).

xtest_ref_moverec_ok()->
    withundo(
      fun()->
              Moved = ris:move("mods[name=a].record[name=s]", b),
              assertEqual(b, ris:q1([Moved,'.file.name']))
      end).

xtest_ref_moverec_abort()->
    withundo(
      fun()->
              assertAbort(move, ["mods[name=a].record[name=r]", b])
      end).

xtest_ref_movemac_ok()->
    withundo(
      fun()->
              Moved = ris:move("mods[name=a].macro[name=\"N\"]", b),
              assertEqual(b, ris:q1([Moved,'.file.name']))
      end).

xtest_ref_movemac_abort()->
    withundo(
      fun()->
              assertAbort(move, ["mods[name=a].macro[name=\"M\"]", b])
      end).

xtest_ref_renfun_ok()->
    withundo(
      fun()->
              New = ris:rename("mods[name=a].function[name=f,arity=0]", ff),
              assertEqual(ff, ris:q1([New,'.name']))
      end).

xtest_ref_renfun_abort()->
    withundo(
      fun()->
              assertAbort(rename, ["mods[name=a].function[name=f,arity=0]", g])
      end).

xtest_ref_renmod_ok()->
    withundo(
      fun()->
              {_,_,Ns} = erlang:now(),
              Name = list_to_atom("a" ++ io_lib:print(Ns)),
              New = ris:rename("mods[name=a]", Name),
              assertEqual(Name, ris:q1([New,'.name']))
      end).

xtest_ref_renmod_abort()->
    withundo(
      fun()->
              assertAbort(rename, ["mods[name=a]", b])
      end).

xtest_ref_renhrl_ok()->
    withundo(
      fun()->
              {_,_,Ns} = erlang:now(),
              Name = "a" ++ io_lib:print(Ns) ++ ".hrl",
              New = ris:rename("files[filename=\"u.hrl\"]", Name),
              assertEqual(Name, ris:q1([New,'.filename']))
      end).

xtest_ref_renhrl_abort()->
    withundo(
      fun()->
              assertAbort(rename, ["files[filename=\"u.hrl\"]", "v.hrl"])
      end).

xtest_ref_renrec_ok()->
    withundo(
      fun()->
              New = ris:rename("mods[name=a].record[name=s]", ss),
              assertEqual(ss, ris:q1([New,'.name']))
      end).

xtest_ref_renrec_abort()->
    withundo(
      fun()->
              assertAbort(rename, ["mods[name=a].record[name=s]", r])
      end).

xtest_ref_renfld_ok()->
    withundo(
      fun()->
              Name = xx,
              New = ris:renamex("mods[name=a].record[name=r].fields[name=x]", Name),
              assertEqual(Name, ris:q1([New,'.name']))
      end).

xtest_ref_renfld_abort()->
    withundo(
      fun()->
              assertAbort(rename, ["mods[name=a].record[name=r].fields[name=x]", y])
      end).

xtest_ref_renmac_ok()->
    withundo(
      fun()->
              New = ris:rename("mods[name=a].macro[name=\"N\"]", "NN"),
              assertEqual("NN", ris:q1([New,'.name']))
      end).

xtest_ref_renmac_abort()->
    withundo(
      fun()->
              assertAbort(rename, ["mods[name=a].macro[name=\"N\"]", "M"])
      end).

xtest_ref_renvar_ok()->
    withundo(
      fun()->
              New = ris:rename("mods[name=a].fun[name=g,arity=2].var[name=\"X\"]",
                               "XX"),
              assertEqual("XX", ris:q1([New,'.name'])) %@todo FIX the refac!
      end).

xtest_ref_renvar_abort()->
    withundo(
      fun()->
              assertAbort(rename, ["mods[name=a].fun[name=g,arity=2].var[name=\"X\"]",
                                   "Y"])
      end).

xtest_ref_intrec_ok()->
    withundo(
      fun()->
              New = ris:intrec("mods[name=a].fun[name=g,arity=1].arguments",
                               {rr,[x]}),
              assertEqualList([x], qu([New,'.fields.name']))
      end).

xtest_ref_intrec_abort()->
    withundo(
      fun()->
              assertAbort(intrec, ["mods[name=a].fun[name=g,arity=1].arguments",
                                  {r,[xx]}])
      end).

xtest_ref_genfun_ok()->
    withundo(
      fun()->
%              New = ris:genfun("mods[name=a].fun[name=f,arity=0].body.sub[value=2]", % @todo genfun bug?
              New = ris:genfun("mods[name=a].fun[name=f,arity=0].body[last]",
                               "XX"),
              assertEqualList("XX", ris:qstr([New,'.value']))
      end).

xtest_ref_genfun_abort()->
    withundo(
      fun()->
              assertAbort(genfun, ["mods[name=a].fun[name=g,arity=1].body.sub[type=\application]",
                                  "X"])
      end).

xtest_ref_expfun_ok()->
    withundo(
      fun()->
              New = ris:expfun("mods[name=a].fun[name=g,arity=1].body.sub[type=implicit_fun]"),
              assertEqual(fun_expr, ris:q1([New,'.type']))
      end).

xtest_ref_expfun_abort()->
    withundo(
      fun()->
              assertAbort(expfun, ["mods[name=a].fun[name=g,arity=1].body"])
      end).

xtest_ref_extfun_ok()->
    withundo(
      fun()->
              New = ris:extfun("mods[name=a].fun[name=g,arity=1].body[last]",
                               ff),
                               ri:cat(a),
              assertEqual(ff, ris:q1([New,'.name']))
      end).

xtest_ref_extfun_abort()->
    withundo(
      fun()->
              assertAbort(extfun, ["mods[name=a].fun[name=g,arity=1].body[last]",
                                  g])
      end).

xtest_ref_elimvar_ok()->
    withundo(
      fun()->
              New = ris:eliminate("mods[name=a].fun[name=f,arity=0].var[name=\"Y\"]"),
              assertEqualList([parenthesis, parenthesis],
                              qu([New, ".type"]))
      end).

xtest_ref_elimvar_abort()->
    withundo(
      fun()->
              assertAbort(eliminate, ["mods[name=a].fun[name=g,arity=2].var[name=\"X\"]"])
      end).

xtest_ref_merge_ok()->
    withundo(
      fun()->
              Sel = [hd(ris:desugar(ris:q("mods[name=a].fun[name=f,arity=0].body.sub[type='+']")))],
              New = ris:merge(Sel, "XX"),
              assertEqualList(["XX", "XX"],
                              qu([New, ".value"]))
      end).

xtest_ref_merge_abort()->
    withundo(
      fun()->
              assertAbort(merge, ["mods[name=a].fun[name=g,arity=1].var[name=\"X\"].bindings",
                               "XX"])
      end).

test_ref_upregex_ok()-> %@todo
    withundo(
      fun()->
              New = ris:upregex(),
              ri:cat(a),
              ?d(New),
              ris:print(New),
              assertEqualList(expr, ris:q1([New, ".type"]))
      end).

% @todo inlfun inlmac tupfun reorder

% ok=ri:build(),reftest_lib:run(reftest_ris).
