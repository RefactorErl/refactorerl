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

%%% @doc Unit test for the spec/type analyser.
%%% @author Denes Peteri <petden@caesar.elte.hu>

%%% eunit:test(reftest_referl_anal_spec_type).

-module(reftest_referl_anal_spec_type).

-include_lib("eunit/include/eunit.hrl").

-include_lib("referl_gen/include/gen_export.hrl").
-include_lib("referl_core/include/core_global.hrl").
-include_lib("referl_lib/include/lib_export.hrl").
-include_lib("referl_core/include/core_export.hrl").

-define(Lib, refusr_sq_lib).

%-compile(export_all).

add_file({Type, Name, Text}) ->
    File = ?ESG:create(#file{path=Name, type=Type, eol={lf,eol},lastmod=now()}),
    ?ESG:insert(?ESG:root(), file, File),
    ?Graph:mklink(File, incl, File),
    ?FileMan:add_text(File, last, Text),
    ?ESG:finalize(),
    File.

spectest_file() ->
    {module, ?MISC:canonical_filename("spectest.erl"),
     "-module(spectest).\n"
     "-export_type([myt3/1]).\n"
     "fi() -> [].\n"
     "fj() -> [].\n"
     "-define(SpecModName, spectest).\n"
     "-spec ?SpecModName:ff() -> ok.\n"
     "ff() -> ok.\n"
     "-define(SpecFunName, fg).\n"
     "-spec ( ?SpecFunName(NameOfB :: B) -> B when B :: any(); \n"
     "    (list(list(ok))) -> ok).\n"
     "fg(alma) -> ok;\n"
     "fg(kortle) -> ok;\n"
     "fg(narancs) -> nok.\n"
     "-define(SpecArity, 2).\n"
     "-spec fh/?SpecArity :: (_,_) -> '0myt'(any(),\n"
     "    '0myt'(spectest:'0myt'('0myt'(a, a),'0myt'(a,a)), a)).\n"
     "fh(_,_) -> [].\n"
     "-opaque ('0myt'(AA, _) :: list(AA)).\n"
     "-type myt2() :: ok.\n"
     "-type myt3(_E) :: ok.\n"
     "-spec fi() -> [{A, A, B, B, C, D}] | ok when A :: any(),\n"
     "    B :: myt2() | {}, is_subtype(D, any()).\n"}.

spectest2_file() ->
    {module, ?MISC:canonical_filename("spectest2.erl"),
     "\n"
     "-module(spectest2).\n"
     "-record(myr, {f1=42 :: noneexistentmodule:nt() | spectest:'0myt'(a,a)}).\n"
     "-spec noneexistentmodule:f() -> ok.\n"
     "-spec spectest:fj() -> [].\n"}.

add_files() ->
    add_file(spectest2_file()),
    add_file(spectest_file()).

%% todo: more EUnit tests; for every node, for every edge, etc. Tests for
%% the new reflib modules and sq.
spec_type_test() ->
    _Filename = ?MISC:canonical_filename("init_sel_test.erl"),
    {setup,
     add_files(),
     fun ?FileMan:drop_file/1 ,
     [?assertEqual(lists:sort(lists:map(fun(Mod) ->
                                            ?Mod:name(Mod)
                                        end, ?Graph:path(?Graph:root(),
                                                         [module]))),
                   lists:sort([spectest2,noneexistentmodule,spectest,erlang])),
      ?assertEqual(lists:sort(lists:map(fun(Spec) ->
                                            ?Graph:data(Spec)
                                        end, ?Graph:path(?Graph:root(),
                                              [module] ++ ?Mod:specs()))),
                   lists:sort([#spec{name=ff,arity=0},
                               #spec{name=fg,arity=1},
                               #spec{name=fh,arity=2},
                               #spec{name=fi,arity=0},
                               #spec{name=f,arity=0},
                               #spec{name=fj,arity=0}
                              ])),
      ?assertEqual(lists:sort(lists:map(fun(Type) ->
                                            ?Graph:data(Type)
                                        end, ?Graph:path(?Graph:root(),
                                              [module] ++ ?Mod:types()))),
                   lists:sort([#namedtype{name='0myt',arity=2,
                                          isopaque=true,isbuiltin=false},
                               #namedtype{name=myt2,arity=0,
                                          isopaque=false,isbuiltin=false},
                               #namedtype{name=myt3,arity=1,
                                          isopaque=false,isbuiltin=false},
                               #namedtype{name=nt,arity=0,
                                          isopaque=false,isbuiltin=false},
                               #namedtype{name=any,arity=0,
                                          isopaque=false,isbuiltin=true},
                               #namedtype{name=list,arity=1,
                                          isopaque=false,isbuiltin=true}
                              ]))
     ]}.

%%% Test cases and test files (the test files are in test/unit_test/anal_type_spec)
%%%
%%% <ul>
%%%     <li>module_qualifier: exspec
%%%         arity_qualifier
%%%         ri:q("mods[name=erlang].***").</li>
%%%     <li>parameter name: exspec</li>
%%%     <li>-spec(...). exspec3
%%%         '-' 'spec' '(' tattr->TFunRef '::' tattr->TypFunction ')' 'stop':
%%%         exspec16_wrong, exspec19
%%%         opaque extype7
%%%         joker extype7</li>
%%%     <li>more clauses: exspec4
%%%         more function clauses; fret, fpar ref edges: exspec4</li>
%%%     <li>specguard; more constraints, only 1 or less specguard node: exspec6
%%%         is_subtype in guard
%%%         list, union, etc. in guard: exspec14</li>
%%%     <li>typexp: union, fun, binary... extype13
%%%         myt(myt(myt(tuple()))) refs? extype5</li>
%%%     <li>more specclauses with guards: exspec7</li>
%%%     <li>load testing: exspec10
%%%         lots of clauses for example: exspec18
%%%         hard to calculate typexp in param: exspec35, exspec34</li>
%%%     <li>polymorphic types, namedtypeparam: exspec13</li>
%%%     <li>type/typeparam refs, typetop</li>
%%%     <li>macros in the name for example: exspec20</li>
%%%     <li>-export_type([card/0]). : exspec10, extype6
%%%         typexpmodref (spec/type)
%%%         refing a nonexistent module; module ref before def; spec in a not
%%%             (yet) loaded module: exspec26, exspec33a</li>
%%%     <li>spec in header: exspec21
%%%         spec and func in header: exspec25
%%%         query: spec module?</li>
%%%     <li>import func with spec from other module: exspec24</li>
%%%     <li>more file including one header with spec: exspec30i
%%%         spec has module qualifier in header: exspec32i</li>
%%%     <li>excpetion tests:
%%%         wrong arity: exspec16_wrong
%%%         builtin type</li>
%%%     <li>refactorings
%%%         tupfun: exspec29
%%%         genfun: exspec31</li>
%%%     <li>database_synchronization
%%%         exspec36
%%%         change in forms (module for example)
%%%         parse error in database_sync</li>
%%%     <li>reflib
%%%         sq</li>
%%%     <li>ri:svg().</li>
%%% </ul>

