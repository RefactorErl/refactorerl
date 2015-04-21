
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

%%% @doc todo
%%%
%%% @todo author

-module(refanal_var_tests).
-include_lib("eunit/include/eunit.hrl").
-include("test.hrl").

-define(setup(Text, Tests),
        {setup, fun() -> add_fun(Text) end, fun del_fun/1,
         fun({_, F}) ->
                 {inorder, [{Title, fun() -> Test(F) end} ||
                               {Title, Test} <- Tests]}
         end}).

basic_test_() ->
    ?setup(
      "f(X) -> X+1.\n",
      [{"def",
        fun(Fun) ->
                [Var] = ?ESG:path(Fun, [funcl, vardef]),
                ?assertMatch(#variable{name="X"}, ?ESG:data(Var))
        end},
       {"bind",
        fun(Fun) ->
                [Var] = ?ESG:path(Fun, [funcl, vardef]),
                [Bind] = ?ESG:path(Var, [{varbind, back}]),
                ?assertMatch(#expr{type=variable, role=pattern, value="X"},
                             ?ESG:data(Bind)),
                ?assertMatch([Bind], ?ESG:path(Var, [varintro]))
        end},
       {"ref",
        fun(Fun) ->
                [Var] = ?ESG:path(Fun, [funcl, vardef]),
                [Ref] = ?ESG:path(Var, [{varref, back}]),
                ?assertMatch(#expr{type=variable, role=expr, value="X"},
                             ?ESG:data(Ref))
        end},
       {"vis",
        fun(Fun) ->
                [Var] = ?ESG:path(Fun, [funcl, vardef]),
                ?assertMatch([Var], ?ESG:path(Fun, [funcl, varvis]))
        end},
       {"cltype",
        fun(Fun) ->
                [Cl] = ?ESG:path(Fun, [funcl]),
                ?assertMatch(#clause{var=scope}, ?ESG:data(Cl))
        end}
      ]).

cleanup_test() ->
    {_File, Fun} = Data = add_fun("f()->X=1.\n"),
    [Var] = ?ESG:path(Fun, [funcl, vardef]),
    del_fun(Data),
    ?assertError(bad_node, ?ESG:data(Var)).

match_test_() ->
    ?setup(
       "f() -> X = 1, X.\n",
       [{"links",
         fun(Fun) ->
                 [E1, E2] = ?ESG:path(Fun, [funcl, body]),
                 [E3] = ?ESG:path(E1, [{esub,1}]),
                 [Var] = ?ESG:path(Fun, [funcl, vardef]),
                 ?assertMatch([E3], ?ESG:path(Var, [{varbind, back}])),
                 ?assertMatch([E1], ?ESG:path(Var, [varintro])),
                 ?assertMatch([E2], ?ESG:path(Var, [{varref, back}]))
         end}]).

clause_test_() ->
    ?setup(
       "f() -> case g() of {X} -> X+1 end.\n",
       [{"def",
         fun(Fun) ->
                 [Var] = ?ESG:path(Fun, [funcl, vardef]),
                 ?assertMatch(#variable{name="X"}, ?ESG:data(Var))
         end},
        {"bind",
         fun(Fun) ->
                 [Var] = ?ESG:path(Fun, [funcl, vardef]),
                 [Bind] = ?ESG:path(Var, [{varbind, back}]),
                 ?assertMatch(#expr{type=variable, role=pattern, value="X"},
                              ?ESG:data(Bind))
         end},
        {"vis",
        fun(Fun) ->
                [Var] = ?ESG:path(Fun, [funcl, vardef]),
                ?assertMatch([Var], ?ESG:path(Fun, [funcl, varvis])),
                ?assert(length(?ESG:path(Var, [{varvis, back}])) =:= 2)
        end}
       ]).

branch_test_() ->
    ?setup(
       "f() -> case g() of {X,Y} -> Y; {X,Z} -> Z end.\n",
       [{"def",
         fun(Fun) ->
                 Vars = ?ESG:path(Fun, [funcl, vardef]),
                 ?assertMatch(["X","Y","Z"], varnames(Vars))
         end},
        {"vis",
         fun(Fun) ->
                 TV = ?ESG:path(Fun, [funcl, varvis]),
                 ?assertMatch(["X"], varnames(TV)),
                 CV1 = ?ESG:path(Fun, [funcl, {body,1}, {exprcl, 1}, varvis]),
                 ?assertMatch(["X","Y"], varnames(CV1)),
                 CV2 = ?ESG:path(Fun, [funcl, {body,1}, {exprcl, 2}, varvis]),
                 ?assertMatch(["X","Z"], varnames(CV2))
         end},
        {"intro",
         fun(Fun) ->
                 [Case] = ?ESG:path(Fun, [funcl, {body,1}]),
                 [P1] = ?ESG:path(Case, [{exprcl,1}, pattern]),
                 [P2] = ?ESG:path(Case, [{exprcl,2}, pattern]),

                 [VX] = ?ESG:path(Fun, [funcl, {vardef, {name, '==', "X"}}]),
                 ?assertMatch([P1, P2, Case],
                              lists:sort(?ESG:path(VX, [varintro]))),

                 [VY] = ?ESG:path(Fun, [funcl, {vardef, {name, '==', "Y"}}]),
                 ?assertMatch([P1], ?ESG:path(VY, [varintro])),

                 [VZ] = ?ESG:path(Fun, [funcl, {vardef, {name, '==', "Z"}}]),
                 ?assertMatch([P2], ?ESG:path(VZ, [varintro]))
         end}
       ]).

inherit_test_() ->
    ?setup(
        "f(X) -> case g(X) of Y -> begin X+Y end end.\n",
        [{"vis",
          fun(Fun) ->
                  TV = ?ESG:path(Fun, [funcl, varvis]),
                  ?assertMatch(["X", "Y"], varnames(TV)),
                  HV = ?ESG:path(Fun, [funcl, {body, 1}, {headcl, 1}, varvis]),
                  ?assertMatch(["X"], varnames(HV)),
                  CV = ?ESG:path(Fun, [funcl, {body, 1}, {exprcl, 1}, varvis]),
                  ?assertMatch(["X", "Y"], varnames(CV)),
                  EV = ?ESG:path(Fun, [funcl, {body, 1}, {exprcl, 1},
                                              {body, 1}, {exprcl, 1}, varvis]),
                  ?assertMatch(["X", "Y"], varnames(EV))
          end}
        ]).

compr_test_() ->
    ?setup(
        "f(L) -> [X || {a,X} <- L, X > 0].\n",
        [{"def",
          fun(Fun) ->
                  Vars = ?ESG:path(Fun, [funcl, vardef]),
                  ?assertMatch(["L"], varnames(Vars)),
                  [Gen] = ?ESG:path(Fun, [funcl, {body, 1}, {exprcl, 2},
                                                 {body, 1}, {exprcl, 1}]),
                  HVars = ?ESG:path(Gen, [vardef]),
                  ?assertMatch(["X"], varnames(HVars))
          end},
         {"vis",
          fun(Fun) ->
                 TV = ?ESG:path(Fun, [funcl, varvis]),
                 ?assertMatch(["L"], varnames(TV)),
                 HV = ?ESG:path(Fun, [funcl, {body, 1}, {exprcl, 1},
                                             {body, 1}, {visib, back}, varvis]),
                 ?assertMatch(["L", "X"], varnames(HV))
         end}
        ]).

% move_topexp_test_() ->
%     ?setup(
%        "f(a) -> X=1, ok;\n"
%        "f(b) -> ok.\n",
%        [{"pre",
%         fun(Fun) ->
%                 [Cl1, Cl2] = ?ESG:path(Fun, [funcl]),
%                 [V] = ?ESG:path(Cl1, [vardef]),
%                 ?assertMatch(#variable{name="X"}, ?ESG:data(V)),
%                 ?assertMatch([], ?ESG:path(Cl2, [vardef]))
%         end},
%         {"do",
%          %% Move `X=1' from clause `f(a)' to clause `f(b)'
%          fun(Fun) ->
%                  %user_default:tree(Fun),
%                  [Cl1, Cl2] = ?ESG:path(Fun, [funcl]),
%                  [{body, E}] = ?Syn:replace(Cl1, {body,1}, []),
%                  ?Syn:replace(Cl2, {body, 1, 0}, [E]),
%                  ?ESG:finalize(),
%                  ?assertMatch([_], ?ESG:path(Cl1, [body])),
%                  ?assertMatch([E,_], ?ESG:path(Cl2, [body])),
%                  ?assertMatch([_,E,_], ?ESG:path(Cl2, [visib]))
%          end},
%         {"defs",
%          fun(Fun) ->
%                  [Cl1, Cl2] = ?ESG:path(Fun, [funcl]),
%                  ?assertMatch([], ?ESG:path(Cl1, [vardef])),
%                  [V] = ?ESG:path(Cl2, [vardef]),
%                  ?assertMatch(#variable{name="X"}, ?ESG:data(V))
%          end},
%         {"visib",
%          fun(Fun) ->
%                  [Cl1, Cl2] = ?ESG:path(Fun, [funcl]),
%                  ?assertMatch([], ?ESG:path(Cl1, [varvis])),
%                  [V] = ?ESG:path(Cl2, [varvis]),
%                  ?assertMatch(#variable{name="X"}, ?ESG:data(V))
%          end},
%         {"intro",
%          fun(Fun) ->
%                  [Cl1, Cl2] = ?ESG:path(Fun, [funcl]),
%                  ?assertMatch([], ?ESG:path(Cl1, [visib, {varintro,back}])),
%                  [V] = ?ESG:path(Cl2, [vardef]),
%                  [E] = ?ESG:path(Cl2, [{body,1}]),
%                  ?assertMatch([E], ?ESG:path(V, [varintro]))
%          end}
%        ]).

add_ref_test_() ->
    ?setup(
      "f() -> X=1.\n",
      [{"pre",
        fun(Fun) ->
                [V] = ?ESG:path(Fun, [funcl, vardef]),
                ?assertMatch(#variable{name="X"}, ?ESG:data(V))
        end},
       {"do",
        %% Add `X=2' after `X=1'
        fun(Fun) ->
                E = ?Syn:create(
                       #expr{type=match_expr},
                       [{esub, ?Syn:create(#expr{type=variable}, ["X"])},
                        {esub, ?Syn:create(#expr{type=integer}, ["2"])}]),
                [Cl] = ?ESG:path(Fun, [funcl]),
                ?Syn:replace(Cl, {body,2,0}, [E]),
                ?ESG:finalize(),
                ?assertMatch([_,E], ?ESG:path(Fun, [funcl, body])),
                ?assertMatch([_,E], ?ESG:path(Fun, [funcl, visib]))
        end},
       {"ref",
        fun(Fun) ->
                [V] = ?ESG:path(Fun, [funcl, vardef]),
                [I] = ?ESG:path(Fun, [funcl, {body,1}]),
                [B, R] = ?ESG:path(Fun, [funcl, body, {esub, 1}]),
                ?assertMatch([B], ?ESG:path(V, [{varbind, back}])),
                ?assertMatch([I], ?ESG:path(V, [varintro])),
                ?assertMatch([R], ?ESG:path(V, [{varref, back}]))

        end}
      ]).


%% f()->X=1.  ~~>  f()->X=2,X=1.  (X=1 is turned into ref)
%% f()->X=1,X+1. (del bind, ref disappears)
%% f()->X=1,X=1,X+1. (del first bind, second bind takes its place)
%% f()->case g() of {X}->t; _->o end.  (add X to the second branch)
%% deep structures: fun, comprehension
%% scope change: X=1, fun()->X=2 end (remove X=1)

%% remove one of multiple bindings

varnames(Vars) ->
    lists:sort([N || V <- Vars, #variable{name=N} <- [?ESG:data(V)]]).

add_fun(Text) ->
    File = ?ESG:create(#file{type=module, path="test.erl"}),
    ?ESG:insert(?ESG:root(), file, File),
    ?FileMan:add_text(File, last, Text),
    ?ESG:finalize(),
    [Form] = ?ESG:path(File, [{form, last}]),
    {File, Form}.

del_fun({File, _}) ->
    ?ESG:remove(?ESG:root(), file, File),
    ?ESG:finalize().
