%%% -*- coding: latin-1 -*-

%%% The  contents of this  file are  subject to  the Erlang  Public License,
%%% Version  1.1, (the  "License");  you may  not  use this  file except  in
%%% compliance  with the License.  You should  have received  a copy  of the
%%% Erlang  Public License  along  with this  software.  If not,  it can  be
%%% retrieved at http://plc.inf.elte.hu/erlang/
%%%
%%% Software  distributed under  the License  is distributed  on an  "AS IS"
%%% basis, WITHOUT  WARRANTY OF ANY  KIND, either expressed or  implied. See
%%% the License  for the specific language governing  rights and limitations
%%% under the License.
%%%
%%% The Original Code is RefactorErl.
%%%
%%% The Initial Developer of the  Original Code is Eötvös Loránd University.
%%% Portions created  by Eötvös  Loránd University are  Copyright 2008-2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc Test cases for test framework for graph representations of equivalence
%%% @author Szabo Bence Janos <szbtadi@caesar.elte.hu>

-module(reftest_refusr_ci_position).
-compile(export_all).
-include("test.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("referl_user/src/refusr_clone_identifierl.hrl").
-define(RootDir, filename:join(["..", "test", "unit_tests", "ci_position"])).
%% ============================================================================
%% Test cases

%% Test.
position_test_() ->
    {setup,
     fun setup/0,
     fun(_P) -> reset() end,
     fun(_P) -> [?_test(test_matrix()),
                ?_test(test_sw_metrics())]
                 end
     }.


test_matrix() ->
    lists:foreach(fun({Pos,Res}) -> 
        Nodes =
        try
            refusr_clone_identifierl_lib:get_clones_by_pos(matrix,Pos)
        catch
            _CL:_E -> error
        end,
        %erlang:display({Nodes,Res,Pos}),
        case Nodes of
            Res -> [?assert(true)];
            _ -> NRes = lists:map(fun(K) -> flat_text(K) end, Nodes), erlang:display(NRes),
                %erlang:display({NRes,Res}),
               [? assert(NRes == Res)]
            end
    end, matrix_pos_and_res()).

test_sw_metrics() ->
    lists:foreach(fun({Pos,Res}) -> 
        Nodes =
        try
            refusr_clone_identifierl_lib:get_clones_by_pos(sw_metrics,Pos)
        catch
            _Cl:_E -> error
        end,
        %erlang:display({Nodes,Res,Pos}),
        case Nodes of
            Res -> [?assert(true)];
            _ -> NRes = lists:map(fun(K) -> flat_text(hd(?Query:exec(K,?Fun:definition()))) end, Nodes),
               [? assert(NRes == Res)]
            end
    end, sw_metrics_pos_and_res()).


%% @doc Setup function.
setup() ->
    [add(?RootDir++"/"++File) || File<-files()].


%% @doc Trim to top level expression.
to_top_lvl_expr(STreeNodes) ->
    UniClones = refusr_clone_identifierl_lib:to_uni_format(STreeNodes),
    TopExprClones = refusr_clone_identifierl_lib:to_tlexpr_clones(UniClones, fstree),
    refusr_clone_identifierl_lib:format(TopExprClones, nodes, linecol).


%% @doc Testfiles.
files() ->
    ["test1.erl","test2.erl","test3.erl"].

% matrix tests
matrix_pos_and_res() ->
    [{{?RootDir++"/test1.erl",19,78},error},{{?RootDir++"/test1.erl",65,160},error},
     {{?RootDir++"/test1.erl",64,246},error},{{?RootDir++"/test1.erl",242,248},error},
     {{?RootDir++"/test1.erl",227,235},error},{{?RootDir++"/test1.erl",293,338},error},
     {{?RootDir++"/test1.erl",443,445},error},{{?RootDir++"/test1.erl",709,710},error},
     {{?RootDir++"/test1.erl",456,467},error},{{?RootDir++"/test1.erl",393,394},error},
     {{?RootDir++"/test1.erl",240,275},["    io:format(\"Ready\")"]},
     {{?RootDir++"/test1.erl",269,288},["    io:format(\"Ready\")","    C = A+B"]},
     {{?RootDir++"/test1.erl",230,311},["    io:format(\"Ready\")","    C = A+B","    ?Clause(C,A,B,ok)"]},
     {{?RootDir++"/test1.erl",279,324},["    C = A+B","    ?Clause(C,A,B,ok)"]},
     {{?RootDir++"/test1.erl",279,429},["    case A of\n        B -> ?Form(B);\n        _ -> ?Clause(A)\n    end",
 "    D = #rec{x = A, y = B}","    D"]},
     {{?RootDir++"/test1.erl",202,429},["    io:format(\"Ready\")","    C = A+B","    ?Clause(C,A,B,ok)",
 "    case A of\n        B -> ?Form(B);\n        _ -> ?Clause(A)\n    end",
 "    D = #rec{x = A, y = B}","    D"]},
     {{?RootDir++"/test1.erl",377,457},["    case A of\n        B -> ?Form(B);\n        _ -> ?Clause(A)\n    end",
 "    D = #rec{x = A, y = B}","    D","    Alma+1"]},
     {{?RootDir++"/test1.erl",492,548},["    MakeProperArgs = fun(Arg)-> [Unit, MaxRank, Arg] end",
 "    U = lists:map(fun(K) -> MakeProperArgs(K) end, [1,2])"]},
     {{?RootDir++"/test1.erl",459,593},["    MakeProperArgs = fun(Arg)-> [Unit, MaxRank, Arg] end",
    "    U = lists:map(fun(K) -> MakeProperArgs(K) end, [1,2])","    U+1"]},
     {{?RootDir++"/test1.erl",472,602},["    MakeProperArgs = fun(Arg)-> [Unit, MaxRank, Arg] end",
  "    U = lists:map(fun(K) -> MakeProperArgs(K) end, [1,2])","    U+1",
  "    ?szilva+U-?Clause+?Form"]},
     {{?RootDir++"/test1.erl",790,843},["    A","    2"]},
     {{?RootDir++"/test1.erl",459,856},["    MakeProperArgs = fun(Arg)-> [Unit, MaxRank, Arg] end",
  "    U = lists:map(fun(K) -> MakeProperArgs(K) end, [1,2])","    U+1",
  "    ?szilva+U-?Clause+?Form",
  "    ?TrueOrThrow(validate(Key, proplists:get_value(Key, Options, undef)),badarg(Key))",
  "    Man = [arm,head,back]","    Man","    A","    2","    ok"]},
  {{?RootDir++"/test1.erl",916,960},[" case A of\n\t[] -> case B of\n\t\talma -> case T of\n\t\t\t\tkamba -> ggg;\n\t\t\t\t_ -> ok\n\t\t\tend;\n\t\t_ -> ok\n\t\tend;\n\t_ -> nok\nend"]},
  {{?RootDir++"/test1.erl",635,657},["    ?TrueOrThrow(validate(Key, proplists:get_value(Key, Options, undef)),badarg(Key))"]},
  {{?RootDir++"/test1.erl",764,805},["    A"]},
  {{?RootDir++"/test1.erl",19,293},["    io:format(\"Ready\")","    C = A+B","    ?Clause(C,A,B,ok)"]},
  {{?RootDir++"/test1.erl",682,836},["    Man = [arm,head,back]","    Man","    A","    2"]},
  {{?RootDir++"/test1.erl",415,420},["    D = #rec{x = A, y = B}"]},
  {{?RootDir++"/test1.erl",401,413},["    D = #rec{x = A, y = B}"]},
  {{?RootDir++"/test1.erl",631,634},["    ?TrueOrThrow(validate(Key, proplists:get_value(Key, Options, undef)),badarg(Key))"]},
  {{?RootDir++"/test1.erl",601,644},["    ?szilva+U-?Clause+?Form","    ?TrueOrThrow(validate(Key, proplists:get_value(Key, Options, undef)),badarg(Key))"]},
  {{?RootDir++"/test1.erl",456,774},["    MakeProperArgs = fun(Arg)-> [Unit, MaxRank, Arg] end",
  "    U = lists:map(fun(K) -> MakeProperArgs(K) end, [1,2])","    U+1",
  "    ?szilva+U-?Clause+?Form",
  "    ?TrueOrThrow(validate(Key, proplists:get_value(Key, Options, undef)),badarg(Key))",
  "    Man = [arm,head,back]","    Man"]},
  {{?RootDir++"/test1.erl",422,429},["    D"]},{{?RootDir++"/test4.hrl",11,46},error},
  {{?RootDir++"/test4.hrl",67,113},error},{{?RootDir++"/test4.hrl",46,167},error},
  {{?RootDir++"/test4.hrl",225,226},error},{{?RootDir++"/test4.hrl",207,292},error},
  {{?RootDir++"/test4.hrl",369,370},error},{{?RootDir++"/test4.hrl",918,932},error},
  {{?RootDir++"/test4.hrl",916,928},error},{{?RootDir++"/test4.hrl",942,950},error},
  {{?RootDir++"/test4.hrl",910,1029},["    funny(alma)+ets:new(valami)","    d()", "    io:format(\"~p~n\",[kappa])"]},
  {{?RootDir++"/test4.hrl",840,1000},[" case A of\n\t[] -> case B of\n\t\talma -> case T of\n\t\t\t\tkamba -> ggg;\n\t\t\t\t_ -> ok\n\t\t\tend;\n\t\t_ -> ok\n\t\tend;\n\t_ -> nok\nend"]},
  {{?RootDir++"/test4.hrl",678,750},["    A+4*5*3+funny(A+1)"]},{{?RootDir++"/test4.hrl",651,731},["    Man = [arm,head,back]","    Man","    A+4*5*3+funny(A+1)"]},
  {{?RootDir++"/test4.hrl",177,201},["    io:format(\"Ready\")","    C = A+B"]},{{?RootDir++"/test4.hrl",344,370},["    Alma+1"]},
  {{?RootDir++"/test4.hrl",743,782},["    2","    ok"]},{{?RootDir++"/test4.hrl",760,787},["    2","    ok"]}
  ].

sw_metrics_pos_and_res() ->
    [{{?RootDir++"/test2.erl",22,35},error},{{?RootDir++"/test2.erl",1,56},error},
    {{?RootDir++"/test2.erl",19,153},error},{{?RootDir++"/test2.erl",19,225},error},
    {{?RootDir++"/test2.erl",132,251},error},{{?RootDir++"/test2.erl",204,311},error},
    {{?RootDir++"/test2.erl",243,429},error},{{?RootDir++"/test2.erl",258,486},error},
    {{?RootDir++"/test2.erl",1070,1138},error},{{?RootDir++"/test2.erl",29,72},error},
    {{?RootDir++"/test2.erl",255,503},["\nalma(A,B) when A == B ->\n    io:format(\"Ready\"),\n    C = A+B,\n    ?Clause(C,A,B,ok);\nalma(A,B) ->\n    case A of\n        B -> ?Form(B);\n        _ -> ?Clause(A)\n    end,\n    D = #rec{x = A, y = B},\n    D;\nalma(_,4) ->\n    val+a-(mi*2),\n    alma(4,4);\nalma(_,_) ->\n    throw(error).\n"]},
    {{?RootDir++"/test2.erl",528,532},["\nkorte(Alma) ->\n    Alma+1.\n"]},
    {{?RootDir++"/test2.erl",508,534},["\nkorte(Alma) ->\n    Alma+1.\n"]},
    {{?RootDir++"/test2.erl",525,785},["\nkorte(Alma) ->\n    Alma+1.\n",
    "\nfunc() ->\n    MakeProperArgs = fun(Arg)-> [Unit, MaxRank, Arg] end,\n    U = lists:map(fun(K) -> MakeProperArgs(K) end, [1,2]),\n    U+1,\n    ?szilva+U-?Clause+?Form,\n    ?TrueOrThrow(validate(Key, proplists:get_value(Key, Options, undef)),badarg(Key)).\n"]},
    {{?RootDir++"/test2.erl",819,931},["   \nfunny(A) when A == 0 ->\n    Man = [arm,head,back],\n    Man;\nfunny(A) when A == 1 ->\n    A;\nfunny(A) when A == 2 ->\n    2;\nfunny(_) ->\n    ok.\n"]},
    {{?RootDir++"/test2.erl",511,932}, ["\nkorte(Alma) ->\n    Alma+1.\n",
    "\nfunc() ->\n    MakeProperArgs = fun(Arg)-> [Unit, MaxRank, Arg] end,\n    U = lists:map(fun(K) -> MakeProperArgs(K) end, [1,2]),\n    U+1,\n    ?szilva+U-?Clause+?Form,\n    ?TrueOrThrow(validate(Key, proplists:get_value(Key, Options, undef)),badarg(Key)).\n",
    "   \nfunny(A) when A == 0 ->\n    Man = [arm,head,back],\n    Man;\nfunny(A) when A == 1 ->\n    A;\nfunny(A) when A == 2 ->\n    2;\nfunny(_) ->\n    ok.\n"]},
    {{?RootDir++"/test2.erl",49,499}, ["\nalma(A,B) when A == B ->\n    io:format(\"Ready\"),\n    C = A+B,\n    ?Clause(C,A,B,ok);\nalma(A,B) ->\n    case A of\n        B -> ?Form(B);\n        _ -> ?Clause(A)\n    end,\n    D = #rec{x = A, y = B},\n    D;\nalma(_,4) ->\n    val+a-(mi*2),\n    alma(4,4);\nalma(_,_) ->\n    throw(error).\n"]}
].


reset()->
    make_ui_request({reset}).


add(File)->
    make_ui_request({add, File}).


make_ui_request(UIArgs)->
    make_ui_request(UIArgs, []).

make_ui_request(UIArgs, Args)->
    try
        ReqID = ?UI:getid(),
        case ?UI:request(ReqID,UIArgs) of
            deny -> {error, {deny, "The request was denied by the job server."++
                                    "Please, try again later."}};
            ok -> ui_loop(ReqID, Args)
        end
    catch
        exit:Reason ->
            Log = reftest_db_test_framework:open_log(append),
            file:write(Log, io_lib:format("EXIT: ~p <br>",[Reason])),
            file:close(Log),
            %{'EXIT',Reason},
            exit(Reason);
        error:Reason ->
            Log = reftest_db_test_framework:open_log(append),
            file:write(Log, io_lib:format("ERROR: ~p <br>",[Reason])),
            file:close(Log)
    end.

ui_loop(ReqID, Args)->
    receive
        {ReqID, reply, R} ->
            R;
        _ ->
            ui_loop(ReqID, Args)
    end.


%% @spec tree_text(node()) -> Chars
%%       Chars = [char() | Chars]
%% @doc Returns the textual representation of the syntactical subtree that
%% starts at `Top' as a deep list.
tree_text(Top) ->
    [[T#token.prews, T#token.text, T#token.postws]
        || Token <- ?Syn:leaves2(Top),
           #lex{data=T=#token{}} <- [?ESG:data(Token)]].


%% @spec flat_text(node()) -> [char()]
%% @doc Returns the textual representation of the syntactical subtree that
%% starts at `Top'.
flat_text(Top) ->
    lists:flatten(tree_text(Top)).
