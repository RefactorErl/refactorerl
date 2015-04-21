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

%%% @doc This module performs differential testing between RefactorErl and
%%% Wrangler. The same operations are performed with the two tools and the
%%% results are compared.
%%%
%%% All operations are made on the same temporary directory, which is given by
%%% the caller of the test.
%%% Random refactorings are generated based on this directory, so the filenames
%%% in the description of the refactorings will have this path. When the
%%% refactorings are performed, the source code in this directory is given to
%%% RefactorErl and Wrangler. Neither the refactoring generator, nor RefactorErl
%%% (when performing the refactoring), nor Wrangler knows about any other
%%% directory.
%%%
%%% Tested with the Wrangler version from the RefactorErl trunk and
%%% Erlang version R12B-3.
%%%
%%% Example usage:
%%% fun()->F=fun(F)->test_difftest:test("/home/user/erl/diff/suite1",
%%%  "/home/user/erl/diff/result", [print_se,{test_count,10}]),
%%%  F(F)end,F(F)end().

%%%
%%% Status: generation for rename_function, rename_var, rename_mod,
%%%         move_fun, extract function, tuple_funpar
%%%         and generalize function seems to be working.
%%%         Operation is stable, but more error handling is needed.
%%%         Simple additional features could still be  implemented.
%%%         Unit testing would be helpful in the future.
%%%         More testing is needed.
%%%
%%% @todo implement all trivial features mentioned in the first proplist
%%% @todo  add option for using whitespace-precise diffing
%%% @todo  persistent cache
%%% @todo an option for replaying failed tests
%%% @todo convert the record to a proplist
%%% @todo incorporate syntree_differ and os_diff
%%% @todo add summary table at the end with refactorings and denies
%%% @todo :wishlist decompose large functions
%%% @todo :wishlist abstract out trivial copy-pasted sections
%%% @todo :wishlist do a pedantic review
%%% @todo :wishlist integrate with test case minimization
%%%        (reduce transitional output to illustrated example cases done debug)
%%% @todo :wishlist think about reorganizing a bit to test_gen
%%% @todo :wishlist Support other options of saveconfig: application
%%%       directories and include directories.
%%% TODO: @see test_gen for further todos!
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>
%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>

-module(reftest_difftest).
-vsn("$Rev: 9568 $").
-export([test/1]).

%-vsn("$Rev: 9568 $").

-include("test.hrl").

 -define(D(S,P),io:format("DEBUG: ~s ~p~n",[S,P])).
%-define(D(X,Y),if X==Y -> ok; true -> ok end).

-define(Guard_TO,2000). % timeout in ms for guards to run

%%% @type tool() = referl | wrangler.
%%%
%%% Represents one of the refactoring tools.

%%% @type ref_result() = {success, Graph::egraph()} | {error, Reason}.
%%%
%%% Represents the result of a refactoring transformation.
%%% If the transformation was successful, it also contains the graph of the
%%% resulted program. If it was not, it contains the reason of the failure.

%%% @type ref_result2() = success | {error, Reason}.
%%%
%%% Similar to ref_result, but does not contain the graph.

%%% @type ct_env() = #ct_env{original_dir = string(),
%%%                    file_list = [string()],
%%%                    work_dir = string(),
%%%                    temp_dir = string(),
%%%                    output = writable_device(),
%%%                    tool1 = tool(),
%%%                    tool2 = tool(),
%%%                    tool_fun1 = ref_fun(),
%%%                    tool_fun2 = ref_fun(),
%%%                    tool_dir1 = string(),
%%%                    tool_dir2 = string(),
%%%                    print_dot = bool(),
%%%                    print_se = bool()}.
%%%
%%% The environment of the differential testing.
%%%
%%% Fields: @see {@link test/4}.

-record(ct_env, {original_dir, file_list, work_dir, temp_dir, output, tool1,
                 tool2, tool_fun1, tool_fun2, tool_dir1, tool_dir2, print_dot,
                 print_se, inwarns, inmods}).

%%% @type failure_reporter() =
%%%           ((ref_desc(), ref_result2(), ref_result2(), ct_env()) ->
%%%            string()).
%%%
%%% A function that can be used to report the failures.
%%% It is called only when a failure has happened.
%%%
%%% The function can assume that the WorkDir/tool_dir1 directory and
%%% WorkDir/tool_dir2 directory contain the result of the transformation of the
%%% two tools. The reporter function has to delete or move somewhere these
%%% directories.

%%% ============================================================================
%%% Interface

%% @spec test(proplist()) ->
%%           [{ref_desc(), ref_result2(), ref_result2()}]
%%
%% @doc Tests the refactoring tools against each other and returns the failing
%% test cases.
%%
%% `Suite' is the directory that contains the source code that will be
%% used for the test.
%% `SuiteDir` is a directory that contains suites.
%% `WorkDir' is the directory which will be used as a working directory.
%% The standard failure reporter will use this directory to store the results.
%% If `WorkDir' does not exits, it will be created.
%%
%% Main options:
%% <ul>
%% <li>`suitedir::string()': default is `"diff_suites"'</li>
%% <li>`suite::string()':    default is undefined</li>
%% <li>`workdir::string()':  default is `"diff_result"'</li>
%% <li>`compile::bool()':    default is `true'</li>
%% <li>`comment::bool()':    default is `true'</li> @todo
%% <li>`whitespace::bool()': default is `false'</li> @todo
%% <li>`refac::('all'|[atom()])':    default is `all'</li>
%% <li>`guard::atom()':      default is `guard'</li>
%% <li>`saveargs::bool()':   default is `false'</li> @todo
%% <li>`defect::bool():'     default is `false'</li>
%% <li>`correct::bool()':    default is `true'</li>
%% <li>`regress::bool()':    default is `false'</li>
%% </ul>
%%
%% Other options:
%% <ul>
%%     <li>`log_output::output_ref()': it specifies where the `fwrite'
%%         messages about the execution should be printed.
%%         The default is `stdout'.</li>
%%     <li>`test_count::natural()': number of tests to perform.
%%         The default is 10.</li>
%%     <li>`rds_to_test::ref_desc()':
%%         The test cases specified here will be executed instead of random test
%%         cases. If specified, `test_count' will be irrelevant.
%%         The default is `undefined.'</li>
%%     <li>`tools::[tool(), tool()]':
%%         which tools to run. The default is `[referl, wrangler]'.</li>
%%     <li>`collect_failures::bool()': if a test case fails, it will continue.
%%         The default is `true.'</li>
%%     <li>`failure_reporter::failure_reporter()': the function that is used to
%%         report the failures.
%%         It is {@link failure_reporter/4} by default.</li>
%%     <li>`print_dot::bool()': if `true', a dot will be printed at each test
%%         case.
%%         It is `true' by default.</li>
%%     <li>`print_se::bool()': if `true', an `s' or `e' character will be
%%         printed after each refactoring execution. An `s' means that the
%%         transformation was successful, `e' means that the tool denied it.
%%         case.
%%         It is `false' by default.</li>
%%     <li>`failure::[both]|[one]|[both, one]': specifies which cases should be
%%         reported. If `one' is in the list, then the cases when only one of
%%         the tools succeeded to perform the transformation will be reported.
%%         If `both' is in the list, then the cases when both tools succeeded to
%%         perform the transformation but their graphs are different will be
%%         reported.
%%         It is `[both, one]' by default.</li>
%% </ul>
%%
%% Example:
%% ```
%% test_difftest:test([{suitedir,"/home/erlang/calculator_suites"},
%%                     {workdir,"/home/erlang/tmp"}]).
%% '''
%%
test(Args0) ->
    Args = ?MISC:proplist_merge_def(Args0, test_default()),

    [LogOut,WorkDir,[Tool1,Tool2],Dot,SE] =
        ?MISC:pgetu([log_output,workdir,tools,print_dot,print_se], Args),
    {W, C} = fopen(LogOut),

    {ToolDir1, ToolDir2} =
        case Tool1 == Tool2 of
            true ->
                {atom_to_list(Tool1) ++ "1",
                 atom_to_list(Tool2) ++ "2"};
            false ->
                {atom_to_list(Tool1),
                 atom_to_list(Tool2)}
        end,

    case filelib:is_file(WorkDir) of
        false ->
            ok = filelib:ensure_dir(WorkDir),
            ok = file:make_dir(WorkDir);
        true ->
            ok
    end,

    Env = #ct_env{
      work_dir = WorkDir,
      temp_dir = filename:join([WorkDir, "temp"]),
      output = W,
      tool1 = Tool1,
      tool2 = Tool2,
      tool_fun1 = refactorer(Tool1),
      tool_fun2 = refactorer(Tool2),
      tool_dir1 = ToolDir1,
      tool_dir2 = ToolDir2,
      print_dot = Dot,
      print_se = SE},

    Suites =
        case ?MISC:pget([suitedir,suite],Args) of
            [[Dir],[]] ->
                case file:list_dir(Dir) of
                    {ok,FL} ->
                        [filename:join(Dir,F) || F <- FL];
                    _ ->
                        exit("Suite dir " ++ Dir ++ " does not exist!")
                end;
            [_,Suite1] ->
                Suite1
            end,
    case Suites of
        [] ->
            exit("No suite available");
        _ ->
            ok
    end,
    R =
        [try
             test(W,Suite,Env,Args)
         catch
             error:E ->
                 io:format("Exception in suite ~p:~n ~p~n, ~p~n",
                           [Suite,E,erlang:get_stacktrace()]),
                 error
         end
         || Suite <- Suites],
    fclose(C),
    R.

%% @spec test_default() -> proplist()
%%
%% @doc Returns the default options for {@link test/4}.
test_default() ->
    [{log_output, stdout},
     {test_count, 50},
     {rds_to_test, undefined},
     {tools, [referl, wrangler]},
     {collect_failures, true},
     {failure_reporter, fun failure_reporter/5},
     {print_dot, true},
     {print_se, true}, %false
     {failure, [both, one]},
     {suitedir, "diff_suites"},
     {workdir, "diff_result"},
     {refac, all},
     {defect, true},
     {correct, true},
     {regress, false},
     {compile,true},
     {guard,guard}].

test(W, Suite, Env0, Opts)->
    {ok,FileList=[_|_]} = file:list_dir(Suite),
    [RDsToTest,Collect,Compile,Guard] =
        ?MISC:pgetu([rds_to_test,collect_failures,compile,guard],Opts),

    TempDir = Env0#ct_env.temp_dir,

    io:format(" (clear_files: "),
    reftest_utils:clear_files(),
    io:format("ok)~n"),

    ok = reftest_utils:del_rec_if_exists(TempDir),
    ok = reftest_utils:copy_rec(Suite, TempDir),

    {IMod,IWarn} =
        case GW = all_compile(TempDir,Compile,Guard) of
            false ->
                throw("suite " ++ Suite ++ " does not compile");
            _ ->
                GW
        end,

    Env = Env0#ct_env{
            inmods = IMod,
            inwarns = IWarn,
            original_dir = Suite,
            file_list = FileList},

    RefDescList =
        case RDsToTest of
            undefined ->
                fwrite(W,"Generating the refactoring descriptions...~n"),
                reftest_gen:gen_refac_list(
                    [{suite,TempDir}, {file_list,FileList}|
                     ?MISC:pcopy([test_count,correct,defect,refac,guard],
                                 Opts)]);
            _ ->
                RDsToTest
        end,

    ok = reftest_utils:del_rec(TempDir),
    fwrite(W, "Performing the refactoring transformations...~n"),

    Result =
        %% acc: the list of failing test cases
        lists:reverse(
          ?MISC:partfold(
             fun (RD, FailedList) ->
                     test_core(RD, FailedList, Env, Opts)
             end, [], RefDescList)),

    fwrite(W, "~nTesting finished.~n", []),
    Fails = length(Result), N = length(RefDescList),
    F=false, T=true,
    {Str,Par} = case {Fails==0, Collect} of
        {T,_} -> {"All tests passed (~w).~n", [N]};
        {F,F} -> {"Failure found.~n", []};
        {F,T} -> {"~w failed out of ~w tests.~n", [Fails, N]}
    end,
    fwrite(W, Str, Par),
    Result.

test_core(RD={{Refac,Args},_}, FailedList,
          #ct_env{original_dir = OriginalDir,
               work_dir = WorkDir,
               temp_dir = TempDir,
               output = W,
               tool_fun1 = ToolFun1,
               tool_fun2 = ToolFun2,
               print_se = PrintSE,
               print_dot = PrintDot} = Env,
          Opts) ->

    case PrintDot of
%        true -> fwrite(W, "~n.");
        true -> fwrite(W, "~n(~w(~p)).~n", [Refac,Args]);
        false -> ok
    end,

    [CollectFailures,FailureReporter,Failure] =
        ?MISC:pgetu([collect_failures,failure_reporter, failure],Opts),

    ok = reftest_utils:del_rec_if_exists(TempDir),

    ok = reftest_utils:copy_rec(OriginalDir, TempDir),
    R1 = try_refac(ToolFun1, {Refac,Args}, Env, Opts),
    case PrintSE of
        true -> print_se(W, R1);
        false -> ok
    end,
    ToolDir1 = filename:join([WorkDir, "tool1"]),
    ok = reftest_utils:del_rec_if_exists(ToolDir1),
    ok = file:rename(TempDir, ToolDir1),

    ok = reftest_utils:copy_rec(OriginalDir, TempDir),
    R2 = try_refac(ToolFun2, {Refac,Args}, Env, Opts),
    case PrintSE of
        true -> print_se(W, R2);
        false -> ok
    end,
    ToolDir2 = filename:join([WorkDir, "tool2"]),
    ok = reftest_utils:del_rec_if_exists(ToolDir2),
    ok = file:rename(TempDir, ToolDir2),

    Result = cmp(R1, R2, Failure),
    RR1 = del_graph(R1),
    RR2 = del_graph(R2),

    case Result of
        true ->
            ok = reftest_utils:del_rec(filename:join([WorkDir, "tool1"])),
            ok = reftest_utils:del_rec(filename:join([WorkDir, "tool2"])),
            {next, FailedList};
        false ->
            io:format("~n",[]),
            Dir = FailureReporter(RD, RR1, RR2, Env, Opts),
            Next =
                case CollectFailures of
                    true  -> next;
                    false -> stop
                end,
            {Next, [{RD, RR1, RR2, Dir}|FailedList]}
    end.

%% @spec (ref_desc(), ref_result2(), ref_result2(), ct_env(), [Opt]) ->
%%           ok
%%
%% @doc This function can create the failure reports about the failing test
%% cases. It has the type {@link failure_reporter()}.
%%
%% @todo decompose, abstract
failure_reporter(RD={{Refac,Args},{Short,Long}}, R1, R2,
                 #ct_env{original_dir=OriginalDir,
                         work_dir=WorkDir,
                         temp_dir=TempDir,
                         output=W,
                         tool1=Tool1,
                         tool2=Tool2,
                         tool_dir1=ToolDir1,
                         tool_dir2=ToolDir2}, Opts) ->

    [CreateRegression] = ?MISC:pgetu([regress],Opts),

    DifDir = next_dir(WorkDir),
    Dir = filename:join(WorkDir, DifDir),
    ok = file:make_dir(Dir),

    case CreateRegression of
        true ->
            {SaferAccept,SaferDir0} =
                case {Tool1,Tool2} of
                    {referl,_} ->
                        {R2,"tool2"};
                    {_,referl} ->
                        {R1,"tool1"}
                end,
            SaferDir = filename:join(WorkDir,SaferDir0),

            Regs = filename:join([WorkDir,"regression", atom_to_list(Refac)]),
            ok = filelib:ensure_dir(filename:join(Regs,".")),
            RegName = next_dir(Regs,2),
            RegPath = filename:join(Regs, RegName),
            ok = filelib:ensure_dir(RegPath),
            ok = reftest_utils:copy_rec(OriginalDir, RegPath),
            Doc =
                case SaferAccept of
                    success ->
                        Out = filename:join(RegPath,"out"),
                        ok = reftest_utils:copy_rec(SaferDir, Out),
                        "The transformation is accepted and committed.";
                    _ ->
                        "The transformation must be denied."
                end,

            SkipAtoms = [create_new_file, has_side_effect, recognize, file],
            RArgs0 = [T || T={K,_} <- Args, not lists:member(K,SkipAtoms)],
            File =
                case ?MISC:pgetu([file],Args) of
                    [Name] -> [{file,filename:basename(Name)}];
                    [] -> []
                end,
            RArgs = File ++ RArgs0,

            {ok, TestFile} = file:open(filename:join(RegPath,"TEST"), [write]),
            io:format(TestFile,
                      "Title: ~s~n"
                      "Goal: ~s~n"
                      "Parameters: ~p.~n"
                      "Status: unsupported~n"
                      "Documentation: ~s~n",
                      [Short, Long, RArgs, Doc]),
            ok = file:close(TestFile);
        _ -> ok
    end,

    ok = reftest_utils:copy_rec(OriginalDir,
                             filename:join([Dir, "orig"])),
    ok = file:rename(filename:join([WorkDir, "tool1"]),
                     filename:join([Dir, ToolDir1])),
    ok = file:rename(filename:join([WorkDir, "tool2"]),
                     filename:join([Dir, ToolDir2])),

    DescFileName = filename:join(Dir, "description"),
    {ok, DescFile} = file:open(DescFileName, [write]),

    ToolName1 = get_tool_name(Tool1),
    ToolName2 = get_tool_name(Tool2),

    io:format(DescFile,
              "Title: ~s~n"
              "Description: ~s~n"
              "Transformation: ~p~n"
              "Parameters: ~p.~n"
              "~s: ~p~n"
              "~s: ~p~n"
              "~nDiffTestSpacer: ================~n~n"
              "Selection:~n ",
              [Short, Long, Refac, Args, ToolName1, R1, ToolName2, R2]),

    [F,PosL,PR,Num] = ?MISC:pget([file,position,posrange,number],Args),
    FileName =
        case F of
            [AbsFileName] ->
                [_|RelFileName] = AbsFileName -- TempDir,
    %% Creating the relative file name (e.g. "a/b.erl") from the absolute file
    %% name (e.g. "/test/workdir/temp/a/b.erl") and the name of the temporary
    %% directory (e.g. "/test/workdir/temp").
                filename:join([OriginalDir, RelFileName]);
            _ -> ""
        end,
    case {PosL,PR,Num} of
        {_,[{Pos1,Pos2}],_} ->
            print_with_mark({Pos1,Pos2}, FileName, DescFile);
        {[Pos],_,[_N]} ->
            print_with_mark({Pos,Pos}, FileName, DescFile); %% @todo
        {[Pos],_,_} ->
            print_with_mark({Pos,Pos}, FileName, DescFile);
        _ -> io:format("~p ~p~n",[FileName,{PosL,PR,Num}])
    end,
    ok = file:close(DescFile),

    fwrite(W,
                  "Failure found:~n~n"
                  "    Refactoring description:~n    ~p~n~n"
                  "    The result of ~s:~n    ~p~n~n"
                  "    The result of ~s:~n    ~p~n~n"
                  "    Failure written in directory ~s.~n",
                  [RD, ToolName1, R1, ToolName2, R2, DifDir]),
    DifDir.

get_tool_name(referl) ->
    "RefactorErl";
get_tool_name(wrangler) ->
    "Wrangler";
get_tool_name(Tool) ->
    atom_to_list(Tool).

%% @spec print_se(writable_device(), ref_result()) -> ok
%%
%% @doc Prints `"s"' if the refactoring was successful and prints `"e"'
%% otherwise.
print_se(W, RefResult) ->
    Text =
        case RefResult of
            {success, _} ->
                "s";
            _ ->
                "["++io_lib:print(RefResult)++"]" %% "e"
        end,
    fwrite(W, Text).

%% @spec del_graph(RefResult::ref_result()) -> ref_result2()
%%
%% @doc If `RefResult' contains a graph, deletes it and returns the `success'
%% atom. In other words, this function removes the graph from the `RefResult'.
del_graph({success, G}) ->
    ets:delete(G),
    success;
del_graph(Error) ->
    Error.

%% @spec cmp(ref_result(), ref_result(), Failure) -> bool()
%%           Failure = [(both | one)]
%%
%% @doc Examines whether the given graphs are the same.
%% `Failure' is either `[both]', `[one]' or `[both, one]'.
cmp(R1, R2, Failure) ->
%    ?D("@cmp R1,R2=",[R1,R2]),
    case {R1, R2} of
        {{refuse, _}, {refuse, _}} ->
            true;
        {{refuse, _}, {success, _}} ->
            not lists:member(one, Failure);
        {{success, _}, {refuse, _}} ->
            not lists:member(one, Failure);
        {{success, G1}, {success, G2}} ->
            case lists:member(both, Failure) of
                true ->
                    reftest_utils:test_graphs(G1, G2, idchecker());
                false ->
                    true
            end;
        _ ->
            false
    end.

idchecker() -> fun
    (_A, _A) ->
%        ?D("id(A,A)",A),
        true;
    (_A=#lex{type=token,data=T1=#token{type=Type}},
     _B=#lex{type=token,data=T2=#token{type=Type}})->
%        ?D("id(LTA,LTB)",[A,B]),
        ?Token:get_value(T1) == ?Token:get_value(T2);
    (_A=#form{type=Type,tag=Tag,pp=PP},
     _B=#form{type=Type,tag=Tag,pp=PP}) ->
%        ?D("id(FOA,FOB)",[A,B]),
        true;
    (_A=#file{type=Type,path=Path},
     _B=#file{type=Type,path=Path}) ->
%        ?D("id(FLA,FLB)",[A,B]),
        true;
    (_A, _B) ->
%        ?D("id(A,B)",[A,B]),
        false
    end.

%%% ============================================================================
%%% Failure reporting

fclose(File) ->
    cl_out:close(File).

fopen(File) ->
    cl_out:open(File).

fwrite(File,Text) ->
    cl_out:fwrite(File,Text).

fwrite(File,Format,Text) ->
    cl_out:fwrite(File,Format,Text).

% @spec ({natural(),natural()}, string(), io_device()) -> ok
print_with_mark(Pos, FileName, OutDev) ->
    {ok, F} = file:open(FileName, [read]),
    print_with_mark_core(Pos, 1, file:read(F, 1), F, OutDev),
    file:close(F),
    io:format(OutDev, "~n",[]).

print_with_mark_core(_, _, eof, _, _) ->
    ok;
print_with_mark_core(Pos={Pos1,Pos2}, I, {ok, Char}, F, OutDev) ->
    case I of
        Pos1 -> io:format(OutDev, "<<<", []);
        _ -> ok
    end,
    io:format(OutDev,"~s",[Char]),
    case Char of
        [$\n] ->
            io:format(OutDev," ",[]);
        _ ->
            ok
    end,
    case I of
        Pos2 -> io:format(OutDev, ">>>", []);
        _ -> ok
    end,
    print_with_mark_core(Pos, I + 1, file:read(F, 1), F, OutDev).

next_dir(Dir) ->
    next_dir(Dir,4).

next_dir(Dir,Width) ->
    {ok, FileNames} = file:list_dir(Dir),
    Max =
        lists:foldl(
            fun(FileName, Max) ->
                    I =
                        try
                            list_to_integer(FileName)
                        catch
                            _:_ -> 0
                        end,
                    case I > Max of
                        true -> I;
                        false -> Max
                    end
            end, 0, FileNames),
    ?MISC:integer_to_list(Max + 1, Width).

%%% ============================================================================
%%% Refactoring

%% @spec try_refac(ref_fun(), ref_desc(), Env::ct_env(), [Opt]) -> ref_result()
%%
%% @doc Tries to execute the given refactoring in the `Env.temp_dir' directory
%% with the files in `Env.file_list'.
try_refac(Refactorer, Args,
          E=#ct_env{temp_dir=Dir, inwarns=IWarn, inmods=IMod}, Opts) ->
    R = try_refac_(Refactorer, Args, E),
    [Compile,Guard] = ?MISC:pgetu([compile,guard],Opts),
    case {R,Compile} of
        {{success, _},true} ->
            case all_compile(Dir,Compile,Guard) of
                false ->
                    {fault,{no_compile,R}};
                {Mod,Warn} ->
                    IK = proplists:get_keys(IMod),
                    MK = proplists:get_keys(Mod),
                    Errs =
                        case ?MISC:pdel(MK--IK,Mod) =:=
                            ?MISC:pdel(IK--MK,IMod) of
                            true  -> [];
                            false -> [guard_mismatch]
                        end ++
                        case Warn=:=IWarn of
                            true  -> [];
                            false -> [warn_mismatch]
                        end,
                    case Errs of
                        [] -> R;
                        _  -> {fault,{Errs,R}}
                    end
            end;
        _ -> R
    end.

all_compile(_,false,_)->
    {[],[]};

all_compile(Dir,true,Guard)->
    Files = filelib:wildcard(filename:join(Dir,"*.erl")),
    CompAggr =
        fun(File,{M,W})->
                case compiles(File,Guard) of
                    {true,M2,W2} -> {next,{[M2,M],[W2,W]}};
                    _            -> {stop,false}
                end
        end,
    case ?MISC:partfold(CompAggr, {[],[]}, Files) of
        false ->
            false;
        {M,W} ->
            NoInd = [{Report,Msg} || {_Line,Report,Msg} <- ?MISC:flatsort(W)],
            {?MISC:flatsort(M),NoInd}
    end.

compiles(File,Guard)->
    Opt =[{warn_format,2}, warn_export_all, warn_export_vars, warn_shadow_vars,
         warn_obsolete_guard, warn_unused_import, bin_opt_info, verbose,
         return], % strong_validation
    case compile:file(File,Opt) of
        {ok,Mod,Warnings} ->
            GuardRes = {Mod,run_guard(Mod,Guard)},
            _ = file:delete(filename:join(io_lib:print(Mod),".beam")),
            {true,GuardRes,Warnings};
        {error,Errors,Warnings} ->
            {false,[Errors,Warnings]}
    end.

run_guard(Mod,Guard)->
    process_flag(trap_exit, true),
    PID =
        spawn_link(
          fun() ->
                  R =
                      try
                          apply(Mod,Guard,[])
                      catch
                          error:undef ->
                              case hd(erlang:get_stacktrace()) of
                                  [Mod,Guard,[]] ->
                                      missing;
                                  _ ->
                                      false
                              end;
                            _ -> false
                      end,
                  exit(R)
          end),
    R =
        receive
            {'EXIT',PID,Bool} ->
                Bool
        after
            ?Guard_TO ->
                to
        end,
    PID ! kill,
    R.

try_refac_(Refactorer, Args,
          #ct_env{file_list=FileList, temp_dir=Dir}) ->
%{_, [{file,_F}|_]}=RD
    Result = Refactorer(Dir, FileList, Args),
%    cat(F),
    case Result of
        {ok, _}      ->
            {success, reftest_utils:dir_to_graph(Dir, FileList)};
        {error,
         "Wrangler failed to perform this refactoring, " ++
         "please report error to erlang-refactor@kent.ac.uk."} ->
            {fault,Result};
        {error,Txt=[_|_]} ->
            {refuse,lists:flatten(Txt)};
        {error,{error,_}} ->
            {fault,Result};
        {error,{exit,_}} ->
            {fault,Result};
        {error, {abort,Txt=[_|_]}} ->
            {refuse, lists:flatten(Txt)};
        {error, {abort,Msg={_,_,_}}} ->
            {refuse, lists:flatten(?Error:error_text(Msg))};
        _ ->
            {unknown, Result}
% dismiss reject decline deny refuse
    end.

%cat(F) ->
%    {ok,Binary} = file:read_file(F),
%    Str = binary_to_list(Binary),
%    ?D("resulting file:",Str).

%% @spec refactorer(tool()) -> ref_fun()
%%
%% @doc Returns a function that will perform `RD' using `Tool'.
refactorer(Tool) ->
    case Tool of
        referl   -> refactorerl_refactorer();
        wrangler -> wrangler_refactorer()
    end.

%% @spec refactorerl_refactorer() -> ref_fun()
%%
%% @doc Returns a function that will perform `RD' using RefactorErl.
refactorerl_refactorer() ->
    fun (Dir, FileList, {Tr, Args}) ->
            reftest_utils:add_files(Dir, FileList),
            {error,"Configuration saved."} =
                trap(fun() -> ?UI:saveconfig([], [], original) end,
                     fun(_)-> get_ui_res() end),
            Result2 =
                try
                    Mod = list_to_atom("reftr_" ++ atom_to_list(Tr)),
                    ?Transform:do(Mod, Args),
                    Result = ?Transform:wait(),
                    case Result of
                        {result, _} ->
                            {ok, ok};
                        Error ->
                            {error, Error}
                    end
                catch
                    _:Error2 -> {error, Error2} % @todo handle 'undef'
                end,
            reftest_utils:clear_files(),
            Result2
    end.

%% @spec wrangler_refactorer() -> ref_fun()
%%
%% @doc Returns a function that will perform `RD' using Wrangler.
wrangler_refactorer() ->
    fun (Dir, _FileList, {Tr, Args}) ->
            try
                Mod = referltr2wrmod(Tr),
                Res =
                    trap(fun()  -> ?UI:wr_transform(Mod,Args) end,
                         fun(_) -> get_ui_res() end),
                ToDel = [ "local_side_effect_tab" ] ++
                        filelib:wildcard("*.swp",Dir),
                [file:delete(filename:join(Dir,F)) || F <- ToDel],
                Res
            catch
                _:Error -> {error, Error} % @todo handle 'undef'
            end
    end.

%% @spec (refac()) -> wrrefac()
referltr2wrmod(Tr) ->
    case proplists:lookup(Tr,rerl2wr_map()) of
        none     -> throw("Target Wrangler module not found");
        {Tr,Mod} -> Mod
    end.

%% @spec () -> [{refac(),wrrefac()}]
rerl2wr_map() ->
    [{rename_fun,  rename_function},
     {rename_var,  rename_variable},
     {rename_mod,  rename_module},
     {move_fun,    move_function},
     {extract_fun, extract_function},
     {tuple_funpar,tuple_funargs},
     {gen,         generalize_function}].

trap(Action, Done)
        when is_function(Action,0), is_function(Done,1) ->
    process_flag(trap_exit, true),
    PID = spawn_link(fun() ->
      try
        referl_ui_evsend:start(self()),
        receive installed -> installed end,
        Answer =
          try
            Action()
          catch
            E       -> Done(E),
                       throw(E);
            error:E -> Done({error,E}),
                       erlang:error(E)
          end,
        exit({ok,Done(Answer)})
      catch
        error:E2 -> exit({error,E2});
        E2       -> exit({throw,E2})
      end
    end),
    receive
        {'EXIT',PID,{throw,Exc}} -> throw(Exc);
        {'EXIT',PID,{error,Exc}} -> erlang:error(Exc);
        {'EXIT',PID,{ok,Reason}} -> Reason
    end.

get_ui_res() ->
    receive
        {status,"Finished"} ->
            {ok,ok};
        {status,S} ->
            {error,S};
        {error,E} ->
            {error,E};
        _ ->
            get_ui_res()
    end.
