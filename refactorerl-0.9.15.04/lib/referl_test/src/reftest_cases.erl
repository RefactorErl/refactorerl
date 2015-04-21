%% -*- coding: latin-1 -*-

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

%%% @doc This module implements auto-testing on the basis of test cases.
%%%      This is the part of the automatic tester that contains the basic
%%%      infrastructure.
%%%
%%%      Note that while this module is fully operational, it will gradually
%%%      be superseded by the experimental 'test_refact' when that one starts
%%%      to stabilize and improves in features.
%%% @author Roland Kiraly <kiralyroland@inf.elte.hu>
%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>


%% TODO:wishlist refactor until completely gone
%% TODO: Testing on windows (diff)

-module(reftest_cases).
-vsn("$ Rev: 2179 $ ").
-include("test.hrl").
%% TODO: include "tester_defs.hrl"

-define(S, string).
-define(U, reftest_utils).
-define(PRE,"reftr_").
-define(DIRNEW,"testout").
-define(DIROLD,"orig").
-define(DIRREF,"out").
-define(DIFF,"diff").
-define(TESTF,"TEST").
-define(STORAGE, 'backup.graph').
-define(Tester, tester).
-define(Logname,"Test cases log").

-define(TIME(Func,Fun,ArgL),
        reftest_utils:print_time(fun()->Func end,Fun,ArgL)).
-define(TIME3(Mod,Fun,Arg),
        reftest_utils:print_time(fun()->apply(Mod,Fun,Arg)end,
                              {Mod,Fun},Arg)).

-export([run/0,run/1,storage/1,lH/2,find/2,test_event_helper/1]).

%% Represents a string that has already been HTML escaped.
-record(html,{s}).

%%% @spec run()-> atom()
%%%
%%% @doc Interface of the auto-tester.
%%% This version of the function can execute all test cases with
%%% default settings.
%%% Default elements of the `Args::proplist()':
%%%
%%% `rootdir'  The root of the test cases
%%% `testcopy' Working copy of the test cases
%%% `outfile'  This `html' file contains the result of the tests
%%% `spec'     List of the subfolders contains the names of the test cases.
%%%            The list contains a names of the tests as a
%%%            `string()' for example ["inline_fun","gen"]. The empty
%%%            list implies all available test cases.
%%% `test'     An alias for `spec'.
%%% `nospec'   List contains module names. This module tests will be out of
%%%            the testing. `nospec' option is opposite of the `spec'.
%%% `skip'     An alias for `nospec'.
%%% `graph'    This option spec. that the graph will created or not.
%%%            It can be `true' or `false'. If it is `true' the graph will
%%%            created into a `jpg' file named `agraph' and `bgraph'
%%%            (graph before and after the transformation). This is
%%%            slow! (in this version this option is not available)
%%% `nodiff'    True or false. Use or not use the OS. diff.
%%%            These options can changed by using `run/1' where the
%%%            only parameter of the function is a proplist contains
%%%            the options.
%%%            E.g.:  run([{spec,[inline_fun,gen]},{...},...]).
%%% `cover'    A list of modules that are to be coverage analysed.
%%%            As a default (if `tr' is specified or the option is not present),
%%%            only the transformations actually used are covered.
%%%            If `all' is specified, all files of RefactorErl are covered.

%%% ============================================================================
%%% Interface functions

run()->
   run([]).

unused_functions() ->
    try
        NullFun = fun() -> never end,
        no_match = NullFun(),

        exec_transform(1,2),
        storage_(1),
        files2graph(1)
    catch
        _:_ -> ok
    end.

run(A0) when is_list(A0)->
    A = A0 ++ ?MISC:pcopy([{test,spec},{skip,nospec}],A0),
    _F = storage({main, "first.graph"}),

    createtable(?Tester),
    mnesia:wait_for_tables([?Tester],infinity),
    OriginalR = proplists:get_value(rootdir, A, filename:join(["..","test"])),
    RootR = proplists:get_value(testcopy, A,
                          filename:join(["..","autotested"])),
    OutFileR = proplists:get_value(outfile, A,
                          filename:join(["..","autotested","testlog.html"])),
    [Original,Root,OutFile] = lists:map(fun ?File:abs_path/1,
                                        [OriginalR,RootR,OutFileR]),
    Graph = proplists:get_value(graph, A, false),
    Diff =  proplists:get_value(nodiff, A, false),
    add(?Tester, {nodiff, Diff}),

    error_logger:add_report_handler(reftest_msg_logger,[]),

    add(?Tester, {graph, Graph}),
    _OkOrError = file:delete(OutFile),
    Sp   = lists:flatten(proplists:get_all_values(spec, A)),
    Spec = [atom_to_list(E) || E <- Sp ],
    Ne  = lists:flatten(proplists:get_all_values(nospec, A)),
    Neg = [atom_to_list(N) || N <- Ne ],

    case Spec of
      [] -> Modules = getDirs(Original, ".svn");
       _ -> Modules = Spec
    end,
    %%Select test modules by the parameter list
    Mods = Modules--Neg,

    Coverage = proplists:get_value(cover, A, tr),
    start_global_coverage(Coverage, Mods),

    [copy(Original,Root,Mod) || Mod <- Mods],

    EndResult =
        try execute(Root, {execute, Mods, Neg}) of
            {Result, finished} ->
                try
                    %io:format("Result::~p~n",[Result]),
                    lH(OutFile,head()),
                    ?TIME(examine(Root,OutFile,Result),
                          examine,[Root,OutFile,"{{{RESULTS OMITTED}}}"]),
                    lH(OutFile,"</body></html>"),
                    {_, _} = deletetable(?Tester),
                    storage({restore,"first.graph"}),
                    storage({cleanup,[?STORAGE,"worker.graph"]}),
                    ok
                catch
                    X:Y ->
                        io:format("  Tester - Exam exception : ~p:~p~n" ++
                                  "stack trace: ~p~n",
                                  [X,Y,erlang:get_stacktrace()]),
                        error
                end;
            error -> error
        catch
            error:Res ->
                io:format("  Tester - Error : ~p~n Stack: ~p~n",
                          [Res, erlang:get_stacktrace()]),
                error;
            throw:Res ->
                io:format("  Tester - Exception : ~p~n", [Res]),
                error;
            exit:Res ->
                io:format("  Tester - Exited : ~p~n", [Res]),
                throw(exit);
            X:Y ->
                io:format("  Tester - Unhandled exception : ~p:~p~n" ++
                          "stack trace: ~p~n",
                          [X,Y,erlang:get_stacktrace()]),
                error
        end,

    finish_global_coverage(Coverage, Root),
    unused_functions(),
    EndResult.

start_global_coverage(A,B) ->
    ?TIME(reftest_utils:start_global_coverage(A,B),start_global_coverage,[A,B]).

finish_global_coverage(A,B) ->
    ?U:finish_global_coverage(A,B).

hE(X) ->
    ?U:hE(X).

concat(A) ->
    ?U:concat(A).

concat(A,B) ->
    ?U:concat(A,B).

%%% ============================================================================
%%% Tester and test manipulation functions


%% @private
execute(Root, {execute, Mods, _Neg})->
    %%SubDirs read all subfolders of the transformation test.
    SubDirs = [getSub({Root, Mod}) || Mod <- Mods],
    %%Run the transformation on the collected directories.
    case execute(Root,SubDirs,[]) of
        {Finished, ok} ->
            {Finished, finished};
         _ ->
            error
    end.

%% @private
execute(Root,[{Trf, CaseDirs}|Params],Finished) ->
 %%Saves the storage
    TransformModuleName = list_to_atom(Trf),
    _B = storage(backup),
    ActResult = [trytest(Root, TransformModuleName, ActualDir)||
                ActualDir <- CaseDirs],
    execute(Root, Params, Finished++ActResult);
execute(_, [], Finished) ->
    {Finished, ok}.

destread_event() ->
    case destread(?Tester, errorevent) of
        [] -> noevent;
        Ev -> Ev
    end.


%% @private
trytest(Root, TransformModuleName, ActualDir)->

    T = case storage(restore) of
            {atomic, T_} ->
                T_;
            R ->
                throw({storage_restore,R})
        end,
    _S = ?TIME3(mnesia,wait_for_tables,[T,infinity]),
    Dir = createPath([TransformModuleName, ActualDir]),
    case find(?Tester, graph) of
        [true] ->
             ?U:draw(createPath([Root,%TransformModuleName,
                                 Dir,?DIRNEW,"before"]));
        _ -> shift
    end,
    try workOnTest(Root, TransformModuleName, ActualDir) of
       {[Result],LintRes} ->
          Event = destread_event(),
          io:format("~nResult of the transformation: ~p~n  Text:~n  ~s~n",
                    [Result,format(Result)]),

          DMesg = case filelib:is_dir(createPath([Root,Dir,?DIRREF])) of
                      true -> "Transformation should be accepted";
                         _ -> "Transformation should be denied"
                  end,
          io:format("  ~s~n",[DMesg]),
          case find(?Tester, graph) of
              [true] ->
                  ?U:draw(createPath([Root,%TransformModuleName,
                                      Dir,?DIRNEW,"before"]));
                   _ -> shift
          end,
          [{Result, LintRes, Event, Dir}]
    catch
        Mesg ->
            E = {internal_error, Mesg},
            io:format("~nError while transforming :~n ~p~n~n",
                      [format(E)]),
            [{[E], [], destread_event(), Dir}];
        _:Mesg ->
            E = {internal_error, {Mesg,erlang:get_stacktrace()}},
            io:format("~nException while transforming :~n ~p:~n ~p~n~n",
                      [Mesg, erlang:get_stacktrace()]),
            [{[E], [], destread_event(), Dir}]
    end.

%% @private
workOnTest(Root, Transform, Dir)->
    Title = lists:sublist("---=== " ++ createPath([Transform,Dir]) ++ " " ++
                          lists:duplicate(75, $=), 80),
    io:format("~n~s~n", [Title]),
    io:format("run only these test cases with:~n~n    reftest_cases:run([{spec, [~p]}]).~n~n", [Transform]),

    ErlFiles = datafiles(createPath([Root,Transform,Dir]),"*.erl"),
    HrlFiles = datafiles(createPath([Root,Transform,Dir]),"*.hrl"),

    Files = ErlFiles++HrlFiles,
    _Copied = copy2test(createPath([Root,Transform,Dir,?DIRNEW]),Files),
    _Moved  = move2test(createPath([Root,Transform,Dir,?DIROLD]),Files),

    try ?TIME(files2graph(Files),files2graph,[Files]) of
        ok_loaded -> [{ok,graph_is_ready}];
        _ -> ?ESG:finalize()
    catch
      _:_-> throw({ierror, graph_is_not_ready})
    end,

    TEST_path = createPath([Root,Transform,Dir,?TESTF]),
    LintRes  = reftest_cases_lint:do_lint_file(TEST_path,?DIROLD),

    ArgsFromTEST =
        try
            {ok,getArgs(createPath([Root,Transform,Dir]))}
        catch
            _:_ ->
                throw({ierror, bad_incoming_arguments})
        end,

    case ArgsFromTEST of
         {ok,A} ->
             PreparedArgList =
                filepth(A, createPath([Root,Transform,Dir,?DIRNEW]), [file]),
             TransformResult = ?TIME(exec_transform
                                     (Transform, PreparedArgList),
                                     exec_transform,
                                     [Transform, PreparedArgList]),

             try
                 {_, LocalMessageFromTrModule} = TransformResult,
                 ?Error:error_text(LocalMessageFromTrModule),
                 io:format("  Error text from the Transform module:~p~n",
                           [LocalMessageFromTrModule])
             catch
                 _:Message -> [{no_local_message, Message}]
             end;
         _ -> TransformResult = throw({ierror, error_type_is_not_available})
    end,
    {TransformResult, LintRes}.

%% @private
exec_transform(Transform, Args)->
    [reftest_utils:exec_transform(Transform, Args)].

%%% ============================================================================
%%% Diff

%% TODO: move to test_utils

%% @type filename() = string()

%% @doc Calls the operating system "diff" command if it's available, returns
%%      an empty difference otherwise.
%% @spec (filename(),filename()) ->
%%           'equal' | {'diff',#html{}}
diff_plain(File,Tofile)->
    Diffparams = ["-E","-r","-B","-t","-d","-s","-N","-b", %"-y",
                  "--suppress-common-lines","-x","*~",
                  "-x",".svn",File,Tofile],
    {Status,Diff} = ?MISC:os_cmd(?DIFF,Diffparams),
    case Status of
        0   -> equal;
        1   -> {diff,hE(Diff)};
        127 -> {diff,hE("")}; %% when the executable is not found
        _   -> Msg = ["Diff status=", io_lib:write(Status),
                      ", message: ", Diff],
               {diff,hE(lists:flatten(Msg))}
    end.

%% @doc Calls syntree_differ and wraps up the result.
%% @spec (filename(),filename()) ->
%%           'eqtree' | {'difftree',#html{}} | {'treediff_defect',#html{}}
diff_tree(A,B)->
     DirDiff = fun(T) ->
                       {difftree,hE("tree difference found: "++
                                    io_lib:print(T) ++"\n\n")}
               end,
     try
%         DiffArgs = [expimpcollect,{keeporder,false}],
         DiffArgs = [],
         case reftest_syntree_differ:run([{dir1,A},{dir2,B}|DiffArgs]) of
             [] ->
                 eqtree;
             R ->
                 DirDiff(R)
         end
     catch %% DEBUG-only!
         E={dir_diff,_Dir1,_Dir2} ->
             DirDiff(E);
         E={dir_error,_Dir} ->
             DirDiff(E);
%         throw:{file_error,File,Status,Error} ->
%             throw({io_error,"syntree_differ file parsing error"});
         C:E ->
             Stack = erlang:get_stacktrace(),
             Ex  = io_lib:format("~p:~p stack:~n ~p~n",[C,E,Stack]),
             Str = ["Unhandled exception in syntree_differ:\n"
                    " A=", A, "\n"
                    " B=", B, "\n",
                    Ex],
             io:format("~s~n",[lists:flatten(Str)]),
             {treediff_defect,hE(Str)}
     end.

%% @doc This calls both a simple diff and a tree diff.
%%      All result strings are HTML escaped by the called functions.
%% @spec (filename(),filename()) ->
%%           'equal' | {'diff',('eqtree'|'difftree'),#html{}} |
%%           {'treediff_defect',#html{}}
diff_both(File,ToFile)->
    case {diff_plain(File,ToFile), diff_tree(File,ToFile)} of
        {_,P={treediff_defect,_}} ->
            P;
        {equal,eqtree} ->
            equal;
        {equal,{difftree,TD}} ->
            M = #html{s="OS diff: 'equal', treediff: 'diff'<br>"},
            {treediff_defect,concat(M,TD)};
        {{diff,D},eqtree} ->
            {diff,eqtree,D};
        {{diff,D},{difftree,TD}} ->
            {diff,difftree,concat(TD,D)}
    end.

%%% ============================================================================
%%% Graph manipulation functions

%% @private
files2graph(Files)->
    try
        [?FileMan:add_file(createPath([filename:dirname(File),
                                       ?DIRNEW,filename:basename(File)]))
         || File <- Files],
        ok_loaded
    catch
        _:_ -> not_loaded
    end.

%%% ============================================================================
%%% File and subdir manipulation functions

%% @private
createPath(X) ->
    ?U:createPath(X).

%% @private
copy2test(Dir,Files)->
    ok = filelib:ensure_dir(filename:join(Dir,".")),
    [file:copy(File,filename:join(Dir,
               filename:basename(File))) || File <- Files].

%% @private
move2test(Dir,Files)->
    ok = filelib:ensure_dir(filename:join(Dir,".")),
    [file:rename(File,filename:join(Dir,
               filename:basename(File))) || File <- Files].

%% @private
%% @doc This function can create a full path from the name of the testfile
filepth(Args, Path, [Key|KeyList])->
    %try
        Args1 = case proplists:get_value(Key, Args) of
                     undefined -> Args;
                     File      -> proplists:delete(Key,Args)
                                     ++ [{Key,filename:join(Path,File)}]
                end,
   filepth(Args1,Path, KeyList);
filepth(Args, _, []) ->
   Args.

%% @private
datafiles(D, Ext)->
    case filelib:is_dir(D) of
         true -> Files = filelib:wildcard(Ext, D),
                 [filename:join(D,File) || File <- Files];
            _ -> throw({find_files,
                        D++" directory does not exist"})
    end.

%% @private
%% @doc Can read the arguments of the transformation from the TEST files
%%      The result is a proplist.
%% @end
getArgs(SubDir)->
    FileName    = filename:join(SubDir,?TESTF),
    Parsed      = ?U:parse_testfile(FileName),
    ParamStr    = proplists:get_value('Parameters',Parsed),
    {ok,Params} = ?U:string_to_term(ParamStr),
    Params.

%% @private
getDirs(Directory,Bad)->
    case filelib:is_dir(Directory) of
        true -> [NextDir || NextDir <- filelib:wildcard("*",Directory),
                              filelib:is_dir(filename:join(Directory,NextDir)),
                              NextDir /= Bad];
        _ -> throw({?MODULE,
                      Directory++" directory does not exist"})
    end.

%% @private
getSub({Dir, Sub})->
    Fold = filename:join(Dir, Sub),
    {Sub,getDirs(Fold,".svn")}.

% @private
copy(Dir1,Dir2,File) ->
    ?TIME(?U:copy_rec(Dir1,Dir2,File),copy_rec,[Dir1,Dir2,File]).

%%% ============================================================================
%%% Database manipulation functions

%% @private
storage(Arg) ->
    ?TIME(storage_(Arg),storage_,[Arg]).

%% @private
storage_({cleanup, [Name|Names]})->
    File = filename:absname_join(
             mnesia:system_info(directory),Name),
    _OkOrError = file:delete(File),
    storage({cleanup,Names});
storage_({cleanup,[]})-> ok;
storage_({main, Name})->
    BData  = [{name,Name},{max, mnesia:system_info(tables)},
             {allow_remote,true},{ram_overrides_dump,true}],
    catch mnesia:activate_checkpoint(BData),
    File  = filename:absname_join(
             mnesia:system_info(directory),Name),
    catch mnesia:backup_checkpoint(Name, File);
storage_(backup)->
    storage({backup,?STORAGE});
storage_({backup, Name})->
    BData  = [{name,Name},{max, mnesia:system_info(tables)},
             {allow_remote,true},{ram_overrides_dump,true}],
    catch mnesia:activate_checkpoint(BData),
    File  = filename:absname_join(
             mnesia:system_info(directory), Name),
    catch mnesia:backup_checkpoint(Name, File);
storage_(restore) ->
    storage({restore, ?STORAGE});
storage_({restore, Name}) ->
    catchex(50),
    File  = filename:absname_join(
            mnesia:system_info(directory), Name),
    catch mnesia:restore(File,[{recreate_tables, []},
                               {skip_tables, [counter, ?Tester]}]);
storage_({copy, Dir, Name})->
    file:copy(filename:absname_join(mnesia:system_info(directory),Name),
              createPath([Dir,Name])).

%% @doc
%% This is a (partial, but very reliable) workaround for a Mnesia bug.
%% The bug was fixed in 4.4.6.
%% Own Id: OTP-7585 Aux Id: seq11046
%% http://www1.erlang.org/doc/apps/mnesia/notes.html
catchex(T) ->
    receive
        {'EXIT',_,normal}=X -> % TODO: epp?
            io:format(" {{{DEBUG: Mnesia bug workaround: ~p}}} ",[X]),
            catchex(T)
    after
        T -> ok
    end.

%% @private
test_event_helper({Key, Event})->
    add(?Tester,{Key, Event}).

%% @private
createtable(Name)->
    try
        mnesia:create_table(Name,[{type,bag}])
    catch
        _:_ -> notable
    end.
%% @private
add(Name, {Key, Value})->
    try
        mnesia:dirty_write({Name, Key, Value})
    catch
        _:_ -> ok
    end.
%% @private
find(Name,Key)->
    try
        List= mnesia:dirty_read({Name, Key}),
        Value = [Val || {_, _, Val} <-List],
        Value
    catch
        _:_ -> no
    end.

%% @private
destread(Name,Key)->
    try
        [begin
             ok = mnesia:dirty_delete_object(Rec),
             Val
         end ||
            Rec={_,_,Val} <- mnesia:dirty_read({Name, Key})]
    catch
        _:_ -> no
    end.

%% @private
deletetable(Name)->
    try
        ?TIME3(mnesia,delete_table,[Name])
    catch
        _:_ -> nodelete
    end.


%%% ============================================================================
%%% HTML log file functions

%% TODO: don't reopen
%% TODO: move to test_utils

%% @private
lH(_,_Data=[])->
    ok;
lH(File, #html{s=Data}) -> %% TODO: accept only HTML input
    lH(File,Data);
lH(File, Data)->
    add_text_to_file(File, Data).

%% Inserts a string at the end of `File'. It also accepts a deep list.
add_text_to_file(File, DeepText) ->
    {_, FileSource} = file:open(File, [append,raw]),
    ok = file:write(FileSource, lists:flatten(DeepText)),
    ok = file:close(FileSource),
    ok.

%% @private
examine(Root, File, Results)->

    Switcher =
       "<script>
        function switcher(id)
        {
            var elem = document.getElementById(id);
            elem.style.display = (elem.style.display == 'none') ? 'block' : 'none';
        }
        </script>",
    lH(File, Switcher),

    [ examine1(Root, File, Result, LintRes, Event, Fold)
        || [{Result, LintRes, Event, Fold}] <- Results ].

treetr(A) ->
    TrTr = [{eqtree,equal}, {difftree,diff}, {idemtree,idempotent}],
    proplists:get_value(A,TrTr,A).

%% @spec (#html{}) -> #html{}
stdefect(Str) ->
    concat(
      [#html{s="<font color=red>"},
       hE("WARNING: A syntree_differ defect is probable: "),
       Str,
       #html{s="<br></font>"}]).

%% TODO: ? move to test_utils

%% @spec ('deny'|'accept',filename(),filename(),filename()) ->
%%           {atom(),#html{}}
analdiff(ShouldAccept,OLD,REF,NEW) ->
    HEnil = hE(""),
    Idem = case diff_both(OLD,NEW) of
               equal           -> idempotent;
               {diff,eqtree,_} -> idemtree; % idempotent;
               {A1=treediff_defect,_} -> A1;
               _               -> diff
           end,
    case ShouldAccept of
        deny ->
            {Idem,HEnil};
        accept ->
            IdErr = case treetr(Idem) of
                        diff ->
                            HEnil;
                        _ ->
                            stdefect(hE("the expected result and the"++
                                        " original state are the same (" ++
                                        atom_to_list(Idem) ++ ")"))
                    end,
            case diff_both(REF,NEW) of
                equal ->
                    {equal, IdErr};
                {diff,Snd,Diff} ->
                    D2 = case Diff of
                             HEnil ->
                                 Diff;
                             _  ->
                                 concat([#html{s="<h3 class=definition>"++
                                               "Differences:</h3>"++
                                               "<font color=brown><code>"},
                                         %% class=diff
                                         ?U:pre2code(Diff),
                                         #html{s="</code></font>"}])
                         end,
                    case {Idem,Snd} of
                        {diff,_}   -> {Snd,  D2};
                        {_,eqtree} -> {Snd,  concat(IdErr,D2)};
                        _          -> {Idem, D2}
                    end;
                {A2=treediff_defect,M} ->
                    {A2,stdefect(M)}
            end
    end.


%%TODO: factor this function into many smaller ones
examine1(Root, File, Result, LintRes, Event, Fold)->
    {StyleAtom, FormattedResult} = format_with_style(Result),
    OLD = createPath([Root,Fold,?DIROLD]),
    REF = createPath([Root,Fold,?DIRREF]),
    NEW = createPath([Root,Fold,?DIRNEW]),
    ShouldAccept = case filelib:is_dir(REF) of
                       true  -> accept;
                       false -> deny
                   end,
    Accepted = case Result of
                   {result,_} -> accept;
                   {abort,_}  -> deny;
                   {error,_}  -> error;
                   _          -> invalid
               end,
    {EventCount,EventTxt} =
        case Event of
            noevent -> {0,hE("")};
            [_|_]   -> {length(Event),
                        concat([#html{s="<pre>"},
                                concat([hE(format_event(E)) || E <- Event]),
                                #html{s="</pre>"}])};
            _       -> {unknown, hE(io_lib:write(Event))}
        end,
    ShouldDiff = case ShouldAccept of
                     accept -> equal;
                     deny   -> idempotent
                 end,
    DiffRow =
        case find(?Tester, nodiff) of
            [false] ->
                {DiffDisp, DiffTxt} = analdiff(ShouldAccept,OLD,REF,NEW),
                DiffLev = treetr(DiffDisp),
                [["files", ShouldDiff, DiffDisp]];
            _ ->
                DiffLev = ShouldDiff,
                DiffTxt = hE(""),
                []
        end,

    TEST_path = filename:join([Root,Fold,?TESTF]),
    {LintA,LintB} = proplists:split(LintRes,proplists:get_keys(LintRes)),
    LintCat   = LintA ++ LintB,
    ToCounts  = fun([])-> [];
                   ([{A,_}|_]=L) -> integer_to_list(length(L)) ++
                                    " " ++ io_lib:write(A) end,
    CatCounts = string:join(lists:map(ToCounts, LintCat), ", "),
    ShowedR   = [ concat(hE("TESTlint: "++reftest_cases_lint:show_result(R)),
                         #html{s="<br>"})
                  || R <- LintRes ],
    LintMsg   = concat(ShowedR),
%%    [io:format("~s: ~s~n", [TEST_path,M]) || M <- ShowedR],
    CompSheet = ?MISC:transpose(
                   [["" ,"expected", "resultant"],
                    ["TEST", 0, CatCounts],
                    ["events", 0, EventCount],
                    ["outcome", ShouldAccept, Accepted]
                   ] ++ DiffRow),

    IsError = ShouldAccept =/= Accepted orelse EventCount =/= 0
              orelse ShouldDiff =/= DiffLev,
    table_header(File, IsError, Fold, File, TEST_path, ShouldAccept, Accepted, EventCount),


    lH(File, "Original file(s) : <br>"),
    html_print_files(Root, Fold, File, ?DIROLD),

    lH(File, "Output file(s) : <br>"),
    html_print_files(Root, Fold, File, ?DIRNEW),

    case find(?Tester, graph) of
        [true] ->
            P1 = createPath([Root,Fold,?DIRNEW,"before.jpg"]),
            P2 = createPath([Root,Fold,?DIRNEW,"after.jpg"]),
            lH(File,concat([#html{s="<a class=index href=\""},
                            hE(P1),
                            #html{s="\" target=_blank>Graph</a>"++
                                  " before the transformation<br>"}])),
            lH(File,concat([#html{s="<a class=index href=\""},
                            hE(P2),
                            #html{s="\" target=_blank>Graph</a>"++
                                  " after the transformation<br>"}]));
        _    -> shift
    end,
    lH(File,concat(hE(get_title(TEST_path)),#html{s="<br>"})),
    case IsError orelse ShouldDiff =/= DiffLev of  %%until the tree diff is ready
        true -> lH(File,?U:table_to_html(CompSheet, "border=1"));
                 %% HTML escaped by table_to_html
                 %%TODO: color the difference
        _    -> shift
    end,
    lH(File,LintMsg), %%HTML escaped in ShowedR
    lH(File,EventTxt),
    lH(File,concat([#html{s="<span class=\"" ++ atom_to_list(StyleAtom) ++
                          "\">"},
                    hE(FormattedResult),
                    #html{s="</span>"}])),
    lH(File,"<br>"),
    lH(File,DiffTxt),
%    lH(File,"</div>"),
    lH(File,"</td></tr></table>").

%% Prints the header of a table entry.
table_header(File, IsError, UnescFolder, File, UnescTestPath, ShouldAccept, Accepted, EventCount) ->
    Class =
        case IsError of
            true  -> "error";
            false -> "noerror"
        end,

    {Status, BGColor} =
        case {ShouldAccept, Accepted} of
            {accept, accept}  -> {"OK: Accepted", "green"};
            {accept, deny}    -> {"Should have been accepted", "red"};
            {deny,   accept}  -> {"Unexpectedly accepted", "red"};
            {deny,   deny}    -> {"OK: Rejected", "green"};
            {_,      error}   -> {"Error during the transformation", "red"};
            {_,      invalid} -> {"Invalid result", "red"}
        end,

    ErrorReports =
        case EventCount of
            0 -> "";
            1 -> "1 error report";
            N -> ?MISC:format("~p error reports", [N])
        end,

    #html{s=Folder}   = hE(UnescFolder),
    #html{s=TestPath} = hE(UnescTestPath),
    Log =
       ["<table class=table width='100%'>"
        "<tr class='switchtr' onclick=\"switcher('", Folder, File,"')\"><td>"
            "<h3 class='function ", Class, "'>",
                "<span class='switcher'>Click for details</span>",
                "<span class='status' style='background: ", BGColor, "'>", Status, "</span>",
                "<span class='errorrep'>", ErrorReports, "</span>",
                Folder,
            "</h3>"
        "</td></tr>"
        "<tr id='", Folder, File, "' style='display: none'><td>"
            "Test folder: "
            "<a href=\"", Folder, "\" target=_blank>", Folder, "</a>"
            "<br>"
            "<a target=_blank href=\"", TestPath, "\">TEST file</a><br>"
        ],
    add_text_to_file(File, lists:concat(Log)),
    print_file(File, TestPath).



%% Prints a list of files into the html file one at a line,
%% along with their contents.
html_print_files(Root, Fold, File, Dir) ->
    Files = datafiles(createPath([Root,Fold,Dir]),"*"),
    [begin
        lH(File,concat([#html{s="[ <a href=\""},
                         hE(Fl),
                         #html{s="\" target=_blank>"},
                         hE(Fl),
                         #html{s="</a> ]<br>"}
                         ])),
        print_file(File, Fl)
     end
     || Fl <- Files],
    lH(File,"<br>").

print_file(File, FileName) ->
    case file:read_file(FileName) of
        {ok, BinaryTxt} ->
            Txt = binary_to_list(BinaryTxt),
            lH(File,concat([#html{s="<pre class='pre'>"},
                             hE(Txt),
                             #html{s="</pre>"}
                             ]));
        _ ->
            lH(File,concat([#html{s="<pre class='pre'>Cannot open file."}]))
    end.


format_event({error, _, {_, Format, Data}}) ->
    ["\nERROR REPORT:\n", io_lib:format(Format, Data)];
format_event({error_report, _, {_, Type, Report}}) ->
    ["\nERROR REPORT:\n", io_lib:format("  ~p: ~p~n", [Type, Report])];
format_event({warning_msg, _, {_, Format, Data}}) ->
    ["\nWARNING REPORT:\n", io_lib:format(Format, Data)];
format_event({warning_report, _, {_, Type, Report}}) ->
    ["\nWARNING REPORT:\n", io_lib:format("  ~p: ~p~n", [Type, Report])];
format_event(_) -> "".

get_title(File) ->
    {ok, Bin} = file:read_file(File),
    Str = binary_to_list(Bin),
    {match, [{S, L}]} = re:run(Str, "Title:(.|\n[ \t])*", [{capture, first}]),
    string:substr(Str, S+1, L).

%% @private
format(Data)->
    {_, Format} = format_with_style(Data),
    Format.

format_with_style(Data)->
    %%io:format("Data : ~p~n",[Data]),
    case Data of
        {result, _}->
            {finish, "Finished."};
        {abort, Mesg} ->
            {abort, "Transformation aborted. Message: " ++
                    ?Error:error_text(Mesg)};
        {exit, ExitInfo} ->
            {exit, "Transformation exited. Reason: "++
                   lists:flatten(io_lib:write(ExitInfo))};
        {_,{ierror, _ExitInfo}} ->
            {badtest, "Bad TEST file"
                      " - something wrong in the Parameter list."};
        [{internal_error, {try_clause,[{ierror,bad_testfile}]}}] ->
            {badtest, "Bad TEST file"
                      " - something wrong in the Parameter list."};
        {internal_error, {try_clause,[{ierror,bad_testfile}]}} ->
            {badtest, "Bad TEST file"
                      " - something wrong in the Parameter list."};
        {internal_error, IMessage} ->
            {badtest, lists:flatten(io_lib:write(IMessage))};
        {error,batch_logic} ->
            {batch_logic, ">ESG batch logic error"};
        Data ->
            {tr_ok, lists:flatten(io_lib:write(Data))}
    end.


%% @private
head()-> concat([#html{s=
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
\"http://www.w3.org/TR/html4/loose.dtd\">
<html><head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">
<style type=\"text/css\">
body {
    font-family: Verdana, Arial, Helvetica, sans-serif;
        margin-left: .25in;
        margin-right: .2in;
        margin-top: 0.2in;
        margin-bottom: 0.2in;
        color: #000000;
        background-color: #ffffff;
}
h1,h2,h4,h3,h5 {
    margin-left: -0.2in;
}
div.navbar {
    background-color: #add8e6;
    padding: 0.2em;
}
h2.indextitle {
    padding: 0.4em;
    background-color: #add8e6;
}
h3.function,h3.typedecl {
    background-color: #add8e6;
    padding-left: 1em;
}
h4{
    background-color: red;
    padding-left: 1em;
}
h5{
    background-color: green;
    padding-left: 1em;
}
div.spec {
    margin-left: 2em;
    background-color: #eeeeee;
}
a.module,a.package {
    text-decoration:none
}
a.module:hover,a.package:hover {
    background-color: #eeeeee;
}
ul.definitions {
    list-style-type: none;
}
ul.index {
    list-style-type: none;
    background-color: #eeeeee;
}
ul {
    list-style-type: square;
}
table {
    border-collapse: collapse;
}
td {
    padding: 3
}
span.finish {
    color: green;
}
span.abort, span.exit {
    color: red;
}
span.badtest, span.batch_logic {
    background-color: red;
}
tr.switchtr {
    cursor: pointer;
}
span.switcher {
    cursor: pointer;
    margin-right: 3em;
    background: blue;
    color: white;
}
span.status {
    margin-right: 3em;
}
span.errorrep {
    background: black;
    color: white;
    margin-right: 3em;
}
pre.pre {
    background: lightgray;
    margin-left: 2em;
    padding: 1em;
}
</style>
<title>Test results</title>
<link rel=stylesheet type=\"text/css\" href=stylesheet.css title=EDoc></head>
<body bgcolor=white><h2 class=indextitle>"},hE(?Logname),#html{s="</h2>"}]).
% .diff { color: brown; }
