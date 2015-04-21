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
%%%
%%% @doc This module implements a tester method which runs predefiniated test
%%%      cases. Before using the tester, it's strongly recommended to run
%%%      `reflib_ui:reset()' function.
%%%
%%%  ==Parameters==
%%%
%%% <dl>
%%%
%%% <dt>{@type {test,Refact_Names::[tuple() | atom() | list()]@}}</dt> <dd>it
%%% specifies which test cases will be run</dd>
%%%
%%% <dt>{@type {skip,Refact_Names::[tuple() | atom() | list()]@}}</dt> <dd>it
%%% specifies which tests will be excepted</dd>
%%%
%%% <dt>{@type {rootdir,path()@}}</dt> <dd>path to test cases folder, default
%%%  value: `../test'</dd>
%%%
%%% <dt>{@type {testcopy,path()@}}</dt> <dd>path to working folder, if it exists
%%% the tester will delete it, also delete this folder when the execution ends
%%% </dd>
%%%
%%% <dt>{@type {logplace,path()@}}</dt> <dd>path to log file, where we can check
%%% the test results, default value: `testresult' </dd>
%%%
%%% <dt>{@type {logname,string()@}}</dt> <dd> name of the log file, default
%%% value: `result.html'</dd>
%%%
%%% <dt>{@type {coverplace,path()@}}</dt> <dd> path to cover files, to get more
%%% info about cover files check the cover module, default value:
%%% `testresult/covers'</dd>
%%%
%%% <dt>{@type {cover,Parameter::list() | all | tr@}}</dt> <dd> `module list'
%%% the cover will be run on the given modules</dd>
%%% <dd> `all' run on all module </dd>
%%% <dd> `tr' run on the modules which used by the tester </dd>
%%%
%%% <dt>{@type {report,Flag:: true | false@}}</dt> <dd> in case of `false'
%%% the catched error, info or warning messages don't appear in HTML log,
%%% default value: `true'</dd>
%%%
%%% <dt>{@type {runall,Flag:: true | false@}}</dt> <dd> in case of `true'
%%% the tester runs all given test cases, unsupported cases are also run,
%%% default value: `false'</dd>
%%%
%%% <dt>{@type {reset,Flag:: true | false@}}</dt> <dd> resets the database
%%% between transformations</dd>
%%%
%%% <dt>{@type {showdesc,Flag:: true | false@}}</dt> <dd> shows the
%%% full descriptions of the test cases as well</dd>
%%%
%%% </dl>
%%%
%%% ==Using examples==
%%%
%%% <dl>
%%%
%%% <dt> test_refact:run(). </dt> <dd> run all test cases </dd>
%%% <dt> test_refact:run([{test,gen}]). </dt>
%%% <dt> test_refact:run([{skip, rename_header}]).</dt> <dd>run all module tests
%%%      except rename_header </dd>
%%% <dt> test_refact:run([{test,[gen,inline_fun]}]). </dt>
%%% <dt> test_refact:run([{test,[gen,{inline_fun,'01'}]}]). </dt>
%%% <dt> test_refact:run([{test,{gen,'01'}},{cover,tr}]). </dt>
%%%
%%% </dl>
%%%
%%% @author Gabor Czini <gczini@gmail.com>

-module(reftest_refact).
-behaviour(gen_event).
-export([run/1,run/0,test_event_helper/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).


-include("test.hrl").
-vsn("$ Rev: 3214 $ ").

-define(TOEXPAND,[file,filename]).
-define(DEBUGMODE,false).
-define(RESULTIN,"out").
-define(ERRORS,"errors").
-define(LOGHTML,"result.html").
-define(MESSAGE,messagesTab).


%% @spec run() -> ok
%% @doc This function runs all test cases.
run() ->
    run([]).

%% @spec run(proplist()) -> ok
%% @doc For the details see the Parameters paragraph above
run(A) ->
    {StartMSec, StartSec, _} = os:timestamp(),
    Original   = proplists:get_value(rootdir, A, filename:join(["..","test", "refact"])),
    Root       = proplists:get_value(testcopy, A, "tmp"),
    Lst        = proplists:get_value(test,A,[]),
    RunAll     = proplists:get_value(runall,A, false),
    NotTodo    = proplists:get_value(skip,A,[]),
    LogPlace   = proplists:get_value(logplace, A, "testresult"),
    CoverPlace = proplists:get_value(coverplace, A,
                                     filename:join(LogPlace,"covers")),
    Coverage   = proplists:get_value(cover, A, []),
    Report     = proplists:get_value(report, A, true),
    Reset      = proplists:get_value(reset, A, false),
    ShowDesc   = proplists:get_value(showdesc, A, false),
    CFGTest    = proplists:get_value(cfgtest, A, false),
    Todo =
        if
            Lst == [] ->
                {ok, Tmp} = file:list_dir(Original),
                [list_to_atom(X)||X<-Tmp]--['.svn'];
            true ->
                make_list(Lst)
        end,
    NeedCopy = make_name(make_list(Todo),Original)
                            --make_name(make_list(NotTodo),Original),
    debug("Needcopy lista:",NeedCopy),

    error_logger:add_report_handler(reftest_refact,[]),
    case Coverage of
        [] -> ok;
        _  -> start_global_coverage(Coverage, NeedCopy)
    end,

    Log=create_log(LogPlace,A,Coverage),
    DetailsTab=create_tab(NeedCopy),
    ets:new(?MESSAGE,[named_table,public,bag]),

    _Result=[do_test(T,Original,Root,Log,LogPlace,DetailsTab,Report,RunAll,Reset,ShowDesc,CFGTest)||
                T<-lists:sort(NeedCopy)],

    case Coverage of
        [] -> ok;
        _  ->
            io:format("~nCoverage:~n"),
            finish_global_coverage(Coverage, Root),
            copy_cover(Root,CoverPlace),
            lH(Log,coverage_text()),
            CoverList=filelib:wildcard(CoverPlace++"/*.cover.html"),
            create_cover_links(Log,CoverList)

    end,

    end_log(Log,DetailsTab),

    delete(Root),
    error_logger:delete_report_handler(reftest_refact),
    ets:delete(?MESSAGE),

    Dir = filename:absname(filename:join([LogPlace, ?LOGHTML])),
    {EndMSec, EndSec, _} = os:timestamp(),
    DiffSec = (EndMSec - StartMSec) * 1000000 + EndSec - StartSec,
    io:format("~nTest(s) finished.~nTesting took ~p seconds~nResults: ~s~n",
              [DiffSec, Dir]).

%% If no `output' environment exists, this function creates one.
create_output_env() ->
    ?Syn:get_env(output) =/= [] orelse ?Syn:create_env(output, original).


%%-----------------COVER--------------------------------------------------------
%% Initialises the coverage engine for all beam files in RefactorErl.
start_global_coverage([], _) ->
    ok;
start_global_coverage(Covers, Mods) ->
    BeamPath = filename:join([".", "lib", "refactorerl", "ebin"]),
    cover:start(),
    case Covers of
        all ->
            io:format("Starting coverage of all modules in ~p ~n", [BeamPath]),
            cover:compile_beam_directory(BeamPath);
        tr  ->
            io:format("Starting coverage of all used transformations~n"),
            Prefix = "reftr_",
            [cover:compile_beam(list_to_atom(Prefix
                    ++ hd(string:tokens(atom_to_list(M),"/")))) || M <- Mods ];
        _   ->
            io:format("Starting coverage of the given modules~n"),
            [cover:compile_beam(Mod) || Mod <- Covers ]
    end,
    io:format("~n Covering the following modules:~n    ~p ~n",
              [cover:modules()]).

%% Ends the module coverage, writes the results to html files.
finish_global_coverage([], _) ->
    ok;
finish_global_coverage(_, CoverDir) ->
    lists:foreach(
        fun(Mod) ->
            ModStr = atom_to_list(Mod),
            CoverPath = filename:join([CoverDir, ModStr ++ ".cover.html"]),
            cover:analyse_to_file(Mod, CoverPath, [html]),
            io:format("Coverage of ~p written to file~n",[Mod])
        end,
        cover:modules()),
    cover:stop().

copy_cover(From,To) ->
    delete(To),
    file:make_dir(To),
    FileList=filelib:wildcard(From++"/*.cover.html"),
    [file:copy(File,filename:join([To,
             filename:basename(File)])) || File <- FileList].

 create_cover_links(Log,CoverList) ->
    [lH(Log,"<a href=\""++filename:join("..",Cover)++"\" target=\"_blank\">"
        ++filename:basename(Cover,".html")++"</a><br>")|| Cover <- CoverList].

%%-------------AUXILIARY FUNCTIONS----------------------------------------------

create_tab(TestCaseList) ->
    TabId=ets:new(testCaseDetails,[]),
    ets:insert(TabId,{allCase,length(TestCaseList)}),
    ets:insert(TabId,{acceptedCase,0}),
    ets:insert(TabId,{caseError,0}),
    ets:insert(TabId,{ceCases,[]}),
    ets:insert(TabId,{refactError,0}),
    ets:insert(TabId,{reCases,[]}),
    ets:insert(TabId,{resultError,0}),
    ets:insert(TabId,{resultCases,[]}),
    ets:insert(TabId,{unsupported,0}),
    ets:insert(TabId,{unsupCases,[]}),
    ets:insert(TabId,{graphError,0}),
    ets:insert(TabId,{graphCases,[]}),
    ets:insert(TabId,{warning,0}),
    ets:insert(TabId,{warningCases,[]}),
    TabId.

%% @private
%% returns a list from the parameter
make_list(L) ->
     case is_list(L) of
        true  -> L;
        false -> [L]
    end.

%% @private
%% makes an atom list from refactoring testcases
make_name([],_Sour) -> [];
make_name([Name|Names],Sour) ->
    case Name of
        {Fold, Case} ->
            NewList=[filename:join([Fold,X])||X<-make_list(Case)],
            [list_to_atom(A)||A<-NewList]++make_name(Names,Sour);
        _            ->
            {Stat,Content}=file:list_dir(filename:join(Sour,Name)),
            case Stat of
                ok    ->
                    NewList=[filename:join(Name,X)
                             ||X<-(make_list(Content)--[".svn","DESC"])],
                    [list_to_atom(A)||A<-NewList]++make_name(Names,Sour);
                error ->
                    io:format("Wrong testfolder name: ~p~n",
                              [filename:join(Sour,Name)]),
                    make_name(Names,Sour)
            end
    end.

debug(Message, Data) ->
    case ?DEBUGMODE of
        true ->
            io:format("DEBUG: ~s -> ~p~n",[Message,Data]);
        _ -> ok
    end.

inc_summary(Tab,Key) ->
    [{_,Value}]=ets:lookup(Tab,Key),
    ets:insert(Tab,{Key,Value+1}).

add_case(Tab,Key,Case) ->
    [{_,Value}]=ets:lookup(Tab,Key),
    ets:insert(Tab,{Key,Value++[Case]}).

test_event_helper({Key, Event}) ->
    ets:insert(?MESSAGE,{Key,Event}).

errorevent2log(_, false) ->
    ok;
errorevent2log(Log, true) ->
    MsgList = ets:tab2list(?MESSAGE),
    MsgList == [] orelse lH(Log,"RefactorERL messages:<br>"),
    [print_out(Log,Msg)|| Msg <- MsgList].


print_out(Log,{errorevent,{Type, _, {_, Format, Data}}}) ->
    case Type of
        warning_msg ->
            lH(Log,"<p><pre>==WARNING REPORT=====<br>"
               ++io_lib:format(Format,Data)++"</pre></p>");
        warning_report ->
            lH(Log,"<p><pre>==WARNING REPORT=====<br>"
               ++io_lib:format("warning ~p: ~p~n",[Format,Data])++"</pre></p>");
        error ->
            lH(Log,"<p><pre>==ERROR REPORT=====<br>"
               ++io_lib:format(Format,Data)++"</pre></p>");
        error_report ->
            lH(Log,"<p><pre>==ERROR REPORT=====<br>"
               ++io_lib:format("error ~p: ~p~n",[Format,Data])++"</pre></p>");
        info_msg ->
            lH(Log,"<p><pre>==INFO REPORT=====<br>"
               ++io_lib:format(Format,Data)++"</pre></p>");
        info_report ->
            lH(Log,"<p><pre>==INFO REPORT=====<br>"
               ++io_lib:format("info ~p: ~p~n",[Format,Data])++"</pre></p>");
        _ ->
            lH(Log,"<p>Unknown report</p>")
    end.

%%-------------------COPY TEST CASE---------------------------------------------
%% @private
%% copies a testcase's files to working directory (default: tool/tmp)
copy(TestCase,From,To)->
        try
            copy_indir(filename:join(From,TestCase),To)
        catch
            A:B->
                io:format("Test files could not be copied from ~p to ~p~n",
                          [From, To]),
                throw({copy_error,A,B})
        end.

%% @private
%% Auxiliary function to copy test folder
copy_files([],_Sour,_Dest) -> ok;
copy_files([File|Files],Sour,Dest) ->
   case filelib:is_dir(filename:join([Sour,File]))of
       true ->
           copy_indir(filename:join([Sour,File]), filename:join([Dest,File])),
           copy_files(Files,Sour,Dest);
       false ->
           file:copy(filename:join([Sour,File]),filename:join([Dest,File])),
           copy_files(Files,Sour,Dest)
   end.

%% @private
%% Support function to copy test folder
copy_indir(From,To) ->
    file:make_dir(To),
    case file:list_dir(From) of
        {ok,F}-> copy_files(F--[".svn"],From,To);
        {error,_} -> io:format("Wrong testcase name: ~p~n",[From])
    end.

%%---------TEST FUNCTION--------------------------------------------------------

%% @private
%% Main testing function
do_test(TestCase,From,To,Log,LogPlace,Details,Report,RunAll,Reset,ShowDesc, CFGTest) ->
    try
        create_output_env(),
        io:format("~n__--==<############# ~s ~s~n",
             [TestCase,lists:duplicate(50-length(atom_to_list(TestCase)),"#")]),
        %copy test case
        case filelib:is_dir(To) of
            true ->
                delete(To);
            false ->
                ok
        end,
        copy(TestCase,From,To),

        %get non prepared arguments from TEST file
        [Ref|_]=string:tokens(atom_to_list(TestCase),"/"),
        RefactName = list_to_atom(Ref),
        {Title,Goal,Term,Desc,Status}=get_args(filename:join(To,"TEST")),
        case RunAll of
            true ->
                StatusChecked = "run every case";
            false ->
                StatusChecked = Status
        end,
        case string:str(string:to_lower(StatusChecked),"unsupported") of
            0             ->
                ets:insert(Details,[
                                    {refact,Ref},
                                    {refactName,RefactName},
                                    {title,Title}, {to,To},
                                    {testcase,TestCase},
                                    {log,Log}, {result,LogPlace},
                                    {cfgtest,CFGTest}]),

                io:format("Title: ~s~n",[Title]),
                case ShowDesc of
                    false ->
                        ok;
                    true ->
                        io:format("Term: ~p~n",[Term]),
                        io:format("Goal: ~s~n",[Goal]),
                        io:format("Description: ~s~n",[Desc])
                end,

                %Prepare arguments, expand filenames with absolute path
                %In ToExpand list are the names of parameters which need to
                %expand with absolute path
                Args=rec_prep_args(Term,To,?TOEXPAND),

                debug("Params",Args),
                debug("RefactName",RefactName),
                debug("Files in tmp folder",filelib:wildcard(To++"/*.?rl")),

                case Reset of
                    true  -> ri:reset();
                    false -> no_reset
                end,

                %Load files to database
                FileList=filelib:wildcard(To++"/*.?rl"),
                Nodes=load_files(FileList),
                debug("Nodes",Nodes),

                %do transformation
                case lists:member('load_error',Nodes) of
                    false ->
                        do_trafo(Args,
                                 filelib:wildcard(filename:join(To,?RESULTIN)
                                                  ++"/*.?rl"),
                                 Details);
                    true  ->
                        inc_summary(Details,caseError),
                        add_case(Details,ceCases,TestCase),
                        io:format("Cannot load source file. Test aborted!"),
                        testcase_header(Log,TestCase,
                                        "<a name=\""++any_to_string(TestCase)
                                        ++"\"><h4>","</h4></a>",Title),
                        lH(Log,"<h6>Cannot load source file. Test aborted!</h6>")
                end,

                %drop files from database
                ?MISC:drop_files(Nodes);
            _ -> inc_summary(Details,unsupported),
                 add_case(Details,unsupCases,TestCase),
                 io:format("Unsupported testcase!~n")
        end,
        errorevent2log(Log,Report),
        ets:delete_all_objects(?MESSAGE)

    catch
        {args_error,A,B} ->
            inc_summary(Details,caseError),
            add_case(Details,ceCases,TestCase),
            io:format("Error while parsing TEST file!~n ~p ~n",[{A,B}]),
             testcase_header(Log,TestCase,
                             "<a name=\""++any_to_string(TestCase)++"\"><h4>",
                             "</h4></a>",""),
             lH(Log,"<h6>Error while parsing TEST file! Reason: <br><pre>"
               ++any_to_string(A)++" "++any_to_string(B)
               ++"</pre><p>Stack contents:<br><pre>"
               ++hE(erlang:get_stacktrace())++"</pre></h6></p>");
        {copy_error,A,B} ->
            inc_summary(Details,caseError),
            add_case(Details,ceCases,TestCase),
            io:format("Error while copying test files!~n ~p ~n",[{A,B}]),
            testcase_header(Log,TestCase,
                            "<a name=\""++any_to_string(TestCase)++"\"><h4>",
                            "</h4></a>",""),
            lH(Log,"<h6>Error while copying test files! Reason: <br>"
               ++any_to_string(A)++" "++any_to_string(B)
               ++"<p>Stack contents:<br><pre>"
               ++hE(erlang:get_stacktrace())++"</pre></h6></p>");
        A:B ->
            inc_summary(Details,refactError),
            add_case(Details,reCases,TestCase),
            io:format("Error in testing!!! Stack contents:~n ~p~n",
                      [erlang:get_stacktrace()]),
            testcase_header(Log,TestCase,
                            "<a name=\""++any_to_string(TestCase)++"\"><h4>",
                            "</h4></a>",""),
            lH(Log,"<h6>Error in testing! Reason: <br><pre>"
               ++any_to_string(A)++" "++any_to_string(B)
               ++"</pre><p>Stack contents:<br><pre>"
               ++hE(erlang:get_stacktrace())++"</pre></h6></p>"),
            errorevent2log(Log,Report)
    end.

do_trafo(Args,FileList,Details) ->
    [{_,TestCase}]  =ets:lookup(Details,testcase),
    [{_,Title}]     =ets:lookup(Details,title),
    [{_,RefactName}]=ets:lookup(Details,refactName),
    [{_,To}]        =ets:lookup(Details,to),
    [{_,Log}]       =ets:lookup(Details,log),
    [{_,Refact}]    =ets:lookup(Details,refact),
    [{_,Result}]    =ets:lookup(Details,result),
    try
        TrResult = reftest_utils:exec_transform(RefactName,Args),
        case filelib:wildcard(To++"/"++?RESULTIN) of
            [] ->
                io:format("The test is not acceptable.~n"),
                case TrResult of
                    {abort,Message} ->
                        inc_summary(Details,acceptedCase),
                        io:format("Transformation aborted. Message: ~s ~n",
                                  [?Error:error_text(Message)]),
                        testcase_header(Log,TestCase,"<h5>","</h5>",Title),
                        lH(Log,"<p> The test is not acceptable.</p>"
                           ++"Transformation aborted. Message:<pre> "
                           ++ hE(?Error:error_text(Message))++"</pre>");
                    {result,_} ->
                        inc_summary(Details,refactError),
                        add_case(Details,reCases,TestCase),
                        io:format("The transformation shouldn't"
                                  ++" work in this test case!~n"),
                        testcase_header(Log,TestCase,"<a name=\""++any_to_string(TestCase)++"\"><h4>","</h4></a>",Title),


                        %-----COPY WRONG TESTCASE-------------------------------------------
			copy_wrong_testcase(Result,Refact,TestCase,To),
                        %-------------------------------------------------------------------
                        lH(Log,"<p> The test is not acceptable.</p>"
                           ++"<h6>The transformation shouldn't"
                           ++" work in this test case!</h6><p><a href=\""
                           ++any_to_string(filename:join([?ERRORS,TestCase]))
                           ++"\" target=\"_blank\">Test case's directory </a></p>");

                    {exit, ExitInfo} ->
                        inc_summary(Details,refactError),
                        add_case(Details,reCases,TestCase),
                        io:format("Transformation exited. Message: ~s ~n",
                                  [lists:flatten(io_lib:write(ExitInfo))]),
                        testcase_header(Log,TestCase,
                                        "<a name=\""++any_to_string(TestCase)
                                        ++"\"><h4>","</h4></a>",Title),
                        lH(Log,"<p>The test is not acceptable.</p>"
                         ++"<h6>Transformation exited. Message: "
                         ++ hE(lists:flatten(io_lib:write(ExitInfo)))++"</h6>");
                    Any ->
                        inc_summary(Details,refactError),
                        add_case(Details,reCases,TestCase),
                        io:format("The result that the transformation gave (although it should have failed) was:~n    ~p~n", [Any]),
                        testcase_header(Log,TestCase,
                                        "<a name=\""++any_to_string(TestCase)
                                        ++"\"><h4>","</h4></a>",Title),
                        lH(Log,"<p>The test is not acceptable.</p>"
                           ++"<h6>Internal error! Message: <pre>"
                           ++hE(any_to_string(Any))
                           ++"</pre></h6>")
                end;
            _ ->
                io:format("The test is acceptable. Diff test result:~n"),
                diff_files(FileList,Details)
        end
    catch
	_A:syntax_error ->
	    inc_summary(Details,warning),
            add_case(Details,warningCases,TestCase),
            io:format("Syntax tree differ can not compare the results.~n"
                      "Please compare them manually!~n"
		      "You can find the files linked into the HTML log.~n"),
            testcase_header(Log,TestCase,
                            "<a name=\""++any_to_string(TestCase)++"\"><h3>",
                            "</h3></a>",Title),
	    %-----COPY WRONG TESTCASE-------------------------------------------
	    copy_wrong_testcase(Result,Refact,TestCase,To),
            %-------------------------------------------------------------------
            lH(Log,"<h6>Syntax tree differ can not compare the results.<br>"
               ++"Please compare them manually!<br>"
	       ++"<a href=\""++any_to_string(filename:join([?ERRORS,TestCase]))
               ++"\" target=\"_blank\">Test case's directory </a>"
               ++"<p>Stack contents:<br><pre>"
               ++hE(erlang:get_stacktrace())++"</pre></h6></p>");
        A:B ->
            inc_summary(Details,refactError),
            add_case(Details,reCases,TestCase),
            io:format("Error in executing the transformation!~n"
                      " exception ~p:~p,~n"
                      " Stack contents:~n ~p~n",
                      [A,B,erlang:get_stacktrace()]),
            testcase_header(Log,TestCase,
                            "<a name=\""++any_to_string(TestCase)++"\"><h4>",
                            "</h4></a>",Title),
            lH(Log,"<h6>Error in executing the transformation! Reason:<br>"
               ++any_to_string(A)++":"++any_to_string(B)
               ++", <p>Stack contents:<br><pre>"
               ++hE(erlang:get_stacktrace())++"</pre></h6></p>")
    end.

%%---------DIFFENTIAL TESTING---------------------------------------------------

diff_files([],Details) ->
    [{_,TestCase}]  =ets:lookup(Details,testcase),
    [{_,Title}]     =ets:lookup(Details,title),
    [{_,Log}]       =ets:lookup(Details,log),
    [{_,CFGTest}]   =ets:lookup(Details,cfgtest),
    CFGRES  = case CFGTest of
                   true -> ?CFGPROP:graph_cfg_errors();
                   _    -> []
              end,               
    case (?GRAPHPROP:graph_prop_errors() 
             ++ ?DATAFLOWPROP:graph_dataflow_errors() 
               ++ CFGRES) of
        []   ->
            inc_summary(Details,acceptedCase),
            io:format("No difference between source files!~n"),
            testcase_header(Log,TestCase,"<h5>","</h5>",Title),
            lH(Log,"<p>The test is acceptable. Diff test result:</p>"
	       ++"<p>Accepted. No difference between source files!</p>");
       
	Errs ->
	    Res = check_missing_funlref(Errs),
	    if
		Res == true ->
		    inc_summary(Details,warning),
		    add_case(Details,warningCases,TestCase),
		    io:format("Inconsistence: ~p~n",[Errs]),
		    testcase_header(Log,TestCase,
				    "<a name=\""++any_to_string(TestCase)
				    ++"\"><h3>","</h3></a>",Title),
		    lH(Log,"<p>The test is acceptable. Diff test result:</p>"
		      ++"<p>Accepted. No difference between source files!</p>"
		      ++"<p>BUT, the result graph or CFG graph is not consistent!</p>"
		      ++"<pre>" ++ io_lib:format("~p",[Errs])
		      ++"</pre>");
		true ->
		    inc_summary(Details,graphError),
		    add_case(Details,graphCases,TestCase),
		    io:format("Inconsistence: ~p~n", [Errs]),
		    testcase_header(Log,TestCase,
				    "<a name=\""++any_to_string(TestCase)
				    ++"\"><h4>",
				    "</h4></a>",Title),
		    lH(Log,"<p>The test is acceptable. Diff test result:</p>"
		      ++"<p>Accepted. No difference between source files!</p>"
		      ++"<p>BUT, the result graph or CFG graph is not consistent!</p>"
		      ++"<pre>" ++ io_lib:format("~p", [Errs])
		      ++"</pre>")
	    end
    end;
diff_files([File|Files],Details) ->
    [{_,TestCase}]  =ets:lookup(Details,testcase),
    [{_,Title}]     =ets:lookup(Details,title),
    [{_,To}]        =ets:lookup(Details,to),
    [{_,Log}]       =ets:lookup(Details,log),
    [{_,Refact}]    =ets:lookup(Details,refact),
    [{_,Result}]    =ets:lookup(Details,result),
    try
        reftest_syntree_differ:run([
                                 {file1,File},
                                 {file2,filename:join(
                                     [To,filename:basename(File)])},
                                 {expimpcollect,true}]),
        diff_files(Files,Details)
    catch
        {file_error,FileName,Stat,Message} ->
            inc_summary(Details,caseError),
            add_case(Details,ceCases,TestCase),
            io:format("File parsing error in ~p, ~n message: {~s,~s}",
                      [filename:absname(FileName),Stat,Message]),
            print_file(FileName),

            testcase_header(Log,TestCase,
                            "<a name=\""++any_to_string(TestCase)++"\"><h4>",
                            "</h4></a>",Title),

           %-----COPY WRONG TESTCASE--------------------------------------------
	    copy_wrong_testcase(Result,Refact,TestCase,To),
           %--------------------------------------------------------------------

            lH(Log,"<p>The test is acceptable. Diff test result:</p>"
               ++"<h6>File parsing error in "++any_to_string(filename:absname(FileName))
               ++", message:<pre>{"++any_to_string(Stat)++","
               ++any_to_string(Message)++"}</pre></h6>");
          {diff,A,B,Pos,Type} ->
            inc_summary(Details,resultError),
            add_case(Details,resultCases,TestCase),
            io:format("Difference in row number ~p between ~p types."
                      " Values:~n   ~p~nand~n   ~p~n",[Pos,Type,A,B]),
            testcase_header(Log,TestCase,
                            "<a name=\""++any_to_string(TestCase)++"\"><h4>",
                            "</h4></a>",Title),

           %-----COPY WRONG TESTCASE--------------------------------------------
	    copy_wrong_testcase(Result,Refact,TestCase,To),
           %--------------------------------------------------------------------

            lH(Log,"<p>The test is acceptable. Diff test result:</p>"
               ++"<h6>Difference in row number "++any_to_string(Pos)
               ++" between "++any_to_string(Type)++" types. Values:<pre> "
               ++hE(any_to_string(A))++"</pre> and <pre>"
               ++hE(any_to_string(B))++"</pre>"
               ++"Affected files:</h6><p><a href=\""
               ++any_to_string(filename:join([?ERRORS,TestCase,
                                              filename:basename(File)]))
               ++"\" target=\"_blank\">Transformation result </a><br><a href=\" "
               ++any_to_string(filename:join([?ERRORS,TestCase,?RESULTIN,
                             filename:basename(File)]))
               ++"\" target=\"_blank\">Expected result </a></p>");
        {length_error,File1,File2} ->
            inc_summary(Details,resultError),
            add_case(Details,resultCases,TestCase),
            io:format("The result file's length (~p) and the original~n file's"
                      " lentgh (~p) isn't equal!~n",[File1,File2]),
            testcase_header(Log,TestCase,
                            "<a name=\""++any_to_string(TestCase)++"\"><h4>",
                            "</h4></a>",Title),

            %-----COPY WRONG TESTCASE-------------------------------------------
	    copy_wrong_testcase(Result,Refact,TestCase,To),
            %-------------------------------------------------------------------

            lH(Log,"<p>The test is acceptable. Diff test result:<p>"
               ++"<h6>The result file's length and the original file's length"
               ++" isn't equal!<br>Affected files:</h6><p><a href=\""
               ++any_to_string(filename:join([?ERRORS,TestCase,
                                              filename:basename(File1)]))
               ++"\" target=\"_blank\">Transformation result </a><br><a href=\" "
               ++any_to_string(filename:join([?ERRORS,TestCase,?RESULTIN,
                             filename:basename(File1)]))
               ++"\" target=\"_blank\">Expected result </a></p>")
    end.


check_missing_funlref([]) -> false;
check_missing_funlref([H|T]) ->
    if
	element(1,H) == missing_funlref -> true;
	true -> check_missing_funlref(T)
    end.

%% Prints the text of the file to the standard output.
%% All lines are prefixed with 4 spaces.
print_file(FileName) ->
    case file:read_file(FileName) of
        {ok, Contents} ->
            ContentsWSpaces = re:replace(Contents, "\n", "\n    ", [{return, list}, global]),
            io:format("~nThe contents of ~s:~n~n    ~s~n", [FileName, ContentsWSpaces]);
        _ ->
            io:format("~nThe contents of ~s cannot be read~n", [FileName])
    end.

%% Copy the wrong testcase's folder from temporary working directory
%% to ?ERRORS directory
copy_wrong_testcase(Result,Refact,TestCase,To) ->
    PathToRefact=filename:join([Result,?ERRORS,Refact]),
    PathToTestCase=filename:join([Result,?ERRORS,TestCase]),
    case filelib:is_dir(PathToRefact) of
        true ->
            ok;
        false ->
            file:make_dir(PathToRefact)
    end,
    file:make_dir(PathToTestCase),
    copy_indir(To,PathToTestCase).

%%----------ARGUMENT MANAGING FUNCTIONS-----------------------------------------

get_args(File)->
    try
        Parsed      = reftest_utils:parse_testfile(File),
        Title       = proplists:get_value('Title',Parsed),
        Goal        = proplists:get_value('Goal',Parsed),
        ParamStr    = proplists:get_value('Parameters',Parsed),
        Doc         = proplists:get_value('Documentation',Parsed),
        Status      = proplists:get_value('Status',Parsed,""),
        {ok,Param} = reftest_utils:string_to_term(ParamStr),
        {Title,Goal,Param,Doc,Status}
    catch
        A:B ->
            throw({args_error,A,B})
    end.

rec_prep_args(PropList,_To,[])->
    PropList;
rec_prep_args(PropList,To,[H|T]) ->
    rec_prep_args(prep_args(PropList,To,H),To,T).

prep_args(PropList,To,What) ->
    case proplists:get_value(What,PropList) of
        undefined ->
            PropList;
        FileName ->
            {ok,Path}=file:get_cwd(),
            proplists:delete(What,PropList)
                ++[{What,filename:join([Path,To,FileName])}]
    end.

%%-------LOAD & DROP FILES------------------------------------------------------

load_files(Files) ->
    FileNodes = [load_file(File) || File <- Files],
    ?Graph:save(before_transformation),
    FileNodes.

load_file(File) ->
    {_,Path}=file:get_cwd(),
    case ?FileMan:add_file(filename:join(Path,File)) of
        {file, Node}    -> Node;
        {error, _} -> load_error
    end.

%drop_files/1 removed to referl_misc module


%%--------DELETE FOLDER---------------------------------------------------------

%% @private
%% Delete tmp folder after running a testcase
delete(Folder) ->
    case file:list_dir(Folder) of
        {ok,[]} ->
            file:del_dir(Folder);
        {ok, Files} ->
            [rec_delete(Folder,File)||File<-Files],
            file:del_dir(Folder);
        _ -> ok
    end.

%% @private
%% Auxiliary function to delete a directory's contents
rec_delete(Folder,File) ->
    Name = filename:join(Folder,File),
    case filelib:is_dir(Name) of
        true ->
            delete(Name);
        false ->
            file:delete(Name)
    end.

%%--------------LOG FUNCTIONS---------------------------------------------------

create_log(LogP,Params,Coverage) ->
    delete(LogP),
    file:make_dir(LogP),
    file:make_dir(filename:join(LogP,?ERRORS)),
    {_, FileSource} = file:open(filename:join(LogP,?LOGHTML), [append,raw]),
    Date=erlang:localtime(),
    lH(FileSource,head(Date,Params)),
    case Coverage of
        [] ->
            ok;
        _ ->
            lH(FileSource,"<br><a href=\"#COVERAGE\">Coverage<a>")
    end,
    debug("file",filename:join(LogP,?LOGHTML)),
    debug("Hol file",file:get_cwd()),
    FileSource.

end_log(FileSource,Details) ->
    SummaryData = ets:tab2list(Details),
    All         = proplists:get_value(allCase,SummaryData,"NoData"),
    Accept      = proplists:get_value(acceptedCase,SummaryData,"NoData"),
    CaseError   = proplists:get_value(caseError,SummaryData,"NoData"),
    RefactError = proplists:get_value(refactError,SummaryData,"NoData"),
    ResultError = proplists:get_value(resultError,SummaryData,"NoData"),
    Unsupported = proplists:get_value(unsupported,SummaryData,"NoData"),
    GraphError  = proplists:get_value(graphError,SummaryData,"NoData"),
    Warnings    = proplists:get_value(warning,SummaryData,"NoData"),
    io:format("~nSummary:~n All testcases: ~p~n"
              ++"Accepted testcases: ~p~n"
              ++"Errors in test input: ~p~n"
              ++"Errors in refactoring: ~p~n"
              ++"Errors in results: ~p~n"
              ++"Errors in graph: ~p~n"
              ++"Unsupported testcases: ~p~n"
	      ++"Warnings: ~p~n",
              [All,Accept,CaseError,RefactError,ResultError, GraphError,
                  Unsupported,Warnings]),
    lH(FileSource,
       summary_text(All,Accept,CaseError,RefactError,
                    ResultError,Unsupported,GraphError,Warnings,Details)),
    ets:delete(Details),
    file:close(FileSource).

lH(FileSource, Data)->
        file:write(FileSource,Data).

any_to_string(X) ->
    case io_lib:deep_char_list(X) of
        true  -> lists:flatten(X);
        false -> io_lib:format("~p",[X])
    end.

%% @private
%% HTML escape a character string or other term
hE(Data)->
    String = any_to_string(Data),
    EscapeChar = fun($&) -> "&amp;";
                    ($<) -> "&lt;" ;
                    ($>) -> "&gt;" ;
                    (C) when (C/=$\n) and (C<32) ->
                            "&#" ++ integer_to_list(C) ++ ";";
                    (C)  -> [C]    end,
    lists:flatmap(EscapeChar, String).

testcase_header(Log,Data,TagStart,TagEnd,Title)->
    lH(Log,
            TagStart ++
            "Test case name: " ++ any_to_string(Data) ++ "<br/>" ++
            "Test case title: " ++ any_to_string(Title) ++
            any_to_string(TagEnd)).

num_to_string(Num) ->
    if
        Num<10 ->
            "0"++any_to_string(Num);
        true -> any_to_string(Num)
    end.

head({{Year,Month,Day},{Hour,Min,_Sec}},Params)->
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
h3{
    background-color: #FFFF00;
    padding-left: 1em;
    font-size: 19px;
}
h4{
    background-color: #FF0000;
    padding-left: 1em;
    font-size: 19px;
}
h5{
    background-color: #00FF00;
    padding-left: 1em;
    font-size: 19px;
}
h6{
  color: #FF0000;
  font-size: 15px;
}
h7{
  color: #00FF00;
  font-size: 15px;
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
</style>
<title>Test results</title>
<link rel=stylesheet type=\"text/css\" href=stylesheet.css title=EDoc></head>
<body bgcolor=white>
<a name=\"TOP\">
<h2 class=indextitle>Test log</h2>
</a>
<p>
Last running: "++num_to_string(Year)++"."++num_to_string(Month)
++"."++num_to_string(Day)++" "++num_to_string(Hour)++":"++num_to_string(Min)
++"<br>
Parameters of last running: "++any_to_string(Params)++
"</p><a href=\"#SUMMARY\">Summary<a>".

coverage_text() ->
    "<a name=\"COVERAGE\">
    <h2 class=indextitle>Coverage</h2>
    </a>".

summary_text(All,Accept,CaseError,RefactError,ResultError,Unsupported,GraphError,Warnings,Details) ->
    SummaryData = ets:tab2list(Details),
    UnsupCases  = proplists:get_value(unsupCases,SummaryData,"NoData"),
    CeCases     = proplists:get_value(ceCases,SummaryData,"NoData"),
    ReCases     = proplists:get_value(reCases,SummaryData,"NoData"),
    ResultCases = proplists:get_value(resultCases,SummaryData,"NoData"),
    GraphCases = proplists:get_value(graphCases,SummaryData,"NoData"),
    WarningCases = proplists:get_value(warningCases,SummaryData,"NoData"),
    "<a name=\"SUMMARY\">
    <h2 class=indextitle>Summary</h2>
    </a>
    Number of all testcases: "++any_to_string(All)++"<br>
    <h7>Accepted testcases: "++any_to_string(Accept)++"</h7><br>"
    ++color_with_caselist("Errors in test inputs: ",CaseError,CeCases,true)
    ++color_with_caselist("Errors in refactorings: ",RefactError,ReCases,true)
    ++color_with_caselist("Errors in results: ",ResultError,ResultCases,true)
    ++color_with_caselist("Graph errors: ",GraphError,GraphCases,true)
    ++color_with_caselist("Unsupported testcases: ",Unsupported,UnsupCases,false)
    ++color_with_caselist("Testcases with warning: ",Warnings,WarningCases,true)
    ++
    "<p>
     <a href=\"#TOP\">Back to the top</a>
     </p>
     </body>
     </html>".

color_with_caselist(Text,Num,Cases,Link) ->
    case Num of
        0 ->
            "";
        _ ->
            case Link of
                true ->
                    "<h6>"++Text++any_to_string(Num)++"<br>"
                    ++"List of these cases:<br>"
                    ++list2string_link(Cases)++"</h6>";
                false ->
                    "<h6>"++Text++any_to_string(Num)++"<br>"
                    ++"List of these cases:<br>"++list2string(Cases)++"</h6>"
            end
    end.

list2string_link([])    -> "";
list2string_link([A|B]) ->
    "<a href=\"#"++any_to_string(A)++"\">"++any_to_string(A)++"</a>; "
    ++list2string_link(B).

list2string([])    -> "";
list2string([A|B]) ->
    any_to_string(A)++"; "++list2string(B).

%-------GEN EVENT CALLBACK FUNCTIONS-------------------------------------------

init([]) ->
    {ok, []}.

handle_event(Event, State) ->
    test_event_helper({errorevent, Event}),
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
