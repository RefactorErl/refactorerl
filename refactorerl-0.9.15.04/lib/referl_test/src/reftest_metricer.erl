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
%%% @author Gabor Czini <gczini@gmail.com>

-module(reftest_metricer).
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
-define(QUERY,["@mod.line_of_code","@mod.num_of_fun","@mod.char_of_code",
	      "@mod.cohesion"]).


%% @spec run() -> ok
%% @doc This function runs all test cases.
run() ->
    run([]).

%% @spec run(proplist()) -> ok
%% @doc For the details see the Parameters paragraph above
run(A) ->
    Original   = proplists:get_value(rootdir, A, filename:join(["..","test"])),
    Root       = proplists:get_value(testcopy, A, "tmp"),
    Lst        = proplists:get_value(test,A,[]),
    NotTodo    = proplists:get_value(skip,A,[]),
    LogPlace   = proplists:get_value(logplace, A, "metric_result"),
    Report     = proplists:get_value(report, A, true),

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

    error_logger:add_report_handler(reftest_metricer,[]),

    Log=create_log(LogPlace,A),
    ets:new(?MESSAGE,[named_table,public,bag]),

    _Result=[do_test(T,Original,Root,Log,Report)||
                T<-lists:sort(NeedCopy)],

    end_log(Log),

    delete(Root),
    error_logger:delete_report_handler(reftest_metricer),
    ets:delete(?MESSAGE),

    io:format("~nFinished.~nYou can see the results"
              " in ~p in ~p directory.~n",[?LOGHTML,LogPlace]).

%% If no `output' environment exists, this function creates one.
create_output_env() ->
    ?Syn:get_env(output) =/= [] orelse ?Syn:create_env(output, original).


%%-------------AUXILIARY FUNCTIONS----------------------------------------------


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

test_event_helper({Key, Event}) ->
    ets:insert(?MESSAGE,{Key,Event}).

errorevent2log(Log,Report) ->
    case Report of
        true ->
            MsgList=ets:tab2list(?MESSAGE),
            if
                MsgList==[] ->
                    ok;
                true ->
                    lH(Log,"RefactorERL messages:<br>"),
                    [print_out(Log,Msg)|| Msg <- MsgList]
            end;
        _ ->
            ok
    end.

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
            io:format("Copying test files...\n"),
            copy_indir(filename:join(From,TestCase),To),
            io:format("Done\n")
        catch
                A:B-> throw({copy_error,A,B})
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
do_test(TestCase,From,To,Log,Report) ->
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
        {Title,_Goal,Term,_Doc,_Status}=get_args(filename:join(To,"TEST")),
               
	io:format("Title: ~s~n",[Title]),
	
	%Prepare arguments, expand filenames with absolute path
	%In ToExpand list are the names of parameters which need to
	%expand with absolute path
	Args=rec_prep_args(Term,To,?TOEXPAND),
	
	debug("Params",Args),
	debug("RefactName",RefactName),
	debug("Files in tmp folder",filelib:wildcard(To++"/*.?rl")),
	
	%Load files to database
	FileList=filelib:wildcard(To++"/*.?rl"),
	Nodes=load_files(FileList),
	debug("Nodes",Nodes),
	
	%do transformation
	case lists:member('load_error',Nodes) of
	    false ->
		testcase_header(Log,TestCase,"<h5>","</h5>",Title),
		
		lH(Log,"<b>Before transformation:</b><br>"),
		ResultsBef = [metric(Log,list_to_atom(lists:nth(2,string:tokens(Mod,"/."))), Query)|| Mod <- filelib:wildcard(To ++ "/*.?rl"), Query <- ?QUERY],
		lH(Log,"Average function complexity: " ++ any_to_string(lists:nth(1,ResultsBef)/lists:nth(2,ResultsBef)/20) ++ "<br>"),

		_TrResult = reftest_utils:exec_transform(RefactName,Args),

	    	lH(Log,"<br><b>After transformation:</b><br>"),
		ResultsAft = [metric(Log,list_to_atom(lists:nth(2,string:tokens(Mod,"/."))), Query)|| Mod <- filelib:wildcard(To ++ "/*.?rl"), Query <- ?QUERY],
		lH(Log,"Average function complexity: " ++ any_to_string(lists:nth(1,ResultsAft)/lists:nth(2,ResultsAft)/20) ++ "<br>");

	    true  ->
		io:format("Cannot load source file. Test aborted!"),
		testcase_header(Log,TestCase,
				"<a name=\""++any_to_string(TestCase)
				++"\"><h4>","</h4></a>",Title),
		lH(Log,"<h6>Cannot load source file. Test aborted!</h6>")
	end,
	
	%drop files from database
	drop_files(Nodes),
   
        errorevent2log(Log,Report),
        ets:delete_all_objects(?MESSAGE)

    catch
        {args_error,A,B} ->
            io:format("Error while parsing TEST file!~n ~p ~n",[{A,B}]),
             testcase_header(Log,TestCase,
                             "<a name=\""++any_to_string(TestCase)++"\"><h4>",
                             "</h4></a>",""),
             lH(Log,"<h6>Error while parsing TEST file! Reason: <br><pre>"
               ++any_to_string(A)++" "++any_to_string(B)
               ++"</pre><p>Stack contents:<br><pre>"
               ++hE(erlang:get_stacktrace())++"</pre></h6></p>");
        {copy_error,A,B} ->
            io:format("Error while copying test files!~n ~p ~n",[{A,B}]),
            testcase_header(Log,TestCase,
                            "<a name=\""++any_to_string(TestCase)++"\"><h4>",
                            "</h4></a>",""),
            lH(Log,"<h6>Error while copying test files! Reason: <br>"
               ++any_to_string(A)++" "++any_to_string(B)
               ++"<p>Stack contents:<br><pre>"
               ++hE(erlang:get_stacktrace())++"</pre></h6></p>");
        A:B ->
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

metric(Log,Mod,Q) ->
    [_,{_,_,Result}] = refusr_sq:run([{positions,none},{output,other}],[{module,Mod}],Q),
    lH(Log,any_to_string(Q) ++ ": " ++ any_to_string(Result)++"<br>"),
    io:format("~p: ~p~n",[Q,Result]),
    Result.
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
    [load_file(File) || File <- Files].

load_file(File) ->
    {_,Path}=file:get_cwd(),
    case ?FileMan:add_file(filename:join(Path,File)) of
        {file, Node}    -> Node;
        {error, _} -> load_error
    end.

drop_files(Nodes) ->
    lists:foreach(fun drop_file/1, Nodes).

drop_file(Node) ->
    try ?File:type(Node) of
        _ -> ?FileMan:drop_file(Node)
    catch
        error:bad_node -> already_dropped
    end.


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

create_log(LogP,Params) ->
    delete(LogP),
    file:make_dir(LogP),
    file:make_dir(filename:join(LogP,?ERRORS)),
    {_, FileSource} = file:open(filename:join(LogP,?LOGHTML), [append,raw]),
    Date=erlang:localtime(),
    lH(FileSource,head(Date,Params)),
    debug("file",filename:join(LogP,?LOGHTML)),
    debug("Hol file",file:get_cwd()),
    FileSource.

end_log(FileSource) ->
    io:format("End"),
    end_text(),
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
    lH(Log,TagStart++" Test case name: "
       ++any_to_string(Data)++any_to_string(TagEnd)),
    if
        Title=="" -> ok;
        true      -> lH(Log,"<p>Test case title: "
                        ++any_to_string(Title)++"</p>")
    end.

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
h3.function,h3.typedecl {
    background-color: #add8e6;
   padding-left: 1em;
}
h4{
    background-color: #FF0000;
    padding-left: 1em;
    font-size: 19px;
}
h5{
    background-color: #add8e6;
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
"</p>".



end_text() ->
    "<p>
     <a href=\"#TOP\">Back to the top</a>
     </p>
     </body>
     </html>".


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
