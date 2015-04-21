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

%%% @doc Unit test for the requests management of the file cache server

-module(reftest_refcore_filecacheserver).
-compile(export_all).

-include("test.hrl").
-define(debug_test,io:format("~p~n",[?LINE])).

-export([seq_test/0, test1/0, test2/0, test3/0, test4/0, test5/0, test6/0, 
         test7/0, para_test/0, test/0]).

-export([get_base/0]).

-define(RootDir, filename:join(["..", "test", "unit_tests", "filecacheserver"])).
-define(WorkDir, filename:join([referl_misc:data_dir(), "tmp", "filecacheserver"])).

-define(TEST, ?WorkDir).
-define(ALTERED, filename:join(?WorkDir, "fibonacci_mod.er")).
-define(ORIGINAL, filename:join(?WorkDir, "fibonacci.erl")).
-define(OVERWRITE, filename:join(?RootDir, "fibonacci.erl")).
-define(READ, fun ?FileCacheMan:get_file_content/1).
-define(RESET, fun ?FileCacheMan:reset/0).
-define(WRITE, fun ?FileCacheMan:file_hash_changed/1).
-define(GENERATE_ALL, fun ri:generate_all/1).

%% =====================
%% Full test
test() -> 
    seq_test() andalso para_test().

%% =====================
%% Sequential tests

seq_test() ->
    test1() andalso 
    test2() andalso 
    test3() andalso 
    test4().

%% =====================
%% Test 1: 
%%        Read: Adds a library to the db, reads first file through 
%%        file cache server, and compares the answer to the actual file content.
test1() -> ?debug_test,
    prepare(),
    reset_db(),
    read_to_db(),
    Item = ?ORIGINAL,
    cleanup(compare(Item)). 
%% =====================

%% =====================
%% Test 2:
%%         Write and read: Adds files to the database, read the content of a file,
%%         then alters the content of one of the files, and reads again. 
%%         Finally, it re-adds the file and read again.
test2() ->?debug_test,
    prepare(),
    file:copy(?ALTERED, ?ORIGINAL),
    reset_db(),
    read_to_db(),
    Item = ?ORIGINAL, 
    Res1 = compare(Item),
    file:copy(?OVERWRITE,Item),
    Res2 = not compare(Item),    
    read_to_db(),
    Res3 = compare(Item),
    cleanup(Res1 andalso Res2 andalso Res3). 
%% =====================

%% =====================
%% Test 3:
%%         Reset: Adds some files, reads a file, and resets the cache. 
%%         If the cache directory doesn't exist, the test is successful.

test3() ->?debug_test,
    prepare(),
    reset_db(),
    read_to_db(),
    Item = get_first_file(),
    ?READ(Item),
    reset_db(),
    cleanup(not filelib:is_dir(?MISC:data_dir() ++ "file_cache" ++ 
                               ?MISC:graph_based_postfix())).
%% =====================

%% =====================
%% Test 4
%%         Finds the "common root" of db files, and gives a generate_all 
%%         order from there. If all db files are cached, the test is successful.
test4() ->?debug_test,
    prepare(),
    reset_db(),
    read_to_db(),
    Files = ?Query:exec(?File:all()),
    Paths = [ ?File:path(N) || N <- Files ],
    List = ?GENERATE_ALL(?WorkDir),
    cleanup(lists:sort(List) =:= 
            lists:sort([ P || P <- Paths, 
                              filename:extension(P) =:= ".erl"])). 
    
%% =====================

%% =====================
%% Parallel tests

para_test() ->
    test5() andalso test6() andalso test7().

%% =====================
%% Test 5:
%%        Parallel read: Tries to read multiple files, some of them more than once.
test5() ->?debug_test,
    prepare(),
    reset_db(),
    read_to_db(),
    [First, Second, Third] = [ get_file(N) || N <- [1,2,3] ],
    PathList = [ First, Second, First, Third, Second, First],
    Answers = run_pp_general(lists:zip([?READ || _ <- lists:seq(1,6)],PathList)),
    Contents = [ binary_to_list(element(2,file:read_file(P))) || P <- PathList ],
    Pairs = lists:zip(Answers, Contents),
    cleanup(lists:all(fun({A, C}) -> A =:= C end, Pairs)).
    
%% =====================

%% =====================
%% Test 6:
%%        Parallel read + write: Tries to read and write a file at the same time.
%%        The test is successful, if the content of the file is equal to
%%        the earlier state of the file.
test6() ->?debug_test,
    prepare(),
    file:copy(?OVERWRITE, ?ORIGINAL),
    reset_db(),
    read_to_db(),
    Path = ?ORIGINAL,
    ?READ(Path),
    Content = ?Syn:flat_text(hd(?Query:exec(?File:find(Path)))),
    file:copy(?ALTERED, ?ORIGINAL),
    Answer = lists:nth(1, run_pp_general([{?READ, Path},{?WRITE, Path}])),
    cleanup(Answer =:= Content).
    
%% =====================
%% Test 7:
%%        Parallel write + read: Tries to read and write a file at the same time.
%%        The test is successful, if the content of the file is equal to
%%        the latter state of the file.
test7() ->?debug_test,
    prepare(),
    % Reading original file in db
    file:copy(?OVERWRITE, ?ORIGINAL),
    reset_db(),
    read_to_db(),
    % Raeding it into cache
    Path = ?ORIGINAL,
    ?READ(Path),
    % Altering file content outside db
    file:copy(?ALTERED, Path),
    Content = ?Syn:flat_text(hd(?Query:exec(?File:find(Path)))),
    % Running competing write and read
    Answer = lists:nth(2, run_pp_general([{?WRITE, Path},{?READ, Path}])),
    cleanup(Answer =:= Content).

%% =====================
%% Private functions

get_base()->
    Path0 = filename:split(filename:dirname(code:which(?MODULE))),
    Path =filename:join(lists:sublist(Path0,length(Path0)-2)),
    Path.

prepare()->   
    ri:addenv(appbase, get_base()),
    create_work_dir(),
    %copy test case
    copy_work_dir(?RootDir, ?WorkDir).

cleanup(ToReturn)->
    %reset db
    reset_db(),
    %remove work dir -- can be replaced by a better solution :)
    os:cmd("rm -rf "++?WorkDir),
    ToReturn.

read_to_db() ->
    ri:add(filename:absname(?TEST)).
    
reset_db() ->
    ri:reset().
    
get_file(N) ->
    lists:nth(N, [ ?File:path(F) || F <- ?Query:exec(?File:all()) ]).
    
get_first_file() ->
    get_file(1).

compare(Path) ->
    Res = list_to_binary(?READ(Path)),
    {_, Answer} = file:read_file(Path),
    Res =:= Answer.

create_work_dir()->
    filelib:ensure_dir(?WorkDir).

copy_work_dir(From, To) ->
    {ok, Files} = file:list_dir(From),
    [ok = copy0(From, To, X) || X <- Files],
    ok.

copy0(_From, _To, [$. | _T]) ->
    ok; 
copy0(From, To, File) ->
 
    NewFrom = filename:join(From, File),
    NewTo   = filename:join(To, File),
 
    case filelib:is_dir(NewFrom) of
 
        true  ->
            ok = filelib:ensure_dir(NewTo),
            copy_work_dir(NewFrom, NewTo);
        
        false ->
            case filelib:is_file(NewFrom) of                
                true  ->
                    ok = filelib:ensure_dir(NewTo),
                    {ok, _} = file:copy(NewFrom, NewTo),
                    ok;
                false ->
                    ok            
            end
    end.


% Usage: pp1:run_pp([fun()->do_some_task end], 10).
run_pp_general(TupleList) when is_list(TupleList) ->
    CWorker = length(TupleList),
    ReceiverPid = produce_receiver(self(), CWorker),
    WorkerFun=fun(Param)->
                      {Fun, Params} = lists:nth(Param, TupleList),
                      Result = case Params of
                          [] -> Fun();
                          _ -> Fun(Params)
                      end,
                      ReceiverPid ! {worker_result, {Param, Result}}                   
              end,
    [spawn(fun()->WorkerFun(I) end) || I <-lists:seq(1,CWorker) ],
    receive
        {receiver_finished, R} -> 
            Sorted = lists:keysort(1, R),
            element(2, lists:unzip(Sorted))
    end.    

run_pp(Fun, CWorker) when is_integer(CWorker) andalso CWorker>0 
    andalso is_function(Fun)->
    ReceiverPid = produce_receiver(self(), CWorker),
    WorkerFun=fun(Param)->
                      Result = Fun(Param),
                      ReceiverPid ! {worker_result, Result}                   
              end,
    [spawn(fun()->WorkerFun(I) end) || I <-lists:seq(1,CWorker) ],
    receive
        {receiver_finished, R} ->R
    end.

produce_receiver(ParentPid, CWorker) when is_pid(ParentPid) ->
    spawn(fun()->
                  Result = receive_loop(CWorker, []),
                  ParentPid ! {receiver_finished, Result}
          end).

receive_loop(0, Acc)->
    Acc;
receive_loop(CWorker, AccIn) when is_integer(CWorker) andalso CWorker>0 ->
    Result = receive
        {worker_result, R} -> R
    end,
    receive_loop(CWorker-1, AccIn ++ [Result]).

