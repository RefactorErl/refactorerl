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

%%% @doc This is a test module for module `cl_out'.
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>

-module(reftest_cl_out).
-vsn("$Rev $").

-export([test/0]).

%% Hopefully a file with this name does not exist.
%% If the test fails, and this file remains, it should be deleted.
%%
%% @todo We should use something like `mktemp' instead of this.
-define(FILE_NAME,"this_file_should_not_exist").

-define(TIMEOUT,1000).

%% @spec test() -> ok
%%
%% @doc It tests the modul and returns ok if the test passed.
test()->
    ok = test_null(),
    ok = test_device(),
    ok = test_file(),
    ok = test_process(),
    ok = test_function(),
    ok = test_list(),
    clean_up_file().

%% Check that file `?FILE_NAME' does not exist.
check_file() ->
    case file:read_file_info(?FILE_NAME) of
        {error,enoent} -> 
            ok;
        _ ->
            throw("The file '"++?FILE_NAME++"' exists, but it should not. "++
                  "Please, delete it manually.")
    end.

%% @doc If file `?FILE_NAME' exists, delete it (because it means that we created
%% it and did not delete it).
clean_up_file() ->
    case file:read_file_info(?FILE_NAME) of
        {error,enoent} -> 
            ok;
        _ ->
            file:delete(?FILE_NAME)
    end.

test_null() ->
    ok = write_hello_3x(null).

test_device() ->
    check_file(),
    {ok,Dev} = file:open(?FILE_NAME,[write]),
    ok = write_hello_3x({device,Dev}),
    ok = write_hello_3x({device,Dev}),
    file:close(Dev),
    {ok,Binary} = file:read_file(?FILE_NAME),
    ok = file:delete(?FILE_NAME),
    Binary = list_to_binary("hellohellohellohellohellohello"),
    ok.

%% @doc Writes "hello".
write_hello(Output) ->
    {W,C} = cl_out:open(Output),
    cl_out:fwrite(W,"he~so",["ll"]),
    cl_out:close(C).

%% @doc Writes "hello" three times.
write_hello_3x(Output) ->
    {W,C} = cl_out:open(Output),
    cl_out:fwrite(W,"he~so",["ll"]),
    write_hello(W),
    cl_out:fwrite(W,"he~so",["ll"]),
    cl_out:close(C).

test_file() ->
    true = (test_file_1(write) == list_to_binary("hellohellohello")),
    true = (test_file_1(append) == list_to_binary("hellohellohellohellohellohello")),
    ok.

test_file_1(Mode) ->
    check_file(),
    ok = write_hello_3x({file,?FILE_NAME,[{mode,Mode}]}),
    ok = write_hello_3x({file,?FILE_NAME,[{mode,Mode}]}),
    {ok,Binary} = file:read_file(?FILE_NAME),
    ok = file:delete(?FILE_NAME),
    Binary.

test_process() ->
    Self = self(),
    Pid = spawn_link(fun() -> hello_receiver(Self,3) end),
    write_hello_3x({process,Pid,[{send_close,false}]}),
    receive_ok().

receive_ok() ->
    ok = 
        receive 
            ok -> ok 
        after 
            ?TIMEOUT -> timeout 
        end.

%% @spec hello_receiver(pid(),integer()) -> ok
%%
%% @doc Receives `{output,"hello"}' `N' times, then sends an `ok' message to
%% `Main', and terminates.
hello_receiver(Main,0) ->
    Main ! ok;
hello_receiver(Main,N) ->
    receive
        {output,"hello"} ->
            hello_receiver(Main,N-1)
    end.

test_function() ->
    Self = self(),
    Pid = spawn_link(fun() -> hello_receiver(Self,3) end),
    write_hello_3x({function, 
                    fun(Text) -> Pid ! {output,Text} end,
                    [{send_close, false}]}),
    receive_ok().

test_list() ->
    check_file(),
    Self = self(),
    Pid = spawn_link(fun() -> hello_receiver(Self,6) end),
    ok = write_hello_3x({list,[{file, ?FILE_NAME,[{mode,append}]},
                               {process, Pid, [{send_close, false}]}]}),
    ok = write_hello_3x({list,[{file, ?FILE_NAME,[{mode,append}]},
                               {process, Pid, [{send_close, false}]}]}),
    {ok, Binary} = file:read_file(?FILE_NAME),
    ok = file:delete(?FILE_NAME),
    Binary = list_to_binary("hellohellohellohellohellohello"),
    receive_ok().
