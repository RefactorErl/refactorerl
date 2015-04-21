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

%%% @author Tamas Hoffman <hoffmantamas@caesar.elte.hu>

-module(referl_htmlserver).
-vsn("$Rev: 9568 $").

-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_call/3,
         handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, dump/0, reset/0, drop/1, 
         generate/1, generate_call/1, getdata/1,
         add_query/2, get_queries/1, del_dir_recurse/2]).

-define(SERVER, htmlserver).

-record(fdata,  {filename        :: string(),
                 filehash        :: integer(),
                 htmldata        :: string()}).

-record(qdata,  {type            :: atom(),
                 queries        :: [string()]}).

-record(state,  {fdata            :: [#fdata{}],
                 qdata            :: [#qdata{}]}).
                 
-include("ui.hrl").

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, 0, []).

dump() ->
    gen_server:call(?SERVER, dump, infinity).

%% @doc Delete all stored data
reset() -> 
    gen_server:cast(?SERVER, reset).

%% @doc Delete data for file
drop(FileName) ->
    gen_server:cast(?SERVER, {drop,FileName}).

%% @doc Generate data for file
generate(FileName) ->
    gen_server:cast(?SERVER, {generate,FileName}).

%% @doc Generate data for file (synchronous)
generate_call(FileName) ->
    gen_server:call(?SERVER, {generate,FileName}, infinity).

%% @doc Get data for FileName
getdata(FileName) ->
    gen_server:call(?SERVER, {getdata,FileName}, infinity).

%% @doc Add query for type
add_query(none, _) -> ok;
add_query(Type, Query) ->
    gen_server:cast(?SERVER, {addquery,Type,Query}).

%% @doc Get queries for type
get_queries(Type) ->
    gen_server:call(?SERVER, {getqueries,Type}, infinity).

%%% ============================================================================
%%% Implementation functions

init(_) ->
    BaseDir=mnesia:system_info(directory)++"/htmlserver",
    StateFile=BaseDir++"/htmlserver_state.dat",
    FileExists=filelib:is_file(StateFile),
    if
        FileExists==true -> 
            {Result, State} = file:consult(StateFile),
            if
                (Result==ok) and (State/=[]) ->
                    FileData=read_saved_files((hd(State))#state.fdata),
                    {ok, (hd(State))#state{fdata=FileData}};
                true ->
                    {ok, #state{fdata=[],qdata=[]}}
            end;
        true ->
            {ok, #state{fdata=[],qdata=[]}}
    end.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(reset, #state{fdata=FData}) ->
    delete_files(FData),
    BaseDir=mnesia:system_info(directory)++"/htmlserver",
    file:delete(BaseDir++"/htmlserver_state.dat"),
    {noreply, #state{fdata=[],qdata=[]}};
handle_cast({drop,FileName}, State=#state{fdata=FData}) ->
    NewData=drop_file(FileName,FData),
    BaseDir=mnesia:system_info(directory)++"/htmlserver",
    file:delete(BaseDir++FileName),
    NewState=State#state{fdata=NewData},
    save(NewState),
    {noreply, NewState};
handle_cast({generate, FileName}, State=#state{fdata=FData}) ->    
    Data=find_data(FileName,FData),
    FileNode=?Query:exec(?File:find(FileName)),
    if
        FileNode==[] -> 
            self() ! {done,FileName},
            {noreply, State};
        true ->
            Hash=referl_misc:file_hash(hd(FileNode)),
            if    
                Data==[] ->
                    {HtmlData,NewHash}=
                    case generate_data(FileName) of 
                        {ok,R} -> {R,Hash};
                        {error,R} -> {R,-1}
                    end,
                    NewState=State#state{
                        fdata=FData++[#fdata{filename=FileName,
                        filehash=NewHash,
                        htmldata=HtmlData}]},
                    save(NewState,FileName,HtmlData);
                true -> 
                    if
                        Data#fdata.filehash/=Hash ->
                            {HtmlData,NewHash}=
                            case generate_data(FileName) of 
                                {ok,R} -> {R,Hash};
                                {error,R} -> {R,-1}
                            end,
                            NewState=State#state{fdata=replace_data(
                                FileName,FData,HtmlData,NewHash)},
                            save(NewState,FileName,HtmlData);
                        true -> NewState=State
                    end
            end,
            self() ! {done,FileName},
            {noreply, NewState}
    end;
handle_cast({addquery, Type, Query}, State=#state{qdata=QData}) ->
    QList=find_query(Type,QData),
    if    
        QList==[] ->
            NewState=State#state{qdata=QData++
                [#qdata{type=Type, queries=[Query]}]};
        true -> 
            NewData=replace_queries(Type,add_string(Query,QList),QData),
            NewState=State#state{qdata=NewData}
    end,
    save(NewState),
    {noreply, NewState}.

handle_call({generate, FileName},_, State=#state{fdata=FData}) ->
    Node=?Query:exec(?File:find(FileName)),
    if 
    length(Node)/=1 -> 
         {reply, ok, State};
    true ->    
        Data=find_data(FileName,FData),
        [FileNode]=?Query:exec(?File:find(FileName)),
        {Hash,HtmlData}=
        case generate_data(FileName) of 
            {ok,R} -> {referl_misc:file_hash(FileNode),R};
            {error,R} -> {-1,R}
        end,
        if    
            Data==[] -> 
                NewState=State#state{
                    fdata=FData++[#fdata{filename=FileName,filehash=Hash,
                                         htmldata=HtmlData}]},
                io:format("~s generated.~n",[FileName]),
                save(NewState,FileName,HtmlData);
            true -> 
                %if
                %    Data#fdata.filehash/=Hash ->
                        NewState=State#state{
                            fdata=replace_data(FileName,FData,HtmlData,Hash)},
                        io:format("~s updated.~n",[FileName]),
                        save(NewState,FileName,HtmlData)%;
                %    true -> NewState=State
                %end
        end,
        {reply, ok, NewState}
    end;
handle_call({getdata,FileName}, {_, _}, State=#state{fdata=FData}) ->
    self()!{done,FileName},
    receive
        {done,File} when File==FileName ->
            Data=find_data(FileName,FData),
            if
                Data==[] ->
                    FoundData=find_data2(FileName,FData);
                true -> 
                    FoundData=Data
            end
    end,
    {reply, FoundData#fdata.htmldata, State};
handle_call({getqueries,Type}, {_, _}, State=#state{qdata=QData}) ->
    QList=lists:sort(find_query(Type,QData)),
    {reply, QList, State};
handle_call(dump, _From, State) ->
    {reply, State, State}.

handle_info({'EXIT', _, _}, State) ->
    {noreply, State};
handle_info({done, _}, State) -> {noreply, State}.
    
terminate(_,_) -> ok.

code_change(_,State,_) -> {ok, State}.

%%% ============================================================================
%%% Helper functions

find_data(FileName,[#fdata{filename=F}=D|_]) when F==FileName -> D;
find_data(FileName,[_|DS]) -> find_data(FileName,DS);
find_data(_,[]) -> [].

find_data2(FileName,[#fdata{filename=F}=D|_]) when F==FileName -> D;
find_data2(FileName,[_|DS]) -> find_data2(FileName,DS);
find_data2(_,[]) -> #fdata{htmldata="File not found!"}.

drop_file(FileName,[#fdata{filename=F}|DS]) when F==FileName -> 
    drop_file(FileName,DS);
drop_file(FileName,[D|DS]) -> [D|drop_file(FileName,DS)];
drop_file(_,[]) -> [].

replace_data(FileName,[#fdata{filename=F}=D|DS],NewData,NewHash) 
    when F==FileName -> 
    [D#fdata{htmldata=NewData,filehash=NewHash}|DS];
replace_data(FileName,[D|DS],NewData,NewHash) -> 
    [D|replace_data(FileName,DS,NewData,NewHash)];
replace_data(_,[],_,_) -> [].

find_query(Type,[#qdata{type=T}=Q|_]) when T==Type -> Q#qdata.queries;
find_query(Type,[_|QS]) -> find_query(Type,QS);
find_query(_,[]) -> [].

add_string(Query,[Q|QS]) when Q/=Query -> [Q|add_string(Query,QS)];
add_string(_,[_|_]=List) -> List;
add_string(Query,[]) -> [Query].

replace_queries(Type,NewList,[#qdata{type=T}=Q|QS]) when T==Type -> 
    [Q#qdata{queries=NewList}|QS];
replace_queries(Type,NewList,[Q|QS]) -> 
    [Q|replace_queries(Type,NewList,QS)];
replace_queries(_,_,[]) -> [].

save(State=#state{fdata=FData}) ->
    BaseDir=mnesia:system_info(directory)++"/htmlserver",
    filelib:ensure_dir(BaseDir++"/htmlserver_state.dat"),
    {ok, File} = file:open(BaseDir++"/htmlserver_state.dat", [write]),
    io:format(File, "~p.", [State#state{fdata=no_file_data(FData)}]),
    file:close(File).
save(State,FileName,HtmlData) ->
    save(State),
    BaseDir=mnesia:system_info(directory)++"/htmlserver",
    filelib:ensure_dir(BaseDir++FileName),    
    file:write_file(BaseDir++FileName, HtmlData,[write,raw]).

no_file_data([FD|FDS]) ->
    [FD#fdata{htmldata=[]}|no_file_data(FDS)];
no_file_data([]) -> [].

read_saved_files([F=#fdata{filename=FileName}|FS]) ->
    BaseDir=mnesia:system_info(directory)++"/htmlserver",
    {X, Binary} = file:read_file(BaseDir++FileName),
    if
        X==error -> read_saved_files(FS);
        true -> [F#fdata{htmldata=binary_to_list(Binary)}|read_saved_files(FS)]
    end;
read_saved_files([]) -> [].

delete_files([#fdata{filename=FileName}|FS]) -> 
    BaseDir=mnesia:system_info(directory)++"/htmlserver",
    file:delete(BaseDir++FileName),
    del_dir_recurse(BaseDir,filename:dirname(BaseDir++FileName)),
    delete_files(FS);
delete_files([]) -> ok.

del_dir_recurse(BaseDir,Path) when BaseDir/=Path ->
    E=file:del_dir(Path),
    case E of 
        ok ->
            SplitList=lists:map(fun(X)-> "/"++X end, string:tokens(Path, "/")),
            del_dir_recurse(BaseDir,
                lists:flatten(lists:reverse(
                    lists:nthtail(1,lists:reverse(SplitList)))));
        _ -> ok
    end;
del_dir_recurse(_,_) -> ok.

generate_data(FileName) -> 
    try
        case ?NITRO_CORE:make_ui_request({html_generate, FileName}) of
            {error, _} -> {error,"Database currently in use!"};
            {ok,R} -> {ok,R}
        end
    catch 
        E -> 
            io:format("Error during the generation of ~p: ~p~n",[FileName,E]),
            {error,"Error during file generation!"} 
    end.
