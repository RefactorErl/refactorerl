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


%%% @author Viktoria Fordos <f-viktoria@elte.hu>

%%% @doc
%%% <b>Temporary module of file cache server.</b>

-module(refcore_filecacheserver).
-vsn("$Rev: 8597 $ ").


% Interface
-export([file_hash_changed/1, get_file_content/1, reset/0,
         generate_all/1, force_generate/1, delete_file/1]).

-export([error_text/2]).

-include("core.hrl").

%% ================
%% Interface

%% @spec file_hash_changed(path()) -> ok
%% @doc Changes the file in cache described by the parameter.
%%       If the file is not in cache, nothing happens.
file_hash_changed(_File)->
    ok.

%% @spec get_file_content(path()) -> string()
%% @doc Returns the content of the file described in the parameter.
%%       This function locks the file, so no other process can change it,
%%       until the reading is finished.
get_file_content(File) when ?IS_NODE(File)-> 
    rpc_call(?REFERL_NODE, ?Syn, flat_text, [File], ?LocalError(not_in_db, [File]));
get_file_content(Path)->
    case find_file_node(Path) of
        [] -> throw(?LocalError(not_in_db, [Path]));
        [Node] -> get_file_content(Node)
    end.


%% @spec reset() -> ok
%% @doc Deletes every file in cache, and the cache directory. It also
%%       removes the filenames from the server state.
reset() ->
    ok.

%% @spec generate_all(fun()) -> [string()]
%% @doc Caches every erlang source file in a directory, and returns the
%%       names of these files. The parameter ActionFun describes the
%%       caching operation.
generate_all(ActionFun) when is_function(ActionFun) ->
    [];
generate_all(Arg) ->
    throw(?LocalError(invalid_argument,[Arg])).


%% @spec force_generate(path()) -> ok
%% @doc Forces the change of the given file in cache.
force_generate(_File) ->
    ok.

%% @spec delete_file(path()) -> ok
%% @doc Deletes the given file from cache.
delete_file(_File) ->
    ok.

 find_file_node(Path) ->
    rpc_call(?REFERL_NODE, ?Graph, path, [?Graph:root(), ?File:find(Path)], 
        ?LocalErr0r(internal_error)).

rpc_call(Node, Mod, Fun, Args, ToThrow) ->
    case rpc:call(Node, Mod, Fun, Args) of
        {badrpc , _ } ->
            throw(ToThrow);
        Result ->
            Result
    end.


error_text(not_in_db, [Path])->
    lists:flatten(["The given file (", ?MISC:any_to_string(Path), ") cannot be found in the database."]);
error_text(invalid_argument, [Arg])->
    lists:flatten(["Invalid argument (", ?MISC:any_to_string(Arg), ") was given."]);
error_text(internal_error,_)->
    "internal error";
error_text(_,_)->
    "unknown error".


