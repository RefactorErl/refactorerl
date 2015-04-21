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

%%% @doc Sorts the modules according to their consisting directories, thus
%%% the modules which are in the same directory will be in the same group.
%%%
%%% @author Kinga Szava <guca04@gmail.com>

-module(refusr_dir_sort).
-vsn("$Rev: 13083 $ ").
-export([sort/0, get_dirs/1, get_mods/1]).

-export([valid_paths/1]).

-include("user.hrl").

%%% ============================================================================
%%% Sorting queries

%% @spec sort() -> Sorted | Error
%%           Sorted = [{Directory, Modules}]
%%           Directory = string()
%%           Modules = [atom()]
%%
%%           Error = {error, no_modules_in_database}
%%                 | {error, no_list_given}
%%                 | {error, bad_list}
%% @doc Sorts every module according to their consisting directories.
%% Equivalent to {@link get_dirs/1}, where the parameter is each module
%% in the graph.
sort() ->
    case ?Query:exec(?Mod:all()) of
        []      -> {error, no_modules_in_database};
        ModList -> get_dirs(ModList)
    end.

%% @spec get_dirs(ParList)-> Sorted | Error
%%           Sorted = [{Directory, Modules}]
%%           Directory = string()
%%           Modules = [atom()]
%%
%%           ParList = [Module]
%%           Module = node() | atom()
%%
%%           Error = {error, no_list_given}
%%                 | {error, bad_list}
%% @doc Sorts the modules (which can be given either as nodes or with
%% their names as atoms) according to their directories. Those which do
%% not belong to any of the directories will be put in a group named
%% "Other".
%% The output looks like the following:
%% <br/> [{"DirName1", [Modules1]},
%% <br/>  {"DirName2", [Modules2]}
%% <br/>  ...
%% <br/>  {"Other", [ModulesN]}]
get_dirs([])->
    {error, no_module_list_given};
get_dirs(undefined)->
    {error, no_module_list_given};
get_dirs(Mods) ->
    try
        dir_sort([module_node(M) || M <- Mods])
    catch
        throw: _ -> {error, bad_list}
    end.

module_node(Mod) when is_atom(Mod) ->
    ?Query:exec1(?Mod:find(Mod), mod_not_found);
module_node({_, _, _} = Mod) ->
    ?Graph:data(Mod),
    Mod;
module_node(_) ->
    throw(badarg).

%% @spec get_mods(Options::proplists())-> [{DirPath, ModList}] | Error
%% DirPath = string()
%% ModList = [atom()]
%% Error = {error, no_directory_given} |
%%	   {error, no_such_directory_in_the_database} | 
%% 	   {error, sorting_directories}
%% @doc Gives back the modules belonging to the given directory list.
%% The Options are the following:
%% ```
%% {dir_list, DirList}
%% DirList = [string()]
%% '''
%% Module filtering on these directories (given with full path).
%%
%% ```
%% {recursive, Recursive}
%% Recursive = true | false
%% '''
%% The sorting should be done in a recursive way, meaning that a directory (besides
%% its direct modules) will get the modules of its subdirectories in the result module list.
get_mods([])->
	{error, no_option_list_given};
get_mods(Options)->
    filter_dir(proplists:get_value(dir_list, Options), proplists:get_value(recursive, Options)).
 

filter_dir(undefined, undefined)->
    {error, bad_option_list_given};
filter_dir(undefined, _)->
    {error, no_directories_given};
filter_dir(DirList, undefined)->
    filter_dir(DirList, false);
filter_dir(DirList, true)->
    recursive(DirList);
filter_dir(DirList, false)->
	[{Dir, filter_modules(Dir)} || Dir <- DirList];
filter_dir(_, _) ->
    {error, wrong_recursive_key}.

filter_modules(Dir) ->
	case lists:keyfind(Dir, 1, sort()) of 
		{_, ModList} -> ModList;
		_ -> []
	end.

recursive(DirList)->
    [{Dir, recursiveDirs(Dir)} ||
	Dir <- DirList].

recursiveDirs(Path)->
    lists:usort(lists:append(
      [filter_modules(Dir) || {Dir, _}<-sort(), 
			lists:prefix(Path, Dir)])).

%%% ============================================================================
%%% Building list

dir_sort(ModList)->merge(mod_group(ModList)).

valid_paths(Mod)->
	groupname(?Query:exec(Mod, ?Mod:file())).

mod_group(ModList)->
    [{Group, ?Mod:name(Mod)}
	  || Mod <- ModList, 
	     Group <- valid_paths(Mod)].

groupname([])     -> ["Other"];
groupname(Files) -> [filename:dirname(?File:path(File)) || File <- Files].



merge([]) -> [];
merge([{FilePath, _} | Rest] = List) ->
    SameDir = [{Path, Mod} || {Path, Mod} <- List, FilePath == Path],
    [{FilePath, [Mod || {_, Mod} <- SameDir]}
     | merge(Rest -- SameDir)].


