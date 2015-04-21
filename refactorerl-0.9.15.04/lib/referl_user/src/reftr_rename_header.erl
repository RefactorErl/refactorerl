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

%%% @doc The rename header refactoring renames the header file to the given
%%%      new name, and makes changes in those files in which it is referred to.
%%%      If the new name of the header file contains a path and this path is not
%%%      equal to the original one, the transformation moves the header file to
%%%      its new place and renames it.
%%%
%%% == Parameters ==
%%% <ul>
%%%  <li> A module (see {@link reflib_args:filename/1}). The header file to
%%%       be modified. Currently it can be specified with a position anywhere
%%%       in the include file.</li>
%%%  <li> A module (see {@link reflib_args:file/1}). The new name of the
%%%       header file.</li>
%%% </ul>
%%%
%%% == Conditions of applicability ==
%%% <ul>
%%%   <li>The type of the file has to be a header file. If the pointed file is
%%%    a module, the transformation will fail.</li>
%%%   <li>The directory must not contain a file having the same name as new
%%%        name given. </li>
%%% </ul>
%%%
%%% == Transformation steps and compensations ==
%%% <ul>
%%%   <li>Rename the header file name to the new name on the graph.</li>
%%%   <li>Rename the references to the header file in the include forms.
%%%       (Actually, the include form will be deleted and recreated with a new
%%%       path and filename).</li>
%%%   <li>Rename or move and rename the file to the new name.</li>
%%% </ul>
%%%
%%% == Implementation status ==
%%%
%%% This refactoring has been implemented.
%%%
%%% @author Roland Kiraly <kiralyroland@inf.elte.hu>

-module(reftr_rename_header).
-vsn(" $Rev: 9568 $ ").

%%% ============================================================================
%%% Exports

%% Callbacks
-export([prepare/1]).

-include("user.hrl").

%%% ============================================================================
%%% Callbacks
%%% @private
prepare(Args)->
    FileNode = ?Args:file(Args),

    OldPath = ?File:path(FileNode),
    Path    = filename:dirname(OldPath),
    ?Check(?File:type(FileNode) == header, ?RefError(file_not_hrl,[OldPath])),

    ArgsInfo = add_transformation_info(Args, FileNode),
    NewPath  = ?Args:ask(ArgsInfo, filename, fun cc_newpath/2, fun cc_error/3, 
                         {Path, OldPath}),

    InclFiles = ?Query:exec(FileNode, ?File:included())--[FileNode],

    [?Transform:touch(F) || F <- [FileNode | InclFiles]],

    [fun()->
       ?Transform:rename(OldPath, NewPath),
       ?File:upd_path(FileNode, NewPath),
       ?FileMan:save_file(FileNode)
     end]
    ++
    [fun(ok)-> ?File:del_include(WInclFile, FileNode) end
                                         || WInclFile <- InclFiles]
    ++
    [fun(ok)-> ?File:add_include(WInclFile, FileNode) end
                                         || WInclFile <- InclFiles]
    ++
    [fun(ok)-> [FileNode] end].

add_transformation_info(Args, File) ->
    Path = ?File:path(File),
    Info = ?MISC:format("Renaming header: ~p", [Path]),
    [{transformation_text, Info} | Args].

%%% ============================================================================
%%% Checks

cc_newpath(NewName, {Path, OldPath}) ->
    ?Check(not lists:member("..", filename:split(NewName)),
           ?RefError(rel_path, [NewName])),
    NewHrl = filename:join(Path, NewName), 
    ?Check(NewHrl =/= OldPath,
           ?RefError(new_hrl_name_identical, [NewName])),
    ?Check(not filelib:is_file(NewHrl), ?RefError(used_header, NewHrl)),
    NewHrl.

cc_error(?RefError(Type, NewHrl), _NewName, _Path) ->
    case Type of
     rel_path     -> 
      ?MISC:format("The path of the header file has to be an absolute path.", []);
     file_not_hrl ->
      ?MISC:format("File ~p does not identify a header file.", [NewHrl]);
     used_header  -> 
      ?MISC:format("The header file ~p is already used.", [NewHrl]);
     new_hrl_name_identical  -> 
      ?MISC:format("The header name is already 'p.", [NewHrl])
    end.
