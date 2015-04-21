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

%%% @doc This module makes a syntax tree based  differential test between
%%%      two erlang source files, or two directories. These directories must
%%%      contain erlang source files or another directories. In case of file
%%%      compare, the differ stops when it finds the first difference. In
%%%      case of directory compare, we will get a list of differencies.
%%%
%%% ==USING==
%%% <dl>
%%% <dt>reftest_syntree_differ:run([{file1,path()},{file2,path()}]).</dt><dd>for files</dd>
%%% <dt>reftest_syntree_differ:run_dir([{dir1,path()},{dir2,path()}]).</dt><dd>for directories</dd>
%%% <dt>Examples:</dt><dd>reftest_syntree_differ:run([{file1,"test.erl"},{file2,"tmp/test.erl"}]).</dd>
%%%     <dd>reftest_syntree_differ:run([{file1,"test.erl"},{file2,"test.erl"},{keeporder,true}]).</dd>
%%% </dl>
%%%
%%% ==Parameters in proplist==
%%%
%%% <dl>
%%%         <dt>{file1,path()},{file2,path()}</dt><dd>run differ for the specified files</dd>
%%%         <dt>{dir1,path()},{dir2,path()}</dt><dd>run differ for specified directories</dd>
%%%         <dt>{expimpcollect,Bool()}</dt>
%%%             <dd>true  - The differ collects export (and import) rows from the
%%%                         modules, and generate one single list from them. So if
%%%                         you have two or more export (import) rows in one of
%%%                         the modules and a single export (import) row in the
%%%                         other module, the differ will not throw diff
%%%                         exception.</dd>
%%%             <dd>false - If there is difference the number of export (import)
%%%                         rows in modules, the differ will throw diff
%%%                         exception. This is default behavior.</dd>
%%%         <dt>{keeporder, Bool()}</dt>
%%%             <dd>true  - The differ sorts the form list, which gets from
%%%                         parser.</dd>
%%%             <dd>false - In this case the differ is form order sensitive.
%%%                         This is default behavior.</dd>
%%% </dl>
%%%
%%%
%%% ==RETURN VALUES(Exceptions)==
%%%
%%% <dl>
%%%         <dt>{file_error,FILE_NAME,STATUS,ERROR}</dt>
%%%             <dd>file_error    - error while parsing source file</dd>
%%%             <dd>FILE_NAME     - name of the file where the error happens</dd>
%%%             <dd>STATUS        - first part of the error message</dd>
%%%             <dd>ERROR         - reason of the error</dd>
%%%
%%%         <dt>{not_dir,DIR_NAME}</dt>
%%%             <dd>not_dir     - error while reading directory</dd>
%%%             <dd>DIR_NAME      - name of the directory where the error happens</dd>
%%%
%%%         <dt>{dir_diff,DIR1_NAME,DIR2_NAME}</dt>
%%%             <dd>dir_diff      - the two given directories contain different files</dd>
%%%             <dd>DIR1_NAME     - first directory name</dd>
%%%             <dd>DIR2_NAME     - second directory name</dd>
%%%
%%%         <dt>{diff,VALUE1,VALUE2,POSITION,TYPE}</dt>
%%%             <dd>diff          - it means there is a difference</dd>
%%%             <dd>VALUE1,VALUE2 - values of nodes where the difference is</dd>
%%%             <dd>POSITION      - row number of nodes in the first source file</dd>
%%%             <dd>TYPE          - types of nodes</dd>
%%% </dl>
%%%
%%%      If there is no difference the return value is simply an empty list.
%%%
%%%
%%% @author Gabor Czini <gczini@gmail.com>


-module(reftest_syntree_differ).
-export([run/1]).
-vsn("$Rev: 9568 $").

-define(BEAMSDIR,"beams").


%%----------------FILE COMPARE----------------------------------------
%% @spec run(proplist()) -> list()
%% @doc This function compares two given files or directories.
run(PropL) ->
    File1 = proplists:get_value(file1, PropL, none),
    File2 = proplists:get_value(file2, PropL, none),
    Dir1 = proplists:get_value(dir1, PropL, none),
    Dir2 = proplists:get_value(dir2, PropL, none),
    case {File1,File2} of
        {none,none} ->
            run_dir(Dir1,Dir2,PropL);
        _ ->
            run_file(File1,File2,PropL)
    end.


run_file(File1,File2,PropL)->
    KeepOrder     = proplists:get_value(keeporder, PropL, false),
    ExpImpCollect = proplists:get_value(expimpcollect, PropL, false),

    OutDir = filename:join(".",?BEAMSDIR),
    file:make_dir(OutDir),

    compile_check(File1,OutDir),
    compile_check(File2,OutDir),

    delete(OutDir),

    SynTree1 = dodger_parse(File1),
    SynTree2 = dodger_parse(File2),

    {Tree1, Tree2} =
        case ExpImpCollect of
            true  -> collect_expimp(SynTree1, SynTree2);
            false -> {SynTree1, SynTree2}
        end,

    case KeepOrder of
        false -> step(lists:usort(Tree1), lists:usort(Tree2));
        true  -> step(Tree1, Tree2)
    end,
    [].

compile_check(File,OutDir) ->
    case is_erl(File) of
        false ->
            ok;
        true ->   
            CompileCheck = compile:file(File, [{outdir,OutDir}]),
            case CompileCheck of
                error -> delete(OutDir),
			 throw({file_error, File, error, "Compile error"});
                _     -> ok
            end
    end.

delete(Folder) ->
    case file:list_dir(Folder) of
	{ok,[]} ->	      
	    file:del_dir(Folder);
	{ok, Files} ->
	    {_,Path} = file:get_cwd(),
	    [file:delete(filename:join([Path,?BEAMSDIR,File]))||File<-Files],
	    file:del_dir(Folder);
	_ -> ok
    end.

dodger_parse(File) ->
    case epp_dodger:parse_file(File) of
        {error, ErrSynTree} -> throw({file_error, File, error, ErrSynTree});
        {_, SynTree}        -> SynTree
    end.


%% Returns whether the file has the extension ".erl".
is_erl(Filename) ->
    RevErl = lists:reverse(".erl"),
    RevFn  = lists:reverse(Filename),
    lists:prefix(RevErl, RevFn).


step([],[]) -> [];
step([A|_B],[]) -> throw({diff,A,'empty-list',erl_syntax:get_pos(A),erl_syntax:type(A)});
step([],[X|_Y]) -> throw({diff,X,'empty-list',erl_syntax:get_pos(X),erl_syntax:type(X)});
step([A|B],[X|Y]) ->
    List1=traverse(A),
    List2=traverse(X),
    cmp(List1,List2),
    step(B,Y).

collect([],_)-> [];
collect([A|B],What)->
     case erl_syntax:type(A) of
        attribute ->
            case erl_syntax_lib:analyze_attribute(A) of
                {What,_List} ->
                   % io:format("~p ~p ~n",[What,erl_syntax_lib:analyze_attribute(A)]),
                    [A]++collect(B,What);
                _ -> collect(B,What)
            end;
        _ -> collect(B,What)
    end.

collect_expimp(SynTree1,SynTree2)->
    Export1=collect(SynTree1,export),
    Export2=collect(SynTree2,export),
    Import1=collect(SynTree1,import),
    Import2=collect(SynTree2,import),
    check(lists:sort(prep_list(Export1)),lists:sort(prep_list(Export2)),2,export),
    check(lists:sort(prep_list(Import1)),lists:sort(prep_list(Import2)),2,import),
    NewTree1=SynTree1--(Export1++Import1),
    NewTree2=SynTree2--(Export2++Import2),
    {NewTree1,NewTree2}.

prep_list([])->[];
prep_list([A|B])->
    {_,Data}=erl_syntax_lib:analyze_attribute(A),
    case is_tuple(Data) of
        true ->
            [Data]++prep_list(B);
        false ->
            Data++prep_list(B)
    end.

traverse(Tree)->
    case erl_syntax:subtrees(Tree) of
        []-> [Tree];
        List ->
            lists:flatten([Tree]++
            [[traverse(Subtree)||Subtree <- Group]|| Group <- List])
    end.

cmp([],[]) -> [];
cmp([A|_B],[]) -> throw({diff,A,[],erl_syntax:get_pos(A),erl_syntax:type(A)});
cmp([],[X|_Y]) -> throw({diff,X,[],erl_syntax:get_pos(X),erl_syntax:type(X)});
cmp([A|B],[X|Y])->
    Typ1=erl_syntax:type(A),
    Typ2=erl_syntax:type(X),
    if
        Typ1==Typ2 ->
            case Typ1 of
                atom ->
                    check(erl_syntax:atom_value(A),
                          erl_syntax:atom_value(X),
                          erl_syntax:get_pos(A),Typ1);
                string ->
                    check(erl_syntax:string_value(A),
                          erl_syntax:string_value(X),
                          erl_syntax:get_pos(A),Typ1);
                variable ->
                    check(erl_syntax:variable_name(A),
                          erl_syntax:variable_name(X),
                          erl_syntax:get_pos(A),Typ1);
                operator ->
                    check(erl_syntax:operator_name(A),
                          erl_syntax:operator_name(X),
                          erl_syntax:get_pos(A),Typ1);
                integer ->
                    check(erl_syntax:integer_value(A),
                          erl_syntax:integer_value(X),
                          erl_syntax:get_pos(A),Typ1);
                attribute ->
                    check(erl_syntax_lib:analyze_attribute(A),
                          erl_syntax_lib:analyze_attribute(X),
                          erl_syntax:get_pos(A),Typ1);
                _ -> []
            end;
        true -> throw({diff,A,X,erl_syntax:get_pos(A),Typ1})
    end,
    cmp(B,Y).

check(A,B,Pos,Type) ->
    if
              A==B -> [];
        true -> throw({diff,A,B,Pos,Type})
    end.

%%-------------------DIRECTORY COMPARE-------------------------------------

%% @spec run_dir(Dir1,Dir2,Proplist) -> list()
%% @doc This function compares two given directories.
run_dir(Dir1,Dir2,PropL) ->
    DirBool1=filelib:is_dir(Dir1),
    DirBool2=filelib:is_dir(Dir2),
    if
        DirBool1==false -> throw({not_dir,Dir1});
        DirBool2==false -> throw({not_dir,Dir2});
        true ->
            {ok,FileList1}=file:list_dir(Dir1),
            {ok,FileList2}=file:list_dir(Dir2),
            if
                FileList1==FileList2 ->
                    lists:flatten([do_cmp(Dir1,Dir2,PropL,File)||
                                      File <- FileList1]);
                true -> throw({dir_diff,Dir1,Dir2})
            end
    end.

do_cmp(Dir1,Dir2,PropL,File) ->
  try
      IsFile=filelib:is_dir(filename:join(Dir1,File)),
      case IsFile of
          false ->
              run_file(filename:join(Dir1,File),
                  filename:join(Dir2,File),PropL);
          true  ->
              run_dir(filename:join(Dir1,File),
                      filename:join(Dir2,File),PropL)
      end
  catch
        A -> {A,filename:join(Dir1,File)}
  end.

