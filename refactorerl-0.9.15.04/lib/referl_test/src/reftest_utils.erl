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

%%% @doc This module contains utility functions for the tester infrastructure.
%%%
%%% @author Csaba Hoch <hoch@inf.elte.hu>
%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>


-module(reftest_utils).
-vsn("$Rev: 13070 $").
-export([add_files/2, clear_files/0, reset_db/0,
         dir_to_graph/2,
         copy_rec/2, copy_rec/3,
         del_rec/1, createPath/1,
         del_rec_if_exists/1]).

-export([start_global_coverage/2,finish_global_coverage/2]).

-export([print_time/3, timer_tc/1]).

-export([draw/1, test_graphs/3]).
-export([hE/1, concat/2, concat/1, flatten/2,
         table_to_html/2, pre2code/1]).

-export([string_to_term/1, parse_testfile/1]).
-export([exec_transform/2]).

-export([error_text/2]).

-include_lib("kernel/include/file.hrl").
-include("test.hrl").

%% Represents a string that has already been HTML escaped.
-record(html,{s}).

%%% -

error_text(no_interaction,[Q]) ->
    ["No interaction is expected, but got ", io_lib:print(Q)].

%%% ============================================================================
%%% Database operations

%% @spec add_files(string(), [string()]) -> ok
%%
%% @doc Adds the given files in the given directory to the database.
add_files(Dir, FileList) ->
    lists:foreach(
      fun (File) ->
              ?FileMan:add_file(filename:join(Dir, File))
      end,
      FileList).

%% @spec clear_files() -> ok
%%
%% @doc Removes all files from the database.
clear_files() ->
    case ?ESG:path(?ESG:root(), [file]) of
        [] ->
            ok;
        [File|_] ->
            ?FileMan:drop_file(File),
            clear_files()
    end.

reset_db() ->
    ui({reset}).

%% @spec dir_to_graph(string(), [string()]) -> egraph()
%%
%% @doc Creates the `egraph' of the given files in the given directory.
dir_to_graph(Dir, FileList) ->
    clear_files(),
    add_files(Dir, FileList),
    G = reflib_egraph:create_egraph(),
    clear_files(),
    G.

%% @spec (string()) -> {filename(),ok}
draw(File)->
     io:format("Dotfile: ~p~n",[File++".dot"]),
     ?DRAW_GRAPH:draw_graph(File++".dot", all),
     os:cmd("dot -Tjpg "++File++".dot -o "++File++".jpg"),
     {File++".jpg", ok}.

%% @spec (atom(), proplist()) -> any()
%% @todo create transform result type
exec_transform(RefactName, Args) when is_atom(RefactName), is_list(Args)->
    case RefactName of
        dupcode ->
            ui({clone_identifierl,
                [{algorithm,filtered_suffix_tree},{minlen,10},
                 {name,clone},{max_invalid_seq_length,0}]});
        _ -> ok
    end,

    {ok,Data} = ui({transform,RefactName,Args ++ [{ask_missing, false}]}),
    
    case RefactName of
        dupcode -> refusr_clone_identifierl_lib:delete_diff_db_hash_clones();
        _ -> ok
    end,
    case Data of
        {result,Results} ->
            [Result] = ?MISC:pgetu([result],Results),
            Result;
        _ -> Data
    end.

%% @private
ui(NameArgs)->
    ReqID = ?UI:getid(),
    ok = ?UI:request(ReqID,NameArgs),
    ui_loop(ReqID).

ui_loop(ReqID)->
    ui_loop(ReqID,[]).

ui_loop(ReqID,Errors)->
    receive
        {ReqID, reply, R} ->
%            ?d(R),
            case Errors of
                []     -> R;
                [Error]-> {ok,{error,Error}}
            end;
        {ReqID, progress, _D} ->
            ui_loop(ReqID,Errors);
        {ReqID, question, Q={QID,_Questions}} ->
            ui({cancel, QID}),
            ui_loop(ReqID,[?LocalError(no_interaction,[Q])])
    end.

%%% ============================================================================
%%% File operations

%% @private
%% TODO: figure out what this could be good for
%% @doc Creates valid path from the element of the parameter list.
createPath(L) -> filename:join(L).


copy_rec(Dir1_,Dir2_,File)->
    Dir1 = filename:join(Dir1_,File),
    Dir2 = filename:join(Dir2_,File),
    copy_rec(Dir1,Dir2).

%% @spec copy_rec(string(), string()) -> ok | {error,string()}
%%
%% @doc Copies `Source' to `Dest' recursively, skipping every file
%%      called ".svn".
copy_rec(Source, Dest) ->
    case filelib:is_file(Source) of
        false ->
            {error,Source ++ " not found"};
        true  ->
            copy_rec_(Source,Dest)
    end.

% @private
copy_rec_(Source,Dest) ->
    case filelib:is_dir(Source) of
        false ->
            {ok, _} = file:copy(Source, Dest);
        true  ->
            ok = filelib:ensure_dir(filename:join(Dest,".")),
            {ok, FileNames} = file:list_dir(Source),
            lists:foreach(
              fun (FileName) ->
                      copy_rec(filename:join(Source, FileName),
                               filename:join(Dest, FileName))
              end,
              FileNames -- [".svn"])
    end,
    ok.

%% @spec del_rec(string()) -> ok
%%
%% @doc Deletes the given object (file or directory) recursively.
del_rec(Object) ->
    {ok, #file_info{type = FileType}} = file:read_file_info(Object),
    case FileType of
        regular ->
            ok = file:delete(Object);
        directory ->
            {ok, FileNames} = file:list_dir(Object),
            lists:foreach(
              fun (FileName) ->
                      del_rec(filename:join(Object, FileName))
              end,
              FileNames),
            ok = file:del_dir(Object)
    end,
    ok.

%% @spec del_rec_if_exists(string()) -> ok
%%
%% @doc Deletes the given object (file or directory) recursively, if it exists.
del_rec_if_exists(Object) ->
    case filelib:is_file(Object) of
        true ->
            del_rec(Object);
        false ->
            ok
    end.

%%% ============================================================================
%%% Coverage

%% @doc Initialises the coverage engine for all beam files in RefactorErl.
start_global_coverage([], _) ->
    ok;
start_global_coverage(Covers, Mods) ->
    io:format("Starting coverage "),
    BeamPath = createPath([".", "lib", "refactorerl", "ebin"]),

    _PID = case cover:start() of
               {ok,PID} ->
                   PID;
               {error,{already_started,PID}} ->
                   PID
           end,
    case Covers of
        all ->
            io:format("of all modules in ~p,", [BeamPath]),
            cover:compile_beam_directory(BeamPath);
        tr  ->
            io:format("of all used transformations,"),
            Prefix = "reftr_",
            [cover:compile_beam(list_to_atom(Prefix ++ M)) || M <- Mods ];
        _   ->
            io:format("of the given modules,"),
            [cover:compile_beam(Mod) || Mod <- Covers ]
    end,
    io:format(" covering the following modules:~n    ~p ~n", [cover:modules()]).

%% @doc Ends the module coverage, writes the results to html files.
finish_global_coverage([], _) ->
    ok;
finish_global_coverage(_, CoverDir) ->
%    CoverDir = "../autotested",
    io:format("Coverage finalisation:~n"),
    lists:foreach(
        fun(Mod) ->
            ModStr = atom_to_list(Mod),
            CoverPath = createPath([CoverDir, ModStr ++ ".cover.html"]),
            cover:analyse_to_file(Mod, CoverPath, [html]),
            io:format(" * coverage of ~p written to file~n",[Mod])
        end,
        cover:modules()),
    Res = cover:stop(),
    io:format("done.~n"),
    Res.


%%% ============================================================================
%%% Timing feedback functions

print_time(Func,Fun,ArgL) when is_atom(Fun) ->
    print_time(Func,io_lib:write(Fun),ArgL);

print_time(Func,{Mod,Fun},ArgL) when is_atom(Mod), is_atom(Fun) ->
    print_time(Func,io_lib:write(Mod)++":"++io_lib:write(Fun),ArgL);

print_time(Func,Fun,ArgL) when is_list(Fun), is_function(Func,0) ->
    ArgS = ?MISC:join([io_lib:print(A)||A<-ArgL]),
    io:format("~s(~s):",[Fun,ArgS]),
    {Time,Res} = timer_tc(Func),
    io:format(" ~f seconds.~n",[Time/1.0e6]),
    Res.

%% @private
timer_tc(Func) when is_function(Func,0) ->
    T1  = now(),
    Res = Func(),
    T2  = now(),
    {timer:now_diff(T2,T1),Res}.

%%% ============================================================================
%%% HTML and string related routines

%% TODO: maybe move to a new HTML module?

%% -spec hE/1 :: (any()) -> #html{}.
%% @doc HTML escape a character string or other term
%% @spec (any()) -> #html{}
%% @throws {double_html_escape,any()}
hE(Data=#html{}) ->
    throw({double_html_escape,Data});
hE(Data)->
    String = ?MISC:any_to_string(Data),
    EscapeChar = fun($&) -> "&amp;";
                    ($<) -> "&lt;" ;
                    ($>) -> "&gt;" ;
                    (C) when (C/=$\n) and (C<32) ->
                            "&#" ++ integer_to_list(C) ++ ";";
                    (C)  -> [C]    end,
    S = lists:flatmap(EscapeChar, String),
    #html{s=S}.

%% TODO: think about a deep_list solution
%% @spec (#html{},#html{}) -> #html{}
concat(#html{s=A},#html{s=B}) ->
    #html{s=A++B}.

%% @spec ([#html{}]) -> #html{}
concat(L) ->
    lists:foldr(fun concat/2, hE(""), L).

%% @spec (#html{}|[any()],#html{}|[any()]) -> #html{}
flatten(A,B) ->
    concat(flatten(A),flatten(B)).

%% @spec (#html{}|[any()]) -> #html{}
%% @doc flattens a #html{} deeplist
flatten(A=#html{}) ->
    A;
flatten(L) ->
    lists:foldr(fun flatten/2, hE(""), L).

%% @spec ([any()],string()) -> #html{}
%% @doc convert a matrix into an HTML table
%% @end
%table_to_html(Table) -> table_to_html(Table,"").
table_to_html(Table,Style)->
    Cell = fun(X)-> hE(X) end,
    Cols = fun(R)-> [[#html{s="<td>"},Cell(C),#html{s="</td>"}] || C <- R] end,
    Rows =          [[#html{s="<tr>"},Cols(R),#html{s="</tr>"}] || R <- Table],
    flatten([#html{s="<table "++Style++">"},
             Rows,
             #html{s="</table>"}]).


%% @doc Converts from 'pre' embeddable format to 'code' embeddable format.
%% @spec (#html{}) -> #html{}
%% @end
%% TODO: would it be feasible to create separate types for these two?
pre2code(#html{s=Pre})->
    Code = string:join([each_line(L) || L<-?MISC:lines(Pre)],"<br />"),
    #html{s=Code}.

% @private
each_line(S) ->
    {Init,Tail} = lists:splitwith(fun(C)->C==$ end,S),
    blank_nbsp(Init) ++ multi_nbsp(Tail).

% @private
blank_nbsp(L) ->
    lists:flatmap(fun($ ) ->
                          "&nbsp;"
                  end,L).

% @private
multi_nbsp(L) ->
    Inner = fun(Bl=[$ ,$ |_]) -> blank_nbsp(Bl);
               (X)            -> X
            end,
    lists:flatmap(Inner,?MISC:group(L)).

%%% ============================================================================%%% Syntactical graph comparison from test_referl_rename_fun
%%%

%% @spec test_graphs(egraph(), egraph(), data_checker_fun()) -> bool()
%%
%% @doc Tests whether the two graphs are isomorph and `Fun' is true for all
%% node-pairs. (The first node is from the first graph, the second is from the
%% second).
test_graphs(Graph1, Graph2, Fun) ->

    %% The queue stores the nodes that are waiting to be processed.
    RootQueue = queue:from_list([?ESG:root()]),

    %% The "found" set stores the nodes that are already found (they are either
    %% in the queue or they are processed).
    RootFound1 = root_set(),
    RootFound2 = root_set(),

    Result = test_graphs(Graph1, Graph2, RootQueue, RootQueue,
                         RootFound1, RootFound2, Fun),
    ets:delete(RootFound1),
    ets:delete(RootFound2),
    Result.

%% @spec root_set() -> ets(node())
%%
%% @doc Returns an ets set that contains only the root node of the graph.
%%
%% @todo Is this solution OK, or should I store the root of the graphs somehow?
root_set() ->
    E = ets:new(root_set, []),
    ets:insert(E, {?ESG:root()}),
    E.

%% @spec test_graphs(egraph(), egraph(), queue(node()), queue(node()),
%%                   ets(node()), ets(node()), data_checker_fun()) -> bool()
%%
%% @doc `Q1' stores the nodes that are waiting to be processed in `G1'.
%% `Found1' stores the nodes that are already found in `G1' (they are either in
%% the queue or they are processed).
%% `Q2' and `Found2' do the same for `G2'.
%% `F' is the function that is used to check the node-pairs.
test_graphs(G1, G2, Q1, Q2, Found1, Found2, F) ->
    case {queue:is_empty(Q1), queue:is_empty(Q2)} of
        {false, false} ->
            {_H1, H1Data, Q1New} = process_head(G1, Q1, Found1),
            {_H2, H2Data, Q2New} = process_head(G2, Q2, Found2),
            case F(H1Data, H2Data) of
                true ->
                    test_graphs(G1, G2, Q1New, Q2New, Found1, Found2, F);
                false ->
                    false
            end;
        {true, true} -> % all nodes of both graphs were examined
            true;
        _ ->
            {false,"The graphs are not isomorph."}
    end.

%% @spec process_head(egraph(), queue(node()), ets(node())) ->
%%           {node(), data(), queue(node())}
%%
%% @doc "Removes" the first element from the queue and adds its neighbours to
%% the `Found' list and to the queue.
process_head(G, Q, Found) ->
    {{value, H}, Q2} = queue:out(Q),
    [{Node, {Data, Links}}] = ets:lookup(G, H),

    QNew = lists:foldl(
             fun({_Tag,OtherNode}, Q3) ->
                     case ets:lookup(Found, OtherNode) of
                         [] ->
                             ets:insert(Found, {OtherNode}),
                             queue:in(OtherNode, Q3);
                         _ ->
                             Q3
                     end
             end,
             Q2,
             Links),
    {Node, Data, QNew}.

%%% ============================================================================
%%% TEST file parsing
%%%

%% @doc parse a string into an Erlang term
%% @spec (string()) -> {ok, term()} |
%%           {error, ErrorInfo, EndLocation} | {error, ErrorInfo}
string_to_term(String) ->
    case erl_scan:string(String) of
        {ok,Tokenized,_EndLine} ->
            erl_parse:parse_term(Tokenized);
        Error ->
            Error
    end.

%% @type testfile_string() = string()

%% @type testfile_prop() = [{atom(),string()}]

%% @doc parse a TEST file into a proplist
%% @spec (filename()) -> tesfile_prop()
parse_testfile(File) ->
    {ok,Binary} = file:read_file(File),
    %% Probably "4.5.5" is enough, but I don't have R12B5 right now to check
    case proplists:get_value(version, module_info(compile)) >= "4.6" of
        true ->
            {match, Result} =
                re:run(Binary,
                       "([^ \n\r\t:]+):\\h*((?:.|(?:\\v\\h+))+)",
                       [global, {capture, all_but_first, list}]),
            [{list_to_atom(Key),
              re:replace(Value, "\\v\\h+", " ", [global, {return, list}])} ||
                [Key, Value] <- Result];
        false ->
            parse_testfile_(binary_to_list(Binary))
    end.

%% @spec (testfile_string()) -> testfile_prop()
parse_testfile_(IString) ->
    String = [$\n|?MISC:dos2unix(IString)],
    Filter_nonlf = fun(S) -> lists:filter( fun(C)->C/=10 end, S ) end,
    Matches_to_proplist = fun([H|T]) ->
        lists:zipwith(
            fun({B1,L},{B2,_})->
                {list_to_atom(lists:sublist(String,B1+1,L-2)),
                 Filter_nonlf(lists:sublist(String,B1+L,B2-B1-L))}
            end,
            [H|T],
            T ++ [{length(String),0}]) end,
    case re:run(String, "\n[A-Z][a-z_]*:", [global, {capture, first}]) of
        {match, [H | T]} ->
            SM = lists:sort([H|T]),
                         Matches_to_proplist(SM);
        _ -> []
    end.

