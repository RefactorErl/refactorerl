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

%%% @author Tibor Pusztai <kondi@elte.hu>
%%% @author Viktoria Fordos <f-viktoria@elte.hu>

-module(referl_ui_web2_services).
-vsn("$Rev$").

-export([start/2, stop/0, execute_query/2, execute_search/2, execute_graph/2, is_restricted/0,
         execute_duplicates/2, get_queue_ids/1, get_queue/1,
         get_dupcode_names/0, get_dupcode_job_by_name/1, get_state/1,
         transform_dupcode_params/1,
         get_type_and_result/1, mark_seen/1, get_parameters/1,
         get_directories/0, get_errors/0, get_file/1, get_mods_with_path/0,
         get_duplicated_diff/3, get_predefineds/2, display_node/1,
         dupcode_algorithms/0, get_fs_files/1, search_fs_files/1, get_db_files/1,
         search_db_files/1, add_to_db/3, update_in_db/3, drop_from_db/3,
         add_appbase/1, add_include/1, save_skeleton/2, get_history/2,
         remove_job/2, get_funs/1, regenerate/1, encode_starting_point/1,
         decode_position_from_starting_point/1, decode_node_from_starting_point/1,
         is_valid_web2_pass/2, set_web2_pass/2, save_as_dynfun/3,
         show_dynfuns/1, delete_dynfuns/0]).

-export([error_text/2]).

-define(ID_TAB,
    filename:join([?MISC:data_dir(),
                   "web2_id_table_v3"++
                   ?MISC:graph_based_postfix()])).
-define(JOB_TAB,
    filename:join([?MISC:data_dir(),
                   "web2_job_table_v3"++
                   ?MISC:graph_based_postfix()])).
-record(referl_web2_job, {id, type, parameters, owner, result, running,
                          seen, database_hash, launched_date, finished_date,
                          cancel_key}).

%-record(referl_web2_counter, {key, counter}).

-define(CONFIG_TAB,
    filename:join([?MISC:data_dir(),
                   "web2_config_table_v3"++
                   ?MISC:graph_based_postfix()])).
-record(referl_web2_config, {key, value}).

-define(PASSWORD_TAB,
    filename:join([?MISC:data_dir(),
                   "web2_password_table_v3"++
                   ?MISC:graph_based_postfix()])).
-record(referl_web2_password, {user, password}).
-define(PASSWORD_SALT, "g?x2$g!3>a@f2be&").

-define(SQ_TAB,
    filename:join([?MISC:data_dir(),
                   "web2_query_table_v3"++
                   ?MISC:graph_based_postfix()])).
-record(referl_web2_cached_sq, {req, result, db_hash}).

-define(DYNFUN_TAB,
    filename:join([?MISC:data_dir(),
                   "web2_dynfun_table_v3"++
                   ?MISC:graph_based_postfix()])).
-record(referl_web2_dynfun, {file, position, gnode, text, user, timestamp,
        dbhash, seen=false}).

-include("ui.hrl").
-include_lib("referl_core/include/core_export.hrl").


%%%% Error handling

error_text(os_dot_error, [Target]) ->
    lists:flatten(["While the result of the dependency analysis was being generated,",
    " the conversion to ", Target, " failed."]);
error_text(file_read_error, File) ->
    "Error while reading the following file: " ++ File.

%%%% Public interface

start(BrowserRoots, RestrictedMode) ->
    set_config(browser_root,
               [normalize_path(BRoot) || BRoot <- BrowserRoots]),
    set_config(restricted_mode, RestrictedMode).

stop() ->
    ok.

is_restricted() ->
    get_config(restricted_mode).

set_web2_pass(User, Password) ->
    Hash = erlang:md5(?PASSWORD_SALT++Password++User),
    set_password(User, Hash).

is_valid_web2_pass(User, Password) ->
    case ?WEB2_SERVICES:is_restricted() andalso User=="admin" of
        false ->
            not_needed;
        _ ->
            Hash = erlang:md5(?PASSWORD_SALT++Password++User),

            case get_password(User) of
                Hash -> true;
                undef -> not_set;
                _ -> false
            end
    end.

save_as_dynfun(User, Ref, Display) ->
    Point = decode_position_from_starting_point(Ref),
    Dynfun = #referl_web2_dynfun{
        file        = proplists:get_value(file, Point),
        position    = proplists:get_value(position, Point),
        gnode       = decode_node_from_starting_point(Ref),
        text        = Display,
        user        = User,
        dbhash      = ?MISC:database_hash(),
        timestamp   = erlang:now()},
    save_dynfun(Dynfun).

delete_dynfuns() ->
    dets_delete_all(web2_dynfun).

show_dynfuns(Options) ->
    Items = dets_foldr(web2_dynfun,
        fun(Fun, AccIn) ->
            AccIn ++ [Fun]
        end, []),
    Filters = proplists:get_all_values(filter, Options),
    Filtered = lists:foldl(fun dynfun_filter/2, Items, Filters),
    Ordered = order_dynfuns(Filtered),

    mark_dynfuns_seen(Filtered),

    case proplists:get_value(out, Options) of
        stdio ->
            io:format("~s~n",[dynfuns_to_text(Ordered)]),
            ok;
        stdout ->
            io:format("~s~n",[dynfuns_to_text(Ordered)]),
            ok;
        {file, File = [C|_]} when is_integer(C) ->
            {ok, IODev} = file:open(File, [write]),
            io:put_chars(IODev, dynfuns_to_text(Ordered)),
            ok = file:close(IODev),
            ok;
        _ ->
            Ordered
    end.

dynfuns_to_text(Items) ->
    CurrentHash = ?MISC:database_hash(),
    io_lib:format("|~-25s|~-50s|~-11s|~-11s|~-7s|~-9s|~n", ["File & Position",
        "Program text", "Reported by", "Reported at", "Is new?",
        "Is valid?"]) ++
    io_lib:format("|~25..-s|~50..-s|~11..-s|~11..-s|~7..-s|~9..-s|~n",
        ["","","","","",""]) ++
    lists:append(lists:map(
        fun(#referl_web2_dynfun{user=User, file=File, position=Pos, text=Text}=Item) ->
            {{Year, Mon, Day}, _} =
                calendar:now_to_local_time(Item#referl_web2_dynfun.timestamp),
            Time = io_lib:format("~4..0w-~2..0w-~2..0w",
                        [Year, Mon, Day]),
            Valid = case Item#referl_web2_dynfun.dbhash of
                        CurrentHash -> "Yes";
                        _ -> "No"
                    end,
            New =   case Item#referl_web2_dynfun.seen of
                        true -> "No";
                        false -> "Yes"
                    end,
            FilePos = io_lib:format("~s:~w", [filename:basename(File), Pos]),
            io_lib:format("|~-25s|~-50s|~-11s|~-11s|~-7s|~-9s|~n",
                [FilePos, Text, User, Time, New, Valid])
        end, Items)).

dynfun_filter(seen, Acc) ->
    dynfun_filter({seen, true}, Acc);
dynfun_filter(new, Acc) ->
    dynfun_filter({seen, false}, Acc);
dynfun_filter({seen, Bool}, Acc) ->
    lists:filter(
        fun (#referl_web2_dynfun{seen = Seen}) ->
            Seen == Bool
        end, Acc);
dynfun_filter({file, Name}, Acc) ->
    lists:filter(
        fun (#referl_web2_dynfun{file = Path}) ->
            re:run(Path, Name, [{capture, none}]) == match
        end, Acc);
dynfun_filter(valid, Acc) ->
    CurrentHash = ?MISC:database_hash(),
    lists:filter(
        fun (#referl_web2_dynfun{dbhash = Hash}) ->
            Hash == CurrentHash
        end, Acc).

order_dynfuns(Items) ->
    CurrentHash = ?MISC:database_hash(),
    WithOrder = lists:map(
        fun(#referl_web2_dynfun{timestamp = Time, dbhash = Hash} = Item) ->
            Order = element(1, Time)*1000000 + element(2, Time) +
                case Hash of
                    CurrentHash -> 10000000000;
                    _ -> 0
                end,
            {Order, Item}
        end, Items),
    Ordered = lists:reverse( lists:keysort(1, WithOrder) ),
    lists:map(fun({_, Item}) -> Item end, Ordered).


mark_dynfuns_seen(Items) ->
    lists:foreach(
        fun(Fun) ->
            case Fun of
                #referl_web2_dynfun{seen=false} = Item ->
                    save_dynfun(Item#referl_web2_dynfun{seen=true});
                _ -> null
            end
        end, Items).

encode_starting_point({Node, File, Pos})->
    base64:encode_to_string(
        term_to_binary([{gnode, Node}, {file, File}, {position, Pos}])).

decode_position_from_starting_point(Term)->
    lists:filter(fun({file,_}) -> true;
                    ({position,_}) -> true;
                     (_) -> false
                 end, binary_to_term(base64:decode(Term))).

decode_node_from_starting_point(Term)->
    case proplists:get_value(gnode, binary_to_term(base64:decode(Term))) of
        Node when ?IS_NODE(Node)-> Node;
        _ -> not_found
    end.


execute_query(Parameters, Owner) ->
    Id = next_id(),
    Job = #referl_web2_job{id = Id,
                           type = semantic_query,
                           parameters = Parameters,
                           owner = Owner,
                           database_hash = ?MISC:database_hash(),
                           running = true,
                           seen = false,
                           launched_date = erlang:now()},
    dets_write(web2_jobs, Job),
    ?WEB2_WEBSOCKET:notify(Owner, job_started, Id),
    spawn(fun() -> execute_query_thread(Job) end),
    Id.

execute_search(Parameters, Owner) ->
  Id = next_id(),
  Job = #referl_web2_job{id = Id,
                         type = search,
                         parameters = Parameters,
                         owner = Owner,
                         database_hash = ?MISC:database_hash(),
                         running = true,
                         seen = false,
                         launched_date = erlang:now()},
  dets_write(web2_jobs, Job),
  ?WEB2_WEBSOCKET:notify(Owner, job_started, Id),
  spawn(fun() -> execute_search_thread(Job) end),
  Id.

execute_graph(Parameters, Owner) ->
    Id = next_id(),
    Job = #referl_web2_job{id = Id,
                           type = graph,
                           parameters = Parameters,
                           owner = Owner,
                           database_hash = ?MISC:database_hash(),
                           running = true,
                           seen = false,
                           launched_date = erlang:now()},
    dets_write(web2_jobs, Job),
    ?WEB2_WEBSOCKET:notify(Owner, job_started, Id),
    spawn(fun() -> execute_graph_thread(Job) end),
    Id.

execute_duplicates(Parameters, Owner) ->
    Id = next_id(),
    Job = #referl_web2_job{id = Id,
                           type = duplicates,
                           parameters = Parameters,
                           owner = Owner,
                           database_hash = ?MISC:database_hash(),
                           running = true,
                           seen = false,
                           launched_date = erlang:now()},
    dets_write(web2_jobs, Job),
    ?WEB2_WEBSOCKET:notify(Owner, job_started, Id),

    case Parameters of
        [{"name", Name}] ->
            spawn(fun() -> dupcode_result_by_name(list_to_atom(Name), Job) end);
        [{"forSelection",true}|_] ->
            spawn(fun() -> execute_duplicates_by_selection(Job) end);
        _ ->
            spawn(fun() -> execute_duplicates_thread(Job) end)
    end,
    Id.

get_dupcode_job_by_name(Name) ->
    dets_foldr(web2_jobs,
        fun(#referl_web2_job{type=duplicates, id=Id, result={List, _}}, AccIn) ->
            case proplists:get_value(name, lists:flatten([List]), List) of
                Name -> [Id];
                _ -> AccIn
            end;
            (_, AccIn) -> AccIn
        end, [] ).

get_queue_ids(User) ->
    Running = dets_match_object(web2_jobs, #referl_web2_job{owner=User,
                                                            running=true,
                                                            _='_'}),
    RunningIds = [ Id || #referl_web2_job{id = Id} <- Running ],
    Unseen = dets_match_object(web2_jobs, #referl_web2_job{owner=User,
                                                           seen=false,
                                                           _='_'}),
    UnseenIds = [ Id || #referl_web2_job{id = Id} <- Unseen ],
    [{running, RunningIds},{unseen, UnseenIds}].

get_queue(User) ->
    Items = dets_foldr(web2_jobs,
        fun(Rec, AccIn) ->
            case (Rec#referl_web2_job.owner =:= User) andalso
                 ((Rec#referl_web2_job.running =:= true) orelse
                  (Rec#referl_web2_job.seen =:= false)) of
                true -> AccIn ++ [job_to_proplist(Rec)];
                false -> AccIn
            end
        end, [] ),
    Ordering = fun(A, B) ->
                   Ax = proplists:get_value(launched, A),
                   Bx = proplists:get_value(launched, B),
                   Ax =< Bx
               end,
    lists:sort(Ordering, Items).

get_history(PageName, User) ->
    Type = case PageName of
        undefined -> undefined;
        _         -> page_name_to_type(PageName)
    end,
    TypeFilter = case Type of
        undefined -> fun(_) -> true end;
        _         -> fun(#referl_web2_job{type=T}) -> T =:= Type end
    end,
    % temporary solution to show all items in the history and to handle permissions
    %UserFilter = case User of
        %undefined -> fun(_) -> true end;
        %_         -> fun(#referl_web2_job{owner=O}) -> O =:= User end
    %end,
    %Items = dets_foldr(web2_jobs,
        %fun(Job, AccIn) ->
            %case UserFilter(Job) andalso TypeFilter(Job) of
                %true -> AccIn ++ [job_to_proplist(Job)];
                %false -> AccIn
            %end
        %end, []),
    Items = dets_foldr(web2_jobs,
        fun(Job, AccIn) ->
            case TypeFilter(Job) of
                true -> AccIn ++ [job_to_proplist_with_permissions(Job, User)];
                false -> AccIn
            end
        end, []),
    Ordering = fun(A, B) ->
                   Ax = proplists:get_value(launched, A),
                   Bx = proplists:get_value(launched, B),
                   Ax >= Bx
               end,
    lists:sort(Ordering, Items).

get_type_and_result(Id) ->
    case get_job(Id) of
        undefined -> {undefined, undefined};
        Job       -> Type = Job#referl_web2_job.type,
                     Result = Job#referl_web2_job.result,
                     {Type, Result}
    end.

get_state(Id) ->
    State = case get_job(Id) of
        undefined -> undefined;
        Job ->
            {struct, [
                {id, Job#referl_web2_job.id},
                {dbchanged,
                    (Job#referl_web2_job.database_hash /= ?MISC:database_hash())},
                {'forSelection',
                    proplists:get_value("forSelection",
                                        Job#referl_web2_job.parameters, false)}]}
    end,
    {state, State}.

get_parameters(Id) ->
    case get_job(Id) of
        undefined -> undefined;
        Job       -> Job#referl_web2_job.parameters
    end.

mark_seen(Id) ->
    update(Id, fun(J) -> J#referl_web2_job{seen = true, running = false} end),
    case dets_match_object(web2_jobs, #referl_web2_job{id=Id,_='_'}) of
        [Job] ->
            Owner = Job#referl_web2_job.owner,
            ?WEB2_WEBSOCKET:notify(Owner, job_seen, Id);
        _ ->
            ok
    end.

get_directories() ->
    case make_ui_request({dir_sort}) of
        {error, _}       -> [];
        {ok, {error, _}} -> [];
        {ok, Dirs}       -> [Dir || {Dir, _} <- Dirs, is_list(Dir), Dir /= "Other"]
    end.

get_errors() ->
    ?WEB2_CACHE:get("errors", fun ?NITRO_HELPER:get_error_forms_in_database/0).

get_file(Path) ->
    % @FIXME UTF8 files aren't handled properly
    % {ok, Ans} -> {content, tr_if_unicode(Ans)};
    % Content -> {content, tr_if_unicode(Content)}
    case catch ?FileCacheMan:get_file_content(Path) of
        {?FileCacheMan, not_in_db, _} ->
            case file:read_file(Path) of
                {ok, Binary} -> {content, Binary};
                {error, Err} -> {error, file:format_error(Err)}
            end;
        Content when is_list(Content) ->
            {content, Content};
        _ ->
            {error, "An unexpected error occured."}
    end.

% tr_if_unicode(Content) when is_binary(Content); is_list(Content) ->
%     case heuristic_encoding_bin(Content) of
%         latin1 -> Content;
%         utf8 -> unicode:characters_to_binary(Content,utf8,latin1)
%     end;
% tr_if_unicode(Content)->
%     Content.

% % http://www.erlang.org/doc/apps/stdlib/unicode_usage.html#id67546
% heuristic_encoding_bin(Bin) ->
%     case unicode:characters_to_binary(Bin,utf8,utf8) of
%     Bin ->
%         latin1;
%     _ ->
%         utf8
%     end.


get_mods_with_path() ->
    Request = {transform, semantic_query,
               [{ask_missing, false},
                {send_back_query_id, true},
                {querystr, "mods.path"},
                {display_opt, [{output, other}]},
                {start_opt, []}]},
    case make_ui_request(Request) of
        {ok, {result, [{result, Groups}]}} ->
            List = [{Name, Path} ||
                    {group_by, {_, Name}, eq, _, Path} <- Groups,
                    Name =/= Path],
            lists:keysort(1, List);
        Error        -> Error
    end.

get_dupcode_names() ->
    case make_ui_request({get_all_dupcode_result}) of
        {ok, Result} ->
            N = lists:flatten(lists:map(fun([Names,_,_])->Names end, Result)),
            lists:map(fun atom_to_list/1, N);
        _ -> []
    end.

dupcode_result_by_name(Name, Job) ->
    case make_ui_request({get_dupcode_result, Name, ui_format, linecol}) of
        {ok, Result} ->
            Res = proplists:get_value(detected_clones, Result),
            Params = proplists:get_value(options, Result),

            {struct, AlgParams} = transform_dupcode_params(
                                      proplists:get_value(
                                          proplists:get_value(algorithm, Params),
                                          dupcode_algorithms())),

            {array, AlgArgs} = proplists:get_value(args, AlgParams),
            NewArgs = lists:map(
                fun({struct, PL}) ->
                    Key =   case proplists:get_value(key, PL) of
                                K when is_list(K) -> list_to_atom(K);
                                K -> K
                            end,
                    Type =  case proplists:get_value(type, PL) of
                                "enum" -> proplists:get_value(enumtype, PL);
                                T -> T
                            end,
                    NewVal =  case proplists:get_value(Key, Params) of
                                  undefined -> undefined;
                                  V when is_atom(V) -> atom_to_list(V);
                                  V -> V
                              end,
                    case NewVal of
                        undefined ->
                            {struct, PL};
                        _ ->
                            case Type of
                                "atoms" ->
                                    {struct, [{selected, NewVal} |
                                              proplists:delete(selected, PL)]};
                                _ ->
                                    {struct, [{default, NewVal} |
                                              proplists:delete(default, PL)]}
                            end
                    end
                end, AlgArgs),

            NewParams = [ {args, {array, NewArgs}} |
                          proplists:delete(args, AlgParams)],

            Update = fun(J) -> J#referl_web2_job{parameters = NewParams} end,
            update(Job#referl_web2_job.id, Update),

            finished(Job, {[{name, Name}], Res});
        Error -> failed(Job, Error)
    end.

get_duplicated_diff(Id, GroupIndex, Indexes) ->
    Job = get_job(Id),
    {_Key, Groups} = Job#referl_web2_job.result,
    Group = lists:nth(GroupIndex, Groups),
    [Nodes1, Nodes2] = [proplists:get_value(nodes, lists:nth(Index, Group)) ||
                        Index <- Indexes],
    case make_ui_request({duplicated_code_const_var_diff, Nodes1, Nodes2}) of
        {ok, Diffs} -> transform_duplicated_diffs(Diffs);
        Error       -> Error
    end.

get_predefineds(FilePath, Pos) ->
    case is_file_in_db(FilePath) andalso
            refusr_predef:get_node_and_type(FilePath, Pos) of
        false ->
            {error, {nofile, "File not in db"}};
        {error, _} = E -> E;
        {Node, Type} ->
            {Node, Type, refusr_predef:get_predef_queries(Type, FilePath, Pos)}
    end.

is_file_in_db(FilePath) ->
    case lists:member(FilePath, get_db_files()) of
        true -> true;
        _ -> false
    end.

display_node(Node) ->
    case lists:flatten(?NITRO_SERVICES:node_to_text(Node)) of
        "Node does not exist anymore." -> "";
        Else -> Else
    end.


get_fs_files(undefined) ->
    case get_config(browser_root) of
        BRoots when length(BRoots) > 1 ->
            return_filelist(
                [{not filelib:is_dir(P), P} || P <- BRoots]);
        Broot -> get_fs_files(Broot)
    end;

get_fs_files(Path=[H|_]) when not is_list(H) ->
    get_fs_files([Path]);

get_fs_files(Paths=[H|_]) when is_list(H) ->
    % @FIXME: use more sophisticated error reporting!
    % This solution causes internal server error (HTTP 500)
    % which gives no information to the user.
    % Currently, 'empty directory' appears in the browser, if some errors occur.
    IsPermitted = lists:all(
        fun(Path) ->
            lists:any(fun(Broot)->
                starts_with(Path, Broot)
            end,get_config(browser_root))
        end, Paths),
    case IsPermitted of
        false -> throw(permission_denied);
        true -> ok
    end,
    FileList = lists:foldl(fun(Path, FileList0)->
        Pattern = Path ++ "/*",
        FL00 = ordsets:from_list([{not filelib:is_dir(P), P}
                || P <- filelib:wildcard(Pattern),
                   filelib:is_file(P)]),
        ordsets:union(FileList0, FL00)
    end, ordsets:new(), Paths),
    return_filelist(FileList).

search_fs_files(Filter) ->
    Pattern = "**/*" ++ Filter ++ "*",
    FileList = lists:foldl(fun(BRoot, FileList0)->
        FL00 = ordsets:from_list(
            [{true, Path} || Path <- search0(Pattern, BRoot)]),
        ordsets:union(FileList0, FL00)
    end, ordsets:new(), get_config(browser_root)),
    return_filelist(FileList).

search0(Pattern, Root)->
    lists:filter(fun(AbsPath) ->
        filelib:is_file(AbsPath) andalso
            not filelib:is_dir(AbsPath)
    end, [filename:absname(Path, Root)
            || Path <- filelib:wildcard(Pattern, Root)]).

get_db_files(Path) ->
    Paths = get_db_files(),
    FileList = case Path of
        undefined ->
            Len = length(get_common_root(Paths)) + 1,
            ExplodedPaths = [ filename:split(F) || F <- Paths ],
            Res = lists:map(
                fun(P) ->
                    AFN=filename:absname(filename:join(lists:sublist(P, Len))),
                    {not(filelib:is_dir(AFN)), AFN}
                end, ExplodedPaths),
            lists:usort(Res);
        _ -> get_db_children(Paths, Path)
    end,
    return_filelist(FileList).

get_funs(Path) ->
    try
        lists:sort(
            fun([{text, A}|_], [{text, B}|_]) -> A < B end,
            [ [{text, fun_string(F)},
               {pos, {struct, get_node_position(F, Path)}}]
               || F<-?NITRO_HELPER:query_request(?Query:seq([?File:find(Path),
                                                             ?File:module(),
                                                             ?Mod:locals_all()]))])
    catch
        request_denied -> []
    end.

get_node_position(Node, Path) ->
    case make_ui_request({function_positions,Node}) of
        {ok, {_, [{_,{{Start, _},_}}],[{_,{{_,End},_}}]}} ->
            [{first, Start}, {last, End}, {file, Path}, {error, ""}];
        _ ->
            [{first, 0}, {last, 0}, {file, Path}, {error, "error"}]
    end.

fun_string(Node) ->
    lists:flatten([atom_to_list(?NITRO_HELPER:query_request(?Fun, name, Node)),
                   "/",
                   integer_to_list(?NITRO_HELPER:query_request(?Fun, arity, Node))]).

regenerate(File) ->
    ?FileCacheMan:force_generate(File).

search_db_files(Filter) ->
    Escaped = escape_for_re(Filter),
    Pattern0 = re_replace_all(Escaped,  "\\*", "[^/]*"),
    Pattern  = re_replace_all(Pattern0, "\\?", "[^/]"),
    RE = "/[^/]*" ++ Pattern ++ "[^/]*$",
    case re:compile(RE, [caseless]) of
        {ok, Mp}->
            AllFile = get_db_files(),
            Files = [{true, Path} || Path <- AllFile, re:run(Path, Mp) =/= nomatch],
            return_filelist(Files);
        _ ->
            return_filelist([])
    end.

add_to_db(Path, User, IsFolder) ->
    spawn(fun() -> add_to_db_thread(Path, User, IsFolder) end).

update_in_db(Path, User, IsFolder) ->
    spawn(fun() -> update_in_db_thread(Path, User, IsFolder) end).

drop_from_db(Path, User, IsFolder) ->
    spawn(fun() -> drop_from_db_thread(Path, User, IsFolder) end).

add_appbase(Path) ->
    case make_ui_request({add_env, appbase, Path}) of
        {ok, {error, E}} -> {error, E};
        {ok, Res}        -> {ok, Res};
        {error, E}       -> {error, E};
        E                -> {error, E}
    end.

add_include(Path) ->
    case make_ui_request({add_env, include, Path}) of
        {ok, {error, E}} -> {error, E};
        {ok, Res}        -> {ok, Res};
        {error, E}       -> {error, E};
        E                -> {error, E}
    end.

%% Properties: "name", "description", "source", "new" (true or false)
save_skeleton(PropList, User) ->
    Name = proplists:get_value("name",        PropList),
    Desc = proplists:get_value("description", PropList),
    Src =  proplists:get_value("source",      PropList),
    New =  proplists:get_value("new",         PropList),
    Res = case New of
        true  -> ?NITRO_SKELETONS:save_skeleton  (Name, Src, User);
        false -> ?NITRO_SKELETONS:update_skeleton(Name, Src, User)
    end,
    case Res of
        ok -> ?NITRO_SKELETONS:update_prev_skeleton_comment(Name, Desc);
        E  -> E
    end.

remove_job(Id, User) ->
    case get_job(Id) of
        undefined ->
            ok;
        #referl_web2_job{cancel_key = CancelKey} ->
            make_ui_request({kill_query, CancelKey}),
            dets_delete(web2_jobs, Id),
            ?WEB2_WEBSOCKET:notify(User, job_removed, Id)
    end.

%%%% Private functions
dets_open(web2_jobs)->
    dets:open_file(?JOB_TAB, [{keypos, #referl_web2_job.id}]),
    ?JOB_TAB;
dets_open(web2_config)->
    dets:open_file(?CONFIG_TAB, [{keypos, #referl_web2_config.key}]),
    ?CONFIG_TAB;
dets_open(web2_password)->
    dets:open_file(?PASSWORD_TAB, [{keypos, #referl_web2_password.user}]),
    ?PASSWORD_TAB;
dets_open(web2_dynfun)->
    dets:open_file(?DYNFUN_TAB, [{keypos, #referl_web2_dynfun.gnode}]),
    ?DYNFUN_TAB;
dets_open(web2_id)->
    dets:open_file(?ID_TAB, []),
    ?ID_TAB;
dets_open(sq_tab)->
    dets:open_file(?SQ_TAB, [{keypos, #referl_web2_cached_sq.req}]),
    ?SQ_TAB.

dets_write(Table, Value) ->
    T = dets_open(Table),
    dets:insert(T, Value),
    dets:close(T).

dets_read(Table, Id) ->
    T = dets_open(Table),
    Return = dets:lookup(T, Id),
    dets:close(T),
    Return.

dets_foldr(Table, Fun, Acc0) ->
    T = dets_open(Table),
    Return = dets:foldr(Fun, Acc0, T),
    dets:close(T),
    Return.

dets_delete(Table, Id) ->
    T = dets_open(Table),
    dets:delete(T, Id),
    dets:close(T).

dets_delete_all(Table) ->
    T = dets_open(Table),
    dets:delete_all_objects(T),
    dets:close(T).

dets_match_object(Table, Pattern) ->
    T = dets_open(Table),
    Return = dets:match_object(T, Pattern),
    dets:close(T),
    Return.

next_id() ->
    OldId = case dets_read(web2_id, next_id) of
        [] -> 1;
        [{next_id, Id}] -> Id
    end,
    dets_write(web2_id, {next_id, OldId + 1}),
    OldId.

set_config(Key, Value) ->
    Config = #referl_web2_config{key = Key,
                                 value = Value},
    dets_write(web2_config, Config).

get_config(Key) ->
    case dets_read(web2_config, Key) of
        [#referl_web2_config{value = V}] -> V;
        _ -> undef
    end.

set_password(User, Hash) ->
    Pass = #referl_web2_password{user = User,
                                 password = Hash},
    dets_write(web2_password, Pass).

get_password(User) ->
    case dets_read(web2_password, User) of
        [#referl_web2_password{password = V}] -> V;
        _ -> undef
    end.

save_dynfun(Dynfun) ->
    dets_write(web2_dynfun, Dynfun).

normalize_path(Input) ->
    RE = "[/\\\\][^/\\\\]+[/\\\\]\\.\\.",
    Output = re:replace(Input, RE, "", [{return,list}]),
    case Output of
        Input -> Input;
        _     -> normalize_path(Output)
    end.

starts_with(Subject, Starting) ->
    Starting == string:sub_string(Subject, 1, length(Starting)).

% FileList = [{is_not_dir_boolean, FilePath}]
return_filelist(FileList) ->
    Sorted = lists:sort(FileList),
    [[{path, Path},
      {name, filename:basename(Path)},
      {ext, extension(Path)},
      {folder, not NotDir}]
     || {NotDir, Path} <- Sorted,
        true ]. %not starts_with(filename:basename(Path), ".")].

extension(Path) ->
    case filename:extension(Path) of
        [$.|Rest] -> Rest;
        []        -> filename:basename(Path);
        Ext       -> Ext
    end.

% Returns [FilePath]
get_db_files() ->
    ?WEB2_CACHE:get("db/files", fun get_db_files_uncached/0).

get_db_files_uncached() ->
    % gets filelist from UI router
    {result, Files0} = ?NITRO_SERVICES:execute_system_query(
        "files",
        {query_display_opt, [{positions, scalar}], needed_pattern, $:}),
    % removes terminal colon characters
    [lists:sublist(File, length(File)-1) || File <- Files0].

% Returns [{is_not_dir_boolean, FilePath}]
get_db_children(AllFile, Path) ->
    Escaped = escape_for_re(Path),
    DirRE  = "^(" ++ Escaped ++ "/[^/]+)/",
    FileRE = "^"  ++ Escaped ++ "/[^/]+$",
    Matches0 = [{P, re:run(P, DirRE)} || P <- AllFile],
    Matches  = [{P, Match} || {P, Match} <- Matches0, Match =/= nomatch],
    Dirs0 = [string:sub_string(P, From + 1, To)
                || {P, {match, [_, {From, To}]}} <- Matches],
    Dirs  = [{false, P} || P <- lists:usort(Dirs0)],
    Files = [{true, P} || P <- AllFile, re:run(P, FileRE) =/= nomatch],
    Dirs ++ Files.

get_common_root([]) ->
    [];

get_common_root(Paths) ->
    ExplodedPaths = [ filename:split(P) || P <- Paths ],
    lists:foldr(
        fun(List, Acc) ->
            lists:sublist(List, compare_lists(List, Acc))
        end, hd(ExplodedPaths), ExplodedPaths).

compare_lists(List1, List2) ->
    case length(List1) =:= 0 orelse length(List2) =:= 0 of
        true -> 0;
        false -> compare_lists(List1, List2, 1)
    end.
compare_lists(List1, List2, N) ->
    case lists:nth(N, List1) =/= lists:nth(N, List2) of
        true -> N-1;
        false ->
            case length(List1) =:= N orelse length(List2) =:= N of
                false -> compare_lists(List1, List2, N+1);
                true -> N
            end
    end.

escape_for_re(S) ->
    re_replace_all(S, "[\\\\\\$\\^\\(\\)]", "\\\\&").

re_replace_all(S, From, To) ->
    re:replace(S, From, To, [global, {return, list}]).

add_to_db_thread(Path, _User, IsFolder) ->
    ?WEB2_WEBSOCKET:notify(db_start, {add, Path, IsFolder}),
    case lists:any(fun(Broot)-> starts_with(Path, Broot) end,
                   get_config(browser_root)) of
        false -> throw(permission_denied);
        true -> ok
    end,
    Callback = fun db_progress/2,
    Result = case make_ui_request({add_dir, Path}, Callback) of
        {ok, {error, E}} -> {error, E};
        {ok, Res}        -> {ok, Res};
        {error, E}       -> {error, E};
        E                -> {error, E}
    end,
    ?WEB2_WEBSOCKET:notify(db_done, {add, Path, Result}).

update_in_db_thread(Path, _User, IsFolder) ->
    ?WEB2_WEBSOCKET:notify(db_start, {update, Path, IsFolder}),
    Callback = fun db_progress/2,
    Result = case make_ui_request({add_dir, Path}, Callback) of
        {ok, {error, E}} -> {error, E};
        {ok, Res}        -> {ok, Res};
        {error, E}       -> {error, E};
        E                -> {error, E}
    end,
    ?WEB2_WEBSOCKET:notify(db_done, {update, Path, Result}).

drop_from_db_thread(Path, _User, IsFolder) ->
    ?WEB2_WEBSOCKET:notify(db_start, {drop, Path, IsFolder}),
    Callback = fun db_progress/2,
    Result = case make_ui_request({drop_dir, Path}, Callback) of
        {ok, {error, E}} -> {error, E};
        {ok, Res}        -> {ok, Res};
        {error, E}       -> {error, E};
        E                -> {error, E}
    end,
    ?WEB2_WEBSOCKET:notify(db_done, {drop, Path, Result}).

db_progress(progress, Arg) ->
    ?WEB2_WEBSOCKET:notify(db_progress, Arg);

db_progress(_Name, _Arg) ->
    ok.

update(Id, UpdateFun) ->
    case dets_read(web2_jobs, Id) of
        [] -> false;
        [Item0 | _ ] ->
            Item1 = UpdateFun(Item0),
            dets_write(web2_jobs, Item1),
            true
    end.

get_job(Id) ->
    case dets_read(web2_jobs, Id) of
          [Q | _] -> Q;
          []      -> undefined
    end.

type_to_page_name(semantic_query) -> "queries";
type_to_page_name(graph)          -> "graphs";
type_to_page_name(duplicates)     -> "duplicates";
type_to_page_name(search)         -> "queries".

page_name_to_type("queries")    -> semantic_query;
page_name_to_type("graphs")     -> graph;
page_name_to_type("duplicates") -> duplicates;
page_name_to_type("search") -> search.

job_to_proplist_with_permissions(Job, User) ->
    [{canBeDeleted, User =:= Job#referl_web2_job.owner} | job_to_proplist(Job)].

job_to_proplist(Job) ->
    [{id, Job#referl_web2_job.id},
     {dbchanged, (Job#referl_web2_job.database_hash /= ?MISC:database_hash())},
     {running, Job#referl_web2_job.running},
     {parameters, Job#referl_web2_job.parameters},
     {page, type_to_page_name(Job#referl_web2_job.type)},
     {launched, Job#referl_web2_job.launched_date},
     {finished, Job#referl_web2_job.finished_date},
     {owner, Job#referl_web2_job.owner},
     {title, job_title(Job)}] ++
    case {Job#referl_web2_job.running, Job#referl_web2_job.type} of
        {false, duplicates} -> [{name, job_name(Job)}];
        _ -> []
    end.

job_name(#referl_web2_job{type = duplicates, result = {Props, _}}) ->
    atom_to_list(proplists:get_value(name, lists:flatten([Props]), Props)).

job_title(#referl_web2_job{type = semantic_query, parameters = Params}) ->
    QueryText = proplists:get_value("queryText", Params),
    case proplists:get_value("from", Params, undefined) of
        {struct, FromProps} ->
            Display = proplists:get_value("display", FromProps),
            QueryText ++ " FROM " ++ Display;
        _  ->
            QueryText
    end;

job_title(#referl_web2_job{type = duplicates, result = {Props, Groups}}) ->
    NameStr = atom_to_list(proplists:get_value(name, lists:flatten([Props]), Props)),
    "Code duplicates analysis " ++
    case NameStr of
        "error" -> "(Returned error)";
        _ ->
            case length(Groups) < 1 of
                true -> "(No results)";
                _ -> "(Name: " ++ NameStr ++ ")"
            end
    end;

job_title(#referl_web2_job{type = graph, parameters = Params}) ->
    Part1 = case list_to_atom(proplists:get_value("level", Params)) of
        module   -> "Module dependency";
        function -> "Function dependency";
        _        -> "Dependency"
    end,
    Part2 = case list_to_atom(proplists:get_value("format", Params)) of
        text     -> "list";
        svg      -> "graph (svg)";
        smart    -> "graph (smart)";
        _        -> "graph"
    end,
    Part1 ++ " " ++ Part2;

job_title(#referl_web2_job{type = search, parameters = Params}) ->
  proplists:get_value("queryText", Params);

job_title(#referl_web2_job{id = Id}) ->
    "Job #" ++ integer_to_list(Id).


execute_query_thread(Job) ->
    Parameters = Job#referl_web2_job.parameters,
    QueryText0 = proplists:get_value("queryText", Parameters),
    QueryText1 = [case C of $\n -> $\s; _ -> C end
                  || C <- QueryText0, C =/= $\r],
    QueryText = convert_query_if_skel(QueryText1),
    From = proplists:get_value("from", Parameters, undefined),
    StartOptions = case From of
        {struct, FromProps} ->
            Encoded = proplists:get_value("ref", FromProps),
            decode_position_from_starting_point(Encoded);
         _ -> []
    end,
    case StartOptions of
        {error, Error} -> failed(Job, {error, Error});
        _ ->
            Callback = fun(Name, Arg) -> save_cancel_key(Job, Name, Arg) end,
            DisplayOptions = [{positions, both},
                              {output, [?SQ_WEB_DISP_KEY, ?SQ_TEXT_DISP_KEY]}],
            run_cached_sq(QueryText, StartOptions, DisplayOptions, Callback, Job)
    end.

save_cancel_key(Job, query_id, CancelKey) ->
    Id = Job#referl_web2_job.id,
    Update = fun(J) ->
                 J#referl_web2_job{cancel_key = CancelKey}
             end,
    update(Id, Update);

save_cancel_key(_Job, _Name, _Arg) ->
    ok.

run_cached_sq(QueryText, StartOptions, DisplayOptions, Callback, Job) ->
    CurrentDBHash = ?MISC:database_hash(),
    case sq_cache_version({QueryText, StartOptions, DisplayOptions}) of
        CurrentDBHash ->
            serve_from_cache({QueryText, StartOptions, DisplayOptions}, Job);
        _ ->
            run_sq(QueryText, StartOptions, DisplayOptions, Callback, Job)
    end.

run_sq(QueryText, StartOptions, DisplayOptions, Callback, Job)->
    Request =
    {transform, semantic_query,
     [{ask_missing, false},
      {send_back_query_id, true},
      {querystr, QueryText},
      {display_opt, DisplayOptions},
      {start_opt, StartOptions}]},
    case make_ui_request(Request, Callback) of
        {ok, Result} ->
            put_sq_under_cache({QueryText, StartOptions, DisplayOptions}, Result),
            finished(Job, Result);
        Error ->
            failed(Job, Error)
    end.

serve_from_cache(Key, Job)->
    Result = case dets_read(sq_tab, Key) of
        [] -> false;
        [Obj = #referl_web2_cached_sq{}] -> Obj#referl_web2_cached_sq.result
    end,
    finished(Job, Result).

sq_cache_version(Key)->
    Result = case dets_read(sq_tab, Key) of
        [] -> false;
        [Obj = #referl_web2_cached_sq{}] -> Obj#referl_web2_cached_sq.db_hash
    end,
    Result.

put_sq_under_cache(Key, Result) ->
    dets_write(sq_tab, #referl_web2_cached_sq{req=Key, result=Result,
                                              db_hash = ?MISC:database_hash()}).

convert_query_if_skel(Query) ->
    case ?NITRO_SKELETONS:determine_sq_request_type(Query) of
        sem_query -> Query;
        skeleton ->
            {Name, Params} = ?NITRO_SKELETONS:parse_skeleton(Query),
            ?NITRO_SKELETONS:evaluate_skeleton(Name, Params, [], onlyconvert)
    end.

execute_graph_thread(Job) ->
    Id = Job#referl_web2_job.id,
    Owner = Job#referl_web2_job.owner,
    Params = Job#referl_web2_job.parameters,
    Format = list_to_atom(proplists:get_value("format", Params)),
    Level = list_to_atom(proplists:get_value("level", Params)),
    Type = list_to_atom(proplists:get_value("type", Params)),

    BaseOptions = case Level of
        functionblock ->
            {array, SubjectsStructs} = proplists:get_value("subjects", Params),
            Subjects = lists:map(fun get_group/1, SubjectsStructs),
            [{groups, Subjects}];
        _ ->
            {{array, RootJson}, {array, ConnectionJson}, {array, ExcludeStructs},
				{array, NoLeafStructs}, {array, ExcludeLibs}} =
                case Level of
                    module   -> {proplists:get_value("rootModule", Params),
								 proplists:get_value("connectionModule", Params),
                                 proplists:get_value("excludeModules", Params),
                                 proplists:get_value("noLeafModules", Params),
								 proplists:get_value("excludeLibs", Params)};
                    function -> {proplists:get_value("rootFunction", Params),
								 proplists:get_value("connectionFunction", Params),
                                 proplists:get_value("excludeFunctions", Params),
                                 proplists:get_value("noLeafFunctions", Params),
								 proplists:get_value("excludeLibs", Params)}
                end,

            RootOptions = case RootJson of
                [{struct, _}|_] -> RootIds = lists:map(fun get_id/1,RootJson),
                               [{starting_nodes, RootIds}];
                _           -> []
            end,
			ConnectionOptions = case ConnectionJson of
                [{struct, _}|_] -> ConnectionIds = lists:map(fun get_id/1,ConnectionJson),
                               [{connection, ConnectionIds}];
                _           -> []
            end,
            RestOptions =
                [{exclude, lists:map(fun get_id/1, ExcludeStructs)},
                 {exclude_children, lists:map(fun get_id/1, NoLeafStructs)},
                 {exclude_otp, proplists:get_value("excludeOtp", Params)},
				 {exclude_lib, lists:map(fun get_id/1, ExcludeLibs)}],
            RootOptions ++ ConnectionOptions ++ RestOptions
    end,

    BaseName = "web2_" ++ Owner ++ "_" ++ integer_to_list(Id),
    Result = case Format of
        text ->
            case draw_graph(Level, Type, BaseOptions ++ [{output, name_terms}]) of
                {ok, Data} -> {ok, {Format, Data}};
                E -> E
            end;
        smart ->
            DotFile = full_path(BaseName ++ ".dot"),
            TxtFile = full_path(BaseName ++ ".txt"),
            case draw_graph(Level, Type, BaseOptions, DotFile) of
                {ok, _} ->
                    try
                        Text = dot_to_txt(DotFile, TxtFile),
                        Json = transform_smart_graph(Text),
                        {ok, {smart, Json}}
                    catch
                        E = {M, _, _} when is_atom(M) -> ?Error:error_text(E)
                    end;
                E -> E
            end;
        collapsible ->
            case draw_graph(Level, Type, BaseOptions ++ [{output, terms}]) of
                {ok, Term} ->
                    {ok, {collapsible, to_struct(Term)}};
                E -> E
            end;
        svg ->
            DotFile = full_path(BaseName ++ ".dot"),
            SvgFile = full_path(BaseName ++ ".svg"),
            case draw_graph(Level, Type, BaseOptions, DotFile) of
                {ok, _} ->
                    try
                        dot_to_svg(DotFile, SvgFile),
                        {ok, {svg, BaseName ++ ".dot", BaseName ++ ".svg"}}
                    catch
                        E = {M, _, _} when is_atom(M) -> ?Error:error_text(E)
                    end;
                E -> E
            end
    end,
    case Result of
        {ok, OkResult}        -> finished(Job, OkResult);
        {error, {_Code, Msg}} -> failed(Job, "Error: " ++ Msg);
        Error                 -> failed(Job, Error)
    end.

execute_search_thread(Job) ->
  Params = Job#referl_web2_job.parameters,

  Query           = proplists:get_value("queryText", Params),
  DatabaseFolders = lists:map(fun(FileStruct) ->
                                proplists:get_value(path, FileStruct)
                              end, get_db_files(undefined)),
  Res             = os:cmd("egrep -IRbi --exclude-dir .svn '" ++ Query ++ "' " ++ string:join(get_config(browser_root), " ") ++ " " ++ string:join(DatabaseFolders, " ")),
  %Res            = os:cmd("grep -IRbi --exclude-dir .svn '" ++ Query ++ "' " ++ "~/Work/test/"),
  % Handle grep errors
  case re:run(Res, "^egrep\:") of
    {match, _} ->
      Error = string:join(re:split(Res, "^grep\:", [{return, list}]), ""),
      failed(Job, Error);
    _ ->
      ResultLines = string:tokens(Res, "\n"),
      Result      = parse_grep_results(ResultLines),
      finished(Job, {result, [{result, Result}]})
  end.

parse_grep_results(ResultLines) ->
  lists:foldl(fun(ResultLine, ResultProp) ->
                SplittedItems            = string:tokens(ResultLine, " "),
                FileWithStartingPosition = string:tokens(hd(SplittedItems), ":"),
                FilePath                 = hd(FileWithStartingPosition),
                FileName                 = lists:last(string:tokens(FilePath, "/")),
                FilePathWithFileName     = FilePath ++ FileName,
                StartingPosition         = lists:nth(2, FileWithStartingPosition),
                ChunkPart                = re:split(ResultLine, "\d*:", [{return, list}]),
                Chunk                    = string:join(lists:nthtail(2, ChunkPart), " "),
                HasFile                  = lists:foldl(fun(Item, Bool) ->
                                                         Bool or lists:member({filePathWithFileName, FilePathWithFileName}, Item)
                                                       end, false, ResultProp),
                case HasFile of
                  true ->
                    FoundFile = hd(lists:filter(fun(Item) ->
                                                  lists:member({filePathWithFileName, FilePathWithFileName}, Item)
                                                end, ResultProp)),
                    OldChunks = proplists:get_value(chunks, FoundFile),
                    NewProp   = lists:delete(FoundFile, ResultProp),
                    NewChunks = lists:append([[{startingPosition, StartingPosition},
                                               {chunk, Chunk}]], OldChunks),

                    lists:append(NewProp, [[ {fileName, FileName},
                                             {filePath, FilePath},
                                             {filePathWithFileName, FilePathWithFileName},
                                             {chunks, NewChunks} ]]);
                  false ->
                    lists:append(ResultProp, [[ {fileName, FileName},
                                                {filePathWithFileName, FilePathWithFileName},
                                                {filePath, FilePath},
                                                {chunks, [[{startingPosition, StartingPosition},
                                                          {chunk, Chunk}]]} ]])
                end
              end, [], ResultLines).

execute_duplicates_by_selection(Job) ->
    Params = Job#referl_web2_job.parameters,

    LineCol = fun(List) ->
        {proplists:get_value("line", List), proplists:get_value("ch", List)}
    end,

    Algorithm = case proplists:get_value(key, Params, matrix) of
                    K when is_list(K) -> list_to_atom(K);
                    K -> K
                end,
    File = proplists:get_value("file", Params),
    {struct, From} = proplists:get_value("from", Params),
    {struct, To} = proplists:get_value("to", Params),

    %Mod = list_to_atom(filename:rootname(filename:basename(File))),
    Req = {clone_identifierl_by_pos, Algorithm, File,
            LineCol(From), LineCol(To), linecol, ui_format},?d(Req),
    case make_ui_request(Req) of
        {ok, Result} ->
            Name = proplists:get_value(clone_name, Result),
            OutFile = proplists:get_value(output_file_path, Result),
            Res = proplists:get_value(detected_clones, Result),
            finished(Job, {[{name, Name}, {output, OutFile}], Res});
        Error        -> failed(Job, Error)
    end.

execute_duplicates_thread(Job) ->
    Params = Job#referl_web2_job.parameters,

    Argfun = fun({struct, Props}) ->
        Key =   case proplists:get_value("key", Props) of
                    K when is_list(K) -> list_to_atom(K);
                    K -> K
                end,
        Type =  case proplists:get_value("type", Props) of
                    "enum" -> proplists:get_value("enumtype", Props);
                    T -> T
                end,
        Default = proplists:get_value("default", Props),
        Value = case Type of
                    "atoms" ->
                        Array = element(2, proplists:get_value("selected", Props)),
                        [get_id_as_atom(Item) || Item <- Array];
                    "atom" ->
                        list_to_atom(Default);
                    "integer" when not is_integer(Default) ->
                        list_to_integer(Default);
                    "float" when not is_float(Default) ->
                        list_to_float(Default);
                    _ ->
                        Default
                end,
        case Key of
            name ->
                case Value of
                    '' -> [];
                    _ -> [{Key, Value}]
                end;
            _ -> [{Key, Value}]
        end
    end,

    {array, Args} = proplists:get_value("args", Params),
    Options = [{format, ui_format}, {postype, linecol}] ++
        [{algorithm, list_to_atom(proplists:get_value("key", Params))}] ++
        lists:append(lists:map(Argfun, Args)),

    case make_ui_request({clone_identifierl, Options}) of
        {ok, Result} ->
            Name = proplists:get_value(clone_name, Result),
            OutFile = proplists:get_value(output_file_path, Result),
            Res = proplists:get_value(detected_clones, Result),
            finished(Job, {[{name, Name}, {output, OutFile}], Res});
        Error        -> failed(Job, Error)
    end.

dupcode_algorithms() ->
    case make_ui_request({get_algorithms}) of
        {ok, Result} ->
            lists:map(fun dupcode_algorithm_options/1, Result);
        Error ->
            {error, Error}
    end.

dupcode_algorithm_options({Alg, Props}) ->
    case make_ui_request({get_algorithm_data, Alg}) of
        {ok, Result} ->
            {Alg, [{args, Result} | Props]};
        Error ->
            {error, Error}
    end.

get_group({struct, Props}) ->
	Group = string:tokens(proplists:get_value("id", Props), " ,"),
	if length(Group) == 1 -> [G] = Group, G;
		true -> Group
	end.

get_id({struct, Props}) ->
    proplists:get_value("id", Props).

get_id_as_atom(Struct) ->
    list_to_atom(get_id(Struct)).

full_path(FileName) ->
    Dir = ?MISC:data_dir(),
    filename:join([Dir, FileName]).

to_struct([I|_]=List) when is_list(I) ->
    Struct = lists:map(fun to_struct/1, List),
    {array, Struct};
to_struct([I|_]=List) when not is_integer(I) ->
    Struct = lists:map(fun to_struct/1, List),
    {struct, Struct};
%%handling dupcode params() ->
to_struct({options, Opts}) ->
    NewOpts = lists:map(fun to_struct/1, Opts),
    {options, {array, NewOpts}};
to_struct({default, true}) ->
    {default, true};
to_struct({default, false}) ->
    {default, false};
%%;
to_struct({Key, Value}) ->
    {Key, to_struct(Value)};
to_struct(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_struct(Else) ->
    Else.

draw_graph(Level, Type, Options, DotFile) ->
	Lvl = case Level of
			module -> mod;
			function -> func;
			functionblock -> mb
		  end,
	RequestName = draw_graph_request_name(Level, Type),
    FullOptions = Options ++ [{level, Lvl}, {type, Type}, {file_path, DotFile}],
    make_ui_request({RequestName, FullOptions}).

draw_graph(Level, Type, Options) ->
	Lvl = case Level of
			module -> mod;
			function -> func;
			functionblock -> mb
		  end,
    RequestName = draw_graph_request_name(Level, Type),
	FullOptions = Options ++ [{level, Lvl}, {type, Type}],
    make_ui_request({RequestName, FullOptions}).

draw_graph_request_name(module,   all)    -> draw_dep_graph;
draw_graph_request_name(module,   cycles) -> draw_dep_graph;
draw_graph_request_name(function, all)    -> draw_dep_graph;
draw_graph_request_name(function, cycles) -> draw_dep_graph;
draw_graph_request_name(functionblock, _) -> draw_dep_graph.

finished(Job, Result) ->
    Id = Job#referl_web2_job.id,
    Owner = Job#referl_web2_job.owner,
    Update = fun(J) ->
                 J#referl_web2_job{running = false,
                                   finished_date = erlang:now(),
                                   result = Result}
             end,
    update(Id, Update),
    ?WEB2_WEBSOCKET:notify(Owner, job_finished, Id).

failed(Job, {error, Error}) ->
    failed(Job, Error);

failed(Job, Error) ->
    finished(Job, {error, Error}).

transform_smart_graph(Text) ->
    Lines = string:tokens([C || C <- Text, C =/= $\r], "\n"),
    {_, _, W, H} = list_to_tuple(string:tokens(hd(Lines), " ")),
    Width  = list_to_num(W),
    Height = list_to_num(H),
    Nodes = [transform_smart_node(E, Width, Height) || E <- Lines,
					(lists:nth(length(E), E) =/= $<) and (hd(E) == $n)],
    Edges = [transform_smart_edge(E) || E <- Lines,
                                        hd(E) == $e],
    {struct, [{nodes, Nodes},
              {edges, Edges}]}.

transform_smart_node(E, Width, Height) ->
    L = string:tokens(E, " "),
    Id = lists:nth(2, L),
    {struct, [{id, Id},
              {label, lists:nth(7, L)},
              {x, 3 * list_to_num(lists:nth(3, L)) / Width},
              {y, (Height - list_to_num(lists:nth(4, L))) / Height},
              {type, transform_node_type(Id)}]}.

transform_smart_edge(E) ->
    L = string:tokens(E, " "),
    Source = lists:nth(2, L),
    Target = lists:nth(3, L),
    {struct, [{id, Source ++ Target ++ lists:last(L)},
              {source, Source},
              {target, Target},
              {color, lists:last(L)}]}.

transform_node_type([$m|_])      -> "module";
transform_node_type([$f|[$b|_]]) -> "functionblock";
transform_node_type([$f|_])      -> "function";
transform_node_type("root")      -> "root";
transform_node_type(_)           -> "unknown".

list_to_num(L) ->
    case string:to_float(L) of
        {error, no_float} -> list_to_integer(L);
        {F, _} -> F
    end.

transform_duplicated_diffs(Diffs) ->
    Merged = case Diffs of
                 []     -> [];
                 {A, B} -> A ++ B
             end,
    [[{array, tuple_to_list(?NITRO_SERVICES:get_node_position(N))} || N <- Nodes] ||
     Nodes <- [[N1 || {N1, _} <- Merged],
               [N2 || {_, N2} <- Merged]]].

transform_dupcode_params([{TName, _}|_] = List) when is_atom(TName) ->
    case proplists:get_value(key, List) of
        undefined ->
            Array = lists:map(fun({_, L}) ->
                                  transform_dupcode_params(L) end, List),
            {array, Array};
        _ ->
            Struct = lists:map(fun({N, L}) ->
                                  {N, transform_dupcode_params(L)} end, List),
            {struct, Struct}
    end;
transform_dupcode_params([I|_]=List) when not is_integer(I) ->
    Struct = lists:map(fun transform_dupcode_params/1, List),
    {array, Struct};
transform_dupcode_params(Atom) when is_atom(Atom) andalso not is_boolean(Atom) ->
    atom_to_list(Atom);
transform_dupcode_params(Else) ->
    Else.

make_ui_request(Request) ->
    make_ui_request(Request, undefined).

make_ui_request(Request, Callback) ->
    ReqId = ?UI:getid(),
    case ?UI:request(ReqId, Request) of
        deny  -> {error, {deny, "The request was denied by the job server."++
                                "\nPlease, try again later."}};
        ok    -> ui_loop(ReqId, Callback)
    end.

ui_loop(ReqId, Callback) ->
    receive
        {ReqId, reply, Reply} -> Reply;
        {ReqId, Name,  Arg}   -> case Callback of
                                     undefined -> ok;
                                     _         -> Callback(Name, Arg)
                                 end,
                                 ui_loop(ReqId, Callback)
    end.

dot_to_svg(Source, Target) ->
    case file:read_file_info(Source) of
        {ok, _}    -> ok;
        {error, _} -> throw(?RefError(file_notexists, [Source]))
    end,
    Res = os:cmd("dot -Tsvg " ++ Source ++ " -o" ++ Target),
    case Res of
        [] -> ok;
        _  -> throw(?LocalError(os_dot_error, ["SVG format"]))
    end.

dot_to_txt(Source, Target) ->
    case file:read_file_info(Source) of
        {ok, _}    -> ok;
        {error, _} -> throw(?RefError(file_notexists, [Source]))
    end,
    Res = os:cmd("dot -Tplain-ext -Gnodesep=0.001 " ++ Source ++" -o " ++ Target),
    case Res of
        [] ->
            file:delete(Source),
            case file:read_file(Target) of
                {ok, Binary} -> file:delete(Target),
                                binary_to_list(Binary);
                {error, _}  -> throw(?LocalError(file_read_error, [Target]))
            end;
        _ -> throw(?LocalError(os_dot_error, ["the inner representation"]))
    end.
