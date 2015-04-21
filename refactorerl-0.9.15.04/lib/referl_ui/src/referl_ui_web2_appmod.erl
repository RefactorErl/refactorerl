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


-module(referl_ui_web2_appmod).
-vsn("$Rev$").

%% Export for yaws appmod
-export([out/1]).

%% For index.yaws to call
-export([serve_index/1]).

%% Record for user session
-record(session, {id, username}).

-define(SESSION_KEY, "referl_web2_session").
-define(DEFAULT_ITEMS_PER_PAGE, 15).

-include("yaws_api.hrl").
-include("ui.hrl").
-include_lib("referl_core/include/core_export.hrl").

%% @doc Serve the index.html but force no caching and ensure session cookies
serve_index(Arg) ->
    wrap_session_handling(Arg, fun serve_index_with_session/1).

serve_index_with_session(_Arg) ->
    NoCacheHeaders = [
                      {header, ["Expires: ", "Tue, 11 Jan 2000 01:00:00 GMT"]},
                      {header, ["Cache-Control: ", "no-store, no-cache, must-revalidate"]},
                      {header, ["Pragma: ", "no-cache"]}
                     ],
    Bindings = [{"loggedInUser", get_username()},
                {"sessionId", get_session_id()},
                {"restricted", atom_to_list(?WEB2_SERVICES:is_restricted())}],
    Html = {ehtml, [{ssi, "index.html", "%%", Bindings}]},
    [NoCacheHeaders, Html].

%% @doc Entry point for the appmod
out(Arg) ->
    wrap_session_handling(Arg, fun route/1).

%% Parses Arg then calls out/2 for pattern matching
route(Arg) ->
    Path = string:tokens(Arg#arg.pathinfo, "/"),
    out(Arg, Path).

%% Serve WebSocket requests
out(_Arg, ["ws"]) ->
    %Opts = [{origin, "http://" ++ (Arg#arg.headers)#headers.host}],
    {websocket, ?WEB2_WEBSOCKET, []};

out(Arg, ["login"]) ->
    ParsedQuery = yaws_api:parse_query(Arg),
    User = proplists:get_value("username", ParsedQuery, undefined),
    Password = proplists:get_value("password", ParsedQuery, ""),
    case User of
        undefined ->
            return_json({struct, [{"error", "'username' parameter is not set"}]});
        _ ->
            case ?WEB2_SERVICES:is_valid_web2_pass(User, Password) of
                false ->
                    return_json({struct, [{"error", "Wrong password!"}]});
                not_set ->
                    return_json({struct, [{"error", "Password hasn't been set."}]});
                _ ->
                    Session = get_session(),
                    set_session(Session#session{username = User}),
                    ?WEB2_SESSIONS:set_user_in_session(Session#session.id, User),
                    out(Arg, ["getUser"])
            end
    end;

out(_Arg, ["getDupcodeAlgorithms"]) ->
    Alg = ?WEB2_SERVICES:dupcode_algorithms(),
    return_json(?WEB2_SERVICES:transform_dupcode_params(Alg));

out(Arg, ["getDupcodeJobIdByName"]) ->
    ParsedQuery = yaws_api:parse_query(Arg),
    Name = proplists:get_value("name", ParsedQuery, ""),
    case ?WEB2_SERVICES:get_dupcode_job_by_name(list_to_atom(Name)) of
        [Id] -> return_json({struct, [{id, Id}]});
        _ -> return_json(false)
    end;

out(Arg, ["getDataFromSession"]) ->
    ParsedQuery = yaws_api:parse_query(Arg),
    Key = proplists:get_value("key", ParsedQuery, undefined),
    Session = get_session(),
    Data = ?WEB2_SESSIONS:get_data_from_session(Session#session.id, Key),
    return_json({struct, [{"data", Data}]});

out(Arg, ["storeDataInSession"]) ->
    {struct, PropList} = decode_post_json(Arg),
    Key = proplists:get_value("key", PropList, undefined),
    Data = proplists:get_value("data", PropList, undefined),
    Session = get_session(),
    ?WEB2_SESSIONS:store_data_in_session(Session#session.id, {Key, Data}),
    return_json({struct, []});

out(_Arg, ["logout"]) ->
    Session = get_session(),
    set_session(Session#session{username = ""}),
    ?WEB2_SESSIONS:remove_user_from_session(Session#session.id),
    return_json(false);

out(_Arg, ["getUser"]) ->
    case get_username() of
        "" ->
            return_json(false);
        Username ->
            return_json({struct, [{"name", Username}]})
    end;

out(_Arg, ["session", "id"]) ->
    Session = get_session(),
    {content, "text/plain; charset=utf-8", Session#session.id};

out(_Arg, ["session", "keep"]) ->
    return_json({struct, [{ok, true}]});

out(_Arg, ["getDirectories"]) ->
    Dirs = ?WEB2_SERVICES:get_directories(),
    return_json({array, Dirs});

out(_Arg, ["errors"]) ->
    Errors = ?WEB2_SERVICES:get_errors(),
    Transform = fun({File, Start, Length, Msg}) ->
                    Message = lists:flatten(Msg),
                    proplist_to_json([{file, File},
                                      {start, Start},
                                      {length, Length},
                                      {message, Message}])
                end,
    return_json(map_to_json(Transform, Errors));

out(_Arg, ["errors", "count"]) ->
    Errors = ?WEB2_SERVICES:get_errors(),
    return_json({struct, [{count, length(Errors)}]});

out(Arg, ["getFile"]) ->
    ParsedQuery = yaws_api:parse_query(Arg),
    Path = proplists:get_value("file", ParsedQuery, undefined),
    Result = ?WEB2_SERVICES:get_file(Path),
    PropList = case Result of
        {content, File} -> [{content, File}];
        {error, Error}  -> [{content,
                             "% Can not load file: " ++
                             Path ++ "\n" ++
                             "% Error: " ++ error_to_list(Error)},
                            {error, error_to_list(Error)}]
    end,
    return_json(proplist_to_json(PropList));

out(Arg, ["queries", "execute"]) ->
    {struct, Parameters} = decode_post_json(Arg),
    Owner = get_username(),
    Id = ?WEB2_SERVICES:execute_query(Parameters, Owner),
    return_json({struct, [{"id", Id}]});

out(Arg, ["search", "execute"]) ->
    {struct, Parameters} = decode_post_json(Arg),
    Owner = get_username(),
    Id = ?WEB2_SERVICES:execute_search(Parameters, Owner),
    return_json({struct, [{"id", Id}]});

out(Arg, ["graphs", "execute"]) ->
    {struct, Parameters} = decode_post_json(Arg),
    Owner = get_username(),
    Id = ?WEB2_SERVICES:execute_graph(Parameters, Owner),
    return_json({struct, [{"id", Id}]});

out(Arg, ["duplicates", "execute"]) ->
    {struct, Parameters} = decode_post_json(Arg),

    ParamsWithAlg = case Parameters of
        [{"forSelection",true}|_] ->
            Alg = list_to_atom(proplists:get_value("algorithm", Parameters, "matrix")),
            Algs = ?WEB2_SERVICES:dupcode_algorithms(),
            {struct, AlgParams} = ?WEB2_SERVICES:transform_dupcode_params(
                                    proplists:get_value(Alg, Algs)),
            Parameters ++ AlgParams;
        _ ->
            Parameters
    end,

    Owner = get_username(),
    Id = ?WEB2_SERVICES:execute_duplicates(ParamsWithAlg, Owner),
    return_json({struct, [{"id", Id}]});

out(Arg, ["duplicates", "diff", IdString, GroupIndexString]) ->
    ParsedQuery = yaws_api:parse_query(Arg),
    Id = list_to_integer(IdString),
    GroupIndex = list_to_integer(GroupIndexString) + 1,
    ItemsStr = proplists:get_value("items", ParsedQuery, ""),
    Indexes = [list_to_integer(S) + 1 ||
               S <- string:tokens(ItemsStr, ",")],
    Ranges = ?WEB2_SERVICES:get_duplicated_diff(Id, GroupIndex, Indexes),
    return_json({struct, [{"ranges", Ranges}]});

out(_Arg, ["jobs", "queue"]) ->
    User = get_username(),
    Queue = ?WEB2_SERVICES:get_queue(User),
    QueueJson = map_to_json(fun job_to_json/1, Queue),
    return_json({struct, [{items, QueueJson}]});

out(Arg, ["jobs", "results", IdString]) ->
    Id = list_to_integer(IdString),
    {Type, Result} = ?WEB2_SERVICES:get_type_and_result(Id),
    State = ?WEB2_SERVICES:get_state(Id),
    ParsedQuery = yaws_api:parse_query(Arg),
    case proplists:get_value("format", ParsedQuery) of
        "text" when Type =:= semantic_query ->
            {content,"text/plain; charset=utf-8", sq_to_text(Result)};
        "text" ->
            {content,"text/plain; charset=utf-8", io_lib_pretty:print(Result)};
        _ ->
            Prop = case Result of
                undefined ->
                    case Type of
                        undefined ->
                            Msg = "The requested job does not exist. " ++
                                  "Maybe it was deleted?",
                            {result, transform_error(Msg)};
                        _         ->
                            {notFinished, true}
                    end;
                _ ->
                    case Type of
                        semantic_query ->
                            case Result of
                                {result, [{result, Result0}]} ->
                                    ReqPage = proplists:get_value("page", ParsedQuery),
                                    ItemsPerPage = proplists:get_value("items_per_page", ParsedQuery),
                                    case proplists:get_value(?SQ_WEB_DISP_KEY, Result0) of
                                        undefined ->
                                            Msg = "The requested job is corrupted. ",
                                            {result, transform_error(Msg)};
                                        TheResult ->
                                            serve_query_result(TheResult, ReqPage, ItemsPerPage)
                                    end;
                                undefined ->
                                    {notFinished, true};
                                Error ->
                                    {result, transform_error(Error)}
                            end;
                        graph ->
                            {result, transform_graph_result(Result)};
                        search ->
                          case Result of
                            {result, [{result, SearchResults}]} ->
                              ReqPage      = proplists:get_value("page", ParsedQuery),
                              ItemsPerPage = proplists:get_value("items_per_page", ParsedQuery),
                              serve_search_result(SearchResults, ReqPage, ItemsPerPage);
                            Error ->
                              { result, transform_error(Error) }
                          end;
                        duplicates ->
                            {result, transform_duplicates_result(Result)}
                    end
            end,
            return_json({struct, [{id, Id}, Prop, State]})
    end;

out(_Arg, ["jobs", "parameters", IdString]) ->
    Id = list_to_integer(IdString),
    case ?WEB2_SERVICES:get_parameters(Id) of
        undefined  -> {status, 404};
        Parameters -> return_json({struct, Parameters})
    end;

out(Arg, ["jobs", "markSeen"]) ->
    {struct, PropList} = decode_post_json(Arg),
    Id = proplists:get_value("id", PropList),
    ?WEB2_SERVICES:mark_seen(Id),
    return_json(null);

out(Arg, ["jobs", "remove"]) ->
    {struct, PropList} = decode_post_json(Arg),
    User = get_username(),
    Id = proplists:get_value("id", PropList),
    ?WEB2_SERVICES:remove_job(Id, User),
    return_json(null);

out(Arg, ["autocomplete", "dupcodeNames"]) ->
    {struct, PropList} = decode_post_json(Arg),
    Term = proplists:get_value("term", PropList),
    Context = case proplists:get_value("context", PropList) of
                  null  -> 1;
                  Value -> Value
              end,
    All = lists:sort(?WEB2_SERVICES:get_dupcode_names()),
    Reordered = case lists:filter(fun(Module) -> iequal(Module, Term) end, All) of
                    [Exact | _] -> Without = lists:delete(Exact, All),
                                   lists:append([Exact], Without);
                    _           -> All
                end,
    Filtered = [{struct, [{id, Module}, {text, Module}]} ||
                Module <- Reordered, istr(Module, Term) > 0],
    Trimmed = lists:sublist(Filtered, Context, 15),
    Json = {struct, [{results, {array, Trimmed}},
                     {context, Context + length(Trimmed)},
                     {more, Context + length(Trimmed) < length(Filtered) + 1}]},
    return_json(Json);

out(Arg, ["autocomplete", "module"]) ->
    {struct, PropList} = decode_post_json(Arg),
    Term = proplists:get_value("term", PropList),
    Context = case proplists:get_value("context", PropList) of
                  null  -> 1;
                  Value -> Value
              end,
    All = ?WEB2_CACHE:get("autocomplete/module/all",
                          fun() -> lists:sort(?NITRO_SERVICES:get_mods()) end),
    Reordered = case lists:filter(fun(Module) -> iequal(Module, Term) end, All) of
                    [Exact | _] -> Without = lists:delete(Exact, All),
                                   lists:append([Exact], Without);
                    _           -> All
                end,
    Filtered = [{struct, [{id, Module}, {text, Module}]} ||
                Module <- Reordered, istr(Module, Term) > 0],
    Trimmed = lists:sublist(Filtered, Context, 15),
    Json = {struct, [{results, {array, Trimmed}},
                     {context, Context + length(Trimmed)},
                     {more, Context + length(Trimmed) < length(Filtered) + 1}]},
    return_json(Json);

out(Arg, ["autocomplete", "function"]) ->
    {struct, PropList} = decode_post_json(Arg),
    Term = proplists:get_value("term", PropList),
    Context = case proplists:get_value("context", PropList) of
                  null  -> 1;
                  Value -> Value
              end,
    All = ?WEB2_CACHE:get("autocomplete/function/all",
                          fun() -> lists:sort(?NITRO_SERVICES:get_funs()) end),
    Reordered = case lists:filter(fun(Module) -> iequal(Module, Term) end, All) of
                    [Exact | _] -> Without = lists:delete(Exact, All),
                                   lists:append([Exact], Without);
                    _           -> All
                end,
    Filtered = [{struct, [{id, Module}, {text, Module}]} ||
                Module <- Reordered, istr(Module, Term) > 0],
    Trimmed = lists:sublist(Filtered, Context, 15),
    Json = {struct, [{results, {array, Trimmed}},
                     {context, Context + length(Trimmed)},
                     {more, Context + length(Trimmed) < length(Filtered) + 1}]},
    return_json(Json);

out(Arg, ["autocomplete", "fullModule"]) ->
    {struct, PropList} = decode_post_json(Arg),
    Term = proplists:get_value("term", PropList),
    Context = case proplists:get_value("context", PropList) of
                  null  -> 1;
                  Value -> Value
              end,
    All = ?WEB2_CACHE:get("autocomplete/fullModule/all",
                          fun() -> ?WEB2_SERVICES:get_mods_with_path() end),
    Reordered = case lists:filter(fun({Name, _Path}) -> iequal(Name, Term) end, All) of
                    [Exact | _] -> Without = lists:delete(Exact, All),
                                   lists:append([Exact], Without);
                    _           -> lists:append([{Term, Term}], All)
                end,
    Filtered = [{struct, [{id, Id}, {text, Text}]} ||
                {Text, Id} <- Reordered, istr(Text, Term) > 0],
    Trimmed = lists:sublist(Filtered, Context, 15),
    Json = {struct, [{results, {array, Trimmed}},
                     {context, Context + length(Trimmed)},
                     {more, Context + length(Trimmed) < length(Filtered) + 1}]},
    return_json(Json);

out(Arg, ["autocomplete", "query"]) ->
    {struct, PropList} = decode_post_json(Arg),
    Term = proplists:get_value("term", PropList),
    List1 = try
        Filtered = ?NITRO_HELPER:filtered_query(Term),
        List = refusr_ac:run(Filtered, detailed),
        [{struct, [{id, lists:flatten(Completion)},
                   {text, lists:flatten(Prefix ++ Completion)},
                   {prefix, lists:flatten(Prefix)},
                   {details, transforms_ac_details(Details)}]}
         || {Prefix, Completion, Details} <- List,
            is_list(Prefix), is_list(Completion)]
    catch
        _:_ -> []
    end,
    AllSkeleton = ?NITRO_SKELETONS:list_skeletons(),
    Skels = [{Name,
              Arity,
              Source,
              Desc,
              ?NITRO_SKELETONS:skeleton_call_format(Name)}
             || {Name, Source, _, Arity, Desc} <- AllSkeleton,
                lists:prefix(string:to_lower(Term), string:to_lower(Name))],
    TermLen = length(Term),
    List2 = [{struct, [{id, string:sub_string(Call, TermLen + 1)},
                       {text, Name},
                       {prefix, string:sub_string(Call, 1, TermLen)},
                       {details, {struct, [{type, "skeleton/" ++
                                                  integer_to_list(Arity)},
                                           {source, Source},
                                           {desc, Desc}]}}]}
             || {Name, Arity, Source, Desc, Call} <- Skels],
    Sorter = fun({struct, A}, {struct, B}) ->
                 proplists:get_value(id, A) < proplists:get_value(id, B)
             end,
    Sorted = lists:sort(Sorter, List1 ++ List2),
    Json = {struct, [{results, {array, Sorted}}]},
    return_json(Json);

out(Arg, ["download"| Name0]) ->
    PropList = yaws_api:parse_query(Arg),
    File = case filename:join(Name0) of
        "download_from_server" ->
            proplists:get_value("path", PropList, "no_path");
        Name1 ->
            Dir = ?MISC:data_dir(),
            filename:join([Dir, Name1])
    end,
    Data = case file:read_file(File) of
        {ok, Binary} -> Binary;
        {error, _}   -> <<>>
    end,
    ContentType = case filename:extension(File) of
        ".svg" -> "image/svg+xml";
        ".dot" -> "application/octet-stream";
        _      -> "application/octet-stream"
    end,
    {content, ContentType, binary_to_list(Data)};

out(Arg, ["downloadDuplicatesResult"]) ->
    PropList = yaws_api:parse_query(Arg),
    File = proplists:get_value("path", PropList, "no_path"),
    Name = filename:basename(File),
    Data = case file:read_file(File) of
        {ok, Binary} -> Binary;
        {error, _}   -> <<>>
    end,
    [   {header, {"Content-disposition", "attachment; filename=\""++Name++"\""}},
        {header, {"Content-Transfer-Encoding", "Binary"}},
        {content, "application/octet-stream", Data}];

out(Arg, ["getPredefineds"]) ->
    {struct, PropList} = decode_post_json(Arg),
    File = proplists:get_value("file", PropList),
    Pos = proplists:get_value("index", PropList),
    Response = case ?WEB2_SERVICES:get_predefineds(File, Pos) of
        {Node, Type, TupleList} ->
            List = [{struct, [{label, L}, {'query', Q}]} || {L, Q} <- TupleList],
            Ref = ?WEB2_SERVICES:encode_starting_point({Node,File,Pos}),
            Display = ?WEB2_SERVICES:display_node(Node),
            [{node, {struct, [{ref, Ref}, {display, Display}]}},
             {type, atom_to_list(Type)},
             {queries, {array, List}}];
        {error, {E, _}} ->
            [{error, atom_to_list(E)}];
        _ ->
            [{queries, {array, []}}]
    end,
    return_json({struct, Response});

out(Arg, ["saveAsDynfun"]) ->
    {struct, PropList} = decode_post_json(Arg),
    Ref = proplists:get_value("ref", PropList),
    Display = proplists:get_value("display", PropList),
    User = get_username(),
    case ?WEB2_SERVICES:save_as_dynfun(User, Ref, Display) of
        ok -> return_json({struct, [{ok, true}]});
        _ -> return_json({struct, [{ok, false}]})
    end;

out(Arg, ["database", "getFiles"]) ->
    {struct, PropList} = decode_post_json(Arg),
    Path = proplists:get_value("path", PropList, undefined),
    List = ?WEB2_SERVICES:get_db_files(Path),
    Files = map_to_json(fun proplist_to_json/1, List),
    return_json({struct, [{files, Files}]});

out(Arg, ["filesystem", "getFiles"]) ->
    {struct, PropList} = decode_post_json(Arg),
    Path = proplists:get_value("path", PropList, undefined),
    List = ?WEB2_SERVICES:get_fs_files(Path),
    Files = map_to_json(fun proplist_to_json/1, List),
    return_json({struct, [{files, Files}]});

out(Arg, ["getFuns"]) ->
    {struct, PropList} = decode_post_json(Arg),
    Path = proplists:get_value("path", PropList, undefined),
    List = ?WEB2_SERVICES:get_funs(Path),
    Funs = map_to_json(fun proplist_to_json/1, List),
    return_json({struct, [{files, Funs}]});

out(Arg, ["regenerateFile"]) ->
    {struct, PropList} = decode_post_json(Arg),
    File = proplists:get_value("file", PropList, undefined),
    ?WEB2_SERVICES:regenerate(File),
    return_json({struct, []});

out(Arg, ["database", "search"]) ->
    {struct, PropList} = decode_post_json(Arg),
    Filter = proplists:get_value("filter", PropList, undefined),
    List = ?WEB2_SERVICES:search_db_files(Filter),
    Files = map_to_json(fun proplist_to_json/1, List),
    return_json({struct, [{files, Files}]});

out(Arg, ["filesystem", "search"]) ->
    {struct, PropList} = decode_post_json(Arg),
    Filter = proplists:get_value("filter", PropList, undefined),
    List = ?WEB2_SERVICES:search_fs_files(Filter),
    Files = map_to_json(fun proplist_to_json/1, List),
    return_json({struct, [{files, Files}]});

out(Arg, ["database", Operation]) ->
    User = get_username(),
    Res = case (not ?WEB2_SERVICES:is_restricted()) orelse User=="admin" of
        true ->
            {struct, PropList} = decode_post_json(Arg),
            Path = proplists:get_value("path", PropList),
            IsFolder = proplists:get_value("isFolder", PropList),
            database_operation(Operation, Path, User, IsFolder),
            {ok, true};
        _ ->
            {ok, false}
    end,
    return_json({struct, [Res]});

out(Arg, ["skeletons", "save"]) ->
    {struct, PropList} = decode_post_json(Arg),
    User = get_username(),
    Res = case ?WEB2_SERVICES:save_skeleton(PropList, User) of
        ok -> {struct, [{ok, true}]};
        E  -> transform_error(E)
    end,
    return_json(Res);

out(Arg, ["history"]) ->
    {struct, PropList} = decode_post_json(Arg),
    User = case proplists:get_value("notJustMine", PropList, false) of
        true  -> undefined;
        false -> get_username()
    end,
    Type = case proplists:get_value("type", PropList, "") of
        ""  -> undefined;
        T   -> T
    end,
    Items = ?WEB2_SERVICES:get_history(Type, User),
    ItemsJson = map_to_json(fun job_to_json/1, Items),
    return_json({struct, [{items, ItemsJson}]});

%% Unknown request
out(_Arg, X) ->
    io:format("Unknown request: ~p~n", X),
    return_json("Unknown request.").


database_operation("add", Path, User, IsFolder) ->
    ?WEB2_SERVICES:add_to_db(Path, User, IsFolder);

database_operation("update", Path, User, IsFolder) ->
    ?WEB2_SERVICES:update_in_db(Path, User, IsFolder);

database_operation("drop", Path, User, IsFolder) ->
    ?WEB2_SERVICES:drop_from_db(Path, User, IsFolder);

database_operation("addInclude", Path, _User, _IsFolder) ->
    ?WEB2_SERVICES:add_include(Path);

database_operation("addAppBase", Path, _User, _IsFolder) ->
    ?WEB2_SERVICES:add_appbase(Path).

%%%% JSON related

decode_post_json(#arg{clidata = undefined}) ->
    {struct, []};

decode_post_json(#arg{clidata = Binary}) ->
    {ok, Json} = decode_binary_json(Binary),
    Json.

decode_binary_json(Binary) ->
    JsonString = unicode:characters_to_list(Binary, utf8),
    json2:decode_string(JsonString).

return_json(ToEncode) ->
    Json = json2:encode(ToEncode),
    {content, "application/json; charset=utf-8", Json}.

%%%% Session related

wrap_session_handling(Arg, OutFunction) ->
    EnsureSession = ensure_session(Arg),
    Response = OutFunction(Arg),
    if
        is_tuple(Response) ->
            [EnsureSession, Response];
        is_list(Response) ->
            [EnsureSession] ++ Response;
        true ->
            throw(unknown_response)
    end.

%% @doc Ensure that session cookie does exist and refer to a valid session.
%% The response has to be included in the response of out(Arg) function!
ensure_session(Arg) ->
    Headers = Arg#arg.headers,
    Cookies = Headers#headers.cookie,
    case yaws_api:find_cookie_val(?SESSION_KEY, Cookies) of
        [] ->
            new_session();
        SessionId ->
            case yaws_api:cookieval_to_opaque(SessionId) of
                {ok, Session} ->
                    put(?SESSION_KEY, Session),
                    ok;
                {error, no_session} ->
                    new_session()
            end
    end.

%% @doc It has to be called after ensure_session.
get_session() ->
    get(?SESSION_KEY).

%% @doc It should be called after ensure_session.
get_session_id() ->
    case get(?SESSION_KEY) of
        undefined ->
            undefined;
        Session ->
            Session#session.id
    end.

%% @doc It has to be called after ensure_session.
set_session(NewSession) ->
    Session = get(?SESSION_KEY),
    if
        Session#session.id == NewSession#session.id ->
            yaws_api:replace_cookie_session(Session#session.id, NewSession),
            put(?SESSION_KEY, NewSession);
        true ->
            throw(can_not_change_session_id)
    end.

%% @doc Creates a new session.
%% The response has to be included in the response of out(Arg) function!
new_session() ->
    EmptySession = #session{username = ""},
    SessionId = yaws_api:new_cookie_session(EmptySession, 30 * 60, ?WEB2_SESSIONS),
    Session = EmptySession#session{id = SessionId},
    yaws_api:replace_cookie_session(SessionId, Session),
    put(?SESSION_KEY, Session),
    yaws_api:setcookie(?SESSION_KEY, SessionId, "/").

%% @doc It has to be called after ensure_session.
get_username() ->
    Session = get(?SESSION_KEY),
    Session#session.username.

%%%% Private functions

transform_error(Error) ->
    {struct, [{error, error_to_list(Error)}]}.

error_to_list({error, {A, [C|_]=Msg}}) when is_atom(A) andalso is_integer(C) ->
    lists:flatten(Msg);

error_to_list({error, E}) ->
    error_to_list(E);

error_to_list({abort, M}) ->
    ?Error:error_text(M);

error_to_list([C|_]=Msg) when is_integer(C) ->
    lists:flatten(Msg);

error_to_list(enoent) ->
    "The file does not exist.";

error_to_list(eacces) ->
    "Missing permission for reading the file, or for searching " ++
    "one of the parent directories.";

error_to_list(enomem) ->
    "There is not enough memory for the contents of the file.";

error_to_list(Error) ->
    lists:flatten(io_lib:format("~p", [Error])).

transforms_ac_details({_Name, Names, Type, " "}) ->
    {struct, [{names, [atom_to_list(Name) || Name <- Names]},
              {type, atom_to_list(Type)}]};

transforms_ac_details({_Name, Names, Type, Desc}) ->
    {struct, [{names, [atom_to_list(Name) || Name <- Names]},
              {type, atom_to_list(Type)},
              {desc, Desc}]};

transforms_ac_details(_) ->
    null.

sq_to_text({result, [{result, TheResult}]})->
    case proplists:get_value(?SQ_TEXT_DISP_KEY, TheResult) of
        undefined -> "The requested text cannot be found.";
        Res -> Res
    end;
sq_to_text(Result)->
    ?MISC:any_to_string(Result).

serve_query_result(FullResult, Page, _ItemsPerPage)
    when Page =:= undefined ->
    {result, transform_query_result(FullResult)};
serve_query_result(FullResult, Page, ItemsPerPage)
    when ItemsPerPage =:= undefined ->
    serve_query_result(FullResult, Page, ?DEFAULT_ITEMS_PER_PAGE);
serve_query_result(FullResult, Page=[_|_], ItemsPerPage) ->
    case can_be_converted_to_integer(Page) of
        true -> serve_query_result(FullResult, list_to_integer(Page), ItemsPerPage);
        false -> {error, transform_error("Bad request was given.")}
    end;
serve_query_result(FullResult, Page, ItemsPerPage=[_|_]) ->
    case can_be_converted_to_integer(ItemsPerPage) of
        true -> serve_query_result(FullResult, Page, list_to_integer(ItemsPerPage));
        false -> {error, transform_error("Bad request was given.")}
    end;
serve_query_result(FullResult, Page, ItemsPerPage)
    when is_integer(Page), is_integer(ItemsPerPage), (Page > 0), (ItemsPerPage > 0) ->
    {PartialResult, NumOfPages} = partition_result(FullResult, Page, ItemsPerPage),
    IsLast = Page >= NumOfPages,
    IsFirst = (Page =:= 1),
    {result, {struct, [{"isFirstPage", IsFirst},
                       {"isLastPage", IsLast},
                       {"numOfPages", NumOfPages},
                       {"page", Page},
                       {"data", transform_query_result(PartialResult)}]}};
% @todo: better error handling!
serve_query_result(_,_,_)->
    {error, transform_error("Bad request was given.")}.

% Serve search result
serve_search_result(FullResult, Page, _ItemsPerPage)
  when Page =:= undefined ->
  {result, transform_search_result(FullResult)};

serve_search_result(FullResult, Page, ItemsPerPage)
  when ItemsPerPage =:= undefined ->
  serve_search_result(FullResult, Page, ?DEFAULT_ITEMS_PER_PAGE);

serve_search_result(FullResult, Page=[_|_], ItemsPerPage) ->
  case can_be_converted_to_integer(Page) of
    true -> serve_search_result(FullResult, list_to_integer(Page), ItemsPerPage);
    false -> { error, transform_error("Bad request was given.")}
  end;

serve_search_result(FullResult, Page, ItemsPerPage=[_|_]) ->
  case can_be_converted_to_integer(ItemsPerPage) of
    true -> serve_search_result(FullResult, Page, list_to_integer(ItemsPerPage));
    false -> { error, transform_error("Bad request was given.")}
  end;

serve_search_result(FullResult, Page, ItemsPerPage)
  when is_integer(Page), is_integer(ItemsPerPage), (Page > 0), (ItemsPerPage > 0) ->
  {PartialResult, NumOfPages} = partition_search_result(FullResult, Page, ItemsPerPage),
  IsLast  = Page >= NumOfPages,
  IsFirst = (Page =:= 1),
  {result, {struct, [{"isFirstPage", IsFirst},
                     {"isLastPage", IsLast},
                     {"numOfPages", NumOfPages},
                     {"page", Page},
                     {"data", transform_search_result(PartialResult)}]}};

serve_search_result(_,_,_) ->
  { error, transform_error("Bad request was given.") }.

% End of search result

% Start of partition_search_result

partition_search_result(FullResult, Page, ItemsPerPage) ->
  % Respond with {PartialResult, NumOfPages}
  {lists:sublist(FullResult, ((Page - 1) * ItemsPerPage) + 1, Page * ItemsPerPage),
                 ceil(length(FullResult) / ItemsPerPage)}.

ceil(X) ->
  T = trunc(X),
  case (X - T) of
    Neg when Neg < 0 -> T;
    Pos when Pos > 0 -> T + 1;
    _ -> T
  end.

% End of partition_search_result

partition_result(FullResult, Page, ItemsPerPage) ->
    partition_by_subs(FullResult, Page, ItemsPerPage, [], 0, 0).
    %lists:sublist(FullResult, (Page-1)*ItemsPerPage+1, ItemsPerPage);

partition_by_subs([], _, _, Result, 0, Pages) ->
    {lists:reverse(Result), Pages};
partition_by_subs([], _, _, Result, _, Pages) ->
    {lists:reverse(Result), Pages+1};
partition_by_subs([Group | Groups], Page, MinLength, Result, Items, Pages) ->
    {NewPages, NewItems} = case Items + group_length(Group) of
        NI when NI >= MinLength -> {Pages + 1, 0};
        NI -> {Pages, NI}
    end,
    NewResult = case Pages+1 of
        Page -> [Group | Result];
        _ -> Result
    end,
    partition_by_subs(Groups, Page, MinLength, NewResult, NewItems, NewPages).

group_length({group_by, _By, eq, _Name, _Value}) ->
    1;
group_length({group_by, _By, list, Items}) ->
    length(Items);
group_length({group_by, _By, propertylist, _Name, Items}) ->
    length(Items);
group_length({list, Items}) ->
    length(Items);
group_length({chain, _, _}) ->
    1;
group_length({eq, _Name, _Value}) ->
    1.

can_be_converted_to_integer(String)->
lists:all(fun(Char)->
        lists:member(Char, [$1,$2,$3,$4,$5,$6,$7,$8,$9,$0])
    end,String).

transform_query_result([]) ->
    {struct, [{"empty", true}]};

transform_query_result([{list, []}]) ->
    {struct, [{"empty", true}]};

transform_query_result([{group_by, _By, eq, Name, _Value} | _Rest]=Input) ->
    transform_query_table(Input, Name);

transform_query_result([{group_by, _By, propertylist, Name, _Val} | _]=Input) ->
    transform_query_table(Input, Name);

transform_query_result([{group_by, _By, list, _Items} | _Rest]=Input) ->
    transform_query_grouped(Input);

transform_query_result([{list, _Items}]=Input) ->
    transform_query_list(Input);

transform_query_result([{chain, _, _} | _Rest]=Input) ->
    transform_query_chains(Input);

transform_query_result([{eq, Name, Value}]) ->
    transform_query_eq(Name, Value).

transform_query_table(Input, Name) ->
    {struct, [{"type", "table"},
              {"property", display(Name)},
              {"rows", map_to_json(fun transform_query_table_row/1, Input)}]}.

transform_query_table_row({group_by, Item, eq, _Name, Value}) ->
    {struct, [{"values", {array, [scalar(Value)]}} |
                transform_query_item_partial(Item)]};

transform_query_table_row({group_by, Item, propertylist, _Name, Values}) ->
    {struct, [{"values", {array, lists:map(fun scalar/1, Values)}} |
                transform_query_item_partial(Item)]}.

transform_query_grouped(Input) ->
    {struct, [{"type", "grouped"},
              {"groups", map_to_json(fun transform_group/1, Input)}]}.

transform_group({group_by, Header, list, Items}) ->
    {struct, [{"header", transform_query_item(Header)},
              {"rows", transform_query_items(Items)}]}.

transform_query_list([{list, Items}]) ->
    {struct, [{"type", "list"},
              {"rows", transform_query_items(Items)}]}.

transform_query_chains(Input) ->
    {struct, [{"type", "chains"},
              {"chains", map_to_json(fun transform_query_chain/1, Input)}]}.

transform_query_chain({chain, Items, WS}) ->
    {struct, [{"end", display(WS)},
              {"items", transform_query_items(Items)}]}.

transform_query_eq(Property, Value) ->
    {struct, [{"type", "scalar"},
              {"property", display(Property)},
              {"value", scalar(Value)}]}.

transform_query_items(Items) ->
    map_to_json(fun transform_query_item/1, Items).

transform_query_item(Item) ->
    {struct, transform_query_item_partial(Item)}.

transform_query_item_partial({Pos, Display}) ->
    [{"display", display(Display)}, {"pos", transform_query_pos(Pos)}].

transform_query_pos(nopos) ->
    null;

transform_query_pos({File, 1, 1}) ->
    {struct, [{file, File}]};

transform_query_pos({File, {Start, {StartLine,_}}, {End, _}}) ->
    {struct, [{file, File},
              {start, Start},
              {start_line, StartLine},
              {'end', End}]}.

transform_graph_result({error, _} = E) ->
    transform_error(E);

transform_graph_result({text, Result}) ->
    {struct, [{type, "text"},
              {list, transform_graph_text_list(Result)}]};

transform_graph_result({svg, Dot, Svg}) ->
    {struct, [{type, "svg"},
              {dot, Dot},
              {svg, Svg}]};

transform_graph_result({Type, Json}) ->
    {struct, [{type, atom_to_list(Type)},
              {data, Json}]}.

transform_search_result([]) ->
  {struct, [{"empty", true}]};

transform_search_result(Result) ->
  {struct, [{type, "search_result"},
            {result, map_to_json(fun transform_file_group/1, Result)}]}.

transform_file_group(FileGroup) ->
  { struct, [
             {fileName, proplists:get_value(fileName, FileGroup)},
             {filePath, proplists:get_value(filePath, FileGroup)},
             {chunks,   map_to_json(fun transform_chunk/1, lists:reverse(proplists:get_value(chunks, FileGroup)))}
           ] }.

transform_chunk(Chunk) ->
  { struct, [
             {startingPosition, proplists:get_value(startingPosition, Chunk)},
             {chunk,            proplists:get_value(chunk, Chunk)}
            ] }.

transform_graph_text_list(List) ->
    map_to_json(fun transform_graph_text_item/1, List).

transform_graph_text_item({From, Tos}) ->
    {struct, [{"from", display(From)},
              {"tos", map_to_json(fun display/1, Tos)}]}.

transform_duplicates_result({error, _} = E) ->
    transform_error(E);

transform_duplicates_result({Props0, Groups}) ->
    Props = lists:flatten([Props0]),
    Json = map_to_json(fun transform_duplicated_group/1, Groups),
    OutFile = case proplists:get_value(output, Props, "") of
        [] -> [];
        Path -> [{output, Path}]
    end,
    Name = atom_to_list(proplists:get_value(name, Props, Props0)),
    {struct, [{groups, Json}, {name, Name}|OutFile]}.

transform_duplicated_group(Group) ->
    Items = map_to_json(fun transform_duplicated_item/1, Group),
    {struct, [{items, Items}]}.

transform_duplicated_item(Item) ->
    Start = proplists:get_value(startpos, Item),
    End   = proplists:get_value(endpos,   Item),
    {struct, [{path, proplists:get_value(filepath, Item)},
              {start, transform_duplicated_pos(Start, -1)},
              {'end', transform_duplicated_pos(End, 0)}]}.

transform_duplicated_pos({Line, Ch}, Offset) ->
    {struct, [{line, Line - 1}, {ch, Ch + Offset}]}.

scalar(Value) when is_number(Value) ->
    Value;

scalar(Value) ->
    display(Value).

display(Value) when is_atom(Value) ->
    atom_to_list(Value);

display(Value) when is_integer(Value) ->
    integer_to_list(Value);

display(Value) when is_list(Value) ->
    Value.

map_to_json(Fun, Items) ->
    {array, lists:map(Fun, Items)}.

proplist_to_json(Proplist) ->
    {struct, Proplist}.

job_to_json(Job) ->
    Transform = fun({K, V0}) ->
        V = case K of
            launched -> transform_now(V0);
            finished -> transform_now(V0);
            parameters -> {struct, V0};
            _        -> V0
        end,
        {K, V}
    end,
    proplist_to_json([Transform(P) || P <- Job]).

transform_now(undefined) ->
    false;

transform_now(Tuple) ->
    {array, tuple_to_list(Tuple)}.

%% Case insensitive string:equal
iequal(String1, String2) ->
    string:equal(string:to_lower(String1), string:to_lower(String2)).

%% Case insensitive string:str
istr(String, SubString) ->
    string:str(string:to_lower(String), string:to_lower(SubString)).
