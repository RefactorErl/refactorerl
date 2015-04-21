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

%%% ============================================================================
%%% Module information
%%%
%%% @doc @todo
%%% @author bkil.hu

-module(ris).
-vsn("$Rev: 9876 $ ").

-export([q/1,q1/1,qstr/1, desugar/1, unpack/1]).
-export([show/1,show/2, print/1, print/2]).
-export([get_running_queries/0, kill_query/1]).

-export([undo/0, add/1, add_byname/1, drop/1, database_synchronization/0,
         add_by_emakefile/1, load_configuration/1, unload_configuration/1,
         current_posmode/0, reset/0, reset/1]).

-export([envs/0, env/1, addenv/2, setenv/2, delenv/1, system_info/0]).

-export([rename/2, move/2, extract/2, inline/1, eliminate/1, reorder/2,
     extfun/2, intrec/2, merge/2, genfun/2, expfun/1, tupfun/2, %%intmac/2,
     upregex/0, elimdupcode/3]).
-export([error_text/2]).

-export([print_dependencies/1]).

-export([clone_identifierl/0, clone_identifierl/1, show_dupcode/1, show_dupcode/2,
        show_dupcode_group/2, show_dupcode_group/3]).

-export([cluster/0, cluster/1, load_previous_clustering/0]).

% build, test, errors, cat_errors,

-include("ui.hrl").
-include_lib("referl_core/include/core_export.hrl").

-define(LocalA(T,E,A), {T,{?LocalError(E,A), ?Error:error_text(?LocalError(E,A))}}).
-define(LocalError2(E,A), ?LocalA(error,E,A)).
-define(LocalAbort2(E,A), ?LocalA(abort,E,A)).
-define(SQ,refusr_sq).
-define(TRANSFORM_SERVER,transform_server).
-record(statistic,{v}).
-record(entity,{e}).
-record(property,{v}).
-record(rich, {text, nodes}).

% @todo
error_text(prop_cont, [Result]) ->
    ["ris: can only continue from nodes (",
    io_lib:print(Result), ")"];
error_text(stat_non_num, []) ->
    ["ris: statistics are only available for properties with numeric values"];
error_text(need_nonempty, [Result]) ->
    ["ris: a non-empty result list is needed instead of (",
    io_lib:print(Result), ")"];
error_text(invalid_type, [X]) ->
    ["ris internal error: unexpected type for value (",
     io_lib:print(X), ")"];
error_text(bad_namefields, [X]) ->
    ["ris: expecting a tuple of a name and a list of fields instead of (",
     io_lib:print(X), ")"];
error_text(no_match, [Types])->
    ["ris: your selection type of ",
     io_lib:print(Types),
     " did not match any supported input combination"];
error_text(need_queryid, [X]) ->
    ["ris: expected an integer queryId instead of (", io_lib:print(X), ")"];
error_text(wrong_options, [X]) ->
    ["ris: the given argument (",
     io_lib:print(X),
     ") is not a valid list of options."];
error_text(bad_query, [X]) ->
    ["ris: the given argument (", io_lib:print(X), ") is not a query"];
error_text(bad_envkey, [X]) ->
    ["ris: (", io_lib:print(X), ") is not a valid atom for an env. node"];
error_text(not_filename, [X]) ->
    ["ris: (", io_lib:print(X), ") is not a string of filename or path"];
error_text(ErrType, ErrParams) ->
    ["ris internal error - unknown error message: {",
     io_lib:print(ErrType), ", ", io_lib:print(ErrParams), "}"].

% ris:q("mods").
% ris:q("mods.name").
% ris:q("mods.fun").
% ris:q("mods.fun.arity:sum").
% ris:q("mods.fun.(calls)+").
% ris:q("mods.fun.{calls}2").
% ris:q([mods,".fun",".name"]).
% ris:add({"../a.erl"}).
% ris:drop({"/root/erl/tool/../a.erl"}).
% ris:add_byname("../a.erl").
% ris:add(mods).
% ris:drop('mods[name=="a"]').


%%% ============================================================================
%% Semantic queries

get_running_queries()->
    uie({get_running_queries, default_output}).


kill_query(QueryId) when is_integer(QueryId)->
    uie({kill_query,QueryId});

kill_query(Q)->
    throw(?LocalAbort2(need_queryid, [Q])).


qd(Sel)->
    desugar(q(Sel)).

qd(Sel,S) ->
    desugar(q(Sel, S)).

desugar(#rich{nodes=N})->
    N;
desugar(X)->
    X.

q(X)->
    q(convert_query(X), none).

q({Q1,union,Q2},S)->
    lists:usort(qd(Q1,S)++qd(Q2,S));
q({Q1,intersect,Q2},S)->
    ?MISC:intersect(qd(Q1,S), qd(Q2,S));
q({Q1,minus,Q2},S) ->
    qd(Q1,S)--qd(Q2,S);
q({_Q1,range,_Q2},_S) ->
    todo;

q(Sel,S)when is_atom(Sel)->
    q(atom_to_list(Sel),S);

q({Sel},S)when is_list(Sel)->
    q(["files[path==\"" ++ Sel ++ "\"]"],S);

q(Sel=#rich{},S)->
    q(desugar(Sel),S);

q(Sel,S=#rich{})->
    q(Sel,desugar(S));

q(Sel,S)when is_list(Sel)->
    case lists:all(fun is_entity/1,Sel) of
        true->
            Sel;
        false->
            case io_lib:char_list(Sel) of %@todo
                true->
                    q_start(Sel,S);
                false ->
                    lists:foldl(fun(Sel1,Start)->
                                    q(Sel1,Start)
                                end, S, Sel)
            end
    end.

q_start(Sel,none)->
    run_q([{output,nodes}],[],Sel);
q_start(_Sel,[])->
    [];
q_start(Sel,Start)->
    case lists:all(fun is_entity/1, Start) of
        true ->
            q_start_ent(Sel, Start);
        false ->
            q_start_nent(Sel, Start)
    end.

q_start_ent(Sel, Start)->
    case [N || #entity{e=N} <- Start] of
        [] -> [];
        Nodes ->
            run_q([{output,nodes}],[{node_list,Nodes}],Sel)
    end.

q_start_nent([$:| Stat], Props=[#property{} | _])->
    Subjects = [Value || {property, Value} <- Props],
    ?Check(lists:all(fun is_integer/1, Subjects), 
           ?LocalError2(stat_non_num, [])),
    StatFun = refusr_sq_lib:stat_fun(list_to_atom(Stat)),
    Result = StatFun(Subjects),
    #rich{text=lists:flatten(io_lib:format("    ~s = ~p\n", [Stat, Result])), 
          nodes=[#statistic{v=Result}]};

q_start_nent(_,Start)->
    throw(?LocalAbort2(prop_cont, [Start])).

is_entity(#entity{})->
    true;
is_entity(_)->
    false.
    %@todo ?SQ:is_entity/1

q1(Sel)->
    Res = unpack(q(Sel)),
    case Res of
        [_|_]->
            hd(Res);
        _->
            throw(?LocalAbort2(need_nonempty,[Res]))
    end.

unpack(#rich{nodes=N})->
    unpack(N);
unpack(#entity{e=E}) ->
    E;
unpack(#property{v=V}) ->
    V;
unpack(#statistic{v=V}) ->
    V;
unpack(N) when is_list(N) ->
    lists:map(fun unpack/1, N);
unpack(N) ->
    N.

qstr(Sel)->
    R = q1(Sel),
    case erlang_type(R) of
        atom ->
            atom_to_list(R);
        integer->
            integer_to_list(R);
        list ->
            case io_lib:char_list(R) of %@todo
                true -> R
            end
    end.

erlang_type(X) when is_atom(X)->
    atom;
erlang_type(X) when is_integer(X)->
    integer;
erlang_type(X) when is_list(X)->
    list;
erlang_type(X) ->
    throw(?LocalError2(invalid_type,[X])).

print(R)->
    show(R, [{out, stdio}]).

print(R,Options) when is_list(Options)->
    case ?MISC:pget([out], Options) of
        [[]]->
            show(R, [{out, stdio} | Options]);
        _ ->
            show(R, Options)
    end;
print(_,O) ->
    throw(?LocalAbort2(wrong_options,[O])).

show(Result0)->
    show(Result0,[]).

show(Result0, Options) when is_list(Options)->
    Text = show_(Result0, Options),
    case ?MISC:pget([out],Options) of
        [[]]->
            Text;
        [[A]] when A==stdio; A==stdout->
            io:format("~s~n",[Text]),
            ok;
        [[FileName=[C|_]]] when is_integer(C)->
            {ok,IODev} = file:open(FileName, [write]),
            io:put_chars(IODev, Text),
            ok = file:close(IODev),
            ok;
        _ ->
            throw(?LocalAbort2(wrong_options,[Options]))
    end;
show(_,O) ->
    throw(?LocalAbort2(wrong_options,[O])).

show_(Result0,Options)->
    case ?MISC:pget([linenum,linecol],Options) of
        [[],[]]->
            case Result0 of
                #rich{text=T}->
                    T;
                _->
                    show_2(Result0, none)
            end;
        [_,_] ->
            show_2(Result0, linecol)
    end.

show_2(Result0, LineCol)->
    Result = desugar(Result0),
    case not is_entity_list(Result) orelse
         is_subset(entity_types(Result), [property, statistic]) of
            true->
                case unpack(Result) of
                    X when is_list(X) and not is_integer(hd(X)) ->
                        string:join([?MISC:any_to_string(V) || V<-X],"\n");
                    X ->
                        ?MISC:any_to_string(X)
                end;
            false->
                ?SQ:format_nodes(unpack(Result0), LineCol)
    end.

%X = "mods.funs"
convert_query(X) when is_list(X) andalso is_integer(hd(X))->
    X;
%X = mods
convert_query(X) when is_atom(X)->
    atom_to_list(X);
%X = [mods, funs]
convert_query(X=[Head| _]) when is_list(X) andalso is_atom(Head)->
    convert_query([atom_to_list(E) || E<-X]);
%X = ["mods", "funs"]
convert_query(X=[Head| Tail]) when is_list(X) andalso is_list(Head)->
    [Head] ++ [case lists:member(hd(Value), not_need_dot()) of
                   true ->
                       Value;
                   false ->
                       lists:flatten([$.]++Value)
               end || Value <- Tail];
%X = [#entity]
convert_query(X=[#entity{}|_]) ->
    X;
convert_query(X) ->
    throw(?LocalError2(bad_query,[X])).


not_need_dot()->
    [$., $[, $:, $0, $1, $2, $3, $4, $5, $6, $7, $8, $9].

%%% ============================================================================

query_nodes(Source,NodeTypes)->
    guard_nodetype(qd(Source),NodeTypes).

guard_nodetype(Nodes,NodeTypes)->
    Accepted = ?MISC:flatsort([NodeTypes]),
    lists:filter(
        fun(E) ->
            lists:member(entity_type(E), Accepted)
        end, Nodes).

dispatch_nodetype([],_Dispatchers)->
    throw(?LocalError2(empty_selection,[]));
dispatch_nodetype(Nodes,Dispatchers)->
    Got = entity_types(Nodes),
    R = [Value ||
            {NodeTypes,Value} <- Dispatchers,
            Accepted <- [lists:flatten([NodeTypes])],
        true == is_subset(Got,Accepted)],
    case R of
        [] ->
            throw(?LocalError2(no_match,[Got]));
        [_] ->
            R;
        [_|_] ->
            throw(?LocalError2(multi_match,[Got,R]))
    end.

is_entity_list(List) when is_list(List) ->
    lists:all(
        fun
            (#statistic{}) -> true;
            (#property{}) -> true;
            (#entity{}) -> true;
            (_) -> false
        end,
        List);
is_entity_list(_) ->
    false.

entity_types(Nodes) when is_list(Nodes)->
    lists:usort(lists:map(fun entity_type/1,Nodes));
entity_types(Node) ->
    [entity_type(Node)].

entity_type(#statistic{})->
    statistic;
entity_type(#property{})->
    property;
entity_type(#entity{e=Entity})->
    node_type(Entity).

node_type(Node)->
    case ?Syn:node_type(Node) of
        form     -> macro;
        variable -> variable;
        field    -> recfield;
        record   -> record;
        module   -> mod;
        file     -> file;
        func     -> function;
        expr     -> expression
    end.

%% todo Move to ?MISC.
is_subset(Sub,Sup)->
    []==Sub--Sup.

refac(Name, Args)->
    Res = transform(Name, Args),
    to_entities(Res).

transform(Name, Args)->
    Res = uie({transform, Name, [{ask_missing,false} | Args]}),
    case Res of
        {result,R,_} ->
            [Result] = ?MISC:pgetu([result],R),
            Result;
        {result,R} ->
            [Result] = ?MISC:pgetu([result],R),
            Result;
        {abort, {_,_}} -> throw(Res)
    end.

uie(FuncArgs)->
    Res = ui(FuncArgs),
    case Res of
        {ok, ResultOK} ->
            ResultOK;
        {error, {_,_}} ->
            throw(Res)
    end.

ui(NameArgs)->
    pong = net_adm:ping(?REFERL_NODE),
%    monitor_node(?REFERL_NODE, true),
%    Ref = {?TRANSFORM_SERVER,?REFERL_NODE},
    ReqID = ?UI:getid(),
    case ?UI:request(ReqID,NameArgs) of
        ok -> ui_loop(ReqID);
        deny -> {error, {deny, "The request was denied by the job server."++
                                "Please, try again later."}}
    end.

ui_loop(ReqID)->
    ui_loop(ReqID,[]).

ui_loop(ReqID,Errors)->
    receive
        {ReqID, reply, R} ->
            case Errors of
                []     -> R;
                [Error]-> {ok,Error}
            end;
        {ReqID, progress, _D} ->
            ui_loop(ReqID,Errors);
        {ReqID, question, Q={QID,_Questions}} ->
            ?UI:request(ReqID, {cancel, QID}),
            ui_loop(ReqID,[?LocalError2(no_interaction,[Q])])
    end.

%%% ============================================================================
%% Managing server

%% @doc Reset the current schema.
reset()->
    uie({reset}).

%% @doc Reset the current schema, and change the used positioning mode to the given one.
reset(PosMod)->
    uie({reset, PosMod}).

%% @doc Returns the currently used positioning mode.
current_posmode() ->
    uie({current_posmode}).

%% @doc Synchronize the contents of the database with the contents of the disc.
%% If a file has been changed on the disk since the last loading, 
%% then it will be reloaded.
%% @spec database_synchronization() -> ok | error
database_synchronization()->
    case ui({synchronize}) of
        {ok, _} -> ok;
        _ -> error
    end.

%% @doc Adds applications, files, which are described in the given Emakefile, 
%% to the database.
add_by_emakefile(EmakefilePath=[C|_]) when is_integer(C) ->
    N = uie({add_by_emakefile, EmakefilePath}),
    to_entities(N);
add_by_emakefile(P) ->
    throw(?LocalAbort2(not_filename,[P])).

%% @doc Adds include paths, macros, which are described in the given Emakefile, 
%% to the database.
load_configuration(EmakefilePath=[C|_]) when is_integer(C) ->
    case uie({load_configuration, EmakefilePath}) of
        [] ->
            ok;
        Else ->
            Else
    end;
load_configuration(P) ->
    throw(?LocalAbort2(not_filename,[P])).

%% @doc Removes include paths, macros, which are described in the given Emakefile, 
%% from the database.
unload_configuration(EmakefilePath=[C|_]) when is_integer(C) ->
    case uie({unload_configuration, EmakefilePath}) of
        [] ->
            ok;
        Else ->
            Else
    end;
unload_configuration(P) ->
    throw(?LocalAbort2(not_filename,[P])).

%% @doc Returns information about the RefactorErl system.
system_info() ->
    uie({system_info}).

%% @doc Undo the last action.
undo()->
    uie({undo,[]}).



%% Returns whether the result of a transformation is (partly) successful.
%result_is_ok(deny)                -> false;
%result_is_ok(abort)               -> false;
%result_is_ok(error)               -> false;
%result_is_ok({error, _})          -> false;
%result_is_ok(Xs) when is_list(Xs) -> lists:any(fun result_is_ok/1, Xs);
%result_is_ok(_)                   -> true.

add({Filename=[C|_]}) when is_integer(C)->
    add_byname(Filename);
add(Source)->
    add_drop(add_dir,Source).

drop(Source)->
    Files = unpack(filter_valid_files(query_nodes(Source, [file, mod]))),
    case Files of
        [] ->
            {ok, []};
        _ ->
            ui({drop_files, Files})
    end.
%    add_drop(drop_dir, Source).
filter_valid_files(Nodes)-> 
    q({Nodes, intersect, q({qd(mods), union, qd(files)}, none)}, none).

add_byname(Source=[C|_]) when is_integer(C)->
    N = uie({add_dir, [Source]}),
    to_entities(N);
add_byname(S) ->
    throw(?LocalAbort2(not_filename, [S])).

add_drop(Which, Source)->
    ModFile = query_nodes(Source, [file, mod]),
    each_ui(Which, unpack(q([ModFile, ".path"]))).

each_ui(Func,Files) ->
    Success =
        [begin
            Res = ui({Func,File}),
            case {Func,Res} of
                {drop_dir, {ok,_}} -> to_entities([File]);
                {add_dir, {ok,N}} -> to_entities(N);
                {drop_dir,_} -> []; %@todo only accept if missing
                {add_dir,_} -> [] %@todo exception
            end
         end || File <- Files],
    lists:append(Success).


%% -----------------------------------------------------------------------------
%% @doc Lists all environment nodes
%% @spec envs() -> []|[{atom(), any()}] | {error, {any(), any()}}
envs()->
    case ui({get_envs}) of
        {ok, Envs} -> lists:map(fun show_env/1, Envs);
        E = {error, {_, _}} -> E
    end.

%% @doc Lists a specific environment node
%% @spec env(atom()) -> {atom(), any()} | not_found | {error, {any(), any()}}
env(Name) when is_atom(Name)-> 
    case ui({get_env, Name}) of
        {ok, Vals} -> show_env({Name,Vals});
        E = {error, {_, _}} -> E
    end;
env(N)->
    throw(?LocalAbort2(bad_envkey,[N])).

%% @doc Adds a new environment node if it does not already exists.
%% @spec addenv(atom(), any()) -> ok | true  | {error, {any(), any()}}
addenv(EnvName, EnvVal) when is_atom(EnvName)->
    case ui({add_env, EnvName, EnvVal}) of
        {ok, Result} -> Result;
        E = {error, {_, _}} -> E
    end;
addenv(N,_)->
    throw(?LocalAbort2(bad_envkey,[N])).

%% @doc Sets the value of an environment node
%% @spec setenv(atom(), any()) -> ok | {error, {any(), any()}}
setenv(Name, Value) when is_atom(Name)->
    case ui({set_env, Name, Value}) of
        {ok, Result} -> Result;
        E = {error, {_, _}} -> E
    end;
setenv(N,_)->
    throw(?LocalAbort2(bad_envkey,[N])).

%% @doc Deletes an environment node
%% @spec delenv(atom()) -> []|[atom()] | {error, {any(), any()}}
delenv(Name) when is_atom(Name)->
    case ui({del_env, Name}) of
        {ok, Result} -> Result;
        E = {error, {_, _}} -> E
    end;
delenv(N)->
    throw(?LocalAbort2(bad_envkey,[N])).

show_env({_,[]})->
    not_found;

show_env(Env)->
    Env.

%% -----------------------------------------------------------------------------

%%% ============================================================================

any_to_atom(X) when is_atom(X)->
    X;
any_to_atom(L=[C|_]) when is_integer(C)->
    list_to_atom(L).

any_to_list(X) when is_atom(X)->
    atom_to_list(X);
any_to_list(L=[C|_]) when is_integer(C)->
    L.

rename(Old, New)->
    Atom = fun(Name)-> {name, any_to_atom(Name)} end,
    Str  = fun(Key) -> fun(Name)-> {Key, any_to_list(Name)} end end,
    L = [{function, {rename_fun,      Atom}},
         {variable, {rename_var,      Str(varname)}},
         {file,     {rename_header,   Str(filename)}},
         {mod,      {rename_mod,      Atom}},
         {record,   {rename_rec,      Atom}},
         {recfield, {rename_recfield, Atom}},
         {macro,    {rename_mac,      Str(macname)}}],
    Nodes = qd(Old),

    [{Fun, Key}] = dispatch_nodetype(Nodes, L),

    Res =
        case New of
            F when is_function(F, 1)->
                [refac(Fun, [{nodes, unpack([Node])}, Key(F([Node]))])
                || Node <- Nodes];
            _ ->
                [refac(Fun, [{nodes, unpack([Node])}, Key(New)])
                 || Node <- Nodes]
        end,
    lists:append(Res).

% ris:rename('mods[name=="a"].fun[name==f]', h).
% ris:rename('mods[name=="a"].fun[name==f].var[name=="X"]', "Y").
% ris:rename('files[name=="e.hrl"]', "y.hrl").
% ris:rename('mods[name=="a"]', "b").
% ris:rename('files.records[name==rec1]', rec2).
% ris:rename('files.records[name==rec1].fields[name==r1]', fld1).
% ris:rename('files.macros[name=="M"]', "Mac2").

move(Source, Dest)->
    Atom = fun(Name)-> {name, any_to_atom(Name)} end,
    File =
        fun(Name) ->
            Path =
                case Name of
                    {Path_} ->
                        Path_;
                    _ ->
                        qstr("files[name==\"" ++ any_to_list(Name) ++ "\"].path")
                end,
            {filename, Path}
        end,
    L = [{function, {move_fun, Atom}},
         {record,   {move_rec, File}},
         {macro,    {move_mac, File}}],
    Nodes = qd(Source),
    [{Fun, Key}] = dispatch_nodetype(Nodes, L),

    % @todo support diverse types
    Res =
        case Dest of
            F when is_function(F,1)->
                [refac(Fun, [{nodes, unpack([Node])}, Key(F([Node]))])
                    || Node <- Nodes];
            _ ->
                [refac(Fun,[{nodes,unpack(Nodes)},Key(Dest)])]
        end,
    lists:append(Res).

% ris:move('mods[name=="a"].fun[name==f]', b).
% ris:move('mods[name=="a"].record[name==r]', b).
% ris:move('mods[name=="a"].macro[name=="M"]', b).
% ris:move('mods[name=="x"].fun[name==f]', fun(Src)-> io:format("~p~n",[ris:qstr([Src,'.mod'])]),a end).

extract(Source, New)->
    extfun(Source, New).

extfun(Source, New)->
    L=[{expression,extract_fun}],
    Nodes = qd(Source),
    [Fun] = dispatch_nodetype(Nodes,L),
    refac(Fun,[{nodes,unpack(Nodes)},{name,any_to_atom(New)}]).

% ris:extract('mods[name=="a"].fun[name==f].expr[.last]', h).

inline(Source)->
    L=[{function,inline_fun},
       {macro,inline_mac}],
    Nodes = qd(Source),
    [Fun] = dispatch_nodetype(Nodes,L),
    Res = [refac(Fun,[{nodes,unpack([Node])}]) || Node <- Nodes],
    lists:append(Res).

% ris:inline('mods[name=="a"].fun[name==f].expr[.last]').

eliminate(Source)->
    L=[{variable,elim_var}],
    Nodes = qd(Source),
    [Fun] = dispatch_nodetype(Nodes,L),
    Res = [refac(Fun,[{nodes,unpack([Node])}]) || Node <- Nodes],
    lists:append(Res).

%@todo

%% -----------------------------------------------------------------------------

intrec(Source,NameFields)->
    %@todo convert
    L = [{expression, introduce_rec}],
    Nodes = qd(Source),
    [Fun] = dispatch_nodetype(Nodes, L),

    Res =
        case NameFields of
            {Name, FieldList} when is_atom(Name), is_list(FieldList)->
                SList = lists:map(fun atom_to_list/1, FieldList),
                FieldText = string:join(SList," "),
                [refac(Fun, [{nodes, unpack([Node])},
                             {name, Name}, {text, FieldText}])
                 || Node <- Nodes];

            F when is_function(F, 1)->
                [begin
                    {Name, Fields} = F([Node]),
                    refac(Fun,[{nodes, unpack([Node])},
                               {name, Name}, {text, Fields}])
                 end || Node <- Nodes];

            _ -> throw(?LocalAbort2(bad_namefields, [NameFields]))
        end,
    lists:append(Res).

% ris:intrec('mods[name=="a"].fun[name==f].expr[.last]', {r3,[f1]}).

merge(Source,New)->
    L = [{expression, merge}],
    Nodes = qd(Source),
    [Fun] = dispatch_nodetype(Nodes, L),
    Res =
        case New of
            F when is_function(F, 1)->
                [refac(Fun, [{nodes, unpack([Node])}, {varname, F([Node])}])
                 || Node <- Nodes];
            _->
                [refac(Fun, [{nodes, unpack([Node])}, {varname, New}])
                 || Node <- Nodes]
        end,
    lists:append(Res).

genfun(Source,NewVar)->
    L=[{expression,gen}],
    Nodes = qd(Source),
    [Fun] = dispatch_nodetype(Nodes,L),
    refac(Fun, [{nodes,unpack(Nodes)}, {varname,NewVar}]).

expfun(Source)->
    L=[{expression,expand_funexpr}],
    Nodes = qd(Source),
    [Fun] = dispatch_nodetype(Nodes,L),
    Res = [refac(Fun,[{nodes,unpack([Node])}]) || Node <- Nodes],
    lists:append(Res).

tupfun(Source, Range)->
    %@todo alternate calling
    L=[{function,tuple_funpar}],
    Nodes = qd(Source),
    [Fun] = dispatch_nodetype(Nodes,L),
    [ refac(Fun,[{nodes,unpack([Node])}, {posrange, Range}]) || Node <- Nodes ]. %@todo

reorder(Source, Order)->
    %@todo validate
    L=[{function,reorder_funpar}],
    Nodes = qd(Source),
    [Fun] = dispatch_nodetype(Nodes,L),
    Res =
        case Order of
            F when is_function(F,1)->
                [refac(Fun,[{nodes,unpack([Node])}, {order,F([Node])}])
                 || Node <- Nodes];
            _->
                [refac(Fun,[{nodes,unpack([Node])}, {order,Order}])
                 || Node <- Nodes]
        end,
    lists:append(Res).

upregex()->
    refac(upgrade_regexp,[]).

elimdupcode(Name, Group, FunName)
        when is_atom(Name) andalso is_integer(Group) andalso is_atom(FunName) ->
    refac(dupcode, [{clone_name,Name}, {group,Group},{name,FunName}]).


%% -----------------------------------------------------------------------------
% mockups

% @todo
run_q(DispOpt,Start,Query)->
    Args = [{display_opt,DispOpt},
            {start_opt,Start},
            {querystr,Query}],
    Result = transform(semantic_query, Args),
    [Text,Nodes] = ?MISC:pgetu([text,nodes], Result),
    V = case is_list(Nodes) of
            false->
                [#statistic{v=Nodes}];
            true->
                to_entities(Nodes)
        end,
    #rich{text=Text, nodes=V}.

to_entities(Nodes)->
    [case is_entity_(E) of
        true->
            #entity{e=E};
        false->
            #property{v=E}
     end || E <- Nodes].

is_entity_({'$gn',_,_})->
    true;
is_entity_(_)->
    false.

%%% ============================================================================
%% Dependencies

%% @spec print_dependencies(Options::proplists()) -> any()

%% @doc print_dep(Options::proplists())
%%         Prints out the dependency graph in either module or function or modulblock level.
%%
%%
%%         The Options are the following
%%         - {level, Level}, Level = mod | func | mb
%%         - {type, Type}, Type = all | cycles
%%         - {starting_nodes, [Identifier]},
%%                  Identifier = node() | Module::atom() | Function::string()
%%         - {exclude, [Identifier]}
%%         - {exclude_children, [Identifier]}
%%         - {exclude_otp, ExcludeOtp}, ExcludeOtp = true | false 
%%
%%         Examples:
%%         - ris:print_dependencies([{level, func}, {type, all}]).
%%         - ris:print_dependencies([{level, mod}, {type, cycles}]).
%%         - ris:print_dependencies([{level, func}, {type, all}, {starting_nodes, [\"ri:print_dep_h/0\"]}]).
%%         - ris:print_dependencies([{type, cycles}, {level, func}, {starting_nodes, [\"ri:print_dep_h/0\", \"ri:draw_dep_h/0\"]}]).
%%         - ris:print_dependencies([{type, all}, {level, func}, {starting_nodes, [\"ri:q/1\"]}, {exclude_otp, true}]).
%%         - ris:print_dependencies([{type, all}, {level, mod}, {exclude_otp, true}, {exclude, [ri, ris]}, {exclude_children, [reflib_ui]}]).
%% @end
print_dependencies(Options) ->
    case proplists:get_value(output, Options) of
        undefined  -> ui({draw_dep_graph, Options ++ [{output,name_terms}]});
        _          -> ui({draw_dep_graph, Options})
    end.


%%%=====================================================================
%%% Duplicate code

clone_identifierl() ->
    clone_identifierl([]).
clone_identifierl(Options)->
    ExtOptions =
        case proplists:get_value(format,Options) of
            undefined -> Options++[{format,nodes}];
            _ -> Options
        end,
    case ui({clone_identifierl, ExtOptions}) of
        {ok,ResultList} when is_list(ResultList)->
            Clones = proplists:get_value(detected_clones, ResultList),
            to_entity(Clones, proplists:get_value(format, ExtOptions));
        _ ->
            error
    end.

to_entity(Clones, nodes) ->
    lists:map(fun(CloneGroup) ->
        to_entity0(CloneGroup, nodes)
    end, Clones);
to_entity(Clones,_) ->
    Clones.

to_entity0(Clone, nodes) ->
    lists:map(fun(Cl) ->
            lists:map(fun(Element) ->
                {entity,Element} end, Cl)
        end, Clone);
to_entity0(Clone,_) ->
    Clone.

show_dupcode(Name) when is_atom(Name) ->
    show_dupcode(Name, nodes).
show_dupcode(Name, Format)
        when is_atom(Name) andalso is_atom(Format)->
    case ui({get_dupcode_result, Name, Format, undef}) of 
        {ok,[]} ->
            throw(?LocalError2(not_found,[]));
        {ok,Result} ->
            Clones = proplists:get_value(detected_clones, Result),
            _EntClones = to_entity(Clones,Format);
        _ -> error
    end.

show_dupcode_group(Name, GroupNumber) when is_atom(Name), is_integer(GroupNumber) ->
    show_dupcode_group(Name,GroupNumber, nodes).

show_dupcode_group(Name, GroupNumber, Format)
        when is_atom(Name), is_integer(GroupNumber),
             is_atom(Format)->
    case ui({get_dupcode_group, Name, GroupNumber, Format}) of
        {ok,{false, undef_name}} ->
            throw(?LocalError2(not_found,[]));
        {ok,{true, Result}} ->
            _EntClones = to_entity0(Result,Format);
        _ -> error
    end.

%%%=====================================================================
%%% ris interface for clustering

cluster(ValueList) ->
    transform(clustering, proplists:delete(ask_missing, ValueList)).
    
cluster() ->
    cluster([]).
    
load_previous_clustering() ->
    uie({cl_prev_clustering}).
