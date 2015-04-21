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

%%% @doc Graph storage server. This is a NIF based implementation of
%%% the semantical graph server interface, using kyoto cabinet as a
%%% key-value store, while the graph implementation remains in erlang.
%%%
%%% @author Andras Nemeth <andras.b.nemeth@ericsson.com>

-module(refdb_kyotomini).
-vsn("$Rev: 12311 $ "). %% "
-behaviour(refcore_gendb).

%%% ----------------------------------------------------------------------------
%%% includes

-include("core.hrl").
-include("refcore_gendb.hrl").

%%% ----------------------------------------------------------------------------
%%% macros

-define(SO_ERROR, throw(shared_object_not_loaded)).

-define(BINARY_DATA, 1).
-ifdef(BINARY_DATA).

-define(TO_RAW(Term),
	term_to_binary(Term)).
-define(TO_TERM(Bin),
	binary_to_term(Bin)).
-else.
-define(TO_RAW(Term),
	       lists:flatten(io_lib:format("~1000p",[Term]))).
-define(TO_TERM(Str),
	       try
		   {ok, Tokens, _} = erl_scan:string(Str ++ "."),
		   {ok, Term} = erl_parse:parse_term(Tokens),
		   Term
	       catch 
		   Ex -> throw({cannot_parse_term, Str, Ex})
	       end).
-endif.

-define(NOT_IMPLEMENTED, {ok, not_implemented}).

%%% ----------------------------------------------------------------------------
%%% exports

-export([init/1,
	 is_path_supported/0,
	 is_parallel_exec_supported/0,
	 create_class/1,
	 erase_class/1,
         code_change/2,
         terminate/2]).


-export([%start_link/0,
         erase_nodes/0,
         root/0, create/1, create_prot/1, update/2, delete/1, data/1, %class/1,
         mklink/3, mklink_prot/3, rmlink/3, links/1, path/2, index/3,
         set_prop/3, get_prop/2, get_props/1, del_prop/2,
         remove_garbage/0, back_links/1]).

-export([backup/0, backup/1, restore/1, ls_backups/0, backup_info/1, rm_backup/1,
         undo/0, redo/0, clean/0]).

-export([save/1, create_graph/1, rename_graph/2, ls_graphs/0, actual_graph/0,
         load_graph/1, delete_graph/1, delete_all_graphs/0]).

% for internal path handling
-export([int_path/3]).

% temporary exports
-export([kdata/1]).

%%% ----------------------------------------------------------------------------
%%% callbacks for gendb

is_path_supported() ->
    false.

is_parallel_exec_supported() ->
    false.

%%% ----------------------------------------------------------------------------
%%% initialization

init([]) ->
    Defaults
	= [
	   {db_type, 'kct'},
	   {apow, 3},
	   {msiz, 4096},
	   {dfunit, 64},
	   {psiz, 8192},
	   {pccap, 128000000},
	   {bnum, 1000000}
	  ],
    init(Defaults);

init(Args) ->
    put(kcdb_args, Args),
    load_so(application:get_env(?APPLICATION_NAME, kcdb_nif_is_loaded)),
    init_database(Args),
    io:format("~nKyoto Cabinet database is selected~n"),
    io:format("Currently you are using the following database parameters:~n"),
    init_print_args(Args),
    io:format("~nFor fine tuning Kyoto Cabinet, use the following startup command "
	      "with your custom db arguments. db_type defaults to 'kct'.~n"
	      "$ bin/referl -dbmod refdb_kyotomini -dbargs \"[{db_type, 'MyType'}, {apow, 1}, {ARG, VAL}, ...]\"~n"),
    io:format("Tuning the database may result in data loss! Use with caution!~n"),
    io:format("For the meaning of the parameters please refer to the Kyoto Cabinet documentation (kclangc.h / kcdbpoen()): "
	      "http://fallabs.com/kyotocabinet/~n~n"),
    {ok, Args}.

init_print_args([{db_type, E} | Args]) ->
    io:format("db_type (file extension): ~p~n", [E]),
    init_print_args(Args);
init_print_args([{Arg, Val} | Args]) ->
    io:format("#~p=~p~n", [Arg, Val]),
    init_print_args(Args);
init_print_args([]) ->
    ok.

%% at schema reset, the db is re-initialized. But to be 'compatible' with the 
%% current release of the ever-changing nif implementation, it have to be skipped
%% with checking 'kcdb_nif_is_loaded' in the application envs, it is okay for now.
load_so({ok, true}) ->
    ok;
load_so(_) ->
    check_release(),
    process_flag(trap_exit, true),
    So = 
	case {os:type(), erlang:system_info(wordsize)} of
	    {{win32, _}, 8} ->
		"kyoto_minimal_x64";
	    _ ->
		"kyoto_minimal"
	end,
    SOFile = filename:join([code:lib_dir(?APPLICATION_NAME), "priv", So]),
    case erlang:load_nif(SOFile, []) of
	{error, {Reason, Text}} ->
	    io:format("Error while loading shared object! ~n~p~n~p",[Reason, Text]),
	    throw(error_loading_so);
	ok ->
	    ok
    end,
    application:set_env(?APPLICATION_NAME, kcdb_nif_is_loaded, true).

check_release() ->
    case ?otpversion < 14 of
        true -> 
            io:format("(probably) incompatible release ~p. At least R14B is required~n", [erlang:system_info(otp_release)]),
	    erlang:halt();
        _ ->
            ok
    end.

init_database(Args) ->
    {KcDataDir, KcDb} = get_kyoto_file(Args),
    KcDbArgs = KcDb ++ get_kyoto_paramlist(Args),
    %%"#apow=3#msiz=4096#dfunit=64#psiz=8192#pccap=128000000#bnum=1000000",
    %% DataDir = mnesia:system_info(directory),
    %% filelib:ensure_dir(filename:join([DataDir, "kyotomini", "x"])),
    %% DataFile = filename:join([DataDir,"kyotomini", "default"]), % kcd is directory hash, ...
    %?d(DataFile ++ "." ++ Extension),
    case kyoto_create_db(KcDataDir, KcDbArgs) of % also badarg if filename is bad
	ok ->
	    ok;
	Error ->
	    io:format("Database error occurred: ~p. Continuing with resetting the database.~n~n", [Error]),
	    reset_database(KcDataDir, KcDbArgs)
    end.

get_kyoto_file(Args) ->
    Extension = atom_to_list(get_kyoto_extension(Args)),
    DataDir = get_data_dir(),
    filelib:ensure_dir(filename:join([DataDir, "x"])),	    
    case string:str(Extension, "kc") of
	0 ->
	    % memory only database, no files will be used
	    {DataDir, Extension};
	_ ->
	    % file based database
	    % kcd is directory hash, ...
	    KcGraph = get_actual_graph(),
	    GraphDir = filename:join([DataDir, KcGraph]), 
	    filelib:ensure_dir(filename:join([GraphDir, "x"])),
	    {GraphDir, filename:join([GraphDir, "database." ++ Extension])}
    end.

get_kyoto_extension(Args) ->
    proplists:get_value(db_type, Args, 'kct').

get_kyoto_paramlist([{db_type, _} | Args]) ->
    get_kyoto_paramlist(Args);
get_kyoto_paramlist([{Key, Val} | Args]) ->
    lists:flatten(io_lib:format("#~p=~p", [Key, Val])) ++
	get_kyoto_paramlist(Args);
get_kyoto_paramlist([]) ->
    [].

%% graphs are stored in the file "_kc_graphs"
%% [{actual_graph, Name}, {graph_list, [GraphName1, GraphName2 | ...]}]
get_graphs_file_contents() ->
    KcGraphFile = kc_graph_file(),
    case file:consult(KcGraphFile) of
	{ok, [Terms]} ->
	    Terms;
	{error, enoent} ->
	    Data = [{actual_graph, default}, {graphs, [default]}],
	    set_graphs_file_contents(Data),
	    Data
    end.    

set_graphs_file_contents(Data) ->
    KcGraphFile = kc_graph_file(),
    file:write_file(KcGraphFile, io_lib:format("~1000p.", [Data])).

get_actual_graph() ->
    Terms = get_graphs_file_contents(),
    %% ActualGraph = proplists:get_value(actual_graph, Terms, default),
    %% atom_to_list(ActualGraph).
    proplists:get_value(actual_graph, Terms, default).

set_actual_graph(Name) when is_atom(Name) ->
    Data = get_graphs_file_contents(),
    NewData = lists:keystore(actual_graph, 1, Data, {actual_graph, Name}),
    set_graphs_file_contents(NewData).

get_graphs() ->
    Data = get_graphs_file_contents(),
    proplists:get_value(graphs, Data).    

set_graphs(Graphs) when is_list(Graphs) ->
    Data = get_graphs_file_contents(),
    NewData = lists:keystore(graphs, 1, Data, {graphs, Graphs}),
    set_graphs_file_contents(NewData).

kc_graph_file() ->
    filename:join([get_data_dir(),  "_kc_graphs"]).

get_graph_dir() ->
    filename:join([get_data_dir(), get_actual_graph()]).

get_data_dir() ->
    filename:join([mnesia:system_info(directory), "kyotomini"]).

reset_database(KcDataDir, KcDbArgs) ->
    Files = filelib:wildcard(KcDataDir ++ "/database.*"),
    lists:foreach(fun(F) -> file:delete(F) end, Files),
    case kyoto_create_db(KcDataDir, KcDbArgs) of
	ok ->
	    ok;
	Error ->
	    throw({cannot_open_db, KcDbArgs, Error})
    end.

%%% ----------------------------------------------------------------------------
%%% behaviour callbacks

code_change(_OldVsn, _State) ->
    no_code_change.

terminate(_Reason, _State) ->
    ok.

%%% ----------------------------------------------------------------------------
%%% graph implementation

erase_nodes() ->
    kerase_nodes().

create_class(_Class) ->
    ok.

erase_class(root) -> %% cheat
    kerase_nodes();
erase_class(_Class) ->
    ok.

root() ->
    {ok, {?NODETAG, root, 0}}.

data({?NODETAG, root, 0}) ->
    {ok, {root}};
data(Node = {?NODETAG, _Class, _Id}) ->
    case kdata(Node) of
	bad_node -> {error, bad_node};
	Result -> {ok, Result}
    end.

create(Data) ->
    Class = get_class(Data),
    ?IS_VALID_CLASS(Class),
    Node = {?NODETAG, Class, next_id(Class)},
    case kcreate(Node, Data) of
	ok -> {ok, Node};
	Er -> {error, Er}
    end.

get_class(Data) ->
    element(1, Data).

create_prot(Data) ->
    case create(Data) of
	{ok, Node} ->
	    case kcreate({Node, protected}, protected) of
		ok -> {ok, Node};
		Er -> kdelete(Node), {error, Er}
	    end;
	Er -> {error, Er}
    end.

update(Node, Data) ->
    case kdata({Node, protected}) of
	protected ->
	    {ok, protected_node};
	_ ->
	    case kupdate(Node, Data) of
		ok ->
		    ok;
		Error ->
		    {error, bad_node}
	    end
    end.

delete(Node) ->
    case kdata({Node, protected}) of
	protected ->
	    {ok, protected_node};
	_ ->
	    delete_links(Node),
	    kdelete(Node)
    end.

%% TODO: maybe it could be written a bit more optimized,
%% without this many database opearation
delete_links(Node = {?NODETAG, _Class, _Id}) ->
    FLinks = kdata({Node, links, fwd}),
    BLinks = kdata({Node, links, back}),
    delete_links_fwd(Node, FLinks),
    delete_links_back(Node, BLinks),
    ok.
    % finally, delete the links physically
    %% kdelete({Node, links, fwd}),
    %% kdelete({Node, links, back}).

delete_links_fwd(Node, [{Tag, TNodes} | Links]) ->
    delete_single_link_fwd(Node, Tag, TNodes),
    delete_links_fwd(Node, Links);
delete_links_fwd(_Node, []) ->
    ok;
delete_links_fwd(_Node, bad_node) ->
    % no links have created yet
    ok.


delete_single_link_fwd(Node, Tag, [{_Ind, TNode} | TNodes]) ->
    rmlink(Node, Tag, TNode),
    delete_single_link_fwd(Node, Tag, TNodes);
delete_single_link_fwd(_Node, _Tag, []) ->
    ok.

delete_links_back(Node, [{Tag, FNodes} | Links]) ->
    delete_single_link_back(Node, Tag, FNodes),
    delete_links_back(Node, Links);
delete_links_back(_Node, []) ->
    ok;
delete_links_back(_Node, bad_node) ->
    % no links have created yet
    ok.

% 'incl' links are between the same node, so we have already
% removed these in delete_single_link_fwd
delete_single_link_back(Node, incl, [{_Ind, Node} | FNodes]) ->
    delete_single_link_back(Node, incl, FNodes);
delete_single_link_back(Node, Tag, [{_Ind, FNode} | FNodes]) ->
    rmlink(FNode, Tag, Node),
    delete_single_link_back(Node, Tag, FNodes);
delete_single_link_back(_Node, _Tag, []) ->
     ok.


%% links are stored in this form:
%% Key = {FNode, links, fwd} or {TNode, links, back}
%% Value = [{Tag, [{Index, Node} | _]} | _]
mklink(FNode = {?NODETAG, FCl, _FId},
       TagInfo,
       TNode = {?NODETAG, TCl, _TId}) ->
    {Tag, Ind} =
	case is_atom(TagInfo) of
	    true ->
		{TagInfo, last};
	    false ->
		TagInfo
	end,
    case ?GET_NEXT_CLASS_FWD(FCl, Tag) of
	{class, TCl} ->
	    case {kexists(FNode), kexists(TNode)} of
		{exists, exists} ->
		    addlinks(FNode, {Tag, Ind}, TNode);
		{no_exists, exists} ->
		    {error, {bad_node, FNode}};
		{exists, no_exists} ->
		    {error, {bad_node, TNode}};
		{no_exists, no_exists} ->
		    {error, {bad_nodes, [FNode, TNode]}}
	    end;
	_ ->
	    {error, bad_link}
    end.

addlinks(FNode, Tag, TNode) ->
    case addlink({FNode, links, fwd}, Tag, TNode) of
	ok ->
	    case addlink({TNode, links, back}, Tag, FNode) of
		ok ->
		    ok;
		bad_node ->
		    removelink({FNode, links, fwd}, Tag, TNode),
		    {error, bad_node}
	    end;
	bad_node ->
	    {error, bad_node}
    end.

addlink(NodeKey, {Tag, Ind}, TNode) ->
    Links =
	case kdata(NodeKey) of
	    bad_node ->
		kcreate(NodeKey, []),
		[]; % links don't exist yet
	    Ls when is_list(Ls) ->
		Ls
	end,
    TagLinks =
	case lists:keysearch(Tag, 1, Links) of
	    {value, {Tag, TagLs}} -> TagLs;
	    false -> []
	end,
    case lists:keymember(TNode, 2, TagLinks) of %lists:keysearch(TNode, 2, TagLinks) of
	true -> %{value, {_, TNode}} ->
	    ok; % link to TNode already exists
	false ->
	    NewTagLinks =
		case Ind of
		    last ->
			Index = length(TagLinks) + 1,
			TagLinks ++ [{Index, TNode}];
			%%lists:keystore(Index, 1, TagLinks, {Index, TNode});
		    _ ->
			[L || L = {I, _} <- TagLinks, I < Ind]
			++ [{Ind, TNode}]
			++ [{I+1, TN} || {I, TN} <- TagLinks, I >= Ind]
		end,
	    %%NewTagLinks = lists:keystore(Index, 1, TagLinks, {Index, TNode}),
	    NewLinks = lists:keystore(Tag, 1, Links, {Tag, NewTagLinks}),
	    case kupdate(NodeKey, NewLinks) of
		bad_node -> {error, bad_node};
		ok -> ok
	    end    
    end.

removelink(NodeKey, Tag, TNode) ->
    case kdata(NodeKey) of
	bad_node ->
	    {error, bad_node};
	Links when is_list(Links) ->
	    case lists:keysearch(Tag, 1, Links) of
		{value, {Tag, TNodes}} ->
		    case lists:keysearch(TNode, 2, TNodes) of
			false ->
			    {error, not_exists};
			{value, {I, TNode}}  ->
			    NewTNodes = [{Ind, TN} || {Ind, TN} <- TNodes, I > Ind] ++
				[{Ind-1, TN} || {Ind, TN} <- TNodes, I < Ind],
			    NewLinks = lists:keystore(Tag, 1, Links, {Tag, NewTNodes}),
			    case kupdate(NodeKey, NewLinks) of
				bad_node -> {error, bad_node};
				ok -> ok
			    end
		    end;
		false ->
		    %{error, {bad_link, NodeKey, Tag, TNode}}
		    {error, not_exists}
	    end
    end.

rmlink(FNode, Tag, TNode) ->
    case removelink({FNode, links, fwd}, Tag, TNode) of
	ok ->
	    removelink({TNode, links, back}, Tag, FNode);
	Er ->
	    Er
    end.

mklink_prot(FNode, TagInfo, TNode) ->
    case mklink(FNode, TagInfo, TNode) of
	ok ->
	    case kcreate({{FNode, TagInfo, TNode}, protected}, protected) of
		ok -> 
		    ok;
		Er ->
		    rmlink(FNode, TagInfo, TNode),
		    {error, Er}
	    end;
	Error ->
	    Error
    end.

% returns only links starting from Node
links(Node) ->
    case kdata({Node, links, fwd}) of
	bad_node ->
	    {ok, []};
	Links ->
	    {ok, [{Tag, TNode} || {Tag, TNodes} <- Links, {_Ind, TNode} <- TNodes]}
    end.

back_links(_Node) ->
    {ok, {not_implemented}}.

remove_garbage() ->
    {ok, not_implemented}.

set_prop(Node, Key, Value) ->
    case {kexists(Node), kdata({Node, props})} of
	{exists, bad_node} ->
	    case kcreate({Node, props}, [{Key, Value}]) of
		ok ->
		    ok;
		Er ->
		    {error, Er}
	    end;
	{exists, Props} when is_list(Props) ->
	    NewProps = lists:keystore(Key, 1, Props, {Key, Value}),
	    case kupdate({Node, props}, NewProps) of
		ok ->
		    ok;
		Er ->
		    {error, Er}
	    end;
	{no_exists, _} ->
	    {error, bad_node}
    end.

get_prop(Node, Key) ->
    case get_props(Node) of
	{error, bad_node} ->
	    {error, bad_node};
	{ok, Props} when is_list(Props) ->
	    case lists:keysearch(Key, 1, Props) of
		{value, Prop} ->
		    {ok, Prop};
		false ->
		    {error, bad_node}
	    end
    end.

get_props(Node) ->
    case kdata({Node, props}) of
	bad_node ->
	    {error, bad_node};
	Props when is_list(Props) ->
	    {ok, Props}
    end.

del_prop(Node, Key) ->
    case get_props(Node) of
	{error, bad_node} ->
	    {error, bad_node};
	{ok, Props} ->
	    NewProps = lists:filter(
			 fun({K,_}) when K =:= Key ->
				 false;
			    (_) ->
				 true
			 end, Props),
	    case kupdate({Node, props}, NewProps) of
		ok ->
		    ok;
		Er ->
		    {error, Er}
	    end
    end.

index(FNode, Tag, TNode) ->
    case kdata({FNode, links, fwd}) of
	bad_node ->
	    {ok, none};
	Links ->
	    {ok, index_int(lists:keysearch(Tag, 1, Links), TNode)}
    end.

index_int({value, {_Tag, Links}}, TNode) ->
    index_search(Links, TNode);
index_int(false, _TNode) ->
    none.

index_search([{Ind, TNode} | _Links], TNode) ->
    Ind;
index_search([_ | Links], TNode) ->
    index_search(Links, TNode);
index_search([], _TNode) ->
    none.

path(Node, Path) ->
    ?HANDLE_PATH_PLEASE(Node, Path).
    %refcore_gendb:handle_path_here(Node, Path, ?MODULE).

%% returns: {FNode, Ind, TNode}
int_path([], _Dir, _Tag) ->
    [];
int_path([Node | Nodes], Dir, Tag) ->
    case kdata({Node, links, Dir}) of
	bad_node ->
            int_path(Nodes, Dir, Tag);
	bad_binary ->
	    {error, bad_binary};
	Links ->
		case lists:keysearch(Tag, 1, Links) of
		    {value, {Tag, TNodeList}} ->
		        lists:append([{{Node, Tag}, Ind, TNode} || {Ind, TNode} <- TNodeList],
				     int_path(Nodes, Dir, Tag));
%		        lists:append([TNode || {Ind, TNode} <- TNodeList],
%				     int_path(Nodes, Dir, Tag));
		    false ->
			int_path(Nodes, Dir, Tag)
		end
    end.

next_id(Obj) ->
    IdKey = {next_id, Obj},
    case kdata(IdKey) of
	bad_node ->
	    kcreate(IdKey, 1),
	    1;
	Id when is_integer(Id) ->
	    NewId = Id + 1,
	    kupdate(IdKey, NewId),
	    NewId
    end.

%%% ----------------------------------------------------------------------------
%%% backup / recovery / undo

backup() ->
    backup("").

backup(CommitLog) when is_atom(CommitLog) ->
    backup(atom_to_list(CommitLog));

backup(CommitLog) when is_list(CommitLog) ->
    Bups = get_backup_list(),
    BupNum = length(Bups) + 1,
    BupName = "backup." ++ integer_to_list(BupNum),
    case kyoto_make_backup(BupName) of
	ok ->
	    {ok, {set_backup_list([{BupNum, CommitLog, calendar:local_time()} | Bups]),
		  BupName}};
	Error ->
	    {error, Error}
    end.

get_backup_list() ->
    GraphDir = get_graph_dir(),
    BackupFile = filename:join([GraphDir, "_kc_backups"]),
    case file:consult(BackupFile) of
	{ok, [B]} ->
	    B;
	{error, enoent} ->
	    []
    end.

set_backup_list(Data) ->
    GraphDir = get_graph_dir(),
    BackupFile = filename:join([GraphDir, "_kc_backups"]),
    file:write_file(BackupFile, io_lib:format("~10000p.", [Data])).

    %% {Num, Bups} =
    %% case kdata(kc_backup_list) of
    %% 	bad_node ->
    %% 	    kcreate(kc_backup_list, []),
    %% 	    {1, []};
    %% 	Bs ->
    %% 	    {length(Bs) + 1, Bs}
    %% end,
    %% BackupFile = "backup." ++ integer_to_list(Num),
    %% case kyoto_make_backup(BackupFile) of
    %% 	ok ->
    %% 	    {ok, {kupdate(kc_backup_list, Bups ++ [{1, CommitLog, calendar:local_time()}]),
    %% 		  BackupFile}};
    %% 	Error ->
    %% 	    {error, Error}
    %% end.

    %% case {kyoto_make_backup(Name), kdata(backup_list)} of
    %% 	{ok, bad_node} ->
    %% 	    {ok, {kcreate(backup_list, [Name]), Name}};
    %% 	{ok, Bups} ->
    %% 	    {ok, {kupdate(backup_list, lists:umerge([Name], Bups)), Name}};
    %% 	{X, _} ->
    %% 	    {ok, {error, X}}
    %% end.

save(before_transformation) ->
    kyoto_make_backup(?BEFORE_TRANSFORMATION_FILE);
save(_FileName) ->
    %?NOT_IMPLEMENTED.
    %% there is no use for doing a sync, since if 
    %% kc is not closed properly, it won't open
    %% next time. So it is skipped here.
    ok = kyoto_sync_db().

restore(Name) when is_integer(Name) ->
    restore("backup." ++ integer_to_list(Name));
restore(Name) when is_atom(Name) ->
	restore(atom_to_list(Name));
restore(Name) when is_list(Name) ->
    case kyoto_load_backup(Name) of
	ok ->
	    ok;
	_Error ->
	    {ok, {invalid_backup, Name}}
    end.

ls_backups() ->
    Bups = get_backup_list(),
    {ok, ["backup." ++ integer_to_list(Id) || {Id, _, _} <- Bups]}.

    %% case kdata(kc_backup_list) of
    %% 	bad_node ->
    %% 	    {ok, []};
    %% 	Bups ->
    %% 	    {ok, ["backup." ++ integer_to_list(Id) || {Id, _, _} <- Bups]}
    %% end.

rm_backup(Name) ->
    case kdata(kc_backup_list) of
	bad_node ->
	    {ok, no_such_backup};
	Bups ->
	    case lists:member(Name, Bups) of
		true ->
		    rm_backup_int(Name, Bups);
		false ->
		    {ok, no_such_backup}
	    end
    end.

rm_backup_int(Name, Bups) ->
    NewBups = lists:filter(fun(B) -> B == Name end, Bups),
    case kyoto_remove_backup(Name) of
	ok ->
	    {ok, kupdate(kc_backup_list, NewBups)};
	nok ->
	    {ok, backup_not_found}
    end.

backup_info(Backup) ->
    Bups = get_backup_list(),
    case lists:keysearch(Backup, 1, Bups) of
	{value, {Backup, Log, DateTime}} ->
	    {ok, {list_to_atom("backup." ++ integer_to_list(Backup)),
		  backup_format_time(DateTime),
		  Log}};
	false ->
	    {invalid_backup, "backup." ++ integer_to_list(Backup)}
    end.

    %% case kdata(kc_backup_list) of
    %% 	bad_node ->
    %% 	    ok;
    %% 	Bups ->
    %% 	    case lists:keysearch(Backup, 1, Bups) of
    %% 		{value, {Backup, Log, Time}} ->
    %% 		    {"backup." ++ integer_to_list(Backup),
    %% 		     backup_format_time(Time),
    %% 		     Log};
    %% 		false ->
    %% 		    {invalid_backup, "backup." ++ integer_to_list(Backup)}
    %% 	    end
    %% end.

backup_format_time(DateTime) ->
    {{YY, MM, DD}, {Hour, Min, Sec}} = DateTime,
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
                  [YY, MM, DD, Hour, Min, Sec]). 

undo() ->
    kyoto_load_backup(?BEFORE_TRANSFORMATION_FILE).

redo() ->
    ?NOT_IMPLEMENTED.

clean() ->
    Bups = get_backup_list(),
    [kyoto_remove_backup("backup." ++ integer_to_list(Id)) || {Id, _, _} <- Bups],
    set_backup_list([]).
    
    %% case kdata(kc_backup_list) of
    %% 	bad_node ->
    %% 	    {ok, no_backups_found};
    %% 	Bups ->
    %% 	    lists:foreach(fun(B) -> kyoto_remove_backup(B) end, Bups),
    %% 	    kupdate(kc_backup_list, [])
    %% end.

%%% ----------------------------------------------------------------------------
%%% multi-graph stuff

create_graph(Name) ->
    Graphs = get_graphs(),
    case lists:member(Name, Graphs) of
	true ->
	    {ok, {graph_already_exists, Name}};
	false ->
	    filelib:ensure_dir(filename:join([get_data_dir(), Name, "x"])), 
	    NewGraphs = Graphs ++ [Name],
	    set_graphs(NewGraphs),
	    {ok, ok}
    end.
    
    %% Graphs = kdata(kc_graphs),
    %% case lists:keymember(Name, 1, Graphs) of
    %% 	true ->
    %% 	    {ok, {graph_already_exists, Name}};
    %% 	false ->
    %% 	    NewGraphId = length(Graphs),
    %% 	    NewGraphs = Graphs ++ [{Name, NewGraphId}],
    %% 	    kupdate(kc_graphs, NewGraphs),
    %% 	    {ok, ok}
    %% end.

rename_graph(OName, NName) ->
    Graphs = get_graphs(),
    case {lists:member(OName, Graphs), lists:member(NName, Graphs)} of
	{true, false} ->
	    kyoto_close_db(),
	    case file:rename(filename:join([get_data_dir(), OName]),
			     filename:join([get_data_dir(), NName])) of
		ok ->
		    NewGraphs = [NName | [E || E <- Graphs, E /= OName]],
		    set_graphs(NewGraphs),
		    set_actual_graph(NName),
		    init_database(get(kcdb_args));
		Error ->
		    {ok, {graph_rename_failed, [OName, NName, Error]}}
	    end;
	{false, _} ->
	    {ok, {graph_not_exist, OName}};
	{_, true} ->
	    {ok, {graph_already_exist, NName}}
    end.

    %% Graphs = kdata(kc_graphs),
    %% case lists:keysearch(OName, 1, Graphs) of
    %% 	{value, {OName, Id}} ->
    %% 	    NewGraphs = lists:keyreplace(OName, 1, Graphs, {NewName, Id}),
    %% 	    kupdate(kc_graphs, NewGraphs);
    %% 	false ->
    %% 	    {ok, {graph_not_exist, OName}}
    %% end.

ls_graphs() ->
    Graphs = get_graphs(),
    {ok, Graphs}.

    %% Graphs = kdata(kc_graphs),
    %% {ok, lists:map(fun({Name, _Id}) -> Name end, Graphs)}.

actual_graph() ->
    {ok, get_actual_graph()}.

    %% Graphs = kdata(kc_graphs),
    %% ActualGraph = get_actual_graph(),
    %% {ok, ActualGraph}.

load_graph(Name) ->
    Graphs = get_graphs(),
    case lists:member(Name, Graphs) of
	true ->
	    kyoto_close_db(),
	    Args = get(kcdb_args),
	    set_actual_graph(Name),
	    init_database(Args),
	    ok;
	false ->
	    {ok, {graph_not_exist, Name}}
    end.

    %% Graphs = kdata(kc_graphs),
    %% case lists:keysearch(Name, 1, Graphs) of
    %% 	{value, {Name, Id}} ->
    %% 	    kupdate(kc_actual_graph, Id);
    %% 	false ->
    %% 	    {ok, {graph_not_exist, Name}}
    %% end.

delete_graph(Name) ->
    ActualGraph = get_actual_graph(),
    case Name == ActualGraph of
	true ->
	    {ok, {graph_is_in_use, Name}};
	false ->
	    Graphs = get_graphs(),
	    case lists:member(Name, Graphs) of
		true ->
		    NewGraphs = [E || E <- Graphs, E /= Name],
		    set_graphs(NewGraphs),
		    GraphDir = filename:join([get_data_dir(), Name]),
		    Files = filelib:wildcard(filename:join([GraphDir, "database."]) ++ "*"),
		    lists:foreach(fun(F) -> file:delete(F) end, Files),
		    file:del_dir(GraphDir),
		    ok;
		false ->
		    {ok, {graph_not_exist, Name}}
	    end
    end.			

delete_all_graphs() ->
    Graphs = get_graphs(),
    lists:foreach(fun(G) -> delete_graph(G) end, Graphs).

%%% ----------------------------------------------------------------------------
%%% encoding / decoding erlang terms for kyoto cabinet

kcreate(Key, Value) ->
    %?d({Key, Value}),
    kyoto_create(?TO_RAW(Key), ?TO_RAW(Value)).

kupdate(Key, Value) ->
    %?d({Key, Value}),
    kyoto_update(?TO_RAW(Key), ?TO_RAW(Value)).

kdelete(Key) ->
    %?d(Key),
    kyoto_delete(?TO_RAW(Key)).

kdata(Key) ->
    case kyoto_data(?TO_RAW(Key)) of
	bad_data_tag -> % error in the data storage!!
	    throw({error, bad_data_tag, Key});
	bad_node -> % let the user decide what to do
	    bad_node;
	Raw ->
	    ?TO_TERM(Raw)
    end.

kexists({?NODETAG, root, 0}) ->
    exists;
kexists(Key) ->
    %?d(Key),
    kyoto_exists(?TO_RAW(Key)).

kerase_nodes() ->
    kyoto_erase_nodes().

%%% ----------------------------------------------------------------------------
%%% functions implemented in C, using kyoto cabinet as the key-value storage

kyoto_create(_Key, _Value) -> ?SO_ERROR.
kyoto_update(_Key, _Value) -> ?SO_ERROR.
kyoto_delete(_Key) -> ?SO_ERROR.
kyoto_data(_Key) -> ?SO_ERROR.
kyoto_exists(_Key) -> ?SO_ERROR.
kyoto_erase_nodes() -> ?SO_ERROR.
kyoto_create_db(_DataDir, _Db) -> ?SO_ERROR.
kyoto_close_db() -> ?SO_ERROR.
kyoto_sync_db() -> ?SO_ERROR.
kyoto_make_backup(_Name) -> ?SO_ERROR.
kyoto_load_backup(_Name) -> ?SO_ERROR.
kyoto_remove_backup(_Name) -> ?SO_ERROR.
