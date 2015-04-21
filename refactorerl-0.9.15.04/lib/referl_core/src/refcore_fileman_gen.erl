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

%%% @doc This module gives the refcore_fileman_gen behaviour which is needed to
%%% implement file manager modules for different positioning methods.
%%% The core of this behaviour is a supervisor implemented in this module which
%%% starts the proper refcore_fileman_name ("name" is the name of a positioning
%%% method) module according to the current positioning mode chosen by the user.
%%%
%%% @author Gabor Hosszu

-module(refcore_fileman_gen).
         
         
%-export([behaviour_info/1]).
         
-export([restart/1, behaviour_info/1]).


-export([form_hash/1, progress_start/6, progress_step/1]).
-export([set_db_hash/1, db_hash/0, inc_sem_db_hash/0]).
-export([set_file_hash/2, file_hash/1]).

-export([hold_prefix/3,input_tokens/1,store_tokens/1,tokenize/1]).
-export([output_env/0,make_backup/1,real_forms/1,orig_text/2,form_hash_neq/2]).
-export([file_text/1,set_form_hash/2,create_file_node/2]).
-export([file_status/1,dep_forms/1,parse_form/4,token_length/1,split_forms/1]).
-export([split_stop_tokens/1,merge/2,includers/1,action_length/1,act_count/1]).
-export([update_lastmod/1,merge_ws/1 ]).
-export([is_file_changed/1]).

-export([is_any_include_changed/1,
         includes_closure/1, includes_closure0/2]). % graph_update/2, add_new_file/2

%%% -*- coding: latin-1 -*-

%% Client interface / 
-export([add_file/1, add_file/2, drop_file/1, drop_file/2, save_file/1]).
-export([add_form/3, add_text/3, drop_form/2]).

%% gen_server callback functions
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% used by ri:cat_errors/1
-export([create_scanner/0]).

-export([get_pmod/0, get_tmodule/0]).

-include("core.hrl").

-behaviour(gen_server).

-record(fmenv, {scanner}).

%% @type progress() = (File::string(), Count::integer(), Max::integer()) -> ok.
%% A progress reporter function type that can be passed to {@link add_file/2}
%% and {@link drop_file/2}. This function is called to report the progress of
%% these operations. `File' is the name of the file on which progress has been
%% made, and there are `Count' steps finished a the time of the function call
%% from the total of `Max'.

-define(EnvKey, refcore_fileman_env).

%% This call style is used because we need these interface functions to work
%% when called recursively from the server process. In particular, the
%% preprocessor (which is called from the server process) must be able to add
%% include files to the graph.
-define(Call(Req), case get(?EnvKey) of
                       undefined ->
                           gen_server:call(?FILEMAN_SERVER, Req, infinity);
                       _ ->
                           handle(Req)
                   end).

-define(TModule, token_module).
-define(FModule, fileman_callback_module).
-define(PMod, posmod).

%%% ============================================================================
%%% Server callbacks

%%% @private
init(PosMod) ->
	put_envkeys(PosMod),
    create_scanner(),
    {ok, [?EnvKey, ?FModule, ?TModule]}.

%%% @private
create_scanner() ->
    Scanner = ?ErlScanner:create(),
    put(?EnvKey, #fmenv{scanner=Scanner}).


%%% @private
handle_call({set_posmod, PosMod}, _From, S) ->
	erase(?TModule),
	erase(?FModule),
	erase(?PMod),
	put_envkeys(PosMod),
	{reply, ok, S};

handle_call(Req, _From, S) ->
    {reply, handle(Req), S}.

%%% @private
handle_cast(_, S) ->
    {noreply, S}.

%%% @private
handle_info(_, S) ->
    {noreply, S}.

%%% @private
terminate(_, _) ->
    ok.

%%% @private
code_change(_, S, _) ->
    {ok, S}.


%%% ===========================================================================
%%% Behaviour core

behaviour_info(callbacks) ->
    [{update,6}, {handle_incons,2}, {make_token, 4}, {parse_error_msg, 6}].

%% @doc Starts supervisor for current posmode set by the user
start_link() ->
    PosMod = get_init_arg(pos),
    gen_server:start_link({local, ?FILEMAN_SERVER}, ?MODULE, PosMod, []).

%% @doc Restarts supervisor with the given posmode
%% The database must be reseted before this.
restart(PosMod) ->
	gen_server:call(?FILEMAN_SERVER, {set_posmod, PosMod}, infinity),
    ?Syn:set_env(db_posmode, PosMod).

%%  Pos argument initialization
%% 1, Get the initial argument
%% 2, Initialize the system with it
%%   - if we have files in the DB, we cannot use the given parameter, 
%%     we have to use the one present as an env node
%%   - if we have no files, we use the given parameter (IF VALID)
get_init_arg(pos) ->
    UserGivenPosmod =
    try 
        begin
            {ok, [[AStr]]} = init:get_argument(pos),
            {ok, Tokens, _} = erl_scan:string(AStr ++ "."),
            {ok, Term} = erl_parse:parse_term(Tokens),
            Term
        end 
    of
        X -> X
    catch
        _:_ -> hd(?availablePosModes)
    end,
    UserGivenPosmodValidated =
    case lists:member(UserGivenPosmod, ?availablePosModes) of
        true -> UserGivenPosmod;
        _    -> case UserGivenPosmod of
                    default ->  default;
                    _       ->  Info = 
                                report_text(bad_user_pos_parameter, 
                                            atom_to_list(UserGivenPosmod)),
                                error_logger:warning_report(Info),
                                bad_user_pos_parameter
                end
    end,
    CurrentPosMod =
    case ?Syn:get_env(db_posmode) of %TODO macro for this env
        %% No posmode is set
        []  -> guess_pos_mode();
        [Pos]   
            -> %% Check if it is a legal one
               case lists:member(Pos, ?availablePosModes) of
                 true ->  Pos;
               %% Illegal posmode
                 _    ->  guess_pos_mode()
               end;
        %% There are more than one pos modes
        _
            -> guess_pos_mode()
    end,
    calculate_system_posmode(UserGivenPosmodValidated, CurrentPosMod).

%% This function calculates the positioning mode which should be set according to
%% the parameter given by the user and the current DB posmode
calculate_system_posmode(bad_user_pos_parameter, CurrentPosMod) ->
    {StartPosMod, Info} =
    case ?Query:exec([file]) of
        []  -> {default, report_text(default, 
                                     erlang:atom_to_list(hd(?availablePosModes)))};
        _   -> {CurrentPosMod, report_text(existing_db,
                                            erlang:atom_to_list(CurrentPosMod))}
    end,
    error_logger:info_report(Info),
    set_pos_env(StartPosMod);
calculate_system_posmode(default, CurrentPosMod) ->
    StartPosMod =
    case ?Query:exec([file]) of
        []  -> Info = report_text(default,
                                  erlang:atom_to_list(hd(?availablePosModes))),
               error_logger:info_report(Info),
               default;
        _   -> CurrentPosMod
    end,
    set_pos_env(StartPosMod);
calculate_system_posmode(UserGivenPosmodValidated, CurrentPosMod) ->
    StartPosMod =
    case ?Query:exec([file]) of
        []  -> UserGivenPosmodValidated;
        _   -> if UserGivenPosmodValidated =/= CurrentPosMod ->
                   Info = report_text(existing_db, 
                                      erlang:atom_to_list(CurrentPosMod)) ++"\n"++
                          report_text(given_posmod_ignored,
                                      erlang:atom_to_list(UserGivenPosmodValidated)),
                   error_logger:warning_report(Info);
                  true -> nothing_to_do
               end,
               CurrentPosMod
    end,
    set_pos_env(StartPosMod).
    
%% Function to guess posmode using form attributes
guess_pos_mode() ->
    case reflib_query:exec([file,form]) of
        []       -> no_guess;
        [Form|_] -> guess_posmode_with_formstartline((?Graph:data(Form))#form.start_line)
    end.

guess_posmode_with_formstartline(undefined) -> abs;
guess_posmode_with_formstartline(_) -> rel.

set_pos_env(no_guess) ->
    error_logger:error_report(report_text(bad_db, [])),
    set_pos_env(default);
set_pos_env(default) ->
    PosMod = hd(?availablePosModes),
    ?Syn:set_env(db_posmode, PosMod),
    PosMod;
set_pos_env(PosMod) ->
    ?Syn:set_env(db_posmode, PosMod),
    PosMod.
    
create_modname(PosMod, ModPrefix) ->
    list_to_atom(lists:concat([ModPrefix, '_',PosMod])).

put_envkeys(PosMod) ->
	put(?FModule, create_modname(PosMod, refcore_fileman)),
	put(?TModule, create_modname(PosMod, reflib_token)),
	put(?PMod, PosMod).

report_text(bad_user_pos_parameter, PMod) ->
    "The given positioning mode ("++PMod++") is invalid.";
report_text(default, PMod) ->
    "The tool will be started with the default positioning mode (" ++ PMod ++ ").";
report_text(existing_db, PMod) ->
    DDir = ?MISC:data_dir(),
    GraphName = 
        case ?Graph:get_dbmod() of
            refdb_mnesia -> "";
            _ -> ", used graph name: "++?MISC:to_list(?Graph:actual_graph())
        end,
    "The actual database (used data dir: "++DDir++GraphName++
        ") was built with positioning mode " ++ PMod ++ ".\n" ++
        "The tool will be used with positioning mode ("++PMod++") for this database.";
report_text(given_posmod_ignored, GivenPMod) ->
    "The given positioning mode will be ignored.\n"++
        "Use ri:db_convert("  ++ GivenPMod  ++ ") " ++
        "to convert database to the given positioning mode."; 
report_text(bad_db, _) ->
    "The database is corrupted and can not be repaired.\n"++
        "Use ri:reset/0 or ri:reset/1 to get a new database.\n"++
        "Be aware of using ri:reset/*! It will remove all of the data "++
        "which was stored in the database.".

%%% ===========================================================================
%%% Fileman interface

get_tmodule() ->
	?Call({get_tmodule, []}).

get_pmod() ->
	?Call({get_pmod, []}).

%% =============================================================================
%%% ============================================================================
%%% Interface functions

%% @spec add_file(string()) -> {file, node()} | {error, Reason}
%%
%% @doc Reads and parses the file `Name' and inserts the syntax tree into
%% the graph. Returns the file node on success (which may contain forms with
%% errors). Note that during preprocessing, every included file will also be
%% added to the graph.
%% Note that this function does not save the graph to the disk.

add_file(Path) ->
    add_file(Path, []).

%% @spec add_file(string(), [Opt]) -> {file, node()} | {error, Reason}
%%
%% @doc The same as {@link add_file/1}, except that options can be passed to
%% modify the default behaviour. Currently supported options are:
%%
%% <dl>
%%
%%  <dt>{@type update | {update, true@}}</dt> <dd>Update the contents of
%%    `File' from disk, re-analysing only the forms that have been
%%    changed since the last update.</dd>
%%
%%  <dt>{@type {progress, progress()@}}</dt> <dd>Progress reporter to be
%%    called during analysis.</dd>
%%
%% </dl>
add_file(Path, Opts) when not is_list(Opts) ->
    add_file(Path, [Opts]);
add_file(Path, Opts) ->
    Update = proplists:get_value(update, Opts, false),
    Progress = proplists:get_value(progress, Opts, none),
    ?Call({add_file, Path, Update, Progress}).


%% @spec add_form(node(), integer() | last, [node()]) -> ok
%%
%% @doc Preprocesses `Tokens', creates a set of forms from the result, and
%% inserts these forms into `File' starting from the position `Index'.
%% `Index' specifies the position between links from `File' with tag `form'.
%% Should not be used to add a special form.
add_form(File, Index, Tokens) ->
    ?Call({add_form, File, Index, Tokens}).


%% @spec drop_form(node(), node()) -> ok
%% @doc Removes `Form' from `File', cleaning up preprocessor-related graph
%% parts.
drop_form(File, Form) ->
    ?Call({drop_form, File, Form}).


%% @spec add_text(node(), integer() | last, string()) -> ok
%%
%% @doc Turns `Text' into tokens, preprocesses them, creates a set of forms
%% from the result, and inserts these forms into `File' starting from the
%% position `Index'. `Index' specifies the position between links from `File'
%% with tag `form'.
add_text(File, Index, Text) ->
    ?Call({add_text, File, Index, Text}).


%% @spec drop_file(node()) -> ok
%%
%% @doc Removes `File' from the graph (together with all other files which
%% depend on it).
drop_file(File) ->
    drop_file(File, []).

%% @spec drop_file(string(), [Opt]) -> {file, node()} | {error, Reason}
%%
%% @doc The same as {@link drop_file/1}, except that options can be passed to
%% modify the default behaviour. Currently supported options are:
%%
%% <dl>
%%
%%  <dt>{@type {progress, progress()@}}</dt> <dd>Progress reporter to be
%%    called during analysis.</dd>
%%
%% </dl>
drop_file(FileName, Opts) ->
    Progress = proplists:get_value(progress, Opts, none),
    ?Call({drop_file, FileName, Progress}).


%% @spec save_file(node()) -> ok | {error, Reason :: string()}
%%
%% @doc Writes the textual contents of `File' back to its source.
save_file(File) ->
    ?Call({save_file, File}).

parse_error_msg(File, Line, FormIndex, Tokens, Msg, Origin) ->
    ?Call({parse_error_msg,[File, Line, FormIndex, Tokens, Msg, Origin]}).

%%% ============================================================================
%%% Implementation

%%% private type
%%% formData() = {Hash::integer(), [#token{}]}. Holds the tokens of a
%%% complete form before storing them in the graph. The hash value is used to
%%% compare with existing forms in the graph.

%%% private type
%%% inputForm() = {Hash::integer(), [{#token{}, node()}]}. Holds the
%%% tokens of a complete form after storing them in the graph. The hash value
%%% will be saved in the form node. The token list can be passed directly to
%%% the preprocessor, see {@link referl_preproc:formTokens()}.

handle({add_file, Info, Update, Progress}) ->
    Info2 =
        case Info of
            {New, Path} when is_list(Path) -> {New, ?MISC:canonical_filename(Path)};
            Path  when is_list(Path)       -> ?MISC:canonical_filename(Path);
            _                              -> Info
        end,
    handle_add_file(Info2, Update, Progress);
handle({add_form, File, Index, Tokens}) ->
    handle_add_form(File, Index, Tokens);
handle({drop_form, File, Form}) ->
    handle_drop_form(File, Form);
handle({add_text, File, Index, Text}) ->
    handle_add_text(File, Index, Text);
handle({drop_file, FileName, Progress}) when is_list(FileName) ->
    CFName = ?MISC:canonical_filename(FileName),
    handle_drop_file(CFName, Progress);
handle({drop_file, File, Progress}) ->
    handle_drop_file(File, Progress);
handle({save_file, File}) ->
    handle_save_file(File);
handle({parse_error_msg, [File, Line, FormIndex, Tokens, Msg, Origin]}) ->
	FMod = get(?FModule),
	FMod:parse_error_msg(File, Line, FormIndex, Tokens, Msg, Origin);

handle({get_tmodule,[]}) ->
	get(?TModule);

handle({get_pmod,[]}) ->
	get(?PMod).

handle_add_file({New, File}, Update, Progress) ->
    case New of
        none ->
            InternalUpdate = 
                case ?FileMan:is_file_changed(File) orelse
                         ?FileMan:is_any_include_changed(File) of
                true -> Update;
                false -> false
            end,
            case InternalUpdate of
                true  -> disk_update(File, Progress);
                false -> {file, File};
                graph -> graph_update(File, Progress) % Internal use only
            end;
        new ->
            add_new_file(File, Progress)
    end;
handle_add_file(Path, Update, Progress) ->
    Info =
        case ?Graph:path(?Graph:root(), [{file, {path, '==', Path}}]) of
            [File] -> {none, File};
            []     -> {new, Path}
        end,
    handle_add_file(Info, Update, Progress).

handle_add_form(File, Index, Tokens) ->
	FMod = get(?FModule),
    {Forms, Actions} = ?FileMan:hold_prefix(File, Index, 
                                            [{ins, ?FileMan:input_tokens(Tokens)}]),
    FMod:update(File, Forms, Actions, none, old, nocheck).

handle_drop_form(File, Form) ->
	FMod = get(?FModule),
    Ind = ?Graph:index(File, form, Form),
    {RealForms, Actions} = ?FileMan:hold_prefix(File, Ind, [{del, Form}]),
    FMod:update(File, RealForms, Actions, none, old, nocheck).

handle_add_text(File, Index, Text) ->
	FMod = get(?FModule),
    NewForms = [{ins, ?FileMan:store_tokens(T)} || T <- ?FileMan:tokenize(Text)],
    {RealForms, Actions} = ?FileMan:hold_prefix(File, Index, NewForms),
    FMod:update(File, RealForms, Actions, none, old, nocheck).


handle_save_file(File) ->
	FMod = get(?FModule),
    FileData = #file{path=Path, eol=Eol} = ?ESG:data(File),
    SavePath =
        case ?FileMan:output_env() of
            original -> Path;
            Dir      -> filename:join(Dir, filename:basename(Path))
        end,
    ?FileMan:make_backup(SavePath),
    case file:open(SavePath, [write]) of
        {ok, Dev} ->
            RealForms  = ?FileMan:real_forms(File),
            FormWTexts = [{Form, ?Syn:flat_text(Form)} || Form <- RealForms],
		
            {_, FormTexts} = lists:unzip(FormWTexts),
            io:put_chars(Dev, ?FileMan:orig_text(Eol, FormTexts)),
            file:close(Dev),
            Ret =
			case FMod:handle_incons(File, FormWTexts) of
                ok ->
                    % This is used for file saving after undo
                    ?ESG:update(File, FileData#file{lastmod=now()}),
                    %update filehash 
                    ?FileMan:set_file_hash(File, ?FileMan:file_hash(File)), 
                    ok;
                Err ->
                    Err
            end,
            ?ESG:finalize(),
            Ret;
        {error, Reason} ->
            {error, file:format_error(Reason)}
    end.


%% @spec disk_update(node(), progress()) -> {file, node()}
%% @doc If none of the included files is changed, then updates 
%% the contents of `File' from its original disk file.
%% If one of the included file is changed, then the file, its includes,  
%% and the dep files of the include files are reloaded (dropped then added).
disk_update(File, Progress) ->
    case ?FileMan:is_any_include_changed(File) of
        true -> reload_file_with_deps(File, Progress);
        false -> update_file(File, Progress)
    end.

reload_file_with_deps(File, Progress)->
    FData = ?Graph:data(File),
    IncFiles = 
        lists:usort(
          [I || I<-?FileMan:includes_closure(File), ?FileMan:is_file_changed(I)] ++
          [File]),
    DepNodes =lists:usort(lists:flatten([?FileMan:includers(IF) 
                                        || IF <- IncFiles ])),
    Deps = lists:usort( [(?Graph:data(DepNode))#file.path 
                         || DepNode<-DepNodes]),
    [drop_file(IncFile) || IncFile <- IncFiles],
    [add_file(Dep, [{progress, Progress}]) || Dep <- Deps],
    add_file(FData#file.path, [{progress, Progress}]).

update_file(File, Progress) -> 
	FMod = get(?FModule),
    FData = ?Graph:data(File),
    {Text, EOL} = ?FileMan:file_text(FData#file.path),
    ?Graph:update(File, FData#file{eol=EOL}),
    Forms = ?FileMan:real_forms(File),
    Merged = ?FileMan:merge(Forms, ?FileMan:tokenize(Text)),
    FMod:update(File, Forms, Merged, Progress, old, check),
    %% see remarks for graph_update
    [handle_add_file((?Graph:data(IF))#file.path, graph, Progress) ||
        IF <- ?FileMan:includers(File)],
    [?FileMan:update_lastmod(IF) || IF <- ?Graph:path(File, [incl])],
    ?ESG:finalize(),
    ?FileMan:file_status(File).

handle_drop_file(File, Progress) ->
	FMod = get(?FModule),
%   [{File, nocheck} | lists:map(fun(I) -> {I, nocheck} end,
%                                ?FileMan:includers(File))],
    lists:foreach(
      fun (DepFile) ->
              ?FileCacheMan:delete_file(DepFile),
              Forms = ?FileMan:real_forms(DepFile),
              FMod:update(DepFile, Forms, [{del, F} || F <- Forms], Progress, old, nocheck), 
              ?ESG:remove(?ESG:root(), file, DepFile)
      end, [ File | ?FileMan:includers(File) ]),
    ?ESG:finalize().

%% @spec add_new_file(string(), progress()) -> {file, node()}|{error, string()}
%% @doc Adds file from `Path' into the graph, assuming that it is not in the
%% graph yet.
add_new_file(Info, Progress) ->
	FMod = get(?FModule),
    try
        case Info of
            Path when is_list(Path) ->
                {Text, EOL} = ?FileMan:file_text(Path),
                File = ?FileMan:create_file_node(Path, EOL);
            File ->
                {Text, EOL} = ?FileMan:file_text((?Graph:data(File))#file.path)
        end,
        ?Graph:update(File, (?Graph:data(File))#file{eol=EOL}),
        InsList = ?FileMan:tokenize(Text), 
				%% kiemelheto
        FMod:update(File, [],
               [{ins, ?FileMan:store_tokens(F)} || F <-InsList],
               Progress, new, nocheck),
        ?ESG:finalize(),
        ?FileMan:file_status(File)
    catch
        {error, Error} ->
            {error, Error}
    end.


%% @spec graph_update(node(), progress()) -> {file, node()}
%% @doc Reparses the contents of `File' without rereading it from the disk.
%% @todo This should be replaced with a more fine-grained updating strategy
%% like invalidating only the referring forms when a preprocessor definition
%% is changed.
graph_update(File, Progress) ->
	FMod = get(?FModule),
    Forms = ?FileMan:real_forms(File),
    Ins =
        [begin
             {Hash, OrigTokens} = ?FileMan:input_tokens(Form),
             {ins, ?FileMan:store_tokens({Hash, [T || {T, _} <- OrigTokens]})}
         end || Form <- Forms],
	%% kiemelni
    FMod:update(File, Forms, [{del, F} || F <- Forms], Progress, old, nocheck),
    ?ESG:finalize(),
    FMod:update(File, Forms, Ins, Progress, old, nocheck),
    ?ESG:finalize(),
    ?FileMan:file_status(File).


%% =============================================================================

%%% ============================================================================
%%% Progress reporter

-record(progress, {op, starttime, file, max, lengths, count=1, totallen}).

progress_start(Op, Now, Path, Lengths, TotalLength, Max) ->
    #progress{op=Op, starttime = Now, file=Path, lengths=Lengths, totallen=TotalLength, max=Max}.

progress_step(Progress) -> progress_step(Progress, 1).

progress_step(Progress=#progress{op=Op, starttime=Start, count=Count, file=File, 
                                 lengths=[Length|RestLs], totallen=TLen, max=Max}, 
              N)
  when is_function(Op, 5) ->
    KBps =
        case sec_diff(Start, os:timestamp()) of
            0.0   -> 0;
%            0   -> 0;
            Sec -> Length / 1024 / Sec
        end,
    Percent =
        case {Max, TLen} of
            {0, _} -> 1;
            {_, 0} -> 1;
            _      -> Length / TLen
        end,
    Op(File, Percent, Count, Max, KBps),
    Progress#progress{lengths=RestLs, count=Count+N};
progress_step(Progress=#progress{op=none}, _) ->
    Progress.

%% todo Move to ?MISC.
sec_diff({MgS1, S1, MiS1}, {MgS2, S2, MiS2}) ->
    (MgS2 - MgS1) * 1000000 + S2 - S1 + (MiS2 - MiS1) / 1000000.



%%% ============================================================================
%%% Syntax level manipulation


%% @spec parse_form(File :: node(), integer(), module | header, referl_preproc:processedForm()) -> node()
parse_form(_File, _Index, module, {form, Form}) -> Form;
parse_form(File, Index, module, {tokens, Tokens}) -> parse(File, Index, Tokens, module);
parse_form(File, Index, module, {vtokens, Dep, Orig, Tokens}) ->
    Form = parse(File, Index, Tokens, Orig),
    ?Graph:mklink(Form, forig, Orig),
    ?Graph:mklink(Form, fdep, Dep),
    Data = ?ESG:data(Form),
    ?ESG:update(Form, Data#form{hash=virtual}),
    Form;
parse_form(_File, _Index, header, {form, Form}) -> Form;
parse_form(File, Index, header, {tokens, [{#token{type='-'}, _},
    {#token{type=record},_}|_]=Tokens}) ->
    parse(File, Index, Tokens, header);
parse_form(_File, _Index, header, {tokens, Tokens}) ->
    Form = ?ESG:create(#form{type=lex, tag=store}),
    [?ESG:insert(Form, flex, Token) || {_, Token} <- Tokens],
    Form.


%% @spec parse(File :: node(), integer(), [{#token{}, node()}], node() | header | module) -> node()
%%
%% @doc Parses `Tokens' and returns the result, the root form of the tree.
%% Origin is the original form in case of vtokens, in other cases it is module
%% or header. See the code of parse_form for more information.
parse(File, FormIndex, Tokens, Origin) ->
    TokenData =
        [{Type, 1, Data}
            || Data={#token{type = Type}, _} <- Tokens],
    case ?ErlParser:parse(TokenData) of
        {error, {Line, _Mod, Msg}} ->
            ErrMsg = parse_error_msg(File, Line, FormIndex, Tokens, Msg, Origin),
            ErrMsg == "" orelse
                error_logger:info_msg(ErrMsg),

            Form = ?ESG:create(#form{type=error, tag={1, ErrMsg}}),
            [begin
                 [?ESG:remove(P, T, N) || {T, P} <- ?Syn:parent(N)], %% possibly an unparsable expr
                 ?ESG:insert(Form, flex, N)
             end || {_, N} <- Tokens],
            Form;
        {ok, Result} -> Result
    end.

    
%%% ============================================================================
%%% File level manipulations

%% @doc Initializes a new file node for the loaded file.
create_file_node(Path, Eol) ->
    Type = case filename:extension(Path) of
               ".erl" -> module;
               _      -> header
           end,
    File = ?ESG:create(#file{type=Type, path=Path, eol=Eol, lastmod=now()}),
    ?ESG:insert(?ESG:root(), file, File),
    ?Graph:mklink(File, incl, File),
    File.

%% private type
%% eol() = {Style, Eof}
%%         Style = {'cr' | 'lf' | 'crlf'}
%%         Eof   = {'eol' | 'noeol'}

%% @spec file_text(string()) -> {string(), eol()}
%% @throws {error, string()}
%% @doc Reads the contents of a file, and returns it in a canonical format.
file_text(Path) ->
    %% TODO: move abs_path to referl_gen
    case file:read_file(?File:abs_path(Path)) of
        {ok, BinaryText} -> file_text(BinaryText, "", any);
        {error, Reason}  -> throw({error, file:format_error(Reason)})
    end.

%% @spec (binary(), string(), 'cr'|'lf'|'crlf'|'any') -> {string(), eol()}
%% @doc Canonicalizes the binary file text into a string.
%% <ol>
%%  <li>detects end-of-line marker style</li>
%%  <li>detects whether a final EOL was present</li>
%%  <li>appends a final `~n' EOL if not present</li>
%%  <li>replaces all EOLs found to `~n'</li>
%% </ol>
file_text(<<"\r\n", _/binary>>=Bin, Text, any) -> file_text(Bin, Text, crlf);
file_text(<<"\r",   _/binary>>=Bin, Text, any) -> file_text(Bin, Text, cr);
file_text(<<"\n",   _/binary>>=Bin, Text, any) -> file_text(Bin, Text, lf);

file_text(<<"\r\n">>, Text, crlf) -> {lists:reverse([$\n|Text]), {crlf, eol}};
file_text(<<"\r">>,   Text, cr)   -> {lists:reverse([$\n|Text]), {cr,   eol}};
file_text(<<"\n">>,   Text, lf)   -> {lists:reverse([$\n|Text]), {lf,   eol}};
file_text(<<>>,       Text, Eol)  -> {lists:reverse([$\n|Text]), {Eol,  noeol}};

file_text(<<"\r\n",Tail/binary>>, Text, crlf)-> file_text(Tail,[$\n|Text],crlf);
file_text(<<"\r",  Tail/binary>>, Text, cr)  -> file_text(Tail,[$\n|Text],cr);
file_text(<<"\n",  Tail/binary>>, Text, lf)  -> file_text(Tail,[$\n|Text],lf);
file_text(<<C,     Tail/binary>>, Text, Eol) -> file_text(Tail,[C  |Text],Eol).


%% @spec orig_text(eol(), Text) -> Text
%%       Text = [char() | Text]
%% @doc Restores the original whitespaces to the text stored inside graph
%% tokens.
orig_text(Eol, Text) when is_list(Text) ->
    [orig_text(Eol, El) || El <- Text];
orig_text({cr, _}, $\n)   -> $\r;
orig_text({crlf, _}, $\n) -> "\r\n";
orig_text({lf, _}, $\n)   -> $\n;
orig_text(_, Ch) when Ch >= 0, Ch =< 255 -> Ch;
orig_text(_, _) -> "".


%% @spec real_forms(node()) -> [node()]
%% @doc Returns the forms that are physically present in a file (i.e. skipping
%% the results of file inclusion).
real_forms(File) ->
    ?Query:exec(File, ?File:real_forms()).

%% @spec includers(node()) -> [node()]
%% @doc Returns files that depend on `File'.
includers(File) ->
    %% Probably these files should be sorted in a way that earlier files do
    %% not depend on later files. Currently this naive approach does not cause
    %% problems.
    ?Graph:path(File, [{incl,back}]) -- [File].


%% @doc Returns the output environment data stored in the graph representation.
%% There should be exactly one environment node; if it is not found,
%% `unsafe_save' is thrown.
output_env() ->
    case ?Graph:path(?Graph:root(), [{env, {name, '==', output}}]) of
        [Output] ->
            #env{value = Value} = ?Graph:data(Output),
            Value;
        _  ->
            throw(unsafe_save)
    end.

%%% ============================================================================
%%% Token level manipulation

%% @spec tokenize(string()) -> [formData()]
%% @doc Turns file text into a form list of token lists.
%% @todo lexical error handling
tokenize(Text) ->
    #fmenv{scanner = Scanner} = get(?EnvKey),
	FMod = get(?FModule),
    case Scanner(Text, ?ErlScanner:init(?implicitFun(FMod, make_token, 4))) of
        {ok, Tokens} ->
            [{form_hash(Form), Form} ||
                Form <- split_forms(merge_ws(split_stop_tokens(Tokens)))];
        {error, {Ln, Mod, Error}, _Line} ->
            throw({Ln, Mod:format_error(Error)})
    end.

%% @spec store_tokens(formData()) -> inputForm()
%% @doc Store token data in the graph.
%% @todo probably ESG should be used to ensure garbage collection of nodes
store_tokens({Hash, Tokens}) ->
    {Hash,
     [{Token, ?Graph:create(#lex{type=token, data=Token})} || Token <- Tokens]}.


%% @doc Split `stop' tokens into a `stop' and a `ws' or `eol' token.
split_stop_tokens([#token{type=stop, text=Text} = Token | Tail]) ->
    [$.|WS] = Text,
    EOL = lists:last(WS),
    Type = if EOL == $\n; EOL == $\r -> eol; true -> ws end,
    [Token#token{text="."}, #token{type=Type, text=WS} |
     split_stop_tokens(Tail)];
split_stop_tokens([Token|Tail]) ->
    [Token|split_stop_tokens(Tail)];
split_stop_tokens([]) ->
    [].

%% @doc Put the contents of `ws' and `eol' tokens into the `prews' and `postws'
%% fields of the proper tokens.
merge_ws([#token{type=WST, text=WS, prews=Pre},
          #token{} = Token | Tail])
  when WST == ws; WST == eol ->
    merge_ws([Token#token{prews=Pre++WS} | Tail]);

merge_ws([#token{postws=Post} = Token | Tail]) ->
    case lists:splitwith(fun (#token{type=eol}) -> true;
                             (_) -> false
                         end, Tail) of
        {Eols, []} ->
            [Token#token{postws=Post ++ join_ws(Eols)}];
        {[], Rest} ->
            [Token | merge_ws(Rest)];
        {[Eol|Eols], Rest} ->
            {Head, Next} =
                lists:splitwith(
                  fun (#token{text=Txt}) ->
                          lists:any(fun(C)-> not lists:member(C," \t\n\r") end,
                                    Txt)
                  end, Eols),
            if
                Next =:= [] ->
                    [Token#token{postws = Post ++ Eol#token.text} |
                        merge_ws(Head ++ Next ++ Rest)];
                true ->
                    [Token#token{postws = Post ++ join_ws([Eol|Head])} |
                     merge_ws(Next ++ Rest)]
            end
    end.

%% @doc Join the textual contents of tokens.
join_ws(WS)                             -> join_ws(WS, []).
join_ws([#token{text=Text}|Tail], Join) -> join_ws(Tail, [Join, Text]);
join_ws([], Join)                       -> lists:flatten(Join).

%% @spec split_forms([#token{}]) -> [[#token{}]]
%% @doc Split a token list into forms.
split_forms([]) -> [];
split_forms(Tokens) ->
    {First, Rest} = split_first_form(Tokens),
    [First|split_forms(Rest)].

split_first_form(Tokens) ->
    split_first_form(Tokens, []).
split_first_form([Head=#token{type=stop}|Tail], Form) ->
    {lists:reverse([Head|Form]), Tail};
split_first_form([], Form) ->
    {lists:reverse(Form), []};
split_first_form([Head|Tail], Form) ->
    split_first_form(Tail, [Head|Form]).

%% =============================================================================

token_length(#token{prews=Pre, text=Text, postws=Post}) ->
    length(Pre) + length(Text) + length(Post).

set_form_hash(Form, Hash) when is_integer(Hash) ->
    case ?ESG:data(Form) of
        #form{hash=virtual} -> ok;
        #form{hash=Hash} -> ok;
        Data ->
            ?Graph:update(Form, Data#form{hash=Hash})
    end;
set_form_hash(Form, Text) when is_list(Text) ->
    set_form_hash(Form, form_hash(Text)).

%% @spec form_hash([#token{}] | Text) -> term()
%%       Text = [char() | Text]
%% @doc Returns the hash value corresponding to the given text that should be
%% stored for the form.
form_hash(Tokens = [#token{}|_]) ->
    form_hash(
      [[PreWS, Text, PostWS] ||
          #token{text=Text, prews=PreWS, postws=PostWS} <- Tokens]);
form_hash(Text) ->
    erlang:phash2(lists:flatten(Text)).

set_file_hash(File, Hash) when is_integer(Hash)->
    case ?ESG:data(File) of
        #file{hash=Hash}->ok;
        Data -> 
            ?ESG:update(File, Data#file{hash=Hash}),
            ?FileCacheMan:file_hash_changed(File)
    end.

file_hash(File = {'$gn', file, _}) ->
    Forms = ?ESG:path(File, [form]),
    HashList = lists:filter(fun(X) -> X /= virtual end,
                            [(?ESG:data(Node))#form.hash || Node <- Forms]),
    erlang:phash2(HashList).
    
db_hash()->
	Files = ?Query:exec([file]),
	HashList = [(?ESG:data(Node))#file.hash || Node <- Files],
    erlang:phash2(HashList).

inc_sem_db_hash()->
    update_sem_db_hash(now()).

set_db_hash(Hash) when is_integer(Hash)->
    update_syn_db_hash(Hash);
set_db_hash({Version, Hash}) when is_tuple(Version), is_integer(Hash)->
    update_sem_db_hash(Version),
    update_syn_db_hash(Hash).

update_sem_db_hash(Version) when is_tuple(Version) ->
    ?Syn:set_env(?sem_database_hash,Version),
    Version.

update_syn_db_hash(Hash) when is_integer(Hash) ->
    case ?Syn:get_env(?syn_database_hash) of
        [Hash] ->
            Hash;
        [] ->
            ?Syn:set_env(?syn_database_hash,Hash),
            Hash;
        _ ->
            inc_sem_db_hash(),
            ?Syn:set_env(?syn_database_hash,Hash),
            Hash
     end.
	
%% =============================================================================    

%% @spec merge(Old::[node()], New::[formData()]) -> [formAction()]
%% @doc Calculates a set of actions that update an `Old' form list to be the
%% same as the forms created from `New'. Tokens to be inserted are stored in
%% the graph during this process.
%% @todo Check if LCS algorithm is better suited. Sometimes it gives much
%% better results (e.g. when a form is moved from the end to the beginning),
%% but it is more costly (however, cutting the common prefix and postfix helps
%% in the usual case). It also needs to be considered wether preprocessor
%% changes can be taken into account with LCS (although a form-based
%% invalidation for preprocessor changes may make it irrelevant).
%%
%% Another note is that when a function is updated, its usually inserted
%% before the old one is deleted. In case of `graph_update', this approach
%% does not really work, analyser modules can't cope with that.

merge(Olds, News) ->
    Merge = merge(Olds, News, []),
    [Elem || Elem = {del, _} <- Merge] ++ 
        [Elem || Elem = {Tag, _} <- Merge, Tag =/= del].

merge(Old, [], R) ->
    lists:reverse(R, [{del, Form} || Form <- Old]);
merge([], New, R) ->
    lists:reverse(R, [{ins, store_tokens(Form)} || Form <- New]);
merge([OldForm | Old]=OL, [{NewHash, _} | New]=NL, R) ->
    #form{type=Type, tag=Tag, hash=OldHash} = ?Graph:data(OldForm),
    if
        Tag =:= include, OldHash =:= NewHash ->
            merge(Old, New, [{hold, OldForm} | R]);
        OldHash =:= NewHash ->
            merge(Old, New, [{hold, OldForm} | R]);
        Type =:= macro;
        Type =:= lex, (Tag =/= store) and (Tag =/= skip) ->
            reload_all_after(OL, R, NL);
        true ->
            merge_diff(OL, NL, R)
    end.

%% @doc When updating a file, a changed form is encountered.
%% Since the form may change how the later forms behave,
%% all forms after it (including the form itself) are completely reloaded.
reload_all_after(OL, R, NL) ->
    lists:reverse(R, [{del, F} || F <- OL] ++
                     [{ins, store_tokens(F)} || F <- NL]).


merge_diff(Old, [{Hash, _}=NewForm | New], R) ->
    case lists:splitwith(fun(F) -> form_hash_neq(F, Hash) end, Old) of
        {_AllDifferent, []} ->
            merge(Old, New, [{ins, store_tokens(NewForm)} | R]);
        {DiffPrefix, [OldForm | Rest]} ->
            merge(Rest, New,
                  [{hold, OldForm} |
                   lists:reverse([{del, F} || F <- DiffPrefix], R)])
    end.

%% @doc Returns whether the given form does NOT have the given hash.
form_hash_neq(Form, Hash) ->
    case ?Graph:data(Form) of
        #form{hash=Hash} -> false;
        _ -> true
    end.

%% @doc Returns whether the top-level include form
%% or any files included through it have been changed on disk
%% after its last change is recorded in the graph representation.
%is_include_form_changed(IncForm) ->
%    [TopIncFile] = ?Graph:path(IncForm, [iref]),
%    IncFiles = ?Graph:path(TopIncFile, [incl]),
%    lists:any(fun is_file_changed/1, IncFiles).

is_any_include_changed(File)->
    IncFiles = case ?File:type(File) of
        header -> includes_closure(File);
        _ -> includes_closure(File) -- [File]
    end,
    lists:any(fun is_file_changed/1, IncFiles).

%% @doc Returns whether the file has been changed on disk
%% after its last change is recorded in the graph representation.
is_file_changed(File) ->
    is_file_changed_by_time(File) andalso is_file_changed_by_form(File).

is_file_changed_by_time(File)->
    #file{lastmod=LastMod, path=Path} = ?ESG:data(File),
    % @todo: Think about how to handle the case when the file, 
    % which may be needed to be updated, is not found on the disk.
    case file:read_file_info(Path) of
        {ok, #file_info{mtime=ModifyTime}} ->
            ModifyTime > calendar:now_to_local_time(LastMod);
        _ -> 
            false
    end.

is_file_changed_by_form(File)->
    #file{path = FilePath, hash = FileHash} = ?ESG:data(File),
    { Text, _ } = ?FileMan:file_text(FilePath),
    HashList = [ Hash || { Hash, _ } <- ?FileMan:tokenize(Text)],
    Changed = erlang:phash2(HashList) /= FileHash,
    not Changed andalso 
        begin
            ?FileMan:update_lastmod(File),
            ?ESG:finalize()
        end,
    Changed.

includes_closure(File)->
    Tab = ets:new(store, [public, ordered_set, {write_concurrency, true}]),
    includes_closure0([File], Tab),
    Result = lists:flatten(ets:match(Tab,{'$1'})),
    ets:delete(Tab),
    Result.

%% @private
includes_closure0([File], Tab) ->
    Works = ?Graph:path(File, [incl]),
    ReallyNewWork =
        [begin
             ets:insert(Tab, {Work}),
             Work
         end || Work <- Works, not  ets:member(Tab, Work)],
    Keys = [ rpc:async_call(node(), ?MODULE, includes_closure0,
                            [[E], Tab])
             || E <- ReallyNewWork],
    [ ok = rpc:yield(K) || K <- Keys],
    ok.
    
%% @spec hold_prefix(node(), integer()|last, [formAction()]) ->
%%                                            {[node()], [formAction()]}
%% @doc Constructs an action list that holds the first `Index' forms of
%% `File', and continues with `Actions'. Returns the list of real nodes as
%% well, because that will be used by `update/4'.
hold_prefix(File, Index, Actions) ->
    Prefix         = ?Graph:path(File, [{form, {1, Index}}]),
    RealForms      = real_forms(File),
    RealSet        = sets:from_list(RealForms),
    HoldRealPrefix = hold_real_prefix(Prefix, RealSet, Actions),
    {RealForms, HoldRealPrefix}.

hold_real_prefix([], _, Tail) -> Tail;
hold_real_prefix([Form|Rest], Real, Tail) ->
    case sets:is_element(Form, Real) of
        true  -> [{hold, Form} | hold_real_prefix(Rest, Real, Tail)];
        false -> hold_real_prefix(Rest, Real, Tail)
    end.


%% @spec input_tokens(node() | [node()]) -> inputForm()
%% @doc Returns token data either from an existing form or a list of existing
%% token nodes.

input_tokens(Tokens) when is_list(Tokens) ->
    TokenData = token_data(Tokens),
    {form_hash(Tokens), TokenData};

input_tokens(Form) ->
    #form{hash=Hash} = ?Graph:data(Form),
    Tokens = ?Syn:leaves(Form),
    RealHash =
    case Hash of
        undefined -> 
            form_hash(Tokens);
        _         ->
            Hash
    end,
    {RealHash, token_data(Tokens)}.

token_data(Tokens) ->
    [case ?Graph:data(Token) of
         #lex{type=token, data=D} -> {D, Token}
     end || Token <- Tokens].

dep_forms(Form) ->
    ?Graph:path(Form, [{fdep, back}]).

%% =============================================================================

action_length({ins, {_, [{    #token{type='-'}},
                         {    #token{type=atom,text="include"}},
                         {    #token{type='('}},
                         {Inc=#token{type=string}},
                         {    #token{type=')'}},
                         {    #token{type=stop}}]}}) ->
    IncludeFile = ?Token:get_value(Inc),
    incfile_length(IncludeFile);
action_length({ins, {_, Tokens}}) ->
    lists:sum([length(Text) || {#token{text=Text}, _} <- Tokens]);
action_length(_) ->
    0.

incfile_length(IncFile) ->
    {ok, Contents} = epp_dodger:parse_file(IncFile),
    SubLens = [incfile_length(SubInc)
                || {tree,attribute,
                    {attr,_,[],none},
                    {attribute,{atom,_,include},[{string,_,SubInc}]}} <- Contents
                    % not file_is_loaded(SubInc)
                    ],
    file_length(IncFile) + lists:sum(SubLens).

file_length(FileName) ->
    case filelib:is_file(FileName) of
        false ->
            0;
        true ->
            {ok, Binary} = file:read_file(FileName),
            binary_length(Binary)
    end.

binary_length(Binary) ->
    binary_length(Binary, 0).

binary_length(<<>>, N) -> N;
binary_length(<<_, Rest/binary>>, N) -> binary_length(Rest, N + 1).
%%binary_length(<<_, Rest/binary>>, N) -> binary_length(<<Rest>>, N + 1).

act_count(Actions)              -> act_count(Actions, 0).
act_count([{ins, _} | Tail], N) -> act_count(Tail, N+1);
act_count([{del, _} | Tail], N) -> act_count(Tail, N+1);
act_count([_        | Tail], N) -> act_count(Tail, N);
act_count([],                N) -> N.

%% =============================================================================

%% @spec file_status(node()) -> {file, node()} | {error, Reason}
file_status(File) ->
    case ?Graph:path(File, [{form, {type, '==', error}}]) of
        []    -> {file, File};
        [E|_] -> {error, (?Graph:data(E))#form.tag}
    end.



%% @doc Updates the last modification time of a file.
update_lastmod(File) ->
    FileData = ?Graph:data(File),
    ?ESG:update(File, FileData#file{lastmod=now()}).


%% @doc Creates a new backup file if the `file.erl' already exists
%% as described in the `backup' environment
%% (a list containing strings, 'datestamp' or 'timestamp'),
%% or `file.erl.bak.Date-Time' by default.
%% Backup is skipped if the `backup' environment contains `no_backup'.
%%
%% @todo Do something meaningful when the file copy fails.
make_backup(File) ->
    BackupStructure =
        case ?Syn:get_env(backup) of
            [no_backup] ->
                [];
            [Backup] ->
                Backup;
            _ ->
                ["bak", ".", datestamp, "-", timestamp]
        end,
    BackupPostfix = [convert_backup_str(BStr) || BStr <- BackupStructure],

    case {filelib:is_file(File), BackupPostfix} of
        {false, _} -> no_backup_needed;
        {true, []} -> no_backup_needed;
        {true, _}  ->
            NewFile = lists:flatten([File, "."] ++ BackupPostfix),
            file:copy(File, NewFile)
    end.

convert_backup_str(datestamp) ->
    {Y, Mo, D} = date(),
    io_lib:format("~4..0B~2..0B~2..0B", [Y, Mo, D]);
convert_backup_str(timestamp) ->
    {H, Mi, S} = time(),
    io_lib:format("~2..0B~2..0B~2..0B", [H, Mi, S]);
convert_backup_str(BStr) ->
    ?MISC:to_list(BStr).
