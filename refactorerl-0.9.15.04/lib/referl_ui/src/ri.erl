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

%%% @doc This module implements a console user interface for the tool.
%%%
%%% The idea behind `ri' is to enable the user to be able to do refactorings
%%% as easily as you can do debugging with Erlang's dbg module.
%%% You give a simple, short command, and get response on the console.
%%% Functions suffixed by `_h' give brief help about the respective function.
%%%
%%% == Server management command list ==
%%% <ul>
%%%   <li>add(FDML)
%%%       - add a module, file/dir or a list of these to the database</li>
%%%   <li>drop(FDML)
%%%       - drop (remove) the said way from the database</li>
%%%   <li>save(FDML)
%%%       - save the said way from the database to physical files</li>
%%%   <li>ls()
%%%       - lists files that are in the database</li>
%%%   <li>undo()
%%%       - undo the transformation (rollback, only once!)</li>
%%%   <li>reset()
%%%       - reset the database to an empty state, but valid schema</li>
%%%   <li>graph(Target)
%%%       - assume no options and call one of the next two</li>
%%%   <li>graph(Atom,Options)
%%%       - assume ".dot" extension and call the one below</li>
%%%   <li>graph(File,Options)
%%%       - draw the database graph with the given options</li>
%%% </ul>

%%% == Transformation command list ==
%%% <ul>
%%%   <li>elimvar(In, Pos)
%%%       - eliminate variable</li>
%%%   <li>elimfun(In, {FunName, Arity})
%%%       - eliminate unused function</li>
%%%   <li>elimmac(In, MacName)
%%%       - eliminate unused macro</li>
%%%   <li>elimrec(In, RecordName)
%%%       - eliminate unused record</li>
%%%   <li>elimfld (In, RecName, RecFldName)
%%%       - eliminate record field</li>
%%%   <li>elimdeadcode(In, {use/except,Types})
%%%       - eliminate deadcode</li>
%%%   <li>extfun (In, Range)
%%%       - extract the selected expressions into a function</li>
%%%   <li>expfun (In, Pos)
%%%       - expand implicit funexpression to function call</li>
%%%   <li>genfun (In, Range, NewVarName)
%%%       - generalize function with a new argument</li>
%%%   <li>inlfun (In, Pos)
%%%       - inline function at application</li>
%%%   <li>inlmac (In, Pos)
%%%       - inline macro at use</li>
%%%   <li>intrec (In, Range, NewRec, [RecFldName1, RecFldName2, ...]))
%%%       - introduce record instead of tuple</li>
%%%   <li>merge  (In, Range, NewVarName)
%%%       - merge common subexpressions into a new variable</li>
%%%   <li>movfun (From, ToMod, [@{FunName,Arity@}|_])
%%%       - move function</li>
%%%   <li>movrec (From, To, [RecName|_])
%%%       - move record</li>
%%%   <li>movmac (From, To, [MacName|_])
%%%       - move macro</li>
%%%   <li>reorder(In, @{FunName,Arity@}, [ArgIdx|_])
%%%       - reorder function arguments</li>
%%%   <li>renfld (In, RecName, RecFldName, NewRecFldName)
%%%       - rename record field</li>
%%%   <li>renfun (In, @{FunName,Arity@}, NewFunName)
%%%       - rename function</li>
%%%   <li>renhrl (FromFile, ToFile)
%%%       - rename header file</li>
%%%   <li>renrec (In, RecName, NewRecName)
%%%       - rename record</li>
%%%   <li>renmac (In, MacName, NewMacName)
%%%       - rename macro</li>
%%%   <li>renmod (From, ToMod)
%%%       - rename module</li>
%%%   <li>renuvars (ModName)
%%%       - rename unused variables</li>
%%%   <li>renvar (In, Range, NewVarName)
%%%       - rename variable</li>
%%%   <li>tupfun (In, Range)
%%%       - change function arguments into tuple</li>
%%%   <li>upregex()
%%%       - upgrade regexp from "regexp" module to "re" module usage</li>
%%% </ul>
%%%
%%% == An explanation of argument types ==
%%% <ul>
%%%   <li>filename as string or module name as atom: In, From and To.</li>
%%%   <li>strings: FromFile, ToFile and MacName.</li>
%%%   <li>atoms: ToMod, RecName, RecFldName and FunName.</li>
%%%   <li>integers: Arity and ArgIdx.</li>
%%% </ul>
%%%
%%% @author bkil.hu

% @todo :wishlist factor down to smaller modules
% @todo expand the comments at the top to reflect added features
% @todo generated function and menu help from spec and doc tags
% @todo support for filename suffixes for add/drop
% @todo :wishlist better parameter checking (Name, Pos, ...)!
% @todo :wishlist verbose commenting with edoc tags
% @todo :wishlist spec tags
% @todo :wishlist research alternatives like
%       Beg,End -> line.col:line.col; line.col:col; line:regex ;
% @todo :wishlist function scope regexps
% @todo: better regexp mark handling
%

-module(ri).
-vsn("$Rev: 12950 $ ").

-export([help/0, help/1, h/0, h/1, fun_help/1]).

-export([add/1, add/2,  drop/1, save/1, stop/0, shutdown/0, add_hrl/1,
         generate/1, addQuery/2, 
         add_by_emakefile/1, load_configuration/1, unload_configuration/1,
         ls/0,    ls/1,   reset/0, reset/1, clean/0, cat/1, cat/2, cat/3,
         checkpoint/0, checkpoint/1, backup/0, backup/1,
         ls_backups/0, ls_checkpoints/0,
         backup_info/1, checkpoint_info/1,
         restore/1, undo/0, graph/0, graph/1, graph/2,
         svg/0,   svg/1,  svg/2, gn/2,
         build/0, build/1, transform/2,
         anal_dyn/0, clean_dyn/0, anal_message/0, database_synchronization/0,
         anal_dyn_spec/0]).
-export([envs/0, env/1, addenv/2, setenv/2,
         delenv/1]).

-export([create_graph/1, rename_graph/2, ls_graphs/0, actual_graph/0,
         load_graph/1, delete_graph/1, delete_all_graphs/0]).

-export([create_graph_h/0, rename_graph_h/0, ls_graphs_h/0, actual_graph_h/0,
         load_graph_h/0, delete_graph_h/0, delete_all_graphs_h/0, clean_h/0]).

-export([add_h/0,    drop_h/0,  save_h/0, stop_h/0, shutdown_h/0,
         add_hrl_h/0, add_by_emakefile_h/0, load_configuration_h/0, 
         generate_h/0, addQuery_h/0,
         unload_configuration_h/0,
         ls_h/0,     reset_h/0, cat_h/0,
         backup_h/0, checkpoint_h/0,
         ls_backups_h/0, ls_checkpoints_h/0,
         backup_info_h/0, checkpoint_info_h/0,
         restore_h/0, undo_h/0, graph_h/0,  svg_h/0, gn_h/0,
         build_h/0,  transform_h/0,
         genspec_h/0, anal_dyn_h/0, clean_dyn_h/0, anal_message_h/0,
         database_synchronization_h/0, anal_dyn_spec_h/0]).

-export([envs_h/0, env_h/0, addenv_h/0, setenv_h/0,
         delenv_h/0]).

-export([elimvar/2, elimfun/2, elimmac/2,
         elimfld/3, elimdeadcode/0, elimdeadcode/1,
         elimdeadcode/2, elimrec/2, extfun/3, expfun/2, genfun/3,
         inlfun/2, inlmac/2, intrec/4,
         merge/3,  movfun/3, movrec/3, movmac/3,
         reorder/3,reorder/1,renfld/4, renfun/3, renofuns/3,
         renhrl/2, renrec/3, renmac/3,
         renmod/2, renuvars/1, renvar/3, tupfun/2, upregex/0,
         appfuncl/1, genspec/2]).
-export([q/1, q/2, q/3, q/4, metric/1, cluster/0, cluster/2, load_previous_clustering/0,
         metricmode/1, metricmode/0, get_running_queries/0, kill_query/1]).

-export([lsl/0]).

-export([elimvar_h/0, elimfun_h/0, elimmac_h/0, elimdeadcode_h/0, 
         extfun_h/0, expfun_h/0, genfun_h/0,
         elimrec_h/0, elimfld_h/0,
         inlfun_h/0, inlmac_h/0, intrec_h/0,
         merge_h/0,  movfun_h/0, movrec_h/0, movmac_h/0,
         reorder_h/0,renfld_h/0, renfun_h/0, renofuns_h/0,
         renhrl_h/0, renrec_h/0, renmac_h/0,
         renmod_h/0, renuvars_h/0,
     renvar_h/0, tupfun_h/0, upregex_h/0,
         appfuncl_h/0, metricmode_h/0]).
-export([q_h/0, metric_h/0, cluster_h/0, load_previous_clustering_h/0, 
         kill_query_h/0]).

-export([lsl_h/0]).

-export([start_nitrogen/0, start_nitrogen/1, stop_nitrogen/0]).

-export([start_web2/0, start_web2/1, stop_web2/0]).

-export([check_layered_arch/2, show_layered_arch/2, show_layered_arch/3,
         check_layered_arch_h/0, show_layered_arch_h/0]).

-export([stored_dupcode_results/0, save_dupcode_result/2, save_dupcode_result/3,
         show_dupcode/1, show_dupcode/2, show_dupcode_group/2, 
         show_dupcode_group/3, show_dupcode_h/0, save_dupcode_result_h/0,
         show_dupcode_group_h/0, stored_dupcode_results_h/0]).

-export([clone_identifierl/0, clone_identifierl/1, clone_identifierl_h/0]).
-export([elimdupcode/2, elimdupcode_h/0]).


-export([errors/0, errors/1]).

-export([error_text/2]).
-export([test/0, test/1, test/2, testall/0]).
-export([is_ericsson_mode/0]).
-export([to_pos/2]).
%-export([rgxpos_text/2]). %%DEBUG
%-export([getparen/1]). %%DEBUG

-export([cat_errors/0, cat_errors/1]).

-export([modsave/1, modload/1]).

-export([draw_dep/1, print_dep/1]).
-export([draw_dep_h/0, print_dep_h/0]).

-export([dir_sort/0, dir_sort/1]).
-export([dir_sort_h/0]).

-export([draw_gen_server/2, draw_gen_fsm/2]).

% @private only for internal use!
-export([global_printer_process/0, get_constrained/3, ui_loop/1]).
-export([ui/1]).

-export([db_convert/1]).

-export([current_posmode/0]).
-export([system_info/0, system_info_h/0]).

-export([deadcode_h/0, deadcode_interface_h/0,
         deadcode/0, deadcode/1, deadcode/2, deadcode_interface/3]).

-export([generate_smart_graph_h/0, generate_smart_graph/1]).

-export([draw_dep_graph/1]).

-export([start_wx/0, start_wx/1, start_wx_h/0]).

-export([start_qt/0, start_qt/1, start_qt_h/0]).

-export([generate_all/1]).

-include("ui.hrl").
-include_lib("referl_core/include/core_export.hrl").

-define(DEF_GRAPH_NAME,graph).
-define(PrintProc,referl_ri_global_printer). %@todo supervise

db_convert(ToPos)->
    case ui({transform, database_posmode_convert,[ToPos]}) of
        {ok,{result,[{result,ok},{renamed,[]},{saved,[]}],[]}} ->
            ok;
        {ok,{abort,{_,  ErrorMsg}}} ->
            message(ErrorMsg)
    end.

up_ui(UIResult)->
    case UIResult of
        error ->
            error;
        {ok, Result} ->
            Result;
        _ ->
            UIResult
    end.
    
ui(NameArgs)->
    _PIDPORT = spawn_printer_process(),
    ReqID = ?UI:getid(),
    case ?UI:request(ReqID,NameArgs) of
        ok -> ui_loop(ReqID);
        deny -> message(["Another request is running. Please, try it again later."]),
                error %Denial should be treated as an error.
    end.

ui_loop(ReqID) ->
    receive
        {ReqID, reply, R} ->
            case R of
                {error, {_Code, Msg}} ->
                    message("Error: " ++ Msg),
                    error;
                _ ->
                    R
            end;

        {ReqID, progress, {add, File, 1, Max}} ->
            message2(io_lib:format("loading: ~s (~p forms)~n~4w|",
                   [File, Max, 1])),
            ?MODULE:ui_loop(ReqID);
        {ReqID, progress, {drop, File, 1, Max}} ->
            message2(io_lib:format("dropping: ~s (~p forms)~n~4w|",
                   [File, Max, 1])),
            ?MODULE:ui_loop(ReqID);
        {ReqID, progress, {add, File, Percent, FormCount, FormMax, KBps}} ->
            print_progress(File, Percent, FormCount, FormMax, KBps),
            ?MODULE:ui_loop(ReqID);
        {ReqID, progress, {drop, File, _Percent, FormCount, FormMax, KBps}} ->
            print_progress(File, FormCount/FormMax, FormCount, FormMax, KBps),
            ?MODULE:ui_loop(ReqID);

        {ReqID, question, {QID,Questions}} ->
            Text1 = " (blank to abort).",
            case form_type(Questions) of
                checkbox->
                    Text0 = "Please select some items from the list",
                    io:format("~s~s~n",[Text0,Text1]),
                    ok = answer_box(ReqID, QID, Questions, checkbox);
                radio->
                    Text0 = "Please choose an item from the list",
                    io:format("~s~s~n",[Text0,Text1]),
                    ok = answer_box(ReqID, QID, Questions, radio);
                other->
                    case interactive(Questions) of
                        true ->
                            Text0 = "Please answer the following question",
                            io:format("~s~s~n",[?MISC:plural(Text0,Questions),Text1]);
                        false ->
                            Text0 = "See the direct information feed below:",
                            io:format("~s~n",[Text0])
                        end,
                    ok = answer0(ReqID, QID, Questions)
            end,
            ?MODULE:ui_loop(ReqID);
        _M ->
            %Ui_loop = io_lib:format(" WARNING ri:ui_loop(~p) got:~n ~p ~n",[ReqID,M]),
            %?d(Ui_loop),
            %?d(io_lib:format(" WARNING ri:ui_loop(~p) got:~n ~p ~n",[ReqID,_M])),
            ?MODULE:ui_loop(ReqID)
    end.

print_progress(File, Percent, FormCnt, FormMax, KBps) ->
    PrgMax       = 30,
    Mark         = $\>,
    KBpsTxt      = ?MISC:format("~5.2f kB/s", [0.0 + KBps]), % format/2 requires float
    KBpsLen      = length(KBpsTxt),
    MarkCnt      = round(Percent*PrgMax),
    PrgWidth     = PrgMax - 1 - KBpsLen,
    PrgWidthTxt  = integer_to_list(PrgWidth),
    FormFNameTxt = ?MISC:format("[~4w/~4w] ~s",
                                [FormCnt, FormMax, filename:basename(File)]),
    case PrgWidth < MarkCnt of
        false ->
            MarkCntTxt = integer_to_list(MarkCnt),
            io:format("\r|~-" ++ PrgWidthTxt ++ "." ++ MarkCntTxt ++ "c ~s| ~s",
                      [Mark, KBpsTxt, FormFNameTxt]);
        true ->
            MarkCntTxt = integer_to_list(MarkCnt - 1 - KBpsLen),
            io:format("\r|~s ~-" ++ PrgWidthTxt ++ "." ++ MarkCntTxt ++ "c| ~s",
                      [KBpsTxt, Mark, FormFNameTxt])
    end,
    case Percent == 1 of
        true  -> io:put_chars("\n");
        false -> ok
    end.


interactive(Questions)->
    Formats = lists:usort([ hd(?MISC:pgetu([format],Q)) || Q <- Questions ]),
    not is_subset(Formats,[info]).

form_type(Questions)->
    Formats = lists:usort([ hd(?MISC:pgetu([format],Q)) || Q <- Questions ]),
    case ?MISC:intersect(Formats, [radio, checkbox]) == [] of
        true -> other;
        false ->
            case is_subset(Formats, [info, radio]) of
                true -> radio;
                false ->
                    case is_subset(Formats, [info, checkbox]) of
                        true -> checkbox;
                        false -> other
                    end
            end
    end.

is_subset(Sub,Sup)->
    []==Sub--Sup.

answer0(ReqID, QID, Questions) ->
    answer1(ReqID, QID, [], Questions). %@todo check validators

answer1(ReqID, QID, Answers, _Questions=[Question|Qs]) ->
    [Format,Text] = ?MISC:pgetu([format,text], Question),
    Result =
        case Format of
            textbox ->
                get_constrained(
                  " "++Text++" ",
                  "",
                  fun(X)->{ok, X}end);
            T when T==yesno; T==checkbox -> % TODO: spank interaction developer
                get_constrained(
                  " "++Text++" (y/n) -> ",
                  "Answer either 'y' or 'n'",
                  fun
                      ("y") -> {ok, yes};
                      ("n") -> {ok, no};
                      (_) ->   {error, incorrect}
                  end);
            info ->
                io:format(" ~s~n", [Text]),
                {just, info};
%            RC when RC==radiotodo orelse RC==checkbox -> %@todo
%                _Max = show_boxes(Questions, RC, 1),
%                answer2(ReqID, QID, Answers, Questions, RC, 1);
            _ ->
                Prompt = " Type in the answer for this question" ++
                    " as a raw Erlang term:",
                get_term(io_lib:format("~s~n ~p ~n -> ", [Prompt,Question]))
        end,
    case Result of
        nothing ->
            ?UI:request(ReqID, {cancel, QID});
        {just, Answer} ->
            answer1(ReqID, QID, [Answer|Answers], Qs)
    end;
answer1(ReqID, QID, Answers, []) ->
    ?UI:request(ReqID, {reply, QID, lists:reverse(Answers)}).

answer_box(ReqID, QID, Questions, Type) ->
    Min = 1,
    Idx0 = show_boxes(Questions, Type, Min),
    {Init, Err, Idx} =
        case Type of
            radio ->
                {" type the index of your choice: ",
                 "Answer with an index in range",
                 Idx0};
            checkbox ->
                {" type the indices (space delimited): ",
                 "Answer with indices in range",
                 tl(Idx0)}
        end,
    Result =
        get_constrained(
          Init,
          Err,
          fun
              (S) ->
                  {Split,_} = ?MISC:string_split(S, [" "], -1, false, false),
                  Indices0 = [case string:to_integer(M) of
                                {error,_} -> Min-1;
                                {N,_Rest}-> N
                              end || M <- Split ],
                  Indices = lists:usort(Indices0),
                  case (Type==checkbox) andalso ([0] == Indices) of
                      true ->
                          A2 = calc_answer(Questions, [], Idx),
                          {ok, A2};
                      false ->
                        Inrange =
                            ((Type==checkbox) or (length(Indices)==1)) andalso
                            lists:all(fun(I)-> I>=Min andalso I<Min+length(Idx) end, Indices),
                        case Inrange of
                            true  ->
                                A2 = calc_answer(Questions, Indices, Idx),
                                {ok, A2};
                            false ->
                                {error, incorrect}
                        end
                  end
          end),
    case Result of
        nothing ->
            ?UI:request(ReqID, {cancel, QID});
        {just, Answers} ->
            ?UI:request(ReqID, {reply, QID, Answers})
    end.

calc_answer(Questions, AnswerIndices, QuestionIndices) ->
    calc_answer(Questions, AnswerIndices, QuestionIndices, 1, 1).

calc_answer([], _, _, _, _) ->
    [];
calc_answer([Q|Questions], AnswerIndices, QuestionIndices, QII, AII) ->
    case (QuestionIndices==[]) orelse (QII<hd(QuestionIndices)) of
        true ->
            [hd(?MISC:pgetu([format], Q)) |
             calc_answer(Questions, AnswerIndices, QuestionIndices, QII+1, AII)];
        false->
            case (AnswerIndices==[]) orelse (AII<hd(AnswerIndices)) of
                true->
                    [no |
                     calc_answer(Questions, AnswerIndices, tl(QuestionIndices), QII+1, AII+1)];
                false->
                    [yes |
                     calc_answer(Questions, tl(AnswerIndices), tl(QuestionIndices), QII+1, AII+1)]
            end
    end.

show_boxes([Question|Qs], Type=checkbox, Min) ->
    io:format(" 0. (none)~n", []),
    show_boxes([Question|Qs], Type, Min, Min, [0]);
show_boxes([Question|Qs], Type=radio, Min) ->
    show_boxes([Question|Qs], Type, Min, Min, []).
show_boxes([Question|Qs], Type, Num, Idx, Idxs) ->
    [Format,Text] = ?MISC:pgetu([format,text], Question),
    case Format of
        info ->
            io:format(" ~s~n", [Text]),
            show_boxes(Qs, Type, Num, Idx+1, Idxs);
        Type ->
            io:format(" ~p. ~s~n", [Num,Text]),
            show_boxes(Qs, Type, Num+1, Idx+1, [Idx | Idxs])
    end;
show_boxes([], _Type, _Num, _Idx, Idxs)->
    lists:reverse(Idxs).


last_chars(String, N) ->
    K=string:len(String),
    case (0=<N andalso N=<K) of
        true ->
            string:substr(String, K-N+1);
        false ->
            String
    end.

get_term(Prompt) ->
    get_constrained(Prompt,
                    "You have given an invalid term.",
                    fun(Pruned)->
                            String = case last_chars(Pruned,1) of
                                         [$.] -> Pruned;
                                         _    -> Pruned ++ "."
                                     end,
                            ?MISC:string_to_term(String)
                    end).

get_constrained(Prompt, Error, Process) ->
    Raw = io:get_line(Prompt),
    Pruned = ?MISC:strip(Raw),
    case Pruned of
        "" ->
            nothing;
        _ ->
            case Process(Pruned) of
                {ok, Result} ->
                    {just, Result};
                _ ->
                    io:format("~s~n",[Error]),
                    ?MODULE:get_constrained(Prompt, Error, Process)
            end
    end.

spawn_printer_process()->
    case whereis(?PrintProc) of
        undefined ->
            PID = spawn_link(fun global_printer_init/0),
            register(?PrintProc,PID),
            referl_ui_evsend:start(PID);
        PID ->
            PID
    end.

global_printer_init()->
%   ?d(spawn_global_printer), TODO
   receive
      installed ->
%         ?d(connected_global_printer), TODO
         global_printer_process()
   end.

global_printer_process()->
    receive
        {global, statusinfo, [{change, Changes}]} ->
            [case ?MISC:pget([rename,content,present,error],Prop) of
                [[New],_,_,_]->
                    message("renamed "++File++" to "++New);
                [_,_,_,[Err]] when Err =/= [] ->
                    message("error in "++File);
                [_,[true],_,_]->
                    message("modified "++File);
                [_,_,[true],_]->
                    ok;
                [_,_,_,[[]]] ->
                    ok;
                [_,_,[false],_]->
                    message("dropped "++File);
                 _->
                    ?d({debug, Changes})
             end || {File,Prop} <- Changes],
            ?MODULE:global_printer_process();
        {global, statusinfo, [{difference, Message}]} ->
            message(Message),
            ?MODULE:global_printer_process();
        {global, _, _} ->
            ?MODULE:global_printer_process();
        M ->
            Msg = "terminating due to unexpected message:",
            io:format(" ERROR: ~p ~s~n ~p~n",[?PrintProc,Msg,M]),
            error
    end.

%% @type mod_file() = atom() | string().
%% @type mod_file_dir_list() = mod_file() | [mod_file_dir_list()].
%% @type eol() = 'cr' | 'lf' | 'crlf' | {eol(), 'eol' | 'noeol'}.
%% @type ri_result() = any(). %% TODO


%% @private
message(Data) ->
    io:format("~ts~n", [Data]).

%% @private
message2(Data) ->
    io:format("~ts", [Data]).

message3({ok, Data}) ->
    message3(Data);

message3(Data) ->
    Data.

%% @private
error_text(not_ready,[File]) ->
    ["File ", File, " is not ready for refactoring"];
error_text(invalid_regexp,[Rgx]) ->
    ["The regular expression ", io_lib:print(Rgx), " is invalid"];
error_text(invalid_pos,[Pos]) ->
    ["The position ", io_lib:print(Pos), " is invalid"];
error_text(unmatched_regexp,[Rgx]) ->
    ["The regular expression ", Rgx, " yielded no result"];
error_text(unbound_idx_regexp,[Rgx,Idx]) ->
    ["The index ",integer_to_list(Idx),
     " is invalid for the regular expression ", Rgx,
     " in this file"];
error_text(no_file,[]) ->
    ["No file specified"];
error_text(internal_unexpected,[X]) ->
    ["Unexpected return value ",
     io_lib:print(X)];
error_text(load_beam, [String]) ->
    ["BEAM loading failure: ", String];

error_text(ErrType, ErrParams) ->
    ["Unknown error: {",
     io_lib:print(ErrType), ", ", io_lib:print(ErrParams), "}"].

%% @doc Gives a brief help about the given function, if it exists.
fun_help(Fun) when is_atom(Fun)->
    fun_help(atom_to_list(Fun));

fun_help(Fun=[C|_]) when is_integer(C)->
    FunH = {list_to_atom(Fun++"_h"), 0},
    RIExportedFuns = proplists:get_value(exports, ?MODULE:module_info()),
    case lists:member(FunH, RIExportedFuns) of
                true ->try
                           apply(?MODULE, element(1, FunH), [])
                       catch
                           _:_ -> message("An unknown error occured.")
                       end;
                false-> message("Helper function for '"++Fun++"' does not exists.")
    end;

fun_help(_)->
    message("Usage:\n" 
           ++ io_lib:write(?MODULE) ++ ":fun_help(FunctionName::string())").

%% @doc Shows brief help text
help() ->
    h().

help(Topic) when is_atom(Topic) ->
    h(Topic).

%% @doc Shows brief help text
h() ->
  message2([
    "The following help topics are available:\n"
    " server   - server management\n"
    " renmov   - rename and move refactorings\n"
    " refac    - other refactorings\n"
    " allrefac - all refactorings\n"
    " regexp   - regexp syntax information\n"
    " usage    - a few words about basic usage\n"
    "\n"
    "call " ++ io_lib:write(?MODULE) ++ ":h(Item) for"
    " a brief description of one of the items above.\n"
    "You can also give one of the exported functions to get help on that.\n"
  ]).

%% @doc Shows brief help text on a topic or function
h(all) ->
    h(),
    lists:foreach(fun h/1, [usage,refac,server,regexp]);
h(Topic) when is_atom(Topic) ->
    try
        message2(ht(Topic))
    catch
        error:function_clause ->
            try
                Name = list_to_atom(atom_to_list(Topic)++"_h"),
                apply(?MODULE,Name,[])
            catch
                error:undef ->
                    message("The given help topic cannot be found!\n"),
                    h()
            end
    end;
h(_) ->
    message("Invalid argument!\n"),
    h().

ht(usage) ->
  [ "You first need to add some files to the database with"
    " the add/1 command. Then you can commit transformations on them,"
    " by referring to them by either module name or filename.\n" ];

ht(renmov) ->
  [ "movfun(from_mod,to_mod,[{f,2},{g,0}])"
    " - move functions to another module\n"
    "movrec(from_mod,to_mod,[rec1,rec2])"
    " - move records to another module\n"
    "movmac(from_mod,to_mod,[\"Mac1\",\"Mac2\"])"
    " - move macro to another module\n"
    "renfld(mod_or_file,rec,oldfield1,newfield1)"
    " - rename record field\n"
    "renfun(mod_or_file,{func,2},newfun)"
    " - rename function\n"
    "renrec(mod_or_file,oldrec,newrec)"
    " - rename record\n"
    "renmac(mod_or_file,\"OldMac\",\"NewMac\")"
    " - rename macro\n"
    "renmod(mod_or_file, newmod)"
    " - rename module\n"
    "renhrl(\"old.hrl\", \"new.hrl\")"
    " - rename header file\n"
    "renvar(mod_or_file, \"X=\", \"NewVar\")"
    " - rename variable\n"
  ];

ht(refac) ->
  [
    "elimvar(mod_or_file,\"X=\")"
    " - eliminate variable\n"
    "elimdeadcode()"
    " - eliminate deadcode\n"
    "elimfun(mod_or_file, {f,2})"
    " - eliminate unused function\n"
    "elimmac(mod_or_file, \"Macro\")"
    " - eliminate unused macro\n"
    "elimrec(mod_or_file, \"Record\")"
    " - eliminate unused record\n"
    "elimfld(mod_or_file, \"Record\", \"RecordField\")"
    " - eliminate unused recordfield\n"
    "extfun(mod_or_file,\"A+B\",newfunc)"
    " - extract function\n"
    "expfun(mod_or_file, \"fun g\")"
    " - expand implicit funexpression to function call\n"
    "genfun(mod_or_file, \"[+]<2\", \"NewArg\")"
    " - generalize function with new argument\n"
    "inlfun(mod_or_file,\"f\\(1\")"
    " - inline function at application\n"
    "inlmac(mod_or_file,\"?Mac\")"
    " - inline macro at use\n"
    "intrec(mod_or_file, \"{X,Y}\", newrec, [f1, f2])"
    " - introduce record in place of tuple\n"
    "merge(mod_or_file,\"1+2\",\"NewVar\")"
    " - merge common subexpressions into a new variable\n"
    "reorder(mod_or_file,{func,2},[2,1])"
    " - reorder function arguments\n"
    "tupfun(mod_or_file,\"A,B\")"
    " - change function arguments into tuple\n"
    "upregex()"
    " - upgrade regexp from \"regexp\" module to \"re\" module usage\n"
  ];

ht(allrefac) ->
    ht(refac) ++ ht(renmov);

ht(server) ->
  [ "add(X)   - add a module, file, directory or a list of these"
    " to the database\n"
    "drop(X)  - drop (remove) like in add/1\n"
    "save(X)  - save from the database to physical file(s)\n"
    "ls()     - list database contents\n"
    "ls(FM)   - list forms in the file or module\n"
    "cat(FM)  - prints the contents of a file or module from the database\n"
    "cat(FM,RM)  - prints the definition of a macro or record\n"
    "cat(FM,F,A) - prints the definition of a function\n"
    "backup() - creates a backup from the current state of the graph\n"
    "backup(CommitLog) - creates a backup as ri:backup/0,\n"
    "but here the user can attach a commit log to the backup file\n"
    "ls_backups() - returns a lists of backups, that has been created before with\n"
    "ri:backup/0 or ri:backup/1\n"
    "backup_info(Backup) - returns information about the given backup\n"
    "restore(Backup) - restores the given backup\n"
    "undo()   - undo the transformation (rollback, only once!)\n"
    "clean() - removes all backups that belongs to the actual graph\n"
    "reset()  - reset the database to an empty state, but valid schema\n"
    "graph(T) - assume no options and call graph/2\n"
    "graph(Atom,Opt) - assume \".dot\" extension and call graph/2\n"
    "graph(File,Opt) - draw the database graph with the given options\n"
    "svg/0,1,2 - calls graph and generates SVG output\n"
    "create_graph(Name) - creates a graph with the given name\n"
    "rename_graph(OldName, NewName) - renames a graph that has the given OldName,\n"
    "with the given NewName\n"
    "ls_graphs() - returns a list of the created graphs\n"
    "actual_graph() - returns the actual graph's name\n"
    "load_graph(Name) - loads the given graph\n"
    "delete_graph(Name) - removes the given graph\n"
    "delete_all_graphs() - removes all graphs\n"
    "gn(Type,Idx) - returns the data of a graph node\n"
  ];

ht(regexp) ->
  [ "You can substitute regexp in place "
    "of positions and posranges like so:\n"
    "* a plain regexp matches precisely a posrange or "
    "marks the start of a position;\n"
    "* use plain angle brackets '<' and '>' to highlight"
    " part of a regexp;\n"
    "* to get angle bracket characters instead,"
    " escape them like \\< and \\>;\n"
    "* the nth match can be selected instead of the first one "
    "by substituting the tuple {\"regexp\",index1} for \"regexp\".\n"
  ].

%%% ============================================================================
%%% Refactorings

extfun_h() ->
    message("Extracts the selected expressions into a function\n"
            "extfun(ModFile,Range_of_body,NewFunc)").

%% @doc Extract function refactoring
extfun(File, Range, Name)
  when is_atom(Name), (is_atom(File) or is_list(File)),
        (is_list(Range) or is_tuple(Range)) ->
    transformr(extract_fun, File, Range, [{name, Name}]);
extfun(_,_,_)->
    message("::(modfile(), regexp() | {regexp(),index()} |{offset(),offset()}, atom())"),
    usage.
% ri:extfun(mod_or_file,"A+B",f).


merge_h() ->
    message("Merges common subexpressions into a new variable \n"
            "merge(ModFile,Range_of_expression,NewVar)").

%% @doc Merge common subexpressions refactoring
merge(File, Range, Varname=[C|_]) when is_integer(C),
        (is_atom(File) or is_list(File)),
        (is_list(Range) or is_tuple(Range)) ->
    transformr(merge, File, Range, [{varname, Varname}]);
merge(_,_,_)->
    message("::(modfile(), regexp() | {regexp(),index()} |{offset(),offset()}, string())"),
    usage.

% ri:merge(mod_or_file,"1+2","V").

inlfun_h() ->
    message("Inline function refactoring\n"
            "inlfun(ModFile,Pos_of_fun_application)").

%% @doc Inline function refactoring
inlfun(File, Pos) when (is_atom(File) or is_list(File)),
        (is_list(Pos) or is_tuple(Pos)) ->
    transformp(inline_fun, File, Pos, []);
inlfun(_,_)->
    message("::(modfile(), regexp() | {regexp(),index()})"),
    usage.

% ri:inlfun(mod_or_file,"f[(]1").

inlmac_h() ->
    message("Inline macro refactoring\n"
            "inlmac(ModFile,Pos_of_macro_use)").

%% @doc Inline macro refactoring
inlmac(File, Pos) when (is_atom(File) or is_list(File)),
        (is_list(Pos) or is_tuple(Pos)) ->
    transformp(inline_mac, File, Pos, []);
inlmac(_,_)->
    message("::(modfile(), regexp() | {regexp(),index()})"),
    usage.
% ri:inlmac(mod_or_file,"?Mac").

tupfun_h() ->
    message("Tuple function arguments\n" ++
       "tupfun(ModFile, Range_of_arguments) where\n" ++
       "ModFile :: atom() | string()\n" ++
       "Range_of_arguments :: regexp() | {regexp(), offset()} | {offset(), offset()}").

%% @doc Tuple function arguments refactoring
tupfun(File, Range) when (is_atom(File) or is_list(File)),
        (is_list(Range) or is_tuple(Range)) ->
    transformr(tuple_funpar, File, Range, []);
tupfun(_,_)->
    message("::(modfile(), regexp() | {regexp(),index()} |{offset(),offset()})"),
    usage.
%%@todo :wishlist Begin,End -> name/arity first last
% ri:tupfun(mod_or_file,"A,B").

%%Not recommended
%reorder(File, Fun, Arity, Order) ->
%    reorder(File, {Fun,Arity}, Order).

reorder_h() ->
    message(
        "reorder(ModFile,{Fun,Arity},PermutationList)\n"
        "Reorder function parameters\n"
        "reorder(ModFile, {Fun, Arity}, PermutationList) or\n"
        "reorder(ModFile) where\n"
        "ModFile :: atom() | string()\n"
        "Fun :: atom()\n"
        "Arity :: integer()\n"
        "PermutationList :: [integer()]\n").

%@todo :wishlist check Order

%% @doc Reorder function arguments refactoring
reorder(File, {Fun,Arity}, Order=[I|_])
  when (is_atom(File) or is_list(File)), is_atom(Fun), is_integer(Arity), is_integer(I) ->
    transform_catch(reorder_funpar, File,
              [{function, Fun}, {arity, Arity}, {order, Order}]);
reorder(_,_,_)->
    message("::(modfile(), {atom(), natural()}, [positive()])"),
    usage.
% ri:reorder(mod_or_file,{f,2},[2,1]).

reorder(File)
  when (is_atom(File) or is_list(File)) ->
    transform_catch(reorder_funpar, File, []);
reorder(_)->
    message("::(modfile())"),
    usage.
% ri:reorder(mod_or_file).




genspec_h() ->
    message("Generate function specification. Usage:\n"
            "genspec(ModFile, {Fun, Arity}) where\n"
            "ModFile :: atom() | string()\n"
            "Fun :: atom()\n"
            "Arity :: integer()").

%% @doc Generate function specification refactoring
genspec(File, {Fun,Arity})
  when (is_atom(File) or is_list(File)), is_atom(Fun), is_integer(Arity) ->
    transform_catch(genspec, File,
              [{function, Fun}, {arity, Arity}]);
genspec(_,_)->
    message("::(modfile(), {atom(), natural()})"),
    usage.






expfun_h() ->
    message("expfun(ModFile,Pos_of_funexpr)").

%% @doc Expand implicit fun expression refactoring
expfun(File, Pos) when (is_atom(File) or is_list(File)),
        (is_list(Pos) or is_tuple(Pos)) ->
    transformp(expand_funexpr, File, Pos, []);
expfun(_,_)->
    message("::(modfile(), regexp() | {regexp(),index()})"),
    usage.
% ri:expfun(mod_or_file, "fun g").

movfun_h() ->
    message("movfun(Source,Target,FunctionList=[{FunctionName,Arity}|_])\n"
        "Moves functions to another module\n"
        "Source :: atom() | string() \n"
        "Target :: atom() | string() \n"
        "FunctionName :: atom() \n"
        "Arity :: integer()").


%% @doc Move function refactoring
movfun(Source, Target, Fun={A,I})
  when is_atom(A), is_integer(I) ->
    movfun(Source, Target, [Fun]);
% ri:movfun(mod_or_file,b,{f,2}).

%%@todo :wishlist check Funlist
movfun(Source, Target, Funlist=[{A,I}|_]) when
        (is_atom(Source) or is_list(Source)),
        (is_atom(Target) or is_list(Target)),
        is_atom(A), is_integer(I) ->
    transform2(move_fun, Source, Target, [{funlist, Funlist}]);
movfun(_,_,_)->
    message("::(modfile(), modfile(), [{atom(), natural()}])"),
    usage.
% ri:movfun(mod_or_file,b,[{f,2},{g,0}]).

movrec_h() ->
    message("movrec(Source,Target,RecordList=[Record|_])\n"
        "Moves records to another module\n"
        "Source :: atom() | string()\n"
        "Target :: atom() | string()\n"
        "Record :: atom()").

%% @doc Move record refactoring
movrec(Source, Target, Rec) when
        is_atom(Rec) ->
    movrec(Source, Target, [Rec]);
% ri:movrec(mod_or_file,b,rec).

%%@todo :wishlist check Reclist
movrec(Source, Target, Reclist=[A|_]) when
        (is_atom(Source) or is_list(Source)),
        (is_atom(Target) or is_list(Target)),
        is_atom(A) ->
    transform2(move_rec, Source, Target, [{reclist, Reclist}]);
movrec(_,_,_)->
    message("::(modfile(), modfile(), [atom()])"),
    usage.
% ri:movrec(mod_or_file,b,[r1,r2]).

movmac_h() ->
    message("movmac(Source,Target,MacroList=[Macro|_])\n"
        "Moves a macro to another module use\n"
        "Source :: atom() | string()\n"
        "Target :: atom() | string()\n"
        "Macro :: string()").

%% @doc Move macro refactoring
movmac(Source, Target, Mac=[C|_]) when
        is_integer(C) ->
    movmac(Source, Target, [Mac]);
% ri:movmac(mod_or_file,b,"Mac").

%%@todo :wishlist check Maclist
movmac(Source, Target, Maclist=[[C|_]|_]) when
        (is_atom(Source) or is_list(Source)),
        (is_atom(Target) or is_list(Target)),
        is_integer(C) ->
    transform2(move_mac, Source, Target, [{maclist, Maclist}]);
movmac(_,_,_)->
    message("::(modfile(), modfile(), [string()])"),
    usage.
% ri:movmac(mod_or_file,b,["Mac1","Mac2"]).

genfun_h() ->
    message("genfun(ModFile,Range_of_body,NewVar)\n"
        "Generalises a function with a new argument\n").

%% @doc Generalize function by new argument refactoring
genfun(File, Range, Newname=[C|_]) when
        (is_atom(File) or is_list(File)),
        (is_list(Range) or is_tuple(Range)),
        is_integer(C) ->
    transformr(gen, File, Range, [{varname, Newname}]);
genfun(_,_,_)->
    message("::(modfile(), regexp() | {regexp(),index()} |{offset(),offset()}, string())"),
    usage.

% ri:genfun(mod_or_file, "[+]\\(2", "Two").

%%Not recommended
%renfun(File, Fun, Arity, Newname) ->
%    renfun(File, {Fun,Arity}, Newname).

renfun_h() ->
    message("Rename function. Usage:\n"
        "renfun(ModFile, {FunctionName, Arity}, NewFun) where\n"
        "ModFile :: atom() | string()\n"
        "FunctionName :: atom()\n"
        "Arity :: integer()\n"
        "NewFun :: atom()").

%% @doc Rename function refactoring
renfun(File, {Fun,Arity}, Newname) when
        (is_atom(File) or is_list(File)),
        is_atom(Fun), is_integer(Arity), is_atom(Newname) ->
    transform_catch(rename_fun, File,
              [{function, Fun}, {arity, Arity}, {name, Newname}]);
renfun(_,_,_)->
    message("::(modfile(), {atom(), natural()}, atom())"),
    usage.


% todo Uncomment the functions when they are ready.
% todo Do not use (half-)Hungarian names.
% rec_syner_h() ->
%     message("rec_syner(ModFile, [Records])").
%
% %% @doc Change the record syntax from tuple syntax to record syntax.
% rec_syner(File, RecordsList) when
%       (is_atom(File) or is_list(File)),
%       is_list(RecordsList) ->
%     transform_catch(recordos, File,  [{reclist, RecordsList}]).

renvar_h() ->
    message("Rename variable\n"
        "renvar(ModFile, Pos_of_variable, NewVar) where\n"
        "ModFile :: atom() | string()\n"
        "Pos_of_variable :: string() | {string(), integer()}\n"
        "NewVar :: string()\n\n"
        "Example: renvar(mod_or_file, \"X=\", \"V\")").

%% @doc Rename variable refactoring
renvar(File, Pos, Newname=[C|_]) when
        (is_atom(File) or is_list(File)),
        (is_list(Pos) or is_tuple(Pos)),
        is_integer(C) ->
    transformp(rename_var, File, Pos, [{varname, Newname}]);
renvar(_,_,_)->
    message("::(modfile(), regexp() | {regexp(),index()}, string())"),
    usage.

% ri:renvar(mod_or_file, "X=", "V").

renuvars_h() ->
    message("Rename unused variables\n"
       "renuvars(ModFile) where\n"
       "ModFile :: atom() | string()").

%% @doc Rename unused variables refactoring
renuvars(Module) when
    (is_atom(Module) or is_list(Module))
    -> transform_catch(rename_unused_vars,Module, []);
renuvars(_)  ->
    message("::(modfile())"),
    usage.

%% @doc Rename overloaded functions refactoring
renofuns_h() ->
    message("Rename overloaded functions\n"
        "renofuns(ModFile, OldName, NewName) where\n"
        "ModFile :: atom() | string()\n"
        "Oldname :: atom()\n"
        "Newname :: atom()").

renofuns(File, Oldname, Newname) when
        (is_atom(File) or is_list(File)),
        is_atom(Oldname),
    is_atom(Newname) ->
        transform_catch(rename_overloaded_funs, File,
              [{funname, Oldname}, {name, Newname}]);
%%      is_tuple(Oldname) ->
%%        (Atom, Pos) = Oldname,
%%        transform_catch(rename_overloaded_funs, File,
%%            [{Atom, Oldname}, {name, Newname}])
%%    end.
renofuns(_,_,_) ->
    message("::(modfile(), atom(), atom())"),
    usage.

renmod_h() ->
    message("Rename module\n"
      "renmod(OldModFile, NewMod)\n"
      "OldModFile :: atom() | string()\n"
      "NewMod :: atom()").

%% @doc Rename module refactoring
renmod(File, Newname) when
        (is_atom(File) or is_list(File)),
        is_atom(Newname) ->
    transform_catch(rename_mod, File, [{name, Newname}]);
renmod(_,_)->
    message("::(modfile(), atom())"),
    usage.

% ri:renmod(mod_or_file, newmod).

renhrl_h() ->
    message("Rename header\n"
      "renhrl(OldHrl, NewHrl)\n"
      "OldHrl :: atom() | string()\n"
      "NewName :: string()").

%% @doc Rename header refactoring
renhrl(File, Newname=[C|_]) when
        (is_atom(File) or is_list(File)), % note: can't be an atom...
        is_integer(C) ->
    transform_catch(rename_header, File, [{name, Newname}]);
renhrl(_,_)->
    message("::(modfile(), string())"),
    usage.

% ri:renhrl("a.hrl", "b.hrl").

renrec_h() ->
    message("Rename record\n"
       "renrec(ModFile, OldRecord, NewRecord) where\n"
       "ModFile :: atom() | string()\n"
       "OldRecord :: atom()\n"
       "NewRecord :: atom()").

%% @doc Rename record refactoring
renrec(File,Record,NewName) when
        (is_atom(File) or is_list(File)),
        is_atom(Record), is_atom(NewName) ->
    transform_catch(rename_rec, File,
              [{record, Record}, {name, NewName}]);
renrec(_,_,_)->
    message("::(modfile(), atom(), atom())"),
    usage.


% ri:renrec(mod_or_file,recname,newrecname).

renfld_h() ->
    message("Rename record field"
        "renfld(ModFile, Record, OldField, NewField) where"
        "ModFile :: atom() | string()"
        "Record :: atom()"
        "OldField :: atom()"
        "NewField :: atom()").

%% @doc Rename record field refactoring
renfld(File,Record,Field,NewName) when
        (is_atom(File) or is_list(File)),
        is_atom(Record), is_atom(Field), is_atom(NewName) ->
    transform_catch(rename_recfield, File,
              [{record,Record}, {recfield,Field}, {name,NewName}]);
renfld(_,_,_,_)->
    message("::(modfile(), atom(), atom(), atom())"),
    usage.


% ri:renfld(mod_or_file,recname,field1,newfield1).

elimvar_h() ->
    message("elimvar(ModFile,Pos_of_variable)\n"
        "Eliminates a variable\n"
        "ModFile :: atom() | string() \n"
        "Pos_of_variable :: regexp() | {regexp(),index()}").

%% @doc Eliminate variable by inlining refactoring
elimvar(File, Pos) when
        (is_atom(File) or is_list(File)),
        (is_list(Pos) or is_tuple(Pos)) ->
    transformp(elim_var, File, Pos, []);
elimvar(_,_)->
    message("::(modfile(), regexp() | {regexp(),index()})"),
    usage.


% ri:elimvar(mod_or_file,"X=").

elimdeadcode_h() ->
    message("Eliminate dead code \n"
        "elimdeadcode() eliminates all deadcodes in the database\n"
        "elimdeadcode({use/except,Types}) eliminates selected deadcodes in the database\n"
        "elimdeadcode(ModFile) eliminates all deadcodes in the selected module\n"
        "elimdeadcode(ModFile,{use/except,Types}) eliminates selected deadcodes in the selected module\n"
        "ModFile :: atom() | string()\n"
        "Types :: list()\n"
        "Types : [func, guard, pattern, var, rec, recfield, mac, expr]").

%% @doc Eliminate deadcode
elimdeadcode() ->
    AllTypes=[func, guard, pattern, var, rec, recfield, mac, expr],
    transform(deadcode_elim, [{types,AllTypes}]).

elimdeadcode(Module) when
    (is_atom(Module) or is_list(Module)) ->
    AllTypes=[func, guard, pattern, var, rec, recfield, mac, expr],
    transform_catch(deadcode_elim, Module, [{types,AllTypes}]);

elimdeadcode({use,Types}) when
    is_list(Types) ->
    transform(deadcode_elim, [{types,Types}]);

elimdeadcode({except,Types}) when
    is_list(Types) ->
    AllTypes=[func, guard, pattern, var, rec, recfield, mac, expr],
    transform(deadcode_elim, [{types,AllTypes--Types}]);

elimdeadcode(_) ->
    message("::(modfile() | {use/except,list()})"),
    usage.

elimdeadcode(Module,{use,Types}) when
    (is_atom(Module) or is_list(Module)) and is_list(Types) ->
    transform_catch(deadcode_elim, Module, [{types,Types}]);

elimdeadcode(Module,{except,Types}) when
    (is_atom(Module) or is_list(Module)) and is_list(Types) ->
    AllTypes=[func, guard, pattern, var, rec, recfield, mac, expr],
    transform_catch(deadcode_elim, Module, [{types,AllTypes--Types}]);

elimdeadcode(_,_) ->
    message("::(modfile(),{use/except,list()})"),
    usage.

%% @doc Eliminate unused function
elimfun_h() ->
    message("elimfun(Source,Function={FunctionName,Arity})\n"
        "Eliminate unused function\n"
        "Source :: atom() | string() \n"
        "FunctionName :: atom() \n"
        "Arity :: integer()").

elimfun(Source, {Fun,Arity}) when
        (is_atom(Source) or is_list(Source)),
        is_atom(Fun), is_integer(Arity) ->
        transform_catch(elim_fun, Source,
              [{function, Fun}, {arity, Arity}]);
elimfun(_,_)->
    message("::(modfile(), {atom(), natural()})"),
    usage.

elimmac_h() ->
    message("Eliminate macro\n"
       "elimmac(ModFile, Macro) where\n"
       "ModFile :: atom() | string()\n"
       "Macro :: atom() | string()").


%% @doc Eliminate unused macro refactoring
elimmac(File, Macro) when
        (is_atom(File) or is_list(File)),
        (is_atom(Macro) or is_list(Macro)) ->
    transform_catch(elim_macro, File, [{macro,Macro}]);
elimmac(_,_)->
    message("::(modfile(), atom() | string(), atom())"),
    usage.

elimrec_h() ->
    message("Eliminate record\n"
       "elimrec(ModFile, Record) where\n"
       "ModFile :: atom() | string()\n"
       "Record :: atom() | string()").


%% @doc Eliminate unused macro refactoring
elimrec(File, Record) when
        (is_atom(File) or is_list(File)),
        (is_atom(Record) or is_list(Record)) ->
    transform_catch(elim_record, File, [{record,Record}]);
elimrec(_,_)->
    message("::(modfile(), atom() | string(), atom())"),
    usage.

elimfld_h() ->
    message("Eliminate unused record field\n"
        "elimfld(ModFile, Record, RecField) where\n"
        "ModFile :: atom() | string()\n"
        "Record :: atom()\n"
        "RecField :: atom()").

%% @doc Eliminate unused record field refactoring
elimfld(File,Record,Field) when
        (is_atom(File) or is_list(File)),
        is_atom(Record), is_atom(Field) ->
    transform_catch(elim_recordfield, File,
              [{record,Record}, {recfield,Field}]);
elimfld(_,_,_)->
    message("::(modfile(), atom(), atom())"),
    usage.

%%Not recommended
%intrec(File, A, B, Newname, Fields) ->
%    intrec(File, {A,B}, Newname, Fields).

intrec_h() ->
    message("intrec(ModFile,Range_of_tuple,NewRecord,Fields=[field|_])\n"
            "Introduces a record by replacing a tuple\n").

%% @doc Introduce record in place of a tuple refactoring
intrec(File, URange, Newname, AFields=[A|_])
  when is_atom(A) ->
    SList = lists:map(fun atom_to_list/1, AFields),
    Fields = string:join(SList," "),
    intrec(File, URange, Newname, Fields);
% ri:intrec(mod_or_file, "{X,Y}", newrec, [f1, f2]).

intrec(File, Range, Newname, Fields=[C|_]) when
        (is_atom(File) or is_list(File)),
        (is_list(Range) or is_tuple(Range)),
        is_atom(Newname), is_integer(C) ->
    transformr(introduce_rec, File, Range,
               [{name,Newname}, {text,Fields}]);
intrec(_,_,_,_)->
    message("::(modfile(), regexp() | {regexp(),index()} |{offset(),offset()}, atom(), [atom()])"),
    usage.


% ri:intrec(mod_or_file, "{X,Y}", rec, "f1 f2"]).

renmac_h() ->
    message("Rename macro\n"
       "renmac(ModFile, OldMacro, NewMacro) where\n"
       "ModFile :: atom() | string()\n"
       "OldMacro :: atom() | string()\n"
       "NewMacro :: atom() | string()").


%% @doc Rename macro refactoring
renmac(File, Macro, Newname) when
        (is_atom(File) or is_list(File)),
        (is_atom(Macro) or is_list(Macro)),
        (is_atom(Newname) or is_list(Newname)) ->
    transform_catch(rename_mac, File, [{macro,Macro}, {macname,Newname}]);
renmac(_,_,_)->
    message("::(modfile(), atom() | string(), atom() | string())"),
    usage.
% ri:renmac(mod_or_file,"Macname","NewMecname").

%intimp(File) ->
%    transform_catch(introduce_import, File, []).

upregex_h() ->
    message("Upgrade regular expression syntax\n"
       "upregex()").

%% @doc Upgrade regular expression syntax refactoring
upregex() ->
    catch_referr(fun()->
        transform(upgrade_regexp,[])
    end).

appfuncl_h()->
    message("Apply function clustering results. Usage:\n"
       "appfuncl(Clusters)").

%% @doc Apply function clustering results refactoring
appfuncl(Clusters)->
    catch_referr(fun()->
                         transform(apply_funcluster,
                                   [{funclusters,Clusters}])
                 end).

metricmode_h()->
    message("metricmode(on), metricmode(off), metricmode(show)").

%% @doc Set the metric mode on/of/shows bad smells
metricmode()-> up_ui(ui({metric_mode})).

metricmode(on) -> up_ui(ui({metric_mode, true, ri}));

metricmode(off) -> up_ui(ui({metric_mode, false}));

metricmode(show) -> ui({metric_mode, show, ri}),
                    ok;

metricmode(_)->
    message("::on | off | show"),
    usage.

metric_h()->
    message("Run a metric query. Usage:\n"
       "metric(Query) where\n"
       "Query :: atom() | string()").


%% @doc Run a metric query
metric(Query) when is_atom(Query)->
    metric(atom_to_list(Query));
metric(Query) when is_list(Query)->
    transform_(metric_query,
               [{querys,Query}],
               fun(Results)->
                   [Result] = ?MISC:pgetu([result],Results),
                   message(Result),
                   ok
               end);
metric(_)->
    message("::(atom() | string())"),
    usage.

cluster_h()->
    message("Clustering. Usage:\n"
       "cluster()\n"
       "cluster(Algorithm,EntityType)").
             
load_previous_clustering_h() ->
    message("Load previous clustering result. Usage:\n"
       "load_previous_clustering()").

cluster()->
    cluster_([]).

cluster(Algorithm,EntityType)->
    cluster_([{algorithm,Algorithm},{entity,EntityType}]).

cluster_(Args)->
    transform_(clustering,
               Args,
               fun(_)->result end).
               
load_previous_clustering() ->
   message([?MISC:any_to_string(up_ui(ui({cl_prev_clustering})))]).

q_h()->
    message2(["q(SemanticQuery)\n"
              "q(ModFile,SemanticQuery)\n"
              "q(ModFile,PositionRegexp,SemanticQuery)\n"
              "q(SemanticQuery,Options)\n"
              "q(ModFile,SemanticQuery,Options)\n"
              "q(ModFile,PositionRegexp,SemanticQuery,Options)\n"
              "Example usage:\n"
              " ri:q(\"mods.funs.name\").\n"
              " ri:q(mod1, \"f\\\\(X, Y\\\\)\", \"@fun.var\").\n"
              " ri:q(\"mods.funs.vars.bindings\",[{groupby,1}]). \n"
              " ri:q(\"mods.funs\",[linenum,{out,\"result.txt\"}]).\n"]).

%% @doc Run a semantic query
q(Q) when is_atom(Q)->
    q(atom_to_list(Q));
q(Q=[C|_]) when is_integer(C)->
    q_(Q, [], addqnone());
q(_)->
    message("::(atom() | string())"),
    usage.

%% @doc Run a semantic query starting from the given file
q(ModFile,Q) when (is_atom(ModFile) or is_list(ModFile)), is_atom(Q)->
    q(ModFile,atom_to_list(Q));
q(ModFile,Q=[C|_]) when (is_atom(ModFile) or is_list(ModFile)), is_integer(C)->
    q_(Q,[],addqmod(ModFile));

q(Q, Options=[O|_]) when is_atom(Q), (is_atom(O) orelse is_tuple(O))->
    q(atom_to_list(Q),Options);
q(Q=[C|_], Options=[O|_]) when is_integer(C), (is_atom(O) orelse is_tuple(O))->
    q_(Q,Options,addqnone());
q(_, _)->
    message("::(modfile() | atom() | string(), atom() | string() | proplist())"),
    usage.

%% @doc Run a semantic query starting from the given position
% q(router, "ex.*\\([ ]*\\)", '@fun.calls').
q(ModFile,Pos=[A|_],Q)
  when (is_atom(ModFile) or is_list(ModFile)), is_integer(A), is_atom(Q)->
    q(ModFile,Pos,atom_to_list(Q));
% q(router, {"ex.*\\([ ]*\\)", 2}, '@fun.calls').
q(ModFile,Pos={[A|_], Index},Q)
  when (is_atom(ModFile) or is_list(ModFile)), is_integer(A), is_atom(Q),
       is_integer(Index), Index >0 ->
    q(ModFile,Pos,atom_to_list(Q));
% q(router, "ex.*\\([ ]*\\)", "@fun.calls").
q(ModFile,Pos=[A|_],Q=[B|_])
  when (is_atom(ModFile) or is_list(ModFile)), is_integer(A), is_integer(B)->
    q_(Q,[],addqmodpos(ModFile,Pos));
% q(router, {"ex.*\\([ ]*\\)", 2}, "@fun.calls").
q(ModFile,Pos={[A|_], Index},Q=[B|_])
  when (is_atom(ModFile) or is_list(ModFile)), is_integer(A), is_integer(B), 
       is_integer(Index), Index > 0 ->
    q_(Q,[],addqmodpos(ModFile,Pos));
% q(*, {atom, *}, *) clause missing

% q(router, '@file.fun', [linenum]).
q(ModFile,Q,Options=[O|_])
  when (is_atom(ModFile) or is_list(ModFile)), is_atom(Q), (is_atom(O) orelse is_tuple(O)) ->
    q(ModFile,atom_to_list(Q),Options);
% q(router, "@file.fun", [linenum]).
q(ModFile,Q=[C|_], Options=[O|_])
  when (is_atom(ModFile) or is_list(ModFile)), is_integer(C), (is_atom(O) orelse is_tuple(O)) ->
    q_(Q,Options,addqmod(ModFile));
q(_,_,_)->
    message("::(modfile(), regexp() | {regexp(),index()} |{position,offset()} | atom() | string(), atom() | string() | proplist())"),
    usage.

%% @doc Run a semantic query starting from the given position with options
% q(router, "ex.*\\([ ]*\\)", '@fun.calls', [linenum]).
q(ModFile,Pos=[A|_],Q,Options=[O|_]) when
        (is_atom(ModFile) or is_list(ModFile)),
        is_integer(A), is_atom(Q),
        (is_atom(O) orelse is_tuple(O)) ->
    q(ModFile,Pos,atom_to_list(Q),Options);
% q(router, "ex.*\\([ ]*\\)", "@fun.calls", [linenum]).
q(ModFile,Pos=[A|_],Q=[QH|_],Options=[O|_]) when
        (is_atom(ModFile) or is_list(ModFile)),
        is_integer(A), is_integer(QH),
        (is_atom(O) orelse is_tuple(O)) ->
    q_(Q,Options,addqmodpos(ModFile,Pos));
% q(router, {"ex.*\\([ ]*\\)",1}, '@fun.calls', [linenum]).
q(ModFile,Pos={[A|_], Index},Q,Options=[O|_]) when
        (is_atom(ModFile) or is_list(ModFile)),
        is_integer(A), is_atom(Q), is_integer(Index), Index > 0,
        (is_atom(O) orelse is_tuple(O)) ->
    q(ModFile,Pos,atom_to_list(Q),Options);
% q(router, {"ex.*\\([ ]*\\)",1}, "@fun.calls", [linenum]).
q(ModFile,Pos={[A|_], Index},Q=[B|_], Options=[O|_])
  when (is_atom(ModFile) or is_list(ModFile)), is_integer(A), is_integer(B),
       is_integer(Index), Index > 0, (is_atom(O) orelse is_tuple(O)) ->
    q_(Q,Options,addqmodpos(ModFile,Pos));
% q(router, {position, 412}, '@fun.calls',[linenum]).
q(ModFile, Pos={position, P}, Q, Options=[O|_])
  when (is_atom(ModFile) or is_list(ModFile)), is_integer(P), is_atom(Q),
       (is_atom(O) orelse is_tuple(O)) ->
    q(ModFile, Pos, atom_to_list(Q), Options);
% q(router, {position, 412}, "@fun.calls",[linenum]).
q(ModFile, _Pos={position, P}, Q=[B|_], Options=[O|_])
  when (is_atom(ModFile) or is_list(ModFile)), is_integer(P), is_integer(B),
       (is_atom(O) orelse is_tuple(O)) ->
    case proplists:get_value(no_check, Options) of
        true ->
            q_(Q, proplists:delete(no_check, Options), 
                fun()->[{file,ModFile},{position,to_pos(ModFile,P)}] end);
        _ ->
            q_(Q,Options,addqmodpos(ModFile,P))
    end;
q(_,_,_,_)->
    message("::(modfile(), regexp() | {regexp(),index()} |{position,offset()}, atom() | string(), proplist())"),
    usage.

get_running_queries()->
    case ui({get_running_queries, default_output}) of
        error ->
            error;
        {ok, []}->
            message("Queries are not running now.");
        {ok, RunningQueries} -> 
            List = 
                [["Running queries\nQuery ID\t\tQuery string\n"],
                 [io_lib:format("~8p\t\t\t~p~n",
                                  [QId,proplists:get_value(querystr,Args)]) || 
                      {QId, _Pid, _Mod, Args} <- RunningQueries]],
            message(List)
    end.

kill_query_h()->
    message2("kill_query( QueryId :: integer() ).\n").

kill_query(QueryId) when is_integer(QueryId)->
    up_ui(ui({kill_query,QueryId}));

kill_query(_)->
     not_found.

addqnone()->
    fun()->
        []
    end.

addqmod(ModFile)->
    fun()->
        [{file,mod2filec(ModFile)}]
    end.

addqmodpos(ModFile,Pos)->
    fun()->
        File = mod2filec(ModFile),
        [{file,File},
        {position,to_pos(File,Pos)}]
    end.

q_(Query=[Ch|_],Options,StartFun)
  when is_integer(Ch), is_list(Options), is_function(StartFun,0) ->
    catch_referr(
      fun()->
              Start = StartFun(),
              DispOpt =
                  [case E of
                       {out,_} ->
                           [];
                       {linenum,true}->
                           [{positions,linecol}];
                       {scalar,true}->
                           [{positions,scalar}];
                       {both,true}->
                           [{positions,both}];
                       {groupby,I} ->
                           [{groupby,I}];
                       _ ->
                           message("warning: unknown option '"++io_lib:print(E)++"'"),
                           []
                   end || E <- proplists:unfold(Options)],
              Args = [{display_opt,lists:append(DispOpt)},
                      {start_opt,Start},
                      {querystr,Query}],
              transform_(semantic_query,Args,
                  fun(Results)->
                    [Result] = ?MISC:pgetu([result],Results),
                    case proplists:lookup_all(out,Options) of
                        []->
                            io:put_chars(Result);
                        [{out,FileName}]->
                            {ok,IODev} = file:open(FileName, [write]),
                            io:put_chars(IODev, Result),
                            ok = file:close(IODev)
                    end,
                    ok
                  end)
      end).

%%% ============================================================================
%%% Others

%cluster_agglom() -> %%@todo :wishlist

%cluster_genetic() -> %%@todo :wishlist

%%% ============================================================================
%%% Server management

%% @private
get_filenode(ModFile) ->
    File = mod2file(ModFile),
    ?Query:exec1(?File:find(File),?RefError(file_not_present,[File])).

cat_h() ->
    message("cat(ModFile).\n"
             "Prints the contents of a file or module from the database.\n"
             "cat(ModFile, RecMac).\n"
             "Prints the definition of a macro or record.\n"
             "cat(ModFile, {FunName, Arity}).\n"
             "Prints the definition of a function.").

%% @doc Display the contents of the given file or module
cat(ModFile) when
        (is_atom(ModFile) or is_list(ModFile)) ->
    case ui({cat_file, ModFile}) of
        {ok, Data} -> message(Data);
        Else -> Else
    end;
    
cat(_)->
    message("::(modfile())"),
    usage.

%% @doc Display the definition of the given record or macro
cat(ModFile, {FunName,Arity}) when
        (is_atom(ModFile) or is_list(ModFile)),
        is_atom(FunName), is_integer(Arity)->
    cat(ModFile, FunName, Arity);

cat(ModFile, RecMac) when
        (is_atom(ModFile) or is_list(ModFile)),
        (is_atom(RecMac) or is_list(RecMac)) ->
     case ui({cat_recmac, ModFile, RecMac}) of
         {ok, Data} -> message2(Data);
         Else -> Else
     end;

cat(_,_)->
    message("::(modfile(), {atom(), natural()} | atom() | string())"),
    usage.

%% @doc Display a function definition
cat(ModFile, FunName, Arity) when
        (is_atom(ModFile) or is_list(ModFile)),
        is_atom(FunName), is_integer(Arity)->
    case ui({cat_fun, ModFile, FunName, Arity}) of
        {ok, Data} -> message2(Data);
        Else -> Else
    end;

cat(_,_,_)->
    message("::(modfile(), atom(), natural())"),
    usage.

%% -----------------------------------------------------------------------------
%% environment nodes

envs_h()->
    message("envs()").

%% @doc Lists all environment nodes
envs()->
    Result = ui({get_envs}),
    case Result of
        {ok, []} ->
            message("no environment variable set");
        {ok, L} ->
            message2(lists:map(fun show_env/1, L));
        error -> error
    end.


env_h()->
    message("env(Variable)").

%% @doc Lists a specific environment node
env(Name)->
    case ui({get_env, Name}) of
        {ok, Vals} -> message2(show_env({Name,Vals}));
        error -> error
    end.

addenv_h()->
    message("addenv(Variable,NewValue)").

%% @doc Adds a new environment node if it does not already exists.
addenv(EnvName, EnvVal) when is_atom(EnvName)->
    ui({add_env, EnvName, EnvVal}),
    ok.


delenv_h()->
    message(["delenv(Variable)\n"]).

%% @doc Deletes an environment node
delenv(Name) when is_atom(Name)->
    case ui({del_env, Name}) of
        {ok ,[]} -> message("not found"), fail;
        {ok, [_|_]} -> ok;
        error -> error
    end.

setenv_h()->
    message("setenv(Variable,Value)").

%% @doc Sets the value of an environment node
setenv(Name, Value) when is_atom(Name)->
    ui({set_env, Name, Value}),
    ok.

show_env({_Name,[]})->
    "error: Environment variable not found\n";

show_env({Name,Values})->
    [io_lib:format("~p = ~p~n", [Name,Value]) || Value <- Values].

%% @doc Returns the names of the files
%% that are described in the named environment.
dirs_by_env(Name) ->
    case ui({get_env, Name}) of
        {ok, Vals} -> Vals;
        error -> error
    end.

%% -----------------------------------------------------------------------------
generate_h() ->
    message("generate(FileDirList).\n
        Generates htmls, used by the Nitrogen based web interface, "
        "for a file or directory or a list of these.\n").

%% @doc Generates html for a file or directory or a list of these.
generate(L=[H|_]) when not is_integer(H) ->
    lists:foreach(fun generate_/1,L);

generate(X) -> generate_(X).

generate_(File=[C|_]) when is_integer(C) ->
    case io_lib:deep_char_list(File) of
        true ->
            ui({generate_dir,File}),
            ok;
        false ->
            message("Error: bad file argument given"),
            error
    end;

generate_(_)->
    message("::(file() | [file()])"),
    usage.

addQuery_h() ->
    message("addQuery(Type, Query).\n"
        "Adds a custom query, which appears in the context menu of the Nitrogen "
        "based web interface, for a specific node type.\n").

%% @doc Adds a custom query for a specific node type.
addQuery(Type,Query) -> referl_htmlserver:add_query(Type,Query).

%% -----------------------------------------------------------------------------

add_h() ->
    message("Type add(ModFileDirList) to add either a module, a file, a directory "
            "or a list of these to the database.\n"
            "Type add(AppBase, App) to add an application to the database.\n"
            "Examples:\n"
            "cd(dir), ri:add(modname).\n"
            "ri:add('dir/modname').\n"
            "ri:add(['dir/modname']).\n"
            "ri:add(\"dir/modname.erl\").\n"
            "ri:add(\"/current/dir/modname.erl\").\n"
            "ri:add(\"path_to_dir/dir\").\n"
            "ri:add(\"compiled.beam\"). \% The .beam must be compiled with debug_info.\n").

%% @doc Adds an application to the database.
add(AppBase, App) ->
    case dirs_by_env(appbase) of
        error -> error;
        AppBDirs -> add_app(AppBase, App, AppBDirs)
    end.

%% @doc Adds applications, files, which are described in the given Emakefile, 
%% to the database.
add_by_emakefile(EmakefilePath) ->
    catch_referr(
      fun()->
              case ui({add_by_emakefile, EmakefilePath}) of
                  error -> error;
                  _     -> ok
              end
      end).

add_by_emakefile_h() ->
    message2(["Adds applications, files, which are described in the given Emakefile, "
             "to the database.\n"
             "add_by_emakefile(EmakefilePath :: string() | [string()]).\n"]).

%% @doc Adds include paths, macros, which are described in the given Emakefile, 
%% to the database.
load_configuration(EmakefilePath) ->
    catch_referr(
      fun()->
              case ui({load_configuration, EmakefilePath}) of
                  error -> error;
                  _     -> ok
              end
      end).

load_configuration_h() ->
    message2(["Adds include paths, macros, which are described in the given Emakefile, "
              " to the database.\n"
              "load_configuration(EmakefilePath :: string() | [string()]).\n"]).

%% @doc Removes include paths, macros, which are described in the given Emakefile, 
%% from the database.
unload_configuration(EmakefilePath) ->
    catch_referr(
      fun()->
              case ui({unload_configuration, EmakefilePath}) of
                  error -> error;
                  _     -> ok
              end
      end).

unload_configuration_h() ->
    message2(["Removes include paths, macros, which are described in the given Emakefile, "
              " from the database.\n"
              "unload_configuration(EmakefilePath :: string() | [string()]).\n"]).

add_app(GivenAppBase, App, AllAppBases)->
    AppBaseS = ?MISC:to_list(GivenAppBase),
    AppS     = ?MISC:to_list(App),
    Bases = [AB || AB <- AllAppBases,
                   {match, _} <- [re:run(AB, AppBaseS)]],
    case dir_to_load(Bases, AppBaseS, AppS) of
        {no_dir, Msg} ->
            Msg;
        Dir ->
            add_src_dir(Dir)
    end.

-spec dir_to_load([string()], string(), string())
            -> string() | {no_dir, Cause :: atom()}.
%% Returns name of the directory to load as a string or {no_dir, Cause}.
%% Preference is given to an exactly matching directory name,
%% and a higher version number.
dir_to_load([], AppBaseS, _) ->
    message("Application base " ++ AppBaseS ++ " not found"),
    {no_dir, missing_appbase};
dir_to_load(Bases, AppBaseS, AppS) ->
    AppNotFound = "Application " ++ AppS ++ " not found under " ++ AppBaseS,
    try
        Bases =/= [] orelse
            throw(empty_bases),
        DirMatches = lists:concat([get_dirs(AppS, Base) || Base <- Bases]),
        DirMatches =/= [] orelse
            throw(no_matching_dir),
        ExactMatchDirs = [Dir || {exact, Dir} <- DirMatches],
        ExactMatchDirs == [] orelse
            throw({has_exact_matches, ExactMatchDirs}),
        PrefixMatchDirs = [Dir || {prefix, Dir} <- DirMatches],
        PrefixMatchDirs =/= [] orelse
            throw({no_matches_at_all, DirMatches}),
        lists:last(PrefixMatchDirs)
    catch
        {has_exact_matches, ExactMatchDirs2} ->
            lists:last(ExactMatchDirs2);
        no_matching_dir ->
            message(AppNotFound),
            {no_dir, not_found};
        {no_matches_at_all, DirMatches2} ->
            message(AppNotFound),
            io:format("Applications under :~n" ++ AppBaseS),
            [io:format("    ~s~n", [Dir]) || Dir <- DirMatches2],
            {no_dir, not_found}
    end.


%% Tries to find a directory for the application name `AppS'.
%% This succeeds if either there is a directory called `AppS' (with an optional version number),
%% or there is at least one directory whose name begins with `AppS'.
%% In these cases, a non-empty list is returned with elements {exact, Dir}
%% and {prefix, Dir}, respectively.
get_dirs(AppS, Base) ->
    BaseApp    = filename:join(Base, AppS),
    IsExactDir = filelib:is_dir(BaseApp),
    VsnDirs    = filelib:wildcard(filename:join(Base, AppS ++ "-*")),
    PrefixDirs = filelib:wildcard(filename:join(Base, AppS ++ "*")),
    case {IsExactDir, VsnDirs, PrefixDirs} of
        {true, _, _} ->
            [{exact, BaseApp}];
        {false, [_|_], _} ->
            [{exact, filename:join(Base, Dir)} || Dir <- VsnDirs];
        _ ->
            [{prefix, filename:join(Base, Dir)} || Dir <- PrefixDirs]
    end.

add_src_dir(Dir) ->
    SrcDir = filename:join(Dir, "src"),
    message("Adding: " ++ SrcDir),
    add(SrcDir).


%% Returns whether the result of a transformation is (partly) successful.
% result_is_ok(deny)                -> false;
% result_is_ok(abort)               -> false;
% result_is_ok(error)               -> false;
% result_is_ok({error, _})          -> false;
% result_is_ok(Xs) when is_list(Xs) -> lists:any(fun result_is_ok/1, Xs);
% result_is_ok(_)                   -> true.

%% @doc Add either a module, a file, a directory or a list of these
add(L=[H|_]) when not is_integer(H) ->
    lists:foreach(fun add_/1,L);
add(X) ->
    catch_referr(
      fun()->
              case add_(X) of
                  error -> error;
                  _     -> ok
              end
      end).

add_(Mod) when is_atom(Mod) ->
    try
        add_(mod2file(Mod,true))
    catch
        ?RefError(mod_not_found,[Mod]) ->
            N = atom_to_list(Mod),
            add_if_exists(N++".erl") orelse
            add_if_exists(N++".beam") orelse
            begin
                message("Error: no matching .erl or .beam"),
                error
            end
    end;
add_(File=[C|_]) when is_integer(C) ->
    case io_lib:deep_char_list(File) of
        true ->
            add_file(File);
        false ->
            message("Error: bad file argument given"),
            error
    end;
add_(_)->
    message("::(modfile() | [modfile()])"),
    usage.

add_if_exists(F)->
    filelib:is_regular(F) andalso
        error =/= add_(F).

print_files(error) ->
    error;
print_files({ok, Infos}) ->
    GoodFiles = [FileName           || {FileName, [{present,true}]} <- Infos],
    ErrInfos  = [{FileName, Errors} || {FileName, [{error,Errors}]} <- Infos],
    message(?MISC:any_to_string({{ok, GoodFiles}, {error, ErrInfos}})).

%% @private
add_file(File) ->
%    Added = ui({add_dir,File}),
%    print_files(Added).
    case lists:suffix(".hrl", File) of
        true ->
            add_hrl(File);
        false ->
            add_erlbeam(File)
    end.

add_erlbeam(File) ->
    ui({add_dir,File}).

add_hrl_h() ->
     message("add_hrl(HeaderFileName)\n"
             "Adds a header file (.hrl) to the database.\n").

add_hrl(File=[C|_]) when is_integer(C) ->
    ui({add,File});
add_hrl(_) ->
    message("::(string())"),
    usage.

%ri:add([a,b]).
%ri:add("a.erl").

drop_h() ->
    message("Type drop(ModFileDirList) to drop a file from the database\n"
        "Examples:\n"
        "ri:drop(modname).\n"
        "ri:drop([modname]).\n"
        "ri:drop(\"dir/modname.erl\").\n"
        "ri:drop(\"/current/dir/modname.erl\").\n"
        "ri:drop(\"path_to_dir/dir\").").

%% @doc Drop either a module, a file, a directory or a list of these
drop(L=[H|_]) when not is_integer(H) ->
    lists:foreach(fun drop/1,L);
drop(Mod) when is_atom(Mod) ->
    catch_referr(fun()->
        drop(mod2file(Mod,true))
    end);
drop(File) when is_list(File) ->
    drop_dir(File);
drop(_)->
    message("::(modfile() | [modfile()])"),
    usage.

%% @ private
%drop_dir drops files, too.
%drop_file(File) ->
%    Dropped = ui({drop,File}),
%    print_files(Dropped).
    
% @private
drop_dir(File) ->
    ui({drop_dir,File}).
%ri:drop(a).

save_h() ->
    message("save(ModFileDirList)\n"
        "Saves a file (debug).\n").

%% @doc Saves a file (debug)
%% @todo Is this still needed?
save(L=[H|_]) when not is_integer(H) ->
    lists:foreach(fun save/1,L);
save(Mod) when is_atom(Mod) ->
    catch_referr(fun()->
        save(mod2file(Mod,true))
    end);
save(File) when is_list(File) ->
%    recurse_erl(File, fun save_file/1).
%save_file(File) ->
    catch_referrsave(fun()->
        ?FileMan:save_file(get_filenode(File))
    end).


lsl_h() ->
    message("Usage: lsl()"). %@todo

lsl() ->
    print_files(ui({status_info,[]})).

ls_h() ->
    message("ls().\n"
        "Lists modules contained in the database.\n"
        "ls(ModFile).\n"
        "Lists includes, records, macros and functions in a file or module.\n").

%% @doc Lists modules contained in the database
ls() ->
    Files = ui({filelist}),
    print_files(Files).

%% @doc Lists includes, records, macros and functions in a file or moule
ls(ModFile) when
        (is_atom(ModFile) or is_list(ModFile))->
    catch_referr(fun() -> ls_(ModFile) end);
ls(_)->
    message("::(modfile())"),
    usage.

ls_(ModFile) ->
    FN  = get_filenode(ModFile),
    GetFunName = fun(F)-> ?MISC:fun_text([?Fun:name(F),?Fun:arity(F)]) end,
    GetRecName = fun(R)-> atom_to_list(?Rec:name(R)) end,
    Gets = [{"includes",?File:includes(),fun ?File:path/1},
            {"records", ?File:records(), GetRecName},
            {"macros",  ?File:macros(),  fun ?Macro:name/1},
            {"funcs",   ?Query:seq([[form],[fundef]]), GetFunName}],
    Text = [ begin Res = ?Query:exec(FN,Q) -- [FN],
                   case Res of
                       "" -> "";
                       _  -> S ++ ":" ++ [ " " ++ D(E) || E <- Res ] ++ "\n"
                   end
             end
           || {S,Q,D} <- Gets ],
    message2(Text).

%% @doc Returns information about the RefactorErl system.
system_info() ->
    message2(lists:map(fun show_prop/1, up_ui(ui({system_info})))).

system_info_h()->
    message(["system_info()\n",
            "Returns information about the RefactorErl system."]).

show_prop({Name,Values=[C|_]}) when is_list(Values) andalso not is_integer(C)->
    [io_lib:format("~p = ~p~n", [Name,Value]) || Value <- Values];
show_prop({Name,Value})->
    [io_lib:format("~p = ~p~n", [Name,Value])].



%% @doc Creates a backup from the current state of the graph.
%% @see ri:backup_info/1
%% @see ri:ls_backups/0
backup() ->
    backup("").

%% @doc Creates a backup as {@link ri:backup/0},
%% but here the user can attach a commit log to the backup file.
%% @see ri:backup_info/1
%% @see ri:ls_backups/0
backup(CommitLog) when is_list(CommitLog) ->
    call_fun({backup, CommitLog});

backup(_) ->
    message("::(string())"),
    usage.

backup_h() ->
    message(["backup()\n",
             "backup(CommitLog)"]).

%% @equiv ri:backup()
checkpoint() ->
    backup().

%% @equiv ri:backup(CommitLog)
checkpoint(CommitLog) ->
    backup(CommitLog).

checkpoint_h() ->
   message(["checkpoint()\n",
            "checkpoint(CommitLog)"]).

%% @doc Returns a lists of backups, that has been created before with
%% {@link ri:backup/0} / {@link ri:checkpoint/0} or
%% {@link ri:backup/1} / {@link ri:checkpoint/1}.
%% @see ri:backup_info/1
ls_backups() ->
    Backups = call_fun({ls_backups}),
    case Backups of
        ok ->
            message3([]);
        _Else ->
            message3(Backups)
    end.

ls_backups_h() ->
    message("ls_backups()").

%% @equiv ri:ls_backups()
ls_checkpoints() ->
    ls_backups().

ls_checkpoints_h() ->
    message("ls_checkpoints()").

%% @spec backup_info(atom() | integer() | string()) -> any()
%% @doc Returns information about the given backup. That information is
%% the time of creation, and the commit-log (if specified earlier).
%% @see ri:ls_backups/0
backup_info(Backup) when is_atom(Backup) or is_list(Backup) or is_integer(Backup) ->
    case call_fun({backup_info, Backup}) of
        {ok, Infos} ->
            BackupName   = element(1, Infos),
            CreationTime = element(2, Infos),
            CommitLog    = element(3, Infos),

            io:format("Informations about ~w:~n", [BackupName]),
            case CommitLog of
                [] ->
                    ok;
                _Else ->
                    io:format("Commit-log: ~s~n", [CommitLog])
            end,
            io:format("Time of creation: ~s~n~n", [CreationTime]);
        Else ->
            Else
    end;

backup_info(_) ->
    message("::(atom() | string() | integer())"),
    usage.

backup_info_h() ->
    message(["backup_info(BackupName)\n",
             "backup_info(CheckpointNumber)"]).

%% equiv ri:backup_info(Backup)
checkpoint_info(Backup) ->
    backup_info(Backup).

checkpoint_info_h() ->
    message(["checkpoint_info(BackupName)\n",
             "checkpoint_info(CheckpointNumber)"]).

%% @spec restore(atom() | list() | integer()) -> any()
%% @doc Restores the given backup.
%% @see ri:backup/1
restore(Backup) when is_atom(Backup) or is_list(Backup) or is_integer(Backup) ->
    call_fun({restore, Backup});

restore(_) ->
    message("::(atom() | string() | integer())"),
    usage.

restore_h() ->
    message(["restore(BackupName)\n",
             "restore(CheckpointNumber)"]).

%% @doc undoes the previous transformation.
undo() ->
    call_fun({undo, []}).

undo_h() ->
    message("undo()").

%% @doc Removes all backups that belongs to the actual graph.
clean() ->
    call_fun(clean).

clean_h() ->
    message("clean().\n"
        "Removes all backups ( and deletes all checkpoints) that belongs to the "
        "actual graph.\n").

%% @spec create_graph(atom()) -> any()
%% @doc Creates a graph with the given name.
%% @see rename_graph/2
%% @see load_graph/1
%% @see delete_graph/1
create_graph(Name) when is_atom(Name) ->
    call_fun({create_graph, Name});

create_graph(_) ->
    message("::(atom())"),
    usage.

create_graph_h() ->
    message("create_graph(Name)").

%% @spec rename_graph(atom(), atom()) -> any()
%% @doc Renames a graph that has the given OldName, with the given NewName.
rename_graph(OldName, NewName) when is_atom(OldName) and is_atom(NewName) ->
    call_fun({rename_graph, OldName, NewName});

rename_graph(_, _) ->
    message("::(atom(), atom())"),
    usage.

rename_graph_h() ->
    message("rename_graph(OldName, NewName)").

%% @doc Returns a list of the created graphs.
%% @see ri:create_graph/1
%% @see ri:actual_graph/0
ls_graphs() ->
    Graphs = ui({ls_graphs}),
    message3(Graphs).

ls_graphs_h() ->
    message("ls_graphs()\n"
            "Returns a list of the created graphs").

%% spec actual_graph() -> atom()
%% @doc Returns the actual graph's name.
%% @see ri:ls_graphs/0
actual_graph() ->
    ActGraph = ui({actual_graph}),
    message3(ActGraph).

actual_graph_h() ->
    message("actual_graph()").

%% @spec load_graph(atom()) -> any()
%% @doc Loads the given graph.
%% @see ri:create_graph/1
load_graph(Name) when is_atom(Name) ->
    call_fun({load_graph, Name});

load_graph(_) ->
    message("::(atom())"),
    usage.

load_graph_h() ->
    message("load_graph(Name)").

%% @spec delete_graph(atom()) -> any()
%% @doc Removes the given graph.
%% @see ri:delete_all_graphs/0
delete_graph(Name) when is_atom(Name) ->
    call_fun({delete_graph, Name});

delete_graph(_) ->
    message("::(atom())"),
    usage.

delete_graph_h() ->
    message("delete_graph(Name)").

%% @doc Removes all graphs.
%% @see ri:delete_graph/1
delete_all_graphs() ->
    call_fun({delete_all_graphs}).

delete_all_graphs_h() ->
    message("delete_all_graphs()\n"
            "Removes all graphs").

call_fun(FunName) when is_atom(FunName) ->
    call_fun({FunName});

call_fun(FunAndParams) ->
    case ui(FunAndParams) of
        {ok, []} ->
            ok;
        Else ->
            Else
    end.


graph_h() ->
    message2([
        "graph()\n"
        "graph(TargetNameFile)\n"
        "graph(TargetFile,FilterList)\n" ]).

%% @doc Draws a graph with default name from database contents (debug)
graph() ->
    graph(?DEF_GRAPH_NAME).

%% @doc Draws a graph from database contents, saves result in `Target' (debug)
graph(Target) ->
    graph(Target,[]).

%% @doc Draws a graph from filtered database contents (debug)
graph(Target, Filter) when is_atom(Target) ->
    graph(atom_to_list(Target)++".dot",Filter);

graph(Target, Filter) ->
    Success = ui({draw, Target, Filter}),
    message(?MISC:any_to_string(Success)). % @todo

svg_h() ->
    message2([
        "svg()\n"
        "svg(TargetNameFile)\n"
        "svg(Target,Options)\n" ]).

%% @doc Draws a graph converted to SVG with default name (debug)
svg() ->
    svg(?DEF_GRAPH_NAME).

%% @doc Draws a graph converted to SVG from database contents (debug)
svg(Target) ->
    svg(Target, []).

%% @doc Draws a graph converted to SVG from filtered database contents (debug)
svg(Target, Opts) when is_atom(Target) ->
    svg(atom_to_list(Target)++".svg",Opts);
svg(Target, Opts) when is_list(Target) ->
    DotName = filename:rootname(Target) ++ ".dot",
    io:format("Making ~s...~n", [DotName]),
    case ui({draw,DotName, Opts}) of %%TODO: tooltip
        error -> error;
        _ -> call_graphviz(DotName, Target)
    end;

svg(_, _)->
    message("::(atom() | string(), proplist())"),
    usage.

call_graphviz(DotName, Target) ->
    io:format("Calling Graphviz...~n"),
    Res = os:cmd("dot -Tsvg "++DotName++" -o"++Target),
    case Res of
        [] -> ok;
        _  -> {error,Res}
    end.

gn_h() ->
    message("gn(TypeAtom,Index)").

%% @doc Prints out data of a graph node (debug)
gn(TypeAtom,Index) when is_atom(TypeAtom), is_integer(Index) ->
    message(io_lib:print(?Graph:data({'$gn',TypeAtom,Index})));
gn(_,_)->
    message("::(atom(), integer())"),
    usage.

stop_h()->
    message("stop()\n"
           "Stops the local node.").

%% @doc Stops the local node.
stop()->
    case ?REFERL_NODE == node() of
        true -> Success = ui({stop}), %act as a server
                message(?MISC:any_to_string(Success)); 
        false -> init:stop() %act as a client
    end.

shutdown_h()->
    message("shutdown()\n"
           "Stops the RefactorErl server,"
           " whether it is running on a local node or on a remote node.").

%% @doc Stops the server.
shutdown()->
    case net_adm:ping(?REFERL_NODE) of
        pong -> case rpc:call(?REFERL_NODE, ri, stop, []) of
                    {badrpc, _Reason} -> message("Cannot stop the RefactorErl Server");
                    _ -> message("The server stopped successfully.")
                end;
        pang -> message("The server is not running on "++
                            ?MISC:any_to_string(?REFERL_NODE)++" node.")
    end.

reset_h() ->
    message("reset().\n"
        "Resets the database by clearing its contents and resetting its schema.\n"
        "reset(PosMode).\n"
        "Resets the database and sets the positioning mode (abs or rel).").

%% @doc Clears database contents and resets its schema
reset()  ->
    metricmode(off),
    Success = ui({reset}),
    message(?MISC:any_to_string(Success)). % @todo

reset(PosMode)  ->
    metricmode(off),
    Success = ui({reset, PosMode}),
    message(?MISC:any_to_string(Success)).    
    
current_posmode() ->
    {ok, CurrPosMode} = ui({current_posmode}),
    Answer = lists:concat(["Current positioning mode is ",atom_to_list(CurrPosMode)]),
    message(Answer).

database_synchronization_h()->
    message("database_synchronization().\n"
            "Synchronizes the database with the contents of the disk \n").

%% @doc Synchronize the contents of the database with the contents of the disc.
%% If a file has been changed on the disk since the last loading, 
%% then it will be reloaded.
database_synchronization()->
    message("\nDatabase synchronization has been started...\n"),
    ui({synchronize}),
    message("\nDatabase synchronization has been finished...\n").

anal_message_h() ->
    message("anal_message()\n").

anal_message() ->
    up_ui(ui({anal_message})).

anal_dyn_h() ->
    message("anal_dyn()\n"
            "Analyses dynamic call constructs.").

anal_dyn() ->
    up_ui(ui({anal_dyn})).

clean_dyn_h() ->
    message("clean_dyn()\n"
            "Cleans the dynamic calls from the database.\n").

clean_dyn() ->
    up_ui(ui({clean_dyn})).

anal_dyn_spec_h() ->
    message("anal_dyn_spec()\n").

anal_dyn_spec() ->
    up_ui(ui({anal_dyn_spec})).


%%% ============================================================================
%%% Regexp helpers

%% @doc gets a posrange in a file from a regexp
to_posrange(File,Range) ->
    rgxpos(File,Range).

%% @doc gets a position in a file from a regexp
to_pos(_,Pos) when is_integer(Pos) ->
    Pos;
to_pos(File,Pos) ->
    {P,_}=rgxpos(File,Pos),
    P.

%% @doc gets a position or a posrange in a file from a regexp
%% @private
%% @todo :wishlist improve rgxpos/3 to be context sensitive and indexless
%% @todo :wishlist check Rgx
rgxpos(File,Pos) ->
    case ?Query:exec(?File:find(File)) of
        [FileNode] -> Text = lists:flatten(?Syn:tree_text(FileNode)),
                      rgxpos_text(Text,Pos);
        _          -> throw(?RefError(file_not_present,[File]))
    end.

%% @doc gets a position or a posrange in text from a regexp
rgxpos_text(_,{Beg,End}) when is_integer(Beg), is_integer(End) ->
    {Beg,End};
rgxpos_text(Text,Rgx=[C|_]) when is_integer(C) ->
    rgxpos_text(Text,{Rgx,1});
rgxpos_text(Text,{Rgx=[C|_],Idx}) when is_integer(Idx), is_integer(C) ->
    case getparen(Rgx) of
        invalid ->
            throw(?LocalError(invalid_regexp,[Rgx]));
        {{P,Q},RRgx} ->
            Matches  =
                case tryrgx(run,[Text,RRgx]) of
                    [] -> throw(?LocalError(unmatched_regexp,[Rgx]));
                    M  -> M
                end,
            {Beg,Len} =
              try
                lists:nth(Idx,Matches)
              catch
                error:function_clause ->
                    throw(?LocalError(unbound_idx_regexp,[Rgx,Idx]))
              end,
            Match = lists:sublist(Text,Beg,Len),
            Pre = [$^] ++ lists:sublist(RRgx,1,Q),
            Suf = lists:nthtail(P-1,RRgx) ++ [$$],
            Tails = length(tryrgx(replace,[Match,Pre,""])),
            Inits = length(tryrgx(replace,[Match,Suf,""])),
            {Beg+Inits, Beg+Len-1-Tails}
%%@todo !!! better separation!!!
    end;
rgxpos_text(_,Pos) -> throw(?LocalError(invalid_pos,[Pos])).

tryrgx(RegFun, [Text,RRgx, Replacement|_]) when RegFun=:=sub;RegFun=:=replace ->
    case catch re:replace(Text, RRgx, Replacement, [{return,list}, multiline]) of
        {'EXIT',{badarg,_}} -> throw(?LocalError(invalid_regexp,[RRgx]));
        Text -> throw(?LocalError(invalid_regexp,[RRgx]));
        NewText=[C|_] when is_integer(C) -> NewText;
        [] -> [];
        _ -> throw(?LocalError(invalid_regexp,[RRgx]))
    end;
tryrgx(RegFun, [Text,RRgx|_]) when RegFun=:=matches;RegFun=:=run ->
    case catch re:run(Text, RRgx, [global, multiline]) of
        {'EXIT',{badarg,_}} -> throw(?LocalError(invalid_regexp,[RRgx]));
        nomatch -> [];
        {match, Matches} ->[{StartPos+1, Length}|| [{StartPos, Length}]<-Matches];
        _ -> throw(?LocalError(invalid_regexp,[RRgx]))
    end;
tryrgx(RegFun,Params=[_,RRgx|_]) ->
    case apply(regexp,RegFun,Params) of
        {match,M} -> M;
        {ok,R,1}  -> R;
        _         -> throw(?LocalError(invalid_regexp,[RRgx]))
    end.

%% @private
%% @doc Gets the location of escaped parenthesis in text
%% Starts up the state machine for getparen/4.
getparen(L=[_|_]) ->
    getparen([],1,[],L).

%% @doc Gets the location of escaped parenthesis in text
%%
%% Example invocations: <ol>
%%  <li>getparen("abc\\tdef")   = {{1,8},"abc\\tdef"}</li>
%%  <li>getparen("abc&lt;de&gt;f")    = {{4,5},"abcdef"}</li>
%%  <li>getparen("a&lt;b\\&lt;cd&lt;ef") = {{2,5},"ab&lt;cdef"}</li>
%%  <li>getparen("a&lt;b\ncd&lt;ef")  = {{2,5},"ab\ncdef"}</li>
%%  <li>getparen("a&lt;bcdef&lt;")    = {{2,6},"abcdef"}</li>
%%  <li>getparen("ab&lt;cdef")     = {{3,6},"abcdef"}</li>
%%  <li>getparen("abcde&lt;f")     = {{1,5},"abcdef"}</li>
%%  <li>getparen("ab&lt;c&lt;d&lt;")     = invalid</li>
%%  </ol>
%%
getparen([], I,R,[$<|T]) ->
    getparen([I],I,R,T);
getparen([],I,R,[$>|T]) ->
    getparen([1,I-1],I,R,T);
getparen([P],I,R,[$>|T]) ->
    getparen([P,I-1],I,R,T);
getparen(S,  I,R,[$\\,C |T]) when (C==$<) or (C==$>) ->
    getparen(S,I+1,R++[C],T);
getparen(S,  I,R,[$\\,C |T]) when is_integer(C) ->
    getparen(S,I+2,R++[$\\,C],T);
getparen(S,  I,R,[C|T])      when is_integer(C), C/=$\\, C/=$<, C/=$> ->
    getparen(S,I+1,R++[C],T);
getparen([], I,R,[])-> {{1,I-1},R};
getparen([P],I,R,[])-> {{P,I-1},R};
getparen([P,Q],_,R,[])-> {{P,Q},R};
getparen(_,_,_,_)-> invalid.

%%% ============================================================================
%%% Other helpers
%% @doc transformation helper for source+dest refactorings
transform2(Ref, Source, Dest, Args) ->
    catch_referr(fun()->
        DestTup = case Ref of
            move_fun ->
                {name,    file2modc(Dest)};
            A when (A==move_rec) or (A==move_mac) ->
                {filename,mod2filec(Dest)}
        end,
        transform_inject(Ref, Source, fun(A)->A end, [DestTup|Args])
     end).

%% @doc transformation helper for refactorings needing a position
transformp(Ref,Source,Pos,Args) ->
    catch_referr(fun()->
        Fun=fun(A)->
                {file,File} = proplists:lookup(file,A),
                [{position,to_pos(File,Pos)} | A] end,
        transform_inject(Ref,Source,Fun,Args)
     end).

%% @doc transformation helper for refactorings needing a posrange
transformr(Ref,Source,Range,Args) ->
    catch_referr(fun()->
        Fun=fun(A)->
                {file,File} = proplists:lookup(file,A),
                [{posrange,to_posrange(File,Range)} | A] end,
        transform_inject(Ref,Source,Fun,Args)
     end).

%% @private
%% @doc transformation helper that prints exceptions
transform_catch(Ref, Source, Args) ->
    catch_referr(fun()->
        transform_inject(Ref, Source, fun(X)->X end, Args)
    end).

%% @private
%% @doc transformation helper that adds both file and module information
transform_inject(Ref, Source, Fun, Arg) when is_function(Fun,1) ->
    Args = [{file,  mod2filec(Source)},
            {module,file2modc(Source)}|Arg],
    transform(Ref, Fun(Args)).

%% @private
catch_referrsave(Fun) when is_function(Fun,0) ->
     catch_referr(fun() ->
         catch_save(Fun)
     end).

%% @private
%% @doc prints out refactorerl exceptions as textual errors
catch_referr(Fun) when is_function(Fun,0) ->
    try
        Fun() %{ok,}
    catch
        E={M,_,_} when is_atom(M) -> %(M==?MODULE) or (M==?Error) ->
            message(?Error:error_text(E)),
            error; %{error,E}
        %Error -> ?d(Error)
        _ -> ok
    end.

%% @private
%% @doc prints out save_file exceptions as textual errors
catch_save(Fun) when is_function(Fun,0) ->
    R = try
            Fun()
        catch
            unsafe_save -> {error,"unsafe save denied"}
        end,
    case R of
        {error,Error} ->
            message(Error);
        ok -> ok
    end.

transform_h()->
    message("transform(Refac,Args)\n"
        "This function is an internal function!\n"
        "Transformation helper that catches messages synchronously.\n"
        "type: transform(Refac,Args)\n"
        "Refac :: atom()\n"
        "Args :: list()").

%% @doc transformation helper that catches messages synchronously
%% exported for debugging purposes
transform(Refac, Args) when is_atom(Refac), is_list(Args) ->
    transform_(Refac, Args, fun(_)->success end).

%% @doc transformation helper that catches messages synchronously
transform_(Refac, Args0, Fun) when is_function(Fun,1),
  is_atom(Refac), is_list(Args0) ->
    Args = [{ask_missing,true} | Args0],
    case ui({transform, Refac, Args}) of
        {ok, {result, R}} ->
            Fun(R);
        {ok, {abort, {_,A}}} ->
            message(A),
            deny;
        {ok, _} ->
            ok;
        {error, {_,E}} ->
            message("Fatal: " ++ E),
            error;
        error ->
            error;
        _ ->
            ok
    end.

%% @doc handle synchronous UI/transform calls
trap(Pre, Action, Done)
  when is_function(Pre,0), is_function(Action,1),
       is_function(Done,1) ->

    State  = Pre(),
    Answer =
        try
            Action(State)
        catch
            E ->
                Done(E),
                throw(E);
              error:E ->
                Done({error,E}),
                erlang:error(E)
        end,
   Done(Answer).

%%% ----------------------------------------------------------------------------
%%% File and module related helpers

%% @private
%% @doc looks up the module name of a loaded and error-free file
file2modc(Source) ->
    _ = guard_ready(Source),
    file2mod(Source).

%% @private
%% @doc looks up the module name of a loaded file
file2mod([]) ->
    throw(?LocalError(no_file,[]));
file2mod(Mod) when is_atom(Mod) ->
    Mod;
file2mod(Fil) when is_list(Fil) ->
    File = filename:absname(Fil),
    FileNode = ?Query:exec1(?File:find(File),
                            ?RefError(file_not_present, [File])),
    ModNode  = ?Query:exec1([FileNode], ?File:module(),
                            ?RefError(file_not_module,  [File])),
    ?Mod:name(ModNode).

%% @private
%% @doc looks up the filename for a loaded and error-free module
mod2filec(Source) ->
    guard_ready(mod2file(Source)).

%% @private
%% @doc looks up the filename for a loaded module
mod2file([],_Amb) ->
    throw(?LocalError(no_file,[]));
mod2file(File,_Amb) when is_list(File) ->
    filename:absname(File);
mod2file(Mod,Amb) when is_atom(Mod), is_boolean(Amb) ->
    FileNodes =
        ?Query:exec(?Query:seq([
            ?Mod:find(Mod),
            ?Mod:file() ])),
    case FileNodes of
        [File] ->
            ?File:path(File);
        [] ->
            throw(?RefError(mod_not_found, [Mod]));
        [_|_] ->
            case Amb of
                false ->
                    throw(?RefError(ambiguous_mod, [Mod]));
                true ->
                    [?File:path(File) || File <- FileNodes]
            end;
        X      -> throw(?LocalError(internal_unexpected, [X]))
    end.

%% @private
%% @doc returns only a single filename, otherwise throws ambiguous_mod
mod2file(X) -> mod2file(X,false).

%% @private
%% @doc ensures that the given file is error-free
guard_ready([])   ->
    throw(?LocalError(no_file,[]));
guard_ready(File) ->
    case state(File) of
        ok      -> File;
        invalid -> throw(?LocalError(not_ready,[File]));
        _       -> throw(?RefError(file_not_present, [File]))
    end.

%% @private
%% @doc gets the status of a file or module
state(Source) ->
    trap(
      fun() ->
              mod2file(Source)
      end,
      fun(F) ->
              {ok,[{F,Prop}]} = ui({status,F}),
              case ?MISC:pget([present,error],Prop) of
                  [[true],[]]    ->
                      ok;
                  [[false],[]] ->
                      missing;
                  [[true],[true]] ->
                      invalid
              end
      end,
      fun(X) -> X end).

%%% ----------------------------------------------------------------------------
%%% Elementary helpers

build_h()->
    message("build()\n"
    "build(no_cpp)\n"
    "Compiles / recompiles the tool.\n"
    "The compilation of cpp sources can be ignored by calling build(no_cpp)\n").

%% @doc Builds RefactorErl.
build() ->
    os:putenv("COMPILE_CPP", "true"),
    os:putenv("ERICSSON", "false"),
    build_source().

%% @doc Builds RefactorErl without compiling the CPP code.
build(no_cpp) ->
    os:putenv("COMPILE_CPP", "false"),
    os:putenv("ERICSSON", "false"),
    build_source();

build(ericsson) ->
    build([{ericsson, true}]);

build(List) when is_list(List) ->
    os:putenv("COMPILE_CPP", "true"),
    case proplists:get_value(ericsson, List, false) of
        true -> 
            os:putenv("ERICSSON", "true");
        false ->
            os:putenv("ERICSSON", "false")
    end,
    case proplists:get_value(force, List, false) of
        true -> 
            build_source(true);
        false ->
            build_source(false)
    end.

    
build_source() ->
    build_source(false).

build_source(Force)  when Force =:= true; Force =:= false ->
    referl_gen_build:run(tool_dir(), tool, [debug_info, {force, Force}]).

%% @private
%% @doc Returns the `tool' directory of RefactorErl.
tool_dir() ->
    filename:join([code:lib_dir(referl_core), "..", ".."]).

%% @spec errors() -> ok | 'errors!'
%% @doc Prints a list of functions that have cross reference problems:
%%      unused, deprecated, or most importantly, undefined functions.
%%      Ideally, this function should print nothing, and return `ok'.
errors() ->
    errors(all).

%% @spec errors(all | loaded) -> ok | 'errors!'
%% @doc  A helper for `errors/0',
%%       this function can print the errors of all known modules (`all'),
%%       or only those that are loaded (`loaded').
errors(Opt) ->
    {ok, [{script, _, Actions}]} =
        file:consult(filename:join(tool_dir(), "refactorerl.script")),
    Mods = [Mod ||  {apply, {application, load,
                             [{application, _App, Opts}]}} <- Actions,
                    {modules, Mods} <- Opts,
                    Mod <- Mods],
%    io:format("Checking modules:~n    ~p~n", [Mods]),
    Problems = [{St, Funs} ||   Mod <- Mods,
                                lists:prefix("ref", atom_to_list(Mod)),
                                {St, Funs} <- xref:m(Mod),
                                Funs =/= []],
    P1 = print_problems(unused, "Unused functions:", Problems, Mods, Opt),
    P2 = print_problems(deprecated, "Deprecated functions:", Problems, Mods, Opt),
    P3 = print_problems(undefined, "Missing functions:", Problems, Mods, Opt),
    case {P1, P2, P3} of
        {ok, ok, ok} -> ok;
        _            -> 'errors!'
    end.

print_problems(ProblemAtom, Descr, Problems, Mods, Opt) ->
    case lists:usort(lists:append([Funs ||  {A, Funs} <- Problems,
                                            A =:= ProblemAtom])) of
        [] ->
            ok;
        ProblemFuns ->
            io:format("~s~n", [Descr]),
            [io:format("  ~p:~p/~p~n", [M,F,A]) || {M,F,A} <- ProblemFuns],
            PrCalled = [{M,F,A} || {_, {M,F,A}} <- ProblemFuns],
            [begin
                io:format("  ~p:~p/~p~n", [PM,PF,PA]),
                [io:format("      <- ~p:~p/~p~n", [M,F,A]) ||
                    {{M,F,A},{M2,F2,A2}} <- ProblemFuns,
                    {M2,F2,A2} =:= {PM,PF,PA},
                    Opt =:= all orelse lists:member(M, Mods)]
             end || {PM,PF,PA} <- lists:usort(PrCalled),
                    Opt =:= all orelse lists:member(PM, Mods)],
            errors
    end.

-ifdef(ericsson).
is_ericsson_mode() ->
    true.
-else.
is_ericsson_mode() ->
    false.
-endif.

%% @doc Runs the unit tests.
test() ->
    %ri:metricmode(off),
    reftest_refact:run().

%% @doc Runs all test cases.
%% @todo Run unit tests and other regression indicators
testall() ->
    %ri:metricmode(off),
    {reftest_lib:run(),
     reftest_refact:run()}.

%% @doc Runs the test cases of a transformation.
%% The name of the transformation can be abbreviated up to ambiguity.
%% @todo Run on a given entity
test(Params) when is_list(Params) ->
    reftest_refact:run(Params);
test(Mod) when is_atom(Mod) ->
    {ok, TestDirFiles} = file:list_dir(filename:join(["..", "test"])),
    ModList = atom_to_list(Mod),
    case [F || F <- lists:usort(TestDirFiles), lists:prefix(ModList, F)] of
        [FileName|_] -> reftest_refact:run([{test, [list_to_atom(FileName)]}]);
        []           -> list_files(TestDirFiles)%;
%        FileNames    -> list_files(FileNames)
    end;
test(_)->
    message("::(atom() | proplist())"),
    usage.

%% @doc Runs a specific test case of a transformation.
%% If the name of the test case is numeric, it can be given as an integer.
%% The name of the transformation can be abbreviated up to ambiguity.
test(Mod, Case) when is_atom(Mod), is_integer(Case) ->
    CaseList = integer_to_list(Case),
    CaseAtom =
        if
            Case < 10 -> list_to_atom([$0|CaseList]);
            true      -> list_to_atom(CaseList)
        end,
    test(Mod, CaseAtom);
test(Mod, Case) when is_atom(Mod), is_atom(Case) ->
    {ok, TestDirFiles} = file:list_dir(filename:join(["..", "test"])),
    ModList = atom_to_list(Mod),
    CaseList = atom_to_list(Case),
    case [F || F <- lists:usort(TestDirFiles), lists:prefix(ModList, F)] of
        [ModName|_] ->
            {ok, TestDir2} = file:list_dir(filename:join(["..", "test", ModName])),
            case lists:member(CaseList, TestDir2) of
                false ->
                    list_files(TestDir2);
                true ->
                    reftest_refact:run([{test, [{list_to_atom(ModName), Case}]}])
            end;
        []         -> list_files(TestDirFiles)%;
        %%FileNames  -> list_files(FileNames)
    end;
test(_,_)->
    message("::(atom(), atom() | integer())"),
    usage.


%% @doc Lists the files of a directory.
list_files(AllFiles) ->
    io:format("Possible parameters:~n"),
    Files = lists:usort(AllFiles) -- [".svn", "DESC"],
    [io:format("    ~s~n", [File]) || File <- Files],
    missing_dir.


%% @doc Returns the list of differences after files have been loaded.
%%      This function does NOT consider forms with load errors.
%% @todo Parallelising this operation does not seem to bring any speedup.
cat_errors() ->
    % todo [file] should have an interface function
    Files = [File || File <- ?Query:exec([file]),
                     not has_errors(File),
                     File =/= error],
    io:format("~B files ", [length(Files)]),
    {TimeDiff, Results} =
        timer:tc(fun() -> pmap(fun cat_errors_with_display/1, Files) end, []),
    {H,M,S} = calendar:seconds_to_time(TimeDiff div 1000000),
    io:format("~nchecked in ~B hours, ~B minutes, ~B seconds~n", [H,M,S]),
    case [{File, Err} || {File, Err} <- Results, Err =/= ok] of
        []     -> no_cat_errors;
        Errors ->
            {ErrFiles, _} = lists:unzip(Errors),
            ErrFileNames = [(?Graph:data(File))#file.path || File <- ErrFiles, File =/= error],
            io:format("Errors found in the following files:~n    ~p~n", [ErrFileNames]),
            Errors
    end.

has_errors(File) ->
    Types = [(?ESG:data(Form))#form.type || Form <- ?Query:exec(File, ?File:real_forms())],
    lists:member(error, Types).


%% @doc Returns the list of differences between the original and the database
%% version of a file.
cat_errors(File) ->
    ?FileMan:create_scanner(),
    #file{path=Path, eol=Eol} = ?ESG:data(File),
    {FileText, _EOL} = ?FileMan:file_text(Path),
    FileHashWForms = [{Hash, tokens_to_text(Tokens, Eol)}
                        || {Hash, Tokens} <- ?FileMan:tokenize(FileText)],
    GForms = ?Query:exec(File, ?File:real_forms()),

    GFileText = ?Syn:flat_text(File),
    case {GFileText == FileText, nows(GFileText) == nows(FileText)} of
        {true, true} ->
            ok;
        {false, true} ->
            ws_diff;
        {false, false} ->
            case length(GForms) == length(FileHashWForms) of
                false -> {not_matching_form_count, length(GForms), length(FileHashWForms)};
                true ->
                    MatchingForms =
                        [{{GForm, (?ESG:data(GForm))#form.hash}, {{FHash, FText}, Idx}}
                         || {GForm, {{FHash, FText}, Idx}} <-
                          lists:zip(GForms, ?MISC:index_list(FileHashWForms))],
                    case [{Idx, GForm, FText}
                            ||  {{GForm, Hash}, {{FHash, FText}, Idx}} <- MatchingForms,
                                Hash =/= FHash] of
                        BadForms = [_|_] ->
                            {changed_forms, BadForms};
                        [] ->
                            MForms =
                                [{Idx, GForm, flat_text(GForm), FText}
                                    || {{GForm, _Hash}, {{_FHash, FText}, Idx}} <- MatchingForms],
                            AllDiffForms =
                                [{Idx, GForm} || {Idx, GForm, GFText, FText} <- MForms,
                                                 not is_similar(GFText, FText)],
                            WSDiffForms =
                                [{Idx, GForm} || {Idx, GForm, GFText, FText} <- MForms,
                                                 not is_similar(nows(GFText), nows(FText))],
                            DiffForms = AllDiffForms -- WSDiffForms,

                            case {DiffForms, WSDiffForms} of
                                {[], []} ->
                                    ok;
                                _ ->
                                    {{diffs, DiffForms}, {ws_diffs, WSDiffForms}}
                            end
                    end
            end
    end.

nows(Text) ->
    re:replace(Text, "[ \n\t\r]+", " ", [global, {return, list}]).

is_similar(invalid_children, _) ->
    false;
is_similar(_, invalid_children) ->
    false;
is_similar(Txt1, Txt2) ->
    Txt1 == Txt2.


tokens_to_text(Tokens, Eol) ->
    ?FileMan:orig_text(Eol, lists:flatten([Pre ++ Text ++ Post ||
     #token{prews=Pre, text=Text, postws=Post} <- Tokens])).

%% @todo Move to ?Syn.
flat_text(Form) ->
    try
        lists:flatten(?Syn:tree_text(Form))
    catch
        throw:{invalid_children, _, _} -> invalid_children
    end.




cat_errors_with_display(File) ->
    Ret = cat_errors(File),
    ErrOrSuccStr =
        case Ret of
            ok -> "o";
            _  -> "X"
        end,
    io:put_chars(ErrOrSuccStr),
    {File, Ret}.


pmap(Fun, Xs) ->
    S = self(),
    [spawn(fun() -> pmap_f(S, Fun, X) end) || X <- Xs],
    pmap_gather(length(Xs)).

pmap_gather(0) ->
    [];
pmap_gather(N) ->
    receive
        {pmap, _, Ret} ->
            [Ret|pmap_gather(N-1)]
    end.

pmap_f(Parent, Fun, X) ->
    Parent ! {pmap, self(), (catch Fun(X))}.

%% -----------------------------------------------------------------------------

modsave(Mod) ->
    refcore_store_graph:save(Mod).

modload(Mod) ->
    refcore_store_graph:load(Mod).

%% -----------------------------------------------------------------------------

%% @doc Start nitrogen with default configuration.
start_nitrogen() ->
    try
        ?NITRO_CORE:start_nitrogen()
    catch
        E={Module,_, _} when is_atom(Module) -> 
            message(?Error:error_text(E)),
            error; 
        exit:Error when is_list(Error)->
            message(Error),
            error
    end.

%% @doc Configure and start nitrogen.
start_nitrogen(Proplist) ->
    try
        ?NITRO_CORE:start_nitrogen(Proplist)
    catch
        E={Module,_, _} when is_atom(Module) -> 
            message(?Error:error_text(E)),
            error; 
        exit:Error when is_list(Error)->
            message(Error),
            error
    end.

%% @doc Stop nitrogen.
stop_nitrogen() ->
    ?NITRO_CORE:stop_nitrogen().

%% -----------------------------------------------------------------------------

%% @doc Start the web2 interface with default configuration.
start_web2() ->
    try
        ?WEB2:start()
    catch
        E={Module,_, _} when is_atom(Module) ->
            message(?Error:error_text(E)),
            error;
        exit:Error when is_list(Error)->
            message(Error),
            error
    end.

%% @doc Configure and start the web2 interface.
start_web2(Proplist) ->
    try
        ?WEB2:start(Proplist)
    catch
        E={Module,_, _} when is_atom(Module) ->
            message(?Error:error_text(E)),
            error;
        exit:Error when is_list(Error)->
            message(Error),
            error
    end.

%% @doc Stop the web2 interface.
stop_web2() ->
    ?WEB2:stop().

%% -----------------------------------------------------------------------------

%% @spec draw_dep(Options::proplists()) -> any()
%% @doc Draws out the dependency graph (in either module or function
%% level) into a dot file. <br/>
%% See {@link print_dep/1} for Options.
draw_dep(Options) ->
    draw_dep_graph(Options).

%% @spec print_dep(Options::proplists()) -> any()

%% @doc Prints out the dependency graph in either module or function level.
%%
%% Options:
%% ```
%% {level, Level}
%% Level = mod | func | mb
%% '''
%%     Determines the examination level: module or function.
%%
%% ```
%% {type, Type}
%% Type = all | cycles
%% '''
%% Deteremines whether the printing or the drawing is on the entire graph, or just on the cycle subgraph
%% (if there is). Subgraph from a given function or module should be given with the <b>starting_nodes</b> key.
%% ```
%% {starting_nodes, Node | NodeList::lists()}
%% Node = node() | Name
%% Name = Module::atom() | Function::string()
%% '''
%% Subgraph from the given node on both levels.
%% A function can be specified (as "Module:Function/Arity" or
%% as a graph node) to only look at cycles touching that fun.
%% Multiple nodes/names can be given in a list.
%% Note, that if the node is not correctly given, in the type of the cycles ({type, cycles}),
%% the query will run on the whole graph.
%% ```
%% {file_path, Path::string()}
%% '''
%%    User defined .dot name. Can be given as a simple .dot file, then the result will be put into
%% the ./dep_files directory, or as an absolute path, when the user can define the exact place.
%% @end

print_dep(Options) when is_list(Options)->
    case proplists:get_value(output, Options) of
        undefined  -> ui({draw_dep_graph, Options ++ [{output,name_terms}]});
        _          -> ui({draw_dep_graph, Options})
    end.
   
draw_dep_h()->
    message("draw_dep(Options::proplists())
        Dependency graph representation in function or module or modulblock level. The result is a .dot file.


        The Options are the following
        - {level, Level}, Level = mod | func | mb
        - {type, Type}, Type = all | cycles 
        - {starting_nodes, [Identifier]},
                Identifier = node() | Module() | Function()
                Module = atom() | string() | Regexp::string()
                Function = atom() | string() | Regexp::string()
        - {file_path, Path::string()}
        - {exclude, [Identifier]}
        - {exclude_children, [Identifier]}
        - {exclude_otp, ExcludeOtp}, ExcludeOtp = true | false 
        - {exclude_lib, [Path]},
                Path = string() | Regexp::string()
        - {connection, {ArcType, Depth, [Identifier]}} | {connection, [Identifier]},
                ArcType = direct | indirect
                Depth = integer()
        - {output, Output},
                Output = simple_dot | nomods_dot

        On moduleblock level
        - {groups, [Group]},
                Group = Path::string() | [Module] | Regexp::string()
                Name = atom() | string()

        Examples:
        - ri:draw_dep([{level, mod}, {starting_nodes, [erlang]}]).
        - ri:draw_dep([{level, mod}, {starting_nodes, [erlang]}, {file_path, \"/home/working/dot/test.dot\" }]).
        - ri:draw_dep([{level, func}, {starting_nodes, [\"lists:hd/1\"]}]).
        - ri:draw_dep([{level, mod},{starting_nodes, [ri, ris]}]).
        - ri:draw_dep([{type, cycles}, {level, func}, {starting_nodes, [\"ri:q/1\", \"ri:draw_dep/1\"]}]).
        - ri:draw_dep([{type, all}, {level, func}, {starting_nodes, [\"ri:q/1\"]}, {exclude_otp, true}, {file_path, \"/tmp/res.dot\"}]).
        - ri:draw_dep([{type, all}, {level, mod}, {exclude_otp, true}, {exclude, [ri, ris]}, {exclude_children, [reflib_ui]}]).").

print_dep_h()->
    message("print_dep(Options::proplists())
        Prints out the dependency graph in either function or module or modulblock level.


        The Options are the following on all level
        - {level, Level}, Level = mod | func | mb
        - {type, Type}, Type = all | cycles
        - {output, Output},
                Output = name_terms | node_terms | terms
                
        The Options are the following on function and module level
        - {starting_nodes, [Identifier]},
                Identifier = node() | Module() | Function()
                Module = atom() | string() | Regexp::string()
                Function = atom() | string() | Regexp::string()
        - {exclude, Identifier | [Identifier]}
        - {exclude_children, [Identifier]}
        - {exclude_otp, ExcludeOtp}, ExcludeOtp = true | false
        - {exclude_lib, [Path]},
                Path = string() | Regexp::string()
        - {connection, {ArcType, Depth, [Identifier]}} | {connection, [Identifier]},
                ArcType = direct | indirect
                Depth = integer()

        On moduleblock level
        - {groups, [Group]},
                Group = Path::string() | [Module] | Regexp::string()

        Examples:
        - ri:print_dep([{level, func}, {type, all}]).
        - ri:print_dep([{level, mod}, {type, cycles}]).
        - ri:print_dep([{level, func}, {type, all}, {starting_nodes, [\"ri:print_dep_h/0\"]}]).
        - ri:print_dep([{type, cycles}, {level, func}, {starting_nodes, [\"ri:print_dep_h/0\", \"ri:draw_dep_h/0\"]}]).
        - ri:print_dep([{type, all}, {level, func}, {starting_nodes, [\"ri:q/1\"]}, {exclude_otp, true}]).
        - ri:print_dep([{type, all}, {level, mod}, {exclude_otp, true}, {exclude, [ri, ris]}, {exclude_children, [reflib_ui]}]).").

%%---------------------------------------------------------------------------
%% @spec dir_sort()-> [{Directory, [Modules]}] | {error, no_list_given} |
%%                    {error, no_modules_in_database}
%% Directory = string()
%% Modules = atom()
%% @doc  Directory sorting for all modules added to the database.
%% The modules are sorted in a list according to their directory in which they are in.
%% Those modules which don't have a directory are put into the "Other" category.
dir_sort()->
    up_ui(ui({dir_sort})).

%%---------------------------------------------------------------------------
%% @spec dir_sort(Options::proplists())-> [{string(), [Modules]}] | {error, no_list_given} |
%%                    {error, no_modules_in_database}
%% Modules = atom()
%% @doc Directory sorting for a given list of modules added to the database.
%% The modules are sorted in a list according to their directory in which they are in.
%% Those modules which don't have a directory are put into the "Other" category.
%% Modules can be given with their names or as nodes.
%% Can be done in a recursive way also (modules of subdirectories are also included).
%%
%% The Options are the following:
%% ```
%% {type, Type}
%% Type = mod | dir
%% '''
%% Filtering according to given modules (mod) or directories (dir).
%%
%% ```
%% {mod_list, ModList}
%% ModList = [node() | atom()]
%% '''
%% Directory filtering on these modules given as nodes or with their names.
%%
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
%% Option working for only dir type filtering.
%% The sorting should be done in a recursive way, meaning that a directory (besides
%% its direct modules) will get the modules of its subdirectories in the result module list.

dir_sort(Options)->
    case proplists:get_value(type, Options) of
        undefined -> {error, no_type_key_given};
        mod ->up_ui(ui({dir_sort_get_dirs, proplists:get_value(mod_list, Options)}));
        dir -> up_ui(ui({dir_sort_get_mods, proplists:delete(type, Options)}));
        _ -> {error, wrong_type_key}
    end.

dir_sort_h()->
    message("
    dir_sort()
    Directory sorting for all modules added to the database.

    dir_sort(Options::proplists())
    Directory sorting for a given list of modules added to the database.

    - {type, Type}, Type = mod | dir
    - {mod_list, ModList}, ModList = [node() | atom()]
    - {dir_list, DirList}, DirList = [string()]
    - {recursive, Recursive}, Recursive = true | false)").


%% ------------------------------------------------------------------------------

%% @doc Inputs of the function are two lists. The elements of the first list
%% define architecture layers as group of modules. Every group of modules must
%% have a name. At default functions in a group can call functions from the
%% same group or from the group directly under own group. But the
%% second list defines additional permits, allowing function
%% calls between two architecture layers given by it's names. After this an
%% input can look like this: [{l1,[mod1,mod2]},{l2,[mod3,mod4,mod5]},{l3,[mod6]}],
%% [{l1,l2}]. The function checks wether in an architecture defined by the
%% input are functions, that insult the hierarchy or not. The output is
%% an empty list if function calls are allowed in the defined architecture
%% or list of disabled function calls in the following format:
%% {error|[{caller fun's mod name:caller fun's name/caller fun's arity,
%% called fun's module name:called fun's name/called fun's arity}, ..]}.
check_layered_arch(ModLists, ArchList) ->
    case  ui({if_layers_show_insults, ModLists, ArchList}) of
        error -> error;
        {ok, {ModNameList,InsultCalls}} ->
            io:format("Modules of each interface:",[]),
            message(ModNameList),
            io:format("Insulting function calls: ~n",[]),
            message(InsultCalls)
    end.

%% @doc Visualises the layers in a graph with the layer violations highlighted.
%% Check {@link check_layered_architecture/2} for detailed parameters. 
%% The result is a .dot file, called layered_arch.dot.
%% Same as calling ```show_layered_arch(LayerList, ArchList, "layered_arch.dot").'''
%% @end
show_layered_arch(ModLists, ArchList)->
    case ui({if_layers_draw, ModLists, ArchList}) of
        error ->
            error;
        {ok, Result} -> Result
    end.

%% @doc Same as {@link show_layered_arch/2}, only the user can give the name of the
%%  generated .dot file.
%% @end
show_layered_arch(ModLists, ArchList, Filename)->
    case ui({if_layers_draw, ModLists, ArchList, Filename}) of
        error ->
            error;
        {ok, Result} -> Result
    end.
    
check_layered_arch_h()->
    message("check_layered_arch(ModLists, ArchList)->any()\n"
           "Outputs the functions that violate the layer access restrictions.\n"
           "ModLists defines architecture layers as a groups of modules.\n"
           "ModLists = [{LayerName:atom(), [Module:atom()]}]\n"
           "ArchList defines additional permits, allowing function calls "
           "between two architecture layers given by it's names\n"
           "ArchList = [{LayerName1:atom(), LayerName2:atom()}]\n").

show_layered_arch_h()->
    message("show_layered_arch(ModLists, ArchList)->any()\n"
           "show_layered_arch(ModLists, ArchList, Filename)\n"
           "Visualises the layers in a graph "
           "with the layer violations highlighted.\n"
           "ModLists defines architecture layers as a groups of modules.\n"
           "ModLists = [{LayerName:atom(), [Module:atom()]}]\n"
           "ArchList defines additional permits, allowing function calls "
           "between two architecture layers given by it's names\n"
           "ArchList = [{LayerName1:atom(), LayerName2:atom()}]\n"
           "FileName is the name of the result, which is a .dot file\n"
           "FileName = string()\n").

%% ------------------------------------------------------------------------------

draw_gen_server(Module, ToFile) ->
    reflib_draw_gen_server:draw(Module, ToFile).

draw_gen_fsm(Module, ToFile) ->
    reflib_draw_gen_fsm:draw(Module, ToFile).


show_dupcode_h()->
    message("show_dupcode(Name)\n"
           "show_dupcode(Name, Format)\n"
           "Returns the clone search result with the name Name, "
           "using Format format.\n"
           "If no Format is specified then the result will be displayed\n"
           "with the original format.\n\n"
           "The Options are the following:\n"
           "Name :: atom()\n"
           "Format :: file_and_loc | prettyprint | nodes\n").

show_dupcode(Name) when is_atom(Name) ->
    show_dupcode(Name, undef);
show_dupcode(_) ->
    show_dupcode_h().

show_dupcode(Name, Format)
        when is_atom(Name) andalso is_atom(Format)->
    case up_ui(ui({get_dupcode_result, Name, Format, undef})) of 
        error -> error;
        [] -> io:format("No result with the name ~p.~n",[Name]);
        Result -> NumOfClones = proplists:get_value(detected_clones_num,Result),
                  Clones = proplists:get_value(detected_clones, Result),
                  StringClones = ?MISC:any_to_string(Clones),
                  io:format("Clone detection result:~n~s~n~n",[StringClones]),
                  io:format("Contains ~p clone groups...~n",[NumOfClones]),
                  io:format("run ri:show_dupcode_group(~p, GroupNumber::integer()) 
                             to see one of the clone groups.~n",[Name])
    end,
    ok;
show_dupcode(_,_) ->
    show_dupcode_h().

show_dupcode_group_h()->
    message("show_dupcode_group(Name, Group)\n"
           "show_dupcode_group(Name, Group, Format)\n"
           "Returns the clone search result with the name Name's, "
           "Group-th group, using Format format.\n"
           "If no Format is specified then the result will be displayed\n"
           "with the original format.\n\n"
           "The Options are the following:\n"
           "Name :: atom()\n"
           "Group :: integer()\n"
           "Format :: file_and_loc | prettyprint | nodes\n").

show_dupcode_group(Name, GroupNumber) when is_atom(Name), is_integer(GroupNumber) ->
    show_dupcode_group(Name,GroupNumber, undef);
show_dupcode_group(_,_) ->
    show_dupcode_group_h().

show_dupcode_group(Name, GroupNumber, Format)
        when is_atom(Name), is_integer(GroupNumber),
             is_atom(Format)->
    case up_ui(ui({get_dupcode_group, Name, GroupNumber, Format})) of
        error -> error;
        {false, undef_name} -> io:format("No result with the name ~p. ~n",[Name]);
        {true, Result} -> StringClone = ?MISC:any_to_string(Result),
                          io:format("~s~n",[StringClone])
    end,
    ok;
show_dupcode_group(_,_,_) ->
    show_dupcode_group_h().




stored_dupcode_results_h()->
    message("stored_dupcode_results()\n"
           "Returns every clone search result's name and the used Options,\n"
           "and the number of clone groups.\n\n").

stored_dupcode_results()->
    case up_ui(ui({get_all_dupcode_result})) of
        error -> error;
        [] -> io:format("No duplicated searches found.~n");
        Result -> 
            lists:map(fun(K) -> 
                [Names, Opts, GroupNum] = K,
                io:format("Stored result found with name(s): ~n"),
                lists:map(fun(Ki) ->
                    io:format("~s~n",[Ki])
                end, Names),
                io:format("Used options: ~p.~n",[Opts]),
                io:format("Found clone groups:~p~n~n",[GroupNum]),
                io:format("~s~n",[string:concat(string:copies("-",80),"\n")])
            end, Result)
    end,
    ok.

save_dupcode_result_h()->
    message("save_dupcode_result(Name, Path)\n"
           "show_dupcode_result(Name, Path, Format)\n"
           "Saves the clone search result with the name Name, "
           "to the file specified in Path using Format format.\n"
           "If no Format is specified then the result will be saved "
           "with the original format.\n\n"
           "The Options are the following:\n"
           "Name :: atom()\n"
           "Path :: string()\n"
           "Format :: file_and_loc | prettyprint | nodes\n").

save_dupcode_result(Name, Filename) when is_atom(Name) ->
    save_dupcode_result(Name, Filename, undef);
save_dupcode_result(_,_) -> 
    save_dupcode_result_h().

save_dupcode_result(Name, Filename, Format) when is_atom(Name), is_atom(Format),
                                                 is_list(Filename) ->
    case up_ui(ui({get_dupcode_result, Name, Format})) of 
        error -> error;
        [] -> io:format("No result with the name ~p.~n",[Name]);
        Result -> NumOfClones = proplists:get_value(detected_clones_num,Result),
                  Clones = proplists:get_value(detected_clones, Result),
                  StringClones = ?MISC:any_to_string(Clones),
                  FileString = io_lib:format("Clone detection result:~n~s~n~n",[StringClones]) ++
                              io_lib:format("Contains ~p clone groups...~n",[NumOfClones]),
                  {ok,IODev} = file:open(Filename, [write]),
                  io:put_chars(IODev, FileString),
                  ok = file:close(IODev),
                  message("Results have been written into " ++ Filename ++ "\n")
    end,
    ok;
save_dupcode_result(_,_,_) ->  
    save_dupcode_result_h().

clone_identifierl_h()->
    message("clone_identifierl()\n"
        "Starts duplicated code analysis with default options on the complete database.\n"
        "The default options are the following.\n"
        "[{algorithm, sw_metrics}, {cache, false}]\n\n"
        "clone_identifierl(Option :: proplist())\n"
        "Starts duplicated code analysis with the given options.\n"
        "The most important options (for the detailed list, visit the wiki site of RefactorErl):\n"
        "-{algorithm, Algorithm}, Algorithm =  matrix | sw_metrics | suffix_tree | \n"
        "                                      filtered_suffix_tree | matrixfilter\n"
        "-{name, Name}, Name :: atom()\n"
        "-{format, Format}, Format = prettyprint | file_and_loc | nodes\n"
        "-{func_list, List},List = [{Module::atom(),Function::atom(),Arity::integer()}]\n"
        "-{files, Files}, Files = [module :: atom() | filepath :: string() |\n"
        "                          regexp :: string()]\n"
        "-{minlen, Minlen}, Minlen :: integer()\n\n"
        "By default, the results appear in your console.\n" 
        "To dump the results into a file, use the following option:\n"
        "-{output, Output}, Output :: atom()\n"
        "Note that not all options are available for every algorithm(see the wiki page).\n\n"
        "Examples:\n"
        "-ri:clone_identifierl([{files, [alg_lib,matrix]},{name, dup1},{output, dups}]).\n"
        "-ri:clone_identifierl([{algorithm, suffix_tree},{format, file_and_loc},{minlen, 40}]).\n"
        "-ri:clone_identifierl([{algorithm, matrix},{diff_limit, 0.2}]).\n\n"
        "For further details, please, refer to the corresponding manual page"
        "(http://pnyf.inf.elte.hu/trac/refactorerl/wiki/CloneIdentifiErl) of our official wiki site.\n").


clone_identifierl()->
    clone_identifierl([]).

clone_identifierl(Options)->
    case up_ui(ui({clone_identifierl, Options})) of
        error -> 
            error;
        ResultList when is_list(ResultList)->
            NumOfCands = proplists:get_value(analysed_candidates_num, ResultList),
            NumOfClones = proplists:get_value(detected_clones_num, ResultList),
            Clones = proplists:get_value(detected_clones, ResultList),
            Name = proplists:get_value(clone_name, ResultList),
            Result = [?MISC:any_to_string(Clones),
                     "\nName: ",
                     io_lib:format("~p",[Name]),
                     "\nAnalysed pairs: ",
                     io_lib:format("~p",[NumOfCands]),
                     " pcs\n",
                     "Detected clones: ",
                     integer_to_list(NumOfClones),
                     " pcs\n"],
            message2(Result),
            case proplists:get_value(output_file_path, ResultList) of
                "" -> 
                    ok;
                Path when is_list(Path) ->
                    message("Results have been written into " ++ Path ++ "\n")
            end,
            ok
    end.

%% @doc Eliminates a previously stored duplicate code group.
elimdupcode(Name, Group)
        when is_atom(Name) andalso is_integer(Group) ->
    transform(dupcode, [{clone_name,Name}, {group,Group}]);
elimdupcode(_, _) ->
    elimdupcode_h().

elimdupcode_h() ->
    message(["Eliminates a previously stored duplicate code search's group.\n",
             "Currently the filtered_suffix_tree algorithm with\n",
             "max_inv_seq_length set to 0 is supported.\n",
            "ri:elimdupcode(Name :: string(), Group :: integer())."]).


    
%% @doc sigma.min.js graph module
%% See refusr_smart_graph.erl generate/1 for details.
    
generate_smart_graph_h() ->
    message("generate(Options::proplist()) -> {result, Result::string()}\n\n" ++
"Options contain the generating options.\n\n" ++
"Options must contain: dependency_options, level\n\n" ++
"output_path is optional. If this property is not given, a random" ++
"file will be generated, otherwise the result will be in the given" ++
"file.\n\n" ++

"Possible generating options:\n" ++
"      dependency_options:: proplist()\n" ++
"      Possible dependency_options: \n" ++
"          level: mod | func\n" ++
"          type: all | cyclic\n" ++
"          starting_nodes: list()\n" ++
"          exclude: list()\n" ++
"          exclude_children: list()\n" ++
"          exclude_otp: true | false\n" ++
"          exclude_lib: list()\n" ++
"      dependency_level: func | mod\n" ++
"      output_path: string()\n\n" ++

 "Examples:\n" ++
"    DepOpts = [{level, mod}, {type, all}, {starting_nodes, [refusr_cyclic_mod]}," ++
              "{exclude, []}, {exclude_children, []}, {exclude_otp, false}].\n" ++
"    ri:generate_smart_graph([" ++
        "{dependency_level, mod}, {dependency_options, DepOpts}]).\n\n" ++
"    ri:generate_smart_graph([" ++
        "{dependency_options, [{type, all}]}, {dependency_level, func}," ++
        "{output_path, \"/tmp/alma.html\"}]).\n").
    
generate_smart_graph(Options) ->
    XOptions = Options ++ [{output_type, js_with_html}],
    Result =  up_ui(ui({generate_smart_graph, XOptions})),
    case Result of
        {result, Path} ->
            message("Open a browser and go to file://" ++ Path ++ " to see the result.");
        Else ->
            Else
    end.

start_wx_h() ->
    message("Type start_wx() or start_wx(UserName :: string()) to start Wx Interface.").

%% spawn( fun() -> ri:add(usr,mnesia) end), timer:sleep(100), 

start_wx() -> start_wx_({none}).
start_wx(UserName) -> start_wx_(UserName).

start_wx_(UserName) ->
    case ?UI:can_read_db() of
        false -> io:format("\nThe database is busy. Try again later.\n",[]),
                 error;
        true  -> case start_proper_wx(UserName) of
                      {error, Error} -> message(Error);
                      ignore         -> io:format("\nThe database is busy. Try again later.\n",[]);
                      _              -> ok
                 end
    end.

start_proper_wx({none}) -> referl_wx:start();
start_proper_wx(UserName) -> referl_wx:start(UserName).

start_qt_h() ->
    message("Type start_qt() to start Qt Gui").

start_qt() ->
    UserName = atom_to_list(node()),
    start_qt(UserName).

start_qt(UserName) ->
    case ?UI:can_read_db() of
        false -> io:format("~nThe database is busy. Try again later.~n"),
                error;
        true ->
            referl_qt:start(UserName)
    end.

%% @doc Interface for deadcode analysis.<br/>
%% Mode can be:<br/>
%% <ul>
%%     <li>print - Prints result</li>
%%     <li>term - Gives result as a term, a list of {Type,Text,File,Line}</li>
%% </ul><br/>
%% Format of Include and Exclude lists:<br/>
%% <ul>
%% <li>List = ["M" | "M:F" | "M:F/A"]</li>
%% <li>M = * | atom (Module name or regular expression)</li>
%% <li>F = * | atom (Function name or regular expression)</li>
%% <li>A = * | integer | [integers]</li>
%% </ul>
%% @spec deadcode() -> any()
deadcode() -> 
    ui({deadcode,print}).
%% @spec deadcode(list()) -> any()
deadcode(Files) -> 
    ui({deadcode,Files}).
%% @spec deadcode(list(),atom()) -> any()
deadcode(Files,Mode) -> 
    ui({deadcode,Files,Mode}).
%% @spec deadcode_interface(list(),list(),atom()) -> any()
deadcode_interface(Include,Exclude,Mode) -> 
    ui({deadcode_interface,Include,Exclude,Mode}).

deadcode_h()->
    message(
        "Interface for deadcode analysis.\n"
        "ri:deadcode().\n"
        "ri:deadcode(Files).\n"
        "ri:deadcode(Files,Mode).\n"
        "ri:deadcode_interface(Include,Exclude,Mode).\n"
        "Mode can be:\n"
        "* print - Prints result\n"
        "* term - Gives result as a term, a list of {Type,Text,File,Line}\n"
        "Format of Include and Exclude lists:\n"
        "List = [\"M\" | \"M:F\" | \"M:F/A\"]\n"
        "M = * | atom (Module name or regular expression)\n"
        "F = * | atom (Function name or regular expression)\n"
        "A = * | integer | [integers]\n").

deadcode_interface_h()->
    deadcode_h().

%% -----------------------------------------------------------------------------
%% @doc Draw dependency graph
%% For more details see refusr_dep_graph.erl draw/1 function
%% @spec draw_dep_graph(Parameters::proplist()) -> string() | list()
draw_dep_graph(PropList)->
    ui({draw_dep_graph,PropList}).

%% =====================================7
%% Interface for file cache server

generate_all(Directory) ->
    up_ui(ui({generate_all, Directory})).
