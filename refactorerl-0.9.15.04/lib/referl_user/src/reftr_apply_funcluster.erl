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
%%% The Initial Developer of the Original Code is E�tv�s Lor�nd University.
%%% Portions created  by E�tv�s Lor�nd University and ELTE-Soft Ltd.
%%% are Copyright 2007-2013 E�tv�s Lor�nd University, ELTE-Soft Ltd.
%%% and Ericsson Hungary. All Rights Reserved.

%%% ============================================================================
%%% Module information

%%% @doc This module implements moving functions with
%%% appendicies based on function clustering result.
%%%
%%% Old status: only one optional compensation is missing according to the
%%% current specification: renaming in case of appendix clash at a target.
%%% Current status: needs rewriting for the ui branch infrastructure, doesn't run
% TODO: add comments
% TODO: add edoc

%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>

-module(reftr_apply_funcluster).
-vsn("$Rev: 9568 $"). % for emacs "

-export([do/1]).
-export([get_operations/2,commit/1,main/0,
         commit/2,real_tr/0,test_tr/0]). %DEBUG
-export([error_text/2]).

-include("user.hrl").

%%% @private
error_text(inv_struc,[X]) ->
    ["Invalid structure: ", io_lib:print(X)];
error_text(file_exists,[F]) ->
    ["File '", F, "' already exists"];
error_text(cant_create_file,[F]) ->
    ["Can't create file '", F, "'"];
error_text(nofile_mod,[M]) ->
    ["Can't find the file for '", io_lib:print(M), "'"];
error_text(no_interaction,[Q]) ->
    ["No interaction is expected, but got ", io_lib:print(Q)];
error_text(deny, _)->
    ["The request was denied by the job server. Please, try again later."];

error_text(_,_) -> unknown.

%%% ----------------------------------------------------------------------------

-record(tr,{transform_do_2, transform_wait_0,
            graph_backup_0, addfile_1}).

-record(ops,{source,target,funs,usedrecmac}).

%-record(acts,{type,action}).
-record(movs,{type,source,target,items}).
-record(cre,{name}).
%-record(rens,{type,mod,old,new}).

%%% ============================================================================

ui(NameArgs)->
    ReqID = ?UI:getid(),
    case ?UI:request(ReqID,NameArgs) of
        ok -> ui_loop(ReqID);
        deny -> {error, ?LocalError(deny,[])}
    end.

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

%%% @private
do(Args) ->
    Clusters = ?Args:funclusters(Args),
    {ok,DestDir} = file:get_cwd(), %@todo
    _Re=catch_referr(
         fun()->
                 Res = get_operations(Clusters,DestDir),
                 ?d(Res),
                 commit(Res)
         end),
    ?d(_Re).

%%% @private
main() ->
    {ok,DestDir} = file:get_cwd(),
    catch_referr(
      fun()->
              Res = get_operations([[{src1,f,1},{src1,k,2},{src2,g,0}],
                                    [{src1,h,2}]],DestDir),
              _Log = commit(test_tr(), Res),
              ok
      end).

%%% ============================================================================

%%% @private
commit(Res) ->
    commit(real_tr(),Res).

%%% @private
commit(Tr,Moves) ->
    check_creates(Moves),
    (Tr#tr.graph_backup_0)(),
    do_creates(Tr,Moves) ++ do_moves(Tr,Moves).

check_creates(Moves) ->
%% TODO: also check in database
    [ case filelib:is_file(T) of
          true  -> throw(?LocalError(file_exists,[T]));
          false -> ok
      end
    || #cre{name=T} <- Moves].

do_creates(Tr,Moves) ->
    [ begin
        (Tr#tr.addfile_1)(T),
        {addfile_1, T}
      end
    || #cre{name=T} <- Moves].

do_moves(Tr,Moves) ->
    [begin
        {NameAtom,ListAtom,TrAtom} =
            case Type of
                rec  -> {filename,reclist,reftr_move_rec};
                mac  -> {filename,maclist,reftr_move_mac};
                func -> {name,    funlist,reftr_move_fun}
            end,
        Params = [{file,S},{NameAtom,T},{ListAtom,FL}],
        Res = (Tr#tr.transform_do_2)(TrAtom, Params), %TODO
%% TODO: handle errors
        {transform_do_2, TrAtom, Params, Res}
     end
    || #movs{type=Type,source=S,target=T,items=FL} <- Moves].

%do_wait() ->
%    ok.

%%% ----------------------------------------------------------------------------
%%% @private
real_tr() ->
    #tr{ transform_do_2   = fun ?Transform:do/2, %TODO
         graph_backup_0   = fun ?Graph:backup/0, %TODO
         addfile_1        = fun addfile/1 }.

%%% @private
test_tr() ->
    #tr{ transform_do_2   = fun test_do/2,
         graph_backup_0   = fun test_backup/0,
         addfile_1        = fun test_addfile/1 }.

addfile(Fil) when is_list(Fil) ->
    Data =
        case lists:suffix(".erl",Fil) of
            true ->
                Mod = filename:basename(Fil,".erl"),
                list_to_binary(["-module(",Mod,").\n"]);
            _    -> ""
        end,
    F    = filename:absname(Fil),
    case file:write_file(F,Data) of
        ok -> ok;
        _  -> throw(?LocalError(cant_create_file,[F]))
    end,
    ?d(add),
    case ui({add_dir,F}) of
        {error, E} -> throw(E);
        Result -> Result
    end,
    ?d(added).

test_do(A,B) ->
    io:format("?Transform:do(~p,~p)~n",[A,B]).

%test_wait() ->
%    io:format("?Transform:wait()~n").

test_backup() ->
    io:format("?Graph:backup()~n").

test_addfile(F) ->
    io:format("addfile(~p)~n",[F]).

catch_referr(Fun) when is_function(Fun,0) ->
    try
        Fun()
    catch
        E -> io:format("error: ~s~n",[?Error:error_text(E)]),
             error %{error,E}
    end.

%%% ============================================================================
%%% @private
get_operations(Clusters,Path) ->
    Ops    = get_fun_recmac(Clusters),
    Fun    = [ #movs{type=func,source=S,target=T,items=FL} ||
                 #ops{source=S,target=T,funs=FL} <- Ops ],
    RecMac = get_hrl_recmac(Path,Ops),
    NewRecMac = [check_hrl(HrlFile) || HrlFile <- RecMac],
    Movs   = NewRecMac ++ [ check_modfile(F) || F <- Fun],
%% TODO: renames if a clash would occur at the target site
    Create1 =
        [ begin
            Name = if is_atom(T) ->
                          filename:join(Path,atom_to_list(T)++".erl");
                      true       -> T
                   end,
            #cre{name=Name}
          end
        || #movs{target=T} <- Movs ],
    Create = lists:usort(Create1),
    Create ++ Movs.

check_hrl(HrlFile) ->
    FName = HrlFile#movs.target,
    case filelib:is_file(FName) of
        true  ->
            NewName    = ui_get_new_name(FName, "header"),
            % todo Is `FName' all right on the next line?
            %      Shouldn't it be `NewName'?
            Path       = filename:dirname(FName),
            NewHrl     = filename:join(Path, atom_to_list(NewName) ++ ".hrl"),
            _NewHrlFile = check_hrl(HrlFile#movs{target=NewHrl});
        false ->
            HrlFile
    end.

%% Gets a new name from the UI.
ui_get_new_name(FName, NameType) ->
    Info    = [{format,info},{text, FName ++ ": This file already exists!"}],
    Textbox =   [{format,textbox},
                 {text, "Please specify a new " ++ NameType ++ " name"},
                 {validator,file},
                 {default,-1}],
    Question = [Info, Textbox],
    [NewName] = ?Transform:question(Question),
    NewName.

check_modfile(ModFile) ->
    FileName = atom_to_list(ModFile#movs.target) ++ ".erl",
    case filelib:is_file(FileName) of
        true  ->
            NewName     = ui_get_new_name(FileName, "module"),
            _NewModFile = check_modfile(ModFile#movs{target=list_to_atom(NewName)});
        false ->
            ModFile
    end.

get_hrl_recmac(Path,Ops) ->
    Uses =
        [ {{S,E},T} ||
            #ops{source=S,target=T,usedrecmac=L} <- Ops,
            E <- L ],
    Targets   = multidict_from_list(Uses),
    Canonical =
        [ {lists:usort(dict:fetch(K,Targets)),K} ||
            K <- dict:fetch_keys(Targets) ],
    CanGroup = group(Canonical),
    Multiple = [ L || {[_,_|_],L} <- CanGroup ],
    autonumber(Multiple,
               fun(N,L) ->
                   T = filename:join(Path,
                                     "inc"++integer_to_list(N)++".hrl"),
                   ModObj  =
                       [ begin
                           {Type,Name} = get_type_name(Obj),
                           {{S,Type},Name}
                         end || {S,Obj} <- L ],
                   ModObjL = group(ModObj),
                   [ #movs{type=Type,source=S,target=T,items=ObjList} ||
                       {{S,Type},ObjList} <- ModObjL ]
               end).

get_fun_recmac(Clusters) ->
    try
      autonumber(Clusters,
        fun(N,Cluster) ->
            Target = list_to_atom("mod"++integer_to_list(N)),
            ModFun  = [  {M,{F,A}}
                      || {M,F,A} <- lists:flatten(Cluster) ],
            ModFunL = group(ModFun),
            [ begin
                Mod     = ?Query:exec1(?Mod:find(SourceM),
                                       ?RefError(mod_not_found,[SourceM])),
                RecMac  = get_funs_recmac(Mod,FunList),
                SourceF = ?Query:exec1(Mod,?Mod:file(),
                                       ?LocalError(nofile_mod,[SourceM])),
                Source  = ?File:path(SourceF),
                #ops{source = Source, target = Target,
                    funs = FunList, usedrecmac = RecMac }
              end
            || {SourceM,FunList} <- ModFunL ]
        end)
    catch
        error:function_clause ->
            throw(?LocalError(inv_struc,[Clusters]))
    end.

get_funs_recmac(Mod,FunList) ->
    RecMac  =
      [ begin
          Fun = ?Query:exec1(
                    [Mod],
                    ?Query:seq([
                        ?Fun:find(F,A),
                        ?Fun:definition()]),
                    ?RefError(fun_not_found,[F,A])),

          get_used_recmac(Mod,Fun)
        end
      || {F,A} <- FunList ],
    lists:flatten(RecMac).

get_used_recmac(Mod,Parent) ->
    get_used_recmac(Mod, Parent, ?File:records(), ?Rec:references()) ++
    get_used_recmac(Mod, Parent, ?File:macros(),  [{mref,back}]).

get_used_recmac(Mod,Parent,NodesPath,BackEdge) ->
    Nodes = ?Query:exec(Mod,
                        ?Query:seq([?Mod:file()]++[NodesPath])),
    Used  =
      [ begin
          Refs = ?Query:exec(Node, BackEdge),
          case lists:any(fun(N)->is_ancestor_of(Parent,N) end,Refs)  of
              true -> [Node];
              _    -> []
          end
        end
      || Node <- Nodes ],
    lists:flatten(Used).

%%% ----------------------------------------------------------------------------

autonumber(Ls,F) ->
    N_Ls = lists:zip(seq(1,length(Ls)),Ls),
    Res  = [ F(N,El) || {N,El} <- N_Ls ],
    lists:flatten(Res).

get_type_name(Obj) ->
    case ?Syn:class(Obj) of
        record -> {rec, ?Rec:name(Obj)};
        form   -> {mac, ?Macro:name(Obj)}
    end.

is_ancestor_of(Ancestor,Child) ->
    {_,OnPath} = lists:unzip(?Syn:root_path(Child)),
    lists:member(Ancestor,OnPath).

group(X) ->
    dict:to_list(multidict_from_list(X)).

multidict_from_list(L) ->
    Append = fun({K,V},D) -> dict:append(K,V,D) end,
    lists:foldl(Append,dict:new(),L).

seq(A,B) when A>B ->
    [];
seq(A,B) ->
    lists:seq(A,B).
