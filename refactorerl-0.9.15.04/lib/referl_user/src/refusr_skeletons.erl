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

%%% @author Viktoria Fordos <v@fviktoria.hu>

-module(refusr_skeletons).

-export([update_skeleton/3, update_prev_skeleton_comment/2, delete_skeleton/1,
         skeleton_call_format/1, save_skeleton/3, list_skeletons/0, 
         evaluate_skeleton/3, evaluate_skeleton/4, try_parse_skeleton/2,
         determine_sq_request_type/1, do_autocomplete_skeleton/1,
         even_list_elements/1, parse_skeleton/1]).

-define(SPARAM_START, $$).
-define(SPARAM_END, $$).

-define(NITRO_ERROR, nitrogen_helper_error).
-define(S_TAB, 
    filename:join([?MISC:data_dir(),
                   "skeleton_table_v2"++
                   ?MISC:graph_based_postfix()])).
-vsn("$Rev: 10547 $ ").

-include("user.hrl").

%%% ============================================================================
%%% Skeletons
%% @doc Replaces the body and the owner of the skeleton, 
%% which is identified by the given name,
%% with the given new body and with the given owner.
%% @spec update_skeleton(Name::string(), NewBody::string(), Owner::string())->
%% ok | {error, ErrorMessage::string()}
update_skeleton(Name=[A|_],NewBody=[B|_],Owner=[C|_]) when is_integer(A) andalso
    is_integer(B) andalso is_integer(C) ->
    try
        check_skeleton_syntax(NewBody),
        [{_,_,_,_,Comment}] = find_in_stab_by_name(Name),
        save_safe_skeleton(Name, NewBody, Owner, Comment)
    catch
        {?MODULE, _Type, ErrorMessage}->{error, ErrorMessage};
        _:_ -> {error, "Unknown error occured"}
    end.

%% @doc Updates the comment of a previously stored skeleton based on 
%% the given skeleton name.
%% @spec update_prev_skeleton_comment(Name::string(), NewComment::string())-> ok
update_prev_skeleton_comment(Name=[A|_], NewComment) when is_integer(A)
    andalso is_list(NewComment)->
    [{_,Body,Owner,_Card,_}] = find_in_stab_by_name(Name),
    save_safe_skeleton(Name, Body, Owner, NewComment).

%% @doc Deletes the skeleton which is identified by the given name
%% @spec delete_skeleton(Name::string())-> ok
delete_skeleton(Name)->
    delete_from_stab({Name, '_', '_', '_', '_'}).

%% @doc Composes a valid skeleton call based on the given skeleton name.
%% @spec skeleton_call_format(Name::string())->string()
skeleton_call_format(Name=[C|_]) when is_integer(C)->
    [{_,_,_,Card,_}] = find_in_stab_by_name(Name),
    ParamStr=case Card of
                 0 -> "";
                 1 -> " `_` ";
                 N -> string:copies(" `_` ,", N-1)++" `_` "
             end,
    Name++"("++ParamStr++").".

%% @doc Saves a new skeleton from the given parameters.
%% @spec save_skeleton(Name::string(), Body::string(), Owner::string())->
%% ok | {error, ErrorMessage::string()}
save_skeleton(Name=[A|_], Body=[B|_], Owner=[C|_]) when is_integer(A) andalso
    is_integer(B) andalso is_integer(C) ->
    case validate_skeleton(Name, Body) of
        ok -> save_safe_skeleton(Name, Body, Owner, "");
        Err={error, _} -> Err
    end.

%% @doc Returns a list of the data of the stored skeletons.
%% @spec list_skeletons()->[]|[Skeleton]
%% Skeleton = {Name, Body, Owner, ParamCardinality, Comment}
%% Name = string()
%% Body = string()
%% Owner = string()
%% ParamCardinality = integer()
%% Comment = string()
list_skeletons()->
    get_stab_elements().

%% @doc Evaluate the skeleton, which is identified by the given name, with the
%% given actual parameters
%% This function gives a {'query', Query, User} tuple if it managed to evaluate
%% the skeleton or a {error, Error} tuple if an error occured
%% @spec evaluate_skeleton(Name::string(), Parameters, User::string()) ->
%% {error, Error} | {'query', Query, User}
%% Error = string()

evaluate_skeleton(Name=[A|_], Parameters, User=[B|_]) when is_integer(A) 
  andalso is_integer(B) andalso is_list(Parameters)->
    eval_skeleton_helper(Name, Parameters, fun(I) -> {'query', I, User} end).

evaluate_skeleton(Name=[A|_], Parameters, _User, onlyconvert) when is_integer(A) 
  andalso is_list(Parameters)-> 
    eval_skeleton_helper(Name, Parameters, fun(I) -> I end).

eval_skeleton_helper(Name, Parameters, FunToRun) ->
    try
        Query = eval_converter_helper(Name, Parameters),
        FunToRun(Query)
    catch
        {?MODULE, _Type, ErrorMessage}-> {error,ErrorMessage};
        {_, _} -> {error,"Unknown error occured"}
    end.

eval_converter_helper(Name, Parameters) ->
        case find_in_stab_by_name(Name) of
            [] -> throw(?LocalError(skel_not_found, "Skeleton was not found."));
            [Result={_, Body, _, _, _}] -> 
                check_skeleton_params_match(Result, Parameters),
                convert_to_sq(Body, Parameters)  
        end.

parse_skeleton(QueryText)->
    {ok, Elements, _}=erl_scan:string(QueryText),
    {_,_, SkelName}=hd(Elements),
    ParamsList=re:split(QueryText,"[``]",[{return,list}]),
    Parameters=case length(ParamsList) of
                   1 ->[]; %skeleton can contain 0 parameters
                   _ -> even_list_elements(ParamsList)
               end,
    {atom_to_list(SkelName), Parameters}.

%%% ============================================================================
%%% helper functions
do_autocomplete_skeleton(Call)->
    lists:foldl(fun({Name,_,_,_,_}, Acc)->
                    FSkel = skeleton_call_format(Name),
                    case lists:prefix(Call, FSkel) of
                        true-> Acc++[{FSkel, FSkel}];
                        false -> Acc
                    end
                end, [], lists:keysort(1, list_skeletons())).
    
determine_sq_request_type(Call=[C|_]) when is_integer(C)->
    try
        check_skeleton_call_syntax(Call),
        skeleton_name_registered(Call),
        skeleton
    catch
        {_, _, _} -> sem_query
    end.

%% Gives query ready to run: {'query', Query, User} (or error)
%% See evaluate_skeleton for more details
try_parse_skeleton({Call, _File, _Pos}, User)->
    {ok, Elements, _}=erl_scan:string(Call),
    {_,_, SkelName}=hd(Elements),
    ParamsList=re:split(Call,"[``]",[{return,list}]),
    Parameters=case length(ParamsList) of
                   1 ->[]; %skeleton can contain 0 parameters
                   _ -> even_list_elements(ParamsList)
               end,
    evaluate_skeleton(atom_to_list(SkelName), Parameters, User).

check_skeleton_params_match({_,_,_,ParamCard,_}, Params) when is_list(Params)->
    case length(Params)==ParamCard of
        true-> ok;
        false -> throw(?LocalError(skel_card_error, 
                        "Cardinality of the actual parameters does not 
                        match the cardinality of the formal parameters."))                       
    end.

convert_to_sq(Body, []) -> Body;

convert_to_sq(Body=[A|_], [HParam|Params]) when is_integer(A) 
  andalso is_list(HParam) andalso is_list(Params)->
    Pattern="(["++[?SPARAM_START]++"][^"++[?SPARAM_END]++
            "]*["++[?SPARAM_END]++"])",
    {ok, Mp}=re:compile(Pattern,[]),
    NewBody=re:replace(Body,Mp,HParam,[{return, list}]),
    convert_to_sq(NewBody, Params).

save_safe_skeleton(Name=[A|_], Body=[B|_], Owner=[C|_], Comment) 
    when is_integer(A) andalso is_integer(B) 
    andalso is_integer(C) andalso is_list(Comment)->
    ParamCardinality=countSkeletonParameters(Body),
    insert_to_stab(Name, Body, Owner, ParamCardinality, Comment).

validate_skeleton(Name=[A|_], Body=[B|_]) 
    when is_integer(A) andalso is_integer(B)->
    try
        check_skeleton_syntax(Body),
        has_unique_skeleton_name(Name),
        ok
    catch
        {?MODULE, _Type, ErrorMessage}->{error, ErrorMessage};
        {_, _} -> {error, "Unknown error occured"}
    end;

validate_skeleton(_,_)->
    {error, "Bad args"}.

%This clause should be used when the start and end symbols 
%of the skeleton's parameters are not equal to each other.
%countSkeletonParameters(Body=[A|_]) 
%     when is_integer(A) andalso ?SPARAM_START/=?SPARAM_END ->
%    StartSymbols=lists:filter(fun(E)->E==?SPARAM_START end, Body),
%    length(StartSymbols);

countSkeletonParameters(Body=[A|_]) 
    when is_integer(A) andalso ?SPARAM_START==?SPARAM_END ->
    StartSymbols=lists:filter(fun(E)->E==?SPARAM_START end, Body),
    length(StartSymbols) div 2.

%This clause should be used when the start and end symbols of the 
%skeleton's parameters are not equal to each other.
%check_skeleton_syntax(Body=[A|_]) 
%     when is_integer(A) andalso ?SPARAM_START/=?SPARAM_END->
%    StartSymbols=lists:filter(fun(E)->E==?SPARAM_START end, Body),
%    EndSymbols=lists:filter(fun(E)->E==?SPARAM_END end, Body),
%    case length(StartSymbols)==length(EndSymbols) of
%        true-> ok;
%        false -> throw({?NITRO_ERROR,
%                        "Cardinality of the start symbol does not match
%                          the cardinality of the end symbol."})
%    end;

check_skeleton_syntax(Body=[A|_]) 
    when is_integer(A) andalso ?SPARAM_START==?SPARAM_END->
    Symbols=lists:filter(fun(E)->E==?SPARAM_START end, Body),
    case even(length(Symbols)) of
        true-> ok;
        false -> throw(?LocalError(skel_card_error, 
                        "Cardinality of the start symbol does not match
                         the cardinality of the end symbol."))
    end.

skeleton_name_registered(Call)->
    Name=lists:takewhile(fun( $( )->false; (_)-> true end, Call),
    case length(find_in_stab_by_name(Name)) of
        0 -> throw(?LocalError(skel_not_found, 
                        Name++" does not exist as a skeleton name."));
        _ -> ok
    end.

skeleton_call_pattern()->
    Name="[^()`\t\n .]+",
    Open="[(]",
    Close="[)]",
    Dot="\.",
    NullParam="[\t\n ]*",
    Param="([\t\n ]*`[^`]*`[\t\n ]*)",
    Params=Param++"(,"++Param++")*",
    ParamList="("++NullParam++"|"++Params++")",
    Name++Open++ParamList++Close++Dot.

check_skeleton_call_syntax(Call=[C|_]) when is_integer(C)->
    {ok, Mp}=re:compile(skeleton_call_pattern(),[]),
    case re:run(Call,Mp) of
        nomatch -> throw(?LocalError(skel_syn_error, "Syntax error in skeleton."));
        _ -> ok
    end.

has_unique_skeleton_name(Name=[A|_]) when is_integer(A)->
    case length(find_in_stab_by_name(Name)) of
        0 -> ok;
        _ -> throw(?LocalError(skel_already_exists, 
                               Name++" already exists as a skeleton name."))
    end.

%%% ============================================================================
%%% Low- level operations on 'skeleton_table' dets table
insert_to_stab(Name,Body, Owner, ParamCardinality, Comment) ->
    dets:open_file(?S_TAB,[]),
    dets:insert(?S_TAB,{Name, Body, Owner, ParamCardinality, Comment}),
    dets:close(?S_TAB).

find_in_stab_by_name(Name) ->
    dets:open_file(?S_TAB,[]),
    Result=dets:match_object(?S_TAB,{Name,'_','_','_','_'}),
    dets:close(?S_TAB),
    Result.

get_stab_elements()->
    dets:open_file(?S_TAB,[]),
    Result=dets:match_object(?S_TAB,{'_','_','_','_','_'}),
    dets:close(?S_TAB),
    Result.
    
delete_from_stab(RowPattern) ->
    dets:open_file(?S_TAB,[]),
    dets:match_delete(?S_TAB,RowPattern),
    dets:close(?S_TAB).

even(A) when is_integer(A)->
    A rem 2 == 0.
    
even_list_elements(List) -> even_list_elements_(List,false).
even_list_elements_([],_) -> [];
even_list_elements_([E|ES],Flag) when Flag ->
    [E|even_list_elements_(ES,false)];
even_list_elements_([_|ES],_) ->
    even_list_elements_(ES,true).	   
