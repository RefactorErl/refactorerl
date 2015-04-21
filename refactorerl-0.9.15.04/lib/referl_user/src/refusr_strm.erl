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

%%% @doc Calculates the value of one of the following metrics for the given strings.
%%% Available metrics:
%%% <ul>
%%% <li> Levenshtein </li>
%%% <li> Soundex </li>
%%% <li> Dice / Sorensen </li>
%%% </ul>

%%% @author Gabor Olah <olikas.g@gmail.com>
%%% @author Viktoria Fordos <f-viktoria@elte.hu>

-module(refusr_strm).
-vsn("$Rev: 10106 $"). %for emacs"



-include("user.hrl").

-export([ getDistance/2 ]).
-export([ levenshtein/2 , soundex/1, dice_sorensen/2 ]).
 
%% ============================================================================

%% @spec getDistance(string(), string()) -> {'lev', integer()}
%% @doc Calculates the distance between two strings
getDistance([],X) when is_list(X) ->
    {'lev', levenshtein([],X)};
getDistance(X,[]) when is_list(X) ->
    {'lev', levenshtein(X,[])};
getDistance(X,Y) when is_list(X) and is_list(Y) ->
    {'lev', levenshtein(X,Y)}.

%% ============================================================================
%% @spec dice_sorensen(string(),string()) -> float()
%% @doc Calculates the similarity, based on Dice/Sorensen metric, 
%% between the two given strings. 
%% Dice / Sorensen
%% 'http://en.wikipedia.org/wiki/Dice%27s_coefficient'
%% When taken as a string similarity measure, the coefficient may be calculated 
%% for two strings, x and y using bigrams as follows:
%% s = (2*nt/(nx+ny))
%% where nt is the number of character bigrams found in both strings, 
%% nx is the number of bigrams in string x and 
%% ny is the number of bigrams in string y. 
%% For example, to calculate the similarity between:
%% night
%% nacht
%% We would find the set of bigrams in each word:
%% {ni,ig,gh,ht}
%% {na,ac,ch,ht}
%% Each set has four elements, and the intersection of these two sets 
%% has only one element: ht.
%% Inserting these numbers into the formula, we calculate, 
%% s = (2 · 1) / (4 + 4) = 0.25.
dice_sorensen([], _) ->
    0;
dice_sorensen(_, []) ->
    0;
dice_sorensen(X,X) when is_list(X) andalso is_list(X)->
    1.0;
dice_sorensen(X,Y) when is_list(X) andalso is_list(Y)->
    BX = get_ngrams_set(2, X),
    BY = get_ngrams_set(2, Y),
    NT = intersection_size(BX,BY),
    2 * NT / (sets:size(BX) + sets:size(BY)).


intersection_size(Set1, Set2) ->
    sets:size(sets:intersection(Set1, Set2)).

get_ngrams_set(N, List) ->
    sets:from_list(get_ngrams(N, List)).

get_ngrams(_, [])->
    [];

get_ngrams(N, List) when is_list(List) andalso is_integer(N) andalso N > 0 ->
    get_ngrams0(N, List, []).

get_ngrams0(N, List, Acc0) when length(List) =< N ->
    Acc0 ++ [List];
get_ngrams0(N, List = [_|Tail], Acc0)->
    get_ngrams0(N, Tail, Acc0 ++ [lists:sublist(List, 1, N)]).

%% @spec levenshtein(string(), string()) -> integer()
%% @doc Calculates the Levenshtein distance between two strings
levenshtein(Samestring, Samestring) -> 0;
levenshtein(String, []) -> length(String);
levenshtein([], String) -> length(String);
levenshtein(Source, Target) ->
    levenshtein_rec(Source, Target, lists:seq(0, length(Target)), 1).

%% @private
%% Recurses over every character in the source string
%% and calculates a list of distances
levenshtein_rec([SrcHead|SrcTail], Target, DistList, Step) ->
    levenshtein_rec(SrcTail,
                    Target,
                    levenshtein_distlist(Target,
                                         DistList,
                                         SrcHead,
                                         [Step],
                                         Step),
                    Step + 1);
levenshtein_rec([], _, DistList, _) ->
    lists:last(DistList).

%% @private
%% Generates a distance list with distance values for every character
%% in the target string
levenshtein_distlist([TargetHead|TargetTail],
                     [DLH|DLT],
                     SourceChar,
                     NewDistList,
                     LastDist) when length(DLT) > 0 ->
    Min = lists:min([LastDist + 1,
                     hd(DLT) + 1,
                     DLH + dif(TargetHead, SourceChar)]),
    levenshtein_distlist(TargetTail,
                         DLT,
                         SourceChar,
                         NewDistList ++ [Min],
                         Min);
levenshtein_distlist([], _, _, NewDistList, _) ->
    NewDistList.

%% @private
%% Calculates the difference between two characters or other values
dif(C, C) -> 0;
dif(_C1, _C2) -> 1.


%% @spec soundex(string()) -> list()
%% @doc Returns the soundex numeric code of a string. The generation algorithm
%% is the following.
%% <ol>
%%    <li>Turn string to uppercase and separate the first letter</li>
%%    <li> Make the character changes to the uppercase string made in
%%           fun changeLetters/1. </li>
%%    <li>Make the remain string uniq</li>
%%    <li>Leave out zeros</li>
%% </ol>

soundex(Input) ->
    [Head|Tail] = string:to_upper( Input),
    Post = uniq(lists:map(fun changeLetters/1,Tail)),
    [Head|lists:filter(fun(X) -> X /= 0 end,
                       lists:flatten(Post))].


%% @private
changeLetters(C) when (C == $A);(C==$E);(C==$I);(C==$O);
                      (C==$U);(C==$H);(C==$H);(C==$Y)->
    0;
changeLetters(C) when (C==$B);(C==$F);(C==$P);(C==$V) ->
    1;
changeLetters(C) when (C==$C);(C==$G);(C==$J);(C==$K);
                      (C==$Q);(C==$S);(C==$X);(C==$Z) ->
    2;
changeLetters(C) when (C==$D);(C==$T) ->
    3;
changeLetters(C) when (C==$L) ->
    4;
changeLetters(C) when (C==$M);(C==$N) ->
    5;
changeLetters(C) when (C==$R) ->
    6;
changeLetters(_) ->
    {error, not_meant_to_be_implemented}.

%% @private
uniq([]) ->
    [];
uniq([X]) ->
    [X];
uniq([Head|Tail]) ->
    uniqacc(Head,Tail).

uniqacc(Head,[Head|Tail]) ->
    uniqacc(Head,Tail);
uniqacc(Head,[]) ->
    [Head];
uniqacc(Head,[NotHead|Tail]) ->
    [Head,uniq([NotHead|Tail])].


%% Deprecated functions

% searchByPos(File, Pos) ->
%     searchByPos(File, Pos, 1).
%
% searchByPos(File, Pos, Limit) ->
%     try ?Args:variable([{file, File}, {position, Pos}]) of
%         Var -> getVar(Var,Limit)
%     catch
%         {_, pos_bad_type, [variable, _ ]} ->
%             try ?Args:function([{file, File}, {position, Pos}]) of
%                 Fun -> getFun( atom_to_list(?Fun:name(Fun)) , Limit)
%             catch
%                 _ -> {error, not_fun}
%             end;
%         _ -> {error, not_var_fun}
%     end.
%
% getVar(Var,Limit) ->
%     VarName = ?Var:name(Var),
%     Scopes = ?Query:exec([Var], ?Query:seq(?Var:scopes(),
%                                            ?Clause:variables())),
%     DistList = [{VarName,getDistance(VarName, ?Var:name(X))}|| X<-Scopes],
%     lists:filter(fun({_, {_, N}}) ->
%                              N =< Limit
%                  end,
%                      DistList).
%
% searchFunction(Name) when is_list(Name) ->
%     getFun(Name, 1).
%
% searchFunction(Name, Limit) when is_list(Name) ->
%     getFun(Name, Limit).
%
% getFun(FunName, Limit) when is_list(FunName) ->
%     Functions = ?Query:exec(?Query:seq(?Mod:all(),?Mod:locals())),
%     DistList = lists:map(fun(Func) ->
%                                  FuncName = ?Fun:name(Func),
%                                  {FuncName,
%                                   getDistance(FunName,
%                                               atom_to_list(FuncName))}
%                          end, Functions),
%     lists:filter(fun({_, {_, N}}) ->
%                              N =< Limit
%                  end,
%                      DistList).

% getDistance2(X,Y) when (is_list(X) and is_list(Y)) ->
%    A=levenshtein(X,Y),
%    B=levenshtein(soundex(X),soundex(Y)),
%    case A<B of
%        true ->
%            {lev,A};
%        _ ->
%            {sou,B}
%    end.

