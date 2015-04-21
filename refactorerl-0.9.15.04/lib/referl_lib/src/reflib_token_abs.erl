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
%%% @doc This module implements an absolute positioning using data given by the
%%% scanner.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Gabor Hosszu

-module(reflib_token_abs).

-behaviour(reflib_token_gen).

-export([map_pos/3,foldpos/5, containing_form/2, form_length/1]).

-include("lib.hrl").

foldpos(Fun, Acc0, File, Ws, PosType) ->
    Tokens = ?Syn:leaves(File), 
    Start  = case PosType of
                 scalar -> 1;
                 linecol -> {1,1}
             end,
    foldpos0(Fun, Acc0, Tokens, Ws, Start, PosType).

foldpos0(Fun, Acc0, Tokens, none, Start, PosType)->
    foldpos1_none(Fun, Acc0, Tokens, Start, PosType);

foldpos0(Fun, Acc0, Tokens, Ws, Start, PosType) ->
    WsMask = ?Token:ws2mask(Ws),
    StMask = lists:takewhile(fun ?Token:id/1, ?MISC:map_not(WsMask)),
    foldpos1(Fun, {WsMask,StMask}, Acc0, Tokens, Start, PosType).

%% @private
foldpos1(_, _, Acc, [], _, _) ->
    Acc;
%@todo perhaps also support linecol
foldpos1(Fun, WsM={WsMask,StMask}, Acc, [Head|Tail], Pos, PosType=scalar) ->
    #lex{data=Data} = ?ESG:data(Head),
    Lens  = ?Token:toklens(Data),
    Start = Pos   + ?Token:sum_mask(StMask,Lens),
    End   = Start + ?Token:sum_mask(WsMask,Lens) - 1,
    case Fun(Head, Data, Start, End, Acc) of
        {stop, Result} ->
            Result;
        {next, Acc1}   ->
            Next = Pos + lists:sum(Lens),
            foldpos1(Fun, WsM, Acc1, Tail, Next, PosType)
    end.

%% "Premature optimization is the root of all evil."
foldpos1_none(_, Acc, [], _, _) ->
    Acc;
foldpos1_none(Fun, Acc, [Head|Tail], Pos, PosType=scalar) -> 
    #lex{data=Data} = ?ESG:data(Head),
    #token{prews=Pre, text=Text} = Data,
    Start = Pos   + length(Pre),
    End   = Start + length(Text) - 1,
    case Fun(Head, Data, Start, End, Acc) of
        {stop, Result} ->
            Result;
        {next, Acc1}   ->
            Next = End + 1 + length(Data#token.postws),
            foldpos1_none(Fun, Acc1, Tail, Next, PosType)
    end;

foldpos1_none(Fun, Acc, [Head|Tail], Pos, PosType=linecol) ->
    #lex{data=Data} = ?ESG:data(Head),
    #token{prews=Pre, text=Text} = Data,
    Start = ?Token:lc_aggr(Pos,Pre), %@todo hm, could take a pretty PosType argument
    {InitT,LastT} = lists:split(length(Text)-1,Text),
    End   = ?Token:lc_aggr(Start,InitT),
    case Fun(Head, Data, Start, End, Acc) of
        {stop, Result} ->
            Result;
        {next, Acc1}   ->
            Next = ?Token:lc_aggr(End,[LastT|Data#token.postws]),
            foldpos1_none(Fun, Acc1, Tail, Next, PosType)
    end.
    
map_pos(File,Tokens,scalar) ->
			lists:map(fun(Token)-> {Token, getpos(File,Token,scalar)} end, Tokens);
map_pos(File,Tokens,linecol) -> 
			lists:map(fun(Token)-> {Token, getpos(File,Token,linecol)} end, Tokens);
map_pos(File,Tokens,both) -> 
			lists:map(fun(Token)-> {Token, getpos(File,Token,both)} end, Tokens).

getpos(_File,Token,scalar) ->
	case (?Graph:data(Token)) of
    	#lex{ type=token, data=(#token{ scalar=Sc }) } -> Sc;
		_ -> not_found %% TODO
	end;
getpos(_File,Token,linecol) ->
	case (?Graph:data(Token)) of
    	#lex{ type=token, data=(#token{ linecol=Lc }) } -> Lc;
		_ -> not_found %% TODO
	end;
getpos(_File,Token,both) ->
	case (?Graph:data(Token)) of
    	#lex{ type=token, data=(#token{ scalar=Sc, linecol=Lc }) } -> {Sc, Lc};
		_ -> not_found %% TODO
	end.
	
containing_form([], _Pos) ->
    not_found;

containing_form([Form|Forms], Pos) ->
    case ?ESG:data(Form) of
        #form{hash=virtual} ->
            containing_form(Forms, Pos);
        #form{form_length=FL} when Pos >= FL ->
            containing_form(Forms, Pos - FL);
        _ ->
            {Form, Pos}
    end.

%% In this case we use this record field to store the length of the form, and
%% NOT the starting scalar position of the form.
form_length(Form) ->
    (?Graph:data(Form))#form.form_length.
