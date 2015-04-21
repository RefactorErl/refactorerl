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
%%% @doc This module implements a relative positioning. The absolute position of
%%% forms is stored and a relative (offset) position is also stored for tokens.
%%% 
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Daniel Horpacsi
%%% @author Gabor Hosszu

-module(reflib_token_rel).

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
    
    
map_pos(File, Tokens, Opt) ->
    TokensParentForm = ets:new(tpform, [bag]),
    FormsPosData    = ets:new(formspdata, [bag]),
    ets_put_form_positions(File, FormsPosData, Opt),
    try
        [ets:insert(TokensParentForm, {parent_n_child, ?Token:token2form(Token), Token}) || Token <- Tokens],
        pos4tokens_wforms(ets:tab2list(TokensParentForm), FormsPosData, Opt)
    after
      ets:delete(TokensParentForm),
      ets:delete(FormsPosData)
    end.


ets_put_form_positions(File, FormsPosData, P) ->
    Forms = ?Query:exec(File, ?File:real_forms()),
    lists:map(
    	fun(Form) ->
    		Pos =
			case P of
				scalar ->
    				#form{start_scalar=StSc} = ?ESG:data(Form),
    				StSc;
				linecol->
    				#form{start_line=Line} = ?ESG:data(Form),
    				Line;
    				_  ->
    				#form{start_scalar=StSc, start_line=Line} = ?ESG:data(Form),
    				{StSc, Line}
			end,
    		ets:insert(FormsPosData, {f2p, Form, Pos})
    	end, Forms).


pos4tokens_wforms([], _Form2Pos, _PosType) ->
    [];
pos4tokens_wforms([{parent_n_child, Form, Token}|Rest], FormsPosData, Opt) ->
    {F2Tokens, Rest2} = lists:splitwith(fun({parent_n_child, F, _}) -> F == Form end, Rest),
    Tokens = [Token | [T || {parent_n_child, _, T} <- F2Tokens]],
    [{f2p, Form, FPos}] = ets:match_object(FormsPosData, {f2p, Form, '_'}),
      Adjusted = [ { Tok , token_pos_set(Tok, FPos, Opt)  }  || Tok <- Tokens  ], 
    Adjusted ++ pos4tokens_wforms(Rest2, FormsPosData, Opt).

token_pos_set(Token,{ScStart, StartLine},both) ->
	Sc = token_pos_set(Token,ScStart,scalar),
	Lc = token_pos_set(Token,StartLine,linecol),
	{Sc, Lc};	

token_pos_set(Token, StartLine,linecol) ->
	case (?Graph:data(Token))#lex.data of
		#token{ linecol= {{SL, SC}, {EL, EC}} } 
			 -> 
				NewBegin = {StartLine + SL, SC},
				NewEnd = {StartLine + EL, EC},
				{NewBegin, NewEnd};
		_	->  not_found
	end;

token_pos_set(Token,ScStart,scalar) ->
	case (?Graph:data(Token))#lex.data of
		#token{ scalar= {P1, P2} } 
		                       -> { ScStart + P1, ScStart + P2 };
		_					   -> not_found
	end.

containing_form([], _Pos) ->
    not_found;
containing_form([Form|Forms], Pos) ->
    case ?ESG:data(Form) of
        #form{hash=virtual} ->
            containing_form(Forms, Pos);
		A					->
			Len = formlength_relpos(Form,A),
			case Len < Pos of
				true 	->  containing_form(Forms, Pos - Len);
				false 	->	{Form, Pos}
			end
    end.
    
%%	Calculate form length in case of relative token positioning
%%	(a little bit complex method...)
formlength_relpos(Form,FormData) ->
	case FormData of
		#form{start_scalar=_StSc} ->
					GetLastLeaf = ?Syn:last_leaf(),
					[LastLeaf]    = GetLastLeaf(Form),
					#lex{ data=#token{ scalar={_,Pos}, postws=PostWs } } = ?ESG:data(LastLeaf),
					1 + Pos + length(PostWs);
		_				  -> -1
	end.

form_length(Form) ->
    formlength_relpos(Form, ?ESG:data(Form)).
