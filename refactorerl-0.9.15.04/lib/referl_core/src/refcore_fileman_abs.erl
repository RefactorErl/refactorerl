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

%%% @doc File handler interface module for absolute positioning. This module
%%% converts between the textual and graph representation of files 
%%% (parsing and saving).
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Robert Kitlei <kitlei@inf.elte.hu>
%%% @author Gabor Hosszu

-module(refcore_fileman_abs).
-vsn("$Rev: 12913 $"). % for emacs"

-behaviour(refcore_fileman_gen).
-include("core.hrl").

-export([update/6, handle_incons/2, make_token/4, parse_error_msg/6]).

handle_incons(File, FormWTexts) ->
    %% handling inconsistency
    case set_form_len_and_hash_for_all(File, FormWTexts, none) of
       	none -> ok;
    	FormIndex -> handle_incons_file(File, FormIndex)
    end.

%% Calculates form hash for all forms, and returns the index of the first form 
%% which changed
%% In Erlang min(none,Number) equals Number and we use this.
%% We set form_length to the Length of the form
%%	
set_form_len_and_hash_for_all(_File, [], FstIncons) ->
	FstIncons;

set_form_len_and_hash_for_all(File, [ {Form, Text} | FormWTextsTail ], FstIncons) ->
	FormIndex = set_form_len_and_hash(File, Form, Text),
	set_form_len_and_hash_for_all(File, FormWTextsTail, min(FstIncons,FormIndex) ).

set_form_len_and_hash(File, Form, Text) ->
	Hash = ?FileMan:form_hash(Text),
	case ?FileMan:form_hash_neq(Form,Hash) of
		false -> none;
		true ->
			LT = length(Text),
			?FileMan:set_form_hash(Form, Hash),
    		set_form_length(Form, LT),
    		?ESG:index(File,form,Form)
    end.


%%% ============================================================================
%%% Form level manipulations

%%% private type
%%% formAction() = {ins, inputForm()} |
%%%                {hold, Form::node()} |
%%%                {del, Form::node()}

%%	check() = nocheck | check
%%
%% @spec update(node(), [node()], [formAction()], progress(), old|new, check() ) -> ok
%%
%% @doc Updates the contents of `File' by applying the elements of `Actions'.
%% `Forms' is the result of `real_forms(File)' (it is always computed before
%% calling `update'). `Progress' specifies what kind of progress reporting
%% should be done.
%% Check means if we need token position consistency check and update or not

update(File, _Forms, Actions, Progress, OldOrNew, Check) ->
    ALs = [?FileMan:action_length(Action) || Action <- Actions],
    {Lengths, TotalLength} = lists:mapfoldl(fun(X, S) -> {X + S, X + S} end, 0, ALs),
    #file{type=FileType, path=Path} = ?Graph:data(File),
    Now = os:timestamp(),
    ChkNum = 
    case Check of
    	check ->   none;
        nocheck -> -1 %% we use form number -1 as an extremal form
    end,
    Result =
    update(Actions, 1, 0, start, File, FileType,
             ?FileMan:progress_start(Progress, Now, Path, Lengths, TotalLength, 
                                   ?FileMan:act_count(Actions)),OldOrNew, ChkNum),
    
    %update filehash 
    ?FileMan:set_file_hash(File, ?FileMan:file_hash(File)),
    Result.


%% FstIncons is a carried argument which is the index of the first inconsistent
%% form; IndDiff is the difference in the index of the real form in the file
%% and the index given by RefactorErl to the {form, Index} link
update([{hold, Form} | ATail], Index, IndDiff, PSt, File, FT, P, OldOrNew, 
                                                                 FstIncons) ->
    LL = length(?FileMan:dep_forms(Form)),
    Count = 1 + LL,
    NID = IndDiff + LL,
    update(ATail, Index+Count, NID, PSt, File, FT, P, OldOrNew, FstIncons);

update([{del, Form} | ATail], Index, IndDiff, PSt, File, FT, P, OldOrNew, 
                                                                FstIncons) ->
    Ind = ?ESG:index(File,form,Form),
    ?PreProc:detach(File, Form),
    [?ESG:remove(File, form, F) || F <- ?FileMan:dep_forms(Form)],
    ?ESG:remove(File, form, Form),
    update(ATail, Index, IndDiff, PSt, File, FT, ?FileMan:progress_step(P), 
           							OldOrNew, min(FstIncons,Ind-IndDiff));
    
update([{ins, {Hash, Input}} | ATail], Index, IndDiff, PSt, File, FT, P, 
                                                        OldOrNew, FstIncons) -> 
    {NewForms, PSt1} = ?PreProc:preprocess(Input, File, PSt, OldOrNew),
    NewFormIdxs = ?MISC:index_list(NewForms, Index),
    Lst = [add_form_at(FT, File, Hash, Index0, ProcessedTokens, Input)
             					 || {ProcessedTokens, Index0} <- NewFormIdxs],
	MinIndex = lists:min(Lst) - IndDiff,
    update(ATail, Index + length(NewForms), IndDiff, PSt1, File, FT, 
           		?FileMan:progress_step(P), OldOrNew, min(FstIncons,MinIndex) );
    
update([], _Index, _ID, _PSt, File, _FT, _P, _OldOrNew, FstIncons) ->
	case FstIncons of %% First inconsistent form is...
		none -> ok;
		A when A >= 0 	-> handle_incons_file(File, FstIncons);
		_A 				-> ok  %% -1 if we need no check
	end.
    %% here we could shut down the progress reporter if needed.

%% Parses and adds the form at `Index' to the file.
add_form_at(FT, File, Hash, Index, ProcessedTokens, Input) ->
    Form = ?FileMan:parse_form(File, Index, FT, ProcessedTokens),
    ?FileMan:set_form_hash(Form, Hash),
    set_form_length(Form, ProcessedTokens, Input),
    ?ESG:insert(File, {form, Index}, Form),
    Index.

%%
%% We set form_length to the Length of the form
%%

set_form_length(_Form, {vtokens, _, _, _}, _Input) -> 
    ok;
set_form_length(Form, _, TokenWPoss) ->
    Length = lists:sum([?FileMan:token_length(Token) || {Token, _} <- TokenWPoss]),
    set_form_length(Form, Length).
    
set_form_length(Form, Length) -> 
    ?Graph:update(Form, (?ESG:data(Form))#form{form_length=Length}).


%%% ============================================================================
%%% Token level manipulation

%% Create a token node
%% TODO: no ?Token call (move ?FileMan:build into ?Syn)
make_token(Type, Text, LPos={_LStart, _LEnd}, SPos={_SSt, _SEnd}) ->
    ?Token:buildwpos(Type, Text, LPos, SPos).


%% ===========================================================================================
%% Makes the error text more informative in cases when it is possible.
parse_error_msg(File, _Line, _FormIndex, _Tokens, ["syntax error before: ", ErrPosMsg], Origin) ->
    FlatTxt = ?MISC:format(ErrPosMsg) ++ ".",
    case ?MISC:string_to_term(FlatTxt) of
    	{ok, {_ErrToken = #token{text=Text, type=Type, linecol=LC}, _ErrLex}} ->
			case Type of
				'?' ->
				    % a macro for which no substitution could be found
				    % as it is already reported, no further info message is required
				    "";
				_ ->
					OrigFile = case Origin of
									module  -> File;
									header  -> File;
									_Form   -> hd(?Query:exec(Origin, ?Form:file()))
					end,
				    #file{path=Path} = 
					case OrigFile of
						File   -> ?Graph:data(File);
						_Other -> ?Graph:data(OrigFile)
					end,
				    {ErrLine, ErrCol} = element(1, LC),
				    Text2 = ?MISC:format(Text),
				    Text3 = re:replace(Text2, "\n", "\\\\n", [global, {return, list}]),
				    ?MISC:format("~s:~p:~p Parse error before " ++ Text3 ++ "~n",
				                         [Path, ErrLine, ErrCol])

			end;
		{error,{_Num,_ErrAtom,ErrText}} ->
    		?MISC:format(lists:flatten(ErrText) ++ "\n");
    	_Other ->
     		?MISC:format("Syntax error.\n")	
	end;

parse_error_msg(_File, _Index, Mod, _Tokens, Msg, _) ->
    ?MISC:format(Mod:format_error(Msg) ++ "\n").



%% =============================================================================

%% Scanner ~ refcore_erl_scanner:create()
%% Tokenize with a given (Scanner) function, see tokenize
%%
tokenizewith(Text,Scanner) ->
	Sc = Scanner(), 
    case Sc(Text, ?ErlScanner:init(fun make_token/4)) of
        {ok, Tokens} ->
			SplFrms = ?FileMan:split_forms(?FileMan:merge_ws(?FileMan:split_stop_tokens(Tokens))),
           [{?FileMan:form_hash(Form), Form} ||
                Form <- SplFrms];
        {error, {Ln, Mod, Error}, _Line} ->
            throw({Ln, Mod:format_error(Error)})
    end.

%%  Make a file consistent where there are improper token positions
%%  N is the index of the first inconsistent form
handle_incons_file(File, N) ->
    try
        OldTokens = get_original_incons_tokens(File,N),
        NewTokens = get_new_data4incons_tokens(File,N),
        true = (length(OldTokens) == length(NewTokens)),
        update_token_posdata(NewTokens,OldTokens),
        ok
    catch
        _:_ -> reload_file(File)
    end.

reload_file(File) ->
    try
        ?FileMan:drop_file(File),
        ?FileMan:add_file(File),
        ok
    catch
        _:_ ->  Err = log_incons_handling_error(File),
                {error, Err}
    end.

log_incons_handling_error(File) ->
    Err = try 
        "Cannot add file: " ++ ?File:path(File)
    catch
        _:_ -> "A file couldn't be handled properly."
    end,
    error_logger:info_msg(Err),
    Err.

%%	returns list of lexical nodes
get_original_incons_tokens(File,N) -> 
    Forms = ?FileMan:real_forms(File),
	InconsForms = lists:nthtail(N-1,Forms),
	OrigInconsTokens = [ refcore_syntax:leaves(IForm) || IForm <- InconsForms ], %% OrigInconsTokens
	lists:flatten(OrigInconsTokens).
	
%% returns list of token data tuples (records)
get_new_data4incons_tokens(File,N) ->
    FData = ?Graph:data(File),
    {Text, EOL} = ?FileMan:file_text(FData#file.path),
    ?Graph:update(File, FData#file{eol=EOL}),
	Forms = tokenizewith(Text, fun refcore_erl_scanner:create/0),
	NeededForms = lists:nthtail(N-1,Forms),
	NoHashNeededForms = [ NForm || { _Hash, NForm } <- NeededForms ],
	lists:flatten(NoHashNeededForms).

update_token_posdata(NewTokenData,OldTokens) ->
	TokenUpdatePairs = lists:zip(OldTokens,NewTokenData),
	Update = 
		fun(Token, {lex, token, Data}) ->
			#lex{ data=OldData} = ?Graph:data(Token),
			?Graph:update(Token,{lex, token, OldData#token{ scalar=Data#token.scalar, linecol=Data#token.linecol }}) 
						end,
	[ Update(Token,{lex,token,Data}) || {Token,Data} <- TokenUpdatePairs ].
	
