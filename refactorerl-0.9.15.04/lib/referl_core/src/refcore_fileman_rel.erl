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

%%% @doc File handler interface module for relative positioning. This module
%%% converts between the textual and graph representation of files 
%%% (parsing and saving).
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Robert Kitlei <kitlei@inf.elte.hu>
%%% @author Gabor Hosszu

-module(refcore_fileman_rel).
-vsn("$Rev: 12913 $"). % for emacs"


-behaviour(refcore_fileman_gen).
-include("core.hrl").

-export([update/6, handle_incons/2, make_token/4, parse_error_msg/6]).
-export([tokenlist_to_linecol/1]).

handle_incons(_File, FormWTexts) ->
	set_formlist_abs_pos(FormWTexts, 1, 1).
%%

%%	Make forms consistent: set its hash, and if needed recalculate tokens relative positions
%%    + set_form_hash
%%
%%	TODO: this function could carry a true/false atom, which could mean if there was a form 
%%		with improper hash
%%  ColsInLastLine0 is the number of characters in the last line of the previous form,
%%  We have to use this in order to calculate the column property of those tokens (in
%%  the current form) which can be found in the last line (row) of the previous form
set_formlist_abs_pos(List, ActSc, ActLine) ->
	set_formlist_abs_pos(List, ActSc, ActLine, 0).

set_formlist_abs_pos([], _ActSc, _ActLine, _ColsInLastLine0) ->
	ok;

set_formlist_abs_pos([ {Form, Text} | Tail ], ActSc, ActLine, ColsInLastLine0) ->
	Hash = ?FileMan:form_hash(Text),
	case ?FileMan:form_hash_neq(Form,Hash) of
		false -> ok;
		true ->
			?FileMan:set_form_hash(Form, Hash),
			Tokens = [ { extract_data(TNode) , TNode } || TNode <- ?Syn:leaves(Form) ],
			calc_rel_pos(Tokens, ColsInLastLine0)	
    end,
    set_form_abspos(Form, ActSc, ActLine),
	{PlusLines, ColsInLastLine} = text_to_lc(Text),
    set_formlist_abs_pos(Tail, ActSc + length(Text), ActLine + PlusLines,
                                                     ColsInLastLine).


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

update(File, _Forms, Actions, Progress, OldOrNew, _Check) ->
    ALs = [?FileMan:action_length(Action) || Action <- Actions],
    {Lengths, TotalLength} = lists:mapfoldl(fun(X, S) -> {X + S, X + S} end, 0, ALs),
    #file{type=FileType, path=Path} = ?Graph:data(File),
    Now = os:timestamp(),
    %% The last 3 args are beginning scalar and linecol for forms, and 
    %% 0 because there were 0 characters/cols in the last line of the previous form
    %% (there was NO previos form)
    Result = update(Actions, 1, start, File, FileType,
           ?FileMan:progress_start(Progress, Now, Path, Lengths, TotalLength, 
                                   ?FileMan:act_count(Actions)),
           OldOrNew, 1, 1, 0), 
        %update filehash 
    ?FileMan:set_file_hash(File, ?FileMan:file_hash(File)),
    Result.

%%
%%	ActScalar is the absolute scalar position of the current form
%%	ActLine - absolute line
%%  LastCol - how many characters were there in the last line of the previous form?
%%      we should start this with 0!
%% 
update([{hold, Form} | ATail], Index, PSt, File, FT, P, OldOrNew, ActScalar, ActLine, LastCol) ->
    Count = 1 + length(?FileMan:dep_forms(Form)),
    set_form_abspos(Form, ActScalar, ActLine),
    LastToken = getlastleaf(Form),
    Data = extract_data(LastToken),
    case Data#token.scalar of
    	undefined ->
    		update(ATail, Index+Count, PSt, File, FT, P, OldOrNew, ActScalar, ActLine, LastCol);
    	_ -> %% We just get the next position
			 { _ , EndSC } = Data#token.scalar,
			 { _ , EndLC} = Data#token.linecol,
    		 PlusLen = length(Data#token.postws) + EndSC,
			 Text = full_token_text(Data),
			 {NextLine, NLastCol} = 
			 case char_count_after_last_ln(Text) of 
			    %% if there are no characters after the last \n in the last token
			    %% then the next form starts on the line next to the last token
			    %% of this form
				0 -> 		{ActLine + element(1,EndLC) + 1, 0};
				%% the next form starts in the line of the last token of this form
				_ ->		{ActLine + element(1,EndLC), element(2,EndLC)}		
			end,
			update(ATail, Index+Count, PSt, File, 
							FT, P, OldOrNew, ActScalar + PlusLen, NextLine, NLastCol)
    end;

update([{del, Form} | ATail], Index, PSt, File, FT, P, OldOrNew, ActScalar, ActLine, LastCol) ->
    ?PreProc:detach(File, Form),
    [?ESG:remove(File, form, F) || F <- ?FileMan:dep_forms(Form)],
    ?ESG:remove(File, form, Form),
    update(ATail, Index, PSt, File, FT, 
                        ?FileMan:progress_step(P), OldOrNew, ActScalar, ActLine, LastCol);

update([{ins, {Hash, Input}} | ATail], Index, PSt, File, FT, P, OldOrNew, ActScalar, ActLine, LastCol) ->
	NInput = calc_rel_pos(Input, LastCol),
    {NewForms, PSt1} = ?PreProc:preprocess(NInput, File, PSt, OldOrNew),
    NewFormIdxs = ?MISC:index_list(NewForms, Index),
    %% run add on forms in NewFormIdxs
    {NextScalar, NextLine, NLastCol} = 
            add_forms_at(FT, File, Hash, Input, NewFormIdxs, ActScalar, ActLine, LastCol),
    update(ATail, Index + length(NewForms), PSt1, File, FT, 
                            ?FileMan:progress_step(P), OldOrNew, NextScalar, NextLine, NLastCol);

update([], _Index, _PSt, _File, _FT, _P, _OldOrNew, _ActScalar, _ActLine, _LastCol) ->
    %% here we could shut down the progress reporter if needed
    ok.

%% Add forms from an Input
%% returns the start position of the next form (if it were after Input like this:
%%    Input|NextForm|NextForm2... )
%% the result is a tuple of 'startscalar' and 'startlinecol'

add_forms_at(_FT, _File, _Hash, _Input, [], ActScalar, ActLine, LastCols) ->
	{ActScalar,ActLine,LastCols};
	
add_forms_at(FT, File, Hash, Input, [ {ProcessedTokens, Index0} | Tail ], ActScalar, ActLine, LastCols) ->
	{NextScalar, NextLine, NLastCols} = 
	add_form_at(FT, File, Hash, Index0, ProcessedTokens, Input, ActScalar, ActLine, LastCols),
	add_forms_at(FT, File, Hash, Input, Tail, NextScalar, NextLine, NLastCols).
	
%% Parses and adds the form at `Index' to the file.
%% returns the start position of the next form
add_form_at(FT, File, Hash, Index, ProcessedTokens, Input, ActScalar, ActLine, LastCols) ->
    Form = ?FileMan:parse_form(File, Index, FT, ProcessedTokens), 
    ?FileMan:set_form_hash(Form, Hash),
    %% set_form_abspos_calc_len sets abs pos 
    %% (ActScalar/ActLine for the actual form, and calculate its length
    %% for the next form)
    {PlusScalar, PlusLine, LastColsCurr} = set_form_abspos_calc_len(Form, ProcessedTokens, Input, ActScalar, ActLine),
    ?ESG:insert(File, {form, Index}, Form),
    NLastCols =
    case PlusLine of
        0 -> LastCols + LastColsCurr;
        _ -> LastCols
    end,
    {ActScalar + PlusScalar, ActLine + PlusLine, NLastCols }.

%% Calculate relative position for a list of tokens 
%% The "relative linecol" is a tuple {L, C} where L is the number of lines form
%% the start of the form, and C is the number of columns from the start of the
%% line, so in order to calculate this (C) for the first token, we have to know
%% how many columns there are before it - this is StartCol

calc_rel_pos(List, StartCol) when is_list(List) ->
	calc_rel_pos(List,[],0, {0,StartCol});
calc_rel_pos(_List, _StartCol) ->
	ok.
calc_rel_pos([],Acc,_StartScalar, _) ->
	lists:reverse(Acc);
calc_rel_pos([{Data, Node} | Tail ],Acc,StartScalar,StartLC={_StartLine, _StartCol}) ->
	{ScStartOffset, ScEndOffset} = 
	                                calc_token_sc_offset(StartScalar, Data),
	{{LcStartLineOffset, LcStartCol}, {LcEndLineOffset, LcEndCol}} =
	                                calc_token_lc_offset(StartLC, Data),
	{EndScalar, {EndLine, EndCol}} = 
	        calc_next_start(ScEndOffset, LcEndLineOffset, LcEndCol, Data),    			
	H = Data#token{ scalar={ScStartOffset,ScEndOffset} , linecol={{LcStartLineOffset, LcStartCol}, {LcEndLineOffset, LcEndCol}} },
	?Graph:update(Node, {lex, token, H  }),
	calc_rel_pos(Tail, [ { H , Node } | Acc ] ,EndScalar, {EndLine, EndCol}).

%% This function calculates the scalar offset (relative position from the start of the form)
%% of a token from token data and a given start scalar position 
%% StartScalar - Start relative scalar of the token
calc_token_sc_offset(StartScalar, Data) ->
    ScStartOffset = StartScalar + length(Data#token.prews),
	ScEndOffset = ScStartOffset + length(Data#token.text) - 1,
	{ScStartOffset, ScEndOffset}.

%% This function calculates the linecol offset (relative line from the start of the form,
%% and column from the start of the line)
%% of a token from token data and a given StartLine, StartCol 
%% StartLine - start line of the token
%% StartCol - Columns before the first character of the prewhitespace of the token
calc_token_lc_offset({StartLine, StartCol}, Data) ->
	Pre_Pluslc = plus_lines(Data#token.prews),
	LcStartLineOffset = StartLine + Pre_Pluslc,
	LcStartCol   = case Pre_Pluslc of
							0 -> StartCol + length(Data#token.prews) + 1;
							_ -> char_count_after_last_ln(Data#token.prews) + 1
					end,
	LcEndLineOffset	= LcStartLineOffset + plus_lines(Data#token.text), 
	LcEndCol	
	    = case {LcEndLineOffset == LcStartLineOffset, 
	                        char_count_after_last_ln(Data#token.text)} of
			{true, _} ->
				LcStartCol + length(Data#token.text) - 1;
			{false, 0} ->
				LcStartCol + length(Data#token.text) - 1; 
			{false, A} ->
				A - 1
	    end,
    {{LcStartLineOffset, LcStartCol}, {LcEndLineOffset, LcEndCol}}.

% This function calculates StartScalar, StartLine, StartCol for the next token.
% StartScalar is the relative scalar of the position following the last character of
% the post whitespace of this token
% StartLine is the relative line value of the last line of token post whitespace
% StartCol means how many columns there are in the last row of token post whitespace
% See calc_rel_pos for details (why we need StartCol)
calc_next_start(ScEndOffset, LcEndLineOffset, LcEndCol, Data) ->
	StartScalar = ScEndOffset + length(Data#token.postws) + 1,
	StartLine = LcEndLineOffset + plus_lines(Data#token.postws),
	StartCol = case plus_lines(Data#token.postws) of
					0 -> LcEndCol + length(Data#token.postws);
					_ -> char_count_after_last_ln(Data#token.postws)
				end,
	{StartScalar, {StartLine, StartCol}}.

extract_data(TNode) ->
	{ _, _, D} = ?Graph:data(TNode),
	D.

%% Set absolute position for a form
set_form_abspos(Form, AbsSC, AbsLine) ->
    Data = (?Graph:data(Form)),
    case {Data#form.start_scalar, Data#form.start_line} of
        {AbsSC, AbsLine} -> ok;
        _              ->
                    ?Graph:update(Form, Data#form{start_scalar=AbsSC, start_line=AbsLine })
    end.
    
%% This function sets the form properties start_scalar and start_line to
%% ActScalar and ActLine and then calculates the "length" of the current form
%% The "length" means the number of characters in the form, the length of the form
%% in lines, and the number of characters in the last line of the form
%% These values are needed to calculate the absolute position of the next form
%% This function returns the calculated form length (ScalarLength, LineLength)
%% and the columns (characters) in the last line of the form
set_form_abspos_calc_len(Form, {vtokens, Dep, _Orig, _Tokens}, _Input, _ActScalar, _ActLine) ->
  	#form{ start_scalar=AbsSC, start_line=AbsLC } = ?Graph:data(Dep),
  	set_form_abspos(Form, AbsSC, AbsLC),
    {0, 0, 0};
set_form_abspos_calc_len(Form, _, TokenWPoss, ActScalar, ActLine) ->
	%% Token is a token record in this case (tuple)
	TDataList = [ Token || {Token, _} <- TokenWPoss],
    ScalarLength = lists:sum([?FileMan:token_length(Token) || Token <- TDataList]), 
	{LineLength, LastCols} = tokenlist_to_linecol(TDataList),
    set_form_abspos(Form, ActScalar, ActLine),
    {ScalarLength, LineLength, LastCols}.


%%% ============================================================================
%%% Token level manipulation

%% Create a token node
make_token(Type, Text, _LPos={_LStart, _LEnd}, _SPos={_SSt, _SEnd}) ->
    %% TODO: no ?Token call (move ?FileMan:build into ?Syn)
    ?Token:build(Type, Text).


%% ===========================================================================================
%% Makes the error text more informative in cases when it is possible.
parse_error_msg(File, _Line, FormIndex, _Tokens, ["syntax error before: ", ErrPosMsg], Origin) ->
    FlatTxt = ?MISC:format(ErrPosMsg) ++ ".",
    case ?MISC:string_to_term(FlatTxt) of
    	{ok, {_ErrToken = #token{text=Text, type=Type, linecol={_,TokenEndLc}}, _ErrLex}} ->
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
				    LastFormIndex = FormIndex - 1,
				    {LC, #file{path=Path}} =
				    case {LastFormIndex > 0, File == OrigFile} of
						{_, false} -> OrigLine = (?Graph:data(Origin))#form.start_line,
									{linecol_add(OrigLine,TokenEndLc), ?Graph:data(OrigFile)};
				    	{true, _} -> [LastForm] = 
				    				?Query:exec(File, ?File:form(LastFormIndex)),
				    	        	LastFormData =?Graph:data(LastForm),
				    				#form{ start_line=FormLine } = LastFormData,
									LastLeaf    = getlastleaf(LastForm),
									#lex{ data=#token{ linecol={_,LastLeafEnd}, postws=PostWs } }
														  = ?ESG:data(LastLeaf),
									{PrevFormLastTokEndLine, _} = linecol_add(FormLine, LastLeafEnd),
									PrevFormEndLine = PrevFormLastTokEndLine + plus_lines(PostWs),
									{linecol_add(PrevFormEndLine, TokenEndLc),
									?Graph:data(File)};
									
				    	{_, _}	 ->	{{0, 0}, ?Graph:data(File)}
				    end,
				    {ErrLine, ErrCol} = LC,
				    Text2 = ?MISC:format(Text),
				    Text3 = 
				     re:replace(Text2, "\n", "\\\\n", [global, {return, list}]),
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

%% This functions adds the offset LC to the linecol position (tuple: {L, C}) to
%% ActLC
%% The offset is a distance in rows and columns from a position
linecol_add(Line, {L, C}) ->
	{Line + L, C}.

%%	Calculate how many lines there are in a token data list (token record list)
%%	and how many characters there are after the last linebreak.
tokenlist_to_linecol(_L = [ T | Tail ]) ->
    tokenlist_to_linecol([ T | Tail ], {0,0}).
tokenlist_to_linecol([ TokenData | Tail ], {L,C}) ->
    TXT = full_token_text(TokenData),
    LineBrs = plus_lines(TXT),
    case LineBrs of
        0 -> tokenlist_to_linecol(Tail, {L,C + char_count_after_last_ln(TXT)});
        _ -> tokenlist_to_linecol(Tail, {L + LineBrs, char_count_after_last_ln(TXT)})
    end;
tokenlist_to_linecol([],{L,C}) ->
    {L,C}.

getlastleaf(Form) ->
    hd(?Query:exec(Form, ?Syn:last_leaf())).

full_token_text(#token{prews=Pre, postws=Post, text=Text}) ->
    Pre ++ Text ++ Post.

text_to_lc(Text) ->
	{plus_lines(Text), char_count_after_last_ln(Text)}.

%% how many linebreaks are there in this Text? ("plus lines")
plus_lines(TXT) -> length(lists:filter(fun(X) -> X == 10 end, TXT)).
%% how many characters are there in this text, after the last linebreak?
char_count_after_last_ln(TXT) -> length(lists:takewhile(fun(X) -> X /= 10 end, lists:reverse(TXT))).
