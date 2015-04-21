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

%%% @doc Token related queries and manipulations. Tokens are represented with
%%% nodes of class `lex' with the `type' attribute set to `token'. There are
%%% two kinds of tokens. Real tokens come directly from source code, and their
%%% `data' attribute contains the token data (in a `#token{}' record). Virtual
%%% tokens are created by preprocessor substitutions, and they are usually
%%% originated from a real token. Virtual tokens do not contain token data
%%% directly, because it is the same as the original.
%%% The reflib_token_gen module is an interface, it calls functions of other
%%% modules according to the current positioning mode. A reflib_token_name module 
%%% for positioning mode "name" must implement the reflib_token_gen behaviour.
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author Gabor Hosszu

-module(reflib_token_gen).
-vsn("$Rev: 12913 $ ").


-export([get_value/1]).

%% Properties
-export([pos/1, pos/2, pos/3, linecol/1, map_pos/2, map_pos/3]).
-export([get_both_pos/1, get_both_pos/2, map_both_pos/2]).
-export([text/1, data/1, type/1, is_virtual/1]).
-export([form_length/1]).

%% Queries
-export([file/0, form/0, expr/0, typexp/0, clause/0, original/0, virtuals/0]).

%% Manipulations
-export([build/2, buildwpos/4, keyword_value/1]).
-export([foldpos/3, foldpos/4]).
-export([len/1, len/2]).
-export([trim/1]).

%% Helper: scalar to linecol
-export([pos2lc/2]).
-export([lc2pos/2]).

-include("lib.hrl").
-export([map_pos2lc/2,token2form/1,containing_form/2]).
-export([map_lc2pos/2]).

-export([file_lc2sc/2]).

%% Behaviour functions
-export([behaviour_info/1]).

-export([ws2mask/1,sum_mask/2,lc_aggr/2,id/1,toklens/1]).         


%% ============================================================================
%% Internal functions of the behaviour

behaviour_info(callbacks) ->
    [{map_pos,3},{foldpos,5},{containing_form,2}, {form_length,1}].
    	    	
handle_call({Fun,Args}) ->
%	{ _, _, _, [FModule, TModule]} = hd(supervisor:which_children(fileman_sup)),
	TModule = ?FileMan:get_tmodule(),
	apply(TModule,Fun,Args).

%% ============================================================================
%% Token properties

%% @spec data(node()) -> #token{}
%% @doc Returns the original token data of the node. This data is not
%% available directly in the node in case of preprocessor substitutions.
data(Lex) ->
    try ?Graph:data(?Query:exec1(Lex, original(), original)) of
        #lex{type=token, data=D} -> D;
        _ -> erlang:error({bad_token, Lex})
    catch
        throw:original ->
            erlang:error({bad_token, Lex})
    end.

%% @spec text(node()) -> string()
%% @doc Returns the textual form of the token (together with surrounding
%% whitespace). The testual representation is exactly the same as the
%% appearance of the token in the source file.
text(LexNode) ->
    [OrigNode] = ?Query:exec([LexNode], original()),
    #lex{data=#token{text=T,prews=P,postws=S}} = ?Graph:data(OrigNode),
    P++T++S.

%% @spec type(node()) -> atom
%% @doc Returns the type of the token contained by the lexical node.
type(Lex) ->
    (?Token:data(Lex))#token.type.

%% @type line_col() = {natural(), natural()}

%% @spec linecol(node()) -> {line_col(),line_col()}
%% @doc Returns the line and column number of the first and last character
%%      of the token.
linecol(Token) ->
    File = ?Query:exec1(Token, file(), token_file),
    linecol(File, Token).

%% @doc Returns the line and column number of the first and last character
%%      of `Token', assuming it is located in `File'.
linecol(File,Token) ->
    pos(File,Token,linecol).

%% @spec pos(node()) -> {integer(), integer()}
%% @doc Returns the character position of `Token' in the source file that
%% contains the token. The returned tuple contains the indices of the first
%% and last characters of the token without whitespace.
pos(Token) ->
    File = ?Query:exec1(Token, file(), token_file),
    pos(File, Token).

%% @doc Returns the position of `Token' assuming that it is located in `File'.
%% @see pos/1
%% @spec (file_node(),token_node()) -> {integer(),integer()}
pos(File,Token) ->
    pos(File,Token,scalar).

pos(File,Token,PosType)->
	[{_Node, Pos}] = map_pos(File,[Token],PosType),
	Pos.
	

%% @doc Returns the scalar position of all listed tokens from `File' in a
%% single run.
%% Note that it assumes that no duplicate token is given.
%% Use lists:usort/1 if in doubt.
%% @spec (#file{},[#token{}]) -> [{#token{},{integer(),integer()}}]
map_pos(File,Tokens)->  
    map_pos(File,Tokens,scalar).

%% @doc Returns the position of all listed tokens from `File' in a single run.
%% Note that it assumes that no duplicate token is given.
%% Use lists:usort/1 if in doubt.
%% @spec (#file{},[#token{}],scalar|linecol|both) ->
%%  [{#token{},{integer(),integer()}}] | 
%%  [{#token{},{{integer(),integer()}, {integer(),integer()}}}] |
%%  [{#token{},{{integer(),integer()}, {{integer(),integer()}, {integer(),integer()}}}}]
map_pos(File,Tokens,PosOpt) ->
	handle_call({map_pos,[File,Tokens,PosOpt]}).	
	
get_both_pos(Token) ->
    File = ?Query:exec1(Token, file(), token_file),
    get_both_pos(File, Token).
    
get_both_pos(File, Token) ->
	map_both_pos(File, [Token]).
	
map_both_pos(File, Tokens) ->
	handle_call({map_pos,[File,Tokens,both]}).
	
%% @spec form_length(node()) -> integer()
%% @doc Returns the form length (the way we calculate it depends on the current
%% positioning mode)  
form_length(Form) ->
    handle_call({form_length,[Form]}).
	
%%% ----------------------------------------------------------------------------
%%% Generic file token handler

%% @spec foldpos(Fun, Acc0, File::node()) -> AccLast
%%
%%       Fun = (Token::node(), Data::#token{}, Start, End, Acc) ->
%%          {stop, AccLast} | {next, AccNext}
%%
%% @doc Similar to `lists:foldl/3' on the tokens (and their positions) of the
%% file, except that this can stop processing. The token node, token data, and
%% the indices of the first and last character of the token are passed to the
%% function.
foldpos(Fun, Acc0, File) ->
    foldpos(Fun, Acc0, File, both).

%% @type wsmask() = 'none' | 'pre' | 'post' | 'both'

%% @spec (Fun,Acc,node(),wsmask())->Acc
%%       Fun = (node(), #token{}, integer(), integer(), Acc) ->
%%          {stop, Acc} | {next, Acc}

foldpos(Fun, Acc0, File, Ws)->
    foldpos(Fun, Acc0, File, Ws, scalar).

foldpos(Fun, Acc0, File, Ws, PosType) ->
	handle_call({foldpos,[Fun, Acc0, File, Ws, PosType]}).


%% ============================================================================
%% Queries starting from tokens

%% @spec file() -> query(#lex{}, #file{})
%% @doc The result query returns the file that contains the token.
file() ->
    fun (Lex) -> first(file, ?Syn:root_path(Lex)) end.

%% @spec form() -> query(#lex{}, #form{})
%% @doc The result query returns the form that contains the token.
form() ->
    fun (Lex) -> first(form, ?Syn:root_path(Lex)) end.

first(Tag, [{Tag, N}|_])  -> [N];
first(Tag, [_       |Tl]) -> first(Tag, Tl);
first(_,   [])            -> [].

%% @spec expr() -> query(#lex{}, #expr{})
%% @doc The result query returns the direct containing expression of the
%% token, or an empty list if the direct parent of the token is not an
%% expression.
expr() ->
    ?Query:seq(original(), [{elex,back}]).

%% @spec typexp() -> query(#lex{}, #typexp{})

%% @doc The result query returns the direct containing type expression
%% of the token, or an empty list if the direct parent of the token is
%% not a type expression.
typexp() ->
    ?Query:seq(original(), [{tlex,back}]).

%% @spec clause() -> query(#lex{}, #clause{})
%% @doc The result query returns the direct containing clause of the token, or
%% an empty list if the direct parent of the token is not a clause.
clause() ->
    ?Query:seq(original(), [{clex,back}]).

%% @spec original() -> query(#lex{}, #lex{})
%% @doc The result query returns the original of the token.
original() ->
    fun original/1.

original(LexNode) ->
    case ?Graph:path(LexNode, [orig]) of
        []           -> [LexNode];
        [OrigNode|_] -> original(OrigNode)
    end.

    
%% @spec virtuals() -> query(#lex{}, #lex{})
%% @doc The result query returns all virtual tokens originating from the token.
virtuals() ->
    fun (Lex) -> virtuals([Lex], []) end.

virtuals([Head | Tail], Virt) ->
    case ?Graph:path(Head, [{orig, back}]) of
        []  -> virtuals(Tail, Virt);
        New -> virtuals(New ++ Tail, New ++ Virt)
    end;
virtuals([], Virt) -> Virt.


lc_aggr({L,C},Txt) ->
    ?MISC:string_linecol(Txt,inf,{1,L,C}).

id(X) ->
    X.

%% @spec (#token{}) -> natural()
len(Data) ->
    len(Data, both).
%% @spec (#token{},wsmask()) -> natural()
len(Data, Tag) ->
    sum_mask(ws2mask(Tag),toklens(Data)).

sum_mask(M,L) ->
    lists:sum(?MISC:mask(M,L)).

toklens(#token{prews=Pre, text=Text, postws=Post}) ->
    lists:map(fun length/1, [Pre,Text,Post]).

%% @spec (wsmask()) -> [bool()]
ws2mask(none) -> [false,true,false];
ws2mask(pre)  -> [true, true,false];
ws2mask(post) -> [false,true,true];
ws2mask(both) -> [true, true,true].


%%% ----------------------------------------------------------------------------
%%% Token data generation

%% TODO Move this to the appropriate place, maybe even another module.
%% @doc Get text of a lex node / lex record / token record
%% @spec get_value(#token{}|#lex{}|node()) -> string()
get_value(#token{type=Type, text=Text}) ->
    value(Type, ?MISC:strip(Text));
get_value(#lex{data=Token}) ->
    get_value(Token);
get_value(TokenNode) ->
    get_value(?Graph:data(TokenNode)).

%% @spec build(atom(), string()) -> #token{}
%% @doc Creates lexical element data. The text of the element is given in
%% `Text', and it is turned into a token of type `Type'.
build(Type, Text) ->
    #token{type=Type, text=Text}.
%% @spec buildwpos(atom(), string(), integer(), integer()) -> #token{}
%% @doc Creates lexical element data with position information. 
%%  The text of the element is given in `Text', and it is turned into a token of
%%  type `Type'.
buildwpos(Type, Text, LC = {_LSt, _LEnd}, SC = {_SSt, SEnd}) ->
	#token{type=Type, text=Text, scalar=transformpos(sc, SC,Text), linecol=transformpos(lc, LC, Text, SEnd) }.



value(variable, Text) ->
    Text;

value(string, Text) ->
    quoted_value(Text, $\");

value(atom, Text) when hd(Text) == $\' ->
    list_to_atom(quoted_value(Text, $\'));
value(atom, Text) ->
    list_to_atom(Text);

value(char, [$$, $\\ | Text]) ->
    {Val, _} = escape_value(Text),
    Val;
value(char, [$$, C | _]) ->
    C;

value(integer, Text) ->
    case lists:splitwith(fun (C) -> C /= $# end, Text) of
        {Base, [$# | Num]} ->
            erlang:list_to_integer(Num, list_to_integer(Base));
        {Num, []} ->
            list_to_integer(Num)
    end;

value(float, Text) ->
    list_to_float(Text);

value(Type, _Text) ->
    keyword_value(Type).


%% @spec keyword_value(atom()) -> atom()
%% @doc  Returns the keyword value.
%% @TODO Should be auto-generated in the future.
keyword_value(stop)	        -> '.';
keyword_value(Atom)         -> Atom.

quoted_value([Q | Tail], Q) -> quoted_tail_value(Tail, Q, "");
quoted_value(Str, _)        -> {"", Str}.

quoted_tail_value([Q | _], Q, Val) -> lists:reverse(Val);
quoted_tail_value("", Q, Val) -> quoted_tail_value([Q], Q, Val);
quoted_tail_value([$\\ | Text], Q, Val) ->
    {Esc, Tail} = escape_value(Text),
    quoted_tail_value(Tail, Q, [Esc | Val]);
quoted_tail_value([Char | Tail], Q, Val) ->
    quoted_tail_value(Tail, Q, [Char|Val]).

escape_value([$b|T]) -> {$\b,T};
escape_value([$d|T]) -> {$\d,T};
escape_value([$e|T]) -> {$\e,T};
escape_value([$f|T]) -> {$\f,T};
escape_value([$n|T]) -> {$\n,T};
escape_value([$r|T]) -> {$\r,T};
escape_value([$s|T]) -> {$\s,T};
escape_value([$t|T]) -> {$\t,T};
escape_value([$v|T]) -> {$\v,T};
escape_value([D|T]) when D >= $0, D =< $7 ->
    escape_value([D], T);
escape_value([C|T])  -> {C,T};
escape_value("") -> {0, []}.

escape_value(Num, [D|T]) when D >= $0, D =< $7, length(Num) < 3 ->
    escape_value(Num ++ [D], T);
escape_value(Num, Tail) ->
    {erlang:list_to_integer(Num, 8), Tail}.

%% @spec is_virtual(Token::node()) -> true|false
%% @doc Returns true if the given token is a virtual token
is_virtual(Node) ->
    case ?Graph:class(Node) of
        lex ->
            case (?ESG:data(Node))#lex.data of
                virtual -> true;
                _ ->       false
            end;
        _   -> false
    end.

%% @spec trim(#lex{}) -> #lex{}
%% @doc Returns the given lexical element data trimmed on both sides.
trim(Lex) ->
    Data = Lex#lex.data,
    Lex#lex{data=Data#token{prews=[], postws=[]}}.
    
%% " 

%%
%%	Helper functions
%%

%% @doc Conversion between scalar (Pos) and linecol, using the text of the file,
%% which contains the position (a token on that position) (scalar to lc)
%% File:open should be called to get File Binary Data, and it can be passed to 
%% this function, because there is an error handling in it.
%% @spec pos2lc(binary(), integer()) -> {natural(), natural()}
pos2lc(FileBinData,Pos) when is_integer(Pos) ->
    case FileBinData of
        {ok,Binary}    -> hd(map_bin_tolinecol(Binary,[Pos]));
        {error,Reason} -> throw({error, file:format_error(Reason)})
    end.
%% @doc Pos2lc with a list of positions
%% File:open should be called to get File Binary Data, and it can be passed to 
%% this function, because there is an error handling in it.
%% @spec map_pos2lc(binary(), [natural()]) -> [{natural(), natural()}]
map_pos2lc(FileBinData,PosList) ->
    case FileBinData of
        {ok,Binary}    -> map_bin_tolinecol(Binary,PosList);
		Binary when is_binary(Binary) -> map_bin_tolinecol(Binary,PosList);
        {error,Reason} -> throw({error, file:format_error(Reason)})
    end.

%% @doc Conversion between linecol (Pos) and scalar, using the text of the file,
%% which contains the position (a token on that position) (lc to scalar)
%% File:open should be called to get File Binary Data, and it can be passed to 
%% this function, because there is an error handling in it.
%% @spec lc2pos(binary(), {natural(), natural()}) -> natural()
lc2pos(FileBinData, Pos) when is_tuple(Pos) ->
    case FileBinData of
        {ok,Binary}    -> hd(map_bin_toscalar(Binary,[Pos]));
        {error,Reason} -> throw({error, file:format_error(Reason)})
    end.

%% @doc Lc2pos with a list of positions
%% File:open should be called to get File Binary Data, and it can be passed to 
%% this function, because there is an error handling in it.
%% @spec map_lc2pos(binary(), [{natural(), natural()}]) -> [natural()] | not_found
map_lc2pos(FileBinData, PosList) ->
    case FileBinData of
		Binary when is_binary(Binary) -> map_bin_toscalar(Binary,PosList);
        {ok,Binary}    -> map_bin_toscalar(Binary,PosList);
        {error,Reason} -> throw({error, file:format_error(Reason)})
    end.

%% general function for the above conversions, Tp = sc | lc which means the
%% type of the list elements in List
%% it returns not_found if one of the tokens cannot be found
map_bin_gen(B, List, Tp) ->
	%% AvailMax = size(B),
	FSM = 	fun 
			 (_, _,                 _,         _, []) -> [];
			 (F, Tpt, <<$\r,T/binary>>, {_,R,_,A}, [ Pos | Tail ])          
												-> F(F,Tpt,T,{$\r,R+1,1,A+1},[Pos | Tail]);
             (F, Tpt, <<$\n,T/binary>>, {N,R,_,A}, [Pos | Tail ] ) when N/=$\r ->
                                                    F(F,Tpt,T,{$\n,R+1,1,A+1},[Pos | Tail]);
             %% we get a \n (in that case when we use \r\n as and endline character)
             (F, lc, <<$\n  ,T/binary>>, {_N,R,C,A}, [Pos | Tail] ) when {R, C} < Pos ->
             										F(F,lc,T,{$\n,R,C,A},[Pos | Tail]);                                     
             (F, lc, <<H  ,T/binary>>, {_N,R,C,A}, [Pos | Tail] ) when {R, C} < Pos ->
             										F(F,lc,T,{H,R,C+1,A+1},[Pos | Tail]);
             %% we get a \n (in that case when we use \r\n as and endline character)
             (F, sc, <<$\n,T/binary>>, {_N,R,C,A}, [Pos | Tail] ) when A < Pos ->
             										F(F,sc,T,{$\n,R,C,A},[Pos | Tail]);
             (F, sc, <<H  ,T/binary>>, {_N,R,C,A}, [Pos | Tail] ) when A < Pos ->
             										F(F,sc,T,{H,R,C+1,A+1},[Pos | Tail]);
             (F, Tpt, <<H  ,T/binary>>, {_N,R,C,A}, [_Pos | Tail]) ->
             							[ case Tp of
											lc -> A;
											sc -> {R, C}
										  end | F(F,Tpt,T, {H,R,C+1,A+1},Tail) ]; %% [ { R, C } | ..
			 (_, _, _,			_,	_) ->
										 not_found
          end,
    FSM(FSM, Tp, B, {$\n,1,1,1}, List).

%% convert list of linecols to scalar
map_bin_toscalar(B, List) when is_binary(B) ->
	map_bin_gen(B, List, lc).

%% convert list of scalars to linecol
map_bin_tolinecol(B, List) when is_binary(B) ->
	map_bin_gen(B, List, sc).

file_lc2sc(FileNode, PosList) ->
	FileBinData = file:read_file(?File:path(FileNode)),
	map_lc2pos(FileBinData, PosList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns the form, which contains the given token
token2form(Token) ->
    [{file, _}, {form, Form}|_] = ?Syn:root_path(Token),
    dep_form(Form).

%% Returns the originator of a virtual form; conventional forms are returned as is.
dep_form(Form) ->
    case ?Graph:path(Form, [fdep]) of
        []     -> Form;
        [FDep] -> dep_form(FDep)
    end.

%% Moved here from reflib_file
%% @doc Decides containing form for position according to positioning mode
%% "Forms" are where the lookup is needed (sorted).
%% This function can be used by modules implementing a positioning
containing_form(Forms, Pos) ->
	PosModule = ?FileMan:get_tmodule(), %% TODO
   	PosModule:containing_form(Forms, Pos).

	
%% Get real positions (which can be used by positionings) from scanner positions
%% this function is used because e. g. the scanner gives {0,1} for tokens, 
%% when we need {1,1}
%% When the token is a ".", we need another transformation. 
transformpos(sc, {St, End}, [THead | _T ]) ->
	case [THead] of
		"." when St > End -> {End, End};
		"." when St == End -> {End, End};
		"." when St+1 == End -> {End, End};
		"." -> {St+1, St+1}; %% SPECIAL CASE!! SOMETIMES SCANNER RETURNS ". %%something" as a STOP TOKEN! THIS IS AN ERROR!
		_   -> {St+1, End}
	end.

transformpos(lc, P = {{StL, StC}, {EndL, EndC}}, TxT = [THead | _T ], _Pl) ->		
	case {[THead], P}  of
		{".", {{_, _}, {_, 0}}} -> {{StL+1, StC+1}, {EndL, StC + 1}};
		{".", {{_, _St}, {_, _End}}} -> {{StL+1, StC+1}, {EndL+1, StC + 1}};
		{_, {{_, _}, {_, 0}}} -> {{StL+1, StC+1}, {EndL, StC + length(TxT)}};
%		"." when St > End -> {End, End};
%		"." when St == End -> {End, End};
%		"." when St+1 == End -> {End, End};
%		"." -> {St+1, End-1};
		_   -> {{StL+1, StC+1}, {EndL+1, EndC}}
	end.
