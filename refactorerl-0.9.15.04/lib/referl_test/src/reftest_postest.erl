-module(reftest_postest).
-export([compare_positioning_in_file/1,file_token_pos/2, perform_file_test/1]).

%% Random module based testing callbacks
-export([prepare/1, perform_and_check/2]).
-export([mnesia_test/0,otp_test/1]).
-export([give_all_pos/2,give_all_pos_for_files/1]).
-export([lc_check/0, sc_check/0, rel_abs_check/1, rel_abs_compare/2]).
-export([compare_form_length_in_file/1]).
-export([convert_pos/1, convert_pos_speed/1]).
-include("test.hrl").


%% @doc Prepare function for random module based testing.
prepare(Mods) -> [Mods].

%% @doc "Perform and check" function for random module based testing.
perform_and_check(_ModsOrFiles, _Args) ->
	Files = ?Query:exec([file]),
	lists:all(fun check_if_ok/1,
		[compare_positioning_in_file(FileNode) || FileNode <- Files]).

perform_file_test(File) ->
	case refcore_fileman_gen:add_file(File) of
		{file, Node} ->
				Res = compare_positioning_in_file(Node) == true,
				refcore_fileman_gen:drop_file(Node),
				Res;
		{error, R} ->
				io:format("Error: ~p, reset database before testing! \n",[R]),
				error
	end.

mnesia_test() -> otp_test(mnesia).

otp_test(OtpPart) ->
	ri:reset(),
	TestLib = code:lib_dir(OtpPart),
	ri:add(TestLib),
	%% FileNodes = ?Query:exec([file]),
	S = sc_check(),
	L = lc_check(),
	R = rel_abs_check(TestLib),
	Result = 
	S and
	L and
	R,
	case Result of 
		true ->
			io:format("Test is successful.\n");
		_ ->
			io:format("Test failed. (results: sc ~s lc ~s rel-abs ~s )\n", [S,L,R])
	end.

sc_check() ->
    io:format("This function calculates sc-s for all files and tokens with a naive method, and then checks if they equal to the new sc-s. \n"),
    Files = ?Query:exec([file]),
    lists:all(fun check_if_ok/1, [ compare_positioning_in_file(F) || F <- Files ]).

lc_check() ->
    io:format("This function calculates lc-s for all files and tokens with the old binary-based method, and then checks if they equal to the new lc-s. \n"),
	Files = ?Query:exec([file]),
	Tokens0 = [ ?Syn:leaves(FileNode) || FileNode <- Files ],
	Tokens = lists:flatten(Tokens0),
	LCs = [ begin {old_linecol(TL), TL} end || TL <- Tokens ],
%	NewLCs = [ begin {_,_,{_, _, _, _, _, _, NewLC}} = ?Graph:data(X),
%						NewLC end || X <- Tokens ],
	NewLCs = [ begin {?Token:linecol(X), X} end || X <- Tokens ],
	Zip = lists:zip(LCs, NewLCs),
	_Result = lists:all(fun check_if_ok/1,
		[ begin case Old == New of
					true -> true;
					_    -> ?d({T, ?Graph:data(T), Old, "/=", New}), false
				end end
			|| {{Old, _}, {New, T}} <- Zip]).	
			
rel_abs_check(FilePath) ->
    io:format("This function checks if the absolute and relative posmodes store the same token positions. \n"),
	rel_abs_compare(FilePath, scalar) and rel_abs_compare(FilePath,linecol).
			
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compare_form_length_in_file(FilePath) ->
    ri:reset(abs),
    ri:add(FilePath),
    FileNode0 = hd(?Query:exec([file])),
    AFLengths = formlens(FileNode0),
    
    ri:reset(rel),
    ri:add(FilePath),
    FileNode1 = hd(?Query:exec([file])),
    RFLengths = formlens(FileNode1),
    
	Zip = lists:zip(AFLengths, RFLengths),
	_Result = lists:all(fun check_if_ok/1,
		[ begin case A == R of
					true -> true;
					_    -> ?d({A, "/=", R}), false
				end end
			|| {A, R} <- Zip]).	    
    
formlens(FileNode) ->
    GetForms = ?File:forms(),
    Forms = ?Query:exec(FileNode, GetForms),
    [ ?Token:form_length(F) || F <- lists:usort(Forms) ].
    

compare_positioning_in_file(FileNode) ->
	Tokens = ?Syn:leaves(FileNode),
	TokCount = length(Tokens),
	GoodPos =
	calc_and_compare_4every(Tokens,FileNode),
	io:format("Token cnt / positions ok : ~p / ~p \n",[TokCount,GoodPos]),
	TokCount == GoodPos.


give_all_pos(FileNode, PosOpt) ->
	Tokens = ?Syn:leaves(FileNode),
	?Token:map_pos(FileNode, Tokens, PosOpt).

give_all_pos_for_files(PosOpt) ->
	Files = ?Query:exec([file]),
	Tokens = [ {FileNode, ?Syn:leaves(FileNode)} || FileNode <- Files ],
	[ ?Token:map_pos(Fn, TL, PosOpt) || {Fn, TL} <- Tokens ].	

old_linecol(TL) -> 
    File = ?Query:exec1(TL, ?Token:file(), token_file),
	FileBinData = file:read_file(?File:path(File)),
	NodesScalar = lists:usort(?Token:map_pos(File,[TL],scalar)),
	{_Nodes,ScalTupList} = lists:unzip(NodesScalar),
	{ScalBegin, ScalEnd} = lists:unzip(ScalTupList),
	LCBegin = ?Token:map_pos2lc(FileBinData,ScalBegin), 
	LCEnd = ?Token:map_pos2lc(FileBinData,ScalEnd), 
	hd(lists:zip(LCBegin,LCEnd)).

%% PosOpt = linecol | scalar
rel_abs_compare(FilePath, PosOpt) ->
	ri:reset(abs),
	ri:add(FilePath),
	AbsLc = lists:flatten(give_all_pos_for_files(PosOpt)),
	ri:reset(rel),
	ri:add(FilePath),
	RelLc = lists:flatten(give_all_pos_for_files(PosOpt)),
	ToCompare = lists:zip(AbsLc, RelLc),
  %  io:format("Lenght: %i ", length(ToCompare)),

	_Result = lists:all(fun check_if_ok/1,
	[ begin case Abs == Rel of
					true -> true;
					_    -> ?d({Abs, "/=", Rel}), false
				end end || { Abs, Rel } <- ToCompare ]).

convert_pos(FilePath) ->
    ri:reset(abs),
    ri:add(FilePath),
    OriginalAbs = lists:flatten(form_positions()) ++
                  lists:flatten(lex_positions()),

    refcore_convert_pos:to_rel(),
    ConvertedRel = lists:flatten(form_positions()) ++
                   lists:flatten(lex_positions()),
    refcore_convert_pos:to_abs(),
    ReconvertedAbs = lists:flatten(form_positions()) ++
                     lists:flatten(lex_positions()),

    ri:reset(rel),
    ri:add(FilePath),
    OriginalRel = lists:flatten(form_positions()) ++
                  lists:flatten(lex_positions()),
    refcore_convert_pos:to_abs(),
    ConvertedAbs = lists:flatten(form_positions()) ++
                   lists:flatten(lex_positions()),
    refcore_convert_pos:to_rel(),
    ReconvertedRel = lists:flatten(form_positions()) ++
                     lists:flatten(lex_positions()),
    BackAndForthMatch = (OriginalAbs == ReconvertedAbs) and
                        (OriginalRel == ReconvertedRel),
    StraightMatch = (OriginalAbs == ConvertedAbs) and
                    (OriginalRel == ConvertedRel),

    %useful if someone is debugging
    ?d(backandforth_abs),
    to_compare(OriginalAbs, ReconvertedAbs),
    ?d(backandforth_rel),
    to_compare(OriginalRel, ReconvertedRel),
    ?d(straight_abs),
    to_compare(OriginalAbs, ConvertedAbs),
    ?d(straight_rel),
    to_compare(OriginalRel, ConvertedRel),

    BackAndForthMatch and StraightMatch.

form_positions() ->
    Files = [File || {file, File} <- ?Syn:children(?Graph:root())],
    Forms = lists:map(fun(File) ->
        [{      (?Graph:data(Form))#form.form_length,
                (?Graph:data(Form))#form.start_scalar,
                (?Graph:data(Form))#form.start_line} ||
         {form, Form} <- ?Syn:children(File)] end, Files),
    %?d(Forms),
    Forms. 

lex_positions() ->
    Files = [File || {file, File} <- ?Syn:children(?Graph:root())],
    AllTokens = [ ?Syn:leaves(FileNode) || FileNode <- Files ],
    Tokens = lists:flatten(AllTokens),
    Lexes = [ {     (get_lex_data(Lex))#token.scalar,
                    (get_lex_data(Lex))#token.linecol}  || Lex <- Tokens ],
    %?d(Lexes),
    Lexes.

get_lex_data(Lex) ->
    {lex, token, Data} = (?Graph:data(Lex))#lex{},
    Data.

convert_pos_speed(FilePath) ->
    convert_pos_speed(FilePath, 0, [], [], [], []).

convert_pos_speed(_, 10, LoadAbs, ToRel, LoadRel, ToAbs)           ->
    io:format("\nPosition converting speed test \n", []),
    io:format("    using ~p database on ~p tokens:\n", [?Graph:get_dbmod(), length(?Syn:leaves(?Graph:root()))]),
    io:format("    Avg loading time with abs pos mode:  ~p sec (min: ~p, max: ~p)\n",
              [lists:sum(LoadAbs)/10000000, lists:min(LoadAbs)/1000000, lists:max(LoadAbs)/1000000]),
    io:format("    Avg loading time with rel pos mode:  ~p sec (min: ~p, max: ~p)\n",
              [lists:sum(LoadRel)/10000000, lists:min(LoadRel)/1000000, lists:max(LoadRel)/1000000]),
    io:format("    Avg converting time from abs to rel: ~p sec (min: ~p, max: ~p)\n",
              [lists:sum(ToRel)/10000000, lists:min(ToRel)/1000000, lists:max(ToRel)/1000000]),
    io:format("    Avg converting time from rel to abs: ~p sec (min: ~p, max: ~p)\n",
              [lists:sum(ToAbs)/10000000, lists:min(ToAbs)/1000000, lists:max(ToAbs)/1000000]); 
convert_pos_speed(FilePath, Times, LoadAbs, ToRel, LoadRel, ToAbs) ->
    ri:reset(abs),
    {LoadAbsTime, ok} = timer:tc(ri, add, [FilePath]),
    LA = lists:append(LoadAbs, [LoadAbsTime]),
    {ConvertAbsToRelTime, ok} = timer:tc(refcore_convert_pos, to_rel, []),
    TR = lists:append(ToRel, [ConvertAbsToRelTime]),
    ri:reset(rel),
    {LoadRelTime, ok} = timer:tc(ri, add, [FilePath]),
    LR = lists:append(LoadRel, [LoadRelTime]),
    {ConvertRelToAbsTime, ok} = timer:tc(refcore_convert_pos, to_abs, []),
    TA = lists:append(ToAbs, [ConvertRelToAbsTime]),
    convert_pos_speed(FilePath, (Times + 1), LA, TR, LR, TA).

file_token_pos(File,TokenNum) ->
	Tokens = ?Syn:leaves(File),
	TokenData = [ ?ESG:data(Token) || Token <- Tokens],
	TokList = lists:sublist(TokenData,TokenNum),
	naive_tokenpos(TokList).

naive_tokenpos(File, Token) ->
	Tokens = ?Syn:leaves(File),
	ListPart = lists:takewhile(fun(A)-> A =/= Token end, Tokens),
	List = ListPart ++ [Token],
	naive_tokenpos(List).

%% calculate position for last item in TokensAndLast
naive_tokenpos(TokensAndLast) ->
	{B,E,_} =
	lists:foldl(fun next/2,
				{0,0,0},
			  TokensAndLast),
	{B + 1,E}.

next(Token,{_B,_E,Last}) ->
	#lex{ data=#token{text=Text, prews=Pre, postws=Post }} = ?ESG:data(Token),
	NewB = Last + length(Pre),
	NewE = NewB + length(Text),
	NewLast = NewE + length(Post),
	{NewB, NewE, NewLast}.

calc_and_compare_4every(Tokens,File) ->
	calc_and_compare_4every(Tokens,0,File).

calc_and_compare_4every([],Cnt, _File) ->
	Cnt;
	
calc_and_compare_4every([H | T],Cnt,File) ->
	Naive = naive_tokenpos(File,H),
	New	  = ?Token:pos(File,H),
	case Naive == New of
		true -> calc_and_compare_4every(T,Cnt + 1,File);
		false -> 
		?d(H),
		?d({naive, Naive, new, New}),	
		calc_and_compare_4every(T,Cnt,File)
	end.


check_if_ok(A) -> A == true.

to_compare(A,B)->
    ToCompare = lists:zip(A, B),
    lists:all(fun check_if_ok/1,
	    [ begin case C == D of
		            true -> true;
		            _    -> ?d({C, "/=", D}), false
		        end end || { C, D } <- ToCompare ]).  
