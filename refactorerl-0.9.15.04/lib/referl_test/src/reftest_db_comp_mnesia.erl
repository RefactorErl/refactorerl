-module(reftest_db_comp_mnesia).

-export([mn_file/0, collect_file_id/1, file/1,file_id/2, file_link/1]).
-export([form/2, file_lnk/2, forms/2, form_Ind/1, form_id/1,
		 form_index/2, form_data/1]).

-export([clauses/2, clause_id/1, clause_ind/1, clause_index/2, 
		 recursive_clause_data/1, collect_expr_clause_id/1, collect_expr_clause_ind/1]).

-export([expr/2, expr_id/1, expr_data/2, exprs_data/1, expr_index/2,
		 collect_expr_ind/2, collect_expr_id/2, collect_exprs_id/1, collect_expr_cls_id/2]).

-export([collect_expr_cls_ind/2, recursive_clause/2, rec_expr_data/2, recursive_expr_data/1,
		 recursive_expr_index/1, collect_expr_index/1]).

-include_lib("referl_core/include/core_global.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record(fdata,{id,attrib,props}).
-record(ldata,{id,ind,to}).

file(Node) ->
	rpc:call(Node ,?MODULE, mn_file, []).

file_id(Node,File) ->
	rpc:call(Node ,?MODULE, collect_file_id, [File]).

form(Node,FileLink) ->
	rpc:call(Node ,?MODULE, form_data, [FileLink]).

file_lnk(Node,FormId) ->
	rpc:call(Node ,?MODULE, file_link, [FormId]).

forms(Node,FileId) ->
	rpc:call(Node ,?MODULE, form_id, [FileId]).

form_index(Node, FileId) ->
	rpc:call(Node ,?MODULE, form_Ind, [FileId]).

clauses(Node, FormId) ->
	rpc:call(Node ,?MODULE, clause_id, [FormId]).

clause_index(Node,ClauseId) ->
	rpc:call(Node ,?MODULE, clause_ind, [ClauseId]).

recursive_clause(Node, ClauseId) ->
	rpc:call(Node ,?MODULE, recursive_clause_data, [ClauseId]).

expr(Node, ClauseId) ->
	rpc:call(Node ,?MODULE, expr_id, [ClauseId]).

expr_data(Node, ExpId) ->
	rpc:call(Node ,?MODULE, exprs_data, [ExpId]).

rec_expr_data(Node, ExpId) ->
	rpc:call(Node ,?MODULE, recursive_expr_data, [ExpId]).

expr_index(Node, ExpId) ->
	rpc:call(Node ,?MODULE, recursive_expr_index, [ExpId]).

collect_expr_ind(Node, ExpId) ->
	rpc:call(Node ,?MODULE, collect_expr_index, [ExpId]).

collect_expr_id(Node, ExpId) ->
	rpc:call(Node ,?MODULE, collect_exprs_id, [ExpId]).

collect_expr_cls_id(Node, ExpId) ->
	rpc:call(Node ,?MODULE, collect_expr_clause_id, [ExpId]).

collect_expr_cls_ind(Node, ExpId) ->
	rpc:call(Node ,?MODULE, collect_expr_clause_ind, [ExpId]).

mn_file() ->
	F = fun()->
	Q = qlc:q([F#fdata.attrib || F <- mnesia:table(file)]),
		qlc:e(Q)
	end,
	{atomic,FileList}=mnesia:transaction(F),
	lists:sort(FileList).

collect_file_id(File) ->
	F = fun()->
	Q = qlc:q([F#fdata.id || F <- mnesia:table(file), F#fdata.attrib =:= File]),
		qlc:e(Q)
	end,
	{atomic,FileId}=mnesia:transaction(F),
	FileId.

form_data(FLnk)->
	MnForm = fun()->
	Q = qlc:q([Fm#fdata.attrib || Fm <- mnesia:table(form), Fm#fdata.id =:= FLnk]),
		qlc:e(Q)
	end,
	{atomic,[Form]}=mnesia:transaction(MnForm),
  	Form.

file_link(Id) ->
	MnLink = fun()->
	Q = qlc:q([Fm#ldata.to 
			  || Fm <- mnesia:table('file$lnk'), Fm#ldata.id =:= {Id,form}]),				 
		qlc:e(Q)
	end,
	{atomic,LinkList}=mnesia:transaction(MnLink),
	LinkList.

form_id(FId) ->
	MnLink = fun()->
	Q = qlc:q([Fm#ldata.to 
			  || Fm <- mnesia:table('file$lnk'), Fm#ldata.id =:= {FId,form}]),			 
		qlc:e(Q)
	end,
	{atomic,LinkList}=mnesia:transaction(MnLink),
	%	io:format("LinkList:_~p~n", [LinkList]),
	lists:map(fun form_data/1, LinkList).

form_Ind(FId) ->	
	%io:format("MNFid: ~p~n", [FId]),
	MnLink = fun()->
	Q = qlc:q([Fm#ldata.to 
			  || Fm <- mnesia:table('file$lnk'), Fm#ldata.id =:= {FId,form}]),			 
		qlc:e(Q)
	end,
	{atomic,LinkList}=mnesia:transaction(MnLink),
	lists:sort(lists:flatten(lists:map(fun form_ind/1, LinkList))).

form_ind(Id)->
	%io:format("FID:_~p~n", [FId]),
	MnId = fun()->
	Q = qlc:q([Fm#ldata.ind || Fm <- mnesia:table('file$lnk'),
							   ((Fm#ldata.to =:= Id) and 
							   (element(2,Fm#ldata.id) =:= form))]),			 
		qlc:e(Q)
	end,
	{atomic,LinkId}=mnesia:transaction(MnId),
	LinkId.

clause_id(FId) ->
	%io:format("FID:~p~n", [FId]),
	G = fun()->
	R = qlc:q([L#ldata.to 
			  || L <- mnesia:table('form$lnk'), L#ldata.id == {FId, funcl}]),
		qlc:e(R)
	end,
	{atomic,LinkToId}=mnesia:transaction(G),
	%io:format("FIDLinktoid:~p~n", [LinkToId]),
	LinkToId.

clause_ind([LinkId|Tail]) ->
	clause_ind(LinkId),
	clause_ind(Tail);
clause_ind([]) -> [];
clause_ind(LinkId) ->
	%io:format("LinnkId:~p ~n", [LinkId]),
	G = fun()->
	Q = qlc:q([L#ldata.ind || L <- mnesia:table('form$lnk'),
							  (L#ldata.to =:= LinkId) and
							  (element(2,L#ldata.id) =:= funcl)]),		 
				qlc:e(Q)
	end,
	{atomic,[LinkInd]}=mnesia:transaction(G),
	LinkInd.

recursive_clause_data(ClId) ->
	%io:format("recursiveClause:~p~n", [ClId]),
	F = fun()->
	Q = qlc:q([Cl#fdata.attrib
			  || Cl <- mnesia:table(clause), Cl#fdata.id =:= ClId]),		 
		qlc:e(Q)
	end,
	{atomic,Clause}=mnesia:transaction(F),
	Clause.

expr_id([]) -> [];
expr_id(ClId) ->
	%io:format("mnesia_expr:~p~n", [ClId]),
	G = fun()->
	Q = qlc:q([L#ldata.to 
			  || L <- mnesia:table('clause$lnk'),
				 (element(1,L#ldata.id) =:= ClId) and
				 ((element(2,L#ldata.id) == body) or
				 (element(2,L#ldata.id) == pattern) or
				 (element(2,L#ldata.id) == name) or 
				 (element(2,L#ldata.id) == guard))]),		 
		qlc:e(Q)
	end,
	{atomic,LinkToId}=mnesia:transaction(G),
	LinkToId.

exprs_data([ExpId|Tail]) ->
	exprs_data(ExpId) ++ exprs_data(Tail);
exprs_data([]) -> [];
exprs_data(ExpId) ->
	F = fun()->
	Q = qlc:q([Cl#fdata.attrib
			  || Cl <- mnesia:table(expr), Cl#fdata.id =:= ExpId]),	 
		qlc:e(Q)
	end,
	{atomic,Expr}=mnesia:transaction(F),
	[E || E <-Expr, E#expr.type /= fret].

recursive_expr_data(ExpId) ->
	%io:format("expid~p~n", [ExpId]),
	F = fun()->
	Q = qlc:q([Cl#fdata.attrib
			  || Cl <- mnesia:table(expr), Cl#fdata.id =:= ExpId]),		 
		qlc:e(Q)
	end,
	{atomic,Expr}=mnesia:transaction(F),
	Expr.

recursive_expr_index([]) -> [];
recursive_expr_index(LinkId) ->
	%io:format("EINDEX:~p~n_", [LinkId]),
	G = fun()->
	Q = qlc:q([L#ldata.ind || L <- mnesia:table('clause$lnk'),
							  (L#ldata.to == LinkId) and 
							  ((element(2,L#ldata.id) == body) or
							  (element(2,L#ldata.id) == name) or
							  (element(2,L#ldata.id) == guard) or
							  (element(2,L#ldata.id) == pattern))]),			 
				qlc:e(Q)
	end,
	{atomic,LinkInd}=mnesia:transaction(G),
	%io:format("LinkInd: ~p~n", [LinkInd]),
	LinkInd.

collect_expr_index([]) -> [];
collect_expr_index(LinkId) ->
	%io:format("LinkId:~p~n", [LinkId]),
	G = fun()->
	Q = qlc:q([L#ldata.ind || L <- mnesia:table('expr$lnk'),
							   (L#ldata.to =:= LinkId) and
							   (element(2,L#ldata.id) == esub)]),	 
				qlc:e(Q)
	end,
	{atomic,[LinkInd]}=mnesia:transaction(G),
	%io:format("LinkInskfdadd:~p~n", [LinkInd]),
	LinkInd.

collect_exprs_id([]) -> [];
collect_exprs_id(Expr) ->
	G = fun()->
	Q = qlc:q([L#ldata.to || L <- mnesia:table('expr$lnk'),
							 (element(1,L#ldata.id) =:= Expr) and
							 (element(2,L#ldata.id) == esub)]),				 
		qlc:e(Q)
	end,
	{atomic,LinkToId}=mnesia:transaction(G),
	%io:format("LinkToId~p~n", [LinkToId]),
	LinkToId.

collect_expr_clause_id(Expr) ->
	%io:format("Expr:~p~n",[Expr]),
	G = fun()->
	Q = qlc:q([L#ldata.to || L <- mnesia:table('expr$lnk'),
							 (element(1,L#ldata.id) =:= Expr) and
							 ((element(2,L#ldata.id) == headcl) or
							 (element(2,L#ldata.id) == exprcl))]),
	%%headcl exprcl
		qlc:e(Q)
	end,
	{atomic,LinkToId}=mnesia:transaction(G),
	LinkToId.

collect_expr_clause_ind(ClauseId) ->
	G = fun()->
	Q = qlc:q([L#ldata.ind || L <- mnesia:table('expr$lnk'),
							  (L#ldata.to == ClauseId) and
							  ((element(2,L#ldata.id) == exprcl) or
							  (element(2,L#ldata.id) == headcl))]),				 
		qlc:e(Q)
	end,
	{atomic,LinkInd}=mnesia:transaction(G),
	[LinkInd].