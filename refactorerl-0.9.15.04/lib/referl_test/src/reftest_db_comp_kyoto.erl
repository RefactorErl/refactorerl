-module(reftest_db_comp_kyoto).

-include_lib("referl_core/include/core_global.hrl").

-export([file/1, file_id/2, form/2, file_lnk/2, form_index/2, clauses/2, clause_index/3]).
-export([expr_index/3, collect_expr_ind/3, expr/2, expr_data/2]).
-export([collect_expr_id/2, collect_expr_cls_id/2, collect_expr_cls_ind/3]).

file(Node) ->
	{ok,Root} = rpc:call(Node ,refdb_kyotomini, root, []),
	{ok,Child} = rpc:call(Node, refdb_kyotomini, links, [Root]),
	FileNode = [FNode || {file,FNode} <- Child],
	Data = [rpc:call(Node, refdb_kyotomini, data, [FN]) || FN <- FileNode],
	FileList = [FileData || {ok, FileData} <- Data],
	lists:sort(FileList).

file_id(Node,File) ->
	{ok,Root} = rpc:call(Node ,refdb_kyotomini, root, []),
	{ok,Child} = rpc:call(Node, refdb_kyotomini,links, [Root]),
	FileNode = [FNode || {file,FNode} <- Child],
	[FNode || FNode <- FileNode,
			  rpc:call(Node, refdb_kyotomini, data, [FNode]) == {ok, File}].

file_lnk(Node,File) ->
	{ok,FileChild} = rpc:call(Node, refdb_kyotomini, links, [File]),
	[FormNode || {form, FormNode} <- FileChild].

form(Node,FormNode) ->
	{ok,Data} = rpc:call(Node, refdb_kyotomini, data, [FormNode]),
	Data.

form_index(Node, FNode) ->
	{ok,FileChild} = rpc:call(Node, refdb_kyotomini, links, [FNode]),
	FormNodes = [FormNode || {form, FormNode} <- FileChild],
	Indexes = lists:sort([
			  rpc:call(Node, refdb_kyotomini, index, [FNode,form,FormNode])
						 || FormNode <- FormNodes]),
	[Index||{ok,Index}<-Indexes].

clauses(Node,Form)->
	{ok,Clauses} = rpc:call(Node, refdb_kyotomini, links, [Form]),
	[ClNode || {funcl, ClNode} <- Clauses].

clause_index(Node, _Forms, ClauseNode) ->
	Indexes =[[rpc:call(Node, refdb_kyotomini, index,[F,funcl,ClauseNode])
			  || F<-Form]|| Form <- _Forms],
	Index = [[I ||{ok,I} <- Inds, I/=none]||Inds <- Indexes],
	Index.

expr_index(Node, Clauses, ExpNode) ->
	{Tag,ENode} = ExpNode,
	Indexes =[[[rpc:call(Node, refdb_kyotomini, index,[C,Tag,ENode])
			   || C<-Cls]||Cls<-Clause]|| Clause <- Clauses],
	Index = [[[I||{ok,I}<- Ids, I/=none] ||Ids <- Inds]||Inds <- Indexes],
	Index.

collect_expr_ind(_,_,[]) -> [];
collect_expr_ind(Node, Exprs, ExpNode) ->
	{Type,ENode} = ExpNode,	
	Indexes =[[[[rpc:call(Node, refdb_kyotomini, index,[E,Type,ENode])
				|| {_,E} <- Exp]||Exp<-Expr]||Expr<-Expression]
			    || Expression <- Exprs],
	[[[[ExpClsInd||{ok,ExpClsInd}<-ExpClauseInd,ExpClsInd/=none]
	            ||ExpClauseInd<-ExpClauseIndex ]||ExpClauseIndex <- Index]
	            ||Index <- Indexes].

expr(_Node,[]) ->	[];
expr(Node,Clause) ->
	case Clause of
		{_,clause,_} ->
			{ok,CNode} = rpc:call(Node, refdb_kyotomini, links, [Clause]),
			[ {Lnk,{GN,expr,Ind}} || {Lnk,{GN,expr,Ind}} <- CNode, Lnk /= visib];
		{_,GNode} ->
			case GNode of
				{_,clause,_} ->
					{ok,Node} = rpc:call(Node, refdb_kyotomini, links, [GNode]),
					[ {Lnk,{GN,expr,Ind}}
					          || {Lnk,{GN,expr,Ind}} <- Node, Lnk /= visib];
				_ -> []
			end;
		_ -> []
	end.

collect_expr_id(_Node,[]) -> [];
collect_expr_id(Node,[Exp|Tail]) ->
	collect_expr_id(Node,Exp),
	collect_expr_id(Node,Tail);
collect_expr_id(Node,Expr) ->
	{_,GNode} = Expr,
	{ok,ENode} = rpc:call(Node, refdb_kyotomini, links, [GNode]),
	[ {Lnk,{GN,expr,Ind}} || {Lnk,{GN,expr,Ind}} <- ENode, Lnk == esub].

expr_data(_Node,[]) -> [];
expr_data(Node,[Exp|Tail]) ->
	expr_data(Node,Exp),
	expr_data(Node,Tail);
expr_data(Node,Exp) ->
	{_,ENode} = Exp,
			{ok,ExpData} = rpc:call(Node, refdb_kyotomini, data, [ENode]),
			[ExpData].

collect_expr_cls_id(_Node,[]) -> [];
collect_expr_cls_id(Node,[N|Tail]) ->
	collect_expr_cls_id(Node,N),
	collect_expr_cls_id(Node,Tail);
collect_expr_cls_id(Node,Nodes) ->
	{_,ClNode} = Nodes, 
	{ok,List} = rpc:call(Node, refdb_kyotomini, links, [ClNode]),
	[{Typ,{Gn,Type,Ind}} || {Typ,{Gn,Type,Ind}} <- List, 
							(Type == clause) and 
							((Typ == exprcl) or 
							(Typ == headcl))].

collect_expr_cls_ind(_,_,[]) -> [];
collect_expr_cls_ind(Node, Exprs, ClauseNode) ->
	{_,CNode} = ClauseNode,
	Indexes =[[[[rpc:call(Node, refdb_kyotomini, index,[E,headcl,CNode])
				|| {_,E} <- Exp]||Exp<-Expr]
			    ||Expr<-Expression]|| Expression <- Exprs] ++
				 [[[[rpc:call(Node, refdb_kyotomini, index, [E,exprcl,CNode])
				  || {_,E}<-Exp]||Exp<- Expr]||Expr<-Expression]||Expression<- Exprs],
	[[[[ExpClsInd||{ok,ExpClsInd}<-ExpClauseInd,ExpClsInd/=none]
	  ||ExpClauseInd<-ExpClauseIndex ]||ExpClauseIndex <- Index]
	  ||Index <- Indexes].