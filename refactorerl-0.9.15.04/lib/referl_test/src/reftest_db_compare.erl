-module(reftest_db_compare).

-export([start/4]).

-include_lib("referl_core/include/core_global.hrl").

start(Db1, Db2,Node1, Node2) ->
	monitor_node(Node1, true),
	monitor_node(Node2, true),
	KnownNodes = nodes([this, visible]),
	Check_Node1 = lists:member(Node1, KnownNodes),
	Check_Node2 = lists:member(Node2, KnownNodes),
	case {Check_Node1,Check_Node2} of
		{true,true}->
			io:format("START~n"),
			case {Db1,Db2} of
			    {mnesia,mnesia} ->
					start_compare(Node1,Node2,
								  reftest_db_comp_mnesia,reftest_db_comp_mnesia);
				{mnesia,nif} ->
					start_compare(Node1,Node2,
								  reftest_db_comp_mnesia,reftest_db_comp_nif);
				{nif,mnesia} ->
					start_compare(Node1,Node2,
								  reftest_db_comp_nif,reftest_db_comp_mnesia);
				{nif,nif} ->
					start_compare(Node1,Node2,
								  reftest_db_comp_nif,reftest_db_comp_nif);
				{nif,kyoto} ->
					start_compare(Node1,Node2,
								  reftest_db_comp_nif,reftest_db_comp_kyoto);
				{kyoto,nif} ->
					start_compare(Node1,Node2,
								  reftest_db_comp_kyoto,reftest_db_comp_nif);
				{kyoto, mnesia} ->
					start_compare(Node1,Node2,
								  reftest_db_comp_kyoto,reftest_db_comp_mnesia);
				{mnesia, kyoto} ->
					start_compare(Node1,Node2,
								  reftest_db_comp_mnesia, reftest_db_comp_kyoto);
				{kyoto, kyoto} ->
					start_compare(Node1,Node2,
								  reftest_db_comp_kyoto, reftest_db_comp_kyoto)
				
			end;
		_ ->
		   io:format("The ~p or ~p is not in the nodes.~n", [Node1,Node2])
	end.


start_compare(Node1,Node2,ModName1,ModName2) ->
	Files = collect_check_files(Node1, Node2, ModName1, ModName2),
	case Files of
		{Files1,Files2} ->
			io:format("Filecompare: true ~n"),
			Forms = collect_checks_forms(Node1,Node2,
										 ModName1, ModName2, Files1, Files2),
			case Forms of
				{Forms1,Forms2} ->
					io:format("FormCompare: true ~n"),
					Clauses = collect_check_clauses(Node1,Node2,
													ModName1, ModName2,
													Forms1, Forms2),
					case Clauses of
						{Clauses1,Clauses2} ->
							io:format("ClausesCompare: true ~n"),
							Expressions = 
								collect_check_expressions(Node1,Node2,
														  ModName1, ModName2,
														  Clauses1, Clauses2),
							case Expressions of
								{Expressions1,Expressions2} ->
									io:format("ExpressionCompare: true ~n"),
									recursive(Node1,Node2,
											  ModName1,ModName2,
											  Expressions1,Expressions2),
									io:format("End. ~n");
								exp_data_fail ->
									io:format("Compare Error: expressions ~n");
								exp_index_fail ->
									io:format("Compare Error: expressionIndex ~n")
							end;	
						clause_index_fail ->
							io:format("Compare Error: clauses ~n")
					end;
				form_fail ->
					io:format("Compare Error: forms ~n");
				form_index_fail ->
					io:format("Compare Error: formIndex ~n")
			end;			
		error ->
			io:format("Compare Error: files ~n")
	end.

collect_check_files(Node1,Node2,ModName1,ModName2) ->
	FileDataFun1 = apply(ModName1,file,[Node1]),
	FileDataFun2 = apply(ModName2,file, [Node2]),
	
	case check_files(FileDataFun1,FileDataFun2) of
		check_fail ->
			io:format("Compare Error: files"),
			error;
		_ ->
			Files1 = lists:flatten([apply(ModName1,file_id,[Node1,FileData1])
								   || FileData1 <-FileDataFun1]),
			
			Files2 = lists:flatten([apply(ModName2,file_id,[Node2,FileData2])
								   || FileData2 <-FileDataFun2]),
		  	{Files1,Files2}
	end.

collect_checks_forms(Node1,Node2,ModName1,ModName2,FilesId1, FilesId2) ->
	FormIndex1 = [apply(ModName1,form_index,[Node1,FileIds1])
				                    || FileIds1 <-FilesId1],
	FormIndex2 = [apply(ModName2,form_index,[Node2,FileIds2])
				                    || FileIds2 <-FilesId2],
	
	if
		(FormIndex1 == FormIndex2) ->
			io:format("FormIndexCompare: true ~n"),
			FormsId1 = [apply(ModName1,file_lnk,[Node1,FileId1])
					                    || FileId1 <-FilesId1],
			FormsId2 = [apply(ModName2,file_lnk,[Node2,FileId2])
					                    || FileId2 <-FilesId2],
			FormData1 = lists:sort([lists:sort([
							apply(ModName1,form,[Node1,FormId1])
									|| FormId1 <-Id1]) || Id1 <- FormsId1]),
			FormData2 = lists:sort([lists:sort([
							apply(ModName2,form,[Node2,FormId2])
									|| FormId2 <-Id2]) || Id2 <- FormsId2]),
			if 
				(FormData1 == FormData2) ->
				  {FormsId1, FormsId2};
			  true ->
				  form_fail
			end;				  
		true ->
			form_index_fail
	end.

collect_check_clauses(Node1, Node2, ModName1, ModName2, FormsId1, FormsId2) ->
	Clauses1 = [[apply(ModName1,clauses,[Node1,FormId1]) || FormId1 <-Id1]
			                        || Id1 <- FormsId1],
	Clauses2 = [[apply(ModName2,clauses,[Node2,FormId2]) || FormId2 <-Id2]
			                        || Id2 <- FormsId2],
	
	case ModName1 of
		reftest_db_comp_mnesia ->
			ClauseIndex1 = lists:sort([lists:sort([lists:flatten(lists:sort([
									apply(ModName1,clause_index,[Node1,C1])
										|| C1 <-Cls1])) || Cls1 <- Clause1])
									    || Clause1 <- Clauses1]);
		_ ->
			ClauseIndex1 = lists:sort([lists:sort([lists:flatten(lists:sort([
									apply(ModName1,clause_index,[Node1,FormsId1,C1])
										|| C1 <-Cls1])) || Cls1 <- Clause1])
									    || Clause1 <- Clauses1])
	end,
	case ModName2 of
		reftest_db_comp_mnesia ->
			ClauseIndex2 = lists:sort([lists:sort([lists:flatten(lists:sort([
								 	apply(ModName2,clause_index,[Node2,C2])
										|| C2 <-Cls2])) || Cls2 <- Clause2])
									    || Clause2 <- Clauses2]);
		_ ->
			ClauseIndex2 = lists:sort([lists:sort([lists:flatten(lists:sort([
									apply(ModName2,clause_index,[Node2,FormsId2,C2])
										|| C2 <-Cls2])) || Cls2 <- Clause2])
									    || Clause2 <- Clauses2])
	end,

	if
		(ClauseIndex1 == ClauseIndex2) ->
			{Clauses1, Clauses2};
		true ->
			clause_index_fail
	end.

collect_check_expressions(Node1,Node2,ModName1,ModName2,ClausesId1, ClausesId2) ->
	Exp1 = [[lists:sort([apply(ModName1,expr,[Node1,C1]) || C1 <-Cls1])
			               || Cls1 <- Clause1]||  Clause1 <-ClausesId1],
	Exp2 = [[lists:sort([apply(ModName2,expr,[Node2,C2]) || C2 <-Cls2])
			               || Cls2 <- Clause2]||  Clause2 <-ClausesId2],
	case ModName1 of
		reftest_db_comp_mnesia ->
			ExpIndex1 = [[lists:sort(lists:flatten([[
								apply(ModName1,expr_index,[Node1,E1])
								|| E1 <- Expr1] || Expr1 <- Exprs1]))
						        ||Exprs1 <- Expression1]
						        || Expression1 <- Exp1];
		_ ->
			ExpIndex1 = [[lists:sort(lists:flatten([[
								apply(ModName1,expr_index,[Node1,ClausesId1,E1])
									|| E1 <- Expr1]|| Expr1 <- Exprs1]))
						            ||Exprs1 <- Expression1]|| Expression1 <- Exp1]
	end,
	case ModName2 of
		reftest_db_comp_mnesia ->
			ExpIndex2 = [[lists:sort(lists:flatten([[
								apply(ModName2,expr_index,[Node2,E2])
									|| E2 <- Expr2]|| Expr2 <- Exprs2]))
						            || Exprs2 <- Expression2] || Expression2 <- Exp2];
		_ ->
			ExpIndex2 = [[lists:sort(lists:flatten([[
								apply(ModName2,expr_index,[Node2,ClausesId2,E2])
									|| E2 <- Expr2]|| Expr2 <- Exprs2]))
						            || Exprs2 <- Expression2] || Expression2 <- Exp2]
	end,
	ExpData1 = [[lists:sort(lists:flatten([[apply(ModName1,expr_data,[Node1,EId1])
								    || EId1 <-ExprsId1]|| ExprsId1<-ExprId1]))
				                    || ExprId1 <- ExpId1] || ExpId1 <- Exp1],
	ExpData2 = [[lists:sort(lists:flatten([[apply(ModName2,expr_data,[Node2,EId2])
								    || EId2 <-ExprsId2]|| ExprsId2<-ExprId2]))
				                    || ExprId2 <- ExpId2] || ExpId2 <- Exp2],
	
	if
		(ExpIndex1 == ExpIndex2) ->
			io:format("ExpIndex: true ~n"),
			if
				(ExpData1 == ExpData2) ->
					io:format("ExpData: true ~n"),
					{Exp1,Exp2};
				true ->
					exp_data_fail
			end;
		true ->
			
			exp_index_fail
	end.
						
check_files([],[]) ->
	true;
check_files([H|T],[H2|T2]) ->
	if
		((H#file.type =:= H2#file.type) and (H#file.path =:= H2#file.path)) ->
			check_files(T,T2);		
		true ->
			check_fail
	end.

recursive(Node1,Node2,ModName1,ModName2,Exp1,Exp2) ->
	
	ExpId1 = [[[[apply(ModName1,collect_expr_id,[Node1,Expr1]) || Expr1 <-Exprs1]
			   ||Exprs1<-Expression1]|| Expression1 <- Expressions1 ]
			   || Expressions1 <- Exp1],
	
	ExpId2 = [[[[apply(ModName2,collect_expr_id,[Node2,Expr2]) || Expr2 <-Exprs2]
			   ||Exprs2<-Expression2]|| Expression2 <- Expressions2 ]
			   || Expressions2 <- Exp2],
	
	ClauseId1 = [[[[apply(ModName1,collect_expr_cls_id,[Node1,ExpCls1])
				   || ExpCls1<-ExpClause1]|| ExpClause1 <- ExpClauses1]
				   ||ExpClauses1<-ExpressionClauses1] 
				   || ExpressionClauses1 <- Exp1],
	
	ClauseId2 = [[[[apply(ModName2,collect_expr_cls_id,[Node2,ExpCls2])
				   || ExpCls2<-ExpClause2]|| ExpClause2 <- ExpClauses2]
				   ||ExpClauses2<-ExpressionClauses2]
				   || ExpressionClauses2 <- Exp2],
	
	Expr1 = lists:sort([lists:sort(lists:flatten([[[[
					apply(ModName1,expr_data,[Node1,RExp1]) || RExp1 <-RecExpr1]
							||RecExpr1<-RecExprs1]|| RecExprs1 <-RecExpression1]
							|| RecExpression1 <- RecExpressions1]))
					        || RecExpressions1 <- ExpId1, RecExpressions1 /=[]]),
	
	Expr2 = lists:sort([lists:sort(lists:flatten([[[[
					apply(ModName2,expr_data,[Node2,RExp2]) || RExp2 <-RecExpr2]
							||RecExpr2<-RecExprs2]|| RecExprs2 <-RecExpression2]
							|| RecExpression2 <- RecExpressions2]))
					        || RecExpressions2 <- ExpId2, RecExpressions2 /=[]]),
		
	case ModName1 of
		reftest_db_comp_mnesia ->
			ClauseIndex1 = [[lists:sort(lists:flatten([[[
					apply(ModName1,collect_expr_cls_ind,[Node1,RecCls1])
							|| RecCls1 <-RecClause1]||RecClause1<-RecClauses1]
							||RecClauses1<-RClause1])) ||RClause1 <- RClauses1]
						    || RClauses1 <- ClauseId1],
			
			ExpIndex1 = lists:sort([lists:sort(lists:flatten([[[[
					apply(ModName1,collect_expr_ind,[Node1,RecExpInd1])
							|| RecExpInd1 <-RecExpressionInd1]
							||RecExpressionInd1<-RecExpIndex1]
							||RecExpIndex1<-RecExpressionIndex1]
							||RecExpressionIndex1<-RecExpressionsIndex1]))
							||RecExpressionsIndex1<-ExpId1, RecExpressionsIndex1/=[]]);
		_ ->
			ClauseIndex1 = [[lists:sort(lists:flatten([[[
					apply(ModName1,collect_expr_cls_ind,[Node1,Exp1,RecCls1])
							|| RecCls1 <-RecClause1]||RecClause1<-RecClauses1]
							||RecClauses1<-RClause1])) ||RClause1 <- RClauses1]
						    || RClauses1 <- ClauseId1],
			
			ExpIndex1 = lists:sort([lists:sort(lists:flatten([[[[
					apply(ModName1,collect_expr_ind,[Node1,Exp1,RecExpInd1])
							|| RecExpInd1 <-RecExpressionInd1]
						    ||RecExpressionInd1<-RecExpIndex1]
							||RecExpIndex1<-RecExpressionIndex1]
							||RecExpressionIndex1<-RecExpressionsIndex1]))
						    ||RecExpressionsIndex1<-ExpId1, RecExpressionsIndex1/=[]])
	end,
	
	case ModName2 of
		reftest_db_comp_mnesia ->
			ClauseIndex2 =[[lists:sort(lists:flatten([[[
				    apply(ModName2,collect_expr_cls_ind,[Node2,RecCls2])
						    ||RecCls2 <- RecClause2]||RecClause2 <- RecClauses2]
							||RecClauses2 <- RClause2]))||RClause2 <- RClauses2]
						    ||RClauses2  <- ClauseId2],
			
			ExpIndex2 =lists:sort([lists:sort(lists:flatten([[[[
					apply(ModName2,collect_expr_ind,[Node2,RecExpInd2])
							||RecExpInd2<-RecExpressionInd2]
							||RecExpressionInd2<-RecExpIndex2]
							||RecExpIndex2<-RecExpressionIndex2]
							||RecExpressionIndex2<-RecExpressionsIndex2]))
						    ||RecExpressionsIndex2<-ExpId2, RecExpressionsIndex2/=[]]);
		_ ->
			ClauseIndex2 =[[lists:sort(lists:flatten([[[
					apply(ModName2,collect_expr_cls_ind,[Node2,Exp2,RecCls2])
							||RecCls2 <- RecClause2]||RecClause2 <- RecClauses2]
							||RecClauses2 <- RClause2]))||RClause2 <- RClauses2]
						    ||RClauses2  <- ClauseId2],
			
			ExpIndex2 =lists:sort([lists:sort(lists:flatten([[[[
				   apply(ModName2,collect_expr_ind,[Node2,Exp2,RecExpInd2])
							||RecExpInd2<-RecExpressionInd2]
							||RecExpressionInd2<-RecExpIndex2]
							||RecExpIndex2<-RecExpressionIndex2]
							||RecExpressionIndex2<-RecExpressionsIndex2]))
						    ||RecExpressionsIndex2<-ExpId2, RecExpressionsIndex2/=[]])
	end,
	
	FlattenClauseIndex1 = lists:sort(lists:flatten(ClauseIndex1)),
	FlattenClauseIndex2 =lists:sort(lists:flatten(ClauseIndex2)),

	IF =
		if
			((FlattenClauseIndex1 == FlattenClauseIndex2) and
				                (ExpIndex1 == ExpIndex2))->
				io:format("Recursive_Clause_Exp_Indexes_Compare: true ~n"),
				if
					(Expr1 == Expr2) ->
						io:format("Recursive_Clause_Exp_Compare: true ~n"),						
						recursive_compare(Node1,Node2,
										 ModName1,ModName2,
										 ExpId1,ExpId2,
										 ClauseId1,ClauseId2);
					true ->
						io:format("Compare Error: recursive clause expressions")
				end;
			true ->
				io:format("Compare Error: recursive expressions index")
		end,
	IF.

recursive_compare(Node1,Node2,ModName1,ModName2,Expr1,Expr2,Clause1,Clause2) ->
	case {lists:flatten(Expr1), lists:flatten(Expr2)} of
		{[],[]} -> expressions_end;
		_ -> 
			recursive(Node1,Node2,ModName1,ModName2,Expr1,Expr2)
	end,
	
	case {lists:flatten(Clause1),lists:flatten(Clause2)} of
		{[],[]} ->
			clauses_end;
		_ ->
			ExpClause1 = [apply(ModName1,expr,[Node1,Cls1]) || Cls1 <-Clause1],
			ExpClause2 = [apply(ModName2,expr,[Node2,Cls2]) || Cls2 <-Clause2],
			
			Exp1 = lists:sort(lists:flatten([apply(ModName1,expr_data,[Node1,Cls1])
											 || Cls1 <-ExpClause1])),
			Exp2 = lists:sort(lists:flatten([apply(ModName2,expr_data,[Node2,Cls2])
											 || Cls2 <-ExpClause2])),
			if
				(Exp1 == Exp2) ->
					io:format("RecExp compare: true ~n"),
					recursive(Node1,Node2,
							  ModName1,ModName2,
							  ExpClause1,ExpClause2);
				true ->
					io:format("Compare Error: recursive expressions")
			end
	end.
