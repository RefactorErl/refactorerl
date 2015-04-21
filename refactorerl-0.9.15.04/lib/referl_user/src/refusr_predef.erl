%%% -*- coding: utf-8 -*-

%%% The  contents of this  file are  subject to  the Erlang  Public License,
%%% Version  1.1, (the  "License");  you may  not  use this  file except  in
%%% compliance  with the License.  You should  have received  a copy  of the
%%% Erlang  Public License  along  with this  software.  If not,  it can  be
%%% retrieved at http://plc.inf.elte.hu/erlang/
%%%
%%% Software  distributed under  the License  is distributed  on an  "AS IS"
%%% basis, WITHOUT  WARRANTY OF ANY  KIND, either expressed or  implied. See
%%% the License  for the specific language governing  rights and limitations
%%% under the License.
%%%
%%% The Original Code is RefactorErl.
%%%
%%% The Initial Developer of the  Original Code is Eötvös Loránd University.
%%% Portions created  by Eötvös  Loránd University are  Copyright 2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @author Tibor Pusztai <kondi@elte.hu>

-module(refusr_predef).
-vsn("$Rev$").

-export([get_node_and_type/2, get_predef_queries/1, get_predef_queries/3,
         get_proper_node/2]).

-include("user.hrl").

%%%% Public interface

get_node_and_type(FilePath, Pos) ->
    FileNode = ui_request({graph_query, ?File, find, [FilePath]}),
    case graph_query([?Query:seq(FileNode, ?File:token(Pos))]) of
        [Token|_] -> get_node_and_type(Token);
        _         -> {error, {nonode, "No node found"}}
    end.

get_predef_queries(variable) ->
    [{"Variable References","@var.references"},
     {"Variable Bindings","@var.bindings"},
     {"Variable Origin","@expr.origin"},
     {"Variable Reach","@expr.reach"}];
get_predef_queries(funappl) ->
    [{"Function References","@fun.references"},
     {"Function Definition","@def"}];
get_predef_queries(funref) ->
    [{"Function References","@fun.references"},
     {"Function Definition","@def"}];
get_predef_queries(fundef) ->
    [{"Function References","@fun.references"}];
get_predef_queries(module) ->
    % @todo: maybe @file.funs.called_by can be used instead
    [{"Module References","@file.funs.refs"},
     {"Exported Functions","@file.exports"},
     {"Functions In Module","@file.funs"},
     {"Records In Module","@file.records"}];
get_predef_queries(recdef) ->
    [{"Record References","@record.references"},
     {"All field References","@record.fields.references"}];
get_predef_queries(recexpr) ->
    [{"Record References","@record.references"},
     {"Record Definition","@expr.records"},
     {"All Field References","@record.fields.references"}];
get_predef_queries(recfield) ->
    [{"Field References","@field.references"},
     {"All Field References","@field.record.references"}];
get_predef_queries(macrodef) ->
    [{"Macro Definition","@def"},
     {"Macro References","@macro.references"},
     {"Expand all references", "@macro.all_refs.macro_value"}];
get_predef_queries(_) ->
    [].

get_predef_queries(Type, File, Pos) ->
    StartOptions = [{file, File},{position, Pos}],
    Request = {transform, semantic_query,
                    [{ask_missing, false},
                    {send_back_query_id, true},
                    {querystr, "@expr.macro_value"},
                    {display_opt, [{output, 'nodes'}]},
                    {start_opt, StartOptions}]},
    case ui_request(Request) of
        {result,[{result,[{'nodes',[Data]},_]}]} when Data =/= "macro_value_not_found"->
            [{"Macro Value: " ++ Data, "@expr.macro_value"}|get_predef_queries(Type)];
        _ -> get_predef_queries(Type)
    end.

get_proper_node(Query, Node0) ->
    ExprQuery = lists:sublist(Query, 5) == "@expr",
    Node = if
        ExprQuery -> Node0;
        true      -> get_semantic_node(Node0)
    end,
    Type = get_node_type(Node),
    ValidStarts = case Type of
        macrodef ->    ["@expr","@expression","@macro"];
        module ->      ["@expr","@expression","@file","@module","@mod"];
        recdef ->      ["@expr","@expression","@record","@rec"];
        recexpr ->     ["@expr","@expression","@record","@rec"];
        %recfielddef -> ["@expr","@expression","@recfield","@field"];
        recfield ->    ["@expr","@expression","@recfield","@field"];
        variable ->    ["@expr","@expression","@variable","@var"];
        funappl ->     ["@expr","@expression","@function","@fun"];
        fundef ->      ["@expr","@expression","@function","@fun","@clause"];
        funref ->      ["@expr","@expression","@function","@fun","@clause"];
        atom ->        ["@expr","@expression"];
        _ ->           []
    end,
    case Type of
        none -> {error, "Unknown node. Did you check the errors?"};
        _    -> QueryStart = case string:tokens(Query, ".") of
                                 [Head|_] -> Head;
                                 []       -> ""
                             end,
                case lists:any(fun(X) -> QueryStart == X end, ValidStarts) of
                    true -> Node;
                    _    -> {error, "'" ++ QueryStart ++ "' " ++
                                    "is an illegal selector for this node."}
                end
    end.

%%%% Private functions

get_node_and_type(Token) ->
    Any = [ [{llex,back}, mref],                            %macro ref
            [{{elex, back}, {type, '==', record_access}}],  %recaccess
            [{{elex, back}, {type, '==', variable}}],       %var
            ?Query:seq(?Token:expr(), ?Expr:field()),       %field ref
            [{{elex, back}, {type, '==', record_expr}}],    %recexpr
            ?Query:seq(?Token:expr(), ?Expr:record()),      %recref
            ?Query:seq(?Token:typexp(), ?Expr:fielddef()),  %field def
            ?Query:seq(?Token:expr(), ?Expr:module()),      %modq
            ?Query:seq([?Token:expr(),
                        expr_parent_appl()]),               %funappl without mod qualifier
            ?Query:seq([lex_parent_attr(),
                        expr_parent_funref()]),             %funref without mod qualifier
            ?Query:seq([lex_parent_funref()]),              %funref /
            ?Query:seq([?Token:expr(),
                        expr_parent_implfun()]),            %implicit_fun without mod qualifier
            ?Query:seq([lex_parent_implfun()]),             %implicit_fun syntax elem (fun, /)
            ?Query:seq([?Token:expr(), ?Expr:parent(),
                        expr_parent_implfun()]),            %implicit_fun with mod qualifier
            ?Query:seq([?Token:expr(), ?Expr:parent(),      %funref without mod qualifier
                        ?Expr:function()]),
            ?Query:seq([?Token:expr(), ?Expr:parent(),
                        expr_parent_appl()]),               %funappl with mod qualifier
            ?Query:seq([?Token:expr(), ?Expr:parent(),
                        ?Expr:parent(), ?Expr:function()]), %funref infix
            ?Query:seq([?Token:expr(), ?Expr:nameof(),
                        ?Clause:form(), ?Form:func()]),     %fundef
            [{{flex, back}, {type, '==', macro}}],          %macro form
            [{{flex, back}, {type, '==', module}}],         %module form
            ?Query:seq(?Token:form(), ?Form:record()),      %rec form
            ?Token:expr()
            ],
    case graph_query(Token, ?Query:any(Any)) of
            []         -> {Token, none};
            [Node|_]   -> {Node, get_node_type(Node)};
            {error, E} -> {error, E};
            Error      -> {error, Error}
    end.

lex_parent_attr() ->
    [{{elex, back}, {role, '==', attr}}].

lex_parent_implfun() ->
    [{{elex, back}, {type, '==', implicit_fun}}].

lex_parent_funref() ->
    [{{elex, back}, {type, '==', funref}}].

expr_parent_funref() ->
    [{{esub, back}, {type, '==', funref}}].

expr_parent_appl() ->
    [{{esub, back}, {type, '==', application}}].

expr_parent_implfun() ->
    [{{esub, back}, {type, '==', implicit_fun}}].

get_node_type(Node) ->
    case ui_request({graph_data, Node}) of
        #form{type=module}        -> module;
        #form{type=record}        -> recdef;
        #typexp{type=spec_field}  -> recfield;
        #form{type=macro}         -> macrodef;
        #expr{type=application}   -> funappl;
        #expr{type=implicit_fun}  -> funref;
        #expr{type=funref}        -> funref;
        #clause{type=fundef}      -> fundef;
        #expr{type=variable}      -> variable;
        #expr{type=record_expr}   -> recexpr;
        #expr{type=record_field}  -> recfield;
        #expr{type=record_access} -> recexpr;
        #expr{type=record_update} -> recexpr;
        #expr{type=atom}          -> atom_type(Node);
        #variable{}               -> variable;
        #module{}                 -> module;
        #record{}                 -> recdef;
        #field{}                  -> recfield;
        #func{}                   -> fundef;
        _                         -> none
    end.

%% @doc Get the semantic node of a syntax or semantic node
get_semantic_node(Node) ->
    Type = case ui_request({graph_data, Node}) of
        #form{type=module}        -> module;
        #form{type=record}        -> recdef;
        #typexp{type=spec_field}  -> recfielddef;
        #expr{type=application}   -> funref;
        #expr{type=implicit_fun}  -> funref;
        #expr{type=funref}        -> funref;
        #clause{type=fundef}      -> fundef;
        #expr{type=variable}      -> variable;
        #expr{type=record_expr}   -> recexpr;
        #expr{type=record_field}  -> recfield;
        #expr{type=record_access} -> recexpr;
        #expr{type=record_update} -> recexpr;
        #expr{type=atom}          -> atom_type(Node);
        _                         -> semantic
    end,
    case Type of
        module      -> N=graph_query(Node, ?Form:module());
        recdef      -> N=graph_query(Node, ?Form:record());
        recfielddef -> N=graph_query(Node, [{fielddef, 1}]);
        variable    -> N=graph_query(Node, ?Expr:variables());
        funref      -> N=graph_query(Node, ?Query:any(?Expr:function(), ?Expr:dynfunction()));
        fundef      -> N=graph_query(Node, ?Query:seq([?Clause:form(), ?Form:func()]));
        recexpr     -> N=graph_query(Node, ?Expr:record());
        recfield    -> N=graph_query(Node, ?Expr:field());
        _           -> N=[Node]
    end,
    case N of
        [] -> Node;
        _  -> hd(N)
    end.

atom_type(Node) ->
    Path = graph_query(Node, ?Expr:fields()),
    if
        Path /= []        -> recfield;
        true              -> atom
    end.

graph_query(Start, GraphQuery) ->
    ui_request({graph_query, ?Query, exec, [Start, GraphQuery]}).

graph_query(GraphQuery) ->
    ui_request({graph_query, ?Query, exec, GraphQuery}).

ui_request(Request) ->
    ReqId = ?UI:getid(),
    FullResult = case ?UI:request(ReqId, Request) of
        deny -> {error, {deny, "UI request denied"}};
        ok -> ui_loop(ReqId)
    end,
    case FullResult of
        {ok, Result} -> Result;
        {error, E}   -> E;
        E            -> E
    end.

ui_loop(ReqId) ->
    receive
        {ReqId, reply, Reply} -> Reply;
        {ReqId, _, _}         -> ui_loop(ReqId)
    end.
