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

%%% @author Tamas Hoffman <hoffmantamas@caesar.elte.hu>

%TODO: macros in funs in macros, open trees at filefilter

% branch for errors (new element/action, under header)

-module(referl_htmlgen).
-vsn("$Rev: 9568 $").

-export([generate/1,generate_node/2]).

-include_lib("referl_user/src/user.hrl").

%% @spec generate(string()) -> string()
%% @doc Returns a html text with links on specific nodes.
generate([_|_]=Filename) ->
    Node=?Query:exec(?File:find(Filename)),
    if 
        length(Node)/=1 ->
            error;
        true ->
            lists:flatten(tree_text(hd(Node)))
    end.
    
%% @spec generate_node(node(), integer()) -> string()
%% @doc Returns a html text with links on specific nodes starting from Node
%% with given window info.  
generate_node({_,_,_}=Node,Window) ->
    lists:flatten(tree_text_node(Node,Window)).

%% @doc Returns the textual representation of the syntactical subtree that
%% starts at `Top' as a deep list.
tree_text(Top) ->
    [html_conversion(T#token.prews,comment)
     ++ add_links(LexNode,T,Data) 
     ++ html_conversion(T#token.postws,comment) || 
         {LexNode,T,Data} <- leaves(Top)].

%% @doc Add links to the lexical data
add_links(LexNode={_,_,Id},T,Data) ->
    add_linkstart(LexNode,Data,true) 
    ++ highLight(T#token.type,html_conversion(T#token.text)) 
    ++ add_linkend(Id,lists:reverse(Data)).
add_linkstart(_,[],_) -> 
    [];
add_linkstart(LexNode={_,_,Id},[{From,_,Type,{_,PType,PId}}|Datas],First) 
when (Id==From) and (First) -> 
    "<a name=jump_" ++ atom_to_list(PType) ++ integer_to_list(PId)
    ++ " class='mylink" ++ highLight(Type)++ "' href='javascript:void(0)' 
       onclick=javascript:selectednode=this;cd.value='"
    ++ atom_to_list(PType) ++ "|" ++ integer_to_list(PId)
    ++ "';$(cb).click();>" 
    ++ add_linkstart(LexNode,Datas,false);
add_linkstart(LexNode={_,_,Id},[{From,_,Type,{_,PType,PId}}|Datas],_) 
when (Id==From) -> 
    "<span name=jump_" ++ atom_to_list(PType) ++ integer_to_list(PId) 
    ++ " class='mylink" ++ highLight(Type) ++ "'>" 
    ++ add_linkstart(LexNode,Datas,false);
add_linkstart(LexNode,[_|Datas],_) -> 
    add_linkstart(LexNode,Datas,false).

add_linkend(_,[]) -> 
    [];
add_linkend(Id,[{_,To,_,_}|[]]) when (Id==To) -> 
    "</a>" ++ add_linkend(Id,[]);
add_linkend(Id,[{_,To,_,_}|Datas]) when (Id==To) -> 
    "</span>" ++ add_linkend(Id,Datas);
add_linkend(Id,[_|Datas]) -> 
    add_linkend(Id,Datas).

%% @doc Returns the textual representation of the syntactical subtree that
%% starts at `Top' as a deep list, with an added window info.
tree_text_node(Top,Window) ->
    [html_conversion(T#token.prews,comment)
     ++ add_links_node(LexNode,T,Data,Window) 
     ++ html_conversion(T#token.postws,comment) || 
         {LexNode,T,Data} <- leaves(Top)].

add_links_node(LexNode={_,_,Id},T,Data,Window) ->
    add_linkstart_node(LexNode,Data,true,Window) 
    ++ highLight(T#token.type,html_conversion(T#token.text)) 
    ++ add_linkend(Id,lists:reverse(Data)).

add_linkstart_node(_,[],_,_) -> 
    [];
add_linkstart_node(LexNode={_,_,Id},[{From,_,Type,Parent}|Datas],First,Window)
when (Id==From) and (First) -> 
    {_,PType,PId}=Parent,
    "<a name=jump_" ++ atom_to_list(PType) ++ integer_to_list(PId)
    ++ " class='mylink" ++ highLight(Type) 
    ++ "' href='javascript:void(0)' 
         onclick=javascript:selectednode=this;cd.value='"
    ++ atom_to_list(PType) ++ "|" ++ integer_to_list(PId) 
    ++ "|" ++ integer_to_list(Window)
    ++ "';$(cb).click();>" 
    ++ add_linkstart_node(LexNode,Datas,false,Window);
add_linkstart_node(LexNode={_,_,Id},[{From,_,Type,Parent}|Datas],_,Window) 
when (Id==From) ->
    {_,PType,PId}=Parent, 
    "<span name=jump_" ++ atom_to_list(PType) ++ integer_to_list(PId)
    ++ " class='mylink" ++ highLight(Type) ++ "'>" 
    ++ add_linkstart_node(LexNode,Datas,false,Window);
add_linkstart_node(LexNode,[_|Datas],_,Window) -> 
    add_linkstart_node(LexNode,Datas,false,Window).

%% @doc Returns the leaves of the syntactical subtree that starts at `Top',
%% in the correct syntactical order. This yields the original token nodes of
%% the subtree with attached data for links.
leaves(Top) ->
    Nodes = ets:new(visited_nodes, [set]),
    try
        {From,To,Type}=interval_helper(find_tokens(Top)),
        if
            From==0 -> NextSearchNodes=[{0,0,0,0}];
            (From>0) or (To<0) -> NextSearchNodes=[{From,To,Type,Top}];
            true -> NextSearchNodes=[{0,0,0,0}]++[{From,To,Type,Top}]
        end,
        leaves(Top, Nodes, NextSearchNodes,false)
    after
        ets:delete(Nodes)
    end.

leaves(Top={_,_,Id}, Nodes, SearchIntervals=[{_,Max,_,_}|_],InMacro) ->
    case ets:lookup(Nodes, Top) of
        [] ->
            ets:insert(Nodes, {Top}),
            case refcore_syntax:children(Top) of
                [] ->  
                    Data=get_data(Top,SearchIntervals,InMacro),
                    #lex{data=T=#token{}}=?ESG:data(Top),
                    if              
                        (Id>Max) and (not InMacro) -> {Top,T,[]};                  
                        true -> {Top,T,Data}
                    end;
                Children ->
                    NodeData=?ESG:data(Top),
                    case NodeData of
                        #lex{type=subst} -> IM=true;
                        _ -> IM=InMacro
                    end,
                    lists:flatten([check_children(C,Nodes,SearchIntervals,IM) ||
                        {_,C} <- Children])
            end;
        _  ->
            []
    end.

%% @doc Returns data associated with the node Top
get_data(Top,SearchIntervals,InMacro) when InMacro ->
    Node=find_orig_node(Top),
    if
        Node==[] -> SearchIntervals;
        true ->
            {_,_,Type}=interval_helper(find_tokens(hd(Node))),
            if
                Type==none -> [];
                true -> 
                    try 
                        check_macro_for_fun(hd(Node),id(Top),Type)
                        ++ [{id(Top),id(Top),Type,hd(Node)}]
                    catch
                        _ -> [] 
                    end
            end
    end;
get_data(_,SearchIntervals,_) -> SearchIntervals.

%% @doc Checks a macro for function applications
check_macro_for_fun(N,Id,T) when T==atom ->
    Node=?Query:exec1(N,[{esub,back}],error),
    NodeData=?ESG:data(Node),
    case NodeData of
        #expr{type=infix_expr} ->
            ApplicationNode=?Query:exec1(Node,[{esub,back}],error);
        _ ->
            ApplicationNode=Node
    end,
    {Nodes,_}=get_application_nodes(ApplicationNode),
    if
        (Node==N) -> [];
        true ->
            %Tokens=lists:usort(lists:flatten([check_macro(Nd) || Nd<-Nodes])),
            LexTokens=lists:map(fun(X) -> deep_orig_search(X) end, Nodes),
            {From,To,Type}=interval_helper({LexTokens,funappl}),
            if
                (From=<Id) and (Id=<To) -> [{From,To,Type,ApplicationNode}];
                true -> []
            end
    end;
check_macro_for_fun(_,_,_) -> [].

%% @doc Get data for Node, and update the searchinterval
check_children(Node,Nodes,SearchIntervals=[{Min,Max,_,_}|_],IM) ->
    {From,To,Type}=interval_helper(find_tokens(Node)),
    if
        From==0 -> NextSearchNodes=SearchIntervals;
        (From>Max) or (To<Min) -> NextSearchNodes=[{From,To,Type,Node}];
        true -> NextSearchNodes=SearchIntervals++[{From,To,Type,Node}]
    end,
    leaves(Node, Nodes, NextSearchNodes, IM).

%% @doc Returns lexical node intervals for paths
interval_helper({[],Type}) -> {0,0,Type};
interval_helper({Nodes,Type}) ->
    LexTokens=lists:usort(lists:flatten([check_macro(N) || N<-Nodes])),
    if
        LexTokens==[] -> {0,0,Type};
        true -> {id(hd(LexTokens)),id(lists:last(LexTokens)),Type}
    end.

%% @doc Returns paths for lexical nodes that define Node
find_tokens(Node) ->
    NodeData=?ESG:data(Node),
    %?d(NodeData),
    case NodeData of
        #typexp{type=spec_field} ->
            {?Query:exec(Node,[{tlex,1}]),
            recfielddef};
        #form{type=module} ->
            {?Query:exec(Node,[{flex,2}]),
            mod};
        #form{type=record} ->
            {?Query:exec(Node,[{flex,2}]),
            recdef};
        #form{type=macro} ->
            {?Query:exec(Node,[{flex,2}]),
            macrodef};
        #expr{type=application} ->
            get_application_nodes(Node);
        #expr{type=implicit_fun} ->
            get_application_nodes(Node);
        #expr{type=funref} ->
            {[?Query:exec1(Node,[{esub,1},{elex,1}],error),
                ?Query:exec1(Node,[{elex,1}],error),
                ?Query:exec1(Node,[{esub,2},{elex,1}],error)],
            funappl};
        #clause{type=fundef} ->
            {?Query:exec(Node,[{name,1},{elex,1}]),
            fundef};    
        #expr{type=variable,role=R} when R/=attr ->
            {?Query:exec(Node,[{elex,1}]),
            var};
        #expr{type=record_expr} ->
            {?Query:exec(Node,[{elex,2}]),
            recexpr};
        #expr{type=record_field} ->
            {?Query:exec(Node,[{elex,1}]),
            recfield};
        #expr{type=record_access} ->
            {?Query:exec(Node,[{elex,2}]),
            recexpr};
        #expr{type=record_update} ->
            {?Query:exec(Node,[{elex,2}]),
            recexpr};
        #expr{type=atom} ->
            Path=?Query:exec(Node,?Expr:fields()),
            if
                Path/=[] -> {?Query:exec(Node,[{elex,1}]),
                            recfield};
                true -> {?Query:exec(Node,[{elex,1}]),
                        atom}
            end;
        #form{type=error} ->
            {[],error};
        _ -> 
            {[],none}
    end.

%% @doc Get nodes for a function application
get_application_nodes(Node) ->
    N=?Query:exec1(Node,[{esub,1}],error),
    NodeData=?ESG:data(N),
    case NodeData of
        #expr{type=infix_expr} ->
                {[?Query:exec1(N,[{esub,1},{elex,1}],error),
                ?Query:exec1(N,[{elex,1}],error),
                ?Query:exec1(N,[{esub,2},{elex,1}],error)],funappl};
        #expr{type=variable} ->{?Query:exec(Node,[{elex,1}]),var}; 
        #expr{} -> 
            {?Query:exec(N,[{elex,1}]),funappl}
    end.

%% @doc Check if we're in a macro
check_macro([Node]) -> check_macro(Node);
check_macro(Node) ->
    NodeData=?ESG:data(Node),
    case NodeData of
        #lex{type=token,data=virtual} ->
            %HasParam=?Query:exec(Node,[{llex,1},{llex,4}]),
            Nodes=[?Query:exec1(Node,[{llex,1},{llex,1}],error),
                ?Query:exec1(Node,[{llex,1},{llex,2}],error)],
            [check_macro(N) || N<-Nodes];
        _ -> 
            Node
    end.

%%% ============================================================================
%%% Html specific functions

highLight(comment,Text)  -> "<span class='hl_comment'>"++Text++"</span>";
highLight(variable,Text) -> "<span class='hl_var'>"    ++Text++"</span>";
highLight(string,Text)   -> "<span class='hl_string'>" ++Text++"</span>";
highLight('case',Text)   -> "<span class='hl_keyword'>"++Text++"</span>";
highLight('of',Text)     -> "<span class='hl_keyword'>"++Text++"</span>";
highLight('end',Text)    -> "<span class='hl_keyword'>"++Text++"</span>";
highLight('if',Text)     -> "<span class='hl_keyword'>"++Text++"</span>";
highLight('when',Text)   -> "<span class='hl_keyword'>"++Text++"</span>";
highLight('fun',Text)    -> "<span class='hl_keyword'>"++Text++"</span>";
highLight('receive',Text)-> "<span class='hl_keyword'>"++Text++"</span>";
highLight('try',Text)    -> "<span class='hl_keyword'>"++Text++"</span>";
highLight('catch',Text)  -> "<span class='hl_keyword'>"++Text++"</span>";
highLight('module',Text) -> "<span class='hl_keyword'>"++Text++"</span>";
highLight('export',Text) -> "<span class='hl_keyword'>"++Text++"</span>";
highLight(_,Text) -> Text.

highLight(recdef)      -> " hl_keyword";
highLight(recfielddef) -> " hl_recfield";
highLight(macrodef)    -> " hl_keyword";
highLight(fundef)      -> " hl_fundef";
highLight(funappl)     -> " hl_funappl";
highLight(recexpr)     -> " hl_rec";
highLight(recfield)    -> " hl_recfield";
highLight(recaccess)   -> " hl_recfield";
highLight(_) -> [].

%% @doc Convert characters for html display
html_conversion([],comment) -> [];
html_conversion(S,comment) -> 
    IsComment=lists:any(fun(X)-> X==true end,[is_comment_char(C) || C <- S]),
    Text=lists:flatten([replace_chars(C) || C <- S]),
    if
        IsComment == true -> highLight(comment,Text);
        true -> Text
    end.

html_conversion([]) -> [];
html_conversion(S) -> lists:flatten([replace_chars(C) || C <- S]).

is_comment_char(C) when ((C/=32) and (C/=10) and (C/=9)) -> true;
is_comment_char(_) -> false.

replace_chars(C) ->
    case C of
        32 -> "&nbsp;";                   % ' '
        10 -> "<br />";                   % '\n'
        9  -> "<b>&nbsp;&nbsp;&nbsp;&nbsp;</b>"; % '\t'
        $< -> "&lt;";                     % '<' 
        $> -> "&gt;";                     % '>'
        $& -> "&amp;";                    % '&'
        34 -> "&quot;";                   % '"'
        _  -> C
    end.

%%% ============================================================================
%%% Helper functions

%% @doc Find topmost orig node in the syntactic tree
find_orig_node(Node) ->
    PrevNode=?Query:exec(Node,[{orig,back}]),
    if
        PrevNode==[] ->
            RealPrevNode=?Query:exec(Node,[{elex,back}]),
            if
                RealPrevNode/=[] -> RealPrevNode;
                true -> []
            end;
        true -> 
            find_orig_node(hd(PrevNode))
    end.

%% @doc Find last orig node in the syntactic tree (for macros)
deep_orig_search(Node) ->
    Result=?Query:exec(Node,[{orig,1}]),
    if
        Result==[] -> Node;
        true -> deep_orig_search(hd(Result))
    end.

id({_,_,Id}) -> Id.
