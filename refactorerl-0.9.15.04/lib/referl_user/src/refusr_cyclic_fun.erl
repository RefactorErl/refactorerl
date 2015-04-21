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

%%% @doc Cyclic depedency examination in function level.
%%% Draws out the dependency graph.
%%% At first run a graph is used, which is stored into a dets table for further
%%% and faster use.
%%% <br/> X function is
%%% dependent from Y function if there is a Y function call in the
%%% definition of X. (X -> Y)<br/> For example, X -> Y -> Z -> X is a
%%% cycle.
%%% @author Kinga Szava <guca04@gmail.com>

-module(refusr_cyclic_fun).
-vsn("$Rev: 13083 $ ").
-export([print_cycle/0, print_cycle/1, check_cycle/0, check_cycle/1, check_function/1, get_routes/3]).

-export([get_relations/1]).

-export([get_deps/0, get_deps/1]).

-export([draw/0, draw/1, draw_cycles/0, draw_cycles/1]).

-export([build_table/5]).

-export([funcalls/1]).

-export([funcalls_for_lib/1]).

-export([error_text/2]).

-include("user.hrl").
-include("refusr_lib_relations.hrl").




error_text(invalid_type, [])->
    "Invalid type command given, as a default, routes listed above.";
error_text(bad_gnode, [])->
    "A bad argument was given, a function type gnode should be the argument!";
error_text(written, Message = [C|_]) when is_integer(C)->
    Message;
error_text(_, _)->
    unknown.


%%% ============================================================================
%%% Building the dets table
%% @spec build_table(Name, Args, Exceptions, Leaves, Data) -> ok
%% @doc Builds up the proper dets table from a built digraph.
%% Only for development use. (Data argument is only for the uniformity.)
build_table(Name, [], Exceptions, Leaves, _)->
    Graph = digraph:new(),
    try
        build_graph(Graph, Exceptions, Leaves),
        Cycles = cycles(Graph, digraph:vertices(Graph), [], []),
	?LibRel:build_paths(Graph, Name, Cycles, func)
    after
        digraph:delete(Graph)
    end;

%build_table(Name, Args = [C|_], Exceptions, Leaves,  Data) when is_integer(C)->
%build_table(Name, [Args], Exceptions, Leaves,  Data);

build_table(Name, Args, Exceptions, Leaves,  _) ->
    Graph = digraph:new(),
    try
        lists:foldl(fun(Arg, _)->
            % There is no need to update the Exceptions during the execution
			build_subgraph(Graph, Arg, Exceptions, Leaves) end,
			Exceptions, Args),
        Cycles = cycles(Graph, digraph:vertices(Graph), [], []),
	?LibRel:build_paths(Graph, Name, Cycles, func)
    after
        digraph:delete(Graph)
    end.

%%===========================================================================
%% @spec check_cycle() -> {ok, no_cyclic_dependency} | {string(), list()}
%% @doc Builds a directed graph, checks for cycles in function level. If
%% cycles are found then the following tuple is the result: {Number of
%% cycles, Cycle list}.
check_cycle()->
	check_cycle([]).

check_cycle(ParList)->
  {_, Res} = check_function([{type, check} | ParList]),
  {integer_to_list(length(Res)) ++  " cycle(s)", Res}.

%% @spec check_function(Function)-> Result | {error, bad_function_name}
%% | erlang:error(error, Args)
%% Function = node() | string()
%% Result = {Bool | list()}
%% Bool = true | false
%% @doc Creates a subgraph from the given function as a starting node
%% and checks for cycles. The result is a tuple, where the first element
%% is true/false depending on whether there's cycle ot not from the given node.
%% The second is the a gnode list in which the nodes in the cycle are indicated.
%% <br/>The function can be given as a graph node or  in the following
%% order and combination:<br/> "module:name/arity" (eg.: "erlang:hd/1").
check_function([{_, _} | _ ] = Options)-> %% call from ri
   output(?CatchInternal(?LibRel:invest_table(func, [{type, check} | Options])));
check_function(Function)->
   output(?CatchInternal(?LibRel:invest_table(func, [{type, check}, {gnode, Function}]))).

output([])->
	{false, []};
output(Res)->
	{true, Res}.

%% @spec print_cycle() -> ok
%% @doc Prints the result of check_cycle/0 to the standard output.
print_cycle()->
   print_cycle([]).

print_cycle([{_, _} | _ ] = Options)->
    ?CatchInternal(?LibRel:invest_table(func, [{type, print} | Options]));

print_cycle(FromNode)->
   ?CatchInternal(?LibRel:invest_table(func, [{type, print}, {gnode, FromNode}])).


%% @spec get_routes(Start, End, Type) -> any()
%% Start = gnode() | Name
%% Name = string() | atom()
%% End = Start
%% Type = list | draw | {list, nodes} | {list, names} | {draw, File}
%% File = string()
%% @doc Gives back a route of function calls between two given functions
%% (the first element of the list will be the starting function, the last
%% the end function) The route is depicted with a list of functions (or their nodes).
%% Depending on the Type parameter the result can be gained as a list of nodes or names, or
%% a .dot file is generated for representation.
%% Unless the File parameter is correct and given as a full path, in the case of drawing,
%% the generated .dot file will be in the dep_files directory.
get_routes(Start, End, Type)->
    Graph = digraph:new(),
    try
	[Node1 | _] = refusr_lib_relations:verify_node(Start, func),
	[Node2 | _] = refusr_lib_relations:verify_node(End, func),
	build_subgraph(Graph, [Node1], {[],[]}, []),
	Result = check_route_vertex(Graph, Node1, Node2,
				    digraph:out_neighbours(Graph, Node1),
				    [Node1], []),
	show_result(Graph, Result, Type)
    catch
	{error, InfoString, InfoPar} ->
	    io:format(InfoString, InfoPar)
    after
	digraph:delete(Graph)
    end.

show_result(_Graph, Routes, list)->
	io:format("Routes found: ~p~n", [
					 [refusr_lib_relations:print_names(Res, func)
					  || Res <- Routes]]);
show_result(Graph, Routes, draw)->
  %%   Graph = digraph:new(),
%%     try
	Sub = digraph_utils:subgraph(Graph, lists:flatten(Routes)),
	write_file(Sub, "./proba.dot");
%%     after
%% 	digraph:delete(Graph)
%%     end;
show_result(Graph, Routes, _)->
    show_result(Graph, Routes, list),
    throw(?LocalErr0r(invalid_type)).

check_route_vertex(_Graph, _Start, _End, [], _Path, Routes) ->
    Routes;
check_route_vertex(Graph, Start, End, [Start | Tail], Path, Routes) -> %%Cycle from the Start node
    check_route_vertex(Graph, Start, End, Tail, Path, Routes);
check_route_vertex(Graph, Start, End, [End | Tail], Path, Routes) ->
    Route = lists:reverse([End | Path]),
    check_route_vertex(Graph, Start, End, Tail, Path,
		       [Route| Routes]);
check_route_vertex(Graph, Start, End, [Hd | Tail], Path, Routes) ->
    case lists:member(Hd, Path) of
	true -> %%Cycle from another node in the path
	    check_route_vertex(Graph, Start, End, Tail, Path, Routes);
	false ->
	    NewRoutes =
		check_route_vertex(Graph, Start, End,
				   digraph:out_neighbours(Graph, Hd),
				   [Hd | Path], Routes),
	    check_route_vertex(Graph, Start, End, Tail, Path, NewRoutes)
    end.


%% @spec get_relations(Functions)-> Relations
%% Functions = node() | [node() | string()]
%% Relations = {Module, Paths::lists(), Cycles::lists()}
%% @doc Prints out the relations and the cycles connected to a given function or functionlist.
%% The parameter functionlist has to be given in a list if the functions are marked by their name
%% as string (["module:function/arity" | ...]).
get_relations({_, func, _} = Node)->
	get_relations([Node]);
get_relations(ParList) when is_list(ParList)->
    Res =
    ?CatchInternal(
        ?LibRel:invest_table(func,
			 [{type, get_rel}, {gnode, ParList}])),
	?LibRel:print_rels(Res, func).
%%--------------------------------------------------------------------------------

%% @spec draw()-> {ok, no_cyclic_dependency} | {integer(), list()}
%% @doc Equivalent to draw(""), prints out the entire graph. Output file is dep_func.dot.
draw()->
  draw([{gnode, []}]).


%% @spec draw(Options::proplists())-> {ok, no_cyclic_dependency} | {integer(), list()} |
%% {error, bad_function_name} | erlang:error(error, Args)
%% @doc Creates a subgraph drawing from the directed graph from a given function as a starting node.
%% The function can be given as a graph node or  in the following
%% order and combination: "module:name/arity" (eg.: "erlang:hd/1").<br/>
%% Options:
%% ```
%% {dot, DotFileName::string()}
%%
%% '''
%%
%% ```
%% {gnode, Node::node() | string()}
%% '''
%% The file's name: dep_func_<i>module_function_arity</i>.dot or it can be given by
%% the user with a file name, then the .dot file will be in the the ./dep_files/ directory,
%% or with absolute path.
%% <br/> The indicators are the following:<br/>
%% - Red edge: cycle edge<br/>
%% - Dashed edge: funcall edge, a function calls, depends on another<br/>
%% - Purple box: module<br/>
%% - Black hexagon: function
draw([{_, _} | _] = Options)->
	?CatchInternal(?LibRel:invest_table(func,[{type, draw} | Options]));
draw(Options) when not is_list(Options) ->
	draw([{gnode, Options}]);
draw(Options)->
    ?CatchInternal(
    case proplists:is_defined(gnode, Options) of
	true ->
           ?LibRel:invest_table(func,[{type, draw} | Options]);
	false ->
		case proplists:is_defined(dot, Options) of
			true ->
			    ?LibRel:invest_table(func,[{type, draw} | Options]);
			false ->
				io:format("Neither dot nor gnode key given properly.~n"),
				draw([{gnode, Options}])
		end
    end).

%% @spec draw_cycles()-> {ok, no_cyclic_dependency} | {integer(), list()}
%% @doc Prints out a subgraph which contains the cycle(s).
%% Unless cycles exist, calls {@link draw/0}.
%% Output file is dep_func_cycles.dot.
draw_cycles()->
    draw_cycles([{gnode, []}]).

%% @spec draw_cycles(PropList::proplists())-> {ok, no_cyclic_dependency} | {integer(), list()} |
%% {error, bad_function_name} | erlang:error(error, Args)
%% @doc Prints out a subgraph which contains the cycle(s) from the given node.
%% Unless cycles exist, calls {@link draw/0}.
%% Output file is dep_func_cycles.dot or user_defined. <br/>
%% Options:
%% ```
%% {dot, DotFileName::string()}
%%
%% '''
%%
%% ```
%% {gnode, Nodes}
%% Nodes = Node | NodeList::lists()
%% Node = string() | gnode()
%% '''
draw_cycles(Options)->
     ?CatchInternal(?LibRel:invest_table(func,[{type, cycles} | Options])).




get_deps()->
    Functions = get_all_functions(),
    get_deps(Functions).

get_deps(Function = {_, func, _}) ->
    get_deps([Function]);

get_deps(Function) when is_list(Function) andalso is_integer(hd(Function))->
    get_deps([Function]);

get_deps(Nodes) when is_list(Nodes)->
    NeededNodes = [convert_to_gnode(Node) || Node <- Nodes],
    [{NodeName, Deps} || {NodeName , Deps, _} <-get_relations(NeededNodes)];

get_deps(Node) when is_atom(Node) ->
    get_deps([Node]).


%%% ============================================================================
%%% Graph queries
get_all_functions()->
    ?Query:exec(?Query:seq(?Mod:all(), ?Mod:locals())).

get_funcall(Function) when not is_list(Function)->
	get_funcall([Function]);
get_funcall(Functions)->
    [{Fun, Callees} || Fun <- Functions ,
                       Callees <- funcalls(Fun),
                       Callees /= []].

get_funcalls(Functions)->
    [{Fun, Callees} || Fun <- Functions ,
                       Callees <- funcalls(Fun)].

get_func_mod(Fun)->
    ?Query:exec1(Fun, ?Fun:module(), mod_not_found).

funcalls_for_lib(Fun) when not is_list(Fun)->
    funcalls_for_lib([Fun]);
funcalls_for_lib(Funs) ->
    lists:usort(lists:flatten([funcalls(Fun) || Fun <- Funs])).

funcalls({_, Type, _}) when Type /= func->
	throw(?LocalErr0r(bad_gnode));

funcalls(Fun)->
	lists:usort([diff_edge_calls(Fun, [funcall, dyncall, ambcall, may_be], [])]).

diff_edge_calls(_, [], Result)->
	Result;
diff_edge_calls(Node, [Hd | Rest], Result)->
	diff_edge_calls(Node, Rest,
		 Result ++ ?Graph:path(Node, [Hd])).


convert_to_gnode(Node = {_,func,_}) ->
    atom_to_list(hd(?CatchInternal(?LibRel:print_names([Node], func))));

convert_to_gnode(Node) when is_list(Node) andalso is_integer(hd(Node)) ->
    Node;

convert_to_gnode(Node) when is_atom(Node) ->
    Node;

convert_to_gnode(_)->
    throw(?LocalErr0r(bad_gnode)).

%%% ============================================================================
%%% Building a digraph
% Note: do not update the exceptions!
build_graph(Graph, Exceptions, Leaves)->
    AllFuns = get_funcalls(get_all_functions()),
    build_vertex(Graph, AllFuns, Exceptions, Leaves).

build_vertex(_, [], Exceptions, _)-> Exceptions;
build_vertex(Graph, [{Fun, Callees} | Rest], {Exceptions, Otp}, Leaves) ->
	%%io:format("Mod: ~p, ModCalls: ~p, Tail:~p,  Exceptions: ~p, Leaves: ~p~n", [Fun, Callees, Rest, Exceptions, Leaves]),
	Bool = lists:member(Fun, Exceptions) orelse is_otp_member(Fun, Otp),
	case Bool of
        true ->
            ok;
        false ->
            check_subgraph(Graph, Fun),
            case lists:member(Fun, Leaves) of
                true ->
                    ok;
                false ->
                    [begin
                         check_subgraph(Graph, Callee),
                         check_edge(Graph, Fun, Callee, "called")
                     end || Callee <- Callees,
                            not lists:member(Callee, Exceptions),
                            not is_otp_member(Callee, Otp)],
                    ok
            end
    end,
    build_vertex(Graph, Rest, {Exceptions, Otp}, Leaves).

is_otp_member(Func, Otp)->
	Name =  ?Mod:name(?LibRel:get_func_mod(Func)),
	lists:member(Name, Otp).

check_subgraph(Graph, Fun) ->
    Mod = get_func_mod(Fun),
    check_vertex(Graph, Mod),
    check_vertex(Graph, Fun),
    check_edge(Graph, Mod, Fun, "").

check_vertex(Graph, Vertex)->
    case digraph:vertex(Graph, Vertex) of
        false -> digraph:add_vertex(Graph, Vertex);
        _ -> ok
    end.

check_edge(Graph, V1, V2, Label)->
    case edge(Graph, V1, V2) of
	[] -> digraph:add_edge(Graph, V1, V2, Label);
	_ -> ok
    end.

edge(G, V1, V2)->
    [digraph:edge(G, E)
     || E <- ?MISC:intersect(digraph:out_edges(G, V1),
                             digraph:in_edges(G, V2))].



cycles(Graph, [], Cycles, Edges)->
    [digraph:add_edge(Graph, E, V1, V2, Label) ||
	{E, V1, V2, Label} <- Edges],
    Cycles;
cycles(Graph, [Head|Tail] = Vertices, Cycles, Edges)->
    case digraph:get_cycle(Graph, Head) of
	false -> cycles(Graph, Tail, Cycles, Edges);
	[_  = Loop| []]  ->
	    cycles(Graph, Tail, [ [Loop, Loop] | Cycles], Edges);
	Cycle ->
	    [{E, _, _, _} = Edge | []] =
		edge(Graph, lists:nth(length(Cycle)-1, Cycle), Head),
	    digraph:del_edge(Graph, E),
	    cycles(Graph, Vertices, is_cycle(Cycles, Cycle,
					     lists:member(Cycle, Cycles)),
		   [ Edge| Edges])
    end.

is_cycle(Cycles, _, true)->
    Cycles;
is_cycle(Cycles, Cycle, false) ->
    [Cycle | Cycles].

build_subgraph(Graph, Node, Exceptions={Exs, _}, Leaves)->
    case get_funcall(Node) of
	[] -> check_exceptions(Graph, Node, Exceptions),
		Exceptions;
	[{Fun, CalledList}]->
	    case lists:subtract(CalledList, digraph:vertices(Graph)) of
		[] ->  build_vertex(Graph, [{Fun, CalledList}], Exceptions, Leaves);
		NewVertices -> build_vertex(Graph, [{Fun, CalledList}], Exceptions, Leaves),
				update_exceptions_subgraph(Graph, NewVertices -- Exs, Exceptions, Leaves)
	    end
    end.

check_exceptions(Graph, {_, _, _} = Node, {Exceptions, Otp})->
	Bool = lists:member(Node, Exceptions) or is_otp_member(Node, Otp),
	case Bool of
		true->
			ok;
		false->
			check_subgraph(Graph, Node)
	end;
check_exceptions(_Graph, [], _Exceptions)->
	ok;
check_exceptions(Graph, NodeList, Exceptions) when is_list(NodeList)->
	[check_exceptions(Graph, Node, Exceptions) || Node <- NodeList].

update_exceptions_subgraph(_, [], Exceptions, _)->
    Exceptions;
update_exceptions_subgraph(Graph, [Node | Tail], Exceptions, Leaves)->
	NewExceptions = build_subgraph(Graph, [Node], Exceptions, Leaves),
	update_exceptions_subgraph(Graph, Tail, NewExceptions, Leaves).


%%----------------------------------------------------------------------


-define(Dot_Header, "digraph dependency_graph_function_level {\n").
-define(Dot_Root, "Nroot0 [shape=\"triangle\", label=\"ROOT\", fontsize=\"18\","
        "URL=\"#ok\", tooltip=\"node={'$gn', root, 0}\"]\n").
-define(Dot_Footer, "}\n").
-define(Dot_Node, "N~s~b [shape=\"~s\", label=\"~s\", fontsize=\"~b\","
        "color=\"~s\"  URL=\"#ok\", tooltip=\"node={'$gn', ~s, ~b}\"]\n").
-define(Dot_Edge, "N~s~b -> N~s~b [color=\"~s\", style =\"~s\" ]\n").
-define(Dot_CEdge, "Nfunc~b -> Nfunc~b [color=\"red\", style=\"~s\" ]\n").

write_file(Graph, File) ->
    case file:open(File, [write]) of
        {ok, Dev}->
            try
                io:put_chars(Dev, ?Dot_Header),
                io:put_chars(Dev, ?Dot_Root),

                [draw_vertex(Vs, Dev)
                 || Component <- digraph_utils:components(Graph),
                    Vs <- Component],
                %% case Result of
                %%    {_, no_cyclic_dependency} -> ok;
                %%    {_, List} ->
                %%        [begin
				%%			if
				%%				length(Edge) == 1 -> draw_loop_edge(Graph, hd(Edge), Dev);
				%%				true -> draw_cyclic_edge(Graph, Edge, Dev)
				%%			end
				%%		end || Edge <- List]
                %% end,
                [draw_edge(Edge, Dev) || Edge <- list_edges(Graph)],

                io:put_chars(Dev, ?Dot_Footer)
            after
                file:close(Dev)
            end;
        {error, Reason}->
            io:format("dep_func.dot: ~s~n", file:format_error(Reason))
    end.

draw_vertex(V, Dev)->
    {_, T, M} = V,
    {Shape, Size, Color} =
        case T of
            module -> {box, 18, purple};
            func   -> {hexagon, 14, black}
        end,

    io:format(Dev, ?Dot_Node, [T, M, Shape, label(V), Size, Color, T, M]),
    case T of
	module -> io:format(Dev, "Nroot0->N~s~b [color= \"black\"]\n", [T, M]);
        _ -> ""
    end.

draw_edge(Edge, Dev)->
    {_, {_, T1, V1}, {_, T2, V2}, Label} = Edge,
    {Color, Style} =
        case Label of
            "called" -> {black, dashed};
            _        -> {black, solid}
        end,
     io:format(Dev, ?Dot_Edge, [T1, V1, T2, V2, Color, Style]).


%% draw_cyclic_edge(_, [_ | []], _) -> ok;
%% draw_cyclic_edge(Graph, Edges, Dev)->
%%   E1 = hd(Edges),
%%   E2 = hd(tl(Edges)),
%%   case edge(Graph, E1, E2) of
%%    	[] ->  draw_cyclic_edge(Graph, tl(Edges), Dev);
%%   	[H | _] ->
%%   	    Style = case element(4, H) of
%% 			"called" ->  "dashed";
%%   			_  ->  "solid"
%% 		    end,
%% 	    io:format(Dev, ?Dot_CEdge, [element(3, E1), element(3, E2), Style]),
%% 	    digraph:del_edge(Graph, element(1, hd(edge(Graph, E1, E2)))),
%% 	    draw_cyclic_edge(Graph, tl(Edges), Dev)
%%     end.

%% draw_loop_edge(Graph, Node, Dev)->
%% 	case edge(Graph, Node, Node) of
%%        		 	[] ->  ok;
%%         		[H | _] ->
%%         		    Style = case element(4, H) of
%% 					"called" ->  "dashed";
%% 					_  ->  "solid"
%% 				    end,
%% 			    io:format(Dev, ?Dot_CEdge, [element(3, Node), element(3, Node), Style]),
%% 			    digraph:del_edge(Graph, element(1, hd(edge(Graph, Node, Node))))
%% 	end.


label(Vertex)->
    case ?Graph:class(Vertex) of
	module -> ?Mod:name(Vertex);
	func   -> atom_to_list(?Fun:name(Vertex)) ++ "/" ++
                      integer_to_list(?Fun:arity(Vertex))
    end.

list_edges(Graph)->
    [digraph:edge(Graph, E) || E <- digraph:edges(Graph)].








