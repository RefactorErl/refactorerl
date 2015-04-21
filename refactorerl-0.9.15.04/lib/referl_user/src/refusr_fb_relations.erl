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

%%% @doc Functionblock relationship examiner.
%%%
%%% @author Kinga Szava <guca04@gmail.com>

-module(refusr_fb_relations).
-vsn("$Rev: 13083 $ ").

-export([get_relations/0, get_relations/1, get_relations/2, is_relation/1, check_cycle/0, check_cycle/1]).

-export([draw/0, draw_cycles/0, draw_cycles/1, draw/1]).

-export([build_table/5]).

-export([error_text/2]).



-include("user.hrl").
-include("refusr_lib_relations.hrl").

-record(row, {number,
	      element,
	      relations,
	      cycles = []
	     }).

error_text(no_fb,[])->
    "No functionblock list was given!";
error_text(bad_arg_type, [])->
    "Error, the form of the arguments was not proper. 
        It should be given in a list or a in a tuple! ";
error_text(fb_num, [])->
    "The number of the functionblocks was not good. It should be exactly two.";
error_text(written, Message = [C|_]) when is_integer(C)->
    Message;
error_text(_,_)->
    unknown.


%% =============================================================================
%% Making proper input list

sort([])->
    case refusr_dir_sort:sort() of
        {error, _} -> throw({error, "No modules exist in the database!~n", []});
        List -> List
    end.

get_node({_, _, _} = Node)->
	Node;
get_node(Name)->
    ?Query:exec(?Mod:find(Name)).


get_mod_funcall(Mod)->
    lists:flatten([Funcalls
                   || Fun <-[?Query:exec(Mod, ?Mod:locals())],
                      Funcalls <- [?Query:exec(Fun, ?Fun:funcalls())],
                      Funcalls /= []]).

get_func_mod(Fun)->
    ?Query:exec1(Fun, ?Fun:module(), mod_not_found).

%%% ============================================================================
%%% Building the dets table
%%
%% @spec build_table(Name, Args, Exceptions, Leaves, Data) -> ok
%% @doc Builds up the proper dets table from a built digraph.
build_table(Name, Args, Exceptions, Leaves, {undefined, _})->
	build_table(Name, Args,  Exceptions, Leaves, sort([]));
build_table(Name, Args,  Exceptions, Leaves, {[], _})->
	build_table(Name, Args,  Exceptions, Leaves, sort([]));
build_table(Name, Args,  Exceptions, Leaves, {FbList, _})->
	build_table(Name, Args,  Exceptions, Leaves, FbList);
build_table(Name, Args,  Exceptions, Leaves, FbList)->
    Graph = digraph:new(),
    try
	ArgList = [ Element || Arg<-Args,
				(Element = lists:keyfind(Arg, 1, FbList)) /= false],
        build_graph(Graph, {ArgList, FbList}, Exceptions, Leaves),
        Cycles = cycles(Graph, digraph:vertices(Graph), [], []),
	?LibRel:build_paths(Graph, Name, Cycles, fb_rel)
    after
        digraph:delete(Graph)
    end.

 

%%==============================================================================
%% Exported functions

%% @spec get_relations() -> [Relation_Pairs] | {error, bad_argument}
%% @doc Prints out every connection among every functionblock list.
%% Equivalent to get_relations([], true), see {@link get_relations/2}.
get_relations()->
     get_relations([], true).

%% -----------------------------------------------------------------------------
%% @spec get_relations(Arg) -> [Relation_Pairs] | {error, bad_argument}
%%	     Arg = Other::bool() | ParList
%%           ParList = [string()]
%%                   | [{Basename::string(), [FunctionBlock::atom()]}]
%%           Relation_Pairs = tuple(Path)
%%           Path = string()
%%	     Other = bool()
%% @doc If the argument is a boolean value, then it is equivalent to get_relations([], Other).
%% Otherwise, same as get_relations(ParList, true), see {@link get_relations/2}.
get_relations(Other) when not is_list(Other)->
	get_relations([], Other);
get_relations(ParList)->
	get_relations(ParList, true).

%% -----------------------------------------------------------------------------
%% @spec get_relations(ParList, Other) -> [Relation_Pairs] | {error, bad_argument}
%%           ParList = [string()]
%%                   | [{Basename::string(), [FunctionBlock::atom()]}]
%%           Relation_Pairs = tuple(Path)
%%           Path = string()
%%	     Other = bool()
%%
%% @doc Displays the relationship among elements of the given
%% functionblock list. There is a relationship between `fb1' block and
%% `fb2' block if a module of `fb1' is dependent of a module of
%% `fb2'. The result is a tuple-list whose elements represent the
%% relations.
%% The Other parameter stands for whether the category "Other" would be
%% taken into consideration or not. The default value is true.
get_relations({Path, [Hd | _] = List}, Other) when is_atom(Hd)->
	ParList = [filename:join(Path, Next)|| Next <- List], 
	get_relations(ParList, Other);
get_relations(ParList, Other)->
	?CatchInternal(
    begin
        Res = ?LibRel:invest_table(fb_rel,
    			 [{type, get_rel} | ParList]), 
        	io:format("~p~n", [get_rel(
    			  Res,
    			[], Other)])
    end).


%% -----------------------------------------------------------------------------
%% @spec is_relation(ParList)-> true | false | Error
%%           ParList = tuple(Fb)
%%	     Fb = string()
%%                   | {Basename::string(), [FunctionBlocks::atom()]}
%%           Error = {error, no_list_given}
%%                 | {error, bad_argument_list_length}
%%                 | {error, bad_argument_form}
%%
%% @doc Decides whether there is a connection between the two given
%% functionblocks.
is_relation([])->
    throw(?LocalErr0r(no_fb));
is_relation([{_, _} = Arg | []])->
    is_relation(tuple_to_list(Arg));
is_relation({_,_} = Arg)->
    is_relation(tuple_to_list(Arg));
is_relation(Arg) when not is_list(Arg) and not is_tuple(Arg)->
    throw(?LocalErr0r(bad_arg_type));
is_relation(Arg) when length(Arg) == 2 ->
	?CatchInternal(
    case get_rel(?LibRel:invest_table(fb_rel, [{type, get_rel},
			   {gnode, Arg}]), [], true) of
		[{error, Msg}] -> {error, Msg};
		GetRel ->
   			lists:member(list_to_tuple(Arg), GetRel) or
			lists:member(list_to_tuple(lists:reverse(Arg)), GetRel)
   	 end);
is_relation(_) -> 
    throw(?LocalErr0r(fb_num)).


%% -----------------------------------------------------------------------------
%% @spec check_cycle()-> {ok, no_cyclic_dependency}
%%                     | {integer(), list()}
%%                     | {error, bad_argument}
%%
%% @doc Looks for cycles among every function block list. Equivalent
%% to check_cycle([]). See {@link check_cycle/1}.
check_cycle()->
    check_cycle([]).

%% -----------------------------------------------------------------------------
%% @spec check_cycle(ParList) -> {ok, no_cyclic_dependency}
%%                             | {integer(), list()}
%%                             | {error, bad_argument}
%%           ParList = [string()]
%%                   | [{Basename::string(), [FunctionBlock::atom()]}]
%%
%% @doc Checks for cycles in the dependencies between the given
%% functionblock list.
check_cycle([{_, _} | _ ] = Options)-> %% call from ri 
   ?CatchInternal(?LibRel:invest_table(fb_rel, [{type, check} |Options]));
check_cycle(ParList)->
   ?CatchInternal(?LibRel:invest_table(fb_rel, [{type, check}, {gnode, ParList}])).


%% =============================================================================
%% Draw graph

%% -----------------------------------------------------------------------------
%% @spec draw()-> {ok, no_cyclic_dependency} | {integer(), list()}
%% @doc Prints out the entire graph. Output file is fb_relations.dot.
draw()->
    draw([{gnode, []}]).

%% -----------------------------------------------------------------------------
%% @spec draw(PropList) -> {ok, no_cyclic_dependency}
%%                      | {integer(), list()}
%%                      | {error, bad_function_name}
%%
%% @doc Creates a subgraph drawing from the given functionblock list.
%% The file's name: fb_relations.dot or user defined.
%% PropList:
%% ```
%% {dot, DotFileName::string()}
%%
%% '''
%% User defined dot name.
%% ```
%% {gnode, [string()] | [{Basename::string(), [FunctionBlock::atom()]}]}
%% '''
%% Which function blocks should be inspected.
draw(PropList)->
    ?CatchInternal(?LibRel:invest_table(fb_rel, [{type, draw} | PropList])).

%% -----------------------------------------------------------------------------
%% @spec draw_cycles() -> {ok, no_cyclic_dependency}
%%                      | {integer(), list()}
%%                      | {error, bad_function_name}
%%
%% @doc Prints out a subgraph which contains the cycle(s). Unless
%% cycles exist, calls {@link draw/0}. Output file is
%% fb_relations.dot.
draw_cycles()->
    draw_cycles([{gnode, []}]).

%% -----------------------------------------------------------------------------
%% @spec draw_cycles(PropList) -> {ok, no_cyclic_dependency}
%%                      | {integer(), list()}
%%                      | {error, bad_function_name}
%%
%% @doc From the given function block list creates a graph from the cycle list.
%% If there are no cycles, draws out the entire graph.
%% The file's name: fb_relations.dot or user defined.
%% PropList:
%% ```
%% {dot, DotFileName::string()}
%%
%% '''
%% User defined dot name.
%% ```
%% {gnode, [string()] | [{Basename::string(), [FunctionBlock::atom()]}]}
%% '''
%% Which function blocks should be inspected.
draw_cycles(PropList)->
     ?CatchInternal(?LibRel:invest_table(fb_rel, [{type, cycles} | PropList])).


%%======================================================================

get_rel([], Result, _)->
    Result;
get_rel([{Dir, Calls} | Rest], Result, Other)->
    get_rel(Rest, divide_rel(Dir, Calls, Result, Other), Other);
get_rel([#row{number=_, element=Dir, relations = Calls, cycles=_} | Rest], Result, Other)->
    get_rel(Rest, divide_rel(Dir, Calls, Result, Other), Other);
get_rel([[#row{number=_, element=Dir, relations = Calls, cycles=_} | _] | Rest], Result, Other)->
    get_rel(Rest, divide_rel(Dir, Calls, Result, Other), Other).

divide_rel(_, [], Result, _)->
    Result;
divide_rel("Other", _, Result, false)->
    Result;
divide_rel(Dir, ["Other"| Rest], Result, false)->
    divide_rel(Dir, Rest, Result, false);
divide_rel(Dir, [Dir | Rest], Result, Other) ->
    divide_rel(Dir, Rest, Result, Other);
divide_rel(Dir, [Hd | Rest], Result, Other) ->
    Member = 
	lists:member({Dir, Hd}, Result) or
	lists:member({Hd, Dir}, Result),
    if
	Member == false->
	    divide_rel(Dir, Rest,  [{Dir, Hd} | Result], Other);
	true ->
	    divide_rel(Dir, Rest, Result, Other)
    end.

%%=============================================================
%% Graph functions
build_graph(Graph, {[], Fbs}, Exceptions, Leaves)->
    [build_vertex(Graph, Element, lists:member(Path, Leaves)) || {Path, _} = Element <- Fbs,
	lists:member(Path, Exceptions) == false];
build_graph(Graph, {Args, Fbs}, Exceptions, Leaves)->
    build_node(Graph, Args, Fbs, Exceptions, Leaves),
	[digraph:del_vertex(Graph, V)
	   || V<-digraph:vertices(Graph), 
		element(2, V) == module].

build_node(_, [], _, _, _)->
	ok;
build_node(Graph, [{Path, _} = Next | Rest], Fbs, Exceptions, Leaves)->
	case lists:member(Path, Exceptions) of
		false ->
		    build_vertex(Graph, Next, lists:member(Path, Leaves)),
		    build_node(Graph, Rest, Fbs, Exceptions, Leaves);
		true ->
		    build_node(Graph, Rest, Fbs, Exceptions, Leaves)
	end.

build_vertex(Graph, {Path, _}, true)->	
    check_vertex(Graph, Path);
build_vertex(Graph, {Path, ModList}, false)->	
    check_vertex(Graph, Path),
    [begin
    	 check_vertex(Graph, CalledPath),
	 check_edge(Graph, Path, CalledPath)
     end || Mod <- ModList,
	    CalledPath <- get_called_dir(get_node(Mod))].

check_vertex(Graph, Vertex)->
    case  digraph:vertex(Graph, Vertex) of
        false -> digraph:add_vertex(Graph, Vertex);
        _ -> ok
    end.

check_edge(Graph, Vertex1, Vertex2)->
    case edge(Graph, Vertex1, Vertex2) of
	[] -> digraph:add_edge(Graph, Vertex1, Vertex2);
	_ -> ok
    end.

edge(Graph, Vertex1, Vertex2)->
    [digraph:edge(Graph, Edge)
     || Edge <- ?MISC:intersect(digraph:out_edges(Graph, Vertex1),
				digraph:in_edges(Graph, Vertex2))].


get_called_dir(Mod)->
    R =[Path || Called <- get_mod_funcall(Mod),
             (CalledMod = get_func_mod(Called)) /= Mod,
             {Path, _} <- refusr_dir_sort:get_dirs([CalledMod])],
    R.

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
