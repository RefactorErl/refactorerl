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

%%% ============================================================================
%%% Module information

%%% @doc

%%% @author Zsofia Arvay <zsofia.arvay@gmail.com>

-module(refusr_layer).
-vsn("$Rev: 12971 $").

-export([check_layered_architecture/2, show_insults/2]).

-export([draw/2, draw/3]).

-export([error_text/2]).

-include("user.hrl").

%%------------------------------------------------------------------------------
%% error handling
%%------------------------------------------------------------------------------
error_text(badarg, []) ->
    "A bad argument was given.";
error_text(not_valid_moddef, Def)->
    io_lib:format("Invalid module or layer definition: ~p.",[Def]);
error_text(_,_) ->
    unknown.


%% @doc Inputs of the function are two lists. The elements of the first list 
%% define architecture layers as group of modules. Every group of modules must 
%% have a name. At default functions in a group can call functions from the 
%% same group or from the group directly under own group. But the 
%% second list defines additional permits, allowing function 
%% calls between two architecture layers given by it's names. After this an 
%% input can look like this: [{l1,[mod1,mod2]},{l2,[mod3,mod4,mod5]},{l3,[mod6]}], 
%% [{l1,l2}]. The function checks wether in an architecture defined by the 
%% input are functions, that insult the hierarchy or not. The output is 
%% an empty list if function calls are allowed in the defined architecture 
%% or list of disabled function calls in the following format: 
%% [{{caller fun, caller fun's module},{called fun, called fun's module}}, ..].
%% In the output functions and modules are given by nodes, but in the input 
%% we can define modules by it names or with regular expressions, too.
%% We can list regular expressions or put in a file (in this case every single  
%% regexp has to be in a newline in the file). 
check_layered_architecture(Mods=[{_,[_|_]}|_],ArchList) 
    when is_list(ArchList) ->
	ModList = 
            [ {Cimke, lists:usort(create_modlist(ModDef))} 
            || {Cimke,ModDef}<-Mods ],
    FunList = 
        [{Cimke,?Query:exec(Mod, ?Mod:locals())} 
        || {Cimke,Mod}<-ModList],
    FunCalls = 
        [{Cimke,[?Query:exec(Fun, ?Fun:funcalls()) || Fun<-FunListE]} 
        || {Cimke,FunListE}<-FunList],
    FunCallPair = 
        [{Cimke,lists:zip(FunListE,FunCallsE)} 
        || {{Cimke,FunListE},{_,FunCallsE}}<-lists:zip(FunList,FunCalls)],
    FunsList = lists:flatten([FunListE || {_,FunListE}<-FunList]),
    FunListSub = lists:sublist(FunList, length(FunList)-1),
    [DisabledFunListFirst|DisabledFunListTail] = 
        [{Cimke,FunsList--Element} || {Cimke,Element}<-FunList],
    DisabledFunctionsListTmp = 
        [DisabledFunListFirst | 
        [{Cimke,Element2--Element1} 
        || {{_,Element1},{Cimke,Element2}}
            <-lists:zip(FunListSub,DisabledFunListTail)]],

    DisabledFunctionsList = 
        process_archlist(FunList,DisabledFunctionsListTmp,ArchList),

    Result = 
        lists:flatten([check_disabled_functions(Element1,Element2) 
        || {{_,Element1},{_,Element2}}
            <-lists:zip(FunCallPair,DisabledFunctionsList)]),
    {ModList,Result};
check_layered_architecture(_,_) ->
    throw(?LocalErr0r(badarg)).

%%Returns with module nodes of the defined interface layer.
create_modlist([]) -> [];
create_modlist([First|Tail]) ->
    Part = 
    if
        is_atom(First) ->
            ?Query:exec(?Mod:find(First));
        is_tuple(First) ->
            case ?GR_UTILS:check_node(First,module,[]) of
                true -> [First];
                _ -> []
            end;
        is_list(First) ->
            case make_ui_request({refusr_fb_regexp_re,[{type, list},{regexp, [First]}]}) of
                {error, _} -> throw(?LocalError(not_valid_moddef, [First|Tail]));
                [{First,Mods}] -> Mods
            end;
        true ->
            throw(?LocalError(not_valid_moddef, [First|Tail]))
    end,
    Part++create_modlist(Tail);
create_modlist(_) ->
    throw(?LocalErr0r(badarg)).

%% Iterates through the elements of the Archlist. Archlist contains tuples, 
%% that contain two names of module groups and means that function calls from the
%% first module group given by the fisrt element of the tuple allowed to the 
%% second module group given by the second element. If does not exists any 
%% module group under the name ignores it, but a warning will be written to the 
%% output. In each step the function deletes from the list of the disabled
%% functions the allowed functions defined by the tuple.
process_archlist(_,DisabledFunctionsList,[])->
	DisabledFunctionsList;
process_archlist(FunList,DisabledFunctionsList,[{From,To}|Tail])->
	Ret1 = lists:keyfind(To,1,FunList),
	if
		is_tuple(Ret1) -> {_,FunsList} = Ret1,
		Ret2 = lists:keyfind(From,1,DisabledFunctionsList),
		if
			is_tuple(Ret2) -> {Key,DisabledFunsList} = Ret2,
			NewDisabledFunctionsList = 
				lists:keyreplace(Key,1,DisabledFunctionsList,
								{Key,DisabledFunsList--FunsList}),
			process_archlist(FunList,NewDisabledFunctionsList,Tail);
			true -> io:format("Warning: Label named ~p not exsist.\n", [From]),
				process_archlist(FunList,DisabledFunctionsList,Tail)
		end;
		true -> io:format("Warning: Label named ~p not exsist.\n", [To]),
			process_archlist(FunList,DisabledFunctionsList,Tail)			
	end;
process_archlist(_,_,[_|_]) ->
    throw(?LocalErr0r(badarg)).

show({{Afirst,Asecond},{Bfirst,Bsecond}})->
	io_lib:format("{~p:~p/~p,~p:~p/~p}",
		[?Mod:name(Afirst),?Fun:name(Asecond),
		?Fun:arity(Asecond),?Mod:name(Bfirst),
		?Fun:name(Bsecond),?Fun:arity(Bsecond)]).

%% @doc The input is the same as the input of the check_layered_architecture 
%% function. The function takes the result of that function and writes it to 
%% the output in a more readable format: 
%% {error|[{caller fun's mod name:caller fun's name/caller fun's arity,
%% called fun's module name:called fun's name/called fun's arity}, ..]}.
show_insults(Mods,ArchList)->
	{ModGroupList,Result} = check_layered_architecture(Mods,ArchList),
    ModNameList = [ io_lib:format("~n~p:",[Cimke]) ++ [ io_lib:format(" ~p,",[?Mod:name(Mod)]) || Mod<-ModList ] || {Cimke, ModList}<-ModGroupList ],
	InsultCalls = if
		Result == [] -> [];
		true -> "{error|["++[io_lib:format("~n",[])++show(Element) || 
							Element<-Result]++"]}"
	end,
    {ModNameList,InsultCalls}.

make_pair(_,[])->[];
make_pair(Func,[Head|Tail])->
	[{{lists:nth(1,?Query:exec(Func,?Fun:module())),Func},
	{lists:nth(1,?Query:exec(Head,?Fun:module())),Head}}]
	++make_pair(Func,Tail).

%% Checks wether a function is not in the list of disabled functions.
%% Returns with the list of the disabled function pairs like this:
%% [{caller, caller's module}, {called fun, called fun's module}, ..].	
check_disabled_functions([],_) ->[];
check_disabled_functions(_,[]) ->[];
check_disabled_functions([{_,[]}|TailA],B) ->
	check_disabled_functions(TailA,B);
check_disabled_functions([{Func,Lista}|TailA],B) ->
	case lists_intersection(lists:sort(Lista),lists:sort(B)) of
		[] -> check_disabled_functions(TailA,B);
		Other -> [make_pair(Func,Other)|check_disabled_functions(TailA,B)]
	end.

%% The input parameters are two lists, and the output is the 
%% intersection of the two lists.
lists_intersection([],_)->[];
lists_intersection(_,[])->[];
lists_intersection([HeadA|TailA],[HeadB|TailB])->
	if
		HeadA<HeadB -> lists_intersection(TailA,[HeadB|TailB]);
		HeadA>HeadB -> lists_intersection([HeadA|TailA],TailB);
		HeadA==HeadB -> [HeadA|lists_intersection(TailA,TailB)]
	end.

%% @spec draw(LayerList::lists(), ArchList::lists()) -> any()
%% LayerList = [{LayerName::atom(), [Paths::string()]}]
%% @doc Visualisation of layers, representing group of layers and insulting functions.
%% Check {@link check_layered_architecture/2} for detailed parameters. 
%% The result is a .dot file, called layered_arch.dot.
%% Same as calling ```draw(LayerList, ArchList, "layered_arch.dot").'''
%% @end
draw(LayerList, ArchList)->
	draw(LayerList, ArchList, "layered_arch.dot").

%% @spec draw(LayerList::lists(), ArchList::lists(), DotName::string()) -> any()
%% @doc Same as {@link draw/2}, only the user can give the name of the
%%  generated .dot file.
%% @end
draw(LayerList, ArchList, DotName)->
	write_file(DotName, 
		check_layered_architecture(LayerList, ArchList)).

-define(Dot_Header, "digraph layered_architecture_representation {\n").
-define(Dot_SubHeader, "subgraph cluster_~p {\nlabel=\"~s\";~n").
-define(Dot_Footer, "}\n").
-define(Dot_Node, "~p~p [shape=\"~p\", label=~p, fontsize=\"~p\","
        "color=\"~p\"  URL=\"#ok\", tooltip=\"node={'$gn', ~p, ~p}\"];\n").
-define(Dot_Edge, "~p~b -> ~s~b [color=\"black\",  style=\"solid\"];\n").
-define(Dot_InsultEdge, "~p~b -> ~s~b [color=\"red\",  style=\"dashed\"];\n").

write_file(File, {LayerMods, InsultList})->
    io:format("Creating ~p file...~n", [File]),
    case file:open(File, [write]) of
	{ok, Dev}->
	    try
		io:put_chars(Dev, ?Dot_Header),

		draw_insults(Dev, InsultList, LayerMods),
        draw_modules(Dev, LayerMods),

	    	io:put_chars(Dev, ?Dot_Footer)
	    catch
		{mod_not_found, Fun} ->
		 io:format("Module not found for function: ~p~n", [Fun])
	    after
	    	file:close(Dev)
	    end;

        {error, Reason}->
            io:format("~s~n", file:format_error(Reason))
    end.

draw_insults(Dev, InsultList, LayerList)->
	[begin
		draw_parent(Dev, Mod1, Func1, LayerList),
		draw_parent(Dev, Mod2, Func2, LayerList),
		draw_edge(Dev, Func1, Func2, true)
	end || {{Mod1, Func1}, {Mod2, Func2}} <-InsultList].

draw_modules(Dev, [{_, Mods} | _] = LayerList)->
    [begin
		draw_module(Dev, Mod, LayerList)
	end || Mod <-Mods].

draw_module(Dev, Mod, [{_, List} | _] = LayerList)->
    Name = get_layer(LayerList, Mod, lists:member(Mod, List)),
    io:format(Dev, ?Dot_SubHeader, [Name, Name]),
    draw_node(Dev, Mod),
    io:put_chars(Dev, ?Dot_Footer).

draw_node(Dev, {_, Type, Number} = Node)->
	{Shape, Font, Color, Label} = get_features(Node),
	io:format(Dev, ?Dot_Node, [Type, Number, Shape, 
		Label, Font, Color, Type, Number]).

get_features(Node = {_, module, _})->
    {box, 18, purple, atom_to_list(?Mod:name(Node))};
get_features(Node = {_, func, _})->
    {hexagon, 14, black, 
	atom_to_list(?Fun:name(Node)) ++ "/" ++
                      integer_to_list(?Fun:arity(Node))}.

draw_parent(Dev, Mod, Func, [{_, List} | _] = LayerList)->
	Name = get_layer(LayerList, Mod, lists:member(Mod, List)),
	io:format(Dev, ?Dot_SubHeader, [Name, Name]),
	draw_node(Dev, Func),
	draw_edge(Dev, Mod, Func, false),
	io:put_chars(Dev, ?Dot_Footer).

get_layer([_ | [{_, List} | _] = Rest], Mod, false)->
	get_layer(Rest, Mod, lists:member(Mod, List));
get_layer([{Layer, _} | _], _, true)->
	Layer.
	

draw_edge(Dev, {_, Type1, N1}, {_, Type2, N2}, Bool)->
	io:format(Dev, 
		insult_edge(Bool), [Type1, N1, Type2, N2]).

insult_edge(false)->
	?Dot_Edge;
insult_edge(true)->
	?Dot_InsultEdge.


% @doc Makes request to the ui_router, waits for its answer, then returns it
make_ui_request(Args)->
    ReqID = ?UI:getid(),
    case ?UI:request(ReqID,Args) of
        ok -> receive
                  {ReqID, reply, {ok, R}} -> R;
                  {ReqID, reply, {error, {_, _}}} -> {error, some_bad}
              end;
        deny -> {error, deny}
    end.
