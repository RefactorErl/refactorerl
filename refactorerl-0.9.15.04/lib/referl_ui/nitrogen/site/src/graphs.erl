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

%%% @author Viktoria Fordos <v@fviktoria.hu>

%% -*- mode: nitrogen -*-

-module (graphs).
-compile(export_all).
-include_lib("referl_ui/include/nitrogen.hrl").
-include_lib("nitrogen/include/wf.hrl").
-include("records.hrl").
-vsn("$Rev: 12595 $ ").

%%% ============================================================================
%%% HTML output

main() ->
    wf:set(exclude_mod_list,undefined),
    wf:set(exclude_func_list,undefined),
    wf:set(leaves_mod_list,undefined),
    wf:set(leaves_func_list,undefined),
    wf:set(exclude_lib_mod_list,undefined),
    wf:set(exclude_lib_func_list,undefined),
    wf:set(starting_mod_list,undefined),
    wf:set(starting_func_list,undefined),
    wf:set(connection_mod_list,undefined),
    wf:set(connection_func_list,undefined),
    nitrogen_lib:main("graphs.html").

title() -> "RefactorErl Queries".

logout()->
    nitrogen_lib:logout_link(wf:user()).

graph_control_panel()->
    LevelCell=#tablecell{body=[#label{text="Level:"},
                               #dropdown{id=graph_level,
                                         options=[#option{ text="Function", 
                                                           value=func, 
                                                           selected=false},
                                                  #option{ text="Module", 
                                                           value=mod, 
                                                           selected=true }],
                                         html_encode=true,
                                         postback=type_changed}],
                                         class="graph_controll_tablecell"},
    wf:set(graph_level,mod),
    TypeCell=#tablecell{body=[#label{text="Type:"},
                          #dropdown{id=graph_type,
                                    options=[
                                             #option{ text="Whole graph", 
                                                      value=all, 
                                                      selected=true},
                                             #option{ text="Cyclic sub-graph", 
                                                      value=cycles, 
                                                      selected=false}],
                                    html_encode=true}],
                        class="graph_controll_tablecell"},
    ModulesCell=#tablecell{body=[
        #label{text="Expression:"},
        #table{rows=[#tablerow{cells=[
        #tablecell{body=[
        #textbox_autocomplete {id=expression_module, 
                             tag=mods}]},
        #tablecell{body=
        #button{id=add_exclude_mod_button,
         text="Add to",
         postback=add_exclude_mod,
         class=referl_button},
         style="text-align: center"},
        #tablecell{body=
        #button{id=add_leaves_mod_button,
         text="Add to",
         postback=add_leaves_mod,
         class=referl_button},
         style="text-align: center"},
        #tablecell{body=
        #button{id=add_exclude_lib_mod_button,
         text="Add to",
         postback=add_exclude_lib_mod,
         class=referl_button},
         style="text-align: center"},
        #tablecell{body=
        #button{id=add_starting_mod_button,
         text="Add to",
         postback=add_starting_mod,
         class=referl_button},
         style="text-align: center"},
        #tablecell{body=
        #button{id=add_connection_mod_button,
         text="Add to",
         postback=add_connection_mod,
         class=referl_button},
         style="text-align: center"}]},
        #tablerow{cells=[
        #tablecell{body=#checkbox{id=nootp_mod,
                                  text="Exclude OTP",
                                  checked=false}},
        #tablecell{body=[#br{},
        #label{text="Excluded modules:"},
        #textarea{id=exclude_mod_list,class=[nodelist]}]},
		#tablecell{body=[#br{},
        #label{text="Excluded children of:"},
        #textarea{id=leaves_mod_list,class=[nodelist]}]},
		#tablecell{body=[#br{},
        #label{text="Excluded libraries:"},
        #textarea{id=exclude_lib_mod_list,class=[nodelist]}]},
		#tablecell{body=[#br{},
        #label{text="Starting modules:"},
        #textarea{id=starting_mod_list,class=[nodelist]}]},
        #tablecell{body=[#br{},
        #label{text="Connection modules:"},
        #textarea{id=connection_mod_list,class=[nodelist]}]}]}]}],
                            class="graph_controll_tablecell"},
                            
    FunctionsCell=#tablecell{body=[
        #label{text="Expression:"},
        #table{rows=[#tablerow{cells=[
        #tablecell{body=[
        #textbox_autocomplete {id=expression_function, 
                             tag=funs}]},
        #tablecell{body=
        #button{id=add_exclude_func_button,
                text="Add to",
                postback=add_exclude_func,
                class=referl_button},
				style="text-align: center"},
		#tablecell{body=
        #button{id=add_leaves_func_button,
                text="Add to",
                postback=add_leaves_func,
                class=referl_button},
				style="text-align: center"},
		#tablecell{body=
        #button{id=add_exclude_lib_func_button,
                text="Add to",
                postback=add_exclude_lib_func,
                class=referl_button},
				style="text-align: center"},
		#tablecell{body=
        #button{id=add_starting_func_button,
                text="Add to",
                postback=add_starting_func,
                class=referl_button},
				style="text-align: center"},
        #tablecell{body=
        #button{id=add_connection_func_button,
                text="Add to",
                postback=add_connection_func,
                class=referl_button},
				style="text-align: center"}]},
        #tablerow{cells=[#tablecell{body=#checkbox{id=nootp_func,
                                                   text="Exclude OTP",
                                                   checked=false}},
        #tablecell{body=[#br{},
        #label{text="Excluded functions:"},
        #textarea{id=exclude_func_list,class=[nodelist]}]},
		#tablecell{body=[#br{},
        #label{text="Excluded children of:"},
        #textarea{id=leaves_func_list,class=[nodelist]}]},
		#tablecell{body=[#br{},
        #label{text="Excluded libraries:"},
        #textarea{id=exclude_lib_func_list,class=[nodelist]}]},
		#tablecell{body=[#br{},
        #label{text="Starting functions:"},
        #textarea{id=starting_func_list,class=[nodelist]}]},
        #tablecell{body=[#br{},
        #label{text="Connection functions:"},
        #textarea{id=connection_func_list,class=[nodelist]}]}]}]}],
                            class="graph_controll_tablecell"},
    BtnCell1=#tablecell{body=[#button{id=draw_graph,
                                     text="Generate svg graph",
                                     postback=generate_graph,
                                     class=[referl_button,need_db]}],
                       class=["graph_controll_tablecell","centered"]},
    BtnCell2=#tablecell{body=[#button{id=draw_graph,
                                     text="Print dependencies",
                                     postback=print_graph,
                                     class=[referl_button,need_db]}],
                       class=["graph_controll_tablecell","centered"]},
    BtnCell3=#tablecell{body=[#button{id=draw_graph,
                                     text="Generate smart graph (beta)",
                                     postback=generate_java_graph,
                                     class=[referl_button,need_db]}],
                       class=["graph_controll_tablecell","centered"]},    
    ControlTable=#table{id=graph_control_panel,
                        rows=[#tablerow{cells=[LevelCell]},
                              #tablerow{cells=[TypeCell]},
                              #tablerow{id=mod,
                                        cells=[ModulesCell]},
                              #tablerow{id=func,
                                        cells=[FunctionsCell]},
                              #tablerow{cells=[BtnCell1]},
                              #tablerow{cells=[BtnCell2]},
                              #tablerow{cells=[BtnCell3]}]},
    wf:wire(func,#hide{}),
    %see custom_validator's definition
    wf:wire(draw_graph, graph_type, 
        #validate{validators=[
            #custom{text="Please, select type or starting node", 
                    tag=some_tag, 
                    function=fun custom_validator/2}]}),
    ControlTable.

functionblock_control_panel()->
    Dirs= ?NITRO_CORE:get_loaded_directories(),
    Items=gen_sortitem_list(Dirs),
    wf:state(fb_sources, Dirs),
    Source=fb_sources_sortblock(Items),
    Subject=#sortblock { class=["simple","fb_block"], group=subjects, 
                 connect_with_groups=directories, tag=subject, 
                 items=[]},
    wf:state(fb_subjects, []),
    HdRow=#tablerow{cells=[#tablecell{body="Functionblocks", 
                                  class=["graph_controll_tablecell", "bold"]},
                           #tablecell{body="Subjects", 
                                  class=["graph_controll_tablecell", "bold"]}]},
	TypeContents=[#label{text="Type:"},
                          #dropdown{id=graph_fb_type,
                                    options=[
                                             #option{ text="Whole graph", 
                                                      value=all, 
                                                      selected=true},
                                             #option{ text="Cyclic sub-graph", 
                                                      value=cycles, 
                                                      selected=false}],
                                    html_encode=true}],
    RegexpContents=[#label{text="Expression:"},
                    #textbox{id=user_regexp},
                    #button{id=add_re, 
                            text="Add", 
                            postback=add_regexp, 
                            class=[referl_button]}],
	TypeRow=#tablerow{cells=[#tablecell{body=TypeContents,
                                      class="graph_controll_tablecell",
                                      colspan=2,
                                      align="left"}]},
    RegexpRow=#tablerow{cells=[#tablecell{body=RegexpContents,
                                      class="graph_controll_tablecell",
                                      colspan=2,
                                      align="left"}]},
    BdRow=#tablerow{cells=[#tablecell{body=Source, 
                                      class="graph_controll_tablecell", 
                                      valign="top"},
                           #tablecell{body=Subject, 
                                      class="graph_controll_tablecell", 
                                      valign="top"}]},
    CtrRow=#tablerow{cells=[#tablecell{body=#button{id=new_query_button, 
                                                    text="Generate graph", 
                                                    postback=generate_fb_graph, 
                                                class=[referl_button,need_db]}, 
                                        class="graph_controll_tablecell", 
                                        colspan=2,
                                        align="center"}]},
    wf:wire(add_re, 
            user_regexp, 
            #validate { validators=[#is_required { text="Required." }]}),
    
    #table{rows=[TypeRow, RegexpRow, HdRow, BdRow, CtrRow],class="graph_controll_table"}.

result_panel()->
    #panel{id=result_panel, body=[]}.
    
%%% ============================================================================
%%% autocomplete

autocomplete_enter_event(SearchTerm, mods) ->
    Mods=?NITRO_SERVICES:get_mods(),
    DataM=lists:map(fun(Elem)-> 
                           {struct, [{id, list_to_binary(Elem)}, 
                                     {label, list_to_binary(Elem)} , 
                                     {value, list_to_binary(filename:rootname(
                                                              Elem)) }]} 
                    end,Mods),
    List = [{struct,[{id, Id }, {label, Label}, {value, Value}]} || 
            {struct,[{id, Id }, {label, Label}, {value, Value}]} <- DataM, 
            string:str(string:to_lower(binary_to_list(Label)), 
                       string:to_lower(SearchTerm)) > 0],
    mochijson2:encode(List);

autocomplete_enter_event(SearchTerm, funs) ->
    Funs=?NITRO_SERVICES:get_funs(), 
    DataF=lists:map(fun(Elem)-> 
                           {struct, [{id, list_to_binary(Elem)}, 
                                     {label, list_to_binary(Elem)} , 
                                     {value, list_to_binary(Elem) }]} 
                    end,Funs),
    List = [{struct,[{id, Id }, {label, Label}, {value, Value}]} || 
            {struct,[{id, Id }, {label, Label}, {value, Value}]} <- DataF, 
            string:str(string:to_lower(binary_to_list(Label)), 
                       string:to_lower(SearchTerm)) > 0],
    mochijson2:encode(List).

autocomplete_select_event({struct,[{<<"id">>,_},{<<"value">>,Value}]},_Tag)->
    wf:flash(Value),
    ok.

%%% ============================================================================
%%% validators

%Only postback the form, when at least one of those three is selected.
custom_validator(_Tag, _Value) ->
    case {wf:q(graph_type),
          wf:q(expression_module), 
          wf:q(expression_function)} of
        {"none", undefined, undefined} -> false;
        _ ->true
    end.

%%% ============================================================================
%%% Handlers for postback events
event(type_changed)->
    case wf:q(graph_level) of
        "func" -> 
            wf:wire(func,#show{}),
            wf:wire(mod,#hide{});
        "mod" ->
            wf:wire(mod,#show{}),
            wf:wire(func,#hide{})
    end;

event({add_to_list,From,To}) ->
    Text=get_text(From),
    List=wf:q(To),
    if
        (Text/="") and ((List==undefined) or (List=="")) -> wf:set(To,Text);
        Text/="" -> wf:set(To,List++"\n"++Text);
        true -> ok
    end;

event(add_exclude_mod) ->
    event({add_to_list,expression_module,exclude_mod_list});
    
event(add_exclude_func) ->
    event({add_to_list,expression_function,exclude_func_list});

event(add_leaves_mod) ->
    event({add_to_list,expression_module,leaves_mod_list});
    
event(add_leaves_func) ->
    event({add_to_list,expression_function,leaves_func_list});

event(add_exclude_lib_mod) ->
    event({add_to_list,expression_module,exclude_lib_mod_list});
    
event(add_exclude_lib_func) ->
    event({add_to_list,expression_function,exclude_lib_func_list});

event(add_starting_mod) ->
    event({add_to_list,expression_module,starting_mod_list});
    
event(add_starting_func) ->
    event({add_to_list,expression_function,starting_func_list});

event(add_connection_mod) ->
    event({add_to_list,expression_module,connection_mod_list});
    
event(add_connection_func) ->
    event({add_to_list,expression_function,connection_func_list});

event(add_regexp)->
    Regexp=wf:q(user_regexp),
    List=wf:state(fb_sources)++[Regexp]--wf:state(fb_subjects),
    UniqueItems=lists:usort(List),
    wf:state(fb_sources, UniqueItems),
    Items=gen_sortitem_list(UniqueItems),
    wf:replace(src_sortblock, fb_sources_sortblock(Items));

event(print_graph) ->
    {Parameters,Error}=get_parameters(),
    Fun=case Error of
        {error,E} -> fun() -> {error,E} end;
        _ -> fun() -> print_dep_graph(Parameters) end
    end,
    wf:continue({continue, dep_graph_print}, Fun, 60*60*1000);

event(generate_graph)->
    {Parameters,Error}=get_parameters(),
    Fun=case Error of
        {error,E} -> fun() -> {error,E} end;
        _ -> fun() -> gen_dep_graph(Parameters) end
    end,
    wf:continue({continue, dep_graph}, Fun, 60*60*1000);

event(generate_java_graph)->
    {Parameters,Error}=get_parameters(),
    DParameters = element(1, Parameters),
    User = element(2, Parameters),
    Level = proplists:get_value(level, DParameters),
    GenerateOptions = [{output_type, js}, {dependency_options, DParameters}, {dependency_level, Level}],
    Fun=case Error of
        {error,E} -> fun() -> {error,E} end;
        _ -> fun() -> ?NITRO_SERVICES:generate_smart_graph(GenerateOptions, User) end
    end,
    wf:continue({continue, draw_graph}, Fun, 60*60*1000);

event(generate_fb_graph)->
	gen_fb_graph(lists:map(fun get_group/1, get_group(wf:state(fb_subjects))));

event(logout) ->
    nitrogen_lib:logout(wf:user()).

get_group(List) ->
	Group = string:tokens(List, " ,"),
	if length(Group) == 1 -> [G] = Group, G;
		true -> Group
	end.
%%% ============================================================================
%%% Handlers for continous events    
continue({continue, _}, timeout) ->
    create_message("The execution of the analysis has been 
                    exceeded the timeout limit.", error);

continue({continue, dep_graph_print}, Result) ->
    case Result of
        {error, Reason} -> create_message(Reason, error);
        {result, R} ->
            FormattedResult = 
                [io_lib:format("<br />~p -> ~p", [Entity, Deps]) 
                ||{Entity, Deps} <- R],
            create_message(io_lib:format("<strong>Relations</strong>:
                                          ~s<br /><br />",
                [FormattedResult]), information)
    end;

continue({continue, draw_graph}, {Result,Script}) ->
    case Result of
        {error, Reason} -> create_message(Reason, error);
        result -> make_javagraph(Script);
        _ -> create_error_message("An error occured while generating" ++
             " smart graph.")
    end;

continue({continue, _}, Result) ->
    %{_, Ref}=dets:open_file(?SQ_TAB),
    %RelationsResult=dets:match(Ref,'$1'),
    %io:format("~p~n",[RelationsResult]),
    case Result of
        {error, Reason} -> create_message(Reason, error);
        _ -> create_message(Result, result)
    end.
%%% ============================================================================
%%% Handlers for sort events    
sort_event(subject, Items) -> 
    wf:state(fb_subjects,Items),
    ok;

sort_event(source,Items) ->
    wf:state(fb_sources, Items),
    ok.
%%% ============================================================================
%%% Helper functions

%% @doc Creates a javascript for a graph from given binary file data

make_javagraph(Script) ->
    wf:session(javagraph,Script),
    wf:replace(result_panel,create_message_table(
         #link{text="See generated graph", 
               title = "See result", 
               url="javascript:newWindow('graphresult')"})),
     wf:wire(result_panel,#hide{}),
     wf:wire(result_panel,#appear{}).

%% @doc Parse a node line
parse_graphnode_line(E,Width,Height) ->
    L=string:tokens(E," "),
    {lists:nth(2,L),lists:nth(7,L),list_to_num(lists:nth(3,L))/Width,
        (Height-list_to_num(lists:nth(4,L)))/Height}.

%% @doc Parse an edge line
parse_graphedge_line(E) ->
    L=string:tokens(E," "),
    {lists:nth(2,L),lists:nth(3,L),lists:last(L)}.

%% @doc Converts string to integer or float
list_to_num(L) ->
    case string:to_float(L) of
        {error,no_float} -> list_to_integer(L);
        {F,_} -> F
    end.

%% @doc Converts float or integer to string
num_to_list(N) ->
    try
        float_to_list(N)
    catch
        _:_ -> integer_to_list(N)
    end.

%% @doc Node attributes
get_node_name({_,module,Id}) -> "m"++integer_to_list(Id);
get_node_name({_,func,Id}) -> "f"++integer_to_list(Id).

get_node_size([$m|_]) -> "6";
get_node_size([$f|_]) -> "4";
get_node_size(_) -> "10".

get_node_color([$m|_]) -> "#000000";
get_node_color([$f|_]) -> "#666666";
get_node_color(_) -> "#000000".

%% @doc Get parameters for a dependency analysis
get_parameters() ->
    %feedback
    create_message("Running analysis, please wait..", information),
    %start async
    Level=wf:q(graph_level),
    Type=wf:q(graph_type),
    case wf:q(nootp_mod) of
        "on" -> OtpMod=true;
        _ -> OtpMod=false
    end,
    case wf:q(nootp_func) of
        "on" -> OtpFunc=true;
        _ -> OtpFunc=false
    end,
    Gnode=case Level of
              "func"->funs;
              "mod"-> mods;
              _ ->    none
          end,
        
    {Options,Error}=try
    case Gnode of
        none -> 
            {[{level,list_to_existing_atom(Level)}, 
                {type, list_to_existing_atom(Type)}],ok};
        mods ->
			{[{level,list_to_existing_atom(Level)},
				{type,list_to_existing_atom(Type)},
				{exclude,string:tokens(get_text(exclude_mod_list),"\n")},
				{exclude_children,string:tokens(get_text(leaves_mod_list),"\n")},
				{exclude_lib,string:tokens(get_text(exclude_lib_mod_list),"\n")},
				{exclude_otp,OtpMod},
				{starting_nodes,string:tokens(get_text(starting_mod_list),"\n")},
				{connection,string:tokens(get_text(connection_mod_list),"\n")}],ok};
        funs -> 
			{[{level,list_to_existing_atom(Level)},
				{type,list_to_existing_atom(Type)},
				{exclude,string:tokens(get_text(exclude_func_list),"\n")},
				{exclude_children,string:tokens(get_text(leaves_func_list),"\n")},
				{exclude_lib,string:tokens(get_text(exclude_lib_func_list),"\n")},
				{exclude_otp,OtpFunc},
				{starting_nodes,string:tokens(get_text(starting_func_list),"\n")},
				{connection,string:tokens(get_text(connection_func_list),"\n")}],ok}
    end
    catch {error,E} -> 
        {0,{error,E}} 
    end,
    {{Options,wf:user()},Error}.

get_function([]) -> [];
get_function(FunText) ->
    Result=re:split(FunText,"([:/])",[{return,list},group]),
    case Result of
        [[Module,":"],[Function,"/"],[Arity]] when length(Module)>0, 
                length(Function)>0, length(Arity)>0 ->
            Fun=try
                    ?NITRO_SERVICES:get_function(Module,Function,Arity)
                catch
                    _:_ -> []
                end,
            if
                Fun==[] -> throw({error,
                    "Error: The following function doesn't exist: "++FunText});
                true -> hd(Fun)
            end;
        _ -> throw({error,"Error: Bad function format: "++FunText})
    end.
    
%% @doc Get string from a html element
get_text(Id) ->
    Result=wf:q(Id),
    if
        Result==undefined -> "";
        true -> Result
    end.

gen_dep_graph({Options, User})->
    ?NITRO_SERVICES:generate_dependency_graph(Options, User).

print_dep_graph({Options, User})->
    ?NITRO_SERVICES:print_dependency_graph(Options, User).

draw_java_graph({Options, User})->
    FileName=User++".dot",
    TargetDir= ?NITRO_CORE:get_images_root(),
    DotName=filename:join([TargetDir,FileName]),
    XOptions=Options++[{dot, DotName}],
    {?NITRO_SERVICES:generate_dependency_graph(XOptions, User),
    DotName}.

start_gen_fb_graph({Parameters, Type, User})->
    ?NITRO_SERVICES:generate_fb_graph(Parameters, Type, User).

gen_fb_graph(Subjects)->
    %feedback
    create_message("Running analysis, please wait..", information),
    %start async
    Parameters={Subjects, list_to_existing_atom(wf:q(graph_fb_type)), wf:user()},
    Fun = fun() -> start_gen_fb_graph(Parameters) end,
    wf:continue({continue, fb_graph}, Fun, 60*60*1000).

create_message(Content, Type)->
    Message=case Type of
                result -> create_result_message(Content);
                information -> create_information_message(Content);
                _ -> create_error_message(Content)
            end,
    wf:replace(result_panel,create_message_table(Message)),
    wf:wire(result_panel,#hide{}),
    wf:wire(result_panel,#appear{}).

create_message_table(Message)->
    Cell=#tablecell{body=Message, 
                    class=["graph_controll_tablecell", "red_border"]},
    #table{id=result_panel, 
           rows=[#tablerow{cells=[Cell]}], 
           class="query_result_table"}.

create_information_message(Text)->
    [#p{body=wf:to_list(Text)}].

create_error_message(Text)->
    wf:wire(#notification{type=error, text=Text}),
    [Text].

create_result_message({DotName, SvgName})->
    [#link{text="Generated graph in .dot", 
           title = "Download result", 
           url="images?image="++DotName,
           style="margin:15px; padding:5px;" },
     #br{},
     #br{},
     #link{text="See generated graph in .svg", 
           title = "See result in new window", 
           url="javascript:newWindow('images?image="++SvgName++"');", 
           style="margin:15px; padding:5px;" }].

fb_sources_sortblock(Items)->
    #sortblock{id=src_sortblock, class=["simple","fb_block"], group=directories, 
               connect_with_groups=subjects, tag=source, 
               items=Items}.

gen_sortitem_list(Items)->
    [#sortitem { tag=Item, body=Item, class="fb_block_item" } || 
        Item <- Items, Item/=[]].
