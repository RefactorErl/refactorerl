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

%%% @author Adam Szabo <otisonoza@caesar.elte.hu>

%%% @doc
%%% After validating the generating options, the module generates a sigma.min
%%% javascript or a HTML file with the previous javascript using a generated
%%% dot and txt file.

-module(refusr_smart_graph).

-export([generate/1, error_text/2]).

-include("user.hrl").

-vsn("$Rev: 12595 $").

%%% ===========================================================================
%%% Generating functions

%% @spec generate(Options::proplist()) -> {result, Result::string()}
%%
%% @doc Returns a javascript, if the output_path property is js,
%%     return a html file's path if the property is js_with_html.
%%
%% Options contain the generating options.
%%
%% Options must contain: output_type, dependency_options, level
%%
%% output_path is optional. If this property is not given, a random
%% file will be generated, otherwise the result will be in the given
%% file.
%%
%% Possible generating options:
%%      output_type: js | js_with_html
%%      dependency_options:: proplist()
%%      Possible dependency_options:
%%          level: mod | func
%%          type: all | cyclic
%%          starting_nodes: list()
%%          exclude: list()
%%          exclude_children: list()
%%          exclude_otp: true | false
%%          exclude_lib: list()
%%      dependency_level: func | mod
%%      output_path: string()
%%
%% Examples:
%%    DepOpts = [{level, mod},{type, all},{starting_nodes,[refusr_cyclic_mod]},
%%              {exclude, []}, {exclude_children, []}, {exclude_otp, false}].
%%    refusr_smart_graph:generate([{output_type, js},
%%        {dependency_level, mod}, {dependency_options, DepOpts}]).
%%
%%    refusr_smart_graph:generate([{output_type, js_with_html},
%%        {dependency_options, [{type, all}]}, {dependency_level, func},
%%        {output_path, "/tmp/alma.html"}]).

generate(Options) ->
    Options,
    validate(Options),
    DOptions = proplists:delete(dot, Options),
    Type = proplists:get_value(output_type, DOptions),
    OutputPath = case proplists:is_defined(output_path, DOptions) of
        true -> proplists:get_value(output_path, DOptions);
        false -> filename:join(?MISC:data_dir(), make_unique_name("sq_", ".html"))
    end,
    DependencyLevel = proplists:get_value(dependency_level, DOptions),
    DotPath = generate_dependency_graph_new(DependencyLevel, DOptions),
    Result = case Type of
        js -> generate_js(DotPath);
        js_with_html -> generate_html(DotPath, OutputPath)
    end,
    {result, Result}.

generate_js(DotPath) ->
    ResultFile=filename:join(filename:dirname(DotPath), make_unique_name("txt_", ".txt")),
    Result = os:cmd("dot -Tplain-ext -Gnodesep=0.001 "++DotPath++" -o "++ResultFile),
    case Result of
        [] ->
            file:delete(DotPath),
            {X, Binary} = file:read_file(ResultFile),
            if
                X==error -> throw(?LocalError(file_read_error, ResultFile));
                true ->
                    file:delete(ResultFile),
                    make_javagraph(Binary)
            end;
        _ -> throw(?LocalError(os_error, []))
    end.

generate_html(DotPath, OutputPath) ->
    Script = generate_js(DotPath),
    {Status, Dev} = file:open(OutputPath, [write]),
    case Status of
        ok ->
    case file:write(Dev,
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" " ++
    "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" ++
"<html style=\"width:100%; height:100%\" " ++
"xmlns=\"http://www.w3.org/1999/xhtml\">" ++
"<head>" ++
"<meta http-equiv=\"Content-Type\" "++
"content=\"text/html; charset=UTF-8\" />" ++
"<meta http-equiv=\"Connection: keep-alive\" />" ++
"<script src=\""++ get_jquery() ++"\"" ++
"type='text/javascript' charset='utf-8'></script>" ++
"<script src=\""++ get_jquery_ui() ++"\"" ++
"type='text/javascript' charset='utf-8'></script>" ++
"<script src=\""++ get_sigmajs() ++"\"" ++
"type='text/javascript' charset='utf-8'></script>" ++
"</head> <body style=\"width:100%; height:100%\"> <div id=\"gr\" "++
"style=\"width:100%; height:100%\"> </div>" ++
"<script>" ++
"function test() {" ++ Script ++ "}" ++
"$(document).ready( function() { test(); });" ++
"</script> </body> </html>") of
    {error, _} -> throw(?LocalError(file_write_error, OutputPath));
    ok -> file:close(Dev),
            OutputPath
    end;
        error ->
            throw(?LocalError(file_write_error, OutputPath))
    end.

make_javagraph(Binary) ->
    GraphData0=binary_to_list(Binary),
    GraphData1=lists:filter(fun(Char) -> Char =/= $\r end, GraphData0),
    Lines=string:tokens(GraphData1,"\n"),
    {_,_,W,H}=list_to_tuple(string:tokens(hd(Lines)," ")),
    Width=list_to_num(W),
    Height=list_to_num(H),
    Nodes=[parse_graphnode_line(E,Width,Height) || E<-Lines,
			(lists:nth(length(E), E) =/= $<) and (hd(E) == $n)],
    Edges=[parse_graphedge_line(E) || E<-Lines, hd(E)==$e],
    Script=generate_graph_script(Nodes,Edges),
    Script.

generate_graph_script(Nodes,Edges) ->
    NodesScript=lists:foldr(fun(E,Text) ->
        {Name,Label,X,Y}=E,
        "g.addNode('"++Name++"',{
        x: "++num_to_list(X*3)++",
        y: "++num_to_list(Y)++",
        size:'"++get_node_size(Name)++"',
        label: '"++Label++"',
        color: '"++get_node_color(Name)++"'});"++Text end,"",Nodes),
    EdgesScript=lists:foldr(fun(E,Text) ->
        {From,To,Color}=E,
		Type = case Color of
		           "green" -> "curve";
		           _ -> "line"
			   end,
        "g.addEdge('"++From++To++Color++"','"++From++"','"
        ++To++"',{color: '"++Color++"', type: '"++Type++"'});"++Text end,"",Edges),

"g = sigma.init(document.getElementById('gr'));

g.drawingProperties({
  defaultLabelColor: '#000',
  font: 'Arial',
  edgeColor: 'source',
  defaultEdgeArrow: 'target'
});

g.bind('overnodes',function(event){
var nodes = event.content;
var neighbors = {};
g.iterEdges(function(e){
if(nodes.indexOf(e.source)<0 && nodes.indexOf(e.target)<0){
e.hidden=1;
}else{
neighbors[e.source] = 1;
neighbors[e.target] = 1;
}
}).iterNodes(function(n){
if(!neighbors[n.id] && nodes.indexOf(n.id)!=0){
n.hidden = 1;
}else{
n.hidden = 0;
}
}).draw(2,2,2);
}).bind('outnodes',function(){
g.iterEdges(function(e){
e.hidden = 0;
}).iterNodes(function(n){
n.hidden = 0;
}).draw(2,2,2);
});"
++NodesScript++EdgesScript++"g.draw();".

generate_dependency_graph_new(Level, SGOptions)    ->
    Options=proplists:get_value(dependency_options, SGOptions),
    DotName=filename:join(?MISC:data_dir(), make_unique_name("dot_", ".dot")),
    OptsNoDot = proplists:delete(dot, Options),
    MyOpts = lists:flatten(OptsNoDot ++ 
                    [{file_path, DotName},{level,Level},{output,simple_dot}]),
    refusr_dep_graph:draw(MyOpts),
    DotName.

%%% ===========================================================================
%%% Functions for generating

get_node_size([$m|_]) -> "6";
get_node_size([$f|_]) -> "4";
get_node_size(_) -> "10".

get_node_color([$m|_]) -> "#000000";
get_node_color([$f|_]) -> "#666666";
get_node_color(_) -> "#000000".

list_to_num(L) ->
    case string:to_float(L) of
        {error,no_float} -> list_to_integer(L);
        {F,_} -> F
    end.

num_to_list(N) ->
    try
        float_to_list(N)
    catch
        _:_ -> integer_to_list(N)
    end.

parse_graphnode_line(E,Width,Height) ->
    L=string:tokens(E," "),
    {lists:nth(2,L),lists:nth(7,L),list_to_num(lists:nth(3,L))/Width,
        (Height-list_to_num(lists:nth(4,L)))/Height}.

parse_graphedge_line(E) ->
    L=string:tokens(E," "),
    {lists:nth(2,L),lists:nth(3,L),lists:last(L)}.

get_base()->
    Path0 = filename:split(filename:dirname(code:which(?MODULE))),
    filename:join(lists:sublist(Path0,length(Path0)-3)).

get_sigmajs()->
    get_base() ++ 
		"/lib/referl_ui/web2/app/lib/sigma/sigma.min.js".

get_jquery()->
    get_base() ++ 
		"/lib/referl_ui/web2/app/lib/jquery/jquery.min.js".

get_jquery_ui()->
    get_base() ++ 
		"/lib/referl_ui/nitrogen/site/static/nitrogen/jquery-ui.js".

%%% ===========================================================================
%%% Validating functions

validate(Options) ->
    validate(output_type, Options).

validate(output_type, Options) ->
    case proplists:is_defined(output_type, Options) of
        true ->
            Type = proplists:get_value(output_type, Options),
            case Type of
                js -> validate(dependency_options, Options);
                js_with_html -> validate(dependency_options, Options);
                _ -> throw(?LocalError(bad_output_type,[Type]))
            end;
        false -> throw(?LocalError(no_output_type,[]))
    end;

validate(dependency_options, Options) ->
    case proplists:is_defined(dependency_options, Options) of
        true ->
            _ = validate(dependency_level, Options);
        false -> throw(?LocalError(no_dependency_options,[]))
    end;

validate(dependency_level, Options) ->
    case proplists:is_defined(dependency_level, Options) of
        true ->
            DependencyLevel = proplists:get_value(dependency_level, Options),
            case DependencyLevel of
                mod -> ok;
                func -> ok;
                _ -> throw(?LocalError(bad_level_type, DependencyLevel))
            end;
        false -> throw(?LocalError(no_level_type,[]))
    end.

%%% ===========================================================================
%%% Error functions

error_text(bad_output_type, ErrorArg) when is_atom(ErrorArg) ->
    "Bad output type:" ++ erlang:atom_to_list(ErrorArg);
error_text(bad_output_type, _ErrorArg) ->
    "Bad output type";
error_text(no_dpendency_type, _ErrorArg) ->
    "No output type was given";
error_text(no_dependency_options, _ErrorArg) ->
    "No dependency options were given";
error_text(bad_level_type, ErrorArg) when is_atom(ErrorArg) ->
    "Bad level type:" ++ erlang:atom_to_list(ErrorArg);
error_text(bad_level_type, _ErrorArg) ->
    "Bad level type";
error_text(no_level_type, _ErrorArg) ->
    "No level type was given";
error_text(badarg, _ErrorArg) ->
    "Bad arguments were given";
error_text(file_read_error, File) ->
    "Error while reading the following file: " ++ File;
error_text(file_write_error, File) ->
    "Error while writing to file: " ++ File;
error_text(os_error, _ErrorArg) ->
    "Operation not permitted".

%%% ===========================================================================
%%% Naming functions

make_unique_name(Prefix, Suffix)->
    string:concat(string:concat(Prefix,make_uid()), Suffix).

make_uid()->
    Ref = make_ref(),
    re:replace(base64:encode_to_string(term_to_binary(Ref)),
               "[^a-zA-Z0-9]",
               "",
               [global, {return, list}]).
