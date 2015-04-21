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

%%% @doc Dependency examination.

%%% @author Barbara Oláh <olahb@caesar.elte.hu>
%%% @author Dávid Mikó <dav3333333@caesar.elte.hu>

-module(refusr_dep_graph).

-export([draw/1]).

-export([new_graph/0, read_graph/1, write_graph/1, filter_graph/11,
            add_vertex/3, add_edge/5, delete_mbgraph/0,
            new_mb_graph/0, read_mb_graph/1, write_mb_graph/1]).

-export([graph_to_dot/4, graph_to_terms/2, graph_to_terms_name/2,
         graph_to_terms_two/2]).

-export([delete_dep_files/0]).

-export([error_text/2]).

-include("user.hrl").

- on_load(init/0).

-record(dep_query, {number,
                    level,
                    type,
                    otp,
                    exclude,
                    exclude_children,
                    starting_nodes,
                    exclude_lib,
                    groups,
                    connection,
                    file}).

-define(SQTABLE, 
    filename:join([?MISC:data_dir(), 
                    "dep_files" ++ ?MISC:graph_based_postfix() 
                    ++ "/saved_queries"])).
-define(DBHASHTABLE, 
    filename:join([?MISC:data_dir(), 
                    "dep_files" ++ ?MISC:graph_based_postfix() 
                    ++ "/db_hash"])).                   


%%% ============================================================================
%%% Error messages
%%% ============================================================================

%% @spec error_text(ErrorType::error(), ErrorData::any()) -> [string()]
%% @doc Returns a textual description of the error term.
error_text(error_no_lib, [])->
	["RefactorErl cannot find the dep_graph shared library!"];
error_text(error_par, {level, nolevel})->
	["No level has been given"];
error_text(error_par, {Option, Parameter}) when is_atom(Parameter)->
	["Bad ", atom_to_list(Option), " parameter: ", atom_to_list(Parameter)];
error_text(error_par, {Option, Parameter}) when is_integer(Parameter)->
	["Bad ", atom_to_list(Option), " parameter: ", integer_to_list(Parameter)];
error_text(error_par, {Option, _})->
	["Bad ", atom_to_list(Option), " parameter"];
error_text(error_par, Option)->
	["Unknown option: ", Option];
error_text(error_node, Parameter)->
	[Parameter, " is not existing"];
error_text(error_regexp, [])->
	["Bad regular expression"];
error_text(error_regexp, RegExp) when is_atom(RegExp)->
	["Bad regular expression: ", atom_to_list(RegExp)];
error_text(error_regexp, _)->
	["Bad regular expression"];
error_text(error_stn_con, _)->
	["Cannot use both starting_nodes and connection options"];
error_text(error_nomods, _)->
	["nomods_dot output only usable with function level"];
error_text(error_output, _)->
	["Error while making the graph's output"];
error_text(error_load_module, _)->
	["Modules could not be loaded"];
error_text(error_write, _)->
	["Error while writing cache file"];
error_text(error_read, _)->
	["Error while reading cache file"].


%%% ============================================================================
%%% Nif initialization
%%% ============================================================================

%% @spec init() -> ok | Error
%% @doc Load the "dep_graph" nif file
init()->
    Nif = filename:join(code:lib_dir(referl_user, priv), "dep_graph"),
    try
        ok = erlang:load_nif(Nif, 0)
    catch
        _:_ ->
            ok
    end.

%%% ============================================================================
%%% Queries
%%% ============================================================================

%% @spec get_all_functions() -> list()
%% @doc Get the functions from the semantic programgraph
get_all_functions()->
    ?Query:exec(?Query:exec(?Mod:all()),?Mod:locals()).

%% @spec get_all_modules() -> list()
%% @doc Get the modules from the semantic programgraph
get_all_modules()->
   ?Query:exec(?Mod:all()).

%% @spec get_all_modules_name() -> list()
%% @doc Get the modules' names from the semantic programgraph
get_all_modules_name()->
   [ ?Mod:name(M) || M <- ?Query:exec(?Mod:all())].

%% @spec get_all_mod_name_str() -> list()
%% @doc Get the modules' names from the semantic programgraph 
%%      and tmake he modul name's type to string
get_all_mod_name_str()->
   [ atom_to_list(?Mod:name(M)) || M <- ?Query:exec(?Mod:all())].
   
%% @spec get_module_of_function(Fun::tuple()) -> tuple()
%% @doc Get the given function's module 
get_module_of_function(Fun)->
    ?Query:exec(Fun,?Fun:module()).

%% @spec get_functions_of_module(Mod::tuple()) -> list()
%% @doc Get the given module's functions
get_functions_of_module(Mod)->
    ?Query:exec(Mod,?Mod:locals()).
   
%% @spec get_funcalls(Fun::tuple) -> list()
%% @doc Get the functions, that the given function calls 
get_funcalls(Fun)->
    ?Query:exec(Fun, ?Fun:funcalls()).

%% @spec get_func_arity(Fun::tuple()) -> integer()
%% @doc Get the given function's arity
get_func_arity(Fun)->
    ?Fun:arity(Fun).

%% @spec get_func_name(Fun::tuple()) -> atom()
%% @doc Get the given function's name
get_func_name(Fun)->
    ?Fun:name(Fun).

%% @spec get_module_name(Mod::tuple()) -> atom()
%% @doc Get the given module's name
get_module_name(Mod)->
    ?Mod:name(Mod).  

%% @spec get_dyncalls(Fun::tuple()) -> list()
%% @doc Get the functions, that the given function calls dynamic
get_dyncalls(Fun)->
    ?Query:exec(Fun, ?Dynfun:dyncalls()).

%% @spec get_ambcalls(Fun::tuple()) -> list()
%% @doc Get the functions, that the given function calls ambiguous
get_ambcalls(Fun)->
    ?Query:exec(Fun, ?Dynfun:ambcalls()).
    
%% @spec get_function_node({ModName::atom(),FuncName::atom(),Arity::integer()}) 
%%                                                                     -> list() 
%% @doc Get a function node by its module, name and arity
get_function_node({ModName,FuncName,Arity}) 
    when is_atom(ModName), is_atom(FuncName), is_integer(Arity)->
       case refcore_graph:path({'$gn',root,0}, ?Mod:find(ModName)) of
               [Module] ->
                        case ?Query:exec(Module,?Mod:local(FuncName, Arity)) of
                               []           -> throw({not_existing_node,
            ModName ++ ":" ++ FuncName ++ "/" ++ Arity});
                               [{_,Type,Id}]   -> [Type, Id]
                        end;
                _       ->  throw({not_existing_node,
            ModName ++ ":" ++ FuncName ++ "/" ++ Arity})
        end;
get_function_node(_) ->
    [].

%% @spec get_module_node(ModName::atom()) -> list()
%% @doc Get a module node by its name
get_module_node(ModName) when is_atom(ModName)->
        case refcore_graph:path({'$gn',root,0}, ?Mod:find(ModName)) of
            [{_,Type, Id}]  -> [Type,Id];
            _               -> throw({not_existing_node,
									   convert(ModName)})
        end;
get_module_node(_) ->
    [].

%% @spec get_all_file_path() -> list()
%% @doc Get all file path from the semantic programgraph
get_all_file_path() ->
     [ ?File:path(File) || File <- ?Query:exec(?File:all())].

%%% ============================================================================
%%% NIF functions
%%% ============================================================================

%% @spec new_graph() -> integer()
%% @doc Create new graph
new_graph()->
	 throw(?LocalError(error_no_lib, [])).
     
%% @spec new_mb_graph() -> integer()
%% @doc Create new modulblock graph
new_mb_graph()->
	 throw(?LocalError(error_no_lib, [])).

%% @spec write_graph(OutputFile::string()) -> integer()
%% @doc Write graph to lgf file.
write_graph(_OutputFile)->
	 throw(?LocalError(error_no_lib, [])).

%% @spec write_mb_graph(OutputFile::string()) -> integer()
%% @doc Write modulblock graph to lgf file.
write_mb_graph(_OutputFile)->
	 throw(?LocalError(error_no_lib, [])).

%% @spec read_graph(InputFile::string()) -> integer()
%% @doc Read graph from lgf file.
read_graph(_InputFile)->
	 throw(?LocalError(error_no_lib, [])).
     
%% @spec read_mb_graph(InputFile::string()) -> integer()
%% @doc Read modulblock graph from lgf file.
read_mb_graph(_InputFile)->
	 throw(?LocalError(error_no_lib, [])).

%% @spec add_vertex(Type::atom(), Id::integer(), Name::atom()) -> integer()
%% @doc Add a vertex to the graph.
add_vertex(_Type,_Id,_Name)->
	 throw(?LocalError(error_no_lib, [])).

%% @spec add_edge(SourceType::atom(), SourceId::integer(), TargetType::atom(), 
%%                TargetId::integer(), EgdeType::atom()) -> integer()     
%% @doc Add an edge to the graph.
add_edge(_SourceType, _SourceId, _TargetType, _TargetId, _EgdeType)->
	 throw(?LocalError(error_no_lib, [])).

%% @spec delete_mbgraph() -> integer() 
%% @doc Delete modulblock graph.
delete_mbgraph()->
	 throw(?LocalError(error_no_lib, [])).
     
%% @spec filter_graph(Level::atom(), Type::atom(), ExcludeOtp::list(), 
%%                    ExculdeNodes::list(), ExcludeChildren::list(),
%%                    ExcludeLibrary::list(), StartingNodes::list(),
%%                    MBGroups::list(), Connextion::list(), CType::integer(),
%%					  MaxDepth::integer()) -> integer()
%% @doc Filter the dependency graph.
filter_graph(_Level, _Type, _ExcludeOtp, _ExculdeNodes, _ExcludeChildren, 
             _ExcludeLibrary, _StartingNodes, _MBGroups, _Connection, _CType, _MaxDepth)->
	 throw(?LocalError(error_no_lib, [])).

%% @spec graph_to_dot(GraphLevel::atom(), OutputFile::string(), 
%%                    DotParameters::string(), DotType::atom()) 
%%                                                              -> integer()
%% @doc Write the graph to dot file.
graph_to_dot(_GraphLevel,_OutputFile,_DotParameters,_DotType)->
	 throw(?LocalError(error_no_lib, [])).

%% @spec graph_to_terms(GraphLevel::atom(), OutputFile::string()) -> integer()
%% @doc Write the graph to erlang terms.
graph_to_terms(_GraphLevel,_OutputFile)->
	 throw(?LocalError(error_no_lib, [])).

%% @spec graph_to_terms_two(GraphLevel::atom(), OutputFile::string()) -> integer()
%% @doc Write the graph to erlang terms.
graph_to_terms_two(_GraphLevel,_OutputFile)->
	 throw(?LocalError(error_no_lib, [])).
            
%% @spec graph_to_terms_name(GraphLevel::atom(), OutputFile::string()) -> integer()
%% @doc Write the graph to erlang terms.
graph_to_terms_name(_GraphLevel,_OutputFile)->
	 throw(?LocalError(error_no_lib, [])).

%%% ============================================================================
%%% Build graph
%%% ============================================================================

%% @spec build_graph() -> ok
%% @doc Build dependency graph
build_graph()->
    new_graph(),
    graph_add_elements(),
    graph_add_call_edges(),
    write_graph(check_data_dir() ++ "dep_graph.lgf"),
    ok.

%% @spec graph_add_elements() -> ok
%% @doc Add module and function verticles, and module -> function contain edges
graph_add_elements()->
    lists:map(  fun(M = {_,MType,MId})-> 
                        add_vertex(MType, MId, get_module_name(M)),
                        lists:map(  fun(F = {_,FType,FId})->
                                        FName = create_func_vertex_name(F),
                                        add_vertex(FType,FId,FName),
                                        add_edge(MType,MId,FType,FId,contain)
                                    end,
                         get_functions_of_module(M))
                end, 
                get_all_modules()),
    ok.

%% @spec graph_add_call_edges() -> ok
%% @doc Add all funcall, dyncall, ambcall edges to the graph    
graph_add_call_edges()->
    lists:map(  fun(Source = {_,SType,SId}) ->
                    lists:map( fun(Target = {_,TType,TId}) ->
                                    add_edge(SType,SId,TType,TId,funcall),
                                    graph_add_modcall_edge(Source,Target)
                                end,
                    get_funcalls(Source)),
                    lists:map( fun(Target = {_,TType,TId}) ->
                                    add_edge(SType,SId,TType,TId,dyncall),
                                    graph_add_modcall_edge(Source,Target)
                                end,
                    get_dyncalls(Source)),
                    lists:map( fun(Target = {_,TType,TId}) ->
                                    add_edge(SType,SId,TType,TId,ambcall),
                                    graph_add_modcall_edge(Source,Target)
                                end,
                    get_ambcalls(Source))
                end,
        get_all_functions()),
    ok.    

%% @spec graph_add_modcall_edge(Fun1::tuple(), Fun2::tuple) -> ok
%% @doc Add modcall edges to the graph, except loop edges
graph_add_modcall_edge(Fun1, Fun2)->
    [Mod1] = get_module_of_function(Fun1),
    [Mod2] = get_module_of_function(Fun2),
    case ({_,SType,SId} = Mod1) /= ({_,TType,TId} = Mod2) of
        true -> add_edge(SType,SId,TType,TId,modcall);
        _ -> ok
    end.

%% @spec create_func_vertex_name(Fun::tuple()) -> string()
%% @doc Create name for a function vertex
create_func_vertex_name(Fun)->
    Name = get_func_name(Fun),
    Arity = get_func_arity(Fun),
    list_to_atom(lists:concat([Name, "/", Arity])).

%%% ============================================================================
%%% Draw graph
%%% ============================================================================

%% @spec draw(PropList::proplist()) -> list() | no_return()
%% @doc Get the parameters from the given proplist and call the rigth 
%% drawing function
draw(PropList) when is_list(PropList)->
    prepare_drawing(PropList);
draw(_)->
    throw(?LocalError(error_par, [])).

%% @spec prepare_drawing(PropList::list())-> list()
%% @doc Prepare graph drawing (cache)
prepare_drawing(PropList)->
    case check_db_change() of
        true -> delete_dep_files(),
                start_drawing(PropList);
        _ ->    case is_exist_query(get_props(PropList)) of
                    {true, FileName} -> 
                        case filelib:is_file(
                                filename:join(
                                    [check_data_dir(),FileName])) of 
                            true -> create_output(PropList,FileName);
                            _ ->    delete_from_table(FileName),
                                    start_drawing(PropList)
                        end;
                    _ -> start_drawing(PropList)
                end
    end.

%% @spec create_output(PropList::list(),FileName::string())-> DepGraph::tuple()
%% DepGraph = {FileName::string(), Type::atom()}
%% @doc Create the right output from a dependency graph
create_output(PropList,FileName)->
    [Level,_,_,_,_,_,_,Output,_,DotParam,FilePath,_] = get_props(PropList),
    case Level of
        mb ->   new_mb_graph(),
                read_mb_graph(filename:join([check_data_dir(),FileName]));
        _ -> new_graph(),
                read_graph(filename:join([check_data_dir(),FileName]))
    end,
    graph_output(Level,Output,DotParam,FilePath).
    
%% @spec start_drawing(PropList::list()) -> FilteredGraph::list() | no_return()
%% @doc Start drawing the dependency graph on the chosen level
start_drawing(PropList) ->
    [Level,Type,Otp,Ex,ExCh,StN,ExLib,Out,Groups,DotPars,FilePath,Con] 
                                                          = get_props(PropList),
    CFN = add_new_save(Level,Type,Otp,Ex,ExCh,StN,ExLib,Groups,Con),
    case Level of
        mb -> 
            draw(mb,Groups,Out,DotPars,FilePath,Type,CFN);
        func -> 
            draw(func,Type,Otp,Ex,ExCh,StN,ExLib,Out,DotPars,FilePath,CFN,Con);
        mod -> 
            draw(module,Type,Otp,Ex,ExCh,StN,ExLib,Out,DotPars,FilePath,CFN,Con);
        [] ->
            throw(?LocalError(error_par, {level, nolevel}));
        _ -> 
            throw(?LocalError(error_par, {level, Level}))
    end.

%% @spec draw(Level::atom(), Type::atom() , Otp::atom(), 
%%            Exclude::list(), ExChildren::list(), StNodes::list(), 
%%            ExLib::list(), Output::string(), DotParam::string(), 
%%            FilePath::string(), CacheFN::string(), Conn::list()) 
%%                                                      -> FilteredGraph::list()
%% @doc Build or read the graph, then filter it and write to dot or txt file
draw(Level,Type,Otp,Exclude,ExChildren,StNodes,ExLib,
                                        Output,DotParam,FilePath,CacheFN,Conn)->
    {T, M, Co} = case Conn of
			{direct, C } -> {0,0,C};
			{direct, Md, C } when Md > 1 -> {0,Md,C};
			{indirect, C } -> {1,0,C};
			[] -> {1,0,[]};
			L when is_list(L) -> {1,0,L};
			_ -> throw(?LocalError(error_par, {connection, Conn}))
		end,
    {Cyclic,ExcOtp,ExNodes,ExCh,ExLibr,StN,Con} = 
        filter_graph_pars(Type, Otp, Exclude, ExChildren, StNodes, ExLib, Co),
	check_graph(),
    filter_graph(Level,Cyclic,ExcOtp,ExNodes,ExCh,ExLibr,StN,[],Con,T,M),
    write_graph(check_data_dir() ++ CacheFN), 
    graph_output(Level,Output,DotParam,FilePath).

%% @spec filter_graph_pars(Type::atom(),Otp::atom(),Exclude::list(),
%%          ExChildren::list(),StNodes::list(),ExLib::list(),Conn::list()) ->
%%               {Cyclic::aton(),ExcOtp::list(),ExNodes::list(),
%%                      ExCh::list(),ExLibr::list(),StN::list(), Con::list()}
%% @doc Compute filter_graph/9 function's parameters
filter_graph_pars(Type,Otp,Exclude,ExChildren,StNodes,ExLib,Conn)->
    case Otp of
        true -> 
                ExcOtpSet = sets:from_list(list_of_all_otp_mod()),
                ModsSet = sets:from_list(get_all_modules_name()),
                ExcOtp = sets:to_list(sets:intersection(ExcOtpSet,ModsSet));
        _ ->    ExcOtp = []
    end,
    Cyclic = make_type_par(Type),
    ExNodes = make_param_list(Exclude, exclude),
    ExCh = make_param_list(ExChildren, exclude_children),
    StN = make_param_list(StNodes, starting_nodes),
    Con = make_param_list(Conn, connection),
    ExLibr = make_exclude_lib_list(ExLib),
	case {StN,Con} of
		{[],[]} -> true;
		{[],_} -> true;
		{_,[]} -> true;
		{_,_} -> throw(?LocalError(error_stn_con, []))
	end,
    {Cyclic,ExcOtp,ExNodes,ExCh,ExLibr,StN,Con}.

%% @spec draw(Level::atom(), Groups::list(), RegExp::string(), Output::string(), 
%%            DotParam::string(), Type::string(), CacheFN::string())
%%                                                    -> ModulBlockGraph::list()
%% @doc Build modulblock graph, then write it to dot or txt file        
draw(mb, Groups, Output, DotParam, FilePath, Type, CacheFN)->
    delete_mbgraph(),
    GroupsPar = groups_parameters(Groups),
    Cyclic = make_type_par(Type),
	check_graph(),
    filter_graph(mb,Cyclic,[],[],[],[],[],GroupsPar,[],2,0),
    write_mb_graph(check_data_dir() ++ CacheFN),
    graph_output(mb,Output,DotParam,FilePath).

%% @spec groups_parameters(Groups::list) -> GroupsPars::list()
%% @doc Compute filter_graph/9 function's groups parameter
groups_parameters(Groups) when is_list(Groups)->
    {MBLibs,MBList}=lists:partition(fun(A)->filelib:is_dir(A) end,Groups),   
    MBLibsElements = [scan_library_no_lib(E) || E <- MBLibs],
    MBNames = try
				lists:map( 
					fun(List) when is_list(List) ->
						case io_lib:printable_unicode_list(List) of
							 false ->
								lists:flatten([ 
								  case Mod of
									{'$gn',module,_}    -> get_module_name(Mod);
									M when is_atom(M)   ->  get_module_node(M),
															M;
									{M} when is_atom(M) ->  get_module_node(M),
															M;
									M when is_list(M)	-> 
										[ list_to_atom(E) || 
											E <- regexp_matches(M,
													get_all_mod_name_str())];
									_ -> 
										throw({not_existing_node, convert(Mod)})
								  end        
										|| Mod <- List]);
							true ->
								lists:flatten([ list_to_atom(E) 
									|| E <- regexp_matches(List,
											get_all_mod_name_str())])
								end;
					(_) ->
						throw(?LocalError(error_par, {groups, Groups}))
						end, 
                    MBList)
                catch
					throw:{not_existing_node, Node} ->
						throw(?LocalError(error_node, Node));
					throw:{regexp_error, RegExp} ->
						throw(?LocalError(error_regexp, RegExp));
					_:_ -> throw(?LocalError(error_par, {groups, Groups}))
				end,
    MBLibsElements ++ MBNames;
groups_parameters(Groups)->
	throw(?LocalError(error_par, {groups, Groups})).

%% @spec graph_output(LevelPar::atom(), Output::string(), 
%%                    DotParam::string(), FilePath::string())
%%                      -> Result::tuple()
%% Result = {FileName::string(), Type::atom()}
%% @doc Create the graph output
graph_output(LevelPar,Output,DotParam,FilePath)->
  try  
    FileName =  case FilePath of
                    []  -> case Output of
								node_terms -> check_data_dir() ++ "dep_graph.txt";
								terms -> check_data_dir() ++ "dep_graph.txt";
								name_terms -> check_data_dir() ++ "dep_graph.txt";
								_ -> check_data_dir() ++ "dep_graph.dot"
						   end;
                    _   -> FilePath
                end,
    Level = case LevelPar of
                    function -> func;
                    mod -> module;
                    _ -> LevelPar
            end,
    case {Level,Output} of
        {_, complex_dot}     -> 
                                Type = dot,
                                DotPar = make_dot_par(DotParam),
                                graph_to_dot(Level,FileName, DotPar, complex);
        {func,nomods_dot}    -> 
                                Type = dot,
                                DotPar = make_dot_par(DotParam),
                                graph_to_dot(Level, FileName, DotPar, nomods);
		{_, nomods_dot}     ->
								Type = dot,
								throw(nomods_error);
        {_, node_terms}      -> 
                                Type = term,
                                graph_to_terms(Level,FileName);
        {_, terms}           ->
                                Type = term,
                                graph_to_terms_two(Level,FileName);
		{_, name_terms}      ->
								Type = term,
								graph_to_terms_name(Level,FileName);
        {_, simple_dot}      -> 
                                Type = dot,
                                DotPar = make_dot_par(DotParam),
                                graph_to_dot(Level, FileName, DotPar, simple);
        _                    ->
                                Type = dot,
								DotPar = make_dot_par(DotParam),
                                graph_to_dot(Level, FileName, DotPar, simple)
    end,
    case {Level,Type} of
        {_, term} -> read_file(FileName);
        {_, dot}  -> FileName ++ " file had been created"
    end
  catch
	throw:dotpar -> throw(?LocalError(error_par, {dotpars, DotParam}));
	throw:nomods_error -> throw(?LocalError(error_nomods, []));
    _:_ -> throw(?LocalError(error_output, []))
  end.
    
%%% ============================================================================
%%% Drawing graph's helper functions
%%% ============================================================================

%% @spec get_props(PropList::proplist()) -> list()
%% @doc Get the parameters from the given proplist
get_props(PropList)->
    Level = verify_props(proplists:get_value(level,PropList)),
    case Level of
        mod     -> ok;
        func    -> ok;
        mb      -> ok;
        []      -> throw(?LocalError(error_par, {level, nolevel}));
        _       -> throw(?LocalError(error_par, {level, Level}))
    end,
    Type = verify_props(proplists:get_value(type,PropList)),
    Otp = verify_props(proplists:get_value(exclude_otp,PropList)),
    Exclude = verify_props(proplists:get_value(exclude,PropList)),
    ExChildren = verify_props(proplists:get_value(exclude_children,PropList)),
    StNodes = verify_props(proplists:get_value(starting_nodes,PropList)),
    ExLib = verify_props(proplists:get_value(exclude_lib,PropList)),
    Output = verify_props(proplists:get_value(output,PropList)),
    Groups = verify_props(proplists:get_value(groups,PropList)),
    DotPars = verify_props(proplists:get_value(dotpars,PropList)),
    FilePath = verify_props(proplists:get_value(file_path,PropList)),
    Conn = verify_props(proplists:get_value(connection,PropList)),
    Keys = proplists:get_keys(PropList),
    [is_exist_key(K) || K <- Keys],
    [Level,Type,Otp,Exclude,ExChildren,StNodes,
                ExLib,Output,Groups,DotPars,FilePath,Conn].

%% @spec verify_props(Param) -> [] | Param
%% Param = atom() | list() 
%% @doc If the proplist item is undefined it gives back an empty list, 
%% else it gives back the term.
verify_props(undefined)->
    [];
verify_props(Param)->
    Param.

%% @spec is_exist_key(Key::atom()) -> ok | error
%% @doc Decides from a key that is valid or not
is_exist_key(Key)->
    case Key of
        level -> ok;
        type -> ok;
        exclude_otp ->ok;
        exclude -> ok;
        exclude_children -> ok;
        starting_nodes -> ok;
        exclude_lib -> ok;
        output -> ok;
        groups -> ok;
        dotpars -> ok;
        file_path -> ok;
        connection -> ok;
        _ -> throw(?LocalError(error_par, atom_to_list(Key)))
    end.

%% @spec make_type_par(Type::atom()) -> true | false
%% @doc Convert a graph type parameter to boolean
make_type_par(Type)->
    case Type of
        cyclic -> true;
		cycles -> true;
        all -> false;
		[] -> false;
        _ -> throw(?LocalError(error_par, {type, Type}))
    end.

%% @spec make_dot_par(List::proplist()) -> string()
%% @doc Make a dot parameter from proplist
make_dot_par(List) when is_list(List)->
    try
        StringList = [ atom_to_list(T) ++ "=" ++ V ++ "; " 
                                                            || {T,V} <- List],
        param_string(StringList,"")
    catch
        _:_ -> throw(dotpar)
    end;
make_dot_par(_)->
	throw(dotpar).

%% @spec param_string(List::list(),Acc::string()) -> string()
%% @doc Make one string from a list of string
param_string([],Acc)->
    Acc;
param_string([H | T],Acc)->
    param_string(T,Acc ++ H).
   
%% @spec check_graph() -> ok
%% @doc Check that the database changed. If it changed, then it build a new
%% dependency graph, else read the graph from file   
check_graph()->
  try
    case global:registered_names() of
        [] ->
            global:register_name(depgraph,self()),
            new_graph(),
            case check_db_change() of
                true    -> build_graph();
                _       ->
                    case is_exist_graph() of
                        true -> read_graph(check_data_dir() ++ "dep_graph.lgf");
                        _    -> build_graph()
                    end 
            end,
            global:unregister_name(depgraph),
            ok;
        _  -> throw(running)
    end
    catch
        running ->  
                timer:sleep(1000),
                check_graph()
    end.

%% @spec make_param_list(List::list(),Option::atom()) -> list()
%% @doc Make a proper parameter from the list that it got
make_param_list(List, _) when is_list(List)->
    lists:flatten([ 
        try 
            case Elem of
                {Mod} when is_atom(Mod) -> get_module_node(Mod);
                {'$gn',Type,Id}         -> [Type, Id]; 
                {_,_,_}                 -> get_function_node(Elem);
                M when is_atom(M)      -> get_node_from_string(atom_to_list(M));
                S when is_list(S)      -> get_regexp_matches(S);
                _ -> []
            end
        catch
			throw:{not_existing_node, Node} ->
				throw(?LocalError(error_node, Node));
			throw:{regexp_error, RegExp} -> 
				throw(?LocalError(error_regexp, RegExp));
			_:_ ->
				throw(?LocalError(error_regexp, List))

        end
    || Elem <- List]);
make_param_list(List, Option)->
    throw(?LocalError(error_par, {Option, List})).

%% @spec convert(Elem) -> list()
%% @doc Convert the parameter input into a string list
convert(Elem)->
    case is_list(Elem) of
        true -> Elem;
        _ -> lists:flatten(io_lib:format("~p",[Elem]))
    end.

%% @spec get_node_from_string(S::string()) -> graph_node()
%% @doc Make a graph node from string
get_node_from_string(S)->
    case string:tokens(S,":/") of
        [_] -> 
                    
                    get_module_node(list_to_atom(S));
        [Mo,F,A] ->
                    get_function_node({list_to_atom(Mo),
                    list_to_atom(F),list_to_integer(A)});
        _ ->
                 throw({not_existing_node, S})   
    end.
    
%% @spec make_exclude_lib_list(List::list()) -> list()
%% @doc Do exclude libraries option
make_exclude_lib_list(List) when is_list(List)->
    {Paths,Regexps} = lists:partition(fun(B)->filelib:is_dir(B) end,List),
    RegexpMatches = try
		[regexp_matches(R,get_all_file_path()) || R <- Regexps]
	catch
		throw:{regexp_error, RegExp} -> throw(?LocalError(error_regexp, RegExp))
	end,
    lists:flatten(
        lists:map(fun(A) -> [scan_library(R) || R <- A] end, RegexpMatches) 
		++ [scan_library(E) || E <- Paths]);
make_exclude_lib_list(Parameter)->
	throw(?LocalError(error_par, {exclude_lib, Parameter})).
    
%% @spec list_of_all_otp_mod() -> list()
%% @doc Create a list of all otp modules' names
list_of_all_otp_mod() ->
    scan_library(code:lib_dir()).

%% @spec scan_library(LibraryPath::string()) -> list() | Error
%% @doc Look over a libary, collect folders and erlang modules
scan_library(LibraryPath) ->
    try
        case filelib:is_dir(LibraryPath) of
            true -> ListFolder = element(2,file:list_dir(LibraryPath));
            _ -> case filelib:is_file(LibraryPath) of
                    true ->  ListFolder = [LibraryPath];
                    _ ->     ListFolder = []
                end
        end,
        ErlFilesList = [erlang:list_to_atom(filename:basename(E,".erl")) 
            || E <- ListFolder, filename:extension(E) == ".erl"],            
        LibrariesList = [E || E <- ListFolder, 
            filelib:is_dir(filename:join([LibraryPath, E]))],
        ErlFilesList ++ 
            lists:append(lists:map(fun(E) ->
                                   scan_library(filename:join([LibraryPath, E])) 
                                   end, LibrariesList))
    catch
        _:_ ->
            throw(?LocalError(error_load_module, []))
    end.

%% @spec scan_library_no_lib(LibraryPath::string()) -> list() | Error
%% @doc Look over a libary, collect only erlang modules
scan_library_no_lib(LibraryPath) ->
    try
        case filelib:is_dir(LibraryPath) of
            true -> ListFolder = element(2,file:list_dir(LibraryPath));
            _ -> case filelib:is_file(LibraryPath) of
                    true ->  ListFolder = [LibraryPath];
                    _ ->     ListFolder = []
                end
        end,
        [erlang:list_to_atom(filename:basename(E,".erl")) 
            || E <- ListFolder, filename:extension(E) == ".erl"]
    catch
        _:_ ->
            throw(?LocalError(error_load_module, []))
    end.
    
%%% ============================================================================
%%% Reguler expression functions
%%% ============================================================================

%% @spec get_regexp_matches(RegExp::string()) -> list()
%% @doc Get the nodes that matches regular expression
get_regexp_matches(RegExp)->
    case string:tokens(RegExp,":/") of
        [_] -> 
                [get_node_from_string(R)
                    || R <- regexp_matches(RegExp,get_all_mod_name_str())];
        [M,F,A] ->  regexp_match_func({M,F,A});
        [] ->       regexp_match_func({"","",""});
        _ ->
                 throw({regexp_error, RegExp})   
    end.

%% @spec regexp_match_func({Mod::string(),Func::string,Ar::string()}) -> list()
%% @doc Find regular expression matches is function level
regexp_match_func({M,F,A}) when is_list(M) and is_list(F) and is_list(A)->
    AL = fun(X) -> atom_to_list(X) end,
    IL = fun(X) -> integer_to_list(X) end,
    ModList = [ E || E <- get_all_modules(),is_match(M,AL(get_module_name(E)))],
    FuncList = lists:flatten([get_functions_of_module(Mo) || Mo <- ModList]),
    MatchFuns = [ E || E <- FuncList, is_match(F,AL(get_func_name(E)))],
    [ [Fu,Id] || E = {_,Fu,Id} <- MatchFuns, is_match(A,IL(get_func_arity(E)))];
regexp_match_func(_)->
    throw({regexp_error, []}).	

%% @spec is_match(RegExp::string(),Elem::string()) -> true | false
%% @doc Decide that that the given element matches the regular expression or not
is_match("",_) ->
    true;
is_match(Regexp,Elem) when is_list(Regexp) and is_list(Elem) ->
    case re:run(Elem,Regexp) of
        {match,[{0, A}]} -> case length(Elem) of
								A -> true;
								_ -> false
							end;
        {match, _} -> false;
        nomatch -> false;
        _ -> throw({regexp_error, Regexp})
    end;
is_match(_,_) ->
    false.

%% @spec regexp_matches(Regexp::string(), Elements::list()) -> list() | error
%% @doc Find the matches of the regular expression
regexp_matches(Regexp, Elements) when is_list(Regexp) and is_list(Elements) ->
    try
        Result = [ {Elem, re:run(Elem,Regexp)} || Elem <- Elements],
        [E || {E , {match,[{0, A}]}} <- Result, A =:= length(E)]
    catch
        _:_ -> throw({regexp_error, Regexp})
    end;
regexp_matches(Regexp,_) ->
    throw({regexp_error, Regexp}).

%%% ============================================================================
%%% Check files
%%% ============================================================================

%% @spec check_data_dir() -> Path::string() | Error
%% @doc Check the operating system and gives back the rigth format of the path 
%% of dependency graph's files
check_data_dir()->
    case filelib:ensure_dir(Path = 
                filename:join([?MISC:data_dir(), 
                               "dep_files"++?MISC:graph_based_postfix()]) 
                                ++ "/") of
        ok ->
            Path;
        {error, Reason}->
            Err = "Error with the dep_files directory. Reason: " 
                    ++ file:format_error(Reason),
            throw(?LocalError(error, Err))
    end.

%% @spec check_db_change() -> true | false | no_return()
%% @doc Check that the database has been changed or not
check_db_change()->
    Table = case dets:open_file(?DBHASHTABLE) of
        {ok, Ref}  -> Ref;
        _ -> 
            check_data_dir(),
            dets:open_file(db_hash,[{type,bag},{file,?DBHASHTABLE}]),
            db_hash
    end,
    try  
        Hash = ?MISC:database_hash(),
        case dets:match(Table, Hash) of
            [] ->   dets:insert(Table, Hash),
                    true;
            _  ->   false
        end
    catch
        {error, Error} ->
            Err = "Error writing db_hash, reason:" ++ file:format_error(Error),
            throw(?LocalError(error, Err))
    after
        dets:close(Table)
    end.
        
%% @spec read_file(Fname::string()) -> list()
%% @doc Read erlang terms from file
read_file(Fname) ->
    case file:consult(Fname) of
        {ok, [Data]} -> Data;
        _ -> []
    end.

%% @spec is_exist_graph() -> true| false
%% @doc Check that the file which contains the graph is exist or not
is_exist_graph()->
    File = check_data_dir() ++ "dep_graph.lgf",
    filelib:is_file(File).
    
%% @spec delete_dep_files() -> ok
%% @doc Delete dependency's data
delete_dep_files()->
    Path = check_data_dir(),
    {ok, Files} = file:list_dir(Path),
    FileList = lists:map(fun(File) -> filename:join([Path,File]) end, Files),
    [file:delete(F) || F <- FileList],
    ok.

%%% ============================================================================
%%% saved_queries table's functions
%%% ============================================================================
    
%% @spec open_sq_table() -> Table::atom()
%% @doc Open a dets table
open_sq_table()->
    case dets:open_file(?SQTABLE) of
        {ok, Ref}  -> Ref;
        {error, _} -> dets:open_file(saved_queries,[{type,bag},{file,?SQTABLE}]),
                        saved_queries
    end.

%% @spec add_new_save(Level::atom(), Type::atom(), 
%%                    Otp::atom(), Ex::list(), ExCh::list(), StN::list(), 
%%                    ExLib::list(), Groups::list(), Conn::list()) -> string()
%% @doc Add a new query to 'saved_queries' dets table   
add_new_save(Level, Type, Otp, Ex, ExCh, StN, ExLib, Groups, Conn)->
    Table = open_sq_table(),
    try
        {_, N} = lists:nth(3, dets:info(Table)),
        Filename = "dep_graph_" ++ atom_to_list(Level) 
                    ++ integer_to_list(N+1) ++ ".lgf",
        dets:insert(Table, #dep_query{number = N+1,
                         level = Level,
                         type = Type,
                         otp = Otp,
                         exclude = Ex,
                         exclude_children = ExCh,
                         starting_nodes = StN,
                         exclude_lib = ExLib,
                         groups = Groups,
                         connection = Conn,
                         file = Filename}),
        Filename
    catch
        _:_ -> throw(?LocalError(error_write, []))
    after
        dets:close(Table)
    end.

%% @spec is_exist_query(ParamList::list())-> true | false
%% @doc Decide that a query exist or not
is_exist_query(ParamList)->
    Table = open_sq_table(),
    try
        [Level,Type,Otp,Ex,ExCh,StN,ExLib,_,Groups,_,_,Conn] = ParamList, 
        case dets:match(Table, #dep_query{number = '_',
                                     level = Level,
                                     type = Type,
                                     otp = Otp,
                                     exclude = Ex,
                                     exclude_children = ExCh,
                                     starting_nodes = StN,
                                     exclude_lib = ExLib,
                                     groups = Groups,
                                     connection = Conn,
                                     file = '$1'}) of
            [] -> false;
            [File | _] -> {true,File}
        end
    catch
        _:_ -> throw(?LocalError(error_read, []))
    after
        dets:close(Table)
    end.

%% @spec delete_from_table(FileName::string()) -> ok    
%% @doc Delete a query from 'saved_queries' dets table  
delete_from_table(FileName)->
    Table = open_sq_table(),
    try
        dets:match_delete(Table, #dep_query{number = '_',
                                     level = '_',
                                     type = '_',
                                     otp = '_',
                                     exclude = '_',
                                     exclude_children = '_',
                                     starting_nodes = '_',
                                     exclude_lib = '_',
                                     groups = '_',
                                     file = FileName})
    catch
        _:_ -> throw(?LocalError(error_write, []))
    after
        dets:close(Table)
    end.
