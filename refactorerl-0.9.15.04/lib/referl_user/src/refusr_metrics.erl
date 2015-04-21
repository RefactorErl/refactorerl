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

%%% @author Roland Kiraly <kiralyroland@inf.elte.hu>
%%% @author Judit Koszegi <kojqaai.inf.elte.hu>

%%% @doc <b>Metric Query Language</b>
%%%
%%% The query language is relatively simple and easy to expand
%%% both regarding syntax and the list of metrics that can be
%%% implemented in this module.
%%%
%%% Usage in command line:
%%%
%%% Metric Query::show metric_function for nodetype nodelist
%%%                                                    aggregation
%%%
%%% metric functions::
%%%        line_of_code | char_of_code | number_of_fun
%%%        |number_of_macros | number_of_records
%%%        |included_files | imported_modules
%%%        |number_of_funpath | function_calls_in
%%%        |function_calls_out | cohesion,
%%%        |otp_used | function_sum | max_application_depth
%%%        |max_depth_of_calling | min_depth_of_calling
%%%        |max_depth_of_cases | max_depth_of_structs
%%%        | number_of_funclauses |branches_of_recursion
%%%        | mcCabe |calls_for_function
%%%        | calls_from_function |number_of_funexpr
%%%        | number_of_messpass |fun_return_points
%%%        | average_size
%%%
%%% nodetype::module | function
%%%
%%% nodelist::('modname',...)
%%%           |({'modname','funname',arity},...)
%%%
%%% aggregation::
%%%          sum|min|max|fmaxname|avg|tolist|maxlist
%%%
%%%
%%% Implemented metrics:
%%%
%%% <b>Effective Line of code</b> The number of the lines of part
%%%  of the text, function, or module. The number of empty lines is not
%%%  included in the sum. As the number of lines can be measured on more
%%%  functions, or modules and the system is capable of returning the
%%%  sum of these, the number of lines of the whole loaded program text
%%%  can be enquired.
%%%
%%% <b>Characters of the code</b> The number of characters in a program
%%%  text. This metric is capable of measuring both the codes of functions
%%%  and modules and with the help of aggregating functions we can enquire
%%%  the total and average number of characters in a cluster, or in the
%%%  whole source text.
%%%
%%% <b>Number of functions</b> This metric gives the number of functions
%%%  implemented in the concrete module, but it does not contain the number
%%%  of non-defined functions in the module.
%%%
%%% <b>Number of macros</b> This metric gives the number of defined macros
%%%  in the concrete module, or modules. It is also possible to enquire the
%%%  number of implemented macros in a module.
%%%
%%% <b>Number of records</b> This metric gives the number of defined
%%%  records in a module. It is also possible to enquire the number of
%%%  implemented records in a module.
%%%
%%% <b>Number of included files</b> This metric gives the number of visible
%%%  header files in a module.
%%%
%%% <b>Imported modules</b> This metric gives the number of imported modules
%%%  used in a concrete module. The metric does not contain the number of
%%%  qualified calls (calls that have the following form:
%%%  `module:function'.
%%%
%%% <b>Number of funpath</b> The total number of function paths in a module.
%%%  The metric, besides the number of internal function links, also
%%%  contains the number of external paths, or the number of paths that
%%%  lead outward from the module.
%%%
%%% <b>Function calls into the module</b> Gives the number of function calls
%%%  into a module from other modules. It can not be implemented to measure
%%%  a concrete function. For that we use the `calls_for' function.
%%%
%%% <b>Function calls from the module</b> Gives the number of every function
%%%  call from a module towards other modules. It can not be implemented to
%%%  measure a concrete function. For that we use the `calls_from' function.
%%%
%%% <b>Cohesion of the module</b> The number of call-paths of functions
%%%  that call each other.
%%%
%%% <b>Function sum</b> The sum calculated from the functions complexity
%%%  metrics that characterizes the complexity of the function. It can
%%%  be calculated using various metrics together.
%%%  We can define metrics that are necessary to calculate the metrics
%%%  constituting the sum.
%%%
%%% <b>OTP used</b> Gives the number of OTP callback modules used in modules.
%%%  We are planning to expand the function, besides measuring the number
%%%  of modules, to also measure the number of implemented callback functions.
%%%
%%% <b>Max depth of calling</b> The length of function call-paths, namely
%%%  the path with the maximum depth. It gives the depth of non-recursive
%%%  calls.
%%%
%%%  <b>Depth of applications</b> Maximum depth of embedded function
%%%  applications in the module `Mod' or the function `Fun'.
%%%
%%% <b>Min depth of calling</b> The minimum depth of functions embedding
%%%  within a module. It gives the depth of non-recursive calls.
%%%
%%% <b>Min depth of cases</b> Gives the minimum of `case' control
%%%  structures embedded in case of a given function. In case of a
%%%  module it measures the same regarding all of the functions in
%%%  the module.
%%%
%%% <b>Max depth of cases</b> Gives the maximum of case control structures
%%%  embedded in `case' of a concrete function (how deeply are the case
%%%  control structures embedded). In case of a module it measures the
%%%  same regarding all the functions in the module. Measuring does not
%%%  break in case of `case-expressions', namely when the `case' is not
%%%  embedded into a `case' structure.
%%%
%%% <b>Max depth of structs</b> Gives the maximum of structures
%%%  embedded in a function (how deeply are the block, case, fun, if,
%%%  receive, try structures embedded)
%%%
%%% <b>Number of funclauses</b> Gives the number of a functions clauses.
%%%  Counts all distinct branches, but does not add the functions
%%%  having the same name, but different arity, to the sum.
%%%
%%% <b>Branches of recursion</b> Gives the number of a certain function's
%%%  branches, how many times a function calls itself, and not the number
%%%  of clauses it has besides definition.
%%%
%%% <b>McCabe</b> McCabe cyclomatic complexity metric. We define the
%%%  complexity metric in a control flow graph with the number of defined
%%%  basic edges, namely the number of outputs a function can have
%%%  disregarding the number of function outputs functions within the
%%%  function can have. Functions called each count as one possible output.
%%%
%%% <b>Calls for the function</b> This metric gives the number of calls
%%%  for a concrete function. It is not equivalent with the number of
%%%  other functions calling the function, because all of these other
%%%  functions can refer to the measured one more than once.
%%%
%%% <b>Calls from the function</b> This metric gives the number of calls
%%%  from a certain function, namely how many times does a function refer
%%%  to another one (the result includes recursive calls as well).
%%%
%%% <b>Number of funexpr</b> Gives the number of function expressions
%%%  in a given module. It does not measure the call of function expressions,
%%%  only their initiation.
%%%
%%% <b>Number of message passing</b> In case of functions it measures the
%%%  number of code snippets implementing messages from a function, while
%%%  in case of modules it measures the total number of messages in all of
%%%  the modules functions.
%%%
%%% <b>Average size</b> The average value of the given complexity metrics
%%% (e.g. Average `branches_of_recursion' calculated from the functions of
%%% the given module).
%%%
%%% <b>Function return points</b> The metric gives the number of the functions
%%%  possible return points or the functions of the given module.
%%%
%%% <b>Max length of line</b> Gives the length of the longest line of the given
%%%    module or function.
%%%
%%% <b>Average length of line</b> Gives the average length of the lines within
%%%    the given module or function.
%%%
%%% <b>No space after comma</b> Gives the number of cases when there are not any
%%%    whitespaces after a comma or a semicolon in the given module's or
%%%    function's text.
%%%
%%% <b>Is tail recursive</b> This metric returns with 1, if the given function
%%%    is tail recursive; with 0, if it is recursive, but not tail recursive;
%%%    and -1 if it is not a recursive function (direct and indirect recursions
%%%    are also examined).
%%%
%%% <b>Aggregation filters:</b>
%%%
%%% max      : maximum on the result list
%%%
%%% tolist   : default return value of the query
%%%
%%% fmaxname : maximum with the name of the node
%%%
%%% maxlist  : all the elements where the value equals to maximum
%%%
%%% avg      : average on the result list
%%%
%%% min      : minimum of the result list
%%%
%%% sum      : sum of the result list
%%%
%%% <b>Usage in the Emacs interface:</b>
%%%
%%% M-x refactorerl-metrics-query
%%%
%%% show max_depth_of_cases for function ({'a','f',1},{'a','f',2})
%%%
%%% show number_of_funpath for module ('a','b')
%%%
%%% show number_of_fun for module ('a','b') sum
%%%
%%% Helper functions:
%%%
%%% show metrics|filters

-module(refusr_metrics).
-vsn("$Rev: 13017 $ ").

-export([prepare/1]).

% @todo unexport
-export([cmd/1, preQuery/1, metric/1, mtr/1]).

% @todo unexport
-export([fun_return_points/1]). %% DEPRICATED. Use this function instead:
                                %% ?Fun:return_points/1
                                %% //?Query:exec(?Fun:return_points(Fun)).//

-include("user.hrl").

-define(LEXER, refusr_m_lexer).
-define(PARSER, refusr_m_parser).

%%% @private
prepare(Args)->
    fun()->
        [Query] = ?MISC:pgetu([querys],Args),
        Result =
            case cmd(Query) of
                {list, List} ->
                    io_lib:format(
                        "Query: ~s~n"
                        "Result:~n"
                        "~s",
                        [Query,
                            [["   ", createText(E), "\n"] || E <- List]]);
                {helpfun, FNList} ->
                    io_lib:format(
                        "Query: ~s~n"
                        "Result:~n"
                        "~s",
                        [Query,
                            [["   ", createText({fn, FN}), "\n"] || FN <- FNList]]);
                {Agr, Data} ->
                    io_lib:format(
                        "Query: ~s~n"
                        "Result:~n"
                        "~s",
                        [Query,
                            ["   ", createText({Agr, Data})]])%% ;
                %% Se ->
                %%     io_lib:format("~s", [SE])
            end,
        lists:flatten(Result)
    end.


%%% @private
numformat({Data}) when is_list(Data) ->
   concat("", Data);
numformat({Num, {Module, Function, Arity}}) ->
    "("++Module++":"++Function++"/"++integer_to_list(Arity)
    ++" with "++integer_to_list(Num)++")";
numformat({Num, Name}) ->
    "("++Name++" with "++numformat(Num)++")";
numformat(Num) when is_integer(Num) ->
    integer_to_list(Num);
numformat(Num) when is_float(Num) ->
    float_to_list(Num).

%%% @private
concat(Acc, [{N, {Mod, Fun, Arity}}|Ls])->
    concat(Acc++" {"++Mod++":"++Fun
              ++"/"++integer_to_list(Arity)++","
              ++integer_to_list(N)++"}", Ls);
concat(Acc, [{N, Name}|Ls])->
    concat(Acc++" ("++Name++" "
    ++integer_to_list(N)++")", Ls);
concat(Acc, []) ->
    lists:flatten(Acc).

%%% @private
createText({fn, FNl})->
    atom_to_list(FNl);
createText({ResultList})
          when is_list(ResultList)->
    "Maximum elements of the result: "++numformat({ResultList});
createText({Agr, Data})->
    atom_to_list(Agr)++" is "++numformat(Data);
createText({{Module, Function, Arity}, _, nonode})->
    Module++":"++Function++"/"++integer_to_list(Arity)
    ++" is not found";
createText({{Module, Function, Arity}, Me, NumON})->
    atom_to_list(Me)++" of the '"++Module++":"++Function++"/"
    ++integer_to_list(Arity)++"' is "++numformat(NumON);
createText({Name, _, nonode})->
    "'"++Name++"' is not found";
createText({Name, Me, NumON})->
    atom_to_list(Me)++" of the '"++Name
    ++"' is "++numformat(NumON);
createText(A) ->
  A.

%%% @spec cmd(Query::string()) -> string()
%%% @doc Interface function of the command line interface.
cmd(Query) ->
     case ?LEXER:string(Query) of
          {ok, Tokens, _EndL} ->
          case ?PARSER:parse(Tokens) of
               {ok, PrT} ->  preQuery(PrT);
               {error, Err} -> format_error({parser_error, Err})
          end;
          {error, Err, _LNum} -> format_error({scanner_error, Err})
     end.

format_error({parser_error, {_,_, Mesg}})->
    throw(?RefError(m_parser_error, [Mesg]));
format_error({scanner_error, {_,_, Err}}) ->
    throw(?RefError(m_scanner_error, [Err]));
format_error({bad_command, Command})->
    throw(?RefError(bad_command, [Command]));
format_error({bad_filter, Filter}) ->
    throw(?RefError(bad_filter, [Filter]));
format_error({metric_fun, Metric}) ->
    throw(?RefError(metric_fun, [Metric]));
format_error(Other) ->
    case io_lib:deep_char_list(Other) of
        true -> throw(?RefError(m_unknown_error, [Other]));
           _ -> throw(?RefError(m_unknown_error, [io_lib:write(Other)]))
    end.

%%% @private
%%% Helper
funlist()->
      [%module_sum,
      line_of_code, char_of_code,
      number_of_fun, number_of_macros, number_of_records,
      included_files, imported_modules, number_of_funpath,
      function_calls_in, function_calls_out, cohesion,
      otp_used, function_sum, max_application_depth,
      max_depth_of_calling, min_depth_of_calling,
      max_depth_of_cases, min_depth_of_cases,
      max_depth_of_structs, number_of_funclauses,
      branches_of_recursion, mcCabe, calls_for_function,
      calls_from_function, number_of_funexpr, number_of_messpass,
      fun_return_points, average_size,
      max_length_of_line, average_length_of_line,
      no_space_after_comma, is_tail_recursive].

%%% @private
filterlist()->
    [sum, min, max, fmaxname,
     avg, tolist, maxlist].

%%% @spec preQuery({{atom(), Metric::string(), NodeType::atom(),
%%%                                 Ident::list()},Filter::atom()})
%%%                                               -> ResultList::list()
%%%                                                  | {error, Reason}
%%% @private
preQuery({{show, Metric, NodeType, Ident}, Filter}) ->
    execQuery({Metric, NodeType, Ident, Filter});
preQuery({show, Metric, NodeType, Ident}) ->
    execQuery({Metric, NodeType, Ident, tolist});
%TODO: Handling other types of the messages
preQuery({show, metrics})->
    {helpfun, funlist()};
preQuery({show, filters})->
    {helpfun, filterlist()};
preQuery(Command) ->
    format_error({bad_command, Command}).


%%% @private
execQuery({Metric, NodeType, IdentList, Filter}) ->
    Nodes = find_node(NodeType, IdentList),
    ResultList = [show({Metric, NodeType, Node})|| Node <- Nodes,
                                                       Node /= error],
    agreg(Filter, ResultList).

%%% @private
agreg(tolist, Result)->
    {list, Result};

agreg(max, Result)->
    RList = [Num || {_, _, Num} <- Result, is_integer(Num)],
    {max, lists:max(RList)};

agreg(fmaxname, Result)->
    RList = [{Num, Name}|| {Name, _, Num} <- Result, is_integer(Num)],
    {Name, Max} = maxkey(RList),
    {fmaxname, {Max, Name}};

agreg(min, Result)->
    RList = [Num || {_, _, Num} <- Result, is_integer(Num)],
    {min, lists:min(RList)};

agreg(sum, Result) ->
    RList = [Num || {_, _, Num} <- Result, is_integer(Num)],
    {sum, lists:foldl(fun(X, Sum) -> X + Sum end, 0, RList)};

agreg(avg, Result) ->
    Rd = [R || {_, R, Num} <- Result, Num =/= 0],
    %TODO DB where element of the tuple is not 0.
    RList = [Num || {_, _, Num} <- Result, is_integer(Num), Num =/= 0],
    NData = lists:foldl(fun(X, Sum) -> X + Sum end, 0, RList),
    {avg, NData/length(Rd)};

agreg(maxlist, Result)->
    RList = [{Num, Name}|| {Name, _, Num} <- Result, is_integer(Num)],
    MaxList = maxkeys2(RList),
    {maxlist, MaxList};

agreg(Filter, _Result) ->
    format_error({bad_filter, Filter}).

%%% @private
maxkey(MTuples)->
    {L2, _L1} = lists:unzip(MTuples),
    Key = lists:max(L2),
    {_, Name} = lists:keyfind(Key, 1, MTuples),
    {Name, Key}.

%%% @private
maxkeys2(MTuples)->
    {L2, _L1} = lists:unzip(MTuples),
    Key = lists:max(L2),
    TList = lists:filter(
                fun({K, _})->
                     K == Key
                 end, MTuples),
    {lists:flatten(TList)}.

%%% @private
find_node(function, FunList) ->
    WList = [begin
         [ModNode] = ?Query:exec(?Mod:find(list_to_atom(Mod))),
         {?Query:exec(ModNode, ?Fun:find(list_to_atom(Name),Arity)),
                                          {Mod, Name, Arity}} end
                                          || {Mod, Name, Arity} <- FunList],
    extractList([],WList);
find_node(module, ModList) ->
       WList = [{?Query:exec(?Mod:find(list_to_atom(Mod))), Mod}
                                                           || Mod <- ModList],
       extractList([],WList).

%%% @private
extractList(Acc,[Head|List])->
    case Head of
        {[], Name}     -> Acc1 = Acc++[{result, {nonode, Name}}];
        {[Node], Name} -> Acc1 = Acc++[{result, {Node, Name}}];
        Er     -> Acc1 = Acc++[{error, Er}]
    end,
    extractList(Acc1, List);
extractList(Acc,[]) ->
    Acc.

%% interface for semantic query
metric({Metric, NodeType, Node}) ->
    case show({Metric, NodeType, {result, {Node, x}} }) of
        {x, _Metric, Int} -> Int;
        Err -> throw(?RefError(sq_metric_error, [Err]))
    end.

%%% @doc interface function for refanal_metric
mtr({Mtr, NdType, Nd}) ->
    case show({Mtr, NdType, {result, {Nd, refresh}}}) of
        {refresh, RMtr, Value} -> {RMtr, Value};
        Other                  -> {error, Other}
    end.

%%% @private
%% metric name is enough to run apply
show({Metric, NodeType, Node})->
   Fun = case Metric of
      module_sum            -> fun modsum/1;
      line_of_code          -> fun loc/1;
      char_of_code          -> fun choc/1;
      number_of_fun         -> fun nof/1;
      number_of_macros      -> fun num_of_macros/1;
      number_of_records     -> fun num_of_rec/1;
      included_files        -> fun inc_file/1;
      imported_modules      -> fun imp_mod/1;
      number_of_funpath     -> fun number_of_funpath/1;
      function_calls_in     -> fun fcalls_in/1;
      function_calls_out    -> fun fcalls_out/1;
      cohesion              -> fun cohesion/1;
      otp_used              -> fun otp_used/1;
      function_sum          -> fun fun_inside/1;
      max_application_depth -> fun max_application_depth/1;
      max_depth_of_calling  -> fun max_depth_of_calling/1;
      min_depth_of_calling  -> fun min_depth_of_calling/1;
      max_depth_of_cases    -> fun max_depth_of_cases/1;
      min_depth_of_cases    -> fun min_depth_of_cases/1;
      max_depth_of_structs  -> fun max_depth_of_structs/1;
      number_of_funclauses  -> fun funclauses/1;
      branches_of_recursion -> fun b_recursion/1;
      mcCabe                -> fun mCb/1;
      calls_for_function    -> fun calls_for/1;
      calls_from_function   -> fun calls_from/1;
      number_of_funexpr     -> fun funexp/1;
      number_of_messpass    -> fun messpass/1;
      fun_return_points     -> fun fun_return_points/1;
      average_size          -> fun average_size/1;
      max_length_of_line    -> fun max_length_of_line/1;
      average_length_of_line-> fun avg_length_of_line/1;
      no_space_after_comma  -> fun no_space_after_comma/1;
      is_tail_recursive     -> fun is_tail_recursive/1;
                          _ -> format_error({metric_fun, [Metric]})
   end,
   try
     case Node of
        {result, {nonode, Name}} ->
            {Name, Metric, nonode};
        {result, {RNode, Name}} ->
            Result  = Fun({NodeType, RNode}),
            Handled = result_handler(Result),
            {Name, Metric, Handled};
        {error, Er} ->
            format_error({error, Er})
     end
   catch
     error:Err -> Err
   end.

%%% @private
result_handler(Data) when is_list(Data) ->
    length(Data);
result_handler(true) ->
    true;
result_handler(false) ->
    false;
result_handler(Data) when is_number(Data) ->
    Data.

%%% @private
loc({file, File})->
    string:tokens(lists:flatten(?Syn:tree_text(File)),"\n");
loc({module, Mod})->
    [File] = ?Query:exec(Mod, ?Mod:file()),
    loc({file, File});
loc({function, Fun})->
    [Def] = ?Query:exec(Fun, ?Fun:definition()),
    Text = ?Syn:tree_text(Def),
    string:tokens(lists:flatten(Text),"\n");
loc({clause, Cl})->
    Text = ?Syn:tree_text(Cl),
    string:tokens(lists:flatten(Text),"\n").

%%% @private
choc({module, Mod})->
    [File] = ?Query:exec(Mod, ?Mod:file()),
    erlang:length(lists:flatten(?Syn:tree_text(File)));
choc({function, Fun}) ->
    [Def| _] = ?Query:exec(Fun,?Fun:definition()),
    Text = ?Syn:tree_text(Def),
    string:join(string:tokens(lists:flatten(Text),"\n"),"");
choc({clause, Cl})->
    Text = ?Syn:tree_text(Cl),
    string:join(string:tokens(lists:flatten(Text),"\n"),"").

%%% @private
nof({module, Mod})->
    ?Query:exec(Mod, ?Mod:locals());
nof({Type, _}) when (Type == function) orelse (Type == clause)->
    throw(?RefError(incompat, [number_of_function])).

%%% @private
num_of_macros({module, Mod})->
    [File] = ?Query:exec(Mod, ?Mod:file()),
    ?Query:exec(File, ?File:macros());
num_of_macros({Type, _}) when (Type == function) orelse (Type == clause)->
    throw(?RefError(incompat, [number_of_macros])).

%%% @private
num_of_rec({module, Mod})->
    [File] = ?Query:exec(Mod, ?Mod:file()),
    ?Query:exec(File, ?File:records());
num_of_rec({Type, _}) when (Type == function) orelse (Type == clause)->
    throw(?RefError(incompat, [number_of_records])).

%%% @private

inc_file({module, Mod})->
    [File] = ?Query:exec(Mod, ?Mod:file()),
    ?Query:exec(File, ?File:includes())
    -- [File];
inc_file({Type, _}) when (Type == function) orelse (Type == clause)->
    throw(?RefError(incompat, [included_files])).

%%% @private
imp_mod({module, Mod})->
    [File] = ?Query:exec(Mod, ?Mod:file()),
    Forms = ?Query:exec(File, ?File:forms()),
    IncForms = [IncForm||IncForm <- Forms,
                             ?Form:type(IncForm) == import],
    unic([],[],IncForms);
imp_mod({Type, _}) when (Type == function) orelse (Type == clause)->
    throw(?RefError(incompat, [implemented_modules])).

%%% @private
unic(Acc, AttrList, [H|List])->
     [Attr] = ?Query:exec(H,[{eattr,1}]),
     Value = ?Expr:value(Attr),
     case lists:member(Value, AttrList) of
          true  -> unic(Acc, AttrList, List);
          false -> unic(Acc++[H], AttrList++[Value], List)
     end;
unic(Acc, _, []) ->
    Acc.

%%% @private
%% The result is the number of function path (calls between functions)
%%    from, to and in the module `Mod'.
number_of_funpath({module, Mod})->
     cohesion({module, Mod})++fcalls_in({module, Mod})
     ++fcalls_out({module, Mod});
number_of_funpath({Type, _}) when (Type == function) orelse (Type == clause)->
    throw(?RefError(incompat, [number_of_funpath])).

%% This functions result is the number of the incoming function calls
%%      into the module `Mod'.

%%% @private
fcalls_in({module, Mod})->
    Functs      = ?Query:exec(Mod, ?Mod:locals()),
    FunCalls    = [?Query:exec(Fun, ?Fun:called()) || Fun <- Functs],
    [FunCall|| FunCall <- lists:flatten(FunCalls),
                        ?Query:exec(FunCall, ?Fun:module()) /= [Mod]];

fcalls_in({Type, _}) when (Type == function) orelse (Type == clause)->
    throw(?RefError(incompat, [function_calls_in])).


%% This functions result is the number of the outgoing function calls
%%      from the module `Mod'.
%%% @private
fcalls_out({module, Mod})->
    Functs = ?Query:exec(Mod, ?Mod:locals()),
    RefFuns = [?Query:exec(Fun, ?Fun:funcalls())|| Fun <-Functs],
    Mods = [?Query:exec(RFun, ?Fun:module())|| RFun <- lists:flatten(RefFuns)],
    [ModX || ModX <- Mods, [Mod] /= ModX];
fcalls_out({Type, _}) when (Type == function) orelse (Type == clause)->
    throw(?RefError(incompat, [function_calls_out])).

%%% @private
cohesion({module, Mod})->
    Functs  = ?Query:exec(Mod, ?Mod:locals()),
    RefFuns = [?Query:exec(Fun, ?Fun:funcalls())|| Fun <-Functs],
    Mods = [?Query:exec(RFun, ?Fun:module())|| RFun <- lists:flatten(RefFuns)],
    [ModX || ModX <- Mods, [Mod] == ModX];
cohesion({Type, _}) when (Type == function) orelse (Type == clause)->
    throw(?RefError(incompat, [cohesion])).

%%% @private
funlist(module)->
      [line_of_code, char_of_code,
      number_of_fun, number_of_macros, number_of_records,
      included_files, imported_modules, number_of_funpath,
      function_calls_in, function_calls_out, cohesion,
      otp_used, max_application_depth,
      max_depth_of_calling, min_depth_of_calling,
      max_depth_of_cases, min_depth_of_cases,
      max_depth_of_structs, number_of_funclauses,
      branches_of_recursion, mcCabe, number_of_funexpr,
      number_of_messpass, average_size,
      max_length_of_line, average_length_of_line,
      no_space_after_comma].

modsum({module, Mod})->
    calculateModuleSum(Mod);
modsum({Type, _}) when (Type == function) orelse (Type == clause)->
    throw(?RefError(incompat, [modsum])).

%%% @private
calculateModuleSum(Mod)->
    List = allMetricOfTheModule(atom_to_list(?Mod:name(Mod)),
                                funlist(module), [sum], module),
    MList = [Number || {sum, Number} <- lists:flatten(List), is_integer(Number)],
    lists:sum(MList).

%%% @private
allMetricOfTheModule(Modules, ListOfTheMetrics, Filters, NodeType)->
    [[calcMod(Metric, NodeType, Modules, Filter)
                            || Metric <- ListOfTheMetrics]
                            || Filter <- Filters].

%%% @private
calcMod(Metric, NodeType, Node, Filter)->
    [preQuery({{show, Metric,
                   NodeType, [Node]}, Filter})].

%%% @private
%% Returns the number of OTP function calls within the given module or function
otp_used({module, Mod})->
    lists:flatten([otp_used({function, Fun})
                  || Fun <- ?Query:exec(Mod, ?Mod:locals())]);

otp_used({function, Fun})->
    OtpFun =
        fun(Func)->
               ?Fun:is_autoimported(?Fun:name(Func), ?Fun:arity(Func))
        end,
    [Ref || Ref<-?Query:exec(Fun, ?Fun:funcalls()), OtpFun(Ref)];

otp_used({Type, _}) when Type == clause ->
    throw(?RefError(incompat, [otp_used])).

%%% @private
fun_sum()->
    [line_of_code, char_of_code, number_of_funclauses,
     branches_of_recursion,
     mcCabe, calls_for_function, calls_from_function,
     fun_return_points, number_of_messpass].

%%% @private
fun_inside({Type, Fun}) when (Type == function) orelse (Type == clause)->
    [Module] = ?Query:exec(Fun, ?Fun:module()),
    FunName = [{atom_to_list(?Mod:name(Module)),
                atom_to_list(?Fun:name(Fun)),
                ?Fun:arity(Fun)}],
    Rs = [preQuery({{show, Metric, Type, FunName}, sum}) || Metric <- fun_sum()],
    Vals = [Val || {sum, Val} <- lists:flatten(Rs), is_integer(Val)],
    lists:sum(Vals);

fun_inside({module, _}) ->
    throw(?RefError(incompat, [fun_inside])).

%% This function returns the maximum depth of embedded function
%%      applications in the module `Mod' or the function `Fun'.
%%% @private
max_application_depth({module, Mod}) ->
    Functions = ?Query:exec(Mod, ?Mod:locals()),
    Depths    = [max_application_depth({function, Fun}) || Fun <- Functions],
    lists:max([0] ++ Depths);
max_application_depth({function, Fun}) ->
    TopExprs = ?Query:exec(Fun, ?Query:seq(
                                  [?Fun:definition(),
                                  ?Form:clauses(),
                                  ?Clause:exprs()])),
    max_application_depth({exprs, TopExprs});
max_application_depth({clause, Cl}) ->
    TopExprs = ?Query:exec(Cl, ?Clause:exprs()),
    max_application_depth({exprs, TopExprs});
max_application_depth({exprs, Exprs}) ->
    Depths     = [max_application_depth({expr, Expr, ?Expr:type(Expr)}) ||
                     Expr <- Exprs],
    lists:max([0] ++ Depths);
max_application_depth({expr, Expr, application}) ->
    1 + max_application_depth({expr, Expr, other});
max_application_depth({expr, Expr, _}) ->
    SubExprs    = ?Query:exec(Expr,
                     ?Query:any(
                        ?Expr:children(),
                        ?Query:seq([?Expr:clauses(),?Clause:exprs()])
                       )
                    ),
    max_application_depth({exprs, SubExprs}).

%% This function returns the maximum depth of the call tree
%%      starting from the module `Mod' or the function `Fun'.
%%% @private
max_depth_of_calling({module, Mod}) ->
    Functions = ?Query:exec(Mod, ?Mod:locals()),
    Depths    = [max_depth_of_calling({function, Fun}) || Fun <- Functions],
    lists:max([0] ++ Depths);
max_depth_of_calling({function, Fun}) ->
    max_depth_of_calling({function, Fun}, {historyset, sets:from_list([Fun])});
max_depth_of_calling({clause, Cl}) ->
    Fun = ?Query:exec(Cl, ?Query:seq(?Clause:form(), ?Form:func())),
    max_depth_of_calling({clause, Cl}, {historyset, sets:from_list([Fun])}).

%%% @private
max_depth_of_calling(A, B) ->
    depth_of_calling(A, B, fun  max_depth_of_calling/2, fun lists:max/1).


%% max_depth_of_calling({Type, Node}, {historyset, History}) ->
%%     case Type of
%%         function ->
%%             CalledList = ?Query:exec(Node, ?Fun:funcalls());
%%         clause   ->
%%             CalledList = ?Query:exec(Node, ?Query:seq([?Clause:exprs(),
%%                                                        ?Expr:deep_sub(),
%%                                                        ?Expr:function()]))
%%     end,
%%     CalledSet   = sets:from_list(CalledList),
%%     NewCalled   = sets:subtract(CalledSet, History),
%%     NewCallList = sets:to_list(NewCalled),
%%     Depths      = [1 + max_depth_of_calling(
%%                          {function, Called},
%%                          {historyset, sets:add_element(Called, History)}) ||
%%                       Called <- NewCallList],
%%     lists:max([0 | Depths]).

%%% @private
%% TODO: reimplement it
min_depth_of_calling({module, Mod})->
    Functions = ?Query:exec(Mod, ?Mod:locals()),
    Depths    = [max_depth_of_calling({function, Fun}) || Fun <- Functions],
    D = lists:delete(0,lists:usort(Depths)),
    case D of
        [] -> 0;
        _-> lists:min(D)
    end;
min_depth_of_calling({function, Fun}) ->
    min_depth_of_calling({function, Fun}, {historyset, sets:from_list([Fun])});
min_depth_of_calling({clause, Cl}) ->
    Fun = ?Query:exec(Cl, ?Query:seq(?Clause:form(), ?Form:func())),
    min_depth_of_calling({clause, Cl}, {historyset, sets:from_list([Fun])}).

%%% @private
min_depth_of_calling(A, B) ->
    depth_of_calling(A, B, fun  min_depth_of_calling/2, fun lists:min/1).

depth_of_calling({Type, Node}, {historyset, History}, Func, Comp) ->
    case Type of
        function ->
            CalledList = ?Query:exec(Node, ?Fun:funcalls());
        clause   ->
            CalledList = ?Query:exec(Node, ?Query:seq([?Clause:exprs(),
                                                       ?Expr:deep_sub(),
                                                       ?Expr:function()]))
    end,
    CalledSet   = sets:from_list(CalledList),
    NewCalled   = sets:subtract(CalledSet, History),
    NewCallList = sets:to_list(NewCalled),
    Depths      = [1 + Func(
                         {function, Called},
                         {historyset, sets:add_element(Called, History)}) ||
                      Called <- NewCallList],
    case Depths of
        [] -> 0;
        _ -> Comp(Depths)
    end.

%%% @private
min_depth_of_cases({module, Mod})->
    Functions = ?Query:exec(Mod, ?Mod:locals()),
    Depths    = lists:flatten([max_case_depth(Fun)|| Fun <- Functions]),
    case lists:usort(Depths)--[0] of
        [] -> 0;
        [Min|_] -> Min
    end;

min_depth_of_cases({function, Fun})->
    FunClauses = ?Query:exec(Fun, ?Query:seq(
                                  ?Fun:definition(), ?Form:clauses())),
    TopExprs   = [Expr ||   Cl   <- FunClauses,
                            Expr <- ?Query:exec(Cl, ?Clause:exprs())],
    min_depth_of_cases_top_exprs(TopExprs);

min_depth_of_cases({clause, Cl})->
    TopExprs   = ?Query:exec(Cl, ?Clause:exprs()),
    min_depth_of_cases_top_exprs(TopExprs).

min_depth_of_cases_top_exprs(TopExprs)->
    Depths = lists:flatten([max_case_depth(Expr, 0) || Expr <- TopExprs]),
    case lists:usort(Depths) -- [0] of
        [] -> 0;
        [Min|_] -> Min
    end.

%%% @private
max_depth_of_cases({module, Mod})->
    Functions = ?Query:exec(Mod, ?Mod:locals()),
    Depths    = [max_case_depth(Fun)|| Fun <- Functions],
    lists:max(Depths);
max_depth_of_cases({function, Fun})->
    max_case_depth(Fun);
max_depth_of_cases({clause, Cl})->
    TopExprs   = ?Query:exec(Cl, ?Clause:exprs()),
    lists:max(lists:flatten([0|[max_case_depth(Expr, 0) || Expr <- TopExprs]])).

%% Returns the maximal depth of embedded case structures.
%%% @private
max_case_depth(Fun) ->
    FunClauses = ?Query:exec(Fun, ?Query:seq(
                                  ?Fun:definition(), ?Form:clauses())),
    TopExprs   = [Expr ||   Cl   <- FunClauses,
                            Expr <- ?Query:exec(Cl, ?Clause:exprs())],
    lists:max(lists:flatten([0|[max_case_depth(Expr, 0) || Expr <- TopExprs]])).

%% Returns a deep list of maximal depths of embedding.
%%% @private
max_case_depth_({Expr, case_expr}, Depth) ->
    Exprs = ?Query:exec(Expr, ?Query:seq(?Expr:clauses(), ?Clause:exprs())),
    [Depth|[max_case_depth(E, Depth + 1) || E <- Exprs, E =/= Expr]];
max_case_depth_({Expr, _}, Depth)->
    Exprs = ?Query:exec(Expr, ?Query:seq(?Expr:clauses(), ?Clause:exprs())),
    [Depth|[max_case_depth(E, Depth ) || E <- Exprs, E =/= Expr]].


max_case_depth(Expr, Depth) ->
    [max_case_depth_({E, ?Expr:type(E)}, Depth)
         || E <- ?Query:exec(Expr, ?Expr:sub())].

%% Returns the maximal depth of embedded structures (block, case, fun,
%% if, receive, try).
%% @private
max_depth_of_structs({module, Mod})->
    Functions = ?Query:exec(Mod, ?Mod:locals()),
    Depths    = [max_s_depth(Fun)|| Fun <- Functions],
    lists:max(Depths);
max_depth_of_structs({function, Fun})->
    max_s_depth(Fun);
max_depth_of_structs({clause, Cl}) ->
    TopExprs   = ?Query:exec(Cl, ?Clause:exprs()),
    lists:max(lists:flatten([0|[max_s_depth(Expr, 0) || Expr <- TopExprs]])).


%%% @private
max_s_depth(Fun) ->
    FunClauses = ?Query:exec(Fun, ?Query:seq(
                                     ?Fun:definition(), ?Form:clauses())),
    TopExprs   = [Expr ||   Cl   <- FunClauses,
                            Expr <- ?Query:exec(Cl, ?Clause:exprs())],
    lists:max(lists:flatten([0|[max_s_depth(Expr, 0) || Expr <- TopExprs]])).

max_s_depth_({Expr, true}, Depth) ->
    Exprs = ?Query:exec(Expr, ?Query:seq(?Expr:clauses(), ?Clause:exprs())),
    [Depth|[max_s_depth(E, Depth + 1) || E <- Exprs, E =/= Expr]];
max_s_depth_({Expr, _}, Depth)->
    Exprs = ?Query:exec(Expr, ?Query:seq(?Expr:clauses(), ?Clause:exprs())),
    [Depth|[max_s_depth(E, Depth ) || E <- Exprs, E =/= Expr]].


max_s_depth(Expr, Depth) ->
    Types = [fun_expr, case_expr, receive_expr, block_expr, try_expr, if_expr],
    [max_s_depth_({E, lists:member(?Expr:type(E), Types)}, Depth)
         || E <- ?Query:exec(Expr, ?Expr:sub())].

%% max_s_depth({Expr, Type}, Depth) ->
%%     Types = [fun_expr, case_expr, receive_expr,block_expr,try_expr,if_expr],
%%     IsContainer = lists:member(Type,Types),
%%     case IsContainer of
%%         true ->
%%             TopExprs = ?Query:exec(Expr, ?Query:seq(?Expr:clauses(),
%%                                                     ?Clause:exprs())),
%%             NewDepth = Depth + 1;
%%         false ->
%%             % todo This part gets the subexpressions of Expr.
%%             % Since for andalso and orelse, these are connected not
%%             % only by [sub], but also a clause, the following code is needed.
%%             % todo Add this code as ?Expr:real_sub.
%%             Exprs = ?Query:exec(Expr, ?Query:seq(?Expr:clauses(),
%%                                                  ?Clause:exprs())),
%%             case Exprs of
%%                 [] ->
%%                     TopExprs = ?Query:exec(Expr, ?Expr:sub());
%%                 _->
%%                     TopExprs = Exprs
%%             end,
%%             NewDepth = Depth
%%     end,
%%     Rest = [max_s_depth(E, NewDepth) || E <- TopExprs, E =/= Expr],
%%     [Depth | Rest];
%% max_s_depth(Expr, Depth) ->
%%     max_s_depth({Expr, ?Expr:type(Expr)}, Depth).

%%% @private
funclauses({function, Fun})->
    ?Query:exec(Fun, ?Query:seq(
                             ?Fun:definition(), ?Form:clauses()));

%%% @private
funclauses({module, Mod}) ->
    FunCs = ?Query:exec(Mod, ?Mod:locals()),
    Clauses = [?Query:exec(FunNode, ?Query:seq(
                             ?Fun:definition(), ?Form:clauses())) ||
                                                      FunNode <- FunCs],
    lists:flatten(Clauses);

funclauses({clause, Cl}) ->
    [Cl].

%%% @private
b_recursion({module, Mod}) ->
    FunCs = ?Query:exec(Mod, ?Mod:locals()),
    lists:flatten([b_recursion({function, Func}) || Func <- FunCs]);
b_recursion({function, Fun}) ->
    Fundef = ?Query:exec(Fun, ?Fun:definition()),
    % todo Should count non-direct recursions as well, e.g.
    %      the following should return 2, not 1.
    %      Possibly also keep the current implementation with a different name.
    %           f() -> f() + g().
    %           g() -> f().
    lists:flatten([?Query:exec(Fun, ?Fun:applications(Expr))
        || Expr <- ?Query:exec([Fundef], ?Query:seq(?Form:clauses(),
                                                     ?Clause:exprs()))]);
b_recursion({clause, Cl}) ->
    Fun = ?Query:exec(Cl,  ?Query:seq(?Clause:form(),
                                      ?Form:func())),
    lists:flatten([?Query:exec(Fun, ?Fun:applications(Expr))
                   || Expr <- ?Query:exec(Cl, ?Clause:exprs())]).
%%% @private
mCb({module, Mod})->
    Functions = ?Query:exec(Mod, ?Mod:locals()),
    FunMCBs   = [mCb({function,Fun}) || Fun <- Functions],
    lists:sum(FunMCBs);
mCb({function, Fun})->
    Fundef    = ?Query:exec(Fun, ?Fun:definition()),
    Clauses   = ?Query:exec(Fundef, ?Form:clauses()),
    ClauseMCB = length(Clauses)-1,
    ClauseMCB + lists:sum([mCb({clause, Cl}) || Cl <- Clauses]);
mCb({clause, Cl})->
    TopExprs  = ?Query:exec(Cl, ?Clause:exprs()),
    AllExprs  = ?Query:exec(TopExprs, ?Expr:top_deep_sub()),
    IfExprs   = [Expr || Expr <- AllExprs,
                         ?Expr:type(Expr) == 'if_expr'],
    CaseExprs = [Expr || Expr <- AllExprs,
                         ?Expr:type(Expr) == 'case_expr'],
    IfPaths   = [length(?Query:exec(IfExpr, ?Expr:clauses()))-1
                 || IfExpr <- IfExprs],
    CasePaths = [length(?Query:exec(CaseExpr, ?Expr:clauses()))-2
                 || CaseExpr <- CaseExprs],
    IfMCB     = lists:sum(IfPaths),
    CaseMCB   = lists:sum(CasePaths),
    IfMCB + CaseMCB + 1.

%%% @private
calls_for({clause, Cl}) ->
    Fun = ?Query:exec(Cl,  ?Query:seq(?Clause:form(), ?Form:func())),
    ?Query:exec(Fun, ?Fun:applications());
calls_for({function, Fun})->
    ?Query:exec(Fun, ?Fun:applications());
calls_for({module, _})->
    throw(?RefError(incompat, [calls_for_function])).


%%% @private
calls_from({clause, Cl}) ->
    ?Query:exec(Cl, ?Query:seq([?Clause:exprs(), ?Expr:deep_sub(),
                                ?Expr:function()]));
calls_from({function, Fun})->
    ?Query:exec(Fun, ?Fun:funcalls());
calls_from({module, _})->
    throw(?RefError(incompat, [calls_from_function])).

%%% @private
funexp({module, Mod})->
   Functions = ?Query:exec(Mod, ?Mod:locals()),
   lists:flatten([funexp({function, Fun}) || Fun <- Functions]);
funexp({function, Fun})->
    type_expression(Fun, fun_expr);
funexp({clause, Cl}) ->
    TopExprs   = ?Query:exec(Cl, ?Clause:exprs()),
    lists:usort([Expr || Expr <- ?Query:exec(TopExprs, ?Expr:deep_sub()),
                         ?Expr:type(Expr)  == fun_expr]).


%%% @private
messpass({module, Mod})->
   Functions = ?Query:exec(Mod, ?Mod:locals()),
   lists:flatten([messpass({function, Fun}) || Fun <- Functions]);
messpass({function, Fun})->
   type_expression(Fun, send_expr);
messpass({clause, Cl}) ->
    TopExprs   = ?Query:exec(Cl, ?Clause:exprs()),
    lists:usort([Expr || Expr <- ?Query:exec(TopExprs, ?Expr:deep_sub()),
                         ?Expr:type(Expr)  == send_expr]).

%%% @private
%%% TODO:need to optimize -
%%% Too many collected expressions are in the result list before
%%% selecting the given type so the function is very slow.
type_expression(Fun, ExprType) ->
    FunClauses = ?Query:exec(Fun, ?Query:seq(
                                  ?Fun:definition(), ?Form:clauses())),
    TopExprs   = [Expr || Cl <- FunClauses, Expr
                             <- ?Query:exec(Cl, ?Clause:exprs())],
    %%EmbExpr = lists:flatten([nested(Expr, [Expr]) || Expr <- TopExprs]),
    lists:usort(
    [Expr || Expr <- ?Query:exec(TopExprs, ?Expr:deep_sub()),
             ?Expr:type(Expr)  == ExprType]).

%% nested({Expr, Type}, List) when Type == case_expr;
%%                                 Type == try_expr;
%%                                 Type == if_expr;
%%                                 Type == fun_expr;
%%                                 Type == list_comp;
%%                                 Type == list_gen->
%%     Exprs = ?Query:exec(Expr, ?Query:seq(?Expr:clauses(), ?Clause:exprs())),
%%     [Expr|[nested(E, List) || E <- Exprs, E =/= Expr]];
%% nested({Expr, _NotNExpr}, List) ->
%%     TopExprs = [E || E <- ?Query:exec(Expr, ?Expr:sub())],
%%     [Expr|[nested(E, List) || E <- TopExprs, E =/= Expr]];
%% nested(Expr, List) ->
%%     nested({Expr, ?Expr:type(Expr)}, List).

%%% @private
average_size({module, _Mod})->
    [];
average_size({Type, _}) when (Type == function) orelse (Type == clause)->
    throw(?RefError(incompat, [average_size])).

%%% @private
fun_return_points({module, Mod})->
    Functions = ?Query:exec(Mod, ?Mod:locals()),
    lists:flatten([fun_return_points({function, Fun}) || Fun <- Functions]);
fun_return_points({function, Fun})->
    FunClauses = ?Query:exec(Fun, ?Query:seq(
                                  ?Fun:definition(), ?Form:clauses())),
    LastTopExprs = [lists:last(
            ?Query:exec(Cl, ?Clause:exprs())) || Cl <- FunClauses],
    lists:flatten([return_points(LTP, ?Expr:type(LTP)) || LTP <- LastTopExprs]);
fun_return_points({clause, Cl})->
    LastTopExpr = lists:last(?Query:exec(Cl, ?Clause:exprs())),
    lists:flatten(return_points(LastTopExpr, ?Expr:type(LastTopExpr))).

%%% @private
return_points(Expr, Kind) when  Kind == case_expr;
                                Kind == try_expr;
                                Kind == if_expr;
                                Kind == receive_expr ->
    Clauses = ?Query:exec(Expr, ?Expr:clauses()),
    HeadClauses = ?Query:exec(Expr, [headcl]),
    Exprs = [lists:last(?Query:exec(Cl, ?Clause:exprs()))
                                            || Cl <- Clauses -- HeadClauses],
    [return_points(ExprL,?Expr:type(ExprL)) || ExprL <- Exprs];
return_points(Expr, _Other)->
    [Expr].

%%% @private
max_length_of_line({module, Mod}) ->
    [File] = ?Query:exec(Mod, ?Mod:file()),
    Text = lists:flatten(?Syn:tree_text(File)),
    lists:max(lists:map(fun length/1, string:tokens(Text,"\n")));
max_length_of_line({function, Fun}) ->
    [Def] = ?Query:exec(Fun, ?Fun:definition()),
    Text = lists:flatten(?Syn:tree_text(Def)),
    lists:max(lists:map(fun length/1, string:tokens(Text,"\n")));
max_length_of_line({clause, Cl}) ->
    Text = lists:flatten(?Syn:tree_text(Cl)),
    lists:max(lists:map(fun length/1, string:tokens(Text,"\n"))).

%%% @private
avg_length_of_line({module, Mod}) ->
    [File] = ?Query:exec(Mod, ?Mod:file()),
    Text = lists:flatten(?Syn:tree_text(File)),
    List = lists:map(fun length/1, string:tokens(Text,"\n")),
    lists:sum(List) div length(List);
avg_length_of_line({function, Fun}) ->
    [Def] = ?Query:exec(Fun, ?Fun:definition()),
    Text = lists:flatten(?Syn:tree_text(Def)),
    List = lists:map(fun length/1, string:tokens(Text,"\n")),
    lists:sum(List) div length(List);
avg_length_of_line({clause, Cl}) ->
    Text = lists:flatten(?Syn:tree_text(Cl)),
    List = lists:map(fun length/1, string:tokens(Text,"\n")),
    lists:sum(List) div length(List).


%%% @private
no_space_after_comma({module, Mod}) ->
    [File] = ?Query:exec(Mod, ?Mod:file()),
    Text = lists:flatten(?Syn:tree_text(File)),
    length(lists:filter(fun ([]) -> [];
                            ([X|_]) -> X =/= $
                        end,
                        tl(string:tokens(Text,","))));
no_space_after_comma({function, Fun}) ->
    [Def] = ?Query:exec(Fun, ?Fun:definition()),
    Text = lists:flatten(?Syn:tree_text(Def)),
    length(lists:filter(fun ([]) -> [];
                            ([X|_]) -> not lists:member(X, [$\s, $\n, $\t])
                        end,
                        tl(string:tokens(Text,",;"))));
no_space_after_comma({clause, Cl}) ->
    Text = lists:flatten(?Syn:tree_text(Cl)),
    length(lists:filter(fun ([]) -> [];
                            ([X|_]) -> not lists:member(X, [$\s, $\n, $\t])
                        end,
                        tl(string:tokens(Text,",;")))).

%%% @private
%%% return value: 1 means tail recursive function,
%%%               0 means recursive, but not tail recursive function
%%%              -1 means non-recursive function
is_tail_recursive({clause, Cl}) ->
    [Fun] = ?Query:exec(Cl, ?Query:seq(?Clause:form(),
                                     ?Form:func())),
    Exprs = ?Query:exec(Cl, ?Clause:exprs()),
    case lists:reverse(Exprs) of
        [] -> -1; %% is not a recursive function
        [X|Xs] ->
            case lists:all
                (fun(Expr) -> not is_recursive(Fun, Expr) end,
                 Xs) of
                false -> 0;  %% is a recursive function, but not tail recursive
                true  -> determine_is_tail_rec_helper(Fun, X)
            end
    end;
is_tail_recursive({function, Fun}) ->
    [Def] = ?Query:exec(Fun, ?Fun:definition()),
    Exprs = ?Query:exec(Def, ?Query:seq(?Form:clauses(),
                                        ?Clause:exprs())),
    case lists:reverse(Exprs) of
        [] -> -1; %% is not a recursive function
        [X|Xs] ->
            case lists:all
                (fun(Expr) -> not is_recursive(Fun, Expr) end,
                 Xs) of
                false -> 0;  %% is a recursive function, but not tail recursive
                true  -> determine_is_tail_rec_helper(Fun, X)
            end
    end;
is_tail_recursive({module, _Mod}) ->
    throw(?RefError(incompat, [is_tail_recursive])).

%%% @private
%%% helper function for is_tail_recursive function
is_recursive(Fun, Expr) ->
    %has direct recursive call
    length(?Query:exec(Fun, ?Fun:applications(Expr))) > 0
        orelse
        %has indirect recursive call
        has_indirect_recursion(Fun, ?Query:exec(Expr, ?Expr:funapps()), []).

%%% @private
%%% helper function for is_tail_recursive function
has_indirect_recursion(_Fun, [], _Examined) -> false;
has_indirect_recursion(Fun, [F1|Fs], Examined) ->
    Fun == F1  orelse
        has_indirect_recursion
          (Fun,
           Fs ++ (?Query:exec(F1, ?Fun:funcalls()) -- Examined),
           [F1|Examined]).


determine_is_tail_rec_helper(Fun, TExpr)->
	case  is_recursive(Fun, TExpr)  andalso
		  (?Expr:type(TExpr) /=  try_expr orelse
		  	(?Expr:type(TExpr) == try_expr andalso
			 ?Expr:has_after_clause(TExpr) == false)) of
                             false -> -1; % is not a recursive function
                             true -> 1 % is a tail recursive function
                        end.


