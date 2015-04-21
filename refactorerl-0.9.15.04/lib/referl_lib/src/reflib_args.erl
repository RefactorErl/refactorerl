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

%%% @doc Generic refactoring argument handler. The goal of this module is to
%%% let refactoring code be independent of parameter representation. Parameter
%%% lists are represented by property lists, but refactorings need not know
%%% the property keys used for a certain parameter. This allows multiple ways
%%% of specifying a parameter (like a function can be specified by a file and
%%% a position, or a module and a function name).
%%%
%%% Parameters are classified as follows:
%%% <dl>
%%%  <dt>Source parameters</dt>
%%%  <dd>These are usually existing entities which are to be modified by the
%%%      refactoring. Interface functions are named after the kind of entity
%%%      that is required by the refactoring, and they return graph nodes.</dd>
%%%
%%%  <dt>Target parameters</dt>
%%%  <dd>These parameter values are usually either properties (like a new
%%%      name) or entities that need not exist (like a target file which can
%%%      be created). Target interface functions thus always return a property
%%%      value, which can be used to identify target entities (e.g. a module
%%%      by its name).</dd>
%%% </dl>
%%%
%%% @type arglist() = [{Key::atom(), Value}]. Refactoring argument list, which
%%% may contain the following key-value pairs:
%%% <table border="1">
%%%  <tr><th>key</th><th>value type</th><th>constraint</th></tr>
%%%  <tr><td>`arity'</td>  <td>{@type integer()}</td><td>Non-negative</td></tr>
%%%  <tr><td>`create_new_file'</td> <td>{@type bool()}</td></tr>
%%%  <tr><td>`file'</td>     <td>{@type string()}</td>
%%%                                                <td>Valid filename</td></tr>
%%%  <tr><td>`filename'</td> <td>{@type string()}</td>
%%%                                                <td>Valid filename</td></tr>
%%%  <tr><td>`function'</td> <td>{@type atom()}</td></tr>
%%%  <tr><td>`funlist'</td>
%%%                    <td>{@type [{Fun::atom(), Arity::integer()@}]}</td></tr>
%%%  <tr><td>`funclusters'</td>
%%       <td>{@type [[{Mod::atom(), Fun::atom(), Arity::integer()@}]]}</td></tr>
%%%  <tr><td>`has_side_effect'</td> <td>{@type bool()}</td></tr>
%%%  <tr><td>`macname'</td>    <td>{@type atom()|string()}</td></tr>
%%%  <tr><td>`macro'</td>    <td>{@type atom()|string()}</td></tr>
%%%  <tr><td>`maclist'</td><td>{@type [atom()|string()]}</td></tr>
%%%  <tr><td>`module'</td>   <td>{@type atom()}</td></tr>
%%%  <tr><td>`name'</td>     <td>{@type atom()|string()}</td></tr>
%%%  <tr><td>`number'</td>   <td>{@type number()}</td></tr>
%%%  <tr><td>`order'</td>    <td>{@type [integer()]}</td>
%%%                                 <td>The numbers 1..n in any order</td></tr>
%%%  <tr><td>`position'</td> <td>{@type integer()}</td><td>Positive</td></tr>
%%%  <tr><td>`posrange'</td><td>{@type {Begin::integer(),End::integer()@}}</td>
%%%                                       <td>1 &lt;= Begin &lt;= End</td></tr>
%%%  <tr><td>`record'</td><td>{@type atom()}</td></tr>
%%%  <tr><td>`recfield'</td><td>{@type atom()}</td></tr>
%%%  <tr><td>`reclist'</td><td>{@type [atom()]}</td></tr>
%%%  <tr><td>`text'</td>     <td>{@type string()|atom()}</td></tr>
%%%  <tr><td>`varname'</td>  <td>{@type string()}</td>
%%%                                          <td>Valid variable name</td></tr>
%%% </table>
%%%
%%% @author Laszlo Lovei <lovei@inf.elte.hu>
%%% @author bkil.hu <v252bl39h07fgwqm@bkil.hu>

-module(reflib_args).
-vsn("$Rev: 12196 $ ").

-export([string/1, string/2, string/3, recfield_names/1,
         integer/1, integer/3,
         name/1, atom/3, bool/3]).
-export([funname/1]).
-export([function/1, variable/1, variable/2, module/1, moduse/1, file/1, form/1,
         expression/1, clause/1, functions/1, record/1, record_field/1, records/1,
         import_form/1, macro/1, macros/1, funclusters/1]).
-export([varname/1, filename/1, macname/1, macuse/1]).
-export([expr_range/1, order/1]).
-export([error_text/2]).
-export([ask_missing/1]).
-export([ask/6, ask/5, ask/4]).
-export([algorithm/1, entity/1, decomposition/1,
         %transformfun/1, mergefun/1,
         %antigravity/1, distfun/1, population_size/1, iterations/1,
         %mutation_rate/1, crossover_rate/1, elite_count/1,
         %maximum_cluster_size/1, maximum_start_cluster_size/1,
         library_limit/1, headers/1, show_goodness/1, only_best/1,
         store_results/1, cluster_options/1, decomp_options/1]).

-include("lib.hrl").
-define(CallArg(A,S,L),?MISC:call_arg(A,S,L)).

%%% @type node() = refcore_graph:node()

%%% ============================================================================
%%% Error text and actions

error_text(bad_atom, [Text]) ->
    ["\"", Text, "\" is not a valid atom (use single quotes)"];
error_text(bad_funclusters, [Clusters]) ->
    ["\"", io_lib:print(Clusters), "\" is not a valid function clustering"];
error_text(not_inside_funcl, _) ->
    "The given position has to be inside a function clause.".

%% Module actions
error_action(bad_mod_pos, [_File, Pos]) ->
    Info = ?MISC:format("The given position (" ++ integer_to_list(Pos) ++
                            ") has to indicate a module."),
    module([{ask_missing, true}, {info_text, Info}]);

error_action(bad_mod_name, [ModName]) ->
    Info = ?MISC:format("The given module (" ++
                            atom_to_list(ModName) ++ ") does not exist."),
    module([{ask_missing, true}, {info_text, Info}]);

error_action(bad_moduse_pos, [File, Pos]) ->
    Info = ?MISC:format("The given position (" ++ integer_to_list(Pos) ++
                            ") has to indicate a module."),
    moduse([{file, File},{ask_missing, true}, {info_text, Info}]);

error_action(bad_moduse_name, [ModName]) ->
    Info = ?MISC:format("The given module (" ++
                            atom_to_list(ModName) ++ ") does not exist."),
    moduse([{ask_missing, true}, {info_text, Info}]);

%% File actions
error_action(bad_file, [File]) ->
    Info = ?MISC:format("The given file (" ++
                            File ++ ") cannot be found in the database."),
    file([{ask_missing, true}, {info_text, Info}]);

%% Function actions
error_action(bad_fun_pos, [ModName, Pos]) ->
    Info = ?MISC:format("The given position (" ++ integer_to_list(Pos) ++
                            ") has to indicate function."),
    function([{module, ModName}, {ask_missing, true}, {info_text, Info}]);

error_action(bad_fun_name, [ModName, FunName, Arity]) ->
    Info = ?MISC:format("The function " ++
                            atom_to_list(FunName) ++ "/" ++
                            integer_to_list(Arity) ++ " does not exist."),
    function([{module, ModName},{ask_missing, true}, {info_text, Info}]);

%% Function name actions
error_action(bad_fun_name, [ModName, FunName]) ->
    Info = ?MISC:format("The function " ++
                            atom_to_list(FunName) ++ " does not exist."),
    funname([{module, ModName},{ask_missing, true}, {info_text, Info}]);

%% Macro actions
error_action(bad_macro_pos, [File, Pos]) ->
    Info = ?MISC:format("The given position (" ++ integer_to_list(Pos) ++
                            ") has to indicate a macro."),
    macro([{file, File},{ask_missing, true}, {info_text, Info}]);

error_action(bad_macro_name, [File, Macro]) ->
    Info = ?MISC:format("The macro ?~p does not exist.", [Macro]),
    macro([{file, File},{ask_missing, true}, {info_text, Info}]);

error_action(bad_macuse_pos, [File, Pos]) ->
    Info = ?MISC:format("The given position (" ++ integer_to_list(Pos) ++
                            ") has to indicate a macro usage."),
    macuse([{file, File},{ask_missing, true}, {info_text, Info}]);

%% Record actions
error_action(bad_rec_pos, [File, Pos]) ->
    Info = ?MISC:format("The given position (" ++ integer_to_list(Pos) ++
                            ") has to indicate a record."),
    record([{file, File},{ask_missing, true}, {info_text, Info}]);

error_action(bad_rec_name, [File, Record]) ->
    Info = ?MISC:format("The record #~p does not exist.", [Record]),
    record([{file, File},{ask_missing, true}, {info_text, Info}]);

error_action(bad_recfield_pos, [File, Pos]) ->
    Info = ?MISC:format("The given position (" ++ integer_to_list(Pos) ++
                            ") has to indicate a record field."),
    record_field([{file, File},{ask_missing, true}, {info_text, Info}]);

error_action(bad_recfield_name, [File, Record, RecField]) ->
    Info = ?MISC:format("The record field #~p.~p does not exist.",
                        [Record, RecField]),
    record_field([{file, File},{record, Record},{ask_missing, true},
                  {info_text, Info}]);

%% Variable actions
error_action(bad_var_pos, [File, Pos]) ->
    ?Transform:question([[{format,info},
                          {text,"The given position (" ++
                               integer_to_list(Pos) ++
                               ") has to indicate a variable or has" ++
                               " to be inside a function clause."}]]),
    variable([{file,File},{ask_missing,true}]);

error_action(bad_var_name, [Var]) ->
    Info = ?MISC:format("The name ~p is not a valid variable name", [Var]),
    varname([{ask_missing, true}, {info_text, Info}]);

%% Form actions
error_action(bad_importform_pos, [File, Pos]) ->
    Info = ?MISC:format("The given position (" ++ integer_to_list(Pos) ++
                            ") has to indicate an import form."),
    import_form([{file,File},{ask_missing,true}, {info_text, Info}]).


error_case(true, {ErrAction, Arg}, _) ->
    error_action(ErrAction, Arg);
error_case(false, _, {ErrAtom, ErrArgList}) ->
    throw(?RefError(ErrAtom, ErrArgList)).


%%% ============================================================================
%%% Database independent functions

%% @spec string(arglist()) -> string()
%% @doc Returns a generic target string value. This value is specified with a
%% `text' key in the argument list.
string(Args) -> string(text, "text: ", Args).

%% @spec recfield_names(arglist()) -> string()
%% @doc Returns target string value, which specifies the field names
%% of a record.

recfield_names(Args) -> string(text, "record field names: ", Args).

%% @spec string(arglist(), string()) -> string()
%% @doc Returns a generic target string value. This value is specified with a
%% `text' key in the argument list.
string(Args, Text) -> string(text, Text, Args).

%% @spec integer(arglist()) -> integer()
%% @doc Returns a generic target integer value. This value is specified with a
%% `number' key in the argument list.
integer(Args) -> integer(number, "A number", Args).

%% @spec name(arglist()) -> atom()
%% @doc Returns a generic target name as an atom (which can be used as a
%% function or module name). This value is specified with a `name' key in the
%% argument list.
name(Args) ->
    DefText = "Please enter a target name (an atom):",
    NewArgs = ask_missing(name, Args, atom, DefText, fun list_to_atom/1),
    atom(name, "Target name", NewArgs).

%% gabre
%% @spec funname(arglist()) -> string()
%% @doc Returns a function name without arity.
%% This value is specified with a `funname' key
%% in the argument list.

funname(Args) ->
    get_arg_data(Args, fun funname_with_ask/1, fun funname_from_arg_list/1).

funname_sorter(Afun, Bfun) ->
    (?Fun:name(Afun)) =< (?Fun:name(Bfun)).

funname_with_ask(Args) ->
    NArgs = getModule(Args), %% Module properly set

    case (proplists:is_defined(funname,NArgs) or
    proplists:is_defined(position,NArgs)) of

    false ->
    %   DefText = "Please enter a function name (an atom):",
    %   NewArgs = ask_missing(funname, Args, atom, DefText, fun list_to_atom/1),
    %   funname_from_arg_list(NewArgs);
        Funs = ?Query:exec(module(NArgs),
                               ?Query:seq([?Mod:file(),
                                           ?File:forms(),
                                           ?Form:func()])),
        SFuns = lists:sort(fun funname_sorter/2, Funs),
        SFunsWcount = grpcnt(SFuns,fun ?Fun:name/1),
        OFuns = [ element(1,X) || X <- SFunsWcount, element(2,X) > 1 ],
        ?Check(OFuns /= [], ?RefErr0r(no_ofun)),
        Qu = add_to_proplist(info, "Please specify a function:", false),
        Question = [Qu] ++
            [add_to_proplist(radio,
                                 create_fun_text(?Fun:name(F),
                                                 ?Fun:arity(F)),
                                true)
             || F <- OFuns],
        Ans = ?Transform:question(Question),
        get_selected_entity(tl(Ans), Funs);

    _ -> funname_from_arg_list(NArgs)

end.

funname_from_arg_list(Args) ->
    ?CallArg(Args, "Function name",
        [{fun funnamebyname/3, [module, funname, ask_missing]},
         {fun funnamebypos/3, [file, position, ask_missing]} %% funbypos
           ]).

funnamebypos(File, Pos, AskMissing) ->
    funbypos(File, Pos, AskMissing).

funnamebyname(Mod, Funname, AskMissing) ->
    [Module] = ?Query:exec(?Mod:find(Mod)),
    FunList = ?Query:exec(Module, ?Mod:locals()),
    FunsWFName = [ X || X <- FunList, ?Fun:name(X) == Funname ],
    case FunsWFName of
        []        -> error_case(AskMissing,
                               {bad_fun_name, [Mod, Funname]},
                               {fun_not_found, [Mod, Funname]});
        FList     ->
                        hd(FList)
end.

%% Helper functions

grpcnt([B | Rest],Fun) ->
    grpcnt([B | Rest],Fun,0).
grpcnt([A,B | Rest],Fun,X) ->
    case Fun(A) =:= Fun(B) of
        true -> grpcnt([B | Rest],Fun,X+1);
        false -> [{A,X+1}] ++ grpcnt([B | Rest],Fun,0)
    end;

grpcnt([A],_Fun,X) ->
    [{A,X+1}];
grpcnt([],_Fun,_X) ->
    [].


%% @spec string(atom(), string(), arglist()) -> string()
%% @doc Returns an arbitrary string parameter from `Args' using the key `Key'.
%% In case of error, `Desc' is used to describe the parameter in the error
%% message. Use only when {@link string/1} is insufficient.
string(Key, Desc, Args) ->
    DefText = "Please enter the " ++ Desc,
    NewArgs = ask_missing(Key, Args, text, DefText),
    ?CallArg(NewArgs, Desc, [{stringval(Key), [Key]}]).

stringval(Key) ->
    fun
        (Atom) when is_atom(Atom) -> atom_to_list(Atom);
        (Str) when is_list(Str) -> Str;
        (_) -> throw(?RefError(arg_type, [Key, "string"]))
    end.

%% @spec integer(atom(), string(), arglist()) -> integer()
%% @doc Returns an arbitrary integer parameter from `Args' using the key `Key'.
%% In case of error, `Desc' is used to describe the parameter in the error
%% message. Use only when {@link integer/1} is insufficient.
integer(Key, Desc, Args) ->
    ?CallArg(Args, Desc, [{intval(Key), [Key]}]).

intval(Key) ->
    fun
        (Num) when is_integer(Num) -> Num;
        (Num) when is_float(Num)   -> trunc(Num);
        (_) -> throw(?RefError(arg_type, [Key, "integer"]))
    end.

%% @spec bool(atom(), string(), arglist()) -> bool()
%% @doc Returns an arbitrary boolean parameter from `Args' using the key `Key'.
%% In case of error, `Desc' is used to describe the parameter in the error
%% message.
bool(Key, Desc, Args) ->
    ?CallArg(Args, Desc, [{boolval(Key), [Key]}]).

boolval(Key) ->
    fun
        (B) when is_boolean(B) -> B;
        (_) -> throw(?RefError(arg_type, [Key, "bool"]))
    end.

%% @spec atom(atom(), string(), arglist()) -> atom()
%% @doc Returns an arbitrary atom parameter from `Args' using the key `Key'.
%% In case of error, `Desc' is used to describe the parameter in the error
%% message. Use only when {@link name/1} is insufficient.
atom(Key, Desc, Args) ->
    ?CallArg(Args, Desc, [{atomval(Key), [Key]}]).

%% Note: since `erl_scan:string/1` identifies `spec` as a category in itself
%%       and not as an atom, it is dealt with as a unique case.
atomval(Key) ->
    fun
        (Atom) when is_atom(Atom) -> Atom;
        ("spec") -> spec;
        (Str) when is_list(Str) ->
            case erl_scan:string(Str) of
                {ok, [{atom, _, Atom}], _} -> Atom;
                _ -> throw(?LocalError(bad_atom, [Str]))
            end;
        (_) -> throw(?RefError(arg_type, [Key, "atom"]))
    end.

%% @spec filename(arglist()) -> string()
%% @doc Returns the target file name. This value is specified with
%% a `filename' key in the argument list.
filename(Args) ->
    DefText = "Please enter the target file name:",
    NewArgs = ask_missing(filename, Args, header, DefText),
    string(filename, "Target file name", NewArgs).

%% @spec order(arglist()) -> [integer()]
%% @doc Returns the target order (a list of consecutive integers starting from
%% 1, in an arbitrary order). This value is specified with an `order' key in
%% the argument list.
order(Args) ->
    DefText = "Please specify the order (e.g 3 1 2):",
    NewArgs = ask_missing(order, Args, none, DefText, fun convert_order/1),
    ?CallArg(NewArgs, "Order", [{fun orderbylst/1, [order]}]).

convert_order(Order) ->
    try
        [list_to_integer(T) || T <- string:tokens(Order, " ")]
    catch
        error:badarg -> []
    end.

orderbylst(List) -> List.

ask_missing(Args) ->
    case proplists:is_defined(ask_missing, Args) of
        true ->
            Interaction = proplists:get_all_values(ask_missing, Args),
            lists:all(fun(I) -> I == true end, Interaction);
        false ->
            false
    end.

% -spec ask(args(), atom(), (data(), infos()) -> collected(),
%           (referror_action(), data(), infos()) -> string(), infos()) -> collected().
%% @doc This function inputs an argument using `ArgFun', a function of arity one from `Args'.
%% `ArgFun' has to take both `Args' and `[{ask_missing, true}]' as arguments.
%% It then preconverts the data using `PreConvert',
%% collects infos (`CollectAndCheckFun', getting the asked arg
%% and additionally supplied arguments `AddInfos') and does checks;
%% if the checks fail and interactions are on,
%% it displays a format error and tries to get another input from the user.
%% The error is displayed using `AskErrorFmt/3',
%% which takes the ?RefError, the collected data and the additional infos,
%% and returns `{ErrorFmt, ErrorFmtData}'.
%% @todo Should return the new `Args' as well.
ask(Args, ArgFun, CollectAndCheckFun, AskErrorFmt, AddInfos, PreConvert) ->
    InitData = ?Args:ArgFun(Args),
    case ask_missing(Args) of
        false ->
            ConvInitData = PreConvert(InitData),
            CollectAndCheckFun(ConvInitData, AddInfos);
        true ->
            ask2(Args, CollectAndCheckFun, AddInfos, ArgFun, AskErrorFmt, PreConvert, InitData)
    end.

ask(Args, ArgFun, CollectAndCheckFun, AskErrorFmt, AddInfos) ->
    PreConvert = fun(X) -> X end,
    ask(Args, ArgFun, CollectAndCheckFun, AskErrorFmt, AddInfos, PreConvert).

%% @doc The only difference between function `ask/5' and this one is that
%%      `AddInfos' is not required here, and the functions
%%      `CollectAndCheckFun' and `AskErrorFmt' do not receive it.
ask(Args, ArgFun, CollectAndCheckFun, AskErrorFmt) ->
    CnC2 = fun(Data, unused)        -> CollectAndCheckFun(Data) end,
    AEF2 = fun(Error, Data, unused) -> AskErrorFmt(Error, Data) end,
    ask(Args, ArgFun, CnC2, AEF2, unused).

%@todo document
ask2(Args, CollectAndCheckFun, AddInfos, ArgFun, AskErrorFmt, PreConvert, Data) ->
    ConvData = PreConvert(Data),
    try
        CollectAndCheckFun(ConvData, AddInfos)
    catch
        % @todo Should only catch thrown ?RefError(_, _) and ?LocalError(_, _)
        throw:Error ->
            ErrorTxt = AskErrorFmt(Error, ConvData, AddInfos),
            NewData = ?Args:ArgFun([ {ask_missing, true}, {info_text, ErrorTxt}
                                     |proplists:delete(ArgFun,Args)]),
            ask2(Args, CollectAndCheckFun, AddInfos, ArgFun, AskErrorFmt,
                 PreConvert, NewData)
    end.

%%% ============================================================================
%%% Database dependent functions

% @todo document
funclusters(Args) ->
    ?CallArg(Args, "Function clusters",
        [{fun funclusters_/1, [funclusters]}]).

funclusters_(Clusters) ->
    try
        [[ begin
               {M,F,A}=Fun,
               true = is_atom(M) andalso is_atom(F) andalso is_integer(A)
           end
           || Fun <- Cluster ] || Cluster <- Clusters],
        Clusters
    catch
        _:_ -> throw(?RefError(bad_funclusters,[Clusters]))
        %@todo badmatch
    end.


get_arg_data(Args, FunWithAsking, FunWithoutAsking) ->
    case ask_missing(Args) of
        true ->
            FunWithAsking(Args);
        _ ->
            FunWithoutAsking(Args)
    end.

add_to_proplist(Format, Text, Default) ->
    [{format, Format}, {text, Text}, {default, Default}].

add_to_proplist(Format, Text, Default, Validator) ->
    [{format,Format}, {text, Text}, {default, Default}, {validator, Validator}].

create_fun_text(Name, Arity) ->
    lists:flatten(?MISC:fun_text([Name, Arity])).

%% @spec function(arglist()) -> node()
%% @doc Returns the source function node. This value is specified in the
%% argument list with
%% <ul>
%%  <li>`module', `function', and `arity' keys, or</li>
%%  <li>`file' and `position' keys.</li>
%% </ul>
function(Args) ->
    get_arg_data(Args, fun fun_with_ask/1, fun fun_from_arg_list/1).

%% Function check whether the function is specified in the Args parameter, if no
%% it asks the user to select a function from a list.
fun_with_ask(Args) ->
    NArgs = getModule(Args),
    case proplists:is_defined(function,NArgs) or
        proplists:is_defined(position,NArgs) of
        false ->
            Funs = ?Query:exec(module(NArgs),
                               ?Query:seq([?Mod:file(),
                                           ?File:forms(),
                                           ?Form:func()])),
            ?Check(Funs /= [], ?RefErr0r(no_fun)),
            FilteredFuns = filter_min_arity(Funs, NArgs),
            Qu = add_to_proplist(info, "Please specify a function:", false),
            Question = [Qu] ++
                [add_to_proplist(radio,
                                 create_fun_text(?Fun:name(F),
                                                 ?Fun:arity(F)),
                                true)
                 || F <- FilteredFuns],
            Ans = ?Transform:question(Question),
            get_selected_entity(tl(Ans), FilteredFuns);
        _     ->
            fun_from_arg_list(NArgs)
    end.

filter_min_arity(Funs, Args) ->
    case proplists:lookup(minarity, Args) of
        {minarity, Arity} -> lists:filter(fun(F) -> ?Fun:arity(F) >= Arity end, Funs);
        none              -> Funs
    end.

fun_from_arg_list(Args) ->
    ?CallArg(Args, "Function",
             [{fun funbyname/4, [module, function, arity, ask_missing]},
              {fun funbypos/3, [file, position, ask_missing]},
              {fun funnode/1, [nodes]}]).

funnode([Node])-> %@todo verify
    Node.

funbyname(Mod, Fun, Arity, AskMissing) ->
    Function = ?Query:exec(
                  ?Query:seq([?Mod:find(Mod),
                              ?Mod:local(Fun, Arity)])),

    case Function of
        [Result] -> Result;
        _        -> error_case(AskMissing,
                               {bad_fun_name, [Mod, Fun, Arity]},
                               {fun_not_found, [Mod, Fun, Arity]})
    end.

%% @todo Accept the `fun' token of implicit functions
funbypos(File, Pos, AskMissing) ->
    Expr = ?Query:exec(
              ?Query:seq([?File:find(File),
                          ?File:token(Pos),
                          ?Token:expr()])),

    ModName = ?Mod:name(modbyfile(File,AskMissing)),

    case Expr of
        [_Result] ->
            case ?Query:exec(Expr, ?Query:seq([?Expr:nameof(),
                                               ?Clause:form(),
                                               ?Form:func()])) of
                [Fun] -> Fun;
                []    ->
                    funbyexportlist(Expr, Pos, AskMissing, ModName)
            end;
        _         -> error_case(AskMissing,
                                {bad_fun_pos, [ModName, Pos]},
                                {pos_bad_type, ['fun', Pos]})
    end.

funbyexportlist(Expr, Pos, AskMissing, ModName) ->
    case ?Query:exec(Expr, ?Expr:function()) of
       [Fun] -> Fun;
       [] ->
           case ?Query:exec(Expr, ?Query:seq(?Expr:parent(),
                                             ?Expr:function())) of
               [Fun] -> Fun;
               [] -> funbyref(Expr, Pos, AskMissing, ModName)
           end
    end.

funbyref(Expr, Pos, AskMissing, ModName) ->
    case ?Query:exec(Expr, ?Expr:parent()) of
        [] -> throw(?RefError(pos_bad_type, ['fun', Pos]));
        [Par] ->
            case ?Expr:type(Par) of
                T when T =:= application;
                       T =:= implicit_fun ->
                    Func = ?Query:exec(Par, ?Expr:function()),
                    case Func of
                        [Result] -> Result;
                        _        -> error_case(AskMissing,
                                               {bad_fun_pos, [ModName, Pos]},
                                               {pos_bad_type, ['fun', Pos]})
                    end;
                infix_expr ->
                    Func = ?Query:exec(Par,
                                       ?Query:seq(?Expr:parent(),
                                                  ?Expr:function())),
                    case Func of
                        [Result] -> Result;
                        _        -> error_case(AskMissing,
                                               {bad_fun_pos, [ModName, Pos]},
                                               {pos_bad_type, ['fun', Pos]})
                    end;
                _          -> error_case(AskMissing,
                                         {bad_fun_pos, [ModName, Pos]},
                                         {pos_bad_type, ['fun', Pos]})
            end
    end.

%% @spec functions(arglist()) -> [node()]
%% @doc Returns the source function node list. This value is specified with
%% `file' and `funlist' keys in the argument list.
functions(Args) ->
    get_arg_data(Args, fun funs_with_ask/1, fun funs_from_arg_list/1).

funs_with_ask(Args) ->
    NArgs = getFile(Args),
    case proplists:is_defined(funlist, NArgs) of
        false ->
            Funs = ?Query:exec(module(NArgs),
                               ?Query:seq([?Mod:file(),
                                           ?File:forms(),
                                           ?Form:func()])),
            Qu = add_to_proplist(info,
                                 "Please select the functions from the list:",
                                 false),
            {_Args2, Qu2} = get_info_text(Args, Qu),
            Question = [Qu2] ++
                [add_to_proplist(checkbox,
                                 create_fun_text(?Fun:name(F),
                                              ?Fun:arity(F)),
                                 true,
                                 function)
                 || F <- Funs],
            Ans = ?Transform:question(Question),
            get_selected_entities(tl(Ans), Funs);
        true  ->
            funs_from_arg_list(NArgs)
    end.

funs_from_arg_list(Args) ->
    ?CallArg(Args, "Function list",
             [{fun funsbyname/2, [file, funlist]},
              {fun funbypos_/2, [file, position]},
              {fun funnodes/1, [nodes]}]).

%% The function returns the first selected entity.
get_selected_entity(Ans, Entities) ->
    hd(get_selected_entities(Ans, Entities)).

%% The function returns every selected entity if there is
%% any. Otherwise throws an exception.
get_selected_entities(Ans, Entities) ->
    SelectedEnts = proplists:get_all_values(yes,lists:zip(Ans, Entities)),
    case SelectedEnts of
        [] ->
            throw(?RefErr0r(cancelled));
        _  ->
            SelectedEnts
    end.


funnodes(Nodes)-> %@todo verify
    Nodes.

funsbyname(File, Funs) ->
    Mod = ?Query:exec1(?Query:seq([?File:find(File), ?File:module()]),
                       ?RefError(file_not_module, [File])),
    [?Query:exec1(Mod,
                  ?Mod:local(Fun, Arity),
                  ?RefError(fun_not_found, [Fun, Arity])) ||
        {Fun, Arity} <- Funs].

funbypos_(File,Pos) ->
    [funbypos(File,Pos,false)].

%% @spec macro(arglist()) -> node()
%% @doc Returns the source macro node. This value is specified in the
%% argument list with
%% <ul>
%%  <li>`file' and `macro' keys, or</li>
%%  <li>`file' and `position' keys.</li>
%% </ul>
macro(Args) ->
    get_arg_data(Args, fun mac_with_ask/1, fun mac_from_arg_list/1).

mac_with_ask(Args) ->
    NArgs = getFile(Args),
    case proplists:is_defined(macro, NArgs) or
        proplists:is_defined(position, NArgs) of
        false ->
            File = filebyname(proplists:get_value(file, NArgs), true),
            Macros = ?Query:exec(File, ?File:macros()),
            Qu = add_to_proplist(info, "Please specify a macro:", false),
            {_Args2, Qu2} = get_info_text(Args, Qu),
            Question = [Qu2] ++
                [add_to_proplist(radio, ?Macro:name(M), false) || M <- Macros],
            Ans = ?Transform:question(Question),
            get_selected_entity(tl(Ans), Macros);
        _     ->
            mac_from_arg_list(NArgs)
    end.

mac_from_arg_list(Args) ->
    ?CallArg(Args, "Macro",
             [{fun macbyname/3, [file, macro, ask_missing]},
              {fun macbypos/3, [file, position, ask_missing]},
              {fun macnode/1, [nodes]}]).

macnode([Node])-> %@todo verify
    Node.

macbyname(File, Macro, AskMissing) ->
    Mac = ?Query:exec(
              ?Query:seq([?File:find(File),
                          ?Macro:find(Macro)])),

    case Mac of
        [Result] -> Result;
        _        -> error_case(AskMissing,
                               {bad_macro_name, [File, Macro]},
                               {mac_not_found, [Macro]})
    end.

macbypos(File, Pos, AskMissing) ->
    T = ?Query:exec(?Query:seq([?File:find(File), ?File:token(Pos)])),

    case T of
        [_Result] ->
            case ?Query:exec([T], [{llex,back},mref]) of
                [Macro] -> Macro;
                []      ->
                    Mac =
                        ?Query:exec([T],
                                    ?Query:seq([[{{flex,back},
                                                  {type, '==', macro}}] ])),
                    case Mac of
                        [Res] -> Res;
                        _     -> error_case(AskMissing,
                                            {bad_macro_pos, [File, Pos]},
                                            {pos_bad_type, [mac, Pos]})
                    end
            end;
        _         -> error_case(AskMissing,
                                {bad_macro_pos, [File, Pos]},
                                {pos_bad_type, [mac, Pos]})
    end.

%% @spec macuse(arglist()) -> node()
%% @doc Returns the source macro use node. This value is specified in the
%% argument list with `file' and `position' keys.
macuse(Args) ->
    get_arg_data(Args, fun macuse_with_ask/1, fun macuse_from_arg_list/1).

macuse_with_ask(Args) ->
    NArgs = getFile(Args),
    NewArgs =
        case proplists:is_defined(position, NArgs) of
            false ->
                File = filebyname(proplists:get_value(file, NArgs), true),
                Macros =
                    ?Query:exec(File, ?Query:seq(?File:macros(),
                                                 [{mref,back}, {llex,2}])),
                                                % MacUses = {mref,back} -> {llex,2}
                MacData =
                    [{?Token:text(M), element(1, ?Token:pos(File, M))} ||
                        M <- Macros],
                ?Check(Macros /= [], ?RefError(no_macuse,[])),
                Qu = add_to_proplist(info, "Please specify a macro:", false),
                {_Args2, Qu2} = get_info_text(Args, Qu),
                Question = [Qu2] ++
                    [add_to_proplist(radio,
                                     io_lib:format("~s:~p", [Text, Pos]),
                                     false)
                     || {Text, Pos} <- MacData],
                Ans = ?Transform:question(Question),
                {_, Pos} = get_selected_entity(tl(Ans), MacData),
                NArgs ++ [{position, Pos}];
            true  ->
                NArgs
        end,
    macuse_from_arg_list(NewArgs).

macuse_from_arg_list(Args) ->
    ?CallArg(Args, "Macro",
             [{fun macusebypos/3, [file, position, ask_missing]},
              {fun macusenode/1, [nodes]}]).

macusenode([Node])-> %@todo verify
    Node.


macusebypos(File, Pos, AskMissing) ->
    P= ?Query:exec(
         ?Query:seq([?File:find(File),
                     ?File:token(Pos),
                     [{llex,back}]])),

    case P of
        [_Result] ->
            case ?Query:exec([P], [mref]) of
                [_] -> P;
                []  -> error_case(AskMissing,
                                  {bad_macuse_pos, [File, Pos]},
                                  {pos_bad_type, [mac, Pos]})
            end;
        _         -> error_case(AskMissing,
                                {bad_macuse_pos, [File, Pos]},
                                {pos_bad_type, [mac, Pos]})
    end.

%% @spec macros(arglist()) -> [node()]
%% @doc Returns the source macro node list. This value is specified with
%% `file' and `maclist' keys in the argument list.
macros(Args) ->
    get_arg_data(Args, fun macs_with_ask/1, fun macs_from_arg_list/1).


macs_with_ask(Args) ->
    NArgs = getFile(Args),
    case proplists:is_defined(maclist, NArgs) of
        false ->
            File = proplists:get_value(file, NArgs),
            Macros =?Query:exec(?Query:seq(?File:find(File), ?File:macros())),
            Qu = add_to_proplist(info,
                                 "Please select the macros from the list:",
                                 false),
            {_Args2, Qu2} = get_info_text(Args, Qu),
            Question = [Qu2] ++
                [add_to_proplist(checkbox, ?Macro:name(Mac), false)
                 ||  Mac <- Macros],
            Ans = ?Transform:question(Question),
            get_selected_entities(tl(Ans), Macros);
        true  ->
            macs_from_arg_list(NArgs)
    end.

macs_from_arg_list(Args) ->
    ?CallArg(Args, "Macro list",
             [{fun macsbyname/2, [file, maclist]},
              {fun macnodes/1, [nodes]}]).

macnodes(Nodes)-> %@todo verify
    Nodes.

macsbyname(File, Macs) ->
    F = ?Query:exec1(?File:find(File), ?RefError(file_not_present, [File])),
    [?Query:exec1(F, ?File:macro(Mac),
                  ?RefError(mac_not_found, [Mac])) || Mac <- Macs].

%% @spec record(arglist()) -> node()
%% @doc Returns the source record node. This value is specified in the
%% argument list with
%% <ul>
%%  <li>`file' and `record' keys, or</li>
%%  <li>`file' and `position' keys.</li>
%% </ul>
record(Args) ->
    get_arg_data(Args, fun record_with_ask/1, fun record_from_arg_list/1).

%% @doc This function asks the user to specify a record if it isn't
%% specified yet.

record_with_ask(Args) ->
    NArgs = getFile(Args),

    case proplists:is_defined(record, NArgs) or
        proplists:is_defined(position, NArgs) of
        false ->
            File = filebyname(proplists:get_value(file, NArgs), true),
            Records = ?Query:exec(File, ?File:records()),
            Qu = add_to_proplist(info, "Please specify a record:", false),
            {_Args2, Qu2} = get_info_text(Args, Qu),
            Question = [Qu2] ++
                [add_to_proplist(radio,
                                 io_lib:format("~p",[?Rec:name(R)]),
                                 false)
                 || R <- Records],
            Ans = ?Transform:question(Question),
            get_selected_entity(tl(Ans), Records);
        _     ->
            record_from_arg_list(NArgs)
    end.

record_from_arg_list(Args) ->
    ?CallArg(Args, "Record",
             [{fun recbyname/3, [file, record, ask_missing]},
              {fun recbypos/3,  [file, position, ask_missing]},
              {fun recnode/1, [nodes]}]).

%% @doc This function asks the user to specify a record if it isn't
%% specified yet and returns an extended ArgList.

getRecord(Args) ->
    NArgs = getFile(Args),
    NewArgs =
        case proplists:is_defined(record, NArgs)
            or proplists:is_defined(position, NArgs) of
            false ->
                File = filebyname(proplists:get_value(file, NArgs), true),
                Records = ?Query:exec(File, ?File:records()),
                Qu = [{format,info},{text,"Please specify a record:"}],
                {_Args2, Qu2} = get_info_text(Args, Qu),
                Question = [Qu2] ++
                    [add_to_proplist(radio,
                                     io_lib:format("~p",[?Rec:name(R)]),
                                     false)
                     || R <- Records],
                Ans = ?Transform:question(Question),
                SelectedRec = get_selected_entity(tl(Ans), Records),
                NArgs ++ [{record, ?Rec:name(SelectedRec)}];
            _     ->
                NArgs
        end,
    NewArgs.

recnode([Node])-> %@todo verify
    Node.

recbyname(File, Record, AskMissing) ->
    Rec = ?Query:exec(
             ?Query:seq([?File:find(File),
                         ?Rec:find(Record)])),

    case Rec of
        [Result] -> Result;
        _        -> error_case(AskMissing,
                               {bad_rec_name, [File, Record]},
                               {rec_not_found, [Record]})
    end.

recbypos(File, Pos, AskMissing) ->
    Token = ?Query:exec1(
                 ?Query:seq(?File:find(File),
                            ?File:token(Pos)),
                 ?RefError(no_token, Pos)),
    Rec = [Form || Form <- ?Query:exec(Token, ?Token:form()),
                   ?Form:type(Form) == record], %rec-ben hasznalt rec?
    case ?Query:exec(Rec, ?Form:record()) of
        [Result] -> Result;
        []       -> Record = ?Query:exec(Token,
                                         ?Query:seq([?Token:expr(),
                                                     ?Expr:record()])),
                    case Record of
                        [Result] -> Result;
                        _        -> error_case(AskMissing,
                                               {bad_rec_pos, [File, Pos]},
                                               {pos_bad_type, [rec, Pos]})
                    end
    end.


%% @spec record_field(arglist()) -> node()
%% @doc Returns the source record field node. This value is
%% specified in the argument list with
%% <ul>
%%  <li>`file', `record' and `recfield' keys, or</li>
%%  <li>`file' and `position' keys.</li>
%% </ul>

record_field(Args) ->
    get_arg_data(Args, fun recfield_with_ask/1, fun recfield_from_arg_list/1).

recfield_with_ask(Args) ->
    NArgs = getRecord(Args),
    case proplists:is_defined(recfield, NArgs) or
        proplists:is_defined(position, NArgs) of
        false ->
            File = proplists:get_value(file, NArgs),
            Record = recbyname(File, proplists:get_value(record, NArgs),
                               ask_missing(NArgs)),
            RecFields = ?Query:exec(Record, ?Rec:fields()),
            Qu = add_to_proplist(info,"Please specify a record field:",false),
            {_Args2, Qu2} = get_info_text(Args, Qu),
            Question = [Qu2] ++
                [add_to_proplist(radio,
                                 io_lib:format("~p", [?RecField:name(RF)]),
                                 false)
                 || RF <- RecFields],
            Ans = ?Transform:question(Question),
            get_selected_entity(tl(Ans), RecFields);
        _     ->
            recfield_from_arg_list(NArgs)
    end.

recfield_from_arg_list(Args) ->
    ?CallArg(Args, "Record field",
             [{fun recfieldbyname/4, [file, record, recfield, ask_missing]},
              {fun recfieldbypos/3,  [file, position, ask_missing]},
              {fun fldnode/1, [nodes]}]).

fldnode([Node])-> %@todo verify
    Node.

recfieldbyname(File, Record, Field, AskMissing) ->
    R = ?Query:exec(
           ?Query:seq([?File:find(File),
                       ?Rec:find(Record)])),

    Rec = case R of
              [Result] -> Result;
              _        -> error_case(AskMissing,
                                     {bad_rec_name, [File, Record]},
                                     {rec_not_found, [Record]})
          end,

    RecField = ?Query:exec(Rec, ?Rec:field(Field)),
    case RecField of
        [Res] -> Res;
        _     -> error_case(AskMissing,
                            {bad_recfield_name, [File, ?Rec:name(Rec),Field]},
                            {recfld_not_found, [?Rec:name(Rec),Field]})
    end.

recfieldbypos(File, Pos, AskMissing) ->
    [Token] = ?Query:exec(?Query:seq(?File:find(File), ?File:token(Pos))),
    case ?Query:exec(Token, ?Query:seq(?Token:typexp(), ?Expr:fielddef())) of
        [Field]  -> Field;
        []       ->
            RecField = ?Query:exec(Token,
                                   ?Query:seq(?Token:expr(), ?Expr:field())),
            case RecField of
                [Result] -> Result;
                _        -> error_case(AskMissing,
                                       {bad_recfield_pos, [File, Pos]},
                                       {pos_bad_type, [recfield, Pos]})
            end
    end.

%% @spec records(arglist()) -> [node()]
%% @doc Returns the source record node list. This value is specified with
%% `file' and `reclist' keys in the argument list.
records(Args) ->
    get_arg_data(Args, fun records_with_ask/1, fun records_from_arg_list/1).

records_with_ask(Args) ->
    NArgs = getFile(Args),
    case proplists:is_defined(reclist, NArgs) of
        false ->
            File = proplists:get_value(file, NArgs),
            Records =?Query:exec(?Query:seq(?File:find(File), ?File:records())),
            Qu = add_to_proplist(info,
                                 "Please select the records from the list:",
                                 false),
            {_Args2, Qu2} = get_info_text(Args, Qu),
            Question = [Qu2] ++
                [add_to_proplist(checkbox,
                                 io_lib:format("~p", [?Rec:name(Rec)]),
                                 false)
                 ||  Rec <- Records],
            Ans = ?Transform:question(Question),
            get_selected_entities(tl(Ans), Records);
        true  ->
            records_from_arg_list(NArgs)
    end.

records_from_arg_list(Args) ->
    ?CallArg(Args, "Record list",
             [{fun recsbyname/2, [file, reclist]},
              {fun recnodes/1, [nodes]}]).

recnodes(Nodes)-> %@todo verify
    Nodes.

recsbyname(File, Recs) ->
    F = ?Query:exec1(?File:find(File), ?RefError(file_not_present, [File])),
    [?Query:exec1(F, ?File:record(Rec),
                  ?RefError(rec_not_found, [Rec])) || Rec <- Recs].

%% @spec variable(arglist()) -> node()
%% @doc Returns the source variable node. This value is specified with
%% `file' and `position' keys in the argument list.
variable(Args) ->
    ?CallArg(Args, "Variable",
             [{fun varbypos/2, [file, position]},
              {fun varnode/1, [nodes]}]).

varnode([Node])-> %@todo verify
    Node.

varbypos(File, Pos) ->
    ?Query:exec1(
        ?Query:seq([?File:find(File),
                    ?File:token(Pos),
                    ?Token:expr(),
                    ?Expr:variables()]),
        ?RefError(pos_bad_type, ['variable',Pos])).

variable(Args, Transformation) ->
    NArgs = getFile(Args),
    NewArgs = NArgs ++ [{transform, Transformation}],

    ?CallArg(NewArgs, "Variable",
             [{fun varbypos/4, [file, position, ask_missing, transform]},
              {fun varnode/1, [nodes]}]).

get_var_data(Var, File) ->
    [Token] = ?Query:exec(Var, ?Query:seq(?Var:bindings(), [elex])),
    {_, Pos} = ?Token:pos(File, Token),
    {Pos, ?Var:name(Var)}.
%%    ?d(io_lib:format("~s:~p", [?Var:name(V), Pos])),
%%    add_to_proplist(radio, io_lib:format("~s:~p", [?Var:name(V), Pos]), false).

varbypos(File, Pos, AskMissing, Transformation) ->
    Variable = ?Query:exec(
                   ?Query:seq([?File:find(File),
                               ?File:token(Pos),
                               ?Token:expr(),
                               ?Expr:variables()])),
    case Variable of
        [Result] -> Result;
        _        ->
            case AskMissing of
                true  ->
                    ErrorText = "The given position ("
                        ++ integer_to_list(Pos) ++
                        ") has to indicate a variable.\n",
                    [FileNode] = ?Query:exec(?File:find(File)),
                    FunCl = ?Query:exec1(FileNode,
                                         ?Query:seq([?File:token(Pos),
                                                     ?Token:expr(),
                                                     ?Expr:clause()]),
                                         ?LocalError(not_inside_funcl, [])),
                    FunForm = ?Query:exec(FunCl, ?Clause:form()),
                    AllVars = ?Query:exec(FunForm,
                                          ?Query:seq([?Form:clauses(),
                                                      ?Clause:variables()])),
                    PatternVars = ?Query:exec(FunForm,
                                              ?Query:seq([?Form:clauses(),
                                                          ?Clause:patterns(),
                                                          ?Expr:varbinds()])),
                    Vars =
                        case Transformation of
                            eliminate -> AllVars -- PatternVars;
                            rename    -> AllVars;
                            _         -> throw(?RefErr0r(unknown_exception))
                        end,
                    ?Check(Vars /= [], ?RefError(no_var, [])),
                    VarsData =
                        [ get_var_data(V, FileNode) || V <- Vars],
                    Qu = add_to_proplist(info,
                                         ErrorText ++
                                             "Please specify a variable:",
                                         false),
                    Question = [Qu] ++
                        [add_to_proplist(radio,
                                         io_lib:format("~s:~p",[N, P]),
                                         false)
                         || {P, N} <- VarsData],
                    Ans = ?Transform:question(Question),
                    {Position, _} = get_selected_entity(tl(Ans), VarsData),
                    variable([{file,File},{position, Position},
                              {ask_missing,true}],eliminate);
                false -> throw(?RefError(pos_bad_type, ['variable',Pos]))
            end
    end.

%% @spec varname(arglist()) -> string()
%% @doc Returns the target variable name. This value is specified with
%% a `varname' key in the argument list.
varname(Args) ->
    DefText = "Please type in a new variable name:",
    NewArgs = ask_missing(varname, Args, variable, DefText),
    ?CallArg(NewArgs, "Target variable name",
             [{fun varstr/2, [varname, ask_missing]}]).


%% Displays an error message and requests new input in a textbox.
%% If the `info_text' property is set in `Args', it is used as
%% an additonal message; otherwise, only `DefText' is shown.
%% If `Validator' is `none', it is unused.
%% The resulting value is converted by `ConverterFun'.
ask_missing(Name, Args, Validator, DefText) ->
    ask_missing(Name, Args, Validator, DefText, fun(X) -> X end).

ask_missing(Name, Args, Validator, DefText, ConverterFun) ->
    case ask_missing(Args) andalso (not proplists:is_defined(Name, Args)) of
        false ->
            Args;
        true  ->
            Val =
                case Validator of
                    none -> [];
                    _    -> [{validator,Validator}]
                end,
            Textbox =
                [{format,textbox},
                 {text, DefText}] ++
                Val ++
                [{default,-1}],
            {Args2, Textbox2} = get_info_text(Args, Textbox),
            [NewValue] = ?Transform:question([Textbox2]),
            [{Name, ConverterFun(NewValue)} | proplists:delete(missing_text, Args2)]
    end.

%% If `Args' has an `info_text' property, removes it and puts it before
%% the `text' of the `Textbox'.
%% It also prefixes the text with `transformation_text' from `Args'
%% (and does not remove this element).
get_info_text(Args, Textbox) ->
    Text    = proplists:get_value(text, Textbox),
    TrTexts = proplists:get_all_values(transformation_text, Args),
    TrText  = ?MISC:join(lists:reverse(TrTexts), "\n"),
    NewText =
        case proplists:get_value(info_text, Args) of
            undefined -> ?MISC:flatjoin([TrText, Text], "\n");
            InfoText  -> ?MISC:flatjoin([TrText, InfoText, Text], "\n")
        end,
    NewTextbox = [case Elem of
                    {text, _} -> {text, NewText};
                    _         -> Elem
                  end || Elem <- Textbox],
    NewArgs    = proplists:delete(info_text, Args),
    {NewArgs, NewTextbox}.

varstr(Var, AskMissing) ->
    case ?Var:valid_name(Var) of
        true  -> Var;
        false ->
            case AskMissing of
                true  -> error_action(bad_var_name, [Var]);
                false -> throw(?RefErr0r(bad_var_name))
            end
    end.

%% @spec expression(arglist()) -> node()
%% @doc Returns the source expression. This value is specified with
%% `file' and `position' keys in the argument list.
expression(Args) ->
    ?CallArg(Args, "Source expression",
             [{fun exprbypos/2, [file, position]},
              {fun exprnode/1, [nodes]}]).

exprnode([Node])-> %@todo verify
    Node.

exprbypos(File, Pos) ->
    ?Query:exec1(
       ?Query:seq([?File:find(File),
                   ?File:token(Pos),
                   ?Token:expr()]),
       ?RefError(token_parent, [expr])).

clause(Args)->
    ?CallArg(Args, "Source clause",
             [{fun clausebypos/2, [file, position]},
              {fun clausenode/1, [node_list]}]).

clausenode([Node])->
    case ?Syn:node_type(Node) of
        clause ->
            Node;
        expr ->
            ?Query:exec1(?Query:seq([Node, ?Expr:clause(), ?Clause:funcl()]),
                         ?RefError(token_parent, [expr]));


        _ ->
            ?RefError(token_parent, [expr])
    end.


clausebypos(File, Pos) ->
    ?Query:exec1(
      ?Query:any([
                  %clause body
                   ?Query:seq([?File:find(File),
                               ?File:token(Pos),
                               ?Token:expr(),
                               ?Expr:clause()]),
                   %clause head
                   ?Query:seq([?File:find(File),
                               ?File:token(Pos),
                               ?Token:expr(),
                               ?Expr:nameof()])
                   ]),
       ?RefError(illegal_pos, [File, Pos])).

%% @spec file(arglist()) -> node()
%% @doc Returns the source file node. This value is specified with
%% a `file' key in the argument list.
file(Args) ->
    get_arg_data(Args, fun file_with_ask/1, fun file_from_arg_list/1).

file_with_ask(Args) ->
    case proplists:is_defined(file, Args) of
        false ->
            Question =
                add_to_proplist(textbox,
                                "Please type in the path of source file:",
                                -1,
                                module),
            {_, Question2} = get_info_text(Args, Question),
            [NewFile] = ?Transform:question([Question2]),
            filebyname(NewFile, true);
        _     ->
            file_from_arg_list(Args)
    end.


file_from_arg_list(Args) ->
    ?CallArg(Args, "Source file",
             [{fun filebyname/2, [file, ask_missing]},
              {fun filenode/1, [nodes]}]).

%% This function asks the user to specify a source file if it isn't
%% specified yet and returns the extended ArgList.
getFile(Args) ->
    case (not ask_missing(Args)) orelse proplists:is_defined(file, Args) of
        false ->
            Question =
                add_to_proplist(textbox,
                                "Please type in the path of source file:",
                                -1,
                                module),
            {_, Question2} = get_info_text(Args, Question),
            [NewFile] = ?Transform:question([Question2]),
            FileNode = filebyname(NewFile, true),
            [{file, ?File:path(FileNode)} | Args];
        _     ->
            Args
    end.

filenode([Node])-> %@todo verify + don't lookup here
    case ?Syn:node_type(Node) of
    file ->
        Node;
    _ ->
         hd([File || {file, File} <- ?Syn:root_path(Node)])
    end.

filebyname(File, AskMissing) ->
    F = ?Query:exec(?File:find(File)),

    case F of
        [Result] -> Result;
        _        -> error_case(AskMissing,
                               {bad_file, [File]},
                               {file_not_present, [File]})
    end.

%% @spec form(arglist()) -> node()
%% @doc Returns the form node. This value is specified with
%% a `file' and `position' keys in the argument list.
form(Args) ->
    ?CallArg(Args, "Form", [{fun formbypos/2, [file, position]}]).

formbypos(File, Pos) ->
    ?Query:exec1(
       ?Query:seq([?File:find(File),
                   ?File:token(Pos),
                   ?Token:form()]),
       ?RefError(illegal_pos, [File, Pos])).

import_form(Args) ->
    get_arg_data(Args, fun impform_with_ask/1, fun impform_from_arg_list/1).

impform_with_ask(Args) ->
    NArgs = getFile(Args),
    case proplists:is_defined(position, NArgs) of
        false ->
            Forms = ?Query:exec(
                       ?Query:seq([?File:find(proplists:get_value(file,NArgs)),
                                   ?File:forms()])),
            Imports = lists:filter(fun(F) -> ?Form:type(F) == import end,Forms),
            Qu = add_to_proplist(info, "Please specify an import:", false),
            {_, Qu2} = get_info_text(Args, Qu),
            Question = [Qu2] ++
                [add_to_proplist(radio, create_impform_text(F), false)
                 || F <- Imports],
            Ans = ?Transform:question(Question),
            get_selected_entity(tl(Ans), Imports);
        true  -> impform_from_arg_list(NArgs)
    end.

impform_from_arg_list(Args) ->
    ?CallArg(Args, "Import form",
            [{fun import_form_bypos/3, [file, position, ask_missing]},
             {fun impnode/1, [nodes]}]).

impnode([Node])-> %@todo verify
    Node.

import_form_bypos(File, Position, AskMissing) ->
    Form = ?Query:exec(
               ?Query:seq([?File:find(File),
                           ?File:token(Position),
                           ?Token:form()])),
    case Form of
        [Result] ->
            case ?Form:type(Result) == import of
                true  -> Result;
                false -> error_case(AskMissing,
                                    {bad_importform_pos, [File, Position]},
                                    {illegal_pos, [File, Position]})
            end;
        _        ->
            error_case(AskMissing,
                       {bad_importform_pos, [File, Position]},
                       {illegal_pos, [File, Position]})
    end.

create_impform_text(Form) ->
    [Module] = ?Query:exec(Form, ?Form:expr(1)),
    [FormToken] = ?Query:exec(Form, [{flex,2}]),
    FunList =
        ?Query:exec(Form, ?Query:seq([?Form:expr(2),
                                      ?Expr:children(),
                                      ?Expr:function()])),
    FunInfos = [ [?Fun:name(Fun), ?Fun:arity(Fun)] || Fun <- FunList],
    FunsText = ["[" | ?MISC:funlist_text(FunInfos)] ++ "]",
    io_lib:format("~p:~s:~s",
                  [element(1, ?Token:pos(FormToken)),
                   ?Expr:value(Module), FunsText]).

%% @spec macname(arglist()) -> string()
%% @doc Returns the target macro name. This value is specified with
%% a `macname' key in the argument list.
macname(Args) ->
    DefText = "Please type in a new macro name:",
    NewArgs = ask_missing(macname, Args, macro, DefText, fun convert_mac_name/1),
    ?CallArg(NewArgs, "Target macro name", [{fun macrostr/1, [macname]}]).


convert_mac_name(NewName)when is_atom(NewName) ->
    NewName;
convert_mac_name(NewName) ->
    case string:to_upper(hd(NewName)) =:= hd(NewName) of
        false -> list_to_atom(NewName);
        _     -> NewName
    end.

macrostr(Name) when is_atom(Name) ->
    io_lib:write(Name);
macrostr(Name) when is_list(Name) ->
    case ?Var:valid_name(Name) of
        true  -> Name;
        false -> throw(?RefErr0r(bad_mac_name))
    end.

%% @spec module(arglist()) -> node()
%% @doc Returns the source module node. This value is specified in the
%% argument list with
%% <ul>
%%  <li>a `module' key, or</li>
%%  <li>a `file' key.</li>
%% </ul>
module(Args) ->
    get_arg_data(Args, fun module_with_ask/1, fun module_from_arg_list/1).

module_with_ask(Args) ->
    case proplists:is_defined(module, Args)
        or proplists:is_defined(file, Args) of
        false ->
            Question = add_to_proplist(textbox,
                                       "Please type in the module name:",
                                       -1,
                                       module),
            {_, Question2} = get_info_text(Args, Question),
            [NewMod] = ?Transform:question([Question2]),
            modbyname(list_to_atom(NewMod), true);
        true  ->
            module_from_arg_list(Args)
    end.

module_from_arg_list(Args) ->
    ?CallArg(Args, "Source module",
             [{fun modbyname/2, [module, ask_missing]},
              {fun modbypos/3,  [file, position, ask_missing]},
              {fun modbyfile/2, [file, ask_missing]},
              {fun modnode/1, [nodes]}]).

%% This function asks the usert to specify the module, if there is no
%% module or file specified in the ArgList and returns the extended
%% ArgList.
getModule(Args) ->
    case proplists:is_defined(module, Args) or
        proplists:is_defined(file, Args) of
        false ->
            Question = add_to_proplist(textbox,
                                       "Please type in the module name:",
                                       -1,
                                       module),
            {_, Question2} = get_info_text(Args, Question),
            [NewMod] = ?Transform:question([Question2]),
            NewModName = modbyname(list_to_atom(NewMod), true),
            [{module, ?Mod:name(NewModName)} | Args];
        true  ->
            Args
    end.

modnode([Node])-> %@todo verify
    Node.

modbyname(Mod, AskMissing) ->
    Module = ?Query:exec(?Mod:find(Mod)),

    case Module of
        [Result] -> Result;
        _        -> error_case(AskMissing,
                               {bad_mod_name, [Mod]},
                               {mod_not_found, [Mod]})
    end.

modbypos(File, Pos, AskMissing) ->
    Mod = ?Query:exec(
              ?Query:seq( [?File:find(File),
                           ?File:token(Pos),
                           ?Token:expr(),
                           ?Expr:module() ])),

    case Mod of
        [Result] -> Result;
        _        -> error_case(AskMissing,
                               {bad_mod_pos, [File, Pos]},
                               {pos_bad_type, [module, Pos]})
    end.

modbyfile(File, AskMissing) ->
    Mod = ?Query:exec(
             ?Query:seq([?File:find(File),
                         ?File:module()])),

    case Mod of
        [Result] -> Result;
        _        -> error_case(AskMissing,
                               {bad_file, [File]},
                               {file_not_module, [File]})
    end.

% @todo document
moduse(Args) ->
    get_arg_data(Args, fun moduse_with_ask/1, fun moduse_from_arg_list/1).

moduse_with_ask(Args) ->
    NArgs = getFile(Args),
    case proplists:is_defined(position, Args)
        or proplists:is_defined(module, Args) of
        false ->
            File = filebyname(proplists:get_value(file, NArgs), false),
            [SourceMod] = ?Query:exec(File, ?File:module()),
            Modules = ?Query:exec(File,
                                  ?Query:seq([?File:forms(),?Form:func(),
                                              ?Fun:funcalls(),?Fun:module()])),
            ?Check(Modules /= [], ?RefError(no_moduse, [])),
            ModUses =
                lists:filter(fun(M) -> ?Mod:name(M) /= ?Mod:name(SourceMod)
                                           andalso
                                           ?Mod:name(M) /= erlang end, Modules),
            ?Check(ModUses /= [], ?RefError(no_moduse, [])),
            Qu = add_to_proplist(info,
                                 "Please specify a module to import:",
                                 false),
            Question = [Qu] ++
                [add_to_proplist(radio,
                                 io_lib:format("~p", [?Mod:name(ModUse)]),
                                 false)
                 || ModUse <- ModUses],
            Ans = ?Transform:question(Question),
            get_selected_entity(tl(Ans), ModUses);
        true ->
            moduse_from_arg_list(NArgs)
    end.

moduse_from_arg_list(Args) ->
    ?CallArg(Args, "Module usage",
             [{fun modusebyname/2, [module, ask_missing]},
              {fun modusebypos/3,  [file, position, ask_missing]},
              {fun modusenode/1, [nodes]}]).

modusenode([Node])-> %@todo verify
    Node.

modusebyname(Mod, AskMissing) ->
    Module = ?Query:exec(?Mod:find(Mod)),

    case Module of
        [Result] -> Result;
        _        -> error_case(AskMissing,
                               {bad_moduse_name, [Mod]},
                               {mod_not_found, [Mod]})
    end.

modusebypos(File, Pos, AskMissing) ->
    Mod = ?Query:exec(
              ?Query:seq( [?File:find(File),
                           ?File:token(Pos),
                           ?Token:expr(),
                           ?Expr:module() ])),

    case Mod of
        [Result] -> Result;
        _        -> error_case(AskMissing,
                               {bad_moduse_pos, [File, Pos]},
                               {pos_bad_type, [module, Pos]})
    end.

%% @spec expr_range(arglist()) -> [node()]
%% @doc Returns the source expression range. A valid expression range is
%% either a non-empty continuous list of top level expressions linked with the
%% same tag to a clause, or a single expression. This value is specified with
%% `file' and `posrange' keys in the argument list.
expr_range(Args) ->
    % {Pos,Pos1} = proplists:get_value(posrange,Args),
    % ?d({refusr_clone_identifierl:get_clones([{algorithm,matrix},{positions,[{file,Pos,Pos1}]}])}),
    ?CallArg(Args, "Expression range",
             [{fun expr_range/2,   [file, posrange]},
              {fun expr_posnumr/3, [file, position, number]},
              {fun exprnodes/1, [nodes]}]).

exprnodes(Nodes)-> %@todo verify
    Nodes.

expr_posnumr(File, Pos1, Num) ->
    ?Check(Num>=1, ?RefErr0r(bad_range)),
    IllP  = fun(Loc,Path)-> ?Query:exec1(Loc,Path,
                            ?RefError(illegal_pos, [File, Pos1])) end,
    F      = get_filenode(File),
    Start  = IllP(F, ?File:token(Pos1)),
    StPat  = IllP(Start, [{elex,back}]),
    FunCl  = IllP(StPat, [{pattern,back}]),
    FunDef = IllP(FunCl, [{funcl,back}, fundef]),
    Arity  = ?Fun:arity(FunDef),
    ?Check(Num=<Arity, ?RefErr0r(bad_range)),
    IsIdx  = fun(I) -> StPat =:= IllP(FunCl, [{pattern,I}]) end,
    {Idx,_Pos} = ?MISC:list_find(IsIdx, lists:seq(1,Arity-Num+1)),
    EndIdx = Idx+Num-1,
    ?Check((Idx>0) andalso (EndIdx=<Arity), ?RefErr0r(bad_range)),
    EndE   = IllP(FunCl, [{pattern,EndIdx}]),
    End    = hd(lists:reverse(?Query:exec(EndE,[elex]))),
    expr_range_common(Start, End).

%% todo When `End' is already in the prews of a token that is self-sufficient,
%%      e.g. `abc' in `case A of blah -> body ; abc -> ... end', it does not
%%      go back to `body', as it accepts `abc' as a rightmost token.
%% todo Should work even if a `stop' token (end of form) separates `Start' and `End'.
expr_range(File, {Pos1, Pos2}) ->
    F     = get_filenode(File),
    Start = ?Query:exec1(F, ?File:token(Pos1),
                         ?RefError(illegal_pos, [File, Pos1])),
    End   = ?Query:exec1(F, ?File:token(Pos2),
                         ?RefError(illegal_pos, [File, Pos2])),
    {Join, _Link, _LeftRest, _RightRest} =
        joint(?Syn:root_path(Start, left), ?Syn:root_path(End, right)),
    RealStart = skip_to_token(left, Join, Start),
    RealEnd   = skip_to_token(right, Join, End),
    expr_range_common(RealStart, RealEnd).

%% Skips to the first token that begins/ends an expression,
%% depending on whether `Side' is `left' or `right'.
%% Since `Start..End' is a range within the leaves of `Join',
%% only those nodes need to be checked.
skip_to_token(Side, Join, Node) ->
    case is_side_token(Side, Node) of
        true ->
            Node;
        false ->
            Tokens = ?Syn:leaves(Join),
            {Prevs, [Node|Afters]} =
                lists:splitwith(fun(Token) -> Token =/= Node end, Tokens),
            Nexts =
                case Side of
                    right -> lists:reverse(Prevs);
                    left  -> Afters
                end,
            % Note: some constructs have a rightmost token at unexpected places.
            % E.g.  `x' is rightmost, but is not represented as such in
            %       f() -> #rec{x=0}.
            case lists:dropwhile(fun(Token) -> not is_side_token(Side, Token) end, Nexts) of
                [OKToken|_] -> OKToken;
                []          -> Node
            end
    end.

%% Returns whether the token is a leftmost/rightmost child of its parent.
is_side_token(Side, Node) ->
    TokenIdxFun =
        case Side of
            left  -> fun hd/1;
            right -> fun lists:last/1
        end,
    [{_, Parent} | _] = ?Syn:parent(Node),
    {_, Child} = TokenIdxFun(?Syn:children(Parent)),
    Child == Node.

get_filenode(File) ->
    ?Query:exec1(?File:find(File), ?RefError(file_not_present, [File])).

expr_range_common(Start, End) ->
    Exprs = range(joint(?Syn:root_path(Start, left),
                        ?Syn:root_path(End, right))),
    [First] = ?Query:exec(hd(Exprs), ?Syn:first_leaf()),
    [Last]  = ?Query:exec(lists:last(Exprs), ?Syn:last_leaf()),
    ?Check(First =:= Start andalso Last =:= End,
           ?RefError(bad_range, [Start, End, First, Last])),
    Exprs.

%% Turns the result of `joint/2' into a list of expressions.
range({Expr, esub, F, L}) ->
    Chld = ?Syn:children(Expr),
    case {hd(Chld), lists:last(Chld)} of
        {{esub,F}, {esub,L}} -> [Expr];
        _ ->
            FI = ?Syn:index(Expr, esub, F),
            LI = ?Syn:index(Expr, esub, L),
            ?ESG:path(Expr, [{esub, {FI, LI+1}}])
    end;
range({Expr, elex, _, _}) ->
    [Expr];
range({Cls,  CT, F, L}) when CT == body; CT == pattern; CT == guard ->
    FI = ?Syn:index(Cls, CT, F),
    LI = ?Syn:index(Cls, CT, L),
    ?ESG:path(Cls, [{CT, {FI, LI+1}}]);
range({Lex, llex, _, _}) ->
    %% No multiple parents here
    [{Tag, Parent}] = ?Syn:parent(Lex),
    range({Parent, Tag, Lex, Lex});
range(_) ->
    throw(?RefErr0r(bad_range)).

%% @spec joint(P1::[{atom(), node()}], P2::[{atom(), node()}]) ->
%%         {node(), atom(), node(), node()}
%% @doc Returns the fork point of the argument paths. `P1' and `P2' must have
%% a common prefix which is continued with the same link tag (the prefix may
%% be empty). The returned tuple contains the last common node after the
%% longest such prefix, the common link tag, and the continuing nodes (they
%% may be the same when the two paths are continued with different link tags).
joint([{L,N}=H|T1], [H|T2])     -> joint(T1, T2, {?Graph:root(), L, N});
joint([{L,N1}|_],   [{L,N2}|_]) -> {?Graph:root(), L, N1, N2};
joint(_, _)                     -> throw(?RefErr0r(bad_range)).

%% Drop the common prefix. The third argument contains the last two common
%% nodes and the link tag between them.
joint([{L,N}=H|T1], [H|T2],      {_,_,P}) -> joint(T1, T2, {P,L,N});
joint([{L, N1}|_],  [{L, N2}|_], {_,_,P}) -> {P, L, N1, N2};
joint(_,           _,            {P,L,C}) -> {P, L, C, C}.

%%%======================================================================
%%% Clustering args

%% @spec clustering_args_base(proplist(), atom(), [atom()], string(),
%%                             [string()], atom(), value(), fun()) -> value()
%%
%% @doc This function returns the appropriate argument from the proplist.
%%      If it is already in there, it returns the value, if it is not,
%%      it forwards the parameters.
clustering_args_base(Args, ArgType, OptionAtoms, MainText,
                     OptionString, QuestionStyle,
                     Default, Validator) ->
    case proplists:get_value(ArgType, Args, undefined) of
        undefined -> clustering_args_question_base(Args, ArgType, OptionAtoms,
									MainText, OptionString, QuestionStyle,
                                    Default, Validator);
        Value -> Value
    end.

%% @spec clustering_args_question_base(proplist(), atom(), [atom()], string(),
%%                             [string()], atom(), value(), fun()) -> value()
%%
%% @doc This function returns the appropriate argument from the proplist.
%%      It is not in the list, so it asks the user, or returns default value,
%%      if it is not allowed to ask.
clustering_args_question_base(Args, ArgType, OptionAtoms, MainText,
                     OptionString, QuestionStyle,
                     Default, Validator) ->
    case proplists:get_value(ask_missing, Args) of
        true ->
            NewArgs =
                case proplists:is_defined(ArgType, Args) of
                    false -> Question =
                        case QuestionStyle of
                            radio ->
                                Qu = add_to_proplist(info, MainText,
                                                     false,
                                                     Validator),
                                {_, Qu2} = get_info_text(Args, Qu),
                                [Qu2] ++
                                    [add_to_proplist(radio, Option,
                                                     false,
                                                     Validator)
                                        || Option <- OptionString];
                            %yesno ->
                            %    [add_to_proplist(yesno, MainText,
                            %                     false ,Validator)];
                            %select ->
                            %    SelectString = io_lib:format(
                            %                       " - Select from: ~p",
                            %                       [OptionString]),
                            %    [add_to_proplist(textbox,
                            %                         MainText++SelectString,
                            %                         Default,
                            %                         Validator)];
                            type ->
                                [add_to_proplist(textbox,
                                                     MainText,
                                                     Default,
                                                     Validator)]

                            %string ->
                            %    DefaultString = io_lib:format(" (Default: ~p)",
                            %                                  [Default]),
                            %    [[{format, textbox},
                            %     {type, string},
                            %     {text, MainText++DefaultString},
                            %     {default, Default},
                            %     {validator,Validator}]]
                        end,
                        %?d(Question),
                        Ans = ?Transform:validated_question(Question),
                        case QuestionStyle of
                            radio ->
                                AnsAtom = get_selected_entity(tl(Ans),
                                                              OptionAtoms);
                            %select -> [Temp] = Ans,
                            %     AnsAtom = list_to_atom(Temp);
                            _ -> [AnsAtom] = Ans
                        end,
                        Args ++ [{ArgType, AnsAtom}];
                    true  ->
                        Args
                end,

            %ArgValue =
                proplists:get_value(ArgType, NewArgs);

            %case QuestionStyle of
                %type -> arg_val_for_type(ArgValue, {Validator, Converter},
                                         %CallerFunction);
                %_ -> arg_val(ArgValue, OptionAtoms, CallerFunction)
            %end;
        false ->
			Val = proplists:get_value(ArgType, Args, undefined),
            case Val of
                undefined -> Default;
                _ -> Val
            end
    end.

%arg_val(ArgValue, Options, Function) ->
    %case lists:member(ArgValue, Options) of
        %true -> ArgValue;
        %false -> Function([{ask_missing, true}])
    %end.

%arg_val_for_type(ArgValue, {Validator,Converter}, Function) ->
    %case Validator(ArgValue) of
        %{true, _} -> Converter(ArgValue);
        %_ -> Function([{ask_missing, true}])
    %end.

%%%======================================================================
%% The following functions describe clustering parameters.

algorithm(Args) ->
    clustering_args_base(Args, algorithm, [agglom_attr,genetic],
                         "Please select the clustering algorithm",
                         ["Agglomerative", "Genetic"], radio, agglom_attr,
                         fun default_validator/1).

entity(Args) ->
    clustering_args_base(Args, entities, [module, function],
                         "Please select the clustering entities",
                         ["Module", "Function"], radio, module,
                         fun default_validator/1).

decomposition(Args) ->
    clustering_args_base(Args, decompose, [yes,no],
                         "May I offer a decomposition of the modules?",
                         ["Yes","No"], radio, no,
                         fun default_validator/1).

transformfun(Args) ->
    clustering_args_base(Args, transformfun, [zero_one, none],
                         "Choose transform function",
                         ["Zero or one","None"], radio, none,
                         fun default_validator/1).

mergefun(Args) ->
    clustering_args_base(Args, mergefun, [smart],
                         "Choose merge function",
                         ["Smart"], radio, smart,
                         fun default_validator/1).

antigravity(Args) ->
    clustering_args_base(Args, anti_gravity, [],
                         "Antigravity (Default:0.5)", [], type, 0.5,
                         fun check_probability/1).

distfun(Args) ->
    clustering_args_base(Args, distfun, [weight, call_sum],
                         "Choose distance function",
                         ["Weight", "Call Sum"], radio, weight,
                         fun default_validator/1).

population_size(Args) ->
    clustering_args_base(Args, population_size, [],
                         "Choose population size",[],  type, 12,
                         fun check_integer/1).

iterations(Args) ->
    clustering_args_base(Args, iterations, [],
                         "Choose iteration numbers", [], type, 10,
                         fun check_integer/1).

mutation_rate(Args) ->
    clustering_args_base(Args, mutation_rate, [],
                         "Choose mutation rate", [], type, 1,
                         fun check_probability/1).

crossover_rate(Args) ->
    clustering_args_base(Args, crossover_rate, [],
                         "Choose crossover rate", [], type, 1,
                         fun check_probability/1).

elite_count(Args) ->
    clustering_args_base(Args, elite_count, [],
                         "Choose elite count", [], type, 2,
                         fun check_integer/1).

maximum_cluster_size(Args) ->
    clustering_args_base(Args, max_cluster_size, [],
                         "Choose maximum cluster size", [], type, 5,
                         fun check_integer/1).

maximum_start_cluster_size(Args) ->
    clustering_args_base(Args, max_start_cluster_size, [],
                         "Choose maximum stating cluster size",
                         [], type, 2,
                         fun check_integer/1).

library_limit(Args) ->
    clustering_args_base(Args, library_limit, [],
                         "Minimum call number for library modules",
                         [], type, 5,
                         fun check_integer/1).

headers(Args) ->
    clustering_args_base(Args, headers, [],
                         "Format of header file names", [], type, ".hrl",
                         fun default_validator/1).

show_goodness(Args) ->
    clustering_args_base(Args, show_goodness, [yes, no],
                         "Do you need the goodness values?",
                         ["Yes", "No"], radio, yes,
                         fun default_validator/1).

only_best(Args) ->
    clustering_args_base(Args, only_best, [yes, no],
                         "May I show only the best clustering solution?",
                         ["Yes", "No"], radio, no,
                         fun default_validator/1).

store_results(Args) ->
    clustering_args_base(Args, store_results, [yes, no],
                         "Do you want to store the results?",
                         ["Yes", "No"], radio, no,
                         fun default_validator/1).

cluster_options(Args) ->
    case proplists:get_value(algorithm, Args) of
        agglom_attr ->
            ClusterArgs = proplists:get_value(cluster_options, Args, [])
                          ++ [proplists:lookup(ask_missing, Args)],
            [{transformfun, transformfun(ClusterArgs)},
             {distfun, distfun(ClusterArgs)},
             {anti_gravity, antigravity(ClusterArgs)},
             {mergefun, mergefun(ClusterArgs)}];
        genetic ->
            ClusterArgs = proplists:get_value(cluster_options, Args, [])
                          ++ [proplists:lookup(ask_missing, Args)],
            [{population_size, population_size(Args)},
             {iterations, iterations(ClusterArgs)},
             {mutation_rate, mutation_rate(ClusterArgs)},
             {crossover_rate, crossover_rate(ClusterArgs)},
             {elite_count, elite_count(ClusterArgs)},
             {max_cluster_size, maximum_cluster_size(ClusterArgs)},
             {max_start_cluster_size,
                 maximum_start_cluster_size(ClusterArgs)}]
    end.

decomp_options(Args) ->
    DecompArgs = proplists:get_value(decompose_options, Args, [])
                 ++ [proplists:lookup(ask_missing, Args)],
    [{headers, [?Args:headers(DecompArgs)]}].

%%% ============================================================================
%%
%% Validator functions for clustering
%%%
%%% These functions check the answers given by the user.

default_validator(String) ->
    {true, String}.

%check_list(List) ->
    %case is_list(List) of
        %true -> {true, List};
        %false -> {false, undefined}
    %end.

%check_transformfun(String) ->
    %try list_to_existing_atom(String) of
        %none     -> {true, undefined};
        %zero_one -> {true, zero_one};
        %_Othrws  -> {false, undefined}
    %catch
        %_:_ -> {false, undefined}
    %end.

%check_distfun(String) ->
    %try list_to_existing_atom(String) of
        %weight   -> {true, weight};
        %call_sum -> {true, call_sum};
        %_Othrws  -> {false, undefined}
    %catch
        %_:_ -> check_fun(String ++ ".", 4)
    %end.

%check_mergefun(String) ->
    %try list_to_existing_atom(String) of
        %smart   -> {true, smart};
        %_Othrws -> {false, undefined}
    %catch
        %_:_ -> check_fun(String ++ ".", 3)
    %end.

%check_fun(String, Arity) ->
    %case erl_scan:string(String) of
        %{ok, Tokens, _} ->
            %case erl_parse:parse_exprs(Tokens) of
                %{ok,[{'fun', 1, {function, _Mod, _Fun, Arity}}] = Exprs} ->
                    %{value, Fun, _} = erl_eval:exprs(Exprs, []),
                    %{true, Fun};
                %{ok,[{'fun', 1, {function, _Fun, Arity}}] = Exprs} ->
                    %{value, Fun, _} = erl_eval:exprs(Exprs, []),
                    %{true, Fun};
                %_Otherwise ->
                    %{false, undefined}
            %end;
        %{error, _, _} ->
            %{false, undefined}
    %end.

check_integer(String) when is_integer(String) ->
	{true, String};
check_integer(String) ->
    case erl_scan:string(String) of
        {ok, [{integer, _, V}], _} when 0 =< V ->
            {true, V};
        _Otherwise ->
            {false, undefined}
    end.

check_probability(String) when is_float(String); is_integer(String) ->
    case (0 =< String) andalso (String =< 1) of
        true -> {true, String};
        false -> {false, undefined}
    end;
check_probability(String) ->
    case erl_scan:string(String) of
        {ok, [{float, _, V}], _} when 0 =< V, V =< 1   ->
            {true, V};
        {ok, [{integer, _, V}], _} when 0 =< V, V =< 1 ->
            {true, V};
        _Otherwise ->
            {false, undefined}
    end.


