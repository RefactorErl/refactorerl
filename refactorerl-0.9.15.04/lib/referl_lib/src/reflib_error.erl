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

%%% @doc Standard refactoring errors. This module contains the textual
%%% description of standard refactoring errors that can be thrown using the
%%% macro `?RefError'.
%%%
%%% Error terms that are thrown by refactoring modules have the
%%% following format:
%%%
%%% ```{Module, Type, Details}'''
%%%
%%% where `Module' is the "owner" module of the error (it's like a
%%% namespace for errors), `Type' is an atom that identifies the error
%%% condition, and `Details' is a list, its contents depend on `Type'.
%%%
%%% There are two macros that should be used to throw errors:
%%%
%%% <ul>
%%%
%%%  <li>`?RefError(Type, Detail)' creates a standard error term that
%%%    belongs to this module.</li>
%%%
%%%  <li>`?LocalError(Type, Detail)' creates an error that belongs to
%%%    the module that uses the macro.</li>
%%%
%%% </ul>
%%%
%%% Both should be used like this: `throw(?RefError(missing_arg, [Key]))'.
%%%
%%% @author Lovei Laszlo <lovei@inf.elte.hu>

-module(reflib_error).
-vsn("$Rev: 12913 $").

-include("lib.hrl").

%%% ============================================================================
%%% Exports

-export([error_text/1]).


%%% ============================================================================
%%% Error messages

%% @spec error_text(error()) -> String
%%       String = [char() | String]
%% @doc Returns a textual description of the error term.
error_text(X) ->
    lists:flatten(error_text_(X)).

error_text_({_ErrorInfo, Text}) ->
    Text;
error_text_({?MODULE, Type, Detail}) ->
    ?MISC:to_list(error_text(Type, Detail));
error_text_({Mod, Type, Detail}) ->
    try
        case Mod:error_text(Type, Detail) of
            unknown -> unknown_error_text(Mod, Type, Detail);
            Text    -> Text
        end
    catch
        _:_ ->
            unknown_error_text(Mod, Type, Detail)
    end;
error_text_(Error)->
    io_lib:format("~p~n", [Error]).


%% @doc  Error text in case of an unknown error.
unknown_error_text(Mod, Type, Detail) ->
    io_lib:format("Unknown error: ~p:~p~nError details: ~p",
                  [Mod, Type, Detail]).


% Removed
%error_text(ambiguous_token, _) ->
%    "The selection has multiple references";
%error_text(missing_opt, [Arg]) ->
%    ["Option ", atom_to_list(Arg), " is not specified"];
%error_text(lex_not_found, [_Path, _Pos]) ->
%    ["There is no lexical element in the given position"];

error_text(bad_dep_table, _)->
    "Cannot find the proper row in depenedency table, " ++
    "we have to rebuild the table. Please, try the query again!~n";
error_text(no_token, Pos) ->
    "There is no token specified in the given position (" ++
    integer_to_list(Pos) ++ ").";
error_text(no_var, _) ->
    "There is no variable in the specified function clause which can be transformed.";
error_text(no_macuse, _) ->
    "There is no macro usage in this module.";
error_text(no_moduse, _) ->
    "There is no imported function usage in this module.";
error_text(no_fun, _) ->
    "There is no function defined in this module.";
error_text(no_ofun, _) ->
    "There is no overloaded function defined in this module.";
error_text(not_overloaded_fun, [Funname]) ->
    atom_to_list(Funname) ++ " is not an overloaded function.";
error_text(source_and_target_equals, _) ->
    "The target module should not be the same as the source module";
error_text(module_macro_found, _) ->
    "Source contains a ?MODULE qualifier and it isn't supported yet.";
error_text(target_not_found, _) ->
    "Target module not found";
error_text(source_not_found, _) ->
    "Source module not found";
error_text(cancelled, _) ->
    "Transformation aborted by user";
error_text(yaws_not_loaded,_) ->
    "Yaws module is not loaded";
error_text(illegal_pos, [File, Pos]) ->
    ["Position ", integer_to_list(Pos), " not found in file ", File];
error_text(token_parent, [Type]) ->
    ["The selection has to be inside ", ?MISC:add_article(node_kind_text(Type))];
error_text(file_not_module, [File]) ->
    ["File ", File, " does not identify a module"];
error_text(file_not_hrl, [File]) ->
    ["File ", File, " does not identify a header file"];
error_text(rel_path, [_]) ->
    ["The path of the header file has to be an absolute path"];
error_text(mod_not_found, [Name]) ->
    ["Module '", atom_to_list(Name), "' not found"];
error_text(file_not_present, [Name]) ->
    ["File ", Name, " cannot be found in the database"];
error_text(no_file, [Type]) ->
    ["File for the ",atom_to_list(Type)," not found"];
error_text(no_file, [Type,Name]) ->
    ["File for ",atom_to_list(Type)," '", ?MISC:any_to_string(Name), "' not found"];
error_text(ambiguous_mod,[Mod]) ->
    ["Multiple files loaded with the module name ",
     io_lib:write_atom(Mod)];
error_text(form_not_found, []) ->
    ["Graph consistency error: form not found"];
error_text(fun_not_found, FunInfo) ->
    ["Function ", ?MISC:fun_text(FunInfo), " not found"];
error_text(side_effect, [FunInfo]) ->
    ["Function ", ?MISC:fun_text(FunInfo), " has side effects"];
error_text(fun_def_not_found, [Name, Arity]) ->
    ["The definition of the function is not in the database: ",
                           ?MISC:format("~p/~p",[Name, Arity])];
error_text(rec_not_found, [Name]) ->
    ["Record ", atom_to_list(Name), " not found"];
error_text(recfld_not_found, RecFld) ->
    ["Record field ", ?MISC:recfld_text(RecFld), " not found"];
error_text(mac_not_found, [Name]) ->
    ["Macro ", ?MISC:to_list(Name), " not found"];
error_text(fun_exists, FunInfo) ->
    ["Function ", ?MISC:fun_text(FunInfo), " already exists"];
error_text(var_exists, VarName) ->
    ["Variable ", VarName, " already exists"];
error_text(rec_exists, RecName) ->
    ["Record ", RecName, " already exists"];
error_text(mac_exists, RecName) ->
    ["Macro ", RecName, " already exists"];
error_text(imported_fun_exists, [_Mod, FunInfo]) ->
    ["Function ", ?MISC:fun_text(FunInfo), " is already imported"];
error_text(autoimported_fun_exists, FunInfo) ->
    ["Function ", ?MISC:fun_text(FunInfo), " is autoimported"];
error_text(missing_arg, [Desc]) ->
    ["Missing argument: ", Desc];
error_text(arg_type, [Name, Type]) ->
    ["Argument \"", atom_to_list(Name), "\" has to be ", ?MISC:add_article(Type)];
error_text(pos_bad_type, [ExpectedType, Pos]) ->
    ["The given position (", integer_to_list(Pos), ") ",
     "has to indicate ", ?MISC:add_article(node_kind_text(ExpectedType))];
error_text(bad_order, [Length]) ->
    ["The given order should have all values from 1 to ", integer_to_list(Length)];
error_text(order_not_list, []) ->
    ["The new order has to be given as a list"];
error_text(order_arity_mismatch, [Arity]) ->
    ["The given order should have the same arity as the function: ",
     integer_to_list(Arity)];
error_text(bad_range, [Start, End, DeterminedStart, DeterminedEnd]) ->
    try
        #lex{data=#token{text=StartText}} = ?Graph:data(Start),
        #lex{data=#token{text=EndText}} = ?Graph:data(End),
        {{S1, S2}, _}        = ?Token:linecol(Start),
        {       _, {E1, E2}} = ?Token:linecol(End),

        #lex{data=#token{text=DStartText}} = ?Graph:data(DeterminedStart),
        #lex{data=#token{text=DEndText}} = ?Graph:data(DeterminedEnd),
        {{DS1, DS2}, _}          = ?Token:linecol(DeterminedStart),
        {         _, {_DE1, DE2}} = ?Token:linecol(DeterminedEnd),


        case S1 == E1 of
            true ->
                [?MISC:format("Range ~p:~p-~p (~s...~s) is not an expression/range of expressions~n", [S1, S2, E2, StartText, EndText]),
                 ?MISC:format("note: the smallest containing range is ~p:~p-~p (~s...~s)", [DS1, DS2, DE2, DStartText, DEndText])];
            false ->
                [?MISC:format("Range ~p:~p-~p:~p (~s...~s) is not an expression/range of expressions", [S1, S2, E1, E2, StartText, EndText])]
        end
    catch
        _ ->
            ["A valid expression range has to be selected"]
    end;
error_text(bad_range, []) ->
    ["A valid expression range has to be selected"];
error_text(outside_used_vars, List) ->
    ["Variable used outside the selection: ",
     ?MISC:separated_text(List)];
error_text(var_exists_app, List)->
    ["No visible names may be used: ", ?MISC:separated_text(List)];
error_text(bad_kind, []) ->
    ["The selection is not acceptable for this transformation"];
error_text(bad_kind, BadKind) ->
    ["The selection is ", ?MISC:add_article(node_kind_text(BadKind)),
     ", which is not acceptable for this transformation"];
error_text(parent_not_form, []) ->
    ["The selection has to be a valid part of a function body"];
error_text(recursive_subexpr, []) ->
    ["The selection may not contain recursive calls"];
error_text(bad_var_name, []) ->
    ["Illegal variable name given"];
error_text(bad_mac_name, []) ->
    ["Illegal macro name given"];
error_text(quoted_atom, []) ->
    ["Quoted atoms are not allowed"];
error_text(module_exists,[Name]) ->
    ["Collision with existing module \"", atom_to_list(Name), "\""];
error_text(file_exists,[Path]) ->
    ["Collision with existing file \"", Path, "\""];
error_text(file_notexists,[Path]) ->
    ["File does not exist: \"", Path, "\""];
error_text(file_notdir,[Path]) ->
    ["Path is not a directory: \"", Path, "\""];
error_text(file_acces,[FilePath,Mode]) ->
    ["Cannot access the file in ", ?MISC:format("~p",[Mode]),
     " mode: \"", FilePath, "\""];
error_text(file_open,[FilePath,Reason]) ->
    ["Cannot open the file: \"", FilePath, "\" (",
     ?MISC:format("~p", [Reason]), ")"];
error_text(file_eof,[FilePath,Reason]) ->
    ["Unexpected end of file: \"", FilePath, "\" (",
     ?MISC:format("~p", [Reason]), ")"];
error_text(file_load_wrong_datatype,[FilePath,Reason]) ->
    ["File contains wrong data: \"", FilePath,
     "\". The required data type is ", ?MISC:format("~p", [Reason]), "."];
error_text(file_error,[FilePath,Reason]) ->
    ["Cannot process file: \"", FilePath, "\", reason: ",
     ?MISC:format("~p", [Reason])];
error_text(incompat,[FunName])->
    ["The metric type '"
     ,?MISC:format("~p", [FunName])
     ,"' is incompatible with the given node"];
error_text(metric_fun,[Metric])->
    ["Unknown metric function: ",?MISC:format("~p", [Metric])];
error_text(m_parser_error, [Mesg])->
    ["Parse error in the Metrics Query: ",?MISC:format("~p", [Mesg])];
error_text(m_scanner_error, [Err])->
    ["Lexical error in the Metrics Query: ",?MISC:format("~p", [Err])];
error_text(m_bad_filter, [Filter])->
    ["Bad filter: ",?MISC:format("~p", [Filter])];
error_text(sq_metric_error, [Err])->
    ["Error in metric property: ",?MISC:format("~p", [Err])];
error_text(mac_error, _Virtuals)->
    % todo Use `Virtuals' to provide more information about the ambiguity.
    ["The transformation is denied because of an ambiguous macro substitution"];
error_text(list_to_integer_error, [String])->
    ["Cannot convert string to integer: ",String];
error_text(port_format_error, [Port])->
    ["Not a valid port format (have to be a number): ", Port];
error_text(ip_format_error, [IP])->
    ["Not a valid IP format: ",IP];
error_text(name_format_error, [Name])->
    ["Not a valid server name: ",Name];
% used in renaming reftr
error_text(new_varname_identical, Name)->
    ["New varname identical to the old one: ",Name];
error_text(new_funname_identical, Name)->
    ["New function name identical to the old one: ",atom_to_list(Name)];
error_text(new_fieldname_identical, Name)->
    ["New fieldname identical to the old one: ",atom_to_list(Name)];
error_text(new_hrl_name_identical, Path)->
    ["New header name and path identical to the old one: ",Path];
error_text(new_macname_identical, Name)->
    ["New macro name identical to the old one: ",Name];
error_text(new_modname_identical, [Name])->
    ["New module name identical to the old one: ", atom_to_list(Name)];
error_text(new_rec_name_identical, [Name])->
    ["New record name identical to the old one: ", atom_to_list(Name)];
error_text(underscore_not_allowed, [Name])->
    ["A used variable cannot be replaced with an underscore: ", Name];
error_text(deny, _)->
    ["The request was denied by the job server. Please, try again later."];

error_text(ErrType, ErrParams) ->
    ["Unknown error: {",
     io_lib:print(ErrType), ", ", io_lib:print(ErrParams), "}"].




%% @doc The textual representation of a node type or kind.
%% If several node types are given in a list, the first one is selected.
%% @todo Should be included in the XML representation and generated.
node_kind_text([Type|_])    -> node_kind_text(Type);
node_kind_text(guard)       -> "guard";
node_kind_text(pattern)     -> "pattern";
node_kind_text(list_comp)   -> "list comprehension";
node_kind_text(list_gen)    -> "list generator";
node_kind_text(binary)      -> "binary";
node_kind_text(filter)      -> "filter";
node_kind_text('fun')       -> "fun expression";
node_kind_text(application) -> "function application";
node_kind_text(rec)         -> "record";
node_kind_text(recfield)    -> "record field";
node_kind_text(expr)        -> "expression";
node_kind_text(Type)        -> ["code of type \"", atom_to_list(Type), "\""].
