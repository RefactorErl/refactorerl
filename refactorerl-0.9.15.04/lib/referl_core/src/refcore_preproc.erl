
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

%%% @doc Erlang preprocessor graph transformations. This module contains the
%%% functions that build the graph representation of the lexical layer of
%%% Erlang files, perform preprocessing of Erlang source code, and delete
%%% the representation from the graph.
%%%
%%% The lexical layer representation will be described here in the future.
%%%
%%% The following preprocessor transformations are performed by this module:
%%%
%%% <ul>
%%%
%%% <li>`ifdef', `ifndef', `else', and `endif' directives are handled
%%%   according to symbolic names stored in `#env{name=def}' graph
%%%   nodes. Parts excluded from compilation are stored in the graph, but
%%%   not processed any further (dropped by the preprocessor).</li>
%%%
%%% <li>`include' and `include_lib' directives are handled by reading and
%%%   storing the specified files (@see ?FileMan:add_file/1), and
%%%   substituting their contents during preprocessing. Note that macro and
%%%   record definitions are not substituted, only unknown attributes and
%%%   function definitions.</li>
%%%
%%% <li>`define' directives are parsed and macros are stored using
%%%   `#form{type=macro, tag=Name}' graph nodes.</li>
%%%
%%% <li>`?MACRO' and `?MACRO(Par, ..., Par)' references are substituted when
%%%   the macro body is available, other macros trigger a "Non-local macro"
%%%   warning message.</li>
%%%
%%% </ul>
%%%
%%% @author Robert Kitlei <kitlei@inf.elte.hu>
%%% @author Laszlo Lovei <lovei@inf.elte.hu>

%%% Plans:
%%%   Include line number with tokens, possibly {file, line} -- better error
%%%      reporting

-module(refcore_preproc).
-vsn("$Rev: 13087 $ ").


-export([preprocess/2, preprocess/4, detach/2]).

-include("core.hrl").

%%% @type formTokens() = [{#token{}, node()}]. A list that contains token
%%% data for a form.

%%% @type processedForm() = {token, formTokens()} |
%%%                         {vtokens, Dep::node(), Orig::node(), formTokens()} |
%%%                         {form, node()}.
%%% Represetation of a form after preprocessing. The result may be a comlpeted
%%% lexical form that needs no further processing, or a list of tokens.
%%% Virtual forms may appear after file inclusion; these are entirely consist
%%% of virtual tokens copied from form `Orig', with their origin in another
%%% file, which is referred by form `Dep'.

%% @spec preprocess(formTokens(), node()) -> [processedForm()]
%%
%% @doc Processes `Tokens' according to the Erlang preprocessor rules. The
%% input should be a list of tokens that make a single, complete form,
%% directly from the scanner; the output is a preprocessed list of forms.
%% Forms tagged `form' does not need any more processing, these are already
%% parsed by the preprocessor (e.g. macro definitions); other forms are
%% subject to further parsing.
preprocess(Tokens, File) ->
    {Result, _} = preprocess(Tokens, File, start, old),
    Result.

%% @spec preprocess(formTokens(), node(), state(), old|new) ->
%%                                           {[processedForm()], state()}
%%
%% @doc Re-entrant preprocessor that handles conditional compilation. The
%% same notes apply as for `preprocess/2'. First time this function must be
%% called with `start' as the state value, subsequent calls should pass the
%% next form token list and the state returned from the previous call. Note
%% that file inclusion will close the active ESG batch, so returned form
%% nodes must be inserted into the syntax tree by the caller immediately.
preprocess(Tokens, File, start, OldOrNew) ->
    preprocess(Tokens, File, [copy], OldOrNew);

preprocess([{      #token{type='-'},                T1},
            {IfT = #token{type=atom, text=IfTxt},   T2},
            {      #token{type='('},                T3},
            {NaT = #token{type=AtVar},              T4},
            {      #token{type=')'},                T5},
            {      #token{type=stop},               T6}], _File, St, _OldOrNew)
  when ((IfTxt=="ifdef") or (IfTxt=="ifndef")), ((AtVar==atom) or (AtVar==variable)) ->
    If     = ?Token:get_value(IfT),
    Name   = ?Token:get_value(NaT),
    TNodes = [T1, T2, T3, T4, T5, T6],
%%     Def    = ?Graph:path(?Graph:root(),
%%                          [{env, {{name, '==', def}, 'and',
%%                                  {value, '==', macro_name(Name)}}}]),
    Def = macro_env_defs(macro_name(Name)),
    {[{form, create_form(lex, If, TNodes)}],
     if
         ((If == ifdef)  and (Def /= [])) or
         ((If == ifndef) and (Def == [])) ->
             [copy | St];

         true ->
             [skip | St]
     end};

preprocess([{#token{type='-'},                T1},
            {#token{type=atom, text="else"},  T2},
            {#token{type=stop},               T3}], _File, [StTop|St], _OldOrNew) ->
    {[{form, create_form(lex, else, [T1, T2, T3])}],
     case StTop of
         copy -> [skip|St];
         skip -> [copy|St]
     end};

preprocess([{#token{type='-'},                T1},
            {#token{type=atom, text="endif"}, T2},
            {#token{type=stop},               T3}], _File, [_|St], _OldOrNew) ->
    {[{form, create_form(lex, endif, [T1, T2, T3])}], St};

preprocess(Form, File, St=[StTop|_], OldOrNew) ->
    case StTop of
        copy ->
            {form(Form, File, OldOrNew), St};
        skip ->
            {[{form, create_form(lex, skip, token_nodes(Form))}], St}
    end.

%% @spec form(formTokens(), node(), old|new) -> [processedForm()]
%%
%% @doc Preprocesses a form: macro definitions are stored (without further
%% preprocessing), included files are linked to the top level file, other
%% forms are subject to macro substitution. The ?MODULE macro is created
%% here when encountering a `-module' attribute. Preprocessor directives are
%% turned into an analysed form, other forms are returned as token lists.
form([{       #token{type='-'},                        T1},
      {IncT = #token{type=atom,      text=IncTxt},     T2},
      {       #token{type='('},                        T3},
      {FNT  = #token{type=string},                     T4},
      {       #token{type=')'},                        T5},
      {       #token{type=stop},                       T6}], File, OldOrNew)
  when IncTxt =:= "include"; IncTxt =:= "include_lib" ->
    Include = ?Token:get_value(IncT),
    Filename = ?Token:get_value(FNT),
    TNodes = [T1, T2, T3, T4, T5, T6],
    try
        IncName = find_include(Include, Filename, File),
        include(File, IncName, TNodes, OldOrNew)
    catch
        throw:Error ->
            [{form, create_form(error, Error, TNodes)}]
    end;

form([{#token{type='-'},                 T1},
      {#token{type=atom, text="define"}, T2},
      {#token{type='('},                 T3} | Tail], _File, _OldOrNew) ->
    TNodes = [T1, T2, T3],
    try define(Tail) of
        {Name, Body} ->
            [{form, create_form(macro, Name, TNodes ++ Body)}]
    catch
        throw:Error ->
            [{form, create_form(error, Error, TNodes ++ token_nodes(Tail))}]
    end;

form(Tokens, File, _OldOrNew) ->
    try
        [{tokens, macro_subst(Tokens, File)}]
    catch
        throw:Error ->
            [{form, create_form(error, Error, token_nodes(Tokens))}]
    end.

%% @spec token_nodes(formTokens()) -> [node()]
%% @doc Returns the token nodes from a token data list.
token_nodes(Tokens) ->
    [Node || {_Data, Node} <- Tokens].

%% @spec create_form(atom(), atom(), node()) -> node()
%% @doc Creates a lexical-only form
create_form(Type, Tag, Tokens) ->
    Form = ?ESG:create(#form{type=Type, tag=Tag}),
    [?ESG:insert(Form, flex, Token) || Token <- Tokens ],
    Form.

%% @spec define(formTokens()) -> [node()]
%% @doc Analyses macro definition, returns the lexical nodes
define([{NameT = #token{type=NameType}, T} | Tokens])
  when NameType == atom; NameType == variable ->
    Name = ?Token:get_value(NameT),
    {macro_name(Name), [T | define(arglist, Tokens, none)]}.

define(arglist, [{#token{type='('}, T1},
                 {#token{type=')'}, T2},
                 {#token{type=','}, T3}     | Tokens], none) ->
    Arg = ?ESG:create(#lex{type=arg}),
    ?ESG:insert(Arg, llex, T1),
    ?ESG:insert(Arg, llex, T2),
    [Arg, T3 | define(body, Tokens, none)];
define(arglist, [{#token{type='('},      T} | Tokens], none) ->
    Arg = ?ESG:create(#lex{type=arg}),
    ?ESG:insert(Arg, llex, T),
    define(nextarg, Tokens, Arg);
define(arglist, [{#token{type=','},      T} | Tokens], none) ->
    [T | define(body, Tokens, none)];

define(nextarg, [{#token{type=MacArg}, T} | Tokens], Arg)
        when MacArg == variable; MacArg == '_' ->
    ?ESG:insert(Arg, llex, T),
    define(argsep, Tokens, Arg);

define(argsep, [{#token{type=','},       T} | Tokens], Arg) ->
    ?ESG:insert(Arg, llex, T),
    define(nextarg, Tokens, Arg);
define(argsep, [{#token{type=')'},      T1},
                {#token{type=','},      T2} | Tokens], Arg) ->
    ?ESG:insert(Arg, llex, T1),
    [Arg, T2 | define(body, Tokens, none)];

define(body, Tokens, none) ->
    define(body, Tokens, ?ESG:create(#lex{type=body}));

define(body, [{#token{type=')'},         T},
              {#token{type=stop},        Stop} ], Body) ->
    [Body, T, Stop];
%% Quote from epp.erl: "Be nice, allow no right paren!"
define(body, [{#token{type=stop},        Stop} ], Body) ->
    [Body, Stop];
define(body, [{_, T} | Tokens], Body) ->
    ?ESG:insert(Body, llex, T),
    define(body, Tokens, Body);

define(Stat, [], _Def) ->
    throw({define, Stat}).


%% @spec macro_subst(formTokens(), node()) -> formTokens()
%% @doc Performs macro substitution in a token list
macro_subst([Head = {#token{type='?'}, Q} | Tail], File) ->
    case macro(Tail, File, Q) of
        skip ->
            case env_subst(Tail) of
                {subst, Subst, Rest} -> Subst ++ macro_subst(Rest, File);
                no_subst             -> [Head | macro_subst(Tail, File)]
            end;
        {done, Subst, Rest} -> Subst ++ macro_subst(Rest, File);
        {App, Params, Rest} ->
            macro_subst(substitute(App, Params) ++ Rest, File)
    end;
macro_subst([Head | Tail], File) ->
    [Head | macro_subst(Tail, File)];
macro_subst([], _File) ->
    [].


env_subst([{#token{type=Type, text=Text}, _}, T2={#token{type=Type2}, _} | Rest])
        when Type == atom, Type2 /= '('; Type == variable, Type2 /= '(' ->
    MacName = macro_name(Text),
    case macro_env_defs(MacName) of
        []           -> no_subst;
        [Body|Bodys] ->
            Bodys == [] orelse
                error_logger:warning_msg(
                    ?MISC:format("Multiple env bodies given for macro ?~s~n",
                                 [MacName])),
            Tokens = body_to_tokens(Body),
            {subst, Tokens, [T2|Rest]}
    end;
env_subst(_) ->
    no_subst.

body_to_tokens(_Body) ->
    % todo Tokenize the body using the Erlang scanner.
    % todo Turn the tokens into #token{}s.
    % todo Make token nodes in the graph.
    [].

%% @doc Returns the environmentally set macro definitions for a macro name.
%%      If no body is given (using ri:addenv(def, MacName)),
%%      a default body containing the atom 'placeholder_atom' is used,
%%      othewise (using ri:addenv(def, {MacName, Body})) the given body is used.
macro_env_defs(Name) ->
    Defs = ?Graph:path(?Graph:root(), [{env, {name, '==', def}}]),
    [EnvBody || Def <- Defs,
                #env{value=Value} <- [?Graph:data(Def)],
                EnvBody <- macro_env_body(Name, Value)].

macro_env_body(Name, Name) when is_list(Name) -> ["placeholder_atom"];
macro_env_body(Name, {Name, Body})            -> [Body];
macro_env_body(_, _)                          -> [].


%% @spec macro(formTokens(), node(), node()) ->
%%       skip |
%%       {done, formTokens(), formTokens()} |
%%       {node(), [formTokens()], formTokens()}
%% @doc Analyses macro substitution. Returns `skip' for not existing macros,
%% `{done, Result, Rest}' for special macros substituted here, and
%% `{App, Params, Rest}' for normal macros to be substituted later.
macro([{NameT = #token{type=NameType}, N} | Tokens], File, First)
  when NameType == atom; NameType == variable ->
    Name = ?Token:get_value(NameT),
    case Name of
        "MODULE"        -> predef_module_macro(First, N, Name, Tokens, File);
        "MODULE_STRING" -> predef_module_macro(First, N, Name, Tokens, File);
        "FILE"          -> predef_file_macro(First, N, Name, Tokens, File);
        "LINE"          -> predef_line_macro(First, N, Name, Tokens);
        _ ->
            Arity = macapp_arity(Tokens),
            user_def_macro(Name, Arity, File, First, N, Tokens)
    end.

%% @doc Note: line numbers are not accurately handled.
predef_line_macro(First, N, Name, Tokens) ->
    App = create_macro_app(Name, [First, N]),
    RandomLineNumber = 42,
    MToken = #token{type=integer, text=integer_to_list(RandomLineNumber)},
    {done, [{MToken, virtual(none, App)}], Tokens}.

predef_file_macro(First, N, Name, Tokens, File) ->
    App = create_macro_app(Name, [First, N]),
    #file{path=Path} = ?Graph:data(File),
    MToken = #token{type=string, text="\"" ++ Path ++ "\""},
    {done, [{MToken, virtual(none, App)}], Tokens}.


predef_module_macro(First, N, Name, Tokens, File) ->
    case ?Graph:data(File) of
        #file{type=header} ->
            % note: using XModName helps us give no false positives
            %       when two or more files include the header with ?MODULE
            XModName = "macro name expanded in header file",
            predef_module_macro_found(First, N, Name, Tokens, XModName);
        #file{type=module, path=Path} ->
            ModName = filename:basename(Path, ".erl"),
            predef_module_macro_found(First, N, Name, Tokens, ModName);
        _ ->
            skip
    end.

predef_module_macro_found(First, N, Name, Tokens, ModName) ->
    App = create_macro_app(Name, [First, N]),
    MToken =
        if
           Name == "MODULE" ->
               #token{type=atom, text=ModName};
           true ->
               #token{type=string, text="\"" ++ ModName ++ "\""}
        end,
    {done, [{MToken, virtual(none, App)}], Tokens}.


%% @doc Handles user defined (not built-in) macros.
user_def_macro(Name, Arity, File, First, N, Tokens) ->
    case find_macdef(Name, Arity, File) of
        skip ->
            skip;
        {Def, FoundArity} ->
            App = create_macro_app(Name, [First, N]),
            ?Graph:mklink(App, mref, Def),
            case FoundArity of
                no_args -> {App, [], Tokens};
                _       -> macro_params(Tokens, App)
            end
    end.

%% Finds the definition of a macro.
find_macdef(Name, Arity, File) ->
    MacName = macro_name(Name),
    MacDefs = macs_in_file(File, MacName),
    #file{path=Path} = ?Graph:data(File),
    case [MacDef || MacDef <- MacDefs,
                    macdef_arity(MacDef) == Arity] of
        [MacDef|Rest] ->
            ErrMsg = mac_conflicting_defs_msg(MacName, Arity, Path),
            Rest == [] orelse
                error_logger:warning_msg(ErrMsg),
            {MacDef, Arity};
        [] ->
            case [MacDef || MacDef <- MacDefs,
                            macdef_arity(MacDef) == no_args] of
                [MacDef|Rest] ->
                    % In this case, ?x(...) is found, but only ?x is substituted
                    % as there is no definition for ?x with the required arity
                    ErrMsg = mac_conflicting_defs_msg(MacName, Arity, Path),
                    Rest == [] orelse
                        error_logger:warning_msg(ErrMsg),
                    {MacDef, no_args};
                [] ->
                    #file{path=Path} = ?Graph:data(File),
                    ErrMsg = ?MISC:format("Cannot find body for macro ?~s/~p in ~s~n",
                                             [MacName, Arity, Path]),
                    error_logger:warning_msg(ErrMsg),
                    skip
            end
    end.

mac_conflicting_defs_msg(MacName, Arity, Path) ->
    ?MISC:format("Macro ?~s/~p in ~s seems to have multiple, conflicting definitions~n",
                 [MacName, Arity, Path]).

%% @doc Returns the defining forms of all macros in `File' that have the name `Name'.
%%      Order is not retained.
macs_in_file(File, MacName) ->
    MacroPath = [{incl, back}, incl,
                 {form, {{type, '==', macro}, 'and', {tag, '==', MacName}}}],
    lists:usort(?Graph:path(File, MacroPath)).


macdef_arity(MacDef) ->
    Lexs = ?Graph:path(MacDef, [flex]),
    case [Lex || Lex <- Lexs, #lex{type=arg} <- [?Graph:data(Lex)]] of
        [] ->
            no_args;
        [ArgNode] ->
            ArgVars = [Node || Node <- ?Graph:path(ArgNode, [llex]),
                               #lex{data=#token{type=variable}} <- [?Graph:data(Node)]],
            length(ArgVars)
    end.


%% @doc Returns the arity of the macro:
%%      `no_args' for ?X that is not followed by parentheses,
%%      the number of parameters if there are parameters.
macapp_arity([{#token{type='('}, _N} | Rest]) ->
    TokenWWeights = add_token_weights(Rest, [], 1, [')']),

    CommaCount = lists:sum([1 || {1, #token{type=','}} <- TokenWWeights]),
    case {CommaCount, TokenWWeights} of
        {0, [{0, #token{type=')'}}]} ->
            0;
        {0, _} ->
            1;
        {_, _} ->
            CommaCount + 1
    end;
macapp_arity(_Ts) ->
    no_args.

add_token_weights([], _, _, [_|_]) ->
    throw(unbalanced_macro_application);
add_token_weights(_, RevWeighted, _, []) ->
    lists:reverse(RevWeighted);
add_token_weights([{Token = #token{type=ClType}, _} | Rest], RevWeighted, CurrWeight, [ClType|RestCls]) ->
    Weight = CurrWeight - 1,
    NewRevWeighted = [{Weight, Token} | RevWeighted],
    add_token_weights(Rest, NewRevWeighted, Weight, RestCls);
add_token_weights([{Token = #token{type=Type}, _} | Rest], RevWeighted, CurrWeight, Closers) ->
    case close_paren(Type, Rest) of
        {type, ClType} ->
            Weight = CurrWeight + 1,
            NewRevWeighted = [{Weight, Token} | RevWeighted],
            add_token_weights(Rest, NewRevWeighted, Weight, [ClType|Closers]);
        none ->
            add_token_weights(Rest, [{CurrWeight, Token} | RevWeighted], CurrWeight, Closers)
    end.


create_macro_app(Name, Tokens) ->
    App = ?ESG:create(#lex{type=subst, data=macro_name(Name)}),
    [?ESG:insert(App, llex, T) || T <- Tokens],
    App.

macro_params([{#token{type='('}, O},
              {#token{type=')'}, C} | Tokens], App) ->
    ?ESG:insert(App, llex, O),
    ?ESG:insert(App, llex, C),
    {App, [], Tokens};

macro_params([{#token{type='('}, N} | Tokens], App) ->
    ?ESG:insert(App, llex, N),
    macro_param(Tokens, [], App).

macro_param(Tokens, Params, App) ->
    Par = ?ESG:create(#lex{type=param}),
    ?ESG:insert(App, llex, Par),
    macro_param(Tokens, Par, [], [], Params, App).

%% @spec macro_param(Tokens::formTokens(), node(), formTokens(), ParStack,
%%       [formTokens()], node()) -> {node(), [formTokens()], formTokens()}
%%   ParStack = [op_paren|op_brace|op_bracket]
%%
%% @doc Continues macro substitution analysis when parameters are present.
%% <ul>
%%  <li>`Tokens' is the input token list</li>
%%  <li>`ParOb' is the node of the last parameter (which is being analysed)</li>
%%  <li>`Ps' is the reversed list of tokens in the last parameter</li>
%%  <li>`Paren' is the stack of parentheses</li>
%%  <li>`Params' is the reversed parameter list (of token lists)</li>
%%  <li>`App' is the macro application node</li>
%% </ul>
macro_param([P={#token{type=ClParen}, N} | Tokens],
            ParOb, Ps, [ClParen|Paren], Params, App) ->
    ?ESG:insert(ParOb, llex, N),
    macro_param(Tokens, ParOb, [P|Ps], Paren, Params, App);

macro_param([{#token{type=')'}, N} | Tokens],
            _ParOb, Ps, [], Params, App) ->
    ?ESG:insert(App, llex, N),
    {App, lists:reverse([lists:reverse(Ps)|Params]), Tokens};

macro_param([{#token{type=','}, N} | Tokens],
            ParOb, Ps, [], Params, App) ->
    ?ESG:insert(ParOb, llex, N),
    macro_param(Tokens, [lists:reverse(Ps)|Params], App);

macro_param([P={#token{type=Type}, N} | Tokens],
            ParOb, Ps, Paren, Params, App) ->
    ?ESG:insert(ParOb, llex, N),
    case close_paren(Type, Tokens) of
        {type, ClType} ->
            macro_param(Tokens, ParOb, [P|Ps], [ClType | Paren], Params, App);
        none ->
            macro_param(Tokens, ParOb, [P|Ps], Paren, Params, App)
    end.

close_paren('(',       _Tokens)                    -> {type, ')'};
close_paren('{',       _Tokens)                    -> {type, '}'};
close_paren('[',       _Tokens)                    -> {type, ']'};
close_paren('<<',      _Tokens)                    -> {type, '>>'};
close_paren('fun',     [{#token{type='('}, _}|_])  -> {type, 'end'};
close_paren('begin',   _Tokens)                    -> {type, 'end'};
close_paren('if',      _Tokens)                    -> {type, 'end'};
close_paren('case',    _Tokens)                    -> {type, 'end'};
close_paren('receive', _Tokens)                    -> {type, 'end'};
close_paren('try',     _Tokens)                    -> {type, 'end'};
close_paren('cond',    _Tokens)                    -> {type, 'end'};
close_paren(_,         _Tokens)                    -> none.

%% @spec substitute(node(), formTokens()) -> formTokens()
%% @doc Generates the result of a macro substitution
substitute(App, Params) ->
    [Def] = ?Graph:path(App, [mref]),
    ArgTokens = [?Graph:data(A) ||
                    A <- ?Graph:path(Def, [{flex, {type, '==', arg}}, llex])],
    Args = [Var || #lex{data=#token{type=variable, text=Var}} <- ArgTokens],
    Body = ?Graph:path(Def, [{flex, {type, '==', body}}, llex]),
    substitute([{(?Graph:data(N))#lex.data, N} || N <- Body],
               Args, Params, App).

%% @spec substitute(Body::formTokens(), [string()], [formTokens()], node()) ->
%%          formTokens()
substitute([{#token{type=variable, text=Arg}, N}|Rest], Args, Params, App) ->
    [{Data, virtual(Node, App)} ||
        {Data, Node} <- subs_arg(Arg, N, Args, Params)] ++
        substitute(Rest, Args, Params, App);
substitute([{#token{type='?'}, _},
            {#token{type='?'}, _},
            {#token{type=variable, text=Arg}, N}|Rest], Args, Params, App) ->
    %% Proper text substitution could be implemented here if needed
    [{#token{type=string, text="\"" ++ Arg ++ "\""}, virtual(N, App)} |
     substitute(Rest, Args, Params, App)];
substitute([{Data, Node} | Rest], Args, Params, App) ->
    [{Data, virtual(Node, App)} | substitute(Rest, Args, Params, App)];
substitute([], _, _, _) ->
    [].

subs_arg(Name, _, [Name|_], [Value|_]) ->
    Value;
subs_arg(Name, Node, [_|Args], [_|Params]) ->
    subs_arg(Name, Node, Args, Params);
subs_arg(Name, Node, [], _) ->
    [{#token{type=variable, text=Name}, Node}].


macro_name(Atom) when is_atom(Atom) -> atom_to_list(Atom);
macro_name(Str)  when is_list(Str)  -> Str.

-ifdef(ericsson).

% spec path_of_existing_header(HeaderBaseName::file:name_all()) ->
%   Path::string() | not_exist
path_of_existing_header(HeaderBaseName) ->
    PredFun = fun(FileNode)->
        case filename:basename((?Graph:data(FileNode))#file.path) of
            HeaderBaseName -> false;
            _ -> true
        end
    end,
    Headers = ?Graph:path(?Graph:root(), [{file, {type, '==', header}}]),
    case lists:dropwhile(PredFun, Headers) of
        [] -> not_exist;
        [File|_] -> (?Graph:data(File))#file.path
    end.
% This is the implementation of the Ericsson specific include handling.
% Looks for an include by applying the following strategy:
%   First, it looks for an existing header file
%       whose name equals to basename(Name).
%       If a file is matched, then the path of the file is returned.
%   If no result is found, then the original business logic will be applied.
find_include(InclType, Name, FileNode) ->
    HeaderBaseName = filename:basename(Name),
    case path_of_existing_header(HeaderBaseName) of
        not_exist -> find_include_regular(InclType, Name, FileNode);
        Path -> Path
    end.
-else.
%% Looks for the first existing include file in the list of include
%% directories.
find_include(InclType, Name, FileNode) ->
    find_include_regular(InclType, Name, FileNode).
-endif.

%% Looks for the first existing include file in the list of include
%% directories.
find_include_regular(include, Name, FileNode) ->
    case ?Graph:path(?Graph:root(), [{file, {path, '==', Name}}]) of
        [_InclFile] -> Name;
        _ ->
            #file{path=FilePath} = ?Graph:data(FileNode),
            RealName = real_path(Name),
            Base = filename:dirname(FilePath),
            AppBases = dirs_by_env(appbase),
            Dirs = dirs_by_env(include) ++
                   component_dir(AppBases, "include") ++
                   component_dir(AppBases, "src"),
            case [Filename ||   Dir      <- [Base | Dirs],
                                Filename <- [filename:join(Dir, RealName)],
                                filelib:is_file(Filename)] of
                [Filename|_] -> ?MISC:canonical_filename(Filename);
                []           -> error_no_include(FilePath, Name)
            end
    end;

find_include_regular(include_lib, Name, FileNode) ->
    AppBases = dirs_by_env(appbase),
    IncName =
        case lists:member($/, Name) of
            false ->
                filename:join(["..", "include", Name]);
            true ->
                Name
        end,

    [BaseName|Rest] = filename:split(IncName),
    Dir             = filename:join(Rest),
    #file{path=FilePath} = ?Graph:data(FileNode),
    case BaseName of
        ".." ->
            IncPath = filename:join(filename:dirname(FilePath), IncName),
            case filelib:is_file(IncPath) of
                true ->
                    ?MISC:canonical_filename(IncPath);
                false ->
                    error_no_include_lib(FilePath, Name)
            end;
        _ ->
            case app_files(AppBases, BaseName, Dir) of
                [Filename|_] -> ?MISC:canonical_filename(Filename);
                []           -> error_no_include_lib(FilePath, Name)
            end
    end.

error_no_include(FilePath, Name) ->
    error_logger:warning_msg("~s: Include file not found: ~s~n", [FilePath, Name]),
    throw({no_include_file, Name}).

error_no_include_lib(FilePath, Name) ->
    error_logger:warning_msg("~s: Include lib not found: ~s~n", [FilePath, Name]),
    throw({no_include_file, Name}).

%% @doc Returns the specified component subdirectory (e.g. 'src' or 'include')
%% from all applications.
component_dir(AppBases, SubDir) ->
    Dirs = [filelib:wildcard(filename:join([AppBase, "*", SubDir]))
                || AppBase <- AppBases],
    [Dir || Dir <- Dirs, filelib:is_dir(Dir)].

%% @doc Resolves the beginning environment variable of the path.
real_path([$$|Path]) ->
    [EnvVar|Rest] = filename:split(Path),
    EnvPath       = get_env_path(EnvVar),
    filename:join([EnvPath | Rest]);
real_path(Path) ->
    Path.

%% Returns the string associated with the environment variable `EnvVar'.
%% Primarily it searches for an #env{name=env_var, value={EnvName, Path}}
%% environment in the graph,
%% secondarily it checks the OS environment variables.
get_env_path(EnvVar) ->
    case proplists:get_value(EnvVar, ?Syn:get_env(env_var)) of
        undefined ->
            case os:getenv(EnvVar) of
                false    -> throw({unknown_env_in_include, [$$|EnvVar]});
                EnvPath2 -> EnvPath2
            end;
        EnvPath2 ->
            EnvPath2
    end.

%% Returns the names of the files
%% that are described in the named environment.
dirs_by_env(Name) ->
    [(?Graph:data(Dir))#env.value ||
        Dir <- ?Graph:path(?Graph:root(), [{env, {name, '==', Name}}])].

%% Returns the files in the specified directory of the application.
%% The function tries to access all version numbers of `BaseName'.
app_files(AppBases, BaseName, Dir) ->
    BN = BaseName,
    PossibleNames = ?MISC:format("{~s,~s-*,~s-*.*,~s-*.*.*,~s-*.*.*.*}",
                                 [BN,BN,BN,BN,BN]),
    [filename:join(AD, Dir) ||
        AppBase <- AppBases,
        AD <- filelib:wildcard(filename:join(AppBase, PossibleNames))].



%% @spec include(node(), string(), [node()], old|new) -> [processedForm()]
%% @doc Adds an include file into the graph. Returns the form list that is the
%% result of including the file.
%% If the includer file is already loaded, the included file also has to be loaded;
%% in this case, the function tries to update the contents of the include file
%% from disk.
include(File, NonCanIncName, Tokens, OldOrNew) ->
    IncName = ?MISC:canonical_filename(NonCanIncName),
    FileType = (?Graph:data(File))#file.type,
    case ?Graph:path(?Graph:root(), [{file, {path, '==', IncName}}]) of
        [File2] -> Add = {none, File2};
        _       -> Add = {new, File2=?FileMan:create_file_node(IncName, eol)}
    end,
    ?Graph:mklink(File, incl, File2),
    AddResult =
        case OldOrNew of
            old ->
                ?FileMan:add_file(Add, [{update, true}]);
            new ->
                ?FileMan:add_file(Add, [])
        end,
    case AddResult of
        {error, Reason} ->
            ?Graph:rmlink(File, incl, File2),
            throw({include_error, IncName, Reason});
        {file, IncFile} ->
            [?Graph:mklink(File, incl, Inc) ||
                Inc <- ?Graph:path(IncFile, [incl])],
            if
                FileType =:= header ->
                    Form = create_form(lex, include, Tokens),
                    ?Graph:mklink(Form, iref, IncFile),
                    [{form, Form}];
                FileType =:= module ->
                    IncNode = ?ESG:create(#lex{type=incl}),
                    [?ESG:insert(IncNode, llex, T) || T <- Tokens],
                    IncForm = create_form(lex, include, [IncNode]),
                    ?Graph:mklink(IncForm, iref, IncFile),
                    [{form, IncForm} |
                     [{vtokens, IncForm, Orig, macro_subst(Form, File)} ||
                         {Orig, Form} <- include_forms(IncFile, IncNode)]]
            end
    end.

%% @spec include_forms(node(), node()) -> [{node(), formTokens()}]
include_forms(IncFile, IncNode) ->
    lists:flatmap(
      fun
          (Form) ->
              include_form(?Graph:data(Form), Form, IncNode)
      end,
      ?Graph:path(IncFile, [form])).

%% @spec include_form(#form{}, node(), node()) -> [formTokens()]
include_form(#form{tag=store}, Form, IncNode) ->
    [{Form, lists:flatten([include_tokens(T, IncNode) || T <- ?Graph:path(Form, [flex])])}];
%% TODO: what about include_lib?
include_form(#form{tag=include}, Form, IncNode) ->
    [File] = ?Graph:path(Form, [iref]),
    include_forms(File, IncNode);
include_form(#form{}, _Form, _IncNode) ->
    [].

include_tokens(Token, IncNode) ->
    [{Data, virtual(Orig, IncNode)} || {Orig, Data} <- orig_tokens(Token)].

orig_tokens(Token) ->
    case ?Graph:data(Token) of
        #lex{type=token, data=virtual} ->
            case ?Graph:path(Token, [orig]) of
                [Orig] ->
                    orig_tokens(Orig);
                [] ->
                    % currently, the only such token is the one
                    % that comes from ?MODULE
                    [{none, (?Graph:data(L))#lex.data} || L <- ?Syn:leaves(Token)]
            end;
        #lex{type=token, data=Data} ->
            [{Token, Data}]
    end.

%% Creates a virtual token in the ?ESG layer.
virtual(Token, Source) ->
    VT = ?ESG:create(#lex{type=token, data=virtual}),
    Token == none orelse
        ?Graph:mklink(VT, orig, Token),
    ?ESG:insert(VT, llex, Source),
    VT.

%% @spec detach(node(), node()) -> ok
%% @doc Detaches preprocessor-generated form `Form' from its container file
%% `File' by deleting preprocessor-generated links. Currently this only means
%% cleaning up file inclusion information, other preprocessor-related stuff is
%% represented in the syntax tree.
detach(File, Form) ->
    case ?Graph:path(Form, [iref]) of
        [] -> ok;
        [Incl] -> remove_include(File, Incl)
    end.

remove_include(File, Incl) ->
    OtherIncl = ?Graph:path(File, [form, iref]) -- [Incl],
    Hold = lists:append([?Graph:path(I, [incl]) || I <- OtherIncl]),
    lists:foreach(
      fun (Drop) -> ?Graph:rmlink(File, incl, Drop) end,
      ?Graph:path(Incl, [incl]) -- Hold).
