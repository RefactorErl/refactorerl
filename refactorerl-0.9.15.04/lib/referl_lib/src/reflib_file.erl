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
%%% The Initial Developer of the Original Code is E�tv�s Lor�nd University.
%%% Portions created  by E�tv�s Lor�nd University and ELTE-Soft Ltd.
%%% are Copyright 2007-2013 E�tv�s Lor�nd University, ELTE-Soft Ltd.
%%% and Ericsson Hungary. All Rights Reserved.

%%% @doc File properties and file based queries

-module(reflib_file).
-vsn("$Rev: 9609 $ ").

%% =============================================================================
%% Exports

%% Properties
-export([path/1, length/1, type/1, includable/2, hash/1, all/0]).

%% Queries
-export([token/1, token/2,
         find/1, module/0, forms/0, form/1, real_forms/0, error_forms/0,
         include_form/1, includes/0, included/0,
         records/0, record/1, macros/0, macro/1, module_form/1]).

%% Manips
-export([add_include/2, del_include/2, add_form/2, add_form/3,
         del_form/1, del_form/2, reload_form/2, upd_path/2]).

%% Useful path calculators
-export([abs_path/1, rel_path/2, rel_incl_path/2]).

-include("lib.hrl").

%% =============================================================================
%% File related properties

%% @spec all() -> query(root(), #file{})
%% @doc The query returns every file in the graph.
all() ->
    [file].

%% @spec path(node()) -> string()
%% @doc Returns the stored path of the file.
path(FileNode) ->
    (?Graph:data(FileNode))#file.path.

%% @spec type(node()) -> atom()
%% @doc Returns the type of the file (module or header).
type(FileNode) ->
    (?Graph:data(FileNode))#file.type.

%% @spec hash(node()) -> integer()
%% @doc Returns the hash of the file.
hash(FileNode) ->
    (?ESG:data(FileNode))#file.hash.

%% @spec length(node()) -> integer()
%% @doc Return the length of the file (number of characters).
length(FileNode) ->
    erlang:length(lists:flatten(?Syn:tree_text(FileNode))).

%% @spec includable(node(#file{}), node(#file{type=header})) -> bool()
%% @doc Returns true if record and macro names do not conflict after
%% the inclusion. The names are collected and checked recursively in
%% header files.
includable(Target, Incl) ->
    NoClash =
        fun(Tag) ->
                X1 = existing_ent_with_source(Target, Tag),
                X2 = existing_ent_with_source(Incl,   Tag),
 % io:format("DEBUG: Targ=~p, Incl=~p~n",[X1,X2]),
                {XN1, _} = lists:unzip(X1--X2),
                    %% Is in Target, but isn't in Incl
                {XN2, _} = lists:unzip(X2--X1),
                    %% Is in Incl, but isn't in Target
                ?MISC:intersect(XN1, XN2) == []
        end,
    lists:all(NoClash,[macro,record,module]).

existing_ent_with_source(File, Tag) ->
    {FileQuery,GetName} =
        case Tag of
            record -> {records(),fun ?Rec:name/1};
            macro  -> {macros(), fun ?Macro:name/1};
            module -> {module(), fun ?Mod:name/1}
        end,
    ?MISC:flatsort(
       [{GetName(Node), File} || Node <- ?Query:exec(File, FileQuery)]
       ++ [existing_ent_with_source(F, Tag) ||
              F <- (?Query:exec(File, includes()) -- [File])]).


%% =============================================================================
%% Queries starting from files

%% @spec token(Pos::integer()) -> query(#file{}, #lex{})
%% @doc Returns the token that is at the specified position  in the file text.
token(Pos) when Pos > 0 ->
    token(Pos, both);
token(0)-> throw(?RefError(no_token, 0)).

%% @spec token(Pos::integer(), Ws::none|pre|post|both) -> query(#file{}, #lex{})

%% @doc Returns the token that is at the specified position in the
%% file text, but when `Ws' is given:
%% <ul>
%%   <li>`none' means whitespace is not accepted</li>
%%   <li>`pre' means whitespace is handled as prefix (the next token
%%   is the result)</li>
%%   <li>`post' means whitespace is postfix (the previous token is the
%%   result)</li>
%%   <li>`both' means both prefix and postfix are accepted</li>
%% </ul>
token(Pos, Ws) when Pos > 0 ->
    fun (File) ->
        Forms = ?ESG:path(File, [form]),
        case ?Token:containing_form(Forms, Pos) of
            not_found ->
                [];
            {Form, RemPos} ->
                ?Token:foldpos(
                    fun (Node, _D, Start, End, _) when Start =< RemPos, RemPos =< End ->
                            {stop, [Node]};
                        (_,_,_,_,Acc) ->
                            {next, Acc}
                    end, [], Form, Ws)
        end
    end;
token(0,_)-> throw(?RefError(no_token, 0)).

%% @spec find(string()) -> query(root(), #file{})
%% @doc Finds file node by its path.
find(Path) ->
    CPath = ?MISC:canonical_filename(Path),
    [{file, {path, '==', CPath}}].

%% @spec module() -> query(#file{}, #module{})
%% @doc The result query returns the described erlang module.
module() ->
    [moddef].

%% @spec module_form(node()) -> query(#file{}, #form{})
%% @doc Returns the module form of the file.
module_form(File) ->
    Forms = ?Query:exec(File, [form]),
    lists:filter(fun(Form) -> (?Graph:data(Form))#form.type == module end, Forms).

%% @spec forms() -> query(#file{}, #form{})
%% @doc The result query returns all the contained forms.
forms() ->
    [form].

%% @spec form(integer()) -> query(#file{}, #form{})
%% @doc The result query returns the nth contained form.
form(I) ->
    [{form, I}].

%% @spec real_forms() -> query(#file{}, #form{})
%% @doc Returns the forms that are physically present in a file (i.e. skipping
%% the results of file inclusion).
real_forms() ->
    [{form, {hash, '/=', virtual}}].

%% @spec error_forms() -> query(#file{}, #form{})
%% @doc The result query returns all error forms.
error_forms() ->
    [{form, {type,'==',error}}].


%% @spec includes() -> query(#file{}, #file{})
%% @doc The result query returns the included files.
includes() ->
    [incl].

%% @spec included() -> query(#file{}, #file{})
%% @doc The result query returns files where the file is included.
included() ->
    [{incl, back}].


%% @spec include_form(IncFile::node()) -> query(#file{}, #form{})
%% @doc The result query returns the form that includes `IncFile' in the file.
include_form(IncFile) ->
    [form, {intersect, IncFile, {iref, back}}].


%% @spec records() -> query(#file{}, #record{})
%% @doc The result query returns all the defined records.
records() ->
    [record].

%% @spec record(atom() | string()) -> query(#file{}, #record{})
%% @doc The result query returns the defined record by name .
record(Name) ->
    [{record, {name, '==', ?MISC:to_atom(Name)}}].

%% @spec macros() -> query(#file{}, #form{})
%% @doc The result query returns all the defined macros.
macros() ->
    [{form, {type, '==', macro}}].

%% @spec macro(atom() | string()) -> query(#file{}, #form{})
%% @doc The result query returns the defined macro by name.
macro(Name) ->
    [{form, {{type, '==', macro}, 'and',
             {tag, '==', ?MISC:to_list(Name)}}}].

%%% ============================================================================
%%% Manipulations

%% @spec add_include(node(#file{}), node(#file{}) | string()) -> ok
%% @doc Adds an include form to the file if the include path is not present.
add_include(File, InclPath) when is_list(InclPath) ->
    [InclFile] = ?Graph:path(?Graph:root(), find(InclPath)),
    case lists:member(InclFile, ?Graph:path(File, includes())) of
        true ->
            ok;
        false ->
            Path = rel_incl_path(InclPath, ?File:path(File)),
            Tokens =
                [?ESG:create(#lex{type=token,
                                  data=?Token:build(Type, Text)}) ||
                    {Type, Text} <-
                        [{'-', "-"},
                         {atom, "include"},
                         {'(', "("},
                         {string, "\"" ++ Path ++ "\""},
                         {')', ")"},
                         {stop, ".\n"}]],
            ?FileMan:add_form(File, form_index(File, include), Tokens)
    end;
add_include(File, InclFile) ->
    add_include(File, path(InclFile)).

%% @spec del_include(node(#file{}), node(#file{})) -> ok
%% @doc Removes the inclusion of `InclFile' from `File'. This includes
%% removing the corresponding `-include' form from `File'.
del_include(File, InclFile) ->
    InclForm = ?Query:exec1(File, include_form(InclFile), include_not_present),
    ?FileMan:drop_form(File, InclForm).

%% @spec del_form(node(#form{})) -> ok
%% @doc Removes a form from its file.
%% @see del_form/2
del_form(Form) ->
    File = ?Query:exec1(Form, ?Form:file(), ?RefError(no_file,[form])),
    del_form(File, Form).

%% @spec del_form(node(#file{}), node(#form{})) -> ok
%% @doc Removes a form from a file. The form may be reinserted at another
%% place. Note that this function does not do preprocessor clean-up, use
%% {@link referl_fileman:drop_form/2} for that purpose.
del_form(File, Form) ->
    ?ESG:remove(File, form, Form).

%% @spec add_form(node(#file{}), node(#form{})) -> ok
%% @doc Inserts a form below a file, maintaining the order of form types:
%% `func', `module', `export', `import', `include', `macro', `record'.
add_form(File, Form) ->
    ?ESG:insert(File,{form, form_index(File, ?Form:type(Form))}, Form).

%% @spec add_form(node(#file{}), integer(), node(#form{})) -> ok
%% @doc Inserts a form with the given index.
add_form(File, Index, Form) ->
    ?ESG:insert(File,{form, Index}, Form).

%% @spec reload_form(node(#file{}), node(#form{})) -> ok
%% @doc Parses the form's text in the given file, and rebuilds the form's subtree
%% in the database.
reload_form(File, Form) ->
    Index = ?Graph:index(File, form, Form),
    Text = ?Syn:flat_text(Form),
    ?FileMan:drop_form(File, Form),
    ?FileMan:add_text(File, Index, Text).

%% @spec form_index(node(), atom()) -> integer() | last
%% @doc Determines the index of a new form.
form_index(_File, func) -> last;
form_index(File, Type) ->
    PrevForms   = [Form || Form <- ?Graph:path(File, [form]),
                           lists:member(?Form:type(Form),
                                        antecedent_types(Type))],
    erlang:length(PrevForms) + 1.


%% @spec antecedent_types(atom()) -> [node()]
%% @doc Returns the form types that may appear earlier in the file.
antecedent_types(Type) ->
    List = [module, export, import, include, macro, record],
    lists:takewhile(fun(Elem) -> Elem /= Type end, List) ++ [Type].

%% @spec upd_path(node(), string()) -> node()
%% @doc Updates the path of the file. Note that if this function is called
%% during an active ESG batch, the whole file will be reanalysed.
upd_path(File, NewPath) ->
    Data = ?ESG:data(File),
    ?ESG:update(File, Data#file{path = NewPath}).


%% -----------------------------------------------------------------------------
%% Path calculators

%% TODO: move to referl_gen
%% Generates a canonical absolute path by eliminating "./" and "../" relative
%% path elements.
abs_path([]) ->
    [];
abs_path(Path) ->
    Comp   = filename:split(filename:absname(Path)),
    NonDot = lists:filter(fun(S)->S/="."end, tl(Comp)),
    DotDot = fun ("..",{ I, R}) -> {I+1,R    };
                 ( S,  { 0, R}) -> {0,  [S|R]};
                 (_S,  {-1,_R}) -> {-1, []   };
                 (_S,  { I, R}) -> {I-1,R    } end,
    {I,Dirs} = lists:foldr(DotDot,{0,[]},NonDot),
    case I of
        0 -> filename:join([hd(Comp)|Dirs]);
        _ -> []
    end.


rel_path(Dir, Path) ->
    AbsDir  = filename:split(abs_path(Dir)),
    AbsPath = filename:split(abs_path(Path)),
    case {AbsDir, AbsPath} of
        {[X|_], [X|_]} -> filename:join(path_elems(AbsDir, AbsPath));
        _              -> Path
    end.

rel_incl_path(Incl, Path) ->
    Dirs = [AbsDir
            || Dir <- [filename:dirname(Path) | stored_include_paths()],
               AbsDir <- [abs_path(Dir)]],
    RelPaths = [rel_path(Dir, Incl) || Dir <- lists:usort(Dirs)],
    {_, MinRelPath} = lists:min([{erlang:length(P), P} || P <- [Incl|RelPaths]]),
    MinRelPath.

stored_include_paths() ->
    [(?Graph:data(Env))#env.value
     || Env <- ?Graph:path(?Graph:root(), [{env, {name, '==', include}}])].

path_elems([X|Dir], [X|AbsPath]) -> path_elems(Dir, AbsPath);
path_elems(Dir, AbsPath)         -> rel_part(Dir) ++ AbsPath.
rel_part([])  -> []; %% ["."]
rel_part(Dir) -> lists:duplicate(erlang:length(Dir), "..").
