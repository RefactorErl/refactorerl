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

%%% @author Matyas Karacsonyi <k_matyas@inf.elte.hu >

-module(refcore_loadbeam).
-vsn("$Rev: 4099 $ ").

-export([start/3]).

-include("core.hrl").

%% @spec start(FileName, string(), boolean()) -> Result
%%            FileName = atom() | list()
%%            Result   = {ok, node()} | {error, string()}
%% @doc Creates a syntactic tree from a BEAM file, which was compiled using
%% `debug_info' option.
start(Filename, OutputDir, ToSave) ->
    case filelib:is_dir(OutputDir) of
        true ->
            Module = filename:basename(Filename, ".beam"),
            NewName = filename:join(OutputDir, Module++".erl"), %@todo fixme
            case filelib:is_file(NewName) of
                false ->
                    case is_loaded(Module) of
                        false -> perform(Filename, NewName, ToSave);
                        true  -> {error, "Module is already loaded"}
                    end;
                true -> {error, "Output file already exists"}
            end;
        false -> {error, "Output directory does not exist"}
    end.

perform(Filename, NewName, ToSave) ->
    case beam_lib:chunks(Filename, [abstract_code]) of
	{ok, {_, [{abstract_code, {_, AbstractCode}}]}} ->
	    FileNode =
		?Syn:construct(transform_abs(AbstractCode, NewName)),
	    ToSave andalso ?FileMan:save_file(FileNode),
	    {ok, FileNode};
	_ ->
	    {error, "No abstract code"}
    end.

%% @spec is_loaded(string()) -> boolean()
%% @doc Checks whether the file is already loaded.
is_loaded(ModuleName) ->
    LoadedFiles = [filename:basename((?Graph:data(Node))#file.path, ".erl")
                   || Node <- ?Graph:path(?Graph:root(), [file])],
    [1 || FileName <- LoadedFiles, FileName == ModuleName] /= [].

%% @spec transform_abs([AbstractCode], string()) -> AbstractCodeTree
%%   AbstractCodeTree = {atom(), Value} |
%%                      {{atom(), atom()}, Value} |
%%                      {{atom(), atom(), int()}, Value}
%%   Value = atom() | float() | int() | list()
%% @doc Creates a tree description from an Erlang abstract code, which
%% easily can be loaded into RefactorErl by using {@link
%% erl_syntax:construct/1}.
transform_abs([{attribute, _, file, _} | AbsCode], NewName) ->
    {{file, NewName},
     [Node || Node <- [transform_abs(Node) || Node <- AbsCode], Node /= []]}.


transform_abs({attribute, _, module, Name}) ->
    {{form, module, Name}, []};
transform_abs({attribute, _, export, List}) ->
    {{form, export}, [{funlist, [{funref, get_type(Name), get_type(Arity)}
                                || {Name, Arity} <- List]}]};
transform_abs({attribute, _, import, {Module, List}}) ->
    {{form, import}, [get_type(Module), {funlist, 
					 [{funref, get_type(Name), get_type(Arity)}
					  || {Name, Arity} <- List]}]};
transform_abs({attribute, _, file, {Path, Line}}) ->
    {{form, file}, {string, Path}, {integer, Line}};
transform_abs({attribute, _, record, {Name, Fields}}) ->
    {{form, record, Name},
     [case F of
	  {record_field, _, {atom, _, FieldName}} ->
	      {{spec_field, FieldName}, []};
	  {record_field, _, {atom, _, FieldName}, DefaultVal} ->
	      {{spec_field, FieldName}, transform_abs(DefaultVal)}
      end || F <- Fields]};

%%% todo: -opaque
transform_abs({attribute, _, type, {Name, Params, []}}) ->
    {{typedef, Name}, transform_abs(Params)};
transform_abs({attribute, _, spec, {{Name, _Arity}, ParTypes, RetType}}) ->
    {{spec, Name}, [transform_abs(ParT) || ParT <- ParTypes], transform_abs(RetType)};
transform_abs({attribute, _, spec, {{Name, _Arity}, Clauses}}) ->
    {{spec, Name}, {spec_union, [transform_abs(Cl) || Cl <- Clauses]}};

transform_abs({attribute, _, Value, Attr}) ->
    {{attrib, Value}, get_type(Attr)};


transform_abs({type, _, union, Elements}) ->
    {union, [transform_abs(E) || E <- Elements]};
transform_abs({type, _, 'fun', [{type, _, product, ParTypes}, RetType]}) ->
    {spec_clause, [transform_abs(ParT) || ParT <- ParTypes], transform_abs(RetType)};
transform_abs({type, _, Name, []}) ->
    {type, Name};
transform_abs({type, _, Name, Params}) ->
    {{type, Name}, [transform_abs(P) || P <- Params]};
transform_abs({paren_type, _, Elements}) ->
    {paren_type, [transform_abs(E) || E <- Elements]};

transform_abs({function, _, Name, _, Clauses}) ->
    {func, [transform_abs({fun_clause, get_type(Name), C}) || C <- Clauses]};

transform_abs({Kind, Header, {clause, _, Pattern, Guard, Body}}) ->
    {Kind, Header,
	   [transform_abs(P) || P <- Pattern],
	   guard_disj(Guard),
	   [transform_abs(B) || B <- Body]};
transform_abs({'catch', {clause, _, Pattern, Guard, Body}}) ->
    {pattern, [tuple_to_infix(Pattern)],
              guard_disj(Guard),
              [transform_abs(B) || B <- Body]};
transform_abs({guard, {clause, _, [], Guard, Body}}) ->
    {guard, guard_disj(Guard), [transform_abs(B) || B <- Body]};
transform_abs({Kind, {clause, _, Pattern, Guard, Body}}) ->
    {Kind, [transform_abs(P) || P <- Pattern],
	   guard_disj(Guard),
	   [transform_abs(B) || B <- Body]};

transform_abs({record, _, Name, Attr}) ->
    {{record_expr, Name}, [transform_abs(A) || A <- Attr]};
transform_abs({record, _, Var, Name, Fields}) ->
    {{record_update, Name}, transform_abs(Var), [transform_abs(F) || F <- Fields]};
transform_abs({record_field, _, {atom, _, Field}, Value}) ->
    {{record_field, Field}, transform_abs(Value)};
transform_abs({record_field, _, Access, Field, Value}) ->
    {{record_access, Field}, transform_abs(Access), transform_abs(Value)};
transform_abs({record_index, _, Name, Field}) ->
    {{record_index, Name}, transform_abs(Field)};

transform_abs({'fun', _, Attr}) ->
    transform_abs(Attr);
transform_abs({clauses, Clauses}) ->
    {'fun', [transform_abs({fun_scope, C}) || C <- Clauses]};
transform_abs({function, Module, Name, Arity}) ->
    {implicit_fun, {{infix_expr, ':'}, get_type(Module), get_type(Name)}, get_type(Arity)};
transform_abs({function, Name, Arity}) ->    
    {implicit_fun, get_type(Name), get_type(Arity)};

transform_abs({block, _, Clauses}) ->
    {block_expr, [transform_abs(C) || C <- Clauses]};
transform_abs({'case', _, HeadCl, Branches}) ->
    {'case', transform_abs(HeadCl),
     [transform_abs({pattern, B}) || B <- Branches]};
transform_abs({'catch', _, Expr}) ->
    {'catch', transform_abs(Expr)};
transform_abs({'if', _, Clauses}) ->
    {'if', [transform_abs({guard, C}) || C <- Clauses]};
transform_abs({'receive', _, Clauses}) ->
    {'receive', [transform_abs({pattern, C}) || C <- Clauses], []};
transform_abs({'receive', _, Clauses, TimeOut, TBody}) ->
    {'receive', [transform_abs({pattern, C}) || C <- Clauses],
                [transform_abs(TimeOut), [transform_abs(B) || B <- TBody]]};
transform_abs({'try', _, HeadCl, ExprCl, CatchCl, AfterCl}) ->
    {'try', [transform_abs(H) || H <- HeadCl],
            [transform_abs({pattern, E}) || E <- ExprCl],
            [transform_abs({'catch', C}) || C <- CatchCl],
            [transform_abs(A) || A <- AfterCl]};

transform_abs(List) when (element(1, List) == nil) or (element(1, List) == cons) ->
    abslist_to_list(List);
transform_abs({lc, _, HeadExpr, Generator}) ->
    {list_comp, transform_abs(HeadExpr),
		[transform_abs(G) || G <- Generator]};
transform_abs({generate, _, Pattern, Clause}) ->
    {list_gen, transform_abs(Pattern), transform_abs(Clause)};

transform_abs({tuple, _, Attr}) ->
    {tuple, [transform_abs(A) || A <- Attr]};

transform_abs({match, _, LeftOp, RightOp}) ->
    {match_expr, transform_abs(LeftOp), transform_abs(RightOp)};
transform_abs({op, _, '!', LeftOp, RightOp}) ->
    {send_expr, transform_abs(LeftOp), transform_abs(RightOp)};
transform_abs({op, _, Operator, LeftOp, RightOp}) when (Operator == 'and') or 
                                                   (Operator == 'or') or
                                                   (Operator == 'andalso') or 
                                                   (Operator == 'orelse') ->
    {{infix_expr, Operator}, {paren, transform_abs(LeftOp)},
			     {paren, transform_abs(RightOp)}};
transform_abs({op, _, Operator, LeftOp, RightOp}) ->
    {{infix_expr, Operator}, transform_abs(LeftOp),
			     transform_abs(RightOp)};
transform_abs({op, _, Operator, Operand}) ->
    {{prefix_expr, Operator}, transform_abs(Operand)};

transform_abs({bin, _, Parameters}) ->
    {bin, [transform_abs(P) || P <- Parameters]};
transform_abs({bc, _, HeadExpr, Generator}) ->
    {bin_comp, transform_abs(HeadExpr),
	       [transform_abs(G) || G <- Generator]};
transform_abs({b_generate, _, Pattern, Clause}) ->
    {bin_gen, transform_abs(Pattern), transform_abs(Clause)};
transform_abs({bin_element, _, Value, Size, TSL}) ->
    {binary_field,
     [case Size of
	  default -> transform_abs(Value);
	  _       -> {size_qualifier, transform_abs(Value), transform_abs(Size)}
      end,
      get_tsl(TSL)]};

transform_abs({call, _, Name, Parameters}) ->
    {app, transform_abs(Name), [transform_abs(P) || P <- Parameters]};
transform_abs({remote, _, Module, Fun}) ->
    {{infix_expr, ':'}, transform_abs(Module), transform_abs(Fun)};

transform_abs({var, _, '_'}) ->
    {joker, []};
transform_abs({Kind, _, Value}) ->
    {Kind, Value};

transform_abs(_) ->
    [].
    

%% @spec abslist_to_list(tuple()) -> list()
%% @doc
abslist_to_list(List) ->
    abslist_to_list(List, []).

abslist_to_list({nil, _}, List) ->
    case List of
        [] ->
            {cons, []};
        List ->
            {cons, {list, List}}
    end;
abslist_to_list({cons, _, Value, Rest}, List) ->
    abslist_to_list(Rest, List ++ [transform_abs(Value)]);
abslist_to_list(Value, List) ->
    {cons, {list, List}, transform_abs(Value)}.

%% @spec get_tsl(Value) -> list()
%%       Value = atom() | list()
%% @doc
get_tsl(default) ->
    [];
get_tsl([]) ->
    [];
get_tsl([Type | Rest]) ->
    [case Type of
	{T, Size} ->
	    {{bit_size_expr, Size}, get_type(T)};
	_ ->
	    get_type(Type)
    end | get_tsl(Rest)];
get_tsl(Type) ->
    get_type(Type).

%% @spec guard_disj(list()) -> tuple()
%% @doc
guard_disj([]) ->
    [];
guard_disj([Head|Rest]) ->
    case Rest of
	[] ->
	    guard_conj(Head);
	_ ->
	    {{infix_expr, ';'}, guard_conj(Head), guard_disj(Rest)}
    end.

%% @spec guard_conj(list()) -> tuple()
%% @doc
guard_conj([Head|Rest]) ->
    case Rest of
	[] ->
	    transform_abs(Head);
	_ ->
	    {{infix_expr, ','}, transform_abs(Head), guard_conj(Rest)}
    end.

%% @spec tuple_to_infix(tuple()) -> tuple()
%% @doc
tuple_to_infix([{tuple, _, [Class, Pattern, _]}]) ->
    case Class of
        'throw' ->
            transform_abs(Pattern);
        Class ->
            {{infix_expr, ':'}, transform_abs(Class), transform_abs(Pattern)}
    end.

%% @spec get_type(Value) -> {Type, Value}
%%    Value = atom() | integer() | float() | list() | tuple()
%%    Type  = atom()
%% @doc
get_type(Value) when is_atom(Value) ->
    {atom, Value};
get_type(Value) when is_integer(Value) ->
    {integer, Value};
get_type(Value) when is_float(Value) ->
    {float, Value};
get_type(Value) when is_tuple(Value) ->
    {tuple, [get_type(element(I, Value)) || I <- lists:seq(1, size(Value))]};
get_type(Value) when ((hd(Value) >= $A) and (hd(Value) =< $Z)) orelse
		     ((hd(Value) >= $a) and (hd(Value) =< $z))->
    {string, Value};
get_type(Value) ->
    {cons, {list, [get_type(Element) || Element <- Value]}}.
