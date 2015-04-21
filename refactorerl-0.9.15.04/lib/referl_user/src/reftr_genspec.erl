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

%%% == Implementation status ==
%%% This feature is _not_ fully implemented.

%%% @author Gabor Olah <olikas.g@gmail.com>
-module(reftr_genspec).

-export([prepare/1]).

-include("user.hrl").
-include("spec.hrl").

prepare(Args) ->
    Func = ?Args:function(Args),
    Spec = refusr_spec:run_tr(Func),
    ConstructRes = make_construct_spec(Func, Spec),
    [fun() ->
             NewNode = ?Syn:construct(ConstructRes),
	     [FunDef] = ?Query:exec(Func, [{fundef, back}]),
             [File] = ?Syn:get_file(FunDef),
             Index = ?ESG:index(File,form,FunDef),
	     ?Transform:touch(File),
             ?ESG:insert(File, {form,Index}, NewNode)
     end].




make_construct_spec(Func, Spec) ->
    mcs(Func, Spec).

mcs(Func, Funsig) ->
    %?d({Func, Funsig}),
    FuncName  = reflib_function:name(Func),
    ArgTypes  = [type(A) || A <- (Funsig#type.value)#funsigvalue.args],
    RetValue  = type((Funsig#type.value)#funsigvalue.retval),
    {{spec,FuncName}, ArgTypes, RetValue}.

type([T]) ->
    type(T);
type({'type', 'tuple', 'any'}) ->
    {type, tuple};
type({'type', 'tuple', Elements}) ->
    {{type, tuple}, [type(X) || X <- Elements]};
type({'type', 'list', 'nil'}) ->
    {type, nil};
type({'type', 'list', 'any'}) ->
    {type, list};
type({'type', 'list', V}) ->
    {{type, list}, [type(V)]};
type({'type', 'union', Types}) ->
    {union, [type(X) || X <- Types]};
type({'type', TypeName, _Value}) when is_atom(TypeName) ->
    {type, TypeName}.


