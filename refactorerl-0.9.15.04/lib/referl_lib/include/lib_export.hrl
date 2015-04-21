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


%%% ===========================================================================
%%% Name of the server node
%-define(REFERL_NODE, node()).
-define(REFERL_NODE,
            begin
                {ok, RAppNode} = application:get_env(refactorerl, referl_node),
                RAppNode
            end).


%%% ===========================================================================
%%% Modules

-define(Transform,      reflib_transform).
-define(Error,          reflib_error).
-define(Info,           reflib_information).
-define(Query,          reflib_query).
-define(Args,           reflib_args).
-define(File,           reflib_file).
-define(Form,           reflib_form).
-define(Clause,         reflib_clause).
-define(Expr,           reflib_expression).
-define(Mod,            reflib_module).
-define(Fun,            reflib_function).
-define(Dynfun,         reflib_dynfun).
-define(Var,            reflib_variable).
-define(Rec,            reflib_record).
-define(RecField,       reflib_record_field).
-define(Macro,          reflib_macro).
-define(Token,          reflib_token_gen).
-define(UI,             reflib_ui_router).
-define(Spec,           reflib_spec).
-define(SpecClause,     reflib_spec_clause).
-define(SpecGuard,      reflib_spec_guard).
-define(SpecParam,      reflib_spec_param).
-define(Type,           reflib_type).
-define(TypeParam,      reflib_type_param).
-define(TypeBody,       reflib_type_body).
-define(Typexp,         reflib_typexp).

-define(UIB,            reflib_ui).
-define(DRAW_GRAPH,     reflib_draw_graph).
-define(GR_UTILS,       reflib_graph_utils).

%%% ===========================================================================
%%% Module error reporting

-define(LocalError(Type, Detail), {?MODULE, Type, Detail}).
-define(RefError(Type, Detail),   {?Error, Type, Detail}).

-define(Check(Expr, Msg), case Expr of true -> ok; _ -> throw(Msg) end).

-define(LocalErr0r(Type),         ?LocalError(Type, [])).
-define(RefErr0r(Type),           ?RefError(Type, [])).
