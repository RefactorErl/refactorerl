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
%%% Types

%% @type generator() = {generator,
%%           ModuleName::atom(),
%%           FunctionName::atom(),
%%           Arguments::list()}.
%%
%% The generator record contain a module name, a function name and a argument
%% list. Theses three value describe the `ModName:FunName/FunArity' function
%% where `FunArity' is the length of the `Arguments' list.
%% This record is describe a function call method. The function call produce 
%% (generate) the return value that is interested by us.
%%
%% See the {@link ucl_options:generate/1} function.
-record(generator, {
    modname,
    funname,
    args}).


%% @type fun_result(Arity) = function(Arity) | ModFun | generator()
%%       Arity = natural()
%%       ModFun = {ModName::atom(), FunName::atom()}.
%% This union type is used in the validator functions of the algorithm step.
%% When a step need a function it is not matter how give that. The function can
%% be a function expression, a module name function name pair or a generator
%% record which has a function result. In over all the given function must be
%% an function expression or an exported function with `Arity' arity.


