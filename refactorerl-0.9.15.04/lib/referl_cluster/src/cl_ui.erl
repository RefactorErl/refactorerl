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

%%% @doc This module contains interface functions for cluster ui interface.
%%% Two algorithm are available - agglomerative/genetic.
%%% This module is the engine of the Emacs Clustering interface (Cluster UI).
%%%
%%% @author Roland Kiraly <kiralyroland@inf.elte.hu>

-module(cl_ui).
-vsn("$Rev: 9629 $").%"
-include_lib("stdlib/include/qlc.hrl").
-include_lib("../include/cluster.hrl").
-export([run/1,cl_options/1]).
-export([prepare/1,refresh/0]).

-define(TBNAME,cl_ui).
-record(?TBNAME,{id = 0, options, fittnum, result}).

%% Interface
%%----------------------------------------------------------------

%% TODO this function should be removed
%% TODO cl_db:update(all) could take another parameter than `all'
%%% @ spec recalc() -> {atom(), atom()}
%%%
%%% @ doc  This function recalculates the Attribute Matrix uses
%%% cl_interface:recalculate_matrix/0
%recalc()->
%   cl_db:update(all),
%   {ok, attr_matrix}.


%%% @spec run({Options::proplist(), Alg::atom(), CreateDb::atom}) ->
%%%                                            ClResultMain::list()
%%%
%%% @doc This function can execute the clustering with several options.
%%% <ul>
%%% 	<li> Alg: specifies the algorithm which is used. </li>
%%% 	<li> Options: contains the clustering options from the emacs interface. </li>
%%% 	<li> CreateDb: ...save result into a storage or not. </li>
%%% </ul>
run({Opt, Alg, CreateDb})->
    Terms       = [Def || {_, Def} <- cl_options_in(Alg)],
    Opts = correct_opt(Opt,Terms),
    Options     = [convert(Value,Type) || {Value, Type}
                                     <- lists:zip(Opts, Terms)],
    OptionName  = [Name || {Name, _} <- cl_options_in(Alg)],
    FOpt        = lists:zip(OptionName,Options),
    FinalOpt    = [{alg, Alg}, {entity_type, module}]++FOpt,
    Clusterings = ?ClInterface:run_cluster([{log_output, null}]++FinalOpt),
    case Alg =/= agglom_attr orelse 
         lists:keyfind(distfun, 1, Options) =/= weight of
		 true ->  Fitt_Num = ?ClInterface:fitness([{clusterings, Clusterings},
								{fitness_options, FinalOpt}]);
         false -> Fitt_Num = ?ClInterface:weight([{clusterings, Clusterings},
                              {entities, lists:keyfind(entities, 1, Options)}])
    end,
    
    case CreateDb of 
        t -> store_result(FinalOpt, Fitt_Num, Clusterings);
        %% case table_handler() of
        %%    {table, _} ->
        %%         store_result(FinalOpt, Fitt_Num, Clusterings);
        %%    _ -> throw({error, cl_ui_crashed})
        %% end;
        [] -> ok
    end,
    Algorithm = case Alg of
       agglom_attr -> "Agglomerative algorithm";
       genetic     -> "Genetic algorithm";
       _           -> undefined
    end,
    VClustering = [[Lista,fn,Number]
          || {Lista,Number}<-lists:zip(Clusterings,Fitt_Num)],
    {[Algorithm]++VClustering, Fitt_Num}.

%%% @spec cl_options_in(Alg::atom()) -> OptionList::list()
%%%
%%% @doc Options for the main function - this
%%% is not interface function.
%%% Alg: specifies the used algorithm.
cl_options_in(Alg)->
   ?ClInterface:run_cluster_default(Alg).

%%% @spec cl_options(Alg::atom()) -> OptionList::list()
%%%
%%% @doc Returns the default options of the specified clustering algorithm.
%%% The result is a list which contains the default values.
%%% Alg: specifies the used algorithm.
cl_options(Alg)->
      [{Lab, Def} || {{_Name, Def},{_Name, Lab}}
           <- lists:zip(?ClInterface:run_cluster_default(Alg),
                        ?ClInterface:run_cluster_labels(Alg))].

%%% Helpers -------------------------------------------------
convert(Value,Term)->
   if
     Value == [] -> V = Term;
     true        -> V = Value
   end,
   try
      case w_type(Term) of
         float    -> list_to_float(V);
         int      -> list_to_integer(V);
         %% TODO:Need a branch for the [empty] lists
         %% for example this term {nil, []} will be [nil ] on
         %% the side of the emacs interface
         %% The empty string is equivalent with the empty list
         atom     -> list_to_atom(V);
         %function -> not_implemented;   ---   NOT YET IMPLEMENTED
         _        -> V
      end
   catch
     _:_ -> reflib_ui:message(error,"Invalid type ~s",[V]),
        V
   end.

w_type(T)->
    if
      is_atom(T)    -> atom;
      is_integer(T) -> int;
      is_float(T)   -> float;
      %is_function(T)-> function;   ---   NOT YET IMPLEMENTED
      true             -> string
    end.

correct_opt(Opt,Term)->
    [valid(O,T) || {O,T} <- lists:zip(Opt,Term)].

valid(Op,Term)->
    if
    Op == [] ->
             Term;
    true    ->
             Op
    end.

store_result(Opt, Fitt, Cl_res)->
    Qdc = qlc:q([Id || {_, Id, _, _, _} <-mnesia:table(?TBNAME)]),
    Qds = mnesia:async_dirty(fun()-> qlc:e(Qdc)  end),
    case Qds of
        [] -> Id = 0;
         _ -> Id = lists:max(Qds)
    end,
    Record=#cl_ui{id = Id + 1,
                  options=Opt,
                  fittnum = Fitt,
                  result = Cl_res},
    mnesia:dirty_write(?TBNAME, Record).

%%% @private
prepare(Modules)->
    if
         Modules /= []->
           lists:map(fun refcore_fileman:add_file/1, Modules)
% TODO this function should be removed
%           cl_interface:recalculate_attr([])
    end,
    table_handler(),
    {ok, modules_are_loaded}.

exists_table(Tab)->
    try
        mnesia:table_info(Tab,arity),
    {Tab,exists}
    catch
        _:_ -> {Tab, noexists}
    end.

table_handler()->
    case exists_table(?TBNAME) of
         {?TBNAME, exists}   -> {table, exists};
         {?TBNAME, noexists} ->
              mnesia:create_table(?TBNAME,
                                [{attributes,record_info(fields,?TBNAME)},
                                 {disc_copies,[node()]},
                                 {type, bag}]),
              {table, created}%% ;
         %% _ -> throw({error, cl_ui_table})
    end.

%%% @doc Refreshing the mnesia tables. This means deleting the existing tables.
refresh()->
    case exists_table(?TBNAME) of
       {?TBNAME, exists}   ->
            mnesia:delete_table(?TBNAME),
            {cl_ui, recreated};
       {?TBNAME, noexists} ->
            {cl_ui, noexec}
    end.
