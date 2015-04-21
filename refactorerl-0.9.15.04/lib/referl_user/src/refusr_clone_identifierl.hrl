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


-define(TLEClones, refusr_clone_identifierl_tlexprs_clones).
-define(CachedTLEClones, refusr_clone_identifierl_tlexprs_clones_with_cache).
-define(FunClones, refusr_clone_identifierl_funcls_clones).
-define(STree, refusr_clone_identifierl_suffix_tree).
-define(Alphabet, refusr_clone_identifierl_candidates).
-define(Metric, refusr_clone_identifierl_metrics).
-define(Lib, refusr_clone_identifierl_lib).
-define(FSTree, refusr_clone_identifierl_filtered_suffix_tree).
-define(MatrixFilter, refusr_clone_identifierl_matrixfilter).
-define(Igraph, refusr_igraph).

-define(TrueOrThrow(__Expr__, ToThrow), (__Expr__) orelse throw(ToThrow)).


-define(TEMPFILE, filename:join([?MISC:data_dir(), "clone_identifier_temp"++
                                 ?MISC:graph_based_postfix()++
                                 ?Lib:current_time_str()])).

-define(unit_key_pos, 2).
-define(unit_data_pos, 1).

-record(unit, {id :: tuple(),
               parent :: tuple(),
               alphabet = [] :: list()}).

-record(clone_item, {items = [] :: list(),
                     score :: float() | non_neg_integer()}).

-record(ci_results, {key :: binary(),
                     name :: atom(),
                     opts :: list(),
                     results :: list(),
                     db_hash :: tuple()}).

-record(storages, {cf :: integer(),
                   exp :: integer(),
                   lay :: integer(),
                   name :: integer(),
                   storage :: integer()}).

-record(metric_values, {key, metric_val=undef}).
-define(metric_tab, metric_values_store).

-define(filter_tab, filter_values_store).
-record(filter_values, {node,
                        same_binding=undef,
                        ratio_funcalls_sets=undef,
                        expr_type=undef,
                        clauses=undef,
                        expr_fun_apps=undef,
                        expr_children=undef,
                        simple_expr=undef,
                        simple_match=undef,
                        max_depth=undef,
                        branching_and_funexp_num_of_cls=undef,
                        branching_and_funexp_called_by_hd=undef,
                        branching_and_funexp_called_by_cls=undef,
                        %case_pattern_type=undef,
                        %case_num_of_pat_vars=undef,
                        rs_same = undef,
                        funapp_same = undef,
                        simple_send = undef,
                        list_comp_head = undef,
                        recfield_same = undef
                       }).
-record(filtering_metric, {key, calc_fun, arbitrate_fun}).

-define(cf_metric_tab, cf_metric_values_store).

-record(cf_metric_values, {node,
                           num_of_arc=undef,
                           num_of_dec=undef,
                           num_of_loops=undef,
                           num_of_exits=undef,
                           num_of_nodes=undef,
                           avg_nesting_level=undef,
                           num_of_indep_path=undef,
                           num_of_messpass=undef,
                           num_of_throw=undef,
                           num_of_spawn=undef}).

-define(exp_metric_tab, exp_metric_values_store).
-record(exp_metric_values, {node,
                             num_of_funcalls=undef,
                             num_of_unique_funcalls=undef,
                             avg_complexity_of_decs=undef,
                             num_of_var_binds=undef,
                             num_of_funexpr=undef,
                             num_of_records=undef,
                             num_of_unused_matching=undef,
                             num_of_guarded_clauses=undef,
                             num_of_dirty_expressions=undef,
                             num_of_infix_ops=undef,
                             num_of_tles=undef}).

-define(lay_metric_tab, lay_metric_values_store).
-record(lay_metric_values, {node,
                            vol_of_comment=undef,
                            num_of_comment=undef,
                            num_of_macs=undef,
                            eloc=undef,
                            avg_length_of_var_names=undef,
                            num_of_funcls=undef,
                            num_of_funcls_guards=undef,
                            num_of_tokens=undef}).

-define(name_metric_tab, name_metric_values_store).
-record(name_metric_values, {node,
                             name=undef}).

-record(metric, {key, calc_fun, delta}).

-define(sw_filter_tab, sw_metrics_filter_values_store).
-record(sw_filter_values, {node,
                           fun_pres_alphabet=undef,
                           mod_and_fun_pres_alphabet=undef,
                           num_of_token=undef,
                           ratio_funcalls_sets=undef,
                           max_depth_of_funcls=undef,
                           softer_max_depth_of_funcls=undef
                       }).

-define(categories, [cf, exp, lay, name, name_stricter]).
