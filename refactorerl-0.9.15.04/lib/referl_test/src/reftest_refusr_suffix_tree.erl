%%% -*- coding: latin-1 -*-

%%% The  contents of this  file are  subject to  the Erlang  Public License,
%%% Version  1.1, (the  "License");  you may  not  use this  file except  in
%%% compliance  with the License.  You should  have received  a copy  of the
%%% Erlang  Public License  along  with this  software.  If not,  it can  be
%%% retrieved at http://plc.inf.elte.hu/erlang/
%%%
%%% Software  distributed under  the License  is distributed  on an  "AS IS"
%%% basis, WITHOUT  WARRANTY OF ANY  KIND, either expressed or  implied. See
%%% the License  for the specific language governing  rights and limitations
%%% under the License.
%%%
%%% The Original Code is RefactorErl.
%%%
%%% The Initial Developer of the  Original Code is Eötvös Loránd University.
%%% Portions created  by Eötvös  Loránd University are  Copyright 2008-2009,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc Test cases for test framework for graph representations of equivalence
%%% @author Szabo Bence Janos <szbtadi@caesar.elte.hu>

-module(reftest_refusr_suffix_tree).
-compile(export_all).
-include("test.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("referl_user/src/refusr_clone_identifierl.hrl").
-define(RootDir, filename:join(["..", "test", "unit_tests", "suffix_tree"])).
%% ============================================================================
%% Test cases

%% Test.
stree_fstree_test_() ->
    {setup,
     fun setup/0,
     fun(_P) -> reset() end,
     fun(P) -> [?_test(test_stree_fstree(P)),
                ?_test(test_stree_good(P))] end
     }.


test_stree_fstree({STreeNodes, FSTreeNodes}) ->      
    TopSTreeNodes = to_top_lvl_expr(STreeNodes),
    [ ?assert(is_subclone(TopSTreeNodes, FSTreeNode))
        || FSTreeNode <- FSTreeNodes].


test_stree_good({STree, FSTree}) ->
    UniFSTree = refusr_clone_identifierl_lib:to_uni_format(STree),
    UniSTree = refusr_clone_identifierl_lib:to_uni_format(FSTree),
    [ ?assert(is_equal_clone(Clone))
        || Clone <- UniSTree],
    [ ?assert(is_equal_clone(Clone))
        || Clone <- UniFSTree].


%% @doc Setup function.
setup() ->
    [add(?RootDir++"/"++File) || File<-files()],
    {ok, SuffixTreeResult} = 
        make_ui_request({clone_identifierl,[{algorithm,suffix_tree},{minlen,20},{format,nodes}]}),
    timer:sleep(1000),
    {ok, FilteredSuffixTreeResult} =
        make_ui_request({clone_identifierl,[{algorithm,filtered_suffix_tree},{minlen,20},{format,nodes}]}),
    STreeNodes = proplists:get_value(detected_clones, SuffixTreeResult),
    FSTreeNodes = proplists:get_value(detected_clones, FilteredSuffixTreeResult),
    {STreeNodes, FSTreeNodes}.



%% @doc Trim to top level expression.
to_top_lvl_expr(STreeNodes) ->
    UniClones = refusr_clone_identifierl_lib:to_uni_format(STreeNodes),
    TopExprClones = refusr_clone_identifierl_lib:to_tlexpr_clones(UniClones, fstree),
    refusr_clone_identifierl_lib:format(TopExprClones, nodes, linecol).



%% @doc Subclone investigation.
is_subclone(MainClones, SubClone) ->
    lists:any(fun(K) -> is_subclone0(K,SubClone) end, MainClones).

is_subclone0(MainClone, Clone) ->
    lists:all(fun(K) -> is_subset(MainClone, K) end, Clone).

is_subset(MainClone, K) ->
    lists:any(fun(U) -> lists:subtract(K,U) == [] end, MainClone).




%% @doc Suffix tree good result investigation.
is_equal_clone(Clone) ->
    lists:all(fun(K) -> is_equal_clone_item(K) end, Clone).

is_equal_clone_item(#clone_item{items = Items}) ->
    Head = hd(Items),
    HeadTokenList = Head#unit.alphabet,
    lists:all(fun(#unit{alphabet = Alphabet}) ->
        Alphabet == HeadTokenList 
    end, Items).


%% @doc Testfiles.
files() ->
    ["group1.erl","group2.erl","group3.erl","group4.erl","group5.erl",
     "test1.erl","test2.erl","test3.erl","test4.erl","test5.erl","test6.erl",
     "test8.erl","test9.erl","test10.erl","test11.er","mnesia_test.erl"].

reset()->
    make_ui_request({reset}).


add(File)->
    make_ui_request({add, File}).


make_ui_request(UIArgs)->
    make_ui_request(UIArgs, []).

make_ui_request(UIArgs, Args)->
    try
        ReqID = ?UI:getid(),
        case ?UI:request(ReqID,UIArgs) of
            deny -> {error, {deny, "The request was denied by the job server."++
                                    "Please, try again later."}};
            ok -> ui_loop(ReqID, Args)
        end
    catch
        exit:Reason ->
            Log = reftest_db_test_framework:open_log(append),
            file:write(Log, io_lib:format("EXIT: ~p <br>",[Reason])),
            file:close(Log),
            %{'EXIT',Reason},
            exit(Reason);
        error:Reason ->
            Log = reftest_db_test_framework:open_log(append),
            file:write(Log, io_lib:format("ERROR: ~p <br>",[Reason])),
            file:close(Log)
    end.

ui_loop(ReqID, Args)->
    receive
        {ReqID, reply, R} ->
            R;
        _ ->
            ui_loop(ReqID, Args)
    end.
