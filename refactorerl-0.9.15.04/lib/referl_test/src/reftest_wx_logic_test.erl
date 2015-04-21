-module(reftest_wx_logic_test).

%% -compile(export_all).

-include("test.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("referl_ui/src/wx.hrl").

tree_new_test() ->
    ?assertEqual(?Logic:tree_new(), []).

tree_new_root_test() ->
    Id = 1,
    Data = my_data,
    ?assertEqual([{Id,no_parent,Data,[]}],
                 ?Logic:tree_root(Id, Data)).

tree_add_node_test() ->
    Id = 1,
    Data = my_data,
    Tree = ?Logic:tree_root(Id, Data),
    IdNew = Id + 1,
    DataNew = my_data_new,
    OkResult = [{Id,no_parent,Data,[IdNew]}, {IdNew,Id,DataNew,[]}],
    ?assertEqual(OkResult,
                 ?Logic:tree_add_node(Tree, Id, IdNew, DataNew, basic_eq())).

tree_node_to_new_tree_test() ->
    NewTreeId = 1,
    A = 0,
    C = 2,
    Tree = tree_with_three_nodes(A, NewTreeId, C),
    SmallTree = ?Logic:tree_root(1, data(NewTreeId)),
    Result = ?Logic:tree_node_to_new_tree(Tree, NewTreeId, basic_eq()),
    SupposedToDelete = [A,C],
    ?assertEqual({SmallTree, SupposedToDelete}, Result).

tree_get_node_test() -> 
    Src1 = 3,
    Src2 = 10,
    Tree = tree_with_three_nodes_and_parents({1, no_parent},{2, 1},{Src1, 1}),
    Node = ?Logic:tree_get_node(Tree, Src1, basic_eq()),
    Node2 = ?Logic:tree_get_node(Tree, Src2, basic_eq()),
    ?assertMatch({Src1,_,_,_}, Node),
    ?assertEqual(notfound, Node2).
    
tree_with_three_nodes(A,B,C) -> 
    tree_with_three_nodes_and_parents({A, no_parent},{B, A},{C, B}).
tree_with_three_nodes_and_parents({A, _},{B, BP},{C, CP}) ->
    RootData = data(A),
    D1 = data(B),
    D2 = data(C),
    ?Logic:tree_add_node(
        ?Logic:tree_add_node(?Logic:tree_root(A, RootData),
                             BP, B, D1, basic_eq()), 
                                CP, C, D2, basic_eq()).

tree_delete_subtree_test() ->
    A = 2,
    DelId = 3,
    C = 6,
    Tree = tree_with_three_nodes(A,DelId,C),
    SmallTree = ?Logic:tree_root(A, data(A)),
    {ResultTree, Deleted} = ?Logic:tree_delete_subtree(Tree, DelId, basic_eq()),
    SupposedToDelete = [DelId, C],
    ?assertEqual({SmallTree, SupposedToDelete}, {ResultTree, lists:sort(Deleted)}).

tree_delete_node_test() ->
    A = 5,
    B = 4,
    DelId = 6,
    Tree = tree_with_three_nodes(A,B,DelId),
    AnotherTree = ?Logic:tree_add_node(?Logic:tree_root(A, data(A)), 
                                       A, B, data(B), basic_eq()),
    ResultTree = ?Logic:tree_delete_node(Tree, DelId, basic_eq()),
    ?assertEqual(AnotherTree, ResultTree).

tree_get_subtree_test() ->
    A = 5,
    B = 4,
    C = 6,
    Tree = tree_with_three_nodes(A,B,C),
    SmallTree = ?Logic:tree_root(C, data(C)),
    {NewTree, Deleted} = ?Logic:tree_get_subtree(Tree, C, basic_eq()),
    SupposedToDelete = lists:sort([A, B]),
    ?assertEqual({SmallTree, SupposedToDelete}, {NewTree, lists:sort(Deleted)}).

tree_transformation_test() ->
    Tree = tree_with_three_nodes(1,2,3),
    Tree2 = tree_with_three_nodes(4,5,6),
    NewTree = ?Logic:tree_transformation(Tree,
                       fun(no_parent)-> no_parent;
                          (A)-> A + 3 end
                      ,fun(_, Atom) -> L = atom_to_list(Atom),  
                                    Ts = string:tokens(L, "_"),
                                    Num = list_to_integer(lists:last(Ts)),
                                    ?d(Num),
                                    data(Num + 3) end),
    ?assertEqual(Tree2, NewTree).
                                    

basic_eq() -> fun(A,B) -> A == B end.
data(Num) -> list_to_atom("my_data_" ++ integer_to_list(Num)).

pairs_test() ->
    Result = ?Logic:pairs([1,2,3,4,5,6], fun(A) -> A + 4 end),
    OkResult = [{5,6}, {7,8}, {9,10}],
    ?assertEqual(OkResult, Result),
    ?assertError(function_clause, ?Logic:pairs([1], fun(A) -> A + 7 end)).

pairs2_test() ->
    Result = ?Logic:pairs2([1,2,3,4,5,6], fun(A) -> A + 4 end),
    OkResult = [{5,6}, {6,7},  {7,8}, {8,9}, {9,10}],
    ?assertEqual(OkResult, Result),
    ?assertEqual([{8, 8}], ?Logic:pairs2([1], fun(A) -> A + 7 end)).

floor_and_ceil_test() ->
    {inorder,
        [
           ?assertEqual(3, ?Logic:floor(math:pi())),
           ?assertEqual(4, ?Logic:ceil(math:pi()))
        ]}.

text_functions_test() ->
    {inorder,
        [
           ?assertEqual("123456789", 
                        ?Logic:trim_whitespace("  \n\n \t123456789\n \n \t  ")),
           ?assertEqual("123", ?Logic:write("123")),
           ?assertEqual("123", ?Logic:write(123)),
           ?assertEqual("term", ?Logic:write(term)),
           ?assertEqual(314,  ?Logic:list_to_num("314")),
           ?assertEqual(3.14,  ?Logic:list_to_num("3.14")),
           ?assertEqual("314",  ?Logic:num_to_list(314)),
           ?assertEqual(float_to_list(3.14),  ?Logic:num_to_list(3.14))
        ]}.
