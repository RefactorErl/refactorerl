-module(test_dataflow).

-export([analyse_test/2, test/2]).

test(List, Type) when is_list(List) ->
    dets:open_file(Type, [{type, set}]),
    Result = [test(Expr, Type) || 
                  Expr <- List, 
                  try reflib_expression:type({'$gn',expr,Expr}) /= fret  
                      andalso
                      reflib_expression:type({'$gn',expr,Expr}) /= fpar
                  of 
                      A -> A
                  catch 
                      _:_ -> false
                  end],
    %%dets:to_ets(Type, ets:new(Type, [ordered_set])),
    dets:close(Type),
    Result;
test(Expr, Type)->
io:format("~p ~n", [Expr]),
    case Type of 
        forw -> Par = [];
        back -> Par = [back]
    end,
    {A, B} = timer:tc(refanal_dataflow, reach, [[{'$gn',expr,Expr}],Par, true]),
    {C, D} = timer:tc(refanal_dataflow, reach_1st, [[{'$gn',expr,Expr}],Par, true]),
    Intersect = B -- (B --D),
    LengthD = length(D), 
    LengthB = length(B),
    Time = A/C,
    Length = LengthD/LengthB,
    Bool = Intersect == D,
    dets:insert(Type, {{Expr, 0}, LengthB, A, B, Time, Length, Bool}),
    dets:insert(Type, {{Expr, 1}, LengthD, C, D, Time, Length, Bool}),
%%io:format("~p~n", [Expr]),
    {length(B), {Time, Length, Bool}}.

analyse_test([], _)  -> 
    ok;
analyse_test(List, Type)->
    Result = test(List, Type),
    {_Result1, Result2} = lists:unzip(Result),
    {analyse_test1(Result2), analyse_test2(Result)}.

analyse_test1([]) ->
    ok;
analyse_test1(List) ->
    {Time, Length, Equal} = lists:unzip3(List),
    All = lists:all(fun(X) -> X end, Equal),
    Length2 = lists:filter(fun(X) -> X /= 1.0 end, Length),
    io:format("Best time and length: ~p~nSmaller result set: ~p~nCheck: ~p~n", 
              [{lists:max(Time), lists:min(Length)}, 
               {length(Length2), Length2}, All]).

analyse_test2([]) ->
    ok;
analyse_test2(List)->
    Result = [ Res || {B, Res} <- List, B /= 1], 
    {Time, Length, _Equal} = lists:unzip3(Result),
    S = [{T, L} || {T, L, _} <- Result, T < 1.0 ],
    Slow = [T || T <- Time, T < 1.0],
    io:format("Slower: ~p~n", 
              [{length(Slow), lists:min(Slow), S}]),
    io:format("Avg: ~p~n", 
              [{avg(Time), avg(Length)}]).

avg([]) ->
    hiba;
avg(List) ->
    lists:sum(List)/length(List).
