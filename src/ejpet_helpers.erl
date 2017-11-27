-module(ejpet_helpers).
-author('nicolas.michel.lava@gmail.com').

-export([melt/2]).


melt(Empty, M) when map_size(Empty) == 0 ->
    M;
melt(M, Empty) when map_size(Empty) == 0 ->
    M;
melt(M1, M2) when is_map(M1) and is_map(M2) ->
    L1 = maps:to_list(M1),
    L2 = maps:to_list(M2),
    L3 = melt(L1, L2),
    maps:from_list(L3);
melt([], L) ->
    L;
melt(L, []) ->
    L;
melt(L1, L2) ->
    lists:reverse(melt_(L1, L2, [])).

melt_([], List, Acc) ->
    List ++ Acc;
melt_(List, [], Acc) ->
    List ++ Acc;
melt_([{Key, Values} | Tail], List, Acc) ->
    OtherValues = proplists:get_value(Key, List, []),
    OtherListTail = proplists:delete(Key, List),
    melt_(Tail, OtherListTail, [{Key, Values ++ OtherValues} | Acc]).
