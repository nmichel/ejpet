-module(ejpet_helpers).
-author('nicolas.michel.lava@gmail.com').

-export([melt/2]).


melt([], []) ->
    [];
melt([], L) ->
    L;
melt(L, []) ->
    L;
melt(L1, L2) ->
    melt_(L1, L2, []).

melt_([], List, Acc) ->
    List ++ Acc;
melt_(List, [], Acc) ->
    List ++ Acc;
melt_([{Key, Values} | Tail], List, Acc) ->
    OtherValues = proplists:get_value(Key, List, []),
    OtherListTail = proplists:delete(Key, List),
    melt_(Tail, OtherListTail, [{Key, Values ++ OtherValues} | Acc]).
