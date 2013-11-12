-module(ejpet_mochijson2_generators).

-export([generate_matcher/1]).


%% ---- Object

generate_matcher({object, any}) ->
    fun({struct, Pairs}) when is_list(Pairs) ->
            true;
       (_) ->
            false
    end;
generate_matcher({object, Conditions}) ->
    PairMatchers = lists:map(fun generate_matcher/1, Conditions),
    fun({struct, Items}) when is_list(Items) ->
            R = [continue_until_match(Items, PairMatcher) || PairMatcher <- PairMatchers],
            NonSatisfied = lists:dropwhile(fun({true, _}) ->
                                                   true;
                                              (_) ->
                                                   false
                                           end, R),
            NonSatisfied == [];
       (_) ->
            false
    end;
generate_matcher({pair, any, ValMatcherDesc}) ->
    ValMatcher = generate_matcher(ValMatcherDesc),
    fun({_Key, Val}) ->
            ValMatcher(Val);
       (_) ->
            false
    end;
generate_matcher({pair, KeyMatcherDesc, any}) ->
    KeyMatcher = generate_matcher(KeyMatcherDesc),
    fun({Key, _Val}) ->
            KeyMatcher(Key);
       (_) ->
            false
    end;
generate_matcher({pair, KeyMatcherDesc, ValMatcherDesc}) ->
    KeyMatcher = generate_matcher(KeyMatcherDesc),
    ValMatcher = generate_matcher(ValMatcherDesc),
    fun({Key, Val}) ->
            KeyMatcher(Key) and ValMatcher(Val);
       (_) ->
            false
    end;

%% ---- List

generate_matcher({list, empty}) ->
    fun([]) ->
            true;
       (_) ->
            false
    end;

generate_matcher({list, any}) ->
    fun(Items) when is_list(Items) ->
            true;
       (_) ->
            false
    end;
generate_matcher({list, Conditions}) ->
    ItemMatchers = lists:map(fun({find, Expr}) ->
                                     Matcher = generate_matcher(Expr),
                                     fun(Items) when is_list(Items) ->
                                             continue_until_match(Items, Matcher);
                                        (_) ->
                                             {false, []}
                                     end;
                                (Expr) ->
                                     Matcher = generate_matcher(Expr),
                                     fun([]) ->
                                             {Matcher([]), []};
                                        ([Head|Tail]) ->
                                             {Matcher(Head), Tail};
                                        (_) ->
                                             {false, []}
                                     end
                             end, Conditions),
    fun(Items) ->
            {Statuses, _Tail} =
                lists:foldl(fun(Matcher, {Acc, ItemList})->
                                    {S, R} = Matcher(ItemList),
                                    {[S | Acc], R}
                            end, {[], Items}, ItemMatchers),
            lists:foldl(fun(S, Acc) ->
                                S and Acc
                        end, true, lists:reverse(Statuses)) % lists:reverse() could be omitted here (because we only do boolean operations).
    end;

%% ----- Iterable

generate_matcher({iterable, any}) ->
    %% jsx represents both list and object as erlang lists.
    %% Therefore, checking if an item is an erlang list is enough to say 
    %% that it is an iterable.
    %% 
    fun({struct, _What}) ->
            true;
       (What) when is_list(What) ->
            true;
       (_) ->
            false
    end;

generate_matcher({iterable, Conditions}) ->
    Matchers = lists:map(fun generate_matcher/1, Conditions),
    fun({struct, Items}) ->
            R = [continue_until_object_value_match(Items, Matcher) || Matcher <- Matchers],
            NonSatisfied = lists:dropwhile(fun({true, _}) ->
                                                   true;
                                              (_) ->
                                                   false
                                           end, R),
            NonSatisfied == [];
       (Items) when is_list(Items) ->
            io:format("Match list as iterable ~p~n", [Items]),
            R = [continue_until_list_value_match(Items, Matcher) || Matcher <- Matchers],
            NonSatisfied = lists:dropwhile(fun({true, _}) ->
                                                   true;
                                              (_) ->
                                                   false
                                           end, R),
            NonSatisfied == [];
       (_) ->
            false
    end;

%% ----- Descedant

generate_matcher({descendant, Conditions}) ->
    Matchers = lists:map(fun generate_matcher/1, Conditions),
    fun({struct, Items}) ->
            R = [deep_continue_until_object_value_match(Items, Matcher) || Matcher <- Matchers],
            NonSatisfied = lists:dropwhile(fun({true, _}) ->
                                                   true;
                                              (_) ->
                                                   false
                                           end, R),
            NonSatisfied == [];
       (Items) when is_list(Items) ->
            R = [deep_continue_until_list_value_match(Items, Matcher) || Matcher <- Matchers],
            NonSatisfied = lists:dropwhile(fun({true, _}) ->
                                                   true;
                                              (_) ->
                                                   false
                                           end, R),
            NonSatisfied == [];
       (_) ->
            false
    end;

%% ---- Unit

generate_matcher({string, String}) ->
    BinString = list_to_binary(String),
    fun(What) ->
            case What of 
                BinString->
                    true;
                (_) ->
                    false
            end
    end;
generate_matcher({number, Number}) ->
    fun(What) ->
            case What of 
                Number ->
                    true;
                (_) ->
                    false
            end
    end;
generate_matcher(What) when What == true;
                            What == false;
                            What == null ->
    fun(Item) ->
            case Item of
                What ->
                    true;
                (_) ->
                    false
            end
    end;
generate_matcher(eol) ->
    fun([]) ->
            true;
       (_) ->
            false
    end.

%% -----

continue_until_match([], Matcher) ->
    {Matcher([]), []};
continue_until_match([Item | Tail], Matcher) ->
    case Matcher(Item) of 
        true ->
            {true, Tail};
        _ ->
            continue_until_match(Tail, Matcher)
    end.

continue_until_object_value_match([], _Matcher) ->
    {false, []};
continue_until_object_value_match([{Key, Val} | Tail], Matcher) ->
    case Matcher(Val) of 
        true ->
            {true, Tail};
        _ ->
            continue_until_object_value_match(Tail, Matcher)
    end.

continue_until_list_value_match([], _Matcher) ->
    {false, []};
continue_until_list_value_match([Item | Tail], Matcher) ->
    case Matcher(Item) of 
        true ->
            {true, Tail};
        _ ->
            continue_until_list_value_match(Tail, Matcher)
    end.


deep_continue_until_object_value_match([], _Matcher) ->
    {false, []};
deep_continue_until_object_value_match([{_Key, Val} | Tail], Matcher) ->
    case Matcher(Val) of 
        true ->
            {true, Tail};
        _ ->
            case Val of
                {struct, Pairs} ->
                    case deep_continue_until_object_value_match(Pairs, Matcher) of 
                        {true, _R} ->
                            {true, Tail};
                        _ ->
                            deep_continue_until_object_value_match(Tail, Matcher)
                    end;
                [_|_] ->
                    case deep_continue_until_list_value_match(Val, Matcher) of 
                        {true, _R} ->
                            {true, Tail};
                        _ ->
                            deep_continue_until_object_value_match(Tail, Matcher)
                    end;
                _ ->
                    deep_continue_until_object_value_match(Tail, Matcher)
            end
    end.

deep_continue_until_list_value_match([], _Matcher) ->
    {false, []};
deep_continue_until_list_value_match([Item | Tail], Matcher) ->
    case Matcher(Item) of 
        true ->
            {true, Tail};
        _ ->
            case Item of
                {struct, Pairs} ->
                    case deep_continue_until_object_value_match(Pairs, Matcher) of 
                        {true, _} ->
                            {true, Tail};
                        _ ->
                            deep_continue_until_list_value_match(Tail, Matcher)
                    end;
                [_|_] ->
                    case deep_continue_until_list_value_match(Item, Matcher) of 
                        {true, _} ->
                            {true, Tail};
                        _ ->
                            deep_continue_until_list_value_match(Tail, Matcher)
                    end;
                _ ->
                    deep_continue_until_list_value_match(Tail, Matcher)
            end
    end.
