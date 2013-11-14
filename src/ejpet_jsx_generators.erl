-module(ejpet_jsx_generators).

-export([generate_matcher/1]).


%% ---- Capture 

generate_matcher({capture, Pattern, Mode}) ->
    Matcher = generate_matcher(Pattern),
    fun(JSON) ->
            case Matcher(JSON) of
                {true, Captures} ->
                    {true, [{Mode, JSON} | Captures]};
                R ->
                    R
            end
    end;


%% ---- Object

generate_matcher({object, any}) ->
    fun([{}]) ->
            {true, []};
       ([{_, _} | _]) ->
            {true, []};
       (_) ->
            {false, []}
    end;
generate_matcher({object, Conditions}) ->
    PairMatchers = lists:map(fun generate_matcher/1, Conditions),
    fun(Items) when is_list(Items) ->
            R = [continue_until_match(Items, PairMatcher) || PairMatcher <- PairMatchers],
            {AccCaptures, AccFailedCount} = 
                lists:foldl(fun({{true, Captures}, _}, {CapAcc, FailedAcc}) ->
                                    {Captures ++ CapAcc, FailedAcc};
                               (_, {CapAcc, FailedAcc}) ->
                                    {CapAcc, FailedAcc + 1}
                            end, {[], 0}, R),
            case AccFailedCount of
                0 ->
                    {true, AccCaptures};
                _ ->
                    {false, []}
            end;
       (_) ->
            {false, []}
    end;
generate_matcher({pair, any, ValMatcherDesc}) ->
    ValMatcher = generate_matcher(ValMatcherDesc),
    fun({_Key, Val}) ->
            ValMatcher(Val);
       (_) ->
            {false, []}
    end;
generate_matcher({pair, KeyMatcherDesc, any}) ->
    KeyMatcher = generate_matcher(KeyMatcherDesc),
    fun({Key, _Val}) ->
            KeyMatcher(Key);
       (_) ->
            {false, []}
    end;
generate_matcher({pair, KeyMatcherDesc, ValMatcherDesc}) ->
    KeyMatcher = generate_matcher(KeyMatcherDesc),
    ValMatcher = generate_matcher(ValMatcherDesc),
    fun({Key, Val}) ->
            {S1, Cap1} = KeyMatcher(Key),
            {S2, Cap2} = ValMatcher(Val),
            case S1 and S2 of
                true ->
                    {true, Cap1 ++ Cap2};
                _ ->
                    {false, []}
            end;
       (_) ->
            {false, []}
    end;

%% ---- List

generate_matcher({list, empty}) ->
    fun([]) ->
            {true, []};
       (_) ->
            {false, []}
    end;

generate_matcher({list, any}) ->
    fun([]) ->
            {true, []};
       ([{}]) -> % jsx special form for empty object
            {false, []};
       ([_|_]) ->
            {true, []};
       (_) ->
            {false, []}
    end;

generate_matcher({list, Conditions}) ->
    ItemMatchers = lists:map(fun({find, Expr}) ->
                                     Matcher = generate_matcher(Expr),
                                     fun([{}]) -> % jsx special form for empty object
                                             {{false, []}, []};
                                        (Items) when is_list(Items) ->
                                             continue_until_match(Items, Matcher);
                                        (_) ->
                                             {{false, []}, []}
                                     end;
                                (Expr) ->
                                     Matcher = generate_matcher(Expr),
                                     fun([{}]) -> % jsx special form for empty object
                                             {{false, []}, []};
                                        ([]) ->
                                             {Matcher([]), []};
                                        ([Head|Tail]) ->
                                             {Matcher(Head), Tail}
                                     end
                             end, Conditions),
    fun(Items) when is_list(Items) ->
            {Statuses, _Tail} =
                lists:foldl(fun(Matcher, {Acc, ItemList})->
                                    {S, R} = Matcher(ItemList),
                                    {[S | Acc], R}
                            end, {[], Items}, ItemMatchers),
            {FinalStatus, AccCaptures} = 
                lists:foldl(fun({S, Captures}, {Stat, Acc}) ->
                                    {S and Stat, Captures ++ Acc}
                            end, {true, []}, lists:reverse(Statuses)),
            case FinalStatus of
                true ->
                    {FinalStatus, AccCaptures};
                _ ->
                    {false, []}
            end;
       (_) ->
            {false, []}
    end;

%% ----- Iterable

generate_matcher({iterable, any}) ->
    %% jsx represents both list and object as erlang lists.
    %% Therefore, checking if an item is an erlang list is enough to say 
    %% that it is an iterable.
    %% 
    fun(What) when is_list(What) ->
            {true, []};
       (_) ->
            {false, []}
    end;

generate_matcher({iterable, Conditions}) ->
    Matchers = lists:map(fun generate_matcher/1, Conditions),
    fun(Items) when is_list(Items) ->
            R = [continue_until_value_match(Items, Matcher) || Matcher <- Matchers],
            {AccCaptures, AccFailedCount} = 
                lists:foldl(fun({{true, Captures}, _}, {CapAcc, FailedAcc}) ->
                                    {Captures ++ CapAcc, FailedAcc};
                               (_, {CapAcc, FailedAcc}) ->
                                    {CapAcc, FailedAcc + 1}
                            end, {[], 0}, R),
            case AccFailedCount of
                0 ->
                    {true, AccCaptures};
                _ ->
                    {false, []}
            end;
       (_) ->
            false
    end;

%% ----- Descedant

generate_matcher({descendant, Conditions}) ->
    Matchers = lists:map(fun generate_matcher/1, Conditions),
    fun (Items) when is_list(Items) ->
            R = [deep_continue_until_value_match(Items, Matcher) || Matcher <- Matchers],
            {AccCaptures, AccFailedCount} = 
                lists:foldl(fun({{true, Captures}, _}, {CapAcc, FailedAcc}) ->
                                    {Captures ++ CapAcc, FailedAcc};
                               (_, {CapAcc, FailedAcc}) ->
                                    {CapAcc, FailedAcc + 1}
                            end, {[], 0}, R),
            case AccFailedCount of
                0 ->
                    {true, AccCaptures};
                _ ->
                    {false, []}
            end;
        (_) ->
            {false, []}
    end;

%% ---- Unit

generate_matcher({string, String}) ->
    BinString = list_to_binary(String),
    fun(What) ->
            case What of 
                BinString->
                    {true, []};
                (_) ->
                    {false, []}
            end
    end;
generate_matcher({number, Number}) ->
    fun(What) ->
            case What of 
                Number ->
                    {true, []};
                (_) ->
                    {false, []}
            end
    end;
generate_matcher(What) when What == true;
                            What == false;
                            What == null ->
    fun(Item) ->
            case Item of
                What ->
                    {true, []};
                (_) ->
                    {false, []}
            end
    end;
generate_matcher(eol) ->
    fun([]) ->
            {true, []};
       (_) ->
            {false, []}
    end.

%% -----

continue_until_match([], Matcher) ->
    {Matcher([]), []};
continue_until_match([Item | Tail], Matcher) ->
    case Matcher(Item) of 
        R = {true, _} ->
            {R, Tail};
        _ ->
            continue_until_match(Tail, Matcher)
    end.

continue_until_value_match([{}], _Matcher) ->
    {{false, []}, []};
continue_until_value_match([], _Matcher) ->
    {{false, []}, []};
continue_until_value_match([{_Key, Val} | Tail], Matcher) ->
    case Matcher(Val) of 
        R = {true, _} ->
            {R, Tail};
        _ ->
            continue_until_value_match(Tail, Matcher)
    end;
continue_until_value_match([Item | Tail], Matcher) ->
    case Matcher(Item) of 
        R = {true, _} ->
            {R, Tail};
        _ ->
            continue_until_value_match(Tail, Matcher)
    end.

deep_continue_until_value_match([{}], _Matcher) ->
    {{false, []}, []};
deep_continue_until_value_match([], _Matcher) ->
    {{false, []}, []};
deep_continue_until_value_match([{_Key, Val} | Tail], Matcher) ->
    case Matcher(Val) of 
        R = {true, _} ->
            {R, Tail};
        _ ->
            case Val of
                [_|_] ->
                    case deep_continue_until_value_match(Val, Matcher) of 
                        {R2 = {true, _}, _} ->
                            {R2, Tail};
                        _ ->
                            deep_continue_until_value_match(Tail, Matcher)
                    end;
                _ ->
                    deep_continue_until_value_match(Tail, Matcher)
            end
    end;
deep_continue_until_value_match([Item | Tail], Matcher) ->
    case Matcher(Item) of 
        R = {true, _} ->
            {R, Tail};
        _ ->
            case Item of
                [_|_] ->
                    case deep_continue_until_value_match(Item, Matcher) of 
                        {R2 = {true, _}, _} ->
                            {R2, Tail};
                        _ ->
                            deep_continue_until_value_match(Tail, Matcher)
                    end;
                _ ->
                    deep_continue_until_value_match(Tail, Matcher)
            end
    end.
