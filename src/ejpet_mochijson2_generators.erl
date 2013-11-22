-module(ejpet_mochijson2_generators).

-export([generate_matcher/2]).


%% ---- Capture 

generate_matcher({capture, Pattern, Mode}, Options) ->
    Matcher = generate_matcher(Pattern, Options),
    fun(JSON) ->
            case Matcher(JSON) of
                {true, Captures} ->
                    {true, [{Mode, JSON} | Captures]};
                R ->
                    R
            end
    end;

%% ---- Object

generate_matcher({object, any}, _Options) ->
    fun({struct, Pairs}) when is_list(Pairs) ->
            {true, []};
       (_) ->
            {false, []}
    end;
generate_matcher({object, Conditions}, Options) ->
    PairMatchers = lists:map(fun(C) ->
                                     generate_matcher(C, Options)
                             end, Conditions),
    fun({struct, Items}) when is_list(Items) ->
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
generate_matcher({pair, any, ValMatcherDesc}, Options) ->
    ValMatcher = generate_matcher(ValMatcherDesc, Options),
    fun({_Key, Val}) ->
            ValMatcher(Val);
       (_) ->
            {false, []}
    end;
generate_matcher({pair, KeyMatcherDesc, any}, Options) ->
    KeyMatcher = generate_matcher(KeyMatcherDesc, Options),
    fun({Key, _Val}) ->
            KeyMatcher(Key);
       (_) ->
            {false, []}
    end;
generate_matcher({pair, KeyMatcherDesc, ValMatcherDesc}, Options) ->
    KeyMatcher = generate_matcher(KeyMatcherDesc, Options),
    ValMatcher = generate_matcher(ValMatcherDesc, Options),
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

generate_matcher({list, empty}, _Options) ->
    fun([]) ->
            {true, []};
       (_) ->
            {false, []}
    end;

generate_matcher({list, any}, _Options) ->
    fun(Items) when is_list(Items) ->
            {true, []};
       (_) ->
            {false, []}
    end;
generate_matcher({list, Conditions}, Options) ->
    ItemMatchers = lists:map(fun({find, Expr}) ->
                                     Matcher = generate_matcher(Expr, Options),
                                     fun(Items) when is_list(Items) ->
                                             continue_until_match(Items, Matcher);
                                        (_) ->
                                             {{false, []}, []}
                                     end;
                                (Expr) ->
                                     Matcher = generate_matcher(Expr, Options),
                                     fun([]) ->
                                             {Matcher([]), []};
                                        ([Head|Tail]) ->
                                             {Matcher(Head), Tail};
                                        (_) ->
                                             {{false, []}, []}
                                     end
                             end, Conditions),
    fun(Items) ->
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
            end
    end;

%% ----- Iterable

generate_matcher({iterable, any}, _Options) ->
    %% jsx represents both list and object as erlang lists.
    %% Therefore, checking if an item is an erlang list is enough to say 
    %% that it is an iterable.
    %% 
    fun({struct, _What}) ->
            {true, []};
       (What) when is_list(What) ->
            {true, []};
       (_) ->
            {false, []}
    end;

generate_matcher({iterable, Conditions}, Options) ->
    Matchers = lists:map(fun(C) ->
                                 generate_matcher(C, Options)
                         end, Conditions),
    fun({struct, Items}) ->
            R = [continue_until_object_value_match(Items, Matcher) || Matcher <- Matchers],
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
       (Items) when is_list(Items) ->
            R = [continue_until_list_value_match(Items, Matcher) || Matcher <- Matchers],
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

%% ----- Descedant

generate_matcher({descendant, Conditions}, Options) ->
    Matchers = lists:map(fun(C) ->
                                 generate_matcher(C, Options)
                         end, Conditions),
    fun({struct, Items}) ->
            R = [deep_continue_until_object_value_match(Items, Matcher) || Matcher <- Matchers],
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
       (Items) when is_list(Items) ->
            R = [deep_continue_until_list_value_match(Items, Matcher) || Matcher <- Matchers],
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

generate_matcher({string, String}, _Options) ->
    BinString = list_to_binary(String),
    fun(What) ->
            case What of 
                BinString->
                    {true, []};
                (_) ->
                    {false, []}
            end
    end;
generate_matcher({regex, String}, Options) ->
    %% TODO - move the production of MP into the parser, which will store a evaluation function
    %% instead of String. Compile options should be passed to the parser, and also the runtime options.
    %% 
    BinString = list_to_binary(String),
    Options = [],
    {ok, MP} = re:compile(BinString, Options),
    fun(What) when is_list(What); is_binary(What) ->
            case re:run(What, MP) of 
                {match, _}->
                    {true, []};
                _ ->
                    {false, []}
            end;
       (_) ->
            {false, []}
    end;
generate_matcher({number, Number}, Options) ->
    case proplists:get_value(number_strict_match, Options) of 
        true ->
            fun(What) ->
                    case What of 
                        Number ->
                            {true, []};
                        _ ->
                            {false, []}
                    end
            end;
        _ ->
            fun(What) ->
                    case What == Number of 
                        true ->
                            {true, []};
                        _ ->
                            {false, []}
                    end
            end
    end;
generate_matcher(any, _Options) ->
    fun(_) ->
            {true, []}
    end;
generate_matcher(What, _Options) when What == true;
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
generate_matcher(eol, _Options) ->
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

continue_until_object_value_match([], _Matcher) ->
    {{false, []}, []};
continue_until_object_value_match([{_Key, Val} | Tail], Matcher) ->
    case Matcher(Val) of 
        R = {true, _} ->
            {R, Tail};
        _ ->
            continue_until_object_value_match(Tail, Matcher)
    end.

continue_until_list_value_match([], _Matcher) ->
    {{false, []}, []};
continue_until_list_value_match([Item | Tail], Matcher) ->
    case Matcher(Item) of 
        R = {true, _} ->
            {R, Tail};
        _ ->
            continue_until_list_value_match(Tail, Matcher)
    end.


deep_continue_until_object_value_match([], _Matcher) ->
    {{false, []}, []};
deep_continue_until_object_value_match([{_Key, Val} | Tail], Matcher) ->
    case Matcher(Val) of 
        R = {true, _} ->
            {R, Tail};
        _ ->
            case Val of
                {struct, Pairs} ->
                    case deep_continue_until_object_value_match(Pairs, Matcher) of 
                        {R2 = {true, _}, _} ->
                            {R2, Tail};
                        _ ->
                            deep_continue_until_object_value_match(Tail, Matcher)
                    end;
                [_|_] ->
                    case deep_continue_until_list_value_match(Val, Matcher) of 
                        {R2 = {true, []}, _R} ->
                            {R2, Tail};
                        _ ->
                            deep_continue_until_object_value_match(Tail, Matcher)
                    end;
                _ ->
                    deep_continue_until_object_value_match(Tail, Matcher)
            end
    end.

deep_continue_until_list_value_match([], _Matcher) ->
    {{false, []}, []};
deep_continue_until_list_value_match([Item | Tail], Matcher) ->
    case Matcher(Item) of 
        R = {true, _} ->
            {R, Tail};
        _ ->
            case Item of
                {struct, Pairs} ->
                    case deep_continue_until_object_value_match(Pairs, Matcher) of 
                        {R2 = {true, _}, _} ->
                            {R2, Tail};
                        _ ->
                            deep_continue_until_list_value_match(Tail, Matcher)
                    end;
                [_|_] ->
                    case deep_continue_until_list_value_match(Item, Matcher) of 
                        {R2 = {true, _}, _} ->
                            {R2, Tail};
                        _ ->
                            deep_continue_until_list_value_match(Tail, Matcher)
                    end;
                _ ->
                    deep_continue_until_list_value_match(Tail, Matcher)
            end
    end.
