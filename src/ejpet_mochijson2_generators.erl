-module(ejpet_mochijson2_generators).

-export([generate_matcher/2]).


%% ---- Capture 

generate_matcher({capture, Pattern, Mode}, Options) ->
    Matcher = generate_matcher(Pattern, Options),
    fun(JSON, Params) ->
            case Matcher(JSON, Params) of
                {true, Captures} ->
                    {true, [{Mode, JSON} | Captures]};
                R ->
                    R
            end
    end;

%% ---- Injection 

generate_matcher({inject, Type, Name}, Options) when is_list(Name) ->
    generate_matcher({inject, Type, list_to_binary(Name)}, Options);
generate_matcher({inject, boolean, Name}, _Options) ->
    fun(What, Params) when What == true; What == false ->
            case proplists:get_value(Name, Params) of 
                What ->
                    {true, []};
                _ ->
                    {false, []}
            end;
       (_, _Params) ->
            {false, []}
    end;
generate_matcher({inject, string, Name}, _Options) ->
    fun(What, Params) when is_binary(What) ->
            case proplists:get_value(Name, Params) of 
                What ->
                    {true, []};
                _ ->
                    {false, []}
            end;
       (_, _Params) ->
            {false, []}
    end;
generate_matcher({inject, number, Name}, Options) ->
    fun(What, Params) when is_number(What) ->
            case proplists:get_value(Name, Params) of 
                undefined ->
                    {false, []};
                Number ->
                    case proplists:get_value(number_strict_match, Options) of 
                        true ->
                            case What =:= Number of
                                true ->
                                    {true, []};
                                _ ->
                                    {false, []}
                            end;
                        _ ->
                            case What == Number of
                                true ->
                                    {true, []};
                                _ ->
                                    {false, []}
                            end
                    end
            end;
       (_, _Params) ->
            {false, []}
    end;
generate_matcher({inject, regex, Name}, _Options) ->
    fun(What, Params) when is_binary(What) ->
            case proplists:get_value(Name, Params) of 
                undefined ->
                    {false, []};
                MP ->
                    try re:run(What, MP) of 
                        {match, _} ->
                           {true, []};
                        _ ->
                           {false, []}
                    catch
                        _:_ ->
                           {false, []}
                    end
            end;
       (_, _Params) ->
            {false, []}
    end;

%% ---- Object

generate_matcher({object, any}, _Options) ->
    fun({struct, Pairs}, _Params) when is_list(Pairs) ->
            {true, []};
       (_, _Params) ->
            {false, []}
    end;
generate_matcher({object, Conditions}, Options) ->
    PairMatchers = lists:map(fun(C) ->
                                     generate_matcher(C, Options)
                             end, Conditions),
    fun({struct, Items}, Params) when is_list(Items) ->
            R = [continue_until_match(Items, PairMatcher, Params) || PairMatcher <- PairMatchers],
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
       (_, _Params) ->
            {false, []}
    end;
generate_matcher({pair, any, ValMatcherDesc}, Options) ->
    ValMatcher = generate_matcher(ValMatcherDesc, Options),
    fun({_Key, Val}, Params) ->
            ValMatcher(Val, Params);
       (_, _Params) ->
            {false, []}
    end;
generate_matcher({pair, KeyMatcherDesc, any}, Options) ->
    KeyMatcher = generate_matcher(KeyMatcherDesc, Options),
    fun({Key, _Val}, Params) ->
            KeyMatcher(Key, Params);
       (_, _Params) ->
            {false, []}
    end;
generate_matcher({pair, KeyMatcherDesc, ValMatcherDesc}, Options) ->
    KeyMatcher = generate_matcher(KeyMatcherDesc, Options),
    ValMatcher = generate_matcher(ValMatcherDesc, Options),
    fun({Key, Val}, Params) ->
            {S1, Cap1} = KeyMatcher(Key, Params),
            {S2, Cap2} = ValMatcher(Val, Params),
            case S1 and S2 of
                true ->
                    {true, Cap1 ++ Cap2};
                _ ->
                    {false, []}
            end;
       (_, _Params) ->
            {false, []}
    end;

%% ---- List

generate_matcher({list, empty}, _Options) ->
    fun([], _Params) ->
            {true, []};
       (_, _Params) ->
            {false, []}
    end;

generate_matcher({list, any}, _Options) ->
    fun(Items, _Params) when is_list(Items) ->
            {true, []};
       (_, _Params) ->
            {false, []}
    end;
generate_matcher({list, Conditions}, Options) ->
    ItemMatchers = lists:map(fun({find, Expr}) ->
                                     Matcher = generate_matcher(Expr, Options),
                                     fun(Items, Params) when is_list(Items) ->
                                             continue_until_match(Items, Matcher, Params);
                                        (_, _Params) ->
                                             {{false, []}, []}
                                     end;
                                (Expr) ->
                                     Matcher = generate_matcher(Expr, Options),
                                     fun([], Params) ->
                                             {Matcher([], Params), []};
                                        ([Head|Tail], Params) ->
                                             {Matcher(Head, Params), Tail};
                                        (_, _Params) ->
                                             {{false, []}, []}
                                     end
                             end, Conditions),
    fun(Items, Params) ->
            {Statuses, _Tail} =
                lists:foldl(fun(Matcher, {Acc, ItemList})->
                                    {S, R} = Matcher(ItemList, Params),
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
    fun({struct, _What}, _Params) ->
            {true, []};
       (What, _Params) when is_list(What) ->
            {true, []};
       (_, _Params) ->
            {false, []}
    end;

generate_matcher({iterable, Conditions}, Options) ->
    Matchers = lists:map(fun(C) ->
                                 generate_matcher(C, Options)
                         end, Conditions),
    fun({struct, Items}, Params) ->
            R = [continue_until_object_value_match(Items, Matcher, Params) || Matcher <- Matchers],
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
       (Items, Params) when is_list(Items) ->
            R = [continue_until_list_value_match(Items, Matcher, Params) || Matcher <- Matchers],
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
       (_, _Params) ->
            {false, []}
    end;

%% ----- Descedant

generate_matcher({descendant, Conditions}, Options) ->
    Matchers = lists:map(fun(C) ->
                                 generate_matcher(C, Options)
                         end, Conditions),
    fun({struct, Items}, Params) ->
            R = [deep_continue_until_object_value_match(Items, Matcher, Params) || Matcher <- Matchers],
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
       (Items, Params) when is_list(Items) ->
            R = [deep_continue_until_list_value_match(Items, Matcher, Params) || Matcher <- Matchers],
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
       (_, _Params) ->
            {false, []}
    end;

%% ---- Unit

generate_matcher({string, BinString}, _Options) ->
    fun(What, _Params) ->
            case What of 
                BinString ->
                    {true, []};
                (_) ->
                    {false, []}
            end
    end;
generate_matcher({regex, BinString}, Options) ->
    %% TODO - move the production of MP into the parser, which will store a evaluation function
    %% instead of String. Compile options should be passed to the parser, and also the runtime options.
    %% 
    {ok, MP} = re:compile(BinString, Options),
    fun(What, _Params) when is_list(What); is_binary(What) ->
            try re:run(What, MP) of 
                {match, _} ->
                    {true, []};
                _ ->
                    {false, []}
            catch 
                _:_ ->
                    {false, []}
            end;
       (_, _Params) ->
            {false, []}
    end;
generate_matcher({number, Number}, Options) ->
    case proplists:get_value(number_strict_match, Options) of 
        true ->
            fun(What, _Params) when is_number(What) ->
                    case What of 
                        Number ->
                            {true, []};
                        _ ->
                            {false, []}
                    end;
               (_, _Params) ->
                    {false, []}
            end;
        _ ->
            fun(What, _Params) when is_number(What) ->
                    case What == Number of 
                        true ->
                            {true, []};
                        _ ->
                            {false, []}
                    end;
               (_, _Params) ->
                    {false, []}
            end
    end;
generate_matcher(any, _Options) ->
    fun(_, _Params) ->
            {true, []}
    end;
generate_matcher(What, _Options) when What == true;
                                      What == false;
                                      What == null ->
    fun(Item, _Params) ->
            case Item of
                What ->
                    {true, []};
                (_) ->
                    {false, []}
            end
    end;
generate_matcher(eol, _Options) ->
    fun([], _Params) ->
            {true, []};
       (_, _Params) ->
            {false, []}
    end.

%% -----

continue_until_match([], Matcher, Params) ->
    {Matcher([], Params), []};
continue_until_match([Item | Tail], Matcher, Params) ->
    case Matcher(Item, Params) of 
        R = {true, _} ->
            {R, Tail};
        _ ->
            continue_until_match(Tail, Matcher, Params)
    end.

continue_until_object_value_match([], _Matcher, _Params) ->
    {{false, []}, []};
continue_until_object_value_match([{_Key, Val} | Tail], Matcher, Params) ->
    case Matcher(Val, Params) of 
        R = {true, _} ->
            {R, Tail};
        _ ->
            continue_until_object_value_match(Tail, Matcher, Params)
    end.

continue_until_list_value_match([], _Matcher, _Params) ->
    {{false, []}, []};
continue_until_list_value_match([Item | Tail], Matcher, Params) ->
    case Matcher(Item, Params) of 
        R = {true, _} ->
            {R, Tail};
        _ ->
            continue_until_list_value_match(Tail, Matcher, Params)
    end.


deep_continue_until_object_value_match([], _Matcher, _Params) ->
    {{false, []}, []};
deep_continue_until_object_value_match([{_Key, Val} | Tail], Matcher, Params) ->
    case Matcher(Val, Params) of 
        R = {true, _} ->
            {R, Tail};
        _ ->
            case Val of
                {struct, Pairs} ->
                    case deep_continue_until_object_value_match(Pairs, Matcher, Params) of 
                        {R2 = {true, _}, _} ->
                            {R2, Tail};
                        _ ->
                            deep_continue_until_object_value_match(Tail, Matcher, Params)
                    end;
                [_|_] ->
                    case deep_continue_until_list_value_match(Val, Matcher, Params) of 
                        {R2 = {true, []}, _R} ->
                            {R2, Tail};
                        _ ->
                            deep_continue_until_object_value_match(Tail, Matcher, Params)
                    end;
                _ ->
                    deep_continue_until_object_value_match(Tail, Matcher, Params)
            end
    end.

deep_continue_until_list_value_match([], _Matcher, _Params) ->
    {{false, []}, []};
deep_continue_until_list_value_match([Item | Tail], Matcher, Params) ->
    case Matcher(Item, Params) of 
        R = {true, _} ->
            {R, Tail};
        _ ->
            case Item of
                {struct, Pairs} ->
                    case deep_continue_until_object_value_match(Pairs, Matcher, Params) of 
                        {R2 = {true, _}, _} ->
                            {R2, Tail};
                        _ ->
                            deep_continue_until_list_value_match(Tail, Matcher, Params)
                    end;
                [_|_] ->
                    case deep_continue_until_list_value_match(Item, Matcher, Params) of 
                        {R2 = {true, _}, _} ->
                            {R2, Tail};
                        _ ->
                            deep_continue_until_list_value_match(Tail, Matcher, Params)
                    end;
                _ ->
                    deep_continue_until_list_value_match(Tail, Matcher, Params)
            end
    end.
