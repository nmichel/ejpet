-module(ejpet_mochijson2_generators).
-author('nicolas.michel.lava@gmail.com').

-export([generate_matcher/3]).


%% ---- Capture 

generate_matcher({capture, Pattern, Mode}, Options, CB) ->
    Matcher = CB(Pattern, Options, CB),
    fun(JSON, Params) ->
            case Matcher(JSON, Params) of
                {true, Captures} ->
                    {true, add_captures(Captures, Mode, [JSON])};
                R ->
                    R
            end
    end;

%% ---- Injection 

generate_matcher({inject, Type, Name}, Options, CB) when is_list(Name) ->
    generate_matcher({inject, Type, list_to_binary(Name)}, Options, CB);
generate_matcher({inject, boolean, Name}, _Options, _CB) ->
    fun(What, Params) when What == true; What == false ->
            case proplists:get_value(Name, Params) of 
                What ->
                    {true, empty()};
                _ ->
                    {false, empty()}
            end;
       (_, _Params) ->
            {false, empty()}
    end;
generate_matcher({inject, string, Name}, _Options, _CB) ->
    fun(What, Params) when is_binary(What) ->
            case proplists:get_value(Name, Params) of 
                What ->
                    {true, empty()};
                _ ->
                    {false, empty()}
            end;
       (_, _Params) ->
            {false, empty()}
    end;
generate_matcher({inject, number, Name}, Options, _CB) ->
    fun(What, Params) when is_number(What) ->
            case proplists:get_value(Name, Params) of 
                undefined ->
                    {false, empty()};
                Number ->
                    case proplists:get_value(number_strict_match, Options) of 
                        true ->
                            case What =:= Number of
                                true ->
                                    {true, empty()};
                                _ ->
                                    {false, empty()}
                            end;
                        _ ->
                            case What == Number of
                                true ->
                                    {true, empty()};
                                _ ->
                                    {false, empty()}
                            end
                    end
            end;
       (_, _Params) ->
            {false, empty()}
    end;
generate_matcher({inject, regex, Name}, _Options, _CB) ->
    fun(What, Params) when is_binary(What) ->
            case proplists:get_value(Name, Params) of 
                undefined ->
                    {false, empty()};
                MP ->
                    try re:run(What, MP) of 
                        {match, _} ->
                           {true, empty()};
                        _ ->
                           {false, empty()}
                    catch
                        _:_ ->
                           {false, empty()}
                    end
            end;
       (_, _Params) ->
            {false, empty()}
    end;

%% ---- Object

generate_matcher({object, any}, _Options, _CB) ->
    fun({struct, Pairs}, _Params) when is_list(Pairs) ->
            {true, empty()};
       (_, _Params) ->
            {false, empty()}
    end;
generate_matcher({object, Conditions}, Options, CB) ->
    PairMatchers = lists:map(fun(C) ->
                                     CB(C, Options, CB)
                             end, Conditions),
    fun({struct, Items}, Params) when is_list(Items) ->
            R = [continue_until_match(Items, PairMatcher, Params) || PairMatcher <- PairMatchers],
            {AccCaptures, AccFailedCount} = 
                lists:foldl(fun({{true, Captures}, _}, {CapAcc, FailedAcc}) ->
                                    {melt_captures(CapAcc, Captures), FailedAcc};
                               (_, {CapAcc, FailedAcc}) ->
                                    {CapAcc, FailedAcc + 1}
                            end, {empty(), 0}, R),
            case AccFailedCount of
                0 ->
                    {true, AccCaptures};
                _ ->
                    {false, empty()}
            end;
       (_, _Params) ->
            {false, empty()}
    end;
generate_matcher({pair, any, ValMatcherDesc}, Options, CB) ->
    ValMatcher = CB(ValMatcherDesc, Options, CB),
    fun({_Key, Val}, Params) ->
            ValMatcher(Val, Params);
       (_, _Params) ->
            {false, empty()}
    end;
generate_matcher({pair, KeyMatcherDesc, any}, Options, CB) ->
    KeyMatcher = generate_matcher(KeyMatcherDesc, Options, CB),
    fun({Key, _Val}, Params) ->
            KeyMatcher(Key, Params);
       (_, _Params) ->
            {false, empty()}
    end;
generate_matcher({pair, KeyMatcherDesc, ValMatcherDesc}, Options, CB) ->
    KeyMatcher = generate_matcher(KeyMatcherDesc, Options, CB),
    ValMatcher = CB(ValMatcherDesc, Options, CB),
    fun({Key, Val}, Params) ->
            {S1, Cap1} = KeyMatcher(Key, Params),
            {S2, Cap2} = ValMatcher(Val, Params),
            case S1 and S2 of
                true ->
                    {true, melt_captures(Cap1, Cap2)};
                _ ->
                    {false, empty()}
            end;
       (_, _Params) ->
            {false, empty()}
    end;

%% ---- List

generate_matcher({list, empty}, _Options, _CB) ->
    fun([], _Params) ->
            {true, empty()};
       (_, _Params) ->
            {false, empty()}
    end;

generate_matcher({list, any}, _Options, _CB) ->
    fun(Items, _Params) when is_list(Items) ->
            {true, empty()};
       (_, _Params) ->
            {false, empty()}
    end;
generate_matcher({list, Conditions}, Options, CB) ->
    ItemMatchers = lists:map(fun({find, Exprs}) ->
                                     Matchers = lists:foldr(fun(Expr, Acc) ->
                                                                [CB(Expr, Options, CB) | Acc]
                                                            end, [], Exprs),
                                     fun(Items, Params) when is_list(Items) ->
                                             continue_until_span_match(Items, Matchers, Params);
                                        (_, _Params) ->
                                             {{false, empty()}, []}
                                     end;
                                (Expr) ->
                                     Matcher = CB(Expr, Options, CB),
                                     fun([], Params) ->
                                             {Matcher([], Params), []};
                                        ([Head|Tail], Params) ->
                                             {Matcher(Head, Params), Tail};
                                        (_, _Params) ->
                                             {{false, empty()}, []}
                                     end
                             end, Conditions),
    fun(Items, Params) ->
            {Statuses, _Tail} =
                lists:foldl(fun(Matcher, {Acc, ItemList})->
                                    {S, R} = Matcher(ItemList, Params),
                                    {[S | Acc], R}
                            end, {[], Items}, ItemMatchers),
            Res = {FinalStatus, _AccCaptures} = 
                lists:foldl(fun({S, Captures}, {Stat, Acc}) ->
                                    {S and Stat, melt_captures(Acc, Captures)}
                            end, {true, empty()}, Statuses),
            case FinalStatus of
                true ->
                    Res;
                _ ->
                    {false, empty()}
            end
    end;

%% ----- Iterable

generate_matcher({iterable, any}, _Options, _CB) ->
    %% jsx represents both list and object as erlang lists.
    %% Therefore, checking if an item is an erlang list is enough to say 
    %% that it is an iterable.
    %% 
    fun({struct, _What}, _Params) ->
            {true, empty()};
       (What, _Params) when is_list(What) ->
            {true, empty()};
       (_, _Params) ->
            {false, empty()}
    end;

generate_matcher({iterable, Conditions, Flags}, Options, CB) ->
    Matchers = lists:map(fun(C) ->
                                 CB(C, Options, CB)
                         end, Conditions),
    DoMatch =
        fun(Items, Params) ->
                R = [continue_until_value_match(Items, Matcher, Params, Flags) || Matcher <- Matchers],
                {AccCaptures, AccFailedCount} = 
                    lists:foldl(fun({{true, Captures}, _}, {CapAcc, FailedAcc}) ->
                                        {melt_captures(CapAcc, Captures), FailedAcc};
                                   (_, {CapAcc, FailedAcc}) ->
                                        {CapAcc, FailedAcc + 1}
                                end, {empty(), 0}, R),
                case AccFailedCount of
                    0 ->
                        {true, AccCaptures};
                    _ ->
                        {false, empty()}
                end
           end,
    fun({struct, Items}, Params) when is_list(Items) ->
            DoMatch(Items, Params);
       (Items, Params) when is_list(Items) ->
            DoMatch(Items, Params);
       (_, _Params) ->
            {false, empty()}
    end;

%% ----- Descedant

generate_matcher({descendant, Conditions, Flags}, Options, CB) ->
    Matchers = lists:map(fun(C) ->
                                 CB(C, Options, CB)
                         end, Conditions),
    DoMatch =
        fun(Items, Params) ->
                R = [deep_continue_until_value_match(Items, Matcher, Params, Flags) || Matcher <- Matchers],
                {AccCaptures, AccFailedCount} = 
                    lists:foldl(fun({{true, Captures}, _}, {CapAcc, FailedAcc}) ->
                                        {melt_captures(CapAcc, Captures), FailedAcc};
                                   (_, {CapAcc, FailedAcc}) ->
                                        {CapAcc, FailedAcc + 1}
                                end, {empty(), 0}, R),
                case AccFailedCount of
                    0 ->
                        {true, AccCaptures};
                    _ ->
                        {false, empty()}
                end
        end,
    fun({struct, Items}, Params) when is_list(Items) ->
            DoMatch(Items, Params);
       (Items, Params) when is_list(Items) ->
            DoMatch(Items, Params);
       (_, _Params) ->
            {false, empty()}
    end;

%% ---- Unit

generate_matcher({string, BinString}, _Options, _CB) ->
    fun(What, _Params) ->
            case What of 
                BinString ->
                    {true, empty()};
                _ ->
                    {false, empty()}
            end
    end;
generate_matcher({regex, BinString}, Options, _CB) ->
    %% TODO - move the production of MP into the parser, which will store a evaluation function
    %% instead of String. Compile options should be passed to the parser, and also the runtime options.
    %% 
    {ok, MP} = re:compile(BinString, Options),
    fun(What, _Params) when is_binary(What) ->
            try re:run(What, MP) of 
                {match, _} ->
                    {true, empty()};
                _ ->
                    {false, empty()}
            catch 
                _:_ ->
                    {false, empty()}
            end;
       (_, _Params) ->
            {false, empty()}
    end;
generate_matcher({number, Number}, Options, _CB) ->
    case proplists:get_value(number_strict_match, Options) of 
        true ->
            fun(What, _Params) when is_number(What) ->
                    case What of 
                        Number ->
                            {true, empty()};
                        _ ->
                            {false, empty()}
                    end;
               (_, _Params) ->
                    {false, empty()}
            end;
        _ ->
            fun(What, _Params) when is_number(What) ->
                    case What == Number of 
                        true ->
                            {true, empty()};
                        _ ->
                            {false, empty()}
                    end;
               (_, _Params) ->
                    {false, empty()}
            end
    end;
generate_matcher(any, _Options, _CB) ->
    fun(_, _Params) ->
            {true, empty()}
    end;
generate_matcher(What, _Options, _CB) when What == true;
                                           What == false;
                                           What == null ->
    fun(Item, _Params) ->
            case Item of
                What ->
                    {true, empty()};
                _ ->
                    {false, empty()}
            end
    end;
generate_matcher(eol, _Options, _CB) ->
    fun([], _Params) ->
            {true, empty()};
       (_, _Params) ->
            {false, empty()}
    end.

%% -----

check_span_match(What, [], _Params, Acc) ->
    Captures =
        lists:foldl(fun (Cap, CapAcc) ->
                        melt_captures(CapAcc, Cap)
                    end,
                    empty(), Acc),
    {{true, Captures}, What};
check_span_match(What, [Matcher|Tail], Params, Acc) ->
    case What of
        [] ->
            Stat = Matcher(What, Params),
            case Stat of
                {false, _} ->
                    {Stat, What};
                {true, C} ->
                    Captures =
                        lists:foldl(fun (Cap, CapAcc) ->
                                        melt_captures(CapAcc, Cap)
                                    end,
                                    C, Acc),
                    {{true, Captures}, Tail}
            end;
        [E|Rest] ->
            Stat = Matcher(E, Params),
            case Stat of
                {false, _} ->
                    {Stat, Rest};
                {true, Cap} ->
                    check_span_match(Rest, Tail, Params, [Cap | Acc])
            end
    end.

continue_until_span_match([], Matchers, Params) ->
    check_span_match([], Matchers, Params, []);
continue_until_span_match(What = [_ | Tail], Matchers, Params) ->
    case check_span_match(What, Matchers, Params, []) of
        R = {{true, _}, _} ->
            R;
        _ ->
            continue_until_span_match(Tail, Matchers, Params)
    end.

continue_until_match([], Matcher, Params) ->
    {Matcher([], Params), []};
continue_until_match([Item | Tail], Matcher, Params) ->
    case Matcher(Item, Params) of 
        R = {true, _} ->
            {R, Tail};
        _ ->
            continue_until_match(Tail, Matcher, Params)
    end.

continue_until_value_match([], _Matcher, _Params, _Flags) ->
    {{false, empty()}, []};
continue_until_value_match(Iterable, Matcher, Params, true) ->
    {continue_until_end_(Iterable, Matcher, Params), empty()};
continue_until_value_match([Item = {struct, _Val} | Tail], Matcher, Params, Flags) ->
    case Matcher(Item, Params) of 
        R = {true, _} ->
            {R, Tail};
        _ ->
            continue_until_value_match(Tail, Matcher, Params, Flags)
    end;
continue_until_value_match([{_Key, Val} | Tail], Matcher, Params, Flags) ->
    case Matcher(Val, Params) of 
        R = {true, _} ->
            {R, Tail};
        _ ->
            continue_until_value_match(Tail, Matcher, Params, Flags)
    end;
continue_until_value_match([Item | Tail], Matcher, Params, Flags) ->
    case Matcher(Item, Params) of 
        R = {true, _} ->
            {R, Tail};
        _ ->
            continue_until_value_match(Tail, Matcher, Params, Flags)
    end.

continue_until_end_(Iterable, Matcher, Params) ->
    continue_until_end_(Iterable, Matcher, Params, {false, empty()}).

continue_until_end_([], _Matcher, _Params, Acc) ->
    Acc;
continue_until_end_([Item = {struct, _Val} | Tail], Matcher, Params, {AccStatus, AccCaptures}) ->
    {LocalStatus, LocalCaptures} = Matcher(Item, Params),
    continue_until_end_(Tail, Matcher, Params, {LocalStatus or AccStatus, melt_captures(AccCaptures, LocalCaptures)});
continue_until_end_([{_Key, Val} | Tail], Matcher, Params, {AccStatus, AccCaptures}) ->
    {LocalStatus, LocalCaptures} = Matcher(Val, Params),
    continue_until_end_(Tail, Matcher, Params, {LocalStatus or AccStatus, melt_captures(AccCaptures, LocalCaptures)});
continue_until_end_([Item | Tail], Matcher, Params, {AccStatus, AccCaptures}) ->
    {LocalStatus, LocalCaptures} = Matcher(Item, Params),
    continue_until_end_(Tail, Matcher, Params, {LocalStatus or AccStatus, melt_captures(AccCaptures, LocalCaptures)}).

deep_continue_until_value_match([], _Matcher, _Params, _Flags) ->
    {{false, empty()}, []};
deep_continue_until_value_match(Iterable, Matcher, Params, true) ->
    {deep_continue_until_end_(Iterable, Matcher, Params), empty()};
deep_continue_until_value_match([Item = {struct, Pairs} | Tail], Matcher, Params, Flags) ->
    case Matcher(Item, Params) of 
        R = {true, _} ->
            {R, Tail};
        _ ->
            case deep_continue_until_value_match(Pairs, Matcher, Params, Flags) of 
                {R2 = {true, _}, _} ->
                    {R2, Tail};
                _ ->
                    deep_continue_until_value_match(Tail, Matcher, Params, Flags)
            end
    end;
deep_continue_until_value_match([{_Key, Val} | Tail], Matcher, Params, Flags) ->
    case Matcher(Val, Params) of 
        R = {true, _} ->
            {R, Tail};
        _ ->
            case Val of
                {struct, Pairs} ->
                    case deep_continue_until_value_match(Pairs, Matcher, Params, Flags) of 
                        {R2 = {true, _}, _} ->
                            {R2, Tail};
                        _ ->
                            deep_continue_until_value_match(Tail, Matcher, Params, Flags)
                    end;
                [_|_] ->
                    case deep_continue_until_value_match(Val, Matcher, Params, Flags) of 
                        {R2 = {true, _}, _R} ->
                            {R2, Tail};
                        _ ->
                            deep_continue_until_value_match(Tail, Matcher, Params, Flags)
                    end;
                _ ->
                    deep_continue_until_value_match(Tail, Matcher, Params, Flags)
            end
    end;
deep_continue_until_value_match([Item | Tail], Matcher, Params, Flags) ->
    case Matcher(Item, Params) of 
        R = {true, _} ->
            {R, Tail};
        _ ->
            case Item of
                {struct, Pairs} ->
                    case deep_continue_until_value_match(Pairs, Matcher, Params, Flags) of 
                        {R2 = {true, _}, _} ->
                            {R2, Tail};
                        _ ->
                            deep_continue_until_value_match(Tail, Matcher, Params, Flags)
                    end;
                [_|_] ->
                    case deep_continue_until_value_match(Item, Matcher, Params, Flags) of 
                        {R2 = {true, _}, _} ->
                            {R2, Tail};
                        _ ->
                            deep_continue_until_value_match(Tail, Matcher, Params, Flags)
                    end;
                _ ->
                    deep_continue_until_value_match(Tail, Matcher, Params, Flags)
            end
    end.

deep_continue_until_end_(Iterable, Matcher, Params) ->
    deep_continue_until_end_(Iterable, Matcher, Params, {false, empty()}).

deep_continue_until_end_([], _Matcher, _Params, Acc) ->
    Acc;
deep_continue_until_end_([Item = {struct, Props} | Tail], Matcher, Params, {AccStatus, AccCaptures}) ->
    {LocalStatus, LocalCaptures} = Matcher(Item, Params),
    LocalAcc = {LocalStatus or AccStatus, melt_captures(AccCaptures, LocalCaptures)},
    R = deep_continue_until_end_(Props, Matcher, Params, LocalAcc),
    deep_continue_until_end_(Tail, Matcher, Params, R);
deep_continue_until_end_([{_Key, Val} | Tail], Matcher, Params, {AccStatus, AccCaptures}) ->
    {LocalStatus, LocalCaptures} = Matcher(Val, Params),
    LocalAcc = {LocalStatus or AccStatus, melt_captures(AccCaptures, LocalCaptures)},
    case Val of
        {struct, Props} ->
            R = deep_continue_until_end_(Props, Matcher, Params, LocalAcc),
            deep_continue_until_end_(Tail, Matcher, Params, R);
        [_|_] ->
            R = deep_continue_until_end_(Val, Matcher, Params, LocalAcc),
            deep_continue_until_end_(Tail, Matcher, Params, R);
        _ ->
            deep_continue_until_end_(Tail, Matcher, Params, LocalAcc)
    end;
deep_continue_until_end_([Item | Tail], Matcher, Params, {AccStatus, AccCaptures}) ->
    {LocalStatus, LocalCaptures} = Matcher(Item, Params),
    LocalAcc = {LocalStatus or AccStatus, melt_captures(AccCaptures, LocalCaptures)},
    case Item of
        {struct, Props} ->
            R = deep_continue_until_end_(Props, Matcher, Params, LocalAcc),
            deep_continue_until_end_(Tail, Matcher, Params, R);
        [_|_] ->
            R = deep_continue_until_end_(Item, Matcher, Params, LocalAcc),
            deep_continue_until_end_(Tail, Matcher, Params, R);
        _ ->
            deep_continue_until_end_(Tail, Matcher, Params, LocalAcc)
    end.

empty() ->
    {struct, []}.

melt_captures({struct, P1}, {struct, P2}) ->
    {struct, ejpet_helpers:melt(P1, P2)}.

add_captures({struct, Pairs}, Name, Values) ->
    {struct, ejpet_helpers:melt([{Name, Values}], Pairs)}.

