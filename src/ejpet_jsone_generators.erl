-module(ejpet_jsone_generators).
-author('nicolas.michel.lava@gmail.com').

-export([generate_matcher/3]).


%% ---- Capture 

generate_matcher({capture, Pattern, Mode}, Options, CB) -> %% Mode is Name
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
            case ejpet_helpers:get_value(Name, Params) of
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
            case ejpet_helpers:get_value(Name, Params) of
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
            case ejpet_helpers:get_value(Name, Params) of
                undefined ->
                    {false, empty()};
                Number ->
                    case ejpet_helpers:get_value(number_strict_match, Options) of
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
            case ejpet_helpers:get_value(Name, Params) of
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
    fun(#{}, _Params) ->
            {true, empty()};
       ([{}], _Params) ->
            {true, empty()};
       ([{_, _} | _], _Params) ->
            {true, empty()};
       (_, _Params) ->
            {false, empty()}
    end;
generate_matcher({object, Conditions}, Options, CB) ->
    PairMatchers = lists:map(fun(C) ->
                                     CB(C, Options, CB)
                             end, Conditions),
    DoMatch =
        fun(Items = [{_,_} | _], Params) ->
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
        end,
    fun(M = #{}, Params) ->
            DoMatch(maps:to_list(M), Params);
       (Items, Params) when is_list(Items) ->
            DoMatch(Items, Params);
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
    KeyMatcher = CB(KeyMatcherDesc, Options, CB),
    fun({Key, _Val}, Params) ->
            KeyMatcher(Key, Params);
       (_, _Params) ->
            {false, empty()}
    end;
generate_matcher({pair, KeyMatcherDesc, ValMatcherDesc}, Options, CB) ->
    KeyMatcher = CB(KeyMatcherDesc, Options, CB),
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
    fun([], _Params) ->
            {true, empty()};
       ([{}], _Params) -> % jsx special form for empty object
            {false, empty()};
       ([{_, _} |_], _Params) -> % jsx form for non empty object
            {false, empty()};
       ([_|_], _Params) ->
            {true, empty()};
       (_, _Params) ->
            {false, empty()}
    end;
    
generate_matcher({list, Conditions}, Options, CB) ->
    ItemMatchers = lists:map(fun(Expr) ->
                                 CB(Expr, Options, CB)
                             end, Conditions),
    fun([{}], _Params) -> % jsx special form for empty object
            {false, empty()};
       ([{_, _} | _], _Params) -> %% jsx special form for non empty object
            {false, empty()};
       ([], _Params) -> %% cannot match anything in an empty list
            {false, empty()};
       (Items, Params) when is_list(Items) ->
            {Statuses, _Tail} =
                lists:foldl(fun(Matcher, {Acc, ItemList})->
                                    {S, R} = Matcher(ItemList, Params),
                                    {[S | Acc], R}
                            end, {[], Items}, ItemMatchers),
            Res = {FinalStatus, _AccCaptures} = 
                lists:foldl(fun({S, Captures}, {Stat, Acc}) ->
                                    {S and Stat, melt_captures(Acc, Captures)}
                            end, {true, empty()}, lists:reverse(Statuses)),
            case FinalStatus of
                true ->
                    Res;
                false ->
                    {false, empty()}
            end;
       (_, _Params) ->
            {false, empty()}
    end;

generate_matcher({span, Exprs}, Options, CB) ->
    generate_matcher({span, Exprs, false}, Options, CB);
generate_matcher({span, Exprs, eol}, Options, CB) ->
    generate_matcher({span, Exprs, true}, Options, CB);
generate_matcher({span, Exprs, Strict}, Options, CB) ->
    Matchers = lists:foldr(fun(Expr, Acc) ->
                               [CB(Expr, Options, CB) | Acc]
                           end, [], Exprs),
    fun(Span, Params) when is_list(Span) ->
        check_span_match(Span, Matchers, Params, [], Strict)
    end;

generate_matcher({find, Expr}, Options, CB) ->
    SpanMatcher = CB(Expr, Options, CB),
    fun(Span, Params) ->
        continue_until_span_match(Span, SpanMatcher, Params)
    end;

%% ----- Iterable

generate_matcher({iterable, any}, _Options, _CB) ->
    %% jsone represents both list and object as erlang lists.
    %% Therefore, checking if an item is an erlang list is enough to say 
    %% that it is an iterable.
    %% 
    fun(What, _Params) when is_list(What) ->
            {true, empty()};
       (#{}, _Params) ->
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
    fun(M = #{}, Params) ->
            DoMatch(maps:to_list(M), Params);
       (Items, Params) when is_list(Items) ->
            DoMatch(Items, Params);
       (_, _Params) ->
            {false, empty()}
    end;

%% ----- Descendant

generate_matcher({descendant, Conditions, Flags}, Options, CB) ->
    Matchers = lists:map(fun(C) ->
                                 CB(C, Options, CB)
                         end, Conditions),
    DoMatch =
        fun(Items, Params) when is_list(Items) ->
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
                end;
           (_, _Params) ->
                {false, empty()}
        end,
    fun(M = #{}, Params) ->
            DoMatch(maps:to_list(M), Params);
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
    %% instead of BinString. Compile options should be passed to the parser, and also the runtime options.
    %% 
    %% TODO : re:compile crashes when passed unknown options
    %% ReOptions = filter_re_options(Options)
    %% {ok, mp} = re:compile(BinString, ReOptions),
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
    case ejpet_helpers:get_value(number_strict_match, Options) of
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
            nil when What == null ->
                {true, empty()};
            What ->
                {true, empty()};
            _ ->
                {false, empty()}
        end
    end.

%% -----

check_span_match([], [_|_], _Params, _Acc, _Strict) ->
   {{false, empty()}, []};
check_span_match([_|_], [], _Params, _Acc, true) ->
   {{false, empty()}, []};
check_span_match(What, [], _Params, Acc, _Strict) ->
    Captures =
        lists:foldl(fun (Cap, CapAcc) ->
                        melt_captures(CapAcc, Cap)
                    end,
                    empty(), lists:reverse(Acc)),
    {{true, Captures}, What};
check_span_match([E|Rest], [Matcher|Tail], Params, Acc, Strict) ->
    Stat = Matcher(E, Params),
    case Stat of
        {false, _} ->
            {Stat, Rest};
        {true, Cap} ->
            check_span_match(Rest, Tail, Params, [Cap | Acc], Strict)
    end.

continue_until_span_match([], _SpanMatcher, _Params) ->
    {{false, empty()}, []};
continue_until_span_match(What = [_ | Tail], SpanMatcher, Params) ->
    case SpanMatcher(What, Params) of
        R = {{true, _}, _} ->
            R;
        _ ->
            continue_until_span_match(Tail, SpanMatcher, Params)
    end.

continue_until_match([], _Matcher, _Params) ->
    {{false, empty()}, []};
continue_until_match([Item | Tail], Matcher, Params) ->
    case Matcher(Item, Params) of 
        R = {true, _} ->
            {R, Tail};
        _ ->
            continue_until_match(Tail, Matcher, Params)
    end.

continue_until_value_match([{}], _Matcher, _Params, _Flags) ->
    {{false, empty()}, []};
continue_until_value_match([], _Matcher, _Params, _Flags) ->
    {{false, empty()}, []};
continue_until_value_match(Iterable, Matcher, Params, true) ->
    {continue_until_end_(Iterable, Matcher, Params), []};
continue_until_value_match([{_Key, Val} | Tail], Matcher, Params, false) ->
    case Matcher(Val, Params) of 
        R = {true, _} ->
            {R, Tail};
        _ ->
            continue_until_value_match(Tail, Matcher, Params, false)
    end;
continue_until_value_match([Item | Tail], Matcher, Params, false) ->
    case Matcher(Item, Params) of 
        R = {true, _} ->
            {R, Tail};
        _ ->
            continue_until_value_match(Tail, Matcher, Params, false)
    end.

continue_until_end_(Iterable, Matcher, Params) ->
    continue_until_end_(Iterable, Matcher, Params, {false, empty()}).

continue_until_end_([{}], _Matcher, _Params, Acc) ->
    Acc;
continue_until_end_([], _Matcher, _Params, Acc) ->
    Acc;
continue_until_end_([{_Key, Val} | Tail], Matcher, Params, {AccStatus, AccCaptures}) ->
    {LocalStatus, LocalCaptures} = Matcher(Val, Params),
    continue_until_end_(Tail, Matcher, Params, {LocalStatus or AccStatus, melt_captures(AccCaptures, LocalCaptures)});
continue_until_end_([Item | Tail], Matcher, Params, {AccStatus, AccCaptures}) ->
    {LocalStatus, LocalCaptures} = Matcher(Item, Params),
    continue_until_end_(Tail, Matcher, Params, {LocalStatus or AccStatus, melt_captures(AccCaptures, LocalCaptures)}).

deep_continue_until_value_match(M = #{}, _Matcher, _Params, _Flags) when map_size(M) == 0 ->
    {{false, empty()}, []};
deep_continue_until_value_match([{}], _Matcher, _Params, _Flags) ->
    {{false, empty()}, []};
deep_continue_until_value_match([], _Matcher, _Params, _Flags) ->
    {{false, empty()}, []};
deep_continue_until_value_match(M = #{}, Matcher, Params, Flags) ->
    deep_continue_until_value_match(maps:to_list(M), Matcher, Params, Flags);
deep_continue_until_value_match(Iterable, Matcher, Params, true) ->
    {deep_continue_until_end_(Iterable, Matcher, Params), empty()};
deep_continue_until_value_match([{_Key, Val} | Tail], Matcher, Params, Flags) ->
    case Matcher(Val, Params) of 
        R = {true, _} ->
            {R, Tail};
        _ ->
            case Val of
                #{} when map_size(Val) > 0 ->
                    case deep_continue_until_value_match(Val, Matcher, Params, Flags) of 
                        {R2 = {true, _}, _} ->
                            {R2, Tail};
                        _ ->
                            deep_continue_until_value_match(Tail, Matcher, Params, Flags)
                    end;
                [_|_] ->
                    case deep_continue_until_value_match(Val, Matcher, Params, Flags) of 
                        {R2 = {true, _}, _} ->
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
                #{} when map_size(Item) > 0 ->
                    case deep_continue_until_value_match(Item, Matcher, Params, Flags) of 
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

deep_continue_until_end_(M = #{}, _Matcher, _Params, Acc) when map_size(M) == 0 ->
    Acc;
deep_continue_until_end_([{}], _Matcher, _Params, Acc) ->
    Acc;
deep_continue_until_end_([], _Matcher, _Params, Acc) ->
    Acc;
deep_continue_until_end_(M = #{}, Matcher, Params, Acc) ->
    deep_continue_until_end_(maps:to_list(M), Matcher, Params, Acc);
deep_continue_until_end_([{_Key, Val} | Tail], Matcher, Params, {AccStatus, AccCaptures}) ->
    {LocalStatus, LocalCaptures} = Matcher(Val, Params),
    LocalAcc = {LocalStatus or AccStatus, melt_captures(AccCaptures, LocalCaptures)},
    case Val of
        M = #{} when map_size(M) >= 0 ->
            R = deep_continue_until_end_(Val, Matcher, Params, LocalAcc),
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
        #{} ->
            R = deep_continue_until_end_(Item, Matcher, Params, LocalAcc),
            deep_continue_until_end_(Tail, Matcher, Params, R);
        [_|_] ->
            R = deep_continue_until_end_(Item, Matcher, Params, LocalAcc),
            deep_continue_until_end_(Tail, Matcher, Params, R);
        _ ->
            deep_continue_until_end_(Tail, Matcher, Params, LocalAcc)
    end.

empty() ->
    #{}.

melt_captures(Empty, C) when map_size(Empty) == 0 ->
    C;
melt_captures(C, Empty) when map_size(Empty) == 0 ->
    C;
melt_captures(P1, P2) ->
    ejpet_helpers:melt(P1, P2).

add_captures(Empty, Name, Values) when map_size(Empty) == 0 ->
    maps:put(Name, Values, #{});
add_captures(Pairs, Name, Values) ->
    New = maps:put(Name, Values, #{}),
    ejpet_helpers:melt(New, Pairs).
