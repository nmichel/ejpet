-module(ejpet_mochijson2_generators_tools).

-export([continue_until_match/2,
         continue_until_object_value_match/2,
         continue_until_list_value_match/2,
         deep_continue_until_object_value_match/2,
         deep_continue_until_list_value_match/2]).


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
