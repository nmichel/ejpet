-module(matcher_tools).

-export([continue_until_match/2]).


continue_until_match([], Matcher) ->
    {false, []};
continue_until_match([Item | Tail], Matcher) ->
    case Matcher(Item) of 
        true ->
            {true, Tail};
        _ ->
            continue_until_match(Tail, Matcher)
    end.

