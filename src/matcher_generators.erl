-module(matcher_generators).

-export([generate_matcher/1]).


generate_matcher({object, Conditions}) ->
    PairMatchers = lists:map(fun generate_pair_matcher/1, Conditions),
    fun(Items) when is_list(Items) ->
            R = [matcher_tools:continue_until_match(Items, PairMatcher) || PairMatcher <- PairMatchers],
            NonSatisfied = lists:dropwhile(fun({true, _}) ->
                                                   true;
                                              (_) ->
                                                   false
                                           end, R),
            NonSatisfied == [];
       (_) ->
            false
    end;
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
    end.

generate_pair_matcher({any, ValMatcherDesc}) ->
    ValMatcher = generate_matcher(ValMatcherDesc),
    fun({_Key, Val}) ->
            ValMatcher(Val)
    end;
generate_pair_matcher({KeyMatcherDesc, any}) ->
    KeyMatcher = generate_matcher(KeyMatcherDesc),
    fun({Key, _Val}) ->
            KeyMatcher(Key)
    end;
generate_pair_matcher({KeyMatcherDesc, ValMatcherDesc}) ->
    KeyMatcher = generate_matcher(KeyMatcherDesc),
    ValMatcher = generate_matcher(ValMatcherDesc),
    fun({Key, Val}) ->
            KeyMatcher(Key) and ValMatcher(Val)
    end.
