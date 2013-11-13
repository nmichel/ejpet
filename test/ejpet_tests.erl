-module(ejpet_tests).
-author('nicolas.michel.lava@gmail.com').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

match_test_() ->
    Tests = [{"**/42", "42", false},
             {"**/42", "[42]", true}
            ],
    [{Pattern ++ " | " ++ Test ++ " | " ++ atom_to_list(Expected),
      ?_test(?assert(ejpet:match(Pattern, list_to_binary(Test)) =:= Expected))} || {Pattern, Test, Expected} <- Tests].

-endif.
