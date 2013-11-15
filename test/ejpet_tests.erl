-module(ejpet_tests).
-author('nicolas.michel.lava@gmail.com').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(BACKEND, jsx).
-define(REF_BACKEND, jsx).

run_jsx_test_() ->
    Tests = [{"**/42",
              "42",
              {false, []}},
             {"(?<full>**/42)",
              "[42]",
              {true, [{"full", <<"[42]">>}]}},
             {"**/(?<toto>42)",
              "[[[[[[[[[[[[[[[[[[[[[[[[{\"foo\": 42}]]]]]]]]]]]]]]]]]]]]]]]]",
              {true, [{"toto", <<"42">>}]}},
              {"**/(?<toto><42>)",
               "[[[[[[[[[[[[[[[[[[[[[[[[{\"foo\": 42}]]]]]]]]]]]]]]]]]]]]]]]]",
               {true,[{"toto", <<"{\"foo\":42}">>}]}},
              {"**/(?<toto>[42])",
               "[[[[[[[[[[[[[[[[[[[[[[[[1, 42]]]]]]]]]]]]]]]]]]]]]]]]",
               {false, []}}
             ],

    lists:foldl(fun({Pattern, JSON, Expected}, Acc) ->
                        %% Execute the test, using the specified backend
                        %% 
                        O = ejpet:compile(Pattern, ?BACKEND),
                        {Status, Captures} = ejpet:run(O, (ejpet:backend(O)):decode(list_to_binary(JSON))),

                        %% Transform captures to text
                        %% 
                        JSONCaptures = [{VarName, (ejpet:backend(O)):encode(Cap)} || {VarName, Cap} <- Captures],

                        %% Parse again and stringify captures using the reference backend
                        %% 
                        RefCaptures = [{VarName, ?REF_BACKEND:encode(?REF_BACKEND:decode(Cap))} || {VarName, Cap} <- JSONCaptures],

                        %% Produce test function
                        %% 
                        [?_test(?assert(Expected == {Status, JSONCaptures})) | Acc]
                end, [], Tests).

-endif.
