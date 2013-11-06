-module(matcher_generators_tests).
-author('nicolas.michel.lava@gmail.com').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
    Tests = [
             {"true",
              [{<<"true">>, true},
               {<<"false">>, false},
               {<<"42">>, false},
               {<<"\"foo\"">>, false},
               {<<"{\"foo\": []}">>, false}
              ]}
            ],
    generate_test_list(Tests).

match_test_() ->
    Tests = [
             {"{\"foo\": 42}",
              [{<<"{\"foo\": 42}">>, true},
               {<<"{\"foo\": 41}">>, false},
               {<<"{\"foo\": []}">>, false},
               {<<"{\"foo\": \"42\"}">>, false},
               {<<"{\"neh\": [], \"foo\": 42, \"bar\": {\"neh\": false}}">>, true},
               {<<"{\"neh\": [], \"foo\": 40, \"bar\": {\"neh\": 42}}">>, false}
              ]},
             {"{_:{_:42}, _:true}",
              [{<<"{\"foo\": true, \"bar\": {\"neh\": 42}}">>, true}
              ]}
            ],
    generate_test_list(Tests).

generate_test_list(TestDescs) ->
    lists:foldl(fun({Matcher, T}, Acc) ->
                        {_, AST} = json_parser:parse(json_scanner:tokenize(Matcher)),
                        F = matcher_generators:generate_matcher(AST),
                        lists:foldl(fun ({Node, Status}, Acc) ->
                                            TestName = Matcher ++ " | " ++ binary_to_list(Node),
                                            [{TestName, ?_test(?assert(F(jsx:decode(Node)) == Status))} | Acc]
                                    end, Acc, T)
                end, [], TestDescs).

-endif.

