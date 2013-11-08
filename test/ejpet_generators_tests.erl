-module(ejpet_generators_tests).
-author('nicolas.michel.lava@gmail.com').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(BACKENDS, [jsx, jiffy]).


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

object_test_() ->
    Tests = [
             {"{}",
              [{<<"{}">>, true},
               {<<"{\"foo\": []}">>, true},
               {<<"{\"foo\": [], \"bar\": {}}">>, true},
               {<<"[]">>, false},
               {<<"[{}]">>, false}]},
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

list_test_() ->
    Tests = [
             {"[]",
              [{<<"[]">>, true},
               {<<"[true]">>, false}
              ]},
             {"[*]",
              [{<<"[]">>, true},
               {<<"[true]">>, true},
               {<<"[true, false, {\"toto\":42}]">>, true}
              ]},
             {"[true]",
              [{<<"[true]">>, true},
               {<<"[false]">>, false},
               {<<"[]">>, false}]},
             {"[true, false]",
              [{<<"[true, false]">>, true},
               {<<"[false]">>, false},
               {<<"[true]">>, false},
               {<<"[false, true]">>, false},
               {<<"[42, true, false]">>, false},
               {<<"[true, false, 42]">>, false},
               {<<"[true, true, false]">>, false}]},
             {"[true, *, false]",
              [{<<"[true, false]">>, true},
               {<<"[false, true]">>, false},
               {<<"[true, 42, false]">>, true},
               {<<"[42, true, false]">>, false},
               {<<"[true, 42, \"foo\", false]">>, true},
               {<<"[true, 42, false, \"foo\"]">>, false}]},
             {"[*, [1, *, [*, 42, *]], *]",
              [{<<"[[1, [42]]]">>, true},
               {<<"[[1, []]]">>, false},
               {<<"[[42, [42]]]">>, false},
               {<<"[[1, [42], [\"foo\"]]]">>, false}]},
             {"[*, {_: [*, 42]}]",
              [{<<"[{\"foo\": [42]}]">>, true},
               {<<"[42, {\"bar\": 42, \"foo\": [42]}]">>, true},
               {<<"[42, {\"foo\": [\"neh\", 42]}]">>, true},
               {<<"[42, {\"foo\": [42, \"neh\"]}]">>, false},
               {<<"[{\"foo\": [42]}, \"neh\"]">>, false}
              ]}
            ],
    generate_test_list(Tests).

iterable_test_() ->
    Tests = [
             {"<>",
              [{<<"[]">>, true},
               {<<"{}">>, true},
               {<<"{\"foo\":42}">>, true},
               {<<"{\"bar\": {}, \"foo\": 42}">>, true},
               {<<"[42]">>, true},
               {<<"[\"foo\", 42]">>, true},
               {<<"[{\"foo\":42}]">>, true}
              ]},
             {"<42>",
              [
               {<<"{\"foo\":42}">>, true},
               {<<"{\"bar\": {}, \"foo\": 42}">>, true},
               {<<"[42]">>, true},
               {<<"[\"foo\", 42, 13]">>, true},
               {<<"[{\"foo\":42}]">>, false},
               {<<"[{\"foo\":42}, 42]">>, true},
               {<<"[41]">>, false}
              ]},
             {"<42, {_:[*, 42, *]}>",
              [
               {<<"[1, \"foo\", {\"bar\": [42]}, 42]">>, true},
               {<<"{\"foo\": 42, \"neh\": {\"bar\": [42]}}">>, true}
              ]},
             {"*/42",
              [
               {<<"{\"foo\":42}">>, true},
               {<<"{\"bar\": {}, \"foo\": 42}">>, true},
               {<<"[42]">>, true},
               {<<"[\"foo\", 42, 13]">>, true},
               {<<"[{\"foo\":42}]">>, false},
               {<<"[{\"foo\":42}, 42]">>, true},
               {<<"[41]">>, false}
              ]}
            ],
    generate_test_list(Tests).

descendant_test_() ->
    Tests = [
             {"**/42",
              [
               {<<"[]">>, false},
               {<<"{}">>, false},
               {<<"42">>, false},
               {<<"[42]">>, true},
               {<<"{\"foo\":42}">>, true},
               {<<"{\"bar\": {}, \"foo\": 42}">>, true},
               {<<"[\"foo\", 42]">>, true},
               {<<"[[{\"bar\": [{\"foo\": [\"bar\", 42, 13]}]}]]">>, true}
              ]},
             {"**/[*, 42]",
              [
               {<<"[]">>, false},
               {<<"{}">>, false},
               {<<"[42]">>, false},
               {<<"[\"foo\", [42]]">>, true},
               {<<"{\"foo\" : [42]}">>, true},
               {<<"[[{\"bar\": [{\"foo\": [\"this one matches\", 42]}]}], \"next does not match\", 42]">>, true}
              ]}
            ],
    generate_test_list(Tests).

complex_test_() ->
    Tests = [
             {"{_:[*]}",
              [{<<"{\"foo\": [42]}">>, true},
               {<<"{\"foo\": []}">>, true},
               {<<"{\"bar\": [\"neh\", 42, {}]}">>, true},
               {<<"{\"bar\": 42, \"foo\": {}}">>, false}
              ]}
            ],
    generate_test_list(Tests).

generate_test_list(TestDescs) ->
    [generate_test_list(TestDescs, Backend) || Backend <- ?BACKENDS].

generate_test_list(TestDescs, jsx) ->
    lists:reverse(
      lists:foldl(fun({Pattern, T}, Acc) ->
                          {_, AST} = ejpet_parser:parse(ejpet_scanner:tokenize(Pattern)),
                          F = ejpet_generators_jsx:generate_matcher(AST),
                          lists:foldl(fun ({Node, Status}, Acc) ->
                                              TestName = Pattern ++ " | " ++ binary_to_list(Node) ++ " | " ++ atom_to_list(Status),
                                              [{TestName, ?_test(?assert(F(jsx:decode(Node)) == Status))} | Acc]
                                      end, Acc, T)
                  end, [], TestDescs));
generate_test_list(TestDescs, jiffy) ->
    lists:reverse(
      lists:foldl(fun({Pattern, T}, Acc) ->
                          {_, AST} = ejpet_parser:parse(ejpet_scanner:tokenize(Pattern)),
                          F = ejpet_jiffy_generators:generate_matcher(AST),
                          lists:foldl(fun ({Node, Status}, Acc) ->
                                              TestName = Pattern ++ " | " ++ binary_to_list(Node) ++ " | " ++ atom_to_list(Status),
                                              [{TestName, ?_test(?assert(F(jiffy:decode(Node)) == Status))} | Acc]
                                      end, Acc, T)
                  end, [], TestDescs)).

-endif.

