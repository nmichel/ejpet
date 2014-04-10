-module(ejpet_generators_tests).
-author('nicolas.michel.lava@gmail.com').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
    Tests = [
             {"true",
              [{<<"true">>, {true, []}},
               {<<"false">>, {false, []}},
               {<<"42">>, {false, []}},
               {<<"\"foo\"">>, {false, []}},
               {<<"{\"foo\": []}">>, {false, []}}
              ]}
            ],
    ejpet_test_helpers:generate_test_list(Tests).

object_test_() ->
    Tests = [
             {"{}",
              [{<<"{}">>, {true, []}},
               {<<"{\"foo\": []}">>, {true, []}},
               {<<"{\"foo\": [], \"bar\": {}}">>, {true, []}},
               {<<"[]">>, {false, []}},
               {<<"[{}]">>, {false, []}}
              ]},
             {"{\"foo\": 42}",
              [{<<"{\"foo\": 42}">>, {true, []}},
               {<<"{\"foo\": 41}">>, {false, []}},
               {<<"{\"foo\": []}">>, {false, []}},
               {<<"{\"foo\": \"42\"}">>, {false, []}},
               {<<"{\"neh\": [], \"foo\": 42, \"bar\": {\"neh\": false}}">>, {true, []}},
               {<<"{\"neh\": [], \"foo\": 40, \"bar\": {\"neh\": 42}}">>, {false, []}}
              ]},
             {"{_:{_:42}, _:true}",
              [{<<"{\"foo\": true, \"bar\": {\"neh\": 42}}">>, {true, []}}
              ]}
            ],
    ejpet_test_helpers:generate_test_list(Tests).

list_test_() ->
    Tests = [
             {"[]",
              [{<<"[]">>, {true, []}},
               {<<"[true]">>, {false, []}}
              ]},
             {"[*]",
              [{<<"[]">>, {true, []}},
               {<<"[true]">>, {true, []}},
               {<<"[true, false, {\"toto\":42}]">>, {true, []}}
              ]},
             {"[true]",
              [{<<"[true]">>, {true, []}},
               {<<"[false]">>, {false, []}},
               {<<"[]">>, {false, []}}]},
             {"[true, false]",
              [{<<"[true, false]">>, {true, []}},
               {<<"[false]">>, {false, []}},
               {<<"[true]">>, {false, []}},
               {<<"[false, true]">>, {false, []}},
               {<<"[42, true, false]">>, {false, []}},
               {<<"[true, false, 42]">>, {false, []}},
               {<<"[true, true, false]">>, {false, []}}]},
             {"[true, *, false]",
              [{<<"[true, false]">>, {true, []}},
               {<<"[false, true]">>, {false, []}},
               {<<"[true, 42, false]">>, {true, []}},
               {<<"[42, true, false]">>, {false, []}},
               {<<"[true, 42, \"foo\", false]">>, {true, []}},
               {<<"[true, 42, false, \"foo\"]">>, {false, []}}]},
             {"[*, [1, *, [*, 42, *]], *]",
              [{<<"[[1, [42]]]">>, {true, []}},
               {<<"[[1, []]]">>, {false, []}},
               {<<"[[42, [42]]]">>, {false, []}},
               {<<"[[1, [42], [\"foo\"]]]">>, {false, []}}]},
             {"[*, {_: [*, 42]}]",
              [{<<"[{\"foo\": [42]}]">>, {true, []}},
               {<<"[42, {\"bar\": 42, \"foo\": [42]}]">>, {true, []}},
               {<<"[42, {\"foo\": [\"neh\", 42]}]">>, {true, []}},
               {<<"[42, {\"foo\": [42, \"neh\"]}]">>, {false, []}},
               {<<"[{\"foo\": [42]}, \"neh\"]">>, {false, []}}
              ]}
            ],
    ejpet_test_helpers:generate_test_list(Tests).

iterable_test_() ->
    Tests = [
             {"<>",
              [{<<"[]">>, {true, []}},
               {<<"{}">>, {true, []}},
               {<<"{\"foo\":42}">>, {true, []}},
               {<<"{\"bar\": {}, \"foo\": 42}">>, {true, []}},
               {<<"[42]">>, {true, []}},
               {<<"[\"foo\", 42]">>, {true, []}},
               {<<"[{\"foo\":42}]">>, {true, []}}
              ]},
             {"<42>",
              [
               {<<"{\"foo\":42}">>, {true, []}},
               {<<"{\"bar\": {}, \"foo\": 42}">>, {true, []}},
               {<<"[42]">>, {true, []}},
               {<<"[\"foo\", 42, 13]">>, {true, []}},
               {<<"[{\"foo\":42}]">>, {false, []}},
               {<<"[{\"foo\":42}, 42]">>, {true, []}},
               {<<"[41]">>, {false, []}}
              ]},
             {"<42, {_:[*, 42, *]}>",
              [
               {<<"[1, \"foo\", {\"bar\": [42]}, 42]">>, {true, []}},
               {<<"{\"foo\": 42, \"neh\": {\"bar\": [42]}}">>, {true, []}}
              ]},
             {"*/42",
              [
               {<<"{\"foo\":42}">>, {true, []}},
               {<<"{\"bar\": {}, \"foo\": 42}">>, {true, []}},
               {<<"[42]">>, {true, []}},
               {<<"[\"foo\", 42, 13]">>, {true, []}},
               {<<"[{\"foo\":42}]">>, {false, []}},
               {<<"[{\"foo\":42}, 42]">>, {true, []}},
               {<<"[41]">>, {false, []}}
              ]},
             {"<{_:42}>",
              [{<<"[{\"bar\": 42}]">>, {true, []}}
              ]},
             {"<{_:[*, 42, *]}>",
              [
               {<<"[{\"bar\": [42]}]">>, {true, []}}
             ]},
             {"<42>",
              [{<<"[1, \"foo\", {\"bar\": [42]}, 42]">>, {true, []}}
              ]},
             {"<[*, 42, *]>",
              [{<<"[[42]]">>, {true, []}}
              ]},
             {"{_:[*, 42, *]}",
              [{<<"{\"bar\": [42]}">>, {true, []}}
              ]},
             {"<{_:[*, 42, *]}>",
              [{<<"{\"foo\": {\"bar\": [42]}}">>, {true, []}}
              ]},
             {"<{_:[*, 42, *]}>",
              [{<<"[{\"bar\": [42]}]">>, {true, []}}
              ]}
            ],
    ejpet_test_helpers:generate_test_list(Tests).

descendant_test_() ->
    Tests = [
             {"**/42",
              [
               {<<"[]">>, {false, []}},
               {<<"{}">>, {false, []}},
               {<<"42">>, {false, []}},
               {<<"[42]">>, {true, []}},
               {<<"{\"foo\":42}">>, {true, []}},
               {<<"{\"bar\": {}, \"foo\": 42}">>, {true, []}},
               {<<"[\"foo\", 42]">>, {true, []}},
               {<<"[[{\"bar\": [{\"foo\": [\"bar\", 42, 13]}]}]]">>, {true, []}}
              ]},
             {"**/[*, 42]",
              [
               {<<"[]">>, {false, []}},
               {<<"{}">>, {false, []}},
               {<<"[42]">>, {false, []}},
               {<<"[\"foo\", [42]]">>, {true, []}},
               {<<"{\"foo\" : [42]}">>, {true, []}},
               {<<"[[{\"bar\": [{\"foo\": [\"this one matches\", 42]}]}], \"next does not match\", 42]">>, {true, []}}
              ]},
             {"**/{_:42}",
              [{<<"[{\"bar\": 42}]">>, {true, []}}
              ]}
            ],
    ejpet_test_helpers:generate_test_list(Tests).

complex_test_() ->
    Tests = [
             {"{_:[*]}",
              [{<<"{\"foo\": [42]}">>, {true, []}},
               {<<"{\"foo\": []}">>, {true, []}},
               {<<"{\"bar\": [\"neh\", 42, {}]}">>, {true, []}},
               {<<"{\"bar\": 42, \"foo\": {}}">>, {false, []}}
              ]}
            ],
    ejpet_test_helpers:generate_test_list(Tests).

capture_test_() ->
    Tests = [
             {"(?<value>{_:[*]})",
              [{<<"{\"foo\": [42]}">>, {true, [{"value", [<<"{\"foo\":[42]}">>]}]}}
              ]},
             {"(?<full>{_:(?<local>[*])})",
              [{<<"{\"foo\": [42]}">>, {true, [{"full", [<<"{\"foo\":[42]}">>]}, {"local", [<<"[42]">>]}]}}
              ]},
             {"
              <
                  {
                      _:[*, (?<found><42>), *]
                  }
              >
              ",
              [{<<"[1, 2, {\"foo\": 42, \"bar\": [{\"neh\": 42}]}, 41, 42]">>, {true, [{"found", [<<"{\"neh\":42}">>]}]}}
              ]}
            ],
    ejpet_test_helpers:generate_test_list(Tests).

utf8_test_() ->
    Tests = [
             {"{_:[(?<value>_)]}",
              [{<<"{\"foo\": [\"漂亮的綠色汽車\"]}"/utf8>>, {true, [{"value", [<<"\"漂亮的綠色汽車\""/utf8>>]}]}},
               {<<"{\"bar\": [\"éléphant\"]}"/utf8>>, {true, [{"value", [<<"\"éléphant\""/utf8>>]}]}}
              ]},
             {<<"{_:[(?<value>#\"漂.*色\")]}"/utf8>>,
              [{<<"{\"foo\": [\"漂亮的綠色汽車\"]}"/utf8>>, {true, [{"value", [<<"\"漂亮的綠色汽車\""/utf8>>]}]}},
               {<<"{\"foo\": [\"!漂亮的綠色汽車\"]}"/utf8>>, {true, [{"value", [<<"\"!漂亮的綠色汽車\""/utf8>>]}]}},
               {<<"{\"foo\": [\"!漂亮的綠?汽車\"]}"/utf8>>, {false, []}}
              ]},
             {<<"{_:[(?<value>#\"^漂\")]}"/utf8>>,
              [{<<"{\"foo\": [\"漂亮的綠色汽車\"]}"/utf8>>, {true, [{"value", [<<"\"漂亮的綠色汽車\""/utf8>>]}]}},
               {<<"{\"foo\": [\"!漂亮的綠色汽車\"]}"/utf8>>, {false, []}}
              ]}
            ],
    ejpet_test_helpers:generate_test_list(Tests).

injection_test_() ->
    Tests = [
             {"(!<what>number)",
              [{<<"42">>, [{<<"what">>, 42}], {true, []}},
               {<<"42">>, [{<<"what">>, 0}], {false, []}},
               {<<"40">>, [{<<"what">>, 42}], {false, []}},
               {<<"false">>, [{<<"what">>, 42}], {false, []}}
              ]},
             {"(!<what>boolean)",
              [{<<"true">>, [{<<"what">>, true}], {true, []}},
               {<<"true">>, [{<<"what">>, false}], {false, []}},
               {<<"false">>, [{<<"what">>, false}], {true, []}},
               {<<"false">>, [{<<"what">>, true}], {false, []}},
               {<<"42">>, [{<<"what">>, true}], {false, []}},
               {<<"true">>, [{<<"what">>, 27}], {false, []}}
              ]},
             {"(!<what>string)",
              [{<<"\"foo\"">>, [{<<"what">>, <<"foo">>}], {true, []}},
               {<<"\"foo\"">>, [{<<"bar">>, false}], {false, []}},
               {<<"\"漂亮的綠色汽車\"">>, [{<<"what">>, <<"漂亮的綠色汽車">>}], {true, []}},
               {<<"\"漂亮的綠色汽車\"">>, [{<<"what">>, <<"漂亮的綠色">>}], {false, []}},
               {<<"42">>, [{<<"what">>, <<"漂亮的綠色">>}], {false, []}},
               {<<"\"漂亮的綠色汽車\"">>, [{<<"what">>, 42}], {false, []}}
              ]},
             {"{_:[(!<what>number)]}",
              [{<<"{\"foo\": [42]}">>, [{<<"what">>, 42}], {true, []}},
               {<<"{\"foo\": [false]}">>, [{<<"what">>, 42}], {false, []}},
               {<<"{\"foo\": [true, 42]}">>, [{<<"what">>, 42}], {false, []}}
              ]}
            ],
    ejpet_test_helpers:generate_test_list(Tests).

injection_regex_test_() ->
    {ok, MP1} = re:compile("foo"),
    {ok, MP2} = re:compile("^foo.*\\bbar$"),
    Tests = [
             {"(!<what>regex)",
              [{<<"\"foo\"">>, [{<<"what">>, MP1}], {true, []}},
               {<<"\"nehfoobar\"">>, [{<<"what">>, MP1}], {true, []}},
               {<<"\"nehbar\"">>, [{<<"what">>, MP1}], {false, []}},
               {<<"42">>, [{<<"what">>, MP1}], {false, []}},
               {<<"{\"foo\":42}">>, [{<<"what">>, MP1}], {false, []}},

               {<<"\"fooneh\\bbar\"">>, [{<<"what">>, MP2}], {true, []}},
               {<<"\"foo\\bbar\"">>, [{<<"what">>, MP2}], {true, []}},
               {<<"\"nehfoo\\bbar\"">>, [{<<"what">>, MP2}], {false, []}},
               {<<"\"foo\\bbarneh\"">>, [{<<"what">>, MP2}], {false, []}}
              ]}
            ],
    ejpet_test_helpers:generate_test_list(Tests).

injection_and_capture_test_() ->
    {ok, MP1} = re:compile("^id_\\d+$"),
    {ok, MP2} = re:compile("^grp_\\d+$"),
    Tests = [
             {"*/{\"sender\":(!<who>number),\"text\":(?<text>_)}",
              [{<<"[{\"sender\":42,\"text\":\"foo\"}, {\"sender\":24,\"text\":\"bar\"}]">>, [{<<"who">>, 42}], {true, [{"text", [<<"\"foo\"">>]}]}},
               {<<"[{\"sender\":42,\"text\":\"foo\"}, {\"sender\":24,\"text\":\"bar\"}]">>, [{<<"who">>, 24}], {true, [{"text", [<<"\"bar\"">>]}]}}
              ]},
             {"*/{\"sender\":(?<who>{\"name\":(!<from>regex)}),\"text\":(?<text>_)}",
              [{<<"[{\"sender\":{\"name\": \"id_42\"},\"text\":\"foo\"}, {\"sender\":{\"name\": \"grp_42\"},\"text\":\"bar\"}]">>, [{<<"from">>, MP1}], {true, [{"text", [<<"\"foo\"">>]},
                                                                                                                                                                {"who", [<<"{\"name\":\"id_42\"}">>]}]}},
               {<<"[{\"sender\":{\"name\": \"id_42\"},\"text\":\"foo\"}, {\"sender\":{\"name\": \"grp_42\"},\"text\":\"bar\"}]">>, [{<<"from">>, MP2}], {true, [{"text", [<<"\"bar\"">>]},
                                                                                                                                                                {"who", [<<"{\"name\":\"grp_42\"}">>]}]}}
              ]}
            ],
    ejpet_test_helpers:generate_test_list(Tests).

global_iterable_capture_test_() ->
    Tests = [
             {"*/(?<node>{\"codec\":_, \"lang\":(?<lang>_)})/g",
              [{<<"[{\"codec\": \"audio\", \"lang\": \"fr\"}, {\"codec\": \"video\", \"lang\": \"en\"}, {\"codec\": \"foo\", \"lang\": \"it\"}]">>,
                {true, [{"node", [<<"{\"codec\":\"audio\",\"lang\":\"fr\"}">>, <<"{\"codec\":\"video\",\"lang\":\"en\"}">>, <<"{\"codec\":\"foo\",\"lang\":\"it\"}">>]},
                        {"lang", [<<"\"fr\"">>, <<"\"en\"">>, <<"\"it\"">>]}]}}
              ]}
            ],
    ejpet_test_helpers:generate_test_list(Tests).

global_descendant_capture_test_() ->    
    Tests = [
             {"**/(?<fortytwo>42)/g",
              [{<<"[42, 42, [42], {\"42\":42}, [{\"42\":[42]}]]">>,
                {true, [{"fortytwo", [<<"42">>, <<"42">>, <<"42">>, <<"42">>, <<"42">>]}]}}
              ]}
            ],
    ejpet_test_helpers:generate_test_list(Tests).

-endif.

