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
             {"<!42!>",
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
             {"<![*, 42]!>",
              [
               {<<"[]">>, {false, []}},
               {<<"{}">>, {false, []}},
               {<<"[42]">>, {false, []}},
               {<<"[\"foo\", [42]]">>, {true, []}},
               {<<"{\"foo\" : [42]}">>, {true, []}},
               {<<"[[{\"bar\": [{\"foo\": [\"this one matches\", 42]}]}], \"next does not match\", 42]">>, {true, []}}
              ]},
             {"<!{_:42}!>",
              [{<<"[{\"bar\": 42}]">>, {true, []}}
              ]},

             {"<! 42, \"PI\" !>",
              [
               {<<"42">>, {false, []}}
               ,{<<"\"PI\"">>, {false, []}}

               ,{<<"[42]">>, {false, []}}
               ,{<<"[\"PI\"]">>, {false, []}}
               ,{<<"{\"k\": 42}">>, {false, []}}
               ,{<<"{\"k\": \"PI\"}">>, {false, []}}

               ,{<<"[42, \"PI\"]">>, {true, []}}
               ,{<<"[\"PI\", 42]">>, {true, []}}
               ,{<<"[\"spam\", \"PI\", \"spam\", 42, \"spam\"]">>, {true, []}}
               ,{<<"[\"spam\", \"Pi\", \"spam\", 42, \"spam\"]">>, {false, []}}
               ,{<<"{\"kpi\": \"PI\", \"k42\": 42}">>, {true, []}}
               ,{<<"{\"k42\": 42, \"kpi\": \"PI\"}">>, {true, []}}
               ,{<<"{\"k1\": \"spam\", \"k42\": 42, \"k2\": \"spam\", \"kpi\": \"PI\", \"k3\": \"spam\"}">>, {true, []}}
               ,{<<"{\"k1\": \"spam\", \"k42\": 41, \"k2\": \"spam\", \"kpi\": \"PI\", \"k3\": \"spam\"}">>, {false, []}}

               ,{<<"[\"spam\", [\"PI\"], \"spam\", {\"k42\": 42}, \"spam\"]">>, {true, []}}
               ,{<<"{\"k1\": \"spam\", \"k42\": [42], \"k2\": \"spam\", \"kpi\": {\"kpi\": \"PI\"}, \"k3\": \"spam\"}">>, {true, []}}

               ,{<<"[\"spam\", [\"spam\", \"PI\", \"spam\"], \"spam\", {\"kspan\": \"spam\", \"k42\": 42}, \"spam\"]">>, {true, []}}
               ,{<<"{\"k1\": \"spam\", \"k42\": [42, \"spam\"], \"k2\": \"spam\", \"kpi\": {\"kpi\": \"PI\", \"kspam\": \"spam\"}, \"k3\": \"spam\"}">>, {true, []}}

               ,{<<"[[[[[[[[[[42]]]], {\"k1\": [{\"k2\": {\"k3\": [1, 2, 3, [\"spam\", \"PI\"], 4, 5]}}]}]]]]]]">>, {true, []}}
               ,{<<"[[[[[[[[[[41]]]], {\"k1\": [{\"k2\": {\"k3\": [1, 2, 3, [\"spam\", \"PI\"], 4, 5]}}]}]]]]]]">>, {false, []}}
               ,{<<"[[[[[[[[[[42]]]], {\"k1\": [{\"k2\": {\"k3\": [1, 2, 3, [\"spam\", \"pI\"], 4, 5]}}]}]]]]]]">>, {false, []}}
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

descendant_capture_test_() ->
    Tests = [
             {"<! (?<cap42>{_:42}), 24 !>",
              [
               {<<"[{\"k1\": 42}, 24]">>, {true, [{"cap42", [<<"{\"k1\":42}">>]}]}}
               ,{<<"[{\"k1\": 42}]">>, {false, []}}
               ,{<<"[{\"uk1\": {\"uk1\": \"spam\", \"k1\": 42}}, [[[]], 24, {}]]">>, {true, [{"cap42", [<<"{\"uk1\":\"spam\",\"k1\":42}">>]}]}}
               ,{<<"[{\"uk1\": {\"uk1\": \"spam\", \"k1\": 42, \"k24\": 24}}, [[[]], \"not 42\", {}]]">>, {true, [{"cap42", [<<"{\"uk1\":\"spam\",\"k1\":42,\"k24\":24}">>]}]}}
              ]}
             ,{"<! (?<cap42>{_:42}), (?<cap24>{_:24}) !>",
              [
               {<<"[{\"uk1\": {\"uk1\": \"spam\", \"k1\": 42, \"k24\": 24}}, [[[]], \"not 42\", {}]]">>, % Capture the same node
                {true, [{"cap24", [<<"{\"uk1\":\"spam\",\"k1\":42,\"k24\":24}">>]},
                        {"cap42", [<<"{\"uk1\":\"spam\",\"k1\":42,\"k24\":24}">>]}]}}
               ,{<<"[{\"uk1\": {\"uk1\": \"spam\", \"k1\": 42, \"foo\": {\"k24\":24}}}, [[[]], \"not 42\", {}]]">>, % Captured nodes are different
                 {true, [{"cap24", [<<"{\"k24\":24}">>]},
                         {"cap42", [<<"{\"uk1\":\"spam\",\"k1\":42,\"foo\":{\"k24\":24}}">>]}]}}
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
             {"<{\"sender\":(!<who>number),\"text\":(?<text>_)}>",
              [{<<"[{\"sender\":42,\"text\":\"foo\"}, {\"sender\":24,\"text\":\"bar\"}]">>, [{<<"who">>, 42}], {true, [{"text", [<<"\"foo\"">>]}]}},
               {<<"[{\"sender\":42,\"text\":\"foo\"}, {\"sender\":24,\"text\":\"bar\"}]">>, [{<<"who">>, 24}], {true, [{"text", [<<"\"bar\"">>]}]}}
              ]},
             {"<{\"sender\":(?<who>{\"name\":(!<from>regex)}),\"text\":(?<text>_)}>",
              [{<<"[{\"sender\":{\"name\": \"id_42\"},\"text\":\"foo\"}, {\"sender\":{\"name\": \"grp_42\"},\"text\":\"bar\"}]">>, [{<<"from">>, MP1}], {true, [{"text", [<<"\"foo\"">>]},
                                                                                                                                                                {"who", [<<"{\"name\":\"id_42\"}">>]}]}},
               {<<"[{\"sender\":{\"name\": \"id_42\"},\"text\":\"foo\"}, {\"sender\":{\"name\": \"grp_42\"},\"text\":\"bar\"}]">>, [{<<"from">>, MP2}], {true, [{"text", [<<"\"bar\"">>]},
                                                                                                                                                                {"who", [<<"{\"name\":\"grp_42\"}">>]}]}}
              ]}
            ],
    ejpet_test_helpers:generate_test_list(Tests).

global_iterable_capture_test_() ->
    Tests = [
             {"<(?<node>{\"codec\":_, \"lang\":(?<lang>_)})>/g",
              [{<<"[{\"codec\": \"audio\", \"lang\": \"fr\"}, {\"codec\": \"video\", \"lang\": \"en\"}, {\"codec\": \"foo\", \"lang\": \"it\"}]">>,
                {true, [{"node", [<<"{\"codec\":\"audio\",\"lang\":\"fr\"}">>, <<"{\"codec\":\"video\",\"lang\":\"en\"}">>, <<"{\"codec\":\"foo\",\"lang\":\"it\"}">>]},
                        {"lang", [<<"\"fr\"">>, <<"\"en\"">>, <<"\"it\"">>]}]}}
              ]},
             {"[*, <42, #\"foo\">/g]",
              [
               {<<"[\"i\", [42, \"foo\"]]">>, {true, []}}
               ,{<<"[\"i\", [\"barfoo\", 42]]">>, {true, []}}
               ,{<<"[\"i\", [{}, \"barfoo\", \"whatever\", 42, \"neh\"]]">>, {true, []}}
              ]},

             %% ----- global shallow match
             %%
             %% check non capturing behaviour (must behave as non-global shallow matching)
             %%
             {"<42, \"foo\">/g",
              [
               {<<"[42, \"foo\"]">>, {true, []}}
               ,{<<"[\"foo\", 42]">>, {true, []}} %% order does not matter
               ,{<<"[\"foo\", \"42\"]">>, {false, []}} %% every conditions must be met
               ,{<<"[\"header\", \"foo\", 34, 42]">>, {true, []}} %% non matching items do not matter
               ,{<<"{\"header\": \"header value\", \"foo key\": \"foo\", \"thirtyfor\": 34, \"fortytwo\": 42}">>, {true, []}} %% works for list and object
               ,{<<"{\"header\": \"header value\", \"foo key\": \"foo value\", \"thirtyfor\": 34, \"fortytwo\": 42}">>, {false, []}} %% every conditions must be matched
              ]
             },

             %% check capturing behaviour
             %%
             {<<"<(?<forty2>42), \"foo\">/g">>,
              [
               {<<"[42, \"foo\"]">>, {true, [{"forty2", [<<"42">>]}]}} %% capture
               ,{<<"[\"foo\", 42]">>, {true, [{"forty2", [<<"42">>]}]}} %% order does not matter
               ,{<<"[42, \"foo\", 42]">>, {true, [{"forty2", [<<"42">>, <<"42">>]}]}} %% capture all matching items
               ,{<<"{\"forty2\": 42, \"foo\": \"foo\", \"42\": 42}">>, {true, [{"forty2", [<<"42">>, <<"42">>]}]}} %% work for objects too
              ]
             },
             {<<"<(?<foo>\"foo\"), (?<forty2>42)>/g">>,
              [
               {<<"[42, \"foo\"]">>, {true, [{"forty2", [<<"42">>]}, {"foo", [<<"\"foo\"">>]}]}} %% capture
               ,{<<"[\"foo\", 42]">>, {true, [{"forty2", [<<"42">>]}, {"foo", [<<"\"foo\"">>]}]}} %% order does not matter
               ,{<<"[42, \"foo\", 42]">>, {true, [{"forty2", [<<"42">>, <<"42">>]}, {"foo", [<<"\"foo\"">>]}]}} %% capture all matching items
               ,{<<"{\"forty2\": 42, \"foo\": \"foo\", \"42\": 42}">>, {true, [{"forty2", [<<"42">>, <<"42">>]}, {"foo", [<<"\"foo\"">>]}]}} %% work for objects too
              ]
             },
             {<<"<(?<node><(?<val>_)>/g)>/g">>,
              [
               {<<"[1, 2, 3]">>, {false, []}}
               ,{<<"[[1, 2, 3]]">>,
                 {true, [{"node", [<<"[1,2,3]">>]},
                         {"val", [<<"1">>, <<"2">>, <<"3">>]}]}}
               ,{<<"{}">>, {false, []}}
               ,{<<"[{}]">>, {false, []}}
               ,{<<"[{\"a\":42}]">>, {true, [{"node", [<<"{\"a\":42}">>]},
                                             {"val", [<<"42">>]}]}}
               ,{<<"{\"a\":[]}">>, {false, []}}
               ,{<<"{\"a\": [1, 2, 3], \"b\": [4, 5]}">>, {true, [{"val",[<<"1">>,<<"2">>,<<"3">>,<<"4">>,<<"5">>]},
                                                                  {"node",[<<"[1,2,3]">>,<<"[4,5]">>]}]}}
               ,{<<"[\"hello\", {\"a\": \"an a\", \"b\": \"a b\", \"c\": \"a c\"}, [1, 2, {\"3\":3}], \"world\"]">>,
                 {true,[{"val", [<<"\"an a\"">>,<<"\"a b\"">>,<<"\"a c\"">>,<<"1">>,<<"2">>, <<"{\"3\":3}">>]},
                        {"node", [<<"{\"a\":\"an a\",\"b\":\"a b\",\"c\":\"a c\"}">>,
                                  <<"[1,2,{\"3\":3}]">>]}]}}
              ]
             },

             %% ----- deep global match
             %%
             %% check capturing behaviour
             %%
             {<<"<!(?<forty2>42)!>/g">>,
              [
               {<<"[42, \"foo\"]">>, {true, [{"forty2", [<<"42">>]}]}} %% capture
               ,{<<"[\"foo\", 42]">>, {true, [{"forty2", [<<"42">>]}]}} %% order does not matter
               ,{<<"[42, \"foo\", 42]">>, {true, [{"forty2", [<<"42">>, <<"42">>]}]}} %% capture all matching items
               ,{<<"{\"forty2\": 42, \"foo\": \"foo\", \"42\": 42}">>, {true, [{"forty2", [<<"42">>, <<"42">>]}]}} %% work for objects too
               ,{<<"[42, 42, [42], {\"42\":42}, [{\"42\":[42]}]]">>,
                 {true, [{"forty2", [<<"42">>, <<"42">>, <<"42">>, <<"42">>, <<"42">>]}]}}
              ]
             },
             {<<"<<!(?<foo>#\"foo\")!>>/g">>,
              [
               {<<"[\"foo\"]">>, {false, []}} %% first level list cannot match
               ,{<<"[[\"nehfoo\"]]">>, {true, [{"foo", [<<"\"nehfoo\"">>]}]}} %% nested list matches
               ,{<<"[[[[\"foobar\"]]]]">>, {true, [{"foo", [<<"\"foobar\"">>]}]}} %% very nested list matches
               ,{<<"{\"fookey\":\"foo\"}">>, {false, []}} %% first level object cannot match
               ,{<<"[{\"fookey\":\"foo\"}]">>, {true, [{"foo", [<<"\"foo\"">>]}]}} %% nested object matches
               ,{<<"[[{\"key\": [{\"fookey\":\"foo\"}]}]]">>, {true, [{"foo", [<<"\"foo\"">>]}]}} %% very nested object matches
              ]
             },
             {<<"<(?<node><!(?<foo>#\"foo\")!>/g)>/g">>,
              [
               {<<"[{\"deep\": [{\"whatever\": \"nehfoo\"}]},
                    {\"a\":\"non-fOo\", \"fkey\": \"ifoo\"},
                    {\"fookey\":\"foo\"}]">>, {true,[{"node", [<<"{\"deep\":[{\"whatever\":\"nehfoo\"}]}">>,
                                                               <<"{\"a\":\"non-fOo\",\"fkey\":\"ifoo\"}">>,
                                                               <<"{\"fookey\":\"foo\"}">>]},
                                                     {"foo",[<<"\"nehfoo\"">>,<<"\"ifoo\"">>,<<"\"foo\"">>]}]} %% very nested object matches, and all matching items are captured
               }
              ]
             }
            ],
    ejpet_test_helpers:generate_test_list(Tests).

global_descendant_capture_test_() ->    
    Tests = [
             {"<!(?<fortytwo>42)!>/g",
              [{<<"42">>,
                {false, []}},
               {<<"[42, 42, [42], {\"42\":42}, [{\"42\":[42]}]]">>,
                {true, [{"fortytwo", [<<"42">>, <<"42">>, <<"42">>, <<"42">>, <<"42">>]}]}},
               {<<"{\"a\": 42, \"b\": [1, 2, [42, \"42\"], {\"aa\": 42}]}">>,
                {true, [{"fortytwo", [<<"42">>, <<"42">>, <<"42">>]}]}}
              ]}
            ],
    ejpet_test_helpers:generate_test_list(Tests).

-endif.

