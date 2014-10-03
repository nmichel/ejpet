-module(ejpet_parser_tests).
-author('nicolas.michel.lava@gmail.com').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


capture_test_() ->
    Tests = [
             {"(?<var1>42)",
              {{capture,{{number,42},
                         <<102, 152,199,206,1>>},"var1"},
               <<99, 102,152,199,206,1, 49,6,87,2>>}},
             {"(!<var1>string)",
              {{inject, string, "var1"}, <<105, 71,144,171,7, 49,6,87,2>>}},
             {"(!<var1>boolean)",
              {{inject, boolean, "var1"}, <<105, 30,43,102,1, 49,6,87,2>>}},
             {"(!<var1>number)",
              {{inject, number, "var1"}, <<105, 194,56,92,7, 49,6,87,2>>}},
             {"(!<var1>regex)",
              {{inject, regex, "var1"}, <<105, 200,189,120,0, 49,6,87,2>>}},
             {"{_:[*, (!<var1>regex), *]}",
              {{object,[{{pair,any,{{list,[{find,[{{inject,regex,"var1"}, <<105, 200,189,120,0, 49,6,87,2>>}]}]},
                                   <<108, 105,200,189,120,0,49,6,87,2>>}},
                         <<112,97, 108,105,200,189,120,0,49,6,87,2>>}]},
               <<111, 112,97,108,105,200,189,120,0,49,6,87,2>>}}
            ],
    lists:foldl(fun({Pattern, Expected}, Acc) ->
                        {[], AST} = ejpet_parser:parse(ejpet_scanner:tokenize(Pattern, [])),
                        [{Pattern, ?_test(?assert(AST =:= Expected))} | Acc]
                end, [], Tests).

-endif.

