-module(ejpet_parser_tests).
-author('nicolas.michel.lava@gmail.com').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


capture_test_() ->
    Tests = [
             {"(?<var1>42)",
              {capture,{number,42},"var1"}},
             {"(!<var1>string)",
              {inject, string, "var1"}},
             {"(!<var1>boolean)",
              {inject, boolean, "var1"}},
             {"(!<var1>number)",
              {inject, number, "var1"}},
             {"(!<var1>regex)",
              {inject, regex, "var1"}},
             {"{_:[*, (!<var1>regex), *]}",
              {object,[{pair,any,{list,[{find,{inject,regex,"var1"}}]}}]}}
            ],
    lists:foldl(fun({Pattern, Expected}, Acc) ->
                        {[], AST} = ejpet_parser:parse(ejpet_scanner:tokenize(Pattern, [])),
                        [{Pattern, ?_test(?assert(AST =:= Expected))} | Acc]
                end, [], Tests).

-endif.

