-module(ejpet_parser_tests).
-author('nicolas.michel.lava@gmail.com').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


capture_test_() ->
    Tests = [
             {"(?<var1>42)",
              {capture,{number,42},"var1"}},
             {"(42)",
              {capture,{number,42},positional}}
            ],
    lists:foldl(fun({Pattern, Expected}, Acc) ->
                        {[], AST} = ejpet_parser:parse(ejpet_scanner:tokenize(Pattern)),
                        [{Pattern, ?_test(?assert(AST =:= Expected))} | Acc]
                end, [], Tests).

-endif.

