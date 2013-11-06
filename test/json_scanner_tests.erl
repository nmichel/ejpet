-module(json_scanner_tests).
-author('nicolas.michel.lava@gmail.com').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

illegal_pattern_test_() ->
    Tests = [
             {"* /"},
             {"** /"}
            ],
    [{Expr, ?_test(?assertError(function_clause, json_scanner:tokenize(Expr)))} || {Expr} <- Tests].

legal_pattern_test_() ->
    Tests = [
             {"*/",
              [star_slash]},
             
             {"**/",
              [double_star_slash]},
             
             {"* */",
              [star, star_slash]},
             
             {"**/*",
              [double_star_slash, star]},
             
             {"*/{_:42}",
              [star_slash, open_curvy_brace, underscore, column, {number, 42}, close_curvy_brace]},
             
             {"{ _ : */ {_: 42 } }",
              [open_curvy_brace, underscore, column, star_slash,
               open_curvy_brace, underscore, column, {number, 42}, close_curvy_brace, close_curvy_brace]},
             
             {"  {  \"answer is 42\":{ _:42, \"bar\":_}, _:true, \"foo\":_ } ",
              [open_curvy_brace, {string,"answer is 42"}, column, open_curvy_brace, underscore, column, {number,42}, coma,
               {string,"bar"}, column, underscore, close_curvy_brace, coma, underscore, column, true,coma, {string,"foo"},
               column, underscore, close_curvy_brace]
             }
            ],
    [{Expr, ?_test(?assert(json_scanner:tokenize(Expr) =:= Expected))} || {Expr, Expected} <- Tests].

%      {"{\"foo\":*/{_:42}}",
%      }]
-endif.
