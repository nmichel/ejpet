-module(ejpet_scanner_tests).
-author('nicolas.michel.lava@gmail.com').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

tokenize_test_() ->
    Tests = [
             {"42",
              [{number, 42}]},
             {"\"42\"",
              [{string, "42"}]},
             {"true",
              [true]},
             {"false",
              [false]},
             {"null",
              [null]},
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
             },
             {"<>",
              [open_angle_brace, close_angle_brace]},
             {"[]",
              [open_square_brace, close_square_brace]},
             {"<\"foo\"{_:42[]}:**/>",
              [open_angle_brace, {string, "foo"}, open_curvy_brace, underscore, column, {number, 42}, open_square_brace, close_square_brace, close_curvy_brace, column, double_star_slash, close_angle_brace]}
            ],
    [{Expr, ?_test(?assert(ejpet_scanner:tokenize(Expr) =:= Expected))} || {Expr, Expected} <- Tests].

-endif.
