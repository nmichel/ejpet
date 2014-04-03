-module(ejpet_parser).
-author('nicolas.michel.lava@gmail.com').

-export([parse/1]).

-define(STOB(S), (list_to_binary(S))).
-define(BS(S, T), <<S, T/binary>>).
-define(BB(B, T), <<B/binary, T/binary>>).

parse(Tokens) ->
    pattern(Tokens).

pattern(Tokens = [open_paren, {inject, _} | _]) ->
    expr(Tokens);
pattern([open_paren, {capture, Name} | Tail]) ->
    {[close_paren | R], Expr = {_AST, Hash}} = expr(Tail),
    {R, {{capture, Expr, Name}, ?BS("capture", Hash)}};
pattern(Tokens) ->
    expr(Tokens).

expr([true | Tail]) ->
    {Tail, {true, <<"true">>}};
expr([false | Tail]) ->
    {Tail, {false, <<"false">>}};
expr([null | Tail]) ->
    {Tail, {null, <<"null">>}};
expr([Item = {number, Value} | Tail]) ->
    VS = lists:concat([Value]),
    {Tail, {Item, ?BS("number", ?STOB(VS))}};
expr([Item = {string, String} | Tail]) ->
    {Tail, {Item, ?BS("string", String)}};
expr([Item = {regex, String} | Tail]) ->
    {Tail, {Item, ?BS("regex", String)}};
expr([double_star_slash | Tail]) ->
    {R, Expr = {_AST, Hash}} = pattern(Tail),
    case R of
        [slash_g | R2] ->
            {R2, {{descendant, [Expr], true}, ?BS("descendantg", Hash)}};
        _ ->
            {R, {{descendant, [Expr], false}, ?BS("descendant", Hash)}}
    end;
expr([star_slash | Tail]) ->
    {R, Expr = {_AST, Hash}} = pattern(Tail),
    case R of
        [slash_g | R2] ->
            {R2, {{iterable, [Expr], true}, ?BS("iterableg", Hash)}}; %% '*' '/' Expr '/g' is a syntactic suger for '<' Expr '/g' '>'
        _ ->
            {R, {{iterable, [Expr], false}, ?BS("iterable", Hash)}} %% '*' '/' Expr is a syntactic suger for '<' Expr '>'
    end;
expr([underscore | Tail]) ->
    {Tail, {any, <<"any">>}};
expr([open_curvy_brace | Tail]) ->
    expr_object(Tail);
expr([open_square_brace | Tail]) ->
    expr_list(Tail);
expr([open_angle_brace | Tail]) ->
    expr_iterable(Tail);
expr([open_paren, {inject, Name}, Type, close_paren | Tail])
  when Type == string;
       Type == number;
       Type == boolean;
       Type == regex ->
    {Tail, {{inject, Type, Name}, ?BS("inject", ?STOB(lists:concat([Type, Name])))}}.

%% -----

expr_object([close_curvy_brace | Tail]) ->
    {Tail, {{object, any}, <<"objectany">>}};
expr_object(List) ->
    expr_object_head(List).

expr_object_head(List) ->
    {R, Expr = {_AST, Hash}} = expr_pair(List),
    case R of
        [coma | _Tail] ->
            expr_object_tail(R, [Expr]);
        [close_curvy_brace | Tail] ->
            {Tail, {{object, [Expr]}, ?BS("object", Hash)}}
    end.

expr_object_tail([close_curvy_brace | Tail], Acc) ->
    HashAcc = lists:foldr(fun({_, HE}, A) ->
                                  <<A/binary, HE/binary>>
                          end, <<"">>, Acc),
    {Tail, {{object, lists:reverse(Acc)}, ?BS("object", HashAcc)}};
expr_object_tail([coma | Tail], Acc) ->
    {R, Expr} = expr_pair(Tail),
    expr_object_tail(R, [Expr | Acc]).

expr_pair([{string, String}, column, underscore | Tail]) ->
    {Tail, {{pair, {string, String}, any}, ?BB(?BS("pairstring", String), ?STOB("any"))}};
expr_pair([{string, String}, column | Tail]) ->
    {R, Expr = {_AST, Hash}} = pattern(Tail),
    {R, {{pair, {string, String}, Expr}, ?BB(?BS("pairstring", String), Hash)}};
expr_pair([underscore, column | Tail]) ->
    {R, Expr = {_AST, Hash}} = pattern(Tail),
    {R, {{pair, any, Expr}, ?BS("pairany", Hash)}}.

%% -----

expr_list([close_square_brace | Tail]) ->
    {Tail, {{list, empty}, <<"listempty">>}};
expr_list([star, close_square_brace | Tail]) ->
    {Tail, {{list, any}, <<"listany">>}};
expr_list(List) ->
    expr_list_head(List).

expr_list_head([star, coma | List]) ->
    {R, Expr = {_AST, Hash}} = pattern(List),
    case R of
        [coma | _Tail] ->
            expr_list_tail(R, [{{find, Expr}, ?BS("find", Hash)}]); % '*' ',' Expr Tail
        [close_square_brace | Tail] -> % '*', ',', Expr, ']'
            {Tail, {{list, [{{find, Expr}, ?BS("find", Hash)}, {eol, <<"eol">>}]}, % strict : previous expression must match the end of list
                    ?BB(?BS("listfind", Hash), ?STOB("eol"))}}
    end;
expr_list_head(List) ->
    {R, Expr = {_AST, Hash}} = pattern(List),
    case R of
        [coma | _Tail] -> % Expr ',' Tail
            expr_list_tail(R, [Expr]);
        [close_square_brace | Tail] -> % Expr ']'
            {Tail, {{list, [Expr, {eol, <<"eol">>}]}, % strict : previous expression must match the end of list
                    ?BB(?BS("list", Hash), ?STOB("eol"))}}
    end.

expr_list_tail([close_square_brace | Tail], Acc) -> % ']'
    HashAcc = lists:foldr(fun({_, HE}, A) ->
                                  <<A/binary, HE/binary>>
                          end, <<"">>, Acc),
    {Tail, {{list, lists:reverse([{eol, <<"eol">>} | Acc])}, ?BB(?BS("list", HashAcc), ?STOB("eol"))}}; % strict : previous expression must match the end of list
expr_list_tail([coma, star, close_square_brace | Tail], Acc) -> % ',' '*' ']'  Tail
    HashAcc = lists:foldr(fun({_, HE}, A) ->
                                  <<A/binary, HE/binary>>
                          end, <<"">>, Acc),
    {Tail, {{list, lists:reverse(Acc)}, ?BS("list", HashAcc)}}; % loose : previous match may be followed by zero of more items
expr_list_tail([coma, star, coma | Tail], Acc) -> % ',' '*' ',' Expr Tail
    {R, Expr = {_AST, Hash}} = pattern(Tail),
    expr_list_tail(R, [{{find, Expr}, ?BS("find", Hash)} | Acc]);
expr_list_tail([coma | Tail], Acc) -> % ',' Expr Tail
    {R, Expr} = pattern(Tail),
    expr_list_tail(R, [Expr | Acc]).

%% -----

expr_iterable([close_angle_brace | Tail]) ->
    {Tail, {{iterable, any}, <<"iterableany">>}};
expr_iterable(List) ->
    expr_iterable_head(List).

expr_iterable_head(List) ->
    {R, Expr = {_AST, Hash}} = pattern(List),
    case R of
        [coma | _Tail] -> % Expr ',' Tail
            expr_iterable_tail(R, [Expr]);
        [slash_g, close_angle_brace | Tail] -> % Expr '/g' '>'
            {Tail, {{iterable, [Expr], true}, ?BS("iterableg", Hash)}};
        [close_angle_brace | Tail] -> % Expr '>'
            {Tail, {{iterable, [Expr], false}, ?BS("iterable", Hash)}}
    end.

expr_iterable_tail([slash_g, close_angle_brace | Tail], Acc) -> % '/g' '>'
    HashAcc = lists:foldr(fun({_, HE}, A) ->
                                  <<A/binary, HE/binary>>
                          end, <<"">>, Acc),
    {Tail, {{iterable, lists:reverse(Acc), true}, ?BS("iterableg", HashAcc)}};
expr_iterable_tail([close_angle_brace | Tail], Acc) -> % '>'
    HashAcc = lists:foldr(fun({_, HE}, A) ->
                                  <<A/binary, HE/binary>>
                          end, <<"">>, Acc),
    {Tail, {{iterable, lists:reverse(Acc), false}, ?BS("iterable", HashAcc)}};
expr_iterable_tail([coma | Tail], Acc) -> % ',' Expr Tail
    {R, Expr} = pattern(Tail),
    expr_iterable_tail(R, [Expr | Acc]).
