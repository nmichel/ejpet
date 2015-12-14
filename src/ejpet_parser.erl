-module(ejpet_parser).
-author('nicolas.michel.lava@gmail.com').

-export([parse/1]).

-define(RESULT(E), {E, ejpet_memoize:build_key(E)}).

parse(Tokens) ->
    pattern(Tokens).

pattern(Tokens = [open_paren, {inject, _} | _]) ->
    expr(Tokens);
pattern([open_paren, {capture, Name} | Tail]) ->
    {[close_paren | R], Expr} = expr(Tail),
    {R, ?RESULT({capture, Expr, unicode:characters_to_binary(Name, utf8, utf8)})};
pattern(Tokens) ->
    expr(Tokens).

expr([true | Tail]) ->
    {Tail, ?RESULT(true)};
expr([false | Tail]) ->
    {Tail, ?RESULT(false)};
expr([null | Tail]) ->
    {Tail, ?RESULT(null)};
expr([Item = {number, _Value} | Tail]) ->
    {Tail, ?RESULT(Item)};
expr([Item = {string, _String} | Tail]) ->
    {Tail, ?RESULT(Item)};
expr([Item = {regex, _String} | Tail]) ->
    {Tail, ?RESULT(Item)};
expr([underscore | Tail]) ->
    {Tail, ?RESULT(any)};
expr([open_curvy_brace | Tail]) ->
    expr_object(Tail);
expr([open_square_brace | Tail]) ->
    expr_list(Tail);
expr([open_angle_brace | Tail]) ->
    expr_iterable(Tail);
expr([open_angle_brace_bang | Tail]) ->
    expr_descendant(Tail);
expr([open_paren, {inject, Name}, Type, close_paren | Tail])
  when Type == string;
       Type == number;
       Type == boolean;
       Type == regex ->
    {Tail, ?RESULT({inject, Type, Name})}.

%% -----

expr_object([close_curvy_brace | Tail]) ->
    {Tail, ?RESULT({object, any})};
expr_object(List) ->
    expr_object_head(List).

expr_object_head(List) ->
    {R, Expr} = expr_pair(List),
    case R of
        [coma | _Tail] ->
            expr_object_tail(R, [Expr]);
        [close_curvy_brace | Tail] ->
            {Tail, ?RESULT({object, [Expr]})}
    end.

expr_object_tail([close_curvy_brace | Tail], Acc) ->
    {Tail, ?RESULT({object, lists:reverse(Acc)})};
expr_object_tail([coma | Tail], Acc) ->
    {R, Expr} = expr_pair(Tail),
    expr_object_tail(R, [Expr | Acc]).

expr_pair([{string, String}, column, underscore | Tail]) ->
    {Tail, ?RESULT({pair, {string, String}, any})};
expr_pair([{string, String}, column | Tail]) ->
    {R, Expr} = pattern(Tail),
    {R, ?RESULT({pair, {string, String}, Expr})};
expr_pair([underscore, column | Tail]) ->
    {R, Expr} = pattern(Tail),
    {R, ?RESULT({pair, any, Expr})}.

%% -----

expr_list([close_square_brace | Tail]) ->
    {Tail, ?RESULT({list, empty})};
expr_list([star, close_square_brace | Tail]) ->
    {Tail, ?RESULT({list, any})};
expr_list([star, coma | Tail]) ->
    expr_list_tail(Tail, []);
expr_list(List) ->
    expr_list_head(List, []).

expr_list_head(L, Acc) ->
    {R, Expr} = pattern(L),
    case R of
        [close_square_brace | Tail] ->
            {Tail, ?RESULT({list, lists:reverse([Expr | Acc]), eol})};
        [coma, star, close_square_brace | Tail] ->
            {Tail, ?RESULT({list, lists:reverse([Expr | Acc])})};
        [coma, star, coma | Tail] ->
            expr_list_tail(Tail, [Expr | Acc]);
        [coma | Tail] ->
            expr_list_head(Tail, [Expr | Acc])
    end.

expr_list_tail(List, Acc) ->
    {R, Exprs} = expr_list_span(List, []),
    case R of 
        [close_square_brace | Tail] ->
            {Tail, ?RESULT({list, lists:reverse([{find, Exprs}| Acc])})};
        [coma, star, close_square_brace | Tail] ->
            {Tail, ?RESULT({list, lists:reverse([{find, Exprs}| Acc])})};
        [coma, star, coma | Tail] ->
            expr_list_tail(Tail, [{find, Exprs} | Acc])
    end.

expr_list_span(List, Acc) ->
    {R, Expr} = pattern(List),
    case R of
        [close_square_brace | _] ->
            {R, lists:reverse([{eol, <<"eol">>}, Expr | Acc])};
        [coma, star | _] ->
            {R, lists:reverse([Expr | Acc])};
        [coma | Tail] ->
            expr_list_span(Tail, [Expr | Acc])
    end.

%% -----

expr_iterable([close_angle_brace | Tail]) ->
    {Tail, ?RESULT({iterable, any})};
expr_iterable(List) ->
    expr_iterable_head(List).

expr_iterable_head(List) ->
    {R, Expr} = pattern(List),
    case R of
        [coma | _Tail] -> % Expr ',' Tail
            expr_iterable_tail(R, [Expr]);
        [close_angle_brace, slash_g | Tail] -> % Expr '>' '/g'
            {Tail, ?RESULT({iterable, [Expr], true})};
        [close_angle_brace | Tail] -> % Expr '>'
            {Tail, ?RESULT({iterable, [Expr], false})}
    end.

expr_iterable_tail([close_angle_brace, slash_g | Tail], Acc) -> % '>' '/g'
    {Tail, ?RESULT({iterable, lists:reverse(Acc), true})};
expr_iterable_tail([close_angle_brace | Tail], Acc) -> % '>'
    {Tail, ?RESULT({iterable, lists:reverse(Acc), false})};
expr_iterable_tail([coma | Tail], Acc) -> % ',' Expr Tail
    {R, Expr} = pattern(Tail),
    expr_iterable_tail(R, [Expr | Acc]).

%% -----

expr_descendant(List) ->
    expr_descendant_head(List).

expr_descendant_head(List) ->
    {R, Expr} = pattern(List),
    case R of
        [coma | _Tail] -> % Expr ',' Tail
            expr_descendant_tail(R, [Expr]);
        [close_angle_brace_bang, slash_g | Tail] -> % Expr '>' '/g'
            {Tail, ?RESULT({descendant, [Expr], true})};
        [close_angle_brace_bang | Tail] -> % Expr '>'
            {Tail, ?RESULT({descendant, [Expr], false})}
    end.

expr_descendant_tail([close_angle_brace_bang, slash_g | Tail], Acc) -> % '>' '/g'
    {Tail, ?RESULT({descendant, lists:reverse(Acc), true})};
expr_descendant_tail([close_angle_brace_bang | Tail], Acc) -> % '>'
    {Tail, ?RESULT({descendant, lists:reverse(Acc), false})};
expr_descendant_tail([coma | Tail], Acc) -> % ',' Expr Tail
    {R, Expr} = pattern(Tail),
    expr_descendant_tail(R, [Expr | Acc]).

