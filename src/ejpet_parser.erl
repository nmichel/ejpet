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
    {R, ?RESULT({capture, Expr, Name})};
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
expr([double_star_slash | Tail]) ->
    {R, Expr} = pattern(Tail),
    case R of
        [slash_g | R2] ->
            {R2, ?RESULT({descendant, [Expr], true})};
        _ ->
            {R, ?RESULT({descendant, [Expr], false})}
    end;
expr([star_slash | Tail]) ->
    {R, Expr} = pattern(Tail),
    case R of
        [slash_g | R2] ->
            {R2, ?RESULT({iterable, [Expr], true})}; %% '*' '/' Expr '/g' is a syntactic suger for '<' Expr '>' '/g'
        _ ->
            {R, ?RESULT({iterable, [Expr], false})} %% '*' '/' Expr is a syntactic suger for '<' Expr '>'
    end;
expr([underscore | Tail]) ->
    {Tail, ?RESULT(any)};
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
expr_list(List) ->
    expr_list_head(List).

expr_list_head([star, coma | List]) ->
    {R, Expr} = pattern(List),
    case R of
        [coma | _Tail] ->
            expr_list_tail(R, [{find, Expr}]); % '*' ',' Expr Tail
        [close_square_brace | Tail] -> % '*', ',', Expr, ']'
            {Tail, ?RESULT({list, [{find, Expr}, ?RESULT(eol)]})} % strict : previous expression must match the end of list
    end;
expr_list_head(List) ->
    {R, Expr} = pattern(List),
    case R of
        [coma | _Tail] -> % Expr ',' Tail
            expr_list_tail(R, [Expr]);
        [close_square_brace | Tail] -> % Expr ']'
            {Tail, ?RESULT({list, [Expr, ?RESULT(eol)]})} % strict : previous expression must match the end of list
    end.

expr_list_tail([close_square_brace | Tail], Acc) -> % ']'
    {Tail, ?RESULT({list, lists:reverse([{eol, <<"eol">>} | Acc])})}; % strict : previous expression must match the end of list
expr_list_tail([coma, star, close_square_brace | Tail], Acc) -> % ',' '*' ']'  Tail
    {Tail, ?RESULT({list, lists:reverse(Acc)})}; % loose : previous match may be followed by zero of more items
expr_list_tail([coma, star, coma | Tail], Acc) -> % ',' '*' ',' Expr Tail
    {R, Expr} = pattern(Tail),
    expr_list_tail(R, [{find, Expr} | Acc]);
expr_list_tail([coma | Tail], Acc) -> % ',' Expr Tail
    {R, Expr} = pattern(Tail),
    expr_list_tail(R, [Expr | Acc]).

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

