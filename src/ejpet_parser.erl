-module(ejpet_parser).
-author('nicolas.michel.lava@gmail.com').

-export([parse/1]).

parse(Tokens) ->
    pattern(Tokens).

pattern(Tokens = [open_paren, {inject, _} | _]) ->
    expr(Tokens);
pattern([open_paren, {capture, Name} | Tail]) ->
    {[close_paren | R], Expr} = expr(Tail),
    {R, {capture, Expr, Name}};
pattern([open_paren | Tail]) ->
    {[close_paren | R], Expr} = expr(Tail),
    {R, {capture, Expr, positional}};
pattern(Tokens) ->
    expr(Tokens).

expr([true | Tail]) ->
    {Tail, true};
expr([false | Tail]) ->
    {Tail, false};
expr([null | Tail]) ->
    {Tail, null};
expr([Item = {number, _Value} | Tail]) ->
    {Tail, Item};
expr([Item = {string, _String} | Tail]) ->
    {Tail, Item};
expr([Item = {regex, _String} | Tail]) ->
    {Tail, Item};
expr([double_star_slash | Tail]) ->
    {R, Expr} = pattern(Tail),
    {R, {descendant, [Expr]}};
expr([star_slash | Tail]) ->
    {R, Expr} = pattern(Tail),
    case R of
        [slash_g | R2] ->
            {R2, {iterable, [Expr], true}}; %% '*' '/' Expr '/g' is a syntactic suger for '<' Expr '/g' '>'
        _ ->
            {R, {iterable, [Expr], false}} %% '*' '/' Expr is a syntactic suger for '<' Expr '>'
    end;
expr([underscore | Tail]) ->
    {Tail, any};
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
    {Tail, {inject, Type, Name}}.

%% -----

expr_object([close_curvy_brace | Tail]) ->
    {Tail, {object, any}};
expr_object(List) ->
    expr_object_head(List).

expr_object_head(List) ->
    {R, Expr} = expr_pair(List),
    case R of
        [coma | _Tail] ->
            expr_object_tail(R, [Expr]);
        [close_curvy_brace | Tail] ->
            {Tail, {object, [Expr]}}
    end.

expr_object_tail([close_curvy_brace | Tail], Acc) ->
    {Tail, {object, lists:reverse(Acc)}};
expr_object_tail([coma | Tail], Acc) ->
    {R, Expr} = expr_pair(Tail),
    expr_object_tail(R, [Expr | Acc]).

expr_pair([{string, String}, column, underscore | Tail]) ->
    {Tail, {pair, {string, String}, any}};
expr_pair([{string, String}, column | Tail]) ->
    {R, Expr} = pattern(Tail),
    {R, {pair, {string, String}, Expr}};
expr_pair([underscore, column | Tail]) ->
    {R, Expr} = pattern(Tail),
    {R, {pair, any, Expr}}.

%% -----

expr_list([close_square_brace | Tail]) ->
    {Tail, {list, empty}};
expr_list([star, close_square_brace | Tail]) ->
    {Tail, {list, any}};
expr_list(List) ->
    expr_list_head(List).

expr_list_head([star, coma | List]) ->
    {R, Expr} = pattern(List),
    case R of
        [coma | _Tail] ->
            expr_list_tail(R, [{find, Expr}]); % '*' ',' Expr Tail
        [close_square_brace | Tail] -> % '*', ',', Expr, ']'
            {Tail, {list, [{find, Expr}, eol]}} % strict : previous expression must match the end of list
    end;
expr_list_head(List) ->
    {R, Expr} = pattern(List),
    case R of
        [coma | _Tail] -> % Expr ',' Tail
            expr_list_tail(R, [Expr]);
        [close_square_brace | Tail] -> % Expr ']'
            {Tail, {list, [Expr, eol]}} % strict : previous expression must match the end of list
    end.

expr_list_tail([close_square_brace | Tail], Acc) -> % ']'
    {Tail, {list, lists:reverse([eol | Acc])}}; % strict : previous expression must match the end of list
expr_list_tail([coma, star, close_square_brace | Tail], Acc) -> % ',' '*' ']'  Tail
    {Tail, {list, lists:reverse(Acc)}}; % loose : previous match may be followed by zero of more items
expr_list_tail([coma, star, coma | Tail], Acc) -> % ',' '*' ',' Expr Tail
    {R, Expr} = pattern(Tail),
    expr_list_tail(R, [{find, Expr} | Acc]);
expr_list_tail([coma | Tail], Acc) -> % ',' Expr Tail
    {R, Expr} = pattern(Tail),
    expr_list_tail(R, [Expr | Acc]).

%% -----

expr_iterable([close_angle_brace | Tail]) ->
    {Tail, {iterable, any}};
expr_iterable(List) ->
    expr_iterable_head(List).

expr_iterable_head(List) ->
    {R, Expr} = pattern(List),
    case R of
        [coma | _Tail] -> % Expr ',' Tail
            expr_iterable_tail(R, [Expr]);
        [slash_g, close_angle_brace | Tail] -> % Expr '/g' '>'
            {Tail, {iterable, [Expr], true}};
        [close_angle_brace | Tail] -> % Expr '>'
            {Tail, {iterable, [Expr], false}}
    end.

expr_iterable_tail([slash_g, close_angle_brace | Tail], Acc) -> % '/g' '>'
    {Tail, {iterable, lists:reverse(Acc), true}};
expr_iterable_tail([close_angle_brace | Tail], Acc) -> % '>'
    {Tail, {iterable, lists:reverse(Acc), false}};
expr_iterable_tail([coma | Tail], Acc) -> % ',' Expr Tail
    {R, Expr} = pattern(Tail),
    expr_iterable_tail(R, [Expr | Acc]).
