-module(ejpet_parser).
-author('nicolas.michel.lava@gmail.com').

-export([parse/1]).


parse([true | Tail]) ->
    {Tail, true};
parse([false | Tail]) ->
    {Tail, false};
parse([null | Tail]) ->
    {Tail, null};
parse([Item = {number, Value} | Tail]) ->
    {Tail, Item};
parse([Item = {string, String} | Tail]) ->
    {Tail, Item};
parse([double_star_slash | Tail]) ->
    {R, Expr} = parse(Tail),
    {R, {descendant, [Expr]}};
parse([star_slash | Tail]) ->
    {R, Expr} = parse(Tail),
    {R, {iterable, [Expr]}}; %% '*' '/' Expr is a syntactic suger for '<' Expr '>'
parse([underscore | Tail]) ->
    {Tail, any};
parse([open_curvy_brace | Tail]) ->
    parse_object(Tail);
parse([open_square_brace | Tail]) ->
    parse_list(Tail);
parse([open_angle_brace | Tail]) ->
    parse_iterable(Tail).

%% -----

parse_object([close_curvy_brace | Tail]) ->
    {Tail, {object, any}};
parse_object(List) ->
    parse_object_head(List).

parse_object_head(List) ->
    {R, Expr} = parse_pair(List),
    case R of
        [coma | Tail] ->
            parse_object_tail(R, [Expr]);
        [close_curvy_brace | Tail] ->
            {Tail, {object, [Expr]}}
    end.

parse_object_tail([close_curvy_brace | Tail], Acc) ->
    {Tail, {object, lists:reverse(Acc)}};
parse_object_tail([coma | Tail], Acc) ->
    {R, Expr} = parse_pair(Tail),
    parse_object_tail(R, [Expr | Acc]).

parse_pair([{string, String}, column, underscore | Tail]) ->
    {Tail, {pair, {string, String}, any}};
parse_pair([{string, String}, column | Tail]) ->
    {R, Expr} = parse(Tail),
    {R, {pair, {string, String}, Expr}};
parse_pair([underscore, column | Tail]) ->
    {R, Expr} = parse(Tail),
    {R, {pair, any, Expr}}.

%% -----

parse_list([close_square_brace | Tail]) ->
    {Tail, {list, empty}};
parse_list([star, close_square_brace | Tail]) ->
    {Tail, {list, any}};
parse_list(List) ->
    parse_list_head(List).

parse_list_head([star, coma | List]) ->
    {R, Expr} = parse(List),
    case R of
        [coma | Tail] ->
            parse_list_tail(R, [{find, Expr}]); % '*' ',' Expr Tail
        [close_square_brace | Tail] -> % '*', ',', Expr, ']'
            {Tail, {list, [{find, Expr}, eol]}} % strict : previous expression must match the end of list
    end;
parse_list_head(List) ->
    {R, Expr} = parse(List),
    case R of
        [coma | Tail] -> % Expr ',' Tail
            parse_list_tail(R, [Expr]);
        [close_square_brace | Tail] -> % Expr ']'
            {Tail, {list, [Expr, eol]}} % strict : previous expression must match the end of list
    end.

parse_list_tail([close_square_brace | Tail], Acc) -> % ']'
    {Tail, {list, lists:reverse([eol | Acc])}}; % strict : previous expression must match the end of list
parse_list_tail([coma, star, close_square_brace | Tail], Acc) -> % ',' '*' ']'  Tail
    {Tail, {list, lists:reverse( Acc)}}; % loose : previous match may be followed by zero of more items
parse_list_tail([coma, star, coma | Tail], Acc) -> % ',' '*' ',' Expr Tail
    {R, Expr} = parse(Tail),
    parse_list_tail(R, [{find, Expr} | Acc]);
parse_list_tail([coma | Tail], Acc) -> % ',' Expr Tail
    {R, Expr} = parse(Tail),
    parse_list_tail(R, [Expr | Acc]).

%% -----

parse_iterable([close_angle_brace | Tail]) ->
    {Tail, {iterable, any}};
parse_iterable(List) ->
    parse_iterable_head(List).

parse_iterable_head(List) ->
    {R, Expr} = parse(List),
    case R of
        [coma | Tail] -> % Expr ',' Tail
            parse_iterable_tail(R, [Expr]);
        [close_angle_brace | Tail] -> % Expr '>'
            {Tail, {iterable, [Expr]}}
    end.

parse_iterable_tail([close_angle_brace | Tail], Acc) -> % '>'
    {Tail, {iterable, lists:reverse(Acc)}};
parse_iterable_tail([coma | Tail], Acc) -> % ',' Expr Tail
    {R, Expr} = parse(Tail),
    parse_iterable_tail(R, [Expr | Acc]).
