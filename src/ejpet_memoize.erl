-module(ejpet_memoize).
-author('nicolas.michel.lava@gmail.com').

-export([build_key/1]).

-define(STOB(S), <<(erlang:phash2(S)):4/little-signed-integer-unit:8>>).
-define(BS(S, T), <<S, (T)/binary>>).
-define(BB(B, T), <<(B)/binary, (T)/binary>>).

build_key({capture, {_AST, Hash}, Name}) ->
    ?BB(?BS("c", Hash), ?STOB(Name));
build_key(true) ->
    <<"t">>;
build_key(false) ->
    <<"f">>;
build_key(null) ->
    <<"n">>;
build_key({number, Value}) ->
    ?BS("f", ?STOB(Value));
build_key({string, String}) ->
    ?BS("s", ?STOB(String));
build_key({regex, String}) ->
    ?BS("r", ?STOB(String));
build_key(any) ->
    <<"a">>;
build_key({inject, Type, Name}) ->
    ?BB(?BS("i", ?STOB(Type)), ?STOB(Name));
build_key({object, any}) ->
    <<"oa">>;
build_key({object, Acc}) ->
    ?BS("o", fold_expr_list(Acc));
build_key({pair, {string, String}, any}) ->
    ?BB(?BS("ps", ?STOB(String)), ?STOB("any"));
build_key({pair, {string, String}, {_AST, Hash}}) ->
    ?BB(?BS("ps", ?STOB(String)), Hash);
build_key({pair, any, {_AST, Hash}}) ->
    ?BS("pa", Hash);
build_key({list, empty}) ->
    <<"le">>;
build_key({list, any}) ->
    <<"la">>;
build_key({find, {_AST, Hash}}) ->
    ?BS("f", Hash);
build_key(eol) ->
    <<"e">>;
build_key({list, Acc}) ->
    ?BS("l", fold_list_expr_list(Acc));
build_key({list, Acc, eol}) ->
    ?BB(build_key({list, Acc}), build_key(eol));
build_key({iterable, any}) ->
    <<"ia">>;
build_key({iterable, Acc, true}) ->
    ?BS("ig", fold_expr_list(Acc));
build_key({iterable, Acc, false}) ->
    ?BS("i", fold_expr_list(Acc));
build_key({descendant, Acc, true}) ->
    ?BS("dg", fold_expr_list(Acc));
build_key({descendant, Acc, false}) ->
    ?BS("d", fold_expr_list(Acc)).

fold_expr_list(Ks) ->
    lists:foldl(fun({_, HE}, A)->
                        <<A/binary, HE/binary>>
                end, <<"">>, Ks).

fold_list_expr_list(Ks) ->
    lists:foldl(fun({find, Exprs}, A) ->
                        HE = fold_expr_list(Exprs),
                        <<A/binary, HE/binary >>;
                   ({_, HE}, A)->
                        <<A/binary, HE/binary>>
                end, <<"">>, Ks).
