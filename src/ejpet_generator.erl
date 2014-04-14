-module(ejpet_generator).
-author('nicolas.michel.lava@gmail.com').

-export([generate_matcher/3,
         generate_matcher/4]).

generate_matcher(What, Option, Backend) ->
    generate_matcher(What, Option, Backend, ejpet_default_cache:build_cache_fun()).

generate_matcher(What, Option, Backend, CacheFn) ->
    F = fun({Expr, Key}, Opt, CB) ->
                case CacheFn({get, Key}) of
                    {true, Matcher} ->
                        Matcher;
                    false ->
                        Matcher = Backend:generate_matcher(Expr, Opt, CB),
                        {ok, Matcher} = CacheFn({store, Key, Matcher}),
                        Matcher
                end
        end,
    F(What, Option, F).
