-module(ejpet_generator).
-author('nicolas.michel.lava@gmail.com').

-export([generate_matcher/3]).

generate_matcher({What, _Key}, Option, Backend) ->
    Backend:generate_matcher(What, Option).

