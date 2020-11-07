-module(ejpet_default_cache).
-author('nicolas.michel.lava@gmail.com').

-export([start_server/0,
         stop_server/1,
         build_cache_fun/0,
         build_cache_fun/1]).

start_server() ->
    gen_server:start_link(ejpet_default_cache_srv, [], []).

stop_server(Name) ->
    gen_server:cast(Name, stop).

build_cache_fun() ->
    fun({get, _}) ->
            false;
        ({store, _Key, Matcher}) ->
            {ok, Matcher}
    end.

build_cache_fun(Name) ->
    fun(Op) ->
            gen_server:call(Name, Op)
    end.
