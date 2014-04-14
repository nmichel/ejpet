-module(ejpet_default_cache).
-author('nicolas.michel.lava@gmail.com').

-export([start_server/0,
         stop_server/1,
         build_cache_fun/0,
         build_cache_fun/1]).

start_server() ->
    gen_server:start_link(ejpet_default_cache_srv, [], []).

stop_server(Pid) ->
    gen_server:cast(Pid, stop).

build_cache_fun() ->
    fun({get, _}) ->
            false;
       ({store, _Key, Matcher}) ->
            {ok, Matcher}
    end.

build_cache_fun(Pid) ->
    fun(Op) ->
            gen_server:call(Pid, Op)
    end.
