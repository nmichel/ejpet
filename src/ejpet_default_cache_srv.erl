-module(ejpet_default_cache_srv).
-author('nicolas.michel.lava@gmail.com').

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {cache}).

init([]) ->
    {ok, #state{cache = dict:new()}}.

handle_call({get, K}, _From, State = #state{cache = D}) ->
    Reply =
        case dict:find(K, D) of 
            {ok, V} ->
                {true, V};
            error ->
                false
        end,
    {reply, Reply, State};
handle_call({store, K, M}, _From, State = #state{cache = D}) ->
    % UpdateFn = fun(_) -> 
    %                    throw(found_already_present_error) % 
    %            end,
    % D2 = dict:update(K, UpdateFn, M, D),
    D2 = dict:store(K, M, D),
    {reply, {ok, M}, State#state{cache = D2}}.

handle_cast(stop, State) ->
    {stop, stopped, State}.

handle_info(_Info, State) ->
    {stop, unexpected_msg, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

