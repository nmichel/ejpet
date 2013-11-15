-module(ejpet).
-author('nicolas.michel.lava@gmail.com').

-export([compile/1, compile/2,
         backend/1,
         run/2,
         match/2, match/3]).


-define(DEFAULT_BACKEND, jsx).


compile(Pattern) ->
    compile(Pattern, ?DEFAULT_BACKEND).

compile(Pattern, Backend) ->
    {[], AST} = ejpet_parser:parse(ejpet_scanner:tokenize(Pattern)),
    {ejpet, Backend, (generator(Backend)):generate_matcher(AST)}.

backend({ejpet, Backend, _Fun}) ->
    Backend.

run({ejpet, _Backend, Fun}, Node) ->
    Fun(Node).

match(Pattern, JSON) when is_list(JSON) ->
    match(Pattern, list_to_binary(JSON));
match({ejpet, Backend, Fun}, JSON) ->
    Node = decode(JSON, Backend),
    case Fun(Node) of
        {true, Captures} ->
            io:format("Captures: ~p~n", [Captures]),
            {true, [{Name, encode(Capture, Backend)} || {Name, Capture} <- Captures]};
        R ->
            R
    end;
match(Pattern, JSON) ->
    match(Pattern, JSON, ?DEFAULT_BACKEND).

match(Pattern, JSON, Backend) ->
    Opaque = compile(Pattern, Backend),
    match(Opaque, JSON).

%% -----

generator(Backend) ->
    list_to_atom("ejpet_" ++ atom_to_list(Backend) ++ "_generators").

decode(JSON, Backend) when is_binary(JSON) ->
    Backend:decode(JSON).

encode(Node, Backend) ->
    Backend:encode(Node).
    
