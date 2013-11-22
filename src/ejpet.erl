-module(ejpet).
-author('nicolas.michel.lava@gmail.com').

-export([decode/2, encode/2,
         generator/1,
         compile/1, compile/3,
         backend/1,
         run/2,
         match/2, match/3, match/4]).


-define(DEFAULT_BACKEND, jsx).
-define(DEFAULT_OPTIONS, []).


%% -----
%% Generator options
%% {number_strict_match, (true|false)}
%% -----

decode(JSON, Backend) when is_binary(JSON) ->
    Backend:decode(JSON).

encode(Node, Backend) ->
    case Backend:encode(Node) of
        R when is_binary(R) ->
            R;
        R when is_list(R) ->
            list_to_binary(R)
    end.

generator(Backend) when is_atom(Backend) ->
    list_to_atom("ejpet_" ++ atom_to_list(Backend) ++ "_generators").

compile(Pattern) ->
    compile(Pattern, ?DEFAULT_BACKEND, ?DEFAULT_OPTIONS).

compile(Pattern, Backend, Options) ->
    {[], AST} = ejpet_parser:parse(ejpet_scanner:tokenize(Pattern)),
    {ejpet, Backend, (generator(Backend)):generate_matcher(AST, Options)}.

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
            {true, [{Name, encode(Capture, Backend)} || {Name, Capture} <- Captures]};
        R ->
            R
    end;
match(Pattern, JSON) ->
    match(Pattern, JSON, ?DEFAULT_BACKEND, ?DEFAULT_OPTIONS).

match(Pattern, JSON, Options) ->
    match(Pattern, JSON, ?DEFAULT_BACKEND, Options).
    
match(Pattern, JSON, Backend, Options) ->
    Opaque = compile(Pattern, Backend, Options),
    match(Opaque, JSON).
