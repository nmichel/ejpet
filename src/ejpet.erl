-module(ejpet).
-author('nicolas.michel.lava@gmail.com').

-export([decode/2, encode/2,
         generator/1,
         compile/1, compile/3,
         backend/1,
         run/2, run/3,
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
    {[], AST} = ejpet_parser:parse(ejpet_scanner:tokenize(Pattern, Options)),
    {ejpet, Backend, (generator(Backend)):generate_matcher(AST, Options)}.

backend({ejpet, Backend, _Fun}) ->
    Backend.

run(Node, EPM) ->
    run(Node, EPM, []).

run(Node, {ejpet, _Backend, Fun}, Params) ->
    Fun(Node, Params).

match(Subject, Pattern) ->
    match(Subject, Pattern, ?DEFAULT_OPTIONS, []).

match(Subject, Pattern, Options) ->
    match(Subject, Pattern, Options, []).

match(Subject, Pattern, Options, Params) when is_list(Subject) ->
    match(list_to_binary(Subject), Pattern, Options, Params);
match(Subject, {ejpet, Backend, Fun}, _Options, Params) ->
    Node = decode(Subject, Backend),
    case Fun(Node, Params) of
        {true, Captures} ->
            {true, [{Name, encode(Capture, Backend)} || {Name, Capture} <- Captures]};
        R ->
            R
    end;
match(Subject, Pattern, Options, Params) ->
    Opaque = compile(Pattern, ?DEFAULT_BACKEND, Options),
    match(Subject, Opaque, Options, Params).
