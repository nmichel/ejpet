-module(jsonbroker).
-author('nicolas.michel.lava@gmail.com').

-compile([export_all]).


%% "{*, \"toto\": *}"
% match([{<<"toto">>, _}]) ->
%     true;
% match([_ | Tail]) ->
%     match(Tail).

%% "{*, \"toto\": *, *}"
% match([{<<"toto">>, _}, _]) ->
%     true;
% match([_ | Tail]) ->
%     match(Tail).

%% -----

%% "[42, *]"
% match([42 | _]) ->
%     true.

%% "[*, 42, *]"
% match([42 | _]) ->
%     true;
% match([_ | Tail]) ->
%     match(Tail).

%% "[*, 42]"
% match([42]) ->
%     true;
% match([_ | Tail]) ->
%     match(Tail).

%% -----

deep([{}]) ->
    io:format("{}"),
    false;
deep([]) ->
    false;
deep(Object = [{_Key, Val} | Tail]) ->
    io:format("{"),
    deep_object(Object),
    io:format("}");
deep(List = [C|_]) ->
    io:format("["),
    deep_list(List),
    io:format("]");
deep(Val) when is_number(Val) ->
    io:format("~p", [Val]),
    false;
deep(Val) when is_binary(Val) ->
    io:format("~p", [Val]),
    false;
deep(true) ->
    io:format("true"),
    false;
deep(false) ->
    io:format("false"),
    false;
deep(null) ->
    io:format("null"),
    null.

deep_pair({Key, Val}) ->
    io:format("~p:", [Key]),
    deep(Val).

deep_object([]) ->
    false;
deep_object([Pair | Tail]) ->
    deep_pair(Pair),
    deep_object(Tail).

deep_list([]) ->
    false;
deep_list([Item | Tail]) ->
    deep(Item),
    deep_list(Tail).


% "{\"toto\"}". %% Match an object with a "toto" key.
% "{:12}". %% Match an object with a key with value 12.
% "{:12, \"foo\"}". %% Match an object with a value 12 and a key "foo".
% "[*, 42]". %% Match a list ending with 42
% "[*, 42, *]". %% Match a list containing 42
% "[42, *]". %% Match a list begining by 42
% "/**/[*,*/<\"toto\">]".
% "/**/{_:*/<\"toto\">}".

% deep(What) ->
%     case type(What) of
%         object ->
%             deep_object(What);
%         list ->
%             deep_list(What);
%         pair ->
%             {_K, V} = What,
%             deep(V);
%         _ ->
%             false
%     end.

% deep_object([Pair | Tail]) ->
%     deep(Pair),
%     deep_object(Tail).

% deep_list([Item | Tail]) ->
%     deep(Item),
%     deep_list(Tail).

%% -----

%% -----

test() ->
    [{"{*, \"toto\": *}",
      [{<<"{\"key\": [12, 13, 14], \"other\": {\"toto\": 42}}">>, error},
       {<<"{\"key\": [12, 13, 14], \"toto\": 42}">>, true},
       {<<"{\"key\": [12, 13, 14], \"toto\": 42, \"after\": {}}">>, false}]},
     {"[*, 42, *]",
      []},
     {"[42, *]",
      []},
     {"[*, 42]",
      []}
    ].

% test(Expr, Tests) ->
%     Matcher = jsonbroker:build_matcher(Expr),
%     lists:map(fun({String, Expected}) ->
%                       JSON = jsx:decode(String),
%                       Expected = 
%                           try
%                               Matcher:check(JSON)
%                           catch _:_ ->
%                               false
%                           end
%               end, Tests).
