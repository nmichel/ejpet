-module(ejpet_scanner).
-author('nicolas.michel.lava@gmail.com').

-export([tokenize/1]).


tokenize(Pattern) ->
    tokenize(Pattern, [state_root], []).

tokenize([], [state_root | _], Acc) ->
    lists:reverse(Acc);
tokenize([${ | T], State = [state_root | _], Acc) ->
    tokenize(T, State, [open_curvy_brace | Acc]);
tokenize([$} | T], State = [state_root | _], Acc) ->
    tokenize(T, State, [close_curvy_brace | Acc]);
tokenize([$[ | T], State = [state_root | _], Acc) ->
    tokenize(T, State, [open_square_brace | Acc]);
tokenize([$] | T], State = [state_root | _], Acc) ->
    tokenize(T, State, [close_square_brace | Acc]);
tokenize([$< | T], State = [state_root | _], Acc) ->
    tokenize(T, State, [open_angle_brace | Acc]);
tokenize([$> | T], State = [state_root | _], Acc) ->
    tokenize(T, State, [close_angle_brace | Acc]);
tokenize([$, | T], State = [state_root | _], Acc) ->
    tokenize(T, State, [coma | Acc]);
tokenize([$: | T], State = [state_root | _], Acc) ->
    tokenize(T, State, [column | Acc]);
tokenize([$_ | T], State = [state_root | _], Acc) ->
    tokenize(T, State, [underscore | Acc]);
tokenize([$*, $*, $/ | T], State = [state_root | _], Acc) ->
    tokenize(T, State, [double_star_slash | Acc]);
tokenize([$*, $/ | T], State = [state_root | _], Acc) ->
    tokenize(T, State, [star_slash | Acc]);
tokenize([$* | T], State = [state_root | _], Acc) ->
    tokenize(T, State, [star | Acc]);
tokenize([$( | T], State = [state_root | _], Acc) ->
    tokenize(T, State, [open_paren | Acc]);
tokenize([$) | T], State = [state_root | _], Acc) ->
    tokenize(T, State, [close_paren | Acc]);

tokenize([$\n | T], State = [state_root | _], Acc) ->
    tokenize(T, State, Acc);
tokenize([$\r | T], State = [state_root | _], Acc) ->
    tokenize(T, State, Acc);
tokenize([$\t | T], State = [state_root | _], Acc) ->
    tokenize(T, State, Acc);
tokenize([$\s | T], State = [state_root | _], Acc) ->
    tokenize(T, State, Acc);

tokenize([$t, $r, $u, $e | T], State = [state_root | _], Acc) ->
    tokenize(T, State, [true | Acc]);
tokenize([$f, $a, $l, $s, $e | T], State = [state_root | _], Acc) ->
    tokenize(T, State, [false | Acc]);
tokenize([$n, $u, $l, $l | T], State = [state_root | _], Acc) ->
    tokenize(T, State, [null | Acc]);

tokenize([$?, $< | T], State = [state_root | _], Acc) ->
    tokenize(T, [{state_capture, []} | State], Acc);
tokenize([$> | T], [{state_capture, Name = [_|_]} | Tail], Acc) ->
    tokenize(T, Tail, [{capture, lists:reverse(Name)} | Acc]);
tokenize([$_ | T], [{state_capture, Name} | Tail], Acc) ->
    tokenize(T, [{state_capture, [$_ | Name]} | Tail], Acc);
tokenize([V | T], [{state_capture, Name} | Tail], Acc) when V >= $A, V =< $Z ->
    tokenize(T, [{state_capture, [V | Name]} | Tail], Acc);
tokenize([V | T], [{state_capture, Name} | Tail], Acc) when V >= $a, V =< $z ->
    tokenize(T, [{state_capture, [V | Name]} | Tail], Acc);
tokenize([V | T], [{state_capture, Name} | Tail], Acc) when V >= $0, V =< $9 ->
    tokenize(T, [{state_capture, [V | Name]} | Tail], Acc);

tokenize([V | T], State = [state_root | _], Acc) when V >= $0, V =< $9 -> %% unroll
    tokenize(T, [{state_number, [V]} | State], Acc);
tokenize([V | T], [{state_number, Num} | Tail], Acc) when V >= $0, V =< $9 -> %% unroll
    tokenize(T, [{state_number, [V | Num]} | Tail], Acc);
tokenize(T, [{state_number, Num} | Tail], Acc) ->
    tokenize(T, Tail, [{number, list_to_integer(lists:reverse(Num))} | Acc]);

tokenize([$" | T], State = [state_root | _], Acc) ->
    tokenize(T, [{state_string, ""} | State], Acc);
tokenize([$_ | T], [{state_string, String} | Tail], Acc) ->
    tokenize(T, [{state_string, [$_ | String]} | Tail], Acc);
tokenize([$. | T], [{state_string, String} | Tail], Acc) ->
    tokenize(T, [{state_string, [$. | String]} | Tail], Acc);
tokenize([$\s | T], [{state_string, String} | Tail], Acc) ->
    tokenize(T, [{state_string, [$\s | String]} | Tail], Acc);
tokenize([V | T], [{state_string, String} | Tail], Acc) when V >= $A, V =< $Z ->
    tokenize(T, [{state_string, [V | String]} | Tail], Acc);
tokenize([V | T], [{state_string, String} | Tail], Acc) when V >= $a, V =< $z ->
    tokenize(T, [{state_string, [V | String]} | Tail], Acc);
tokenize([V | T], [{state_string, String} | Tail], Acc) when V >= $0, V =< $9 ->
    tokenize(T, [{state_string, [V | String]} | Tail], Acc);
tokenize([$" | T], [{state_string, String} | Tail], Acc) ->
    tokenize(T, Tail, [{string, lists:reverse(String)} | Acc]).
