-module(ejpet_scanner).
-author('nicolas.michel.lava@gmail.com').

-export([tokenize/2]).


-record(config, {
          apply_escape_sequence = false,
          raw_string = false
         }).


tokenize(Pattern, Options) ->
    Config = #config{apply_escape_sequence = proplists:get_value(apply_escape_sequence, Options),
                     raw_string = proplists:get_value(raw_string, Options)},
    tokenize(Pattern, [state_root], [], Config).

tokenize([], [state_root | _], Acc, Config) ->
    lists:reverse(Acc);
tokenize([${ | T], State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [open_curvy_brace | Acc], Config);
tokenize([$} | T], State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [close_curvy_brace | Acc], Config);
tokenize([$[ | T], State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [open_square_brace | Acc], Config);
tokenize([$] | T], State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [close_square_brace | Acc], Config);
tokenize([$< | T], State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [open_angle_brace | Acc], Config);
tokenize([$> | T], State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [close_angle_brace | Acc], Config);
tokenize([$, | T], State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [coma | Acc], Config);
tokenize([$: | T], State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [column | Acc], Config);
tokenize([$_ | T], State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [underscore | Acc], Config);
tokenize([$*, $*, $/ | T], State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [double_star_slash | Acc], Config);
tokenize([$*, $/ | T], State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [star_slash | Acc], Config);
tokenize([$* | T], State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [star | Acc], Config);
tokenize([$( | T], State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [open_paren | Acc], Config);
tokenize([$) | T], State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [close_paren | Acc], Config);

tokenize([$\n | T], State = [state_root | _], Acc, Config) ->
    tokenize(T, State, Acc, Config);
tokenize([$\r | T], State = [state_root | _], Acc, Config) ->
    tokenize(T, State, Acc, Config);
tokenize([$\t | T], State = [state_root | _], Acc, Config) ->
    tokenize(T, State, Acc, Config);
tokenize([$\s | T], State = [state_root | _], Acc, Config) ->
    tokenize(T, State, Acc, Config);

tokenize([$t, $r, $u, $e | T], State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [true | Acc], Config);
tokenize([$f, $a, $l, $s, $e | T], State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [false | Acc], Config);
tokenize([$n, $u, $l, $l | T], State = [state_root | _], Acc, Config) ->
    tokenize(T, State, [null | Acc], Config);

tokenize([$?, $< | T], State = [state_root | _], Acc, Config) ->
    tokenize(T, [{state_capture, []} | State], Acc, Config);
tokenize([$#, $" | T], State = [state_root | _], Acc, Config) ->
    tokenize(T, [{state_string, ""}, {state_pattern} | State], Acc, Config);
tokenize([$" | T], State = [state_root | _], Acc, Config) ->
    tokenize(T, [{state_string, ""} | State], Acc, Config);

tokenize([$> | T], [{state_capture, Name = [_|_]} | Tail], Acc, Config) ->
    tokenize(T, Tail, [{capture, lists:reverse(Name)} | Acc], Config);
tokenize([$_ | T], [{state_capture, Name} | Tail], Acc, Config) ->
    tokenize(T, [{state_capture, [$_ | Name]} | Tail], Acc, Config);
tokenize([V | T], [{state_capture, Name} | Tail], Acc, Config) when V >= $A, V =< $Z ->
    tokenize(T, [{state_capture, [V | Name]} | Tail], Acc, Config);
tokenize([V | T], [{state_capture, Name} | Tail], Acc, Config) when V >= $a, V =< $z ->
    tokenize(T, [{state_capture, [V | Name]} | Tail], Acc, Config);
tokenize([V | T], [{state_capture, Name} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_capture, [V | Name]} | Tail], Acc, Config);

tokenize([$-, V | T], State = [state_root | _], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_number, [V, $-]} | State], Acc, Config);
tokenize([$+, V | T], State = [state_root | _], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_number, [V, $+]} | State], Acc, Config);
tokenize([V | T], State = [state_root | _], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_number, [V]} | State], Acc, Config);

tokenize([V | T], [{state_number, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_number, [V | Num]} | Tail], Acc, Config);
tokenize([$., V | T], [{state_number, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_decimal, [V, $. | Num]} | Tail], Acc, Config);
tokenize([$e, $+, V | T], [{state_number, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_frac, [V, $+, $e, $0, $. | Num]} | Tail], Acc, Config);
tokenize([$e, $-, V | T], [{state_number, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_frac, [V, $-, $e, $0, $. | Num]} | Tail], Acc, Config);
tokenize([$e, V | T], [{state_number, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_frac, [V, $+, $e, $0, $. | Num]} | Tail], Acc, Config);
tokenize([$E, $+, V | T], [{state_number, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_frac, [V, $+, $E, $0, $. | Num]} | Tail], Acc, Config);
tokenize([$E, $-, V | T], [{state_number, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_frac, [V, $-, $E, $0, $. | Num]} | Tail], Acc, Config);
tokenize([$E, V | T], [{state_number, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_frac, [V, $+, $E, $0, $. | Num]} | Tail], Acc, Config);
tokenize(T, [{state_number, Num} | Tail], Acc, Config) ->
    tokenize(T, Tail, [{number, list_to_integer(lists:reverse(Num))} | Acc], Config);

tokenize([V | T], [{state_decimal, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_decimal, [V | Num]} | Tail], Acc, Config);
tokenize([$e, $+, V | T], [{state_decimal, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_frac, [V, $+, $e | Num]} | Tail], Acc, Config);
tokenize([$e, $-, V | T], [{state_decimal, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_frac, [V, $-, $e | Num]} | Tail], Acc, Config);
tokenize([$e, V | T], [{state_decimal, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_frac, [V, $+, $e | Num]} | Tail], Acc, Config);
tokenize([$E, $+, V | T], [{state_decimal, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_frac, [V, $+, $E | Num]} | Tail], Acc, Config);
tokenize([$E, $-, V | T], [{state_decimal, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_frac, [V, $-, $E | Num]} | Tail], Acc, Config);
tokenize([$E, V | T], [{state_decimal, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_frac, [V, $+, $E | Num]} | Tail], Acc, Config);
tokenize(T, [{state_decimal, Num} | Tail], Acc, Config) ->
    tokenize(T, Tail, [{number, list_to_float(lists:reverse(Num))} | Acc], Config);

tokenize([V | T], [{state_frac, Num} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_frac, [V | Num]} | Tail], Acc, Config);
tokenize(T, [{state_frac, Num} | Tail], Acc, Config) ->
    tokenize(T, Tail, [{number, list_to_float(lists:reverse(Num))} | Acc], Config);

tokenize([$_ | T], [{state_string, String} | Tail], Acc, Config) ->
    tokenize(T, [{state_string, [$_ | String]} | Tail], Acc, Config);
tokenize([$^ | T], [{state_string, String} | Tail], Acc, Config) ->
    tokenize(T, [{state_string, [$^ | String]} | Tail], Acc, Config);
tokenize([$$ | T], [{state_string, String} | Tail], Acc, Config) ->
    tokenize(T, [{state_string, [$$ | String]} | Tail], Acc, Config);
tokenize([$. | T], [{state_string, String} | Tail], Acc, Config) ->
    tokenize(T, [{state_string, [$. | String]} | Tail], Acc, Config);
tokenize([$\s | T], [{state_string, String} | Tail], Acc, Config) ->
    tokenize(T, [{state_string, [$\s | String]} | Tail], Acc, Config);
tokenize([V | T], [{state_string, String} | Tail], Acc, Config) when V >= $A, V =< $Z ->
    tokenize(T, [{state_string, [V | String]} | Tail], Acc, Config);
tokenize([V | T], [{state_string, String} | Tail], Acc, Config) when V >= $a, V =< $z ->
    tokenize(T, [{state_string, [V | String]} | Tail], Acc, Config);
tokenize([V | T], [{state_string, String} | Tail], Acc, Config) when V >= $0, V =< $9 ->
    tokenize(T, [{state_string, [V | String]} | Tail], Acc, Config);
tokenize([$" | T], [{state_string, String}, {state_pattern} | Tail], Acc, Config) ->
    tokenize(T, Tail, [{regex, lists:reverse(String)} | Acc], Config);
tokenize([$" | T], [{state_string, String} | Tail], Acc, Config) ->
    tokenize(T, Tail, [{string, lists:reverse(String)} | Acc], Config).
