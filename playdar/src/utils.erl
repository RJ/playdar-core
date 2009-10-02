-module(utils).
-export([uuid_gen/0, min/2, max/2]).
-import(random).

%% UUID Generation: http://github.com/travis/erlang-uuid/blob/master/uuid.erl
uuid_gen() -> list_to_binary(string:to_upper(to_string(v4()))).

v4() ->
    v4( random:uniform(math:pow(2, 48)) - 1, 
        random:uniform(math:pow(2, 12)) - 1, 
        random:uniform(math:pow(2, 32)) - 1, 
        random:uniform(math:pow(2, 30)) - 1).
v4(R1, R2, R3, R4) ->
    <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>.
to_string(U) ->
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", get_parts(U))).
 
get_parts(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
    [TL, TM, THV, CSR, CSL, N].


% min/max
min(A,B) ->
    if 
        A < B -> A;
        true -> B
    end.

max(A,B) ->
    if 
        A < B -> B;
        true -> A
    end.