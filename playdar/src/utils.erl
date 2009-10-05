-module(utils).
-export([uuid_gen/0, min/2, max/2, levenshtein/2, list_agg/1 ]).
-import(random).

%% UUID Generation: http://github.com/travis/erlang-uuid/blob/master/uuid.erl
uuid_gen() -> list_to_binary(string:to_upper(to_string(v4()))).

v4() ->
    v4( random:uniform(round(math:pow(2, 48))) - 1, 
        random:uniform(round(math:pow(2, 12))) - 1, 
        random:uniform(round(math:pow(2, 32))) - 1, 
        random:uniform(round(math:pow(2, 30))) - 1).
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

% list_agg([a,a,a,b,b,c]) -> [{a,3}, {b,2}, {c,1}]
list_agg([])        -> [];
list_agg([H|T])     -> list_agg(T, [{H,1}]).
list_agg([], Agg)   -> Agg;
list_agg([H|T], [{Ah,An}|At])     when H == Ah  -> list_agg(T, [{Ah,An+1}|At]);
list_agg([H|T], [{Ah,_An}|_At]=Agg) when H /= Ah  -> list_agg(T, [{H,1}|Agg]).


% edit dist
levenshtein(Samestring, Samestring) -> 0;
levenshtein(String, []) -> length(String);
levenshtein([], String) -> length(String);
levenshtein(Source, Target) ->
    levenshtein_rec(Source, Target, lists:seq(0, length(Target)), 1).

%% Recurses over every character in the source string and calculates a list of distances
levenshtein_rec([SrcHead|SrcTail], Target, DistList, Step) ->
    levenshtein_rec(SrcTail, Target, levenshtein_distlist(Target, DistList, SrcHead, [Step], Step), Step + 1);
levenshtein_rec([], _, DistList, _) ->
    lists:last(DistList).

%% Generates a distance list with distance values for every character in the target string
levenshtein_distlist([TargetHead|TargetTail], [DLH|DLT], SourceChar, NewDistList, LastDist) when length(DLT) > 0 ->
    Min = lists:min([LastDist + 1, hd(DLT) + 1, DLH + dif(TargetHead, SourceChar)]),
    levenshtein_distlist(TargetTail, DLT, SourceChar, NewDistList ++ [Min], Min);
levenshtein_distlist([], _, _, NewDistList, _) ->
    NewDistList.

% Calculates the difference between two characters or other values
dif(C, C) -> 0;
dif(_, _) -> 1.

