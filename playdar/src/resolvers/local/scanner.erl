-module(scanner).
-export([scan_dir/1]).

scan_dir(Dir) ->
    scan(filelib:wildcard(Dir ++ "/*")).

scan([])    -> ok;
scan([H|T]) ->
    case filelib:is_dir(H) of
        true  ->    scan_dir(H);
        false ->    case filelib:is_file(H) of
                        true    -> handle_file(H);
                        false   -> ok
                    end
    end,
    scan(T).

handle_file(F) ->
    io:format("File: ~s~n",[F]).
