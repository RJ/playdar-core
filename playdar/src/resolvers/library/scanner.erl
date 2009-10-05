-module(scanner).
-export([scan_dir/2]).

scan_dir(Dir, Pid) ->
    taglib_driver:start_link("priv/taglib_driver/taglib_json_reader"),
    do_scan_dir(Dir, Pid),
    Pid ! {scanner, finished}.

do_scan_dir(Dir, Pid) ->
    io:format("Scanning DIR: ~s~n", [Dir]),
    scan(filelib:wildcard(Dir ++ "/*"), Pid).

scan([], _Pid)    -> ok;
scan([H|T], Pid) ->
    case filelib:is_dir(H) of
        true  ->    do_scan_dir(H, Pid);
        false ->    case filelib:is_file(H) of
                        true    -> Mtime = 0, handle_file(H, Mtime, Pid);
                        false   -> ok
                    end
    end,
    scan(T, Pid).

handle_file(F, Mtime, Pid) ->
    L = taglib_driver:parsefile(F),
    Pid ! {scanner, {file, F, Mtime, L}}.

