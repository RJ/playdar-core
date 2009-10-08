-module(scanner).
-include_lib("kernel/include/file.hrl").

-export([scan_dir/2]).

scan_dir(Dir, Pid) ->
    taglib_driver:start_link("playdar_modules/library/priv/taglib_driver/taglib_json_reader"),
    do_scan_dir(Dir, Pid),
    Pid ! {scanner, finished}.

do_scan_dir(Dir, Pid) ->
    io:format("Scanning DIR: ~s~n", [Dir]),
    scan(filelib:wildcard(Dir ++ "/*"), Pid).

scan([], _Pid)    -> ok;
scan([H|T], Pid) ->
    case filelib:is_dir(H) of
        true  ->    do_scan_dir(H, Pid);
        false ->    case file:read_file_info(H) of
                        {ok, Info} ->
                            handle_file(H, Info, Pid);
                        _   -> ok
                    end
    end,
    scan(T, Pid).

handle_file(F, #file_info{size=Size, mtime=Mtime}, Pid) ->
    L = taglib_driver:parsefile(F),
    Pid ! {scanner, {file, F, Mtime, Size, L}}.

