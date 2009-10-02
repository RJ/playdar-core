-module(scanner).
-export([scan_dir/2]).

scan_dir(Dir, Pid) ->
    taglib_driver:start_link("priv/taglib_driver/taglib_json_reader"),
    do_scan_dir(Dir, Pid),
    Pid ! {scanner, finished}.

do_scan_dir(Dir, Pid) ->
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
%%     case proplists:get_value(<<"error">>, L) of
%%         undefined -> % found tags:
%%             Art     = proplists:get_value(<<"artist">>, L, ""),
%%             Trk     = proplists:get_value(<<"track">>, L, ""),
%%             io:format("TAGS  ~s\t~s - ~s~n",[F, Art, Trk]);
%% 
%%         Error ->
%%             io:format("ERROR ~s\t~s~n",[F, Error])
%%     end.
