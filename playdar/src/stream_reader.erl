-module(stream_reader).

-export([start_link/3]).

start_link({struct, A}, Pid, Ref) ->
    Url = proplists:get_value(<<"url">>, A),
    Mod = url2mod(Url),
    io:format("Delegating to mod: ~p for url: ~p~n", [Mod, Url]),
    Mod:start_link({struct, A}, Pid, Ref).
            

% you better have a module called PROTO_reader where proto is in the url:
% PROTO://path/goes/here
url2mod(Url) ->
    [Proto|_Rest] = string:tokens(binary_to_list(Url), ":"),
    Mod = list_to_atom( Proto ++ "_reader" ),
    Mod.

