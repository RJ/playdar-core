% Examines the URL in a response, and tells you the module that can be
% used to stream the data. Examples include http:// file://.
-module(stream_reader).

-export([start_link/3]).

start_link({struct, A}, Pid, Ref) ->
    case proplists:get_value(<<"url">>, A) of
        undefined ->
            io:format("A doesnt contain <<url>>: ~w~n",[A]),
            undefined;
            
        Url ->
            Mod = url2mod(Url),
            io:format("Delegating to mod: ~p for url: ~p~n", [Mod, Url]),
            Mod:start_link({struct, A}, Pid, Ref)
    end.
            

% you better have a module called PROTO_reader where proto is in the url:
% PROTO://path/goes/here
%
% TODO  this chould be dynamic too, so mods can register abilities to stream
%       specified protocols.
url2mod(Url) when is_binary(Url) ->
    [Proto|_Rest] = string:tokens(binary_to_list(Url), ":"),
    Mod = list_to_atom( Proto ++ "_reader" ),
    Mod.

