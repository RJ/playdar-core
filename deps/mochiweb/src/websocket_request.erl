%%
%% This is a wrapper for the Socket connection
%% @author Dave Bryson [http://weblog.miceda.org]
%%
-module(websocket_request,[Socket, Path]).

-export([get/1, get_data/0, send/1]).

get(path)   -> Path;
get(socket) -> Socket.

%% Return the data from the Socket. Parse it from the WebSocket format
get_data() ->
    case gen_tcp:recv(Socket, 0) of
	    {ok,Data} ->
	        unframe(binary_to_list(Data));
        _Other ->
	        exit(normal)
    end.

%% Send the data to the client. Format it in the WebSocket format
send(Data)      ->  gen_tcp:send(Socket, [0] ++ Data ++ [255]).

%% Borrowed from Joe Armstrong's example
unframe([0|T])  -> unframe1(T).
unframe1([255]) -> [];
unframe1([H|T]) -> [H|unframe1(T)].
