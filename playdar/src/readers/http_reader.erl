-module(http_reader).

-include_lib("kernel/include/file.hrl").

-export([start_link/3]).

start_link(A, Pid, Ref) ->
    spawn_link(fun()->run(A,Pid,Ref)end).

% starts sending data to Pid
run({struct, A}, Pid, Ref) ->
    Url = binary_to_list(proplists:get_value(<<"url">>, A)),
    
    {ok, Id} = http:request(get, {Url, []}, [], 
                               [{sync, false}, 
                                {stream, self}, 
                                {version, 1.1}, 
                                {body_format, binary}]),
    receive

        {http, {Id, stream_start, Headers}} ->
            io:format("Headers rcvd: ~p~n",[Headers]),
            % snag the content-length and content-type headers:
            Headers1 = [ { "content-type", 
                           proplists:get_value("content-type", Headers, 
                                               "binary/unknown-not-specified")} ],
            Headers2 = case proplists:get_value("content-length", Headers) of
                undefined -> Headers1;
                Len -> [ {"content-length", Len} | Headers1 ]
            end,
            Pid ! {Ref, headers, Headers2},
            start_streaming(Id, Pid, Ref);
            
        {http, {Id, error, Reason}} ->
            io:format("HTTP req fail~n",[]),
            Pid ! {Ref, error, Reason};
            
        XXX ->
            io:format("Rvd: ~p~n", [XXX]), ok
            
    after 10000 ->
            io:format("HTTP timeout on headers~n",[]),
            Pid ! {Ref, error, timeout}
    end.
    
    
start_streaming(Id, Pid, Ref) ->
    receive
        {http, {Id, stream, Bin}} ->
            Pid ! {Ref, data, Bin},
            start_streaming(Id, Pid, Ref);
        
        {http, {Id, error, Reason}} ->
            Pid ! {Ref, error, Reason};
            
        {http, {Id, stream_end, _Headers}} ->
            Pid ! {Ref, eof}
    end.

