-module(http_reader).
-behaviour(playdar_reader).
-include("playdar.hrl").
-include_lib("kernel/include/file.hrl").

-define(HTTP_HEADERS_TIMEOUT, 5000).
-define(HTTP_STREAM_TIMEOUT, 10000).

-export([start_link/3, reader_protocols/0]).

start_link(A, Pid, Ref) ->
    spawn_link(fun()->run(A,Pid,Ref)end).

reader_protocols() ->
    [ {"http", {?MODULE, start_link}} ].


% starts sending data to Pid
run({struct, A}, Pid, Ref) ->
    Url = binary_to_list(proplists:get_value(<<"url">>, A)),
    ?LOG(info, "Requesting ~p", [Url]),
    {ok, Id} = http:request(get, {Url, []}, [], 
                               [{sync, false}, 
                                {stream, self}, 
                                {version, 1.1}, 
                                {body_format, binary}]),
    receive

        {http, {Id, stream_start, Headers}} ->
            ?LOG(info, "Serving stream: ~s", [Url]),
            % snag the content-length and content-type headers:
            ContentType = proplists:get_value("content-type", Headers, 
                                             "binary/unknown-not-specified"),
            ContentLength = proplists:get_value("content-length", Headers),
            % some results from web indexes to audio files have expired and
            % just give you an html response that says bugger off:
            case string:str(ContentType, "text/html") of
                0 ->
                    Headers1 = [ { "content-type", ContentType } ],
                    Headers2 = case ContentLength of
                        undefined -> Headers1;
                        Len -> [ {"content-length", Len} | Headers1 ]
                    end,
                    ?LOG(info, "starting stream, type: ~s size: ~s", [ContentType, ContentLength]),
                    Pid ! {Ref, headers, Headers2},
                    start_streaming(Id, Pid, Ref);
                _ ->
                    ?LOG(warning, "Got content type of ~s - aborting, probably failed.", [ContentType]),
                    Pid ! {Ref, error, "Suspicious content type header"}
            end;
            
        {http, {Id, error, Reason}} ->
            ?LOG(warning, "HTTP req failed for ~s",[Url]),
            Pid ! {Ref, error, Reason}
            
    after ?HTTP_HEADERS_TIMEOUT ->
            ?LOG(warning, "HTTP timeout on receiving headers for ~s",[Url]),
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
    
    after ?HTTP_STREAM_TIMEOUT ->
            ?LOG(warning, "Timeout receiving stream", []),
            Pid ! {Ref, error, timeout}
    end.

