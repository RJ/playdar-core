-module(playdartcp_stream).
-include("playdar.hrl").
-include("playdartcp.hrl").

-export([start/1, start/2]).

start(Sock) -> start(Sock, unknown).

start(Sock, Mode) ->
    Pid = spawn(fun()-> start_real(Sock, Mode) end),
    {ok, Pid}.

% New connection established - the parent that setup this socket outbound 
% will have sent the first sending/requesting packet to initiate everything.
start_real(Sock, Mode) ->
    case gen_tcp:recv(Sock, 0, 3000) of
        {ok, Packet} ->
            % get remote Ip address
            {ok, {RemoteIp, _SockPort}} = inet:peername(Sock),
            case (catch ?B2T(Packet)) of
                
                {sending, Ref, Sid} ->
                    % is this transfer we are about to receive valid?
                    case playdartcp_router:consume_transfer({Ref,Sid}) of
                        Pid when is_pid(Pid) ->
                            case Mode of
                                recv ->
                                    ?LOG(info, "receive_stream", []),
                                    receive_stream(Ref, Sid, Sock, Pid);
                                unknown ->
                                    ok = gen_tcp:send(Sock, ?T2B({requesting, Ref, Sid})),
                                    ?LOG(info, "receive_stream", []),
                                    receive_stream(Ref, Sid, Sock, Pid)
                            end;
                        
                        unknown ->
                            ?LOG(warn, "Invalid transfer key", []),
                            gen_tcp:close(Sock)
                    end;
                
                {requesting, Ref, Sid} ->
                    % is the transfer they are asking for permitted?
                    case playdartcp_router:consume_transfer({Ref,Sid}) of
                        Ip when Ip == RemoteIp -> % should be Ip of client
                            case Mode of 
                                unknown ->
                                    ok = gen_tcp:send(Sock, ?T2B({sending, Ref, Sid})),
                                    send_stream(Ref, Sid, Sock);
                                send ->
                                    send_stream(Ref, Sid, Sock)
                            end;                            
                            
                        Else ->
                            ?LOG(warn, "Not sending, request invalid Key: ~p, Transfertoken: ~p", [{Ref,Sid}, Else])
                    end;
                
                _Else ->
                    ?LOG(warn, "Unhandled first packet in streamconn", []),
                    gen_tcp:close(Sock)
                
            end;
        
        {error, closed} ->
            ?LOG(warn, "Remote peer closed our streaming socket without sending data", []),
            error;
        
        {error, timeout} ->
            ?LOG(warn, "timeout waiting for first packet (sending/receiving header)", []),
            gen_tcp:close(Sock)
    end.

receive_stream(Ref, Sid, Sock, Pid) -> 
    case gen_tcp:recv(Sock, 0, 10000) of
        {ok, Packet} ->
            case (catch ?B2T(Packet)) of
                {sid_response, Ref, Sid, M} ->
                    case M of
                        {headers, H} ->
                            ?LOG(info, "headers arrived: ~p", [H]),
                            Pid ! {Ref, headers, H},
                            receive_stream(Ref, Sid, Sock, Pid);
                        {data, D} -> 
                            Pid ! {Ref, data, D},
                            receive_stream(Ref, Sid, Sock, Pid);
                        {error, E} -> 
                            ?LOG(warn, "Error for transfer: ~p",[E]),
                            Pid ! {Ref, error, E},
                            gen_tcp:close(Sock);
                        eof -> 
                            Pid ! {Ref, eof},
                            gen_tcp:close(Sock)
                    end;
                Wtf ->
                    ?LOG(info, "Unhandled packet in receive_stream: ~p", [Wtf]),
                    error
            end;
        
        {error, closed} -> Pid ! {Ref, eof};
        
        {error, timeout} ->
            ?LOG(warn, "Timeout receiving packets", []),
            Pid ! {Ref, error, timeout}
        
    end.
            


send_stream(Ref, Sid, Sock) ->
    ?LOG(info, "send_stream", []),
    A = playdar_resolver:result(Sid),
    case playdar_reader_registry:get_streamer(A, self(), Ref) of
        undefined ->
            ?LOG(warn, "No streamer available, aborting", []),
            Msg = ?T2B({sid_response, Ref, Sid, {error, 5031}}),
            gen_tcp:send(Sock, Msg),
            gen_tcp:close(Sock),
            ok;
        Sfun ->         
            % we trap exits, so if the streaming fun crashes we can catch it
            % and fwd an error message to the recipient
            process_flag(trap_exit, true),
            Sfun(),
            receive
                {Ref, headers, Headers} ->
                    M = ?T2B({sid_response, Ref, Sid, {headers, Headers}}),
                    ok = gen_tcp:send(Sock, M),
                    stream_result_body(Ref, Sid, Sock);
                
                {'EXIT', _Pid, Reason} when Reason /= normal ->
                    ?LOG(error, "Streamer fun crashed: ~p", [Reason]),
                    Msg = ?T2B({sid_response, Ref, Sid, {error, 999}}),
                    ok = gen_tcp:send(Sock, Msg),
                    ok
            
                after 10000 ->
                    M = ?T2B({sid_response, Ref, Sid, {error, headers_timeout}}),
                    ok = gen_tcp:send(Sock, M),
                    ok
            end
    end.
    
stream_result_body(Ref, Sid, Sock) ->
    receive
        {'EXIT', _Pid, Reason} when Reason /= normal ->
            ?LOG(error, "Streamer fun crashed whilst streaming body: ~p", [Reason]),
            Msg = ?T2B({sid_response, Ref, Sid, {error, 999}}),
            ok = gen_tcp:send(Sock, Msg),
            ok;
        
        {Ref, data, Data} ->
            Msg = ?T2B({sid_response, Ref, Sid, {data, Data}}),
            ok = gen_tcp:send(Sock, Msg),
            stream_result_body(Ref, Sid, Sock);
        
        {Ref, error, _Reason} ->
            Msg = ?T2B({sid_response, Ref, Sid, {error, -1}}),
            ok = gen_tcp:send(Sock, Msg),
            ok;
        
        {Ref, eof} ->
            Msg = ?T2B({sid_response, Ref, Sid, eof}),
            ok = gen_tcp:send(Sock, Msg),
            ok
    
    after 10000 ->
            Msg = ?T2B({sid_response, Ref, Sid, {error, timeout}}),
            ok = gen_tcp:send(Sock, Msg),
            ok
    end.