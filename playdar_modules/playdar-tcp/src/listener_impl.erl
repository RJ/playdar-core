-module(listener_impl).
-include("playdartcp.hrl").
-include("playdar.hrl").
%% API
-export([start_link/1]).

start_link(Port) -> spawn_link(fun()->
                                {ok, Sock} = gen_tcp:listen(Port, ?TCP_OPTS_SERVER),
                                do_accept(Sock)
                               end).
    
do_accept(LSock) ->
    case gen_tcp:accept(LSock) of
        
        {ok, Sock} ->
            Pid = spawn(fun()->handle(Sock)end),
            gen_tcp:controlling_process(Sock, Pid),
            do_accept(LSock);
        
        {error, Err} ->
            ?LOG(warning, "Couldn't accept incoming connection: ~p", [Err]),
            throw(playdartcp_listen_accept_error)
    end.

% Is this a control connection, or a stream/filetransfer?
% once we know, delegate socket to correct type of process.
handle(Sock) ->
    % rcv just 1 msg: the connection mode
    case gen_tcp:recv(Sock, 0, 3000) of
        {ok, Packet} ->
            case (catch ?B2T(Packet)) of
                {conntype, control} ->
                    ?LOG(info, "handle accept, control connection",[]),
                    {ok, Pid} = playdartcp_conn:start(Sock, in),
                    gen_tcp:controlling_process(Sock, Pid);
                
                {conntype, stream} ->
                    ?LOG(info, "handle accept, stream connection",[]),
                    {ok, Pid} = playdartcp_stream:start(Sock),
                    gen_tcp:controlling_process(Sock, Pid);
                
                Er ->
                    ?LOG(warning, "Unknown socket mode, aborting connection: ~p", [Er]),
                    gen_tcp:close(Sock)
            end;
        
        {error, timeout} ->
            ?LOG(info, "Timeout waiting for connection mode packet", []),
            gen_tcp:close(Sock);

        {error, X} ->
            ?LOG(warning, "Connection failed: ~p", [X]),
            gen_tcp:close(Sock)
    end.