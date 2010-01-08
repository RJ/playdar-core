% manages tcp connection used for transfering files
% needs DRYing out a bit and tidying up once things settle down
-module(playdartcp_stream).
-behaviour(gen_server).
-include("playdar.hrl").
-include("playdartcp.hrl").

%% API
-export([start/1, start/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {sock, mode, ip, current, ref, sid, pid}).

start(Sock) -> start(Sock, unknown).

start(Sock, Mode) ->
    gen_server:start_link(?MODULE, [Sock ,Mode], []).

%%

init([Sock, Mode]) ->
    process_flag(trap_exit, true),
    {ok, {RemoteIp, _SockPort}} = inet:peername(Sock),
    ok = inet:setopts(Sock, [{active, once}]),
    {ok, #state{sock=Sock, mode=Mode, ip=RemoteIp, current=setup}}.
    

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

% Incoming packet:
handle_info({tcp, Sock, Packet}, State = #state{sock=Sock}) ->
    Term = ?B2T(Packet),
    Ret = handle_packet(Term, State),
    inet:setopts(Sock, [{active, once}]),
    Ret;

handle_info({tcp_error, Sock, Reason}, State = #state{current=receive_stream,sock=Sock}) ->
    State#state.pid ! {State#state.ref, error, Reason},
    gen_tcp:close(Sock),
    {stop, normal, State};

handle_info({tcp_closed, Sock}, State = #state{current=receive_stream,sock=Sock}) ->
    State#state.pid ! {State#state.ref, eof},
    {stop, normal, State};

handle_info({'EXIT', Pid, Reason}, State) ->
    ?LOG(warn, "Pid ~p exited: ~p", [Pid, Reason]),
    {noreply, State};

handle_info(Info, State) ->
    ?LOG(warn, "UNHANDLED: ~p", [Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    ?LOG(info, "TERMINATED ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% setup: 

handle_packet({sending, Ref, Sid}, State = #state{current=setup, mode=Mode, sock=Sock}) ->
    case playdartcp_router:consume_transfer({Ref,Sid}) of
        Pid when is_pid(Pid) ->
            case Mode of
                unknown ->
                    ok = gen_tcp:send(Sock, ?T2B({requesting, Ref, Sid}));
                _ -> nothing
            end,
            ?LOG(info, "current -> receive_stream", []),
            playdartcp_router:stream_started(Pid, Sid),
            {noreply, State#state{current=receive_stream,ref=Ref, sid=Sid, pid=Pid}};
                    
        unknown ->
            ?LOG(warn, "Invalid transfer key", []),
            gen_tcp:close(Sock),
            {stop, normal, State}
    end;

handle_packet({requesting, Ref, Sid}, State = #state{current=setup, mode=Mode, sock=Sock}) ->
    case playdartcp_router:consume_transfer({Ref,Sid}) of
        Ip when Ip == State#state.ip -> % should be Ip of client
            case Mode of 
                unknown ->
                    ok = gen_tcp:send(Sock, ?T2B({sending, Ref, Sid}));
                _ ->
                    nothing
            end,
            A = playdar_resolver:result(Sid),
            case playdar_reader_registry:get_streamer(A, self(), Ref) of
                undefined ->
                    ?LOG(warn, "No streamer available, aborting", []),
                    Msg = ?T2B({sid_response, Ref, Sid, {error, 5031}}),
                    gen_tcp:send(Sock, Msg),
                    gen_tcp:close(Sock),
                    {stop, normal, State};
                Sfun ->         
                    Sfun(),
                    {noreply, State#state{current=send_stream, ref=Ref, sid=Sid}}                    
            end;

        Else ->
            ?LOG(warn, "Not sending, request invalid Key: ~p, Transfertoken: ~p", [{Ref,Sid}, Else]),
            gen_tcp:close(Sock),
            {stop, normal, State}
    end;

handle_packet(_, State = #state{current=setup, sock=Sock}) ->
    ?LOG(warn, "Unhandled first packet in streamconn", []),
    gen_tcp:close(Sock),
    {stop, normal, State};

%% receive_stream:

handle_packet({sid_response, Ref, Sid, M}, 
              State = #state{current=receive_stream,pid=Pid,ref=Ref,sid=Sid,sock=Sock}) ->
    case M of
        {headers, H} ->
            ?LOG(info, "headers arrived: ~p", [H]),
            Pid ! {Ref, headers, H},
            {noreply, State};
        {data, D} -> 
            Pid ! {Ref, data, D},
            {noreply, State};
        {error, E} -> 
            ?LOG(warn, "Error for transfer: ~p",[E]),
            Pid ! {Ref, error, E},
            gen_tcp:close(Sock),
            {stop, normal, State};
        eof ->
            ?LOG(warn, "EOF for transfer: ~p",[Sid]),
            Pid ! {Ref, eof},
            gen_tcp:close(Sock),
            {stop, normal, State}
    end;

handle_packet(Wtf, State = #state{current=receive_stream,sock=Sock}) ->
    ?LOG(info, "Unhandled packet in receive_stream: ~p", [Wtf]),
    gen_tcp:close(Sock),
    {stop, normal, State};

% send_stream:

handle_packet({Ref, headers, Headers}, 
              State=#state{current=send_stream, sock=Sock, ref=Ref, sid=Sid}) ->
    M = ?T2B({sid_response, Ref, Sid, {headers, Headers}}),
    ok = gen_tcp:send(Sock, M),
    {noreply, State#state{current=send_stream_body}};

handle_packet({Ref, _, _} = Packet, 
              State=#state{current=send_stream_body, sock=Sock, ref=Ref, sid=Sid}) ->
    case Packet of 
        {Ref, data, Data} ->
            Msg = ?T2B({sid_response, Ref, Sid, {data, Data}}),
            ok = gen_tcp:send(Sock, Msg),
            {noreply, State};
        
        {Ref, error, Reason} ->
            Msg = ?T2B({sid_response, Ref, Sid, {error, Reason}}),
            ok = gen_tcp:send(Sock, Msg),
            gen_tcp:close(Sock),
            {stop, normal, State};
        
        {Ref, eof} ->
            Msg = ?T2B({sid_response, Ref, Sid, eof}),
            ok = gen_tcp:send(Sock, Msg),
            gen_tcp:close(Sock),
            {stop, normal, State}
    end.
