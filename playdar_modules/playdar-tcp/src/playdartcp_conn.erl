% this process owns a tcp socket connected to a remote playdar instance
-module(playdartcp_conn).
-include("playdar.hrl").
-include("playdartcp.hrl").
-behaviour(gen_server).
%% API
-export([start/2, start/3, send_msg/2, request_sid/4, stats/1, stats/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {sock,       % pid: gen_tcp pid
                authed,     % bool: connection authed and ready?
                name,       % string: name of remote peer
                props,      % proplist of remote peer settings
                inout,      % atom: in|out - direction connection was made in
                seenqids,   % ets of QIDs we've seen (to ignore dupes)
                transfers,  % ets of SID/Ref for active transfers
                conntime,   % localtime() when connection established
                weshare,    % bool: are we sharing content down this link?
                theyshare   % bool: is the remote peer sharing with us?
                }).

%% API
start(Sock, InOut)          ->  start(Sock, InOut, ?CONFVAL({playdartcp, share}, false)).
start(Sock, InOut, Share)   ->  gen_server:start(?MODULE, [Sock, InOut, Share], []).

send_msg(Pid, Msg) when is_tuple(Msg) -> gen_server:call(Pid, {send_msg, Msg}). 

%initiate stream:
request_sid(Pid, Sid, RecPid, Ref)    -> gen_server:cast(Pid, {request_sid, Sid, RecPid, Ref}).

% stats on connection bandwidth etc:
stats(Pid)       -> stats(Pid, undefined).
stats(Pid, Opts) -> gen_server:call(Pid, {stats,Opts}).



%% gen_server callbacks

init([Sock, InOut, Share]) ->
    {ok, {SockAddr, SockPort}} = inet:peername(Sock),
    ?LOG(info, "playdartcp_conn:init, inout:~w Remote:~p:~p", [InOut,SockAddr,SockPort]),
    % kill the connection if not authed fast enough:
    timer:send_after(15000, self(), auth_timeout),
    % if we are initiating the connection, ie it's outbound, send our auth:
    case InOut of
        out ->
            % Needs more DRY - props also created in response to auth packet below
            PublicPort = ?CONFVAL({playdartcp,port},60211),
            Props = [{share, Share}, {public_port, PublicPort}],
            Msg = ?T2B({auth, ?CONFVAL(name, "unknown"), Props}),
            ok = gen_tcp:send(Sock, Msg);
        in ->
            noop
    end,
    ok = inet:setopts(Sock, [{active, once}]),
    SQ = ets:new(seenqids,[]),
	Transfers = ets:new(transfers,[]),
    {ok, #state{    sock=Sock, 
                    authed=false, 
                    inout=InOut, 
                    weshare=Share,
                    theyshare=false, % they'll tell us when they auth.
                    seenqids=SQ, 
                    transfers=Transfers,
                    conntime=erlang:localtime()}}.


handle_call({stats,Opts}, _From, State) ->
    case Opts of
        O when is_list(O) ->
            {reply, {State#state.conntime, inet:getstat(State#state.sock, O)}, State};
        _ ->
            {reply, {State#state.conntime, inet:getstat(State#state.sock)}, State}
    end;

handle_call({send_msg, Msg}, _From, State) when is_tuple(Msg) ->
    ?LOG(info, "send_msg: ~p", [Msg]),
    MsgB = ?T2B(Msg),
    ok = gen_tcp:send(State#state.sock, MsgB),
    {reply, ok, State}.

handle_cast({request_sid, Sid, RecPid, Ref}, State) ->
    playdartcp_router:register_transfer({Ref,Sid}, RecPid),
    Msg = ?T2B({request_stream, Ref, Sid}),
	ok = gen_tcp:send(State#state.sock, Msg),    
	{noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(auth_timeout, State = #state{authed = true})  -> {noreply, State};
handle_info(auth_timeout, State = #state{authed = false}) -> 
    ?LOG(warning, "Dropping connection, failed to auth in time.", []),
    {stop, normal, State};

% Incoming packet:
handle_info({tcp, Sock, Packet}, State = #state{sock=Sock}) ->
    Term = ?B2T(Packet),
    ?LOG(info, "INCOMING (~p)\n~p", [State#state.name, Term]),
    {Reply, NewState} = handle_packet(Term, State),
    ok = inet:setopts(Sock, [{active, once}]),
    {Reply, NewState};


% Refwd query that originally arrived at this connection
% but only if not already solved!
handle_info({fwd_query, #qry{qid=Qid, obj=Q}}, State = #state{authed=true}) ->
    case playdar_resolver:solved(Qid) of
        true ->
            ?LOG(info, "Not fwding query ~p already solved", [Qid]),
            {noreply, State};
        false ->
            ?LOG(info, "Fwding query ~p to peers", [Qid]),
            M = {rq, Qid, playdartcp_router:sanitize_msg(Q)},
            % send to all except ourselves (we are the query origin)
            playdartcp_router:broadcast(M, self()),
            {noreply, State}
    end;

handle_info({tcp_error, Sock, Reason}, State = #state{sock=Sock}) ->
    io:format("CLOSED ~p~n",[Reason]),
    {stop, normal, State};

handle_info({tcp_closed, Sock}, State = #state{sock=Sock}) ->
    io:format("CLOSED OK~n"),
    {stop, normal, State};

handle_info({tcp, _Data, Sock}, State = #state{sock=Sock, authed=false}) ->
    ?LOG(warning, "Data received but not AUTHed, disconnecting!", []),
    {stop, not_authed, State};

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) ->
    ?LOG(info, "playdartcp connection terminated", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

%% Incoming auth/peer ID:
handle_packet({auth, Name, Props}, State = #state{sock=Sock, authed=false}) when is_list(Name), is_list(Props) -> 
    ?LOG(info, "new playdartcp connection authed as ~s, props: ~p", [Name, Props]),
    Sharing = {State#state.weshare, proplists:get_value(share, Props, false)==true},
    case playdartcp_router:register_connection(self(), Name, Sharing) of 
        ok ->
            TheyShare = proplists:get_value(share, Props) == true,
            % send our details to them if the connection was inbound, ie we didnt send yet:
            case State#state.inout of
                in ->
                    DefName = "unknown-"++erlang:integer_to_list(random:uniform(9999999)), % HACK
                    % tell them if we are sharing content with them:
                    PublicPort = ?CONFVAL({playdartcp,port},60211),
                    OurProps = [{share, State#state.weshare}, {public_port, PublicPort}],
                    M = ?T2B({auth, ?CONFVAL(name, DefName), OurProps}),
                    ok = gen_tcp:send(Sock, M),
                    {noreply, State#state{authed=true, name=Name, props=Props, theyshare=TheyShare}};
                out ->
                    {noreply, State#state{authed=true, name=Name, props=Props, theyshare=TheyShare}}
            end;
        
        disconnect ->
            ?LOG(info, "Abandoning connection, duplicate name",[]),
            {stop, normal, State}
    end;

%% Incoming query:
handle_packet({rq, Qid, {struct, L}}, State = #state{authed=true, weshare=true}) when is_list(L) ->
    ?LOG(info, "Got a RQ: ~p", [L]),
	playdartcp_router:seen_qid(Qid),
    % do nothing if we dispatched, or already received this qid
    case ets:lookup(State#state.seenqids, Qid) of
        [{Qid, true}] -> 
            {noreply, State};
        [] ->           
            ets:insert(State#state.seenqids, {Qid,true}),
            Cbs = [ fun(Ans)-> playdartcp_router:send_query_response(Ans, Qid, State#state.name) end ],
			Qry = #qry{obj = {struct, L}, qid = Qid, local = false},
            playdar_resolver:dispatch(Qry, Cbs),
            % Schedule this query to be sent to all other peers after a delay.
            % When the time is up, the query will be fwded ONLY IF the query is
            % still in an unsolved state.
            % This hardcoded delay is deliberate - it means cancellation msgs
            % can reach the search frontier immediately - cancellations msgs
            % are always fwded with no delay.
            % TODO implement cancellation msgs :)
            case ?CONFVAL({playdartcp, fwd}, false) of
                true ->
                    Delay = ?CONFVAL({playdartcp, fwd_delay},500),
                    timer:send_after(Delay, self(), {fwd_query, Qry}), 
                    {noreply, State};
                _ ->
                    {noreply, State}
            end
    end;

%% Incoming answer to a query:
handle_packet({result, Qid, {struct, L}}, State = #state{authed=true}) when is_list(L) ->
	Sid = proplists:get_value(<<"sid">>, L),
	Url = io_lib:format("playdartcp://~s\t~s", [Sid, State#state.name]),
	playdar_resolver:add_results(Qid, {struct, 
							   [{<<"url">>, list_to_binary(Url)}|L]}),
	{noreply, State};

% request for us to send a file
handle_packet({request_stream, Ref, Sid}, State = #state{authed=true, weshare=true}) ->
	case playdar_resolver:result(Sid) of
		undefined ->
            ?LOG(info,"sid not found: ~p", [Sid]),
			Msg = ?T2B({sid_response, Ref, Sid, {error, 404}}),
			ok = gen_tcp:send(State#state.sock, Msg),
			{noreply, State};
		{struct, L} when is_list(L) ->
			Url = proplists:get_value(<<"url">>, L),
            ?LOG(info, "Sid ~p has Url: ~p", [Sid, Url]), 
			case Url of
				undefined ->
                    ?LOG(info, "Error, this sid has no url: ~p", [Sid]),
					Msg = ?T2B({sid_response, Ref, Sid, {error, 5030}}),
					ok = gen_tcp:send(State#state.sock, Msg),
					{noreply, State};
				_ ->
                    handle_stream_request(send, Ref, Sid, State),                          
					{noreply, State}			
			end
	end;

% We asked for a file, which is ok, but we need to initiate the connection
handle_packet({request_stream_ready, Ref, Sid}, State = #state{authed=true}) ->
    ?LOG(info, "We are prompted to initiate a connection for ~s", [Sid]),
    handle_stream_request(rcv, Ref, Sid, State),
    {noreply, State};    

% the only kind of sid_response possible here is an error.
% all headers, data, etc send in the stream tcp connection
handle_packet({sid_response, Ref, Sid, M}, State = #state{authed=true}) ->
    case M of
        {error, Code} ->
            case playdartcp_router:consume_transfer({Ref,Sid}) of
                undefined -> 
                    ?LOG(info, "sid_response error received for unknown transfer", []),
                    {noreply, State};
                P when is_pid(P) ->
                    P ! {Ref, error, {inline_error_maybe_legacy, Code}},
                    {noreply, State};
                _ ->
                    ?LOG(info, "sid_response error received in wrong place", []),
                    {noreply, State}
            end;            
        _ ->
            ?LOG(warn, "Unexpected sid_response packet, disconnecting", []),
            gen_tcp:close(State#state.sock),
            {stop, normal}
    end;

handle_packet(Data, State) ->
    io:format("GOT UNKNOWN, state: ~p ~p~n", [State, Data]),
    {noreply, State}.


% Handle stream creation if a peer requests a stream from us
% Direction: send/rcv, depending if we are asking-for or offering the file
handle_stream_request(Direction, Ref, Sid, State = #state{inout=out}) ->
    {ok, {Address, _SockPort}} = inet:peername(State#state.sock),
    Port = proplists:get_value(public_port, State#state.props),
    ?LOG(info, "handle_stream_request [~p] -> ~p:~p", [Direction, Address, Port]),
    case gen_tcp:connect(Address, Port, ?TCP_OPTS, 5000) of
            {ok, Sock} ->
                ok = gen_tcp:send(Sock, ?T2B({conntype, stream})),
                case Direction of
                    send ->
                        playdartcp_router:register_transfer({Ref,Sid}, Address), % only send to the correct peer IP
                        ?LOG(info, "Sending {sending} header", []),
                        gen_tcp:send(Sock, ?T2B({sending, Ref, Sid})),
                        {ok, Pid} = playdartcp_stream:start(Sock, send),
                        gen_tcp:controlling_process(Sock, Pid);
                    rcv ->
                        ?LOG(info, "Sending {requesting} header", []),
                        gen_tcp:send(Sock, ?T2B({requesting, Ref, Sid})),
                        {ok, Pid} = playdartcp_stream:start(Sock, recv),
                        gen_tcp:controlling_process(Sock, Pid)
                end,
                ?LOG(info, "Created stream process(~p) for ~s to ~p:~p", [Pid, Sid, Address, Port]),
                ok;      
            {error, timeout} ->
                ?LOG(warn, "Failing to connect to ~p:~p for streaming", [Address,Port]),
                % TODO send an error back down the control channel
                error;
            {error, Reason} ->
                ?LOG(warn, "Failed to create stream process to ~p:~p Reason: ~p", [Address,Port,Reason]),
                error
    end;

handle_stream_request(send, Ref, Sid, State = #state{inout=in}) ->
    % tell them we're ready, and they should connect to us
    {ok, {Address, _SockPort}} = inet:peername(State#state.sock),
    playdartcp_router:register_transfer({Ref,Sid}, Address), % only send to the correct peer IP
    Msg = ?T2B({request_stream_ready, Ref, Sid}),
    ok = gen_tcp:send(State#state.sock, Msg).




