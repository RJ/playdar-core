-module(p2p_conn).
-include("playdar.hrl").
-behaviour(gen_server).
-define(T2B(T), term_to_binary(T)).
-define(B2T(T), binary_to_term(T)).
%% API
-export([start/2, send_msg/2, request_sid/4, stats/1, stats/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {sock, authed, name, props, inout, seenqids, transfers, conntime}).

%% API
start(Sock, InOut) ->
    gen_server:start(?MODULE, [Sock, InOut], []).

send_msg(Pid, Msg) when is_tuple(Msg) ->
    gen_server:call(Pid, {send_msg, Msg}). 

request_sid(Pid, Sid, RecPid, Ref) ->
	gen_server:cast(Pid, {request_sid, Sid, RecPid, Ref}).

stats(Pid) -> stats(Pid, undefined).
stats(Pid, Opts) -> gen_server:call(Pid, {stats,Opts}).

%% gen_server callbacks

init([Sock, InOut]) ->
    {ok, {SockAddr, SockPort}} = inet:peername(Sock),
    ?LOG(info, "p2p_conn:init, inout:~w Remote:~p:~p", [InOut,SockAddr,SockPort]),
    % kill the connection if not authed after 10 seconds:
    timer:send_after(15000, self(), auth_timeout),
    % if we are initiating the connection, ie it's outbound, send our auth:
    case InOut of
        out ->
            Msg = ?T2B({auth, ?CONFVAL(name, "unknown"), []}),
            % don't report b/w counters for auth msgs
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
	ets:insert(State#state.transfers, {{Sid, Ref},RecPid}),
    Msg = ?T2B({request_sid, Ref, Sid}),
	ok = gen_tcp:send(State#state.sock, Msg),    
	{noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(auth_timeout, State = #state{authed = true})  -> {noreply, State};
handle_info(auth_timeout, State = #state{authed = false}) -> {stop, normal, State};

% Incoming p2p packet:
handle_info({tcp, Sock, Packet}, State = #state{sock=Sock}) ->
    Term = ?B2T(Packet),
    case Term of
        {sid_response, _, Sid, {data, _}} ->
            ok;
            %?LOG(info, "INCOMING (~p) data packet for ~p", [State#state.name, Sid]);
        _ ->
            ?LOG(info, "INCOMING (~p)\n~p", [State#state.name, Term])
    end,
    {Reply, NewState} = handle_packet(Term, State),
    ok = inet:setopts(Sock, [{active, once}]),
    {Reply, NewState};

% Refwd query that originally arrived at this connection
% but only if not already solved!
handle_info({fwd_query, Qid, Qpid, Rq}, State = #state{authed=true}) ->
    case qry:solved(Qpid) of
        true ->
            ?LOG(info, "Not fwding query ~p already solved", [Qid]),
            {noreply, State};
        false ->
            ?LOG(info, "Fwding query ~p to peers", [Qid]),
            M = {rq, Qid, p2p_router:sanitize_msg(Rq)},
            % send to all except ourselves (we are the query origin)
            p2p_router:broadcast(M, self()),
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
    {stop, not_authed, State}.



terminate(_Reason, _State) ->
    ?LOG(info, "p2p connection terminated", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions


%% Incoming auth/peer ID:
handle_packet({auth, Name, Props}, State = #state{sock=Sock, authed=false}) when is_list(Name), is_list(Props) -> 
    ?LOG(info, "new p2p connection authed as ~s, props: ~p", [Name, Props]),
    case p2p_router:register_connection(self(), Name) of
        ok ->
            % send our details to them if the connection was inbound, ie we didnt send yet:
            case State#state.inout of
                in ->
                    DefName = "unknown-"++erlang:integer_to_list(random:uniform(9999999)), % HACK
                    M = ?T2B({auth, ?CONFVAL(name, DefName), []}),
                    ok = gen_tcp:send(Sock, M),
                    % dont report b/w counters for auth msgs
                    {noreply, State#state{authed=true, name=Name, props=Props}};
                out ->
                    {noreply, State#state{authed=true, name=Name, props=Props}}
            end;
        
        disconnect ->
            ?LOG(info, "Abandoning connection, duplicate name",[]),
            {stop, normal, State}
    end;

%% Incoming query:
handle_packet({rq, Qid, Rq={struct, L}}, State = #state{authed=true}) when is_list(L) ->
    ?LOG(info, "Got a RQ: ~p", [L]),
	p2p_router:seen_qid(Qid),
    % do nothing if we dispatched, or already received this qid
    case ets:lookup(State#state.seenqids, Qid) of
        [{Qid, true}] -> 
            {noreply, State};
        _ ->           
            ets:insert(State#state.seenqids, {Qid,true}),
            Cbs = [ fun(Ans)-> p2p_router:send_query_response(Ans, Qid, State#state.name) end ],
            Qpid = resolver:dispatch(Rq, Qid, Cbs),
            % Schedule this query to be sent to all other peers after a delay.
            % When the time is up, the query will be fwded ONLY IF the query is
            % still in an unsolved state.
            % This hardcoded delay is deliberate - it means cancellation msgs
            % can reach the search frontier immediately - cancellations msgs
            % are always fwded with no delay.
            timer:send_after(?CONFVAL({p2p, fwd_delay},500), self(), {fwd_query, Qid, Qpid, Rq}), 
            {noreply, State}
    end;

%% Incoming answer to a query:
handle_packet({result, Qid, {struct, L}}, State = #state{authed=true}) when is_list(L) ->
    case resolver:qid2pid(Qid) of
        Qpid when is_pid(Qpid) ->
            Sid = proplists:get_value(<<"sid">>, L),
            Url = io_lib:format("p2p://~s\t~s", [Sid, State#state.name]),
            qry:add_result(Qpid, {struct, 
                                  [{<<"url">>, list_to_binary(Url)}|L]}),
            {noreply, State};
        _ ->
            {noreply, State}
    end;

handle_packet({request_sid, Ref, Sid}, State = #state{authed=true}) ->
	case resolver:sid2pid(Sid) of
		undefined ->
            ?LOG(info,"sid not found: ~p", [Sid]),
			Msg = ?T2B({sid_response, Ref, Sid, {error, 404}}),
			ok = gen_tcp:send(State#state.sock, Msg),
			{noreply, State};
		Qpid ->
			{struct, L} = qry:result(Qpid, Sid),
			Url = proplists:get_value(<<"url">>, L),
            ?LOG(info, "Sid ~p has Url: ~p", [Sid, Url]), 
			case Url of
				undefined ->
                    ?LOG(info, "Error, this sid has no url: ~p", [Sid]),
					Msg = ?T2B({sid_response, Ref, Sid, {error, 5030}}),
					ok = gen_tcp:send(State#state.sock, Msg),
					{noreply, State};
				_ ->
                    ConnPid = self(),
					spawn(fun()->stream_result(Ref, Sid, State#state.sock, Qpid, ConnPid)end),
					{noreply, State}			
			end
	end;

handle_packet({sid_response, Ref, Sid, M}, State = #state{authed=true}) ->
	case ets:lookup(State#state.transfers, {Sid, Ref}) of
		[{{Sid,Ref},Pid}] ->
			case M of
				{headers, H}->
                    ?LOG(info, "headers arrived: ~p", [H]),
                    Pid ! {Ref, headers, H};
				{data, D} 	-> Pid ! {Ref, data, D};
				{error, E} 	-> 
                    ?LOG(warn, "Error for transfer: ~p",[E]),
                    Pid ! {Ref, error, E};
				eof			-> Pid ! {Ref, eof}
			end,
			{noreply, State};
		_ ->
			?LOG(info, "sid_response arrived, but no registered handler",[]),
            %TODO send cancel msg for this transfer
			{noreply, State}
	end;

handle_packet(Data, State) ->
    io:format("GOT UNKNOWN, state: ~p ~p~n", [State, Data]),
    {noreply, State}.


stream_result(Ref, Sid, Sock, Qpid, ConnPid) ->
	A = qry:result(Qpid, Sid),
	case playdar_reader_registry:get_streamer(A, self(), Ref) of
		undefined ->
			Msg = ?T2B({sid_response, Ref, Sid, {error, 5031}}),
			ok = gen_tcp:send(Sock, Msg),
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
					stream_result_body(Ref, Sid, Sock, ConnPid);
				
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
    
stream_result_body(Ref, Sid, Sock, ConnPid) ->
    receive
		{'EXIT', _Pid, Reason} when Reason /= normal ->
            ?LOG(error, "Streamer fun crashed whilst streaming body: ~p", [Reason]),
			Msg = ?T2B({sid_response, Ref, Sid, {error, 999}}),
			ok = gen_tcp:send(Sock, Msg),
			ok;
		
        {Ref, data, Data} ->
			Msg = ?T2B({sid_response, Ref, Sid, {data, Data}}),
			ok = gen_tcp:send(Sock, Msg),
            stream_result_body(Ref, Sid, Sock, ConnPid);
        
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