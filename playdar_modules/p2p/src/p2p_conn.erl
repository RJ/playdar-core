-module(p2p_conn).
-include("playdar.hrl").
-behaviour(gen_server).
-define(T2B(T), term_to_binary(T)).
-define(B2T(T), binary_to_term(T)).
%% API
-export([start/2, send_msg/2, request_sid/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {sock, authed, name, props, inout, seenqids, transfers}).

%% API
start(Sock, InOut) ->
    gen_server:start(?MODULE, [Sock, InOut], []).

send_msg(Pid, Msg) when is_tuple(Msg) ->
    gen_server:call(Pid, {send_msg, Msg}). 

request_sid(Pid, Sid, RecPid, Ref) ->
	gen_server:cast(Pid, {request_sid, Sid, RecPid, Ref}).

%% gen_server callbacks
init([Sock, InOut]) ->
    ?LOG(info, "p2p_conn:init, inout:~w", [InOut]),
    % kill the connection if not authed after 10 seconds:
    timer:send_after(15000, self(), auth_timeout),
    % if we are initiating the connection, ie it's outbound, send our auth:
    case InOut of
        out ->
            gen_tcp:send(Sock, ?T2B({auth, ?CONFVAL(name, "unknown"), []}));
        in ->
            noop
    end,
    ok = inet:setopts(Sock, [{active, once}]), 
    SQ = ets:new(seenqids,[]),
	Transfers = ets:new(transfers,[]),
    {ok, #state{sock=Sock, authed=false, inout=InOut, seenqids=SQ, transfers=Transfers}}.


handle_call({send_msg, Msg}, _From, State) when is_tuple(Msg) ->
    ?LOG(info, "send_msg: ~p", [Msg]),
    gen_tcp:send(State#state.sock, ?T2B(Msg)),
    {reply, ok, State}.

handle_cast({request_sid, Sid, RecPid, Ref}, State) ->
	ets:insert(State#state.transfers, {{Sid, Ref},RecPid}),
	gen_tcp:send(State#state.sock, ?T2B({request_sid, Ref, Sid})),
	{noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(auth_timeout, State = #state{authed = true})  -> {noreply, State};
handle_info(auth_timeout, State = #state{authed = false}) -> {stop, normal, State};

handle_info({tcp, Sock, Packet}, State = #state{sock=Sock}) ->
    Term = ?B2T(Packet),
    %?LOG(info, "INCOMING: ~p", [Term]),
    Rep = handle_packet(Term, State),
    ok = inet:setopts(Sock, [{active, once}]),
    Rep;

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
                    gen_tcp:send(Sock, ?T2B({auth, ?CONFVAL(name, "unknown"), []}));
                out ->
                    noop
            end,
            {noreply, State#state{authed=true, name=Name, props=Props}};
        
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
            resolver:dispatch(Rq, Qid, Cbs),
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
			Msg = {sid_response, Ref, Sid, {error, 404}},
			gen_tcp:send(State#state.sock, ?T2B(Msg)),
			{noreply, State};
		Qpid ->
			{struct, L} = qry:result(Qpid, Sid),
			Url = proplists:get_value(<<"url">>, L),
			case Url of
				undefined ->
					Msg = {sid_response, Ref, Sid, {error, 5030}},
					gen_tcp:send(State#state.sock, ?T2B(Msg)),
					{noreply, State};
				_ ->
					spawn(fun()->stream_result(Ref, Sid, State#state.sock, Qpid)end),
					{noreply, State}			
			end
	end;

handle_packet({sid_response, Ref, Sid, M}, State = #state{authed=true}) ->
	case ets:lookup(State#state.transfers, {Sid, Ref}) of
		[{{Sid,Ref},Pid}] ->
			case M of
				{headers, H}-> Pid ! {Ref, headers, H};
				{data, D} 	-> Pid ! {Ref, data, D};
				{error, E} 	-> Pid ! {Ref, error, E};
				eof			-> Pid ! {Ref, eof}
			end,
			{noreply, State};
		_ ->
			?LOG(info, "sid_response arrived, but no registered handler",[]),
			{noreply, State}
	end;

handle_packet(Data, State) ->
    io:format("GOT UNKNOWN, state: ~p ~p~n", [State, Data]),
    {noreply, State}.


stream_result(Ref, Sid, Sock, Qpid) ->
	A = qry:result(Qpid, Sid),
	case playdar_reader_registry:get_streamer(A, self(), Ref) of
		undefined ->
			Msg = {sid_response, Ref, Sid, {error, 5031}},
			gen_tcp:send(Sock, ?T2B(Msg)),
			ok;
		Sfun ->			
			% we trap exits, so if the streaming fun crashes we can catch it
			% and fwd an error message to the recipient
			process_flag(trap_exit, true),
			Sfun(),
			receive
				{Ref, headers, Headers} ->
					M = {sid_response, Ref, Sid, {headers, Headers}},
					gen_tcp:send(Sock, ?T2B(M)),
					stream_result_body(Ref, Sid, Sock);
				
				{'EXIT', _Pid, _Reason} ->
					Msg = {sid_response, Ref, Sid, {error, 999}},
					gen_tcp:send(Sock, ?T2B(Msg)),
					ok
			
				after 10000 ->
					M = {sid_response, Ref, Sid, {error, headers_timeout}},
					gen_tcp:send(Sock, ?T2B(M)),
					ok
			end
	end.
    
stream_result_body(Ref, Sid, Sock) ->
    receive
		{'EXIT', _Pid, _Reason} ->
			Msg = {sid_response, Ref, Sid, {error, 999}},
			gen_tcp:send(Sock, ?T2B(Msg)),
			ok;
		
        {Ref, data, Data} ->
			Msg = {sid_response, Ref, Sid, {data, Data}},
			gen_tcp:send(Sock, ?T2B(Msg)),
            stream_result_body(Ref, Sid, Sock);
        
        {Ref, error, _Reason} ->
            Msg = {sid_response, Ref, Sid, {error, -1}},
			gen_tcp:send(Sock, ?T2B(Msg)),
			ok;
        
        {Ref, eof} ->
			Msg = {sid_response, Ref, Sid, eof},
			gen_tcp:send(Sock, ?T2B(Msg)),
            ok
    
    after 10000 ->
			Msg = {sid_response, Ref, Sid, {error, timeout}},
			gen_tcp:send(Sock, ?T2B(Msg)),
			ok
    end.