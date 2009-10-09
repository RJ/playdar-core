-module(p2p_conn).
-include("playdar.hrl").
-behaviour(gen_server).

%% API
-export([start/2, send_msg/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {sock, authed, name, props, inout, seenqids}).

%% API
start(Sock, InOut) ->
    gen_server:start(?MODULE, [Sock, InOut], []).

send_msg(Pid, Msg) ->
    gen_server:call(Pid, ?MODULE, {send_msg, Msg}). 

%% gen_server callbacks
init([Sock, InOut]) ->
    % kill the connection if not authed after 10 seconds:
    timer:send_after(10000, self(), auth_timeout),
    % if we are initiating the connection, ie it's outbound, send our auth:
    case InOut of
        out ->
            gen_tcp:send(Sock, {auth, ?CONFVAL(name, "unknown"), []});
        in ->
            noop
    end,
    ok = inet:setopts(Sock, [{active, once}]), 
    SQ = ets:new(seenqids,[]),
    {ok, #state{sock=Sock, authed=false, inout=InOut, seenqids=SQ}}.


handle_call({send_msg, Msg}, _From, State) ->
    gen_tcp:send(State#state.sock, Msg),
    {reply, ok, State};
    
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(auth_timeout, State = #state{authed = true})  -> {noreply, State};
handle_info(auth_timeout, State = #state{authed = false}) -> {stop, auth_timeout, State};

%% Incoming auth/peer ID:
handle_info({tcp, Sock, {auth, Name, Props}}, State = #state{sock=Sock, authed=false}) when is_list(Name), is_list(Props) -> 
    ?LOG(info, "new p2p connection authed as ~s, props: ~p", [Name, Props]),
    % send our details to them if the connection was inbound, ie we didnt send yet:
    case State#state.inout of
        in ->
            gen_tcp:send(Sock, {auth, ?CONFVAL(name, "unknown"), []});
        out ->
            noop
    end,
    p2p_router:register_connection(self(), Name),
    ok = inet:setopts(Sock, [{active, once}]),
    {noreply, State#state{authed=true, name=Name, props=Props}};

%% Incoming query:
handle_info({tcp, Sock, {rq, Rq={struct, L}}}, State = #state{sock=Sock, authed=true}) when is_list(L) ->
    ?LOG(info, "Got a RQ: ~p", [L]),
    Qid = proplists:get_value(<<"qid">>, L),
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
handle_info({tcp, Sock, {result, {struct, L}}}, State = #state{sock=Sock, authed=true}) when is_list(L) ->
    Qid = proplists:get_value(<<"qid">>, L),
    case resolver:qid2pid(Qid) of
        Qpid when is_pid(Qpid) ->
            {struct, L2} = proplists:get_value(<<"result">>, L),
            Sid = proplists:get_value(<<"sid">>, L2),
            Url = io_lib:format("p2p://~s ~s", [State#state.name, Sid]),
            qry:add_result(Qpid, {struct, 
                                  [{<<"url">>, list_to_binary(Url)}|L2]}),
            {noreply, State};
        _ ->
            {noreply, State}
    end;

handle_info({tcp, Sock, Data}, State = #state{sock=Sock, authed=true}) ->
    io:format("GOT ~p~n", [Data]),
    gen_tcp:send(State#state.sock, io_lib:format("I got ~p~n",[Data])),
    ok = inet:setopts(Sock, [{active, once}]), 
    {noreply, State};

handle_info({tcp_error, Sock, Reason}, State = #state{sock=Sock}) ->
    io:format("CLOSED ~p~n",[Reason]),
    {stop, closed_socket_err, State};

handle_info({tcp_closed, Sock}, State = #state{sock=Sock}) ->
    io:format("CLOSED OK~n"),
    {stop, closed_socket_ok, State};

handle_info({tcp, _Data, Sock}, State = #state{sock=Sock, authed=false}) ->
    ?LOG(warning, "Data received but not AUTHed, disconnecting!", []),
    {stop, not_authed, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

