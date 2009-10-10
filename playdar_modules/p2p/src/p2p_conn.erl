-module(p2p_conn).
-include("playdar.hrl").
-behaviour(gen_server).
-define(T2B(T), term_to_binary(T)).
-define(B2T(T), binary_to_term(T)).
%% API
-export([start/2, send_msg/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {sock, authed, name, props, inout, seenqids}).

%% API
start(Sock, InOut) ->
    gen_server:start(?MODULE, [Sock, InOut], []).

send_msg(Pid, Msg) when is_tuple(Msg) ->
    gen_server:call(Pid, ?MODULE, {send_msg, Msg}). 

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
    {ok, #state{sock=Sock, authed=false, inout=InOut, seenqids=SQ}}.


handle_call({send_msg, Msg}, _From, State) when is_tuple(Msg) ->
    gen_tcp:send(State#state.sock, ?T2B(Msg)),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(auth_timeout, State = #state{authed = true})  -> {noreply, State};
handle_info(auth_timeout, State = #state{authed = false}) -> {stop, normal, State};

handle_info({tcp, Sock, Packet}, State = #state{sock=Sock}) ->
    Term = ?B2T(Packet),
    ?LOG(info, "INCOMING: ~p", [Term]),
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
handle_packet({rq, Rq={struct, L}}, State = #state{authed=true}) when is_list(L) ->
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
handle_packet({result, {struct, L}}, State = #state{authed=true}) when is_list(L) ->
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

handle_packet(Data, State = #state{authed=false}) ->
    io:format("GOT(unauthed) ~p~n", [Data]),
    {noreply, State};

handle_packet(Data, State = #state{authed=true}) ->
    io:format("GOT(authed) ~p~n", [Data]),
    {noreply, State}.