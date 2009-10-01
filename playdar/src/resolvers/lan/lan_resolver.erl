%% Uses UDP multicast to resolve against other instances of this code
%% running elsewhere on the LAN. Streams via HTTP.

-module(lan_resolver).

-behaviour(gen_server).
-behaviour(playdar_resolver).

%% API
-export([start_link/0, resolve/2, weight/0, targettime/0, send_response/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {sock, seenqids}).
-define(BROADCAST, {239,255,0,1}).

%% API
start_link()            -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
resolve(Q, Qpid)        -> gen_server:cast(?MODULE, {resolve, Q, Qpid}).
weight()                -> 95.
targettime()            -> 50.

%% Used as a callback when qry results arrive:
send_response(A, Qid, Ip, Port) -> 
    gen_server:cast(?MODULE, {send_response, A, Qid, Ip, Port}).

%% gen_server callbacks
init([]) ->
	{R1,R2,R3} = now(),
	random:seed(R1,R2,R3),
	SQ = ets:new(seenqids,[]),
	LAddr = {0,0,0,0},
    {ok, Sock} = gen_udp:open(60210, [binary, 
                                     {reuseaddr, true},{ip, ?BROADCAST}, 
                                     {add_membership, {?BROADCAST, LAddr}}]),
    {ok, #state{sock=Sock, seenqids=SQ}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({resolve, Q, Qpid}, State) ->
    {struct, Parts} = Q,
	Qid = qry:qid(Qpid),
	% Ignore if we've dealt with this qid already
	case ets:lookup(State#state.seenqids, Qid) of
		[{Qid, true}] -> 
			{noreply, State};
		_ ->
			ets:insert(State#state.seenqids, {Qid,true}),
			Msg = {struct, [ {<<"_msgtype">>, <<"rq">>},{<<"qid">>,Qid} | Parts ]},
			gen_udp:send(State#state.sock, ?BROADCAST, 60210, mochijson2:encode(Msg)),
			{noreply, State}
	end;
 
handle_cast({send_response, A, Qid, Ip, Port}, State) ->
    %io:format("lan sending response: ~w ~w ~w ~w~n", [A, Qid, Ip, Port]),
    {struct, Parts} = A,
    Msg = {struct, [    {<<"_msgtype">>, <<"result">>},
                        {<<"qid">>, Qid},
                        {<<"result">>, 
                         {struct, proplists:delete(<<"url">>,Parts)}}
                    ]},
    gen_udp:send(State#state.sock, Ip, Port, mochijson2:encode(Msg)),
    {noreply, State}.

handle_info({udp, _Socket, {A,B,C,D}=Ip, _InPortNo, Packet}, State) ->
    io:format("RCVD: ~p~n", [Packet]),
    {struct, L} = mochijson2:decode(Packet),
    case proplists:get_value(<<"_msgtype">>,L) of
        <<"result">> ->
            Qid = proplists:get_value(<<"qid">>, L),
            case resolver:qid2pid(Qid) of
                Qpid when is_pid(Qpid) ->
                    {struct, L2} = proplists:get_value(<<"result">>, L),
                    Sid = proplists:get_value(<<"sid">>, L2),
                    Url = io_lib:format("http://~w.~w.~w.~w:60210/sid/~s",
                                        [A,B,C,D,Sid]),
                    qry:add_result(Qpid, {struct, 
                                          [{<<"url">>, list_to_binary(Url)}|L2]}),
                    {noreply, State};
                _ ->
                    {noreply, State}
            end;
        
        <<"rq">> ->
            Qid = proplists:get_value(<<"qid">>, L),
            % do nothing if we dispatched, or already received this qid
            case ets:lookup(State#state.seenqids, Qid) of
                [{Qid, true}] -> {noreply, State};
                _ ->           
                    ets:insert(State#state.seenqids, {Qid,true}),
                    Cbs = [ fun(Ans)-> ?MODULE:send_response(Ans, Qid, Ip, 60210) end ],
                    resolver:dispatch({struct,L}, Qid, Cbs),
                    {noreply, State}
            end;
            
        _ -> {noreply, State}
    end.
    
terminate(_Reason, State) ->
    gen_udp:close(State#state.sock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

