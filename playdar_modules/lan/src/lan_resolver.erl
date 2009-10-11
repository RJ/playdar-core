%% Uses UDP multicast to resolve against other instances of this code
%% running elsewhere on the LAN. Streams via HTTP.

-module(lan_resolver).

-include("playdar.hrl").
-behaviour(gen_server).
-behaviour(playdar_resolver).

%% API
-export([start_link/0, resolve/3, weight/1, targettime/1, name/1, send_response/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {sock, sockp, seenqids, broadcast, port}).

-define(BROADCAST, {239,255,0,1}). % default, can be changed in config
-define(PORT, 60210). % can be changed in config

%% API
start_link()            -> gen_server:start_link(?MODULE, [], []).
resolve(Pid, Q, Qpid)   -> gen_server:cast(Pid, {resolve, Q, Qpid}).
weight(_Pid)            -> 95.
targettime(_Pid)        -> 50.
name(_Pid)              -> "Lan".

%% Used as a callback when qry results arrive:
send_response(Pid, A, Qid, Ip, Port) -> 
    gen_server:cast(Pid, {send_response, A, Qid, Ip, Port}).

%% gen_server callbacks
init([]) ->
	{R1,R2,R3} = now(),
	random:seed(R1,R2,R3),
	SQ = ets:new(seenqids,[]),
	LAddr = {0,0,0,0},
    Port = ?CONFVAL({lan,port},?PORT),
    BC = ?CONFVAL({lan,broadcast},?BROADCAST),
    % Broadcast socket:
    {ok, Sock} = gen_udp:open(Port, [binary, 
                                     {reuseaddr, true},{ip, BC}, 
                                     {add_membership, {BC, LAddr}}]),
    % Normal socket, which direct replies are sent to:
    {ok, SockP}= gen_udp:open(Port, [binary, 
                                     {reuseaddr, true}, {ip, {0,0,0,0}}]),
    resolver:add_resolver(?MODULE, name(self()), weight(self()), targettime(self()), self()),
    {ok, #state{sock=Sock, 
                sockp=SockP,
                seenqids=SQ, 
                broadcast=BC,
                port=Port
               }}.

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
            ?LOG(info, "LAN dispatching query", []),
			ets:insert(State#state.seenqids, {Qid,true}),
			Msg = {struct, [ {<<"_msgtype">>, <<"rq">>},{<<"qid">>,Qid} | Parts ]},
			gen_udp:send(State#state.sock, State#state.broadcast, State#state.port, mochijson2:encode(Msg)),
			{noreply, State}
	end;
 
handle_cast({send_response, A, Qid, Ip, Port}, State) ->
    ?LOG(debug, "sending response for qry ~s to ~w", [Qid, Ip]),
    {struct, Parts} = A,
    Hostname = ?CONFVAL(name, ""),
    Msg = {struct, [    {<<"_msgtype">>, <<"result">>},
                        {<<"qid">>, Qid},
                        {<<"result">>, 
                         {struct, [
                                   {<<"source">>, list_to_binary(Hostname)},
                                   {<<"port">>, ?CONFVAL({web, port}, 60210)} |
                                   proplists:delete(<<"url">>,
                                    proplists:delete(<<"source">>,Parts))
                         ]}}
                    ]},
    gen_udp:send(State#state.sock, Ip, Port, mochijson2:encode(Msg)),
    {noreply, State}.

% could me a msg from broadcast socket, or private one (meaning a direct reply):
handle_info({udp, _Sock, {A,B,C,D}=Ip, _InPortNo, Packet}, State) ->
    ?LOG(debug, "received msg: ~s", [Packet]),
    {struct, L} = mochijson2:decode(Packet),
    case proplists:get_value(<<"_msgtype">>,L) of
        <<"result">> ->
            Qid = proplists:get_value(<<"qid">>, L),
            case resolver:qid2pid(Qid) of
                Qpid when is_pid(Qpid) ->
                    {struct, L2} = proplists:get_value(<<"result">>, L),
                    HttpPort = proplists:get_value(<<"port">>, L, 60210),
                    Sid = proplists:get_value(<<"sid">>, L2),
                    Url = io_lib:format("http://~w.~w.~w.~w:~w/sid/~s",
                                        [A,B,C,D,HttpPort,Sid]),
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
                    This = self(),
                    Cbs = [ fun(Ans)-> ?MODULE:send_response(This, Ans, Qid, Ip, State#state.port) end ],
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

