%% Uses UDP multicast to resolve against other instances of this code
%% running elsewhere on the LAN. Streams via HTTP.
-module(lan_resolver).
-include("playdar.hrl").
-behaviour(gen_server).
-behaviour(playdar_resolver).

%% API
-export([start_link/0, resolve/2, weight/1, targettime/1, name/1, localonly/1]).

-export([send_response/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {sock, sockp, seenqids, broadcast, port}).

-define(BROADCAST, {239,255,0,1}). % default, can be changed in config
-define(PORT, ?DEFAULT_WEB_PORT).  % can be changed in config

%% API
start_link()            -> gen_server:start_link(?MODULE, [], []).
resolve(Pid, Qry)       -> gen_server:cast(Pid, {resolve, Qry}).
weight(_Pid)            -> 95.
targettime(_Pid)        -> 50.
name(_Pid)              -> "Lan".
localonly(_Pid)			-> false.

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
    playdar_resolver:add_resolver(?MODULE, self()),
    {ok, #state{sock=Sock, 
                sockp=SockP,
                seenqids=SQ, 
                broadcast=BC,
                port=Port
               }}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({resolve, #qry{obj = Q, qid = Qid}}, State) ->
    {struct, Parts} = Q,
	% Ignore if we've dealt with this qid already
	case ets:lookup(State#state.seenqids, Qid) of
		[{Qid, true}] -> 
			?LOG(info, "LAN ignoring dispatch request for duplicate qid: ~s",[Qid]),
			{noreply, State};
		_ ->
            ?LOG(info, "LAN dispatching query ~s", [Qid]),
			ets:insert(State#state.seenqids, {Qid,true}),
			Msg = {struct, [ {<<"_msgtype">>, <<"rq">>},{<<"qid">>,Qid} | Parts ]},
			gen_udp:send(State#state.sockp, State#state.broadcast, State#state.port, mochijson2:encode(Msg)),
			{noreply, State}
	end;
 
handle_cast({send_response, A, Qid, Ip, Port}, State) ->
    ?LOG(info, "sending response for qry ~s to ~p", [Qid, Ip]),
    {struct, Parts} = A,
    Hostname = ?CONFVAL(name, ""),
    Msg = {struct, [    {<<"_msgtype">>, <<"result">>},
                        {<<"qid">>, Qid},
                        {<<"result">>, 
                         {struct, [
                                   {<<"source">>, list_to_binary(Hostname)},
                                   {<<"port">>, ?CONFVAL({web, port}, ?DEFAULT_WEB_PORT)} |
                                   proplists:delete(<<"url">>,
                                    proplists:delete(<<"source">>,
                                     proplists:delete(<<"port">>, Parts)))
                         ]}}
                    ]},
    gen_udp:send(State#state.sockp, Ip, Port, mochijson2:encode(Msg)),
    {noreply, State}.

% msg on the broadcast socket = a new incoming query
handle_info({udp, Sock, Ip, _InPortNo, Packet}, State = #state{sock=Sock}) ->
    {struct, L} = mochijson2:decode(Packet),
    case proplists:get_value(<<"_msgtype">>,L) of
        <<"rq">> ->
            Qid = proplists:get_value(<<"qid">>, L),
            % do nothing if we dispatched, or already received this qid
            case ets:lookup(State#state.seenqids, Qid) of
                [{Qid, true}] -> 
                    ?LOG(info, "ignoring dupe query from ~p, ~s ~s", [Ip, Qid, Packet]),
                    {noreply, State};
                _ ->
                    ?LOG(info, "received query from ~p, ~s: ~s", [Ip, Qid, Packet]),
                    ets:insert(State#state.seenqids, {Qid,true}),
                    This = self(),
                    Cbs = [ fun(Ans)-> ?MODULE:send_response(This, Ans, Qid, Ip, State#state.port) end ],
					Qry = #qry{obj = {struct, L}, qid = Qid, local = false},
                    playdar_resolver:dispatch(Qry, Cbs),
                    {noreply, State}
            end;
            
        _ -> {noreply, State}
    end;

% msg on the direct socket = a direct response to us from a query
handle_info({udp, Sock, {A,B,C,D}=Ip, _InPortNo, Packet}, State = #state{sockp=Sock}) ->
    {struct, L} = mochijson2:decode(Packet),
    case proplists:get_value(<<"_msgtype">>,L) of
        <<"result">> ->
            Qid = proplists:get_value(<<"qid">>, L),
			?LOG(info, "received result from ~p, ~s: ~s", [Ip, Qid, Packet]), 
			case proplists:get_value(<<"result">>, L) of
				{struct, L2} ->
					% if this result came with an URL, honor it, else build one.
					% (yes, it's possible to respond with an external web url,
					%  but is not normally considered correct behaviour --
					%  playdar nodes should typically proxy everything)
					case proplists:get_value(<<"url">>, L2) of
						undefined ->
							DefPort = proplists:get_value(port, ?CONFVAL(web, []), ?DEFAULT_WEB_PORT),
							HttpPort = proplists:get_value(<<"port">>, L, DefPort),
							Sid = proplists:get_value(<<"sid">>, L2),
							Url = io_lib:format("http://~w.~w.~w.~w:~w/sid/~s",
												[A,B,C,D,HttpPort,Sid]),
							playdar_resolver:add_results(Qid, {struct, 
												  		[{<<"url">>, list_to_binary(Url)}|L2]});
						_Url ->
							playdar_resolver:add_results(Qid, {struct, L2})
					end,                            
					{noreply, State};
                _ ->
                    {noreply, State}
            end;
		_ -> {noreply, State}
	end.

terminate(_Reason, State) ->
    gen_udp:close(State#state.sock),
    gen_udp:close(State#state.sockp),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

