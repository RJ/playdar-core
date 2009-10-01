-module(lan_resolver).

-behaviour(gen_server).

%% API
-export([start_link/0, resolve/2, weight/0, targettime/0, send_response/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {sock}).
-define(BROADCAST, {239,255,0,1}).

%% API
start_link()            -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
resolve(Q, Qpid)        -> gen_server:cast(?MODULE, {resolve, Q, Qpid}).
weight()                -> 95.
targettime()            -> 50.

send_response(A, Qid, Ip, Port) -> 
    gen_server:cast(?MODULE, {send_response, A, Qid, Ip, Port}).

%% gen_server callbacks
init([]) ->
    LAddr = {192,168,1,67},
    {ok, Sock} = gen_udp:open(60210, [binary, 
                                     {reuseaddr, true},{ip, ?BROADCAST}, 
                                     {add_membership, {?BROADCAST, LAddr}}]),
    {ok, #state{sock=Sock}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({resolve, Q, Qpid}, State) ->
    {struct, Parts} = Q,
    Msg = {struct, [{<<"_msgtype">>, <<"rq">>},{<<"qid">>,qry:qid(Qpid)}|Parts]},
    gen_udp:send(State#state.sock, ?BROADCAST, 60210, mochijson2:encode(Msg)),
    {noreply, State};
 
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

handle_info({udp, Socket, {A,B,C,D}=Ip, InPortNo, Packet}, State) when Ip /=  {192,168,1,67} ->
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
                                          [{<<"url">>, list_to_binary(Url)}|L2]});
                _ ->
                    io:format("noop1",[])
            end;
        
        <<"rq">> ->
            Qid = proplists:get_value(<<"qid">>, L),
            Cbs = [ fun(Ans)-> ?MODULE:send_response(Ans, Qid, Ip, 60210) end ],
            resolver:dispatch({struct,L}, Qid, Cbs);
            
        _ -> io:format("noop2",[])
    end,
    {noreply, State};
    
handle_info({udp, _Socket, _Ip, _InPortNo, _Packet}, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    gen_udp:close(State#state.sock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

