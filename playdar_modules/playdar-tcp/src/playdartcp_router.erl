-module(p2p_router).
-behaviour(gen_server).
-include("playdar.hrl").
-include("p2p.hrl").

-export([start_link/1, register_connection/2, send_query_response/3, 
		 connect/2, peers/0, bytes/0, broadcast/1, broadcast/2, seen_qid/1, 
         disconnect/1, sanitize_msg/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {listener, conns, seenqids, piddb, namedb}).

start_link(Port) -> gen_server:start({local, ?MODULE}, ?MODULE, [Port], []).

register_connection(Pid, Name) ->
    gen_server:call(?MODULE, {register_connection, Pid, Name}).

send_query_response(Ans, Qid, Name) ->
    gen_server:cast(?MODULE, {send_query_response, Ans, Qid, Name}).

connect(Ip, Port) ->
    gen_server:call(?MODULE, {connect, Ip, Port}).

disconnect(Name) ->
    gen_server:call(?MODULE, {disconnect, Name}).

peers() -> gen_server:call(?MODULE, peers).
bytes() -> gen_server:call(?MODULE, bytes).

broadcast(M) when is_tuple(M) -> broadcast(M, undefined).
broadcast(M, Except) when is_tuple(M) -> gen_server:cast(?MODULE, {broadcast, M, Except}).

seen_qid(Qid) -> gen_server:cast(?MODULE, {seen_qid, Qid}).

%% ====================================================================
%% Server functions
%% ====================================================================
init([Port]) ->
    process_flag(trap_exit, true),
    Pid = case ?CONFVAL({p2p, listen}, true) of
              true  -> listener_impl:start_link(Port);
              false -> undefined
          end,
    % setup regular msgs to aggregate/calculate bandwidth usage:
    timer:send_interval(1000, self(), calculate_bandwidth_secs),
    %timer:send_interval(60000, self(), calculate_bandwidth_mins),
    %timer:send_interval(3600000, self(), calculate_bandwidth_hrs),
    {ok, #state{	listener=Pid,
					seenqids=ets:new(seenqids,[]), 
					conns=[],
                    piddb=ets:new(piddb,[]),
                    namedb=ets:new(namedb,[])
               }}.

handle_call({disconnect, Name}, _From, State) ->
    case ets:lookup(State#state.namedb, Name) of
        [{_,Pid}] ->
            erlang:exit(Pid, disconnect_request),
            {reply, ok, State};
        [] -> 
            {reply, noname, State}
    end;

handle_call({connect, Ip, Port}, _From, State) ->
        case gen_tcp:connect(Ip, Port, ?TCP_OPTS, 10000) of
            {ok, Sock} ->
                {ok, Pid} = p2p_conn:start(Sock, out),
                gen_tcp:controlling_process(Sock, Pid),
                ?LOG(info, "New outbound connection to ~p:~p pid:~p", [Ip, Port,Pid]),
                {reply, {ok, Pid}, State};        
            {error, Reason} ->
                ?LOG(warn, "Failed to connect to ~p:~p Reason: ~p", [Ip,Port,Reason]),
                {reply, {error, Reason}, State}
        end;
        
handle_call(peers, _From, State) ->
    Db = ets:tab2list(State#state.namedb),
    {reply, Db, State};

handle_call(bytes, _From, State) -> 
    {reply, ets:tab2list(State#state.piddb), State};

handle_call({register_connection, Pid, Name}, _From, State) ->
    % TODO we should probably kick the old conn with this name
    % so ppl can reconnect if their old conn lags out
    case ets:lookup(State#state.namedb, Name) of
        [] ->
            link(Pid),
            N = now(),
            BwStats = {{N,[]}, {N,[]}, {N,[]}},
            ets:insert(State#state.piddb, {Pid, Name, BwStats}), % secs, mins, hrs
            ets:insert(State#state.namedb, {Name, Pid}),
            {reply, ok, State};
        _  ->
            {reply, disconnect, State}
    end.


%%

handle_cast({seen_qid, Qid}, State) ->
    % TODO may want to use a bloom filter instead of ets here:
	ets:insert(State#state.seenqids, {Qid, true}),
	{noreply, State};

handle_cast({broadcast, M, Except}, State) ->
    NamePidList = ets:tab2list(State#state.namedb),
    lists:foreach(fun({_Name, Pid})->
                          if
                              Pid == Except -> noop;
                              true ->
                                p2p_conn:send_msg(Pid, M)
                          end
                  end, NamePidList),
    {noreply, State};
    
handle_cast({send_query_response, {struct, Parts}, Qid, Name}, State) ->
    case ets:lookup(State#state.namedb, Name) of
        [{_,Pid}] when is_pid(Pid)->
            Msg = {result, Qid, sanitize_msg({struct, Parts})},                                     
            p2p_conn:send_msg(Pid, Msg),
            {noreply, State};
        [] ->
            {noreply, State}
    end;

handle_cast({resolve, Q, Qpid}, State) ->
    {struct, Parts} = Q,
    Qid = qry:qid(Qpid),
    % Ignore if we've dealt with this qid already
    case ets:lookup(State#state.seenqids, Qid) of
        [{_,true}] -> 
            {noreply, State};
        _ ->
            ?LOG(info, "P2P dispatching query", []),
            ets:insert(State#state.seenqids, {Qid,true}),
            Msg = {rq, Qid, {struct, Parts }},
            p2p_router:broadcast(Msg),            
            {noreply, State}
    end.
%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(calculate_bandwidth_secs, State) ->
%%     Now = now(),
%%     Pids = [P||{_N,P}<-State#state.conns],
%%     lists:foreach(
%%         fun(Pid)->
%%             [{_, {SecT,SecUp,SecDown,SecL}, Mins, Hrs}] = ets:lookup(State#state.bwdb, Pid),
%%             [{_,Up, Down}] = ets:lookup(State#state.bytesdb, Pid),
%%             TimeDiff  = timer:now_diff(Now, SecT),
%%             UpDiff    = Up-SecUp,
%%             DownDiff  = Down-SecDown,
%%             CSecUp  = round((UpDiff * 1000000)/TimeDiff),
%%             CSecDown= round((DownDiff * 1000000)/TimeDiff),
%%             NewSecL   = lists:sublist([{CSecUp,CSecDown}|SecL],60),
%%             ets:insert(State#state.bwdb, {Pid, {Now, Up, Down, NewSecL}, Mins, Hrs})            
%%         end, Pids),
    {noreply, State};

%% handle_info(calculate_bandwidth_mins, State) ->
%%     Now = now(),
%%     Pids = [P||{_N,P}<-State#state.conns],
%%     lists:foreach(
%%         fun(Pid)->
%%             [{_, Secs, {MinT,MinUp,MinDown,MinL}, Hrs}] = ets:lookup(State#state.bwdb, Pid),
%%             [{_,Up, Down}] = ets:lookup(State#state.bytesdb, Pid),
%%             TimeDiff  = timer:now_diff(Now, MinT),
%%             UpDiff    = Up-MinUp,
%%             DownDiff  = Down-MinDown,
%%             CMinUp  = round((UpDiff * 1000000)/TimeDiff),
%%             CMinDown= round((DownDiff * 1000000)/TimeDiff),
%%             NewMinL   = lists:sublist([{CMinUp,CMinDown}|MinL],60),
%%             ets:insert(State#state.bwdb, {Pid, Secs, {Now, Up, Down, NewMinL}, Hrs})            
%%         end, Pids),
%%     {noreply, State};
%% 
%% handle_info(calculate_bandwidth_hrs, State) ->
%%     Now = now(),
%%     Pids = [P||{_N,P}<-State#state.conns],
%%     lists:foreach(
%%         fun(Pid)->
%%             [{_, Secs, Mins, {HrT,HrUp,HrDown,HrL}}] = ets:lookup(State#state.bwdb, Pid),
%%             [{_,Up, Down}] = ets:lookup(State#state.bytesdb, Pid),
%%             TimeDiff  = timer:now_diff(Now, HrT),
%%             UpDiff    = Up-HrUp,
%%             DownDiff  = Down-HrDown,
%%             CHrUp  = round((UpDiff * 1000000)/TimeDiff),
%%             CHrDown= round((DownDiff * 1000000)/TimeDiff),
%%             NewHrL   = lists:sublist([{CHrUp,CHrDown}|HrL],60),
%%             ets:insert(State#state.bwdb, {Pid, Secs, Mins, {Now, Up, Down, NewHrL}})            
%%         end, Pids),
%%     {noreply, State};

handle_info({'EXIT', Pid, _Reason}, State) ->
    case ets:lookup(State#state.piddb, Pid) of
        [{_, Name, _Bw}] ->
            ?LOG(info, "Removing user from registered cons: ~p", [Name]),
            ets:delete(State#state.namedb, Name),
            ets:delete(State#state.piddb, Pid),
            {noreply, State}
    end.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

% strips the internal "url" property, and replaces the source name if desired:
sanitize_msg({struct, Parts}) ->
    Name = ?CONFVAL(name, ""),
    case ?CONFVAL({p2p, rewrite_identity}, false) of
        true -> % reset the source var regardless
            {struct,    [ {<<"source">>, list_to_binary(Name)} |
                          proplists:delete(<<"url">>,
                            proplists:delete(<<"source">>,Parts)) ]};
        _ -> % leave it intact if it exists
            case proplists:get_value(<<"source">>, Parts) of
                undefined ->
                    {struct, [{<<"source">>, list_to_binary(Name)} |
                              proplists:delete(<<"url">>,Parts)]};
                _ ->
                    {struct, proplists:delete(<<"url">>,Parts)}
            end
    end.