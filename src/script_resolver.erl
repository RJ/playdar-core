-module(script_resolver).

-behaviour(gen_server).
-behaviour(playdar_resolver).
-include("playdar.hrl").

%% playdar_resolver API:
-export([start_link/1, resolve/3, weight/1, targettime/1, name/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {port, exe, name, tt, weight}).

%% API
start_link(Exe)         -> gen_server:start_link(?MODULE, [Exe], []).
resolve(Pid, Q, Qpid)   -> gen_server:cast(Pid, {resolve, Q, Qpid}).
weight(Pid)             -> gen_server:call(Pid, weight).
targettime(Pid)         -> gen_server:call(Pid, targettime).
name(Pid)               -> gen_server:call(Pid, name).

%%

init([Exe]) ->
    Port = open_port({spawn, Exe}, [binary, {packet, 4}, use_stdio]),
    DefName = <<"undefined">>,
    {ok, #state{port=Port,exe=Exe,name=DefName,tt=50,weight=1}}.

handle_call(weight, _From, State) -> {reply, State#state.weight, State};
handle_call(targettime, _From, State) -> {reply, State#state.tt, State};
handle_call(name, _From, State) -> {reply, State#state.name, State}.

handle_cast({resolve, Q, Qpid}, #state{port=Port} = State) ->
    {struct, Parts} = Q,
    Qid = qry:qid(Qpid),
    Msg = {struct, [ {<<"_msgtype">>, <<"rq">>},{<<"qid">>,Qid} | Parts ]},
    Encoded = mochijson2:encode(Msg),
    port_command(Port, Encoded),
    {noreply, State}.

handle_info({Port, {data, Data}}, #state{port=Port} = State) ->
    {struct, L} = mochijson2:decode(Data),
    case proplists:get_value(<<"_msgtype">>,L) of
        
        <<"results">> ->
            Qid = proplists:get_value(<<"qid">>, L),
            case resolver:qid2pid(Qid) of
                Qpid when is_pid(Qpid) ->
                    case proplists:get_value(<<"results">>, L) of
                        Results when is_list(Results) ->
                            ?LOG(info, "Got results from script: ~p", [Results]),
                            qry:add_results(Qpid, Results);
                        _ ->
                            ?LOG(warning, "Script ~s - invalid results returned~n", [State#state.name])
                    end,
                    {noreply, State};
                _ ->
                    ?LOG(warning, "Script responded with invalid QID", []),
                    {noreply, State}
            end;
        
        <<"settings">> ->
            Name    = binary_to_list(
                        proplists:get_value(<<"name">>, L, State#state.name)),
            Weight  = proplists:get_value(<<"weight">>, L, State#state.weight),
            TT      = proplists:get_value(<<"targettime">>, L, State#state.tt),
            resolver:add_resolver(?MODULE, Name, Weight, TT, self()),
            {noreply, State#state{name=Name, weight=Weight, tt=TT}};

        _ ->
            ?LOG(warning, "Unhandled _msgtype for script response: ~s~n",[Data]),
            {noreply, State}
    end;

handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    ?LOG(warning, "script_resolver, port terminated for script: ~s~n",[State#state.name]),
    {stop, {port_terminated, Reason}, State}.

terminate({port_terminated, _Reason}, _State) ->    ok;

terminate(_Reason, #state{port = Port} = _State) ->     port_close(Port).

code_change(_OldVsn, State, _Extra) ->     {ok, State}.



