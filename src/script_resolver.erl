-module(script_resolver).

-behaviour(gen_server).
-behaviour(playdar_resolver).
-include("playdar.hrl").

%% playdar_resolver API:
-export([start_link/1, resolve/2, weight/1, targettime/1, name/1, localonly/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {port, exe, name, tt, weight, localonly}).

%% API
start_link(Exe)         -> gen_server:start_link(?MODULE, [Exe], []).
resolve(Pid, Qry)       -> gen_server:cast(Pid, {resolve, Qry}).
weight(Pid)             -> gen_server:call(Pid, weight).
targettime(Pid)         -> gen_server:call(Pid, targettime).
name(Pid)               -> gen_server:call(Pid, name).
localonly(Pid)			-> gen_server:call(Pid, localonly).
%%

init([Exe]) ->
    Port = open_port({spawn, Exe}, [binary, {packet, 4}, use_stdio]),
    DefName = <<"undefined">>,
    {ok, #state{port=Port,exe=Exe,name=DefName,tt=50,weight=1,localonly=false}}.

handle_call(weight, _From, State) -> {reply, State#state.weight, State};
handle_call(targettime, _From, State) -> {reply, State#state.tt, State};
handle_call(name, _From, State) -> {reply, State#state.name, State}.

handle_cast({resolve, #qry{obj={struct, Parts}, qid=Qid}}, #state{port=Port} = State) ->
    Msg = {struct, [ {<<"_msgtype">>, <<"rq">>},{<<"qid">>,Qid} | Parts ]},
    Encoded = mochijson2:encode(Msg),
    port_command(Port, Encoded),
    {noreply, State}.

handle_info({Port, {data, Data}}, #state{port=Port} = State) ->
    {struct, L} = mochijson2:decode(Data),
    case proplists:get_value(<<"_msgtype">>,L) of
        
        <<"results">> ->
            Qid = proplists:get_value(<<"qid">>, L),
			Results = proplists:get_value(<<"results">>, L),
			resolver:add_results(Qid, Results),
			{noreply, State};
        
        <<"settings">> ->
            Name    = binary_to_list(
                        proplists:get_value(<<"name">>, L, State#state.name)),
            Weight  = proplists:get_value(<<"weight">>, L, State#state.weight),
            TT      = proplists:get_value(<<"targettime">>, L, State#state.tt),
			Local   = proplists:get_value(<<"localonly">>, L, State#state.localonly),
            resolver:add_resolver(?MODULE, Name, Weight, TT, self()),
            {noreply, State#state{	name=Name, 
									weight=Weight, 
									tt=TT, 
									localonly=Local
								 }};
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



