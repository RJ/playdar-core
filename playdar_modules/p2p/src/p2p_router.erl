-module(p2p_router).

-behaviour(gen_server).
-include("playdar.hrl").
-include("p2p.hrl").

-export([start_link/1, register_connection/2, send_query_response/3, connect/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {listener, conns}).

start_link(Port) -> gen_server:start({local, ?MODULE}, ?MODULE, [Port], []).

register_connection(Pid, Name) ->
    gen_server:call(?MODULE, {register_connection, Pid, Name}).

send_query_response(Ans, Qid, Name) ->
    gen_server:cast(?MODULE, {send_query_response, Ans, Qid, Name}).

connect(Ip, Port) ->
    gen_server:cast(?MODULE, {connect, Ip, Port}).

%% ====================================================================
%% Server functions
%% ====================================================================
init([Port]) ->
    process_flag(trap_exit, true),
    %Port = 60211,
    Pid = listener_impl:start(Port),
    {ok, #state{listener=Pid, conns=[]}}.

handle_call({register_connection, Pid, Name}, _From, State) ->
    link(Pid),
    {reply, ok, State#state{conns=[{Name, Pid}|State#state.conns]}}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({connect, Ip, Port}, State) ->
    spawn(
     fun()->
        case gen_tcp:connect(Ip, Port, ?TCP_OPTS, 10000) of
            {ok, Sock} ->
                ?LOG(info, "New outbound connection to ~p:~p", [Ip, Port]),
                _Pid = p2p_conn:start(Sock, out),
                ok;        
            {error, Reason} ->
                ?LOG(warn, "Failed to connect to ~p:~p Reason: ~p", [Ip,Port,Reason]),
                ok
        end
     end),
    {noreply, State};     
    
handle_cast({send_query_response, {struct, Parts}, Qid, Name}, State) ->
    case proplists:get_value(Name, State#state.conns) of
        {Name, Pid} ->
            Hostname = ?CONFVAL(name, ""),
            Msg = {struct, [    {<<"_msgtype">>, <<"result">>},
                                {<<"qid">>, Qid},
                                {<<"result">>, 
                                 {struct, [
                                           {<<"source">>, list_to_binary(Hostname)} |
                                              proplists:delete(<<"url">>,
                                               proplists:delete(<<"source">>,Parts))
                                          ]}}
                           ]},
            p2p_conn:send_msg(Pid, {result, Msg}),
            {noreply, State};
        undefined ->
            {noreply, State}
    end.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({'EXIT', Pid, _Reason}, State) ->
    L = [ {Name, Pid1} || {Name, Pid1} <- State#state.conns, Pid == Pid1 ],
    case L of
        [] ->
            {noreply, State};
        {N, _P} ->
            ?LOG(info, "Removing user from registered cons: ~p", N),
            Conns = proplists:delete(N, State#state.conns),
            {noreply, State#state{conns=Conns}}
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

