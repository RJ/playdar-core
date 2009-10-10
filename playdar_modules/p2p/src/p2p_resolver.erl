-module(p2p_resolver).
-behaviour(gen_server).
-behaviour(playdar_resolver).
-include("playdar.hrl").

-export([start_link/0, resolve/3, weight/1, targettime/1, name/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {seenqids}).

start_link()            -> gen_server:start_link({local,?MODULE},?MODULE,[],[]).

resolve(_Pid, Q, Qpid)   -> gen_server:cast(p2p_router, {resolve, Q, Qpid}).
weight(_Pid)            -> 60.
targettime(_Pid)        -> 1000.
name(_Pid)              -> "p2p".


%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok,_} = p2p_router:start_link(?CONFVAL({p2p,port},60211)),
    lists:foreach(fun({Ip,Port})->p2p_router:connect(Ip,Port)end, ?CONFVAL({p2p,peers},[])),
    resolver:add_resolver(?MODULE, name(self()), weight(self()), targettime(self()), self()),
    {ok, #state{ seenqids=ets:new(seenqids,[]) }}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

