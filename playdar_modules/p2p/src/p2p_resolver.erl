-module(p2p_resolver).
-behaviour(gen_server).
%-behaviour(playdar_resolver).
-include("playdar.hrl").

-export([start_link/0, resolve/3, weight/1, targettime/1, name/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {seenqids}).

start_link()            -> gen_server:start_link({local,?MODULE},?MODULE,[],[]).

resolve(Pid, Q, Qpid)   -> gen_server:cast(Pid, {resolve, Q, Qpid}).
weight(_Pid)            -> 60.
targettime(_Pid)        -> 1000.
name(_Pid)              -> "p2p".


%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok,_} = p2p_router:start_link(?CONFVAL({p2p,port},60211)),
    {ok, #state{ seenqids=ets:new(seenqids,[]) }}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({resolve, Q, Qpid}, State) ->
    {struct, _Parts} = Q,
    Qid = qry:qid(Qpid),
    % Ignore if we've dealt with this qid already
    case ets:lookup(State#state.seenqids, Qid) of
        [{Qid, true}] -> 
            {noreply, State};
        _ ->
            ?LOG(info, "P2P dispatching query", []),
            ets:insert(State#state.seenqids, {Qid,true}),
            %Msg = {struct, [ {<<"_msgtype">>, <<"rq">>},{<<"qid">>,Qid} | Parts ]},
            
            {noreply, State}
    end.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

