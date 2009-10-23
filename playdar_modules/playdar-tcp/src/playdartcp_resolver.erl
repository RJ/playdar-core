-module(p2p_resolver).
-behaviour(gen_server).
-behaviour(playdar_reader).
-behaviour(playdar_resolver).
-include("playdar.hrl").

%% playdar_reader exports:
-export([reader_protocols/0]).

-export([start_link/0, resolve/3, weight/1, targettime/1, name/1, p2p_reader/3, reader_start_link/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {seenqids}).

start_link()            -> gen_server:start_link({local,?MODULE},?MODULE,[],[]).

resolve(_Pid, Q, Qpid)  -> gen_server:cast(p2p_router, {resolve, Q, Qpid}).
weight(_Pid)            -> 60.
targettime(_Pid)        -> 1000.
name(_Pid)              -> "p2p".

reader_start_link(A, Pid, Ref) -> spawn_link( ?MODULE, p2p_reader, [A, Pid, Ref]). 

%% ====================================================================
%% Server functions
%% ====================================================================
reader_protocols() ->
    [ {"p2p", {?MODULE, reader_start_link}} ].

init([]) ->
    {ok,_} = p2p_router:start_link(?CONFVAL({p2p,port},60211)),
    % Connect to any peers listed in the config file:
    lists:foreach(fun({Ip,Port})->p2p_router:connect(Ip,Port)end, ?CONFVAL({p2p,peers},[])),
    % Register us as a playdar resolver:
    resolver:add_resolver(?MODULE, name(self()), weight(self()), targettime(self()), self()),
    % Register our web request handler (for our localhost web gui)
    http_registry:register_handler("p2p", fun p2p_web:http_req/2, "P2P Connection Status Page", "/p2p"),
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

p2p_reader({struct, A}, Pid, Ref) ->
	"p2p://"++Rest = binary_to_list(proplists:get_value(<<"url">>, A)),
	[Sid, Name] = string:tokens(Rest, "\t"),
	?LOG(info, "Requesting p2p stream for '~p' from '~p'", [Sid, Name]),
	%TODO name2pid
	case proplists:get_value(Name, p2p_router:peers()) of
		P when is_pid(P) ->
			p2p_conn:request_sid(P, list_to_binary(Sid), Pid, Ref);
		undefined ->
			Pid ! {Ref, error, wtf}
	end.
	
	
	