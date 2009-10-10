-module(listener_impl).
-behaviour(gen_listener_tcp).
-include("p2p.hrl").
-include("playdar.hrl").
%% API
-export([start/1]).

%% gen_listener_tcp callbacks
-export([init/1,
         handle_accept/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% @doc Start the server.
start(Port) ->
    gen_listener_tcp:start({local, ?MODULE}, ?MODULE, [Port], []).

init([Port]) ->
    ?LOG(info, "listener_impl init",[]),
    {ok, {Port, ?TCP_OPTS_SERVER}, nil}.

handle_accept(Sock, State) ->
    ?LOG(info, "handle accept",[]),
    {ok, Pid} = p2p_conn:start(Sock, in),
    gen_tcp:controlling_process(Sock, Pid),
    {noreply, State}.

handle_call(Request, _From, State) ->
    {reply, {illegal_request, Request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
