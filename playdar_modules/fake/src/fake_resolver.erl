-module(fake_resolver).
-include("playdar.hrl").
-behaviour(gen_server).
-behaviour(playdar_resolver).

%% API
-export([start_link/0, resolve/3, weight/1, targettime/1, name/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {}).

%% API
start_link()            -> gen_server:start_link(?MODULE, [], []).
resolve(Pid, Q, Qpid)   -> gen_server:cast(Pid, {resolve, Q, Qpid}).
weight(_Pid)            -> 50.
targettime(_Pid)        -> 25.
name(_Pid)              -> "Fake Mokele Resolver".

%% gen_server callbacks
init([]) ->
    resolver:add_resolver(?MODULE, name(self()), weight(self()), targettime(self()), self()),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({resolve, Q, Qpid}, State) ->
    case Q of
        {struct, Mq} -> % Mq is a proplist
            case string:to_lower(
                 binary_to_list(proplists:get_value(<<"artist">>, Mq, ""))) of
                "mokele" ->
                    Rep =   {struct, [
                                {<<"artist">>, <<"Mokele">>},
                                {<<"track">>,  <<"Hiding in your Insides!">>},
                                {<<"album">>,  <<"">>},
                                {<<"score">>, 0.2},
                                {<<"url">>, <<"file:///tmp/test.txt">>}
								%{<<"url">>, <<"http://www.playdar.org/hiding.mp3">>}
                            ]},
                    qry:add_result(Qpid, Rep);
                _ -> noop
            end;
        _ -> noop
    end,
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

