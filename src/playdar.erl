-module(playdar).
-behaviour(application).
-export([start/0,
         start/2,
         shutdown/0,
         stop/1
         ]).

start() -> start(foo, []).

start(_, StartArgs) ->
    inets:start(),
    case playdar_supervisor:start_link(StartArgs) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

shutdown() ->
    application:stop(playdar).

stop(_State) ->
    ok.
