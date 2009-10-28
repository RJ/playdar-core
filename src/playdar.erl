%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(playdar).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the playdar server.
start() ->
    playdar_deps:ensure(),
    ensure_started(crypto),
    application:start(playdar).

%% @spec stop() -> ok
%% @doc Stop the playdar server.
stop() ->
    Res = application:stop(playdar),
    application:stop(crypto),
    Res.
