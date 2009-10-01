%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(playdar_httpd).
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
%% @doc Start the playdar_httpd server.
start() ->
    playdar_httpd_deps:ensure(),
    ensure_started(crypto),
    application:start(playdar_httpd).

%% @spec stop() -> ok
%% @doc Stop the playdar_httpd server.
stop() ->
    Res = application:stop(playdar_httpd),
    application:stop(crypto),
    Res.
