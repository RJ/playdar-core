%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the playdar_httpd application.

-module(playdar_httpd_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for playdar_httpd.
start(_Type, _StartArgs) ->
    playdar_httpd_deps:ensure(),
    playdar_httpd_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for playdar_httpd.
stop(_State) ->
    ok.
