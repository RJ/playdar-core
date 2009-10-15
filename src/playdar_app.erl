%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the playdar application.

-module(playdar_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for playdar.
start(_Type, _StartArgs) ->
    playdar_deps:ensure(),
    playdar_ctl:init(),
    playdar_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for playdar.
stop(_State) ->
    ok.
