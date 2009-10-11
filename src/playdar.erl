%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(playdar).
-author('author <author@example.com>').
-export([start/0, stop/0, do_log/4]).

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

% TODO proper logging!
% for now, this fun is called by the ?LOG macro
do_log(Mod, Log_Level, Log_Format, Log_Args) ->  
    case Log_Level of
        debug -> noop;
        _ ->
            {{Log_Y,Log_M,Log_D},{Log_Hr,Log_Min,Log_Sec}} = erlang:localtime(),
            io:format("~w/~2..0w/~2..0w ~2..0w:~2..0w:~2..0w ~w ~w\t" ++ Log_Format ++ "\n",
                      [Log_Y, Log_M, Log_D, Log_Hr,Log_Min,Log_Sec,Mod,Log_Level|Log_Args])
    end,
    ok.