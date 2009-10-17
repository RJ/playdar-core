-module(playdar_ctl).
-include("playdar.hrl").

-export([init/0, cmd/1, start/0, register_command/3, register_default_commands/0]).

start() ->
    [NodeS | Args] = init:get_plain_arguments(),
    Node = list_to_atom(NodeS),
    case rpc:call(Node, ?MODULE, cmd, [Args]) of
        {badrpc, R} ->
            io:format("Failed to communicate with ~p: ~p~n", [Node, R]),
            halt(?WTF);
        Ret ->
            halt(Ret)
    end.    

init() ->
    ets:new(ctlcmds, [set, public, named_table]),
    register_default_commands(),
    ok.

cmd("")         -> usage();

cmd([Cmd|Args]) -> 
    case ets:lookup(ctlcmds, Cmd) of
        [{Cmd, _Desc, Fun}] ->
            Result = case Fun of 
                        {M,F} when is_atom(M), is_atom(F) -> 
                            erlang:apply(M,F,[Args]); 
                        F when is_function(F) -> 
                            erlang:apply(F,[Args]) 
                     end,
            case Result of
                ?WTF ->
                    usage(),
                    ?WTF;
                X ->
                    X
            end;
        [] ->
            usage(),
            ?WTF
    end.
    

usage() ->
    io:format("~nUsage: playdarctl <cmd> [<args>..]~n"),
    io:format("~nCommands:~n"),
	io:format("  start         Start Playdar as background daemon~n"),
	io:format("  start-debug   Start Playdar as interactive foreground process~n"),
	io:format("  stop          Stop a running instance~n~n"),
    lists:foreach(fun({Cmd, Desc, _Fun})->
                          Pad = string:chars($\s, 14 - length(Cmd)),
                          io:format("  ~s~s~s~n",[Cmd, Pad, Desc])
                  end, [ A || A = {Z,X,C} <- ets:tab2list(ctlcmds), X /= "" ]),
    io:format("~n"),
    ?OK.


% Fun must have an arity of 1 (gets passed a list of args)
% Fun can also be {Mod, Fun}. 
% eg: register_command("ping", "prints out pong", {ping_svr, do_ping}).
% eg: register_command("ping", "prints out pong", fun ping/1
register_command(Cmd, Desc, Fun) ->
    ets:insert(ctlcmds, {Cmd, Desc, Fun}),
    ok.

register_default_commands() ->
    Cmds = [
		{"stop",
		 "", fun stop/1},
        {"ping",
         "Ask playdar daemon to pong", fun ping/1 },
        {"status",
         "Check status of running playdar instance", fun status/1 },
        {"numfiles",
         "Report how many files are indexed by the library module", fun numfiles/1},
        {"scan",
         "<dir> - scans dir for audio files, adds to library", fun scan/1},
        {"resolvers",
         "Lists currently loaded resolvers", fun resolvers/1},
        {"dump-library",
         "Debug command, dump library metadata", fun dump_library/1}
    ],
    lists:foreach( fun({Cmd, Desc, Fun}) -> 
                           register_command(Cmd,Desc,Fun)
                   end, Cmds),
    ok.
        
%% Built-in commands

status([]) ->
    io:format("~p~n", [init:get_status()]),
    ?OK.
    
ping([]) ->
    io:format("PONG~n"),
    ?OK.

numfiles([]) ->
    case resolver:resolver_pid(library_dets) of
        P when is_pid(P) ->
            Num = proplists:get_value(num_files, library_dets:stats(P), 0),
            io:format("~w~n", [Num]),
            ?OK;
        _ ->
            io:format("Library module not loaded~n"),
            ?ERROR
    end.

scan([Dir]) ->
    case resolver:resolver_pid(library_dets) of
        P when is_pid(P) ->
            library_dets:scan(P, Dir),
            ?OK;
        _ ->
            io:format("Library module not loaded~n"),
            ?ERROR
    end;

scan(_) ->
    ?WTF.

stop([]) ->
	% halt in 1 sec, so the rpc call can return OK in the meantime:
	spawn(fun()-> timer:sleep(1000), halt() end),
	?OK.
	
resolvers([]) ->
    R = resolver:resolvers(),
    io:format("   W    TT   Name~n"),
    lists:foreach(fun(L) ->
                          io:format("~4.w~6.w   ~s~n", [ proplists:get_value(weight, L),
                                                         proplists:get_value(targettime, L),
                                                         proplists:get_value(name, L) ])
                  end, R),
    ?OK.

dump_library([]) ->
    All = library_dets:dump_library(resolver:resolver_pid(library_dets)),
    
    lists:foreach(fun(P) ->
                          io:format("~s\t~s\t~s\t~s\t~s\t~w\n",
                                    [   proplists:get_value(artist, P),
                                        proplists:get_value(album, P),
                                        proplists:get_value(track, P),
                                        proplists:get_value(mimetype, P),
                                        proplists:get_value(url, P),
                                        proplists:get_value(duration, P) ])
                  end, All),
    ?OK.
    
    


