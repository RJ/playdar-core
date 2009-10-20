-module(modules_sup).
-behaviour(supervisor).
-include("playdar.hrl").
-export([init/1, start_link/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Checks the playdar modules directory and loads all modules
%% MODDIR/yourmodulename/ebin 
%% if this dir contains a .app file, it loads the application
%% else it inspects all .beam files and tries to make use of what exists
%% based on which behaviours they implement.
init([]) ->
    ModDir = "playdar_modules",
    % get all suitable subdirs (that contain an ebin dir)
    Dirs0 = [ Dir || Dir <- filelib:wildcard(ModDir ++ "/*"), 
                            filelib:is_dir(Dir), 
                            filelib:is_dir(Dir++"/ebin") ],
    Dirs = [ D || D <- Dirs0, begin L = string:tokens(D, "/"), lists:member(lists:nth(length(L),L), ?CONFVAL(modules_blacklist,[])) == false end ],
    Specs0 = [
        case A = filelib:wildcard(Dir++"/ebin/*.app") of
            % Is this module an OTP Application:
            [AppName0] ->
                %io:format("AppName0: ~s~n",[AppName0]),
                AppName = hd(string:tokens(AppName0, ".app")),
                case filelib:is_file(A) of
                    false -> undefined;
                    true  ->
                        code:add_path(Dir++"/ebin"),
                        case application:start(list_to_atom(AppName)) of
                            ok ->
                                ?LOG(info, "Playdar module app started: ~s", [AppName]),
                                undefined;
                            {error, Reason} ->
                                ?LOG(error, "Playdar module ~s failed to load: ~p", [AppName, Reason]),
                                undefined
                        end
                end;
            % This module is just a beam or two, decide what to do:
            _ ->
                % this will return a childspec if we need to supervise anything:
                code:add_path(Dir++"/ebin"),
                process_simple_dir(Dir++"/ebin")
        end
        || Dir <- Dirs ],
    
    Specs = [ Spec || Spec <- lists:flatten(Specs0), Spec /= undefined ],
    % anything to start?
    %?LOG(info, "Starting modules: ~p", [Specs]),
    case Specs of
        [] -> ignore;
        _  -> {ok,
                {{one_for_one,
                  length(Specs), 1} %Limit worker restarts to 1x(resolver count) per second.
                , Specs}}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

process_simple_dir(Dir) ->
    % strip off the .beam and pass module name to be processed:
    [ process_module(path_to_module(M)) 
      || M <- filelib:wildcard(Dir++"/*.beam") ].

path_to_module(Path) ->
    Slashed = string:tokens(Path, "/"),
    File = lists:nth(length(Slashed), Slashed), % strip path
    Mod = lists:sublist(File, length(File)-5),  % strip .beam
    list_to_atom(Mod).

process_module(Mod) ->
    code:purge(Mod),
    case code:load_file(Mod) of
        {error, What} ->
            ?LOG(error, "Failed to load module: ~p Reason: ~p", [Mod, What]),
            undefined;
        {module, Mod} ->
            % Find out if this module implements any playdar behaviours:
            CheckedBehaviours = [playdar_resolver, playdar_reader],
            Behaviours = lists:flatten([ 
            [ Beh || Beh <- Bs, lists:member(Beh, CheckedBehaviours)]
            || {behaviour, Bs} <- Mod:module_info(attributes) 
            ]),
            case lists:member(playdar_reader, Behaviours) of 
                % if it's a playdar_streamer, register it:
                true  ->
                    lists:foreach( fun({Proto, F}) ->
                                    playdar_reader_registry:register_handler(Proto, F)
                                   end, Mod:reader_protocols());
                false -> nevermind
            end,
            case lists:member(playdar_resolver, Behaviours) of 
                % if it's a playdar_resolver, supervise it, it will
                % register itself on startup:
                true  ->
                    {Mod, 
                      {Mod, start_link, []}, 
                      permanent, 5, worker, [Mod]};
                false -> undefined
            end
    end.

