-module(playdar_config).
-include("playdar.hrl").
-export([load/1, confval/2]).
-include("default_config.hrl"). % must be after the export

load(Etc) ->
    case filelib:is_dir(Etc) of
        true  -> ConfigFiles = filelib:wildcard(Etc ++ "/*.conf");
        false -> ConfigFiles = []
    end,
    
    case ConfigFiles of
        [] ->
            % Set the defaults, if not specified:
            {ok, Terms} = default_config(),
            lists:foreach(fun({K,V}) ->
                              application:set_env(playdar, K, V)
                          end, Terms);
        _ ->
            lists:foreach(fun handle_config/1, ConfigFiles)
    end,

    % we must have a host name
    case application:get_env(playdar, name) of
        undefined ->
            case inet:gethostname() of
                {ok, Hostname} ->
                    application:set_env(playdar, name, Hostname);
                _ ->
                    application:set_env(playdar, name, "No-name")
            end;
        _ -> ok
    end,

    %io:format("Loaded config: ~p~n", [application:get_all_env(playdar)]),
    ok.


handle_config(FileFull) ->
    Toks = string:tokens(FileFull, "/"),
    File = lists:nth(length(Toks), Toks),
    %io:format("File:~s~n",[File]),
    F  = lists:sublist(File, length(File)-5), % strip .conf
    Fa = list_to_atom(F),
    
    case file:consult(FileFull) of
        {ok, Terms} ->
            ?LOG(info, "Reading ~s", [File]),
            lists:foreach(  fun({K,V})->
                                case Fa of
                                    playdar -> application:set_env(playdar, K, V);
                                    _       -> application:set_env(playdar, {Fa, K}, V)
                                end                                      
                            end, Terms),
            ok;
        
        {error, {_LineNumber, erl_parse, _ParseMessage} = Reason} ->
            ExitText = lists:flatten(File ++ " approximately in the line "
                                     ++ file:format_error(Reason)),
            ?LOG(error, "Problem loading playdar config file ~n~s", [ExitText]),
            error;
        
        {error, Reason} ->
            ExitText = lists:flatten(File ++ ": " ++ file:format_error(Reason)),
            ?LOG(error, "Problem loading playdar config file ~n~s", [ExitText]),
            error
    end,
    ok.

confval(ConfVal_K,ConfVal_Def) ->
        case application:get_env(playdar, ConfVal_K) of
            {ok, ConfVal_Result} -> ConfVal_Result;
            _ -> ConfVal_Def
        end.