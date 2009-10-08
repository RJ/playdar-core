-module(playdar_config).

-include("playdar.hrl").

-export([load/0]).

load() ->
    case inet:gethostname() of
        {ok, Hostname} ->
            application:set_env(playdar, hostname, Hostname);
        _ ->
            ?LOG(error, "Couldn't lookup hostname",[]),
            application:set_env(playdar, hostname, "UnknownHost")
    end,
    ok.

%% load() ->
%%     File = case application:get_env(config) of                    
%%         {ok, Path} -> Path;
%%         undefined ->
%%                 case os:getenv("PLAYDAR_CONFIG_PATH") of
%%                     false -> guess_config();
%%                     Path  -> Path
%%                 end
%%     end,
%% 
%%     case file:consult(File) of
%%         {ok, Terms} ->
%%             ?LOG(info, "Using config file: ~s", [File]),
%%             parse_terms(Terms),
%%             ok;
%%         
%%         {error, {_LineNumber, erl_parse, _ParseMessage} = Reason} ->
%%             ExitText = lists:flatten(File ++ " approximately in the line "
%%                                      ++ file:format_error(Reason)),
%%             ?LOG(error, "Problem loading playdar config file ~n~s", [ExitText]),
%%             exit(ExitText);
%%         
%%         {error, Reason} ->
%%             ExitText = lists:flatten(File ++ ": " ++ file:format_error(Reason)),
%%             ?LOG(error, "Problem loading playdar config file ~n~s", [ExitText]),
%%             exit(ExitText)
%%     end.
%% 
%% 
%% parse_terms({K,V}) -> application:set_env(playdar, K, V);
%% parse_terms(K) -> application:set_env(playdar, K, true).
%% 
%% guess_config() -> "/etc/playdar/playdar.conf".
