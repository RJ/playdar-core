%TODO should probably have a log server that buffers and writes to a log file..

% logging macros, eg: ?L(info, "something is: ~s", [S]).
-define(LOG(Log_Level, Log_Format, Log_Args), 
        begin 
            case Log_Level of
                debug -> noop;
                _ ->
                    {{Log_Y,Log_M,Log_D},{Log_Hr,Log_Min,Log_Sec}} = erlang:localtime(),
                    io:format("~w/~2..0w/~2..0w ~2..0w:~2..0w:~2..0w ~w ~w\t" ++ Log_Format ++ "\n",
                              [Log_Y, Log_M, Log_D, Log_Hr,Log_Min,Log_Sec,?MODULE,Log_Level|Log_Args])
            end,
            ok
        end ).


-define(CONFVAL(ConfVal_K,ConfVal_Def),
        case application:get_env(playdar, ConfVal_K) of
            {ok, ConfVal_Result} -> ConfVal_Result;
            _ -> ConfVal_Def
        end ).
