%TODO should probably have a log server that buffers and writes to a log file..

% logging macros, eg: ?L(info, "something is: ~s", [S]).
-define(LOG(Log_Level, Log_Format, Log_Args), 
        begin 
            {{Log_Y,Log_M,Log_D},{Log_Hr,Log_Min,Log_Sec}} = erlang:localtime(),
            io:format("~w/~2..0w/~2..0w ~2..0w:~2..0w:~2..0w ~w ~w\t" ++ Log_Format ++ "\n",
                      [Log_Y, Log_M, Log_D, Log_Hr,Log_Min,Log_Sec,?MODULE,Log_Level|Log_Args]),
            ok
        end ).

