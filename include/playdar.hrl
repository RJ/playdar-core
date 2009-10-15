%TODO should probably have a log server that buffers and writes to a log file..

% logging macros, eg: ?L(info, "something is: ~s", [S]).
-define(LOG(Log_Level, Log_Format, Log_Args), playdar:do_log(?MODULE, Log_Level, Log_Format, Log_Args)).

-define(CONFVAL(ConfVal_K,ConfVal_Def), playdar_config:confval(ConfVal_K,ConfVal_Def)).

% use by playdar_ctl and ctl commands:
-define(OK, 0).
-define(ERROR, 1).
-define(WTF, 99).
