% include this in all your playdar related code

% logging macros, eg: ?L(info, "something is: ~s", [S]).
-define(LOG(Level, Format, Args), playdar_logger:do_log(?MODULE, Level, Format, Args)).

% lookup config value from *.conf files in playdar's etc dir
-define(CONFVAL(K,Def), playdar_config:confval(K,Def)).

% use by playdar_ctl and ctl commands:
-define(OK, 0).
-define(ERROR, 1).
-define(WTF, 99).

-define(DEFAULT_WEB_PORT, 60210).

% project-wide struct
-record(qry,      {qid,      % query id
                   obj,      % json obj, mochijson2 format
                   local     % true if local user created it, false if via lan etc
                  }).
