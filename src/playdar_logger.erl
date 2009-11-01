-module(playdar_logger).
-include("playdar.hrl").
-behaviour(gen_server).

-export([start_link/0, do_log/4, register_logger/3]).
-export([http_req/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {loggers}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% called by the ?LOG macro:
do_log(Mod, Log_Level, Log_Format, Log_Args) ->
	gen_server:cast(?MODULE, {do_log, Mod, Log_Level, Log_Format, Log_Args}).

register_logger(Fun, Levels, Pid) ->
	gen_server:call(?MODULE, {register_logger, Fun, Levels, Pid}).

%% ====================================================================
%% Server functions
%% ====================================================================
init([]) ->
	process_flag(trap_exit, true),
	spawn(fun()->
		playdar_logger:register_logger(fun default_logger/4, all, undefined)
	end),
    {ok, #state{loggers=[]}}.

handle_call({register_logger, Fun, Levels, Pid}, _From, State) ->
	State1 = State#state{ loggers = [ {Pid, Levels, Fun} | State#state.loggers ] },
	if
		is_pid(Pid) -> link(Pid);
		true -> ok
	end,
    {reply, ok, State1}.

handle_cast({do_log, Mod, Level, Format, Args}, State) ->
    {{Y,M,D},{Hr,Min,Sec}} = erlang:localtime(),
	DateStr = io_lib:format("~w/~2..0w/~2..0w ~2..0w:~2..0w:~2..0w",
							[Y, M, D, Hr, Min, Sec]),
	Line = io_lib:format(Format, Args),
	lists:foreach(fun ({_Pid, Levels, Fun})->
					case Levels == all orelse lists:member(Level, Levels) of
						true ->
							Fun(DateStr, Level, Mod, Line);
						false -> ok
					end
				  end, State#state.loggers),
	{noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, _Reason}, State) ->
    ?LOG(info, "Removed logger due to pid exiting: ~p", [Pid]),
    Loggers1 = lists:keydelete(Pid, 1, State#state.loggers),
    State1 = State#state{loggers = Loggers1},
    {noreply, State1}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

default_logger(Date, Level, Mod, Line) ->
	io:format("~s ~w ~w ~s~n", [Date, Level, Mod, Line]).


http_req(Req, DocRoot) ->
	Qs = Req:parse_qs(),
	DefaultLevels = "info,warning,error",
	LevStr = proplists:get_value("levels", Qs, DefaultLevels),
	HighlightModule = proplists:get_value("module", Qs, ""),
	Levels = case LevStr of
						 "all"  -> all;
						 Str    ->
							 [ list_to_atom(S) || S <- string:tokens(Str, ",") ]
					 end,
	case Req:get(path) of
		"/logger" -> 
			Url = "/logger/feed?levels=" ++ LevStr,
			Vars = [{iframe_url, Url}, {levels, LevStr}, {highlight_module, HighlightModule}],
			playdar_web:render(Req, DocRoot ++ "/logger.html", Vars);
		"/logger/feed" ->
			Pid = self(),
			playdar_logger:register_logger(fun(Date, Level, Mod, Line) ->
												   Pid ! {Date, Level, Mod, Line}
										   end, Levels, Pid),
			Response = Req:ok({"text/html; charset=utf-8",
							   [{"Server","Mochiweb-Test"}],
							   chunked}),
			% first bit of html we'll send:
			First = "<html><head><title>Playdar Iframe logger feed</title></head><body>",    
			Response:write_chunk(First),
			feed_logger(Response)
	end.

feed_logger(Response) ->
    receive
        {Date, Level, Mod, Line} ->
            Html = io_lib:format("<script language=\"javascript\">"
								 "parent.log(\"~s\",\"~w\",\"~w\",\"~s\");"
								 "</script>\n",
                                 [Date, Level, Mod, slashes(Line)]),
            Response:write_chunk(Html),
            feed_logger(Response)
    end.
    
slashes(S) -> erlydtl_filters:escapejs(erlydtl_filters:escapejs(S)).

