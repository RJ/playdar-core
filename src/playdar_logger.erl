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


http_req(Req, _DocRoot) ->   
    % tell the logger to send us all log msgs.
    % will auto-unregister when our process exits:
    Pid = self(),
	Qs = Req:parse_qs(),
	Levels = case proplists:get_value("levels", Qs, "") of
		""  -> all;
		Str ->
			[ list_to_atom(S) || S <- string:tokens(Str, ",") ]
	end,
    playdar_logger:register_logger(fun(Date, Level, Mod, Line) ->
                                    Pid ! {Date, Level, Mod, Line}
                                   end, Levels, Pid),
    Response = Req:ok({"text/html; charset=utf-8",
                       [{"Server","Mochiweb-Test"}],
                       chunked}),
    % first bit of html we'll send:
    First = "<html><head><title>Playdar Logger Output</title></head><body>"
			"<h1>Playdar Log Output</h1>"
			"<h2>Levels: "  ++ io_lib:format("~p", [Levels]) ++ "</h2>"
            "<p>This will only update as new lines are logged, so go do something then check this page</p>"
            "<table>",    
    Response:write_chunk(First),
    feed_logger(Response, 0).

feed_logger(Response, N) ->
    receive
        {Date, Level, Mod, Line} ->
            Col = case N rem 2 of
                      1 -> "white";
                      0 -> "lightgrey"
                  end,
            Html = io_lib:format("<tr style=\"background-color: " ++ Col ++ "\"><td nowrap>~w</td><td nowrap>~s</td><td nowrap>~w<br/>~w</td><td>~s</td></tr>",
                                 [N, Date, Level, Mod, Line]),
            Response:write_chunk(Html),
            feed_logger(Response, N+1)
    end.
    

