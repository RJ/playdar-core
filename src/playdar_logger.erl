-module(playdar_logger).
-include("playdar.hrl").
-behaviour(gen_server).

-export([start_link/0, do_log/4, register_logger/3]).

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

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

default_logger(Date, Level, Mod, Line) ->
	io:format("~s ~w ~w ~s~n", [Date, Level, Mod, Line]).