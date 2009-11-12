-module(mplayer_master).
-behaviour(gen_server).

% http://www.mplayerhq.hu/DOCS/tech/slave.txt

%% API
-export([start_link/0, play/1, stop/0, send/1, np/0, get_property/1, pause/0,
		 volume/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {port}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

play(File) 		-> gen_server:cast(?MODULE, {play, File}).
stop()			-> send("stop").
pause()			-> send("pause").
send(Cmd) 		-> gen_server:cast(?MODULE, {cmd, Cmd}).
np() 			-> get_property("path").
volume() 		-> get_property("volume").
%set_volume		
get_property(P)	-> gen_server:call(?MODULE, {get_property, P}).

%% gen_server callbacks

init([]) ->
	Exe = "mplayer -slave -quiet -idle",
	Port = open_port({spawn, Exe}, [{line, 1024}, use_stdio, stderr_to_stdout, hide]),
	port_connect(Port, self()),
    State = #state{port = Port},
    {ok, State}.

handle_call({get_property, P}, _From, State) ->
	Cmd = io_lib:format("pausing_keep get_property ~s\n", [P]),
	port_command(State#state.port, Cmd),
	case receive_line(State) of
		timeout -> {reply, timeout, State};
		Resp ->
			case Resp of
				"Failed to get value of property" ++ _Rest ->
					{reply, null, State};
				Resp1 ->
					% answers look like: 'ANS_<prop>=...'
					Retval = lists:nthtail(length(P)+5, Resp1),
					case Retval of 
						"(null)" -> 
							{reply, null, State};
						_ -> 
							{reply, Retval, State}
					end
			end
	end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({play, File}, State) ->
	F = io_lib:format("loadfile \"~s\"\n", [File]),
	%io:format("Sending: ~s\n", [F]),
	port_command(State#state.port, F),
	{noreply, State};

handle_cast({cmd, Cmd}, State) ->
	CmdN = io_lib:format("pausing_keep ~s\n", [Cmd]),
	port_command(State#state.port, CmdN),
	{noreply, State};

handle_cast(Msg, State) ->
	io:format("Unhandled cast: ~p\n", [Msg]), %% TODO remove
    {noreply, State}.

handle_info({Port, {data, {_Flag, Line}}}, State = #state{port=Port}) ->
	{noreply, handle_line(Line, State)};
	
handle_info(Info, State) ->
	io:format("Unhandled info: ~p\n", [Info]),
    {noreply, State}.

terminate(_Reason, State) ->
	(catch port_command(State#state.port, "quit")),
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------

receive_line(_State = #state{port=Port}) ->
	receive
		{Port, {data, {_Flag, Line}}} ->
			Line
	after 1000 ->
		timeout
	end.
			
handle_line("Starting playback...", State) ->
	io:format("PLAYBACK STARTING\n"),
	State;

handle_line(L, State) ->
	io:format("UNHANDLED LINE: ~s\n", [L]),
	State.
