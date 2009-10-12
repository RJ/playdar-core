% taglib interface to external scanner binary
-module(taglib_driver).
-behaviour(gen_server).
-include("playdar.hrl").
-export([start_link/1, parsefile/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {port}).

start_link(BinPath) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [BinPath], []).

parsefile(File) when is_list(File) ->
    gen_server:call(?MODULE, {parsefile, File}, infinity).

%%

init([Exe]) ->
	process_flag(trap_exit, true),
    Port = open_port({spawn, Exe}, [binary, {packet, 4}, use_stdio]),
    {ok, #state{port=Port}}.

handle_call({parsefile, File}, _From, #state{port=Port} = State) ->
    % TODO: if this file crashes the scanner binary, trap it here
    %       return "no tags" then restart the binary
	?LOG(info, "Scanning ~s", [File]),
    port_command(Port, File),
    receive
        {Port, {data, Data}} ->
            {struct, Tags} = mochijson2:decode(Data),
            {reply, Tags, State};
		
		{'EXIT', Port, Reason} ->
			?LOG(error, "SCANNER CRASHED!!!! ~p", [Reason]),
			{stop, Reason, State}
    end.

handle_cast(_Msg, State) ->    {noreply, State}.

handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, {port_terminated, Reason}, State}.

terminate({port_terminated, _Reason}, _State) ->    ok;

terminate(_Reason, #state{port = Port} = _State) ->     port_close(Port).

code_change(_OldVsn, State, _Extra) ->     {ok, State}.


