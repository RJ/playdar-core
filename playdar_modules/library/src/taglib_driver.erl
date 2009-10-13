% taglib interface to external scanner binary
-module(taglib_driver).
-behaviour(gen_server).
-include("playdar.hrl").
-export([start_link/1, parsefile/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {port, err, exe}).

start_link(BinPath) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [BinPath], []).

parsefile(File) when is_list(File) ->
    gen_server:call(?MODULE, {parsefile, File}, infinity).

%%

init([Exe]) ->
	process_flag(trap_exit, true),
    Port = open_port({spawn, Exe}, [binary, {packet, 4}, use_stdio]),
    Err = {struct, [{<<"error">>, <<"default error">>}]},
    {ok, #state{port=Port, err=Err, exe=Exe}}.

handle_call({parsefile, File}, _From, #state{port=Port} = State) ->
    ?LOG(info, "Scanning ~s", [File]),
    port_command(Port, File),
    receive
        {Port, {data, Data}} ->
            case (catch mochijson2:decode(Data)) of
                {struct, Tags} ->
                    {reply, Tags, State};
                {'EXIT', _} -> % error parsing json
                    ?LOG(warning, "Scanner returned invalid JSON!",[]),
                    {reply, State#state.err, State}
            end;
		
		{'EXIT', Port, Reason} ->
            ?LOG(error, "SCANNER CRASHED!!!! ~p", [Reason]),
            NewPort = open_port({spawn, State#state.exe}, [binary, {packet, 4}, use_stdio]),
			{reply, State#state.err, State#state{port=NewPort}}
    end.

handle_cast(_Msg, State) ->    {noreply, State}.

handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    ?LOG(error, "Scanner died",[]),
    {stop, {port_terminated, Reason}, State}.

terminate({port_terminated, _Reason}, _State) ->    ok;

terminate(_Reason, #state{port = Port} = _State) ->     port_close(Port).

code_change(_OldVsn, State, _Extra) ->     {ok, State}.


