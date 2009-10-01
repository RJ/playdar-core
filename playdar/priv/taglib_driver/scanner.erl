-module(scanner).
-behaviour(gen_server).
-export([start_link/1, parsefile/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {port}).

start_link(BinPath) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [BinPath], []).

parsefile(File) ->
    gen_server:call(?MODULE, {parsefile, File}).

%%

init([Exe]) ->
    Port = open_port({spawn, Exe}, [binary, {packet, 4}, use_stdio]),
    {ok, #state{port=Port}}.

handle_call({parsefile, File}, _From, #state{port=Port} = State) ->
    Port ! {self(), {command, File}},
    receive
        {Port, {data, Data}} ->
            {reply, Data, State};
        X -> {reply, X, State}
        after 1000 -> % if it takes this long, you have serious issues.
            {stop, port_timeout, State}
    end.

handle_cast(_Msg, State) ->    {noreply, State}.

handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, {port_terminated, Reason}, State}.

terminate({port_terminated, _Reason}, _State) ->    ok;

terminate(_Reason, #state{port = Port} = _State) ->     port_close(Port).

code_change(_OldVsn, State, _Extra) ->     {ok, State}.


