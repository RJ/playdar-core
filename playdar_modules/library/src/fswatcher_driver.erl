% taglib interface to external scanner binary
-module(fswatcher_driver).
-behaviour(gen_server).
-include("playdar.hrl").
-export([start_link/2, watch/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {port, librarypid, dirs}).

start_link(LibraryPid, Dirs) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [LibraryPid, Dirs], []).

watch(Dir) ->
    gen_server:call(?MODULE, {restart_port, Dir}).

start_fswatcher_port([]) ->
    undefined;

start_fswatcher_port(Dirs) ->
    Exe = "playdar_modules/library/priv/fswatcher_driver/fswatcher",
    open_port({spawn_executable, Exe}, [{args, Dirs}, binary, {line, 2048}, use_stdio]).
%%

init([LibraryPid, Dirs]) ->
    {ok, #state{port = start_fswatcher_port(Dirs), librarypid = LibraryPid, dirs = Dirs}}.

handle_call({restart_port, Dir}, _From, #state{port = Port} = State) ->
    (catch port_close(Port)),
    Dirs = State#state.dirs ++ [Dir],
    {reply, {Dir, dir_added}, State#state{port = start_fswatcher_port(Dirs), dirs = Dirs}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, {data, {_, Line}}}, #state{port = Port} = State) ->
    File = binary_to_list(Line),
    library_dets:file_changed(State#state.librarypid, File),
    {noreply, State};

handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, {port_terminated, Reason}, State}.

terminate({port_terminated, _Reason}, _State) ->
    ok;

terminate(_Reason, #state{port = Port} = _State) ->
    (catch port_close(Port)),
    exit(Port, interrupt).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
