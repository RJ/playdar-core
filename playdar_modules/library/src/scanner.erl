% Scans dirs and files using the taglib driver, updates library accordingly.
-module(scanner).        
-behaviour(gen_server).
-include_lib("kernel/include/file.hrl").
-include("playdar.hrl").

%% API
-export([start_link/1, scan_dir/2, scan_file/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {libpid, port, lasttime, self, tref, exe}).

% time, in ms, to stop scanner binary if idle:
-define(IDLE_TIMEOUT, 10000). 

%% API
start_link(LibPid)  -> gen_server:start_link(?MODULE, [LibPid], []).
scan_dir(Pid, Dir)  -> gen_server:call(Pid, {scan_dir, Dir},  infinity).
scan_file(Pid, F)   -> gen_server:call(Pid, {scan_file, F}, 30000).

%% gen_server callbacks
init([LibPid]) ->
    process_flag(trap_exit, true),
    Exe = ?CONFVAL({library, taglib_driver}, "playdar_modules/library/priv/taglib_driver/taglib_json_reader"),
    {ok, #state{libpid=LibPid, port=undefined, lasttime=now(), self=self(), tref=undefined, exe=Exe}}.

handle_call({scan_dir, Dir}, From, State) ->
    spawn(fun()-> 
                  Resp = (catch scan(filelib:wildcard(Dir ++ "/*"), State#state.self)),
                  library_dets:sync(State#state.libpid),
                  gen_server:reply(From, Resp)
          end),
    {noreply, State};

handle_call({scan_file, File}, _From, State) ->
    State0 = taglib_drv_ensure_started(State),
    State1 = State0#state{lasttime=now()},
    case file:read_file_info(File) of
        {ok, #file_info{size=Size, mtime=Mtime}} ->
            case readtags(File, State1#state.port) of
                {ok, Tags} ->
                    library_dets:add_file(State#state.libpid, File, Mtime, Size, Tags),
                    {reply, ok, State1};
				{error, scanner_crashed} -> {reply, {error, scanner_crashed}, State#state{port=undefined}};
				{error, scanner_bug}     -> {reply, {error, scanner_bug}, State#state{port=undefined}};
                {error, R} ->
                    {reply, {error, R}, State1}
            end;
        _   -> 
            {reply, error, State1}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

% stop the scanner binary if inactive:
handle_info(port_idle, #state{port=Port, lasttime=LT} = State) when is_port(Port) ->
    Diff = timer:now_diff(now(), LT),
    case Diff > (1000*?IDLE_TIMEOUT) of
        true  ->
            ?LOG(info, "Stopping scanner binary, idle timeout reached.",[]),
            (catch port_close(Port)), 
            {noreply, State#state{port=undefined}};
        false -> {noreply, State}
    end;

handle_info(port_idle, State) -> {noreply, State};
                                                      
handle_info({'EXIT', Port, Reason}, State) when is_port(Port) ->
    ?LOG(error, "Scanner terminated: ~p",[Reason]),
    {noreply, State#state{port=undefined}};

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, #state{port = Port} = _State) when is_port(Port) -> 
	(catch port_close(Port)), ok;
terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

scan([], _Pid)     -> ok;
scan([H|T], Pid)   ->
    %?LOG(info, "Scan(~s)", [H]),
    case filelib:is_dir(H) of
        true  ->    scan(filelib:wildcard(H ++ "/*"), Pid);
        false ->    
            case ?MODULE:scan_file(Pid, H) of
                ok-> ok;
                R -> ?LOG(error, "Error scanning file (~p) ~s", [R,H])
            end
    end,
    scan(T, Pid).

taglib_drv_ensure_started(State = #state{port=Tl}) when is_port(Tl) ->
    State;
taglib_drv_ensure_started(State) ->
    ?LOG(info, "Spawning taglib_driver.",[]),
    timer:cancel(State#state.tref),
    Grace = 5000, % give time for a small scan to finish before timeout counter applies
    {ok, TRef} = timer:send_after(?IDLE_TIMEOUT + Grace, State#state.self, port_idle),
    Exe = ?CONFVAL({library, taglib_driver}, State#state.exe),
    Port = open_port({spawn, Exe}, [binary, {packet, 4}, use_stdio]),
    State#state{port = Port, tref=TRef }.

readtags(File, Port) ->
    ?LOG(info, "readtags: ~s", [File]),
    port_command(Port, File),
    receive
        {Port, {data, Data}} ->
            case (catch mochijson2:decode(Data)) of
                {struct, Tags} ->
                        case proplists:get_value(<<"error">>, Tags) of
                            undefined -> {ok, Tags};
                            _ -> {error, no_tags}
                        end;
                {'EXIT', _} -> % error parsing json
                    ?LOG(warning, "Scanner returned invalid JSON!",[]),
                    {error, scanner_bug}
            end;
        
        {'EXIT', Port, Reason} ->
            ?LOG(error, "SCANNER CRASHED!!!! ~p", [Reason]),
            {error, scanner_crashed}
    
    after 60000 ->
            ?LOG(error, "Scanner timedout reading: ~s", [File]),
            (catch port_close(Port)),
            {error, timeout}
    end.
