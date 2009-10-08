-module(resolver).

-behaviour(gen_server).
-include("playdar.hrl").
%% API
-export([start_link/0, dispatch/2, dispatch/3, qid2pid/1, sid2pid/1, 
         resolvers/0, register_sid/2, add_resolver/5, resolver_pid/1,
         queries/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {queries, sources, resolvers}).
-record(resolver, {mod, name, weight, targettime, pid}).

%% API
start_link()        -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% dispatch a Query, returns the query pid
dispatch(Q, Qid)        -> dispatch(Q, Qid, []).
dispatch(Q, Qid, Cbs)   -> gen_server:call(?MODULE, {dispatch, Q, Qid, Cbs}).
qid2pid(Qid)            -> gen_server:call(?MODULE, {qid2pid, Qid}).
sid2pid(Sid)            -> gen_server:call(?MODULE, {sid2pid, Sid}).
register_sid(Sid, Qpid) -> gen_server:cast(?MODULE, {register_sid, Sid, Qpid}).
resolvers()             -> gen_server:call(?MODULE, resolvers).
queries()               -> gen_server:call(?MODULE, queries).


% Helper function, handy for getting pid of library for scanning.
resolver_pid(Mod)  -> 
    case lists:filter(fun(R)->proplists:get_value(mod,R) == Mod end, 
                      resolver:resolvers()) of
        [] -> undefined;
        [List] -> proplists:get_value(pid,List)
    end.

add_resolver(Mod, Name, Weight, TargetTime, Pid) ->
    gen_server:cast(?MODULE,{add_resolver, Mod, Name, Weight, TargetTime, Pid}).

%% gen_server callbacks
init([]) ->
	{R1,R2,R3} = now(),
	random:seed(R1,R2,R3),
    process_flag(trap_exit, true),
    % This ets maps Qids to query pids:
    Tid = ets:new(queries, []),
    % and this one maps Source IDs to query pids
    Tid2= ets:new(sources, []),
    % Load script-resolvers:
    Scripts = [ "/home/rj/src/playdar/contrib/demo-script/demo-resolver.php" ],
    ScriptSpecs = [ {list_to_atom(Script), 
                     {script_resolver, start_link, [Script]}, 
                     transient, 5, worker, [script_resolver]} 
                   || Script <- Scripts ],
    ok = supervisor:check_childspecs(ScriptSpecs),
    Specs = ScriptSpecs,    
    % Adding to our own supervisor from here must be done by another proc or
    % it deadlocks:
    lists:foreach(fun(Spec) -> 
                    spawn(supervisor, start_child, [resolver_sup, Spec])
                  end, Specs),
    % Resolvers will call resolver:add_resolver() to register themselves
    % once they've finished their startup routines.
    {ok, #state{    queries=Tid, 
                    sources=Tid2, 
                    resolvers=[]}}.

handle_call(queries, _From, State) ->
    {reply, ets:tab2list(State#state.queries), State};
    
handle_call(resolvers, _From, State) ->
    % the #resolver record is internal to this module
    % so we make a proplist for handing out externally:
    Res = [ [{mod, R#resolver.mod},
             {name, R#resolver.name},
             {weight, R#resolver.weight},
             {targettime, R#resolver.targettime},
             {pid, R#resolver.pid}] || R <- State#state.resolvers ],
    {reply, Res, State};

handle_call({qid2pid, Qid}, _From, State) ->
    case ets:lookup(State#state.queries, {qid, Qid}) of
        [{_, Pid}] ->
            {reply, Pid, State};
        _ ->
            {reply, undefined, State}
    end;
    
handle_call({sid2pid, Sid}, _From, State) ->
    case ets:lookup(State#state.sources, {sid, Sid}) of
        [{_, Pid}] ->
            {reply, Pid, State};
        _ ->
            {reply, undefined, State}
    end;
    
handle_call({dispatch, Q, Qid, Cbs}, _From, State) ->
	% First of all, do nothing if a query with this Qid already exists:
	case ets:lookup(State#state.queries, {qid, Qid}) of
		[P] when is_pid(P) ->
			{reply, P, State};
		_ ->
			% spawn a qry process to hold this query:
			{ok, Pid} = gen_server:start(qry, [Q, Qid], []),
			% register callbacks before triggering search:
			lists:foreach(fun(C) -> qry:add_result_callback(Pid, C) end, Cbs),
			% keep track of qid -> qry_pid
			ets:insert(State#state.queries, {{qid, Qid}, Pid}),
			% dispatch to resolvers
			start_resolver_pipeline(Q, Pid, State#state.resolvers),
			{reply, Pid, State}
	end.


handle_cast({add_resolver,Mod, Name, Weight, TargetTime, Pid}, State) ->
    link(Pid),
    ?LOG(info, "add_resolver: ~w '~s' Weight:~w TT:~w Pid:~w",
               [Mod, Name, Weight, TargetTime, Pid]),
    R = #resolver{mod=Mod, name=Name, weight=Weight, targettime=TargetTime, pid=Pid},
    % 4 is the pos in the #resolver tuple for "weight":
    Resolvers = lists:reverse(lists:keysort(4, [R|State#state.resolvers])),
    {noreply, State#state{resolvers=Resolvers}};

handle_cast({register_sid, Sid, Qpid}, State) ->
    ?LOG(debug, "Register sid: ~p to qpid: ~p",[Sid, Qpid]),
    ets:insert(State#state.sources, {{sid, Sid}, Qpid}),
    {noreply, State}.

handle_info({'EXIT', Pid, _Reason}, State) ->
    % remove this crashed resolver from our list,
    % the supervisor will restart it as necessary:
    R = lists:filter( fun(X)-> X#resolver.pid /= Pid end, State#state.resolvers),
    {noreply, State#state{resolvers=R}};
           
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

start_resolver_pipeline(Q, Qpid, Resolvers) ->
    start_resolver_pipeline(Q, Qpid, Resolvers, {-1, -1}).

start_resolver_pipeline(_, _, [], _) -> ok;

start_resolver_pipeline(Q, Qpid, [H|Resolvers], {LastWeight, LastTime}) ->
    case LastWeight of
        -1 -> % first iteration
            (H#resolver.mod):resolve(H#resolver.pid, Q, Qpid),
            start_resolver_pipeline(Q, Qpid, Resolvers, {H#resolver.weight, H#resolver.targettime});
        _  -> 
            if
                LastWeight == H#resolver.weight ->
                    % same weight, dispatch immediately
                    (H#resolver.mod):resolve(H#resolver.pid, Q, Qpid),
                    start_resolver_pipeline(Q, Qpid, Resolvers, {LastWeight, utils:min(H#resolver.targettime, LastTime)});
                true ->
                    % dispatch after delay, save timer ref for possible cancellation if solved
                    {ok, Tref} = timer:apply_after(LastTime, H#resolver.mod, resolve, [H#resolver.pid, Q, Qpid]),
                    qry:add_timer(Qpid, Tref),
                    start_resolver_pipeline(Q, Qpid, Resolvers, {H#resolver.weight, H#resolver.targettime})
            end
    end.

