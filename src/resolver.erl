-module(resolver).

-behaviour(gen_server).
-include("playdar.hrl").
%% API
-export([start_link/0, dispatch/1, dispatch/2, sid2qid/1, 
         resolvers/0, register_sid/2, add_resolver/5, resolver_pid/1,
         queries/0, add_query_timer/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state,    {queries, sources, resolvers}).

-record(rq,      {qry,       % the #qry struct
                  solved,    % bool - seen a 1.0 result
                  ctime,     % creation time
                  callbacks, % list of funs
                  results,   % list of results
                  timers     % used during pipeline for dispatching
                 }).

-record(resolver, {mod, name, weight, targettime, pid}). %TODO localonly, preference

%% API
start_link()        -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% dispatch a Query, returns the query pid
dispatch(Qry)           -> dispatch(Qry, []).
dispatch(Qry, Callbacks)-> gen_server:call(?MODULE, {dispatch, Qry, Callbacks}).

% associate a source id with a query id:
register_sid(Sid, Qid)  -> gen_server:cast(?MODULE, {register_sid, Sid, Qid}).
% lookup associated qid for a sid
sid2qid(Sid)            -> gen_server:call(?MODULE, {sid2qid, Sid}).

% list all loaded resolvers
resolvers()             -> gen_server:call(?MODULE, resolvers).
% list all active query ids
queries()               -> gen_server:call(?MODULE, queries).

% add a timer, which will be cancelled if the query is marked as solved
add_query_timer(Qid, Tref) -> gen_server:cast(?MODULE, {add_query_timer, Qid, Tref}).

% Helper function, handy for getting pid of library for scanning.
resolver_pid(Mod)  -> 
    case lists:filter(fun(R)->proplists:get_value(mod,R) == Mod end, 
                      resolver:resolvers()) of
        [] -> undefined;
        [List] -> proplists:get_value(pid,List)
    end.

% resolver modules call this to register themselves, get added to the pipeline:
add_resolver(Mod, Name, Weight, TargetTime, Pid) ->
    gen_server:cast(?MODULE,{add_resolver, Mod, Name, Weight, TargetTime, Pid}).



%% gen_server callbacks



init([]) ->
	{R1,R2,R3} = now(),
	random:seed(R1,R2,R3),
    process_flag(trap_exit, true),
    % This ets maps Qids to query objs:
    Tid = ets:new(queries, []),
    % and this one maps Source IDs to query pids
    Tid2= ets:new(sources, []),
    % Load script-resolvers:
    Scripts = ?CONFVAL(scripts, []),
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
    {reply, [Qid||{Qid,_RQ} <- ets:tab2list(State#state.queries)], State};
    
handle_call(resolvers, _From, State) ->
    % the #resolver record is internal to this module
    % so we make a proplist for handing out externally:
    Res = [ [{mod, R#resolver.mod},
             {name, R#resolver.name},
             {weight, R#resolver.weight},
             {targettime, R#resolver.targettime},
             {pid, R#resolver.pid}] || R <- State#state.resolvers ],
    {reply, Res, State};
    
handle_call({sid2qid, Sid}, _From, State) ->
    case ets:lookup(State#state.sources, Sid) of
        [{Sid, Qid}] ->
            {reply, Qid, State};
        [] ->
            {reply, undefined, State}
    end;
    
handle_call({dispatch, Qry, Callbacks}, _From, State) ->
    Qid = Qry#qry.qid,
	% First of all, do nothing if a query with this Qid already exists:
	case ets:lookup(State#state.queries, Qid) of
		[{Qid,_RQ}] ->
			{reply, Qid, State};
		_ ->
            % dispatch the qry:
            RQ  = #rq{qry=Qry, solved=false, ctime=erlang:localtime(), 
                      callbacks=Callbacks, timers=[], results=[]},
            ets:insert(State#state.queries, {Qid, RQ}),
			% dispatch to resolvers
			start_resolver_pipeline(Qry, State#state.resolvers),
			{reply, Qid, State}
	end.


handle_cast({add_query_timer, Qid, Tref}, State) ->
    case ets:lookup(State#state.queries, Qid) of
        [{Qid, RQ}] ->
            % update the RQ with an additional timer reference
            % we cancel this tref if solved->true, which stops the pipeline.
            RQ1 = RQ#rq{timers = [Tref | RQ#rq.timers]},
            ets:insert(State#state.queries, {Qid, RQ1}),
            {noreply, State};
        [] ->
            {noreply, State}
    end;            

handle_cast({add_resolver,Mod, Name, Weight, TargetTime, Pid}, State) ->
    link(Pid),
    ?LOG(info, "add_resolver: ~w '~s' Weight:~w TT:~w Pid:~w",
               [Mod, Name, Weight, TargetTime, Pid]),
    R = #resolver{mod=Mod, name=Name, weight=Weight, targettime=TargetTime, pid=Pid},
    % 4 is the pos in the #resolver tuple for "weight":
    Resolvers = lists:reverse(lists:keysort(4, [R|State#state.resolvers])),
    {noreply, State#state{resolvers=Resolvers}};

handle_cast({register_sid, Sid, Qid}, State) ->
    ?LOG(debug, "Register sid: ~p to qpid: ~p",[Sid, Qid]),
    ets:insert(State#state.sources, {Sid, Qid}),
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

% TODO make this process persist for the duration of the pipeline, instead of
% using timer callbacks. Then register/kill this pid when solved=true instead
% of cancelling a bunch of timers. Seems like it would be more elegant.
start_resolver_pipeline(Qry, Resolvers) when is_record(Qry, qry) ->
    spawn(fun()->
                  start_resolver_pipeline(Qry, Resolvers, {-1, -1})
          end).

start_resolver_pipeline(_, [], _) -> ok;

start_resolver_pipeline(Qry, [H|Resolvers], {LastWeight, LastTime}) when is_record(Qry, qry) ->
    case LastWeight of
        -1 -> % first iteration
            (H#resolver.mod):resolve(H#resolver.pid, Qry),
            start_resolver_pipeline(Qry, Resolvers, {H#resolver.weight, H#resolver.targettime});
        _  -> 
            if
                LastWeight == H#resolver.weight ->
                    % same weight, dispatch immediately
                    (H#resolver.mod):resolve(H#resolver.pid, Qry),
                    start_resolver_pipeline(Qry, Resolvers, {LastWeight, utils:min(H#resolver.targettime, LastTime)});
                true ->
                    % dispatch after delay, save timer ref for possible cancellation if solved
                    {ok, Tref} = timer:apply_after(LastTime, H#resolver.mod, resolve, [H#resolver.pid, Qry]),
                    add_query_timer(Qry#qry.qid, Tref),
                    start_resolver_pipeline(Qry, Resolvers, {H#resolver.weight, H#resolver.targettime})
            end
    end.

