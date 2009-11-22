%% This is one of the key processes. It loads up all the resolver modules and
%% adds them to the resolver pipeline. It is the entry point for dispatching
%% new queries, which it runs thru the resolver pipeline. Resolvers report 
%% back any results they find to this process, and it maintains all active
%% queries and results in ETS tables.
%%
%% Queries are uniquely identified by GUIDs called Query IDs, "Qids"
%% Results to queries also have GUIDs called Source IDs, "Sids"
-module(playdar_resolver).
-behaviour(gen_server).
-include("playdar.hrl").
-define(MIN_SCORE, 0.6).

%% API
-export([start_link/0, dispatch/1, dispatch/2, sid2qid/1, 
         resolvers/0, register_sid/2, add_resolver/2, resolver_pid/1,
         queries/0, add_results/2, results/1, result/1,
		 solved/1, register_query_observer/2, gc/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state,    {queries, sources, resolvers}).

% how we internally represent an active query:
-record(rq,      {qry,        % the #qry struct (see playdar.hrl)
                  solved,     % bool - seen a 1.0 result
                  ctime,      % creation time
                  callbacks,  % list of funs to fire per result
                  observers,  % list of pids to broadcast qry events to
                  results,    % list of results
                  pipelinepid % pid of process running the pipeline
                 }).

-record(resolver, {mod, name, weight, targettime, pid, localonly}).

%% API

start_link()        -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% dispatch a Query, returns the query pid
dispatch(Qry)           -> dispatch(Qry, []).
dispatch(Qry, Callbacks)-> gen_server:call(?MODULE, {dispatch, Qry, Callbacks}).

% associate a source id with a query id:
register_sid(Sid, Qid)  -> gen_server:cast(?MODULE, {register_sid, Sid, Qid}).

sid2qid(Sid)            -> gen_server:call(?MODULE, {sid2qid, Sid}).

% list all loaded resolvers
resolvers()             -> gen_server:call(?MODULE, resolvers).

% list all active query ids
queries()               -> gen_server:call(?MODULE, queries).

% get pid of loaded resolver module (mainly for debugging)
resolver_pid(Mod)  -> 
    case lists:filter(fun(R)->proplists:get_value(mod,R) == Mod end, 
                      ?MODULE:resolvers()) of
        [] -> undefined;
        [List] -> proplists:get_value(pid,List)
    end.

% resolver modules call this to register themselves, get added to the pipeline:
add_resolver(Mod, Pid) -> gen_server:cast(?MODULE,{add_resolver, Mod, Pid}).

% resolvers call this to report new results:
add_results(Qid, Result) when is_tuple(Result)  -> add_results(Qid, [Result]);
add_results(Qid, Results) when is_list(Results) -> gen_server:cast(?MODULE, {add_results, Qid, Results}).

% get all results for this query:
results(Qid) -> gen_server:call(?MODULE, {results, Qid}).

% get specific result object for this sid:
result(Sid)  -> gen_server:call(?MODULE, {result, Sid}).

% bool: is qry solved
solved(Qid) -> gen_server:call(?MODULE, {solved, Qid}).

% this pid will be sent all incoming results for query
register_query_observer(Qid, Pid) -> 
    gen_server:call(?MODULE, {register_query_observer, Qid, Pid}).

gc(Age) -> gen_server:cast(?MODULE, {gc, Age}).

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
                     {playdar_script_resolver, start_link, [Script]}, 
                     transient, 5, worker, [playdar_script_resolver]} 
                   || Script <- Scripts ],
    ok = supervisor:check_childspecs(ScriptSpecs),
    Specs = ScriptSpecs,    
    % Adding to our own supervisor from here must be done by another proc or
    % it deadlocks:
    lists:foreach(fun(Spec) -> 
                    spawn(supervisor, start_child, [playdar_resolver_sup, Spec])
                  end, Specs),
    % Resolvers will call playdar_resolver:add_resolver() to register themselves
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
             {pid, R#resolver.pid},
			 {localonly, R#resolver.localonly}] || R <- State#state.resolvers ],
    {reply, Res, State};

handle_call({solved, Qid}, _From, State) ->
	case ets:lookup(State#state.queries, Qid) of
		[{Qid, RQ}] ->
			{reply, RQ#rq.solved, State};
		[] ->
			{reply, false, State}
	end;

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
			P = start_resolver_pipeline(Qry, State#state.resolvers),
            RQ  = #rq{qry=Qry, solved=false, ctime=erlang:localtime(), 
                      callbacks=Callbacks, results=[], pipelinepid=P,
                      observers=[]},
            ets:insert(State#state.queries, {Qid, RQ}),
			{reply, Qid, State}
	end;

handle_call({results, Qid}, _From, State) ->
	case ets:lookup(State#state.queries, Qid) of
		[{Qid, RQ}] when is_record(RQ, rq) ->
			Results = sort_results(RQ#rq.results),
			Ret = {Results, RQ#rq.qry, RQ#rq.solved}, 
			{reply, Ret, State};
		[] ->
			{reply, undefined, State}
	end;

% get the source proplist from the full list, by sid
handle_call({result, Sid}, _From, State) ->
	case ets:lookup(State#state.sources, Sid) of
		[{Sid, Qid}] ->
			case ets:lookup(State#state.queries, Qid) of
				[{Qid, #rq{results = Results}}] ->
					L = lists:filter( fun({struct, E}) -> 
											  proplists:get_value(<<"sid">>, E) == Sid 
									  end, Results ),
					case L of
						[] -> {reply, undefined, State};
						[R]-> {reply, R, State}
					end;
				[] ->
					{reply, undefined, State}
			end;
		[] ->
			{reply, undefined, State}
	end;

handle_call({register_query_observer, Qid, Pid}, _From, State) ->
	case ets:lookup(State#state.queries, Qid) of
        [{Qid, RQ}] ->
			RQ1 = RQ#rq{observers=[Pid|RQ#rq.observers]},
			ets:insert(State#state.queries, {Qid, RQ1}),
			{reply, ok, State};
		[] -> 
			{reply, undefined, State}
	end.


handle_cast({gc, Age}, State) ->
	Now = calendar:datetime_to_gregorian_seconds(erlang:localtime()),
	F = fun({_Qid, El}, Acc) ->
				Date = calendar:datetime_to_gregorian_seconds(El#rq.ctime),
				Expired = Date+Age =< Now,
				case Expired of
					true ->
						[El|Acc];
					false ->
						Acc
				end
		end,
	RQs = ets:foldl(F, [], State#state.queries),
	?LOG(info, "Deleting ~w expired queries", [length(RQs)]),
	% TODO modules can register to get this list, pre-deletion.
	lists:foreach(
	  fun(RQ) ->
		Qid = (RQ#rq.qry)#qry.qid,
		lists:foreach(fun({struct, E}) -> 
						Sid = proplists:get_value(<<"sid">>, E),
						?LOG(info, "Removing sid->qid: {~s,~s}", [Sid, Qid]),
						ets:delete(State#state.sources, Sid)
				  	  end, RQ#rq.results ),
		?LOG(info, "Removing qid: ~s", [Qid]),
		ets:delete(State#state.queries, Qid)
	  end, RQs),	
	{noreply, State};

handle_cast({add_resolver,Mod, Pid}, State) ->
    link(Pid),
	Name 		= Mod:name(Pid),
	Weight 		= ?CONFVAL({Mod, weight}, Mod:weight(Pid)),
	TargetTime 	= ?CONFVAL({Mod, targettime}, Mod:targettime(Pid)),
	LocalOnly	= ?CONFVAL({Mod, localonly}, Mod:localonly(Pid)),
    ?LOG(info, "add_resolver: ~w '~s' Weight:~w TT:~w Pid:~w Localonly:~w",
               [Mod, Name, Weight, TargetTime, Pid, LocalOnly]),
    R = #resolver{
				  mod=Mod, 
				  name=Name, 
				  weight=Weight, 
				  targettime=TargetTime, 
				  pid=Pid,
				  localonly=LocalOnly
				 },
    % 4 is the pos in the #resolver tuple for "weight":
    Resolvers = lists:reverse(lists:keysort(4, [R|State#state.resolvers])),
    {noreply, State#state{resolvers=Resolvers}};

handle_cast({register_sid, Sid, Qid}, State) ->
    ?LOG(debug, "Register sid: ~p to qpid: ~p",[Sid, Qid]),
    ets:insert(State#state.sources, {Sid, Qid}),
    {noreply, State};

% resolvers call this to report new results:
handle_cast({add_results, Qid, Results}, State) ->
    case ets:lookup(State#state.queries, Qid) of
        [{Qid, RQ}] ->
            {Results1, Solved, Sids} = tidy_results(RQ, Results),
			% remove results below min_score
			Results2 = lists:filter(fun({struct, E}) -> 
											proplists:get_value(<<"score">>, E, 1.0) >= ?MIN_SCORE
									end, Results1),
            case Results2 of 
                [] ->
                    {noreply, State};
                _ ->
                    ?LOG(debug, "add_results ~s (~w)",  [Qid, length(Results2)]),
                    % add new results, and update the "solved" field if appropriate
                    RQ1 = RQ#rq{results = Results2 ++ RQ#rq.results,
                                solved = RQ#rq.solved or Solved},
                    % Did we just change the solved status to true? (will never go true->false)
                    case NewlySolved = (RQ#rq.solved /= RQ1#rq.solved) of
                        true -> 
                            ?LOG(info, "SOLVED, aborting remaining pipeline for ~s", [Qid]),
                            % don't bother asking any other resolvers about this qry
                            erlang:exit(RQ#rq.pipelinepid, solved);						
                        false -> noop
                    end,					
                    ets:insert(State#state.queries, {Qid, RQ1}),
                    % register the new SIDs
                    ets:insert(State#state.sources, [ {Sid, Qid} || Sid <- Sids]),
                    % broadcast to all observers of this query
                    %?LOG(info, "Newly solved: ~w Qid: ~s", [NewlySolved, Qid]),
                    lists:foreach(fun(ObsPid) -> case NewlySolved of
                                                     true -> ObsPid ! solved;
                                                     false -> noop
                                                 end,
                                                 ObsPid ! {results, Qid, Results2}
                                  end,
                                  RQ#rq.observers),
                    % fire callbacks
                    lists:foreach(fun(Cb)->
                                          lists:foreach(fun(R)->
                                                                Cb(R)
                                                        end, Results2)
                                  end, RQ1#rq.callbacks ),
                    {noreply, State}
            end;
        [] ->
            ?LOG(warning, "add_results to invalid QID ~s",  [Qid]),
            {noreply, State}
    end.

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


% The pipeline passes the qry to all resolvers in sequence, with appropriate
% delays based on resolver configuration. This process is killed by the 
% main resolver if the query is solved.
start_resolver_pipeline(Qry, Resolvers) when is_record(Qry, qry) ->
    spawn(fun()->run_pipeline(Qry, Resolvers, {-1, -1})end).

run_pipeline(_, [], _) -> ok;

% skip any resolvers that specify localonly=true if this is a non-local query:
run_pipeline(Qry = #qry{local=Qlocal}, [H|Resolvers], {LastWeight, LastTime}) 
when is_record(Qry, qry), H#resolver.localonly == true, Qlocal == false -> 
	?LOG(pipeline, "Skipping resolver ~w (localonly)", [H#resolver.mod]),
	run_pipeline(Qry, Resolvers, {LastWeight, LastTime});

% first run, lastweight=-1 ... dispatch to the first resolver.
run_pipeline(Qry, [H|Resolvers], {-1, _LastTime}) 
when is_record(Qry, qry) ->
	?LOG(pipeline, "Dispatching to ~s", [H#resolver.name]),
	(H#resolver.mod):resolve(H#resolver.pid, Qry),
    run_pipeline(Qry, Resolvers, {H#resolver.weight, H#resolver.targettime});

run_pipeline(Qry, [H|Resolvers], {LastWeight, LastTime}) 
when is_record(Qry, qry) ->
	case LastWeight == H#resolver.weight of
		true ->
			% same weight, dispatch immediately
			?LOG(pipeline, "Dispatching to ~s", [H#resolver.name]),
			(H#resolver.mod):resolve(H#resolver.pid, Qry),
			Time = erlang:min(H#resolver.targettime, LastTime),
			run_pipeline(Qry, Resolvers, {LastWeight, Time});
		false ->
			timer:sleep(LastTime),
			?LOG(pipeline, "Dispatching to ~s", [H#resolver.name]),
			(H#resolver.mod):resolve(H#resolver.pid, Qry),
			run_pipeline(Qry, Resolvers, {H#resolver.weight, H#resolver.targettime})
	end.



% results are sorted primarily by score, if scores are equal, by preference.
sort_results([]) -> [];
sort_results(Results) when is_list(Results) ->
	lists:sort(fun results_sorter/2, Results).

results_sorter({struct, A}, {struct, B}) ->
	Ascore = proplists:get_value(<<"score">>, A, 0),
	Bscore = proplists:get_value(<<"score">>, B, 0),
	case Ascore == Bscore of
		false ->
			Ascore > Bscore;
		true ->
			Apref = proplists:get_value(<<"preference">>, A, 0),
			Bpref = proplists:get_value(<<"preference">>, B, 0),
			Apref > Bpref
	end.


% given list of results from a resolver, it returns a 3-tuple:
% {Results1, Solved, Sids}
% Results1: new list of results, with added sids / sanitized
% Solved: did any of these results solve the query? (ie, 1.0 score)
% Sids: list of all sids from Results
%
% This isn't especially pretty, but it lets us do it all in one pass
%
tidy_results(#rq{ qry=Qry }, Results) ->
    {struct, Q} = Qry#qry.obj,
	F = fun({struct, R}, {Rs, Sol, Sids}) ->
		    % decide if this solves the qry
            {Solved, R1} = case proplists:get_value(<<"score">>, R) of
                               undefined ->
                                   Gv = fun(Prop, Lst) ->
                                                playdar_utils:clean(
                                                  proplists:get_value(Prop, Lst, <<"">>))
                                        end,
                                   ArtQ = Gv(<<"artist">>, Q),
                                   TrkQ = Gv(<<"track">>,  Q),
                                   ArtR = Gv(<<"artist">>, R),
                                   TrkR = Gv(<<"track">>,  R),
                                   Sc = playdar_utils:calc_score({ArtQ, ArtR}, {TrkQ, TrkR}),
                                   {Sc >= 0.99, [ {<<"score">>, Sc} | R ]};
                               1.0 ->
                                   {true, R};
                               _ ->
                                   {false, R}
                           end,
            % add a sid if one doesnt exist
            {R2, Sid} = case proplists:get_value(<<"sid">>, R1) of
                            undefined ->
                                Uuid = playdar_utils:uuid_gen(),
                                {[{<<"sid">>, Uuid} | R1], Uuid};
                            S ->
                                {R1, S}
                        end,               
            {[{struct, R2}|Rs], Sol or Solved, [Sid|Sids]}
		end,
	lists:foldl(F, {[], false, []}, Results).
