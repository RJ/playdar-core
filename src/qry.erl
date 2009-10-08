%% Represents a query, receives and holds all matches
-module(qry).
-behaviour(gen_server).
-include("playdar.hrl").
-define(MIN_SCORE, 0.5).

%% API
-export([start/1, start/2]).
-export([add_result/2, add_results/2, results/1, qid/1, add_timer/2, 
         mark_as_solved/1, solved/1, q/1, result/2, add_result_callback/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {q, qid, results, timers, solved, callbacks}).

%% API
start(Q, Qid)               -> gen_server:start(?MODULE, [Q, Qid], []).
start(Q)                    -> gen_server:start(?MODULE, [Q, utils:uuid_gen()], []).
add_result(Pid, Result)     -> add_results(Pid, [Result]).
add_results(Pid, Results)   -> gen_server:cast(Pid, {add_results, Results}).
results(Pid)                -> gen_server:call(Pid, {results}).
result(Pid, Sid)            -> gen_server:call(Pid, {result, Sid}).
qid(Pid)                    -> gen_server:call(Pid, {qid}).
add_timer(Pid, Tref)        -> gen_server:cast(Pid, {add_timer, Tref}).
solved(Pid)                 -> gen_server:call(Pid, {solved}).
mark_as_solved(Pid)         -> gen_server:cast(Pid, {mark_as_solved}).
q(Pid)                      -> gen_server:call(Pid, {q}).
add_result_callback(Pid, C) -> gen_server:cast(Pid, {add_result_callback, C}).

%% gen_server callbacks
init([Q, Qid]) ->
	{R1,R2,R3} = now(),
	random:seed(R1,R2,R3),
    {ok, #state{    q = Q, 
                    qid = Qid, 
                    results = [], 
                    timers = [], 
                    solved = false, 
                    callbacks = [] }}.

handle_call({q}, _From, State)      -> {reply, State#state.q, State};
handle_call({qid}, _From, State)    -> {reply, State#state.qid, State};
handle_call({results}, _From, State)-> {reply, State#state.results, State};
handle_call({solved}, _From, State) -> {reply, State#state.solved, State};

% get the source proplist from the full list, by sid
handle_call({result, Sid}, _From, State) ->
    L = lists:filter( fun({struct, E}) -> proplists:get_value(<<"sid">>, E) == Sid end,
                      State#state.results
                    ),
    case L of
        [] -> {reply, undefined, State};
        _  -> {reply, hd(L), State}
    end.

handle_cast({add_result_callback, Cb}, State) ->
    {noreply, State#state{callbacks= [Cb|State#state.callbacks]}};

handle_cast({mark_as_solved}, State) ->
    [ timer:cancel(Tref) || Tref <- State#state.timers ],
    {noreply, State#state{timers=[], solved=true}};

handle_cast({add_timer, Tref}, State) ->
    case State#state.solved of
        true  -> timer:cancel(Tref), {noreply, State};
        _ -> {noreply, State#state{timers=[Tref | State#state.timers]}}
    end;

handle_cast({add_results, Results}, State) ->
    Uuify = fun(A) ->
        Score = proplists:get_value(<<"score">>, A),
        case Score of
            1.0 -> qry:mark_as_solved(self());
            _ -> noop
        end,
        % give it a sid if one isn't provided:
        Sid = proplists:get_value(<<"sid">>, A),
        case Sid of
            undefined -> 
                Uuid = utils:uuid_gen(),
                resolver:register_sid(Uuid, self()),
                [{<<"sid">>, Uuid} | A];
            _  -> 
                resolver:register_sid(Sid, self()),
                A % already has a sid
        end
    end,
    ?LOG(debug, "add_results ~s ~w",  [State#state.qid, length(Results)]),
    Results1 = State#state.results ++ 
        [   begin Item = {struct, Uuify(R)},
                  lists:foreach(fun(Cb)->Cb(Item)end,State#state.callbacks),
                  Item
            end || {struct, R} <- Results, 
                   proplists:get_value(<<"score">>, R, 0) >= ?MIN_SCORE ],
    {noreply, State#state{results=Results1}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

