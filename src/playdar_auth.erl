-module(playdar_auth).
-behaviour(gen_server).
-include("playdar.hrl").

%% API
-export([start_link/0, gen_formtoken/0, consume_formtoken/1, 
         check_auth/1, create/2, all/0, revoke/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {tokdb, authdb}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

gen_formtoken() ->
    gen_server:call(?MODULE, gen_formtoken).

consume_formtoken(Token) when is_list(Token) ->
    gen_server:call(?MODULE, {consume_formtoken, Token}).

check_auth(Token) when is_list(Token) -> check_auth(list_to_binary(Token));
check_auth(Token) when is_binary(Token) ->
    gen_server:call(?MODULE, {check_auth, Token}).

create(Token, Props) when is_binary(Token) ->
    gen_server:call(?MODULE, {create, Token, Props}).

all() ->
    gen_server:call(?MODULE, all).

revoke(Token) when is_binary(Token) ->
    gen_server:call(?MODULE, {revoke, Token}).

%% gen_server callbacks

init([]) ->
    Dir = ?CONFVAL(authdbdir, "."),
    {ok, D} = dets:open_file(Dir ++ "/auth.db",[]),
    {ok, #state{    
                    tokdb=ets:new(tokdb,[]), 
                    authdb=D
               }}.

handle_call({revoke, Token}, _From, State) ->
    dets:delete(State#state.authdb, Token),
    {reply, ok, State};

handle_call(all, _From, State) ->
    All = dets:foldl(fun(E,Acc)-> [E|Acc] end, [], State#state.authdb),
    {reply, All, State};

handle_call({create, Token, Props}, _From, State) ->
    dets:insert(State#state.authdb, {Token, Props}),
    {reply, ok, State};

handle_call({check_auth, Token}, _From, State) ->
    case dets:lookup(State#state.authdb, Token) of
        [{Token, Props}] -> {reply, Props, State};
        _ -> {reply, undefined, State}
    end;

handle_call({consume_formtoken, Token}, _From, State) ->
    case ets:lookup(State#state.tokdb, Token) of
        [] -> {reply, undefined, State};
        [{Token,_}] -> 
            ets:delete(State#state.tokdb, Token),
            {reply, ok, State}
    end;

handle_call(gen_formtoken, _From, State) ->
    Tok = binary_to_list(utils:uuid_gen()),
    ets:insert(State#state.tokdb, {Tok, now()}),
    {reply, Tok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

