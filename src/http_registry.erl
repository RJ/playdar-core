% Keeps a map of url prefix -> module/handler
% so plugins can register to handle certain web requests.
-module(http_registry).

-behaviour(gen_server).

%% API
-export([start_link/0, get_handler/1, register_handler/2, get_all/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {db}).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_handler(Url) -> 
    gen_server:call(?MODULE, {get_handler, Url}).

register_handler(Urlprefix, Fun) -> 
    gen_server:cast(?MODULE, {register, Urlprefix, Fun}).

get_all() -> gen_server:call(?MODULE, all).

%% gen_server callbacks
init([]) ->
    P = ets:new(db, []),
    % hardcoded handlers that ship by default:
    register_handler("api", fun playdar_http_api:http_req/1),
    {ok, #state{db=P}}.

handle_call(all, _From, State) ->
    Paths = [ P || {P,_} <- ets:tab2list(State#state.db) ],
    {reply, Paths, State};


handle_call({get_handler, Url}, _From, State) ->
    case ets:lookup(State#state.db, prefix(Url)) of
        []          -> {reply, undefined, State};
        [{_,Fun}]   -> {reply, Fun, State}
    end.

handle_cast({register, Urlprefix, Fun}, State) ->
    ets:insert(State#state.db, {Urlprefix, Fun}),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

% Eg: /api/?method=foo -> api
prefix(Url) -> hd(string:tokens(Url,"/")).
