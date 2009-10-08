%% @doc Supervisor for the playdar application.

-module(playdar_sup).
-author('author <author@example.com>').
-include("playdar.hrl").
-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    inets:start(),
    application:start(crypto), % it isn't started automatically on windows?
    ok = playdar_config:load(get_etc_dir()),
    DefaultWebConfig = [{port, 60210}, {max, 100}, {ip, "0.0.0.0"}, {docroot, playdar_deps:local_path(["priv", "www"])}],
    WebConfig = ?CONFVAL(web, DefaultWebConfig),
    io:format("WebConfig = ~p~n", [WebConfig]),
    % Specs:
    Web         = { playdar_web,
                    {playdar_web, start, [WebConfig]},
                    permanent, 5000, worker, dynamic},

    Auth        = { playdar_auth,
                    {playdar_auth, start_link, []},
                    permanent, 5000, worker, []},
    
    ResolverSup = { resolver_sup,
                    {resolver_sup, start_link, []},
                    transient, infinity, supervisor, [resolver_sup]},

    Reader      = { playdar_reader_registry, 
                    {playdar_reader_registry, start_link, []},
                    permanent, 1000, worker, [] },
    
    ModulesSup  = { modules_sup,
                    {modules_sup, start_link, []},
                    transient, infinity, supervisor, [modules_sup]},

    HttpReg     = { http_registry, 
                    {http_registry, start_link, []},
                    permanent, 1000, worker, [] },


    Processes = [Reader, ResolverSup, HttpReg, Auth, ModulesSup, Web],

    {ok, {{one_for_one, 10, 10}, Processes}}.

% gets dir with playdar config files in
get_etc_dir() ->
    case os:getenv("PLAYDAR_ETC") of
        false -> "./etc"; % TODO guess sensible default, per OS etc.
        Dir -> 
            case filelib:is_dir(Dir) of
                true -> Dir;
                false -> exit("No such etc directory")
            end
    end.

    
