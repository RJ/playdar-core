%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for playdar.

-module(playdar_web).
-author('author <author@example.com>').
-import(random).
-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    {Port, Options2} = get_option(port, Options1),
    {Ip, Options3} = get_option(ip, Options2),
    Loop = fun (Req) -> ?MODULE:loop(Req, DocRoot) end,
    MochiOpts = [   {max, 20},
                    {port, 60210},
                    {ip, Ip},
                    %{docroot, DocRoot},
                    {name, ?MODULE}, 
                    {loop, Loop}
                    | Options3
                ],
    %io:format("~p~n",[MochiOpts]),
    mochiweb_http:start(MochiOpts).
                

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
	{R1,R2,R3} = now(),
	random:seed(R1,R2,R3),
    io:format("GET ~p~n", [Req:get(raw_path)]),
    "/" ++ Path = Req:get(path),
    
    case Path of
        "" -> 
            Req:ok({"text/html", "<h1>Playdar</h1>Playdar-erlang is running"});
        
        % serving a file that was found by a query, based on SID:
        "sid/" ++ SidL ->
            Sid = list_to_binary(SidL),
            case resolver:sid2pid(Sid) of
                undefined ->
                    Req:not_found();
                Qpid ->
                    Ref = make_ref(),
                    A = qry:result(Qpid, Sid),
                    stream_reader:start_link(A, self(), Ref),
                    stream_result(Req, Ref)
            end;

        % hardcoded support for api/
        "api/" ++ _ ->
            playdar_http_api:http_req(Req);
        
        % else hand off to module:
        _ -> 
            Parts = string:tokens(Req:get(path),"/"),
            Mod = hd(Parts),
            Modules = [], % modules whitelist (TODO, use loaded plugins)
            case lists:foldl( fun(X,A) -> case X of Mod -> A+1 ; _ -> A end end,
                              0, Modules) of
                0 ->
                    Req:not_found();
                _ ->
                    Module = list_to_atom("playdar_http_" ++ Mod),
                    % TODO filter/verify valid module names
                    Module:http_req(Req)
           end
    end.


%% Internal API

stream_result(Req, Ref) ->
    receive
        {Ref, headers, Headers0} ->
            {Mimetype0, Headers} = get_option("content-type", Headers0),
            Mimetype = case Mimetype0 of 
                undefined -> "binary/unspecified";
                X when is_list(X) -> X
            end,
            Resp = Req:ok( { Mimetype, Headers, chunked } ),
            %io:format("Headers sent~n",[]),
            stream_result_body(Req, Resp, Ref)
            
        after 12000 ->
            Req:ok({"text/plain", "Timeout on headers/initialising stream"})
    end.
    
stream_result_body(Req, Resp, Ref) ->
    receive
        {Ref, data, Data} ->
            Resp:write_chunk(Data),
            stream_result_body(Req, Resp, Ref);
        
        {Ref, error, _Reason} ->
            err;
        
        {Ref, eof} ->
            ok
    
    after 10000 ->
        io:format("10secs timeout on streaming~n",[]),
            timeout
    end.
    

get_option(Option, Options) -> get_option(Option, Options, undefined).
get_option(Option, Options, Def) ->
    {proplists:get_value(Option, Options, Def), proplists:delete(Option, Options)}.
