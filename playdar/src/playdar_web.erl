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
            Resolvers = [ [{"mod", atom_to_list(proplists:get_value(mod, Pl))}|proplists:delete(mod,Pl)]
                               || Pl <- resolver:resolvers() ],
            Vars = [ {resolvers, Resolvers} ],
            render(Req, DocRoot ++ "/index.html", Vars);
        
        % serving a file that was found by a query, based on SID:
        "sid/" ++ SidL ->
            Sid = list_to_binary(SidL),
            case resolver:sid2pid(Sid) of
                undefined ->
                    Req:not_found();
                Qpid ->
                    Ref = make_ref(),
                    A = qry:result(Qpid, Sid),
                    StreamFun = stream_registry:get_streamer(A, self(), Ref),
                    StreamFun(),
                    stream_result(Req, Ref)
            end;

        "queries" ->
            Qrys = resolver:queries(),
            Fun = fun({{qid, Qid},Qpid}) ->
                Json = mochijson2:encode(qry:q(Qpid)),
                NumResults = length(qry:results(Qpid)),
                [{qid,Qid}, {qry,Json}, {num_results,NumResults}]
            end,
            Vars = [ {queries, [Fun(Q) || Q <- Qrys]} ],
            render(Req, DocRoot ++ "/queries.html", Vars);
        
        "queries/" ++ Qid ->
            case resolver:qid2pid(list_to_binary(Qid)) of
                undefined -> Req:not_found();
                Qpid when is_pid(Qpid)->
                    Results = [ [{list_to_atom(binary_to_list(K)),V}||{K,V}<-L] 
                                || {struct, L} <- qry:results(Qpid) ],
                    Vars = [ {qid, Qid},
                             {qry, mochijson2:encode(qry:q(Qpid))}, 
                             {results, Results} ],
                    render(Req, DocRoot ++ "/query.html", Vars)
            end;
        
        % hand off dynamically:
        _ -> 
            case http_registry:get_handler(Req:get(path)) of
                undefined ->
                    Req:not_found();     
                Handler ->
                    Handler(Req)
            end
    end.


%% Internal API
  
render(Req, File, Vars) ->
    ok = erlydtl:compile(File, tpl_index),
    {ok, HtmlIO} =  tpl_index:render(Vars),
    Html = lists:flatten(HtmlIO),
    Req:ok({"text/html",[{"Server", "Playdar"}],Html}).

stream_result(Req, Ref) ->
    receive
        {Ref, headers, Headers0} ->
            {Mimetype0, Headers} = get_option("content-type", Headers0),
            Mimetype = case Mimetype0 of 
                undefined -> "binary/unspecified";
                X when is_list(X) -> X
            end,
            Resp = Req:ok( { Mimetype, [{"Server", "Playdar"}|Headers], chunked } ),
            %io:format("Headers sent~n",[]),
            stream_result_body(Req, Resp, Ref)
            
        after 12000 ->
            Req:ok({"text/plain", [{"Server", "Playdar"}], "Timeout on headers/initialising stream"})
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
