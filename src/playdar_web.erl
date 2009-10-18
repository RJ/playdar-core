%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for playdar.

-module(playdar_web).
-export([start/1, stop/0, loop/2, render/3]).
-include("playdar.hrl").

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun(Req) -> ?MODULE:loop(Req, DocRoot) end,
    Opts = [ {loop, Loop}, {name, ?MODULE} | Options1 ],
    mochiweb_http:start(Opts).
                

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    % TODO filter non /sid/ reqs unless from localhost
	{R1,R2,R3} = now(),
	random:seed(R1,R2,R3),
    Peer = Req:get(peer),
    ?LOG(info, "~s ~s ~s", [string:to_upper(atom_to_list(Req:get(method))),
                            Req:get(raw_path), Peer]),
    % Reqs from localhost can do anything
    % reqs from elsewhere are only allowed to stream.
    % this presumes they did the resolving using lan plugin or something.
    case Req:get(peer) of
        "127.0.0.1"         -> loop1(Req, DocRoot);
        "::1"               -> loop1(Req, DocRoot);
        "0:0:0:0:0:0:0:1"   -> loop1(Req, DocRoot);
        _ ->
            case Req:get(path) of
                "/sid/" ++ _ -> 
                    loop1(Req, DocRoot);
                _ ->
                    Req:respond({403, [], <<"<h1>Not Authorised</h1>">>})
            end
    end.
                    
loop1(Req, DocRoot) ->    
    "/" ++ Path = Req:get(path),        
    case Path of
        "" -> 
            Resolvers = [ [{"mod", atom_to_list(proplists:get_value(mod, Pl))}|proplists:delete(mod,Pl)]
                               || Pl <- resolver:resolvers() ],
            HttpMenus = http_registry:get_all(),
            Vars = [ {resolvers, Resolvers}, 
                     {protocols, playdar_reader_registry:get_all()},
                     {http_paths, HttpMenus}
                   ],
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
                    case playdar_reader_registry:get_streamer(A, self(), Ref) of
                        undefined ->
                            Req:respond({503, [], <<"Playdar server error: no such protocol handler">>});
                        Sfun -> 
                            Sfun(),
                            stream_result(Req, Ref)
                    end
            end;

        "auth_1/" ->
            Qs = Req:parse_qs(),
            Required = ["website", "name" ],
            case lists:member(undefined, [ proplists:get_value(R, Qs) ||
                                           R <- Required]) of
                true ->
                    Req:not_found();

                false ->
                    Ftok = playdar_auth:gen_formtoken(),
                    case proplists:get_value("json", Qs) of 
                        undefined ->
                            FormVars= [
                                       {"receiverurl", proplists:get_value("receiverurl",Qs)},
                                       {"formtoken", Ftok},
                                       {"website", proplists:get_value("website", Qs)},
                                       {"name", proplists:get_value("name", Qs)}
                                      ],
                            render(Req, DocRoot ++ "/auth.html", [{formvars, FormVars}]);

                        _  ->
                            Resp = mochijson2:encode({struct, [{<<"formtoken">>,list_to_binary(Ftok)}]}),
                            Req:ok({"text/javascript; charset=utf-8",[],Resp})
                    end
            end;
            

        "auth_2/" ->
            Qs = Req:parse_post(),
            io:format("~p~n", [Qs]),
            Required = ["website", "name", "formtoken"],
            AllPresent = not lists:member(undefined, [ proplists:get_value(R, Qs) ||
                                                  R <- Required]),
            FormTokOk = (ok == playdar_auth:consume_formtoken(proplists:get_value("formtoken", Qs))),
            io:format("AllPressent ~w formtok ~w~n", [AllPresent, FormTokOk]),
            if
                AllPresent and FormTokOk ->
                    % create the entry in the auth db:
                    AuthCode = utils:uuid_gen(),
                    % m_pauth->create_new(tok, req.postvar("website"), req.postvar("name"), req.useragent() );
                    playdar_auth:create(AuthCode, [ {website, proplists:get_value("website",Qs)},
                                            {name, proplists:get_value("name",Qs)}
                                          ]),
                    case proplists:get_value("receiverurl",Qs,"") of
                        "" ->
                            case proplists:get_value("json", Qs) of 
                                undefined ->
                                    Vars = [    {website,proplists:get_value("website",Qs)},
                                                {name,proplists:get_value("name",Qs)},
                                                {authcode, AuthCode}
                                           ],
                                    render(Req, DocRoot ++ "/auth.na.html", Vars);
                                _ ->
                                    Resp = mochijson2:encode({struct, [{<<"authtoken">>,AuthCode}]}),
                                    Req:ok({"text/javascript; charset=utf-8",[],Resp})
                            end;

                        RecUrl ->
                            % verify RecUrl doesnt contain newlines
                            AuthStr = binary_to_list(AuthCode),
                            Url = case lists:member($?, RecUrl) of
                                true ->
                                    RecUrl ++ "&authtoken=" ++ AuthStr ++ "#" ++ AuthStr;
                                false ->
                                    RecUrl ++ "?authtoken=" ++ AuthStr ++ "#" ++ AuthStr
                            end,
                            io:format("Redirect: ~s~n", [Url]),
                            Req:respond({301, [{"Location", Url}], <<"">>})
                    end;

                true ->
                    Req:not_found()
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
        
        "authcodes" ->
            Qs = Req:parse_qs(),
            case proplists:get_value("revoke",Qs) of
                undefined ->
                    Codes = [ [{code, Code}|Props] || {Code, Props} <- playdar_auth:all() ],
                    render(Req, DocRoot ++ "/authcodes.html", [{codes, Codes}]);
                Code ->
                    playdar_auth:revoke(list_to_binary(Code)),
                    Req:respond({302, [{"Location", "/authcodes"}], <<"">>})
            end;

        % serve any file under /static/ verbatim
        "static/" ++ StaticFile ->
        io:format("static:~s~n",[StaticFile]),
            Req:serve_file("static/" ++ StaticFile, DocRoot);

		"crossdomain.xml" ->
			case ?CONFVAL(crossdomain, false) of
				true ->
					Req:serve_file("crossdomain.xml", DocRoot);
				false ->
					Req:not_found()
			end;
		
        % hand off dynamically:
        _ -> 
            case http_registry:get_handler(Req:get(path)) of
                undefined ->
                    Req:not_found();     
                Handler ->
                    Handler(Req, DocRoot)
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
