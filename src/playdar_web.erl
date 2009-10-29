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
                               || Pl <- playdar_resolver:resolvers() ],
            HttpMenus = playdar_http_registry:get_all(),
			% change bools to strings, for rendering (yuk, TODO hack erlydtl)
			Resolvers1 = [  begin
							    LOS = case proplists:get_value(localonly, Resolver, false) of
										 true -> "yes";
										 false -> "no"
								 	 end,
								[ {localonly, LOS} | proplists:delete(localonly, Resolver) ]
							end || Resolver <- Resolvers ],
            Vars = [ {resolvers, Resolvers1}, 
                     {protocols, playdar_reader_registry:get_all()},
                     {http_paths, HttpMenus}
                   ],
            render(Req, DocRoot ++ "/index.html", Vars);
        
        % serving a file that was found by a query, based on SID:
        "sid/" ++ SidL ->
            Sid = list_to_binary(SidL),
            case playdar_resolver:result(Sid) of
                undefined ->
                    Req:not_found();
                A ->
                    Ref = make_ref(),
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
                                       {"receiverurl", proplists:get_value("receiverurl",Qs,"")},
                                       {"formtoken", Ftok},
                                       {"website", proplists:get_value("website", Qs,"")},
                                       {"name", proplists:get_value("name", Qs, "")}
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
                    AuthCode = playdar_utils:uuid_gen(),
                    % m_pauth->create_new(tok, req.postvar("website"), req.postvar("name"), req.useragent() );
                    playdar_auth:create(AuthCode, [ {website, proplists:get_value("website",Qs, "")},
                                            {name, proplists:get_value("name",Qs, "")}
                                          ]),
                    case proplists:get_value("receiverurl",Qs, "") of
                        "" ->
                            case proplists:get_value("json", Qs, "") of 
                                undefined ->
                                    Vars = [    {website,proplists:get_value("website",Qs, "")},
                                                {name,proplists:get_value("name",Qs, "")},
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
            Qids = playdar_resolver:queries(),
            Fun = fun(Qid) ->
						  {Results, #qry{obj = Qryobj}, Solved} = playdar_resolver:results(Qid),
						  Json = mochijson2:encode(Qryobj),
                		  NumResults = length(Results),
                		  [{qid,Qid}, {solved, Solved}, 
						   {qry,Json}, {num_results,NumResults}]
            	  end,
            Vars = [ {queries, [Fun(Qid) || Qid <- Qids]} ],
            render(Req, DocRoot ++ "/queries.html", Vars);
        
        "queries/" ++ QidL ->
			Qid = list_to_binary(QidL),
            case playdar_resolver:results(Qid) of
                undefined -> Req:not_found();
                {ResultsList, #qry{obj = Qryobj}, Solved} ->
                    Results = [ [{list_to_atom(binary_to_list(K)),V}||{K,V}<-L] 
                                || {struct, L} <- ResultsList ],
                    Vars = [ {qid, Qid},
                             {qry, mochijson2:encode(Qryobj)},
							 {solved, Solved}, 
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
            Req:serve_file("static/" ++ StaticFile, DocRoot);

		"crossdomain.xml" ->
			% crossdomain support is on by default, but can be disabled in config:
			case ?CONFVAL(crossdomain, true) of
				true ->
					Req:serve_file("crossdomain.xml", DocRoot);
				false ->
					Req:not_found()
			end;
		
        % hand off dynamically:
        _ -> 
            case playdar_http_registry:get_handler(Req:get(path)) of
                undefined ->
                    Req:not_found();     
                Handler ->
                    Handler(Req, DocRoot)
            end
    end.


%% Internal API
  
render(Req, File, Vars) ->
    % if render is crashing, Vars is probably invalid:
    %?LOG(info, "render(~s) ~p", [File, Vars]),
    ok = erlydtl:compile(File, tpl), % TODO compiled templates could be cached
    {ok, HtmlIO} =  tpl:render(Vars),
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
