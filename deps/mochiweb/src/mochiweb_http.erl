%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc HTTP server.

-module(mochiweb_http).
-author('bob@mochimedia.com').
-export([start/0, start/1, stop/0, stop/1]).
-export([loop/2, default_body/1]).
-export([after_response/2, reentry/1]).

-define(IDLE_TIMEOUT, 30000).

-define(MAX_HEADERS, 1000).
-define(DEFAULTS, [{name, ?MODULE},
                   {port, 8888}]).

set_default({Prop, Value}, PropList) ->
    case proplists:is_defined(Prop, PropList) of
        true ->
            PropList;
        false ->
            [{Prop, Value} | PropList]
    end.

set_defaults(Defaults, PropList) ->
    lists:foldl(fun set_default/2, PropList, Defaults).

parse_options(Options) ->
    WwwLoop = proplists:get_value(loop, Options),
    WSLoop  = proplists:get_value(wsloop, Options),
    Loop = fun (S) ->
                   ?MODULE:loop(S, {WwwLoop,WSLoop})
           end,
    Options1 = [{loop, Loop}, {wsloop, Loop} | proplists:delete(loop, proplists:delete(wsloop, Options))],
    set_defaults(?DEFAULTS, Options1).

stop() ->
    mochiweb_socket_server:stop(?MODULE).

stop(Name) ->
    mochiweb_socket_server:stop(Name).

start() ->
    start([{ip, "127.0.0.1"},
           {loop, {?MODULE, default_body}}]).

start(Options) ->
    mochiweb_socket_server:start(parse_options(Options)).

frm(Body) ->
    ["<html><head></head><body>"
     "<form method=\"POST\">"
     "<input type=\"hidden\" value=\"message\" name=\"hidden\"/>"
     "<input type=\"submit\" value=\"regular POST\">"
     "</form>"
     "<br />"
     "<form method=\"POST\" enctype=\"multipart/form-data\""
     " action=\"/multipart\">"
     "<input type=\"hidden\" value=\"multipart message\" name=\"hidden\"/>"
     "<input type=\"file\" name=\"file\"/>"
     "<input type=\"submit\" value=\"multipart POST\" />"
     "</form>"
     "<pre>", Body, "</pre>"
     "</body></html>"].

default_body(Req, M, "/chunked") when M =:= 'GET'; M =:= 'HEAD' ->
    Res = Req:ok({"text/plain", [], chunked}),
    Res:write_chunk("First chunk\r\n"),
    timer:sleep(5000),
    Res:write_chunk("Last chunk\r\n"),
    Res:write_chunk("");
default_body(Req, M, _Path) when M =:= 'GET'; M =:= 'HEAD' ->
    Body = io_lib:format("~p~n", [[{parse_qs, Req:parse_qs()},
                                   {parse_cookie, Req:parse_cookie()},
                                   Req:dump()]]),
    Req:ok({"text/html",
            [mochiweb_cookies:cookie("mochiweb_http", "test_cookie")],
            frm(Body)});
default_body(Req, 'POST', "/multipart") ->
    Body = io_lib:format("~p~n", [[{parse_qs, Req:parse_qs()},
                                   {parse_cookie, Req:parse_cookie()},
                                   {body, Req:recv_body()},
                                   Req:dump()]]),
    Req:ok({"text/html", [], frm(Body)});
default_body(Req, 'POST', _Path) ->
    Body = io_lib:format("~p~n", [[{parse_qs, Req:parse_qs()},
                                   {parse_cookie, Req:parse_cookie()},
                                   {parse_post, Req:parse_post()},
                                   Req:dump()]]),
    Req:ok({"text/html", [], frm(Body)});
default_body(Req, _Method, _Path) ->
    Req:respond({501, [], []}).

default_body(Req) ->
    default_body(Req, Req:get(method), Req:get(path)).

loop(Socket, Body) ->
    inet:setopts(Socket, [{packet, http}]),
    request(Socket, Body).

request(Socket, Body) ->
    case gen_tcp:recv(Socket, 0, ?IDLE_TIMEOUT) of
        {ok, {http_request, Method, Path, Version}} ->
            headers(Socket, {Method, Path, Version}, [], Body, 0);
        {error, {http_error, "\r\n"}} ->
            request(Socket, Body);
        {error, {http_error, "\n"}} ->
            request(Socket, Body);
        _Other ->
            gen_tcp:close(Socket),
            exit(normal)
    end.

reentry(Body) ->
    fun (Req) ->
            ?MODULE:after_response(Body, Req)
    end.

headers(Socket, Request, Headers, _Body, ?MAX_HEADERS) ->
    %% Too many headers sent, bad request.
    inet:setopts(Socket, [{packet, raw}]),
    Req = mochiweb:new_request({Socket, Request,
                                lists:reverse(Headers)}),
    Req:respond({400, [], []}),
    gen_tcp:close(Socket),
    exit(normal);
    
headers(Socket, Request, Headers, {WwwLoop, WSLoop}, HeaderCount) ->
    case gen_tcp:recv(Socket, 0, ?IDLE_TIMEOUT) of
        {ok, http_eoh} ->
            {_, {abs_path,Path}, _} = Request,
	        case websocket_check(Socket, Path, Headers) of
                true ->  % a websocket request
            		inet:setopts(Socket, [{packet, raw}]),
                    WSRequest = websocket_request:new(Socket,Path),
                    WSLoop(WSRequest);
	            false -> % normal http request
		            inet:setopts(Socket, [{packet, raw}]),
		            Req = mochiweb:new_request({Socket, Request,
					                           lists:reverse(Headers)}),
            		WwwLoop(Req),
            		?MODULE:after_response({WwwLoop, WSLoop}, Req)
    	    end;
        {ok, {http_header, _, Name, _, Value}} ->
            headers(Socket, Request, [{Name, Value} | Headers], {WwwLoop, WSLoop},
                    1 + HeaderCount);
        _Other ->
            gen_tcp:close(Socket),
            exit(normal)
    end.

after_response(Body, Req) ->
    Socket = Req:get(socket),
    case Req:should_close() of
        true ->
            gen_tcp:close(Socket),
            exit(normal);
        false ->
            Req:cleanup(),
            ?MODULE:loop(Socket, Body)
    end.

websocket_check(Socket,Path,Headers) ->
    case proplists:get_value('Upgrade',Headers) of
        "WebSocket" ->
            websocket_send_handshake(Socket,Path,Headers), 
            true;
        _Other ->
            false
    end.

websocket_send_handshake(Socket,Path,Headers) ->
    Origin   = proplists:get_value("Origin",Headers),
    Location = proplists:get_value('Host',  Headers),
    Proto = "HTTP/1.1 101 Web Socket Protocol Handshake\r\nUpgrade: WebSocket\r\nConnection: Upgrade\r\n",
    Resp = Proto ++
    "WebSocket-Origin: " ++ Origin ++ "\r\n" ++
    "WebSocket-Location: ws://" ++ Location ++ Path ++ "\r\n\r\n",
    gen_tcp:send(Socket, Resp).

