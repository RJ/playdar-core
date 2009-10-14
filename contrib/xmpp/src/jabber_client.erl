-module(jabber_client).
-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-behaviour(gen_server).
-include("playdar.hrl").

%% API
-export([start_link/0, peers/0, send_msg/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

% peers is a list of JIDs of playdar-compatible peers only, not people on IM clients
-record(state, {session, jid, peers}).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

peers() -> gen_server:call(?MODULE, peers).

send_msg(Jid, Msg) when is_list(Msg)   -> send_msg(Jid, list_to_binary(Msg));
send_msg(Jid, Msg) when is_binary(Msg) -> gen_server:call(?MODULE, {send_msg, Jid, Msg}).

%% gen_server callbacks
init([]) ->
    application:start(exmpp),
    Session = exmpp_session:start(),
    JID = exmpp_jid:make(?CONFVAL({jabber,username}, undefined),
                         ?CONFVAL({jabber,domain}, undefined),
                         "playdar-"++integer_to_list(random:uniform(100000000))),
    exmpp_session:auth_basic(Session, JID, ?CONFVAL({jabber, password}, undefined)),
    _StreamId = exmpp_session:connect_TCP(Session, ?CONFVAL({jabber,domain}, undefined), 5222),
    gen_server:cast(?MODULE, do_login),
	Peersdb = ets:new(peers,[]),
    {ok, #state{session=Session, jid=JID, peers=Peersdb}}.

handle_call(peers, _From, State) ->
	R = [ Jid || {Jid, _} <- ets:tab2list(State#state.peers) ],
	{reply, R, State};

handle_call({send_msg, Jid, Msg}, _From, State) ->
	M  = exmpp_message:normal(Msg),
	M2 = exmpp_stanza:set_recipient(M, exmpp_jid:make(Jid)),
	M3 = exmpp_stanza:set_sender(M2, State#state.jid), 
	exmpp_session:send_packet(State#state.session, M3),
	{reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(do_login, State) ->
    exmpp_session:login(State#state.session),
    Presence  = exmpp_presence:set_status(exmpp_presence:available(), 
                                          "Playdar Bot (not human)"),
    Presence1 = exmpp_presence:set_show(Presence, xa),
    exmpp_session:send_packet(State#state.session, Presence1),
    {noreply, State}.

handle_info(Rec = #received_packet{packet_type=presence, from=From, raw_packet=_Packet}, State) ->
	case playdar_capable(From) of
		false ->
			{noreply, State};
		true ->
			case Rec#received_packet.type_attr of
				"unavailable" ->
					io:format("OFFLINE ~p~n",[From]),
					ets:delete(State#state.peers, From),
					{noreply, State};
				"available" -> 
					io:format("ONLINE ~p~n",[From]),
					ets:insert(State#state.peers, {From, true}),
					{noreply, State};
				X ->
					io:format("UNKNOWN PRESENCE ~p ~p~n",[X, From])
			end		
	end;    
            
handle_info(_Record = #received_packet{packet_type=message, raw_packet=Packet}, State) ->
    case exmpp_message:get_body(Packet) of
        undefined -> foo;
        Body -> io:format("RCV Chat msg body: ~p~n", [Body]),
                M  = exmpp_message:normal(<<"THanks for the msg!">>),
                M2 = exmpp_stanza:set_recipient(M, exmpp_xml:get_attribute(Packet, from, <<"unknown">>)),
                M3 = exmpp_stanza:set_sender(M2, State#state.jid), 
                exmpp_session:send_packet(State#state.session, M3)
    end,
    {noreply, State};
            
handle_info(Msg, State) ->
    io:format("UNHANDLED RCV: ~p~n",[Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions


%% Process exmpp packet:
%-record(received_packet,
%        {
%          packet_type, % message, iq, presence
%          type_attr,   % depend on packet. Example: set, get, subscribe, etc
%          from,        % JID
%          id,          % Packet ID
%          queryns,     % IQ only: Namespace of the query
%          raw_packet   % raw exmpp record
%        }).

%% Send the same packet back for each message received
echo_packet(MySession, Packet) ->
    From = exmpp_xml:get_attribute(Packet, from, <<"unknown">>),
    To = exmpp_xml:get_attribute(Packet, to, <<"unknown">>),
    TmpPacket = exmpp_xml:set_attribute(Packet, from, To),
    TmpPacket2 = exmpp_xml:set_attribute(TmpPacket, to, From),
    NewPacket = exmpp_xml:remove_attribute(TmpPacket2, id),
    exmpp_session:send_packet(MySession, NewPacket).


playdar_capable({_User, _Domain, Resource} = _Jid) ->
	R = binary_to_list(Resource),
	string:str(R, "playdar") /= 0.