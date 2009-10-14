-module(jabber_client).
-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-behaviour(gen_server).
-include("playdar.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {session, jid}).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks
init([]) ->
    application:start(exmpp),
    Session = exmpp_session:start(),
    JID = exmpp_jid:make(?CONFVAL({jabber,username}),
                         ?CONFVAL({jabber,domain}),
                         "playdar-"++integer_to_list(random:uniform(100000000))),
    exmpp_session:auth_basic(Session, JID, ?CONFVAL({jabber, password})),
    _StreamId = exmpp_session:connect_TCP(Session, "jabber.org", 5222),
    gen_server:cast(?MODULE, do_login),
    {ok, #state{session=Session, jid=JID}}.

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

handle_info(Rec = #received_packet{packet_type=presence, from=From, raw_packet=Packet}, State) ->
    case Rec#received_packet.type_attr of
        "unavailable" ->
            io:format("OFFLINE ~p~n",[From]);
        "available" -> 
            io:format("ONLINE ~p~n",[From]);
        X ->
            io:format("UNKNOWN PRESENCE ~p ~p~n",[X, From])
    end,
    {noreply, State};
            
handle_info(Record = #received_packet{packet_type=message, raw_packet=Packet}, State) ->
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
    io:format("RCV: ~p~n",[Msg]),
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
