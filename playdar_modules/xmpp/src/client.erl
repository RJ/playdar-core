-module(client).
-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-export([start/0, stop/1]).
-export([init/0]).

start() ->
    spawn(?MODULE, init, []).

stop(EchoClientPid) ->
    EchoClientPid ! stop.


init() ->
    application:start(exmpp),
    MySession = exmpp_session:start(),
    MyJID = exmpp_jid:make("playdartest1", "jabber.org", random),
    %% Create a new session with basic (digest) authentication:
    exmpp_session:auth_basic_digest(MySession, MyJID, "password"),
    %% Connect in standard TCP:
    _StreamId = exmpp_session:connect_TCP(MySession, "jabber.org", 5222),
    session(MySession, MyJID).

%% We are connected. We now log in (and try registering if authentication fails)
session(MySession, _MyJID) ->
    %% Login with defined JID / Authentication:
    exmpp_session:login(MySession),
    %% We explicitely send presence:
    exmpp_session:send_packet(MySession,
			      exmpp_presence:set_status(
				exmpp_presence:available(), "Echo Ready")),
    loop(MySession).

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
loop(MySession) ->
    receive
        stop ->
            exmpp_session:stop(MySession);

        Rec = #received_packet{packet_type=presence, from=From, raw_packet=Packet} ->
            case Rec#received_packet.type_attr of
                "unavailable" ->
                    io:format("OFFLINE ~p~n",[From]);
                "available" -> 
                    io:format("ONLINE ~p~n",[From]);
                X ->
                    io:format("UNKNOWN PRESENCE ~p ~p~n",[X, From])
            end;

        %% If we receive a message, we reply with the same message
        Record = #received_packet{packet_type=message, raw_packet=Packet} ->
            io:format("~p~n", [Record]),
            echo_packet(MySession, Packet),
            loop(MySession);
        Record ->
            io:format("~p~n", [Record]),
            loop(MySession)
    end.

%% Send the same packet back for each message received
echo_packet(MySession, Packet) ->
    From = exmpp_xml:get_attribute(Packet, from, <<"unknown">>),
    To = exmpp_xml:get_attribute(Packet, to, <<"unknown">>),
    TmpPacket = exmpp_xml:set_attribute(Packet, from, To),
    TmpPacket2 = exmpp_xml:set_attribute(TmpPacket, to, From),
    NewPacket = exmpp_xml:remove_attribute(TmpPacket2, id),
    exmpp_session:send_packet(MySession, NewPacket).
