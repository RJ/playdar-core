Playdar p2p resolver
====================
This resolver starts a p2p servent and can accept and make connections with
other peers. Queries are broadcast to all connected peers.
Queries, results and content streams are multiplexed down a single connection.

This is a work in progress, not suitable for large networks atm.

TODO
----
* flow control/bandwidth metering.
* UPnP router port-fwd autosetup. Probably using a C prog as driver.
* refwd/cancel/configuration to allow darknet style network.
* XMPP integration for finding hosts and bootstrapping the network.

Usage
-----
p2p_router:connect("peername_or_ip", 60210).
p2p_router:peers().

p2p.conf (no config is needed for defaults)
-------------------------------------------
% Port servent listens on:
{port, 60210}.
% Peers to connect to on startup:
{peers,[
    {"peer1", 60210},
    {"peer2", 9999}
]}.
