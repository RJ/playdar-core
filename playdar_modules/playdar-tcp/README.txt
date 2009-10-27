playdar-tcp resolver
====================
This resolver allows you to link two playdar installs via a TCP connection.
This is more efficient than HTTP and allows you to easily tunnel your home
playdar (via ssh) to your work machine, for example.

It is also hoped that people will host playdar nodes with large free repos,
such as magnatune, archive.org etc. This resolver would be useful for that.

Currently streams are multiplexed (with no flow control) down the same pipe
as queries, so you will notice whilst streaming over a low bandwidth link
that queries can't be recieved at the same time. (will be fixed "soon").

***ATTENTION***

By default this resolver will not handle incoming queries - if you are sure
you want to share your library via this resolver, edit the config file.

Edit playdartcp.conf.example and save as "playdartcp.conf" in your Playdar
etc directory.


Usage
-----
From the erl shell, you can connect to your home machine like so:

 playdartcp_router:connect("myhomepc.example.com", 60211, true).

IP addresses or hostnames are valid.
The last parameter (true/false) indicates whether you want to share you content
with the node you're connecting to.

To see who you're connected to, check the playdartcp page on:

 http://localhost:60210/

