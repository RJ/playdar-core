A fuzzy-searchable database of files available on your local machine.

In priv/ is an erlang port for a taglib reader

To scan, something like this:
 playdarctl scan "/path/to/mp3"

or from the erl shell:
 library_dets:scan(playdar_resolver:resolver_pid(library_dets), "/path/to/mp3"). 


