#!/bin/sh
echo "This only starts the playdar app."
echo "Use start-dev.sh from the mochi code to start playdar and the httpd."
cd `dirname $0`
erl -pa ebin/ -boot start_sasl -s playdar
