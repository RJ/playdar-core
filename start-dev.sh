#!/bin/sh
cd `dirname $0`
exec erl -sname playdar@localhost -pa ebin -boot start_sasl -s reloader -s playdar
