#!/bin/sh
cd `dirname $0`
exec erl -pa ../../ebin -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s playdar_httpd
