#!/bin/sh
cd `dirname $0`
exec erl -pa ebin -boot start_sasl -s playdar
