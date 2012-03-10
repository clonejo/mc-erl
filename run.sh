#!/bin/sh
exec erl -pa $PWD/ebin -boot start_sasl -s mc_erl_app