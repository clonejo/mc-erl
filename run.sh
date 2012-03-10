#!/bin/sh
exec erl -pa $PWD/ebin -boot -s mc_erl_app
